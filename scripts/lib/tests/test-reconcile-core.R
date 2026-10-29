## test-reconcile-core.R -- nucleo do Reconcile (R.1): plan, Bottom-Up, SIN, contrato de entrada, catalogo de erros
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "schema_long", "deck_reader", "evaluation", "reconcile"))
ctx <- pc_rc_ctx()
O <- "2026-01-31"
topo_of <- function(n, move = NULL) env$prevcarga_topology(path = pc_topology_fixture(n, move = move), root = tempdir())

full_run <- function(topo) {
  long <- pc_leaves_long(topo, O, ctx)
  prof <- pc_khat_profile_unit()
  lapply(stats::setNames(topo$order, topo$order), function(ss) {
    bu <- env$prevcarga_reconcile_bottomup(long, ss, topo, ctx)
    env$prevcarga_khat_apply(bu, prof, ctx)
  })
}

test_that("R.1 AC1 - subsistema = soma exata das folhas e SIN soma n subsistemas (3 e 5), count-agnostic", {
  for (n in c(3L, 5L)) {
    topo <- topo_of(n)
    res <- full_run(topo)
    longs <- list()
    for (ss in topo$order) {
      r <- res[[ss]]
      expect_equal(nrow(r), 48L)
      expect_equal(r$y_pred, pc_leaves_expected(topo, ss, 1:48), tolerance = 1e-9)
      longs[[ss]] <- env$prevcarga_reconcile_long(r, ss, ctx)
    }
    sin <- env$prevcarga_reconcile_sin(longs, topo)
    expect_equal(nrow(sin), 48L)
    expect_equal(sin$y_pred, Reduce(`+`, lapply(topo$order, function(ss) pc_leaves_expected(topo, ss, 1:48))),
                 tolerance = 1e-9)
    expect_equal(nrow(attr(sin, "dropped")), 0L)
    ## SIN so fecha com TODOS os subsistemas (n == length(topology$subsystems))
    part <- suppressWarnings(env$prevcarga_reconcile_sin(longs[-1L], topo))
    expect_equal(nrow(part), 0L)
  }
})

test_that("R.1 AC1 CONTROL - mover uma folha muda os DOIS BU_pred envolvidos e deixa os demais em delta 0", {
  base <- topo_of(5L)
  moved <- topo_of(5L, move = list(leaf = "Ls1_1", to = "SS3"))
  a <- full_run(base); b <- full_run(moved)
  for (ss in base$order) {
    d <- max(abs(a[[ss]]$BU_pred - b[[ss]]$BU_pred))
    if (ss %in% c("SS1", "SS3")) expect_gt(d, 0) else expect_identical(d, 0)
  }
})

test_that("R.1 AC3 - cobertura comum: 3 instantes saem, attr dropped e aviso; CONTROL completo sem aviso", {
  topo <- topo_of(3L)
  leaf <- topo$subsystems[["SS2"]]$areas[[1L]]
  long <- pc_leaves_long(topo, O, ctx, drop = list(leaf = leaf, idx = 1:3))
  w <- NULL
  bu <- withCallingHandlers(env$prevcarga_reconcile_bottomup(long, "SS2", topo, ctx),
    prevcarga_warning = function(c) { w <<- c; invokeRestart("muffleWarning") })
  expect_false(is.null(w))
  expect_identical(w$id, "reconcile.coverage_dropped")
  expect_equal(nrow(bu), 45L)
  expect_identical(attr(bu, "dropped"), data.table::data.table(d_plus = 0L, n = 3L))
  expect_equal(bu$BU_pred, pc_leaves_expected(topo, "SS2", 4:48), tolerance = 1e-9)  # nada preenchido com 0
  full <- pc_leaves_long(topo, O, ctx)
  expect_no_warning(bu2 <- env$prevcarga_reconcile_bottomup(full, "SS2", topo, ctx))
  expect_equal(nrow(attr(bu2, "dropped")), 0L)
})

test_that("R.1 AC5 - plan: subsistema sem folha nao fecha, folhas presentes vao para stray; CONTROL tudo presente", {
  topo <- topo_of(3L)
  s <- "SS2"
  miss <- topo$subsystems[[s]]$areas[[1L]]
  present <- setdiff(topo$leaves, miss)
  p <- env$prevcarga_reconcile_plan(present, topo)
  expect_identical(p$closed, setdiff(topo$order, s))
  expect_identical(p$stray, setdiff(topo$subsystems[[s]]$areas, miss))
  expect_false(p$sin_closed)
  p0 <- env$prevcarga_reconcile_plan(topo$leaves, topo)
  expect_identical(p0$closed, topo$order)
  expect_identical(p0$stray, character())
  expect_true(p0$sin_closed)
})

test_that("R.1 AC6 - contrato de entrada: lead divergente, tz, granularidade, folha ausente; sem objeto parcial", {
  topo <- topo_of(3L)
  long <- pc_leaves_long(topo, O, ctx)
  fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
  ## lead diferente entre folhas para o mesmo target_datetime
  leaf <- topo$subsystems[["SS1"]]$areas[[2L]]
  bad <- long; bad$lead[bad$area == leaf] <- bad$lead[bad$area == leaf] + 1L
  e <- fail(env$prevcarga_reconcile_bottomup(bad, "SS1", topo, ctx))
  expect_identical(e$id, "contract.reconcile_input"); expect_identical(e$exit_code, 2L)
  expect_match(e$where, leaf, fixed = TRUE)
  ## d_plus divergente: o validador do LONG (M3) reprova antes (d_plus = dia do interval_start)
  bad <- long; bad$d_plus[bad$area == leaf] <- 1L
  expect_identical(fail(env$prevcarga_reconcile_bottomup(bad, "SS1", topo, ctx))$id, "contract.long_dplus_label")
  ## tz diferente de ctx$tz
  bad <- long; bad$target_datetime <- lubridate::with_tz(bad$target_datetime, "UTC")
  e <- fail(env$prevcarga_reconcile_bottomup(bad, "SS1", topo, ctx))
  expect_identical(e$id, "contract.long_tz"); expect_identical(e$exit_code, 2L)
  ## granularidade != 30
  bad <- long; bad$granularity_min <- 60L
  e <- fail(env$prevcarga_reconcile_bottomup(bad, "SS1", topo, ctx))
  expect_identical(e$id, "contract.long_schema"); expect_identical(e$exit_code, 2L)
  ## folha ausente do subsistema
  e <- fail(env$prevcarga_reconcile_bottomup(long[long$area != leaf, ], "SS1", topo, ctx))
  expect_identical(e$id, "completude.leaf_missing"); expect_identical(e$exit_code, 3L)
  ## folha duplicada e subsistema desconhecido
  e <- fail(env$prevcarga_reconcile_bottomup(rbind(long, long[long$area == leaf, ]), "SS1", topo, ctx))
  expect_identical(e$id, "contract.reconcile_input")
  e <- fail(env$prevcarga_reconcile_bottomup(long, "nao_existe", topo, ctx))
  expect_identical(e$id, "contract.reconcile_input")
  ## lista de LONGs por folha equivale ao LONG unico
  parts <- split(long, long$area)
  a <- env$prevcarga_reconcile_bottomup(parts, "SS1", topo, ctx)
  b <- env$prevcarga_reconcile_bottomup(long, "SS1", topo, ctx)
  expect_identical(a$BU_pred, b$BU_pred)
})

test_that("R.1 AC7 - reconcile.tsv: ids do epico, classe/exit do ADR-0040, ASCII, sem duplicata; CONTROLs", {
  lib <- file.path(pc_root(), "scripts", "lib")
  cat <- env$prevcarga_errors_load(file.path(lib, "errors"))
  want <- c(contract.reconcile_input = 2L, contract.khat_convention = 2L, contract.khat_profile = 2L,
            completude.leaf_missing = 3L, completude.khat_no_truth = 3L, integrity.khat_conflict = 9L,
            state.khat_missing = 13L, honesty.khat_after_origin = 15L)
  for (id in names(want)) {
    row <- cat[cat$id == id, ]
    expect_equal(nrow(row), 1L, info = id)
    expect_identical(unname(env$prevcarga_exit_codes[[row$class]]), unname(want[[id]]), info = id)
  }
  warns <- c("reconcile.coverage_dropped", "reconcile.khat_fallback", "reconcile.khat_not_promoted")
  expect_true(all(warns %in% cat$id))
  expect_false(anyDuplicated(cat$id) > 0L)
  tsv <- file.path(lib, "errors", "reconcile.tsv")
  raw <- readBin(tsv, "raw", file.size(tsv))
  expect_true(all(as.integer(raw) < 128L))
  expect_false(as.raw(13L) %in% raw)
  ## todo id referido em reconcile.R consta no TSV
  src <- readLines(file.path(lib, "reconcile.R"), warn = FALSE)
  refs <- unique(sub("^.*\"([a-z_.]+)\".*$", "\\1", regmatches(src, regexpr("prevcarga_(abort|warn)\\(\"[a-z_.]+\"", src))))
  rec <- utils::read.delim(tsv, quote = "", colClasses = "character")
  expect_true(length(refs) > 0L)
  expect_identical(setdiff(refs, cat$id), character())
  expect_true(all(c("contract.reconcile_input", "completude.leaf_missing") %in% rec$id))
  ## CONTROL: id removido do TSV mas ainda chamado reprova
  expect_identical(setdiff(refs, setdiff(cat$id, "contract.reconcile_input")), "contract.reconcile_input")
  ## CONTROL: TSV com id repetido aborta contract.error_catalog_dup
  d <- tempfile("dup-"); dir.create(d)
  file.copy(tsv, file.path(d, "reconcile.tsv")); file.copy(tsv, file.path(d, "zz.tsv"))
  e <- tryCatch(env$prevcarga_errors_load(d), prevcarga_error = identity)
  expect_identical(e$id, "contract.error_catalog_dup")
})

test_that("R.1 - helper: pc_write_leaves grava <dir>/<area>/long.parquet", {
  topo <- topo_of(3L)
  d <- pc_reconcile_state_dir()
  pc_write_leaves(d, pc_leaves_long(topo, O, ctx))
  expect_true(all(file.exists(file.path(d, topo$leaves, "long.parquet"))))
})
