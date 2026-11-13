## test-forecast_tolerance.R -- tolerancia a membro ausente no forecast (F3.3): celula ausente => combina renormaliza sobre os
## presentes; folha sem membro aborta em subsistema/SIN antes do scratch e sai parcial (exit 14) em escopo area; ids config./
## honesty. nunca viram membro ausente. Fixture m6f (SS1 = A1,A2; SS2 = B1), espiao de infer por (sigla, folha).
## Cada FORBIDDEN tem o seu CONTROL. Tudo em tempdir; sem variaveis de ambiente.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("ft-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)

fx_copy <- function(.frame = parent.frame()) {
  st <- withr::local_tempdir("ft-state-", .local_envir = .frame)
  file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
  m6f_ctx(DECK, st, TOPO)
}
mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- env[[nm]]; withr::defer(assign(nm, old, envir = env), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
}
run_fc <- function(ctx, fail_cells = character(), fail_id = "state.artifact_missing", .frame = parent.frame(), ...) {
  log <- new.env(); log$infer <- list(); log$train <- list()
  mock(prevcarga_infer_cell = m6f_infer_spy(env, log, fail_cells = fail_cells, fail_id = fail_id),
       prevcarga_train = m6f_train_spy(env, log), .frame = .frame)
  env$prevcarga_forecast(ctx, skip_validate = TRUE, ...)
}
catch_pc <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
yp <- function(r, f) arrow::read_parquet(file.path(r$result$saida, "folhas", paste0(f, ".parquet")))
no_scratch <- function(ctx) !dir.exists(file.path(ctx$state_dir, "scratch")) || !length(list.files(file.path(ctx$state_dir, "scratch")))
no_forecasts <- function(ctx) !dir.exists(file.path(ctx$state_dir, "forecasts")) || !length(list.files(file.path(ctx$state_dir, "forecasts")))

## painel da folha A1 com todos os membros (o mesmo de m6f_prepare_origin), para o valor esperado
expected_a1 <- function(ctx, drop = character()) {
  mem <- env$prevcarga_members(path = ctx$topology_path, root = ctx$root)
  g <- list(origin = O, cuts = env$prevcarga_resolve_cuts(ctx, origin = O))
  rctx <- ctx; rctx$tz <- g$cuts$tz; rctx$load_label <- g$cuts$load_label
  lm <- stats::setNames(lapply(seq_len(nrow(mem)), function(i) {
    d <- m6f_delivery(env, mem[i, ], "A1", O, ctx)
    list(delivery = data.table::data.table(interval_end = d$predictions$target_datetime, y_pred = d$predictions$y_pred),
         member = list(name = mem$sigla[[i]], delivery_days = d$delivery_days))
  }), mem$sigla)
  panel <- env$.forecast_leaf_panel(rctx, lm, "A1", g, mem)$panel
  for (s in drop) panel[[paste0("f_", s)]] <- NA_real_
  art <- file.path(env$prevcarga_registry_root(ctx), "combina")
  w <- env$prevcarga_combina_apply("A1", art, panel, rctx, members = mem, detail = TRUE)$weights
  list(panel = panel, y = env$prevcarga_produce_combined(panel, w))
}

test_that("[AC1] membro LGBM ausente em A1: y_pred = produce_combined com f_LGBM NA; aviso, membros_ausentes e run_manifest sem LGBM/A1", {
  ctx <- fx_copy()
  r <- run_fc(ctx, fail_cells = "LGBM/A1", escopo = "area", areas = "A1")
  ex <- expected_a1(ctx, drop = "LGBM")
  got <- yp(r, "A1")
  expect_equal(sort(got$y_pred), sort(ex$y[!is.na(ex$y)]), tolerance = 1e-9)
  w <- Filter(function(x) identical(x$id, "warning.forecast_member_absent"), r$warnings)
  expect_length(w, 1L)
  expect_identical(w[[1L]]$details, list(folha = "A1", membro = "LGBM", error_id = "state.artifact_missing"))
  expect_identical(r$result$membros_ausentes, list(list(folha = "A1", membro = "LGBM", error_id = "state.artifact_missing")))
  man <- jsonlite::read_json(file.path(r$result$saida, "run_manifest.json"))
  expect_false("LGBM/A1" %in% names(man$model_versions))
  expect_true(all(c("RF/A1", "HW/A1") %in% names(man$model_versions)))
  ## CONTROL: sem falha o valor e o do painel completo e difere do renormalizado
  c1 <- run_fc(fx_copy(), escopo = "area", areas = "A1")
  full <- expected_a1(ctx)
  expect_equal(sort(yp(c1, "A1")$y_pred), sort(full$y[!is.na(full$y)]), tolerance = 1e-9)
  ## o sentinela do fixture tem media igual sem LGBM (offsets 10/20/30); sem RF a renormalizacao move o valor (prova a renormalizacao)
  c3ctx <- fx_copy()
  c3 <- run_fc(c3ctx, fail_cells = "RF/A1", escopo = "area", areas = "A1")
  ex3 <- expected_a1(c3ctx, drop = "RF")
  expect_equal(sort(yp(c3, "A1")$y_pred), sort(ex3$y[!is.na(ex3$y)]), tolerance = 1e-9)
  expect_false(isTRUE(all.equal(sort(yp(c3, "A1")$y_pred), sort(yp(c1, "A1")$y_pred))))
})

test_that("[AC2] folha B1 sem nenhum membro: sin e subsistema SS2 abortam exit 3 sem scratch nem forecasts/", {
  all_b1 <- paste0(M6F_MEMBERS, "/B1")
  for (sc in list(list(escopo = "sin"), list(escopo = "subsistema", subsistema = "SS2"))) {
    ctx <- fx_copy()
    cond <- catch_pc(do.call(run_fc, c(list(ctx, fail_cells = all_b1), sc)))
    expect_false(is.null(cond))
    if (!is.null(cond)) {
      expect_identical(cond$id, "completude.forecast_leaf_no_member")
      expect_identical(cond$exit_code, 3L)
      expect_identical(cond$where, "B1")
    }
    expect_true(no_scratch(ctx)); expect_true(no_forecasts(ctx))
  }
  ## CONTROL: so um membro de B1 falha => sin entrega
  c1 <- run_fc(fx_copy(), fail_cells = "RF/B1", escopo = "sin")
  expect_true("SIN.parquet" %in% vapply(c1$result$arquivos, function(a) a$path, ""))
})

test_that("[AC3] A2 sem membros em escopo area: A1 entregue, aviso de folha fora e exit 14 com result completo", {
  ctx <- fx_copy()
  cond <- catch_pc(run_fc(ctx, fail_cells = paste0(M6F_MEMBERS, "/A2"), escopo = "area", areas = c("A1", "A2")))
  expect_false(is.null(cond))
  if (!is.null(cond)) {
    expect_identical(cond$id, "partial.forecast_leaf_absent")
    expect_identical(cond$exit_code, 14L)
    res <- cond$result
    expect_identical(res$dropped, list("A2"))
    paths <- vapply(res$arquivos, function(a) a$path, "")
    expect_identical(paths, "folhas/A1.parquet")
    expect_true(file.exists(file.path(res$saida, "folhas", "A1.parquet")))
    expect_false(file.exists(file.path(res$saida, "folhas", "A2.parquet")))
    dr <- Filter(function(x) identical(x$id, "warning.forecast_leaf_dropped"), res$warnings)
    expect_length(dr, 1L); expect_identical(dr[[1L]]$where, "A2")
  }
  ## todas as folhas pedidas vazias => aborta exit 3
  c2 <- catch_pc(run_fc(fx_copy(), fail_cells = paste0(M6F_MEMBERS, "/A2"), escopo = "area", areas = "A2"))
  expect_identical(c2$id, "completude.forecast_leaf_no_member")
})

test_that("[AC4] id config./topology./honesty./runtime. nunca vira membro ausente: aborta com o id original sem scratch", {
  for (id in c("honesty.khat_after_origin", "config.usage", "topology.area_undeclared", "runtime.blas_threads")) {
    if (is.null(tryCatch(env$prevcarga_error_lookup(id), error = function(e) NULL))) next
    ctx <- fx_copy()
    cond <- catch_pc(run_fc(ctx, fail_cells = "LGBM/A1", fail_id = id, escopo = "sin"))
    expect_false(is.null(cond))
    if (!is.null(cond)) expect_identical(cond$id, id)
    expect_true(no_scratch(ctx))
  }
  ## CONTROL: state.* e tolerado
  expect_null(catch_pc(run_fc(fx_copy(), fail_cells = "LGBM/A1", fail_id = "state.artifact_missing", escopo = "sin")))
})

test_that("[AC5] CONTROL sem falha: nenhum aviso/ausente e entrega identica entre rodadas (versao e sha256)", {
  a <- run_fc(fx_copy(), escopo = "sin")
  b <- run_fc(fx_copy(), escopo = "sin")
  expect_length(a$result$membros_ausentes, 0L)
  expect_false(any(vapply(a$warnings, function(w) identical(w$id, "warning.forecast_member_absent"), NA)))
  expect_identical(a$result$versao, b$result$versao)
  expect_identical(vapply(a$result$arquivos, function(x) x$sha256, ""), vapply(b$result$arquivos, function(x) x$sha256, ""))
  ## FORBIDDEN: com um membro ausente a versao muda
  d <- run_fc(fx_copy(), fail_cells = "LGBM/A1", escopo = "sin")
  expect_false(identical(d$result$versao, a$result$versao))
})
