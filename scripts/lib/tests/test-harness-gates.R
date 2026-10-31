## test-harness-gates.R -- pack de gates de harness do M7 (M7.5): scripts/tools/gates/m7_harness.R (gate08/gate12 do RF, gate10 da HW,
## sem_crash do dessem). O HP vem de prevcarga_backtest() sobre um deck de fixture, com os plugins REAIS num `Rscript --vanilla` FILHO
## (a lib no globalenv, como a CLI; o interino dos plugins busca a fundacao la). Cada gate passa no HP real e reprova no HP sabotado
## (CONTROL com dente). Tudo em tempdir; sem variaveis de ambiente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
M7T_SRC <- file.path(pc_root(), "scripts", "tools", "gates", "m7_harness.R")
M7T_ENV <- new.env(parent = globalenv())
sys.source(M7T_SRC, envir = M7T_ENV)

## --- sabotagens do HP (puras: copiam os long.parquet e alteram a copia; rodam no filho) -----------------------------------------
sab_map <- function(hp, fn, which = seq_len(nrow(hp$cells))) {
  out <- hp
  out$cells <- data.table::copy(hp$cells)
  dst <- tempfile("sab-"); dir.create(dst)
  for (i in seq_len(nrow(hp$cells))) {
    d <- data.table::as.data.table(arrow::read_parquet(hp$cells$long_parquet[[i]]))
    if (i %in% which) d <- fn(d)
    f <- file.path(dst, sprintf("c%d.parquet", i))
    arrow::write_parquet(as.data.frame(d), f)
    data.table::set(out$cells, i = i, j = "long_parquet", value = f)
  }
  out
}
sab_shift <- function(d) { d$target_datetime <- d$target_datetime + 1800; d }
sab_trunc <- function(d) d[-nrow(d), ]
sab_date <- function(d) { d$forecast_origin <- as.Date(d$forecast_origin); d }
sab_perturb <- function(d) { d$y_pred <- d$y_pred + 1; d }

## --- cenario do filho -------------------------------------------------------------------------------------------------------------
m7t_scenario <- function(root, model, out) {
  pl <- M7H_PLAN[[model]]
  work <- tempfile("m7t-"); dir.create(work)
  last <- lubridate::ymd(pl$last)
  deck <- prevcarga_m7h_deck(file.path(work, "deck"), last, pl$deck_days)
  ctx <- prevcarga_m7h_ctx(root, deck, file.path(work, "state"), pl$overrides)
  origins <- last - (pl$n_origins - 1L):0
  hp <- prevcarga_harness_hp(ctx, model, origins, M7H_CORTE, protocol = pl$protocol, areas = M7H_AREA, models = pl$models)
  R <- list(n_cells = nrow(hp$cells), n_origins = length(unique(hp$cells$origem)), delivery_days = hp$spec$delivery_days,
            verdict = hp$summary$verdict)
  if (identical(model, "random-forest")) {
    R$g08 <- prevcarga_gate_ytrue_real(hp)
    R$g12 <- prevcarga_gate_harness_long(hp)
    R$g12_shift <- prevcarga_gate_harness_long(sab_map(hp, sab_shift))
    R$g12_trunc <- prevcarga_gate_harness_long(sab_map(hp, sab_trunc))
    R$g12_date <- prevcarga_gate_harness_long(sab_map(hp, sab_date))
    R$fo_class <- class(arrow::read_parquet(hp$cells$long_parquet[[1L]])$forecast_origin)
    R$n_rows <- nrow(arrow::read_parquet(hp$cells$long_parquet[[1L]]))
    R$grid_n <- length(prevcarga_delivery_grid(list(origin_day = hp$cells$origem[[1L]], available_until = hp$cells$available_until[[1L]],
                                                    tz = hp$tz), hp$spec$delivery_days))
    ## y_true: deck cuja carga acaba antes da 1a origem => a verdade fresca re-juntada pelo leitor e toda NA
    deck0 <- prevcarga_m7h_deck(file.path(work, "deck0"), last, pl$deck_days, load_until = lubridate::ymd_hm("2025-02-01 00:00", tz = M7H_TZ))
    hp0 <- hp
    hp0$rctx$data_dir <- deck0
    R$g08_na <- prevcarga_gate_ytrue_real(hp0)
  }
  if (identical(model, "holt-winters")) {
    cnt <- new.env(); cnt$n <- 0L
    orig <- prevcarga_plugin_forecast
    assign("prevcarga_plugin_forecast", function(...) { cnt$n <- cnt$n + 1L; orig(...) }, envir = globalenv())
    R$g10 <- prevcarga_gate_harness_parity(hp)
    R$calls <- cnt$n
    cnt$n <- 0L
    R$g10_perturbed <- prevcarga_gate_harness_parity(sab_map(hp, sab_perturb, which = 2L))
    R$calls_perturbed <- cnt$n
    assign("prevcarga_plugin_forecast", orig, envir = globalenv())
  }
  jsonlite::write_json(R, out, auto_unbox = TRUE, null = "null", digits = NA)
  invisible(R)
}

m7t_run_child <- function(model, work) {
  helpers <- file.path(work, "helpers.R")
  dump(c("sab_map", "sab_shift", "sab_trunc", "sab_date", "sab_perturb", "m7t_scenario"), file = helpers, envir = environment(sab_map))
  lines <- c(head(M7T_ENV$.m7h_child_lines(), -1L), "source(a[[5L]])", "m7t_scenario(root, model, out)")
  script <- file.path(work, paste0("child-", model, ".R"))
  out <- file.path(work, paste0("out-", model, ".json"))
  writeLines(lines, script)
  log <- suppressWarnings(system2(pc_rscript_bin(), c("--vanilla", shQuote(script), shQuote(pc_root()), shQuote(model), shQuote(out),
                                                       shQuote(M7T_SRC), shQuote(helpers)), stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(log, "status"))) 0L else as.integer(attr(log, "status")), log = log,
       json = if (file.exists(out)) jsonlite::fromJSON(out, simplifyVector = FALSE) else NULL)
}

WORK <- withr::local_tempdir("m7t-work-")
RF <- m7t_run_child("random-forest", WORK)
HW <- m7t_run_child("holt-winters", WORK)
info <- function(r) paste(utils::tail(r$log, 15L), collapse = "\n")

## ---------------------------------------------------------------------------------------------------------------------------------
test_that("[AC1] gate08 e gate12 passam no HP real do RF (backtest sobre o deck de fixture, varias origens)", {
  expect_false(is.null(RF$json), info = info(RF))
  j <- RF$json
  expect_gte(j$n_origins, 2L)
  expect_true(j$g08$pass, info = j$g08$detail)
  expect_true(j$g12$pass, info = j$g12$detail)
  expect_identical(j$g08$id, "gate08_ytrue_real")
  expect_identical(j$g12$id, "gate12_harness_long")
  expect_identical(j$fo_class, "character")                        # forecast_origin e TEXTO yyyy-MM-dd (ADR-0032)
  expect_identical(j$n_rows, j$grid_n)                             # nrow = grade de prevcarga_delivery_grid, nunca literal
})

test_that("[AC1] gate10 passa no HP real da HW (max|delta| < 1e-6 contra o forecast acoplado single-origin)", {
  expect_false(is.null(HW$json), info = info(HW))
  j <- HW$json
  expect_true(j$g10$pass, info = j$g10$detail)
  expect_identical(j$g10$id, "gate10_harness_parity")
  expect_gte(j$n_origins, 2L)
})

test_that("[AC2] CONTROL gate12: HP deslocado de 1 intervalo (30 min), truncado e com forecast_origin Date reprovam", {
  j <- RF$json
  expect_false(j$g12_shift$pass)
  expect_false(j$g12_trunc$pass)
  expect_false(j$g12_date$pass)
  expect_match(j$g12_shift$detail, "grade")
  expect_match(j$g12_trunc$detail, "nrow")
})

test_that("[AC2] CONTROL gate08: verdade fresca toda NA (carga do deck acaba antes das origens) reprova", {
  expect_false(RF$json$g08_na$pass)
})

test_that("[AC2] CONTROL gate10: uma origem perturbada da max|delta| > 1e-6 e reprova", {
  j <- HW$json$g10_perturbed
  expect_false(j$pass)
  d <- as.numeric(sub("^max\\|delta\\| = ([0-9.e+-]+) .*$", "\\1", j$detail))
  expect_gt(d, 1e-6)
})

test_that("[AC2] FORBIDDEN gate10 via 2o laco do backtest: spy no despachante == 1 chamada por origem", {
  expect_identical(HW$json$calls, HW$json$n_cells)
  expect_identical(HW$json$calls_perturbed, HW$json$n_cells)
})

## --- pack: descoberta, applies_to, mapa dos slots ----------------------------------------------------------------------------------
plg <- new.env(parent = globalenv())
sys.source(file.path(pc_root(), "scripts", "lib", "errors.R"), envir = plg)
sys.source(file.path(pc_root(), "scripts", "tools", "plugin_gates.R"), envir = plg)
GATES_DIR <- file.path(pc_root(), "scripts", "tools", "gates")
pack_rows <- function(model, tier = "fast", dir = GATES_DIR) {
  rows <- plg$.pc_gates_packs(model, list(model = model, tier = tier, gates_dir = dir), tier, dir)
  Filter(function(r) identical(r$source, "m7_harness.R"), rows)
}

test_that("[AC1] pack: gate08/gate12 so no random-forest, gate10 so na holt-winters, sem skip; owner M7, tier fast", {
  expect_identical(vapply(pack_rows("random-forest"), `[[`, "", "gate"), c("gate08_ytrue_real", "gate12_harness_long"))
  expect_identical(vapply(pack_rows("holt-winters"), `[[`, "", "gate"), "gate10_harness_parity")
  expect_length(pack_rows("lgbm"), 0L)
  for (m in c("random-forest", "holt-winters", "dessem-linear", "dessem-regdin"))
    for (r in pack_rows(m)) {
      expect_identical(r$owner, "M7"); expect_identical(r$tier_min, "fast"); expect_identical(r$applies_to, m)
      expect_true(is.function(r$test))
    }
  expect_length(pack_rows("random-forest", tier = "parity"), 0L)
})

test_that("[AC1] CONTROL: sem scripts/tools/gates/m7_harness.R os ids somem da tabela do tier fast", {
  empty <- withr::local_tempdir("m7t-nogates-")
  for (m in c("random-forest", "holt-winters")) {
    expect_gt(length(pack_rows(m)), 0L)
    expect_length(pack_rows(m, dir = empty), 0L)
  }
})

test_that("[AC1] mapa dos slots [DEFERRED M7]: todo slot das tabelas dos runners vira gate do pack (nada some em silencio)", {
  slots <- character()
  for (f in Sys.glob(file.path(pc_root(), "models", "*", "tests", "run_all_gates.R"))) {
    m <- basename(dirname(dirname(f)))
    txt <- paste(readLines(f, warn = FALSE), collapse = " ")
    ids <- regmatches(txt, gregexpr("list\\(\"[a-z0-9_]+\", \"[^\"]*\", \"DEFERRED M7\"", txt))[[1L]]
    ids <- sub("^list\\(\"([a-z0-9_]+)\".*$", "\\1", ids)
    for (id in ids) slots[[paste(m, id, sep = ":")]] <- m
  }
  expect_gt(length(slots), 0L)
  for (k in names(slots)) {
    m <- slots[[k]]
    mapped <- M7T_ENV$M7H_SLOT_MAP[[k]]
    expect_false(is.null(mapped), info = paste("slot sem destino:", k))
    expect_true(mapped %in% M7T_ENV$M7H_PLAN[[m]]$gates, info = k)
    expect_true(mapped %in% vapply(pack_rows(m), `[[`, "", "gate"), info = k)
  }
  expect_setequal(names(M7T_ENV$M7H_SLOT_MAP), names(slots))
})

test_that("[AC1] o pack substitui o slot [DEFERRED M7] do runner (RF gate12, HW harness_parity)", {
  for (case in list(list("random-forest", "gate12", "gate12_harness_long"), list("holt-winters", "harness_parity", "gate10_harness_parity"))) {
    rows <- c(list(plg$.pc_gates_row(case[[2L]], "DEFERRED M7", "fast", source = "gate_table")), pack_rows(case[[1L]]))
    tests <- data.frame(test = sprintf("[DEFERRED M7] %s: x", case[[2L]]), failed = 0L, skipped = TRUE, error = FALSE, msg = "s",
                        stringsAsFactors = FALSE)
    sb <- plg$.pc_gates_substitute(rows, tests)
    expect_false(case[[2L]] %in% vapply(sb$rows, `[[`, "", "gate"))
    expect_true(case[[3L]] %in% vapply(sb$rows, `[[`, "", "gate"))
    expect_equal(nrow(sb$tests), 0L)
  }
})

## --- AC3: proveniencia do HP --------------------------------------------------------------------------------------------------------
test_that("[AC3] nenhum run_all_gates.R cita walk_forward nem caminho fixo (/tmp, results/); o pack nao tem literal de nrow", {
  for (f in Sys.glob(file.path(pc_root(), "models", "*", "tests", "run_all_gates.R"))) {
    txt <- readLines(f, warn = FALSE)
    expect_false(any(grepl("walk_forward|/tmp|results/", txt)), info = f)
  }
  src <- readLines(M7T_SRC, warn = FALSE)
  code <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("walk_forward|/tmp|results/", code)))
  expect_false(any(grepl("\\b(432|216|48)\\b", code)))
})

test_that("[AC3] os runners de RF e HW continuam so chamando prevcarga_gates_run (nenhum edita o pack)", {
  for (m in c("random-forest", "holt-winters")) {
    txt <- readLines(file.path(pc_root(), "models", m, "tests", "run_all_gates.R"), warn = FALSE)
    expect_true(any(grepl("prevcarga_gates_run\\(", txt)))
    expect_false(any(grepl("m7_harness|prevcarga_harness_hp|prevcarga_backtest", txt)))
  }
})
