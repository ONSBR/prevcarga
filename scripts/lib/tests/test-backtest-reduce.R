## test-backtest-reduce.R -- M7.7 (REDUCE): combina honesta por mes, reconcile por origem com k-hat refeito em O, avaliacao com
## y_true fresco, registry de producao intocado (ADR-0023, ADR-0032, ADR-0040). In-process, plugins-brinquedo + deck sintetico
## (areas A, B e o subsistema X). Cada FORBIDDEN (delta 0) tem o seu CONTROL (delta != 0). Tudo em tempdir; sem env vars.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
lib <- file.path(pc_root(), "scripts", "lib")

fx <- new.env()
sys.source(file.path(lib, "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

BT_TZ <- "Etc/GMT+3"
BO1 <- lubridate::ymd("2026-10-05")
BO2 <- lubridate::ymd("2026-10-06")
BO3 <- lubridate::ymd("2026-10-07")
bt_at <- function(day, hm) lubridate::ymd_hm(paste(format(lubridate::as_date(day)), hm), tz = BT_TZ)

## --- brinquedos sensiveis: y_pred = nivel do artefato + ultima carga VISTA (hora cheia para 60 min) -------------------
SENS_30 <- c(
  "toy_inference_features <- function(frame, payload) list(area = frame$load$area_code[1L], last = utils::tail(frame$load$load_mwh, 1L))",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 2L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = payload$model$level[toy_slots(ie)] + inf_features$last))",
  "}")
SENS_60 <- c(
  "toy_inference_features <- function(frame, payload) list(area = frame$area, last = utils::tail(frame$load$load_mwh, 1L))",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 9L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = rep(payload$model$level, length(ie)) + inf_features$last))",
  "}")

toy_dir <- function(base, name, sens, run_yaml = list(), descriptor = list()) {
  pc_toy_variant(base, run_yaml = run_yaml, descriptor = descriptor, name = name,
                 files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines(base, 0L, 0L, character(), "ok"),
                              "R/zz_sens.R" = sens))
}

## toy-h: horario, origin_agnostic H=9 (RF); toy-s: semi-horario, origin_agnostic H=1, data_lag_days 2 (LGBM);
## toy-sc: horario, origin_scoped (HW). `proto`: backtest.protocol_default de cada um.
mk_root <- function(proto = c(h = "daily", s = "daily", sc = "daily")) {
  pc_install_copy(mutate = function(r) {
    tvs <- list(
      toy_dir("toy-60", "toy-h", SENS_60,
              run_yaml = list(split = "origin_agnostic", modes = c("train", "infer"), horizon_days = 9L,
                              backtest = list(protocol_default = proto[["h"]])),
              descriptor = function(d) { d$modes_implemented <- c("train", "infer"); d$stages$forecast <- NULL
                                         d$model_time_offset_min <- 0L; d }),
      toy_dir("toy-30", "toy-s", SENS_30, run_yaml = list(horizon_days = 1L, backtest = list(protocol_default = proto[["s"]])),
              descriptor = list(data_lag_days = 2L, uses_rng = TRUE)),
      toy_dir("toy-60", "toy-sc", SENS_60, run_yaml = list(backtest = list(protocol_default = proto[["sc"]])),
              descriptor = list(model_time_offset_min = 0L)))
    for (tv in tvs) for (n in setdiff(list.files(tv), "_toy-engine"))
      file.copy(file.path(tv, n), file.path(r, "models"), recursive = TRUE)
    file.copy(file.path(tvs[[1L]], "_toy-engine"), file.path(r, "models"), recursive = TRUE)
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [A, B], loss: PX}",
                 "order: [X]", "members:",
                 "  - {sigla: MH, mdir: toy-h, exogena: null}",
                 "  - {sigla: MS, mdir: toy-s, exogena: null}",
                 "  - {sigla: MC, mdir: toy-sc, exogena: null}"),
               file.path(r, "config", "topology.yaml"))
  })
}
ROOT <- mk_root()

DECK <- local({
  d <- tempfile("bt-deck-")
  fx$prevcarga_fixture_deck(d, seed = 5, areas = c("A", "B", "X"), days = seq(BO1 - 30L, BO3, by = "day"))
  d
})

mod <- new.env()
for (m in c("errors", "runtime", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
            "features", "registry", "training", "inference", "build_long_output", "evaluation", "honesty_gate", "combination", "reconcile", "cache",
            "orchestrate", "backtest"))
  sys.source(file.path(lib, paste0(m, ".R")), envir = mod)
suppressMessages(mod$prevcarga_runtime_enforce())

bctx <- function(root = ROOT, data = DECK, .frame = parent.frame()) {
  state <- withr::local_tempdir("bt-state-", .local_envir = .frame)
  ctx <- mod$prevcarga_ctx(list("--data" = data, "--state" = state), root, "backtest")
  ctx$code_commit <- "c0ffee"
  ctx$data_hash <- "deck-fixed"                          # chaves iguais entre decks perturbados (so o dado muda)
  ctx$progress <- function(done, total) NULL
  ctx
}

## troca binding do env do modulo e restaura ao fim do teste
mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- mod[[nm]]; withr::defer(assign(nm, old, envir = mod), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = mod)
}
count_trains <- function(.frame = parent.frame()) {
  cnt <- new.env(); cnt$n <- 0L
  orig <- mod$prevcarga_plugin_train
  mock(prevcarga_plugin_train = function(...) { cnt$n <- cnt$n + 1L; orig(...) }, .frame = .frame)
  cnt
}

run_bt <- function(ctx, origins = BO1, models = NULL, areas = "A", cortes = "08:00", ...)
  mod$prevcarga_backtest(ctx, origins = origins, models = models, areas = areas, cortes = cortes, ...)

cell_long <- function(res, membro, origem, corte, folha = "A") {
  p <- res$paths$members
  f <- p$long_parquet[p$membro == membro & p$folha == folha & p$origem == origem & p$corte == corte]
  data.table::as.data.table(arrow::read_parquet(f))
}
artifact_shas <- function(ctx) {
  ms <- Sys.glob(file.path(ctx$state_dir, "cache", "artifact", "*", "*", "meta.json"))
  stats::setNames(vapply(ms, function(f) jsonlite::read_json(f)$sha256, ""), basename(dirname(ms)))
}
perturb <- function(...) fx$prevcarga_fixture_perturb(DECK, channel = "load", areas = "A", ...)

expect_pc <- function(expr, id, exit = 2L) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (!is.null(cond)) { expect_identical(cond$id, id); expect_identical(cond$exit_code, as.integer(exit)) }
  invisible(cond)
}


## --- apoio do REDUCE -----------------------------------------------------------------------------------------------------
bt_read <- function(res, ...) data.table::as.data.table(arrow::read_parquet(file.path(res$paths$root, ...)))
read_comb <- function(res, folha = "A", corte = "0800") bt_read(res, "combined", folha, corte, "combined.parquet")
ymd_ <- function(x) lubridate::ymd(x)

## injeta falha em celulas (a falha e classificada como a de qualquer celula) sem tocar o codigo de producao
fail_cells <- function(pred, .frame = parent.frame()) {
  orig <- mod$.bt_make_run_cell
  mock(.bt_make_run_cell = function(ctx, plan) {
    f <- orig(ctx, plan)
    function(cell) {
      if (pred(cell)) mod$prevcarga_abort("contract.backtest_long_na", where = "injetada")
      f(cell)
    }
  }, .frame = .frame)
}

## perturba SO a verdade fresca do REDUCE (leitor de carga realizada): sel(interval_end) -> logico. O MAP nao a usa.
with_truth <- function(sel, expr_fn, drop = FALSE) {
  orig <- mod$prevcarga_read_actual
  mock(prevcarga_read_actual = function(ctx, area, from = NULL, to = NULL) {
    out <- orig(ctx, area, from, to)
    hit <- sel(out$interval_end)
    if (drop) out <- out[!hit, , drop = FALSE] else out$y_true[hit] <- out$y_true[hit] * 1.3 + 50
    out
  })
  expr_fn()
}

run_bt <- local({ f <- run_bt; function(...) suppressWarnings(f(...)) })       # avisos de celula nao estacionaria do M4 nao sao o foco aqui

OS_M <- seq(ymd_("2026-09-28"), ymd_("2026-10-02"), by = "day")
O_M <- ymd_("2026-10-01")

test_that("[AC2] a combina do backtest e honesta por mes: FORBIDDEN y_true em target > inicio(m), CONTROL em == inicio(m) e em O-1", {
  ctx <- bctx(); ctx$combina <- list(n_min = 3L)
  res <- run_bt(ctx, origins = OS_M, areas = "A", baseline = "none")
  expect_identical(res$summary$verdict, "ok")
  yp <- function(sel) {
    out <- with_truth(sel, function() run_bt(ctx, origins = OS_M, areas = "A", baseline = "none", resume = TRUE))
    d <- read_comb(out)
    d[d$forecast_origin == "2026-10-01", ]$y_pred
  }
  base <- read_comb(res)[forecast_origin == "2026-10-01", ]$y_pred
  expect_gt(length(base), 0L)
  m0 <- bt_at(O_M, "00:00")
  expect_identical(yp(function(ie) rep(FALSE, length(ie))), base)
  expect_lte(max(abs(yp(function(ie) ie > m0) - base)), 1e-5)                                       # FORBIDDEN
  expect_gt(max(abs(yp(function(ie) ie == m0) - base)), 1e-5)                                       # CONTROL: ultimo intervalo de m-1
  expect_gt(max(abs(yp(function(ie) ie > bt_at(O_M - 1L, "00:00") & ie < m0) - base)), 1e-5)        # CONTROL: O-1
  ## meses sem n_min caem no fallback do M4 e saem carimbados no resumo
  fb <- res$summary$fallback_months
  expect_true("2026-09" %in% fb$mes[fb$folha == "A" & fb$corte_hm == "08:00"])
  ## artefatos: combined + pesos por mes, atomicos (nenhum .tmp)
  expect_true(file.exists(file.path(res$paths$root, "combined", "A", "0800", "combined.parquet")))
  expect_true(file.exists(file.path(res$paths$root, "weights", "2026-10", "A_0800.rds")))
  expect_length(list.files(res$paths$root, pattern = "\\.tmp$", recursive = TRUE, all.files = TRUE), 0L)
})

test_that("[AC2] greps: o REDUCE nunca usa pesos de producao, registry nem redefine mascara ou wmape", {
  src <- readLines(file.path(lib, "backtest.R"))
  code <- paste(vapply(src, function(l) sub("#.*$", "", l), ""), collapse = "\n")
  for (pat in c("combina_apply", "latest\\.txt", "khat_resolve", "khat_promote", "registry_promote", "registry_resolve_latest",
                "wmape <- function", "target <", "ym <"))
    expect_false(grepl(pat, code), info = pat)
  expect_true(grepl("prevcarga_combina_full", code, fixed = TRUE))
})

BR <- c(BO1, BO2)

test_that("[AC1] keep-going: folha faltando => incompleto sem SIN; membro faltando => renormaliza e emite SIN; CONTROL completo", {
  ctx <- bctx()
  fail_cells(function(cell) (cell$folha == "B" && cell$origem == BO1) || (cell$membro == "MC" && cell$origem == BO2))
  res <- run_bt(ctx, origins = BR, areas = NULL, keep_going = TRUE, baseline = "none")
  inc <- res$summary$incompleto
  expect_identical(nrow(inc), 1L)
  expect_identical(inc$origem[[1L]], BO1)
  expect_identical(inc$folhas_faltando[[1L]], "B")
  expect_identical(res$summary$verdict, "partial")
  ## FORBIDDEN: nenhum SIN/subsistema parcial de O1 em reconciled/
  expect_false(dir.exists(file.path(res$paths$reconciled, "2026-10-05")))
  sin <- bt_read(res, "reconciled", "2026-10-06", "0800", "SIN.parquet")
  expect_identical(unique(sin$forecast_origin), "2026-10-06")
  expect_gt(nrow(sin), 0L)
  expect_true(file.exists(file.path(res$paths$reconciled, "2026-10-06", "0800", "subsistemas", "X.parquet")))
  ## O2: combina renormalizada sobre os membros presentes (EQW de MH e MS onde ha n < n_min)
  p <- res$paths$members
  mem <- function(s) { d <- data.table::as.data.table(arrow::read_parquet(p$long_parquet[p$membro == s & p$folha == "A" & p$origem == BO2]))
                       stats::setNames(d$y_pred, as.numeric(d$target_datetime)) }
  mh <- mem("MH"); ms <- mem("MS")
  cb <- read_comb(res)[forecast_origin == "2026-10-06", ]
  key <- as.character(as.numeric(cb$target_datetime))
  want <- vapply(key, function(k) mean(c(mh[k], ms[k]), na.rm = TRUE), 0)
  expect_equal(cb$y_pred, unname(want))
})

test_that("[AC1] CONTROL: cobertura completa => ok, nenhum incompleto e SIN em todas as origens", {
  ctx2 <- bctx()
  ok <- run_bt(ctx2, origins = BR, areas = NULL, keep_going = TRUE, baseline = "none")
  expect_identical(ok$summary$verdict, "ok")
  expect_identical(nrow(ok$summary$incompleto), 0L)
  expect_setequal(unique(bt_read(ok, "reconciled", "2026-10-05", "0800", "SIN.parquet")$forecast_origin), "2026-10-05")
  expect_true(file.exists(file.path(ok$paths$reconciled, "2026-10-06", "0800", "SIN.parquet")))
})

test_that("[AC1] verbo: origem incompleta => exit 14 partial.backtest_delivery com o incompleto no result; completo => ok", {
  ctx <- bctx(); ctx$origin <- BR; ctx$format <- "json"
  fail_cells(function(cell) cell$folha == "B" && cell$origem == BO1)
  args <- list("--keep-going" = TRUE, "--corte" = "08:00", "--baseline" = "none")
  cond <- expect_pc(mod$prevcarga_backtest_run(ctx, args), "partial.backtest_delivery", 14L)
  inc <- cond$result$summary$incompleto
  expect_length(inc, 1L)
  expect_identical(inc[[1L]]$origem, "2026-10-05")
  expect_identical(unlist(inc[[1L]]$folhas_faltando), "B")
})

test_that("[AC3] k-hat por origem: janela [O-W, O-1] inclusiva; FORBIDDEN >= O e O-W-1 delta 0, CONTROL O-1 e O-W delta != 0; latest intocado", {
  ctx <- bctx(); rctx <- mod$.bt_rctx(ctx, list(tz = BT_TZ, load_label = "start"))
  O <- ymd_("2026-10-01"); W <- 10L
  topo <- mod$prevcarga_topology(root = ctx$root)
  truth <- data.table::as.data.table(mod$prevcarga_read_load(rctx, areas = c("A", "B", "X"), from = O - W - 3L, to = O + 3L))
  days <- seq(O - W, O - 1L, by = "day")
  bu <- data.frame(area = "X", forecast_origin = format(O), target_datetime = lubridate::ymd_hm(paste(format(days), "12:30"), tz = BT_TZ),
                   lead = 0L, BU_pred = 100, d_plus = 0L, granularity_min = 30L)
  fn <- function(tr) data.table::as.data.table(mod$.bt_khat_apply(bu, tr, "X", topo, O, W, rctx))[, c("target_datetime", "y_pred"), with = FALSE]
  r <- mod$prevcarga_reach_control(fn, truth, mod$prevcarga_khat_reach_arms(O, W, "X"))
  expect_identical(r$veredito, "HONESTO")
  expect_setequal(r$deltas$arm, c("futuro", "antes_da_janela", "ultimo_dia", "primeiro_dia"))
  expect_true(all(r$deltas$delta[r$deltas$kind == "forbidden"] == 0))
  expect_true(all(r$deltas$delta[r$deltas$kind == "control"] > 1e-5))
  ## latest.txt de producao (treinado depois da janela) nao e lido nem tocado
  st <- ctx$state_dir; key <- file.path(st, "artifacts", "reconcile", "X")
  mod$prevcarga_registry_publish(list(x = 1), list(note = "v_a1"), key, "v_a1")
  mod$prevcarga_registry_promote_latest(key, "v_a1")
  sha <- unname(tools::md5sum(file.path(key, "latest.txt")))
  seen <- new.env(); seen$n <- 0L
  spy <- function(nm) { f <- get(nm, envir = baseenv())
    assign(nm, function(con, ...) { if (is.character(con) && any(grepl("/artifacts/", con, fixed = TRUE))) seen$n <- seen$n + 1L; f(con, ...) }, envir = mod) }
  for (nm in c("readLines", "file", "readRDS")) { spy(nm); local({ n <- nm; withr::defer(rm(list = n, envir = mod)) }) }
  res <- run_bt(ctx, origins = BR, areas = NULL, baseline = "none")
  expect_identical(res$summary$verdict, "ok")
  expect_identical(seen$n, 0L)
  expect_identical(unname(tools::md5sum(file.path(key, "latest.txt"))), sha)
})

test_that("[AC4] o backtest treina o proprio artefato: latest de modelo intocado, cache/ e backtest/ estaveis ao registry gc; CONTROL promote move", {
  ctx <- bctx()
  key <- file.path(ctx$state_dir, "artifacts", "lgbm", "F")
  for (v in c("v_a1", "v_b1")) mod$prevcarga_registry_publish(list(x = v), list(note = v), key, v)
  mod$prevcarga_registry_promote_latest(key, "v_a1")
  writeLines("v_b1", file.path(key, ".pin"))                                       # o gc poupa o pino (alvo do CONTROL)
  lat <- function() readLines(file.path(key, "latest.txt"))
  shas <- function() {
    fs <- sort(unlist(lapply(c("cache", "backtest"), function(d) list.files(file.path(ctx$state_dir, d), recursive = TRUE, full.names = TRUE))),
               method = "radix")
    stats::setNames(vapply(fs, function(f) digest::digest(file = f, algo = "sha256"), ""), sub(ctx$state_dir, "", fs, fixed = TRUE))
  }
  res <- run_bt(ctx, origins = BR, areas = NULL, baseline = "none")
  expect_identical(res$summary$verdict, "ok")
  expect_identical(lat(), "v_a1")
  expect_gt(length(Sys.glob(file.path(ctx$state_dir, "cache", "artifact", "*", "*", "meta.json"))), 0L)
  before <- shas()
  g <- mod$prevcarga_registry_gc(list(state_dir = ctx$state_dir, root = ctx$root))
  expect_identical(shas(), before)
  expect_identical(lat(), "v_a1")
  expect_false(any(grepl("lgbm/F/v_a1", g$removed, fixed = TRUE)))
  mod$prevcarga_registry_promote(key, "v_b1", ctx = list(state_dir = ctx$state_dir, root = ctx$root))      # CONTROL: a checagem esta viva
  expect_identical(lat(), "v_b1")
})

test_that("[AC5] WMAPE do backtest == prevcarga score; verdade perturbada move os dois; pico por interval_start; composicao viva", {
  ctx <- bctx(); fn <- function() run_bt(ctx, origins = BR, areas = NULL, baseline = "persistence", resume = TRUE)
  res <- fn()
  sc <- res$scores
  expect_true(all(c("member", "combined", "sin", "peak_fixed_18_22", "sin_fresh", "awaiting_truth") %in% names(sc)))
  expect_true("skill_persistencia_O1" %in% names(sc$sin))
  expect_true(all(is.na(sc$sin_fresh$sum_check) | sc$sin_fresh$sum_check <= 1e-5))
  rctx <- mod$.bt_rctx(ctx, list(tz = BT_TZ, load_label = "start"))
  score_x <- function(res) {
    rd <- function(o) mod$prevcarga_read_long(file.path(res$paths$reconciled, o, "0800", "subsistemas", "X.parquet"), rctx)
    s <- mod$prevcarga_score(rbind(rd("2026-10-05"), rd("2026-10-06")), ctx, area = "X")$scorecard
    s[order(s$d_plus), ]
  }
  mine <- function(sc) { x <- sc$sin[sc$sin$area == "X", ]
    x <- stats::aggregate(cbind(w = wmape * n, n = n) ~ d_plus, data = x, FUN = sum); x$w / x$n }
  s0 <- score_x(res)
  x0 <- sc$sin[sc$sin$area == "X", ]; x0 <- x0[order(x0$d_plus), ]
  expect_equal(mine(sc), s0$wmape)                      # 1 corte: o agrupamento por corte nao muda o valor
  expect_equal(x0$peak, s0$peak)
  ## CONTROL: a verdade perturbada move os dois igualmente
  pert <- function(ie) rep(TRUE, length(ie))
  both <- with_truth(pert, function() { r <- fn(); list(res = r, score = score_x(r)) })
  res2 <- both$res; s2 <- both$score; x2 <- res2$scores$sin[res2$scores$sin$area == "X", ]; x2 <- x2[order(x2$d_plus), ]
  expect_equal(x2$wmape, s2$wmape)
  expect_gt(max(abs(s2$wmape - s0$wmape)), 1e-6)
  ## CONTROL: pico por interval_end (e nao interval_start) da outro valor
  long <- as.data.frame(mod$.bt_join_truth(mod$.bt_rctx(ctx, list(tz = BT_TZ, load_label = "start")),
                                           bt_read(res, "reconciled", "2026-10-06", "0800", "subsistemas", "X.parquet")))
  long$interval_start <- mod$prevcarga_long_interval_start(long$target_datetime, long$granularity_min, BT_TZ)
  by_start <- mod$prevcarga_peak_wmape(long$interval_start, long$y_true, long$y_pred)
  by_end <- mod$prevcarga_peak_wmape(long$target_datetime, long$y_true, long$y_pred)
  expect_false(isTRUE(all.equal(by_start, by_end)))
  ## composicao viva: perturbar y_pred de um membro numa origem muda o WMAPE do SIN
  orig <- mod$prevcarga_read_long
  mock(prevcarga_read_long = function(path, ctx) { d <- orig(path, ctx)
    if (grepl("/MH/A/2026-10-06/", path, fixed = TRUE)) d$y_pred <- d$y_pred * 1.5
    d })
  res3 <- fn()
  expect_false(isTRUE(all.equal(res3$scores$sin_fresh$wmape, sc$sin_fresh$wmape)))
})

test_that("[AC6] y_true fresco do leitor: sentinela no long do membro nao aparece em scores; origem sem verdade => awaiting e NA", {
  ctx <- bctx()
  res <- run_bt(ctx, origins = c(BO1, BO3), areas = NULL, baseline = "none")
  sc0 <- res$scores
  p <- res$paths$members
  f <- p$long_parquet[p$membro == "MH" & p$folha == "A" & p$origem == BO1]
  d <- arrow::read_parquet(f); d$y_true <- 1e9; arrow::write_parquet(d, f)
  res2 <- run_bt(ctx, origins = c(BO1, BO3), areas = NULL, baseline = "none", resume = TRUE)
  expect_identical(res2$scores$member, sc0$member)
  expect_identical(res2$scores$sin, sc0$sin)
  expect_lt(max(res2$scores$member$wmape, na.rm = TRUE), 1e6)
  ## verdade so ate o fim de O1: a origem O3 fica aguardando verdade, sem 0 nem metrica inventada
  res3 <- with_truth(function(ie) ie > bt_at(BO1 + 1L, "00:00"), function() run_bt(ctx, origins = c(BO1, BO3), areas = NULL,
                                                                                  baseline = "none", resume = TRUE), drop = TRUE)
  aw <- res3$scores$awaiting_truth
  expect_identical(aw$awaiting[aw$origem == BO3], TRUE)
  expect_identical(aw$awaiting[aw$origem == BO1], FALSE)
  expect_true(file.exists(file.path(res3$paths$scores, "scores.json")))
  expect_true(file.exists(file.path(res3$paths$scores, "scores.rds")))
})
