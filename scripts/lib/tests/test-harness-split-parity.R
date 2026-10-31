## test-harness-split-parity.R -- M7.V: paridade do split (reescrita testthat portavel de jobs/tests/test_split_parity.sh e
## test_reduce_split_parity.sh do oraculo; ADR-0023, sem bash/Docker/S3). Tres bracos, todos com y_pred delta 0 (identical):
##   (a) 1 treino `fixed` + K infer == `daily` com o MESMO train_origin (membro origin_agnostic);
##   (b) infer do artefato origin_scoped (`daily`) == forecast acoplado do dia O (`retrain_harness`);
##   (c) rodada operacional (combina_train/apply + reconcile_train/apply sobre o painel do backtest) == REDUCE `full` de uma origem.
## CONTROL de cada braco: o que muda o dado/artefato/pesos congelados da delta != 0 (ou aborta por integridade).
library(testthat)

SP_ROOT <- pc_m7h_root()
SP_DECK <- pc_m7h_deck(lubridate::ymd("2026-08-25"), lubridate::ymd("2026-10-07"))
sp_ymd <- function(x) lubridate::ymd(x)
SP_TZ <- "Etc/GMT+3"
fx <- new.env()
sys.source(file.path(pc_root(), "scripts", "lib", "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

sp_mod <- pc_m7h_mod()
sp_run <- function(ctx, origins, models, areas = "A", protocol = NULL, ...)
  suppressWarnings(sp_mod$prevcarga_backtest(ctx, origins = origins, models = models, areas = areas, cortes = "08:00",
                                              protocol = protocol, baseline = "none", ...))
sp_cell <- function(res, membro, origem, folha = "A") {
  p <- res$paths$members
  f <- p$long_parquet[p$membro == membro & p$folha == folha & p$origem == origem & p$corte == "08:00"]
  d <- data.table::as.data.table(arrow::read_parquet(f))
  d[order(d$target_datetime), ]
}
sp_count_trains <- function(.frame = parent.frame()) {
  cnt <- new.env(); cnt$n <- 0L
  orig <- sp_mod$prevcarga_plugin_train
  pc_m7h_mock(prevcarga_plugin_train = function(...) { cnt$n <- cnt$n + 1L; orig(...) }, .local_envir = .frame)
  cnt
}
sp_perturb <- function(after, at_or_before) {
  fx$prevcarga_fixture_perturb(SP_DECK, channel = "load", areas = "A", after = after, at_or_before = at_or_before)
}

## ---------------------------------------------------------------------------------------------------------------------
test_that("[AC2a] FORBIDDEN: 1 treino `fixed` == `daily` com o mesmo train_origin (y_pred delta 0); CONTROL: treino de outra origem difere", {
  os <- sp_ymd("2026-10-05") + 0:2
  cf <- pc_m7h_ctx(SP_ROOT, SP_DECK); cnt_f <- sp_count_trains()
  fixed <- sp_run(cf, os, "toy-s,toy-h", protocol = "fixed")
  expect_identical(fixed$summary$verdict, "ok")
  expect_identical(cnt_f$n, 2L)                                           # 1 treino por membro, K = 3 infer
  cd <- pc_m7h_ctx(SP_ROOT, SP_DECK)
  daily1 <- sp_run(cd, os[[1L]], "toy-s,toy-h", protocol = "daily")        # daily na menor origem: mesmo train_origin do fixed
  for (m in c("MS", "MH")) {
    f1 <- sp_cell(fixed, m, os[[1L]]); d1 <- sp_cell(daily1, m, os[[1L]])
    expect_gt(nrow(f1), 0L)
    expect_identical(f1$y_pred, d1$y_pred)                                # identical(): todos os d_plus
    expect_identical(f1$target_datetime, d1$target_datetime)
  }
  ## K infer: nas origens seguintes o fixed reaproveita o artefato da menor origem (1 treino), o daily re-treina (CONTROL delta != 0)
  cd3 <- pc_m7h_ctx(SP_ROOT, SP_DECK); cnt_d <- sp_count_trains()
  daily <- sp_run(cd3, os, "toy-s,toy-h", protocol = "daily")
  expect_identical(cnt_d$n, 6L)
  for (m in c("MS", "MH")) {
    last_f <- sp_cell(fixed, m, os[[3L]]); last_d <- sp_cell(daily, m, os[[3L]])
    expect_identical(last_f$target_datetime, last_d$target_datetime)
    expect_gt(max(abs(last_f$y_pred - last_d$y_pred)), 0)                 # CONTROL: train_origin diferente => artefato diferente
  }
})

test_that("[AC2b] FORBIDDEN: infer do artefato origin_scoped (daily) == forecast acoplado do dia O (retrain_harness), todos os d_plus", {
  os <- sp_ymd("2026-10-05") + 0:1
  cd <- pc_m7h_ctx(SP_ROOT, SP_DECK); cr <- pc_m7h_ctx(SP_ROOT, SP_DECK)
  daily <- sp_run(cd, os, "toy-sc", protocol = "daily")
  rh <- sp_run(cr, os, "toy-sc", protocol = "retrain_harness")
  expect_identical(daily$summary$verdict, "ok"); expect_identical(rh$summary$verdict, "ok")
  for (o in os) {
    d <- sp_cell(daily, "MC", o); r <- sp_cell(rh, "MC", o)
    expect_gt(nrow(d), 0L)
    expect_identical(d$y_pred, r$y_pred)
    expect_identical(d$target_datetime, r$target_datetime)
  }
  expect_false(dir.exists(file.path(cr$state_dir, "cache", "artifact")))     # o harness acoplado nao usa artefato de cache
  ## CONTROL 1: a carga de treino perturbada (last_data_day) muda o forecast acoplado e o artefato do daily
  p <- pc_m7h_ctx(SP_ROOT, sp_perturb(sp_ymd("2026-10-03") |> lubridate::force_tz(SP_TZ), sp_ymd("2026-10-04") |> lubridate::force_tz(SP_TZ)))
  pert <- sp_run(p, os, "toy-sc", protocol = "retrain_harness")
  expect_gt(max(abs(sp_cell(pert, "MC", os[[1L]])$y_pred - sp_cell(rh, "MC", os[[1L]])$y_pred)), 0)
  ## CONTROL 2: 1 byte do artefato em cache => integridade (quarentena + aviso), nunca uso silencioso; o re-treino volta a delta 0
  art <- Sys.glob(file.path(cd$state_dir, "cache", "artifact", "*", "*", "object.rds"))
  expect_gte(length(art), 1L)
  b <- readBin(art[[1L]], "raw", file.size(art[[1L]]))
  b[[length(b)]] <- as.raw(bitwXor(as.integer(b[[length(b)]]), 1L))
  writeBin(b, art[[1L]])
  seen <- character()
  again <- withCallingHandlers(
    sp_mod$prevcarga_backtest(cd, origins = os, models = "toy-sc", areas = "A", cortes = "08:00", protocol = "daily", baseline = "none"),
    warning = function(w) { seen <<- c(seen, if (inherits(w, "prevcarga_warning")) w$id else "other"); invokeRestart("muffleWarning") })
  expect_true("warning.cache_corrupt_quarantined" %in% seen)
  expect_true(dir.exists(file.path(cd$state_dir, "cache", "_quarantine")))
  expect_identical(sp_cell(again, "MC", os[[1L]])$y_pred, sp_cell(rh, "MC", os[[1L]])$y_pred)
})

## --- (c) rodada operacional x REDUCE full ---------------------------------------------------------------------------------
SP_OS <- sp_ymd("2026-09-28") + 0:4            # n_min = 3: setembro aquece, outubro e fitado
SP_O <- sp_ymd("2026-10-01")

sp_reduce_run <- function(.frame = parent.frame()) {
  ctx <- pc_m7h_ctx(SP_ROOT, SP_DECK, .local_envir = .frame)
  res <- sp_run(ctx, SP_OS, "toy-s,toy-h,toy-sc", areas = NULL)
  list(ctx = ctx, res = res)
}

test_that("[AC2c] FORBIDDEN: combina_train/apply + reconcile_train/apply (pesos e k-hat congelados) == REDUCE full da origem O, subsistema e SIN", {
  rr <- sp_reduce_run(); ctx <- rr$ctx; res <- rr$res
  expect_identical(res$summary$verdict, "ok")
  plan <- sp_mod$prevcarga_plan(ctx, SP_OS, "08:00", filters = list())
  rctx <- sp_mod$.bt_rctx(ctx, plan)
  ## rodada operacional: ctx de OUTRO state_dir (o registry de producao nunca entra no REDUCE)
  op <- pc_m7h_ctx(SP_ROOT, SP_DECK); op$tz <- rctx$tz; op$load_label <- rctx$load_label
  adir <- file.path(op$state_dir, "combina")
  leaves <- list()
  for (f in c("A", "B")) {
    panel <- sp_mod$.bt_reduce_panel(ctx, plan, res$status, res$paths, f, "08:00")
    expect_gt(nrow(panel), 0L)
    suppressWarnings(sp_mod$prevcarga_combina_train(f, panel, adir, ctx$root, n_min = 3L))
    ap <- sp_mod$prevcarga_combina_apply(f, adir, panel[panel$forecast_origin == format(SP_O), ], rctx, t_cut = "0800")
    full <- data.table::as.data.table(arrow::read_parquet(file.path(res$paths$root, "combined", f, "0800", "combined.parquet")))
    full <- full[full$forecast_origin == format(SP_O), ]
    ap <- data.table::as.data.table(ap)
    expect_gt(nrow(full), 0L)
    expect_identical(ap$y_pred[order(ap$target_datetime)], full$y_pred[order(full$target_datetime)])
    leaves[[f]] <- ap
  }
  tr <- sp_mod$prevcarga_reconcile_train(op, SP_O)
  app <- suppressWarnings(sp_mod$prevcarga_reconcile_apply(unname(leaves), op))
  rec <- file.path(res$paths$reconciled, format(SP_O), "0800")
  ref_ss <- data.table::as.data.table(arrow::read_parquet(file.path(rec, "subsistemas", "X.parquet")))
  ref_sin <- data.table::as.data.table(arrow::read_parquet(file.path(rec, "SIN.parquet")))
  ordr <- function(d) d[order(d$target_datetime), ]
  expect_identical(ordr(data.table::as.data.table(app$long$X))$y_pred, ordr(ref_ss)$y_pred)
  expect_identical(ordr(data.table::as.data.table(app$long$SIN))$y_pred, ordr(ref_sin)$y_pred)
  expect_gt(nrow(ref_sin), 0L)
  expect_identical(tr$X$khat_origin, SP_O)
})

test_that("[AC2c] CONTROL: 1 byte dos pesos congelados => integrity (exit 9); pesos de outra origem/mes => delta != 0", {
  rr <- sp_reduce_run(); ctx <- rr$ctx; res <- rr$res
  plan <- sp_mod$prevcarga_plan(ctx, SP_OS, "08:00", filters = list())
  rctx <- sp_mod$.bt_rctx(ctx, plan)
  panel <- sp_mod$.bt_reduce_panel(ctx, plan, res$status, res$paths, "A", "08:00")
  adir <- file.path(withr::local_tempdir("sp-w-"), "combina")
  suppressWarnings(sp_mod$prevcarga_combina_train("A", panel, adir, ctx$root, n_min = 3L))
  rows <- panel[panel$forecast_origin == format(SP_O), ]
  base <- data.table::as.data.table(sp_mod$prevcarga_combina_apply("A", adir, rows, rctx, t_cut = "0800"))
  wf <- Sys.glob(file.path(adir, "A", "v_*", "weights.rds"))
  expect_length(wf, 1L)
  ## CONTROL integridade
  b <- readBin(wf, "raw", file.size(wf)); b[[length(b)]] <- as.raw(bitwXor(as.integer(b[[length(b)]]), 1L)); writeBin(b, wf)
  e <- tryCatch(sp_mod$prevcarga_combina_apply("A", adir, rows, rctx, t_cut = "0800"), prevcarga_error = function(e) e)
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$exit_code, 9L)
  expect_match(conditionMessage(e), "^\\[prevcarga/integrity\\]")
  ## CONTROL delta != 0: pesos treinados com a verdade trocada pelo previsto de UM membro (concentram o peso nele) diferem dos de O
  adir2 <- file.path(withr::local_tempdir("sp-w2-"), "combina")
  skew <- data.table::copy(data.table::as.data.table(panel))
  fcol <- grep("^f_", names(skew), value = TRUE)[[1L]]
  skew[, y_true := get(fcol)]
  suppressWarnings(sp_mod$prevcarga_combina_train("A", skew, adir2, ctx$root, n_min = 3L))
  other <- data.table::as.data.table(sp_mod$prevcarga_combina_apply("A", adir2, rows, rctx, t_cut = "0800"))
  expect_gt(max(abs(other$y_pred[order(other$target_datetime)] - base$y_pred[order(base$target_datetime)])), 0)
})
