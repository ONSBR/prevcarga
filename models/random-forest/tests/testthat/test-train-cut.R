## test-train-cut.R -- RF.2: corte de treino origin_agnostic (ADR-0023). train_origin = O-1-H, last_data_day = O-1: o
## sha256 do artefato congelado NAO muda com carga de dia >= O nem com emissoes das origens > train_origin (FORBIDDEN) e
## MUDA com a carga de train_origin-1, a carga de O-1 (alvo y_h216 da ultima amostra) e a emissao da origem train_origin
## (CONTROL). Porte de gate01/gate11 do oraculo. O plugin CONFERE o corte e nunca o aplica (aborta [prevcarga/contract]).
library(testthat)

TZ_ <- RF_TZ
at_ <- function(x) lubridate::ymd_hms(x, tz = TZ_)

## deck perturbado em copia: carga entre (after, at_or_before] pelo interval_start
perturb_load <- function(dir, after, at_or_before)
  rf_lib$prevcarga_fixture_perturb(dir, "load", after = at_(after), at_or_before = at_(at_or_before))

test_that("[PLUGINS] corte_treino: FORBIDDEN carga dia >= O e emissoes de origens > train_origin => sha256 identico", {
  spec <- rf_tr_spec(); dir <- rf_fx_store()
  base <- rf_tr_run(dir, spec)
  cuts <- base$cuts
  expect_equal(cuts$train_origin, RF_O - 1L - 9L)
  expect_equal(cuts$last_data_day, RF_O - 1L)
  sha0 <- rf_tr_sha(base$art)
  ## (a) carga de qualquer intervalo com dia >= O (o harness entrega o painel de treino cortado em last_data_day)
  d1 <- perturb_load(dir, "2026-09-30 23:59:59", "2026-10-01 23:30:00")
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  expect_identical(rf_tr_sha(rf_tr_run(d1, spec)$art), sha0)
  ## (b) emissoes das origens nativas > train_origin: issue_time > limite(train_origin) = limite(O) - (O - train_origin) dias
  lim_tro <- cuts$weather_issue_limit - lubridate::days(as.integer(RF_O - cuts$train_origin))
  d2 <- rf_lib$prevcarga_fixture_perturb(dir, "weather_forecast", issue_after = lim_tro)
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  panel2 <- rf_fx_panel(d2, cuts)
  expect_true(any(panel2$weather_forecast$issue_time > lim_tro))     # a perturbacao chega ao painel do plugin
  expect_identical(rf_tr_sha(rf_tr_run(d2, spec)$art), sha0)
})

test_that("[PLUGINS] corte_treino: CONTROL carga de train_origin-1, carga de O-1 e emissao de train_origin => sha256 muda", {
  spec <- rf_tr_spec(); dir <- rf_fx_store()
  base <- rf_tr_run(dir, spec); sha0 <- rf_tr_sha(base$art); cuts <- base$cuts
  ## train_origin - 1 = 2026-09-20; O-1 = 2026-09-30 (alvo y_h216 da ultima amostra)
  c1 <- perturb_load(dir, "2026-09-19 23:59:59", "2026-09-20 23:30:00")
  c2 <- perturb_load(dir, "2026-09-29 23:59:59", "2026-09-30 23:30:00")
  lim_tro <- cuts$weather_issue_limit - lubridate::days(as.integer(RF_O - cuts$train_origin))
  c3 <- rf_lib$prevcarga_fixture_perturb(dir, "weather_forecast", issue_after = lim_tro - lubridate::days(1L),
                                         issue_at_or_before = lim_tro)
  on.exit(unlink(c(c1, c2, c3), recursive = TRUE), add = TRUE)
  for (d in c(c1, c2, c3)) expect_false(identical(rf_tr_sha(rf_tr_run(d, spec)$art), sha0), info = d)
})

test_that("[PLUGINS] corte_treino: prevcarga_honesty_gate (run_fn = treino sobre o painel) => HONESTO (clean2 = forbidden = 0, control > 0)", {
  spec <- rf_tr_spec(); dir <- rf_fx_store()
  cuts <- rf_fx_cuts(dir = dir); cuts$spec <- spec
  full <- rf_fx_panel(dir, cuts, phase = "infer")             # carga ate available_until (inclui o dia O)
  gp <- data.frame(area_code = full$load$area_code, interval_start = full$load$interval_start,
                   interval_end = full$load$interval_end, y_true = full$load$load_mwh)
  base <- rf_tr_run(dir, spec)
  probe <- base$feats$table[nrow(base$feats$table), base$feats$feature_cols, with = FALSE]
  predict_all <- function(model) vapply(names(model), function(n) {
    s <- model[[n]]
    unname(stats::predict(s$model, as.matrix(probe[, s$features, with = FALSE])))
  }, 0)
  run_fn <- function(p) {
    ld <- full$load
    ld$load_mwh <- p$y_true
    ## harness: o painel de treino chega cortado em last_data_day (o plugin so confere)
    ld <- ld[lubridate::as_date(interval_start, tz = cuts$tz) <= cuts$last_data_day]
    pn <- full; pn$load <- ld
    pl <- rf_plug()
    fr <- pl$plugin_ingest(pn, cuts, spec)
    m <- pl$plugin_train(pl$plugin_build_features(fr, spec), spec, cuts)
    data.frame(y_pred = predict_all(m))
  }
  cut <- at_("2026-10-01 00:00:00")
  g <- rf_lib$prevcarga_honesty_gate(run_fn, gp, cut, horizon = 9L)
  expect_identical(g$verdict, "HONESTO")
  expect_true(g$reach_ok)
  expect_lte(g$clean2_delta, 1e-5)
  expect_lte(g$forbidden_delta, 1e-5)
  expect_gt(g$control_delta, 1e-5)
})

test_that("[PLUGINS] corte_treino: painel com dia > last_data_day ou rf_features com origem > train_origin => [prevcarga/contract] exit 2 (confere, nao corta)", {
  spec <- rf_tr_spec(); dir <- rf_fx_store()
  full_ld <- rf_fx_panel(dir, rf_fx_cuts(dir = dir), phase = "infer", hourly = FALSE)$load
  e <- fail(rf_tr_run(dir, spec, panel_edit = function(p) { p$load <- full_ld; p }))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  expect_identical(e$id, "plugin.train_beyond_cut")
  ## rf_features com origem > train_origin
  base <- rf_tr_run(dir, spec); p <- rf_plug()
  ft <- base$feats
  ft$table <- data.table::copy(ft$table)
  ft$table[nrow(ft$table), dataOrigem := dataOrigem + 3L]
  e2 <- fail(p$plugin_train(ft, spec, base$cuts))
  expect_identical(e2$class_name, "contract"); expect_identical(e2$exit_code, 2L)
  expect_identical(e2$details$origem, format(max(ft$table$dataOrigem)))
  expect_identical(fail(p$plugin_train(ft, spec))$class_name, "contract")   # coerencia interna, sem cuts
  ## train_origin do rf_features alem do corte do deck
  ft2 <- base$feats; ft2$train_origin <- ft2$train_origin + 2L
  expect_identical(fail(p$plugin_train(ft2, spec, base$cuts))$class_name, "contract")
  ## alvo alem de last_data_day
  cu <- base$cuts; cu$last_data_day <- cu$last_data_day - 1L
  expect_identical(fail(p$plugin_train(base$feats, spec, cu))$class_name, "contract")
  ## fase infer nao treina
  fi <- base$feats; fi$phase <- "infer"
  expect_identical(fail(p$plugin_train(fi, spec))$class_name, "contract")
  ## CONTROL: o caminho feliz passa com os mesmos cortes
  expect_s3_class(rf_tr_run(dir, spec)$art, "prevcarga_plugin_artifact")
})
