## test-infer-window-min.R -- E2-T3: history_days.infer minimo do HW (2 dias). A predicao com o painel de 45 dias e igual (diff
## maxima 0) a do painel cortado em O - infer. Fixture sintetica pequena (store P.0e); HW univariado, sem clima.
library(testthat)

wm_O <- lubridate::ymd("2024-05-15")
wm_spec <- function() { s <- hw_spec(); s$history_days$train <- 59L; s }

wm_run <- function(infer_days) {
  st <- hw_fx_store(load_until = lubridate::ymd_hm("2024-05-15 09:00", tz = HW_TZ))
  cuts <- hw_fx_cuts(st)
  env <- hw_env(); spec <- wm_spec()
  tdeck <- env$plugin_ingest(hw_fx_panel(st, cuts, "train", history_days = 60L, weather = FALSE), cuts)
  feats <- suppressMessages(env$plugin_build_features(tdeck, spec))
  art <- env$plugin_freeze(env$plugin_train(feats, spec))
  list(env = env, art = art, cuts = cuts, st = st)
}

wm_predict <- function(b, days) {
  deck <- suppressWarnings(b$env$plugin_ingest(hw_fx_panel(b$st, b$cuts, "infer", history_days = days, weather = FALSE), b$cuts))
  inf <- suppressWarnings(b$env$plugin_inference_features(deck, b$art, b$cuts))
  suppressWarnings(b$env$plugin_infer(b$art, inf, b$cuts))
}

test_that("[PLUGINS] infer_min: run.yaml do HW declara history_days.infer = 2", {
  expect_identical(hw_spec()$history_days$infer, 2L)
})

test_that("[PLUGINS] infer_min: painel de 45 dias == painel cortado em O - history_days.infer (diff maxima 0)", {
  b <- wm_run()
  n <- hw_spec()$history_days$infer
  full <- wm_predict(b, 45L)
  short <- wm_predict(b, n)
  expect_identical(dim(short$values), dim(full$values))
  expect_identical(short$delivery$interval_end, full$delivery$interval_end)
  expect_identical(max(abs(short$values - full$values)), 0)
})
