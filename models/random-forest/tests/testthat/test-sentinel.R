## test-sentinel.R -- RF.3, AC 3: deck sentinela (cada valor de carga codifica seu interval_start). A hora H do frame e a
## media dos intervalos H:00 e H:30 (por interval_start); prev_day_mean usa o dia O-1; na volta a 48 a ancora do lead k
## cai em interval_end = interval_start + 30 min e o interpolado em + 60 min.
library(testthat)

sent_min <- function(dia, hora, min = 0L) (as.integer(dia) - as.integer(lubridate::ymd("2026-09-01"))) * 1440 + hora * 60 + min

sent_run <- function() {
  p <- rf_plug()
  dir <- rf_inf_deck("09:30", sentinel = TRUE)
  cuts <- rf_fx_cuts(dir = dir, phase = "infer"); spec <- rf_tr_spec(); cuts$spec <- spec
  panel <- rf_fx_panel(dir, cuts, hourly = FALSE)                       # cru (o CONTROL agrupa o cru)
  frame <- p$plugin_ingest(rf_lib$to_hourly(panel, weather = FALSE), cuts, spec)
  list(p = p, cuts = cuts, spec = spec, panel = panel, frame = frame)
}

test_that("[PLUGINS] sentinela: hora H = media dos sentinelas H:00 e H:30 (por interval_start); prev_day_mean = media do dia O-1", {
  s <- sent_run()
  ld <- s$frame$load
  expect_gt(nrow(ld), 24L * 40L)
  expect_equal(ld$load_mw, sent_min(ld$dia, ld$hora) + 15)
  ft <- s$p$plugin_build_features(s$frame, s$spec)$table
  O <- RF_O
  for (o in c(O - 1L, O)) {
    ## 48 sentinelas do dia anterior por interval_start: minuto do dia 0, 30, ..., 1410
    expect_equal(ft$prev_day_mean[ft$dataOrigem == o], sent_min(o - 1L, 0L) + mean(seq(0, 1410, by = 30)))
    expect_equal(ft$prev_2day_mean[ft$dataOrigem == o], sent_min(o - 2L, 0L) + mean(seq(0, 1410, by = 30)))
  }
  ## CONTROL: um fixture que agrupa por interval_end (e nao interval_start) reprova o sentinela
  bad <- data.table::as.data.table(s$panel$load)
  bad[, h := lubridate::floor_date(interval_end, "hour")]
  bad <- bad[, list(m = mean(load_mwh)), by = h]
  keep <- bad$h >= lubridate::ymd_hm("2026-09-15 00:00", tz = RF_TZ) & bad$h < lubridate::ymd_hm("2026-09-16 00:00", tz = RF_TZ)
  want <- sent_min(lubridate::ymd("2026-09-15"), 0:23) + 15
  expect_false(isTRUE(all.equal(bad$m[keep], want)))
  expect_equal(bad$m[keep] - want, rep(-30, 24))  # deslocamento de 30 min: o erro que o sentinela pega
})

test_that("[PLUGINS] sentinela: volta a 48 = ancora do lead k em interval_start + 30 min, interpolado em + 60 min", {
  s <- sent_run(); p <- s$p
  O <- RF_O
  k <- 1:216
  ## lead k sintetico (linear => spline reproduz exatamente): bloco O-1 = k, bloco O = 1000 + k
  values <- rbind(`2026-09-30` = as.numeric(k), `2026-10-01` = 1000 + k)
  d <- p$.rf_blocks_to_delivery(values, c(O - 1L, O), s$cuts)
  t0 <- lubridate::force_tz(lubridate::as_datetime(O + 1L), RF_TZ)       # O+1 00:00 = interval_start do lead 1 do bloco O
  half <- seq(t0, by = "30 min", length.out = 2L * 216L + 1L)           # t0, t0+30, t0+60, ...
  for (lead in c(1L, 2L, 24L, 25L, 100L, 191L)) {
    anchor <- half[[2L * lead]]; interp <- half[[2L * lead + 1L]]       # interval_start + 30 min / + 60 min
    expect_equal(d$y_pred[d$interval_end == anchor], 1000 + lead, tolerance = 1e-9)
    expect_equal(d$y_pred[d$interval_end == interp], 1000 + lead + 0.5, tolerance = 1e-9)
  }
  ## CONTROL: o instante interval_start + 60 min do lead 1 e o interpolado (1001.5), nao a ancora do lead 2 (1002)
  expect_false(isTRUE(all.equal(d$y_pred[d$interval_end == half[[3L]]], 1000 + 2)))
})
