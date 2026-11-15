## test-infer-lag-window.R -- E2-T3: janela minima de infer do RF (history_days.infer = 9). A carga so e exigida de O-8 a O-1
## (min(keep) - max_lag_days .. O-1); O-9 ausente nao aborta; painel de 45 dias == painel cortado (frame e y_pred identicos).
library(testthat)

P <- rf_source_plugin()
O <- RF_O
cuts <- rf_fx_cuts(phase = "infer")
spec <- rf_fx_spec()
panel <- rf_fx_panel(cuts = cuts)
day_of <- function(p) lubridate::as_date(p$load$interval_start, tz = cuts$tz)
drop_day <- function(p, d) { q <- p; q$load <- p$load[day_of(p) != d]; q }
from_day <- function(p, d) { q <- p; q$load <- p$load[day_of(p) >= d]; q }
ingest <- function(p) P$plugin_ingest(p, cuts, spec)

test_that("[PLUGINS] infer_lag_window: run.yaml e constantes: infer 9, max_lag_days 7, exigido desde O-8", {
  expect_identical(spec$history_days$infer, 9L)
  expect_identical(P$plugin_constants()$max_lag_days, 7L)
})

test_that("[PLUGINS] infer_lag_window: sem O-8 aborta plugin.window com primeiro_exigido O-8 (CONTROL: com O-8 passa)", {
  e <- tryCatch(ingest(from_day(drop_day(panel, O - 8L), O - 9L)), prevcarga_error = identity)
  ## dia O-8 ausente no meio de O-9..O-1 e completude (0 horas); cortar tudo antes de O-7 e window
  expect_identical(e$id, "plugin.completude"); expect_identical(e$details$dia, format(O - 8L))
  e2 <- tryCatch(ingest(from_day(panel, O - 7L)), prevcarga_error = identity)
  expect_s3_class(e2, "prevcarga_error")
  expect_identical(e2$id, "plugin.window"); expect_identical(e2$exit_code, 5L)
  expect_identical(e2$details$primeiro_exigido, format(O - 8L))
  expect_identical(e2$details$primeiro_presente, format(O - 7L))
  expect_type(ingest(from_day(panel, O - 8L)), "list")
})

test_that("[PLUGINS] infer_lag_window: O-9 ausente nao aborta e o frame e identico ao de 45 dias nos dias exigidos", {
  full <- ingest(from_day(panel, O - 45L))
  short <- ingest(from_day(panel, O - 8L))
  expect_identical(short$weather, full$weather)
  expect_identical(short$holidays, full$holidays)
  expect_identical(short$load, full$load[dia >= O - 8L])
  ## O-9 ausente com O-10 presente tambem nao aborta (a janela [-9, 0] do per_origin e do LOCF do clima, nao da carga)
  expect_type(ingest(from_day(drop_day(panel, O - 9L), O - 12L)), "list")
})

test_that("[PLUGINS] infer_lag_window: y_pred do painel cortado em O-8 == painel completo (diff maxima 0)", {
  dir <- rf_inf_deck("09:30")
  full <- rf_inf_run(dir)
  short <- rf_inf_run(dir, panel_edit = function(p) { p$load <- p$load[lubridate::as_date(interval_start, tz = RF_TZ) >= O - 8L]; p })
  expect_identical(max(abs(full$env$values - short$env$values)), 0)
})
