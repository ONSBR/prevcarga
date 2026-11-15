## test-infer-window-min.R -- E2-T3: history_days.infer minimo do LGBM (5 dias) e guarda interna min_history_days_infer = 4.
## Painel de 45 dias == painel cortado em O - infer (predicao diff maxima 0); 3 dias completos => lgbm.window; 5 seguem.
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
P <- lgbm_source_plugin()
K <- lgbm_consts(P)
b <- suppressWarnings(lgbm_fx_base())
O <- LGBM_O
N <- lgbm_lib$prevcarga_model_spec("lgbm", lgbm_root())$history_days$infer

cut_panel <- function(p, from_day) {
  q <- p; q$load <- p$load[lubridate::as_date(interval_start, tz = b$cuts$tz) >= from_day]; q
}
dpanel <- function(days) lgbm_fx_dpanel(b$deck, b$ctx, "infer", b$cuts, history_days = days)

test_that("[PLUGINS] infer_min: run.yaml do LGBM declara history_days.infer = 5 e a guarda interna segue 4", {
  expect_identical(N, 5L)
  expect_identical(K$min_history_days_infer, 4L)
})

test_that("[PLUGINS] infer_min: painel de 45 dias == painel cortado em O - infer (predicao diff maxima 0)", {
  a <- lgbm_fx_infer(b, panel = dpanel(45L))
  s <- lgbm_fx_infer(b, panel = dpanel(N))
  expect_identical(a$delivery$interval_end, s$delivery$interval_end)
  expect_identical(max(abs(unlist(a$values) - unlist(s$values))), 0)
})

test_that("[PLUGINS] infer_min: 3 dias completos antes de O => lgbm.window; 4 e 5 seguem sem erro", {
  p <- dpanel(45L)
  e <- fail(P$plugin_ingest(cut_panel(p, O - 3L), b$cuts))
  expect_s3_class(e, "prevcarga_error")
  expect_match(conditionMessage(e), "3 dias completos")
  expect_null(fail(P$plugin_ingest(cut_panel(p, O - 4L), b$cuts)))
  expect_null(fail(P$plugin_ingest(cut_panel(p, O - N), b$cuts)))
})
