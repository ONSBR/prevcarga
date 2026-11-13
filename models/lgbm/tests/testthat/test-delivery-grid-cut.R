## test-delivery-grid-cut.R -- F3.7: .lgbm_delivery_grid usa cuts$delivery_days (efetivo do despachante) quando presente; sem ele,
## o delivery_days do descriptor (2). Cuts sinteticos; nenhum treino nem inferencia.
library(testthat)

P <- lgbm_source_plugin()
at_time <- function(s) lubridate::ymd_hm(s, tz = LGBM_TZ)
cuts_at <- function(dd = NULL) {
  c(list(origin_day = LGBM_O, forecast_origin = LGBM_O, tz = LGBM_TZ, available_until = at_time("2026-10-01 09:00")),
    if (!is.null(dd)) list(delivery_days = as.integer(dd)))
}

test_that("[PLUGINS] delivery_grid_cut: sem cuts$delivery_days a grade e a do descriptor (2 dias); FORBIDDEN clima completo = mesma grade", {
  g <- P$.lgbm_delivery_grid(cuts_at())
  expect_identical(g, lgbm_lib$prevcarga_delivery_grid(cuts_at(), 2L))
  expect_identical(g, P$.lgbm_delivery_grid(cuts_at(2L)))
  expect_identical(max(g), at_time("2026-10-03 00:00"))
})

test_that("[PLUGINS] delivery_grid_cut: CONTROL cuts$delivery_days = 1 (clima so ate O): a grade termina em O+1 00:00", {
  g <- P$.lgbm_delivery_grid(cuts_at(1L))
  expect_identical(g, lgbm_lib$prevcarga_delivery_grid(cuts_at(1L), 1L))
  expect_identical(max(g), at_time("2026-10-02 00:00"))
  expect_lt(length(g), length(P$.lgbm_delivery_grid(cuts_at())))
})
