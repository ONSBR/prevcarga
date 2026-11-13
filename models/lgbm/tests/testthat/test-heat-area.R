## test-heat-area.R -- BASE no heat_area pela safra de clima, sem ingest/treino (fixture minima no proprio teste).
P <- lgbm_source_plugin(); K <- P$plugin_constants(); TZ <- "America/Sao_Paulo"
d <- lubridate::ymd("2026-09-30")
vt <- lubridate::ymd_hm("2026-09-30 00:00", tz = TZ) + lubridate::minutes(30L * 0:3)
w <- data.table::rbindlist(list(
  data.table::data.table(area_code = "BASE", issue_time = lubridate::ymd_hm("2026-09-29 00:00", tz = "UTC"), valid_time = vt, temperature = 20),
  data.table::data.table(area_code = "BASE", issue_time = lubridate::ymd_hm("2026-09-29 12:00", tz = "UTC"), valid_time = vt, temperature = 25),
  data.table::data.table(area_code = "SP",   issue_time = lubridate::ymd_hm("2026-09-29 12:00", tz = "UTC"), valid_time = vt, temperature = 99)))

test_that("[PLUGINS] heat_area: BASE usa o proprio clima, emissao mais recente (FORBIDDEN)", {
  t <- P$lgbm_weather_vintage(w, "BASE", d, 0L, K, TZ)
  expect_identical(nrow(t), 4L)
  expect_equal(t$Temperatura, rep(25, 4L))
})

test_that("[PLUGINS] heat_area: ZZ1 fora da tabela aborta lgbm.contract (CONTROL)", {
  e <- tryCatch(P$lgbm_weather_vintage(w, "ZZ1", d, 0L, K, TZ), prevcarga_error = identity)
  expect_s3_class(e, "prevcarga_error")
  expect_match(conditionMessage(e), "area ZZ1 sem heat_area", fixed = TRUE)
})
