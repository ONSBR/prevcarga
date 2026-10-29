## test-completude.R -- RF.1: buraco => completude (exit 3), historia curta => window (exit 5). CONTROL: painel completo passa.
library(testthat)

P <- rf_source_plugin()
spec <- rf_fx_spec()
O <- RF_O

for (ph in c("train", "infer")) {
  cuts <- rf_fx_cuts(phase = ph)
  panel <- rf_fx_panel(cuts = cuts)
  first_day <- if (ph == "train") cuts$train_origin - spec$history_days$train else O - spec$history_days$infer

  test_that(sprintf("[PLUGINS] completude: %s -- dia com 23 horas => [prevcarga/completude] exit 3 nomeando area e dia (CONTROL completo)", ph), {
    expect_null(fail(P$plugin_ingest(panel, cuts, spec)))
    day <- O - 30L
    d <- rf_lib$prevcarga_fixture_drop_slots(rf_fx_store(), RF_AREA, day, "10:00", 2L)
    hole <- rf_fx_panel(d, cuts)
    e <- fail(P$plugin_ingest(hole, cuts, spec))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, "plugin.completude")
    expect_identical(e$class_name, "completude"); expect_identical(e$exit_code, 3L)
    expect_identical(e$details$area, RF_AREA)
    expect_identical(e$details$dia, format(day))
    expect_identical(e$details$horas, 23L)
  })

  test_that(sprintf("[PLUGINS] completude: %s -- carga que comeca depois do primeiro dia exigido => [prevcarga/window] exit 5", ph), {
    short <- panel
    short$load <- panel$load[lubridate::as_date(interval_start, tz = cuts$tz) > first_day]
    e <- fail(P$plugin_ingest(short, cuts, spec))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, "plugin.window")
    expect_identical(e$class_name, "window"); expect_identical(e$exit_code, 5L)
    expect_identical(e$details$primeiro_exigido, format(first_day))
    expect_identical(e$details$primeiro_presente, format(first_day + 1L))
    ## CONTROL: comecar exatamente em first_day passa
    ok <- panel
    ok$load <- panel$load[lubridate::as_date(interval_start, tz = cuts$tz) >= first_day]
    expect_null(fail(P$plugin_ingest(ok, cuts, spec)))
  })
}

test_that("[PLUGINS] completude: dia ausente por inteiro no meio da janela tambem e completude (0 horas)", {
  cuts <- rf_fx_cuts(phase = "infer")
  panel <- rf_fx_panel(cuts = cuts)
  day <- O - 10L
  p2 <- panel; p2$load <- panel$load[lubridate::as_date(interval_start, tz = cuts$tz) != day]
  e <- fail(P$plugin_ingest(p2, cuts, spec))
  expect_identical(e$id, "plugin.completude")
  expect_identical(e$details$dia, format(day)); expect_identical(e$details$horas, 0L)
})
