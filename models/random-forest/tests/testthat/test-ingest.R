## test-ingest.R -- RF.1: plugin_ingest (frame, conferencia de cortes, feriados). FORBIDDEN (Delta=0) e CONTROL (Delta!=0).
library(testthat)

P <- rf_source_plugin()
spec <- rf_fx_spec()
O <- RF_O
TZ <- RF_TZ
local0 <- function(d) lubridate::force_tz(lubridate::as_datetime(d), TZ)

train_cuts <- rf_fx_cuts(phase = "train")
infer_cuts <- rf_fx_cuts(phase = "infer")
train_raw <- rf_fx_panel(cuts = train_cuts, hourly = FALSE)       # 30 min cru do leitor
train_panel <- rf_lib$to_hourly(train_raw, weather = FALSE)        # a forma que o despachante entrega (weather_resample raw)
infer_panel <- rf_fx_panel(cuts = infer_cuts)

test_that("[PLUGINS] ingest_frame: train -> 357 origens nativas, carga 2025-09-21..2026-09-30 com 24 h/dia, hora = media H:00 e H:30", {
  expect_identical(train_cuts$train_origin, lubridate::ymd("2026-09-21"))
  expect_identical(train_cuts$last_data_day, lubridate::ymd("2026-09-30"))
  expect_identical(train_cuts$tz, "Etc/GMT+3")
  expect_identical(train_cuts$available_until, lubridate::ymd_hm("2026-10-01 09:00", tz = TZ))
  fr <- P$plugin_ingest(train_panel, train_cuts, spec)
  expect_identical(fr$phase, "train")
  expect_identical(fr$area, RF_AREA)
  expect_identical(fr$origins, seq(lubridate::ymd("2025-09-30"), lubridate::ymd("2026-09-21"), by = "day"))
  expect_length(fr$origins, 357L)
  expect_identical(fr$train_origin, lubridate::ymd("2026-09-21"))
  expect_identical(range(fr$load$dia), lubridate::ymd(c("2025-09-21", "2026-09-30")))
  expect_true(all(fr$load[, .N, by = dia]$N == 24L))
  expect_identical(fr$load$hora[1:24], 0:23)
  expect_identical(names(fr$load), c("area", "dia", "hora", "load_mw"))
  expect_length(fr$dropped_origins, 0L)
  expect_true(all(!fr$weather$locf))
  expect_setequal(unique(fr$weather$day_of_horizon), 1:9)
  ## hora H = media dos dois intervalos com interval_start H:00 e H:30 (agrupa por interval_start, nunca por interval_end)
  ref <- data.table::as.data.table(train_raw$load)[, list(m = mean(load_mwh), n = .N),
    by = list(dia = lubridate::as_date(interval_start, tz = TZ), hora = lubridate::hour(interval_start))]
  expect_true(all(ref$n == 2L))
  j <- merge(fr$load, ref, by = c("dia", "hora"))
  expect_equal(j$load_mw, j$m, tolerance = 0)
  one <- train_raw$load[interval_start %in% (local0(lubridate::ymd("2026-03-10")) + lubridate::hours(14) + lubridate::minutes(c(0, 30)))]
  expect_equal(fr$load[dia == lubridate::ymd("2026-03-10") & hora == 14L, load_mw], mean(one$load_mwh), tolerance = 0)
})

test_that("[PLUGINS] ingest_frame: infer -> origens c(O-1, O), carga termina em O-1 e comeca em O-45; o dia O nunca entra", {
  fr <- P$plugin_ingest(infer_panel, infer_cuts, spec)
  expect_identical(fr$phase, "infer")
  expect_identical(fr$origins, c(O - 1L, O))
  expect_identical(max(fr$load$dia), O - 1L)
  expect_identical(min(fr$load$dia), O - 45L)
  expect_false(any(fr$load$dia >= O))
  expect_true(all(fr$load[, .N, by = dia]$N == 24L))
  expect_true(is.na(fr$train_origin))
  expect_setequal(fr$weather$origin, c(O - 1L, O))
  ## FORBIDDEN: a carga do dia O no painel de infer nao muda o frame (Delta = 0)
  d <- rf_fx_store()
  dO <- rf_lib$prevcarga_fixture_perturb(d, "load", after = local0(O) - lubridate::minutes(1))
  fr_o <- P$plugin_ingest(rf_fx_panel(dO, infer_cuts), infer_cuts, spec)
  expect_identical(fr_o, fr)
  ## CONTROL: a carga de O-1 muda o frame (Delta != 0)
  d1 <- rf_lib$prevcarga_fixture_perturb(d, "load", after = local0(O - 1L) - lubridate::minutes(1),
                                         at_or_before = local0(O) - lubridate::minutes(1))
  fr_1 <- P$plugin_ingest(rf_fx_panel(d1, infer_cuts), infer_cuts, spec)
  expect_false(identical(fr_1$load, fr$load))
  expect_true(all(fr_1$load[dia < O - 1L]$load_mw == fr$load[dia < O - 1L]$load_mw))
})

test_that("[PLUGINS] ingest_cut_check: 4 casos de conferencia abortam [prevcarga/contract] exit 2 com where e ofensor (CONTROL: 23:30 passa)", {
  chk <- function(e, id, field = NULL) {
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, id)
    expect_identical(e$class_name, "contract")
    expect_identical(e$exit_code, 2L)
    expect_identical(e$where, "random-forest/ingestion")
    e
  }
  ## 1. painel de treino com um intervalo de O
  bad <- train_panel; bad$load <- rbind(train_panel$load, infer_panel$load[lubridate::as_date(interval_start, tz = TZ) == O][1L])
  e <- chk(fail(P$plugin_ingest(bad, train_cuts, spec)), "plugin.train_beyond_cut")
  expect_identical(e$details$primeiro, format(local0(O)))
  ## 2. painel de infer com interval_end > available_until
  bad <- infer_panel; extra <- data.table::copy(infer_panel$load[.N])
  extra[, `:=`(interval_start = interval_start + lubridate::minutes(30), interval_end = interval_end + lubridate::minutes(30))]
  bad$load <- rbind(infer_panel$load, extra)
  e <- chk(fail(P$plugin_ingest(bad, infer_cuts, spec)), "plugin.infer_beyond_cut")
  expect_identical(e$details$primeiro, format(extra$interval_end))
  ## 3. clima com issue_time > weather_issue_limit
  bad <- train_panel; ex <- data.table::copy(train_panel$weather_forecast[1L])
  ex[, issue_time := train_cuts$weather_issue_limit + lubridate::hours(1)]
  bad$weather_forecast <- rbind(train_panel$weather_forecast, ex)
  e <- chk(fail(P$plugin_ingest(bad, train_cuts, spec)), "plugin.weather_beyond_limit")
  expect_identical(e$details$issue_time, format(ex$issue_time, tz = "UTC"))
  ## 4. carga com coluna timestamp / load_label (rotulo cru) e fuso fora de cuts$tz
  for (col in c("timestamp", "load_label")) {
    bad <- train_panel; bad$load <- data.table::copy(train_panel$load)[, (col) := 1L]
    e <- chk(fail(P$plugin_ingest(bad, train_cuts, spec)), "plugin.raw_timestamp")
    expect_identical(e$details$column, col)
  }
  bad <- train_panel; bad$load <- data.table::copy(train_panel$load)[, interval_start := lubridate::with_tz(interval_start, "UTC")]
  e <- chk(fail(P$plugin_ingest(bad, train_cuts, spec)), "plugin.raw_timestamp")
  expect_identical(e$details$column, "interval_start")
  ## CONTROL: o painel de treino termina exatamente no intervalo 23:30 de last_data_day e passa; o do infer em available_until
  expect_identical(max(train_raw$load$interval_start), local0(O - 1L) + lubridate::hours(23) + lubridate::minutes(30))
  expect_null(fail(P$plugin_ingest(train_panel, train_cuts, spec)))
  expect_identical(max(infer_panel$load$interval_end), infer_cuts$available_until)
  expect_null(fail(P$plugin_ingest(infer_panel, infer_cuts, spec)))
})

test_that("[PLUGINS] holidays: uniao do painel com a lista legada, ordenada e sem duplicata; sem dataset => contract (sem fallback)", {
  fr <- P$plugin_ingest(train_panel, train_cuts, spec)
  legacy <- P$plugin_constants()$holidays_legacy
  expect_length(legacy, 69L)
  expect_s3_class(legacy, "Date")
  expect_identical(fr$holidays, sort(unique(c(train_panel$holidays[area_code == RF_AREA]$date, legacy))))
  expect_false(is.unsorted(fr$holidays))
  expect_false(anyDuplicated(fr$holidays) > 0L)
  expect_true(all(legacy %in% fr$holidays))
  ## CONTROL: remover do painel um feriado fora da lista legada muda o frame
  gone <- lubridate::ymd("2026-01-01")
  expect_true(gone %in% train_panel$holidays$date && !(gone %in% legacy))
  p2 <- train_panel; p2$holidays <- train_panel$holidays[date != gone]
  fr2 <- P$plugin_ingest(p2, train_cuts, spec)
  expect_false(identical(fr2$holidays, fr$holidays))
  expect_identical(setdiff(fr$holidays, fr2$holidays), gone)
  ## painel sem o dataset => [prevcarga/contract] exit 2 (o oraculo caia em silencio para a lista legada)
  p3 <- train_panel; p3$holidays <- NULL
  e <- fail(P$plugin_ingest(p3, train_cuts, spec))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  expect_identical(e$where, "random-forest/ingestion")
})

test_that("[PLUGINS] ingest_frame: o estagio despachado (panel, cuts) le spec/fase de cuts e a assinatura e canonica", {
  expect_identical(names(formals(P$rf_stage_ingest)), c("panel", "cuts"))
  fr <- P$rf_stage_ingest(train_panel, train_cuts)
  expect_identical(fr, P$plugin_ingest(train_panel, train_cuts, spec))
  no_spec <- train_cuts; no_spec$spec <- NULL
  e <- fail(P$rf_stage_ingest(train_panel, no_spec))
  expect_identical(e$class_name, "contract")
})

test_that("[PLUGINS] ingest_frame: o plugin nao reamostra (FIX 7); declara weather_resample raw; carga de 30 min crua => completude", {
  expect_length(rf_grep_code(file.path(RF_PLUGIN_DIR(), "R"), "to_hourly|floor_date\\([^)]*hour"), 0L)
  expect_identical(P$plugin_descriptor()$weather_resample, "raw")
  ## CONTROL: sem o despachante (painel de 30 min) o plugin acusa 48 linhas por dia em vez de agregar em silencio
  e <- fail(P$plugin_ingest(train_raw, train_cuts, spec))
  expect_identical(e$id, "plugin.completude"); expect_identical(e$details$horas, 48L)
})
