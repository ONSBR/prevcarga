## test-dessem-window-facade.R -- fachadas e despachante consomem descriptor$history_window e o anexo de feriados (E3-T2; ADR-046).
## Plugins fake (com e sem history_window) e read_panel injetado: nenhum teste de nome de modelo; tudo em memoria.
library(testthat)
library(data.table)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "holiday_islands", "deck_reader",
                       "validation", "dates", "registry", "features", "inference_needs", "training", "inference"))

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2023-06-01")
LAST <- O - 1L
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
at <- function(day, hm = "00:00") lubridate::ymd_hm(paste(day, hm), tz = TZ)

## --- descriptors e insumos fake -------------------------------------------------------------------------------
fake_plugin <- function(with_window, id = "fake-a") {
  d <- list(model_id = id, data_lag_days = 0L, weather_forecast_in_train = FALSE, granularity_min = 60L,
            model_time_offset_min = 60L, weather_resample = "hourly")
  if (with_window) d$history_window <- function(origin, spec) env$prevcarga_dessem_window(origin, "ano_civil")
  list(descriptor = d)
}
fake_spec <- list(name = "fake-a", split = "origin_scoped", horizon_days = 1L, granularity_min = 60L,
                  history_days = list(train = 5L, infer = 5L))
fake_rc <- function() {
  au <- at(O)
  list(cuts = data.frame(model = "fake-a", data_lag_days = 0L, stringsAsFactors = FALSE), tz = TZ, load_label = "end",
       available_until = au, weather_issue_cutoff = "available_until", weather_issue_limit = au,
       weather_publication_lag_hours = 0L)
}
## um registro por dia (00:00) em [from, to]: o suficiente para contar dias
day_load <- function(from, to, area = "A") {
  d <- seq(from, to, by = "day")
  data.table(area_code = area, interval_start = at(d), interval_end = at(d) + lubridate::minutes(30L), load_mwh = 1)
}
day_obs <- function(from, to, area = "A") {
  d <- seq(from, to, by = "day")
  data.table(area_code = area, valid_time = at(d), temperature = 20)
}
annex_days <- seq(lubridate::ymd("2019-06-13"), lubridate::ymd("2019-06-20"), by = "day")
fake_annex <- function(days = annex_days, extra_after = NULL) {
  d <- c(days, extra_after)
  list(islands = data.table(area_code = "A", island_id = 1L, from = min(days), to = max(days),
                            holiday_dates = list(max(days))),
       load = day_load(min(d), max(d)), weather_observed = day_obs(min(d), max(d)))
}
fake_panel <- function(from, annex = NULL, to = O) {
  p <- list(load = day_load(from, to), weather_observed = day_obs(from, to),
            weather_forecast = data.table(area_code = "A", issue_time = at(O), valid_time = at(O), temperature = 20),
            holidays = data.table(area_code = "A", date = O, prevcarga_code = 0L))
  if (!is.null(annex)) p$holiday_annex <- annex
  p$attrs <- list(origin = O, tz = TZ)
  p
}
inputs <- function(plugin, rp, ...)
  env$prevcarga_train_inputs(list(mdir = "fake-a"), "A", O, list(weather_issue_cutoff = "available_until"),
                             rc = fake_rc(), read_panel = rp, plugin = plugin, spec = fake_spec, ...)

test_that("[AC4] com history_window: span, holiday_window repassados e anexo fora da contagem de dias", {
  got <- NULL
  w <- env$prevcarga_dessem_window(O, "ano_civil")
  rp <- function(area, span, H, fc, holiday_window = NULL) {
    got <<- list(span = span, w = holiday_window)
    fake_panel(w$common_from, fake_annex(extra_after = c(O, O + 1L)))
  }
  r <- inputs(fake_plugin(TRUE), rp)
  expect_identical(got$span, 1096L)                       # origin - common_from + 1
  expect_identical(got$w$mode, "ano_civil")
  expect_identical(got$w$common_from, w$common_from)
  expect_equal(w$common_from, lubridate::ymd("2020-06-01"))
  ## anexo: so dias <= last_data_day (o dia O e posterior sao cortados); janela comum intocada
  expect_true(all(lubridate::as_date(r$panel$holiday_annex$load$interval_start, tz = TZ) <= LAST))
  expect_true(all(lubridate::as_date(r$panel$holiday_annex$weather_observed$valid_time, tz = TZ) <= LAST))
  expect_identical(r$panel$holiday_annex$islands, fake_annex()$islands)
  expect_true(all(lubridate::as_date(r$panel$load$interval_start, tz = TZ) <= LAST))
  ## contagem: so dias comuns (1095 dias de 2020-06-01 a 2023-05-31); os dias do anexo (8) nao entram
  expect_identical(length(unique(lubridate::as_date(r$panel$load$interval_start, tz = TZ))), 1095L)
})

test_that("[AC4] window.history_short conta so dias comuns: anexo grande nao cobre dia comum faltante", {
  w <- env$prevcarga_dessem_window(O, "ano_civil")
  rp <- function(area, span, H, fc, holiday_window = NULL) fake_panel(w$common_from + 30L, fake_annex())  # 30 dias a menos
  e <- fail(inputs(fake_plugin(TRUE), rp))
  expect_identical(e$id, "window.history_short")
  expect_identical(e$details$available_days, 1065L)
  expect_identical(e$details$required_days, 1095L)
})

test_that("[AC4] sem history_window: history_days como hoje, nada de holiday_window e sem elemento holiday_annex", {
  got <- NULL
  rp <- function(area, span, H, fc) { got <<- span; fake_panel(O - 5L) }      # assinatura SEM holiday_window
  r <- inputs(fake_plugin(FALSE), rp)
  expect_identical(got, 6L)                                                    # origin_scoped: last_data_day - 5 + 1 ..= O
  expect_null(r$panel$holiday_annex)
  expect_identical(names(r$panel), c("load", "weather_observed", "weather_forecast", "holidays", "attrs"))
})

test_that("[AC4] hash do painel: identico sem anexo (formula anterior) e sensivel ao anexo quando existe", {
  old_hash <- function(panel) {                                                # copia literal da formula anterior
    ds <- c("load", "weather_observed", "weather_forecast", "holidays")
    txt <- vapply(ds, function(n) env$.prevcarga_tr_table_text(panel[[n]]), "")
    digest::digest(paste(ds, txt, sep = "\n", collapse = "\n"), algo = "sha256", serialize = FALSE)
  }
  p0 <- fake_panel(O - 5L)
  expect_identical(env$.prevcarga_tr_panel_hash(p0), old_hash(p0))
  p1 <- fake_panel(O - 5L, fake_annex())
  expect_false(identical(env$.prevcarga_tr_panel_hash(p1), old_hash(p0)))
  expect_identical(env$.prevcarga_tr_panel_hash(p1), env$.prevcarga_tr_panel_hash(fake_panel(O - 5L, fake_annex())))
  p2 <- p1; p2$holiday_annex$load <- data.table::copy(p2$holiday_annex$load); p2$holiday_annex$load[1L, load_mwh := 2]
  expect_false(identical(env$.prevcarga_tr_panel_hash(p2), env$.prevcarga_tr_panel_hash(p1)))
})

test_that("[AC4] infer: read_panel recebe holiday_window e span; sem o campo segue history_days$infer", {
  got <- NULL
  orig <- env$prevcarga_read_panel
  assign("prevcarga_read_panel", function(ctx, origin, areas, horizon_days, history_days, truncate_at = NULL,
                                          holiday_window = NULL) {
    got <<- list(h = history_days, w = holiday_window)
    p <- fake_panel(O - 3L, if (!is.null(holiday_window)) fake_annex() else NULL, to = LAST)
    p$attrs <- list(available_until = at(O), weather_issue_limit = at(O))
    p
  }, envir = env)
  withr::defer(assign("prevcarga_read_panel", orig, envir = env))
  cuts <- list(available_until = at(O), weather_issue_limit = at(O), deck_areas = "A")
  needs <- list(weather_forecast = list(segments = list()))
  spec <- fake_spec
  r1 <- env$prevcarga_infer_read_panel(fake_plugin(TRUE), spec, cuts, "A", O, list(), needs = needs)
  expect_identical(got$h, 1095L)
  expect_identical(got$w$mode, "ano_civil")
  expect_identical(names(r1$panel$holiday_annex), c("islands", "load", "weather_observed"))
  r2 <- env$prevcarga_infer_read_panel(fake_plugin(FALSE), spec, cuts, "A", O, list(), needs = needs)
  expect_identical(got$h, 5L)
  expect_null(got$w)
  expect_null(r2$panel$holiday_annex)
})

test_that("[AC4] infer: selecao de area desce no anexo preservando atributos; anexo alem do corte aborta", {
  ax <- fake_annex()
  ax2 <- fake_annex(); ax2$load[, area_code := "B"]; ax2$islands[, area_code := "B"]
  ax$load <- rbind(ax$load, ax2$load); ax$islands <- rbind(ax$islands, ax2$islands)
  data.table::setattr(ax$load, "tz", TZ)
  data.table::setattr(ax$load, "available_until", at(O))
  p <- env$.prevcarga_inf_select_area(fake_panel(O - 3L, ax, to = LAST), "A")
  expect_true(all(p$holiday_annex$load$area_code == "A"))
  expect_true(all(p$holiday_annex$islands$area_code == "A"))
  expect_identical(attr(p$holiday_annex$load, "tz"), TZ)
  expect_identical(attr(p$holiday_annex$load, "available_until"), at(O))
  cuts <- list(available_until = at(O))
  expect_identical(env$prevcarga_infer_panel(p, cuts, fake_spec), p)
  bad <- p
  bad$holiday_annex$load <- rbind(bad$holiday_annex$load,
                                  data.table(area_code = "A", interval_start = at(O, "00:30"),
                                             interval_end = at(O, "01:00"), load_mwh = 1))
  expect_identical(fail(env$prevcarga_infer_panel(bad, cuts, fake_spec))$id, "contract.panel_beyond_cut")
})

## --- despachante -------------------------------------------------------------------------------------------------
half_hour_load <- function(day, area = "A") {
  s <- at(day) + lubridate::minutes(30L * (0:47))
  data.table(area_code = area, interval_start = s, interval_end = s + lubridate::minutes(30L), load_mwh = seq_along(s) * 1)
}
half_hour_obs <- function(day, area = "A") {
  s <- at(day) + lubridate::minutes(30L * (0:47))
  data.table(area_code = area, valid_time = s, temperature = seq_along(s) / 10)
}
disp_panel <- function(extra_day = NULL) {
  ad <- lubridate::ymd("2019-06-20")
  alo <- half_hour_load(ad); aob <- half_hour_obs(ad)
  if (!is.null(extra_day)) { alo <- rbind(alo, half_hour_load(extra_day)); aob <- rbind(aob, half_hour_obs(extra_day)) }
  data.table::setattr(alo, "tz", TZ)
  list(load = half_hour_load(LAST), weather_observed = half_hour_obs(LAST),
       weather_forecast = data.table(area_code = "A", issue_time = at(LAST), valid_time = at(O), temperature = 20),
       holidays = data.table(area_code = "A", date = O, prevcarga_code = 0L),
       holiday_annex = list(islands = data.table(area_code = "A", island_id = 1L, from = ad, to = ad,
                                                 holiday_dates = list(ad)),
                            load = alo, weather_observed = aob),
       attrs = list(tz = TZ, origin = O))
}
disp_cuts <- function() list(origin_day = O, forecast_origin = O, origin = O, tz = TZ, available_until = at(O),
                             last_data_day = LAST, train_origin = O)

test_that("[AC5] to_hourly reamostra a carga e o clima do anexo; ilhas intactas", {
  p <- disp_panel()
  h <- env$to_hourly(p, weather = TRUE)
  expect_equal(nrow(h$holiday_annex$load), 24L)
  expect_equal(nrow(h$holiday_annex$weather_observed), 24L)
  expect_identical(h$holiday_annex$load, env$.prevcarga_hourly_load(p$holiday_annex$load))
  expect_identical(h$holiday_annex$weather_observed, env$.prevcarga_hourly_weather(p$holiday_annex$weather_observed, "area_code"))
  expect_identical(h$holiday_annex$islands, p$holiday_annex$islands)
  expect_identical(attr(h$holiday_annex$load, "tz"), TZ)
  ## weather = FALSE: so a carga do anexo vira horaria
  h2 <- env$to_hourly(p, weather = FALSE)
  expect_equal(nrow(h2$holiday_annex$load), 24L)
  expect_identical(h2$holiday_annex$weather_observed, p$holiday_annex$weather_observed)
})

test_that("[AC5] entrada dos estagios: carga do anexo horaria e deslocada como panel$load", {
  d <- fake_plugin(TRUE)$descriptor
  m <- env$.prevcarga_plugin_stage_input(disp_panel(), disp_cuts(), d, fake_spec, "train")
  hourly_main <- env$.prevcarga_hourly_load(disp_panel()$load)
  hourly_ann <- env$.prevcarga_hourly_load(disp_panel()$holiday_annex$load)
  delta_main <- as.numeric(m$panel$load$interval_start[1L]) - as.numeric(hourly_main$interval_start[1L])
  delta_ann <- as.numeric(m$panel$holiday_annex$load$interval_start[1L]) - as.numeric(hourly_ann$interval_start[1L])
  expect_equal(nrow(m$panel$holiday_annex$load), 24L)
  expect_identical(delta_ann, delta_main)
  expect_false(identical(delta_ann, 0))                                          # houve deslocamento (offset 60)
  expect_equal(as.numeric(m$panel$holiday_annex$load$interval_end - m$panel$holiday_annex$load$interval_start,
                          units = "mins"), rep(60, 24L))
  expect_identical(m$panel$holiday_annex$weather_observed, env$.prevcarga_hourly_weather(
    disp_panel()$holiday_annex$weather_observed, "area_code"))                    # clima nao desloca
  ## painel sem anexo percorre o caminho de antes
  p0 <- disp_panel(); p0$holiday_annex <- NULL
  m0 <- env$.prevcarga_plugin_stage_input(p0, disp_cuts(), d, fake_spec, "train")
  expect_null(m0$panel$holiday_annex)
})

test_that("[AC5] dia do anexo depois de last_data_day aborta plugin.train_beyond_cut nomeando holiday_annex.load", {
  d <- fake_plugin(TRUE)$descriptor
  expect_true(env$.prevcarga_plugin_check_train_panel(disp_panel(), disp_cuts(), d))
  e <- fail(env$.prevcarga_plugin_check_train_panel(disp_panel(extra_day = O), disp_cuts(), d))
  expect_identical(e$id, "plugin.train_beyond_cut")
  expect_match(e$where, "holiday_annex.load", fixed = TRUE)
})

test_that("[AC4] fachadas de treino e inferencia nao citam nome de modelo", {
  for (f in c("training.R", "inference.R")) {
    src <- readLines(file.path(pc_root(), "scripts", "lib", f), warn = FALSE)
    expect_false(any(grepl("dessem", src, ignore.case = TRUE)), info = f)
  }
})
