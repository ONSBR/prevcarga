## test-weather-locf.R -- RF.1: safra de clima por origem nativa e LOCF por day_of_horizon.
library(testthat)

P <- rf_source_plugin()
spec <- rf_fx_spec()
O <- RF_O
TZ <- RF_TZ

## Transcricao de .read_forecast_temp (data_adapter.R:215-289): filtra as emissoes dadas, day_of_horizon = dia(valid) - origem,
## max/min diario com Inf -> NA e LOCF por day_of_horizon sobre a grade completa. `issue_by_origin` = emissao esperada por origem.
oracle_forecast_temp <- function(fc, origin_days, issue_by_origin, horizon = 9L, tz = TZ) {
  w <- data.table::as.data.table(fc)
  w <- w[issue_time %in% issue_by_origin]
  w[, dataOrigem := origin_days[match(issue_time, issue_by_origin)]]
  w[, day_of_horizon := as.integer(lubridate::as_date(valid_time, tz = tz) - dataOrigem)]
  w <- w[day_of_horizon >= 1L & day_of_horizon <= horizon]
  dt <- w[, list(tmax = max(temperature, na.rm = TRUE), tmin = min(temperature, na.rm = TRUE)),
          by = list(dataOrigem, day_of_horizon)]
  dt[is.infinite(tmax), tmax := NA_real_][is.infinite(tmin), tmin := NA_real_]
  g <- data.table::CJ(dataOrigem = origin_days, day_of_horizon = seq_len(horizon))
  dt <- merge(g, dt, by = c("dataOrigem", "day_of_horizon"), all.x = TRUE)
  data.table::setorder(dt, day_of_horizon, dataOrigem)
  dt[, tmax := data.table::nafill(tmax, type = "locf"), by = day_of_horizon]
  dt[, tmin := data.table::nafill(tmin, type = "locf"), by = day_of_horizon]
  data.table::setorder(dt, dataOrigem, day_of_horizon)
  dt[]
}
utc <- function(day, h) lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::hours(h)
limit_T <- function(cuts, T) cuts$weather_issue_limit - lubridate::days(as.integer(cuts$forecast_origin) - as.integer(T))

for (rule in c("origin_midnight_utc", "available_until")) {
  hh <- if (rule == "origin_midnight_utc") 0L else 6L   # emissao esperada: 00Z de T / 06Z de T (available_until = 12:00Z)
  test_that(sprintf("[PLUGINS] weather_vintage: regra %s -- emissao %02dZ de T, day_of_horizon 1..9 e tmax/tmin = transcricao do oraculo", rule, hh), {
    cuts <- rf_fx_cuts(rule = rule, phase = "train")
    panel <- rf_fx_panel(cuts = cuts)
    fr <- P$plugin_ingest(panel, cuts, spec)
    w <- fr$weather
    expect_true(all(!w$locf))
    expect_identical(sort(unique(w$day_of_horizon)), 1:9)
    expect_identical(w$issue_time, utc(w$origin, hh))
    ## o limite da regra: weather_issue_limit(T) = limit - days(O - T); a emissao escolhida esta em (limit(T) - 1d, limit(T)]
    lim <- limit_T(cuts, w$origin)
    expect_true(all(w$issue_time <= lim & w$issue_time > lim - lubridate::days(1L)))
    ora <- oracle_forecast_temp(panel$weather_forecast, fr$origins, utc(fr$origins, hh))
    j <- merge(w, ora, by.x = c("origin", "day_of_horizon"), by.y = c("dataOrigem", "day_of_horizon"), suffixes = c("", ".o"))
    expect_equal(nrow(j), nrow(w))
    expect_lte(max(abs(j$tmax - j$tmax.o), abs(j$tmin - j$tmin.o)), 1e-12)
  })
}

test_that("[PLUGINS] weather_vintage: FORBIDDEN -- emissoes com issue_time > limit(T) nao mudam a safra de T (CONTROL: dentro da janela muda)", {
  cuts <- rf_fx_cuts(rule = "available_until", phase = "train")
  d <- rf_fx_store()
  fr0 <- P$plugin_ingest(rf_fx_panel(d, cuts), cuts, spec)
  T0 <- cuts$train_origin - 100L
  lim <- limit_T(cuts, T0)
  after <- rf_lib$prevcarga_fixture_perturb(d, "weather_forecast", issue_after = lim)
  fr1 <- P$plugin_ingest(rf_fx_panel(after, cuts), cuts, spec)
  expect_identical(fr1$weather[origin <= T0], fr0$weather[origin <= T0])
  expect_false(identical(fr1$weather[origin > T0 + 1L], fr0$weather[origin > T0 + 1L]))
  inside <- rf_lib$prevcarga_fixture_perturb(d, "weather_forecast", issue_after = lim - lubridate::days(1L), issue_at_or_before = lim)
  fr2 <- P$plugin_ingest(rf_fx_panel(inside, cuts), cuts, spec)
  expect_false(isTRUE(all.equal(fr2$weather[origin == T0]$tmax, fr0$weather[origin == T0]$tmax)))
})

test_that("[PLUGINS] carry_forward: origem sem emissao recebe LOCF da origem anterior por day_of_horizon (= gate10) e as 3 iniciais saem do treino", {
  cuts <- rf_fx_cuts(rule = "available_until", phase = "train")
  panel <- rf_fx_panel(cuts = cuts)
  fr0 <- P$plugin_ingest(panel, cuts, spec)
  first3 <- fr0$origins[1:3]
  Tm <- cuts$train_origin - 100L
  in_window <- function(T, x) x > limit_T(cuts, T) - lubridate::days(1L) & x <= limit_T(cuts, T)
  wf <- panel$weather_forecast
  keep <- !in_window(Tm, wf$issue_time)
  for (t in first3) keep <- keep & !in_window(t, wf$issue_time)
  p2 <- panel; p2$weather_forecast <- wf[keep]
  fr <- P$plugin_ingest(p2, cuts, spec)
  expect_identical(fr$dropped_origins, first3)
  expect_false(any(first3 %in% fr$origins))
  expect_false(any(first3 %in% fr$weather$origin))
  expect_identical(fr$origins, setdiff(fr0$origins, first3) |> lubridate::as_date())
  ## T sem emissao: locf == TRUE e igual a origem anterior (T-1) no mesmo day_of_horizon
  wm <- fr$weather[origin == Tm]; wp <- fr$weather[origin == Tm - 1L]
  expect_true(all(wm$locf))
  expect_true(all(is.na(wm$issue_time)))
  expect_identical(wm$tmax, wp$tmax); expect_identical(wm$tmin, wp$tmin)
  ## = transcricao do gate10_carry_forward (data_adapter.R:285-289) sobre o mesmo painel
  ora <- oracle_forecast_temp(p2$weather_forecast, fr0$origins, utc(fr0$origins, 6L))
  ora <- ora[!is.na(tmax)]
  j <- merge(fr$weather, ora, by.x = c("origin", "day_of_horizon"), by.y = c("dataOrigem", "day_of_horizon"), suffixes = c("", ".o"))
  expect_equal(nrow(j), nrow(fr$weather))
  expect_lte(max(abs(j$tmax - j$tmax.o), abs(j$tmin - j$tmin.o)), 1e-12)
  expect_identical(sum(fr$weather$locf), 9L)
  ## CONTROL: com a emissao presente locf == FALSE e o valor muda
  c0 <- fr0$weather[origin == Tm]
  expect_false(any(c0$locf))
  expect_false(isTRUE(all.equal(c0$tmax, wm$tmax)))
})

test_that("[PLUGINS] carry_forward: infer sem safra nem LOCF para O-1/O => [prevcarga/completude] exit 3 nomeando a origem", {
  cuts <- rf_fx_cuts(phase = "infer")
  panel <- rf_fx_panel(cuts = cuts)
  expect_null(fail(P$plugin_ingest(panel, cuts, spec)))
  p2 <- panel; p2$weather_forecast <- panel$weather_forecast[0L]
  e <- fail(P$plugin_ingest(p2, cuts, spec))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$id, "plugin.completude")
  expect_identical(e$class_name, "completude"); expect_identical(e$exit_code, 3L)
  expect_identical(e$details$origem, format(O - 1L))
  ## so O sem safra e sem origem anterior com safra (vintages da janela removidos de O-1 para tras): ainda completude, agora em O-1
  wf <- panel$weather_forecast
  p3 <- panel; p3$weather_forecast <- wf[issue_time > limit_T(cuts, O - 1L) - lubridate::days(0L) & issue_time <= cuts$weather_issue_limit]
  e3 <- fail(P$plugin_ingest(p3, cuts, spec))
  expect_identical(e3$id, "plugin.completude")
  expect_identical(e3$details$origem, format(O - 1L))
  ## CONTROL: LOCF de uma origem anterior com safra cobre O-1 (locf == TRUE) sem erro
  p4 <- panel
  p4$weather_forecast <- wf[!(issue_time > limit_T(cuts, O - 1L) - lubridate::days(1L) & issue_time <= limit_T(cuts, O - 1L))]
  fr <- P$plugin_ingest(p4, cuts, spec)
  expect_true(all(fr$weather[origin == O - 1L]$locf))
  expect_false(any(fr$weather[origin == O]$locf))
})
