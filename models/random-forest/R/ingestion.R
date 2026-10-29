## ingestion.R -- estagio de ingestao do plugin random-forest (RF.1). SO definicoes.
## Recebe o painel do leitor unico e os `cuts` de dates.R e devolve o `rf_frame` (carga horaria por (area, dia, hora),
## safra de clima por origem nativa com LOCF e feriados). O plugin CONFERE os cortes (nunca corta, filtra nem preenche em
## silencio) e nao faz aritmetica de hora do rotulo: a agregacao horaria e de to_hourly (features.R), o deslocamento de
## modelagem e do despachante. Dias = as_date(interval_start, tz = cuts$tz). Sem relogio, sem variavel de ambiente.
## Porte de prevcargadessem3/models/random-forest/R/ingestion/data_adapter.R (somente leitura).

.rf_origin <- function(cuts) if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin

## Conferencia (nao corte) do painel; cada caso aborta nomeando o primeiro ofensor. `phase`: "train" | "infer".
.rf_check_panel <- function(panel, cuts, phase) {
  ld <- panel$load; wx <- panel$weather_forecast
  need <- function(x, cols, ds) {
    if (!is.data.frame(x)) .rf_abort("plugin.missing_column", details = list(dataset = ds))
    miss <- setdiff(cols, names(x))
    if (length(miss)) .rf_abort("plugin.missing_column", details = list(dataset = ds, column = miss[[1L]]))
  }
  bad_col <- intersect(c("timestamp", "load_label"), names(ld))
  if (length(bad_col)) .rf_abort("plugin.raw_timestamp", details = list(dataset = "load", column = bad_col[[1L]]))
  need(ld, c("area_code", "interval_start", "interval_end", "load_mwh"), "load")
  need(wx, c("area_code", "issue_time", "valid_time", "temperature"), "weather_forecast")
  for (col in c("interval_start", "interval_end"))
    if (!inherits(ld[[col]], "POSIXct") || !identical(lubridate::tz(ld[[col]]), cuts$tz))
      .rf_abort("plugin.raw_timestamp", details = list(dataset = "load", column = col, tz = cuts$tz))
  if (!inherits(wx$valid_time, "POSIXct") || !identical(lubridate::tz(wx$valid_time), cuts$tz))
    .rf_abort("plugin.raw_timestamp", details = list(dataset = "weather_forecast", column = "valid_time", tz = cuts$tz))
  if (identical(phase, "train")) {
    over <- which(lubridate::as_date(ld$interval_start, tz = cuts$tz) > cuts$last_data_day)
    if (length(over))
      .rf_abort("plugin.train_beyond_cut", details = list(primeiro = format(min(ld$interval_start[over])),
                                                          last_data_day = format(cuts$last_data_day)))
  } else {
    over <- which(ld$interval_end > cuts$available_until)
    if (length(over))
      .rf_abort("plugin.infer_beyond_cut", details = list(primeiro = format(min(ld$interval_end[over])),
                                                          available_until = format(cuts$available_until)))
  }
  over <- which(wx$issue_time > cuts$weather_issue_limit)
  if (length(over))
    .rf_abort("plugin.weather_beyond_limit", details = list(issue_time = format(min(wx$issue_time[over]), tz = "UTC"),
                                                            weather_issue_limit = format(cuts$weather_issue_limit)))
  invisible(TRUE)
}

## Carga horaria por to_hourly (fundacao: hora = media dos intervalos H:00 e H:30; hora incompleta descartada) na janela
## [first_day, last_day]. Buraco (area x dia sem as 24 horas) => plugin.completude; carga que comeca depois de
## first_day => plugin.window.
.rf_hourly_load <- function(load30, cuts, first_day, last_day) {
  ld <- data.table::as.data.table(load30)
  tz <- cuts$tz
  stub <- list(load = ld,
    weather_observed = data.table::data.table(area_code = character(), valid_time = ld$interval_start[0L],
                                              temperature = numeric()),
    weather_forecast = data.table::data.table(area_code = character(),
                                              issue_time = lubridate::with_tz(ld$interval_start[0L], "UTC"),
                                              valid_time = ld$interval_start[0L], temperature = numeric()),
    attrs = list(tz = tz))
  h <- data.table::as.data.table(.rf_foundation("to_hourly")(stub)$load)
  h[, `:=`(dia = lubridate::as_date(interval_start, tz = tz), hora = lubridate::hour(interval_start))]
  if (!nrow(h) || min(h$dia) > first_day)
    .rf_abort("plugin.window", details = list(primeiro_exigido = format(first_day),
      primeiro_presente = if (nrow(h)) format(min(h$dia)) else NA_character_))
  h <- h[dia >= first_day & dia <= last_day]
  grid <- data.table::CJ(area_code = sort(unique(ld$area_code)), dia = seq(first_day, last_day, by = "day"))
  cnt <- h[, list(N = .N), by = list(area_code, dia)][grid, on = c("area_code", "dia")]
  gap <- cnt[is.na(N) | N != 24L]
  if (nrow(gap))
    .rf_abort("plugin.completude", details = list(area = gap$area_code[[1L]], dia = format(gap$dia[[1L]]),
                                                   horas = if (is.na(gap$N[[1L]])) 0L else gap$N[[1L]]))
  out <- h[, list(area = area_code, dia, hora, load_mw = load_mwh)]
  data.table::setorder(out, area, dia, hora)
  out[]
}

## Feriados: uniao (sem fallback silencioso) das datas do painel para a area com a lista legada do oraculo.
.rf_holidays <- function(panel, area) {
  h <- panel$holidays
  if (!is.data.frame(h) || !all(c("area_code", "date") %in% names(h)))
    .rf_abort("plugin.missing_column", details = list(dataset = "holidays", column = "date"))
  d <- lubridate::as_date(h$date[h$area_code == area])
  sort(unique(c(d, plugin_constants()$holidays_legacy)))
}

## Fase: cuts$phase quando informada pelo chamador; senao carga com dia > last_data_day so existe no infer.
.rf_phase <- function(panel, cuts) {
  if (!is.null(cuts$phase)) return(cuts$phase)
  ld <- panel$load
  infer <- is.data.frame(ld) && "interval_start" %in% names(ld) && nrow(ld) > 0L &&
    any(lubridate::as_date(ld$interval_start, tz = cuts$tz) > cuts$last_data_day)
  if (infer) "infer" else "train"
}

## Estagio: (panel, cuts, spec) -> rf_frame. Janelas, H e historico vem de `spec` (run.yaml) e `cuts`, nunca literais.
##   train: carga [train_origin - history_days.train, last_data_day]; origens nativas
##          seq(train_origin - history_days.train + H, train_origin).
##   infer: carga [O - history_days.infer, O-1] (o dia O nunca entra); safra sobre a janela de origens
##          seq(O - history_days.infer, O), o frame guarda c(O-1, O).
plugin_ingest <- function(panel, cuts, spec) {
  phase <- .rf_phase(panel, cuts)
  .rf_check_panel(panel, cuts, phase)
  area <- unique(panel$load$area_code)
  if (length(area) != 1L)
    .rf_abort("plugin.contract", details = list(reason = "painel deve ter uma unica area", n_areas = length(area)))
  H <- as.integer(spec$horizon_days)
  O <- .rf_origin(cuts)
  if (identical(phase, "train")) {
    first_day <- cuts$train_origin - spec$history_days$train
    last_day <- cuts$last_data_day
    origins <- seq(first_day + H, cuts$train_origin, by = "day")
    keep <- NULL
  } else {
    first_day <- O - spec$history_days$infer
    last_day <- O - 1L
    origins <- seq(first_day, O, by = "day")
    keep <- c(O - 1L, O)
  }
  load <- .rf_hourly_load(panel$load, cuts, first_day, last_day)
  wx <- data.table::as.data.table(panel$weather_forecast)[area_code == area]
  vint <- .rf_weather_vintage(wx, origins, cuts, H)
  lo <- .rf_weather_locf(vint, origins, H, phase, keep)
  weather <- lo$weather
  if (!is.null(keep)) {
    weather <- weather[origin %in% keep]
    origins <- keep
  } else origins <- setdiff(origins, lo$dropped_origins) |> lubridate::as_date()
  data.table::setcolorder(weather, c("origin", "day_of_horizon", "issue_time", "tmax", "tmin", "locf"))
  list(phase = phase, area = area, tz = cuts$tz, origins = origins,
       train_origin = if (identical(phase, "train")) cuts$train_origin else lubridate::as_date(NA),
       load = load, weather = weather, holidays = .rf_holidays(panel, area), dropped_origins = lo$dropped_origins)
}
