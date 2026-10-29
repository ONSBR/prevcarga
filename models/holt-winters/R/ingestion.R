## ingestion.R -- estagio de ingestao do plugin Holt-Winters (HW.1). SO definicoes.
## Recebe o painel do leitor unico (M2) e os `cuts` de dates.R e devolve o deck interno do HW: CargaTempVer (105
## colunas, dia x H01..H48 por interval_start), CargaTempMed (6 colunas), TempPre/CDDPre do clima previsto, a carga do
## dia O (so delimita a entrega) e metadados. O plugin CONFERE os cortes (nunca corta nem preenche) e nao faz conta de
## hora: o rotulo da carga e convertido uma vez na entrada (leitor) e o deslocamento de modelagem e do despachante.
## Unica aritmetica de tempo: o `+ hours(6)` do flag historico de horario de verao (HV). Datas por lubridate com tz
## explicito; sem variavel de ambiente, sem relogio, sem RNG.
## Porte de prevcargadessem3/models/holt-winters/R/ingestion/data_adapter.R (somente leitura).

.hw_origin <- function(cuts) if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin

.hw_limit <- function(panel, cuts) {
  if (!is.null(cuts$weather_issue_limit)) cuts$weather_issue_limit else panel$attrs$weather_issue_limit
}

## Conferencia de entrada (confere, nunca corta nem preenche). Devolve o dia civil (no fuso de cuts) de cada intervalo.
.hw_check_panel <- function(panel, cuts) {
  ld <- panel$load
  if (!is.data.frame(ld)) hw_abort("hw.input_column", "painel sem carga", list(dataset = "load"))
  if ("timestamp" %in% names(ld))
    hw_abort("hw.raw_timestamp", "coluna timestamp na carga; so o leitor converte o rotulo", list(column = "timestamp"))
  wo <- panel$weather_observed
  if (is.data.frame(wo) && "timestamp" %in% names(wo))
    hw_abort("hw.raw_timestamp", "coluna timestamp no clima observado; use valid_time", list(column = "timestamp"))
  miss <- setdiff(c("interval_start", "interval_end", "load_mwh"), names(ld))
  if (length(miss))
    hw_abort("hw.input_column", sprintf("coluna %s ausente na carga", miss[[1L]]), list(columns = miss))
  for (col in c("interval_start", "interval_end")) {
    v <- ld[[col]]
    if (!inherits(v, "POSIXct") || !identical(lubridate::tz(v), cuts$tz))
      hw_abort("hw.input_column", sprintf("%s fora do fuso de cuts (%s)", col, cuts$tz), list(column = col))
  }
  origin <- .hw_origin(cuts)
  day <- lubridate::as_date(ld$interval_start, tz = cuts$tz)
  au <- cuts$available_until
  at_o <- day == origin
  bad <- day > origin | (if (is.null(au)) at_o else at_o & ld$interval_end > au)
  if (any(bad))
    hw_abort("hw.train_day_o", sprintf("intervalo %s fora do corte (dia O=%s)",
                                       format(min(ld$interval_start[bad])), format(origin)),
             list(interval_start = format(min(ld$interval_start[bad]))))
  n_last <- sum(day == cuts$last_data_day)
  if (n_last != 48L)
    hw_abort("hw.last_day_incomplete", sprintf("dia %s com %d intervalos de 48", format(cuts$last_data_day), n_last),
             list(day = format(cuts$last_data_day), n = n_last))
  wf <- panel$weather_forecast
  if (NROW(wf)) {
    if (!all(c("issue_time", "valid_time", "temperature") %in% names(wf)))
      hw_abort("hw.input_column", "clima previsto sem issue_time/valid_time/temperature",
               list(dataset = "weather_forecast"))
    lim <- .hw_limit(panel, cuts)
    if (!inherits(lim, "POSIXct") || length(lim) != 1L || is.na(lim))
      hw_abort("hw.input_column", "cuts$weather_issue_limit ausente", list(column = "weather_issue_limit"))
    over <- wf$issue_time > lim
    if (any(over))
      hw_abort("hw.weather_beyond_cut", sprintf("issue_time=%s depois de weather_issue_limit=%s",
                                                format(max(wf$issue_time[over])), format(lim)),
               list(issue_time = format(max(wf$issue_time[over]))))
  }
  if (NROW(wo) && !all(c("valid_time", "temperature") %in% names(wo)))
    hw_abort("hw.input_column", "clima observado sem valid_time/temperature", list(dataset = "weather_observed"))
  invisible(day)
}

## slot 1..48 do dia civil: H01 = 00:00-00:30 (por interval_start / valid_time, nunca por interval_end)
.hw_slot <- function(t, tz) {
  t <- lubridate::with_tz(t, tz)
  lubridate::hour(t) * 2L + lubridate::minute(t) %/% 30L + 1L
}

## dia x 48 slots; `x` tem Data, slot, v. Media por (Data, slot); slot ausente = NA (nunca 0).
.hw_dcast48 <- function(x, prefix) {
  cols <- sprintf("%s%02d", prefix, 1:48)
  if (!nrow(x)) {
    w <- data.table::data.table(Data = lubridate::ymd(character()))
  } else {
    w <- data.table::dcast(x, Data ~ slot, value.var = "v", fun.aggregate = mean)
    present <- setdiff(names(w), "Data")
    data.table::setnames(w, present, sprintf("%s%02d", prefix, as.integer(present)))
  }
  for (cc in setdiff(cols, names(w))) data.table::set(w, j = cc, value = rep(NA_real_, nrow(w)))
  data.table::setcolorder(w, c("Data", cols))
  data.table::setorderv(w, "Data")
  w
}

.hw_wide_by_slot <- function(dt, time_col, value_col, prefix, tz) {
  x <- data.table::data.table(Data = lubridate::as_date(dt[[time_col]], tz = tz),
                              slot = .hw_slot(dt[[time_col]], tz), v = as.double(dt[[value_col]]))
  .hw_dcast48(x, prefix)
}

## simplified_code -> Feriado; holiday_type_id -> TIPO_FER (uma linha por data)
.hw_holidays <- function(h) {
  if (!NROW(h)) return(data.table::data.table(Data = lubridate::ymd(character()), Feriado = integer(), TIPO_FER = integer()))
  out <- data.table::data.table(Data = lubridate::as_date(h$date), Feriado = as.integer(h$simplified_code),
                                TIPO_FER = as.integer(h$holiday_type_id))
  unique(out, by = "Data")
}

## Flag historico de horario de verao, em lubridate: o oraculo avalia o fuso de Sao Paulo as 06:00 UTC do dia e testa
## "-02"; no fuso de Sao Paulo "-02" e exatamente o horario de verao.
.hw_compute_hv <- function(dias) {
  k <- plugin_constants()
  base <- lubridate::force_tz(lubridate::as_datetime(dias), "UTC") + lubridate::hours(k$hv_shift_hours)
  as.integer(lubridate::dst(lubridate::with_tz(base, k$hv_tz)))
}

.hw_lookup <- function(dias, hol, col) {
  v <- hol[[col]][match(dias, hol$Data)]
  v[is.na(v)] <- 0L
  as.integer(v)
}

## Horizonte (dias de entrega alem de O): de cuts, na falta pelo que o leitor entregou no previsto.
.hw_horizon_days <- function(panel, cuts, origin) {
  h <- cuts$horizon_days
  if (!is.null(h)) return(as.integer(h))
  wf <- panel$weather_forecast
  if (NROW(wf)) return(as.integer(max(lubridate::as_date(wf$valid_time, tz = cuts$tz)) - origin))
  hw_abort("hw.input_column", "cuts$horizon_days ausente e sem clima previsto", list(column = "horizon_days"))
}

## TempPre/CDDPre do dia O ao dia O+H sobre as emissoes que o leitor ja filtrou pela regra weather_issue_cutoff.
## Por (dia, slot) fica a emissao mais fresca. Sem nenhuma emissao: fallback do oraculo (NA, FER do calendario).
.hw_horizon_weather <- function(wf, hol, cuts, origin, horizon_days) {
  dias <- origin + 0:horizon_days
  wf <- if (NROW(wf)) data.table::as.data.table(wf)[lubridate::as_date(valid_time, tz = cuts$tz) %in% dias] else NULL
  if (is.null(wf) || !nrow(wf)) {
    hw_warn("hw.weather_absent", sprintf("area sem previsao no horizonte %s a %s", format(dias[[1L]]), format(dias[[length(dias)]])),
            list(origin_day = format(origin)))
    tp <- data.table::data.table(DIA = dias)
    for (cc in hw_slot_cols()) data.table::set(tp, j = cc, value = rep(NA_real_, length(dias)))
    data.table::set(tp, j = "FER", value = .hw_lookup(dias, hol, "Feriado"))
    cdd <- data.frame(DIA = dias, TEMP = NA_real_, TOT = NA_real_, DS = as.integer(lubridate::wday(dias)),
                      TIPO_FER = .hw_lookup(dias, hol, "TIPO_FER"))
    return(list(TempPre = as.data.frame(tp), CDDPre = cdd, n_steps = as.integer(horizon_days), weather_absent = TRUE))
  }
  data.table::set(wf, j = "DIA", value = lubridate::as_date(wf$valid_time, tz = cuts$tz))
  data.table::set(wf, j = "slot", value = .hw_slot(wf$valid_time, cuts$tz))
  data.table::setorderv(wf, c("DIA", "slot", "issue_time"), c(1L, 1L, -1L))
  wf <- unique(wf, by = c("DIA", "slot"))
  tp <- .hw_dcast48(data.table::data.table(Data = wf$DIA, slot = wf$slot, v = as.double(wf$temperature)), "H")
  data.table::setnames(tp, "Data", "DIA")
  data.table::set(tp, j = "FER", value = .hw_lookup(tp$DIA, hol, "Feriado"))
  cd <- wf[, list(TEMP = mean(temperature, na.rm = TRUE), TOT = sum(temperature, na.rm = TRUE)), by = "DIA"]
  data.table::setorderv(cd, "DIA")
  cdd <- data.frame(DIA = cd$DIA, TEMP = cd$TEMP, TOT = cd$TOT, DS = as.integer(lubridate::wday(cd$DIA)),
                    TIPO_FER = .hw_lookup(cd$DIA, hol, "TIPO_FER"))
  list(TempPre = as.data.frame(tp), CDDPre = cdd, n_steps = as.integer(sum(unique(wf$DIA) > origin)),
       weather_absent = FALSE)
}

.hw_ctm <- function(ctv) {
  data.frame(data = ctv$Data, feriado = ctv$Feriado, diasemana = ctv$DiaSemana, Cmed = ctv$CargaMedia,
             Tmed = ctv$TempMedia, TIPO_FER = ctv$TIPO_FER)
}

.hw_meta <- function(panel, cuts, origin) {
  area <- unique(as.character(panel$load$area_code))
  list(area = if (length(area) == 1L) area else NA_character_, origin_day = origin,
       last_data_day = cuts$last_data_day, train_origin = lubridate::as_date(cuts$train_origin), tz = cuts$tz,
       weather_issue_cutoff = if (is.null(cuts$weather_issue_cutoff)) NA_character_ else cuts$weather_issue_cutoff)
}

## panel, cuts -> deck interno (assinatura canonica do despachante)
plugin_ingest <- function(panel, cuts) {
  day <- .hw_check_panel(panel, cuts)
  origin <- .hw_origin(cuts)
  ld <- data.table::as.data.table(panel$load)
  hist <- ld[day <= cuts$last_data_day]
  hol <- .hw_holidays(panel$holidays)
  wide_ld <- .hw_wide_by_slot(hist, "interval_start", "load_mwh", "H", cuts$tz)
  wo <- panel$weather_observed
  wide_tp <- if (NROW(wo)) {
    wo <- data.table::as.data.table(wo)
    .hw_wide_by_slot(wo[lubridate::as_date(valid_time, tz = cuts$tz) <= cuts$last_data_day],
                     "valid_time", "temperature", "T", cuts$tz)
  } else .hw_dcast48(data.table::data.table(Data = lubridate::ymd(character()), slot = integer(), v = double()), "T")
  ctv <- wide_tp[wide_ld, on = "Data"]
  tm <- rowMeans(as.matrix(ctv[, hw_temp_cols(), with = FALSE]), na.rm = TRUE)
  tm[is.nan(tm)] <- NA_real_
  data.table::set(ctv, j = "TempMedia", value = tm)
  data.table::set(ctv, j = "CargaMedia", value = rowMeans(as.matrix(ctv[, hw_slot_cols(), with = FALSE]), na.rm = TRUE))
  data.table::set(ctv, j = "DiaSemana", value = as.integer(lubridate::wday(ctv$Data)))
  data.table::set(ctv, j = "HV", value = .hw_compute_hv(ctv$Data))
  data.table::set(ctv, j = "FHV15", value = rep(NA, nrow(ctv)))
  data.table::set(ctv, j = "DHV15", value = rep(NA, nrow(ctv)))
  data.table::set(ctv, j = "Feriado", value = .hw_lookup(ctv$Data, hol, "Feriado"))
  data.table::set(ctv, j = "TIPO_FER", value = .hw_lookup(ctv$Data, hol, "TIPO_FER"))
  data.table::setcolorder(ctv, hw_ctv_cols())
  hz <- .hw_horizon_weather(panel$weather_forecast, hol, cuts, origin, .hw_horizon_days(panel, cuts, origin))
  list(CargaTempVer = as.data.frame(ctv), CargaTempMed = .hw_ctm(ctv), TempPre = hz$TempPre, CDDPre = hz$CDDPre,
       load_day_o = data.table::data.table(interval_start = ld$interval_start[day == origin],
                                           interval_end = ld$interval_end[day == origin],
                                           load_mwh = ld$load_mwh[day == origin]),
       n_steps = hz$n_steps, weather_absent = hz$weather_absent, meta = .hw_meta(panel, cuts, origin))
}
