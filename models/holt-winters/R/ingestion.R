## ingestion.R -- estagio de ingestao do plugin Holt-Winters (HW.1). SO definicoes.
## Recebe o painel do leitor unico (M2) e os `cuts` de dates.R e devolve o deck interno do HW: CargaTempVer (105
## colunas, dia x H01..H48 por interval_start; TempMedia e T01..T48 = NA: HW univariado), CargaTempMed (6 colunas), o
## calendario do horizonte (horizon_calendar: DIA, FER, TIPO_FER, DS), a carga do dia O (so delimita a entrega) e metadados. O plugin CONFERE os cortes (nunca corta nem preenche) e nao faz conta de
## hora: o rotulo da carga e convertido uma vez na entrada (leitor) e o deslocamento de modelagem e do despachante.
## Unica aritmetica de tempo: o `+ hours(6)` do flag historico de horario de verao (HV). Datas por lubridate com tz
## explicito; sem variavel de ambiente, sem relogio, sem RNG.
## Porte de prevcargadessem3/models/holt-winters/R/ingestion/data_adapter.R (somente leitura).

.hw_origin <- function(cuts) if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin

## Conferencia de entrada (confere, nunca corta nem preenche). Devolve o dia civil (no fuso de cuts) de cada intervalo.
.hw_check_panel <- function(panel, cuts) {
  ld <- panel$load
  if (!is.data.frame(ld)) hw_abort("hw.input_column", "painel sem carga", list(dataset = "load"))
  if ("timestamp" %in% names(ld))
    hw_abort("hw.raw_timestamp", "coluna timestamp na carga; so o leitor converte o rotulo", list(column = "timestamp"))
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
  invisible(day)
}

## slot 1..48 do dia civil: H01 = 00:00-00:30 (por interval_start / valid_time, nunca por interval_end)
.hw_slot <- function(t, tz) {
  as.integer(format(t, "%H", tz = tz)) * 2L + as.integer(format(t, "%M", tz = tz)) %/% 30L + 1L
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
  base <- lubridate::make_datetime(lubridate::year(dias), lubridate::month(dias), lubridate::day(dias),
                                   k$hv_utc_hour, tz = "UTC")
  as.integer(format(base, "%z", tz = k$hv_tz) == "-0200")
}

.hw_lookup <- function(dias, hol, col) {
  v <- hol[[col]][match(dias, hol$Data)]
  v[is.na(v)] <- 0L
  as.integer(v)
}

## Horizonte (dias de entrega alem de O): de cuts, na falta pelo delivery_days do descriptor.
.hw_horizon_days <- function(cuts) {
  h <- cuts$horizon_days
  if (is.null(h)) h <- plugin_descriptor()$delivery_days - 1L
  as.integer(h)
}

## Calendario do dia O ao dia O+H: so Date + inteiro (nenhuma hora); FER/TIPO_FER do calendario de feriados.
.hw_horizon_calendar <- function(hol, origin, horizon_days) {
  dias <- origin + 0:horizon_days
  data.frame(DIA = dias, FER = .hw_lookup(dias, hol, "Feriado"), TIPO_FER = .hw_lookup(dias, hol, "TIPO_FER"),
             DS = as.integer(lubridate::wday(dias)), stringsAsFactors = FALSE)
}

.hw_ctm <- function(ctv) {
  data.frame(data = ctv$Data, feriado = ctv$Feriado, diasemana = ctv$DiaSemana, Cmed = ctv$CargaMedia,
             Tmed = ctv$TempMedia, TIPO_FER = ctv$TIPO_FER)
}

.hw_meta <- function(panel, cuts, origin) {
  area <- unique(as.character(panel$load$area_code))
  list(area = if (length(area) == 1L) area else NA_character_, origin_day = origin,
       last_data_day = cuts$last_data_day, train_origin = lubridate::as_date(cuts$train_origin), tz = cuts$tz)
}

## panel, cuts -> deck interno (assinatura canonica do despachante)
plugin_ingest <- function(panel, cuts) {
  day <- .hw_check_panel(panel, cuts)
  origin <- .hw_origin(cuts)
  ld <- data.table::as.data.table(panel$load)
  hist <- ld[day <= cuts$last_data_day]
  hol <- .hw_holidays(panel$holidays)
  wide_ld <- .hw_wide_by_slot(hist, "interval_start", "load_mwh", "H", cuts$tz)
  ctv <- wide_ld
  ## HW univariado: TempMedia e T01..T48 ficam NA (layout de 105 colunas preserva os indices fixos dos helpers)
  for (cc in c("TempMedia", hw_temp_cols())) data.table::set(ctv, j = cc, value = rep(NA_real_, nrow(ctv)))
  data.table::set(ctv, j = "CargaMedia", value = rowMeans(as.matrix(ctv[, hw_slot_cols(), with = FALSE]), na.rm = TRUE))
  data.table::set(ctv, j = "DiaSemana", value = as.integer(lubridate::wday(ctv$Data)))
  data.table::set(ctv, j = "HV", value = .hw_compute_hv(ctv$Data))
  data.table::set(ctv, j = "FHV15", value = rep(NA, nrow(ctv)))
  data.table::set(ctv, j = "DHV15", value = rep(NA, nrow(ctv)))
  data.table::set(ctv, j = "Feriado", value = .hw_lookup(ctv$Data, hol, "Feriado"))
  data.table::set(ctv, j = "TIPO_FER", value = .hw_lookup(ctv$Data, hol, "TIPO_FER"))
  data.table::setcolorder(ctv, hw_ctv_cols())
  hd <- .hw_horizon_days(cuts)
  list(CargaTempVer = as.data.frame(ctv), CargaTempMed = .hw_ctm(ctv),
       horizon_calendar = .hw_horizon_calendar(hol, origin, hd),
       load_day_o = data.table::data.table(interval_start = ld$interval_start[day == origin],
                                           interval_end = ld$interval_end[day == origin],
                                           load_mwh = ld$load_mwh[day == origin]),
       n_steps = hd, meta = .hw_meta(panel, cuts, origin))
}
