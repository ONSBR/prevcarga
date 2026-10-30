## ingestion.R -- estagio de ingestao do plugin LGBM (LGBM.1). SO definicoes.
## Recebe o painel do leitor unico (M2.7) e os `cuts` de dates.R (M2.3) e devolve o quadro interno de 48 slots por dia:
## DataHora (= interval_start, no fuso de cuts$tz), Data, Hora (0..47), Minuto, CargaGlobal, is_synthetic, Temperatura,
## com o vetor de feriados em attr(frame, "feriados"). O plugin CONFERE os cortes recebidos (nunca corta nem preenche) e
## nao faz aritmetica de hora (ADR-0019: o despachante aplica o offset do descriptor). Sem variavel de ambiente, sem
## relogio. Datas por lubridate com tz explicito. Os rotulos de calendario nascem na FE (LGBM.2), nao aqui.
## Porte de prevcargadessem3/models/lgbm/R/ingestion/data_adapter.R (somente leitura).

.lgbm_origin <- function(cuts) if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin

## Coluna/dataset exigido pelo `requires` do descriptor; ausente => lgbm.contract "requires ausente: <nome>".
.lgbm_require <- function(x, cols, dataset) {
  if (!is.data.frame(x)) lgbm_abort("lgbm.contract", sprintf("requires ausente: %s", dataset))
  miss <- setdiff(cols, names(x))
  if (length(miss)) lgbm_abort("lgbm.contract", sprintf("requires ausente: %s (%s)", miss[[1L]], dataset))
  invisible(TRUE)
}

.lgbm_check_tz <- function(x, col, cuts, dataset) {
  v <- x[[col]]
  if (!inherits(v, "POSIXct") || !identical(lubridate::tz(v), cuts$tz))
    lgbm_abort("lgbm.contract", sprintf("%s$%s fora do fuso de cuts (%s)", dataset, col, cuts$tz))
  invisible(TRUE)
}

## Confere (nunca corta nem preenche) colunas, fuso e cortes do painel recebido.
lgbm_check_panel <- function(panel, cuts, mode = c("train", "infer")) {
  mode <- match.arg(mode)
  load <- panel$load
  .lgbm_require(load, c("area_code", "interval_start", "interval_end", "load_mwh"), "load")
  if ("timestamp" %in% names(load)) lgbm_abort("lgbm.contract", "coluna timestamp fora do leitor")
  wf <- panel$weather_forecast
  .lgbm_require(wf, c("area_code", "valid_time", "issue_time", "temperature"), "weather_forecast")
  .lgbm_require(panel$holidays, c("date", "prevcarga_code"), "holidays")
  if (!is.character(cuts$tz) || length(cuts$tz) != 1L) lgbm_abort("lgbm.contract", "cuts$tz ausente")
  .lgbm_check_tz(load, "interval_start", cuts, "load")
  .lgbm_check_tz(load, "interval_end", cuts, "load")
  .lgbm_check_tz(wf, "valid_time", cuts, "weather_forecast")
  if (mode == "train") {
    lim_day <- cuts$last_data_day
    if (!inherits(lim_day, "Date") || length(lim_day) != 1L || is.na(lim_day))
      lgbm_abort("lgbm.contract", "cuts$last_data_day ausente")
    bad <- which(lubridate::as_date(load$interval_start, tz = cuts$tz) > lim_day)
    if (length(bad))
      lgbm_abort("lgbm.contract", sprintf("painel de treino alem de last_data_day=%s: %s", format(lim_day),
                                          format(min(load$interval_start[bad]))))
  } else {
    au <- cuts$available_until
    if (is.null(au)) lgbm_abort("lgbm.contract", "cuts$available_until ausente")
    bad <- which(load$interval_end > au)
    if (length(bad))
      lgbm_abort("lgbm.contract", sprintf("intervalo alem de available_until=%s: interval_end=%s", format(au),
                                          format(min(load$interval_end[bad]))))
  }
  lim <- if (!is.null(cuts$weather_issue_limit)) cuts$weather_issue_limit else panel$attrs$weather_issue_limit
  if (!inherits(lim, "POSIXct") || length(lim) != 1L || is.na(lim))
    lgbm_abort("lgbm.contract", "cuts$weather_issue_limit ausente")
  bad <- which(wf$issue_time > lim)
  if (length(bad))
    lgbm_abort("lgbm.contract", sprintf("emissao alem de weather_issue_limit=%s: issue_time=%s", format(lim),
                                        format(max(wf$issue_time[bad]))))
  invisible(TRUE)
}

## Grade de 48 slots por dia; a carga entra por merge no mesmo DataHora (= interval_start). Dia de historia sem os 48
## intervalos => lgbm.completude nomeando o dia (nunca preenche). Infer: intervalo com interval_end > available_until
## e sintetico (CargaGlobal = NA); a auto-deteccao do oraculo (data_adapter.R:653-667) NAO e portada.
lgbm_load_grid <- function(load, days, cuts, mode = c("train", "infer")) {
  mode <- match.arg(mode)
  tz <- cuts$tz
  days <- sort(unique(days))
  O <- .lgbm_origin(cuts)
  if (anyDuplicated(load$interval_start))
    lgbm_abort("lgbm.contract", sprintf("carga com interval_start duplicado: %s",
                                        format(load$interval_start[anyDuplicated(load$interval_start)])))
  grid <- data.table::CJ(Data = days, Hora = 0:47)
  ## indice de slot (Hora 0..47) sobre a meia-noite civil do dia: nao e deslocamento de rotulo da carga
  grid[, Minuto := (Hora %% 2L) * 30L]
  grid[, DataHora := lubridate::force_tz(lubridate::as_datetime(Data), tz) + lubridate::hours(Hora %/% 2L) +
         lubridate::minutes(Minuto)]
  ld <- data.table::data.table(DataHora = load$interval_start, interval_end = load$interval_end,
                               CargaGlobal = load$load_mwh)
  dt <- merge(grid, ld, by = "DataHora", all.x = TRUE)
  data.table::setorder(dt, DataHora)
  hist_end <- if (mode == "infer") O else max(days) + 1L
  n_hist <- dt[Data < hist_end, list(n = sum(!is.na(CargaGlobal))), by = Data]
  inc <- n_hist[n != 48L]
  if (nrow(inc))
    lgbm_abort("lgbm.completude", sprintf("dia %s com %d intervalos de 48", format(inc$Data[[1L]]), inc$n[[1L]]))
  dt[, is_synthetic := FALSE]
  if (mode == "infer") {
    dt[, is_synthetic := is.na(interval_end) | interval_end > cuts$available_until]
    holes <- dt[Data == O & !is_synthetic & is.na(CargaGlobal)]
    if (nrow(holes))
      lgbm_abort("lgbm.completude", sprintf("dia %s sem carga no intervalo %s (antes de available_until)",
                                            format(O), format(holes$DataHora[[1L]])))
    dt[is_synthetic == TRUE, CargaGlobal := NA_real_]
    for (d in c(O, O + 1L)) {
      n <- dt[Data == d, .N]
      if (n != 48L) lgbm_abort("lgbm.completude", sprintf("dia %s com %d linhas de 48", format(d), n))
    }
  }
  dt[, list(DataHora, Data, Hora, Minuto, CargaGlobal, is_synthetic)]
}

## Safra de clima do oraculo (TemperaturaPrevistaDelay): do clima da area (heat_area), so emissoes com
## valid_day - issue_day >= lead; por (Data, Hora) a emissao mais recente. O dia da emissao e o dia em `issue_day_tz`
## (UTC no oraculo, as.Date(issue_time)); o dia valido e o dia civil no fuso da carga. Sem deslocamento de hora.
lgbm_weather_vintage <- function(weather, area, days, lead, consts = plugin_constants(), tz = NULL) {
  heat <- unname(consts$heat_area[area])
  if (length(heat) != 1L || is.na(heat)) lgbm_abort("lgbm.contract", sprintf("area %s sem heat_area", area))
  w <- data.table::as.data.table(weather)
  w <- w[area_code == heat]
  if (is.null(tz)) tz <- lubridate::tz(w$valid_time)
  w[, valid_day := lubridate::as_date(valid_time, tz = tz)]
  w[, issue_day := lubridate::as_date(lubridate::with_tz(issue_time, consts$issue_day_tz))]
  w <- w[as.integer(valid_day - issue_day) >= as.integer(lead) & valid_day %in% days]
  w[, `:=`(DataHora = valid_time, Data = valid_day,
           Hora = lubridate::hour(valid_time) * 2L + lubridate::minute(valid_time) %/% 30L)]
  data.table::setorder(w, Data, Hora, -issue_time)
  w <- w[!duplicated(w, by = c("Data", "Hora"))]
  w[, list(DataHora, Temperatura = temperature)]
}

## Feriados: prevcarga_code >= 1 do deck da area + lista fixa do oraculo; datas civis sem deslocamento.
lgbm_holidays <- function(holidays, consts = plugin_constants(), area = NULL) {
  h <- data.table::as.data.table(holidays)
  if (!is.null(area) && "area_code" %in% names(h)) h <- h[area_code == area]
  d <- lubridate::as_date(h$date[which(h$prevcarga_code >= 1L)])
  sort(unique(c(d, consts$holidays_fixed)))
}

## Janela minima: infer precisa de min_history_days_infer dias completos antes de O; treino (com spec) de history_days$train.
.lgbm_check_window <- function(load, cuts, mode, spec, consts) {
  n <- load[, list(n = .N), by = list(Data = lubridate::as_date(interval_start, tz = cuts$tz))]
  if (mode == "infer") {
    have <- sum(n$n == 48L & n$Data < .lgbm_origin(cuts))
    if (have < consts$min_history_days_infer)
      lgbm_abort("lgbm.window", sprintf("historia de infer com %d dias completos antes de O (minimo %d)", have,
                                        consts$min_history_days_infer))
  } else if (!is.null(spec)) {
    have <- sum(n$n == 48L)
    if (have < spec$history_days$train)
      lgbm_abort("lgbm.window", sprintf("historia de treino com %d dias completos (history_days$train=%d)", have,
                                        spec$history_days$train))
  }
  invisible(TRUE)
}

## Composicao do estagio com spec e modo explicitos (a forma do contrato do ticket).
lgbm_ingest <- function(panel, cuts, spec = NULL, mode = c("train", "infer"), consts = plugin_constants()) {
  mode <- match.arg(mode)
  if (!is.null(spec)) lgbm_check_params(spec$params)
  lgbm_check_panel(panel, cuts, mode)
  load <- data.table::as.data.table(panel$load)
  area <- unique(load$area_code)
  if (length(area) != 1L) lgbm_abort("lgbm.contract", sprintf("painel com %d areas (esperado 1)", length(area)))
  O <- .lgbm_origin(cuts)
  .lgbm_check_window(load, cuts, mode, spec, consts)
  first_day <- lubridate::as_date(min(load$interval_start), tz = cuts$tz)
  last_day <- if (mode == "train") cuts$last_data_day else O + 1L
  days <- seq(first_day, last_day, by = "day")
  frame <- lgbm_load_grid(load, days, cuts, mode)
  wf <- panel$weather_forecast
  temp <- lgbm_weather_vintage(wf, area, days[days <= O], consts$lead_train, consts, cuts$tz)
  if (mode == "infer")
    temp <- rbind(temp, lgbm_weather_vintage(wf, area, O + 1L, consts$lead_target, consts, cuts$tz))
  frame <- merge(frame, temp, by = "DataHora", all.x = TRUE)
  data.table::setorder(frame, DataHora)
  data.table::setcolorder(frame, c("DataHora", "Data", "Hora", "Minuto", "CargaGlobal", "is_synthetic", "Temperatura"))
  data.table::setattr(frame, "feriados", lgbm_holidays(panel$holidays, consts, area))
  data.table::setattr(frame, "area", area)
  ## cortes ja resolvidos por dates.R, para a FE (o estagio recebe so (frame, params)); o plugin nao os recalcula
  data.table::setattr(frame, "cuts", cuts[c("last_data_day", "train_origin")])
  frame[]
}

## Estagio despachado pela fundacao: assinatura canonica (panel, cuts), sem spec nem modo. O modo sai do painel: carga
## com algum intervalo depois de last_data_day so existe no infer (o despachante ja barrou isso no treino). Sem spec,
## a janela de treino (history_days) e conferida pelo validate-deck; o infer confere min_history_days_infer.
plugin_ingest <- function(panel, cuts) {
  ld <- panel$load
  infer <- is.data.frame(ld) && "interval_start" %in% names(ld) && nrow(ld) > 0L &&
    inherits(cuts$last_data_day, "Date") &&
    any(lubridate::as_date(ld$interval_start, tz = cuts$tz) > cuts$last_data_day)
  lgbm_ingest(panel, cuts, spec = NULL, mode = if (infer) "infer" else "train")
}
