## exogena.R -- porta de scripts/lib/fill_forecast_tail.R do oraculo.
## Persistencia (last-value-carried-forward) do dia COMPLETO mais recente estritamente anterior em cada dia
## curto da exogena prevista no horizonte [from, to]. NO-OP quando completa, sem doador, sem doador antes de
## um dia curto ou com mais de `max_fill_days` dias curtos (o contrato do plugin reprova depois).
## Sempre ligado: o kill-switch DESSEM_EXOG_TAIL_FILL nao e portado. Deslocamento por periodo do lubridate.
fill_forecast_tail <- function(dt, time_col, from, to, slots_per_day, area, origin, max_fill_days = 2L) {
  if (is.null(dt) || nrow(dt) == 0L) return(dt)
  stopifnot(time_col %in% names(dt))
  ft <- data.table::as.data.table(data.table::copy(dt))
  needed <- seq(lubridate::as_date(from), lubridate::as_date(to), by = "day")
  dd <- lubridate::date(ft[[time_col]])
  cnt <- vapply(needed, function(d) sum(dd == d), integer(1L))
  complete_days <- needed[cnt >= slots_per_day]
  short_days <- needed[cnt < slots_per_day]
  if (length(short_days) == 0L) return(dt)
  if (length(complete_days) == 0L) return(dt)
  if (length(short_days) > max_fill_days) return(dt)
  src_days <- lapply(short_days, function(d) {
    prev <- complete_days[complete_days < d]
    if (length(prev) == 0L) NULL else max(prev)
  })
  if (any(vapply(src_days, is.null, logical(1L)))) return(dt)
  keep <- ft[!(dd %in% short_days)]
  add_list <- vector("list", length(short_days))
  for (i in seq_along(short_days)) {
    d <- short_days[i]; src_d <- src_days[[i]]
    src <- ft[dd == src_d]
    a <- data.table::copy(src)
    a[[time_col]] <- a[[time_col]] + lubridate::days(as.integer(d - src_d))
    add_list[[i]] <- a
    message(sprintf(paste0("[engine.dessem] area=%s origin=%s: dia %s ausente/curto -> ",
                           "persistido do ultimo dia completo %s (+%dd, %d slots)."),
                    area, as.character(origin), format(d), format(src_d), as.integer(d - src_d), nrow(src)))
  }
  out <- data.table::rbindlist(c(list(keep), add_list), use.names = TRUE)
  data.table::setorderv(out, time_col)
  out[]
}

## EXOGENAHIST em modo clima_hist = previsto_lead0 (E5-T3): a regra unica consumida pelos dois DESSEM. Mesma regra de
## lgbm_weather_vintage com lead 0 (valid_day - issue_day >= 0; dia da emissao em `issue_day_tz`, dia valido no fuso
## da carga `tz`; por valid_time a emissao de maior issue_time). Completamento por DIA INTEIRO: dia de `days` sem 24
## horas pelo lead 0 sai inteiro do weather_observed (nunca mistura fontes no dia) e conta em dias_completados; dia sem
## 24 horas nem la => engine.dessem_exo_gap (completude) com details$dia. Sem aritmetica de hora, sem leitura de arquivo.
## -> list(dt = data.table(DataHora, Exo_Temperatura) ordenado, dias_completados = integer(1), primeiro_dia_lead0 = Date(1)).
exo_hist_lead0 <- function(weather_forecast, weather_observed, area, days, tz, issue_day_tz = "UTC") {
  days <- sort(unique(lubridate::as_date(days)))
  w <- data.table::as.data.table(weather_forecast)[area_code == area]
  w[, `:=`(valid_day = lubridate::as_date(valid_time, tz = tz),
           issue_day = lubridate::as_date(issue_time, tz = issue_day_tz))]
  w <- w[as.integer(valid_day - issue_day) >= 0L & valid_day %in% days]
  data.table::setorder(w, valid_time, -issue_time)
  w <- w[!duplicated(w, by = "valid_time")]
  n_lead <- w[, list(N = .N), by = valid_day]
  lead_days <- sort(n_lead$valid_day[n_lead$N == 24L])
  fill_days <- days[!(days %in% lead_days)]
  o <- data.table::as.data.table(weather_observed)[area_code == area]
  o[, valid_day := lubridate::as_date(valid_time, tz = tz)]
  o <- o[valid_day %in% fill_days]
  o <- o[!duplicated(o, by = "valid_time")]
  for (d in fill_days) {
    d <- lubridate::as_date(d)
    n_obs <- o[valid_day == d, .N]
    if (n_obs != 24L)
      engine_abort("engine.dessem_exo_gap", where = format(d),
                   details = list(dia = format(d), horas_lead0 = n_lead[valid_day == d, sum(N)],
                                  horas_observado = n_obs))
  }
  dt <- data.table::rbindlist(list(w[valid_day %in% lead_days, list(DataHora = valid_time, Exo_Temperatura = temperature)],
                                   o[, list(DataHora = valid_time, Exo_Temperatura = temperature)]))
  data.table::setorder(dt, DataHora)
  list(dt = dt, dias_completados = length(fill_days),
       primeiro_dia_lead0 = if (length(lead_days)) min(lead_days) else lubridate::NA_Date_)
}
