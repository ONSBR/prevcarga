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
