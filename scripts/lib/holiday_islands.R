## holiday_islands.R -- regra unica de datas da janela do DESSEM e das ilhas de feriado (ADR-046; E3-T1).
## Duas funcoes puras e deterministicas, chamadas pelo leitor (E3-T2) e pelo gerador do deck de inferencia (E2).
## Requer errors.R carregado antes. Datas so por lubridate (Date, sem fuso de hora). Arquivo so com definicoes.

prevcarga_dessem_window <- function(origin, mode = c("ano_civil", "referencia")) {
  O <- lubridate::as_date(origin)
  if (missing(mode)) mode <- "ano_civil"
  if (length(mode) != 1L || !is.character(mode) || !mode %in% c("ano_civil", "referencia"))
    prevcarga_abort("contract.constraint_violated", where = "dessem_window.mode")
  if (identical(mode, "referencia"))
    return(list(common_from = O - 1825L, common_to = O - 1L, holiday_from = NULL, mode = mode))
  list(common_from = O - 1095L, common_to = O - 1L,
       holiday_from = lubridate::make_date(lubridate::year(O) - 5L, 1L, 1L), mode = mode)
}

.prevcarga_islands_empty <- function() {
  data.table::data.table(area_code = character(), island_id = integer(),
                         from = lubridate::as_date(character()), to = lubridate::as_date(character()),
                         holiday_dates = list())
}

## iv: data.table(from, to, d) ordenada por from; junta os que se sobrepoem ou encostam (from_k <= to_{k-1} + 1).
.prevcarga_merge_islands <- function(iv) {
  grp <- cumsum(c(TRUE, as.integer(iv$from[-1L]) > cummax(as.integer(iv$to))[-nrow(iv)] + 1L))
  out <- iv[, list(from = min(from), to = max(to), holiday_dates = list(sort(unique(d)))), by = list(g = grp)]
  out[, g := NULL]
  out[]
}

prevcarga_holiday_islands <- function(holidays, origin, common_from, first_day, lag_days = 7L,
                                      holiday_from = NULL) {
  O <- lubridate::as_date(origin)
  cf <- lubridate::as_date(common_from)
  fd <- lubridate::as_date(first_day)
  lag <- as.integer(lag_days)
  if (is.null(holiday_from)) holiday_from <- prevcarga_dessem_window(O, "ano_civil")$holiday_from
  hf <- lubridate::as_date(holiday_from)
  h <- data.table::as.data.table(holidays)[, c("area_code", "prevcarga_code", "date"), with = FALSE]
  h <- h[!is.na(prevcarga_code) & prevcarga_code != 0L]
  h[, date := lubridate::as_date(date)]
  h <- unique(h[date >= hf & date <= O - 1L, list(area_code = as.character(area_code), d = date)])
  if (!nrow(h)) return(.prevcarga_islands_empty())
  data.table::setorderv(h, c("area_code", "d"))
  h[, `:=`(from = d - lag, to = pmin(d, cf - 1L))]
  h <- h[from <= to]
  h[, from := pmax(from, fd)]
  h <- h[from <= to]
  if (!nrow(h)) return(.prevcarga_islands_empty())
  res <- lapply(unique(h$area_code), function(a) {
    iv <- h[area_code == a]
    data.table::setorderv(iv, c("from", "to"))
    m <- .prevcarga_merge_islands(iv)
    m[, `:=`(area_code = a, island_id = seq_len(.N))]
    m
  })
  out <- data.table::rbindlist(res)
  out <- out[, list(area_code, island_id = as.integer(island_id), from, to, holiday_dates)]
  data.table::setorderv(out, c("area_code", "from"))
  out[]
}
