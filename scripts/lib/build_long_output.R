## build_long_output.R -- conversor UNICO e puro da entrega do membro para o painel LONG 8-col (M6.1, ADR-0032/0035).
## Sem I/O, sem ambiente, sem relogio. Requer errors.R, features.R (prevcarga_delivery_grid) e schema_long.R
## carregados. Datas so por lubridate com tz explicito; nenhuma aritmetica de hora de modelagem (e do features.R).
##
## - target_datetime = interval_end, no fuso da carga (ctx$tz); d_plus = dia do interval_start - O;
##   lead = d_plus * 48 + slot; forecast_origin e TEXTO yyyy-MM-dd (nunca Date).
## - Faixa de entrega (ADR-0032 c): inicio COMUM em available_until + 30 (regra unica
##   target_datetime > available_until) e FIM por membro (delivery_days do descriptor, via prevcarga_delivery_grid).
## - y_true vem do truth do leitor unico, casado por interval_end; nunca realimenta y_pred.
## Erros: ids em scripts/lib/errors/build_long.tsv (long_grid/long_offbyone/long_duplicate_lead) e os ja
## declarados contract.member_misaligned (combination.tsv) e contract.long_tz (evaluation.tsv).

## Trabalha em interval_start (a entrega dos plugins o carrega; senao deriva-se pela borda de LEITURA do
## schema_long.R). target_datetime = interval_end sai SO por prevcarga_long_target_datetime (borda de ESCRITA);
## este modulo nao faz +-30 min.
.long_index <- function(delivery, origin, tz) {
  w <- "prevcarga_build_long"
  col <- if ("interval_start" %in% names(delivery)) "interval_start" else "interval_end"
  x <- delivery[[col]]
  if (!inherits(x, "POSIXct") || anyNA(x) || !identical(lubridate::tz(x), tz))
    prevcarga_abort("contract.long_tz", where = w,
                    details = list(got = if (inherits(x, "POSIXct")) lubridate::tz(x) else class(x)[[1L]], want = tz))
  start <- if (identical(col, "interval_start")) x else prevcarga_long_interval_start(x, 30L, tz)
  d_plus <- as.integer(lubridate::as_date(start, tz = tz) - lubridate::as_date(origin))
  slot <- (lubridate::hour(start) * 60L + lubridate::minute(start)) %/% 30L + 1L
  idx <- data.table::data.table(interval_start = start,
                                target_datetime = prevcarga_long_target_datetime(start, 30L, tz),
                                y_pred = as.double(delivery$y_pred), d_plus = d_plus,
                                lead = d_plus * 48L + as.integer(slot))
  idx[order(idx$target_datetime)]
}

.long_align <- function(idx, member, cuts) {
  idx <- idx[idx$target_datetime > cuts$available_until]
  grid <- prevcarga_delivery_grid(cuts, member$delivery_days)
  ok <- nrow(idx) == length(grid) && all(idx$target_datetime == grid)
  if (!ok) {
    fmt <- function(x) if (length(x)) format(x, tz = cuts$tz, usetz = FALSE) else NA_character_
    prevcarga_abort("contract.member_misaligned", where = "prevcarga_build_long",
      details = list(member = member$name,
                     first = fmt(idx$target_datetime[seq_len(min(1L, nrow(idx)))]),
                     last = fmt(idx$target_datetime[seq_len(min(1L, nrow(idx))) * nrow(idx)]),
                     want_first = fmt(grid[1L]), want_last = fmt(grid[length(grid)]),
                     delivery_days = member$delivery_days))
  }
  idx
}

## Junta y_true por igualdade de interval_end == target_datetime (por valor numerico do instante: independe do
## fuso de exibicao). Opera numa copia; y_pred nunca e lido nem escrito aqui.
.long_attach_truth <- function(panel, truth) {
  out <- data.table::copy(panel)
  y <- rep(NA_real_, nrow(out))
  if (!is.null(truth) && nrow(truth)) {
    pos <- match(as.numeric(out$target_datetime), as.numeric(truth$interval_end))
    y <- as.double(truth$load_mwh[pos])
  }
  data.table::set(out, j = "y_true", value = y)
  out
}

.long_validate <- function(p, origin, tz) {
  w <- "prevcarga_build_long"
  if (anyDuplicated(p$lead)) prevcarga_abort("contract.long_duplicate_lead", where = w)
  days <- sort(unique(p$d_plus))
  n <- as.integer(table(factor(p$d_plus, levels = days)))
  inner <- n[days > min(days)]
  if (any(diff(days) != 1L) || any(inner != 48L))
    prevcarga_abort("contract.long_grid", where = w, details = list(d_plus = days, n = n))
  t0 <- lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(origin)), tz)
  grid <- seq(t0, by = "30 min", length.out = max(p$lead) + 1L)         # grid[lead] = interval_start esperado
  want_start <- grid[p$lead]
  if (any(p$interval_start != want_start) ||
      any(p$target_datetime != prevcarga_long_target_datetime(want_start, 30L, tz)))
    prevcarga_abort("contract.long_offbyone", where = w)
  invisible(p)
}

## delivery: data.table(interval_end, y_pred) ja semi-horario; member: list(name, delivery_days, ...);
## origin: Date; available_until: POSIXct em ctx$tz; truth: NULL ou data.table(interval_end, load_mwh).
prevcarga_build_long <- function(delivery, member, area, origin, available_until, truth = NULL, ctx) {
  tz <- ctx$tz
  cuts <- list(origin_day = lubridate::as_date(origin), available_until = available_until, tz = tz)
  idx <- .long_index(delivery, origin, tz)
  p <- .long_align(idx, member, cuts)
  p <- .long_attach_truth(p, truth)
  .long_validate(p, origin, tz)
  data.table::set(p, j = "area", value = rep(as.character(area), nrow(p)))
  data.table::set(p, j = "forecast_origin", value = rep(as.character(lubridate::as_date(origin)), nrow(p)))
  data.table::set(p, j = "granularity_min", value = rep(30L, nrow(p)))
  p <- p[, names(prevcarga_schema_long), with = FALSE]          # descarta o interval_start interno
  prevcarga_schema_long_check(p)
  p
}
