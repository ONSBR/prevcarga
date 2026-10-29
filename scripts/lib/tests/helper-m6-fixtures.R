## helper-m6-fixtures.R -- base dos fixtures do M6 (M6.1; o M6.4 estende). Auto-carregado por test_dir.
## Nao e codigo de produto. Tudo por lubridate com tz explicito, sem env vars, sem I/O.
## Valor sentinela: y_pred/load_mwh codificam os minutos desde a origem do interval_start (pc_m6_minutes),
## de modo que qualquer deslocamento de rotulo muda o valor.

M6_TZ <- "Etc/GMT+3"

m6_fixture_ctx <- function(tz = M6_TZ) list(tz = tz)

m6_member <- function(name = "random-forest", delivery_days = 9L, granularity_min = 60L, split = "origin_agnostic")
  list(name = name, delivery_days = as.integer(delivery_days), granularity_min = as.integer(granularity_min), split = split)

m6_minutes <- function(t, origin, tz = M6_TZ)
  lubridate::time_length(t - lubridate::force_tz(lubridate::as_datetime(origin), tz), "minute")

## Entrega semi-horaria [available_until + 30 .. O + days 00:00] (ou `first`/`last` explicitos), valor sentinela
## = minutos do interval_start desde O. `shift_min`: desloca o carimbo (CONTROL de off-by-one).
m6_fixture_delivery <- function(member = m6_member(), origin = lubridate::ymd("2026-01-15"),
                                available_until = NULL, tz = M6_TZ, first = NULL, last = NULL, shift_min = 0L) {
  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), tz)
  if (is.null(available_until)) available_until <- t0 + lubridate::hours(9)
  if (is.null(first)) first <- available_until + lubridate::minutes(30)
  if (is.null(last)) last <- t0 + lubridate::days(member$delivery_days)
  ie <- seq(first, last, by = "30 min")
  data.table::data.table(interval_end = ie + lubridate::minutes(shift_min),
                         y_pred = m6_minutes(ie - lubridate::minutes(30), origin, tz))
}

## Truth do leitor unico (interval_end, load_mwh) = mesmo sentinela, rotulado por interval_end.
## `label`: "end" (correto) ou "start" (CONTROL: rotulado por interval_start, desloca 1 slot).
m6_fixture_truth <- function(origin = lubridate::ymd("2026-01-15"), days = 10L, tz = M6_TZ, label = "end", perturb = 0) {
  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), tz)
  start <- t0 + lubridate::minutes(30L * (seq_len(48L * days) - 1L))
  data.table::data.table(interval_end = if (identical(label, "end")) start + lubridate::minutes(30) else start,
                         load_mwh = m6_minutes(start, origin, tz) + perturb)
}
