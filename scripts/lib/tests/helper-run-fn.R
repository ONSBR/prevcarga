## helper-run-fn.R -- painel + run_fn sinteticos do honesty-gate (auto-carregado; M3.5 e M3.V estendem).
## Nao e codigo de produto. O `cut` de treino e sempre montado no TESTE como fim de
## last_data_day = O-1-data_lag_days (valor de fixture; o gate nunca o calcula).

## Painel do leitor: 48 intervalos/dia de origin-back a origin+horizon (inclusive), no fuso da carga.
pc_hg_panel <- function(origin, back = 8L, horizon = 9L, tz = "Etc/GMT+3", area = "A1") {
  o <- lubridate::ymd(origin)
  days <- seq(o - lubridate::days(back), o + lubridate::days(horizon), by = "day")
  d0 <- lubridate::force_tz(lubridate::as_datetime(days), tz)
  n <- length(days)
  slot <- rep(0:47, times = n)
  day0 <- rep(d0, each = 48L)
  start <- day0 + lubridate::minutes(30L * slot)
  dia <- rep(seq_len(n), each = 48L)
  y <- 1000 + 100 * sin(2 * pi * slot / 48) + 7 * dia
  data.frame(area = area, interval_start = start, interval_end = start + lubridate::minutes(30L),
             y_true = y, m1 = y * 0.97, m2 = y * 1.02 + 3, stringsAsFactors = FALSE)
}

.pc_rf_out <- function(origin, targets, y_pred) {
  data.frame(area = "A1", forecast_origin = format(lubridate::ymd(origin)),
             target_datetime = targets, lead = seq_along(targets), y_pred = y_pred,
             stringsAsFactors = FALSE)
}

## Inferencia: previsao das 48 meias horas apos `cut` a partir das ultimas 48 observacoes.
.pc_rf_infer <- function(cut, extra) {
  force(cut); force(extra)
  function(panel) {
    ok <- panel$interval_end <= cut + lubridate::minutes(extra)
    base <- mean(utils::tail(panel$y_true[ok], 48L))
    lead <- 1:48
    .pc_rf_out(as.character(lubridate::as_date(cut)), cut + lubridate::minutes(30L * lead),
               base + 0.01 * lead)
  }
}
pc_rf_infer_honest <- function(cut) .pc_rf_infer(cut, 0L)
pc_rf_infer_peek   <- function(cut) .pc_rf_infer(cut, 30L)

## Treino: previsao do dia O (48 slots) por media por slot sobre os dias [d_lo, d_hi].
.pc_rf_train <- function(origin, d_lo_off, d_hi_off) {
  force(origin)
  function(panel) {
    o <- lubridate::ymd(origin)
    day <- lubridate::as_date(panel$interval_start)
    slot <- as.integer(round(as.numeric(difftime(panel$interval_start,
             lubridate::force_tz(lubridate::as_datetime(day), lubridate::tz(panel$interval_start)),
             units = "mins")) / 30))
    ok <- day >= o + d_lo_off & day <= o + d_hi_off
    m <- tapply(panel$y_true[ok], slot[ok], mean)
    tz <- lubridate::tz(panel$interval_start)
    t0 <- lubridate::force_tz(lubridate::as_datetime(o), tz)
    .pc_rf_out(origin, t0 + lubridate::minutes(30L * (1:48)), as.numeric(m[as.character(0:47)]))
  }
}
pc_rf_train_agnostic <- function(origin, H) .pc_rf_train(origin, -1 - H, -1)
pc_rf_train_scoped   <- function(origin) .pc_rf_train(origin, -1e4, -1)
pc_rf_train_lagged   <- function(origin, lag) .pc_rf_train(origin, -1e4, -1 - lag)
pc_rf_train_leaky    <- function(origin) .pc_rf_train(origin, -1e4, 0)

## Run_fns defeituosos.
pc_rf_blind <- function() {
  function(panel) .pc_rf_out("2026-10-01", as.POSIXct(0, tz = "UTC") + 1:48, rep(1, 48))
}
pc_rf_nondet <- function(cut) {
  inner <- pc_rf_infer_honest(cut); n <- 0
  function(panel) { n <<- n + 1; r <- inner(panel); r$y_pred <- r$y_pred + n; r }
}
pc_rf_spy <- function(cut, env) {
  inner <- pc_rf_infer_honest(cut); env$calls <- list()
  function(panel) { env$calls[[length(env$calls) + 1L]] <- data.table::copy(data.table::as.data.table(panel)); inner(panel) }
}
pc_rf_mutating <- function(cut) {
  inner <- pc_rf_infer_honest(cut)
  function(panel) {
    r <- inner(data.table::copy(panel))
    data.table::set(panel, j = "y_true", value = 0)
    r
  }
}
