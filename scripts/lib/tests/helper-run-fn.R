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

## --- Canal de clima (M3.5) ---------------------------------------------------------------------------
## Emissoes do dia O em UTC (00Z/06Z/18Z), convertidas ao fuso da carga; valid_time O..O+9 horario.
pc_hg_weather <- function(origin, tz = "Etc/GMT+3", issues_utc = c(0L, 6L, 18L), areas = "A1") {
  o <- lubridate::force_tz(lubridate::as_datetime(lubridate::ymd(origin)), "UTC")
  vt <- lubridate::force_tz(lubridate::as_datetime(lubridate::ymd(origin)), tz) + lubridate::hours(0:(9 * 24))
  out <- list()
  for (a in areas) for (h in issues_utc) {
    it <- lubridate::with_tz(o + lubridate::hours(h), tz)
    out[[length(out) + 1L]] <- data.frame(
      area_code = a, issue_time = rep(it, length(vt)), valid_time = vt,
      temp_c = 20 + h / 3 + 5 * sin(2 * pi * seq_along(vt) / 24), rh_pct = 60 - h / 2,
      stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

.pc_wf_pred <- function(panel, w, w_used) {
  y <- mean(utils::tail(panel$y_true, 48L))
  temp <- if (is.null(w_used)) 0 else mean(w_used$temp_c) + 0.1 * mean(w_used$rh_pct)
  .pc_rf_out("2026-10-01", as.POSIXct(0, tz = "UTC") + 1:48, y + temp + 0.01 * (1:48))
}
## Honesto: emissao mais fresca com issue_time <= limit (por area; media entre areas).
pc_wf_honest <- function(limit) {
  force(limit)
  function(panel, weather) {
    ok <- weather[weather$issue_time <= limit, , drop = FALSE]
    fr <- stats::ave(as.numeric(ok$issue_time), ok$area_code, FUN = max)
    .pc_wf_pred(panel, weather, ok[as.numeric(ok$issue_time) == fr, , drop = FALSE])
  }
}
## Usa uma emissao fixa (hora UTC 6 ou 18), independente do limite.
pc_wf_issue <- function(hour_utc) {
  force(hour_utc)
  function(panel, weather) {
    h <- as.integer(format(lubridate::with_tz(weather$issue_time, "UTC"), "%H"))
    .pc_wf_pred(panel, weather, weather[h == hour_utc, , drop = FALSE])
  }
}
pc_wf_blind <- function() function(panel, weather) .pc_wf_pred(panel, weather, NULL)
pc_wf_spy <- function(limit, env) {
  inner <- pc_wf_honest(limit); env$calls <- list()
  function(panel, weather) {
    env$calls[[length(env$calls) + 1L]] <- list(panel = data.table::copy(data.table::as.data.table(panel)),
                                                weather = data.table::copy(data.table::as.data.table(weather)))
    inner(panel, weather)
  }
}

## --- Bateria do M3.V ---------------------------------------------------------------------------------------
## Saida no contrato do gate (area, forecast_origin, target_datetime, lead, y_pred) a partir de linhas do painel
## (target_datetime = interval_end do alvo; lead sequencial). Cobre d_plus 0..9 quando o painel tem O..O+9.
pc_hg_long <- function(rows, y_pred, origin = NULL) {
  tz <- lubridate::tz(rows$interval_end)
  o <- if (is.null(origin)) as.character(lubridate::as_date(rows$interval_end[[1L]], tz = tz)) else origin
  data.frame(area = if (nrow(rows)) rows$area else character(), forecast_origin = o,
             target_datetime = rows$interval_end, lead = seq_len(nrow(rows)), y_pred = as.numeric(y_pred),
             stringsAsFactors = FALSE)
}

.pc_hg_origin <- function(cut) as.character(lubridate::as_date(cut, tz = lubridate::tz(cut)))
.pc_hg_dplus <- function(panel, cut) {
  tz <- lubridate::tz(panel$interval_start)
  as.integer(lubridate::as_date(panel$interval_start, tz = tz) - lubridate::as_date(cut, tz = tz))
}
## base honesta: media das ultimas 48 observacoes com interval_end <= cut
.pc_hg_base <- function(panel, cut) mean(utils::tail(panel$y_true[panel$interval_end <= cut], 48L))

## Honesto exceto nos alvos de d_plus == d, onde le o y_true do proprio alvo (vazamento so no horizonte d).
pc_rf_spill <- function(cut, d = 9L) {
  force(cut); force(d)
  function(panel) {
    fut <- panel[panel$interval_end > cut, , drop = FALSE]
    y <- .pc_hg_base(panel, cut) + 0.01 * seq_len(nrow(fut))
    hit <- .pc_hg_dplus(fut, cut) == d
    y[hit] <- fut$y_true[hit]
    pc_hg_long(fut, y, .pc_hg_origin(cut))
  }
}

## Nunca le y_true: so os membros futuros.
pc_rf_members_only <- function(cut) {
  force(cut)
  function(panel) {
    fut <- panel[panel$interval_end > cut, , drop = FALSE]
    pc_hg_long(fut, (fut$m1 + fut$m2) / 2, .pc_hg_origin(cut))
  }
}

## Estilo combina: peso convexo por minimos quadrados fechados de y_true ~ m1, m2 no dia antes do corte,
## projetado em [0, 1], aplicado aos membros futuros (so le y_true ANTES do corte).
pc_rf_combina <- function(cut) {
  force(cut)
  function(panel) {
    past <- panel[panel$interval_end > cut - lubridate::days(1) & panel$interval_end <= cut, , drop = FALSE]
    dm <- past$m1 - past$m2
    w <- min(1, max(0, sum((past$y_true - past$m2) * dm) / sum(dm^2)))
    fut <- panel[panel$interval_end > cut, , drop = FALSE]
    pc_hg_long(fut, w * fut$m1 + (1 - w) * fut$m2, .pc_hg_origin(cut))
  }
}

## Honesto + k * media do y_true futuro (vazamento minusculo de magnitude controlada).
pc_rf_tiny <- function(cut, k) {
  force(cut); force(k)
  function(panel) {
    fut <- panel[panel$interval_end > cut, , drop = FALSE]
    y <- .pc_hg_base(panel, cut) + 0.01 * seq_len(nrow(fut)) + k * mean(panel$y_true[panel$interval_end > cut])
    pc_hg_long(fut, y, .pc_hg_origin(cut))
  }
}

## Painel que termina em `cut` (regiao proibida vazia).
pc_hg_panel_no_future <- function(cut, origin = "2026-10-01", ...) {
  p <- pc_hg_panel(origin, ...)
  p[p$interval_end <= cut, , drop = FALSE]
}
