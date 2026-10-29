## feature_engineering.R -- estagio de features do plugin random-forest (RF.2). SO definicoes.
## Recebe o `rf_frame` da ingestao (RF.1) e devolve `rf_features`: uma linha por origem nativa (dataOrigem), as 1486
## colunas de feature com os NOMES e a ORDEM DE COLUNAS do `merge` do oraculo (a ordem define a ordem das colunas de `x`
## em cada randomForest() e, portanto, o sorteio de mtry) e os 216 alvos y_h001..y_h216.
## Porte de prevcargadessem3/models/random-forest/R/ingestion/data_adapter.R:444-776 e feature_engineering.R:166-460
## (somente leitura). Toda aritmetica de data por inteiros de dia ou lubridate; nenhuma hora do rotulo da carga aqui.
## Sem relogio, sem variavel de ambiente. Saida ASCII.

## --- Geradores de nomes (contrato de features) ------------------------------------------------------------------
.rf_temp_names <- function() {
  n <- plugin_constants()$horizon_days_native
  c(paste0("tmax_", seq_len(n)), paste0("tmin_", seq_len(n)))
}
.rf_origin_names <- function() c("origin_dow", "origin_month", "origin_is_weekend", "origin_is_feriado")
.rf_dow_names <- function(d) paste0("day", d, c("_dow", "_is_weekend", "_is_monday", "_is_tuesday", "_is_wednesday",
                                                "_is_thursday", "_is_friday", "_is_saturday", "_is_sunday", "_is_feriado"))
.rf_hour_names <- function(lead)
  paste0("h", sprintf("%03d", as.integer(lead)),
         c("_is_daily_peak", "_is_evening_peak", "_is_minimum", "_hour_sin", "_hour_cos", "_hour_month_interaction"))
.rf_seasonal_names <- function(d) paste0("day", d, c("_month", "_quarter", "_is_summer", "_is_winter"))
## Natal/Ano-Novo: origem e, por dia-alvo, intercalados (ordem do merge do oraculo)
.rf_xmas_names <- function() {
  c("origin_is_christmas", "origin_is_newyear",
    unlist(lapply(seq_len(plugin_constants()$horizon_days_native),
                  function(d) paste0("day", d, c("_is_christmas", "_is_newyear")))))
}
.rf_holiday_names <- function() c("has_holiday_in_horizon", "days_to_next_holiday", "days_since_last_holiday")
.rf_prev_names <- function(prefix)
  paste0(prefix, c("mean", "peak", "min", "daily_peak", "evening_peak", "dow", "is_weekend", "is_feriado"))
.rf_recent_names <- function() c("prev_7day_mean", "prev_3day_mean", "recent_vs_historical")

## Os 1486 nomes de feature NA ORDEM DE COLUNAS da tabela (temp, origem, dow x9, hora x216, sazonais x9, natal/ano-novo,
## feriados, prev_day, prev_2day, recentes), nao a ordem de agrupamento do all_feature_names() do oraculo.
all_feature_names <- function() {
  k <- plugin_constants()
  c(.rf_temp_names(), .rf_origin_names(),
    unlist(lapply(seq_len(k$horizon_days_native), .rf_dow_names)),
    unlist(lapply(seq_len(k$n_leads), .rf_hour_names)),
    unlist(lapply(seq_len(k$horizon_days_native), .rf_seasonal_names)),
    .rf_xmas_names(), .rf_holiday_names(), .rf_prev_names("prev_day_"), .rf_prev_names("prev_2day_"),
    .rf_recent_names())
}
all_target_names <- function() sprintf("y_h%03d", seq_len(plugin_constants()$n_leads))

.rf_contract_abort <- function(reason, ...)
  .rf_abort("plugin.contract", details = c(list(reason = reason), list(...)), where = "random-forest/feature_engineering")

## --- Datas por inteiros (sem difftime) ---------------------------------------------------------------------------
.rf_md <- function(dates) lubridate::month(dates) * 100L + lubridate::mday(dates)
.rf_in_window <- function(dates, start, end) {
  md <- .rf_md(dates)
  md >= start[[1L]] * 100L + start[[2L]] & md <= end[[1L]] * 100L + end[[2L]]
}
.rf_is_christmas <- function(dates) {
  w <- plugin_constants()$christmas_window
  .rf_in_window(dates, w$start, w$end)
}
.rf_is_newyear <- function(dates) {
  out <- rep(FALSE, length(dates))
  for (w in plugin_constants()$newyear_windows) out <- out | .rf_in_window(dates, w$start, w$end)
  out
}

## --- Construtores de bloco: cada um devolve lista nomeada de colunas (sem dataOrigem), na ordem do oraculo ----------
.rf_block_temp <- function(frame, origins) {
  w <- data.table::as.data.table(frame$weather)
  n <- plugin_constants()$horizon_days_native
  col <- function(v, d) {
    sub <- w[day_of_horizon == d]
    sub[[v]][match(origins, sub$origin)]
  }
  vals <- c(lapply(seq_len(n), function(d) col("tmax", d)), lapply(seq_len(n), function(d) col("tmin", d)))
  stats::setNames(vals, .rf_temp_names())
}

.rf_block_calendar <- function(origins, hol) {
  dow <- lubridate::wday(origins, week_start = 1L)
  stats::setNames(list(dow, lubridate::month(origins), dow %in% c(6L, 7L), origins %in% hol), .rf_origin_names())
}

.rf_block_dow <- function(origins, hol) {
  out <- list()
  for (d in seq_len(plugin_constants()$horizon_days_native)) {
    td <- origins + d
    dow <- lubridate::wday(td, week_start = 1L)
    out <- c(out, stats::setNames(c(list(dow, dow %in% c(6L, 7L)), lapply(1:7, function(k) dow == k),
                                    list(td %in% hol)), .rf_dow_names(d)))
  }
  out
}

.rf_block_hour <- function(origins) {
  k <- plugin_constants()
  n <- length(origins)
  out <- list()
  for (d in seq_len(k$horizon_days_native)) {
    tmonth <- lubridate::month(origins + d)
    for (h in 0L:23L) {
      lead <- (d - 1L) * 24L + h + 1L
      out <- c(out, stats::setNames(list(rep(h %in% k$daily_peak_hours, n), rep(h %in% k$evening_peak_hours, n),
                                         rep(h %in% k$minimum_hours, n), rep(sin(2 * pi * h / 24), n),
                                         rep(cos(2 * pi * h / 24), n), h * tmonth), .rf_hour_names(lead)))
    }
  }
  out
}

.rf_block_seasonal <- function(origins) {
  out <- list()
  for (d in seq_len(plugin_constants()$horizon_days_native)) {
    m <- lubridate::month(origins + d)
    out <- c(out, stats::setNames(list(m, ceiling(m / 3L), m %in% c(12L, 1L, 2L), m %in% c(6L, 7L, 8L)),
                                  .rf_seasonal_names(d)))
  }
  out
}

.rf_block_xmas <- function(origins) {
  out <- list(origin_is_christmas = .rf_is_christmas(origins), origin_is_newyear = .rf_is_newyear(origins))
  for (d in seq_len(plugin_constants()$horizon_days_native)) {
    td <- origins + d
    out <- c(out, stats::setNames(list(.rf_is_christmas(td), .rf_is_newyear(td)),
                                  paste0("day", d, c("_is_christmas", "_is_newyear"))))
  }
  out
}

## has_holiday_in_horizon / days_to_next_holiday / days_since_last_holiday (365 sem feriado), por inteiros de dia
.rf_block_holiday <- function(origins, hol) {
  k <- plugin_constants()
  hs <- sort(unique(as.integer(hol)))
  o <- as.integer(origins)
  has <- vapply(o, function(d0) any(hs %in% (d0 + seq_len(k$horizon_days_native))), NA)
  nxt <- vapply(o, function(d0) { f <- hs[hs > d0]; if (length(f)) as.numeric(min(f) - d0) else k$no_holiday_days }, 0)
  prv <- vapply(o, function(d0) { p <- hs[hs < d0]; if (length(p)) as.numeric(d0 - max(p)) else k$no_holiday_days }, 0)
  stats::setNames(list(has, nxt, prv), .rf_holiday_names())
}

## Estatisticas diarias da carga horaria (um dia por linha); base de prev_* e dos recentes
.rf_daily_stats <- function(frame) {
  k <- plugin_constants()
  ld <- data.table::as.data.table(frame$load)
  ds <- ld[, list(day_mean = mean(load_mw), day_peak = max(load_mw), day_min = min(load_mw),
                  day_daily_peak = max(load_mw[hora %in% k$daily_peak_stat_hours]),
                  day_evening_peak = max(load_mw[hora %in% k$evening_peak_hours])), by = dia]
  ds[, day_dow := lubridate::wday(dia, week_start = 1L)]
  ds[, day_is_weekend := day_dow %in% c(6L, 7L)]
  ds[, day_is_feriado := dia %in% frame$holidays]
  data.table::setorder(ds, dia)
  ds[]
}

## Estatisticas do dia T-lag (lag 1 => prev_day_*, lag 2 => prev_2day_*); sem linha => NA
.rf_block_prev <- function(ds, origins, lag_days, prefix) {
  idx <- match(origins - lag_days, ds$dia)
  src <- c("day_mean", "day_peak", "day_min", "day_daily_peak", "day_evening_peak", "day_dow", "day_is_weekend",
           "day_is_feriado")
  stats::setNames(lapply(src, function(cn) ds[[cn]][idx]), .rf_prev_names(prefix))
}

## prev_7day_mean/prev_3day_mean sobre medias diarias de [T-7, T-1]/[T-3, T-1]; recent_vs_historical = m3 / m7
.rf_block_recent <- function(ds, origins) {
  dn <- as.integer(ds$dia); dm <- ds$day_mean
  mean_in <- function(d0, back) {
    v <- dm[dn >= d0 - back & dn <= d0 - 1L]
    if (length(v)) mean(v, na.rm = TRUE) else NA_real_
  }
  o <- as.integer(origins)
  m7 <- vapply(o, mean_in, 0, back = 7L)
  m3 <- vapply(o, mean_in, 0, back = 3L)
  rvh <- ifelse(!is.na(m3) & !is.na(m7) & m7 != 0, m3 / m7, NA_real_)
  stats::setNames(list(m7, m3, rvh), .rf_recent_names())
}

## Alvos: y_hLLL = carga horaria do dia (T + ceiling(L/24)), hora (L-1) %% 24; NA quando o dia nao esta no frame
.rf_block_targets <- function(frame, origins) {
  ld <- data.table::as.data.table(frame$load)
  key <- as.integer(ld$dia) * 24L + ld$hora
  leads <- seq_len(plugin_constants()$n_leads)
  vals <- lapply(leads, function(L) {
    k <- (as.integer(origins) + as.integer(ceiling(L / 24L))) * 24L + (L - 1L) %% 24L
    ld$load_mw[match(k, key)]
  })
  stats::setNames(vals, all_target_names())
}

## --- Guarda de contrato: todas as colunas presentes; nunca reordena valores nem descarta linhas -------------------
validate_feature_table <- function(dt) {
  if (!data.table::is.data.table(dt) || !("dataOrigem" %in% names(dt)))
    .rf_contract_abort("tabela de features sem dataOrigem", column = "dataOrigem")
  miss <- setdiff(c(all_feature_names(), all_target_names()), names(dt))
  if (length(miss))
    .rf_contract_abort(sprintf("coluna de feature ausente: %s", miss[[1L]]), column = miss[[1L]], n_missing = length(miss))
  invisible(dt)
}

## --- Montagem: (frame, spec) -> rf_features -------------------------------------------------------------------
## Horizonte e janelas sao constantes do oraculo (plugin_constants); `spec` e aceito pela assinatura do contrato.
plugin_build_features <- function(frame, spec) {
  k <- plugin_constants()
  need <- c("phase", "origins", "load", "weather", "holidays", "train_origin")
  if (!is.list(frame) || length(setdiff(need, names(frame))))
    .rf_contract_abort("rf_frame incompleto", missing = setdiff(need, names(frame)))
  origins <- sort(setdiff(frame$origins, frame$dropped_origins) |> lubridate::as_date())
  if (identical(frame$phase, "train")) {
    if (length(origins) && max(origins) > frame$train_origin)
      .rf_contract_abort("origem de treino alem de train_origin", origem = format(max(origins)),
                         train_origin = format(frame$train_origin))
    last_day <- max(frame$load$dia)
    if (last_day > frame$train_origin + k$horizon_days_native)
      .rf_contract_abort("carga alem de train_origin + H (last_data_day)", dia = format(last_day))
  }
  hol <- frame$holidays
  ds <- .rf_daily_stats(frame)
  cols <- c(.rf_block_temp(frame, origins), .rf_block_calendar(origins, hol), .rf_block_dow(origins, hol),
            .rf_block_hour(origins), .rf_block_seasonal(origins), .rf_block_xmas(origins),
            .rf_block_holiday(origins, hol), .rf_block_prev(ds, origins, 1L, "prev_day_"),
            .rf_block_prev(ds, origins, 2L, "prev_2day_"), .rf_block_recent(ds, origins))
  tbl <- data.table::setDT(c(list(dataOrigem = origins), cols, .rf_block_targets(frame, origins)))
  validate_feature_table(tbl)
  list(phase = frame$phase, table = tbl, feature_cols = all_feature_names(), target_cols = all_target_names(),
       train_origin = frame$train_origin, n_test = 0L)
}

## Assinatura canonica do despachante (frame, params); params = spec$params do run.yaml.
rf_feature_engineering <- function(frame, params) plugin_build_features(frame, list(params = params))
