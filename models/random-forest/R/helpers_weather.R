## helpers_weather.R -- safra de clima por origem nativa e LOCF por day_of_horizon (RF.1). SO definicoes.
## Porte de data_adapter.R:.read_forecast_temp (somente leitura). A regra de corte da emissao e da fundacao
## (`weather_issue_cutoff`, que o plugin nunca escolhe): o plugin so desloca `cuts$weather_issue_limit` por dia de origem.
## Sem aritmetica de hora do rotulo da carga; dias por lubridate com tz explicito (cuts$tz). Saida ASCII.

## Safra por origem nativa T: limit(T) = weather_issue_limit - days(O - T) (mesmo relogio de parede da regra efetiva);
## candidatas com issue_time em (limit(T) - 1 dia, limit(T)], a mais fresca. Origem sem candidata nao gera linha
## (vira LOCF em .rf_weather_locf). day_of_horizon = dia(valid_time, tz) - T em 1..horizon; tmax/tmin = max/min diario
## (todas as leituras de 30 min do dia; Inf -> NA, como data_adapter.R:262-275).
.rf_weather_vintage <- function(wx, origins, cuts, horizon) {
  O <- if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin
  wx <- data.table::as.data.table(wx)
  empty <- data.table::data.table(origin = lubridate::as_date(integer()), day_of_horizon = integer(),
    issue_time = lubridate::with_tz(lubridate::as_datetime(numeric()), "UTC"), tmax = numeric(), tmin = numeric())
  if (!length(origins) || !nrow(wx)) return(empty)
  lim <- cuts$weather_issue_limit - lubridate::days(as.integer(O) - as.integer(origins))
  iss <- sort(unique(wx$issue_time))
  pick <- lapply(seq_along(origins), function(i) {
    cand <- iss[iss > lim[[i]] - lubridate::days(1L) & iss <= lim[[i]]]
    if (length(cand)) max(cand) else NA
  })
  have <- which(!vapply(pick, function(x) length(x) == 1L && is.na(x), NA))
  if (!length(have)) return(empty)
  sel <- data.table::data.table(origin = origins[have], issue_time = do.call(c, pick[have]))
  v <- wx[sel, on = "issue_time", nomatch = NULL, allow.cartesian = TRUE]
  v[, day_of_horizon := as.integer(lubridate::as_date(valid_time, tz = cuts$tz)) - as.integer(origin)]
  v <- v[day_of_horizon >= 1L & day_of_horizon <= horizon]
  if (!nrow(v)) return(empty)
  out <- v[, list(tmax = max(temperature, na.rm = TRUE), tmin = min(temperature, na.rm = TRUE)),
           by = list(origin, day_of_horizon, issue_time)]
  out[is.infinite(tmax), tmax := NA_real_][is.infinite(tmin), tmin := NA_real_]
  out[, issue_time := lubridate::with_tz(issue_time, "UTC")]
  data.table::setcolorder(out, c("origin", "day_of_horizon", "issue_time", "tmax", "tmin"))
  data.table::setorder(out, origin, day_of_horizon)
  out[]
}

## LOCF por day_of_horizon sobre a grade completa CJ(origin, 1..horizon) (porta do gate10_carry_forward,
## data_adapter.R:285-289): ordena por (day_of_horizon, origin) e preenche tmax/tmin com o ultimo valor de origem
## anterior; `locf = TRUE` nas linhas preenchidas. Origens que seguem NA: fase train -> `dropped_origins` (saem do
## treino, como as iniciais do oraculo); fase infer -> NA em O-1 ou O aborta plugin.completude nomeando a origem
## (nunca pontua com subconjunto). Devolve list(weather, dropped_origins).
.rf_weather_locf <- function(vint, origins, horizon, phase, keep = NULL) {
  grid <- data.table::CJ(origin = origins, day_of_horizon = seq_len(horizon))
  x <- merge(grid, data.table::as.data.table(vint), by = c("origin", "day_of_horizon"), all.x = TRUE)
  data.table::setorder(x, day_of_horizon, origin)
  x[, `:=`(had_max = !is.na(tmax), had_min = !is.na(tmin))]
  x[, `:=`(tmax = data.table::nafill(tmax, type = "locf"), tmin = data.table::nafill(tmin, type = "locf")),
    by = day_of_horizon]
  x[, locf := (!had_max & !is.na(tmax)) | (!had_min & !is.na(tmin))]
  x[, c("had_max", "had_min") := NULL]
  data.table::setorder(x, origin, day_of_horizon)
  bad <- x[, list(na = anyNA(tmax) | anyNA(tmin)), by = origin][na == TRUE, origin]
  if (identical(phase, "infer")) {
    need <- if (is.null(keep)) origins else keep
    hit <- sort(intersect(bad, need))
    if (length(hit))
      .rf_abort("plugin.completude", details = list(origem = format(hit[[1L]]), motivo = "clima sem safra nem LOCF"))
    dropped <- origins[0L]
  } else dropped <- sort(bad)
  x <- x[!(origin %in% dropped)]
  list(weather = x[], dropped_origins = dropped)
}
