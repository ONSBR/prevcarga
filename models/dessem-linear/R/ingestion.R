## ingestion.R -- dessem-linear (DL.5). So definicoes. Monta o deck do motor a partir do painel JA horario
## (to_hourly: hora incompleta descartada) e JA no frame de modelagem (to_model_frame, model_time_offset_min = 60L,
## aplicado pelo despachante). NENHUMA aritmetica de hora aqui: o equivalente ao +3600 do data_adapter.R do oraculo
## nao existe. Rotulos: CARGAHIST$DataHora = interval_end do frame de modelagem (= inicio real da hora, o rotulo que o
## motor usa); EXOGENAHIST/EXOGENAPREV$DataHora = valid_time da hora como vem do to_hourly (clima nao e deslocado).
##
## plugin_ingest(panel, cuts, spec): `spec` (run.yaml + params) e opcional. Com spec valida a variante ANTES de tocar
## o painel e aplica a janela history_days.train; sem spec (o despachante passa so panel, cuts) a janela e a do proprio
## painel e a variante e resolvida pelo estagio feature_engineering (que recebe params).

## --- Completude (porta de .validate_dessem_deck do oraculo) ---------------------------------------------------
dl_day_of <- function(x) lubridate::as_date(x)

dl_series_gap <- function(what, why, details = NULL)
  dl_abort("plugin.dessem_linear_series_gap", where = "dessem-linear/ingest", reason = paste(what, why),
           details = c(list(serie = what), details))

## cada dia de `from` a `to` com exatamente `per_day` linhas (hora faltante = linha ausente pos-agregacao horaria)
dl_assert_covers_days <- function(dt, col, from, to, what, per_day = 24L) {
  days <- seq(from, to, by = "day")
  dd <- if (nrow(dt)) dl_day_of(dt[[col]]) else lubridate::as_date(numeric())
  got <- vapply(days, function(d) sum(dd == d), integer(1L))
  bad <- which(got != per_day)
  if (length(bad))
    dl_series_gap(what, sprintf("%s com %d/%d horas", format(days[[bad[[1L]]]]), got[[bad[[1L]]]], per_day),
                  list(dia = format(days[[bad[[1L]]]]), horas = got[[bad[[1L]]]], esperado = per_day))
  invisible(TRUE)
}

dl_assert_no_na <- function(dt, col, what) {
  if (anyNA(dt[[col]])) dl_series_gap(what, sprintf("%s com NA", col), list(coluna = col))
  invisible(TRUE)
}

## Janela e completude do deck. CARGAHIST/EXOGENAHIST contiguos e com 24 horas por dia de history_start a
## last_data_day, sem NA; EXOGENAPREV com 24 horas em cada dia O..O+horizon-1; FERIADOS nao vazio.
## Janela menor que history_days.train => window (exit 5); hora ou dia faltante => completude (exit 3).
dl_validate_deck <- function(deck, cuts, spec = NULL) {
  O <- deck$dataPrev
  last <- cuts$last_data_day
  if (!nrow(deck$CARGAHIST)) dl_series_gap("CARGAHIST", "sem linhas")
  first <- min(dl_day_of(deck$CARGAHIST$DataHora))
  hs <- deck$history_start
  if (!is.null(spec) && first > hs)
    dl_abort("plugin.dessem_linear_window_short", where = "dessem-linear/ingest",
             reason = sprintf("CARGAHIST comeca em %s; history_days.train=%d exige %s",
                              format(first), spec$history_days$train, format(hs)),
             details = list(first_day = format(first), required_from = format(hs),
                            history_days = spec$history_days$train))
  if (is.null(spec)) hs <- first
  for (s in c("CARGAHIST", "EXOGENAHIST")) {
    dt <- deck[[s]]
    dl_assert_covers_days(dt, "DataHora", hs, last, s)
    dl_assert_no_na(dt, if (identical(s, "CARGAHIST")) "Carga" else "Exo_Temperatura", s)
  }
  dl_assert_covers_days(deck$EXOGENAPREV, "DataHora", O, O + deck$horizon - 1L, "EXOGENAPREV")
  dl_assert_no_na(deck$EXOGENAPREV, "Exo_Temperatura", "EXOGENAPREV")
  if (!nrow(deck$FERIADOS)) dl_series_gap("FERIADOS", "vazio")
  invisible(TRUE)
}

## --- Montagem do deck -----------------------------------------------------------------------------------------
dl_single_area <- function(panel) {
  a <- unique(panel$load$area_code)
  if (length(a) != 1L)
    dl_abort("plugin.contract", where = "dessem-linear/ingest",
             reason = sprintf("o painel deve trazer exatamente 1 area na carga (veio %d)", length(a)),
             details = list(areas = a))
  a
}

plugin_ingest <- function(panel, cuts, spec = NULL) {
  v <- if (is.null(spec)) list(exogena = NA_character_, model_key = NA_character_) else dl_resolve_variant(spec)
  k <- plugin_constants()
  eng <- dl_engine()
  O <- if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin
  last <- cuts$last_data_day
  ## dias entregues = horizon_days (O..O+8); o motor roda horizon_days + 1 dias (O..O+9), como o oraculo com corte
  ## intradiario (data_adapter.R eng_horizon = horizon + 1): o splinefun monoH.FC de desagregaModeloSH precisa do dia seguinte
  ## para as ultimas horas do ultimo dia entregue. E contagem de dias, nao aritmetica de hora.
  horizon <- (if (is.null(spec)) k$horizon_days else as.integer(spec$horizon_days)) + 1L
  area <- dl_single_area(panel)
  ## com spec: janela de history_days.train dias terminando em last_data_day, mais 1 dia de aquecimento antes
  hs <- if (is.null(spec)) NULL else last - (as.integer(spec$history_days$train) - 1L)
  from <- if (is.null(hs)) NULL else hs - 1L
  in_window <- function(d) if (is.null(from)) d <= last else d <= last & d >= from

  ld <- data.table::as.data.table(panel$load)[area_code == area]
  ld <- ld[, .(interval_end, Carga = load_mwh)]
  data.table::setorder(ld, interval_end)
  cargahist <- ld[in_window(dl_day_of(interval_end)), .(DataHora = interval_end, Carga)]
  au <- cuts$available_until
  origin_load <- ld[dl_day_of(interval_end) == O]
  if (inherits(au, "POSIXct")) origin_load <- origin_load[interval_end <= lubridate::floor_date(au, "hour")]
  hourly_load <- data.table::rbindlist(list(ld[in_window(dl_day_of(interval_end))], origin_load))

  wo <- data.table::as.data.table(panel$weather_observed)[area_code == area]
  exogenahist <- wo[in_window(dl_day_of(valid_time)), .(DataHora = valid_time, Exo_Temperatura = temperature)]
  data.table::setorder(exogenahist, DataHora)

  wf <- data.table::as.data.table(panel$weather_forecast)[area_code == area]
  data.table::setorder(wf, valid_time, -issue_time)
  wf <- wf[, .SD[1L], by = valid_time]
  d_wf <- dl_day_of(wf$valid_time)
  exogenaprev <- wf[d_wf >= O & d_wf <= O + horizon - 1L, .(DataHora = valid_time, Exo_Temperatura = temperature)]
  data.table::setorder(exogenaprev, DataHora)
  exogenaprev <- eng$fill_forecast_tail(exogenaprev, "DataHora", O, O + horizon - 1L, k$hours_per_day, area, O,
                                        max_fill_days = k$max_tail_fill_days)

  h <- data.table::as.data.table(panel$holidays)
  if ("area_code" %in% names(h)) h <- h[area_code == area]
  code <- if ("prevcarga_code" %in% names(h)) "prevcarga_code" else "holiday_type_id"
  feriados <- data.table::data.table(Data = dl_day_of(h$date), Tipo = as.integer(h[[code]]))
  feriados <- eng$normalize_special_day_codes(feriados, area = area)

  deck <- list(CARGAHIST = cargahist, EXOGENAHIST = exogenahist, EXOGENAPREV = exogenaprev, FERIADOS = feriados,
               origin_load = origin_load, hourly_load = hourly_load, dataPrev = O, horizon = as.integer(horizon),
               variant = v$exogena, model_key = v$model_key, area = area, tz = cuts$tz,
               available_until = au, last_data_day = last, history_start = hs)
  dl_validate_deck(deck, cuts, spec)
  deck
}
