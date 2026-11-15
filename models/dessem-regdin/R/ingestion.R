## ingestion.R -- dessem-regdin (RD.1). So definicoes. Monta o deck do motor a partir do painel JA horario (to_hourly:
## hora incompleta descartada) e JA no frame de modelagem (to_model_frame, model_time_offset_min = 60L, aplicado pelo
## despachante). NENHUMA aritmetica de hora aqui: o equivalente ao +3600 do data_adapter.R do oraculo nao existe.
## Rotulos (mesmo contrato do dessem-linear): CARGAHIST$DataHora = interval_end do frame de modelagem (inicio real da
## hora, o rotulo do motor); EXOGENAHIST/EXOGENAPREV$DataHora = valid_time da hora (clima nao e deslocado).
##
## plugin_ingest(panel, cuts, spec): `spec` e opcional. Com spec valida a variante ANTES de tocar o painel e confere a
## janela history_days.train; sem spec (o despachante passa so panel, cuts) a janela e a do proprio painel.
## O plugin CONFERE o painel e nunca corta nem preenche (buraco nunca vira 0).

rd_day_of <- function(x) lubridate::as_date(x)

rd_fmt_days <- function(d, max_n = 10L) {
  d <- sort(unique(d))
  s <- paste(format(utils::head(d, max_n)), collapse = ",")
  if (length(d) > max_n) paste0(s, ",... (", length(d), " dias)") else s
}

rd_panel_contract <- function(reason, details = NULL)
  rd_abort("plugin.regdin_panel_contract", where = "dessem-regdin/ingest", reason = reason, details = details)

## --- Mapeamento de feriados (oraculo data_adapter.R:189-202), ANTES do normalizador de flag-12 do motor ----------
.rd_map_holidays <- function(holidays, tz) {
  h <- data.table::as.data.table(holidays)
  col <- if ("prevcarga_code" %in% names(h)) "prevcarga_code" else "holiday_type_id"
  out <- data.table::data.table(Data = lubridate::as_date(h$date, tz = tz), Tipo = as.integer(h[[col]]))
  if ("simplified_code" %in% names(h)) out[h$simplified_code == 0L & !is.na(h$simplified_code), Tipo := 0L]
  out[!is.na(Tipo) & Tipo > 11L, Tipo := 1L]
  out[]
}

## --- Mapeamento de colunas para o deck do motor ---------------------------------------------------------------------
## Renomeia a coluna de tempo (como chega do despachante) para `DataHora` e o valor para `Carga`/`Exo_Temperatura`.
.rd_hourly_to_deck <- function(x, value_col, time_col, out_col) {
  out <- data.table::data.table(DataHora = x[[time_col]], v = x[[value_col]])
  data.table::setnames(out, "v", out_col)
  data.table::setorder(out, DataHora)
  out[]
}

## --- Anexo de feriados (ADR-046): ilhas da area e dias esperados ---------------------------------------------------------
## Ilhas da area (island_id, from, to); data.table vazia (anexo sem ilha) ou NULL (sem anexo).
.rd_annex_islands <- function(ax, area) {
  if (is.null(ax)) return(NULL)
  isl <- data.table::as.data.table(ax$islands)
  if (!nrow(isl)) return(data.table::data.table(island_id = integer(), from = lubridate::as_date(character()),
                                                to = lubridate::as_date(character())))
  isl[area_code == area, list(island_id = as.integer(island_id), from = lubridate::as_date(from), to = lubridate::as_date(to))]
}

## Dias de ilha esperados: cada dia de [from, to] das ilhas mais os dias que as series do anexo trazem, sempre <= last.
.rd_annex_days <- function(isl, dts, last) {
  d <- lubridate::as_date(c(
    if (!is.null(isl) && nrow(isl)) unlist(lapply(seq_len(nrow(isl)), function(i) seq(isl$from[[i]], isl$to[[i]], by = "day"))),
    unlist(lapply(dts, function(x) rd_day_of(x)))))
  sort(unique(d[d <= last]))
}

## Dia de ilha sem 24 horas dentro do store => plugin.regdin_series_gap nomeando o dia (e a hora, quando e NA).
.rd_assert_annex <- function(ax, area, last) {
  if (is.null(ax)) return(invisible(TRUE))
  k <- plugin_constants()
  ld <- data.table::as.data.table(ax$load)[area_code == area & rd_day_of(interval_end) <= last]
  wo <- data.table::as.data.table(ax$weather_observed)[area_code == area & rd_day_of(valid_time) <= last]
  days <- .rd_annex_days(.rd_annex_islands(ax, area), list(ld$interval_end, wo$valid_time), last)
  if (anyNA(ld$load_mwh)) {
    i <- which(is.na(ld$load_mwh))[[1L]]
    rd_abort("plugin.regdin_series_gap", where = "dessem-regdin/ingest",
             reason = sprintf("carga do anexo com NA em %d hora(s); primeira: dia %s hora %s", sum(is.na(ld$load_mwh)),
                              format(rd_day_of(ld$interval_end[[i]])), format(ld$interval_end[[i]], "%H:%M")),
             details = list(serie = "CARGAFER", dia = format(rd_day_of(ld$interval_end[[i]])), n_na = sum(is.na(ld$load_mwh))))
  }
  for (spec in list(list(ld$interval_end, "CARGAFER"), list(wo$valid_time, "EXOGENAFER"))) {
    dd <- rd_day_of(spec[[1L]])
    got <- vapply(days, function(x) sum(dd == x), integer(1L))
    bad <- which(got != k$hours_per_day)
    if (length(bad))
      rd_abort("plugin.regdin_series_gap", where = "dessem-regdin/ingest",
               reason = sprintf("dia de ilha %s com %d/24 horas em %s; dias: %s", format(days[[bad[[1L]]]]), got[[bad[[1L]]]],
                                spec[[2L]], rd_fmt_days(days[bad])),
               details = list(serie = spec[[2L]], dia = format(days[[bad[[1L]]]]), horas = got[[bad[[1L]]]],
                              dias = format(days[bad])))
  }
  if (anyNA(wo$temperature))
    rd_panel_contract("Exo_Temperatura com NA em EXOGENAFER", list(serie = "EXOGENAFER"))
  invisible(TRUE)
}

## --- Conferencia do painel (confere, nunca corta) ---------------------------------------------------------------------
.rd_assert_panel <- function(panel, cuts, spec = NULL) {
  need <- list(load = c("area_code", "interval_end", "load_mwh"),
               weather_observed = c("area_code", "valid_time", "temperature"),
               weather_forecast = c("area_code", "issue_time", "valid_time", "temperature"),
               holidays = "date")
  for (ds in names(need)) {
    x <- panel[[ds]]
    if (!is.data.frame(x))
      rd_panel_contract(sprintf("dataset %s ausente do painel", ds), list(dataset = ds))
    miss <- setdiff(need[[ds]], names(x))
    if (length(miss))
      rd_panel_contract(sprintf("coluna %s ausente em %s", miss[[1L]], ds), list(dataset = ds, column = miss[[1L]]))
  }
  if (!any(c("prevcarga_code", "holiday_type_id") %in% names(panel$holidays)))
    rd_panel_contract("coluna prevcarga_code/holiday_type_id ausente em holidays", list(dataset = "holidays"))
  O <- if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin
  last <- cuts$last_data_day
  ld <- data.table::as.data.table(panel$load)

  ## (a) nada do dia O em diante alem do que o available_until libera (a carga do dia O e a cabeca de nowcast)
  au <- cuts$available_until
  d <- rd_day_of(ld$interval_end)
  over <- d > O
  if (inherits(au, "POSIXct")) {
    over <- over | (d == O & ld$interval_end > lubridate::floor_date(au, "hour"))
  } else {
    over <- over | d == O
  }
  if (any(over))
    rd_abort("plugin.regdin_train_panel_has_origin", where = "dessem-regdin/ingest",
             reason = sprintf("carga com interval_end=%s do dia O=%s em diante, alem do available_until",
                              format(min(ld$interval_end[over])), format(O)),
             details = list(interval_end = format(min(ld$interval_end[over])), origin_day = format(O)))

  ## (b)/(d) historia: cada dia de [first, last] com 24 horas; janela menor que history_days.train => window
  hist <- ld[d <= last]
  hd <- rd_day_of(hist$interval_end)
  if (!nrow(hist)) rd_abort("plugin.regdin_series_gap", where = "dessem-regdin/ingest", reason = "CARGAHIST sem linhas")
  first <- min(hd)
  if (!is.null(spec)) {
    hs <- rd_history_window(O, spec)$common_from      # janela comum do descriptor (E3-T4); o anexo e conferido a parte
    if (first > hs) {
      miss <- seq(hs, first - 1L, by = "day")
      rd_abort("plugin.regdin_history_short", where = "dessem-regdin/ingest",
               reason = sprintf("CARGAHIST comeca em %s; history_days.train=%d exige %s; dias faltantes: %s",
                                format(first), spec$history_days$train, format(hs), rd_fmt_days(miss)),
               details = list(first_day = format(first), required_from = format(hs),
                              missing_days = format(miss), history_days = spec$history_days$train))
    }
  }
  ## hora de carga historica com NA = hora ausente (o to_hourly mantem a linha): completude nomeando dia e hora
  if (anyNA(hist$load_mwh)) {
    i <- which(is.na(hist$load_mwh))[[1L]]
    rd_abort("plugin.regdin_series_gap", where = "dessem-regdin/ingest",
             reason = sprintf("carga com NA em %d hora(s); primeira: dia %s hora %s", sum(is.na(hist$load_mwh)),
                              format(hd[[i]]), format(hist$interval_end[[i]], "%H:%M")),
             details = list(dia = format(hd[[i]]), n_na = sum(is.na(hist$load_mwh))))
  }
  days <- seq(first, last, by = "day")
  got <- vapply(days, function(x) sum(hd == x), integer(1L))
  bad <- which(got != plugin_constants()$hours_per_day)
  if (length(bad))
    rd_abort("plugin.regdin_series_gap", where = "dessem-regdin/ingest",
             reason = sprintf("dia %s com %d/24 horas de carga; dias: %s", format(days[[bad[[1L]]]]), got[[bad[[1L]]]],
                              rd_fmt_days(days[bad])),
             details = list(dia = format(days[[bad[[1L]]]]), horas = got[[bad[[1L]]]], dias = format(days[bad])))

  ## (e) anexo de feriados: 24 horas por dia de ilha, sem NA de carga
  if (!is.null(panel$holiday_annex)) .rd_assert_annex(panel$holiday_annex, rd_single_area(panel), last)

  ## (c) nenhuma emissao de clima depois do weather_issue_limit
  wf <- panel$weather_forecast
  lim_w <- cuts$weather_issue_limit
  if (NROW(wf) && inherits(lim_w, "POSIXct")) {
    badw <- which(wf$issue_time > lim_w)
    if (length(badw))
      rd_panel_contract(sprintf("emissao de clima issue_time=%s depois de weather_issue_limit=%s",
                                format(max(wf$issue_time[badw])), format(lim_w)),
                        list(issue_time = format(max(wf$issue_time[badw])), weather_issue_limit = format(lim_w)))
  }
  invisible(TRUE)
}

## --- Exogena (temperatura da area) com a persistencia de cauda curta do motor (DL.2) ------------------------------
## kind = "hist": cobre [from, to] por dia com 24 horas; kind = "prev": aplica fill_forecast_tail em O..O+h-1.
## Lacuna maior que a persistencia aceita => contract nomeando os dias; nunca preenche com 0.
.rd_exo <- function(weather, kind, cuts, horizon_days, area = NA_character_, from = NULL) {
  O <- if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin
  k <- plugin_constants()
  w <- data.table::as.data.table(weather)
  if ("area_code" %in% names(w) && !is.na(area)) w <- w[area_code == area]
  if (identical(kind, "hist")) {
    out <- .rd_hourly_to_deck(w[rd_day_of(valid_time) <= cuts$last_data_day], "temperature", "valid_time", "Exo_Temperatura")
    first <- if (is.null(from)) min(rd_day_of(out$DataHora)) else from
    days <- seq(first, cuts$last_data_day, by = "day")
    dd <- rd_day_of(out$DataHora)
    got <- vapply(days, function(x) sum(dd == x), integer(1L))
    bad <- which(got != k$hours_per_day)
    if (length(bad))
      rd_panel_contract(sprintf("EXOGENAHIST sem 24 horas nos dias: %s", rd_fmt_days(days[bad])),
                        list(serie = "EXOGENAHIST", dias = format(days[bad])))
  } else {
    last_day <- O + as.integer(horizon_days) - 1L
    ## emissao mais recente por valid_time (o leitor ja filtrou por weather_issue_limit)
    data.table::setorder(w, valid_time, -issue_time)
    w <- w[, .SD[1L], by = valid_time]
    dd <- rd_day_of(w$valid_time)
    out <- .rd_hourly_to_deck(w[dd >= O & dd <= last_day], "temperature", "valid_time", "Exo_Temperatura")
    out <- rd_engine()$fill_forecast_tail(out, "DataHora", O, last_day, k$hours_per_day, area, O,
                                          max_fill_days = k$max_tail_fill_days)
    days <- seq(O, last_day, by = "day")
    dd <- rd_day_of(out$DataHora)
    got <- vapply(days, function(x) sum(dd == x), integer(1L))
    bad <- which(got != k$hours_per_day)
    if (length(bad))
      rd_panel_contract(sprintf("EXOGENAPREV sem exogena prevista nos dias: %s", rd_fmt_days(days[bad])),
                        list(serie = "EXOGENAPREV", dias = format(days[bad])))
  }
  if (anyNA(out$Exo_Temperatura))
    rd_panel_contract(sprintf("Exo_Temperatura com NA em %s", if (identical(kind, "hist")) "EXOGENAHIST" else "EXOGENAPREV"))
  out
}

## --- Montagem do deck -----------------------------------------------------------------------------------------------
rd_single_area <- function(panel) {
  a <- unique(panel$load$area_code)
  if (length(a) != 1L)
    rd_panel_contract(sprintf("o painel deve trazer exatamente 1 area na carga (veio %d)", length(a)), list(areas = a))
  a
}

plugin_ingest <- function(panel, cuts, spec = NULL) {
  key <- if (is.null(spec)) NA_character_ else .rd_variant_key(spec$params)
  k <- plugin_constants()
  O <- if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin
  last <- cuts$last_data_day
  ## dias ENTREGUES = horizon_days (O..O+8); o motor roda horizon_days + 1 dias (O..O+9), como o oraculo com corte intradiario
  ## (data_adapter.R: n_forecast_days = horizon + 1): o splinefun monoH.FC de desagregaModeloSH precisa do dia seguinte para as
  ## ultimas horas do ultimo dia entregue. E contagem de dias, nao aritmetica de hora.
  horizon <- (if (is.null(spec)) k$horizon_days else as.integer(spec$horizon_days)) + 1L
  .rd_assert_panel(panel, cuts, spec)
  area <- rd_single_area(panel)

  ld <- data.table::as.data.table(panel$load)[area_code == area]
  ld <- .rd_hourly_to_deck(ld, "load_mwh", "interval_end", "Carga")
  dl <- rd_day_of(ld$DataHora)
  cargahist <- ld[dl <= last]
  origin_load <- ld[dl == O]
  hourly_load <- data.table::rbindlist(list(cargahist, origin_load))
  hs <- if (is.null(spec)) NULL else rd_history_window(O, spec)$common_from   # common_from do history_window (E3-T4)

  h <- data.table::as.data.table(panel$holidays)
  if ("area_code" %in% names(h)) h <- h[area_code == area]
  feriados <- rd_engine()$normalize_special_day_codes(.rd_map_holidays(h, cuts$tz), area = area)

  ## anexo de feriados (ADR-046): ja horario e deslocado pelo despachante. Sem anexo, o deck e o de antes.
  ax <- panel$holiday_annex
  cargafer <- exogenafer <- isl <- NULL
  if (!is.null(ax)) {
    la <- data.table::as.data.table(ax$load)[area_code == area & rd_day_of(interval_end) <= last]
    cargafer <- .rd_hourly_to_deck(la, "load_mwh", "interval_end", "Carga")
    wa <- data.table::as.data.table(ax$weather_observed)[area_code == area & rd_day_of(valid_time) <= last]
    exogenafer <- .rd_hourly_to_deck(wa, "temperature", "valid_time", "Exo_Temperatura")
    isl <- .rd_annex_islands(ax, area)
  }

  deck <- list(CARGAHIST = cargahist,
               EXOGENAHIST = .rd_exo(panel$weather_observed, "hist", cuts, horizon, area,
                                     from = min(rd_day_of(cargahist$DataHora))),
               EXOGENAPREV = .rd_exo(panel$weather_forecast, "prev", cuts, horizon, area),
               FERIADOS = feriados, origin_load = origin_load, hourly_load = hourly_load,
               dataPrev = O, horizon = as.integer(horizon), variant = if (is.null(spec)) NA_character_ else spec$params$exogena,
               model_key = key, area = area, tz = cuts$tz, available_until = cuts$available_until,
               last_data_day = last, history_start = hs)
  deck$CARGAFER <- cargafer; deck$EXOGENAFER <- exogenafer; deck$islands <- isl
  deck
}
