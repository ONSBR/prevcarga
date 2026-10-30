## inference_features.R -- estagio de features de inferencia do plugin LGBM (LGBM.4). SO definicoes.
## Porte de build_inference_features (prevcargadessem3/models/lgbm/R/inference_features/inference_features.R:177-412,
## somente leitura). 100% a partir do ARTEFATO (G-ART): sem argumento `params`/`spec`, sem refit, sem variavel de ambiente
## (o Sys.setenv do oraculo e o .inherit_adequacao_flags viram lgbm_check_artifact_params + params do artefato).
## Calendario, LOESS e Sazonalidade/fallbacks so do artefato; nada recomputado. Sem aritmetica de hora (modelo de 30 min;
## o despachante aplica o offset do descriptor). Saida ASCII.

## Adequacao-legado (G-ART): os params do artefato tem que bater com o schema treinado (colunas_entrada_BLF).
## Substitui o Sys.setenv(LGBM_DISABLE_LOESS) e a conferencia por env do oraculo (inference_features.R:192-222).
lgbm_check_artifact_params <- function(artifact) {
  bad <- function(why) lgbm_abort("lgbm.contract", sprintf("artefato inconsistente: %s", why))
  p <- artifact$params
  if (!is.list(p)) bad("sem params")
  lgbm_check_params(p)
  if (isTRUE(p$tipodia_legacy) && is.null(artifact$sazonalidade_fallbacks))
    bad("tipodia_legacy sem sazonalidade_fallbacks")
  cols <- artifact$colunas_entrada_BLF
  if (!is.character(cols) || !length(cols)) bad("colunas_entrada_BLF ausente")
  extra <- lgbm_adequacao_extra_features(p)
  possible <- lgbm_adequacao_extra_features(list(tipodia_legacy = TRUE, calendar_yearend = TRUE))
  miss <- setdiff(extra, cols)
  if (length(miss)) bad(sprintf("%s fora de colunas_entrada_BLF", paste(miss, collapse = ",")))
  over <- setdiff(intersect(possible, cols), extra)
  if (length(over)) bad(sprintf("%s em colunas_entrada_BLF sem o flag em params", paste(over, collapse = ",")))
  for (f in c("min_max_BLF", "sazonalidade", "target_col", "best_iter"))
    if (is.null(artifact[[f]])) bad(sprintf("%s ausente", f))
  if (is.null(artifact$seed) || length(artifact$seed) != 1L || is.na(artifact$seed)) bad("seed ausente")
  invisible(p)
}

.lgbm_inf_require <- function(frame) {
  need <- c("DataHora", "Data", "Hora", "CargaGlobal", "Temperatura", "is_synthetic")
  miss <- setdiff(need, names(frame))
  if (length(miss)) lgbm_abort("lgbm.contract", sprintf("requires ausente: %s (frame)", miss[[1L]]))
  if (!inherits(attr(frame, "feriados"), "Date")) lgbm_abort("lgbm.contract", "frame sem attr feriados (ingestao)")
  invisible(TRUE)
}

## Historia curta => lgbm.window; dia do historico incompleto => lgbm.completude. `O` = menor Data entre as 2 ultimas.
.lgbm_inf_check_history <- function(dt, O, consts) {
  hist <- dt[Data < O, list(n = .N), by = Data]
  have <- sum(hist$n == 48L)
  if (have < consts$min_history_days_infer)
    lgbm_abort("lgbm.window", sprintf("historia de infer com %d dias completos antes de O (minimo %d)", have,
                                      consts$min_history_days_infer))
  invisible(TRUE)
}

## Forma despachada: o despachante entrega (frame, payload). `cuts` e opcional e, quando dado, so e conferido contra o
## frame (o corte efetivo ja vem do is_synthetic da ingestao: interval_end > available_until). Devolve:
## list(dt_features, dt_D, dt_D1, n_real_D, area). Sem na.omit: a ordem e as 48 linhas de O e O+1 sao preservadas.
plugin_inference_features <- function(frame, artifact, cuts = NULL) {
  if (inherits(artifact, "prevcarga_plugin_artifact")) artifact <- artifact$payload
  consts <- plugin_constants()
  params <- artifact$params
  lgbm_check_artifact_params(artifact)
  .lgbm_inf_require(frame)
  keep_rng <- .lgbm_foundation("prevcarga_preserve_rng")
  if (is.null(keep_rng)) lgbm_abort("lgbm.contract", "prevcarga_preserve_rng indisponivel (fundacao)")
  feriados <- attr(frame, "feriados")
  area <- attr(frame, "area")
  keep_rng({
    set.seed(artifact$seed)
    dt <- data.table::copy(data.table::as.data.table(frame))
    data.table::setorder(dt, DataHora)
    days <- sort(unique(dt$Data))
    if (length(days) < 2L) lgbm_abort("lgbm.window", "frame de infer sem os dias O e O+1")
    day_D <- days[[length(days) - 1L]]
    day_D1 <- days[[length(days)]]
    if (!is.null(cuts) && !is.null(cuts$origin_day) && !identical(cuts$origin_day, day_D))
      lgbm_abort("lgbm.contract", sprintf("origem %s de cuts difere do dia O do frame (%s)", format(cuts$origin_day),
                                          format(day_D)))
    .lgbm_inf_check_history(dt, day_D, consts)
    lgbm_add_calendar_cols(dt, feriados, params)
    dt[, HoraDecimal := Hora / 2.0]
    dt[, CargaGlobal_suav := lgbm_loess_daily(CargaGlobal, HoraDecimal, consts$span_prep, params$disable_loess), by = Data]
    dt[, Temperatura_suav := lgbm_loess_daily(Temperatura, HoraDecimal, consts$span_prep, params$disable_loess), by = Data]
    lag_tbl <- dt[, list(Data_next = Data + 1L, Hora = Hora, CargaGlobal_anterior_BLF = CargaGlobal_suav)]
    dt <- merge(dt, lag_tbl, by.x = c("Data", "Hora"), by.y = c("Data_next", "Hora"), all.x = TRUE)
    dt[, Temperatura_prevista_BLF := Temperatura_suav]
    data.table::setorder(dt, Data, Hora)
    dt[, Hora_seno := sin(2.0 * pi * Hora / 48.0)]
    dt <- lgbm_add_wavelets(dt, consts)
    dt <- lgbm_merge_sazonalidade(dt, data.table::as.data.table(artifact$sazonalidade),
                                  if (is.null(artifact$sazonalidade_fallbacks)) NULL
                                  else lapply(artifact$sazonalidade_fallbacks, data.table::as.data.table), params)
    for (lvl in 1L:6L) dt[, (paste0("DiaSemana_", lvl)) := as.integer(DiaSemanaNum == lvl)]
    if (!isTRUE(params$tipodia_legacy)) {
      dt[, DiaEspecial_FDS := as.integer(TipoDia == "FDS")]
      dt[, DiaEspecial_FE := as.integer(TipoDia == "FE")]
      dt[, DiaEspecial_FEFDS := as.integer(TipoDia == "FE+FDS")]
    } else dt[, Feriado := as.integer(Feriado)]
    lgbm_add_fimano_dummies(dt, params)
    for (m in 2L:12L) dt[, (paste0("Mes_", m)) := as.integer(lubridate::month(Data) == m)]
    data.table::setorder(dt, DataHora)
    dt_D <- dt[Data == day_D]
    dt_D1 <- dt[Data == day_D1]
    for (x in list(list(dt_D, day_D), list(dt_D1, day_D1)))
      if (nrow(x[[1L]]) != 48L)
        lgbm_abort("lgbm.completude", sprintf("dia %s com %d linhas de 48", format(x[[2L]]), nrow(x[[1L]])))
    holes <- dt_D[!is_synthetic & is.na(CargaGlobal)]
    if (nrow(holes))
      lgbm_abort("lgbm.completude", sprintf("dia %s sem carga no intervalo %s (antes de available_until)",
                                            format(day_D), format(holes$DataHora[[1L]])))
    list(dt_features = dt, dt_D = dt_D, dt_D1 = dt_D1,
         n_real_D = sum(!is.na(dt_D$CargaGlobal) & !dt_D$is_synthetic), area = area)
  })
}
