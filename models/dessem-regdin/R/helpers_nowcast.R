## helpers_nowcast.R -- dessem-regdin (RD.2): cabeca de nowcast do dia O (harmonizacao DESSEM 16a/16b). Dono: este plugin
## (o motor nao tem cabeca; o oraculo a injeta por um wrapper com estado `.rd_nowcast`, _load_engine.R:300-370, que aqui
## vira funcao explicita). So definicoes: sem estado, sem `<<-`, sem env global, sem patch no motor, sem aritmetica de hora.

## Nucleo de `mm`: media das horas completas do dia cujo rotulo `interval_end` cai no mesmo dia e tem hora <= cut_hour.
## A hora do dia e lida do proprio rotulo (frame ja entregue pelo despachante); meia hora isolada nunca forma hora completa
## (to_hourly ja a descartou), logo available_until 09:00 e 09:30 dao o mesmo corte.
.rd_mm_core <- function(panel_hourly, cut_hour, tz) {
  p <- data.table::as.data.table(panel_hourly)
  vcol <- intersect(c("load_mwh", "load", "Carga"), names(p))[1L]
  if (is.na(vcol))
    rd_abort("plugin.regdin_panel_contract", where = "dessem-regdin/rd_morning_means",
             reason = "painel horario sem coluna de carga (load_mwh/load/Carga)", details = list(column = "load_mwh"))
  ts <- if ("interval_start" %in% names(p)) p$interval_start else p$interval_end
  h <- data.table::data.table(Data = lubridate::as_date(ts, tz = tz),
                              end_day = lubridate::as_date(p$interval_end, tz = tz),
                              end_hour = lubridate::hour(p$interval_end), v = p[[vcol]])
  out <- h[end_day == Data & end_hour <= cut_hour, .(mm = mean(v)), by = Data]
  data.table::setkey(out, Data)
  out[]
}

## mm[d] para cada dia do painel horario (historicos e O): media das horas completas com
## `interval_end <= floor_hour(available_until)`. available_until em 00:00 => nenhuma hora entra => vazio.
rd_morning_means <- function(panel_hourly, available_until, tz) {
  cut_h <- lubridate::floor_date(available_until, "hour")
  if (lubridate::hour(cut_h) == 0L)
    return(data.table::data.table(Data = lubridate::as_date(character()), mm = numeric()))
  .rd_mm_core(panel_hourly, lubridate::hour(cut_h), tz)
}

## mm normalizada como o `fit` do oraculo (dessem_regdin_fit.R:149-155): so se a DM normaliza (chaves do regdin:
## normalizacao$carga / $exogena; no model_parameters.json atual a DM nao e normalizada, quirk fiel ao oraculo).
rd_normalize_mm <- function(mm, config_mdl, constnorm) {
  out <- data.table::copy(data.table::as.data.table(mm))
  on <- any(config_mdl$normalizacao$carga$flag, config_mdl$normalizacao$exogena$flag)
  if (isTRUE(on) && !is.null(constnorm) && nrow(out)) out[, mm := mm / constnorm]
  out[]
}

## mm de REFERENCIA do treino: so do painel ate O-1, no corte fixo `reference_mm_hour` (12:00). Uma linha por dia
## historico (o mesmo numero de linhas validas de qualquer corte em hora cheia > 00:00): o tune_wsvm da cabeca consome o
## RNG como no split-nowcast do oraculo. Os VALORES nao importam e o resultado nunca entra no artefato.
rd_reference_mm <- function(panel_train, ref_hour, tz) .rd_mm_core(panel_train, as.integer(ref_hour), tz)

## Cabeca de nowcast + ancora (substitui o wrapper `execDemandaMediaTS` do oraculo; Pecas A e B).
## Sem `mm` (ou O fora do horizonte) delega ao execDemandaMediaTS do motor (ARIMA puro, sem RNG).
## Devolve list(mdl, infer, head_O) na forma do execDemandaMediaTS (+ head_O). Como o motor, atualiza `serieDM` por
## referencia (addValuesDMRef). `mm` ja vem normalizado (rd_normalize_mm).
rd_dm_nowcast <- function(serieDM, config_mdl, mm, origin_day, seed) {
  if (is.null(mm) || !nrow(mm)) return(execDemandaMediaTS(serieDM, config_mdl))
  input <- obterEntradaFormulaDM_Reg(serieDM, config_mdl)
  sdm <- data.table::copy(input$dadosEntrada$serieDiariaModelo)
  data.table::setorder(sdm, Data)
  if (!(origin_day %in% sdm[Prev == TRUE]$Data)) return(execDemandaMediaTS(serieDM, config_mdl))
  set.seed(as.integer(seed))
  ## Peca A: wSVM radial sobre mm + calendario + temperatura + lags
  sdm[, Carga_l1 := data.table::shift(Carga, 1L)]
  sdm[, Carga_l7 := data.table::shift(Carga, 7L)]
  sdm <- merge(sdm, mm, by = "Data", all.x = TRUE)
  data.table::setorder(sdm, Data)
  exo <- grep("^Exo_", names(sdm), value = TRUE)[1L]
  frm <- stats::as.formula(paste("Carga ~ mm + DiaEspecial + DiaSemana + Mes +", exo, "+ Carga_l1 + Carga_l7"))
  htr <- sdm[Prev == FALSE & !is.na(Carga) & !is.na(mm) & !is.na(Carga_l1) & !is.na(Carga_l7)]
  hm <- tune_wsvm(frm, data = htr, weight = rep(1, nrow(htr)), kernel = "radial",
                  ranges = list(cost = c(1, 10, 100), gamma = c(0.01, 0.1)))
  head_O <- as.numeric(stats::predict(hm$best.model, sdm[Data == origin_day]))[1L]
  ## Peca B: O vira observado (= cabeca); o ARIMA re-fitado preve O+1..
  sdm[Data == origin_day, `:=`(Carga = head_O, Prev = FALSE)]
  fit <- tsibble::as_tsibble(sdm[Prev == FALSE & !is.na(Carga)], index = "Data") %>%
    fabletools::model(fable::ARIMA(input$formulaMDL))
  fc <- data.table::as.data.table(fabletools::forecast(fit, tsibble::as_tsibble(sdm[Prev == TRUE], index = "Data")))
  fc <- fc[, .(Data, Carga.infer = .mean)]
  infer <- rbind(data.table::data.table(Data = origin_day, Carga.infer = head_O), fc)
  data.table::setorder(infer, Data)
  addValuesDMRef(serieDM, infer)
  list(mdl = list(modeloTreinado = fit, dadosEntrada = input$dadosEntrada, parametros = config_mdl,
                  dadosInfer = tsibble::as_tsibble(sdm[Prev == TRUE], index = "Data")),
       infer = infer, head_O = head_O)
}
