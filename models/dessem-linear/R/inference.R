## inference.R -- dessem-linear (DL.7). So definicoes. Estagios `inference` e `forecast` + forma do ticket.
## Mapa para o oraculo (/opt/ons-source/prevcargadessem3/models/dessem-linear/R/inference):
##   predict_dessem_linear  -> plugin_infer (TransformaDeck + mm em dl_inference_features)
##   .dl_predict_pass       -> .dl_predict_pass (recursao de INPUT: o LAG_1 de O+1 recebe a media de O PREVISTA)
##   dessem_linear_infer    -> .dl_envelope (desagregaModeloSH, fatiamento por Passo, matriz 9x48)
## O unico ajuste do infer e a DM(O): set.seed(artifact$seed) e refit parcial por dl_dm_head com mm[O] (1 chamada de
## tune_wsvm). Os SVMs vem congelados (thaw idempotente do motor). Nenhuma leitura de ambiente; RNG do chamador:
## restaurado pelo despachante (prevcarga_plugin_infer). Sem aritmetica de hora: a regra semi-horaria e o carimbo de
## 30 min sao de to_semihourly (fundacao); o deslocamento de 60 min, do despachante.

## o motor devolve `tune` em memoria (forecast acoplado); congelado/descongelado e o proprio best.model
.dl_best <- function(m) if (inherits(m, "tune")) m$best.model else m

## Linha de entrada do perfil de (dp, h): mesma montagem de SVMhorario (modelos_perfil.R), so a linha do dia.
.dl_profile_row <- function(eng, serie_pf, config_mdl, dp, h) {
  cfg <- eng$obterDiaEspPerfilConfig(dp, serie_pf, config_mdl)
  fn <- get(config_mdl$funcDadosPerfil, envir = eng)
  d <- fn(serie = serie_pf, tipoEntrada = cfg$tpEntrada, diaSemana = cfg$ds, hora = h, diaAno = cfg$diasAno,
          lagsmodelo = cfg$hrlag, hv = serie_pf[Data == dp]$HV[1], lagsdsFer = cfg$lagds, tpFer = cfg$diaEsp,
          addHourFac = FALSE)
  d[Data == dp]
}

## Predicao com guarda: preditor NA na linha de entrada (lag sem dado) vira erro de completude com dia e variavel,
## em vez do erro opaco do predict.wsvm.
.dl_predict_row <- function(model, row, what) {
  vars <- all.vars(stats::delete.response(stats::terms(model)))
  vars <- intersect(vars, names(row))
  na <- vars[vapply(vars, function(v) anyNA(row[[v]]), NA)]
  if (length(na))
    dl_abort("plugin.dessem_linear_series_gap", where = "dessem-linear/inference",
             reason = sprintf("%s: preditor sem dado (%s)", what, paste(na, collapse = ",")),
             details = list(serie = what, preditores = na))
  predict(model, row)
}

## Passe de predict (porta de .dl_predict_pass): DM por dia (O: cabeca refeita; O+1..O+8: SVM congelado), write-back da
## DM prevista em serieDM (recursao), 24 perfis com CargaMed = DM, desnormalizacao por norm$pf_cargadm.
.dl_predict_pass <- function(a, f, dm_head) {
  eng <- f$engine
  sDM <- data.table::copy(f$serieDM); sPF <- data.table::copy(f$seriePF)
  O <- format(f$cuts$origin_day)
  for (dp in format(f$processado$DIASPREV)) {
    if (identical(dp, O)) {
      inp <- dl_dm_head_input(eng, sDM, f$config_mdl, f$mm, f$cuts$origin_day, TRUE, a$norm$dm_carga)
      infer <- .dl_predict_row(dm_head, inp$dadosEntrada$serieDiariaModelo[Data == dp], paste0("DM ", dp))
    } else {
      inp <- eng$obterEntradaFormulaDM(dp, sDM, f$config_mdl)
      infer <- .dl_predict_row(.dl_best(a$svm_dm[[dp]]), inp$dadosEntrada$serieDiariaModelo[Data == dp], paste0("DM ", dp))
    }
    sDM[Data == dp, Carga := infer[[1L]]]
    for (h in 0:23) {
      x <- .dl_profile_row(eng, sPF, f$config_mdl, dp, h)
      x[, CargaMed := infer[[1L]]]
      pf <- .dl_predict_row(.dl_best(a$svm_perfil[[dp]][[h + 1L]]), x, sprintf("perfil %s h%02d", dp, h))
      sPF[Data == dp & lubridate::hour(DataHora) == h, `:=`(CargaMed = infer[[1L]], Carga = pf[[1L]])]
    }
  }
  sPF[, CargaMed := CargaMed * a$norm$pf_cargadm][, Carga := Carga * CargaMed]
  sPF[Prev == TRUE, .(DataHora, Carga)]
}

## `to_semihourly`/`prevcarga_*` sao da fundacao e o env do plugin so enxerga base + attach: busca interim no globalenv
## (mesmo padrao de dl_abort/engine_abort) ate o loader expor a fundacao.
dl_foundation <- function(name) {
  f <- get0(name, envir = globalenv(), mode = "function", inherits = FALSE)
  if (is.null(f))
    dl_abort("plugin.contract", where = "dessem-linear/inference", reason = paste0("fundacao sem ", name),
             details = list(missing = name))
  f
}

## Envelope bruto (area, values, delivery, meta): hourly = data.table(DataHora, Carga) O..O+8 no frame de modelagem.
.dl_envelope <- function(hourly, a, cuts, warnings = character()) {
  x <- data.table::data.table(interval_end = hourly$DataHora, y_pred = hourly$Carga)
  acc <- new.env(parent = emptyenv()); acc$ws <- character()
  rule <- function(z) { r <- dl_semihourly_rule(z); acc$sh <- r; r }          # a regra roda UMA vez (aviso unico)
  delivery <- withCallingHandlers(
    dl_foundation("to_semihourly")(x, rule, cuts$available_until, cuts$origin_day + (plugin_constants()$horizon_days - 1L)),
    warning = function(w) {
      acc$ws <- c(acc$ws, if (inherits(w, "prevcarga_warning")) w$id else sub("^\\[([^]]+)\\].*$", "\\1", conditionMessage(w)))
      invokeRestart("muffleWarning")
    })
  v <- matrix(as.numeric(acc$sh), nrow = plugin_constants()$horizon_days, ncol = 48L, byrow = TRUE)
  list(area = a$area, values = v, delivery = delivery,
       meta = list(warnings = unique(c(warnings, acc$ws)), rng_mode = a$rng_mode, dm_head_refit = TRUE))
}

## Estagio infer. `artifact` = payload (congelado ou nao; o thaw do motor e idempotente).
plugin_infer <- function(artifact, inf_feat, cuts) {
  a <- dl_thaw(artifact)
  set.seed(as.integer(a$seed))                                    # mesma posicao de RNG do oraculo
  head <- dl_dm_head(inf_feat$engine, inf_feat$serieDM, inf_feat$config_mdl, inf_feat$mm,
                     inf_feat$cuts$origin_day, constnorm = a$norm$dm_carga)           # unico ajuste do infer
  acc <- new.env(parent = emptyenv()); acc$w <- character()
  hourly <- withCallingHandlers(.dl_predict_pass(a, inf_feat, head),
    dessem_engine_guard = function(cnd) { acc$w <- c(acc$w, cnd$code); invokeRestart("muffleMessage") })
  .dl_envelope(hourly, a, cuts, warnings = acc$w)
}

dl_inference <- function(payload, inf_features, cuts) plugin_infer(payload, inf_features, cuts)

## Forecast acoplado, forma do ticket (panel, cuts, spec; frame de modelagem): MESMO codigo do train + infer, sem freeze
## no meio. O treino ve so ate O-1 (origin_load vazio); o infer ve o dia O ate available_until.
plugin_forecast <- function(panel, cuts, spec) {
  frame <- plugin_ingest(panel, cuts, spec)
  tr <- frame
  tr$origin_load <- frame$origin_load[0L]
  tr$hourly_load <- frame$hourly_load[dl_day_of(interval_end) <= frame$last_data_day]
  feats <- plugin_build_features(tr, spec)
  art <- plugin_train(feats, spec)
  inf <- dl_inference_features(frame, art)
  plugin_infer(art, inf, cuts)
}

## Estagio `forecast` do descriptor (formais do loader): payload ja treinado em memoria + features de infer -> infer.
dl_forecast <- function(payload, inf_features, cuts) plugin_infer(payload, inf_features, cuts)
