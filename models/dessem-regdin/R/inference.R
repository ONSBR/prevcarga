## inference.R -- dessem-regdin (RD.3). So definicoes. Estagios `inference` e `forecast` + forma do ticket.
## Mapa para o oraculo (dessem_regdin_predict.R / dessem_regdin_infer.R):
##   .rd_predict_pass -> .rd_predict_pass (DM refeita pelo caminho do motor; perfil dia a dia com write-back de INPUT)
##   predict_dessem_regdin (TransformaDeck + mm) -> rd_inference_features
##   dessem_regdin_infer (desagregaModeloSH, matriz 9x48) -> .rd_envelope
## O ARIMA nao e congelado (sem mable): o infer o re-fita sobre a serie <= O-1 e CONFERE o `arima_fingerprint` do treino;
## divergencia aborta, nunca entrega. Com `mm` (available_until > 00:00) a cabeca de nowcast roda DEPOIS de
## set.seed(artifact$seed) e o ARIMA re-ancorado em O preve O+1..; sem `mm` vale o ARIMA base. Os SVMs de perfil vem
## congelados (thaw); dias especiais recomputam RL_perfilFeriado. Nenhuma leitura de ambiente; RNG do chamador: restaurado
## pelo despachante (prevcarga_plugin_infer). Sem aritmetica de hora: a regra semi-horaria e o carimbo de 30 min sao de
## to_semihourly (fundacao); o deslocamento de 60 min, do despachante.

## Coleta as guardas classificadas do motor (dessem_engine_guard) e os avisos [id] do plugin em acc$w, para meta$warnings.
## `quiet` silencia os avisos NAO classificados do fable (o oraculo os suprime com suppressWarnings no ARIMA).
.rd_guarded <- function(acc, expr, quiet = FALSE) {
  withCallingHandlers(expr,
    dessem_engine_guard = function(cnd) { acc$w <- c(acc$w, cnd$code); invokeRestart("muffleMessage") },
    warning = function(w) {
      classified <- inherits(w, "prevcarga_warning") || grepl("^\\[", conditionMessage(w))
      if (classified) {
        acc$w <- c(acc$w, if (inherits(w, "prevcarga_warning")) w$id else sub("^\\[([^]]+)\\].*$", "\\1", conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
      if (quiet) invokeRestart("muffleWarning")
    })
}

## Guarda de janela: o painel do infer cobre EXATAMENTE a janela do treino (origin_scoped, history_infer_equals_train).
## `.bypass_window_guard` existe so para o CONTROL do teste (ausente do descriptor e do despachante).
.rd_check_window <- function(inf_feat, artifact, .bypass_window_guard = FALSE) {
  if (isTRUE(.bypass_window_guard)) return(invisible(FALSE))
  got <- inf_feat$panel_window; exp <- artifact$train_window
  if (!identical(as.numeric(got$start), as.numeric(exp$start)) || !identical(as.numeric(got$end), as.numeric(exp$end)))
    rd_abort("plugin.regdin_window_mismatch", where = "dessem-regdin/infer",
             reason = sprintf("painel %s..%s; treino %s..%s", format(got$start), format(got$end), format(exp$start),
                              format(exp$end)),
             details = list(expected = format(c(exp$start, exp$end)), got = format(c(got$start, got$end))))
  invisible(TRUE)
}

## Refit do ARIMA base (execDemandaMediaTS sobre a serie <= O-1, SEM cabeca) e conferencia contra o fingerprint do treino:
## ordem, coeficientes por nome (tolerancia 0) e dm_digest. Qualquer divergencia aborta nomeando o campo/coeficiente.
## `serieDM` e COPIADA (o motor atualiza a serie por referencia e o passe de predict parte da serie limpa).
.rd_check_fingerprint <- function(serieDM, config_mdl, fp, acc = new.env()) {
  drift <- function(field, ..., reason = paste0("campo ", field, if (!is.null(list(...)$coef))
                                                 paste0(": ", paste(list(...)$coef, collapse = ","))))
    rd_abort("plugin.regdin_arima_drift", where = "dessem-regdin/infer", reason = reason,
             details = c(list(field = field), list(...)))
  base <- tryCatch(.rd_guarded(acc, execDemandaMediaTS(data.table::copy(serieDM), config_mdl), quiet = TRUE),
                   error = function(e) drift("refit", message = conditionMessage(e)))
  ## o fable (0.5.0) devolve um mable de "NULL model" SEM erro nem aviso quando a janela nao traz os 12 niveis do fator Mes
  ## ou os regressores sao colineares: o fingerprint nao existiria, entao aborta nomeando a causa provavel
  if (inherits(base$mdl$modeloTreinado[[1L]][[1L]]$fit, "null_mdl"))
    drift("null_model", reason = paste0("campo null_model: o ARIMA refeito devolveu modelo NULL; causa provavel: a janela do ",
                                        "painel nao cobre os 12 meses do fator Mes ou os regressores sao colineares"))
  got <- tryCatch(.rd_arima_fingerprint(base$mdl$modeloTreinado, serieDM),
                  error = function(e) drift("refit", message = conditionMessage(e)))
  if (!identical(unname(got$order), unname(fp$order)))
    drift("order", expected = unname(fp$order), got = unname(got$order))
  if (!identical(got$dm_digest, fp$dm_digest)) drift("dm_digest", expected = fp$dm_digest, got = got$dm_digest)
  if (!setequal(names(got$coef), names(fp$coef)))
    drift("coef", coef = setdiff(union(names(got$coef), names(fp$coef)), intersect(names(got$coef), names(fp$coef))))
  bad <- names(fp$coef)[got$coef[names(fp$coef)] != fp$coef]
  if (length(bad)) drift("coef", coef = bad)
  invisible(base)
}

## o motor devolve `tune` em memoria (forecast acoplado); congelado/descongelado e o proprio best.model
.rd_best <- function(m) if (inherits(m, "tune")) m$best.model else m

## Predicao com guarda: preditor NA na linha de entrada (lag sem dado) vira erro de completude com dia e variavel,
## em vez do erro opaco do predict.wsvm.
.rd_predict_row <- function(model, row, what) {
  vars <- intersect(all.vars(stats::delete.response(stats::terms(model))), names(row))
  na <- vars[vapply(vars, function(v) anyNA(row[[v]]), NA)]
  if (length(na))
    rd_abort("plugin.regdin_series_gap", where = "dessem-regdin/inference",
             reason = sprintf("%s: preditor sem dado (%s)", what, paste(na, collapse = ",")),
             details = list(serie = what, preditores = na))
  stats::predict(model, row)
}

## Passe de predict (porta de dessem_regdin_predict.R:50-107). DM = cabeca de nowcast (com mm, apos set.seed) ou ARIMA base;
## perfil dia a dia EM ORDEM (write-back de input); dia normal = 24 SVMs congelados, dia especial = RL_perfilFeriado.
.rd_predict_pass <- function(inf_feat, artifact, acc = new.env()) {
  f <- inf_feat$features; cfg <- artifact$config_mdl
  O <- inf_feat$cuts$origin_day
  serieDM <- data.table::copy(f$series$serieDM); seriePF <- data.table::copy(f$series$seriePF)
  mm <- inf_feat$mm
  if (nrow(mm)) mm <- rd_normalize_mm(mm, cfg, artifact$norm$dm_carga)
  set.seed(as.integer(artifact$seed))                                  # mesma posicao de RNG do treino/oraculo
  dm <- .rd_guarded(acc, rd_dm_nowcast(serieDM, cfg, mm, O, as.integer(artifact$seed)), quiet = TRUE)
  inferDM <- data.table::copy(data.table::as.data.table(dm$infer))
  inferDM[, Carga.infer := Carga.infer / artifact$norm$pf_cargadm]
  addValuesDMRef(seriePF, inferDM, "CargaMed")
  for (dp in f$dias) {
    dt <- f$day_types[[dp]]
    if (!is.na(dt) && dt != 0L) {                                      # especial: lm recomputado (nao ha SVM)
      .rd_guarded(acc, RL_perfilFeriado(dp, seriePF, cfg, artifact$norm$pf_cargadm), quiet = TRUE)
      next
    }
    svm <- artifact$svm_perfil[[dp]]
    if (is.null(svm))
      rd_artifact_mismatch("svm_perfil", paste0("sem SVM no dia normal ", dp), "24 SVMs", where = "dessem-regdin/inference")
    cmed <- inferDM[Data == lubridate::as_date(dp)]$Carga.infer[1L]
    for (h in 0:23) {
      cp <- obterDiaEspPerfilConfig(dp, seriePF, cfg)
      dEnt <- entradaPerfil(serie = seriePF, tipoEntrada = cp$tpEntrada, diaSemana = cp$ds, hora = h, diaAno = cp$diasAno,
                            lagsmodelo = cp$hrlag, hv = seriePF[Data == dp]$HV[1L], lagsdsFer = cp$lagds, tpFer = cp$diaEsp,
                            addHourFac = FALSE)
      x <- dEnt[Data == dp][, CargaMed := cmed]
      y <- .rd_guarded(acc, .rd_predict_row(.rd_best(svm[[h + 1L]]), x, sprintf("perfil %s h%02d", dp, h)))
      seriePF[Data == dp & lubridate::hour(DataHora) == h, `:=`(CargaMed = cmed, Carga = y[1L])]
    }
  }
  seriePF[, CargaMed := CargaMed * artifact$norm$pf_cargadm]
  seriePF[, Carga := Carga * CargaMed]
  out <- seriePF[Prev == TRUE, .(DataHora, Carga)]
  if (anyNA(out$Carga))
    rd_abort("plugin.regdin_series_gap", where = "dessem-regdin/inference",
             reason = sprintf("previsao horaria com NA em %d horas", sum(is.na(out$Carga))),
             details = list(n_na = sum(is.na(out$Carga))))
  out
}

## `to_semihourly` e da fundacao e o env do plugin so enxerga base + attach: busca interim no globalenv (mesmo padrao de
## rd_abort/rd_warn) ate o loader expor a fundacao. Divida pinada em test-rd1-plugin.R.
rd_foundation <- function(name) {
  f <- get0(name, envir = globalenv(), mode = "function", inherits = FALSE)
  if (is.null(f)) rd_abort("plugin.stage_missing", where = "dessem-regdin/inference", reason = paste0("fundacao sem ", name))
  f
}

## Envelope bruto (area, values, delivery, meta): hourly = data.table(DataHora, Carga) O..O+8 no frame de modelagem.
.rd_envelope <- function(hourly, area, cuts, acc, head_used) {
  x <- data.table::data.table(interval_end = hourly$DataHora, y_pred = hourly$Carga)
  rule <- function(z) { r <- rd_semihourly_rule(z); acc$sh <- r; r }       # a regra roda UMA vez (aviso unico)
  delivery <- .rd_guarded(acc, rd_foundation("to_semihourly")(x, rule, cuts$available_until,
                                                              cuts$origin_day + (plugin_constants()$horizon_days - 1L)))
  v <- matrix(as.numeric(acc$sh), nrow = plugin_constants()$horizon_days, ncol = 48L, byrow = TRUE)
  list(area = area, values = v, delivery = delivery,
       meta = list(warnings = unique(acc$w), head_used = head_used))
}

## Estagio infer. `artifact` = payload (congelado ou nao; o thaw e idempotente). `.bypass_window_guard` so para teste:
## pula a guarda de janela E a conferencia do fingerprint (o dm_digest de outra janela diverge por construcao).
plugin_infer <- function(artifact, inf_feat, cuts, .bypass_window_guard = FALSE) {
  a <- plugin_thaw(artifact)
  acc <- new.env(parent = emptyenv()); acc$w <- character(); acc$sh <- NULL
  .rd_check_window(inf_feat, a, .bypass_window_guard)
  if (!isTRUE(.bypass_window_guard))
    .rd_check_fingerprint(inf_feat$features$series$serieDM, a$config_mdl, a$arima_fingerprint, acc)
  hourly <- .rd_predict_pass(inf_feat, a, acc)
  .rd_envelope(hourly, inf_feat$features$deck$area, cuts, acc, head_used = nrow(inf_feat$mm) > 0L)
}

rd_inference <- function(payload, inf_features, cuts) plugin_infer(payload, inf_features, cuts)

## Forecast acoplado, forma do ticket (panel, cuts, spec; frame de modelagem): MESMO codigo do train + infer, em memoria
## (freeze/thaw sem disco). O treino ve so ate O-1 (origin_load vazio); o infer ve o dia O ate available_until.
plugin_forecast <- function(panel, cuts, spec) {
  frame <- plugin_ingest(panel, cuts, spec)
  tr <- frame
  tr$origin_load <- frame$origin_load[0L]
  feats <- plugin_build_features(tr, spec)
  art <- plugin_thaw(plugin_freeze(plugin_train(feats, spec)))
  inf <- rd_inference_features(frame, art)
  plugin_infer(art, inf, cuts)
}

## Estagio `forecast` do descriptor (formais do loader): payload ja treinado em memoria + features de infer -> infer.
rd_forecast <- function(payload, inf_features, cuts) plugin_infer(payload, inf_features, cuts)
