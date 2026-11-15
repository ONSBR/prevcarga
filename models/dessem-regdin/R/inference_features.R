## inference_features.R -- dessem-regdin (RD.3). So definicoes. Estagio `inference_features` (formais fixos do loader:
## frame, payload): features do motor sobre o painel de infer, `mm` do dia O ate available_until e a janela do painel (para a
## guarda de janela do infer). O artefato (payload) e a UNICA fonte de variante, semente e config_mdl: nada do run.yaml
## efetivo nem do ambiente. available_until vem do frame (ja no frame de modelagem, ADR-0012/0019); nunca e calculado aqui.
## Sem aritmetica de hora. E4-T3 (ADR-045 fase C): a normalizacao, a serie diaria historica, o mm historico e o perfil de feriado
## vem do ARTEFATO (payload$norm/serie_dm/hol_profile/columns); o deck de inferencia pode ter so 15 dias. Mapa para o oraculo: predict_dessem_regdin (TransformaDeck + mm) em dessem_regdin_predict.R.

rd_artifact_mismatch <- function(field, got, exp, where = "dessem-regdin/inference_features")
  rd_abort("plugin.regdin_artifact_mismatch", where = where,
           reason = sprintf("%s: artefato=%s esperado=%s", field, paste(format(got), collapse = ","),
                            paste(format(exp), collapse = ",")),
           details = list(field = field, artifact = got, expected = exp))

## Variante do artefato = a do config_mdl congelado (o payload nao carrega o rotulo; o carimbo e do despachante).
.rd_variant_of_config <- function(config_mdl) {
  keys <- plugin_constants()$model_key
  eng <- rd_engine()
  for (v in names(keys))
    if (isTRUE(all.equal(eng$getConfigModel(keys[[v]]), config_mdl, tolerance = 0))) return(v)
  rd_artifact_mismatch("config_mdl", "desconhecido", unname(keys))
}

## Confere, ANTES de qualquer ajuste, que o payload traz o que o infer consome e cobre os dias-alvo do deck. A conferencia de
## origem/model_id/contract_version/granularidade/split ja foi feita pelo despachante (prevcarga_plugin_check_artifact).
.rd_check_payload <- function(deck, payload) {
  need <- c("svm_perfil", "norm", "config_mdl", "day_types", "dias", "train_window", "arima_fingerprint", "seed")
  if (!is.list(payload) || length(setdiff(need, names(payload))))
    rd_artifact_mismatch("payload", if (is.list(payload)) names(payload) else class(payload), need)
  ## estado derivado (E4-T3): artefato v0.1.5 (sem os campos novos) e recusado pelo primeiro ausente, antes de qualquer ajuste
  ausente <- setdiff(c("serie_dm", "hol_profile", "columns"), names(payload))
  if (length(ausente))
    rd_artifact_mismatch(ausente[[1L]], NULL, "presente (artefato anterior ao estado derivado: retreine o membro)")
  mmc <- sprintf("mm_h%02d", 0:23)
  if (!is.data.frame(payload$serie_dm) || length(setdiff(c("Data", "Prev", "Carga", mmc), names(payload$serie_dm))))
    rd_artifact_mismatch("serie_dm", if (is.data.frame(payload$serie_dm)) names(payload$serie_dm) else class(payload$serie_dm),
                         c("Data", "Prev", "Carga", mmc))
  if (!is.list(payload$columns)) rd_artifact_mismatch("columns", class(payload$columns), "list")
  esp <- payload$dias[!is.na(payload$day_types[payload$dias]) & payload$day_types[payload$dias] != 0L]
  sem <- setdiff(esp, names(payload$hol_profile))
  if (length(sem)) rd_artifact_mismatch("hol_profile", names(payload$hol_profile), sem)
  O <- format(deck$dataPrev)
  if (!length(payload$dias) || !identical(payload$dias[[1L]], O))
    rd_artifact_mismatch("origin_day", if (length(payload$dias)) payload$dias[[1L]] else NA_character_, O)
  invisible(TRUE)
}

## Estagio resolvido por nome (`rd_inference_features`). `frame` = deck da ingestao (frame de modelagem).
rd_inference_features <- function(frame, payload) {
  deck <- frame
  .rd_check_payload(deck, payload)
  variant <- .rd_variant_of_config(payload$config_mdl)
  ## norm do ARTEFATO: a inferencia nao chama getNormaliza*; a serie do deck curto usa a normalizacao do treino
  feats <- plugin_build_features(deck, list(params = list(exogena = variant)), norm = payload$norm)
  if (!identical(feats$dias, payload$dias))
    rd_artifact_mismatch("dias", feats$dias, payload$dias)
  mm <- rd_morning_means(.rd_deck_panel(deck$hourly_load), deck$available_until, deck$tz)   # so horas completas
  if (!(deck$dataPrev %in% mm$Data)) mm <- mm[0L]      # nenhuma hora de O disponivel (00:00 de exibicao): sem cabeca
  ## ultimo dia historico do DECK (a guarda de janela o confere contra train_window$end); a serie e o mm historico vem do artefato
  hist_days <- feats$series$serieDM[Prev == FALSE]$Data
  feats$series$serieDM <- .rd_compose_serieDM(payload$serie_dm, feats$series$serieDM)
  mm <- .rd_compose_mm(payload$serie_dm, mm, deck$available_until, deck$dataPrev)
  list(features = feats, mm = mm, variant = variant,
       panel_window = list(start = min(hist_days), end = max(hist_days)),
       cuts = list(origin_day = deck$dataPrev, available_until = deck$available_until, tz = deck$tz))
}

## Forma do ticket: plugin_inference_features(deck, artifact, cuts). `cuts` ja esta no deck; o parametro so e conferido.
plugin_inference_features <- function(deck, artifact, cuts) {
  if (!is.null(cuts$available_until) && !identical(cuts$available_until, deck$available_until))
    rd_artifact_mismatch("available_until", deck$available_until, cuts$available_until)
  rd_inference_features(deck, artifact)
}
