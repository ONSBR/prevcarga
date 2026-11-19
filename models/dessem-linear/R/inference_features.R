## inference_features.R -- dessem-linear (DL.7). So definicoes. Estagio `inference_features` (formais fixos do loader:
## frame, payload): `mm[O]` do painel ate available_until + series do motor com o config_mdl DO ARTEFATO.
## O artefato (payload) e a UNICA fonte de variante, semente e config_mdl: nada do run.yaml efetivo nem do ambiente.
## available_until vem de cuts via ingestao (deck$available_until, ja no frame de modelagem); nunca e calculado aqui.
## Sem aritmetica de hora. A normalizacao vem de payload$norm (E4-T1/T2); o deck de inferencia pode ter so 15 dias. Mapa para o oraculo: predict_dessem_linear (TransformaDeck + mm) em dessem_linear_predict.R.

## Confere, ANTES de qualquer ajuste, que o artefato e deste deck (origem, area, horizonte, variante). A conferencia de
## origem/model_id/contract_version/granularidade/split ja foi feita pelo despachante (prevcarga_plugin_check_artifact);
## aqui fica a local: variante/model_key (contra o membro, quando o deck a conhece) e coerencia interna do artefato.
## Id de dois segmentos: plugin.dessem_linear_artifact_mismatch (contract, exit 2).
dl_check_payload <- function(deck, payload) {
  mis <- function(field, got, exp)
    dl_abort("plugin.dessem_linear_artifact_mismatch", where = "dessem-linear/inference_features",
             reason = sprintf("%s: artefato=%s esperado=%s", field, paste(format(got), collapse = ","),
                              paste(format(exp), collapse = ",")),
             details = list(field = field, artifact = got, expected = exp))
  need <- c("svm_dm", "svm_perfil", "norm", "head_recipe", "variant", "model_key", "area", "horizon_days", "seed")
  if (!is.list(payload) || length(setdiff(need, names(payload))))
    mis("payload", if (is.list(payload)) names(payload) else class(payload), need)
  ## estado derivado (E4-T2, ADR-045 fase B): artefato v0.1.5 (sem os campos novos) e recusado pelo primeiro ausente
  nova <- c("dm_o_sample", "dm_o_formula", "columns")
  ausente <- setdiff(nova, names(payload))
  if (length(ausente)) mis(ausente[[1L]], NULL, "presente (artefato anterior ao estado derivado: retreine o membro)")
  if (!is.data.frame(payload$dm_o_sample)) mis("dm_o_sample", class(payload$dm_o_sample), "data.frame")
  mmc <- sprintf("mm_h%02d", 0:23)
  if (length(setdiff(c("Data", mmc), names(payload$dm_o_sample))))
    mis("dm_o_sample", names(payload$dm_o_sample), c("Data", mmc))
  if (!(is.character(payload$dm_o_formula) && length(payload$dm_o_formula) == 1L && !is.na(payload$dm_o_formula)))
    mis("dm_o_formula", class(payload$dm_o_formula), "character(1)")
  if (!(is.list(payload$columns) && all(c("dm", "perfil") %in% names(payload$columns))))
    mis("columns", if (is.list(payload$columns)) names(payload$columns) else class(payload$columns), c("dm", "perfil"))
  O <- format(deck$dataPrev)
  dias <- payload$head_recipe$dias
  if (!length(dias) || !identical(dias[[1L]], O)) mis("origin_day", if (length(dias)) dias[[1L]] else NA_character_, O)
  keys <- plugin_constants()$model_key
  if (!(payload$variant %in% names(keys)) || !identical(unname(keys[[payload$variant]]), payload$model_key))
    mis("model_key", payload$model_key, unname(keys[payload$variant]))
  if (!is.na(deck$variant) && !identical(deck$variant, payload$variant)) mis("variant", payload$variant, deck$variant)
  if (!is.na(deck$model_key) && !identical(deck$model_key, payload$model_key))
    mis("model_key", payload$model_key, deck$model_key)
  if (!identical(payload$area, deck$area)) mis("area", payload$area, deck$area)
  ## clima_hist (E5-T4): payload sem o bloco = observado (nao_se_aplica se a variante nao usa clima); o deck traz o modo efetivo
  if (!is.null(deck$clima_hist)) {
    mp <- if (!is.null(payload$clima_hist$modo)) payload$clima_hist$modo else
      if (dl_variant_weatherless(payload$model_key)) "nao_se_aplica" else "observado"
    if (!identical(mp, deck$clima_hist$modo)) mis("clima_hist", mp, deck$clima_hist$modo)
  }
  if (!identical(as.integer(payload$horizon_days), as.integer(deck$horizon)))
    mis("horizon_days", payload$horizon_days, deck$horizon)
  invisible(TRUE)
}

## Estagio resolvido por nome (`dl_inference_features`). `frame` = deck da ingestao (frame de modelagem).
dl_inference_features <- function(frame, payload) {
  deck <- frame
  dl_check_payload(deck, payload)
  mm <- dl_morning_means(deck$hourly_load, deck$available_until, deck$dataPrev)   # so horas completas
  ## norm do ARTEFATO (T1): a inferencia nao chama getNormaliza*; a serie do deck curto usa a normalizacao do treino
  feats <- plugin_build_features(deck, list(params = list(exogena = payload$variant)), norm = payload$norm)
  if (!isTRUE(all.equal(feats$config_mdl, payload$head_recipe$config_mdl)))
    dl_abort("plugin.dessem_linear_artifact_mismatch", where = "dessem-linear/inference_features",
             reason = "config_mdl: artefato difere do motor desta variante",
             details = list(field = "config_mdl", variant = payload$variant))
  list(processado = feats$processado, serieDM = feats$serieDM, seriePF = feats$seriePF, mm = mm,
       config_mdl = payload$head_recipe$config_mdl, norm = payload$norm,
       cuts = list(origin_day = deck$dataPrev, available_until = deck$available_until, tz = deck$tz),
       engine = dl_engine())
}

## Forma do ticket: plugin_inference_features(deck, artifact, cuts). `cuts` ja esta no deck; o parametro so e conferido.
plugin_inference_features <- function(deck, artifact, cuts) {
  if (!is.null(cuts$available_until) && !identical(cuts$available_until, deck$available_until))
    dl_abort("plugin.dessem_linear_artifact_mismatch", where = "dessem-linear/inference_features",
             reason = "available_until do deck difere de cuts", details = list(field = "available_until"))
  dl_inference_features(deck, artifact)
}
