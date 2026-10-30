## inference_features.R -- dessem-linear (DL.7). So definicoes. Estagio `inference_features` (formais fixos do loader:
## frame, payload): `mm[O]` do painel ate available_until + series do motor com o config_mdl DO ARTEFATO.
## O artefato (payload) e a UNICA fonte de variante, semente e config_mdl: nada do run.yaml efetivo nem do ambiente.
## available_until vem de cuts via ingestao (deck$available_until, ja no frame de modelagem); nunca e calculado aqui.
## Sem aritmetica de hora. Mapa para o oraculo: predict_dessem_linear (TransformaDeck + mm) em dessem_linear_predict.R.

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
  if (!identical(as.integer(payload$horizon_days), as.integer(deck$horizon)))
    mis("horizon_days", payload$horizon_days, deck$horizon)
  invisible(TRUE)
}

## Estagio resolvido por nome (`dl_inference_features`). `frame` = deck da ingestao (frame de modelagem).
dl_inference_features <- function(frame, payload) {
  deck <- frame
  dl_check_payload(deck, payload)
  mm <- dl_morning_means(deck$hourly_load, deck$available_until, deck$dataPrev)   # so horas completas
  feats <- plugin_build_features(deck, list(params = list(exogena = payload$variant)))
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
