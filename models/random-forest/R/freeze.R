## freeze.R -- freeze/thaw puro do plugin random-forest (RF.2). SO definicoes. Contrato comum da fundacao (P.0d):
## o plugin devolve DADO PURO (216 sub-modelos + feature_cols/target_cols/train_origin/n_train); o carimbo (model_id,
## split, granularity_min, load_label, timezone, weather_issue_cutoff, params, seed) e do prevcarga_plugin_stamp e a
## persistencia/sha256 do prevcarga_freeze (M1.3). Sem saveRDS, sem relogio, sem caminho, sem hostname, sem
## trained_at/package_version, sem variavel de ambiente. Saida ASCII.

## Identidade do plugin para o carimbo (espelha register.R; nao chama plugin_descriptor(), que referencia os estagios de
## infer ainda nao carregados em testes do treino).
.rf_identity <- function() list(model_id = "random-forest", contract_version = 1L, variant_param = NA_character_,
                                granularity_supported = 60L)

.rf_freeze_abort <- function(reason, ...)
  .rf_abort("plugin.contract", details = c(list(reason = reason), list(...)), where = "random-forest/freeze")

## Zera qualquer environment que o objeto randomForest carregue (call/terms); a chamada vira constante.
.rf_strip_env <- function(fit) {
  fit$call <- quote(randomForest())
  if (!is.null(fit$terms)) attr(fit$terms, ".Environment") <- NULL
  fit
}

.rf_check_payload <- function(payload) {
  tg <- all_target_names()
  ok <- is.list(payload) && all(tg %in% names(payload))
  if (!ok) .rf_freeze_abort("artefato sem os 216 sub-modelos y_h001..y_h216")
  bad <- tg[!vapply(payload[tg], function(s) is.list(s) && inherits(s$model, "randomForest"), NA)]
  if (length(bad)) .rf_freeze_abort("sub-modelo invalido", modelo = bad[[1L]])
  invisible(TRUE)
}

## Assinatura canonica do despachante: model -> payload puro (o despachante carimba e varre a pureza).
rf_freeze <- function(model) {
  .rf_check_payload(model)
  subs <- lapply(model[all_target_names()], function(s) { s$model <- .rf_strip_env(s$model); s })
  c(subs, list(feature_cols = attr(model, "feature_cols"), target_cols = all_target_names(),
               train_origin = attr(model, "train_origin"), n_train = attr(model, "n_train")))
}

rf_thaw <- function(payload) {
  .rf_check_payload(payload)
  payload
}

## Forma do contrato do ticket: (model, cuts, spec) -> artefato carimbado e varrido. train_origin > cuts$train_origin
## (O-1-H) => [prevcarga/contract].
plugin_freeze <- function(model, cuts, spec) {
  payload <- rf_freeze(model)
  if (!inherits(payload$train_origin, "Date") || is.na(payload$train_origin) || payload$train_origin > cuts$train_origin)
    .rf_freeze_abort("train_origin do modelo alem de O-1-H", train_origin = format(payload$train_origin),
                     limite = format(cuts$train_origin))
  art <- .rf_foundation("prevcarga_plugin_stamp")(payload, list(descriptor = .rf_identity()), spec, cuts)
  .rf_foundation("prevcarga_plugin_scan_pure")(art, path = "random-forest/freeze")
  art
}

## Confere a estrutura de um artefato carimbado (model_id, granularidade, 216 sub-modelos); a conferencia contra os
## cortes (train_origin, split) e do prevcarga_plugin_check_artifact; bytes adulterados sao do prevcarga_plugin_thaw.
plugin_thaw <- function(artifact) {
  if (!inherits(artifact, "prevcarga_plugin_artifact") || !is.list(artifact$stamp))
    .rf_freeze_abort("artefato fora da classe prevcarga_plugin_artifact")
  s <- artifact$stamp
  if (!identical(s$model_id, .rf_identity()$model_id)) .rf_freeze_abort("model_id diferente", model_id = s$model_id)
  if (!identical(s$granularity_min, .rf_identity()$granularity_supported))
    .rf_freeze_abort("granularidade diferente", granularity_min = s$granularity_min)
  rf_thaw(artifact$payload)
  artifact
}
