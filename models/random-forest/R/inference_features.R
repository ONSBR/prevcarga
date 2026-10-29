## inference_features.R -- estagio de features de inferencia do plugin random-forest (RF.3). SO definicoes.
## Monta as linhas das DUAS origens nativas do infer: O-1 (carga ate O-2, cobre o dia O) e O (carga ate O-1, cobre
## O+1..O+8). O RF nao le a carga do dia O: o rf_frame de infer (ingestao) so traz carga ate O-1. Reusa
## plugin_build_features (RF.2) e CONFERE, por sub-modelo, que toda feature guardada no treino existe e nao e NA nas duas
## linhas: feature faltante => [prevcarga/contract]; NA => [prevcarga/completude] (exit 3) nomeando origem e feature. Sem o
## "degrade gracefully" do oraculo (predict_rf.R:395-420): nunca ha predicao com subconjunto de features. Sem `params`:
## tudo vem do artefato. Sem relogio, sem variavel de ambiente, nenhuma hora do rotulo. Saida ASCII.

## Sub-modelos do artefato: aceita o artefato carimbado (prevcarga_plugin_artifact) ou o payload puro do despachante.
.rf_payload <- function(artifact) {
  p <- if (inherits(artifact, "prevcarga_plugin_artifact")) artifact$payload else artifact
  tg <- all_target_names()
  if (!is.list(p) || !all(tg %in% names(p)))
    .rf_abort("plugin.contract", details = list(reason = "artefato sem os 216 sub-modelos"),
              where = "random-forest/inference_features")
  p
}

.rf_inf_abort <- function(id, reason, ..., where = "random-forest/inference_features")
  .rf_abort(id, details = c(list(reason = reason), list(...)), where = where)

## (frame, artifact, cuts) -> rf_inf_features. `cuts` e opcional (o estagio despachado so recebe (frame, payload)); quando
## vem, o frame e conferido contra a origem do corte.
plugin_inference_features <- function(frame, artifact, cuts = NULL) {
  payload <- .rf_payload(artifact)
  if (!is.list(frame) || !identical(frame$phase, "infer"))
    .rf_inf_abort("plugin.contract", "rf_frame nao e da fase infer", phase = if (is.list(frame)) frame$phase else NA)
  origins <- sort(lubridate::as_date(frame$origins))
  if (length(origins) != 2L || origins[[2L]] - origins[[1L]] != 1L)
    .rf_inf_abort("plugin.contract", "o frame de infer deve guardar as origens nativas c(O-1, O)",
                  origins = paste(format(origins), collapse = ","))
  if (!is.null(cuts) && !identical(origins[[2L]], .rf_origin(cuts)))
    .rf_inf_abort("plugin.contract", "origem O do frame diferente da origem do corte", origem = format(origins[[2L]]))
  tbl <- plugin_build_features(frame, list())$table
  rows <- tbl[match(origins, tbl$dataOrigem), c("dataOrigem", all_feature_names()), with = FALSE]
  if (anyNA(rows$dataOrigem))
    .rf_inf_abort("plugin.completude", "origem nativa ausente das features",
                  origem = format(origins[is.na(rows$dataOrigem)][[1L]]))
  for (j in names(rows)) if (is.logical(rows[[j]])) data.table::set(rows, j = j, value = as.integer(rows[[j]]))
  req <- unique(unlist(lapply(all_target_names(), function(tg) payload[[tg]]$features), use.names = FALSE))
  absent <- setdiff(req, names(rows))
  if (length(absent))
    .rf_inf_abort("plugin.contract", "feature do artefato ausente das features de infer", feature = absent[[1L]])
  for (i in seq_along(origins)) {
    na <- req[vapply(req, function(f) is.na(rows[[f]][[i]]), NA)]
    if (length(na))
      .rf_inf_abort("plugin.completude", "feature exigida pelo artefato e NA na origem nativa",
                    origem = format(origins[[i]]), feature = na[[1L]])
  }
  rows[, dataOrigem := origins]
  list(origins = origins, rows = rows, blocks = c(dia_O = "O-1", resto = "O"), area = frame$area,
       train_origin = payload$train_origin)
}

## Assinatura canonica do despachante (frame, payload).
rf_inference_features <- function(frame, payload) plugin_inference_features(frame, payload, NULL)
