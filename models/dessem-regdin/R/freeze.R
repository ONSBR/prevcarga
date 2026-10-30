## freeze.R -- dessem-regdin (RD.2). So definicoes. Congela os SVMs de perfil como dado puro pelo serializador do motor
## (engine_svm_freeze, DL.2) e os descongela. SEM mable do fable (o ARIMA e re-fitado no infer, RD.3, conferido pelo
## `arima_fingerprint`). O carimbo comum (model_id, variant, split, ..., train_origin, origin_day) e a varredura de
## pureza sao do despachante da fundacao (prevcarga_plugin_stamp/scan_pure): o artefato aqui nao carrega relogio nem
## caminho. A persistencia (saveRDS) e do prevcarga_freeze (M1.3), nunca do plugin.

rd_freeze <- function(model) {
  model$arima_dm <- NULL                                         # defensivo: mable nunca entra no artefato
  model$svm_perfil <- lapply(model$svm_perfil, function(d) if (is.null(d)) NULL else lapply(d, engine_svm_freeze))
  model
}

## Forma esperada: 24 SVMs por dia normal; feriado = NULL. Falha de thaw => engine_abort ([prevcarga/integrity], exit 9).
rd_thaw <- function(payload) {
  need <- c("svm_perfil", "norm", "config_mdl", "day_types", "dias", "train_window", "arima_fingerprint")
  bad <- function(why) rd_abort("plugin.regdin_artifact_tampered", where = "dessem-regdin/thaw", reason = why,
                                details = list(reason = why))
  miss <- setdiff(need, names(payload))
  if (length(miss)) bad(paste0("campo ausente: ", paste(miss, collapse = ",")))
  payload$svm_perfil <- lapply(payload$svm_perfil, function(d) if (is.null(d)) NULL else lapply(d, engine_svm_thaw))
  ok <- vapply(names(payload$svm_perfil), function(dp) {
    d <- payload$svm_perfil[[dp]]
    dt <- payload$day_types[[dp]]
    if (is.null(d)) !is.na(dt) && dt != 0L else length(d) == 24L
  }, NA)
  if (!all(ok)) bad(paste0("forma do svm_perfil incoerente nos dias: ", paste(names(ok)[!ok], collapse = ",")))
  payload
}

plugin_freeze <- function(model) rd_freeze(model)
plugin_thaw <- function(payload) rd_thaw(payload)
