## freeze.R -- dessem-linear (DL.6). So definicoes. Congela os SVMs como dado puro pelo serializador do motor (DL.2) e
## os descongela; o carimbo (model_id, variant, split, ..., train_origin = origin_day) e a varredura de pureza sao do
## despachante da fundacao (prevcarga_plugin_stamp/scan_pure), logo o artefato aqui nao carrega relogio nem caminho.

dl_freeze <- function(model) {
  eng <- dl_engine()
  model$svm_dm <- lapply(model$svm_dm, eng$engine_svm_freeze)
  model$svm_perfil <- lapply(model$svm_perfil, function(d) lapply(d, eng$engine_svm_freeze))
  model
}

## Falha de thaw => engine_abort ([prevcarga/integrity], exit 9) do motor.
dl_thaw <- function(payload) {
  eng <- dl_engine()
  payload$svm_dm <- lapply(payload$svm_dm, eng$engine_svm_thaw)
  payload$svm_perfil <- lapply(payload$svm_perfil, function(d) lapply(d, eng$engine_svm_thaw))
  payload
}

plugin_freeze <- function(artifact, spec, cuts, ctx) dl_freeze(artifact)
plugin_thaw <- function(x) dl_thaw(x)
