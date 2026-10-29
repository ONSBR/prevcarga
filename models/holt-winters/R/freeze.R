## freeze.R -- freeze/thaw puro do plugin Holt-Winters (HW.3). SO definicoes. Contrato comum da fundacao (P.0d): o plugin
## devolve DADO PURO (matrizes $mean no lugar dos objetos forecast/ets + snapshots do SemiFer); o carimbo comum (load_label,
## timezone, weather_issue_cutoff, split, granularity_min, contract_version, variant) e do prevcarga_plugin_stamp no
## despachante e a persistencia/sha256 do prevcarga_freeze (M1.3). Chaveado pela origem O: nunca `available_until`, relogio,
## caminho, call ou config. Integridade (bytes adulterados => exit 9) e origem/model_id/granularidade sao da fundacao.

.hw_freeze_keep <- function() c("model_id", "origin_day", "train_origin", "mean_interval", "mean_daily", "datainiver", "datafimver",
                     "align_first_idx", "h_fit", "n_steps_fit", "m_mes", "params", "seed",
                     "carga_temp_ver_fer", "carga_temp_med1", "carga_temp_ver")

.hw_frz_abort <- function(field, reason)
  hw_abort("hw.bad_artifact", sprintf("%s: %s", field, reason), list(field = field), where = "holt-winters/freeze")

.hw_clean_df <- function(x) { x <- as.data.frame(x); rownames(x) <- NULL; x }

## model (plugin_train) -> payload puro, so os campos que o infer le, tipos normalizados.
plugin_freeze <- function(model) {
  miss <- setdiff(.hw_freeze_keep(), names(model))
  if (length(miss)) .hw_frz_abort(miss[[1L]], "campo ausente no modelo")
  art <- model[.hw_freeze_keep()]
  h <- as.integer(art$h_fit)
  art$mean_interval <- unname(matrix(as.double(art$mean_interval), nrow = h))
  art$mean_daily <- as.double(art$mean_daily)
  art$h_fit <- h
  art$n_steps_fit <- as.integer(art$n_steps_fit)
  art$align_first_idx <- as.integer(art$align_first_idx)
  art$m_mes <- as.integer(art$m_mes)
  art$seed <- as.integer(art$seed)
  for (k in c("carga_temp_ver_fer", "carga_temp_med1", "carga_temp_ver")) art[[k]] <- .hw_clean_df(art[[k]])
  .hw_foundation("prevcarga_plugin_scan_pure")(art)
  art
}

## payload -> modelo pronto para o infer; forma errada => [prevcarga/contract] nomeando o campo.
plugin_thaw <- function(artifact) {
  a <- artifact
  mi <- a$mean_interval
  if (!is.matrix(mi) || !is.double(mi) || !identical(dim(mi), c(9L, 48L)))
    .hw_frz_abort("mean_interval", "esperado matriz double 9x48")
  if (!is.double(a$mean_daily) || length(a$mean_daily) != 9L) .hw_frz_abort("mean_daily", "esperado double de comprimento 9")
  if (!identical(a$align_first_idx, 2L)) .hw_frz_abort("align_first_idx", "esperado 2L")
  if (!identical(a$h_fit, 9L) || !identical(a$n_steps_fit, 8L)) .hw_frz_abort("h_fit", "esperado h_fit 9L e n_steps_fit 8L")
  for (k in c("origin_day", "train_origin", "datainiver", "datafimver"))
    if (!inherits(a[[k]], "Date") || length(a[[k]]) != 1L || is.na(a[[k]])) .hw_frz_abort(k, "esperado Date")
  for (k in c("carga_temp_ver_fer", "carga_temp_ver"))
    if (!is.data.frame(a[[k]]) || ncol(a[[k]]) != 105L) .hw_frz_abort(k, "esperado data.frame de 105 colunas")
  if (!is.data.frame(a$carga_temp_med1) || ncol(a$carga_temp_med1) != 6L)
    .hw_frz_abort("carga_temp_med1", "esperado data.frame de 6 colunas")
  a
}

hw_freeze <- function(model) plugin_freeze(model)
hw_thaw <- function(payload) plugin_thaw(payload)
