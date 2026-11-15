## freeze.R -- dessem-linear (DL.6). So definicoes. Congela os SVMs como dado puro pelo serializador do motor (DL.2) e
## os descongela; o carimbo (model_id, variant, split, ..., train_origin = origin_day) e a varredura de pureza sao do
## despachante da fundacao (prevcarga_plugin_stamp/scan_pure), logo o artefato aqui nao carrega relogio nem caminho.

## data.frame puro (sem .internal.selfref nem chave de data.table): o que o scan_pure da fundacao aceita. Data em Date.
.dl_plain_df <- function(x) {
  d <- as.data.frame(x)
  attr(d, ".internal.selfref") <- NULL
  attr(d, "sorted") <- NULL
  if ("Data" %in% names(d)) d$Data <- lubridate::as_date(d$Data)
  d
}

dl_freeze <- function(model) {
  eng <- dl_engine()
  model$svm_dm <- lapply(model$svm_dm, eng$engine_svm_freeze)
  model$svm_perfil <- lapply(model$svm_perfil, function(d) lapply(d, eng$engine_svm_freeze))
  if (!is.null(model$dm_o_sample)) model$dm_o_sample <- .dl_plain_df(model$dm_o_sample)   # estado derivado (E4-T2)
  model
}

## Forma do estado derivado de um artefato congelado: so confere o que existe (artefato v0.1.5, sem os campos, e assunto
## de dl_check_payload, exit 2). Forma errada => plugin.dessem_linear_artifact_tampered (integrity, exit 9) nomeando o
## campo e a coluna.
.dl_check_state_shape <- function(p) {
  bad <- function(field, what, column = NA_character_)
    dl_abort("plugin.dessem_linear_artifact_tampered", where = "dessem-linear/thaw",
             reason = sprintf("%s: %s%s", field, what, if (is.na(column)) "" else paste0(" (", column, ")")),
             details = list(field = field, coluna = column))
  s <- p$dm_o_sample
  if (!is.null(s)) {
    if (!is.data.frame(s)) bad("dm_o_sample", "nao e data.frame")
    mmc <- sprintf("mm_h%02d", 0:23)
    for (k in c("Data", mmc)) if (!(k %in% names(s))) bad("dm_o_sample", "coluna ausente", k)
    if (!inherits(s$Data, "Date")) bad("dm_o_sample", "Data nao e Date", "Data")
    for (k in mmc) if (!is.numeric(s[[k]])) bad("dm_o_sample", "coluna nao numerica", k)
  }
  if (!is.null(p$dm_o_formula) && !(is.character(p$dm_o_formula) && length(p$dm_o_formula) == 1L))
    bad("dm_o_formula", "nao e character(1)")
  cl <- p$columns
  if (!is.null(cl)) {
    dias <- p$head_recipe$dias
    ok <- is.list(cl) && is.list(cl$dm) && is.list(cl$perfil) && identical(names(cl$dm), names(cl$perfil)) &&
      (is.null(dias) || identical(names(cl$dm), dias))
    if (!ok) bad("columns", "dm e perfil nao cobrem os mesmos dias do artefato")
    for (dp in names(cl$dm)) {
      if (!is.character(cl$dm[[dp]]$vars)) bad("columns", "dm sem vars", dp)
      pf <- cl$perfil[[dp]]
      if (length(pf) != 24L || !all(vapply(pf, function(z) is.character(z$vars), NA))) bad("columns", "perfil sem 24 horas com vars", dp)
    }
  }
  invisible(p)
}

## Falha de thaw => engine_abort ([prevcarga/integrity], exit 9) do motor; forma errada do estado derivado => artifact_tampered.
dl_thaw <- function(payload) {
  eng <- dl_engine()
  .dl_check_state_shape(payload)
  payload$svm_dm <- lapply(payload$svm_dm, eng$engine_svm_thaw)
  payload$svm_perfil <- lapply(payload$svm_perfil, function(d) lapply(d, eng$engine_svm_thaw))
  if (!is.null(payload$dm_o_sample)) payload$dm_o_sample <- data.table::as.data.table(payload$dm_o_sample)
  payload
}

plugin_freeze <- function(artifact, spec, cuts, ctx) dl_freeze(artifact)
plugin_thaw <- function(x) dl_thaw(x)
