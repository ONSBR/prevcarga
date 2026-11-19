## freeze.R -- dessem-regdin (RD.2). So definicoes. Congela os SVMs de perfil como dado puro pelo serializador do motor
## (engine_svm_freeze, DL.2) e os descongela. SEM mable do fable (o ARIMA e re-fitado no infer, RD.3, conferido pelo
## `arima_fingerprint`). O carimbo comum (model_id, variant, split, ..., train_origin, origin_day) e a varredura de
## pureza sao do despachante da fundacao (prevcarga_plugin_stamp/scan_pure): o artefato aqui nao carrega relogio nem
## caminho. A persistencia (saveRDS) e do prevcarga_freeze (M1.3), nunca do plugin.

rd_freeze <- function(model) {
  model$arima_dm <- NULL                                         # defensivo: mable nunca entra no artefato
  model$svm_perfil <- lapply(model$svm_perfil, function(d) if (is.null(d)) NULL else lapply(d, engine_svm_freeze))
  ## estado derivado (E4-T3): data.frame puro (sem selfref/chave de data.table, que o scan_pure recusa); formula ja e texto
  if (!is.null(model$serie_dm)) model$serie_dm <- .rd_plain_df(model$serie_dm)
  if (!is.null(model$hol_profile))
    model$hol_profile <- lapply(model$hol_profile, function(hp) { hp$sample <- .rd_plain_df(hp$sample); hp })
  model
}

## Forma do estado derivado de um artefato congelado: so confere o que existe (artefato v0.1.5, sem os campos, e assunto de
## .rd_check_payload/rd_thaw, exit 2). Forma errada => plugin.regdin_artifact_tampered (integrity, exit 9) nomeando campo e coluna.
.rd_check_state_shape <- function(p, bad) {
  s <- p$serie_dm
  if (!is.null(s)) {
    if (!is.data.frame(s)) bad("serie_dm nao e data.frame")
    mmc <- sprintf("mm_h%02d", 0:23)
    for (k in c("Data", "Prev", "Carga", mmc)) if (!(k %in% names(s))) bad(paste0("serie_dm: coluna ausente (", k, ")"))
    if (!inherits(s$Data, "Date")) bad("serie_dm: Data nao e Date")
    for (k in mmc) if (!is.numeric(s[[k]])) bad(paste0("serie_dm: coluna nao numerica (", k, ")"))
  }
  hp <- p$hol_profile
  if (!is.null(hp)) {
    if (!is.list(hp)) bad("hol_profile nao e lista")
    esp <- names(p$day_types)[!is.na(p$day_types) & p$day_types != 0L]
    sem <- setdiff(esp, names(hp))
    if (length(sem)) bad(paste0("hol_profile sem o dia especial ", paste(sem, collapse = ",")))
    for (dp in names(hp)) {
      h <- hp[[dp]]
      for (k in c("sd_hist", "sample", "coef", "xlevels", "formula", "diaAno", "HV", "dslag", "diaEsp"))
        if (!(k %in% names(h))) bad(sprintf("hol_profile %s: campo ausente (%s)", dp, k))
      if (!is.data.frame(h$sample) || !is.numeric(h$coef) || !(is.character(h$formula) && length(h$formula) == 1L))
        bad(sprintf("hol_profile %s: forma incoerente (sample/coef/formula)", dp))
    }
  }
  cl <- p$columns
  if (!is.null(cl)) {
    if (!is.list(cl) || !identical(names(cl), p$dias)) bad("columns nao cobre os dias do artefato")
    for (dp in names(cl)) {
      normal <- !is.null(p$svm_perfil[[dp]])
      if (normal && (length(cl[[dp]]) != 24L || !all(vapply(cl[[dp]], function(z) is.character(z$vars), NA))))
        bad(paste0("columns ", dp, ": perfil sem 24 horas com vars"))
    }
  }
  invisible(p)
}

## Forma esperada: 24 SVMs por dia normal; feriado = NULL (e hol_profile no lugar). Falha de thaw => engine_abort ([prevcarga/integrity], exit 9).
rd_thaw <- function(payload) {
  need <- c("svm_perfil", "norm", "config_mdl", "day_types", "dias", "train_window", "arima_fingerprint", "seed")
  bad <- function(why) rd_abort("plugin.regdin_artifact_tampered", where = "dessem-regdin/thaw", reason = why,
                                details = list(reason = why))
  miss <- setdiff(need, names(payload))
  if (length(miss)) bad(paste0("campo ausente: ", paste(miss, collapse = ",")))
  ## estado derivado (E4-T3): v0.1.5 sem os campos => artifact_mismatch (exit 2) nomeando o primeiro; forma errada => tampered (exit 9)
  novo <- setdiff(c("serie_dm", "hol_profile", "columns"), names(payload))
  if (length(novo))
    rd_artifact_mismatch(novo[[1L]], NULL, "presente (artefato anterior ao estado derivado: retreine o membro)",
                         where = "dessem-regdin/thaw")
  .rd_check_state_shape(payload, bad)
  payload$svm_perfil <- lapply(payload$svm_perfil, function(d) if (is.null(d)) NULL else lapply(d, engine_svm_thaw))
  ok <- vapply(names(payload$svm_perfil), function(dp) {
    d <- payload$svm_perfil[[dp]]
    dt <- payload$day_types[[dp]]
    if (is.null(d)) !is.na(dt) && dt != 0L else length(d) == 24L
  }, NA)
  if (!all(ok)) bad(paste0("forma do svm_perfil incoerente nos dias: ", paste(names(ok)[!ok], collapse = ",")))
  payload$serie_dm <- data.table::as.data.table(payload$serie_dm)
  payload$hol_profile <- lapply(payload$hol_profile, function(hp) { hp$sample <- data.table::as.data.table(hp$sample); hp })
  payload
}

plugin_freeze <- function(model) rd_freeze(model)
plugin_thaw <- function(payload) rd_thaw(payload)
