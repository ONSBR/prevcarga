## helpers_series.R -- dessem-regdin (RD.1). So definicoes. Helpers do oraculo (dessem_regdin_fit.R:45-83) com os
## nomes e a semantica preservados; as funcoes de serie vem do motor (env pai). Sem aritmetica de hora.

## Bloco 0 = janela comum + horizonte; Bloco k > 0 = ilhas do anexo (ADR-046 item 1: normalizacao e serieDM so no Bloco 0).
## Sem a coluna Bloco (modo referencia ou deck sem anexo) o resultado e identico ao de antes.
.rd_common_base <- function(SERIECOMP) {
  if ("Bloco" %in% names(SERIECOMP)) SERIECOMP[Bloco == 0L & Prev == FALSE] else SERIECOMP[Prev == FALSE]
}
.rd_common_series <- function(SERIECOMP) {
  if ("Bloco" %in% names(SERIECOMP)) SERIECOMP[Bloco == 0L] else SERIECOMP
}

## Replica local de inclui_CargaMediaRef do motor com a constante DADA (sem getNormalizaCarga); muta seriePF por referencia.
.rd_cargamed_ref <- function(seriePF, const) {
  seriePF[, CargaMed := mean(Carga, na.rm = TRUE) / const, by = .(Data)]
  seriePF[is.nan(CargaMed), CargaMed := NA_real_]
  invisible(seriePF)
}

## norm injetado: as quatro constantes numericas, sem NA e diferentes de 0 (forma gravada no artefato).
## Com `config_mdl`, a constante VAZIA (logical(0)) so vale para a flag de normalizacao que o motor nao liga: flag TRUE com
## constante vazia falha alto (ADR-0005). A flag de cada constante e a que .rd_norm_constants usa para calcula-la (chaves do
## regdin: carga/exogena da DM e perfil$cargadm/perfil$exogena). Sem `config_mdl` a conferencia de vazio fica como antes.
.rd_check_norm <- function(norm, config_mdl = NULL) {
  need <- c("dm_carga", "dm_exo", "pf_cargadm", "pf_exo")
  have <- if (is.list(norm)) need[need %in% names(norm)] else character()
  nz <- config_mdl$normalizacao
  flag <- list(dm_carga = nz$carga$flag, dm_exo = nz$exogena$flag,
               pf_cargadm = nz$perfil$cargadm$flag, pf_exo = nz$perfil$exogena$flag)
  bad <- have[vapply(have, function(k) {
    x <- norm[[k]]
    if (length(x) == 0L) return(!is.null(config_mdl) && isTRUE(flag[[k]]))
    !is.numeric(x) || anyNA(x) || any(x == 0)
  }, NA)]
  miss <- setdiff(need, have)
  if (length(miss) || length(bad)) {
    k <- c(miss, bad)
    rd_abort("plugin.regdin_artifact_mismatch", where = "dessem-regdin/build_series",
             reason = sprintf("norm: constante ausente, vazia com a flag ligada, NA ou 0 (%s)", paste(k, collapse = ",")),
             details = list(field = "norm", constante = k))
  }
  invisible(norm)
}

## serieDM (diaria) e seriePF (horaria) LIMPAS, VERBATIM do processaModelosDMTSPerfil. Chaves do regdin:
## DM = normalizacao$carga/$exogena; perfil = normalizacao$perfil$carga$oper e $perfil$exogena (!= linear, $dm$*).
## norm = NULL: constantes sobre a base (treino). norm dado: vem do argumento, getNormaliza* nao sao chamadas.
.rd_build_series <- function(SERIECOMP, config_mdl, norm = NULL) {
  nz <- config_mdl$normalizacao
  if (!is.null(norm)) .rd_check_norm(norm, config_mdl)
  if (is.null(norm)) {
    hist <- .rd_common_base(SERIECOMP)
    k <- list(dm_carga = getNormalizaCarga(nz$carga, hist), dm_exo = getNormalizaExogena(nz$exogena, hist))
  } else k <- norm
  serieDM <- aggregaSerie(
    serieHist     = data.table::copy(.rd_common_series(SERIECOMP)),
    tipotemp      = config_mdl$exogenaOper,
    normaliza     = any(nz$carga$flag, nz$exogena$flag),
    constnorm     = k$dm_carga,
    constnormtemp = k$dm_exo)
  seriePF <- data.table::copy(SERIECOMP)
  if (is.null(norm)) inclui_CargaMediaRef(seriePF, nz$perfil$cargadm) else .rd_cargamed_ref(seriePF, norm$pf_cargadm)
  normalizaSerieRef(
    serieHist     = seriePF,
    constnorm     = nz$perfil$carga$oper,
    constnormtemp = if (is.null(norm)) getNormalizaExogena(nz$perfil$exogena, hist) else norm$pf_exo)
  list(serieDM = serieDM, seriePF = seriePF)
}

## Guarda anti-treino-circular: zera a Carga das linhas Prev == TRUE (muta SERIECOMP por referencia) e devolve a
## contagem das que tinham Carga nao-NA. NO-OP (0) na serie limpa.
.rd_enforce_real_history <- function(SERIECOMP) {
  n <- SERIECOMP[Prev == TRUE & !is.na(Carga), .N]
  if (n > 0L) SERIECOMP[Prev == TRUE, Carga := NA_real_]
  n
}

## Tipo-de-dia por dia-alvo (0 = normal; != 0 = especial/feriado).
.rd_day_types <- function(seriePF, dias) {
  vapply(dias, function(dp) {
    de <- seriePF[Data == lubridate::as_date(dp)]$DiaEspecial
    if (length(de) == 0L) NA_integer_ else as.integer(max(de, na.rm = TRUE))
  }, integer(1))
}
