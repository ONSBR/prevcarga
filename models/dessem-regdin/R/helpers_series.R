## helpers_series.R -- dessem-regdin (RD.1). So definicoes. Helpers do oraculo (dessem_regdin_fit.R:45-83) com os
## nomes e a semantica preservados; as funcoes de serie vem do motor (env pai). Sem aritmetica de hora.

## serieDM (diaria) e seriePF (horaria) LIMPAS, VERBATIM do processaModelosDMTSPerfil. Chaves do regdin:
## DM = normalizacao$carga/$exogena; perfil = normalizacao$perfil$carga$oper e $perfil$exogena (!= linear, $dm$*).
.rd_build_series <- function(SERIECOMP, config_mdl) {
  nz <- config_mdl$normalizacao
  hist <- SERIECOMP[Prev == FALSE]
  serieDM <- aggregaSerie(
    serieHist     = data.table::copy(SERIECOMP),
    tipotemp      = config_mdl$exogenaOper,
    normaliza     = any(nz$carga$flag, nz$exogena$flag),
    constnorm     = getNormalizaCarga(nz$carga, hist),
    constnormtemp = getNormalizaExogena(nz$exogena, hist))
  seriePF <- data.table::copy(SERIECOMP)
  inclui_CargaMediaRef(seriePF, nz$perfil$cargadm)
  normalizaSerieRef(
    serieHist     = seriePF,
    constnorm     = nz$perfil$carga$oper,
    constnormtemp = getNormalizaExogena(nz$perfil$exogena, hist))
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
