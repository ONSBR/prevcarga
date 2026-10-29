## feature_engineering.R -- dessem-linear (DL.5). So definicoes. Deck -> series do motor (porta de .dl_build_series,
## dessem_linear_fit.R:33-47 do oraculo, que e o preludio de processaModelosDMPerfil). Sem aritmetica de hora.

## Silencia so as duas mensagens esperadas do TransformaDeck (horizonte de 10 dias ajustado ao deck de 9).
dl_muffle <- function(patterns, expr) {
  withCallingHandlers(expr, message = function(m) {
    if (any(vapply(patterns, function(p) grepl(p, conditionMessage(m), fixed = TRUE), NA)))
      invokeRestart("muffleMessage")
  })
}

## serieDM (diaria) e seriePF (horaria) limpas, na mesma ordem de operacoes do processaModelosDMPerfil.
dl_build_series <- function(SERIECOMP, config_mdl) {
  eng <- dl_engine()
  base <- SERIECOMP[Prev == FALSE]
  nrm <- config_mdl$normalizacao
  serieDM <- eng$aggregaSerie(
    serieHist = data.table::copy(SERIECOMP),
    tipotemp = config_mdl$exogenaOper,
    normaliza = any(nrm$dm$carga$flag, nrm$dm$exogena$flag),
    constnorm = eng$getNormalizaCarga(nrm$dm$carga, base),
    constnormtemp = eng$getNormalizaExogena(nrm$dm$exogena, base))
  seriePF <- data.table::copy(SERIECOMP)
  eng$inclui_CargaMediaRef(seriePF, nrm$perfil$cargadm)
  eng$normalizaSerieRef(
    serieHist = seriePF,
    constnorm = nrm$perfil$carga$oper,
    constnormtemp = eng$getNormalizaExogena(nrm$exogena, base))
  list(serieDM = serieDM, seriePF = seriePF)
}

## Constantes de normalizacao calculadas em Prev == FALSE (as do oraculo: dm/perfil).
dl_norm_constants <- function(SERIECOMP, config_mdl) {
  eng <- dl_engine()
  base <- SERIECOMP[Prev == FALSE]
  nrm <- config_mdl$normalizacao
  list(dm_carga = eng$getNormalizaCarga(nrm$dm$carga, base),
       dm_exo = eng$getNormalizaExogena(nrm$dm$exogena, base),
       pf_cargadm = eng$getNormalizaCarga(nrm$perfil$cargadm, base),
       pf_exo = eng$getNormalizaExogena(nrm$exogena, base))
}

## `spec` = list(params = ...); a variante e validada ANTES de qualquer calculo do motor.
plugin_build_features <- function(deck, spec) {
  v <- dl_resolve_variant(spec)
  eng <- dl_engine()
  config_mdl <- eng$getConfigModel(v$model_key)
  eng_deck <- list(CARGAHIST = data.table::copy(deck$CARGAHIST), EXOGENAHIST = data.table::copy(deck$EXOGENAHIST),
                   EXOGENAPREV = data.table::copy(deck$EXOGENAPREV), FERIADOS = data.table::copy(deck$FERIADOS))
  processado <- dl_muffle(c("Exogena prevista nao disponivel", "Ajustanto horizonte"),
    eng$TransformaDeck(deck = eng_deck, dataInicioPrev = deck$dataPrev, horizonteprevisao = "10DIAS"))
  if (length(processado$DIASPREV) > deck$horizon) {
    processado$DIASPREV <- processado$DIASPREV[seq_len(deck$horizon)]
    processado$NDIASPREV <- deck$horizon
  }
  s <- dl_build_series(processado$SERIECOMP, config_mdl)
  frame <- deck
  frame$variant <- v$exogena
  frame$model_key <- v$model_key
  list(processado = processado, serieDM = s$serieDM, seriePF = s$seriePF, config_mdl = config_mdl,
       norm = dl_norm_constants(processado$SERIECOMP, config_mdl), deck = frame)
}
