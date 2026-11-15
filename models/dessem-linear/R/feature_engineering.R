## feature_engineering.R -- dessem-linear (DL.5). So definicoes. Deck -> series do motor (porta de .dl_build_series,
## dessem_linear_fit.R:33-47 do oraculo, que e o preludio de processaModelosDMPerfil). Sem aritmetica de hora.

## Silencia so as duas mensagens esperadas do TransformaDeck (horizonte de 10 dias ajustado ao deck de 9).
dl_muffle <- function(patterns, expr) {
  withCallingHandlers(expr, message = function(m) {
    if (any(vapply(patterns, function(p) grepl(p, conditionMessage(m), fixed = TRUE), NA)))
      invokeRestart("muffleMessage")
  })
}

## Bloco 0 = janela comum + horizonte; Bloco k > 0 = ilhas do anexo (ADR-046 item 1: normalizacao e serieDM so no Bloco 0).
## Sem a coluna Bloco (modo referencia ou deck sem anexo) o resultado e identico ao de antes.
dl_common_base <- function(SERIECOMP) {
  if ("Bloco" %in% names(SERIECOMP)) SERIECOMP[Bloco == 0L & Prev == FALSE] else SERIECOMP[Prev == FALSE]
}
dl_common_series <- function(SERIECOMP) {
  if ("Bloco" %in% names(SERIECOMP)) SERIECOMP[Bloco == 0L] else SERIECOMP
}

## Replica local de inclui_CargaMediaRef (_dessem-engine/R/entrada_perfil.R) com a constante DADA (sem getNormalizaCarga);
## muta seriePF por referencia. Dias sem carga (NaN) ficam NA, como no motor.
dl_cargamed_ref <- function(seriePF, const) {
  seriePF[, CargaMed := mean(Carga, na.rm = TRUE) / const, by = .(Data)]
  seriePF[is.nan(CargaMed), CargaMed := NA_real_]
  invisible(seriePF)
}

## norm injetado: lista com as quatro constantes, numericas e sem NA (a forma gravada no artefato). A constante 0 e recusada.
dl_check_norm <- function(norm) {
  need <- c("dm_carga", "dm_exo", "pf_cargadm", "pf_exo")
  have <- if (is.list(norm)) need[need %in% names(norm)] else character()
  ## constante vazia e legitima: a flag de normalizacao desligada no config faz getNormaliza* devolver logical(0) e a
  ## serie nao divide (a flag decide, nao a constante). Presente e nao vazia: numerica, sem NA e diferente de 0.
  bad <- have[vapply(have, function(k) {
    x <- norm[[k]]
    length(x) > 0L && (!is.numeric(x) || anyNA(x) || any(x == 0))
  }, NA)]
  miss <- setdiff(need, have)
  if (length(miss) || length(bad)) {
    k <- c(miss, bad)
    dl_abort("plugin.dessem_linear_artifact_mismatch", where = "dessem-linear/build_series",
             reason = sprintf("norm: constante ausente, NA ou 0 (%s)", paste(k, collapse = ",")),
             details = list(field = "norm", constante = k))
  }
  invisible(norm)
}

## serieDM (diaria) e seriePF (horaria) limpas, na mesma ordem de operacoes do processaModelosDMPerfil.
## norm = NULL: constantes calculadas sobre a base (caminho do treino). norm dado: as quatro constantes vem do argumento
## e getNormaliza* nao sao chamadas (a serie de um deck curto usa a normalizacao do deck longo).
dl_build_series <- function(SERIECOMP, config_mdl, norm = NULL) {
  eng <- dl_engine()
  nrm <- config_mdl$normalizacao
  if (!is.null(norm)) dl_check_norm(norm)
  if (is.null(norm)) {
    base <- dl_common_base(SERIECOMP)
    k <- list(dm_carga = eng$getNormalizaCarga(nrm$dm$carga, base),
              dm_exo = eng$getNormalizaExogena(nrm$dm$exogena, base))
  } else k <- norm
  serieDM <- eng$aggregaSerie(
    serieHist = data.table::copy(dl_common_series(SERIECOMP)),
    tipotemp = config_mdl$exogenaOper,
    normaliza = any(nrm$dm$carga$flag, nrm$dm$exogena$flag),
    constnorm = k$dm_carga,
    constnormtemp = k$dm_exo)
  seriePF <- data.table::copy(SERIECOMP)
  if (is.null(norm)) eng$inclui_CargaMediaRef(seriePF, nrm$perfil$cargadm) else dl_cargamed_ref(seriePF, norm$pf_cargadm)
  eng$normalizaSerieRef(
    serieHist = seriePF,
    constnorm = nrm$perfil$carga$oper,
    constnormtemp = if (is.null(norm)) eng$getNormalizaExogena(nrm$exogena, base) else norm$pf_exo)
  list(serieDM = serieDM, seriePF = seriePF)
}

## Constantes de normalizacao calculadas em Prev == FALSE (as do oraculo: dm/perfil).
dl_norm_constants <- function(SERIECOMP, config_mdl) {
  eng <- dl_engine()
  base <- dl_common_base(SERIECOMP)
  nrm <- config_mdl$normalizacao
  list(dm_carga = eng$getNormalizaCarga(nrm$dm$carga, base),
       dm_exo = eng$getNormalizaExogena(nrm$dm$exogena, base),
       pf_cargadm = eng$getNormalizaCarga(nrm$perfil$cargadm, base),
       pf_exo = eng$getNormalizaExogena(nrm$exogena, base))
}

## `spec` = list(params = ...); a variante e validada ANTES de qualquer calculo do motor.
plugin_build_features <- function(deck, spec, norm = NULL) {
  if (!is.null(norm)) dl_check_norm(norm)   # antes de qualquer chamada ao motor
  v <- dl_resolve_variant(spec)
  eng <- dl_engine()
  config_mdl <- eng$getConfigModel(v$model_key)
  eng_deck <- list(CARGAHIST = data.table::copy(deck$CARGAHIST), EXOGENAHIST = data.table::copy(deck$EXOGENAHIST),
                   EXOGENAPREV = data.table::copy(deck$EXOGENAPREV), FERIADOS = data.table::copy(deck$FERIADOS))
  ## anexo de feriados (ADR-046): so entra no deck do motor quando existe (sem anexo o deck e o de antes)
  if (!is.null(deck$CARGAFER)) {
    eng_deck$CARGAFER <- data.table::copy(deck$CARGAFER)
    eng_deck$EXOGENAFER <- data.table::copy(deck$EXOGENAFER)
  }
  processado <- dl_muffle(c("Exogena prevista nao disponivel", "Ajustanto horizonte"),
    eng$TransformaDeck(deck = eng_deck, dataInicioPrev = deck$dataPrev, horizonteprevisao = "10DIAS", tz = deck$tz))
  if (length(processado$DIASPREV) > deck$horizon) {
    processado$DIASPREV <- processado$DIASPREV[seq_len(deck$horizon)]
    processado$NDIASPREV <- deck$horizon
  }
  s <- dl_build_series(processado$SERIECOMP, config_mdl, norm)
  frame <- deck
  frame$variant <- v$exogena
  frame$model_key <- v$model_key
  list(processado = processado, serieDM = s$serieDM, seriePF = s$seriePF, config_mdl = config_mdl,
       norm = if (is.null(norm)) dl_norm_constants(processado$SERIECOMP, config_mdl) else norm, deck = frame)
}
