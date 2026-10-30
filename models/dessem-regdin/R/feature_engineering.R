## feature_engineering.R -- dessem-regdin (RD.1). So definicoes. Deck -> series do motor (preludio do
## processaModelosDMTSPerfil). Sem aritmetica de hora.

## Silencia so as mensagens esperadas do TransformaDeck (horizonte de 10 dias ajustado ao deck de 9).
rd_muffle <- function(patterns, expr) {
  withCallingHandlers(expr, message = function(m) {
    if (any(vapply(patterns, function(p) grepl(p, conditionMessage(m), fixed = TRUE), NA)))
      invokeRestart("muffleMessage")
  })
}

## `spec` = list(params = ...); a variante e validada ANTES de qualquer chamada ao motor.
plugin_build_features <- function(deck, spec) {
  key <- .rd_variant_key(spec$params)
  eng <- rd_engine()
  config_mdl <- eng$getConfigModel(key)
  eng_deck <- list(CARGAHIST = data.table::copy(deck$CARGAHIST), EXOGENAHIST = data.table::copy(deck$EXOGENAHIST),
                   EXOGENAPREV = data.table::copy(deck$EXOGENAPREV), FERIADOS = data.table::copy(deck$FERIADOS))
  proc <- rd_muffle(c("Exogena prevista nao disponivel", "Ajustanto horizonte"),
    eng$TransformaDeck(deck = eng_deck, dataInicioPrev = deck$dataPrev, horizonteprevisao = "10DIAS", tz = deck$tz))
  n <- deck$horizon
  if (length(proc$DIASPREV) > n) {
    proc$DIASPREV <- proc$DIASPREV[seq_len(n)]
    proc$NDIASPREV <- n
  }
  n_leak <- .rd_enforce_real_history(proc$SERIECOMP)
  s <- .rd_build_series(proc$SERIECOMP, config_mdl)
  dias <- format(proc$DIASPREV)
  frame <- deck
  frame$model_key <- key
  frame$variant <- spec$params$exogena
  list(processado = proc, config_mdl = config_mdl, series = s, dias = dias,
       day_types = .rd_day_types(s$seriePF, dias), n_leak = n_leak, deck = frame)
}
