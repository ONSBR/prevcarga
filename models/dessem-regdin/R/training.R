## training.R -- dessem-regdin (RD.2). So definicoes. Estagio `training` (ADR-0023, origin_scoped): treino ate O-1
## (`cuts$last_data_day`, que o plugin so CONSOME) com o RNG na posicao do oraculo. A cabeca DM(O) de referencia e ajustada
## so para consumir o gerador e DESCARTADA; o artefato nao depende do corte intradiario (a chave e origem, variante, area).
## Sem aritmetica de hora; sem relogio no artefato; sem mable (o ARIMA e re-fitado no infer, RD.3, com o fingerprint).
## O despachante da fundacao restaura o `.Random.seed` do chamador; a semente vem so de `seed` (run.yaml).

## Adaptador: CARGAHIST/hourly_load do deck (DataHora = interval_end do frame de modelagem, Carga) -> painel de rd_mm.
.rd_deck_panel <- function(load) data.table::data.table(interval_end = load$DataHora, load_mwh = load$Carga)

## Confere (nunca corta): nenhuma linha do dia O ou posterior em CARGAHIST/EXOGENAHIST nem em origin_load.
rd_assert_train_cut <- function(deck) {
  last <- deck$last_data_day
  seen <- c(rd_day_of(deck$CARGAHIST$DataHora), rd_day_of(deck$EXOGENAHIST$DataHora))
  if (NROW(deck$origin_load) > 0L) seen <- c(seen, rep(deck$dataPrev, nrow(deck$origin_load)))
  if (length(seen) && any(seen > last))
    rd_abort("plugin.regdin_train_panel_has_origin", where = "dessem-regdin/plugin_train",
             reason = sprintf("o treino viu o dia %s (last_data_day=%s)", format(max(seen)), format(last)),
             details = list(last_data_day = format(last), max_seen = format(max(seen))))
  invisible(TRUE)
}

## "preserve" (padrao: rng_deviation NULL) | "reseed" (plano B: rng_deviation preenchido no descriptor).
rd_rng_mode <- function(deviation = plugin_descriptor()$rng_deviation) {
  if (is.null(deviation) || !length(deviation) || all(is.na(deviation))) return("preserve")
  rd_warn("warning.regdin_rng_deviation", where = "dessem-regdin/rng_mode",
          details = list(rng_deviation = paste(as.character(deviation), collapse = " ")))
  "reseed"
}

## Fingerprint do ARIMA base (RD_DemandaMediaHyndman): ordem (p,d,q)(P,D,Q), coeficientes nomeados e sha256 da serie DM
## historica. `fit` e o mable do fable (so lido aqui; nunca entra no artefato).
.rd_arima_fingerprint <- function(fit, serieDM) {
  spec <- fit[[1L]][[1L]]$fit$spec
  ord <- as.integer(unlist(spec[, c("p", "d", "q", "P", "D", "Q")], use.names = FALSE))
  tt <- fabletools::tidy(fit)
  coef <- stats::setNames(as.numeric(tt$estimate), as.character(tt$term))
  hist <- data.table::as.data.table(serieDM)[Prev == FALSE, c("Data", "Carga", grep("^Exo", names(serieDM), value = TRUE)),
                                             with = FALSE]
  list(order = stats::setNames(ord, c("p", "d", "q", "P", "D", "Q")), coef = coef,
       dm_digest = digest::digest(hist, algo = "sha256"))
}

## Passe de fit (porta de .rd_fit_pass, dessem_regdin_fit.R:95-133, com a regra de RNG harmonizada 16a):
## set.seed -> ARIMA base (sem RNG) -> cabeca de referencia DESCARTADA (so posiciona o RNG) -> 24 SVMs por dia normal.
.rd_fit_pass <- function(features, seed, mm_ref, mode = c("preserve", "reseed")) {
  mode <- match.arg(mode)
  s <- features$series; cfg <- features$config_mdl
  O <- features$processado$DIASPREV[1L]
  set.seed(as.integer(seed))
  base <- suppressWarnings(execDemandaMediaTS(s$serieDM, cfg))
  ## a linha de O do mm de referencia so ancora o predict da cabeca (valor irrelevante; resultado descartado)
  mm <- data.table::copy(mm_ref)
  if (nrow(mm)) mm <- rbind(mm[Data != O], data.table::data.table(Data = O, mm = mm$mm[nrow(mm)]))
  mm <- rd_normalize_mm(mm, cfg, features$norm$dm_carga)
  ref <- suppressWarnings(rd_dm_nowcast(s$serieDM, cfg, mm, O, as.integer(seed)))
  inferDM <- data.table::copy(data.table::as.data.table(ref$infer))
  inferDM[, Carga.infer := Carga.infer / features$norm$pf_cargadm]
  addValuesDMRef(s$seriePF, inferDM, "CargaMed")
  if (identical(mode, "reseed")) set.seed(as.integer(seed))             # plano B
  dtypes <- features$day_types
  svm <- stats::setNames(vector("list", length(features$dias)), features$dias)
  for (dp in features$dias) {
    if (!is.na(dtypes[[dp]]) && dtypes[[dp]] != 0L) next                # feriado: RL_perfilFeriado recomputado no infer
    svm[[dp]] <- lapply(0:23, function(h) SVMhorario(dp, h, s$seriePF, cfg)$modeloTreinado)
  }
  list(base_fit = base$mdl$modeloTreinado, svm_perfil = svm)
}

## Constantes de normalizacao (Prev == FALSE), as do artefato do oraculo.
.rd_norm_constants <- function(SERIECOMP, config_mdl) {
  h <- SERIECOMP[Prev == FALSE]
  nz <- config_mdl$normalizacao
  list(dm_carga = getNormalizaCarga(nz$carga, h), dm_exo = getNormalizaExogena(nz$exogena, h),
       pf_cargadm = getNormalizaCarga(nz$perfil$cargadm, h), pf_exo = getNormalizaExogena(nz$perfil$exogena, h))
}

## Estagio real (formais fixos do loader: features, params, seed).
rd_training <- function(features, params, seed) {
  deck <- features$deck
  rd_assert_train_cut(deck)
  s <- features$series
  features$norm <- .rd_norm_constants(features$processado$SERIECOMP, features$config_mdl)
  hist_days <- s$serieDM[Prev == FALSE]$Data
  mm_ref <- rd_reference_mm(.rd_deck_panel(deck$CARGAHIST), plugin_constants()$reference_mm_hour, deck$tz)
  mode <- rd_rng_mode()
  fits <- .rd_fit_pass(features, seed = seed, mm_ref = mm_ref, mode = mode)
  list(svm_perfil = fits$svm_perfil, norm = features$norm, config_mdl = features$config_mdl,
       day_types = features$day_types, dias = features$dias,
       train_window = list(start = min(hist_days), end = deck$last_data_day),
       arima_fingerprint = .rd_arima_fingerprint(fits$base_fit, s$serieDM))
}

## Forma do ticket: plugin_train(features, spec).
plugin_train <- function(features, spec) rd_training(features, spec$params, spec$seed)
