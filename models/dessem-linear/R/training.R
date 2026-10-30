## training.R -- dessem-linear (DL.6). So definicoes. Estagio `training` (ADR-0023, origin_scoped): treino ate O-1 com o
## RNG na posicao do oraculo. A cabeca DM(O) de referencia e ajustada so para consumir o gerador e DESCARTADA; o artefato
## nao depende do corte intradiario (a chave e origem, variante, area). Sem aritmetica de hora; sem relogio no artefato.
## O despachante restaura o `.Random.seed` do chamador; a semente vem so de `seed` (run.yaml).

## "preserve" (padrao: rng_deviation NULL) | "reseed" (plano B: rng_deviation preenchido no descriptor).
## O descriptor e lido por NOME no env do plugin; `deviation` so existe para teste e para o forecast acoplado (DL.7).
dl_rng_mode <- function(deviation = plugin_descriptor()$rng_deviation) {
  if (is.null(deviation) || !length(deviation) || all(is.na(deviation))) return("preserve")
  dl_warn("warning.dessem_linear_rng_deviation", where = "dessem-linear/rng_mode",
          details = list(rng_deviation = paste(as.character(deviation), collapse = " ")))
  "reseed"
}

## Instante (frame de modelagem) que fixa so o horario-do-dia (mm_ref_hour) do `mm` de referencia; os valores nao importam.
dl_ref_cut <- function(O, tz) {
  lubridate::make_datetime(lubridate::year(O), lubridate::month(O), lubridate::mday(O),
                           hour = plugin_constants()$mm_ref_hour, tz = tz)
}

## Anti-circular (porta de dessem_linear_fit.R:59-63): zera Carga das linhas Prev == TRUE; NO-OP na serie limpa.
## Muta SERIECOMP por referencia. Devolve o numero de linhas afetadas.
.dl_enforce_real_history <- function(SERIECOMP) {
  n <- SERIECOMP[Prev == TRUE & !is.na(Carga), .N]
  if (n > 0L) SERIECOMP[Prev == TRUE, Carga := NA_real_]
  n
}

## Confere (nunca corta): nenhuma linha do dia O ou posterior em CARGAHIST/EXOGENAHIST nem em origin_load.
dl_assert_train_cut <- function(deck) {
  last <- deck$last_data_day
  seen <- c(dl_day_of(deck$CARGAHIST$DataHora), dl_day_of(deck$EXOGENAHIST$DataHora))
  if (nrow(deck$origin_load) > 0L) seen <- c(seen, rep(deck$dataPrev, nrow(deck$origin_load)))
  if (length(seen) && any(seen > last))
    dl_abort("plugin.dessem_linear_train_sees_day_o", where = "dessem-linear/plugin_train",
             reason = sprintf("o treino viu o dia %s (last_data_day=%s)", format(max(seen)), format(last)),
             details = list(last_data_day = format(last), max_seen = format(max(seen))))
  invisible(TRUE)
}

## Passe de fit (porta de .dl_fit_pass, dessem_linear_fit.R:74-95, com a regra de RNG harmonizada 16a):
## set.seed; por dia-alvo em O..O+8: DM (O: cabeca de referencia descartada) e depois os 24 perfis.
.dl_fit_pass <- function(f, seed, mode = c("preserve", "reseed")) {
  mode <- match.arg(mode)
  eng <- dl_engine()
  O <- f$deck$dataPrev
  dias <- format(f$processado$DIASPREV)
  svm_dm <- list(); svm_pf <- list()
  set.seed(as.integer(seed))
  for (dp in dias) {
    if (identical(dp, format(O))) {
      if (identical(mode, "preserve")) {
        mm_ref <- dl_morning_means(f$deck$hourly_load, dl_ref_cut(O, f$deck$tz), O)
        invisible(dl_dm_head(eng, f$serieDM, f$config_mdl, mm_ref, O, require_origin = FALSE,
                             constnorm = f$norm$dm_carga))               # so consome o RNG; descartada
      } else set.seed(as.integer(seed))                                  # plano B
    } else {
      svm_dm[[dp]] <- eng$SVM_DemandaMedia(dp, f$serieDM, f$config_mdl)$modeloTreinado
    }
    svm_pf[[dp]] <- lapply(0:23, function(h) eng$SVMhorario(dp, h, f$seriePF, f$config_mdl)$modeloTreinado)
  }
  list(svm_dm = svm_dm, svm_perfil = svm_pf)
}

## Artefato de dado puro (sem available_until, trained_at, cut nem caminho; carimbos: despachante da fundacao).
.dl_assemble_artifact <- function(fits, features, spec, n_leak, mode = "preserve") {
  list(svm_dm = fits$svm_dm, svm_perfil = fits$svm_perfil, norm = features$norm,
       head_recipe = list(model_key = features$deck$model_key, config_mdl = features$config_mdl,
                          mm_norm = features$norm$dm_carga, dias = format(features$processado$DIASPREV)),
       variant = features$deck$variant, model_key = features$deck$model_key, area = features$deck$area,
       horizon_days = as.integer(features$deck$horizon), seed = as.integer(spec$seed),
       rng_mode = mode, n_leak = as.integer(n_leak))
}

## Estagio real (formais fixos do loader: features, params, seed).
dl_training <- function(features, params, seed) {
  dl_assert_train_cut(features$deck)
  n_leak <- .dl_enforce_real_history(features$processado$SERIECOMP)
  if (n_leak > 0L) {                                     # so se um write-back vazou: reconstroi series e constantes
    s <- dl_build_series(features$processado$SERIECOMP, features$config_mdl)
    features$serieDM <- s$serieDM; features$seriePF <- s$seriePF
    features$norm <- dl_norm_constants(features$processado$SERIECOMP, features$config_mdl)
  }
  mode <- dl_rng_mode()
  fits <- .dl_fit_pass(features, seed = seed, mode = mode)
  .dl_assemble_artifact(fits, features, list(params = params, seed = seed), n_leak, mode)
}

## Forma do ticket: plugin_train(features, spec).
plugin_train <- function(features, spec) dl_training(features, spec$params, spec$seed)
