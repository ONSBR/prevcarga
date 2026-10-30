## training.R -- estagio de treino do plugin LGBM (LGBM.3). SO definicoes.
## Porte de train_lgbm (prevcargadessem3/models/lgbm/R/training/train_lgbm.R:189-424, somente leitura). Os cortes chegam
## prontos de dates.R (`cuts$last_data_day` = O-3, `cuts$train_origin` = O-2; ADR-0023): o plugin so CONFERE que nenhuma linha
## passa de last_data_day, nunca recalcula nem corta. Seed da spec (nunca fixo no codigo); o RNG e isolado por
## prevcarga_preserve_rng. O modelo em memoria carrega o lgb.Booster; so lgbm_freeze (freeze.R) o converte em dado puro.
## train_cv: true => CV blocos-semana (index-based) + refit em 100%; false => holdout pelos cortes internos de LGBM.2.

## Folds de blocos-semana (cv_week_block = 7 x 48 linhas) round-robin sobre as `n` linhas de treino ordenadas por tempo.
lgbm_cv_folds <- function(n, consts = plugin_constants())
  split(seq_len(n), rep(rep(seq_len(consts$cv_nfolds), each = consts$cv_week_block), length.out = n))

.lgbm_train_params <- function(consts, spec = NULL) {
  p <- list(objective = consts$objective, metric = consts$metric, num_leaves = consts$num_leaves,
            min_data_in_leaf = consts$min_data_in_leaf, learning_rate = consts$learning_rate, verbosity = -1L)
  if (is.null(spec)) return(p)
  c(p, list(seed = as.integer(spec$seed), num_threads = 1L, deterministic = TRUE, force_row_wise = TRUE))
}

## CV + refit: best_iter do lgb.cv (NULL/nao finito/< 1 => num_iterations, train_lgbm.R:338-340); refit em 100% das linhas.
lgbm_train_cv <- function(dtrain, n, spec, consts = plugin_constants()) {
  p <- .lgbm_train_params(consts, spec)
  cv <- lightgbm::lgb.cv(params = p, data = dtrain, nrounds = as.integer(consts$num_iterations),
                         folds = lgbm_cv_folds(n, consts),
                         early_stopping_rounds = as.integer(consts$early_stopping_rounds), verbose = -1L)
  best <- cv$best_iter
  if (is.null(best) || !is.finite(best) || best < 1L) best <- consts$num_iterations
  best <- as.integer(best)
  list(booster = lightgbm::lgb.train(params = p, data = dtrain, nrounds = best, verbose = -1L),
       best_iter = best, train_mode = "cv")
}

## Holdout (train_cv: false): valids + early stopping (:345-357). Diverge do oraculo so no determinismo: seed da spec,
## num_threads = 1, deterministic, force_row_wise (o oraculo deixava o OpenMP no padrao: nao reproduzivel bit a bit e
## ~150x mais lento sob contencao de CPU). Os hiperparametros e o criterio de parada sao os do oraculo.
lgbm_train_holdout <- function(dtrain, dval, spec, consts = plugin_constants()) {
  b <- lightgbm::lgb.train(params = .lgbm_train_params(consts, spec), data = dtrain,
                           nrounds = as.integer(consts$num_iterations), valids = list(val = dval),
                           early_stopping_rounds = as.integer(consts$early_stopping_rounds), verbose = -1L)
  best <- b$best_iter
  if (is.null(best) || !is.finite(best) || best < 1L) best <- b$current_iter()
  list(booster = b, best_iter = as.integer(best), train_mode = "holdout")
}

## GATE 2: a FE entrega MW cru; lag com max <= 1 indica dupla normalizacao.
.lgbm_check_raw_scale <- function(min_max, target_col) {
  lag_col <- "CargaGlobal_anterior_BLF"
  col <- if (lag_col %in% names(min_max)) lag_col else target_col
  mx <- min_max[[col]][["max"]]
  if (!is.na(mx) && mx <= 1)
    lgbm_abort("lgbm.double_norm", sprintf("min_max_BLF$%s max=%s <= 1: FE ja normalizou a carga", col, format(mx)))
  invisible(TRUE)
}

## Forma completa (a do contrato do ticket): features da FE, spec (params + seed) e cuts. Devolve o modelo em memoria.
lgbm_train <- function(features, spec, cuts, consts = plugin_constants()) {
  params <- lgbm_check_params(spec$params)
  keep_rng <- .lgbm_foundation("prevcarga_preserve_rng")
  if (is.null(keep_rng)) lgbm_abort("lgbm.contract", "prevcarga_preserve_rng indisponivel (fundacao)")
  ldd <- cuts$last_data_day
  if (!inherits(ldd, "Date") || length(ldd) != 1L || is.na(ldd)) lgbm_abort("lgbm.contract", "cuts$last_data_day ausente")
  if (is.null(spec$seed) || is.na(spec$seed)) lgbm_abort("lgbm.params", "seed ausente na spec")
  keep_rng({
    set.seed(spec$seed)
    .lgbm_train(features, spec, params, cuts, consts)
  })
}

.lgbm_train <- function(features, spec, params, cuts, consts) {
  ft <- features$feature_table
  cols <- features$colunas_entrada_BLF
  nonorm <- features$colunas_nao_normalizar
  trn_cut <- features$train_cutoff
  val_cut <- features$val_cutoff
  target <- "CargaGlobal"
  if (!data.table::is.data.table(ft) || !length(cols) || !(target %in% names(ft)))
    lgbm_abort("lgbm.contract", "features sem feature_table, colunas de entrada ou alvo CargaGlobal")
  if (!inherits(trn_cut, "Date") || !inherits(val_cut, "Date")) lgbm_abort("lgbm.contract", "cortes internos ausentes nas features")
  ldd <- cuts$last_data_day
  if (nrow(ft) && max(ft$Data) > ldd)
    lgbm_abort("lgbm.contract", sprintf("feature_table com dia %s alem de last_data_day=%s", format(max(ft$Data)), format(ldd)))
  if (max(trn_cut, val_cut) > ldd)
    lgbm_abort("lgbm.contract", sprintf("corte %s alem de last_data_day=%s", format(max(trn_cut, val_cut)), format(ldd)))
  cols_norm <- setdiff(cols, nonorm)
  min_max <- lgbm_compute_min_max(ft, union(cols_norm, target), trn_cut)
  .lgbm_check_raw_scale(min_max, target)
  dn <- data.table::copy(ft)
  lgbm_normalise_dt(dn, min_max)
  tr <- dn[dn$Data <= trn_cut, ]
  va <- dn[dn$Data > trn_cut & dn$Data <= val_cut, ]
  cat_idx <- match(intersect(nonorm, cols), cols)
  mk <- function(d, ref = NULL) lightgbm::lgb.Dataset(data = as.matrix(d[, cols, with = FALSE]), label = d[[target]],
                                                    categorical_feature = cat_idx, reference = ref)
  if (!nrow(tr)) lgbm_abort("lgbm.contract", "nenhuma linha de treino ate o corte")
  dtrain <- mk(tr)
  fit <- if (isTRUE(params$train_cv)) {
    lgbm_train_cv(dtrain, nrow(tr), spec, consts)
  } else {
    if (!nrow(va)) lgbm_abort("lgbm.contract", "holdout sem linhas de validacao entre os cortes internos")
    lgbm_train_holdout(dtrain, mk(va, dtrain), spec, consts)
  }
  list(booster = fit$booster, min_max_BLF = min_max, sazonalidade = features$sazonalidade,
       sazonalidade_fallbacks = features$sazonalidade_fallbacks, colunas_entrada_BLF = cols,
       colunas_nao_normalizar = nonorm, colunas_normalizar_BLF = cols_norm,
       coluna_numerica_BLF = intersect(cols_norm, cols), coluna_categorica_BLF = cat_idx,
       best_iter = fit$best_iter, train_mode = fit$train_mode,
       cv_nfolds = if (identical(fit$train_mode, "cv")) consts$cv_nfolds else NA_integer_,
       n_train_rows = nrow(tr), target_col = target, train_cutoff = trn_cut, val_cutoff = val_cut,
       params = params, seed = as.integer(spec$seed),
       feature_config = list(wavelet_window = consts$wavelet_width, carga_wav_shift_sec = 172800L,
                             loess_span_prep = consts$span_prep, loess_span_inline = consts$span_inline,
                             loess_span_perday = consts$span_perday))
}

## Estagio despachado pela fundacao: assinatura canonica (features, params, seed). Os cortes vem de attr(features, "cuts")
## (INTERIM, F1 adiado: anexado pela FE a partir do frame da ingestao); o plugin nao os recalcula.
plugin_train <- function(features, params, seed) {
  cuts <- attr(features, "cuts")
  if (is.null(cuts)) lgbm_abort("lgbm.contract", "features sem attr cuts (feature engineering)")
  lgbm_train(features, list(params = params, seed = seed), cuts)
}
