## training.R -- estagio de treino do plugin random-forest (RF.2). SO definicoes.
## 216 randomForest independentes (um por hora de antecedencia). Porte de training_plugin.R:229-300 e 563-670
## (somente leitura). `ntree` vem de spec$params$ntree e a semente de spec$seed (nenhum default); set.seed(seed)
## IMEDIATAMENTE antes de cada randomForest(). O RNG do chamador e restaurado. Sem relogio, sem message() de progresso,
## sem variavel de ambiente. O corte de treino e CONFERIDO aqui (ADR-0023), nunca aplicado: o plugin nao filtra.

## Uniao de features do modelo da hora `lead_hour`, na ordem legada; intersect com feature_cols (colunas ausentes saem).
.select_features_for_lead <- function(lead_hour, feature_cols) {
  day_ahead <- ceiling(lead_hour / 24L)
  origin_feats <- grep("^origin_", feature_cols, value = TRUE)
  temp_feats <- grep(paste0("^(tmax|tmin)_[1-", day_ahead, "]$"), feature_cols, value = TRUE)
  dow_feats <- grep(paste0("^day", day_ahead, "_"), feature_cols, value = TRUE)
  hour_feats <- grep(paste0("^h", sprintf("%03d", lead_hour), "_"), feature_cols, value = TRUE)
  seasonal_feats <- grep(paste0("^day", day_ahead, "_(month|quarter|is_summer|is_winter)"), feature_cols, value = TRUE)
  prev_day_feats <- grep("^prev_day_", feature_cols, value = TRUE)
  prev_2day_feats <- grep("^prev_2day_", feature_cols, value = TRUE)
  recent_feats <- grep("^(prev_7day_mean|prev_3day_mean|recent_vs_historical)$", feature_cols, value = TRUE)
  holiday_feats <- grep("^(has_holiday_in_horizon|days_to_next_holiday|days_since_last_holiday)$", feature_cols,
                        value = TRUE)
  intersect(unique(c(origin_feats, temp_feats, dow_feats, hour_feats, seasonal_feats, prev_day_feats, prev_2day_feats,
                     recent_feats, holiday_feats)), feature_cols)
}

.rf_train_abort <- function(id, reason, ...)
  .rf_abort(id, details = c(list(reason = reason), list(...)), where = "random-forest/training")

## O laco dos 216 modelos. Por alvo: X = colunas selecionadas, filtro complete.cases(X) & !is.na(Y) (por modelo, nunca
## na.omit global), set.seed(seed) imediatamente antes de randomForest (mtry/nodesize default). n_test = 0.
train_multioutput_rf <- function(table, feature_cols, target_cols, ntree, seed) {
  models <- stats::setNames(vector("list", length(target_cols)), target_cols)
  imp <- plugin_constants()$importance
  for (tg in target_cols) {
    lead <- as.integer(sub("^y_h", "", tg))
    sel <- .select_features_for_lead(lead, feature_cols)
    X <- as.matrix(table[, sel, with = FALSE])
    Y <- table[[tg]]
    ok <- stats::complete.cases(X) & !is.na(Y)
    if (sum(ok) < 2L)
      .rf_train_abort("plugin.completude", "menos de 2 linhas completas para o modelo", modelo = tg, n_obs = sum(ok))
    set.seed(seed)
    fit <- randomForest::randomForest(x = X[ok, , drop = FALSE], y = Y[ok], ntree = ntree, importance = imp,
                                      do.trace = FALSE)
    models[[tg]] <- list(model = fit, features = sel, lead_hour = lead, day_ahead = as.integer(ceiling(lead / 24)),
                         hour_of_day = (lead - 1L) %% 24L, n_obs = sum(ok))
  }
  models
}

## Conferencia do corte (ADR-0023, origin_agnostic): origens ate train_origin e alvo mais distante train_origin + H
## (= last_data_day = O-1). Com `cuts` (dispatch direto/teste) confere tambem contra os cortes do deck; sem `cuts`
## confere a coerencia interna de rf_features. Nunca corta: so aborta [prevcarga/contract] nomeando a origem.
.rf_check_train_cut <- function(features, cuts) {
  H <- plugin_constants()$horizon_days_native
  tbl <- features$table
  if (!identical(features$phase, "train"))
    .rf_train_abort("plugin.contract", "rf_features nao e da fase train", phase = features$phase)
  if (!data.table::is.data.table(tbl) || !nrow(tbl) || !("dataOrigem" %in% names(tbl)))
    .rf_train_abort("plugin.contract", "tabela de treino vazia ou sem dataOrigem")
  top <- max(tbl$dataOrigem)
  tro <- features$train_origin
  if (!inherits(tro, "Date") || length(tro) != 1L || is.na(tro))
    .rf_train_abort("plugin.contract", "train_origin ausente em rf_features")
  if (top > tro)
    .rf_train_abort("plugin.contract", "origem de treino alem de train_origin", origem = format(top),
                    train_origin = format(tro))
  if (!is.null(cuts)) {
    if (tro > cuts$train_origin)
      .rf_train_abort("plugin.contract", "train_origin do rf_features alem do corte", origem = format(tro),
                      train_origin = format(cuts$train_origin))
    if (top + H > cuts$last_data_day)
      .rf_train_abort("plugin.contract", "alvo alem de last_data_day", origem = format(top),
                      last_data_day = format(cuts$last_data_day))
  }
  invisible(TRUE)
}

## (rf_features, spec) -> modelo (lista de 216 sub-modelos com atributos feature_cols, n_train, train_origin).
## `spec` = list(params = list(ntree), seed); o RNG do chamador e preservado (prevcarga_preserve_rng da fundacao).
plugin_train <- function(features, spec, cuts = NULL) {
  ntree <- spec$params$ntree
  seed <- spec$seed
  if (is.null(ntree) || is.null(seed))
    .rf_train_abort("plugin.contract", "spec sem params$ntree ou seed (nenhum default)")
  .rf_check_train_cut(features, cuts)
  tbl <- data.table::copy(features$table)
  data.table::setorder(tbl, dataOrigem)
  fit <- .rf_foundation("prevcarga_preserve_rng")(
    train_multioutput_rf(tbl, features$feature_cols, features$target_cols, as.integer(ntree), as.integer(seed)))
  structure(fit, feature_cols = features$feature_cols, n_train = nrow(tbl), train_origin = features$train_origin)
}

## Assinatura canonica do despachante (features, params, seed).
rf_training <- function(features, params, seed) plugin_train(features, list(params = params, seed = seed))
