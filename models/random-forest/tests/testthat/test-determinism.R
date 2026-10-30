## test-determinism.R -- RF.2: determinismo do treino 216 randomForest. set.seed(spec$seed) imediatamente antes de cada
## randomForest(); complete.cases por modelo (nunca na.omit global); RNG do chamador intacto; seed 124 muda a saida.
library(testthat)

probe_pred <- function(model, feats) {
  row <- feats$table[nrow(feats$table), feats$feature_cols, with = FALSE]
  vapply(names(model), function(n) unname(stats::predict(model[[n]]$model, as.matrix(row[, model[[n]]$features, with = FALSE]))), 0)
}

test_that("[PLUGINS] determinismo: 2 treinos identicos => sha256 iguais; 216 sub-modelos y_h001..y_h216", {
  a <- rf_tr_run(); b <- rf_tr_run()
  expect_identical(rf_tr_sha(a$art), rf_tr_sha(b$art))
  expect_identical(a$art, b$art)
  expect_identical(names(a$model), sprintf("y_h%03d", 1:216))
  for (n in c("y_h001", "y_h100", "y_h216")) {
    s <- a$model[[n]]
    expect_identical(s$lead_hour, as.integer(sub("^y_h", "", n)))
    expect_identical(s$day_ahead, as.integer(ceiling(s$lead_hour / 24)))
    expect_identical(s$hour_of_day, (s$lead_hour - 1L) %% 24L)
    expect_identical(s$features, rf_plug()$.select_features_for_lead(s$lead_hour, a$feats$feature_cols))
    expect_identical(s$n_obs, nrow(a$feats$table))
  }
  expect_identical(attr(a$model, "n_train"), nrow(a$feats$table))
})

test_that("[PLUGINS] determinismo: CONTROL seed 124 muda as predicoes de pelo menos um sub-modelo (> 1e-5)", {
  a <- rf_tr_run(spec = rf_tr_spec(seed = 123L)); b <- rf_tr_run(spec = rf_tr_spec(seed = 124L))
  d <- abs(probe_pred(a$model, a$feats) - probe_pred(b$model, b$feats))
  expect_gt(max(d), 1e-5)
  expect_false(identical(rf_tr_sha(a$art), rf_tr_sha(b$art)))
})

test_that("[PLUGINS] determinismo: set.seed(spec$seed) imediatamente antes de CADA randomForest (reproducao manual do sub-modelo)", {
  a <- rf_tr_run(); p <- rf_plug()
  for (lead in c(1L, 57L, 216L)) {
    s <- a$model[[sprintf("y_h%03d", lead)]]
    X <- as.matrix(a$feats$table[, s$features, with = FALSE]); Y <- a$feats$table[[sprintf("y_h%03d", lead)]]
    set.seed(123L)
    fit <- randomForest::randomForest(x = X, y = Y, ntree = 5L, importance = TRUE, do.trace = FALSE)
    expect_identical(unname(stats::predict(fit, X)), unname(stats::predict(s$model, X)))
  }
})

test_that("[PLUGINS] determinismo: .Random.seed do chamador identical antes/depois (e nao nasce quando nao existia)", {
  g <- globalenv()
  feats <- rf_tr_run()$feats; spec <- rf_tr_spec()
  set.seed(42L); before <- get(".Random.seed", envir = g)
  m <- rf_plug()$plugin_train(feats, spec)
  expect_identical(get(".Random.seed", envir = g), before)
  rm(".Random.seed", envir = g)
  m2 <- rf_plug()$plugin_train(feats, spec)
  expect_false(exists(".Random.seed", envir = g, inherits = FALSE))
  expect_identical(probe_pred(m, feats), probe_pred(m2, feats))
  set.seed(1L)   # devolve um estado valido ao runner
})

test_that("[PLUGINS] determinismo: complete.cases POR MODELO (n_obs < n linhas na borda), sem na.omit global, n_test = 0", {
  base <- rf_tr_run(); feats <- base$feats
  tb <- data.table::copy(feats$table); n <- nrow(tb)
  tb[(n - 2L):n, y_h216 := NA_real_]          # borda de alvo
  tb[1L, prev_day_mean := NA_real_]           # NA residual numa feature comum a todos os modelos
  feats$table <- tb
  m <- rf_plug()$plugin_train(feats, rf_tr_spec(), base$cuts)
  expect_identical(attr(m, "n_train"), n)                 # nenhuma linha descartada globalmente
  expect_identical(m$y_h216$n_obs, n - 4L)
  expect_identical(m$y_h001$n_obs, n - 1L)
  expect_true(all(vapply(m, function(s) s$n_obs, 1L) < n))
  expect_identical(feats$n_test, 0L)
  ## todas as linhas NA => completude (nunca um modelo vazio)
  tb2 <- data.table::copy(feats$table)[, y_h003 := NA_real_]
  f2 <- feats; f2$table <- tb2
  e <- fail(rf_plug()$plugin_train(f2, rf_tr_spec()))
  expect_identical(e$class_name, "completude")
})

test_that("[PLUGINS] determinismo: ntree e seed sem default (spec sem eles => contract) e wrapper canonico do despachante", {
  base <- rf_tr_run(); p <- rf_plug()
  expect_identical(fail(p$plugin_train(base$feats, list(params = list(), seed = 123L)))$class_name, "contract")
  expect_identical(fail(p$plugin_train(base$feats, list(params = list(ntree = 5L))))$class_name, "contract")
  expect_identical(names(formals(p$rf_training)), c("features", "params", "seed"))
  expect_identical(names(formals(p$rf_feature_engineering)), c("frame", "params"))
  m <- p$rf_training(base$feats, list(ntree = 5L), 123L)
  expect_identical(rf_tr_sha(p$plugin_freeze(m, base$cuts, rf_tr_spec())), rf_tr_sha(base$art))
})
