## test-training.R -- LGBM.3: treino ate last_data_day = O-3, normalizacao so do treino, CV blocos-semana/holdout e freeze.
## Cada gate traz FORBIDDEN (identico) e CONTROL (muda). Deck sintetico P.0e, area ZZ1, O = 2026-10-01 (train_origin = 09-29,
## last_data_day = 09-28). O sha do artefato e o do objeto R plano de plugin_freeze. Paridade com o oraculo e do LGBM.5.
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
D <- lubridate::ymd
P <- lgbm_source_plugin()
K <- lgbm_consts(P)
deck <- lgbm_fx_deck(days = 120L)
base <- lgbm_fx_train(deck, P = P)
LDD <- D("2026-09-28")
sha <- lgbm_fx_sha

## ---- AC1: corte de treino O-3 (FORBIDDEN / CONTROL) --------------------------------------------------------------
test_that("[PLUGINS] train_cortes: [AC1] cuts de dates.R: train_origin = O-1-H e last_data_day = O-1-data_lag_days; o artefato carimba train_origin", {
  expect_identical(base$cuts$train_origin, D("2026-09-29"))
  expect_identical(base$cuts$last_data_day, LDD)
  expect_identical(base$artifact$train_origin, D("2026-09-29"))
  expect_identical(base$artifact$train_cutoff, "2026-09-28")
  expect_identical(base$model$train_mode, "cv")
  expect_lte(max(base$features$feature_table$Data), LDD)
})

test_that("[PLUGINS] gate01_perturbacao: [AC1] FORBIDDEN: carga de todo dia > last_data_day +500 MW => sha256 do artefato identico", {
  d2 <- lgbm_fx_perturb_days(deck, after = LDD, delta = 500)
  r2 <- lgbm_fx_train(d2, P = P)
  expect_identical(sha(r2$artifact), sha(base$artifact))
  expect_identical(r2$artifact$booster_sha256, base$artifact$booster_sha256)
})

test_that("[PLUGINS] gate01_perturbacao: [AC1] CONTROL: carga do dia last_data_day +500 MW => sha256 difere e min_max_BLF ou booster_txt mudam", {
  d3 <- lgbm_fx_perturb_days(deck, days = LDD, delta = 500)
  r3 <- lgbm_fx_train(d3, P = P)
  expect_false(identical(sha(r3$artifact), sha(base$artifact)))
  expect_true(!identical(r3$artifact$min_max_BLF, base$artifact$min_max_BLF) ||
                !identical(r3$artifact$booster_txt, base$artifact$booster_txt))
})

test_that("[PLUGINS] train_cortes: [AC1] painel com intervalo de O entregue ao despachante => contract; com 09-29 entregue ao plugin => contract", {
  ctx <- lgbm_fx_ctx(deck); cuts <- base$cuts
  full <- lgbm_fx_panel(deck, ctx, "infer", cuts)   # carga ate O 09:30
  expect_gt(max(lubridate::as_date(full$load$interval_start, tz = cuts$tz)), LDD)
  lgbm_lib$prevcarga_plugin_unload_all()
  plugin <- lgbm_lib$prevcarga_plugin_load("lgbm", file.path(lgbm_root(), "models"))
  e <- fail(lgbm_lib$prevcarga_plugin_train(plugin, full, cuts, plugin$spec))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  ## so ate 09-29 (um dia alem de last_data_day): a ingestao do plugin confere
  p29 <- lgbm_fx_panel(deck, ctx, "train", cuts)
  p29$load <- full$load[lubridate::as_date(interval_start, tz = cuts$tz) <= D("2026-09-29")]
  e2 <- fail(P$lgbm_ingest(p29, cuts, NULL, "train", K))
  expect_s3_class(e2, "prevcarga_error"); expect_identical(e2$class_name, "contract")
  ## features com dia alem de last_data_day chegando ao treino => contract
  ft <- data.table::copy(base$features)
  ft$feature_table <- data.table::copy(base$features$feature_table)
  ft$feature_table[Data == LDD, Data := D("2026-09-29")]
  e3 <- fail(P$lgbm_train(ft, base$spec, base$cuts, K))
  expect_s3_class(e3, "prevcarga_error"); expect_identical(e3$class_name, "contract")
})

test_that("[PLUGINS] train_cortes: [AC1] o estagio plugin_train (features, params, seed) treina como lgbm_train e le os cortes de attr(features, 'cuts')", {
  expect_identical(names(formals(P$plugin_train)), c("features", "params", "seed"))
  expect_identical(attr(base$features, "cuts")$last_data_day, LDD)
  m <- P$plugin_train(base$features, base$spec$params, base$spec$seed)
  expect_identical(P$lgbm_freeze(m)$booster_sha256, base$artifact$booster_sha256)
  nc <- base$features; attr(nc, "cuts") <- NULL
  expect_identical(fail(P$plugin_train(nc, base$spec$params, base$spec$seed))$class_name, "contract")
  ## RNG do chamador intocado
  set.seed(99); s0 <- .Random.seed
  P$lgbm_train(base$features, base$spec, base$cuts, K)
  expect_identical(.Random.seed, s0)
})

## ---- AC2: normalizacao so do treino (gate03), dupla normalizacao (gate02) ------------------------------------------
test_that("[PLUGINS] gate03_norm_train_only: [AC2] gate03: min_max_BLF so de Data <= train_cutoff, inclui o alvo; identico com a validacao/teste perturbados", {
  ft <- base$features$feature_table
  cols <- union(setdiff(base$features$colunas_entrada_BLF, base$features$colunas_nao_normalizar), "CargaGlobal")
  mm <- P$lgbm_compute_min_max(ft, cols, LDD)
  expect_identical(mm, base$model$min_max_BLF)
  expect_true("CargaGlobal" %in% names(base$model$min_max_BLF))
  expect_gt(base$model$min_max_BLF$CargaGlobal[["max"]], 1)
  ## recomputo manual so nas linhas <= corte
  fit <- ft[Data <= LDD]
  expect_equal(mm$CargaGlobal, c(min = min(fit$CargaGlobal), max = max(fit$CargaGlobal)))
  ## CONTROL: um corte anterior muda o max (a escala nao vem do painel inteiro)
  mm2 <- P$lgbm_compute_min_max(ft, cols, D("2026-06-20"))
  expect_false(identical(mm2$CargaGlobal, mm$CargaGlobal))
  ## amplitude 0 => 0; coluna toda NA => c(0, 1) com aviso
  z <- data.table::data.table(Data = D("2026-01-01") + 0:2, a = c(5, 5, 5), b = NA_real_)
  mmz <- suppressWarnings(P$lgbm_compute_min_max(z, c("a", "b"), D("2026-01-03")))
  expect_warning(P$lgbm_compute_min_max(z, "b", D("2026-01-03")), "toda NA")
  expect_identical(mmz$b, c(min = 0, max = 1))
  P$lgbm_normalise_dt(z, mmz)
  expect_identical(z$a, c(0, 0, 0))
  expect_identical(P$lgbm_norm(c(3, 7), "k", list(k = c(min = 3, max = 7))), c(0, 1))
  expect_identical(P$lgbm_denorm(c(0, 1), "k", list(k = c(min = 3, max = 7))), c(3, 7))
})

## holdout (train_cv: false) nos 3 esquemas. v2_old corta o treino 2 meses antes da validacao: precisa de deck mais longo.
deck_long <- lgbm_fx_deck(days = 170L)
holdout <- list()
for (sch in c("v2_old", "legacy_month", "rolling")) {
  long <- identical(sch, "v2_old")
  holdout[[sch]] <- lgbm_fx_train(if (long) deck_long else deck, P = P, params = list(train_cv = FALSE, cutoff_scheme = sch),
                                  history_days = if (long) 165L else 115L)
}

test_that("[PLUGINS] gate03_norm_train_only: [AC2] gate03 nos 3 esquemas de holdout: validacao/teste perturbados nao mudam min_max_BLF; treino alem do corte nao entra", {
  for (sch in names(holdout)) {
    h <- holdout[[sch]]
    expect_lt(h$features$train_cutoff, LDD)   # o holdout usa os cortes internos, nao last_data_day
    pert <- h$features
    pert$feature_table <- data.table::copy(h$features$feature_table)
    pert$feature_table[Data > h$features$train_cutoff, `:=`(CargaGlobal = CargaGlobal + 5000,
                                                           CargaGlobal_anterior_BLF = CargaGlobal_anterior_BLF + 5000)]
    m2 <- P$lgbm_train(pert, h$spec, base$cuts, K)
    expect_identical(m2$min_max_BLF, h$model$min_max_BLF, info = sch)
    expect_identical(h$model$train_mode, "holdout", info = sch)
    expect_true(is.na(h$model$cv_nfolds))
    expect_lte(h$model$best_iter, K$num_iterations)
    expect_identical(P$lgbm_n_trees(P$lgbm_freeze(h$model)$booster_txt), h$model$best_iter)
  }
  ## cada esquema usa um corte de treino distinto (nao o last_data_day do CV)
  expect_gt(length(unique(vapply(holdout, function(h) format(h$features$train_cutoff), ""))), 1L)
})

test_that("[PLUGINS] gate02_escala_mw: [AC2] gate02: FE ja normalizada (lag com max <= 1) aborta [prevcarga/contract] lgbm.double_norm", {
  ft <- base$features
  ft$feature_table <- data.table::copy(base$features$feature_table)
  ft$feature_table[, CargaGlobal_anterior_BLF := CargaGlobal_anterior_BLF / max(CargaGlobal_anterior_BLF)]
  e <- fail(P$lgbm_train(ft, base$spec, base$cuts, K))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$id, "lgbm.double_norm"); expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  expect_match(conditionMessage(e), "^\\[prevcarga/contract\\] lgbm: ")
})

## ---- AC3: G-CV e gate_cv_refit --------------------------------------------------------------------------------------
test_that("[PLUGINS] gate_cv_refit: [AC3] G-CV: folds 4 x 336 round-robin cobrem todas as linhas, disjuntos", {
  for (n in c(1L, 335L, 336L, 1337L, 5249L)) {
    f <- P$lgbm_cv_folds(n, K)
    expect_identical(f, split(seq_len(n), rep(rep(1:4, each = 336), length.out = n)), info = n)
    expect_identical(sort(unlist(f, use.names = FALSE)), seq_len(n))
    expect_false(anyDuplicated(unlist(f, use.names = FALSE)) > 0L)
  }
  f <- P$lgbm_cv_folds(base$model$n_train_rows, K)
  expect_length(f, 4L)
  expect_identical(f[[1L]][1:336], 1:336); expect_identical(f[[2L]][1L], 337L)
})

test_that("[PLUGINS] gate_cv_refit: [AC3] G-CV/gate_cv_refit: lgb.cv e lgb.train recebem seed da spec + determinismo; refit = 100% com nrounds = best_iter", {
  seen <- list()
  real_cv <- lightgbm::lgb.cv; real_tr <- lightgbm::lgb.train
  testthat::local_mocked_bindings(
    lgb.cv = function(...) { a <- list(...); seen$cv <<- a; real_cv(...) },
    lgb.train = function(...) { a <- list(...); seen$tr <<- a; real_tr(...) }, .package = "lightgbm")
  sp <- base$spec; sp$seed <- 77L
  m <- P$lgbm_train(base$features, sp, base$cuts, K)
  for (nm in c("cv", "tr")) {
    p <- seen[[nm]]$params
    expect_identical(p$seed, 77L, info = nm); expect_identical(p$num_threads, 1L, info = nm)
    expect_true(p$deterministic, info = nm); expect_true(p$force_row_wise, info = nm)
    expect_identical(p$num_leaves, K$num_leaves); expect_identical(p$min_data_in_leaf, K$min_data_in_leaf)
    expect_identical(p$learning_rate, K$learning_rate); expect_identical(p$objective, "regression")
    expect_identical(p$metric, "mse")
  }
  expect_identical(seen$cv$early_stopping_rounds, 50L)
  expect_identical(seen$cv$nrounds, K$num_iterations)
  expect_identical(seen$cv$folds, P$lgbm_cv_folds(m$n_train_rows, K))
  expect_identical(seen$tr$nrounds, m$best_iter)               # gate_cv_refit
  expect_null(seen$tr$valids)
  expect_identical(seen$tr$data$dim()[1L], m$n_train_rows)     # 100% das linhas <= last_data_day
  expect_identical(m$train_mode, "cv"); expect_identical(m$cv_nfolds, 4L)
  expect_identical(P$lgbm_n_trees(P$lgbm_freeze(m)$booster_txt), m$best_iter)
  ## best_iter ausente/nao finito do lgb.cv => num_iterations
  testthat::local_mocked_bindings(
    lgb.cv = function(...) { r <- real_cv(...); r$best_iter <- NA_integer_; r },
    lgb.train = function(params, data, nrounds, ...) { seen$n2 <<- nrounds; real_tr(params = params, data = data, nrounds = 3L, ...) },
    .package = "lightgbm")
  m3 <- P$lgbm_train(base$features, base$spec, base$cuts, K)
  expect_identical(seen$n2, K$num_iterations); expect_identical(m3$best_iter, K$num_iterations)
})

test_that("[PLUGINS] gate_cv_refit: [AC3] CONTROL train_cv false: holdout com valids + early stopping 50 (determinismo da spec), booster_txt muda", {
  h <- holdout$v2_old
  seen <- list()
  real_tr <- lightgbm::lgb.train
  testthat::local_mocked_bindings(
    lgb.train = function(...) { seen$a <<- list(...); real_tr(...) }, .package = "lightgbm")
  m <- P$lgbm_train(h$features, h$spec, base$cuts, K)
  expect_identical(m$train_mode, "holdout")
  expect_identical(names(seen$a$valids), "val")
  expect_identical(seen$a$early_stopping_rounds, 50L)
  expect_identical(seen$a$nrounds, K$num_iterations)
  expect_identical(seen$a$params$seed, h$spec$seed); expect_identical(seen$a$params$num_threads, 1L)
  expect_true(seen$a$params$deterministic); expect_true(seen$a$params$force_row_wise)
  expect_false(identical(P$lgbm_freeze(m)$booster_txt, base$artifact$booster_txt))
  ## holdout sem linhas de validacao => contract (nunca treina as cegas)
  nv <- h$features; nv$val_cutoff <- nv$train_cutoff
  expect_identical(fail(P$lgbm_train(nv, h$spec, base$cuts, K))$class_name, "contract")
})
