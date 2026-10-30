## test-contract.R -- LGBM.1: descriptor x run.yaml pelo loader P.0b. FORBIDDEN (aborta) e CONTROL (run.yaml real passa).
library(testthat)

root <- file.path(lgbm_root(), "models")
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
load_plugin <- function(models_root) {
  lgbm_lib$prevcarga_plugin_unload_all()
  lgbm_lib$prevcarga_plugin_load("lgbm", models_root)
}

test_that("[AC1] run.yaml real x descriptor passam no loader e batem com o contrato do oraculo", {
  p <- load_plugin(root)
  d <- p$descriptor; s <- p$spec
  expect_identical(d$model_id, "lgbm")
  expect_identical(s$split, "origin_agnostic")
  expect_identical(s$granularity_min, 30L)
  expect_identical(s$horizon_days, 1L)
  expect_identical(s$modes, c("train", "infer"))
  expect_identical(s$seed, 123L)
  expect_identical(s$history_days, list(train = 730L, infer = 45L))
  expect_identical(d$delivery_days, 2L)
  expect_identical(d$data_lag_days, 2L)
  expect_identical(d$native_layout, "lgbm_day_48")
  expect_false(d$history_infer_equals_train)
  expect_identical(d$granularity_supported, 30L)
  expect_identical(d$modes_implemented, c("train", "infer"))
  expect_identical(s$backtest$protocol_default, "daily")
  expect_identical(s$params, list(disable_loess = TRUE, calendar_yearend = TRUE, train_cv = TRUE, tipodia_legacy = FALSE,
                                  cutoff_scheme = "v2_old", sazonalidade_scope = "train", prevespera_1230 = FALSE))
  expect_setequal(names(d$params_schema), names(s$params))
  for (n in names(d$params_schema)) expect_null(d$params_schema[[n]]$default)
  expect_identical(names(d$stages), c("ingestion", "feature_engineering", "training", "inference_features", "inference"))
  for (st in d$stages) { expect_gt(length(st$requires), 0L); expect_gt(length(st$provides), 0L) }
  expect_true(all(c("data.load.load_mwh", "data.weather_forecast.temperature", "data.holidays.prevcarga_code") %in%
                    d$stages$ingestion$requires))
  expect_setequal(d$packages, c("lightgbm", "data.table", "lubridate", "wavelets", "zoo"))
  ## M1.2: o spec lido pela topologia continua valido
  ms <- lgbm_lib$prevcarga_model_spec("lgbm", lgbm_root())
  expect_identical(ms$params, s$params)
  expect_identical(ms$history_days$train, 730L)
})

test_that("[AC2] 6 copias mutadas abortam [prevcarga/contract] (exit 2) nomeando o campo antes de qualquer estagio", {
  cases <- list(
    train_cv_ausente = list(f = function(y) { y$params$train_cv <- NULL; y }, field = "params$train_cv"),
    full_window = list(f = function(y) { y$params$sazonalidade_scope <- "full_window"; y }, field = "params$sazonalidade_scope"),
    cutoff_foo = list(f = function(y) { y$params$cutoff_scheme <- "foo"; y }, field = "params$cutoff_scheme"),
    modes_forecast = list(f = function(y) { y$modes <- c("train", "infer", "forecast"); y }, field = "modes"),
    gran_60 = list(f = function(y) { y$granularity_min <- 60L; y }, field = "granularity_min"),
    loess_string = list(f = function(y) { y$params$disable_loess <- "1"; y }, field = "params$disable_loess"))
  for (n in names(cases)) {
    r <- lgbm_fx_runyaml(cases[[n]]$f)
    ## espiao: o plugin_ingest da copia grava um marcador se algum estagio rodar
    spy <- file.path(r, "spy.txt")
    writeLines(c("plugin_ingest <- function(panel, cuts) {",
                 sprintf("  writeLines('ran', '%s'); stop('spy')", spy), "}"), file.path(r, "lgbm", "R", "zz_spy.R"))
    e <- fail(load_plugin(r))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$class_name, "contract", info = n)
    expect_identical(e$exit_code, 2L, info = n)
    expect_true(grepl(cases[[n]]$field, paste(conditionMessage(e), e$details$field), fixed = TRUE), info = n)
    expect_true(grepl("^[ -~]*$", conditionMessage(e)), info = n)
    expect_false(file.exists(spy), info = n)
  }
  ## CONTROL: o run.yaml real passa
  expect_null(fail(load_plugin(lgbm_fx_runyaml())))
})

test_that("params: lgbm_check_params nomeia a chave (ausente, tipo, fora do schema) e aceita o run.yaml real", {
  p <- lgbm_source_plugin()
  real <- yaml::read_yaml(file.path(lgbm_root(), "models", "lgbm", "run.yaml"))$params
  expect_silent(p$lgbm_check_params(real))
  chk <- function(mut, msg) {
    e <- fail(p$lgbm_check_params(mut))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, "lgbm.params")
    expect_identical(e$exit_code, 2L)
    expect_true(grepl(msg, conditionMessage(e), fixed = TRUE), info = msg)
  }
  m <- real; m$train_cv <- NULL; chk(m, "params$train_cv ausente")
  m <- real; m$sazonalidade_scope <- "full_window"; chk(m, "params$sazonalidade_scope=full_window fora do schema")
  m <- real; m$cutoff_scheme <- "foo"; chk(m, "params$cutoff_scheme=foo fora do schema")
  m <- real; m$disable_loess <- "1"; chk(m, "params$disable_loess tipo character != logical")
  m <- real; m$extra_flag <- TRUE; chk(m, "params$extra_flag desconhecido")
  m <- real; m$tipodia_legacy <- NA; chk(m, "params$tipodia_legacy")
})

test_that("catalogo: ids lgbm.* existem em errors/lgbm.tsv com a classe esperada", {
  cat <- lgbm_lib$prevcarga_errors_load(file.path(lgbm_root(), "scripts", "lib", "errors"))
  expect_false(anyDuplicated(cat$id) > 0L)
  got <- setNames(cat$class[match(c("lgbm.params", "lgbm.contract", "lgbm.completude", "lgbm.window", "lgbm.stage_missing"), cat$id)],
                  c("params", "contract", "completude", "window", "stage_missing"))
  expect_identical(unname(got), c("contract", "contract", "completude", "window", "contract"))
})

test_that("estagio sem definicao aborta lgbm.stage_missing (nao ha codigo silencioso); os 5 estagios e freeze/thaw existem", {
  p <- lgbm_source_plugin()
  ## FE (LGBM.2), treino/freeze/thaw (LGBM.3) e inferencia (LGBM.4) ja existem
  for (f in c("plugin_build_features", "plugin_train", "lgbm_freeze", "lgbm_thaw", "plugin_inference_features", "plugin_infer"))
    expect_true(is.function(p[[f]]), info = f)
  ## CONTROL: o despacho por nome acusa a definicao ausente
  rm("plugin_infer", envir = p)
  e <- fail(p$lgbm_stage_inference(NULL, NULL, NULL))
  expect_identical(e$id, "lgbm.stage_missing")
  expect_true(grepl("plugin_infer", conditionMessage(e), fixed = TRUE))
  ## quando a definicao existe, o despacho a encontra
  p$plugin_infer <- function(payload, inf_features, cuts) "ok"
  expect_identical(p$lgbm_stage_inference(NULL, NULL, NULL), "ok")
})

test_that("plugin_constants: hiperparametros, spans, heat_area e feriados fixos do oraculo", {
  k <- lgbm_source_plugin()$plugin_constants()
  expect_identical(k$num_leaves, 16L); expect_identical(k$min_data_in_leaf, 50L)
  expect_identical(k$num_iterations, 5000L); expect_identical(k$early_stopping_rounds, 50L)
  expect_identical(k$cv_nfolds, 4L); expect_identical(k$cv_week_block, 336L)
  expect_identical(k$wavelet_width, 32L); expect_identical(k$carwav_shift_days, 2L)
  expect_identical(k$issue_day_tz, "UTC"); expect_identical(k$min_history_days_infer, 4L)
  expect_identical(unname(k$heat_area[c("SECO", "S", "N", "NE")]), c("SP", "RS", "AM", "CE"))
  expect_s3_class(k$holidays_fixed, "Date")
  expect_length(k$holidays_fixed, 69L)
  expect_false(is.unsorted(k$holidays_fixed))
})
