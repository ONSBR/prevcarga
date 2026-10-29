## test-contract.R -- RF.1: descriptor x run.yaml pelo loader P.0b. CONTROL (run.yaml real passa) e FORBIDDEN (mutacoes abortam).
library(testthat)

load_copy <- function(...) {
  root <- rf_fx_plugin_copy(...)
  rf_lib$prevcarga_plugin_unload_all()
  rf_lib$prevcarga_plugin_load("random-forest", root)
}
field_of <- function(e) if (!is.null(e$details$field)) e$details$field else e$details$key

test_that("[PLUGINS] contrato: run.yaml real + descriptor passam no loader e registram os 5 estagios Tier A", {
  p <- load_copy()
  d <- p$descriptor; s <- p$spec
  expect_identical(s$split, "origin_agnostic")
  expect_identical(s$granularity_min, 60L)
  expect_identical(s$horizon_days, 9L)
  expect_identical(s$history_days, list(train = 365L, infer = 45L))
  expect_identical(s$seed, 123L)
  expect_identical(s$modes, c("train", "infer"))
  expect_identical(s$backtest$protocol_default, "daily")
  expect_identical(s$params, list(ntree = 100L))
  expect_identical(d$model_id, "random-forest")
  expect_identical(d$granularity_supported, 60L)
  expect_identical(d$modes_implemented, c("train", "infer"))
  expect_identical(d$params_schema, list(ntree = list(type = "integer", min = 1L)))
  expect_null(d$params_schema$ntree$default)
  expect_identical(d$native_layout, "rf_wide_hourly_216")
  ## o loader exige funcao: o nome `rf_semihourly_rule` e resolvido no env do plugin (corpo no RF.3)
  expect_true(is.function(d$semihourly_rule))
  expect_identical(d$semihourly_rule, p$env$rf_semihourly_rule)
  expect_identical(d$delivery_days, 9L)
  expect_identical(d$data_lag_days, 0L)
  expect_false(d$history_infer_equals_train)
  expect_setequal(d$packages, c("randomForest", "data.table", "lubridate"))
  expect_identical(names(d$stages), c("ingestion", "feature_engineering", "training", "inference_features", "inference"))
  for (st in d$stages) { expect_gt(length(st$requires), 0L); expect_gt(length(st$provides), 0L) }
  reg <- names(rf_lib$prevcarga_registry())
  expect_true(all(paste0("random-forest/", c("ingestion", "feature_engineering", "training", "inference_features",
                                            "inference")) %in% reg))
  ## M1.2: o spec lido pela topologia continua valido
  ms <- rf_lib$prevcarga_model_spec("random-forest", rf_root())
  expect_identical(ms$params, list(ntree = 100L))
})

test_that("[PLUGINS] contrato: sessao R limpa carrega a copia sem gap", {
  root <- rf_fx_plugin_copy()
  libs <- paste(sprintf("'%s'", file.path(rf_root(), "scripts", "lib",
    paste0(c("errors", "runtime", "config", "topology", "deck_reader", "validation", "dates", "registry", "features"), ".R"))),
    collapse = ", ")
  f <- tempfile("rf-clean-", fileext = ".R")
  writeLines(c(sprintf("options(prevcarga.lib = '%s')", file.path(rf_root(), "scripts", "lib")),
               sprintf("for (f in c(%s)) source(f)", libs),
               sprintf("p <- prevcarga_plugin_load('random-forest', '%s')", root),
               "writeLines(paste(p$descriptor$model_id, length(p$descriptor$stages), sum(grepl('^random-forest/', names(prevcarga_registry())))))"), f)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f)), stdout = TRUE, stderr = TRUE))
  expect_identical(attr(out, "status"), NULL)
  expect_identical(tail(out, 1L), "random-forest 5 5")
})

test_that("[PLUGINS] contrato: 6 mutacoes do run.yaml abortam [prevcarga/contract] exit 2 nomeando o campo (CONTROL: real passa)", {
  cases <- list(
    forecast_em_modes = list(f = function(y) { y$modes <- c("train", "infer", "forecast"); y }, field = "modes"),
    granularity_30 = list(f = function(y) { y$granularity_min <- 30L; y }, field = "granularity_min"),
    ntree_ausente = list(f = function(y) { y$params$ntree <- NULL; y }, field = "params$ntree"),
    ntree_string = list(f = function(y) { y$params$ntree <- "100"; y }, field = "params$ntree"),
    seed_ausente = list(f = function(y) { y$seed <- NULL; y }, field = "seed"),
    cut_invariant = list(f = function(y) { y$cut_invariant <- TRUE; y }, field = "cut_invariant"))
  for (n in names(cases)) {
    e <- fail(load_copy(mutate = cases[[n]]$f))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$class_name, "contract", info = n)
    expect_identical(e$exit_code, 2L, info = n)
    expect_identical(field_of(e), cases[[n]]$field, info = n)
  }
  expect_null(fail(load_copy()))
})

test_that("[PLUGINS] contrato: pacote inexistente em packages => [prevcarga/bundle] exit 6 (nunca 7)", {
  e <- fail(load_copy(add_pkg = "pacote.rf.inexistente"))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "bundle")
  expect_identical(e$exit_code, 6L)
  expect_true("pacote.rf.inexistente" %in% e$details$packages)
})

test_that("[PLUGINS] contrato: nenhum default de ntree/seed e nenhuma leitura de ambiente/relogio no codigo do plugin", {
  r_dir <- file.path(RF_PLUGIN_DIR(), "R")
  expect_identical(rf_grep_code(r_dir, "Sys[.](getenv|time|Date)|setwd[(]|<<-|globalenv[(]|saveRDS[(]|system2?[(]"), character())
  expect_identical(rf_grep_code(r_dir, "as[.]Date|as[.]POSIXct|as[.]POSIXlt|strptime|strftime|ISOdatetime|difftime"), character())
  expect_identical(rf_grep_code(r_dir, "minutes\\(\\s*30|1800"), character())
  expect_identical(rf_grep_code(r_dir, "ntree\\s*=\\s*[0-9]|seed\\s*=\\s*[0-9]"), character())
  for (f in list.files(r_dir, full.names = TRUE)) expect_false(any(utf8ToInt(paste(readLines(f, warn = FALSE), collapse = "")) > 127L), info = f)
  ## CONTROL: a varredura acusa uma isca
  tmp <- tempfile("rf-bait-"); dir.create(tmp)
  writeLines("x <- function() Sys.time()", file.path(tmp, "bait.R"))
  expect_identical(rf_grep_code(tmp, "Sys[.](getenv|time|Date)"), "bait.R:1")
})
