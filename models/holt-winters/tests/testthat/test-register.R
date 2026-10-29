## test-register.R -- gates [PLUGINS] registro / catalogo_hw do plugin Holt-Winters (HW.1; AC 1 e AC 6).

hw_models_root <- function() file.path(hw_root(), "models")
hw_stage_names <- c("ingestion", "feature_engineering", "training", "inference_features", "inference", "forecast")

test_that("[PLUGINS] registro: descriptor x run.yaml real passa e declara o contrato do HW", {
  g0 <- ls(globalenv(), all.names = TRUE)
  p <- hw_lib$prevcarga_plugin_load("holt-winters", hw_models_root())
  d <- p$descriptor
  expect_identical(d$model_id, "holt-winters")
  expect_identical(d$granularity_supported, 30L)
  expect_identical(d$native_layout, "hw_matrix_nx48")
  expect_identical(d$delivery_days, 9L)
  expect_false(d$history_infer_equals_train)
  expect_identical(d$modes_implemented, c("train", "infer", "forecast"))
  expect_identical(d$data_lag_days, 0L)
  expect_identical(d$model_time_offset_min, 0L)
  expect_identical(names(d$params_schema),
                   c("kmeans_seed", "ic", "perfil_mo", "classes_mo", "similar", "feriado", "suaviza"))
  for (n in names(d$params_schema)) expect_null(d$params_schema[[n]]$default, info = n)
  expect_true(is.na(d$variant_param))
  expect_null(d$semihourly_rule)
  expect_identical(names(d$stages), hw_stage_names)
  for (s in hw_stage_names) {
    expect_gt(length(d$stages[[s]]$requires), 0L)
    expect_gt(length(d$stages[[s]]$provides), 0L)
  }
  expect_identical(p$spec$split, "origin_scoped")
  expect_identical(p$spec$params$kmeans_seed, 123L)
  expect_identical(ls(globalenv(), all.names = TRUE), g0)
})

test_that("[PLUGINS] registro: run.yaml real traz o contrato e nenhuma chave extinta", {
  y <- yaml::read_yaml(file.path(hw_root(), "models", "holt-winters", "run.yaml"))
  expect_identical(y$name, "holt-winters")
  expect_identical(y$split, "origin_scoped")
  expect_identical(y$granularity_min, 30L)
  expect_identical(y$horizon_days, 8L)
  expect_identical(y$history_days, list(train = 730L, infer = 45L))
  expect_identical(y$seed, 1234L)
  expect_identical(y$modes, c("train", "infer", "forecast"))
  expect_identical(y$backtest$protocol_default, "retrain_harness")
  expect_identical(names(y$params), c("kmeans_seed", "ic", "perfil_mo", "classes_mo", "similar", "feriado", "suaviza"))
  expect_identical(y$params$kmeans_seed, 123L)
  expect_identical(y$params$perfil_mo, "C")
  expect_identical(y$params$classes_mo, 12L)
  expect_identical(y$params$similar, 0.8)
  expect_true(y$params$feriado)
  expect_true(y$params$suaviza)
  expect_false(any(c("cut_invariant", "protocol", "env") %in% names(y)))
})

test_that("[PLUGINS] registro: CONTROL run.yaml-fixture abortam [prevcarga/contract] nomeando o campo", {
  g0 <- ls(globalenv(), all.names = TRUE)
  cases <- list(
    kmeans_seed = list(mut = function(y) { y$params$kmeans_seed <- NULL; y }, field = "kmeans_seed"),
    granularity = list(mut = function(y) { y$granularity_min <- 60L; y }, field = "granularity_min"),
    split = list(mut = function(y) { y$split <- "origin_agnostic"; y }, field = c("split", "modes")),
    cut_invariant = list(mut = function(y) { y$cut_invariant <- TRUE; y }, field = "cut_invariant"))
  for (nm in names(cases)) {
    root <- hw_fx_runyaml(cases[[nm]]$mut)
    e <- hw_catch(hw_lib$prevcarga_plugin_load("holt-winters", root))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$class_name, "contract", info = nm)
    expect_identical(e$exit_code, 2L, info = nm)
    expect_match(conditionMessage(e), "^\\[prevcarga/contract\\]", info = nm)
    expect_true(any(vapply(cases[[nm]]$field, grepl, NA, x = conditionMessage(e), fixed = TRUE)), info = nm)
  }
  ## CONTROL do CONTROL: o run.yaml real (copia sem mutacao) carrega
  ok <- hw_fx_runyaml()
  expect_s3_class(hw_lib$prevcarga_plugin_load("holt-winters", ok), "prevcarga_plugin")
  expect_identical(ls(globalenv(), all.names = TRUE), g0)
})

## ids literais usados em hw_abort/hw_warn/.hw_stage dentro de `dir`
hw_used_ids <- function(dir) {
  fs <- list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)
  ids <- unlist(lapply(fs, function(f) {
    m <- regmatches(l <- readLines(f, warn = FALSE), gregexpr("hw_(abort|warn)\\(\"([a-z_.]+)\"", l))
    sub("^hw_(abort|warn)\\(\"([a-z_.]+)\"$", "\\2", unlist(m))
  }))
  unique(c(ids, if (any(grepl("plugin.stage_missing", unlist(lapply(fs, readLines, warn = FALSE)), fixed = TRUE)))
    "plugin.stage_missing"))
}
hw_ids_missing <- function(dir, catalog) setdiff(hw_used_ids(dir), catalog$id)

test_that("[PLUGINS] catalogo_hw: os 10 ids hw.* declarados, ASCII, classe esperada, TSV em sincronia", {
  cat <- hw_lib$prevcarga_errors_load()
  want <- c(hw.raw_timestamp = "contract", hw.input_column = "contract", hw.train_day_o = "contract",
            hw.weather_beyond_cut = "contract", hw.artifact_origin = "contract",
            hw.last_day_incomplete = "completude", hw.history_short = "window", hw.weather_absent = "completude",
            hw.bad_param = "contract", hw.fe_flag = "contract")
  expect_true(all(names(want) %in% cat$id))
  got <- cat[match(names(want), cat$id), ]
  expect_identical(got$class, unname(want))
  expect_true(all(nzchar(got$message)) && all(nzchar(got$hint)))
  tsv <- file.path(hw_root(), "scripts", "lib", "errors", "holt_winters.tsv")
  raw <- readBin(tsv, "raw", file.size(tsv))
  expect_true(all(as.integer(raw) <= 127L))
  expect_false(as.raw(13L) %in% raw)
  own <- utils::read.delim(tsv, quote = "", colClasses = "character")
  expect_setequal(own$id, names(want))
  expect_false(anyDuplicated(cat$id) > 0L)
  expect_identical(hw_ids_missing(file.path(hw_root(), "models", "holt-winters", "R"), cat), character())
})

test_that("[PLUGINS] catalogo_hw: CONTROL id inexistente e acento reprovam", {
  cat <- hw_lib$prevcarga_errors_load()
  d <- tempfile("hwr-"); dir.create(d)
  writeLines("hw_abort(\"hw.nao_existe\", \"x\")", file.path(d, "x.R"))
  expect_identical(hw_ids_missing(d, cat), "hw.nao_existe")
  bad <- data.frame(id = "hw.acentu", class = "contract", message = "não", hint = "ok", stringsAsFactors = FALSE)
  e <- hw_catch(hw_lib$prevcarga_catalog_assert_ascii(bad))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$id, "contract.error_catalog_schema")
})
