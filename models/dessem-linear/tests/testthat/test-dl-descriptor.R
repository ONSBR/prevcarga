## test-dl-descriptor.R -- DL.5 AC1: run.yaml completo + descriptor via prevcarga_plugin_load; CONTROL: history_days.infer.

test_that("ac-1 run.yaml e descriptor batem e o loader aceita o plugin", {
  p <- dl_load()
  d <- p$descriptor; s <- p$spec
  expect_identical(s$split, "origin_scoped")
  expect_identical(s$granularity_min, 60L)
  expect_identical(s$modes, c("train", "infer", "forecast"))
  expect_identical(s$seed, 42L)
  expect_identical(s$horizon_days, 9L)
  expect_identical(s$history_days, list(train = 1825L, infer = 1825L))
  expect_identical(s$backtest$protocol_default, "retrain_harness")
  expect_identical(d$model_id, "dessem-linear")
  expect_identical(d$granularity_supported, 60L)
  expect_identical(d$delivery_days, 9L)
  expect_identical(d$model_time_offset_min, 60L)
  expect_true(d$history_infer_equals_train)
  expect_null(d$rng_deviation)
  expect_identical(d$engine, "_dessem-engine")
  expect_identical(d$variant_param, "exogena")
  expect_identical(d$modes_implemented, c("train", "infer", "forecast"))
  expect_true(is.function(d$semihourly_rule))
  expect_true(all(c("stats", "utils", "data.table", "WeightSVM") %in% d$attach))
  for (st in names(d$stages)) {
    expect_true(is.function(d$stages[[st]]$fn), info = st)
    expect_gt(length(d$stages[[st]]$requires), 0L)
    expect_gt(length(d$stages[[st]]$provides), 0L)
  }
})

test_that("ac-1 plugin_constants: mapa exogena -> model_key do motor e as chaves existem no model_parameters.json", {
  p <- dl_load()
  k <- p$env$plugin_constants()
  expect_identical(unname(k$model_key[c("MED", "MAX", "MXMN")]), c("Linear.TmpMedia", "Linear.TmpMaxima", "Linear.TmpMaxMin"))
  eng <- parent.env(p$env)
  for (key in k$model_key) expect_identical(eng$getConfigModel(key)$nomModelo, key)
  expect_identical(k$hours_per_day, 24L); expect_identical(k$mm_ref_hour, 12L); expect_identical(k$max_tail_fill_days, 2L)
})

test_that("ac-1 semihourly_rule chama desagregaModeloSH do motor (2 valores por hora) e cai na duplicacao com aviso", {
  p <- dl_load()
  rule <- p$descriptor$semihourly_rule
  t0 <- lubridate::force_tz(lubridate::ymd_hms("2024-12-23 01:00:00"), DL_TZ)
  x <- data.table::data.table(interval_end = t0 + lubridate::hours(0:23), y_pred = 9000 + 500 * sin((0:23) / 4))
  y <- rule(x)
  expect_length(y, 48L); expect_true(all(is.finite(y)))
  ref <- suppressMessages(parent.env(p$env)$desagregaModeloSH("M", data.table::data.table(DataHora = x$interval_end, Carga = x$y_pred, Modelo = "M")))
  expect_identical(y, ref$Carga)
  ## media das duas meias horas == valor horario (propriedade do desagrega)
  expect_equal((y[c(TRUE, FALSE)] + y[c(FALSE, TRUE)]) / 2, x$y_pred, tolerance = 1e-9)
  ## CONTROL: 1 hora so => o spline falha => duplicacao + aviso do catalogo
  one <- x[1L]
  w <- NULL
  y1 <- withCallingHandlers(dl_with_cli(rule(one)), prevcarga_warning = function(c) { w <<- c; invokeRestart("muffleWarning") })
  expect_identical(y1, rep(one$y_pred, 2L))
  expect_identical(w$id, "warning.dessem_linear_desagrega_fallback")
})

test_that("ac-1 CONTROL: run.yaml com history_days.infer 365 => [prevcarga/contract] nomeando history_days", {
  root <- tempfile("dlroot-"); dir.create(root)
  file.copy(file.path(dl_models(), c("_dessem-engine", "dessem-linear")), root, recursive = TRUE)
  ry <- file.path(root, "dessem-linear", "run.yaml")
  writeLines(sub("infer: 1825", "infer: 365", readLines(ry), fixed = TRUE), ry)
  cond <- expect_dl_error(dl_load(root), "plugin.history_window", "history_days")
  expect_identical(cond$exit_code, 2L)
  expect_identical(cond$class_name, "contract")
})

test_that("estagios de DL.6/DL.7 ainda nao existem: stubs recusam com plugin.stage_missing", {
  p <- dl_load()
  expect_dl_error(p$descriptor$stages$training$fn(NULL, list(), 42L), "plugin.stage_missing", "training")
  expect_dl_error(p$descriptor$freeze(NULL), "plugin.stage_missing", "freeze")
})

test_that("catalogo de erros do plugin: ids unicos na uniao, ASCII, classes da tabela de exit-codes", {
  cat <- dl_lib$prevcarga_errors_load()
  ids <- c("bad_exogena", "mm_missing_origin", "train_sees_day_o", "rng_deviation", "artifact_mismatch",
           "series_gap", "window_short")
  want <- paste0("plugin.dessem_linear_", ids)
  expect_true(all(want %in% cat$id))
  expect_true("warning.dessem_linear_desagrega_fallback" %in% cat$id)
  r <- cat[cat$id %in% c(want, "warning.dessem_linear_desagrega_fallback"), ]
  expect_identical(r$class[match(c("plugin.dessem_linear_bad_exogena", "plugin.dessem_linear_mm_missing_origin",
                                   "plugin.dessem_linear_rng_deviation", "plugin.dessem_linear_window_short"), r$id)],
                   c("contract", "completude", "repro", "window"))
  expect_false(any(grepl("[^\\x01-\\x7F]", unlist(r[c("message", "hint")]), perl = TRUE)))
})
