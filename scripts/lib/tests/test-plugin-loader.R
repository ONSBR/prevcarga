## test-plugin-loader.R -- loader de plugins (P.0b): env isolado, motor por instancia, descriptor x run.yaml.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "topology", "deck_reader", "validation", "registry", "features"), env = environment())

reset_all <- function() { prevcarga_registry_reset(); prevcarga_plugin_unload_all() }

## Aborta classificado: devolve a condicao; confere id, classe, exit e que a mensagem nomeia `field`.
expect_plugin_error <- function(expr, id, field = NULL, class = "contract", exit = 2L) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (is.null(cond)) return(invisible(NULL))
  expect_identical(cond$id, id)
  expect_identical(cond$class_name, class)
  expect_identical(cond$exit_code, exit)
  expect_match(conditionMessage(cond), sprintf("[prevcarga/%s]", class), fixed = TRUE)
  if (!is.null(field)) expect_true(grepl(field, conditionMessage(cond), fixed = TRUE))
  expect_false(grepl("[^\\x01-\\x7F]", conditionMessage(cond), perl = TRUE))
  invisible(cond)
}

load_toy <- function(base, ..., name = base) {
  root <- pc_toy_variant(base, ..., name = name)
  list(root = root, load = function() prevcarga_plugin_load(name, root))
}
stage_ids <- function(m, n) paste0(m, "/", c(prevcarga_plugin_stages, if (n == 6L) "forecast"))

test_that("ac-8 carrega os dois brinquedos isolados, registra Tier A e memoiza", {
  reset_all()
  snap <- pc_globalenv_snapshot()
  for (spec in list(list("toy-30", 5L), list("toy-60", 6L))) {
    m <- spec[[1L]]
    p1 <- prevcarga_plugin_load(m, pc_toy_root())
    p2 <- prevcarga_plugin_load(m, pc_toy_root())
    expect_identical(p1, p2)
    expect_s3_class(p1, "prevcarga_plugin")
    reg <- prevcarga_registry()
    expect_true(all(stage_ids(m, spec[[2L]]) %in% names(reg)))
    expect_identical(sum(startsWith(names(reg), paste0(m, "/"))), spec[[2L]])
    for (id in stage_ids(m, spec[[2L]])) {
      e <- reg[[id]]
      expect_identical(e$tier, "A")
      expect_true(length(e$requires) > 0L && length(e$provides) > 0L)
      expect_true(all(grepl(paste0("^(data[.]|", m, "::)"), c(e$requires, e$provides))))
    }
    expect_true("data.load.load_mwh" %in% reg[[paste0(m, "/ingestion")]]$requires)
    expect_identical(pc_globalenv_snapshot(), snap)
  }
  prevcarga_registry_reset()
  expect_length(prevcarga_registry(), 0L)
  p3 <- prevcarga_plugin_load("toy-30", pc_toy_root())
  expect_identical(p3, prevcarga_plugin_load("toy-30", pc_toy_root()))
  expect_true("toy-30/training" %in% names(prevcarga_registry()))
  expect_identical(pc_globalenv_snapshot(), snap)
})

test_that("ac-9 provides_gap, not_found e toplevel abortam sem registro parcial; CONTROL carrega limpo", {
  reset_all()
  snap <- pc_globalenv_snapshot()
  gap <- load_toy("toy-30", descriptor = function(d) { d$stages$feature_engineering$provides <- "feat.y"; d })
  c1 <- expect_plugin_error(gap$load(), "plugin.provides_gap", "toy-30::feat.x")
  expect_match(conditionMessage(c1), "toy-30/training", fixed = TRUE)
  expect_length(prevcarga_registry(), 0L)
  root <- pc_toy_variant("toy-30")
  unlink(file.path(root, "toy-30", "run.yaml"))
  c2 <- expect_plugin_error(prevcarga_plugin_load("toy-30", root), "plugin.not_found", "run.yaml")
  expect_match(conditionMessage(c2), file.path(root, "toy-30", "run.yaml"), fixed = TRUE)
  expect_error(prevcarga_plugin_load("nao-existe", pc_toy_root()), class = "prevcarga_error")
  top <- load_toy("toy-30", files = list("R/x.R" = c("x_fn <- function() 1", "", "x <- 1")))
  expect_plugin_error(top$load(), "plugin.toplevel_code", "x.R:3")
  lib <- load_toy("toy-30", files = list("R/y.R" = c("y_fn <- function() 1", "library(stats)")))
  expect_plugin_error(lib$load(), "plugin.toplevel_code", "y.R:2")
  expect_length(prevcarga_registry(), 0L)
  expect_identical(pc_globalenv_snapshot(), snap)
  expect_s3_class(prevcarga_plugin_load("toy-30", pc_toy_root()), "prevcarga_plugin")
})

test_that("ac-10 matriz descriptor x run.yaml; erros de run.yaml vem antes de qualquer fonte", {
  reset_all()
  ly <- function(...) load_toy("toy-30", run_yaml = list(...))$load()
  expect_plugin_error(ly(modes = c("train", "infer", "forecast")), "plugin.bad_run_yaml", "modes")
  expect_plugin_error(ly(granularity_min = 60L), "plugin.bad_run_yaml", "granularity_min")
  expect_plugin_error(ly(seed = NULL), "plugin.bad_run_yaml", "seed")
  expect_plugin_error(ly(params = list(k = 3L, k2 = 1L)), "plugin.bad_run_yaml", "params$k2")
  expect_plugin_error(ly(params = list(k = NULL)), "plugin.bad_run_yaml", "params$k")
  expect_plugin_error(ly(params = list(k = 0L)), "plugin.bad_run_yaml", "params$k")
  expect_plugin_error(ly(cut_invariant = TRUE), "plugin.unknown_key", "cut_invariant")
  expect_plugin_error(ly(protocol = "daily"), "plugin.unknown_key", "protocol")
  expect_plugin_error(ly(env = list(a = 1)), "plugin.unknown_key", "env")
  ## run.yaml invalido vence codigo de topo (nada do plugin foi fonteado)
  both <- load_toy("toy-30", run_yaml = list(seed = NULL), files = list("R/x.R" = "x <- 1"))
  expect_plugin_error(both$load(), "plugin.bad_run_yaml", "seed")
  expect_length(prevcarga_registry(), 0L)
  ## guarda de janela: segue o descriptor, nunca o nome
  win <- list(history_days = list(train = 30L, infer = 20L))
  t60 <- function(flag) load_toy("toy-60", run_yaml = win, descriptor = list(history_infer_equals_train = flag))
  expect_plugin_error(t60(TRUE)$load(), "plugin.history_window", "history_days.infer")
  p <- t60(FALSE)$load()
  expect_identical(p$spec$history_days, list(train = 30L, infer = 20L))
  expect_s3_class(prevcarga_plugin_load("toy-60", pc_toy_root()), "prevcarga_plugin")
  expect_s3_class(prevcarga_plugin_load("toy-30", pc_toy_root()), "prevcarga_plugin")
  ## origin_scoped sem forecast em modes
  expect_plugin_error(load_toy("toy-60", run_yaml = list(modes = c("train", "infer")))$load(),
                      "plugin.bad_run_yaml", "modes")
})

test_that("ac-11 pacotes ausentes => bundle (exit 6) listando todos, sem tocar search()", {
  reset_all()
  s0 <- search()
  v <- load_toy("toy-30", descriptor = list(packages = c("pcNaoExiste1", "data.table", "pcNaoExiste2")))
  cond <- expect_plugin_error(v$load(), "plugin.package_missing", "pcNaoExiste1", class = "bundle", exit = 6L)
  expect_true(grepl("pcNaoExiste2", conditionMessage(cond), fixed = TRUE))
  expect_false(grepl("data.table", conditionMessage(cond), fixed = TRUE))
  expect_identical(search(), s0)
  expect_length(prevcarga_registry(), 0L)
  ok <- load_toy("toy-30", descriptor = list(packages = "data.table"), files = list("R/dt.R" =
    "toy_dt <- function() data.table::data.table(a = 1:2)"))
  p <- ok$load()
  expect_equal(nrow(p$env$toy_dt()), 2L)
  expect_identical(search(), s0)
})

test_that("ac-12 motor por instancia: estado isolado, funcao faltante, ordem e motor ausente", {
  reset_all()
  snap <- pc_globalenv_snapshot()
  root <- pc_toy_variant("toy-60")
  rb <- pc_toy_variant("toy-60", name = "toy-60b")
  file.copy(file.path(rb, "toy-60b"), root, recursive = TRUE)
  p1 <- prevcarga_plugin_env("toy-60", root); p2 <- prevcarga_plugin_env("toy-60b", root)
  e1 <- parent.env(p1); e2 <- parent.env(p2)
  expect_false(identical(e1, e2))
  expect_identical(e1$engine_count(), 0)
  e1$engine_fit(c(1, 2)); e1$engine_fit(c(1, 2))
  expect_identical(e1$engine_count(), 2)
  expect_identical(e2$engine_count(), 0)
  e2$engine_fit(c(3, 4))
  expect_identical(c(e1$engine_count(), e2$engine_count()), c(2, 1))
  expect_false(any(c("engine_fit", "engine_predict", "engine_manifest") %in% ls(globalenv())))
  expect_identical(pc_globalenv_snapshot(), snap)
  ## CONTROL: instancia por plugin; funcao faltante, ordem trocada, motor ausente
  reset_all()
  nof <- load_toy("toy-60", engine_files = list("b.R" = "engine_other <- function() 1"))
  expect_plugin_error(nof$load(), "plugin.engine_function_missing", "engine_predict")
  swp <- load_toy("toy-60", engine = list(files = c("b.R", "a.R")))
  expect_plugin_error(swp$load(), "plugin.engine_function_missing", "b.R")
  none <- load_toy("toy-60", descriptor = list(engine = "_nao-existe"))
  expect_plugin_error(none$load(), "plugin.engine_missing", "_nao-existe")
  expect_length(prevcarga_registry(), 0L)
  expect_s3_class(prevcarga_plugin_load("toy-60", pc_toy_root()), "prevcarga_plugin")
})

test_that("FIX 7: weather_resample obrigatorio (hourly|raw) com 60 min, recusado so com 30 min (registry.weather_resample)", {
  reset_all()
  expect_identical(prevcarga_plugin_descriptor("toy-60", pc_toy_root())$weather_resample, "hourly")
  expect_null(prevcarga_plugin_descriptor("toy-30", pc_toy_root())$weather_resample)
  expect_identical(load_toy("toy-60", descriptor = list(weather_resample = "raw"))$load()$descriptor$weather_resample, "raw")
  wr <- function(base, ...) expect_plugin_error(load_toy(base, descriptor = list(...))$load(),
                                                "registry.weather_resample", "weather_resample")
  wr("toy-60", weather_resample = NULL)                       # 60 min sem a declaracao
  wr("toy-60", weather_resample = "half")                     # valor fora de hourly|raw
  wr("toy-60", weather_resample = c("hourly", "raw"))
  wr("toy-30", weather_resample = "hourly")                   # so 30 min: recusado
  ## CONTROL: 30 e 60 suportados exige a declaracao (o toy-60 declara)
  ok <- load_toy("toy-60", descriptor = list(granularity_supported = c(30L, 60L)))$load()
  expect_identical(ok$descriptor$weather_resample, "hourly")
  cat <- prevcarga_errors_load(file.path(pc_root(), "scripts", "lib", "errors"))
  expect_identical(cat$class[cat$id == "registry.weather_resample"], "contract")
})

test_that("ac-13 defaults unicos e validacao do descriptor", {
  reset_all()
  d <- prevcarga_plugin_descriptor("toy-30", pc_toy_root())
  expect_identical(d$data_lag_days, 0L); expect_identical(d$model_time_offset_min, 0L)
  expect_false(d$reads_day_O); expect_false(d$weather_forecast_in_train); expect_false(d$uses_rng)
  expect_false(d$history_infer_equals_train); expect_null(d$rng_deviation); expect_null(d$engine)
  expect_identical(prevcarga_plugin_descriptor("toy-60", pc_toy_root())$model_time_offset_min, 60L)
  expect_identical(load_toy("toy-30", descriptor = list(data_lag_days = 2L))$load()$descriptor$data_lag_days, 2L)
  expect_identical(load_toy("toy-30", descriptor = list(model_time_offset_min = 60L))$load()$descriptor$model_time_offset_min, 60L)
  bad <- function(field, ...) expect_plugin_error(load_toy("toy-30", descriptor = list(...))$load(),
                                                  "plugin.bad_descriptor", field)
  bad("data_lag_days", data_lag_days = -1L); bad("data_lag_days", data_lag_days = 1.5)
  bad("model_time_offset_min", model_time_offset_min = -60L); bad("model_time_offset_min", model_time_offset_min = 1.5)
  bad("model_time_offset_min", model_time_offset_min = "60")
  bad("reads_day_O", reads_day_O = "sim"); bad("delivery_days", delivery_days = 0)
  bad("rng_deviation", rng_deviation = 42)
  bad("rng_deviation", rng_deviation = "texto", uses_rng = FALSE)
  bad("native_layout", native_layout = "foo")
  bad("semihourly_rule", granularity_supported = 60L)
  ok <- load_toy("toy-30", descriptor = list(uses_rng = TRUE, rng_deviation = "dev"))$load()
  expect_identical(ok$descriptor$rng_deviation, "dev")
  expect_plugin_error(load_toy("toy-30", descriptor = list(foo = 1))$load(), "plugin.unknown_key", "descriptor$foo")
  bs <- load_toy("toy-30", descriptor = function(d) { d$stages$training$fn <- function(x, y) 1; d })
  expect_plugin_error(bs$load(), "plugin.bad_signature", "training")
  pf <- load_toy("toy-30", descriptor = function(d) { d$stages$inference$fn <- function(payload, inf_features, cuts, params) 1; d })
  expect_plugin_error(pf$load(), "plugin.params_formal", "inference")
  expect_s3_class(prevcarga_plugin_load("toy-30", pc_toy_root()), "prevcarga_plugin")
})

test_that("ac-14 cache por root, unload e higiene de registry.R (CONTROL: padrao plantado)", {
  reset_all()
  r1 <- pc_toy_variant("toy-30"); r2 <- pc_toy_variant("toy-30")
  p1 <- prevcarga_plugin_load("toy-30", r1); p2 <- prevcarga_plugin_load("toy-30", r2)
  expect_false(identical(p1$env, p2$env))
  expect_length(ls(.prevcarga_plugin_cache, all.names = TRUE), 2L)
  expect_identical(prevcarga_plugin_load("toy-30", r1)$env, p1$env)
  prevcarga_plugin_unload_all()
  expect_length(ls(.prevcarga_plugin_cache, all.names = TRUE), 0L)
  expect_false(identical(prevcarga_plugin_load("toy-30", r1)$env, p1$env))
  forb <- c("(?<![A-Za-z_.])source\\(", "library\\(", "attach\\(", "Sys\\.getenv", "<<-", "switch\\(model",
            "grepl\\(\"dessem", "%in% c\\(\"lgbm")
  scan <- function(file) {
    ln <- readLines(file, warn = FALSE); ln <- ln[!grepl("^\\s*#", ln)]
    unlist(lapply(forb, function(p) which(grepl(p, ln, perl = TRUE))))
  }
  expect_length(scan(file.path(pc_root(), "scripts", "lib", "registry.R")), 0L)
  planted <- tempfile(fileext = ".R")
  writeLines(c("f <- function(model) {", "  switch(model, a = 1)", "}"), planted)
  expect_identical(scan(planted), 2L)
})
