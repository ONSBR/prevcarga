## test-variant-requires.R -- necessidade por variante no descriptor (E5-T1): descriptor$variants, helper puro
## prevcarga_plugin_variant_requires, corte de clima da inferencia e conferencia de colunas pelo requires efetivo.
## Descriptor e painel sinteticos; sem store, sem variavel de ambiente; datas por lubridate com tz explicito.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "topology", "deck_reader", "validation", "registry", "features", "inference"),
              env = environment())

O <- lubridate::ymd("2026-01-16", tz = "Etc/GMT+3")
TOK_ING <- c("frame.deck", "data.load.load_mwh", "data.weather_observed.temperature",
             "data.weather_forecast.temperature", "data.holidays.prevcarga_code")
TOK_UNI <- c("data.load.load_mwh", "data.holidays.prevcarga_code")

stage_fn <- function(stage) {
  nm <- prevcarga_plugin_signatures[[stage]]
  as.function(c(stats::setNames(rep(list(quote(expr = )), length(nm)), nm), quote(invisible(NULL))))
}
fx_raw <- function(variants = NULL) {
  st <- lapply(prevcarga_plugin_stages, function(s)
    list(fn = stage_fn(s), requires = if (s == "ingestion") TOK_ING else "frame.deck", provides = paste0("p_", s)))
  names(st) <- prevcarga_plugin_stages
  d <- list(model_id = "fx", contract_version = 1L, granularity_supported = 30L, modes_implemented = c("train", "infer"),
            params_schema = list(exogena = list(type = "character", allowed = c("UNI", "MED")),
                                 clima_hist = list(type = "integer")),
            variant_param = "exogena", native_layout = "lgbm_day_48", delivery_days = 9L,
            freeze = stage_fn("freeze"), thaw = stage_fn("thaw"), stages = st, weather_cut = "absent")
  if (!is.null(variants)) d$variants <- variants
  d
}
fx_vars <- list(UNI = list(requires = TOK_UNI, ignores_params = "clima_hist"))
fx_descriptor <- function(variants = fx_vars) prevcarga_plugin_normalize_descriptor_for_test(fx_raw(variants))
prevcarga_plugin_normalize_descriptor_for_test <- function(d) .prevcarga_plugin_normalize_descriptor(d, "fx")

expect_bad <- function(expr, reason) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (is.null(cond)) return(invisible(NULL))
  expect_identical(cond$id, "plugin.bad_descriptor")
  expect_identical(cond$exit_code, 2L)
  expect_true(grepl(reason, conditionMessage(cond), fixed = TRUE), info = conditionMessage(cond))
}

test_that("[ac-0] helper: UNI troca so os tokens data.*; MED e NA devolvem o requires do estagio; estagio sem data.* inalterado", {
  d <- fx_descriptor()
  expect_identical(prevcarga_plugin_variant_requires(d, "ingestion", "UNI"), c("frame.deck", TOK_UNI))
  expect_identical(prevcarga_plugin_variant_requires(d, "ingestion", "MED"), TOK_ING)
  expect_identical(prevcarga_plugin_variant_requires(d, "ingestion", NA_character_), TOK_ING)
  expect_identical(prevcarga_plugin_variant_requires(d, "ingestion", NULL), TOK_ING)
  for (v in list("UNI", "MED", NA_character_))
    expect_identical(prevcarga_plugin_variant_requires(d, "feature_engineering", v), "frame.deck")
  ## descriptor sem variants: identico em qualquer variante
  d0 <- fx_descriptor(variants = list())
  expect_identical(prevcarga_plugin_variant_requires(d0, "ingestion", "UNI"), TOK_ING)
})

test_that("[ac-1] variants invalido => plugin.bad_descriptor com o campo; sem variants normaliza igual", {
  expect_bad(fx_descriptor(list(XXX = list(requires = TOK_UNI))), "variants$XXX")
  expect_bad(fx_descriptor(list(UNI = list(requires = "weather.temperature"))), "variants$UNI$requires")
  expect_bad(fx_descriptor(list(UNI = list(ignores_params = "nao_existe"))), "variants$UNI$ignores_params")
  expect_bad(fx_descriptor(list(UNI = list(outro = "x"))), "variants$UNI")
  raw <- fx_raw(fx_vars); raw$variant_param <- NULL; raw$params_schema <- list()
  expect_bad(.prevcarga_plugin_normalize_descriptor(raw, "fx"), "variants")
  raw <- fx_raw(NULL)                       # mesmas closures nos dois lados
  d0 <- .prevcarga_plugin_normalize_descriptor(raw, "fx")
  raw$variants <- fx_vars
  d1 <- .prevcarga_plugin_normalize_descriptor(raw, "fx")
  expect_identical(d0$variants, list())
  expect_identical(d1$variants, fx_vars)
  expect_identical(d1[setdiff(names(d1), "variants")], d0[setdiff(names(d0), "variants")])
  expect_identical(utils::tail(names(d1), 2L), c("variants", "key_params"))   # variants (E5-T1) e key_params (E5-T4) fecham o descriptor
  expect_identical(fx_descriptor(NULL)$variants, list())
})

test_that("[ac-2] corte de clima: variante sem clima nao e cortada; com clima aborta (CONTROL); clima vazio idem", {
  plg <- list(descriptor = fx_descriptor())
  mk <- function(until) list(weather_forecast_until = data.table::data.table(area_code = "SP", until = until),
                             weather_forecast_next = data.table::data.table(area_code = "SP", n_next = 0L))
  for (attrs in list(mk(O + 3L), mk(as.Date(NA)))) {
    cu <- withCallingHandlers(
      .prevcarga_inf_apply_weather_cut(plg, "fx", "SP", O, attrs, list(), variant = "UNI"),
      prevcarga_warning = function(w) fail(paste("warning inesperado", w$id)))
    expect_identical(cu$delivery_days, 9L)
    for (v in list("MED", NA_character_)) {
      e <- tryCatch(.prevcarga_inf_apply_weather_cut(plg, "fx", "SP", O, attrs, list(), variant = v),
                    prevcarga_error = function(e) e)
      expect_identical(e$id, "completude.weather_forecast_short")
    }
  }
  attrs_vazio <- list(weather_forecast_until = data.table::data.table(area_code = character(), until = as.Date(character())))
  expect_identical(.prevcarga_inf_apply_weather_cut(plg, "fx", "SP", O, attrs_vazio, list(), variant = "UNI")$delivery_days, 9L)
  e <- tryCatch(.prevcarga_inf_apply_weather_cut(plg, "fx", "SP", O, attrs_vazio, list(), variant = "MED"),
                prevcarga_error = function(e) e)
  expect_identical(e$id, "completude.weather_forecast_short")
})

test_that("[ac-3] conferencia de colunas pelo requires efetivo: sem clima passa; com clima aborta plugin.missing_column", {
  d <- fx_descriptor()
  mkdf <- function(...) data.frame(...)
  panel <- list(load = mkdf(load_mwh = 1), weather_observed = mkdf(valid_time = 1), weather_forecast = mkdf(valid_time = 1),
                holidays = mkdf(prevcarga_code = 1))
  st <- c("ingestion", "feature_engineering", "training")
  expect_true(.prevcarga_plugin_require_data_cols(panel, d, st, "UNI"))
  e <- tryCatch(.prevcarga_plugin_require_data_cols(panel, d, st, "MED"), prevcarga_error = function(e) e)
  expect_identical(e$id, "plugin.missing_column")
  expect_identical(e$details$token, "data.weather_observed.temperature")
  e <- tryCatch(.prevcarga_plugin_require_data_cols(panel, d, st), prevcarga_error = function(e) e)
  expect_identical(e$id, "plugin.missing_column")
  ## o mesmo para a conferencia do painel de infer (so a parte de colunas: o painel falha antes de qualquer outra coisa)
  expect_true(.prevcarga_plugin_require_data_cols(panel, d, c("ingestion", "inference_features", "inference"), "UNI"))
  ## variante do spec
  expect_identical(.prevcarga_plugin_spec_variant(d, list(params = list(exogena = "UNI"))), "UNI")
  expect_identical(.prevcarga_plugin_spec_variant(d, list(params = list())), NA_character_)
})

test_that("[ac-4] registry.R e inference.R sem literal de variante ou de modelo (linhas nao comentario)", {
  for (f in c("registry.R", "inference.R")) {
    ln <- readLines(file.path(pc_root(), "scripts", "lib", f), warn = FALSE)
    ln <- ln[!grepl("^\\s*#", ln)]
    for (lit in c("\"UNI\"", "\"MED\"", "\"MAX\"", "\"MXMN\"", "Univariado", "dessem-linear", "dessem-regdin"))
      expect_false(any(grepl(lit, ln, fixed = TRUE)), info = paste(f, lit))
  }
})
