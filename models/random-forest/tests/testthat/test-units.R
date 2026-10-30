## test-units.R -- valida o tests/parity/units.yaml do random-forest contra o leitor de units do plugin_golden.R (P.0f/RF.4).
## Movido da suite da fundacao (higiene P.0j ac-1: teste da fundacao nao le <repo>/models real); dono = o proprio plugin.
suppressWarnings(suppressMessages(library(testthat)))
env <- new.env(parent = globalenv())
for (m in c("errors", "runtime")) sys.source(file.path(rf_root(), "scripts", "lib", paste0(m, ".R")), envir = env)
sys.source(file.path(rf_root(), "scripts", "tools", "plugin_golden.R"), envir = env)

RF_UNITS <- file.path(rf_root(), "models", "random-forest", "tests", "parity", "units.yaml")
bad_id <- function(expr) tryCatch({ force(expr); NA_character_ }, prevcarga_error = function(e) paste(e$id, e$where))
mutate_units <- function(from, to, all = FALSE) {
  l <- readLines(RF_UNITS, warn = FALSE)
  i <- grep(from, l, fixed = TRUE)
  stopifnot(length(i) >= 1L)
  l[if (all) i else i[[1L]]] <- sub(from, to, l[if (all) i else i[[1L]]], fixed = TRUE)
  f <- tempfile("units-", fileext = ".yaml"); writeLines(l, f); f
}

test_that("[PLUGINS] units_yaml: units.yaml do random-forest com grupos e cortes por fase validos (RF.4)", {
  s <- env$pc_golden_units(RF_UNITS)
  expect_identical(lengths(s$groups)[c("smoke", "foundation", "walk_forward")], c(smoke = 3L, foundation = 15L, walk_forward = 31L))
  for (u in s$units) {
    O <- u$origin
    expect_identical(u$cuts$train$train_origin, format(O - 10L), info = u$id)
    expect_identical(u$cuts$train$history_start, format(O - 375L), info = u$id)
    expect_identical(u$cuts$infer$history_start, format(O - 45L), info = u$id)
    expect_identical(u$day0_source, "oracle_origin_minus_1"); expect_null(u$oracle_cut); expect_identical(length(u$oracle_env), 0L)
    expect_identical(u$seed, 123L); expect_identical(u$params$ntree, 100L); expect_identical(u$data_areas, u$area)
    expect_identical(lubridate::minute(u$available_until), 0L)
    expect_identical(u$weather_issue_cutoff, "origin_midnight_utc")
  }
  expect_identical(anyDuplicated(vapply(s$units, function(u) u$id, "")), 0L)
  expect_setequal(s$groups$smoke, c("seco-2024-10-01", "rs-2024-07-01", "baoe-2024-01-01"))
})

test_that("[PLUGINS] units_yaml: CONTROL oracle_cut, day0_source native, id duplicado, corte errado, env, hora quebrada => tools.bad_units nomeando unidade.campo", {
  expect_identical(bad_id(env$pc_golden_units(mutate_units("    day0_source: oracle_origin_minus_1", "    day0_source: native"))),
                   "tools.bad_units seco-2024-01-01.day0_source")
  expect_identical(bad_id(env$pc_golden_units(mutate_units("    seed: 123", "    seed: 123\n    oracle_cut: start_label"))),
                   "tools.bad_units seco-2024-01-01.oracle_cut")
  expect_identical(bad_id(env$pc_golden_units(mutate_units("  - id: sp-2024-01-01", "  - id: seco-2024-01-01"))),
                   "tools.bad_units seco-2024-01-01.id")
  expect_identical(bad_id(env$pc_golden_units(mutate_units("train_origin: 2023-12-22", "train_origin: 2023-12-23"))),
                   "tools.bad_units seco-2024-01-01.cuts.train.train_origin")
  expect_identical(bad_id(env$pc_golden_units(mutate_units("history_start: 2023-11-17", "history_start: 2023-11-18"))),
                   "tools.bad_units seco-2024-01-01.cuts.infer.history_start")
  expect_identical(bad_id(env$pc_golden_units(mutate_units("oracle_env: {}", "oracle_env: { X: \"1\" }"))),
                   "tools.bad_units seco-2024-01-01.oracle_env")
  expect_identical(bad_id(env$pc_golden_units(mutate_units("T09:00", "T09:30"))), "tools.bad_units seco-2024-01-01.available_until")
  expect_identical(bad_id(env$pc_golden_units(mutate_units("params: { ntree: 100 }", "params: { ntree: 99 }"))),
                   "tools.bad_units seco-2024-01-01.params")
  expect_match(bad_id(env$pc_golden_units(mutate_units("- seco-2024-10-02", "- seco-2024-10-99"))), "^tools.bad_units ")
})

