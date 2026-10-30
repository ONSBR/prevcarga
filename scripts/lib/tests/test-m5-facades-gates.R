## test-m5-facades-gates.R -- pack de sub-gates [M5] pelas fachadas (M5.8): estrutura, matriz de bracos por campo, verificacao
## cruzada (prevcarga_gate_pack_m5_assert) e a substituicao do slot [DEFERRED M5] por pack descoberto (plugin_gates.R).
## O cenario pesado (treino/infer dos 5 membros) roda nos runners models/<m>/tests/run_all_gates.R --tier fast; aqui so o que e barato.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
.m5_pack <- file.path(pc_root(), "scripts", "tools", "gates", "m5_facades.R")
.m5_env <- local({ e <- new.env(parent = baseenv()); sys.source(.m5_pack, envir = e); e })

test_that("pack M5: descoberto pelo nome do arquivo, 6 gates, dono M5, applies_to = os 5 membros, tier fast, sem skip", {
  expect_true(is.function(.m5_env$prevcarga_gate_pack_m5) && is.function(.m5_env$prevcarga_gate_pack_m5_facades))
  gd <- dirname(.m5_pack)
  d <- .m5_env$prevcarga_gate_pack_m5("lgbm", list(gates_dir = gd), "fast")
  expect_identical(d$gate, c("m5_determinism", "m5_split_honesty", "m5_intraday", "m5_weather_reach", "m5_day0_infer", "m5_transparency"))
  expect_true(all(d$owner == "M5") && all(d$tier_min == "fast") && all(vapply(d$test, is.function, NA)))
  for (a in d$applies_to) expect_setequal(a, c("random-forest", "lgbm", "holt-winters", "dessem-linear", "dessem-regdin"))
  ## o runner so ve a funcao com o nome do arquivo; o pack nao e standalone (nao chama quit)
  expect_false(any(grepl("quit\\(", readLines(.m5_pack))))
})

test_that("pack M5: nenhum switch por nome de modelo nem variavel de ambiente (bracos por campo)", {
  code <- readLines(.m5_pack); code <- code[!grepl("^\\s*##", code)]
  code <- code[!grepl("^PC5G_MODELS <-", code)]
  expect_false(any(grepl("random-forest|lgbm|holt-winters|dessem-linear|dessem-regdin", code)))
  expect_false(any(grepl("Sys\\.getenv|Sys\\.setenv|Sys\\.time|Sys\\.Date", code)))
})

test_that("matriz de bracos esperados vem de spec/descriptor (RF, LGBM, HW, dessem)", {
  ex <- .m5_env$prevcarga_gate_pack_m5_expected
  rf <- ex(list(split = "origin_agnostic", modes = c("train", "infer")), list(uses_rng = TRUE, weather_forecast_in_train = TRUE, reads_day_O = FALSE), 0L)
  expect_identical(rf$m5_determinism, c("FORBIDDEN", "CONTROL"))
  expect_identical(rf$m5_split_honesty, c("FORBIDDEN", "CONTROL"))
  expect_identical(rf$m5_day0_infer, "NOT_APPLICABLE")
  lg <- ex(list(split = "origin_agnostic", modes = c("train", "infer")), list(weather_forecast_in_train = TRUE, reads_day_O = TRUE), 2L)
  expect_identical(lg$m5_determinism, "FORBIDDEN")
  expect_true("FORBIDDEN_LAG" %in% lg$m5_split_honesty)
  expect_identical(lg$m5_day0_infer, "NOT_APPLICABLE")                        # sem modo forecast
  hw <- ex(list(split = "origin_scoped", modes = c("train", "infer", "forecast")), list(uses_rng = TRUE, reads_day_O = FALSE), 0L)
  expect_true("DECKS" %in% hw$m5_split_honesty)
  expect_identical(hw$m5_weather_reach, "NOT_APPLICABLE"); expect_identical(hw$m5_day0_infer, "NOT_APPLICABLE")
  dl <- ex(list(split = "origin_scoped", modes = c("train", "infer", "forecast")),
           list(uses_rng = TRUE, weather_forecast_in_train = TRUE, reads_day_O = TRUE), 0L)
  expect_identical(dl$m5_day0_infer, c("MORNING", "FORBIDDEN"))
  expect_identical(dl$m5_weather_reach, c("FORBIDDEN_AU", "CONTROL_AU", "FORBIDDEN_MID", "CONTROL_MID"))
})

test_that("verificacao cruzada: braco ausente, braco falhando e SKIP derrubam; tabela completa passa", {
  ex <- list(m5_determinism = c("FORBIDDEN", "CONTROL"), m5_intraday = "FORBIDDEN")
  row <- function(g, a, ok = TRUE) data.frame(gate = g, arm = a, ok = ok, delta = 0, detail = "x", stringsAsFactors = FALSE)
  full <- rbind(row("m5_determinism", "FORBIDDEN"), row("m5_determinism", "CONTROL"), row("m5_intraday", "FORBIDDEN"))
  expect_true(.m5_env$prevcarga_gate_pack_m5_assert(full, "x", ex))
  expect_error(.m5_env$prevcarga_gate_pack_m5_assert(full[-2L, ], "x", ex), "ausente.*CONTROL")             # CONTROL esperado falta
  expect_error(.m5_env$prevcarga_gate_pack_m5_assert(rbind(full[-3L, ], row("m5_intraday", "FORBIDDEN", FALSE)), "x", ex), "FORBIDDEN")
  expect_error(.m5_env$prevcarga_gate_pack_m5_assert(rbind(full, row("m5_intraday", "SKIP")), "x", ex), "SKIP")
  expect_error(.m5_env$prevcarga_gate_pack_m5_assert(full[0L, ], "x", ex, gate = "m5_intraday"), "ausente")
})

## ---- substituicao do slot DEFERRED por pack descoberto (plugin_gates.R) -----------------------------------------------------
.m5_toy_runner <- function(root, with_pack) {
  tdir <- file.path(root, "toy-m5", "tests", "testthat"); dir.create(tdir, recursive = TRUE)
  gd <- file.path(root, "gates"); dir.create(gd, showWarnings = FALSE)
  writeLines(c('test_that("[PLUGINS] contrato: ok", { expect_true(TRUE) })',
               'test_that("[DEFERRED M5] fachada: slot do M5", { pc_gate_deferred("M5", "fachada nasce no M5") })',
               'test_that("[DEFERRED M6] schema_long: slot do M6", { pc_gate_deferred("M6", "LONG nasce no M6") })'),
             file.path(tdir, "test-slots.R"))
  if (with_pack)
    writeLines(c('prevcarga_gate_pack_m5x <- function(model, ctx, tier) {',
                 '  d <- data.frame(gate = c("m5_a", "m5_b"), owner = "M5", tier_min = "fast", stringsAsFactors = FALSE)',
                 '  d$applies_to <- list("toy-m5", "toy-m5"); d$test <- list(function() invisible(TRUE), function() invisible(TRUE)); d }'),
               file.path(gd, "m5x.R"))
  gt <- 'gate_table <- data.frame(gate = c("contrato", "fachada", "schema_long"), owner = c("PLUGINS", "DEFERRED M5", "DEFERRED M6"), tier = "fast", stringsAsFactors = FALSE)'
  writeLines(c(sprintf("source(\"%s\")", file.path(pc_root(), "scripts", "tools", "plugin_gates.R")), gt,
               sprintf("prevcarga_gates_run(gate_table, tier = \"fast\", model = \"toy-m5\", gates_dir = \"%s\")", gd)),
             file.path(root, "toy-m5", "tests", "run_all_gates.R"))
  file.path(root, "toy-m5", "tests", "run_all_gates.R")
}

test_that("pack descoberto de dono M5 substitui o slot [DEFERRED M5]; M6 segue SKIP com dono; sem o pack o slot volta (CONTROL)", {
  run <- function(r) pc_with_env(c(TZ = "America/Sao_Paulo"), pc_rscript(c(r, "--tier", "fast")))
  with <- run(.m5_toy_runner(withr::local_tempdir(), TRUE))
  expect_identical(with$status, 0L)
  expect_length(grep("DEFERRED M5", with$stdout), 0L)                       # nenhum slot M5 sobra
  expect_true(any(grepl("^m5_a +\\| \\[M5\\] +\\| PASS", with$stdout)) && any(grepl("^m5_b +\\| \\[M5\\] +\\| PASS", with$stdout)))
  expect_true(any(grepl("^schema_long +\\| \\[DEFERRED M6\\] +\\| SKIP", with$stdout)))
  without <- run(.m5_toy_runner(withr::local_tempdir(), FALSE))             # CONTROL: sem o pack o slot M5 reaparece como SKIP
  expect_true(any(grepl("^fachada +\\| \\[DEFERRED M5\\] +\\| SKIP", without$stdout)))
})

test_that("a substituicao nao esconde duplicata real: gate de pack igual a gate PLUGINS da tabela segue abortando", {
  root <- withr::local_tempdir()
  r <- .m5_toy_runner(root, TRUE)
  writeLines(sub('c("m5_a", "m5_b")', 'c("contrato", "m5_b")', readLines(file.path(root, "gates", "m5x.R")), fixed = TRUE),
             file.path(root, "gates", "m5x.R"))
  o <- pc_with_env(c(TZ = "America/Sao_Paulo"), pc_rscript(c(r, "--tier", "fast")))
  expect_identical(o$status, 1L)
  expect_match(paste(o$stderr, collapse = "\n"), "duplicado")
})
