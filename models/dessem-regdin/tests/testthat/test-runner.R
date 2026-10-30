## test-runner.R -- CONTROL do runner de gates do dessem-regdin (RD.6): o MESMO tests/run_all_gates.R roda num repo-fixture (copia da
## fundacao + um teste trivial por linha da tabela) em PROCESSO FILHO (system2 com o Rscript do R corrente, portavel Linux/Windows).
## Cobre: tabela completa verde => exit 0, saida ASCII, DEFERRED como SKIP com dono, JSON unico no esquema do ADR-0040; gate
## [PLUGINS] falhando => exit 1; tier invalido => exit 2; os tiers parity/parity-full so rodam em paridade (N/A(tier) no fast).
## Sem variaveis de ambiente.
suppressWarnings(suppressMessages(library(testthat)))

rdr_runner_src <- function() file.path(rd_models(), "dessem-regdin", "tests", "run_all_gates.R")

## objetos de topo do runner (a tabela) avaliados sem executar o runner
rdr_gate_table <- function() {
  ex <- parse(rdr_runner_src(), keep.source = FALSE)
  env <- new.env(parent = baseenv())
  for (e in ex)
    if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]])[[1L]] %in% c(".rd_gates", "gate_table")) eval(e, env)
  env$gate_table
}

## repo-fixture: scripts/lib + plugin_gates.R + o runner REAL + um teste por linha da tabela (`failing` = gate que falha)
rdr_fixture_repo <- function(failing = NULL) {
  base <- withr::local_tempdir(.local_envir = parent.frame())
  lib <- file.path(base, "scripts", "lib"); dir.create(lib, recursive = TRUE)
  dir.create(file.path(base, "scripts", "tools", "gates"), recursive = TRUE)
  file.copy(list.files(file.path(rd_root(), "scripts", "lib"), pattern = "[.]R$", full.names = TRUE), lib)
  file.copy(file.path(rd_root(), "scripts", "lib", "errors"), lib, recursive = TRUE)
  file.copy(file.path(rd_root(), "scripts", "tools", "plugin_gates.R"), file.path(base, "scripts", "tools"))
  tdir <- file.path(base, "models", "dessem-regdin", "tests"); dir.create(file.path(tdir, "testthat"), recursive = TRUE)
  file.copy(rdr_runner_src(), tdir)
  gt <- rdr_gate_table()
  tests <- vapply(seq_len(nrow(gt)), function(i) {
    lab <- sprintf("[%s] %s: fixture", gt$owner[[i]], gt$gate[[i]])
    body <- if (grepl("^DEFERRED", gt$owner[[i]])) sprintf("testthat::skip(\"[%s] fixture\")", gt$owner[[i]])
            else if (identical(gt$gate[[i]], failing)) "testthat::expect_true(FALSE)" else "testthat::expect_true(TRUE)"
    sprintf("test_that(%s, { %s })", encodeString(lab, quote = "\""), body)
  }, "")
  writeLines(tests, file.path(tdir, "testthat", "test-fixture.R"))
  file.path(tdir, "run_all_gates.R")
}

rdr_run <- function(runner, ...) {
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner), ...), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  list(status = if (is.null(st)) 0L else as.integer(st), out = out)
}

test_that("[PLUGINS] runner_controle: tabela do ticket (gates do oraculo), DEFERRED com dono, tiers parity so em paridade; rotulos dos testes == tabela", {
  gt <- rdr_gate_table()
  expect_false(anyDuplicated(gt$gate) > 0L)
  need <- c("contract_synthetic", "contract_e2e", "flag12_yearend", "nowcast_parcial", "split_nowcast_med", "split_nowcast_max",
            "escala_mw", "exogena_variante", "corte_clima", "units_yaml", "runner_controle")
  expect_true(all(need %in% gt$gate[gt$owner == "PLUGINS"]))
  expect_identical(gt$owner[match(c("schema_long", "ytrue_real", "sem_crash_harness", "fachada", "cross_os"), gt$gate)],
                   c("DEFERRED M6", "DEFERRED M6", "DEFERRED M7", "DEFERRED M5", "DEFERRED M8"))
  expect_identical(gt$tier[match(c("paridade_smoke", "paridade_control", "mm_head_oracle", "requires_report"), gt$gate)], rep("parity", 4L))
  expect_identical(gt$tier[match(c("paridade_foundation", "paridade_fanout"), gt$gate)], rep("parity-full", 2L))
  ## o mapa cobre gate01..gate13 do oraculo (gate10/gate11 numa linha so)
  expect_setequal(unlist(strsplit(gt$oracle[grepl("^gate", gt$oracle)], ",")), sprintf("gate%02d", 1:13))
  ## cada teste rotulado do plugin tem linha na tabela e vice-versa (a fundacao confere; aqui o conjunto de rotulos)
  files <- list.files(testthat::test_path(), pattern = "^test-.*[.]R$", full.names = TRUE)
  lab <- unlist(lapply(files, function(f) {
    ln <- readLines(f, warn = FALSE)
    m <- regmatches(ln, regexec("^\\s*test_that\\((?:sprintf\\()?\"\\[(PLUGINS|DEFERRED M[0-9]+)\\] ([a-z0-9_]+):", ln, perl = TRUE))
    vapply(Filter(length, m), function(x) x[[3L]], "")
  }))
  expect_setequal(unique(lab), gt$gate)
  ## nenhum teste fica sem rotulo (a fundacao o trata como tools.gate_unowned)
  nolab <- unlist(lapply(files, function(f) {
    ln <- grep("^\\s*test_that\\(", readLines(f, warn = FALSE), value = TRUE)
    ln[!grepl("^\\s*test_that\\((?:sprintf\\()?\"\\[(PLUGINS|DEFERRED M[0-9]+)\\] [a-z0-9_]+:", ln, perl = TRUE)]
  }))
  expect_length(nolab, 0L)
})

test_that("[PLUGINS] runner_controle: tabela verde => exit 0, ASCII, DEFERRED como SKIP com dono, JSON unico; gate [PLUGINS] falhando => exit 1", {
  ok <- rdr_run(rdr_fixture_repo(), "--tier", "fast")
  expect_identical(ok$status, 0L)
  expect_true(all(utf8ToInt(paste(ok$out, collapse = "")) < 128L))
  expect_true(any(grepl("^schema_long +\\| \\[DEFERRED M6\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^ytrue_real +\\| \\[DEFERRED M6\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^sem_crash_harness +\\| \\[DEFERRED M7\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^flag12_yearend +\\| \\[PLUGINS\\] +\\| PASS", ok$out)))
  expect_true(any(grepl("^paridade_smoke +\\| \\[PLUGINS\\] +\\| N/A\\(tier\\)", ok$out)))
  expect_identical(sum(grepl("\\| SKIP", ok$out)), 5L)                    # so as 5 linhas DEFERRED saem como SKIP
  ## --format json: um unico documento JSON no esquema do ADR-0040
  js <- rdr_run(rdr_fixture_repo(), "--tier", "fast", "--format", "json")
  expect_identical(js$status, 0L)
  expect_length(js$out, 1L)
  parsed <- jsonlite::fromJSON(js$out, simplifyVector = FALSE)
  expect_true(parsed$ok); expect_identical(parsed$verb, "plugin-gates")
  ## CONTROL: um gate-fixture [PLUGINS] falhando derruba o runner (exit 1); DEFERRED nunca derruba
  bad <- rdr_run(rdr_fixture_repo(failing = "flag12_yearend"), "--tier", "fast")
  expect_identical(bad$status, 1L)
  expect_true(any(grepl("^flag12_yearend +\\| \\[PLUGINS\\] +\\| FAIL", bad$out)))
  badj <- rdr_run(rdr_fixture_repo(failing = "flag12_yearend"), "--tier", "fast", "--format", "json")
  expect_identical(badj$status, 1L)
  expect_false(jsonlite::fromJSON(badj$out, simplifyVector = FALSE)$ok)
  ## tier invalido => exit 2; tier parity-full roda a tabela inteira
  expect_identical(rdr_run(rdr_fixture_repo(), "--tier", "turbo")$status, 2L)
  expect_identical(rdr_run(rdr_fixture_repo(), "--tier", "parity-full")$status, 0L)
})
