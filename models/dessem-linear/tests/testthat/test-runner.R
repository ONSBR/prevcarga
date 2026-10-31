## test-runner.R -- CONTROL do runner de gates do dessem-linear (DL.9): o MESMO tests/run_all_gates.R roda num repo-fixture (copia da
## fundacao + um teste trivial por linha da tabela) em PROCESSO FILHO (system2 com o Rscript do R corrente, portavel Linux/Windows).
## Cobre: tabela completa verde => exit 0, saida ASCII, DEFERRED como SKIP com dono, JSON unico; gate [PLUGINS] falhando => exit 1;
## `--units fanout` sem relatorio verde e integro => exit 1 apontando o relatorio; com relatorio verde segue. Sem variaveis de ambiente.
suppressWarnings(suppressMessages(library(testthat)))

dlr_runner_src <- function() file.path(dl_models(), "dessem-linear", "tests", "run_all_gates.R")

## objetos de topo do runner (tabela e funcoes do relatorio) avaliados sem executar o runner
dlr_runner_env <- function() {
  ex <- parse(dlr_runner_src(), keep.source = FALSE)
  env <- new.env(parent = baseenv())
  for (e in ex)
    if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]])[[1L]] %in%
        c(".dl_gates", "gate_table", ".dl_sha", ".dl_report_units_sha", ".dl_report_problem")) eval(e, env)
  env
}
dlr_gate_table <- function() dlr_runner_env()$gate_table

## repo-fixture: scripts/lib + plugin_gates.R + o runner REAL + um teste por linha da tabela (`failing` = gate que falha)
dlr_fixture_repo <- function(failing = NULL, report = NULL) {
  base <- withr::local_tempdir(.local_envir = parent.frame())
  lib <- file.path(base, "scripts", "lib"); dir.create(lib, recursive = TRUE)
  dir.create(file.path(base, "scripts", "tools", "gates"), recursive = TRUE)
  file.copy(list.files(file.path(dl_root(), "scripts", "lib"), pattern = "[.]R$", full.names = TRUE), lib)
  file.copy(file.path(dl_root(), "scripts", "lib", "errors"), lib, recursive = TRUE)
  file.copy(file.path(dl_root(), "scripts", "tools", "plugin_gates.R"), file.path(base, "scripts", "tools"))
  tdir <- file.path(base, "models", "dessem-linear", "tests"); dir.create(file.path(tdir, "testthat"), recursive = TRUE)
  dir.create(file.path(tdir, "parity"))
  file.copy(dlr_runner_src(), tdir)
  gt <- dlr_gate_table()
  tests <- vapply(seq_len(nrow(gt)), function(i) {
    lab <- sprintf("[%s] %s: fixture", gt$owner[[i]], gt$gate[[i]])
    body <- if (grepl("^DEFERRED", gt$owner[[i]])) sprintf("testthat::skip(\"[%s] fixture\")", gt$owner[[i]])
            else if (identical(gt$gate[[i]], failing)) "testthat::expect_true(FALSE)" else "testthat::expect_true(TRUE)"
    sprintf("test_that(%s, { %s })", encodeString(lab, quote = "\""), body)
  }, "")
  writeLines(tests, file.path(tdir, "testthat", "test-fixture.R"))
  if (!is.null(report)) {
    writeLines(jsonlite::toJSON(list(units = list(`seco-2024-10-01` = list(files = list(native.rds = strrep("a", 64L))),
                                                   `seco-2024-10-08` = list(files = list(native.rds = strrep("b", 64L))))), auto_unbox = TRUE),
               file.path(tdir, "parity", "golden.lock"))
    writeLines(jsonlite::toJSON(report(file.path(tdir, "parity", "golden.lock")), auto_unbox = TRUE, null = "null", digits = 15, pretty = TRUE),
               file.path(tdir, "parity", "foundation_report.json"))
  }
  file.path(tdir, "run_all_gates.R")
}

dlr_run <- function(runner, ...) {
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner), ...), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  list(status = if (is.null(st)) 0L else as.integer(st), out = out)
}

## relatorio sintetico no esquema do test-parity.R (so os campos que o runner confere); `mutate` altera depois do sha
dlr_report <- function(ok = TRUE, bad_sha = FALSE, other_lock = FALSE) {
  rn <- dlr_runner_env()
  function(lock) {
    units <- list(list(unit = "seco-2024-10-01", ok = ok, max_abs_delta = 0, golden_native_sha256 = if (other_lock) strrep("0", 64L) else strrep("a", 64L)),
                  list(unit = "seco-2024-10-08", ok = TRUE, max_abs_delta = 0, golden_native_sha256 = strrep("b", 64L)))
    list(model = "dessem-linear", all_ok = ok,
         units = units, sha256 = if (bad_sha) strrep("0", 64L) else rn$.dl_report_units_sha(units))
  }
}

test_that("[PLUGINS] runner_controle: tabela do ticket (gate01..gate18 do oraculo), DEFERRED com dono, tiers parity so em paridade", {
  gt <- dlr_gate_table()
  expect_false(anyDuplicated(gt$gate) > 0L)
  need <- c("gate02_escala_mw", "gate04_exogena_params", "gate05_clima_honesto", "gate07_dataprev_o", "gate08_contrato", "gate09_dado_faltante",
            "gate10_modelo_vazio", "gate11_clamp_cross", "gate13_flag12", "gate14_corte_intradiario", "gate15_sem_crash_13h",
            "gate16_cauda_curta", "gate17_split", "gate18_sem_treino_circular", "motor_incluso", "runner_controle")
  expect_true(all(need %in% gt$gate[gt$owner == "PLUGINS"]))
  expect_identical(gt$owner[match(c("gate01_schema_long", "gate06_ytrue_real", "gate03_sem_crash", "fachada", "cross_os"), gt$gate)],
                   c("DEFERRED M6", "DEFERRED M6", "DEFERRED M7", "DEFERRED M5", "DEFERRED M8"))
  expect_identical(gt$tier[match(c("paridade", "paridade_control"), gt$gate)], rep("parity", 2L))
  ## o mapa cobre os gate01..gate18 do oraculo (o gate12 nao existe no oraculo)
  expect_setequal(gt$oracle[grepl("^gate", gt$oracle)], sprintf("gate%02d", c(1:11, 13:18)))
  ## cada teste rotulado do plugin tem linha na tabela e vice-versa (a fundacao confere; aqui o conjunto de rotulos)
  files <- list.files(testthat::test_path(), pattern = "^test-.*[.]R$", full.names = TRUE)
  lab <- unlist(lapply(files, function(f) {
    ln <- readLines(f, warn = FALSE)
    m <- regmatches(ln, regexec("^\\s*test_that\\((?:sprintf\\()?\"\\[(PLUGINS|DEFERRED M[0-9]+)\\] ([a-z0-9_]+):", ln, perl = TRUE))
    vapply(Filter(length, m), function(x) x[[3L]], "")
  }))
  expect_setequal(unique(lab), gt$gate)
})

test_that("[PLUGINS] runner_controle: tabela verde => exit 0, ASCII, DEFERRED M6/M7 como SKIP com dono, JSON unico; gate falhando => exit 1", {
  ok <- dlr_run(dlr_fixture_repo(), "--tier", "fast")
  expect_identical(ok$status, 0L)
  expect_true(all(utf8ToInt(paste(ok$out, collapse = "")) < 128L))
  expect_true(any(grepl("^gate01_schema_long +\\| \\[DEFERRED M6\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^gate06_ytrue_real +\\| \\[DEFERRED M6\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^gate03_sem_crash +\\| \\[DEFERRED M7\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^gate14_corte_intradiario +\\| \\[PLUGINS\\] +\\| PASS", ok$out)))
  expect_true(any(grepl("^paridade +\\| \\[PLUGINS\\] +\\| N/A\\(tier\\)", ok$out)))
  ## nenhum outro skip: so as 5 linhas DEFERRED saem como SKIP
  expect_identical(sum(grepl("\\| SKIP", ok$out)), 5L)
  ## --format json: um unico documento JSON
  js <- dlr_run(dlr_fixture_repo(), "--tier", "fast", "--format", "json")
  expect_identical(js$status, 0L)
  expect_length(js$out, 1L)
  parsed <- jsonlite::fromJSON(js$out, simplifyVector = FALSE)
  expect_true(parsed$ok); expect_identical(parsed$verb, "plugin-gates")
  ## CONTROL: um gate-fixture [PLUGINS] falhando derruba o runner (exit 1); DEFERRED nunca derruba
  bad <- dlr_run(dlr_fixture_repo(failing = "gate14_corte_intradiario"), "--tier", "fast")
  expect_identical(bad$status, 1L)
  expect_true(any(grepl("^gate14_corte_intradiario +\\| \\[PLUGINS\\] +\\| FAIL", bad$out)))
  badj <- dlr_run(dlr_fixture_repo(failing = "gate14_corte_intradiario"), "--tier", "fast", "--format", "json")
  expect_identical(badj$status, 1L)
  expect_false(jsonlite::fromJSON(badj$out, simplifyVector = FALSE)$ok)
  ## tier invalido => exit 2
  expect_identical(dlr_run(dlr_fixture_repo(), "--tier", "turbo")$status, 2L)
})

test_that("[PLUGINS] runner_controle: --units fanout recusa sem relatorio verde e integro (exit 1, mensagem ASCII); com relatorio verde segue", {
  rn <- dlr_runner_env()
  ## sem relatorio, vermelho, sha adulterado e golden (por unidade) de outra geracao do golden.lock: exit 1 apontando o relatorio
  cases <- list(ausente = NULL, vermelho = dlr_report(ok = FALSE), sha = dlr_report(bad_sha = TRUE), lock = dlr_report(other_lock = TRUE))
  for (nm in names(cases)) {
    r <- dlr_run(dlr_fixture_repo(report = cases[[nm]]), "--tier", "parity-full", "--units", "fanout")
    expect_identical(r$status, 1L, info = nm)
    expect_true(any(grepl("foundation_report.json", r$out, fixed = TRUE)), info = nm)
    expect_true(all(utf8ToInt(paste(r$out, collapse = "")) < 128L), info = nm)
    j <- dlr_run(dlr_fixture_repo(report = cases[[nm]]), "--tier", "parity-full", "--units", "fanout", "--format", "json")
    expect_identical(j$status, 1L, info = nm)
    expect_false(jsonlite::fromJSON(paste(j$out, collapse = "\n"), simplifyVector = FALSE)$ok, info = nm)
  }
  ## relatorio verde e integro: o runner segue (os testes do fixture passam)
  go <- dlr_run(dlr_fixture_repo(report = dlr_report()), "--tier", "parity-full", "--units", "fanout")
  expect_identical(go$status, 0L)
  ## outros grupos nao exigem o relatorio
  expect_identical(dlr_run(dlr_fixture_repo(), "--tier", "parity-full", "--units", "foundation")$status, 0L)
  ## o relatorio REAL versionado, quando existe, e integro para o runner (o mesmo sha que o test-parity.R grava)
  real <- file.path(dl_models(), "dessem-linear", "tests", "parity", "foundation_report.json")
  if (file.exists(real)) {
    why <- rn$.dl_report_problem(real, file.path(dirname(real), "golden.lock"))
    expect_true(is.null(why) || grepl("vermelho", why, fixed = TRUE), info = why)
  }
})
