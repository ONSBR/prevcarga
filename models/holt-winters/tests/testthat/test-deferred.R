## test-deferred.R -- SKIPs com dono que a tabela do runner exige, CONTROL do runner e aditividade do Holt-Winters (HW.7; AC 14,
## 15). Os [DEFERRED Mx] nascem SKIP ate o pack do dono existir (descoberto de scripts/tools/gates/*.R; este arquivo nunca e
## editado por M5/M6/M7).
if (!exists("pc_gate_deferred", mode = "function"))
  pc_gate_deferred <- function(owner, reason) testthat::skip(sprintf("[DEFERRED %s] %s", owner, reason))

test_that("[DEFERRED M6] schema_long: saida LONG (gate04 do oraculo)", {
  pc_gate_deferred("M6", "schema LONG do HW nasce no pack do M6 (build_long)")
})
test_that("[DEFERRED M6] ytrue_real: y_true real no LONG (gate07 do oraculo)", {
  pc_gate_deferred("M6", "y_true real nasce no pack do M6 (reconcile/LONG)")
})
test_that("[DEFERRED M7] harness_parity: backtest = producao (gate10 do oraculo)", {
  pc_gate_deferred("M7", "paridade do harness de backtest nasce no pack do M7")
})
test_that("[DEFERRED M5] fachada: treino/infer pela CLI prevcarga", {
  pc_gate_deferred("M5", "fachada da CLI nasce no pack do M5")
})

## ---- CONTROL do runner: repo-fixture com o MESMO run_all_gates.R do HW, um teste por linha da tabela -----------------------
hd_runner_src <- function() file.path(hw_root(), "models", "holt-winters", "tests", "run_all_gates.R")

hd_gate_table <- function() {
  ex <- parse(hd_runner_src(), keep.source = FALSE); env <- new.env(parent = baseenv())
  for (e in ex)
    if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]])[[1L]] %in% c(".hw_gates", "gate_table"))
      eval(e, env)
  if (!is.data.frame(env$gate_table)) stop("gate_table nao encontrada em run_all_gates.R")
  env$gate_table
}

hd_fixture_repo <- function(failing = NULL) {
  base <- withr::local_tempdir(.local_envir = parent.frame())
  lib <- file.path(base, "scripts", "lib"); dir.create(lib, recursive = TRUE)
  dir.create(file.path(base, "scripts", "tools", "gates"), recursive = TRUE)
  file.copy(list.files(file.path(hw_root(), "scripts", "lib"), pattern = "[.]R$", full.names = TRUE), lib)
  file.copy(file.path(hw_root(), "scripts", "lib", "errors"), lib, recursive = TRUE)
  file.copy(file.path(hw_root(), "scripts", "tools", "plugin_gates.R"), file.path(base, "scripts", "tools"))
  tdir <- file.path(base, "models", "holt-winters", "tests"); dir.create(file.path(tdir, "testthat"), recursive = TRUE)
  file.copy(hd_runner_src(), tdir)
  gt <- hd_gate_table()
  tests <- vapply(seq_len(nrow(gt)), function(i) {
    lab <- sprintf("[%s] %s: fixture", gt$owner[[i]], gt$gate[[i]])
    body <- if (grepl("^DEFERRED", gt$owner[[i]])) sprintf("testthat::skip(\"[%s] fixture\")", gt$owner[[i]])
            else if (identical(gt$gate[[i]], failing)) "testthat::expect_true(FALSE)" else "testthat::expect_true(TRUE)"
    sprintf("test_that(%s, { %s })", encodeString(lab, quote = "\""), body)
  }, "")
  writeLines(tests, file.path(tdir, "testthat", "test-fixture.R"))
  file.path(tdir, "run_all_gates.R")
}

hd_run <- function(runner, ...) {
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner), ...),
                                  stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  list(status = if (is.null(st)) 0L else as.integer(st), out = out)
}

test_that("[PLUGINS] runner_controle: fixture verde => exit 0 com tabela ASCII e JSON unico; gate [PLUGINS] falhando => exit 1", {
  gt <- hd_gate_table()
  expect_true(all(c("corte_treino", "contrato_entrada", "determinismo", "higiene_grep", "clima_honesto", "clima_fallback",
                    "schema_long", "ytrue_real", "harness_parity", "fachada", "portabilidade_cross_os") %in% gt$gate))
  expect_identical(gt$owner[match(c("schema_long", "ytrue_real", "harness_parity", "fachada", "portabilidade_cross_os"), gt$gate)],
                   c("DEFERRED M6", "DEFERRED M6", "DEFERRED M7", "DEFERRED M5", "DEFERRED M8"))
  ok <- hd_run(hd_fixture_repo(), "--tier", "fast")
  expect_identical(ok$status, 0L)
  expect_true(all(utf8ToInt(paste(ok$out, collapse = "")) < 128L))
  expect_true(any(grepl("^schema_long +\\| \\[DEFERRED M6\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^corte_treino +\\| \\[PLUGINS\\] +\\| PASS", ok$out)))
  js <- hd_run(hd_fixture_repo(), "--tier", "fast", "--format", "json")
  expect_identical(js$status, 0L)
  expect_length(js$out, 1L)
  env <- jsonlite::fromJSON(js$out, simplifyVector = FALSE)
  expect_true(env$ok); expect_identical(env$result$model, "holt-winters")
  ## CONTROL: um gate [PLUGINS] falhando derruba o runner (exit 1); DEFERRED nunca derruba
  bad <- hd_run(hd_fixture_repo(failing = "determinismo"), "--tier", "fast")
  expect_identical(bad$status, 1L)
  expect_true(any(grepl("^determinismo +\\| \\[PLUGINS\\] +\\| FAIL", bad$out)))
})

## ---- aditividade (AC 15): o epico so escreve em models/holt-winters/ ---------------------------------------------------------
## manifesto sha256 de models/ (exceto holt-winters); ordem radix; caminhos relativos com "/"
hd_manifest <- function(models_dir, exclude = "holt-winters") {
  fs <- list.files(models_dir, recursive = TRUE, all.files = TRUE, full.names = FALSE)
  fs <- fs[!(fs %in% exclude | startsWith(fs, paste0(exclude, "/")) | grepl("(^|/)models/", fs))]
  fs <- fs[order(fs, method = "radix")]
  vapply(fs, function(f) digest::digest(file.path(models_dir, f), algo = "sha256", file = TRUE), "")
}

hd_git <- function(args, dir = hw_root()) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(dir), args), stdout = TRUE, stderr = FALSE))
  st <- attr(out, "status"); list(out = out, status = if (is.null(st)) 0L else as.integer(st))
}

test_that("[PLUGINS] aditividade: manifesto de models/ exceto holt-winters estavel; golden e artefatos ignorados; oraculo intocado", {
  md <- file.path(hw_root(), "models")
  m1 <- hd_manifest(md); m2 <- hd_manifest(md)
  expect_gt(length(m1), 0L)
  expect_identical(m1, m2)
  ## CONTROL: o manifesto ignora o HW e apanha qualquer arquivo novo nos demais modelos (copia em tempdir)
  tmp <- file.path(withr::local_tempdir(), "models"); dir.create(tmp)
  for (d in setdiff(list.files(md), "holt-winters")) file.copy(file.path(md, d), tmp, recursive = TRUE)
  dir.create(file.path(tmp, "holt-winters")); writeLines("x", file.path(tmp, "holt-winters", "a.R"))
  expect_identical(hd_manifest(tmp), m1)
  writeLines("x", file.path(tmp, list.files(tmp)[[1L]], "novo.R"))
  expect_false(identical(hd_manifest(tmp), m1))

  skip_if(!nzchar(Sys.which("git")) || hd_git(c("rev-parse", "--git-dir"))$status != 0L, "git indisponivel")
  ## os commits do epico (assunto "HW.n:") so tocam models/holt-winters/ em models/; golden/artefatos sao gitignored
  commits <- hd_git(c("log", "--no-merges", "--format=%H", "--grep=^HW[.][0-9]"))$out
  for (cm in commits) {
    fl <- hd_git(c("diff-tree", "--no-commit-id", "--name-only", "-r", cm))$out
    mods <- fl[startsWith(fl, "models/")]
    expect_true(all(startsWith(mods, "models/holt-winters/")), info = paste(cm, paste(mods, collapse = ",")))
  }
  expect_identical(hd_git(c("check-ignore", "-q", ".state/golden/x"))$status, 0L)
  expect_identical(hd_git(c("check-ignore", "-q", "models/holt-winters/models/art.rds"))$status, 0L)
  tracked <- hd_git(c("ls-files", "models/holt-winters/models", ".state"))$out
  expect_length(tracked, 0L)
  ## oraculo de referencia: somente leitura
  orc <- "/opt/ons-source/prevcargadessem3"
  skip_if(!dir.exists(file.path(orc, ".git")), "oraculo ausente nesta maquina")
  ## os 3 arquivos/pasta nao rastreados de docs/roadmap/ ja existiam la (notas do plano, nao do HW): so eles sao toleraveis
  st <- hd_git(c("status", "--porcelain"), orc)$out
  expect_length(st[!grepl("^\\?\\? docs/roadmap/", st)], 0L)
})
