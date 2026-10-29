## helper-prevcarga.R -- helper testthat compartilhado (auto-carregado por test_dir). Nao e codigo de produto.

pc_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("pc_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}

pc_source_lib <- function(mods, env = new.env(parent = globalenv())) {
  for (m in mods) sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = env)
  env
}

## LONG montado em memoria como o leitor unico o entrega: `interval_start` anexado pela borda do LONG (schema_long.R).
pc_long_mem <- function(long, tz = NULL)
  pc_source_lib(c("errors", "schema_long"))$prevcarga_long_attach_interval_start(long, tz)

## Seta/restaura variaveis do processo de TESTE (NA = remover) para lancar filhos sujos; portavel
## Linux/Windows (system2(env=) nao funciona no Windows). `code` e avaliado com as variaveis ativas.
pc_with_env <- function(vars, code) {
  old <- Sys.getenv(names(vars), unset = NA)
  on.exit({
    for (n in names(old)) {
      if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(old[[n]]), n))
    }
  }, add = TRUE)
  for (n in names(vars)) {
    if (is.na(vars[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(vars[[n]]), n))
  }
  force(code)
}

pc_rscript_bin <- function() file.path(R.home("bin"), "Rscript")

## Roda `Rscript --vanilla <args>` (script ou vetor de args); stdout/stderr separados, em linhas.
pc_rscript <- function(script_or_args) {
  a <- if (length(script_or_args) == 1L && file.exists(script_or_args)) script_or_args else script_or_args
  tf1 <- tempfile("pc-out"); tf2 <- tempfile("pc-err")
  on.exit(unlink(c(tf1, tf2)), add = TRUE)
  st <- suppressWarnings(system2(pc_rscript_bin(), c("--vanilla", shQuote(a)), stdout = tf1, stderr = tf2))
  list(status = as.integer(st), stdout = readLines(tf1, warn = FALSE), stderr = readLines(tf2, warn = FALSE))
}

pc_run_cli <- function(args = character()) {
  tf1 <- tempfile("pc-out"); tf2 <- tempfile("pc-err")
  on.exit(unlink(c(tf1, tf2)), add = TRUE)
  cli <- file.path(pc_root(), "scripts", "prevcarga")
  st <- suppressWarnings(system2(pc_rscript_bin(), c("--vanilla", shQuote(cli), args), stdout = tf1, stderr = tf2))
  list(status = as.integer(st), stdout = readLines(tf1, warn = FALSE), stderr = readLines(tf2, warn = FALSE))
}

## Script-sonda (tempdir): sonda determinismo; `enforce` liga a guarda; `info` imprime found/effective/check.
pc_probe_script <- function(enforce = TRUE, info = FALSE) {
  lib <- file.path(pc_root(), "scripts", "lib")
  f <- tempfile("probe", fileext = ".R")
  writeLines(c(
    sprintf("options(prevcarga.lib = '%s')", lib),
    sprintf("source('%s'); source('%s')", file.path(lib, "errors.R"), file.path(lib, "runtime.R")),
    if (enforce) "r <- prevcarga_runtime_enforce()",
    "writeLines(paste(lubridate::hour(.POSIXct(0))))",
    "writeLines(paste(sort(c('b','A','a','B','_1')), collapse = ' '))",
    "writeLines(paste(format(crossprod(matrix(1:9/7, 3)), digits = 15), collapse = ' '))",
    "ck <- prevcarga_runtime_check()",
    "writeLines(paste('ok', ck$ok))",
    if (info) c(
      "if (exists('r')) { writeLines(paste('found.tz', r$found$tz)); writeLines(paste('found.omp', r$found$omp)); writeLines(paste('found.openblas', r$found$openblas))",
      "  writeLines(paste('eff.tz', r$effective$tz)); writeLines(paste('eff.pins', paste(unlist(r$effective[c('omp','openblas','mkl','veclib')]), collapse = ' '))); writeLines(paste('eff.lc', r$effective$lc)) }")
  ), f)
  f
}
