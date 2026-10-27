## test-runtime.R -- runtime.R (M1.1): FORBIDDEN processo sujo == limpo; CONTROL sem enforce diverge.
library(testthat)
local({
  e <- pc_source_lib(c("errors", "runtime"))
  for (n in ls(e, all.names = TRUE)) assign(n, get(n, e), envir = globalenv())
})
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

dirty <- c(TZ = "America/Sao_Paulo", LC_ALL = "pt_BR.UTF-8", OMP_NUM_THREADS = "4", OPENBLAS_NUM_THREADS = "4")
clean <- c(TZ = NA, LC_ALL = NA, LANG = NA, OMP_NUM_THREADS = NA, OPENBLAS_NUM_THREADS = NA,
           MKL_NUM_THREADS = NA, VECLIB_MAXIMUM_THREADS = NA)

test_that("[FORBIDDEN] shell sujo == limpo apos enforce; found x effective", {
  p <- pc_probe_script(enforce = TRUE, info = TRUE)
  lim <- pc_with_env(clean, pc_rscript(p))
  suj <- pc_with_env(dirty, pc_rscript(p))
  expect_identical(lim$status, 0L)
  expect_identical(suj$status, 0L)
  probe <- function(x) x[1:4]
  expect_identical(probe(suj$stdout), probe(lim$stdout))
  expect_identical(suj$stdout[1], "0")
  expect_true("ok TRUE" %in% suj$stdout)
  expect_true("eff.tz UTC" %in% suj$stdout)
  expect_true("eff.pins 1 1 1 1" %in% suj$stdout)
  expect_true("found.tz America/Sao_Paulo" %in% suj$stdout)
  expect_true("found.omp 4" %in% suj$stdout)
  expect_true("found.openblas 4" %in% suj$stdout)
  expect_true(any(grepl("^eff.lc .*(UTF-8|utf8)", suj$stdout, ignore.case = TRUE)))
})

test_that("[CONTROL] sem enforce o processo sujo diverge (hora 21 vs 0); braco TZ nunca skipado", {
  p_on <- pc_probe_script(enforce = TRUE)
  p_off <- pc_probe_script(enforce = FALSE)
  lim <- pc_with_env(clean, pc_rscript(p_on))$stdout
  ctl <- pc_with_env(dirty, pc_rscript(p_off))$stdout
  expect_false(identical(ctl[1], lim[1]))
  expect_identical(ctl[1], "21")
  expect_identical(lim[1], "0")
})

good_setters <- function() list(
  tz = function() "UTC", locale = function() "C.UTF-8",
  pins = function() c(omp = "1", openblas = "1", mkl = "1", veclib = "1"), blas = function() 1L)
abort_cond <- function(setters) tryCatch(prevcarga_runtime_enforce(setters), prevcarga_error = identity)

test_that("[CONTROL] setter fake por item falha com runtime.* e exit 7", {
  s <- good_setters(); s$tz <- function() "America/Sao_Paulo"
  c1 <- abort_cond(s)
  expect_s3_class(c1, "prevcarga_error")
  expect_identical(c1$id, "runtime.tz_unfixable")
  expect_match(conditionMessage(c1), "^\\[prevcarga/runtime\\]")
  expect_identical(as.integer(c1$exit_code), 7L)
  s <- good_setters(); s$locale <- function() NA_character_
  c2 <- abort_cond(s)
  expect_identical(c2$id, "runtime.locale_unavailable")
  expect_identical(as.integer(c2$exit_code), 7L)
  s <- good_setters(); s$blas <- function() 4L
  c3 <- abort_cond(s)
  expect_identical(c3$id, "runtime.blas_unfixable")
  expect_match(conditionMessage(c3), "^\\[prevcarga/runtime\\]")
  expect_identical(as.integer(c3$exit_code), 7L)
  expect_false(is.null(c3$details$found))
})

test_that("[POSITIVE] setter real nao falha e devolve invisivel list(found, effective)", {
  old_lc <- vapply(c("LC_CTYPE", "LC_COLLATE", "LC_NUMERIC"), Sys.getlocale, "")
  on.exit(for (n in names(old_lc)) suppressWarnings(Sys.setlocale(n, old_lc[[n]])), add = TRUE)
  vars <- c(TZ = NA, OMP_NUM_THREADS = NA, OPENBLAS_NUM_THREADS = NA, MKL_NUM_THREADS = NA, VECLIB_MAXIMUM_THREADS = NA)
  pc_with_env(vars, {
    expect_no_error(v <- withVisible(prevcarga_runtime_enforce(prevcarga_runtime_setters())))
    expect_false(v$visible)
    expect_identical(v$value$effective$tz, "UTC")
    expect_identical(v$value$effective$blas_threads, 1L)
    expect_true(all(unlist(v$value$effective[c("omp", "openblas", "mkl", "veclib")]) == "1"))
    st <- prevcarga_runtime_stamp()
    expect_identical(st$tz, "UTC")
    expect_identical(st$lc, v$value$effective$lc)
    expect_identical(st$blas_threads, 1L)
    expect_true(prevcarga_runtime_check()$ok)
  })
})

test_that("BLAS real: 1 thread com RhpcBLASctl (POSITIVE)", {
  skip_if_not_installed("RhpcBLASctl")
  expect_identical(prevcarga_runtime_setters()$blas(), 1L)
})

test_that("check/apply nao fatais; check nao altera o ambiente", {
  script <- tempfile("chk", fileext = ".R")
  lib <- file.path(pc_root(), "scripts", "lib")
  writeLines(c(
    sprintf("source('%s'); source('%s')", file.path(lib, "errors.R"), file.path(lib, "runtime.R")),
    "r <- prevcarga_runtime_check()",
    "writeLines(paste('ok', r$ok))",
    "writeLines(paste('off', paste(r$offenders, collapse = ' ')))",
    "writeLines(paste('env', Sys.getenv('TZ'), Sys.getenv('OMP_NUM_THREADS')))",
    "writeLines(paste('found', r$found$tz, r$found$omp))"), script)
  res <- pc_with_env(c(TZ = "America/Sao_Paulo", OMP_NUM_THREADS = "2"), pc_rscript(script))
  expect_identical(res$status, 0L)
  expect_true("ok FALSE" %in% res$stdout)
  off <- strsplit(sub("^off ", "", grep("^off", res$stdout, value = TRUE)), " ")[[1]]
  expect_true(all(c("OMP_NUM_THREADS", "TZ") %in% off))
  expect_true("env America/Sao_Paulo 2" %in% res$stdout)
  expect_true("found America/Sao_Paulo 2" %in% res$stdout)

  s <- good_setters(); s$tz <- function() stop("falha simulada")
  expect_no_error(r <- prevcarga_runtime_apply(strict = FALSE, setters = s))
  expect_false(r$ok)
  expect_true("TZ" %in% r$offenders)
  old <- c(Sys.getlocale("LC_COLLATE"), Sys.getlocale("LC_NUMERIC"))
  on.exit({ Sys.setlocale("LC_COLLATE", old[1]); Sys.setlocale("LC_NUMERIC", old[2]) }, add = TRUE)
  Sys.setlocale("LC_COLLATE", "C"); Sys.setlocale("LC_NUMERIC", "C")
  expect_no_error(r2 <- prevcarga_runtime_apply(strict = FALSE, setters = good_setters()))
  expect_true(r2$ok)
})

test_that("stamp antes de enforce/apply e falha interna", {
  rm(list = ls(.prevcarga_runtime_state), envir = .prevcarga_runtime_state)
  c1 <- tryCatch(prevcarga_runtime_stamp(), prevcarga_error = identity)
  expect_identical(c1$id, "generic.unexpected")
})

test_that("install_root: espaco, barra invertida e ausente", {
  base <- file.path(tempfile("inst"), "inst dir")
  dir.create(file.path(base, "scripts", "lib"), recursive = TRUE)
  file.create(file.path(base, "scripts", "lib", "runtime.R"))
  want <- normalizePath(base, winslash = "/")
  a <- paste0("--file=", base, "/scripts/prevcarga")
  expect_identical(prevcarga_install_root(c("R", "--no-echo", a)), want)
  b <- paste0("--file=", gsub("/", "\\", base, fixed = TRUE), "\\scripts\\prevcarga")
  expect_identical(prevcarga_install_root(c("R", b)), want)
  cnd <- tryCatch(prevcarga_install_root(c("R", "--no-echo")), prevcarga_error = identity)
  expect_identical(cnd$id, "runtime.install_root_unresolved")
  expect_match(conditionMessage(cnd), "^\\[prevcarga/runtime\\]")
  expect_identical(as.integer(cnd$exit_code), 7L)
})

test_that("unico Sys.getenv de runtime.R esta em .prevcarga_runtime_found", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "runtime.R"), warn = FALSE)
  code <- src[!grepl("^\\s*##", src)]
  hit <- grep("Sys\\.getenv", code)
  expect_length(hit, 1L)
  expect_match(code[hit], "v <- Sys.getenv")
})
