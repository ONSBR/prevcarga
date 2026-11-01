## test-run-suite.R -- logica do runner da suite inteira (tests/run_suite.R; M8.12): veredito, lista de skips permitidos e CONTROLs.
## Nao roda a suite (isso e o proprio runner); prova que ele reprova falha, erro, skip fora da lista e gate com status != 0.

rs_env <- new.env(parent = globalenv())
withr::with_options(list(prevcarga.suite.no_main = TRUE), sys.source(file.path(bb_root(), "tests", "run_suite.R"), envir = rs_env))
mk <- function(...) {
  d <- data.frame(dir = "d", file = "f.R", test = "t", failed = 0L, errored = 0L, skip_reason = "", stringsAsFactors = FALSE)
  a <- list(...); for (n in names(a)) d[[n]] <- a[[n]]; d
}

test_that("[AC] run_suite: tudo verde e skips permitidos => ok; gates 0 => ok", {
  rows <- rbind(mk(), mk(skip_reason = "nao-windows"), mk(skip_reason = "x [TBD: ADR-0036] y"), mk(skip_reason = "TBD: ADR-0028 (metrica)"), mk(skip_reason = "[TBD] smoke"), mk(skip_reason = "parity-full so no fechamento"), mk(skip_reason = "deck-real-ausente"),
                mk(skip_reason = "nao-linux"), mk(skip_reason = "pwsh ausente [DEFERRED windows-vm do M8, dono: Gabriel]"))
  expect_true(rs_env$pc_suite_verdict(rows, c(`random-forest` = 0L))$ok)
})

test_that("[CONTROL] run_suite: falha, erro, skip sem motivo permitido, teste vazio e gate != 0 reprovam", {
  expect_false(rs_env$pc_suite_verdict(mk(failed = 1L))$ok)
  expect_false(rs_env$pc_suite_verdict(mk(errored = 1L))$ok)
  v <- rs_env$pc_suite_verdict(mk(skip_reason = "sem motivo conhecido"))
  expect_false(v$ok); expect_identical(v$skips_ruins$skip_reason, "sem motivo conhecido")
  expect_false(rs_env$pc_suite_verdict(mk(skip_reason = "Reason: empty test"))$ok)
  expect_false(rs_env$pc_suite_verdict(mk(skip_reason = "nao-windows || empty test"))$ok)
  g <- rs_env$pc_suite_verdict(mk(), c(lgbm = 0L, `holt-winters` = 1L))
  expect_false(g$ok); expect_identical(g$gates_ruins, "holt-winters")
})

test_that("[AC] run_suite: pc_suite_rows le falhas, erros e motivos de skip de um testthat_results real", {
  skip_if_not_installed("testthat")
  d <- withr::local_tempdir()
  writeLines(c("test_that('a', expect_true(TRUE))", "test_that('b', expect_true(FALSE))", "test_that('c', skip('nao-windows'))", "test_that('d', skip('outro motivo'))"), file.path(d, "test-x.R"))
  res <- testthat::test_dir(d, reporter = "silent", stop_on_failure = FALSE)
  r <- rs_env$pc_suite_rows(res, "x")
  expect_identical(r$failed, c(0L, 1L, 0L, 0L))
  expect_identical(sub("^Reason: ", "", r$skip_reason), c("", "", "nao-windows", "outro motivo"))
  expect_identical(rs_env$pc_suite_bad_skips(r)$test, "d")
})

test_that("[AC] run_suite: lista de membros vem de prevcarga_members (nunca fixa) e os diretorios sao os do ticket", {
  src <- readLines(file.path(bb_root(), "tests", "run_suite.R"))
  expect_true(any(grepl("prevcarga_members(root = root)$mdir", src, fixed = TRUE)))
  expect_false(any(grepl("random-forest|holt-winters|lgbm|dessem-", src[!grepl("^\\s*#", src)])))
  expect_identical(rs_env$PC_SUITE_DIRS, c("scripts/lib/tests", "tests/bundle", "tests/install", "tests/ab"))
})
