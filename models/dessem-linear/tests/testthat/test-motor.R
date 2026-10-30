## test-motor.R -- inclusao dos testes do motor DESSEM unico (models/_dessem-engine/tests/testthat) no gate do dessem-linear (DL.9).
## O motor nao e membro (prefixo `_`, sem run.yaml) e nao tem runner proprio: seus testes rodam aqui, em PROCESSO FILHO (o
## test_dir do motor traz o proprio helper-engine.R e as proprias funcoes, que nao podem colidir com as do plugin). O filho roda no
## tier fast (as linhas de paridade do motor saem como `[tier]`; o golden do motor e do DL.4). Falha ou erro de qualquer teste do
## motor derruba este gate.

test_that("[PLUGINS] motor_incluso: os testes de models/_dessem-engine/tests/testthat passam (processo filho, tier fast)", {
  eng <- file.path(dl_models(), "_dessem-engine", "tests", "testthat")
  expect_true(dir.exists(eng))
  expr <- sprintf(paste0("options(prevcarga.tier = 'fast'); r <- as.data.frame(testthat::test_dir(%s, reporter = 'silent', ",
                         "stop_on_failure = FALSE, stop_on_warning = FALSE)); ",
                         "cat(sprintf('ENGINE n=%%d failed=%%d errors=%%d', nrow(r), sum(r$failed), sum(r$error)), '\\n')"),
                  encodeString(eng, quote = "\""))
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", "-e", shQuote(expr)), stdout = TRUE, stderr = TRUE))
  line <- grep("^ENGINE n=", out, value = TRUE)
  expect_length(line, 1L)
  v <- as.integer(regmatches(line, regexec("n=([0-9]+) failed=([0-9]+) errors=([0-9]+)", line))[[1L]][-1L])
  expect_gt(v[[1L]], 20L)                                   # o motor tem dezenas de testes (nao rodou nada => reprova)
  expect_identical(v[[2L]], 0L); expect_identical(v[[3L]], 0L)
})
