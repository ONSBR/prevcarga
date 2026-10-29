## scripts/tools/gates/m1.R -- sub-gate do M1 (dono M1.V), descoberto por barrel (scripts/tools/gates/<dono>.R).
## Roda os gates de PROCESSO e de FONTE do M1 (suite scripts/lib/tests): cli-process, ascii-bytes, static-greps.
## Uso: Rscript scripts/tools/gates/m1.R   (exit 0 = verde). Nao le variaveis de ambiente.
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- dirname(dirname(dirname(dirname(normalizePath(f, winslash = "/")))))
ok <- TRUE
for (flt in c("cli-process", "ascii-bytes", "static-greps")) {
  res <- as.data.frame(testthat::test_dir(file.path(root, "scripts", "lib", "tests"), filter = flt,
                                          stop_on_failure = FALSE, reporter = "summary"))
  bad <- sum(res$failed) + sum(res$error)
  writeLines(sprintf("gate m1 [%s]: %s (falhas=%d, skips=%d)", flt, if (bad == 0L) "OK" else "FALHOU", bad, sum(res$skipped)))
  ok <- ok && bad == 0L
}
quit(status = if (ok) 0L else 1L, save = "no")
