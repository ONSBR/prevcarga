## scripts/tools/gates/m2.R -- sub-gate do M2 (dono M2.V), descoberto por barrel (scripts/tools/gates/<dono>.R).
## Roda os gates de PROCESSO e de FONTE do M2 (suite scripts/lib/tests): validate-deck-cli, deck-matrix, static-greps-m2.
## Uso: Rscript scripts/tools/gates/m2.R   (exit 0 = verde). Nao le variaveis de ambiente.
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- dirname(dirname(dirname(dirname(normalizePath(f, winslash = "/")))))
ok <- TRUE
for (flt in c("validate-deck-cli", "deck-matrix", "static-greps-m2")) {
  res <- as.data.frame(testthat::test_dir(file.path(root, "scripts", "lib", "tests"), filter = flt,
                                          stop_on_failure = FALSE, reporter = "summary"))
  bad <- sum(res$failed) + sum(res$error)
  writeLines(sprintf("gate m2 [%s]: %s (falhas=%d, skips=%d)", flt, if (bad == 0L) "OK" else "FALHOU", bad, sum(res$skipped)))
  ok <- ok && bad == 0L
}
quit(status = if (ok) 0L else 1L, save = "no")
