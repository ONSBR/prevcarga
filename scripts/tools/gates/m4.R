## scripts/tools/gates/m4.R -- sub-gate do M4 (dono M4.V), descoberto por barrel (scripts/tools/gates/<dono>.R).
## Roda os gates da combina: G2/otimalidade/determinismo/sentinela/qualidade, invariantes, CLI por processo, greps de fonte e smoke.
## Uso: Rscript scripts/tools/gates/m4.R   (exit 0 = verde). Nao le variaveis de ambiente.
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- dirname(dirname(dirname(dirname(normalizePath(f, winslash = "/")))))
ok <- TRUE
for (flt in c("combination-gates", "combination-invariants", "combine-cli", "combination-greps", "combination-smoke")) {
  res <- as.data.frame(testthat::test_dir(file.path(root, "scripts", "lib", "tests"), filter = flt,
                                          stop_on_failure = FALSE, reporter = "summary"))
  bad <- sum(res$failed) + sum(res$error)
  writeLines(sprintf("gate m4 [%s]: %s (falhas=%d, skips=%d)", flt, if (bad == 0L) "OK" else "FALHOU", bad, sum(res$skipped)))
  ok <- ok && bad == 0L
}
quit(status = if (ok) 0L else 1L, save = "no")
