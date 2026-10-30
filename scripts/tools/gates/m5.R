## scripts/tools/gates/m5.R -- sub-gate do M5 (dono M5.9), descoberto por barrel (scripts/tools/gates/<dono>.R).
## Roda os gates da pureza do M5 (greps falsificaveis de features/training/inference/registry) e contrato JSON/ASCII dos verbos train-models, infer-model e registry.
## Uso: Rscript scripts/tools/gates/m5.R   (exit 0 = verde). Nao le variaveis de ambiente.
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- dirname(dirname(dirname(dirname(normalizePath(f, winslash = "/")))))
ok <- TRUE
for (flt in c("m5-purity", "m5-envelope")) {
  res <- as.data.frame(testthat::test_dir(file.path(root, "scripts", "lib", "tests"), filter = flt,
                                          stop_on_failure = FALSE, reporter = "summary"))
  bad <- sum(res$failed) + sum(res$error)
  writeLines(sprintf("gate m5 [%s]: %s (falhas=%d, skips=%d)", flt, if (bad == 0L) "OK" else "FALHOU", bad, sum(res$skipped)))
  ok <- ok && bad == 0L
}
quit(status = if (ok) 0L else 1L, save = "no")
