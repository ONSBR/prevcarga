## scripts/tools/gates/m3.R -- sub-gate do M3 (dono M3.V), descoberto por barrel (scripts/tools/gates/<dono>.R).
## Roda a bateria do honesty-gate, os gates de PROCESSO dos verbos score/check-coherence e os grep gates do M3.
## Uso: Rscript scripts/tools/gates/m3.R   (exit 0 = verde). Nao le variaveis de ambiente.
f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- dirname(dirname(dirname(dirname(normalizePath(f, winslash = "/")))))
ok <- TRUE
for (flt in c("honesty-matrix", "score-cli", "check-coherence-cli", "static-greps")) {
  res <- as.data.frame(testthat::test_dir(file.path(root, "scripts", "lib", "tests"), filter = flt,
                                          stop_on_failure = FALSE, reporter = "summary"))
  bad <- sum(res$failed) + sum(res$error)
  writeLines(sprintf("gate m3 [%s]: %s (falhas=%d, skips=%d)", flt, if (bad == 0L) "OK" else "FALHOU", bad, sum(res$skipped)))
  ok <- ok && bad == 0L
}
quit(status = if (ok) 0L else 1L, save = "no")
