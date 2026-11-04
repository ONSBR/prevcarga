## test-install-etapas-linux.R -- linhas de etapa 'prevcarga: [n/6] ...' do install.sh: so em texto e com stderr num terminal.
## Com stderr redirecionado (testes, CI, scripts) ou em --format json: nenhuma etapa (o contrato de stderr do ADR-0040 continua).
## Bundle FALSO minimo (RUNTIME.json + Rscript stub que sai 0): nao precisa do candidato de dist/, nem de rede. CONTROL: sob um pty
## (`script`), o mesmo instalador imprime as 6 etapas -- o detector tem dentes.
library(testthat)
testthat::skip_on_os(c("windows", "mac"))
if (!all(nzchar(Sys.which(c("zstd", "tar", "sha256sum", "sh"))))) testthat::skip("zstd/tar/sha256sum ausentes no host")

fake_bundle <- function(env = parent.frame()) {
  d <- pci_work_dir(env, "pc-etapas-")
  top <- file.path(d, "src", "prevcarga")
  dir.create(file.path(top, "runtime", "bin"), recursive = TRUE)
  writeLines('{"files":[{"path":"runtime/bin/Rscript","sha256":"x"}],"version":"9.9.9"}', file.path(top, "RUNTIME.json"))
  writeLines(c("#!/bin/sh", "exit 0"), file.path(top, "runtime", "bin", "Rscript"))
  Sys.chmod(file.path(top, "runtime", "bin", "Rscript"), "0755")
  tb <- file.path(d, "prevcarga-9.9.9-linux-x86_64.tar.zst")
  pci_sh(sprintf("cd %s && tar --zstd -cf %s prevcarga", shQuote(file.path(d, "src")), shQuote(tb)))
  sha <- strsplit(pci_sh(sprintf("sha256sum %s", shQuote(tb))), " ", fixed = TRUE)[[1]][1]
  list(path = tb, sha256 = sha, root = file.path(d, "R"))
}
args_for <- function(b, fmt) c("--bundle", b$path, "--sha256", b$sha256, "--root", b$root, "--skip-platform-check", "--format", fmt)
step_lines <- function(x) grep("^prevcarga: \\[[0-9]/6\\] ", gsub("\r", "", unlist(strsplit(x, "\n", fixed = TRUE))), value = TRUE)

test_that("stderr redirecionado (texto e json): instala e NENHUMA linha 'prevcarga: [' em stderr", {
  for (fmt in c("text", "json")) {
    b <- fake_bundle()
    r <- pc_install(args_for(b, fmt))
    expect_identical(r$status, 0L, info = paste(fmt, r$stderr))
    expect_length(step_lines(r$stderr), 0L)
    expect_false(grepl("prevcarga: [", r$stderr, fixed = TRUE), info = fmt)
    expect_identical(readLines(file.path(b$root, "current"), warn = FALSE), "9.9.9")
  }
})

test_that("CONTROL: com stderr num pty (script) o texto mostra as 6 etapas em ordem; json continua sem etapa", {
  if (!nzchar(Sys.which("script"))) skip("util-linux script ausente: sem pty para o CONTROL")
  run_pty <- function(a) {
    cmd <- paste("env -i PATH=/usr/local/bin:/usr/bin:/bin sh", shQuote(pci_installer()), paste(vapply(a, shQuote, ""), collapse = " "))
    out <- suppressWarnings(system2("script", c("-qec", shQuote(cmd), "/dev/null"), stdout = TRUE, stderr = TRUE))
    paste(out, collapse = "\n")
  }
  b <- fake_bundle()
  st <- step_lines(run_pty(args_for(b, "text")))
  expect_identical(sub("^prevcarga: \\[([0-9])/6\\].*$", "\\1", st), as.character(1:6))
  expect_match(st[1], "bundle local prevcarga-9.9.9-linux-x86_64.tar.zst", fixed = TRUE)
  expect_match(st[4], "validando 1 arquivos do RUNTIME.json", fixed = TRUE)
  expect_match(st[5], "ativando a versao 9.9.9", fixed = TRUE)
  b2 <- fake_bundle()
  expect_length(step_lines(run_pty(args_for(b2, "json"))), 0L)
})
