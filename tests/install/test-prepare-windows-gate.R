## test-prepare-windows-gate.R -- scripts/tools/prepare_windows_gate.sh (M8.11): o pacote para a VM Windows fica COMPLETO, sem enviar nada, e os .ps1 sao ASCII.
## Roda no Linux com um zip FALSO (so o sha256 importa aqui) e uma entrega Linux de brinquedo. Tudo em sandbox/ (gitignored).
library(testthat)
script <- file.path(pci_root(), "scripts", "tools", "prepare_windows_gate.sh")
skip_if(!nzchar(Sys.which("bash")) || !nzchar(Sys.which("sha256sum")), "bash/sha256sum ausentes")

prep <- function(args) {
  so <- tempfile(); se <- tempfile(); on.exit(unlink(c(so, se)))
  st <- system2("env", c(paste0("PATH=", R.home("bin"), ":/usr/local/bin:/usr/bin:/bin"), "bash", shQuote(script), vapply(args, shQuote, "")), stdout = so, stderr = se)
  list(status = as.integer(st), out = readLines(so, warn = FALSE), err = readLines(se, warn = FALSE))
}
world <- function() {
  w <- pci_work_dir(parent.frame())
  zip <- file.path(w, "prevcarga-9.9.9-windows-x86_64.zip"); writeBin(as.raw(1:200), zip)
  lin <- file.path(w, "lin"); dir.create(lin); writeLines("{}", file.path(lin, "manifest.json")); writeLines("{\"origin\":\"2026-10-01\"}", file.path(lin, "run_manifest.json"))
  deck <- file.path(w, "deck"); dir.create(deck); writeLines("{}", file.path(deck, "manifest.json"))
  st <- file.path(w, "st"); dir.create(st); writeLines("x", file.path(st, "a.txt"))
  lb <- file.path(w, "prevcarga-9.9.9-linux-x86_64.tar.zst"); writeBin(as.raw(5:50), lb)
  list(w = w, zip = zip, lin = lin, deck = deck, st = st, lb = lb, sha = digest::digest(file = zip, algo = "sha256"))
}
EXPECTED <- c("gate/install/install.ps1", "gate/install/prevcarga.cmd", "gate/install/prevcarga-sync.ps1", "gate/install/error-catalog.tsv",
              "gate/install/tests/windows-vm.Tests.ps1", "gate/install/tests/windows-vm.Helpers.ps1", "gate/install/tests/windows-vm-cross.Tests.ps1",
              "gate/install/tests/install.Tests.ps1", "gate/install/tests/prevcarga-sync.Tests.ps1", "gate/install/tests/PSScriptAnalyzerSettings.psd1",
              "gate/tests/windows/Invoke-WindowsGate.ps1", "gate/tests/windows/write-receipt.R", "gate/tests/windows/control-perturb.R", "gate/tests/windows/release-check.R", "gate/scripts/tools/bundle_gates.R",
              "gate/scripts/tools/receipts.R", "gate/docs/reference/windows-gate.md", "bundle/prevcarga-9.9.9-windows-x86_64.zip",
              "bundle/prevcarga-9.9.9-windows-x86_64.zip.sha256", "ref/linux-x86_64/manifest.json", "ref/linux-x86_64/run_manifest.json", "ref/deck/manifest.json",
              "ref/state/a.txt", "ref/linux-bundle.sha256", "SHA256SUMS", "PACOTE.json")

test_that("[AC] o pacote fica completo (todos os arquivos que a VM exige), com SHA256SUMS valido, .ps1/.R/.cmd em ASCII e .cmd em CRLF", {
  W <- world(); out <- file.path(W$w, "pacote")
  r <- prep(c("--out", out, "--bundle", W$zip, "--sha256", W$sha, "--linux-output", W$lin, "--deck", W$deck, "--state", W$st, "--linux-bundle", W$lb))
  expect_identical(r$status, 0L, info = paste(r$err, collapse = "\n"))
  expect_length(setdiff(EXPECTED, list.files(out, recursive = TRUE, all.files = TRUE)), 0L)
  ## SHA256SUMS confere com o conteudo
  sums <- readLines(file.path(out, "SHA256SUMS")); expect_gt(length(sums), 20L)
  for (l in sums) { h <- substr(l, 1L, 64L); p <- substring(l, 67L); expect_identical(digest::digest(file = file.path(out, p), algo = "sha256"), h, info = p) }
  expect_false("SHA256SUMS" %in% sub("^.{66}", "", sums))
  ## ASCII: nenhum .ps1/.cmd/.R/.psd1/.tsv do pacote tem byte > 127
  fs <- list.files(file.path(out, "gate"), pattern = "\\.(ps1|cmd|R|psd1|tsv)$", recursive = TRUE, full.names = TRUE)
  expect_gt(length(fs), 10L)
  for (f in fs) expect_false(any(as.integer(readBin(f, "raw", file.size(f))) > 127L), info = f)
  cmd <- readBin(file.path(out, "gate", "install", "prevcarga.cmd"), "raw", 1e5)
  expect_true(grepl("\r\n", rawToChar(cmd), fixed = TRUE)); expect_false(grepl("[^\r]\n", rawToChar(cmd)))     # o pacote preserva os bytes (CRLF)
  ## o catalogo GERADO tem cabecalho e linhas; o bundle copiado == original
  cat1 <- readLines(file.path(out, "gate", "install", "error-catalog.tsv")); expect_gt(length(cat1), 10L)
  expect_identical(digest::digest(file = file.path(out, "bundle", basename(W$zip)), algo = "sha256"), W$sha)
  pj <- jsonlite::read_json(file.path(out, "PACOTE.json")); expect_identical(pj$bundle_sha256, W$sha); expect_true(pj$ref)
  expect_match(paste(r$out, collapse = "\n"), "Invoke-WindowsGate.ps1", fixed = TRUE)
})

test_that("[AC] sem upload: o script nao chama rede/aws e escreve so em --out (e no .zip ao lado com --zip)", {
  x <- paste(readLines(script, warn = FALSE), collapse = "\n")
  expect_false(grepl("\\baws\\b|\\bcurl\\b|\\bwget\\b|\\bscp\\b|\\brsync\\b|ssh ", gsub("#[^\n]*", "", x)))
  expect_false(grepl("getenv|printenv|\\$\\{?(HOME|USER|AWS_[A-Z_]*)\\}?", gsub("#[^\n]*", "", x)))
  expect_false(any(as.integer(readBin(script, "raw", file.size(script))) > 127L))
})

test_that("[CONTROL] entradas invalidas falham: sem --bundle (8), --out nao vazio (8), sha256 divergente (9), nome do bundle errado (8), entrega Linux sem manifest (8), pacote sem o roteiro (2)", {
  W <- world()
  expect_identical(prep(c("--out", file.path(W$w, "p1")))$status, 8L)
  full <- file.path(W$w, "p2"); dir.create(full); writeLines("x", file.path(full, "y"))
  expect_identical(prep(c("--out", full, "--bundle", W$zip))$status, 8L)
  expect_identical(prep(c("--out", file.path(W$w, "p3"), "--bundle", W$zip, "--sha256", strrep("0", 64L)))$status, 9L)
  bad <- file.path(W$w, "bundle.zip"); file.copy(W$zip, bad)
  expect_identical(prep(c("--out", file.path(W$w, "p4"), "--bundle", bad))$status, 8L)
  empty <- file.path(W$w, "vazia"); dir.create(empty)
  expect_identical(prep(c("--out", file.path(W$w, "p5"), "--bundle", W$zip, "--linux-output", empty, "--deck", W$deck, "--state", W$st))$status, 8L)
  expect_identical(prep(c("--out", file.path(W$w, "p6"), "--bundle", W$zip, "--linux-output", W$lin))$status, 8L)       # --linux-output sem --deck/--state
  r <- prep(c("--out", file.path(W$w, "p7"), "--bundle", W$zip, "--bogus"))
  expect_identical(r$status, 8L); expect_match(r$err[[1L]], "^prevcarga: erro \\[config\\] \\(exit 8\\)$")
  ## CONTROL do teste de completude: um arquivo removido do pacote e acusado pela lista esperada
  out <- file.path(W$w, "ok"); expect_identical(prep(c("--out", out, "--bundle", W$zip))$status, 0L)
  unlink(file.path(out, "gate", "docs", "reference", "windows-gate.md"))
  expect_true("gate/docs/reference/windows-gate.md" %in% setdiff(EXPECTED[1:17], list.files(out, recursive = TRUE, all.files = TRUE)))
})

test_that("[AC] --suite-tree (W16) copia a arvore versionada do HEAD com tests/run_suite.R e --test-lib copia o testlib; sem as opcoes o pacote nao tem repo/ nem testlib/", {
  skip_if(!nzchar(Sys.which("git")), "git ausente")
  W <- world(); out <- file.path(W$w, "pacote"); tl <- file.path(W$w, "tl"); dir.create(tl); writeLines("x", file.path(tl, "testthat.txt"))
  r <- prep(c("--out", out, "--bundle", W$zip, "--sha256", W$sha, "--suite-tree", "--test-lib", tl))
  expect_identical(r$status, 0L, info = paste(r$err, collapse = "\n"))
  expect_true(file.exists(file.path(out, "repo", "tests", "run_suite.R")) && file.exists(file.path(out, "repo", "scripts", "lib", "runtime.R")))
  expect_true(file.exists(file.path(out, "testlib", "testthat.txt")))
  expect_false(dir.exists(file.path(out, "repo", ".git")))
  out2 <- file.path(W$w, "pacote2")
  r2 <- prep(c("--out", out2, "--bundle", W$zip, "--sha256", W$sha))
  expect_identical(r2$status, 0L)
  expect_false(dir.exists(file.path(out2, "repo")) || dir.exists(file.path(out2, "testlib")))
  r3 <- prep(c("--out", file.path(W$w, "pacote3"), "--bundle", W$zip, "--test-lib", file.path(W$w, "nao-existe")))
  expect_identical(r3$status, 8L)
})
