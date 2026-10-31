## test-install-e2e-windows.R -- ponte testthat para a suite Pester do instalador Windows (M8.4; install/tests/install.Tests.ps1).
## So roda em Windows: invoca Pester 5.5.0 em `powershell.exe -ExecutionPolicy Bypass` e em `pwsh` (7), mais PSScriptAnalyzer 1.22.0 com o
## settings fixo; qualquer teste com falha ou skip nao justificado reprova. Ao concluir grava o recibo `install-e2e-windows` (irmao do `install-e2e-linux`).
## Fora do Windows: skip explicito `nao-windows`. [DEFERRED M8.6] a execucao real num host Windows (nao existe um nesta maquina de dev).
library(testthat)

pcw_candidate <- function() {
  fs <- c(Sys.glob(file.path(pci_root(), "dist", "prevcarga-*-windows-x86_64.zip")),
          Sys.glob(file.path(pci_root(), "dist", "*", "prevcarga-*-windows-x86_64.zip")))
  if (length(fs)) sort(fs, method = "radix")[length(fs)] else NA_character_
}

## Roda um script PowerShell (-File) no shell dado e devolve list(status, out).
pcw_ps <- function(shell, file, args = character()) {
  out <- suppressWarnings(system2(shell, c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(file), args), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  list(status = as.integer(st), out = out)
}

test_that("[AC1-AC7] Pester 5.5.0 (powershell.exe 5.1 e pwsh 7) e PSScriptAnalyzer 1.22.0 sobre install.ps1/prevcarga.cmd", {
  skip_if_not(.Platform$OS.type == "windows", "nao-windows")
  cand <- pcw_candidate()
  skip_if(is.na(cand), "sem bundle candidato em dist/ (scripts/tools/build_bundle_windows.ps1)")
  tests <- normalizePath(file.path(pci_root(), "install", "tests", "install.Tests.ps1"), mustWork = TRUE)
  runner <- tempfile(fileext = ".ps1"); on.exit(unlink(runner), add = TRUE)
  writeLines(c(
    "param([string]$Tests, [string]$Candidate)",
    "Import-Module Pester -RequiredVersion 5.5.0 -ErrorAction Stop",
    "$c = New-PesterContainer -Path $Tests -Data @{ Candidate = $Candidate }",
    "$r = Invoke-Pester -Container $c -PassThru -Output Minimal",
    "if ($r.FailedCount -gt 0 -or $r.SkippedCount -gt 0 -or $r.NotRunCount -gt 0) { exit 1 }",
    "exit 0"), runner)
  shells <- c("powershell.exe", if (nzchar(Sys.which("pwsh"))) "pwsh")
  expect_true("powershell.exe" %in% shells)
  res <- lapply(shells, function(s) pcw_ps(s, runner, c(shQuote(tests), shQuote(normalizePath(cand)))))
  for (i in seq_along(res)) expect_identical(res[[i]]$status, 0L, info = paste(shells[i], paste(tail(res[[i]]$out, 30), collapse = "\n")))

  settings <- normalizePath(file.path(pci_root(), "install", "tests", "PSScriptAnalyzerSettings.psd1"), mustWork = TRUE)
  inst <- normalizePath(file.path(pci_root(), "install", "install.ps1"), mustWork = TRUE)
  sa <- tempfile(fileext = ".ps1"); on.exit(unlink(sa), add = TRUE)
  writeLines(c(
    "param([string]$Path, [string]$Settings)",
    "Import-Module PSScriptAnalyzer -RequiredVersion 1.22.0 -ErrorAction Stop",
    "$f = @(Invoke-ScriptAnalyzer -Path $Path -Settings $Settings | Where-Object { $_.Severity -in 'Error', 'Warning' })",
    "$f | ForEach-Object { Write-Output ($_.RuleName + ' ' + $_.Line + ' ' + $_.Message) }",
    "if ($f.Count -gt 0) { exit 1 }", "exit 0"), sa)
  a <- pcw_ps("powershell.exe", sa, c(shQuote(inst), shQuote(settings)))
  expect_identical(a$status, 0L, info = paste(a$out, collapse = "\n"))

  ok <- all(vapply(res, function(r) identical(r$status, 0L), TRUE)) && identical(a$status, 0L)
  e <- new.env(); sys.source(file.path(pci_root(), "scripts", "tools", "receipts.R"), envir = e)
  x <- tempfile("pcw-x-"); on.exit(unlink(x, recursive = TRUE), add = TRUE); utils::unzip(cand, exdir = x)
  root <- if (file.exists(file.path(x, "RUNTIME.json"))) x else file.path(x, "prevcarga")
  out <- e$pc_receipt_write("install-e2e-windows", cand, "windows-x86_64", if (ok) "PASS" else "FAIL",
                            dir = file.path(pci_root(), "tests", ".receipts"), extracted_root = root)
  expect_true(file.exists(out))
})
