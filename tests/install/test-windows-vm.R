## test-windows-vm.R -- ponte testthat para o roteiro Pester `install/tests/windows-vm.Tests.ps1` (M8.11; gate da VM Windows limpa).
## So roda em Windows: invoca Pester 5.5.0 (versao FIXA) em `powershell.exe -ExecutionPolicy Bypass`, le o resultado por It e grava o recibo
## `windows-vm` (PASS integral ou FAIL com `passo_falho`) para o sha256 do zip, por pc_receipt_write(). Fora do Windows: skip explicito `nao-windows`.
## O ponto de entrada operacional da VM e tests/windows/Invoke-WindowsGate.ps1 (docs/reference/windows-gate.md); esta ponte e a forma de rodar so este
## roteiro a partir de um R de dev no Windows. [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] NUNCA executada nesta maquina de dev (Linux).
library(testthat)

pcvm_candidate <- function() {
  fs <- c(Sys.glob(file.path(pci_root(), "dist", "prevcarga-*-windows-x86_64.zip")),
          Sys.glob(file.path(pci_root(), "dist", "*", "prevcarga-*-windows-x86_64.zip")))
  if (length(fs)) sort(fs, method = "radix")[length(fs)] else NA_character_
}

test_that("[AC3] roteiro Pester windows-vm: PASS grava o recibo windows-vm para o sha256 do zip; FAIL grava FAIL com o passo falho", {
  skip_if_not(.Platform$OS.type == "windows", "nao-windows")
  cand <- pcvm_candidate()
  skip_if(is.na(cand), "sem bundle candidato em dist/ (scripts/tools/build_bundle_windows.ps1)")
  sha <- digest::digest(file = cand, algo = "sha256")
  tests <- normalizePath(file.path(pci_root(), "install", "tests", "windows-vm.Tests.ps1"), mustWork = TRUE)
  work <- tempfile("pcvm-work-"); dir.create(work)
  facts <- file.path(work, "facts.json"); out <- file.path(work, "pester.json")
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  runner <- file.path(work, "run.ps1")
  writeLines(c(
    "param([string]$Tests, [string]$Bundle, [string]$Sha256, [string]$Facts, [string]$Out, [string]$Work)",
    "Import-Module Pester -RequiredVersion 5.5.0 -ErrorAction Stop",
    "$c = New-PesterContainer -Path $Tests -Data @{ Bundle = $Bundle; Sha256 = $Sha256; Facts = $Facts; Work = (Join-Path $Work 'vm') }",
    "$r = Invoke-Pester -Container $c -PassThru -Output Minimal",
    "$t = @(); foreach ($x in @($r.Tests)) { $t += @{ name = [string]$x.ExpandedName; result = [string]$x.Result } }",
    "$o = @{ failed = [int]$r.FailedCount; skipped = [int]$r.SkippedCount; notrun = [int]$r.NotRunCount; tests = $t }",
    "[IO.File]::WriteAllText($Out, (ConvertTo-Json -InputObject $o -Depth 5))",
    "exit 0"), runner)
  st <- system2("powershell.exe", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(runner), shQuote(tests), shQuote(normalizePath(cand)),
                                     sha, shQuote(facts), shQuote(out), shQuote(work)), stdout = TRUE, stderr = TRUE)
  expect_true(file.exists(out), info = paste(utils::tail(st, 20), collapse = "\n"))
  res <- jsonlite::read_json(out)
  failed <- vapply(Filter(function(x) identical(x$result, "Failed"), res$tests), function(x) x$name, "")
  expect_identical(res$failed, 0L, info = paste(failed, collapse = " | "))
  expect_identical(res$skipped + res$notrun, 0L)       # skip so com insumo ausente: o recibo PASS exige a suite integral
  f <- if (file.exists(facts)) jsonlite::read_json(facts) else list()
  resultado <- if (res$failed > 0L) "FAIL" else if (res$skipped + res$notrun == 0L) "PASS" else NA_character_
  skip_if(is.na(resultado), "recibo nao gravado: houve skip (insumo ausente)")
  e <- new.env(); sys.source(file.path(pci_root(), "scripts", "tools", "receipts.R"), envir = e)
  x <- tempfile("pcvm-x-"); on.exit(unlink(x, recursive = TRUE), add = TRUE); utils::unzip(cand, exdir = x)
  root <- if (file.exists(file.path(x, "RUNTIME.json"))) x else file.path(x, "prevcarga")
  p <- e$pc_receipt_write("windows-vm", cand, "windows-x86_64", resultado, dir = file.path(pci_root(), "tests", ".receipts"), extracted_root = root)
  r <- jsonlite::read_json(p)
  r$shells <- f$shells; r$bytes_gt_127 <- f$bytes_gt_127 %||% 0L
  r["passo_falho"] <- list(if (length(failed)) failed[[1L]] else NULL)
  jsonlite::write_json(r, p, auto_unbox = TRUE, null = "null", pretty = TRUE)
  expect_identical(jsonlite::read_json(p)$sha256, sha)
})
