## test-install-windows-static.R -- checagens ESTATICAS (portaveis, rodam no Linux) de install/install.ps1 e install/prevcarga.cmd (M8.4).
## A execucao real (Pester em 5.1 e 7, registro HKCU, cmd) e [DEFERRED M8.6] (test-install-e2e-windows.R); aqui so o que o texto prova.
library(testthat)
root <- pci_root()
ps1 <- readLines(file.path(root, "install", "install.ps1"), warn = FALSE)
cmd <- readLines(file.path(root, "install", "prevcarga.cmd"), warn = FALSE)
sh <- readLines(file.path(root, "install", "install.sh"), warn = FALSE)
tsv <- utils::read.delim(file.path(root, "scripts", "lib", "errors", "install.tsv"), sep = "\t", quote = "", colClasses = "character")
code <- function(x) x[!grepl("^\\s*#", x)]

test_that("[AC7] instalador e lancador sao ASCII puro (nenhum byte > 127)", {
  for (f in c("install/install.ps1", "install/prevcarga.cmd", "install/tests/install.Tests.ps1", "install/tests/PSScriptAnalyzerSettings.psd1")) {
    b <- readBin(file.path(root, f), "raw", file.size(file.path(root, f)))
    expect_false(any(as.integer(b) > 127L), info = f)
  }
})

test_that("[AC6] nenhuma variavel de ambiente lida e HKLM nunca tocado; CONTROL: o detector acusa", {
  expect_length(grep("\\$env:|GetEnvironmentVariable|\\[Environment\\]::Expand|HKLM|LocalMachine|-Verb\\s+RunAs", code(ps1)), 0L)
  expect_length(grep("\\$env:", c(code(ps1), "$env:LOCALAPPDATA")), 1L)   # CONTROL
  expect_true(any(grepl("CurrentUser.OpenSubKey\\('Environment'", ps1)))
})

test_that("[AC5] lancador sem `set ` (CONTROL: acusa um lancador com set), CRLF, exit 6 ASCII, sem blocos entre parenteses", {
  pat <- "(?i)^\\s*set\\s"
  expect_length(grep(pat, cmd, perl = TRUE), 0L)
  expect_length(grep(pat, c(cmd, "  SET TZ=UTC"), perl = TRUE), 1L)
  expect_true(any(grepl("prevcarga: erro [bundle] (exit 6)", cmd, fixed = TRUE)))
  expect_gte(sum(grepl("exit /b 6", cmd, fixed = TRUE)), 2L)
  expect_true(any(grepl("runtime\\R\\bin\\Rscript.exe", cmd, fixed = TRUE)))
  raw <- readBin(file.path(root, "install", "prevcarga.cmd"), "raw", file.size(file.path(root, "install", "prevcarga.cmd")))
  expect_true(as.raw(13L) %in% raw)
  expect_false(any(grepl("\\(\\s*$", cmd)))   # nenhum bloco "( ... )" multilinha
})

test_that("[AC1/AC7] os error.id emitidos existem no catalogo de install.tsv (sem digitos) e o mesmo conjunto do install.sh", {
  used <- unique(gsub("'", "", unlist(regmatches(ps1, gregexpr("'install\\.[a-z_]+'", ps1)))))
  expect_true(all(used %in% tsv$id), info = paste(setdiff(used, tsv$id), collapse = ","))
  expect_true(all(c("install.sha_missing", "install.sha_mismatch", "install.manifest_invalid", "install.no_previous_version",
                    "install.state_inside_versions", "install.download_failed", "install.preflight_failed") %in% used))
  sh_ids <- unique(gsub("[^a-z_.]", "", unlist(regmatches(sh, gregexpr("pc_die install\\.[a-z_]+", sh)))))
  expect_setequal(setdiff(sub("^pc_die", "", sh_ids), ""), intersect(sub("^pc_die", "", sh_ids), c(used, "install.usage")))
})

test_that("[AC1-AC4] semantica espelhada do install.sh: hash antes de Expand-Archive, stage+finally, Replace atomico, N=2, previous", {
  i_hash <- grep("Get-FileHash -Algorithm SHA256", ps1, fixed = TRUE)[1]
  i_exp <- grep("Expand-Archive", ps1, fixed = TRUE)[1]
  expect_true(!is.na(i_hash) && !is.na(i_exp) && i_hash < i_exp)
  expect_true(any(grepl("prevcarga_runtime_validate(r, strict = TRUE)", ps1, fixed = TRUE)))
  expect_true(any(grepl("-UseBasicParsing", ps1, fixed = TRUE)) && any(grepl("Tls12", ps1, fixed = TRUE)))
  expect_true(any(grepl("[IO.File]::Replace", ps1, fixed = TRUE)))
  expect_false(any(grepl("SymbolicLink|Junction|New-Item -ItemType (SymbolicLink|Junction)", ps1)))
  expect_true(any(grepl("\\.stage-", ps1)) && any(grepl("finally", ps1, fixed = TRUE)))
  expect_true(any(grepl("'previous'", ps1, fixed = TRUE)))
  expect_true(any(grepl("Set-StrictMode -Version 3", ps1, fixed = TRUE)) && any(grepl("ErrorActionPreference = 'Stop'", ps1, fixed = TRUE)))
  # mesmos argumentos do install.sh
  for (a in c("Version", "ReleaseBase", "Bundle", "Sha256", "Root", "Data", "State", "SkipPlatformCheck", "Rollback", "Uninstall", "Yes", "Purge", "Format"))
    expect_true(any(grepl(paste0("\\$", a, "\\b"), ps1[1:40])), info = a)
})
