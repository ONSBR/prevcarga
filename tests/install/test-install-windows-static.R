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

## Unicas atribuicoes permitidas: as 6 limpezas `set "X="` depois de `setlocal` (equivalem ao `env -u R_HOME -u R_LIBS -u R_LIBS_USER -u R_LIBS_SITE
## -u R_PROFILE_USER -u R_ENVIRON_USER` do Linux; decisao do Gabriel no M8.9: o lancador e hermetico a library herdada). Qualquer outro `set` falha.
LAUNCHER_UNSETS <- c("R_HOME", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE", "R_PROFILE_USER", "R_ENVIRON_USER")
launcher_bad_sets <- function(x) {
  i <- grep("(?i)^\\s*set\\s", x, perl = TRUE)
  i[!(trimws(x[i]) %in% paste0('set "', LAUNCHER_UNSETS, '="'))]
}
test_that("[AC5] lancador: so as 6 limpezas `set \"X=\"` apos setlocal; qualquer outro `set ` falha (CONTROL), CRLF, exit 6 ASCII, sem blocos", {
  expect_length(launcher_bad_sets(cmd), 0L)
  sl <- grep("(?i)^\\s*setlocal\\s*$", cmd, perl = TRUE); expect_length(sl, 1L)
  for (v in LAUNCHER_UNSETS) {                                                 # cada limpeza exatamente 1x, entre setlocal e o Rscript.exe
    r <- which(trimws(cmd) == paste0('set "', v, '="'))
    expect_length(r, 1L); expect_lt(sl, r); expect_lt(r, grep("Rscript.exe", cmd, fixed = TRUE)[1])
  }
  expect_length(grep("(?i)^\\s*set\\s", cmd, perl = TRUE), length(LAUNCHER_UNSETS))   # e nenhuma outra atribuicao
  expect_length(launcher_bad_sets(c(cmd, "  SET TZ=UTC")), 1L)            # CONTROL: outra variavel
  expect_length(launcher_bad_sets(c(cmd, 'set "R_HOME=C:\\x"')), 1L)       # CONTROL: R_HOME com valor
  expect_length(launcher_bad_sets(c(cmd, "set R_LIBS=")), 1L)               # sem aspas: fora da lista exata
  expect_length(launcher_bad_sets(c(cmd, 'set "R_LIBS_USER=/x"')), 1L)        # variavel da lista, mas com valor
  expect_length(launcher_bad_sets(c(cmd, 'set "R_PLATFORM="')), 1L)           # variavel parecida, fora da lista
  expect_true(any(grepl("prevcarga: erro [bundle] (exit 6)", cmd, fixed = TRUE)))
  expect_gte(sum(grepl("exit /b 6", cmd, fixed = TRUE)), 2L)
  expect_true(any(grepl("runtime\\R\\bin\\Rscript.exe", cmd, fixed = TRUE)))
  raw <- readBin(file.path(root, "install", "prevcarga.cmd"), "raw", file.size(file.path(root, "install", "prevcarga.cmd")))
  expect_true(as.raw(13L) %in% raw)
  expect_false(any(grepl("\\(\\s*$", cmd)))
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

test_that("[AC7] install.ps1 le o catalogo GERADO pelo NOME das colunas (id,class,exit_code,message,hint), nao por posicao", {
  skip_if(!nzchar(Sys.which("Rscript")))
  out <- tempfile(fileext = ".tsv"); on.exit(unlink(out))
  st <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(file.path(root, "scripts", "tools", "export_error_catalog.R")), "--out", shQuote(out)),
                stdout = FALSE, stderr = FALSE)
  expect_identical(as.integer(st), 0L)
  gen <- utils::read.delim(out, sep = "\t", quote = "", colClasses = "character", check.names = FALSE)
  expect_identical(names(gen), c("id", "class", "exit_code", "message", "hint"))
  # o mesmo algoritmo do Read-PcCatalog: indice de cada coluna pelo nome do cabecalho
  hdr <- strsplit(readLines(out, n = 1L), "\t", fixed = TRUE)[[1]]
  col <- stats::setNames(seq_along(hdr), hdr)
  row <- strsplit(grep("^install\\.sha_missing\t", readLines(out), value = TRUE), "\t", fixed = TRUE)[[1]]
  want <- tsv[tsv$id == "install.sha_missing", ]
  expect_identical(row[col[["message"]]], want$message)
  expect_identical(row[col[["hint"]]], want$hint)
  expect_identical(row[col[["class"]]], want$class)
  expect_false(identical(row[3L], want$message))   # CONTROL: leitura posicional (coluna 3) pegaria exit_code
  expect_true(any(grepl("$col[$f[$i]] = $i", ps1, fixed = TRUE)) || any(grepl("$col[$f[$i]] = $i", gsub("\\s+", " ", ps1), fixed = TRUE)))
  expect_true(any(grepl("$col['message']", ps1, fixed = TRUE)) && any(grepl("$col['hint']", ps1, fixed = TRUE)))
})
