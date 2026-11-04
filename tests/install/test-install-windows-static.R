## test-install-windows-static.R -- checagens ESTATICAS (portaveis, rodam no Linux) de install/install.ps1 e install/prevcarga.cmd (M8.4).
## A execucao real (Pester em 5.1 e 7, registro HKCU, cmd) e [DEFERRED M8.6] (test-install-e2e-windows.R); aqui so o que o texto prova.
library(testthat)
root <- pci_root()
ps1 <- readLines(file.path(root, "install", "install.ps1"), warn = FALSE)
cmd <- readLines(file.path(root, "install", "prevcarga.cmd"), warn = FALSE)
sh <- readLines(file.path(root, "install", "install.sh"), warn = FALSE)
tsv <- utils::read.delim(file.path(root, "scripts", "lib", "errors", "install.tsv"), sep = "\t", quote = "", colClasses = "character")
code <- function(x) x[!grepl("^\\s*#", x)]

test_that("[AC7] instaladores (ps1 e sh) e lancador sao ASCII puro (nenhum byte > 127)", {
  for (f in c("install/install.ps1", "install/install.sh", "install/prevcarga.cmd", "install/tests/install.Tests.ps1", "install/tests/PSScriptAnalyzerSettings.psd1")) {
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

test_that("[AC1-AC4] semantica espelhada do install.sh: hash antes da extracao (ZipFile), stage+finally, Replace atomico, N=2, previous", {
  i_hash <- grep("Get-FileHash -Algorithm SHA256", ps1, fixed = TRUE)[1]
  i_call <- grep("^\\s*try \\{ Expand-PcZip ", ps1)                    # a chamada da extracao no Invoke-PcInstall
  i_inst <- grep("^function Invoke-PcInstall", ps1)
  i_fetch <- grep("$zip = Get-PcVerifiedBundle", ps1, fixed = TRUE)
  expect_length(i_call, 1L); expect_length(i_fetch, 1L)
  expect_true(!is.na(i_hash) && i_hash < i_inst && i_inst < i_fetch && i_fetch < i_call)   # hash (no Get-PcVerifiedBundle) antes de extrair
  expect_length(grep("Expand-Archive", code(ps1), fixed = TRUE), 0L)      # sem o cmdlet lento do 5.1
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

## Extracao por System.IO.Compression.ZipFile (M8 install-progresso): mantem a protecao de zip-slip que o Expand-Archive dava.
ps1_fun <- function(name) {
  i <- grep(paste0("^function ", name, "\\b"), ps1); stopifnot(length(i) == 1L)
  j <- grep("^\\}", ps1); j <- j[j > i][1]
  ps1[i:j]
}
test_that("extracao por ZipFile: zip-slip recusado (GetFullPath + separador final + sem caixa), dir so cria, ZipArchive fechado em finally", {
  f <- ps1_fun("Expand-PcZip")
  expect_true(any(grepl("Add-Type -AssemblyName System.IO.Compression.FileSystem", f, fixed = TRUE)))
  expect_true(any(grepl("[IO.Compression.ZipFile]::OpenRead(", f, fixed = TRUE)))
  expect_true(any(grepl("GetFullPath($Dest).TrimEnd('\\', '/') + '\\'", f, fixed = TRUE)))        # raiz com separador final
  expect_true(any(grepl("GetFullPath([IO.Path]::Combine($root, $e.FullName))", f, fixed = TRUE)))
  # o guard tem de ser a forma negada: entrada que NAO comeca pela raiz -> erro (uma inversao recusaria tudo)
  slip_re <- "^\\s*if \\(-not \\$full\\.StartsWith\\(\\$root, \\[StringComparison\\]::OrdinalIgnoreCase\\)\\) \\{"
  slip <- grep(slip_re, f)
  expect_length(slip, 1L)
  expect_true(any(grepl("Write-PcError 'install.manifest_invalid'", f[slip:(slip + 2L)], fixed = TRUE)))
  expect_true(any(grepl("EndsWith('/')", f, fixed = TRUE)))
  i_fin <- grep("} finally {", f, fixed = TRUE); expect_length(i_fin, 1L)
  expect_true(any(grepl("$za.Dispose()", f[i_fin:length(f)], fixed = TRUE)))
  expect_true(any(grepl("Write-Progress -Activity $act -Completed", f[i_fin:length(f)], fixed = TRUE)))
  expect_true(any(grepl("$ProgressPreference = 'Continue'", f, fixed = TRUE)))   # local, so com a barra ligada
  expect_true(any(grepl("if ($script:PcProgress) { $ProgressPreference = 'Continue' }", f, fixed = TRUE)))
  # a falha de extracao continua manifest_invalid 'falha ao extrair' e o PCEXIT do zip-slip nao e emitido 2x
  expect_true(any(grepl("Write-PcError 'install.manifest_invalid' $zip ('falha ao extrair: '", ps1, fixed = TRUE)))
  expect_true(any(grepl("if ($_.Exception.Message -eq 'PCEXIT') { throw }", ps1, fixed = TRUE)))
  # CONTROL: o detector acusa um guard invertido (sem o -not) e um guard removido
  expect_length(grep(slip_re, sub("-not $full.StartsWith", "$full.StartsWith", f, fixed = TRUE)), 0L)
  expect_length(grep(slip_re, f[-slip]), 0L)
})

test_that("etapas e barra so em texto com stderr no terminal: IsErrorRedirected + Format, Write-PcStep guardado, N fixo", {
  expect_true(any(grepl("[Console]::IsErrorRedirected", ps1, fixed = TRUE)))
  expect_true(any(grepl("$script:PcProgress = ($script:Format -eq 'text') -and -not (Test-PcErrRedirected)", ps1, fixed = TRUE)))
  expect_true(any(grepl("$script:PcProgress = $false", ps1, fixed = TRUE)))                       # default desligado
  st <- ps1_fun("Write-PcStep")
  expect_true(any(grepl("if (-not $script:PcProgress) { return }", st, fixed = TRUE)))
  expect_true(any(grepl("Write-PcStream 'err'", st, fixed = TRUE)))
  # toda etapa passa pelo Write-PcStep e todo Write-Progress fica sob $script:PcProgress (a funcao de extracao)
  expect_length(grep("prevcarga: \\[[0-9]", code(ps1)), 0L)                                    # nenhuma etapa escrita fora do helper
  calls <- grep("Write-PcStep [0-9]", ps1, value = TRUE)
  expect_setequal(as.integer(sub(".*Write-PcStep ([0-9]).*", "\\1", calls)), 1:6)
  expect_true(any(grepl("$script:PcSteps = 6", ps1, fixed = TRUE)))
  wp <- grep("Write-Progress", ps1)
  f <- grep("^function Expand-PcZip", ps1); g <- grep("^\\}", ps1); g <- g[g > f][1]
  expect_true(all(wp > f & wp < g))
  # o script segue SilentlyContinue no topo (Invoke-WebRequest)
  expect_true(any(grepl("^\\$ProgressPreference = 'SilentlyContinue'", ps1)))
})

sh_fun <- function(name) {
  i <- grep(paste0("^", name, "\\(\\)"), sh); stopifnot(length(i) == 1L)
  j <- grep("^\\}", sh); j <- j[j > i][1]
  sh[i:j]
}
test_that("install.sh: etapas, banner e mensagens so pelo pc_ui ([ -t 2 ] e fora do json), POSIX (sem bashismo no helper)", {
  ui <- sh_fun("pc_ui")
  expect_true(any(grepl('[ "$PC_FORMAT" = json ] && return 1', ui, fixed = TRUE)))
  expect_true(any(grepl("[ -t 2 ] || return 1", ui, fixed = TRUE)))
  for (f in c("pc_step", "pc_note", "pc_banner", "pc_intro", "pc_next_steps")) {
    b <- sh_fun(f)
    expect_identical(trimws(b[2]), "pc_ui || return 0", info = f)          # primeira linha de cada helper: a guarda
    expect_true(any(grepl(">&2", b, fixed = TRUE)), info = f)
    expect_false(any(grepl(">&1|/dev/stdout", b)), info = f)
  }
  body <- sh_fun("pc_step")
  calls <- grep("pc_step [0-9]", sh, value = TRUE)
  expect_setequal(as.integer(sub(".*pc_step ([0-9]).*", "\\1", calls)), 1:6)
  expect_length(grep("\\[\\[|^\\s*(function|local)\\s", code(sh)), 0L)
})

## Banner do instalador = o da CLI (scripts/lib/banner.R): as 6 linhas da curva ficam COPIADAS literalmente no install.ps1 e no
## install.sh (o instalador roda antes de existir bundle/R). Paridade com .prevcarga_banner_curve(); CONTROL: uma linha alterada reprova.
banner_env <- new.env(); sys.source(file.path(root, "scripts", "lib", "banner.R"), envir = banner_env)
CURVE <- banner_env$.prevcarga_banner_curve()
copied_curve <- function(x) {
  a <- grep("^\\s*# >>> curva do banner", x); z <- grep("^\\s*# <<< curva do banner <<<", x)
  if (length(a) != 1L || length(z) != 1L || z <= a) return(character())
  seg <- x[(a + 1L):(z - 1L)]
  m <- regmatches(seg, regexec("^\\s*'([^']*)'\\s*(\\\\)?\\s*(>&2)?\\s*$", seg))
  vapply(Filter(length, m), `[[`, "", 2L)
}
test_that("banner: a curva copiada no install.ps1 e no install.sh e identica a .prevcarga_banner_curve(); CONTROL: linha alterada reprova", {
  expect_length(CURVE, 6L)
  for (x in list(ps1 = ps1, sh = sh)) expect_identical(copied_curve(x), CURVE)
  mut <- ps1; k <- grep("^\\s*'\\|\\|\\|\\|______", mut); expect_length(k, 1L)
  mut[k] <- sub("______", "_____-", mut[k], fixed = TRUE)
  expect_false(identical(copied_curve(mut), CURVE))                              # CONTROL ps1
  mut <- sh; k <- grep("^\\s*'                    ______", mut); expect_length(k, 1L)
  mut[k] <- sub("______", "_____ ", mut[k], fixed = TRUE)
  expect_false(identical(copied_curve(mut), CURVE))                              # CONTROL sh
  expect_false(identical(copied_curve(sh[-grep("^\\s*'____      __", sh)]), CURVE))   # CONTROL: linha removida
  # titulo e linha de versao no formato da CLI; sem cor (nenhum escape ANSI)
  expect_true(any(grepl("'prevcarga - instalador'", ps1, fixed = TRUE)) && any(grepl("'prevcarga - instalador'", sh, fixed = TRUE)))
  expect_true(any(grepl("('versao ' + $V)", ps1, fixed = TRUE)) && any(grepl("printf 'versao %s\\n'", sh, fixed = TRUE)))
  expect_length(grep("\\[3[0-9]m|\\x1b|\\\\033|\\\\e\\[|`e\\[", c(code(ps1), code(sh))), 0L)
})

test_that("install.ps1: banner, explicacoes e proximos passos sob $script:PcProgress, em stderr; a linha final continua em stdout", {
  for (f in c("Write-PcNote", "Write-PcIntro", "Write-PcNextSteps")) {
    b <- ps1_fun(f)
    expect_identical(trimws(b[2]), "if (-not $script:PcProgress) { return }", info = f)
    expect_true(any(grepl("Write-PcStream 'err'", b, fixed = TRUE)), info = f)
    expect_false(any(grepl("Write-PcStream 'out'|Write-Host|Write-Output|Write-Information", b)), info = f)
  }
  # nenhum outro texto de UI fora dos helpers (o banner so sai pelo Write-PcIntro)
  expect_length(grep("'(proximos passos|o que vai acontecer):", code(ps1)), 2L)
  expect_true(all(grep("'(proximos passos|o que vai acontecer):", ps1) > grep("^function Write-PcIntro", ps1)))
  i_ok <- grep("^\\s*Write-PcOk 'install' ", ps1); i_ns <- grep("^\\s*Write-PcNextSteps \\$bin \\$pathUpdated", ps1)
  expect_length(i_ok, 1L); expect_length(i_ns, 1L); expect_identical(i_ns, i_ok + 1L)   # proximos passos DEPOIS da linha final
  expect_true(any(grepl("('prevcarga: instalado ' + $v + ' em '", ps1, fixed = TRUE)))
  # intro antes da etapa 1 nos dois fluxos (com -Bundle: versao do nome do arquivo; sem: -Version ou latest.txt)
  intro <- grep("^\\s*Write-PcIntro ", ps1); st1 <- grep("^\\s*Write-PcStep 1 ", ps1)
  expect_length(intro, 2L); expect_identical(intro + 1L, st1)
  expect_true(any(grepl("-match '^prevcarga-(.+)-windows-x86_64\\.zip$') { $bv = $Matches[1] }", ps1, fixed = TRUE)))
  # proximos passos: Windows = terminal novo para o PATH; data_dir ja configurado nao e sugerido; so flags reais
  ns <- ps1_fun("Write-PcNextSteps")
  expect_true(any(grepl("abra um terminal novo para o PATH valer", ns, fixed = TRUE)))
  expect_true(any(grepl("prevcarga doctor", ns, fixed = TRUE)))
  expect_true(any(grepl("Get-PcConfigValue 'data_dir'", ns, fixed = TRUE)) && any(grepl("if ($d) {", ns, fixed = TRUE)))
  expect_true(any(grepl("-Data <dir>", ns, fixed = TRUE)) && any(grepl("$Data", ps1[1:40], fixed = TRUE)))
  expect_true(any(grepl("-Rollback", grep("Write-PcNote", ps1, value = TRUE), fixed = TRUE)))
  # CONTROL: a guarda removida de um helper e detectada
  b <- ps1_fun("Write-PcNote"); expect_false(identical(trimws(b[-2][2]), "if (-not $script:PcProgress) { return }"))
})
