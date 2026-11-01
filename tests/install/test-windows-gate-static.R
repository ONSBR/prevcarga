## test-windows-gate-static.R -- checagens ESTATICAS (rodam no Linux) do gate da VM Windows (M8.11): o que o texto prova, sem executar nada de Windows.
## A execucao real e [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] (tests/windows/Invoke-WindowsGate.ps1; docs/reference/windows-gate.md).
## Bracos: ASCII; nenhuma leitura de variavel de ambiente; cobertura gate x tabela do roteiro (com CONTROL); It tags dos Pester; parse com pwsh SO se existir.
library(testthat)
root <- pci_root()
GATE_FILES <- c("tests/windows/Invoke-WindowsGate.ps1", "tests/windows/write-receipt.R", "tests/windows/control-perturb.R", "tests/windows/release-check.R",
                "install/tests/windows-vm.Tests.ps1", "install/tests/windows-vm.Helpers.ps1", "install/tests/windows-vm-cross.Tests.ps1",
                "scripts/tools/prepare_windows_gate.sh", "docs/reference/windows-gate.md")
rd <- function(f) readLines(file.path(root, f), warn = FALSE)
code <- function(x) x[!grepl("^\\s*#", x)]

## --- cobertura -------------------------------------------------------------------------------------------------------------------------
## ids W## do roteiro (1a coluna da tabela de conferencia) e do gate (chaves de $script:Items)
doc_rows <- function(doc) {
  a <- grep("^## 4\\.", doc); b <- grep("^## 5\\.", doc)
  if (length(a) && length(b)) doc <- doc[seq(a[[1L]], b[[1L]] - 1L)]          # so a tabela de conferencia (a secao 7 repete os ids com outro sentido)
  r <- grep("^\\| W[0-9]{2} \\|", doc, value = TRUE)
  cells <- lapply(strsplit(r, "\\|"), function(x) trimws(x[-1L]))
  data.frame(id = vapply(cells, `[`, "", 1L), pendencia = vapply(cells, `[`, "", 2L), origem = vapply(cells, `[`, "", 3L),
             como = vapply(cells, `[`, "", 4L), esperado = vapply(cells, `[`, "", 5L), stringsAsFactors = FALSE)
}
gate_ids <- function(gate) unique(sub("^\\s*'(W[0-9]{2})'.*", "\\1", grep("^\\s*'W[0-9]{2}' = ", gate, value = TRUE)))
coverage_gaps <- function(doc, gate) {
  d <- doc_rows(doc); g <- gate_ids(gate)
  used <- unique(unlist(regmatches(gate, gregexpr("'W[0-9]{2}'", gate))))
  list(sem_linha_no_doc = setdiff(g, d$id), sem_item_no_gate = setdiff(d$id, g),
       celula_vazia = d$id[!nzchar(d$pendencia) | !nzchar(d$origem) | !nzchar(d$como) | !nzchar(d$esperado)],
       nunca_reportado = setdiff(g, gsub("'", "", used)))
}
## Pendencias DEFERIDAS conhecidas (docs/estado/*.md, "windows-vm" / "DEFERRED M8.6"): cada uma tem de aparecer em alguma linha do roteiro
DEFERRED_KEYWORDS <- c(build = "build_bundle_windows", instalador = "install.ps1", pester = "install.Tests.ps1", sync = "prevcarga-sync.Tests.ps1",
                       analyzer = "PSScriptAnalyzer", aspas = "-e", maxpath = "MAX_PATH", hermetico = "R_LIBS", doctor = "doctor",
                       sujo = "OPENBLAS", cross = "cross_os", nfr = "nfr-30min", matriz = "contrato de erros", catalogo = "error-catalog",
                       equivalencia = "sh x ps1", receipts = "cross-platform",
                       suite = "run_suite.R", pythonfree = "release-check.R")
keyword_gaps <- function(doc) { txt <- paste(doc, collapse = "\n"); names(DEFERRED_KEYWORDS)[!vapply(DEFERRED_KEYWORDS, grepl, NA, x = txt, fixed = TRUE)] }

test_that("[AC] o gate cobre todo item da tabela do roteiro (e vice-versa); cada linha tem pendencia, origem, comando e resultado esperado", {
  doc <- rd("docs/reference/windows-gate.md"); gate <- rd("tests/windows/Invoke-WindowsGate.ps1")
  d <- doc_rows(doc)
  expect_gte(nrow(d), 17L)
  expect_identical(sort(gate_ids(gate)), sort(d$id))
  gaps <- coverage_gaps(doc, gate)
  for (k in names(gaps)) expect_length(gaps[[k]], 0L)
  expect_length(keyword_gaps(doc), 0L)
})

test_that("[CONTROL] o detector de cobertura acusa: linha removida do roteiro, item removido do gate, celula vazia, pendencia deferida sem linha", {
  doc <- rd("docs/reference/windows-gate.md"); gate <- rd("tests/windows/Invoke-WindowsGate.ps1")
  sem14 <- doc[!grepl("^\\| W14 \\|", doc)]
  expect_identical(coverage_gaps(sem14, gate)$sem_linha_no_doc, "W14")
  gate_sem <- gate[!grepl("^\\s*'W13' = ", gate)]
  expect_identical(coverage_gaps(doc, gate_sem)$sem_item_no_gate, "W13")
  vazio <- sub("^(\\| W09 \\|[^|]*\\|[^|]*\\|)[^|]*\\|", "\\1 |", doc)
  expect_identical(coverage_gaps(vazio, gate)$celula_vazia, "W09")
  expect_true("maxpath" %in% keyword_gaps(gsub("MAX_PATH", "", doc, fixed = TRUE)))
})

test_that("[AC] todo It [Wnn] da suite Pester corresponde a um item do gate, e os itens de Pester (W03,W04,W08-W15) tem ao menos um It", {
  its <- unlist(lapply(c("install/tests/windows-vm.Tests.ps1", "install/tests/windows-vm-cross.Tests.ps1"), function(f) {
    x <- grep("^\\s*It '\\[W[0-9]{2}\\]", rd(f), value = TRUE); sub("^\\s*It '\\[(W[0-9]{2})\\].*", "\\1", x) }))
  ids <- gate_ids(rd("tests/windows/Invoke-WindowsGate.ps1"))
  expect_length(setdiff(its, ids), 0L)
  expect_length(setdiff(c("W03", "W04", "W08", "W09", "W10", "W11", "W12", "W13", "W14", "W15"), its), 0L)
  expect_identical(length(grep("^\\s*It '", rd("install/tests/windows-vm.Tests.ps1"))), length(grep("^\\s*It '\\[W", rd("install/tests/windows-vm.Tests.ps1"))))   # nenhum It sem tag
})

## --- ASCII e variavel de ambiente ---------------------------------------------------------------------------------------------------------
test_that("[AC] arquivos do gate sao ASCII puro (nenhum byte > 127); CONTROL: o detector acusa um acento", {
  for (f in GATE_FILES) {
    b <- readBin(file.path(root, f), "raw", file.size(file.path(root, f)))
    expect_false(any(as.integer(b) > 127L), info = f)
  }
  expect_true(any(as.integer(charToRaw("a\u00e7")) > 127L))
})

env_hits <- function(x) grep("\\$env:|GetEnvironmentVariable|SetEnvironmentVariable|Sys\\.getenv|Sys\\.setenv|\\bgetenv\\b|\\$\\{?[A-Z_]+\\}?\\s*:-|HKLM|LocalMachine|RunAs", code(x), value = TRUE)
test_that("[AC] nenhuma leitura/escrita de variavel de ambiente (nem HKLM/RunAs) no gate; CONTROL: o detector acusa", {
  for (f in setdiff(GATE_FILES, c("docs/reference/windows-gate.md", "install/tests/windows-vm.Tests.ps1", "install/tests/windows-vm.Helpers.ps1"))) expect_length(env_hits(rd(f)), 0L)
  expect_length(env_hits(c("$x = $env:LOCALAPPDATA")), 1L)
  expect_length(env_hits(c("Sys.getenv('TZ')")), 1L)
  expect_length(env_hits(c("[Environment]::SetEnvironmentVariable('A','b')")), 1L)
})

## No Pester, as variaveis de ambiente aparecem so como AJUSTE do ambiente do processo FILHO (shell sujo, ProcessStartInfo): nunca lidas do ambiente.
test_that("[AC] Pester windows-vm: nenhum `$env:`/Get/SetEnvironmentVariable e nenhum HKLM de escrita; HKLM so lido para provar que o PATH de maquina nao foi tocado", {
  for (f in c("install/tests/windows-vm.Tests.ps1", "install/tests/windows-vm.Helpers.ps1")) {
    x <- code(rd(f))
    expect_length(grep("\\$env:|GetEnvironmentVariable|SetEnvironmentVariable|RunAs", x), 0L)
    hk <- grep("LocalMachine", x, value = TRUE)
    expect_true(all(grepl("OpenSubKey\\('SYSTEM", hk)) && !any(grepl("OpenSubKey\\([^)]*,\\s*\\$true", hk)))   # LocalMachine so leitura
  }
})

test_that("[AC] o gate nao envia nada: sem aws/curl/upload/Invoke-RestMethod de escrita; so a leitura opcional por -ReleaseBase", {
  for (f in c("tests/windows/Invoke-WindowsGate.ps1", "scripts/tools/prepare_windows_gate.sh")) {
    x <- paste(code(rd(f)), collapse = "\n")
    expect_false(grepl("aws |\\bcurl\\b|\\bscp\\b|UploadFile|UploadString|Invoke-RestMethod|-Method\\s+(Put|Post)", x), info = f)
  }
})

test_that("[AC] estrutura do gate: exit codes, sha256 obrigatorio, sem elevacao, PS 5.1 (sem ?? / ?. / ternario / &&)", {
  g <- paste(code(rd("tests/windows/Invoke-WindowsGate.ps1")), collapse = "\n")
  expect_true(grepl("exit 1", g, fixed = TRUE) && grepl("exit 2", g, fixed = TRUE) && grepl("exit 3", g, fixed = TRUE))
  expect_true(grepl("informe -Sha256", g, fixed = TRUE))
  expect_false(grepl("\\bStart-Process\\b[^\\n]*-Verb|RunAs", g))
  for (f in c("tests/windows/Invoke-WindowsGate.ps1", "install/tests/windows-vm.Tests.ps1", "install/tests/windows-vm.Helpers.ps1", "install/tests/windows-vm-cross.Tests.ps1")) {
    x <- paste(code(rd(f)), collapse = "\n")
    expect_false(grepl("\\?\\?|\\?\\.|\\s&&\\s|\\s\\|\\|\\s|\\?\\s+[^:\\n]+\\s:\\s", gsub("'[^'\\n]*'|\"[^\"\\n]*\"", "''", x)), info = f)
  }
  expect_true(grepl("Pester 5.5.0|5\\.5\\.0", paste(rd("tests/windows/Invoke-WindowsGate.ps1"), collapse = "\n")))
})

test_that("[AC] pwsh disponivel => parse-check dos .ps1 (SEM instalar nada); senao skip explicito", {
  pw <- Sys.which("pwsh")
  skip_if(!nzchar(pw), "pwsh ausente nesta maquina: parse-check dos .ps1 nao executado [DEFERRED windows-vm do M8, dono: Gabriel] (nunca instalar)")
  for (f in grep("\\.ps1$", GATE_FILES, value = TRUE)) {
    p <- file.path(root, f)
    out <- system2(pw, c("-NoProfile", "-Command", shQuote(sprintf("$e = $null; [void][System.Management.Automation.Language.Parser]::ParseFile('%s', [ref]$null, [ref]$e); if ($e) { $e | ForEach-Object { $_.Message }; exit 1 }", p))), stdout = TRUE, stderr = TRUE)
    expect_null(attr(out, "status"), info = paste(f, paste(out, collapse = " ")))
  }
})

test_that("[AC] pendencias diferidas do repo apontam para este gate (docs/estado: install, sync, m8-bundle-windows, m8-e2e-bundle, nfr-performance, verificar)", {
  for (f in c("install", "sync", "m8-bundle-windows", "m8-e2e-bundle", "nfr-performance", "verificar", "m8-bundle")) {
    expect_true(any(grepl("windows-gate", rd(file.path("docs", "estado", paste0(f, ".md"))), fixed = TRUE)), info = f)
  }
  expect_true(file.exists(file.path(root, "docs", "estado", "windows-gate.md")))
})

## --- fechamento do M8 (M8.12): W16 (suite inteira no Windows nativo) e W17 (Python-free do bundle windows) ---------------------------------
test_that("[AC] W16 e W17 estao no gate e no roteiro, sao PENDENTES (Gabriel) no estado e nao fingem resultado", {
  gate <- rd("tests/windows/Invoke-WindowsGate.ps1"); doc <- rd("docs/reference/windows-gate.md"); est <- rd("docs/estado/windows-gate.md")
  d <- doc_rows(doc)
  for (id in c("W16", "W17")) {
    expect_true(id %in% gate_ids(gate), info = id); expect_true(id %in% d$id, info = id)
    expect_true(any(grepl(paste0("Add-Result '", id, "'"), gate, fixed = TRUE)), info = id)
  }
  expect_true(any(grepl("run_suite.R", d$como[d$id == "W16"], fixed = TRUE)) || any(grepl("run_suite.R", d$pendencia[d$id == "W16"], fixed = TRUE)))
  expect_true(any(grepl("release-check.R", d$como[d$id == "W17"], fixed = TRUE)))
  expect_true(any(grepl("W16", est, fixed = TRUE)) && any(grepl("W17", est, fixed = TRUE)) && any(grepl("PENDENTE", est, fixed = TRUE)))
  ## sem -SuiteRepo/-TestLib o W16 e SKIP (INCOMPLETO), nunca PASS: o texto do gate declara o SKIP
  g <- paste(gate, collapse = "\n")
  expect_true(grepl("Add-Result 'W16' 'SKIP'", g, fixed = TRUE))
  ## W16 roda o runner pelo Rscript.exe do bundle instalado (nao pelo R do host); W17 idem com release-check.R
  expect_true(grepl("Invoke-Proc $rsNow $a 'W16'", g, fixed = TRUE) && grepl("Invoke-Proc $rsNow $a 'W17'", g, fixed = TRUE))
})

test_that("[AC] release-check.R: ASCII, sem variavel de ambiente, mesmos gates do test-python-free-release.R; roda de verdade no stage Linux (platform parametrizada) e acusa injecao [CONTROL]", {
  f <- "tests/windows/release-check.R"
  expect_length(env_hits(rd(f)), 0L)
  src <- paste(rd(f), collapse = "\n")
  expect_true(all(vapply(c("pc_bundle_python_free", "pc_bundle_net_grep", "pc_bundle_portability_grep", "prevcarga_runtime_validate", "4.5.3"), grepl, NA, x = src, fixed = TRUE)))
  skip_if(!nzchar(Sys.which("Rscript")) && !file.exists(file.path(R.home("bin"), "Rscript")))
  rs <- file.path(R.home("bin"), "Rscript")
  w <- pci_work_dir(environment())
  b <- file.path(w, "prevcarga"); dir.create(b)
  dir.create(file.path(b, "code"), showWarnings = FALSE)
  tools_env <- new.env(parent = globalenv()); sys.source(file.path(root, "scripts", "tools", "bundle_manifest.R"), envir = tools_env)
  tools_env$pc_code_tree(root, file.path(b, "code"))                                  # o MESMO filtro do build do bundle
  ## RUNTIME.json minimo e valido (files[] com 1 arquivo)
  rj <- list(version = "0.0.0", platform = "linux-x86_64", r_version = "4.5.3", code_commit = strrep("a", 40),
             files = list(list(path = "code/renv.lock", sha256 = "x")))
  rj$files[[1L]]$sha256 <- digest::digest(file = file.path(b, "code", "renv.lock"), algo = "sha256")
  writeLines(jsonlite::toJSON(rj, auto_unbox = TRUE), file.path(b, "RUNTIME.json"))
  run <- function(extra = character()) {
    out <- suppressWarnings(system2(rs, c("--vanilla", shQuote(file.path(root, "tests", "windows", "release-check.R")), paste0("--repo=", root),
                                          paste0("--bundle-root=", b), "--platform=linux-x86_64", extra), stdout = TRUE, stderr = TRUE))
    list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"), out = out)
  }
  ok <- run()
  expect_identical(ok$status, 0L, info = paste(ok$out, collapse = "\n"))
  expect_true(any(grepl("^\\[PASS\\] python_free", ok$out)) && any(grepl("^\\[PASS\\] runtime_json", ok$out)))
  writeLines("print(1)", file.path(b, "code", "x.py"))
  bad <- run()
  expect_identical(bad$status, 1L)
  expect_true(any(grepl("^\\[FAIL\\] python_free.*code/x.py", bad$out)))
  unlink(file.path(b, "code", "x.py"))
  expect_identical(run("--expect-commit=bbbb")$status, 1L)                      # CONTROL: code_commit divergente reprova
})
