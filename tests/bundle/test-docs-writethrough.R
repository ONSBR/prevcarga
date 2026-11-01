## test-docs-writethrough.R -- write-through do encerramento do M8 (M8.12; AC 3): grep sobre docs/estado/m8-bundle.md, README.md e
## docs/reference/MANUAL-USUARIO.md. Verbos citados vem da lista DESCOBERTA de scripts/lib/cli/*.R (nunca escrita aqui); --origin e o unico flag
## de data; zero PREVCARGA_; WSL2 nunca e requisito; todo `id` de tests/ab/ab_excecoes.yaml aparece; topicos obrigatorios nos tres arquivos.
## CONTROL: um verbo fora da lista, um flag de data, PREVCARGA_, WSL2 como requisito e um id removido plantados numa copia reprovam.
## O roadmap M8-BUNDLE.md so some depois do recibo `ab-corte` CORTE-OK dos DOIS bundles (AC 3).

dw_root <- bb_root()
DW_FILES <- c("docs/estado/m8-bundle.md", "README.md", "docs/reference/MANUAL-USUARIO.md")

dw_read <- function(f, root = dw_root) readLines(file.path(root, f), warn = FALSE, encoding = "UTF-8")

## Verbos canonicos: o loader real (cli.R) sobre scripts/lib/cli/*.R
dw_verbs <- function() {
  lib <- file.path(dw_root, "scripts", "lib")
  withr::local_options(prevcarga.lib = lib, .local_envir = parent.frame())
  e <- new.env(parent = globalenv())
  for (f in c("errors", "runtime", "config", "banner", "cli")) sys.source(file.path(lib, paste0(f, ".R")), envir = e)
  sort(unique(e$prevcarga_verbs_discover()$verb), method = "radix")
}

## Trechos de codigo: spans `...` e blocos cercados por ```; o resto e prosa (nao e grep de verbo).
dw_code <- function(lines) {
  fence <- grepl("^\\s*```", lines)
  inblk <- cumsum(fence) %% 2L == 1L & !fence
  spans <- unlist(regmatches(lines[!inblk & !fence], gregexpr("`[^`]+`", lines[!inblk & !fence])))
  c(lines[inblk], gsub("`", "", spans))
}

dw_cited_verbs <- function(lines) {
  m <- unlist(regmatches(dw_code(lines), gregexpr("\\bprevcarga\\s+[a-z][a-z0-9-]*", dw_code(lines), perl = TRUE)))
  sort(unique(sub("^prevcarga\\s+", "", m)), method = "radix")
}

dw_flags <- function(lines) sort(unique(unlist(regmatches(dw_code(lines), gregexpr("(?<![A-Za-z0-9])--[A-Za-z][A-Za-z0-9-]*", dw_code(lines), perl = TRUE)))), method = "radix")
DW_DATE_FLAG <- "^--(as-of|asof|date|dia|day|since|until|from|to|inicio|fim|start|end|data-inicio|data-fim|data-ini|corte-data|ate|de)$"

dw_wsl_required <- function(lines) {
  l <- lines[grepl("WSL2?", lines, perl = TRUE)]
  l[!grepl("n[aã]o (e|é|exige|precisa|requer)|sem WSL|dispensa|nunca", l, perl = TRUE, ignore.case = TRUE)]
}

dw_ids <- function() {
  l <- readLines(file.path(dw_root, "tests", "ab", "ab_excecoes.yaml"), warn = FALSE)
  sub("^\\s*-\\s*id:\\s*", "", grep("^\\s*-\\s*id:\\s*", l, value = TRUE))
}

DW_TOPICS <- c("install.sh", "install.ps1", "forecast", "prevcarga-sync", "verificar", "--oficial", "release-pack", "upload",
               "6 (bundle)", "8 (config)", "9 (integrity)", "10 (network)", "12 (repro)", "13 (state)")

## Todas as violacoes de um arquivo (vazio = PASS)
dw_violations <- function(lines, verbs, ids) {
  v <- list()
  bad_verbs <- setdiff(dw_cited_verbs(lines), verbs)
  if (length(bad_verbs)) v$verbo_fora_da_lista <- bad_verbs
  fl <- dw_flags(lines); bad_fl <- fl[grepl(DW_DATE_FLAG, fl, perl = TRUE)]
  if (length(bad_fl)) v$flag_de_data <- bad_fl
  if (any(grepl("PREVCARGA_", lines, fixed = TRUE))) v$prevcarga_env <- "PREVCARGA_"
  old <- unlist(regmatches(lines, gregexpr("\\b(morning|prever|treinar)\\b|--as-of", lines, perl = TRUE)))
  if (length(old)) v$vocabulario_antigo <- unique(old)
  w <- dw_wsl_required(lines); if (length(w)) v$wsl2_requisito <- w
  txt <- paste(lines, collapse = "\n")
  miss_id <- ids[!vapply(ids, grepl, NA, x = txt, fixed = TRUE)]; if (length(miss_id)) v$id_ausente <- miss_id
  miss_tp <- DW_TOPICS[!vapply(DW_TOPICS, grepl, NA, x = txt, fixed = TRUE)]; if (length(miss_tp)) v$topico_ausente <- miss_tp
  non_ascii <- which(vapply(lines, function(s) any(utf8ToInt(s) > 127L), NA))
  if (length(non_ascii)) v$nao_ascii <- non_ascii
  v
}

test_that("[AC] a lista de verbos vem do loader de scripts/lib/cli/*.R e traz os verbos do M8", {
  vs <- dw_verbs()
  expect_gt(length(vs), 15L)
  expect_true(all(c("forecast", "verificar", "release-pack", "doctor") %in% vs))
  expect_false(any(c("morning", "prever", "treinar") %in% vs))
})

for (f in DW_FILES) {
  test_that(sprintf("[AC] write-through %s: verbos canonicos, so --origin como flag de data, zero PREVCARGA_, WSL2 nao e requisito, ids do A/B e topicos presentes", f), {
    expect_true(file.exists(file.path(dw_root, f)))
    v <- dw_violations(dw_read(f), dw_verbs(), dw_ids())
    expect_identical(names(v), NULL, info = paste(names(v), vapply(v, function(x) paste(x, collapse = ","), ""), collapse = " | "))
  })
}

test_that("[AC] ab_excecoes.yaml tem as 3 entradas e todas citadas (nenhuma entrada acrescentada aqui)", {
  expect_setequal(dw_ids(), c("regdin.versao_pacotes", "dessem_linear.hora_incompleta_clima_previsto", "dessem_linear.sp_2024_11_15_residuo_numerico"))
})

test_that("[AC] --origin e citado como o flag de data nos docs de usuario", {
  for (f in c("README.md", "docs/reference/MANUAL-USUARIO.md")) expect_true("--origin" %in% dw_flags(dw_read(f)), info = f)
})

test_that("[CONTROL] o grep acusa: verbo fora da lista, flag de data, PREVCARGA_, WSL2 como requisito, id removido, topico removido, vocabulario antigo", {
  vs <- dw_verbs(); ids <- dw_ids()
  base <- dw_read("README.md")
  expect_identical(names(dw_violations(base, vs, ids)), NULL)
  expect_identical(dw_violations(c(base, "Rode `prevcarga prever --origin 2026-10-15` hoje."), vs, ids)$verbo_fora_da_lista, "prever")
  expect_identical(dw_violations(c(base, "```", "prevcarga treinar --x", "```"), vs, ids)$verbo_fora_da_lista, "treinar")
  expect_identical(dw_violations(c(base, "Use `prevcarga forecast --as-of 2026-10-15`."), vs, ids)$flag_de_data, "--as-of")
  expect_identical(dw_violations(c(base, "Use `prevcarga forecast --date 2026-10-15`."), vs, ids)$flag_de_data, "--date")
  expect_true("PREVCARGA_" %in% dw_violations(c(base, "defina PREVCARGA_DATA"), vs, ids)$prevcarga_env)
  expect_identical(dw_violations(c(base, "Requer WSL2 instalado."), vs, ids)$wsl2_requisito, "Requer WSL2 instalado.")
  expect_identical(names(dw_violations(c(base, "WSL2 nao e requisito."), vs, ids)), NULL)
  expect_identical(dw_violations(sub("regdin.versao_pacotes", "x", base, fixed = TRUE), vs, ids)$id_ausente, "regdin.versao_pacotes")
  expect_identical(dw_violations(gsub("install.ps1", "inst", base, fixed = TRUE), vs, ids)$topico_ausente, "install.ps1")
  expect_true("morning" %in% dw_violations(c(base, "rodada morning"), vs, ids)$vocabulario_antigo)
  ## prosa nao e verbo: `prevcarga e` fora de codigo nao conta; `prevcarga-sync` nao e `prevcarga <verbo>`
  expect_identical(dw_cited_verbs(c("o prevcarga e um CLI", "`prevcarga-sync --what artifacts`")), character())
})

test_that("[AC] roadmap do M8 so some depois do recibo ab-corte CORTE-OK dos dois bundles (senao a copia arquivada existe)", {
  rd <- file.path(dw_root, "tests", ".receipts")
  ok <- character()
  for (f in Sys.glob(file.path(rd, "*.ab-corte.json"))) {
    r <- jsonlite::read_json(f)
    if (identical(r$veredito, "CORTE-OK") && identical(r$resultado, "PASS")) ok <- c(ok, r$platform)
  }
  corte_ok <- all(c("linux-x86_64", "windows-x86_64") %in% ok)
  existe <- file.exists(file.path(dw_root, "docs", "roadmap", "M8-BUNDLE.md")) || file.exists(file.path(dw_root, "docs", "_archive", "roadmap", "M8-BUNDLE.md"))
  expect_true(corte_ok || existe, info = "roadmap M8 removido sem recibo ab-corte CORTE-OK nos dois bundles")
})
