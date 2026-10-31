## test-utilities-error-contract.R -- gate TRANSVERSAL do contrato de erros dos utilitarios (M8.6; ADR-0040; AC 6 e 7 do ticket).
## Os 5 utilitarios fora do pacote R (install.sh, install.ps1, prevcarga-sync.sh, prevcarga-sync.ps1, scripts/tools/upload), falhando por
## argumento (8), hash (9) e rede (10), em json e em texto:
##   json  : stdout = UM JSON valido com ok=false, exit_code == codigo de saida, error.id presente no catalogo carregado por
##           `prevcarga_errors_load()` (uniao dos scripts/lib/errors/*.tsv), classe coerente; stderr vazio.
##   texto : 1a linha de stderr `prevcarga: erro [<classe>] (exit <n>)`; nenhum byte > 127 em stdout/stderr.
## Mais o teste de SINCRONIA do catalogo distribuido (catalogo gerado == uniao dos TSV == o do bundle candidato; todo id literal dos
## utilitarios consta nele; CONTROL: id ausente injetado reprova citando o id). Nada de rede real, bucket real ou credencial: servidor
## local de teste, `aws` falso e portas fechadas. Os .ps1 rodam sob powershell/pwsh quando existem; senao skip EXPLICITO por utilitario.

pce_skip_posix <- function() {
  testthat::skip_on_os(c("windows", "mac"))
  testthat::skip_if(!nzchar(Sys.which("curl")), "curl ausente no host")
}
pce_E <- local({ e <- NULL; function() { if (is.null(e)) { e <<- new.env(parent = globalenv()); options(prevcarga.lib = file.path(pci_root(), "scripts", "lib"))
  sys.source(file.path(pci_root(), "scripts", "lib", "errors.R"), envir = e) }; e } })
pce_catalog <- function() pce_E()$prevcarga_errors_load(dir = file.path(pci_root(), "scripts", "lib", "errors"), refresh = TRUE)

## roda `interp script args` sob env -i; devolve status/stdout/stderr (linhas)
pce_run <- function(interp, script, args, path = "/usr/local/bin:/usr/bin:/bin") {
  so <- tempfile(); se <- tempfile(); on.exit(unlink(c(so, se)))
  st <- suppressWarnings(system2("env", c("-i", paste0("PATH=", path), interp, shQuote(script), vapply(args, shQuote, "")), stdout = so, stderr = se, env = character()))
  list(status = as.integer(st), stdout = readLines(so, warn = FALSE), stderr = readLines(se, warn = FALSE),
       raw = c(readBin(so, "raw", file.size(so)), readBin(se, "raw", file.size(se))))
}

## confere o contrato de UMA celula (utilitario x causa x formato)
pce_check <- function(r, want_exit, cat, info) {
  testthat::expect_identical(r$status, want_exit, info = info)
  testthat::expect_true(all(r$raw < as.raw(128L)), info = paste(info, "(byte > 127)"))
}
pce_check_json <- function(r, want_exit, cat, info) {
  pce_check(r, want_exit, cat, info)
  testthat::expect_length(r$stderr, 0L)
  testthat::expect_length(r$stdout, 1L)                                   # UM unico documento
  testthat::expect_true(jsonlite::validate(paste(r$stdout, collapse = "\n")), info = info)
  j <- jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
  testthat::expect_false(j$ok, info = info)
  testthat::expect_identical(j$exit_code, want_exit, info = info)
  testthat::expect_true(j$error$id %in% cat$id, info = paste(info, "id fora do catalogo:", j$error$id))
  row <- cat[cat$id == j$error$id, ]
  testthat::expect_identical(j$error$class, row$class[[1L]], info = info)
  invisible(j)
}
pce_check_text <- function(r, want_exit, cat, info) {
  pce_check(r, want_exit, cat, info)
  testthat::expect_identical(r$stdout, character(), info = info)
  cl <- cat$class[match(want_exit, vapply(cat$class, function(k) pce_E()$prevcarga_exit_codes[[k]], 0L))]
  testthat::expect_identical(r$stderr[[1L]], sprintf("prevcarga: erro [%s] (exit %d)", cl, want_exit), info = info)
}

## cada utilitario: funcao(causa, formato) -> resultado de pce_run; causa em arg / hash / rede
pce_matrix <- function(runner, cat, name) {
  want <- c(arg = 8L, hash = 9L, rede = 10L)
  for (causa in names(want)) for (fmt in c("json", "text")) {
    r <- runner(causa, fmt)
    info <- paste(name, causa, fmt)
    if (fmt == "json") pce_check_json(r, want[[causa]], cat, info) else pce_check_text(r, want[[causa]], cat, info)
  }
}
fmt_args <- function(fmt) if (fmt == "json") c("--format", "json") else character()

test_that("[install.sh] argumento (8), hash (9) e rede (10), em json e em texto", {
  pce_skip_posix()
  cat <- pce_catalog(); pci_ensure_catalog(); d <- pci_work_dir()
  fake <- file.path(d, "bundle.tar.zst"); writeLines("nao e um bundle", fake)
  runner <- function(causa, fmt) {
    a <- switch(causa,
      arg = c("--bogus"),
      hash = c("--bundle", fake, "--sha256", strrep("0", 64L), "--root", file.path(d, "R"), "--skip-platform-check"),
      rede = c("--release-base", pc_dead_url(), "--version", "1.0.0", "--root", file.path(d, "R2"), "--skip-platform-check"))
    pce_run("sh", pci_installer(), c(a, fmt_args(fmt)))
  }
  pce_matrix(runner, cat, "install.sh")
})

test_that("[prevcarga-sync.sh] argumento (8), hash (9) e rede (10), em json e em texto", {
  pce_skip_posix()
  testthat::skip_if(!requireNamespace("httpuv", quietly = TRUE), "httpuv ausente")
  cat <- pce_catalog(); sh <- pc_sync_install(); b <- pc_fixture_bucket(forecasts = FALSE)
  sv <- pc_serve_bucket(b$dir, "tamper"); S <- file.path(pci_work_dir(), "S")
  runner <- function(causa, fmt) {
    base <- switch(causa, arg = c("--bogus"), hash = c("--what", "artifacts", "--release-base", sv$url, "--state", S),
                   rede = c("--what", "artifacts", "--release-base", pc_dead_url(), "--state", S))
    pce_run("sh", sh, c(base, fmt_args(fmt)))
  }
  pce_matrix(runner, cat, "prevcarga-sync.sh")
})

test_that("[scripts/tools/upload] argumento (8), chave existente/hash (9) e rede (10), em json e em texto (aws FALSO)", {
  pce_skip_posix()
  testthat::skip_if(!nzchar(Sys.which("bash")), "bash ausente")
  cat <- pce_catalog(); d <- pci_work_dir()
  catf <- file.path(d, "error-catalog.tsv")
  pci_sh(sprintf("cd %s && %s --vanilla scripts/tools/export_error_catalog.R --out %s", shQuote(pci_root()), shQuote(pci_rscript()), shQuote(catf)))
  bin <- file.path(d, "bin"); dir.create(bin); dir.create(file.path(d, "bucket"))
  writeLines(c("#!/usr/bin/env bash", sprintf("D='%s'", d),
    "[ -e \"$D/net_down\" ] && { echo 'Could not connect to the endpoint URL' >&2; exit 255; }",
    "key=''; prev=''; for a in \"$@\"; do [ \"$prev\" = --key ] && key=\"$a\"; prev=\"$a\"; done",
    "if [ \"$1\" = s3api ] && [ \"$2\" = head-object ]; then",
    "  [ -e \"$D/bucket/$key\" ] && { echo '{}'; exit 0; }",
    "  echo 'An error occurred (404) when calling the HeadObject operation: Not Found' >&2; exit 254; fi",
    "echo 'comando inesperado' >&2; exit 2"), file.path(bin, "aws"))
  Sys.chmod(file.path(bin, "aws"), "0755")
  pkg <- file.path(d, "pkg"); dir.create(file.path(pkg, "releases", "1.0.0"), recursive = TRUE); writeLines("x", file.path(pkg, "releases", "1.0.0", "a.bin"))
  dir.create(file.path(d, "bucket", "releases", "1.0.0"), recursive = TRUE); writeLines("x", file.path(d, "bucket", "releases", "1.0.0", "a.bin"))
  up <- file.path(pci_root(), "scripts", "tools", "upload")
  runner <- function(causa, fmt) {
    a <- switch(causa, arg = c("--bogus"), hash = c("--bucket", "b", "--catalog", catf, pkg), rede = c("--bucket", "b", "--catalog", catf, pkg))
    if (causa == "arg") a <- c(a, "--catalog", catf)
    if (causa == "rede") writeLines("x", file.path(d, "net_down")) else unlink(file.path(d, "net_down"))
    pce_run("bash", up, c(a, fmt_args(fmt)), path = paste0(bin, ":/usr/local/bin:/usr/bin:/bin"))
  }
  pce_matrix(runner, cat, "upload")
})

test_that("[install.ps1 e prevcarga-sync.ps1] argumento (8) em json e em texto (so com pwsh/powershell; senao skip EXPLICITO)", {
  shell <- c(Sys.which("pwsh"), Sys.which("powershell"))
  shell <- shell[nzchar(shell)]
  testthat::skip_if(length(shell) == 0L, "[DEFERRED M8.6 / owner: Gabriel, host Windows ou com pwsh 7] pwsh/powershell ausentes nesta maquina de dev: matriz dos .ps1 nao executada")
  cat <- pce_catalog(); sh <- shell[[1L]]
  ps <- function(script, args) {
    so <- tempfile(); se <- tempfile(); on.exit(unlink(c(so, se)))
    st <- suppressWarnings(system2(sh, c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(script), vapply(args, shQuote, "")), stdout = so, stderr = se))
    list(status = as.integer(st), stdout = readLines(so, warn = FALSE), stderr = readLines(se, warn = FALSE), raw = c(readBin(so, "raw", file.size(so)), readBin(se, "raw", file.size(se))))
  }
  for (u in c("install.ps1", "prevcarga-sync.ps1")) for (fmt in c("json", "text")) {
    r <- ps(file.path(pci_root(), "install", u), c("--bogus", fmt_args(fmt)))
    if (fmt == "json") pce_check_json(r, 8L, cat, paste(u, "arg", fmt)) else pce_check_text(r, 8L, cat, paste(u, "arg", fmt))
  }
})

## --- sincronia do catalogo distribuido (AC 7) ---------------------------------------------------------------------------------
UTILS <- function() file.path(pci_root(), c("install/install.sh", "install/install.ps1", "install/prevcarga-sync.sh", "install/prevcarga-sync.ps1", "scripts/tools/upload"))

## ids literais usados por um utilitario (chamadas pc_die / Write-PcError)
pce_used_ids <- function(txt) {
  m <- c(regmatches(txt, gregexpr("pc_die\\s+\"?([a-z]+\\.[a-z_]+)", txt, perl = TRUE))[[1L]],
         regmatches(txt, gregexpr("Write-PcError\\s+'([a-z]+\\.[a-z_]+)'", txt, perl = TRUE))[[1L]])
  sort(unique(sub("^.*?([a-z]+\\.[a-z_]+)\"?'?$", "\\1", m, perl = TRUE)), method = "radix")
}

test_that("catalogo gerado == uniao ordenada por id dos scripts/lib/errors/*.tsv; todo id literal dos 5 utilitarios consta nele", {
  tmp <- tempfile("cat-", fileext = ".tsv"); on.exit(unlink(tmp))
  pci_sh(sprintf("cd %s && %s --vanilla scripts/tools/export_error_catalog.R --out %s", shQuote(pci_root()), shQuote(pci_rscript()), shQuote(tmp)))
  gen <- readBin(tmp, "raw", file.size(tmp))
  ## uniao independente: cada TSV lido a cru, linhas ordenadas por id (radix), exit_code pela tabela unica de errors.R
  ex <- pce_E()$prevcarga_exit_codes
  rows <- do.call(rbind, lapply(Sys.glob(file.path(pci_root(), "scripts", "lib", "errors", "*.tsv")), function(f)
    utils::read.delim(f, sep = "\t", quote = "", comment.char = "", colClasses = "character")))
  rows <- rows[order(rows$id, method = "radix"), ]
  want <- c(paste(c("id", "class", "exit_code", "message", "hint"), collapse = "\t"),
            paste(rows$id, rows$class, as.integer(ex[rows$class]), rows$message, rows$hint, sep = "\t"))
  expect_identical(gen, charToRaw(paste0(paste(want, collapse = "\n"), "\n")))
  expect_false(any(duplicated(rows$id)))
  ids <- rows$id
  for (u in UTILS()) {
    used <- pce_used_ids(paste(readLines(u, warn = FALSE), collapse = "\n"))
    expect_gt(length(used), 0L)
    expect_identical(setdiff(used, ids), character(), info = basename(u))
  }
  ## o catalogo carregado por prevcarga_errors_load() e o mesmo conjunto de ids
  expect_setequal(pce_catalog()$id, ids)
  ## o catalogo versionado nao existe: so o GERADO (gitignored)
  expect_false(file.exists(file.path(pci_root(), "scripts", "lib", "errors", "error-catalog.tsv")))
})

test_that("[CONTROL] um error.id ausente do catalogo, injetado numa copia de um utilitario, reprova citando o id", {
  ids <- pce_catalog()$id
  for (u in UTILS()) {
    txt <- paste(readLines(u, warn = FALSE), collapse = "\n")
    inj <- if (grepl("\\.ps1$", u)) paste0(txt, "\nWrite-PcError 'sync.fantasma' 'x'\n") else paste0(txt, "\npc_die sync.fantasma x\n")
    miss <- setdiff(pce_used_ids(inj), ids)
    expect_identical(miss, "sync.fantasma", info = basename(u))                # o detector TEM dentes e cita o id
  }
})

test_that("catalogo gerado == o do bundle candidato de dist/ (code/install/error-catalog.tsv); sem candidato, skip explicito", {
  testthat::skip_on_os(c("windows", "mac"))
  cand <- pci_candidate()
  testthat::skip_if(is.na(cand) || !nzchar(Sys.which("zstd")), "[DEFERRED M8.6 / owner: Gabriel, build do bundle em jammy] sem bundle candidato em dist/: sincronia com o catalogo do bundle nao executada")
  d <- pci_work_dir()
  pci_sh(sprintf("zstd -dc %s | tar -x -C %s --wildcards '*/code/install/error-catalog.tsv'", shQuote(cand), shQuote(d)))
  f <- Sys.glob(file.path(d, "*", "code", "install", "error-catalog.tsv"))
  expect_length(f, 1L)
  tmp <- tempfile("cat-", fileext = ".tsv"); on.exit(unlink(tmp))
  pci_sh(sprintf("cd %s && %s --vanilla scripts/tools/export_error_catalog.R --out %s", shQuote(pci_root()), shQuote(pci_rscript()), shQuote(tmp)))
  expect_identical(readBin(tmp, "raw", file.size(tmp)), readBin(f, "raw", file.size(f)))
})
