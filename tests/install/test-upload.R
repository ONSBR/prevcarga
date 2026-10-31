## test-upload.R -- scripts/tools/upload (M8.5; ADR-0039, ADR-0040). So Linux (bash + `aws` falso); skip explicito no Windows.
## Um `aws` FALSO num diretorio temporario prefixado ao PATH do processo filho (log de chamadas + bucket simulado em disco): o teste
## nunca toca rede, bucket real nem credencial. Cobre: --list, recusa de chave versionada ja publicada SEM nenhum envio, regravacao so de
## latest.txt, queda de rede (exit 10) e o contrato de erro JSON (um unico documento em stdout, stderr vazio, error.id no catalogo
## GERADO). Confere por grep que so scripts/tools/upload (no codigo de produto) conhece o servico.

pci_up_skip <- function() {
  testthat::skip_on_os("windows")
  testthat::skip_if(!nzchar(Sys.which("bash")), "bash ausente")
}
UP <- file.path(pci_root(), "scripts", "tools", "upload")

## aws falso: bucket simulado = arquivos em <d>/bucket; calls.log = uma linha por chamada; <d>/net_down => falha de rede
fake_aws <- function(env = parent.frame(), bucket = character()) {
  d <- tempfile("fake-aws-"); dir.create(file.path(d, "bin"), recursive = TRUE); dir.create(file.path(d, "bucket"))
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  for (k in bucket) { p <- file.path(d, "bucket", k); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); writeLines("antigo", p) }
  writeLines(c("#!/usr/bin/env bash", sprintf("D='%s'", d), "printf '%s\\n' \"$*\" >> \"$D/calls.log\"",
    "[ -e \"$D/net_down\" ] && { echo 'Could not connect to the endpoint URL' >&2; exit 255; }",
    "key=''; prev=''; for a in \"$@\"; do [ \"$prev\" = --key ] && key=\"$a\"; prev=\"$a\"; done",
    "if [ \"$1\" = s3api ] && [ \"$2\" = list-objects-v2 ]; then",
    "  (cd \"$D/bucket\" && find . -type f | sed 's|^\\./||' | LC_ALL=C sort | tr '\\n' '\\t'); echo; exit 0; fi",
    "if [ \"$1\" = s3api ] && [ \"$2\" = head-object ]; then",
    "  [ -e \"$D/bucket/$key\" ] && { echo '{}'; exit 0; }",
    "  echo 'An error occurred (404) when calling the HeadObject operation: Not Found' >&2; exit 254; fi",
    "if [ \"$1\" = s3 ] && [ \"$2\" = cp ]; then",
    "  dst=\"${4#s3://}\"; dst=\"${dst#*/}\"; mkdir -p \"$(dirname \"$D/bucket/$dst\")\"; cp \"$3\" \"$D/bucket/$dst\"; exit 0; fi",
    "echo 'comando inesperado' >&2; exit 2"), file.path(d, "bin", "aws"))
  Sys.chmod(file.path(d, "bin", "aws"), "0755")
  list(dir = d, bin = file.path(d, "bin"),
       calls = function() if (file.exists(file.path(d, "calls.log"))) readLines(file.path(d, "calls.log"), warn = FALSE) else character(),
       keys = function() sort(list.files(file.path(d, "bucket"), recursive = TRUE, all.files = TRUE), method = "radix"),
       down = function(on = TRUE) if (on) writeLines("x", file.path(d, "net_down")) else unlink(file.path(d, "net_down")))
}

catalog_file <- local({
  f <- NULL
  function() {
    if (is.null(f)) {
      f <<- tempfile("catalog-", fileext = ".tsv")
      system2(pci_rscript(), c("--vanilla", shQuote(file.path(pci_root(), "scripts", "tools", "export_error_catalog.R")), "--out", shQuote(f)),
              stdout = FALSE, stderr = FALSE)
    }
    f
  }
})

## executa o upload com o aws falso no PATH do FILHO; devolve status, stdout, stderr
run_up <- function(fk, args, catalog = TRUE) {
  o <- tempfile(); e <- tempfile(); on.exit(unlink(c(o, e)))
  a <- c(args, if (isTRUE(catalog)) c("--catalog", catalog_file()))
  st <- suppressWarnings(system2("bash", c(shQuote(UP), shQuote(a)), stdout = o, stderr = e,
                                 env = paste0("PATH=", fk$bin, ":", dirname(pci_rscript()), ":", Sys.getenv("PATH"))))
  list(status = as.integer(st), stdout = readLines(o, warn = FALSE), stderr = readLines(e, warn = FALSE))
}
pj <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
cat_ids <- function() utils::read.delim(catalog_file(), sep = "\t", quote = "", colClasses = "character")$id

## diretorio no layout do release-pack
mk_dir <- function(files, env = parent.frame()) {
  d <- tempfile("up-dir-"); dir.create(d); withr::defer(unlink(d, recursive = TRUE), envir = env)
  for (f in files) { p <- file.path(d, f); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); writeLines(paste("conteudo", f), p) }
  d
}

test_that("--list imprime so as chaves versionadas (sem latest.txt), uma por linha; JSON: um documento", {
  pci_up_skip()
  fk <- fake_aws(bucket = c("releases/1.0.0/SHA256SUMS", "releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst", "releases/latest.txt",
                            "artifacts/combina/A1/latest.txt", "artifacts/combina/A1/v_0123456789ab/artifact.rds"))
  r <- run_up(fk, c("--bucket", "b", "--list"))
  expect_identical(r$status, 0L); expect_length(r$stderr, 0L)
  expect_identical(r$stdout, c("artifacts/combina/A1/v_0123456789ab/artifact.rds", "releases/1.0.0/SHA256SUMS",
                               "releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst"))
  j <- pj(run_up(fk, c("--bucket", "b", "--list", "--format", "json")))
  expect_true(j$ok); expect_identical(unlist(j$result$chaves), r$stdout)
  e <- run_up(fake_aws(), c("--bucket", "b", "--list", "--format=json"))                     # bucket vazio: lista vazia, exit 0
  expect_identical(e$status, 0L); expect_length(pj(e)$result$chaves, 0L)
})

test_that("[FORBIDDEN] chave versionada ja publicada -> exit 9 upload.key_exists SEM nenhuma chamada de envio", {
  pci_up_skip()
  fk <- fake_aws(bucket = "releases/1.0.0/SHA256SUMS")
  d <- mk_dir(c("releases/1.0.0/SHA256SUMS", "releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst", "releases/latest.txt"))
  r <- run_up(fk, c("--bucket", "b", d, "--format", "json"))
  expect_identical(r$status, 9L); expect_length(r$stderr, 0L); expect_length(r$stdout, 1L)
  j <- pj(r)
  expect_false(j$ok); expect_identical(j$exit_code, 9L); expect_identical(j$error$id, "upload.key_exists")
  expect_true(j$error$id %in% cat_ids()); expect_identical(j$error$class, "integrity")
  expect_identical(j$error$where, "releases/1.0.0/SHA256SUMS")
  expect_false(any(grepl("^s3 cp|put-object", fk$calls())))                                   # nenhum envio, nem das chaves novas
  expect_identical(fk$keys(), "releases/1.0.0/SHA256SUMS")
  tx <- run_up(fk, c("--bucket", "b", d))                                                     # texto: erro ASCII em stderr, stdout vazio
  expect_identical(tx$status, 9L); expect_length(tx$stdout, 0L)
  expect_identical(tx$stderr[[1L]], "prevcarga: erro [integrity] (exit 9)")
  expect_false(any(grepl("[^\\x01-\\x7F]", tx$stderr, perl = TRUE)))
})

test_that("[CONTROL] chaves novas sao enviadas (versionadas com Object Lock, latest.txt por ultimo e sem trava); SHA256SUMS da raiz nao sobe", {
  pci_up_skip()
  fk <- fake_aws(bucket = "releases/latest.txt")
  d <- mk_dir(c("SHA256SUMS", "releases/1.0.0/SHA256SUMS", "releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst", "releases/latest.txt"))
  r <- run_up(fk, c("--bucket", "b", d, "--format", "json"))
  expect_identical(r$status, 0L); expect_length(r$stderr, 0L); expect_length(r$stdout, 1L)
  j <- pj(r); expect_true(j$ok); expect_identical(j$result$enviadas, 2L); expect_identical(j$result$latest, 1L)
  expect_setequal(fk$keys(), c("releases/1.0.0/SHA256SUMS", "releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst", "releases/latest.txt"))
  cp <- grep("^s3 cp", fk$calls(), value = TRUE)
  expect_length(cp, 3L)
  expect_true(all(grepl("--object-lock-mode COMPLIANCE", cp[1:2], fixed = TRUE)))
  expect_match(cp[[3L]], "releases/latest.txt", fixed = TRUE); expect_false(grepl("object-lock", cp[[3L]], fixed = TRUE))
  expect_identical(readLines(file.path(fk$dir, "bucket", "releases", "latest.txt")), "conteudo releases/latest.txt")   # regravado
})

test_that("so latest.txt e regravado quando as demais chaves nao existem no pacote", {
  pci_up_skip()
  fk <- fake_aws(bucket = c("artifacts/lgbm/A1/latest.txt", "artifacts/lgbm/A1/v_0123456789ab/artifact.rds"))
  d <- mk_dir("artifacts/lgbm/A1/latest.txt")
  r <- run_up(fk, c("--bucket", "b", d, "--format=json"))
  expect_identical(r$status, 0L); expect_identical(pj(r)$result$enviadas, 0L)
  cp <- grep("^s3 cp", fk$calls(), value = TRUE)
  expect_length(cp, 1L); expect_match(cp, "artifacts/lgbm/A1/latest.txt", fixed = TRUE)
  expect_identical(readLines(file.path(fk$dir, "bucket", "artifacts/lgbm/A1/latest.txt")), "conteudo artifacts/lgbm/A1/latest.txt")
})

test_that("queda de rede -> exit 10 upload.network (list, head-object e envio), JSON unico e stderr vazio; nada sobrescrito", {
  pci_up_skip()
  fk <- fake_aws(); fk$down()
  d <- mk_dir("releases/1.0.0/SHA256SUMS")
  for (args in list(c("--bucket", "b", "--list"), c("--bucket", "b", d))) {
    r <- run_up(fk, c(args, "--format", "json"))
    expect_identical(r$status, 10L); expect_length(r$stderr, 0L); expect_length(r$stdout, 1L)
    j <- pj(r); expect_identical(j$error$id, "upload.network"); expect_true(j$error$id %in% cat_ids()); expect_identical(j$error$class, "network")
  }
  expect_length(fk$keys(), 0L)
  fk$down(FALSE)                                                    # CONTROL: com a rede de volta o mesmo comando passa
  expect_identical(run_up(fk, c("--bucket", "b", d, "--format", "json"))$status, 0L)
})

test_that("falha de rede DEPOIS do preflight (no envio) tambem e exit 10", {
  pci_up_skip()
  fk <- fake_aws()
  ## o aws falso passa a falhar no cp (head-object continua respondendo 404)
  s <- readLines(file.path(fk$bin, "aws"))
  writeLines(sub("^if \\[ \"\\$1\" = s3 \\] && \\[ \"\\$2\" = cp \\]; then$", "if [ \"$1\" = s3 ]; then echo 'Connect timeout' >&2; exit 255; fi; if false; then", s),
             file.path(fk$bin, "aws"))
  d <- mk_dir("releases/1.0.0/SHA256SUMS")
  r <- run_up(fk, c("--bucket", "b", d, "--format", "json"))
  expect_identical(r$status, 10L); expect_identical(pj(r)$error$id, "upload.network"); expect_length(r$stderr, 0L)
})

test_that("argumentos: sem --bucket, flag desconhecida, <dir> inexistente ou --list com <dir> -> exit 8 upload.usage", {
  pci_up_skip()
  fk <- fake_aws()
  for (args in list(c("--list"), c("--bucket", "b", "--nao-existe"), c("--bucket", "b", file.path(tempdir(), "nao-existe")),
                    c("--bucket", "b", "--list", tempdir()), c("--bucket"), c("--bucket", "b", "--format", "xml"))) {
    r <- run_up(fk, c(args, "--format=json"))
    expect_identical(r$status, 8L, info = paste(args, collapse = " "))
    expect_length(r$stderr, 0L); expect_identical(pj(r)$error$id, "upload.usage")
  }
  expect_length(fk$calls(), 0L)                                     # uso invalido nunca chega ao aws
})

test_that("o catalogo e GERADO em tmp quando --catalog nao e dado (nunca lido do repo); ids do upload constam do catalogo", {
  pci_up_skip()
  expect_true(all(c("upload.usage", "upload.key_exists", "upload.network") %in% cat_ids()))
  fk <- fake_aws(bucket = "releases/1.0.0/SHA256SUMS")
  d <- mk_dir("releases/1.0.0/SHA256SUMS")
  r <- run_up(fk, c("--bucket", "b", d, "--format", "json"), catalog = FALSE)
  expect_identical(r$status, 9L); expect_length(r$stderr, 0L)
  j <- pj(r)
  row <- utils::read.delim(catalog_file(), sep = "\t", quote = "", colClasses = "character")
  row <- row[row$id == "upload.key_exists", ]
  expect_identical(j$error$message, row$message); expect_identical(j$error$hint, row$hint)
  expect_false(file.exists(file.path(pci_root(), "install", "error-catalog.tsv")) && FALSE)   # nao e lido do repo: o script nao o cita
  src <- readLines(UP, warn = FALSE); src <- sub("^\\s*#.*$", "", src)
  expect_false(any(grepl("install/error-catalog", src, fixed = TRUE)))
})

test_that("so scripts/tools/upload conhece o servico de armazenamento no codigo de produto (grep)", {
  pat <- paste0("aws", " s3", "|AWS", "_PROFILE")
  prod <- c(Sys.glob(file.path(pci_root(), "scripts", "prevcarga")), Sys.glob(file.path(pci_root(), "scripts", "lib", "*.R")),
            Sys.glob(file.path(pci_root(), "scripts", "lib", "cli", "*.R")), Sys.glob(file.path(pci_root(), "scripts", "lib", "errors", "*.tsv")),
            Sys.glob(file.path(pci_root(), "install", "*")), Sys.glob(file.path(pci_root(), "config", "*")),
            Sys.glob(file.path(pci_root(), "models", "*", "R", "*.R")), Sys.glob(file.path(pci_root(), "scripts", "tools", "upload")))
  prod <- c(prod, list.files(file.path(pci_root(), "install"), recursive = TRUE, full.names = TRUE))   # install/tests/ (M8.4)
  prod <- unique(prod[!dir.exists(prod)])
  hit <- prod[vapply(prod, function(f) any(grepl(pat, readLines(f, warn = FALSE), perl = TRUE)), NA)]
  expect_identical(sub(paste0(pci_root(), "/"), "", hit, fixed = TRUE), "scripts/tools/upload")
  bait <- tempfile(fileext = ".R"); writeLines(paste0("x <- '", "aws", " s3 cp'"), bait)       # CONTROL: o grep acha uma isca
  expect_true(any(grepl(pat, readLines(bait), perl = TRUE)))
})

test_that("fallback embutido (catalogo ausente/vazio) tem o MESMO texto do catalogo gerado", {
  pci_up_skip()
  fk <- fake_aws(bucket = "k/x")
  empty <- tempfile(fileext = ".tsv"); writeLines("", empty)
  d <- mk_dir("k/x")
  r <- run_up(fk, c("--bucket", "b", d, "--format", "json", "--catalog", empty), catalog = FALSE)
  cat <- utils::read.delim(catalog_file(), sep = "\t", quote = "", colClasses = "character")
  for (id in c("upload.key_exists")) {
    row <- cat[cat$id == id, ]
    j <- pj(r)
    expect_identical(c(j$error$class, j$error$message, j$error$hint), c(row$class, row$message, row$hint))
  }
  fk$down(); r2 <- run_up(fk, c("--bucket", "b", d, "--format", "json", "--catalog", empty), catalog = FALSE)
  row <- cat[cat$id == "upload.network", ]
  expect_identical(c(pj(r2)$error$message, pj(r2)$error$hint), c(row$message, row$hint))
  r3 <- run_up(fk, c("--bucket", "b", "--format", "json", "--catalog", empty), catalog = FALSE)
  row <- cat[cat$id == "upload.usage", ]
  expect_identical(c(pj(r3)$error$message, pj(r3)$error$hint), c(row$message, row$hint))
})
