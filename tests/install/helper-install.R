## helper-install.R -- helper dos testes do instalador Linux (M8.3; auto-carregado por testthat::test_dir). Nao e codigo de produto.
## Tudo em sandbox/ (gitignored, MESMO disco do repo: um bundle extraido tem ~500 MB; nunca em /tmp) e apagado ao fim.
## Nunca toca ~/.local, ~/.profile, ~/.Renviron nem os symlinks do R: raiz, perfil e HOME dos testes sao diretorios temporarios.
## O bundle candidato vem de dist/ (gitignored): dist/prevcarga-*-linux-x86_64.tar.zst ou dist/<sub>/prevcarga-*.tar.zst.
## Os bracos que dependem dele fazem skip com motivo (pci_skip_unless_ready), nunca verde silencioso.

pci_root <- function() {
  d <- normalizePath(tryCatch(testthat::test_path(), error = function(e) getwd()), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("pci_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}

pci_rscript <- function() file.path(R.home("bin"), "Rscript")

## --- diretorios de trabalho --------------------------------------------------------------------------------------
pci_work_dir <- function(env = parent.frame(), prefix = "pc-install-") {
  base <- file.path(pci_root(), "sandbox")
  dir.create(base, showWarnings = FALSE, recursive = TRUE)
  d <- tempfile(prefix, tmpdir = base)
  dir.create(d, recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE, force = TRUE), envir = env)
  normalizePath(d, winslash = "/")
}

## Raiz de instalacao temporaria (nao criada: o instalador a cria).
pc_fixture_install_root <- function(env = parent.frame()) file.path(pci_work_dir(env), "R")

## --- candidato e base ---------------------------------------------------------------------------------------------
pci_candidate <- function() {
  fs <- c(Sys.glob(file.path(pci_root(), "dist", "prevcarga-*-linux-x86_64.tar.zst")),
          Sys.glob(file.path(pci_root(), "dist", "*", "prevcarga-*-linux-x86_64.tar.zst")))
  if (length(fs)) sort(fs, method = "radix")[length(fs)] else NA_character_
}

pci_have_tools <- function() all(nzchar(Sys.which(c("zstd", "tar", "sha256sum", "cp", "sh"))))

pci_skip_unless_ready <- function() {
  testthat::skip_on_os(c("windows", "mac"))
  if (!pci_have_tools()) testthat::skip("zstd/tar/sha256sum ausentes no host")
  if (is.na(pci_candidate())) testthat::skip("sem bundle candidato em dist/ (rode scripts/tools/build_bundle_linux.sh)")
  invisible(TRUE)
}

pci_sh <- function(cmd) {
  out <- system2("sh", c("-c", shQuote(cmd)), stdout = TRUE, stderr = TRUE)
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  if (st != 0L) stop("pci_sh falhou (", st, "): ", cmd, "\n", paste(out, collapse = "\n"), call. = FALSE)
  invisible(out)
}

.pci_state <- new.env(parent = emptyenv())

## Libs pesadas que o instalador nao exercita (BH 150 MB, arrow 65 MB...): a variante `slim` as remove para os testes de
## ciclo de vida ficarem rapidos. O teste do shell sujo usa o bundle COMPLETO (slim = FALSE).
.pci_slim_drop <- c("BH", "arrow", "ggplot2", "translations", "survival", "Matrix", "RcppArmadillo", "lightgbm", "mgcv")

## Escreve o RUNTIME.json do stage (reusa as funcoes REAIS do repo: bundle_manifest.R + runtime.R).
pci_env <- function() {
  if (!is.null(.pci_state$env)) return(.pci_state$env)
  lib <- file.path(pci_root(), "scripts", "lib")
  options(prevcarga.lib = lib)
  e <- new.env(parent = globalenv())
  for (f in c(file.path(lib, c("errors.R", "runtime.R")), file.path(pci_root(), "scripts", "tools", "bundle_manifest.R")))
    sys.source(f, envir = e)
  .pci_state$env <- e
}

## Base extraida (memoizada por sessao): bundle candidato + code/ do WORKTREE sobreposto (assim o teste exercita o codigo
## deste ticket, nao o do commit que gerou o tarball) + catalogo gerado + lancador. Devolve <dir>/prevcarga.
pci_base <- function(slim = TRUE) {
  key <- if (slim) "slim" else "full"
  if (!is.null(.pci_state[[key]]) && dir.exists(.pci_state[[key]])) return(.pci_state[[key]])
  e <- pci_env()
  d <- file.path(pci_root(), "sandbox", paste0("pc-install-base-", key, "-", Sys.getpid()))
  unlink(d, recursive = TRUE, force = TRUE); dir.create(d, recursive = TRUE)
  pci_sh(sprintf("zstd -dc %s | tar -x -C %s", shQuote(pci_candidate()), shQuote(d)))
  root <- file.path(d, "prevcarga")
  if (slim) {
    unlink(file.path(root, "runtime", "lib", "R", "library", .pci_slim_drop), recursive = TRUE, force = TRUE)
  }
  unlink(file.path(root, "code"), recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "code", "install"), recursive = TRUE)
  e$pc_code_tree(pci_root(), file.path(root, "code"))
  pci_sh(sprintf("cd %s && %s --vanilla scripts/tools/export_error_catalog.R --out %s", shQuote(pci_root()), shQuote(pci_rscript()),
                 shQuote(file.path(root, "code", "install", "error-catalog.tsv"))))
  file.copy(file.path(pci_root(), "install", "prevcarga"), file.path(root, "code", "install", "prevcarga"))
  Sys.chmod(file.path(root, "code", "install", "prevcarga"), "0755")
  unlink(file.path(root, "RUNTIME.json"))
  utils::capture.output(e$pc_runtime_json_write(root, version = "base", platform = "linux-x86_64", code_commit = strrep("a", 40)))
  .pci_state[[key]] <- root
  reg.finalizer(.pci_state, function(s) unlink(d, recursive = TRUE, force = TRUE), onexit = TRUE)
  root
}

## --- bundles de teste -----------------------------------------------------------------------------------------------
## Reempacota a base com outra versao (config/VERSION + RUNTIME.json; `prevcarga --version` imprime a versao).
##   corrupt = "none"      bundle integro
##   corrupt = "manifest"  um files[].sha256 errado no RUNTIME.json (o sha256 do TARBALL continua batendo)
##   corrupt = "truncate"  tarball cortado ao meio (sha256 calculado sobre o arquivo cortado: confere, extracao falha)
## Devolve list(path, sha256, version). O arquivo se chama prevcarga-<version>-linux-x86_64.tar.zst.
pc_fixture_bundle <- function(version, corrupt = c("none", "manifest", "truncate"), slim = TRUE, env = parent.frame()) {
  corrupt <- match.arg(corrupt)
  e <- pci_env()
  base <- pci_base(slim)
  d <- pci_work_dir(env, "pc-bundle-")
  root <- file.path(d, "prevcarga")
  pci_sh(sprintf("cp -al %s %s", shQuote(base), shQuote(root)))   # hardlinks: nunca escreva IN PLACE, sempre unlink + grave
  vf <- file.path(root, "code", "config", "VERSION")
  unlink(vf); writeLines(version, vf)
  rj <- file.path(root, "RUNTIME.json")
  x <- jsonlite::read_json(rj, simplifyVector = FALSE)
  x$version <- version
  paths <- vapply(x$files, function(f) f$path, "")
  x$files[[match("code/config/VERSION", paths)]]$sha256 <- digest::digest(file = vf, algo = "sha256")
  if (identical(corrupt, "manifest")) {
    i <- match("code/config/topology.yaml", paths)
    s <- x$files[[i]]$sha256
    x$files[[i]]$sha256 <- paste0(if (substr(s, 1L, 1L) == "0") "1" else "0", substring(s, 2L))
  }
  unlink(rj)
  con <- file(rj, open = "wb"); writeBin(charToRaw(e$prevcarga_runtime_json_canonical(x, with_built_at = TRUE)), con); close(con)
  out <- file.path(d, sprintf("prevcarga-%s-linux-x86_64.tar.zst", version))
  pci_sh(sprintf("tar --sort=name --owner=0 --group=0 --numeric-owner -C %s -cf - prevcarga | zstd -3 -T0 -q -f -o %s",
                 shQuote(d), shQuote(out)))
  unlink(root, recursive = TRUE, force = TRUE)
  if (identical(corrupt, "truncate")) {
    sz <- file.size(out); b <- readBin(out, "raw", sz %/% 2L)
    unlink(out); writeBin(b, out)
  }
  list(path = out, sha256 = digest::digest(file = out, algo = "sha256"), version = version)
}

## --- arvore ------------------------------------------------------------------------------------------------------------
## sha256 da listagem ordenada (radix) `caminho  sha256-do-arquivo` de TODA a arvore (arquivos e diretorios vazios);
## arvore ausente = sha da listagem vazia. Base dos FORBIDDEN de byte-identidade.
pc_tree_sha <- function(dir) {
  if (!dir.exists(dir)) return(digest::digest("", algo = "sha256", serialize = FALSE))
  rel <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE), method = "radix")
  line <- vapply(rel, function(p) {
    f <- file.path(dir, p)
    if (dir.exists(f)) paste0(p, "/") else paste(p, digest::digest(file = f, algo = "sha256"))
  }, "")
  digest::digest(paste(line, collapse = "\n"), algo = "sha256", serialize = FALSE)
}

## --- instalador -----------------------------------------------------------------------------------------------------------
pci_installer <- function() file.path(pci_root(), "install", "install.sh")

## Garante install/error-catalog.tsv (gerado, gitignored) para o install.sh do repo e o devolve.
pci_ensure_catalog <- function() {
  out <- file.path(pci_root(), "install", "error-catalog.tsv")
  pci_sh(sprintf("cd %s && %s --vanilla scripts/tools/export_error_catalog.R --out %s", shQuote(pci_root()), shQuote(pci_rscript()), shQuote(out)))
  out
}

## Roda o instalador (ou uma variante) com o ambiente LIMPO (env -i): so PATH minimo. profile/HOME sao temporarios.
## Devolve list(status, stdout, stderr).
pci_run <- function(script, args, home = NULL, cwd = NULL) {
  so <- tempfile(); se <- tempfile(); on.exit(unlink(c(so, se)))
  envs <- c("PATH=/usr/local/bin:/usr/bin:/bin", if (!is.null(home)) paste0("HOME=", home))
  st <- system2("env", c("-i", envs, "sh", shQuote(script), vapply(args, shQuote, "")),
                stdout = so, stderr = se, env = character())
  list(status = as.integer(st), stdout = paste(readLines(so, warn = FALSE), collapse = "\n"),
       stderr = paste(readLines(se, warn = FALSE), collapse = "\n"))
}

pc_install <- function(args, home = NULL, script = pci_installer()) pci_run(script, args, home = home)

## Variante de TESTE do instalador que troca `current` ANTES de validar o manifesto (CONTROL do AC 2: o teste de
## "manifesto ruim => current intacto" precisa reprovar nesta variante). Copia install.sh + lancador + catalogo para um dir novo.
pc_installer_variant_swap_first <- function(env = parent.frame()) {
  d <- pci_work_dir(env, "pc-variant-")
  src <- readLines(pci_installer())
  i <- grep("^env -u R_HOME \"\\$STAGE/runtime/bin/Rscript\"", src)
  stopifnot(length(i) == 1L)
  out <- append(src, c("mkdir -p \"$ROOT/versions/$VER\" && pc_swap_file \"$ROOT/current\" \"$VER\"   # VARIANTE DE TESTE: troca antes de validar"), after = i - 1L)
  f <- file.path(d, "install.sh")
  writeLines(out, f)
  file.copy(file.path(pci_root(), "install", c("prevcarga", "error-catalog.tsv")), d)
  f
}

pci_json <- function(txt) jsonlite::fromJSON(txt, simplifyVector = FALSE)
