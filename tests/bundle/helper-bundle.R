## helper-bundle.R -- helper dos testes do bundle (M8.1; auto-carregado por testthat::test_dir). Nao e codigo de produto.
## Tudo em tempfile(): nunca toca data/, config/ nem o repo; o tarball candidato vem de dist/ (gitignored) e e opcional.

bb_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("bb_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}

## Ambiente com errors.R + runtime.R + bundle_manifest.R + bundle_gates.R (as funcoes reais do repo).
bb_env <- function() {
  root <- bb_root()
  lib <- file.path(root, "scripts", "lib")
  withr::local_options(prevcarga.lib = lib, .local_envir = parent.frame())
  e <- new.env(parent = globalenv())
  for (f in c(file.path(lib, "errors.R"), file.path(lib, "runtime.R"),
              file.path(root, "scripts", "tools", c("bundle_manifest.R", "bundle_gates.R"))))
    sys.source(f, envir = e)
  e
}

bb_rscript <- function() file.path(R.home("bin"), "Rscript")

## Stage sintetico de um bundle: <tmp>/prevcarga/{runtime/,syslib/,code/} com o code/ REAL (pc_code_tree) e um runtime/ de brinquedo.
## Devolve a raiz do bundle (onde mora RUNTIME.json).
bb_stage <- function(e, src = bb_root(), env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = env)
  root <- file.path(base, "prevcarga")
  dir.create(file.path(root, "code"), recursive = TRUE)
  e$pc_code_tree(src, file.path(root, "code"))
  dir.create(file.path(root, "runtime", "lib", "R", "etc"), recursive = TRUE)
  writeLines("R_LD_LIBRARY_PATH=${R_HOME}/lib:${R_HOME}/../../../syslib:/usr/lib", file.path(root, "runtime", "lib", "R", "etc", "ldpaths"))
  dir.create(file.path(root, "syslib"))
  writeLines("so-de-brinquedo", file.path(root, "syslib", "libtoy.so.1"))
  root
}

bb_write_runtime <- function(e, root, ...) {
  args <- list(root = root, version = "0.0.0-test", platform = "linux-x86_64", code_commit = strrep("a", 40),
               apt_snapshot = "20261027T000000Z")
  args[names(list(...))] <- list(...)
  utils::capture.output(id <- do.call(e$pc_runtime_json_write, args))
  id
}

## Candidato: dist/prevcarga-*.tar.zst ou, na falta, dist/repro-1/ (saida do build de reprodutibilidade).
bb_dist_tarball <- function() {
  fs <- c(Sys.glob(file.path(bb_root(), "dist", "prevcarga-*-linux-x86_64.tar.zst")),
          Sys.glob(file.path(bb_root(), "dist", "repro-1", "prevcarga-*-linux-x86_64.tar.zst")))
  if (length(fs)) sort(fs, method = "radix")[length(fs)] else NA_character_
}

## Extrai o tarball em tempdir (so se o host tem tar+zstd) e devolve <dir>/prevcarga.
bb_extract <- function(tarball, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  st <- system2("tar", c("--zstd", "-xf", shQuote(tarball), "-C", shQuote(d)), stdout = FALSE, stderr = FALSE)
  if (!identical(st, 0L)) stop("bb_extract: tar --zstd falhou")
  file.path(d, "prevcarga")
}

## Comparacao de locks: pacotes comuns com versao diferente e fora de `desvios` (lista de list(pacote, ...)).
bb_lock_offenders <- function(lock_new, lock_ref, desvios) {
  n <- lock_new$Packages; o <- lock_ref$Packages
  common <- intersect(names(n), names(o))
  diff <- common[vapply(common, function(p) !identical(n[[p]]$Version, o[[p]]$Version), NA)]
  listed <- vapply(desvios, function(d) d$pacote, "")
  sort(setdiff(diff, listed), method = "radix")
}
