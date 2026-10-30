## bundle_manifest.R -- ferramenta de DEV (fora do bundle), compartilhada pelos builds Linux e Windows (M8.1).
## Funcoes: pc_code_tree (code/ do bundle) e pc_runtime_json_write (RUNTIME.json). O formato canonico e o runtime_id
## vem de scripts/lib/runtime.R (fonte unica, a mesma do doctor). Sem relogio: `built_at` so entra se o chamador o
## informar (o build o deriva do commit); fora do runtime_id.
##
## CLI (Rscript --vanilla scripts/tools/bundle_manifest.R <sub> ...):
##   code-tree --src <repo> --dest <dir>
##   write --root <raiz do bundle> --version <v> --platform <linux-x86_64|windows-x86_64> --code-commit <sha>
##         [--apt-snapshot <AAAAMMDDTHHMMSSZ>] [--built-at <ISO8601Z>] [--compilados <arquivo: 1 pacote por linha>]
## Quando carregado por source() (testes) so define as funcoes.

.pc_bm_lib <- function() {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f) == 1L && grepl("bundle_manifest\\.R$", f))
    return(file.path(dirname(dirname(normalizePath(f, winslash = "/"))), "lib"))
  getOption("prevcarga.lib")
}

pc_code_tree <- function(src, dest) {
  dirs <- c("scripts", "models", "config")
  fs <- unlist(lapply(dirs, function(d) {
    if (!dir.exists(file.path(src, d))) return(character())
    file.path(d, list.files(file.path(src, d), recursive = TRUE, all.files = TRUE))
  }))
  fs <- c(fs, "renv.lock")
  drop <- grepl("^scripts/(tools|data-prep)/|(^|/)tests/|\\.md$|^models/[^/]+/models/", fs)
  for (f in fs[!drop]) {
    dir.create(dirname(file.path(dest, f)), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(src, f), file.path(dest, f), copy.date = FALSE))
  }
  invisible(sort(fs[!drop], method = "radix"))
}

pc_runtime_json_write <- function(root, version, platform, code_commit, apt_snapshot = NULL, compilados = list(),
                                  built_at = NULL, r_version = paste(R.version$major, R.version$minor, sep = ".")) {
  lock <- file.path(root, "code", "renv.lock")
  stopifnot(file.exists(lock))
  lock_r <- jsonlite::read_json(lock)$R$Version
  if (!identical(lock_r, r_version))
    stop(sprintf("R do build (%s) difere de R.Version do renv.lock (%s)", r_version, lock_r), call. = FALSE)
  rel <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  rel <- sort(setdiff(gsub("\\", "/", rel, fixed = TRUE), "RUNTIME.json"), method = "radix")
  files <- lapply(rel, function(p) list(path = p, sha256 = .prevcarga_runtime_file_sha(file.path(root, p))))
  x <- list(version = version, platform = platform, r_version = r_version,
            renv_lock_hash = digest::digest(file = lock, algo = "sha256"),
            code_commit = code_commit, apt_snapshot = apt_snapshot, compilados = as.list(compilados),
            built_at = built_at, files = files)
  ## o arquivo carrega built_at (lateral, ausente se nao informado); o runtime_id o ignora
  canon <- prevcarga_runtime_json_canonical(x, with_built_at = TRUE)
  out <- file.path(root, "RUNTIME.json")
  con <- file(out, open = "wb"); writeBin(charToRaw(canon), con); close(con)
  id <- prevcarga_runtime_id(out)
  cat(id, "\n", sep = "")
  invisible(id)
}

.pc_bm_main <- function(argv) {
  lib <- .pc_bm_lib()
  source(file.path(lib, "errors.R")); source(file.path(lib, "runtime.R"))
  opt <- function(k, default = NULL) { i <- which(argv == paste0("--", k)); if (length(i)) argv[i[1L] + 1L] else default }
  sub <- argv[1L]
  if (identical(sub, "code-tree")) {
    n <- pc_code_tree(opt("src"), opt("dest")); cat(sprintf("code-tree: %d arquivos\n", length(n)))
  } else if (identical(sub, "write")) {
    comp <- list()
    if (!is.null(f <- opt("compilados")) && file.exists(f)) {   # 1 pacote por linha; versao vem do renv.lock empacotado
      pk <- readLines(f, warn = FALSE); pk <- pk[nzchar(pk)]
      lk <- jsonlite::read_json(file.path(opt("root"), "code", "renv.lock"))$Packages
      comp <- lapply(pk, function(p) list(pacote = p, versao = lk[[p]]$Version, motivo = "sem binario P3M jammy; compilado do fonte"))
    }
    pc_runtime_json_write(opt("root"), opt("version"), opt("platform"), opt("code-commit"), opt("apt-snapshot"),
                          compilados = comp, built_at = opt("built-at"))
  } else stop("uso: bundle_manifest.R code-tree|write ...", call. = FALSE)
}

if (identical(environment(), globalenv()) && length(grep("--file=.*bundle_manifest\\.R$", commandArgs(FALSE))))
  .pc_bm_main(commandArgs(TRUE))
