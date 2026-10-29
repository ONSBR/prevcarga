#!/usr/bin/env Rscript
## write_store_manifest.R -- utilitario de DESENVOLVIMENTO (fora do pacote): grava o manifest.json do store atual
## numa copia/overlay do store. O produto (CLI) nunca escreve no data_dir; so este utilitario.
## Uso: Rscript --vanilla scripts/tools/write_store_manifest.R --target <dir> [--template <json>] [--format text|json]
## Saida: 0 gravado ou no-op (identico); 2 [contract] se existir manifest diferente (arquivo intocado).
args <- commandArgs(trailingOnly = TRUE)
.arg <- function(name, default = NULL) if (length(i <- which(args == name)) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else default
lib <- file.path(dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]]),
                                               winslash = "/"))), "lib")
root <- dirname(dirname(lib))
options(prevcarga.lib = lib)
source(file.path(lib, "errors.R"))
source(file.path(lib, "runtime.R"))
invisible(prevcarga_runtime_enforce())
source(file.path(lib, "deck_reader.R"))

target   <- .arg("--target")
template <- .arg("--template", file.path(root, "config", "deck-manifest.store.json"))
fmt      <- .arg("--format", "text")

run <- function() {
  if (is.null(target) || !dir.exists(target)) prevcarga_abort("config.usage", where = "--target")
  if (!file.exists(template)) prevcarga_abort("contract.manifest_missing", where = template)
  new <- readBin(template, "raw", file.size(template))
  chk <- tempfile("chk-manifest"); dir.create(chk)
  on.exit(unlink(chk, recursive = TRUE), add = TRUE)
  writeBin(new, file.path(chk, "manifest.json"))
  prevcarga_deck_manifest(chk)                       # aborta [contract] se o template for invalido
  dst <- file.path(target, "manifest.json")
  if (file.exists(dst)) {
    if (identical(readBin(dst, "raw", file.size(dst)), new)) { cat("manifest.json identico: no-op\n"); return(invisible(0L)) }
    prevcarga_abort("contract.manifest_exists_different", where = "manifest.json")
  }
  tmp <- file.path(target, "manifest.json.tmp")
  writeBin(new, tmp)
  if (!file.rename(tmp, dst)) { unlink(tmp); prevcarga_abort("generic.unexpected", where = "manifest.json") }
  cat("manifest.json gravado\n")
  invisible(0L)
}

out <- prevcarga_with_errors(run())
if (!is.null(out$cond)) {
  writeLines(prevcarga_render_error(out$cond, format = if (fmt == "json") "json" else "text", verb = "write_store_manifest"),
             con = if (fmt == "json") stdout() else stderr())
}
quit(save = "no", status = out$exit_code)
