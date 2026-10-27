#!/usr/bin/env Rscript
# Gera install/error-catalog.tsv (uniao ordenada por id dos scripts/lib/errors/*.tsv).
# O arquivo gerado NAO e versionado (.gitignore); o M8 roda este tool no stage do bundle.
lib <- file.path(dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))))), "lib")
if (file.exists(file.path(lib, "runtime.R"))) source(file.path(lib, "runtime.R"))
if (exists("prevcarga_runtime_fix", mode = "function")) prevcarga_runtime_fix()
options(prevcarga.lib = lib)
source(file.path(lib, "errors.R"))
args <- commandArgs(trailingOnly = TRUE)
out  <- if (length(i <- which(args == "--out"))) args[i + 1L] else file.path(dirname(dirname(lib)), "install", "error-catalog.tsv")
cat_df <- prevcarga_errors_load(dir = file.path(lib, "errors"), refresh = TRUE)
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
prevcarga_export_error_catalog(out, dir = file.path(lib, "errors"))   # TAB, LF, UTF-8 sem BOM, ordenado por id
cat(sprintf("error-catalog: %d linhas -> %s\n", nrow(cat_df), out))
