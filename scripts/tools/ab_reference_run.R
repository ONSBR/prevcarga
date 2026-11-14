#!/usr/bin/env Rscript
## ab_reference_run.R -- gerador da referencia do A/B a partir do clean-room (ADR-0041; substitui o ab_oracle_run.sh do ADR-0034). ASCII.
## Ferramenta de DESENVOLVIMENTO (so Linux, fora do bundle). Roda `pc_ab_clean_run` (fachada train + infer-cell sobre o deck do caso, regra de
## clima origin_midnight_utc) para cada caso executavel do cases.tsv, SEM switch por modelo (a matriz de casos vem da topologia/membros).
##
##   Rscript --vanilla scripts/tools/ab_reference_run.R --cases <cases.tsv> --data <store> --out <dir>
##       [--ref <checkout limpo da tag> --ref-tag <tag>] [--limit <n>]
## Com --ref/--ref-tag: o codigo (scripts/lib, scripts/tools, models) e o do CHECKOUT da tag (arvore suja => tools.reference_dirty; HEAD fora
## da tag => tools.reference_not_tag; exit 2) e a saida e a REFERENCIA do A/B. Sem eles: roda o codigo desta arvore (o lado "clean" do A/B).
## O store (`--data`, somente leitura) entra por overlay de symlinks em tempdir; os artefatos treinados ficam em tempdir; nada e escrito em
## data/ nem na arvore de referencia. Saida: `<out>/<origem>/membros/<sigla>/<area>.parquet` (+ manifest.json por origem) e `<out>/run.json`
## (commit, tag, n de casos). Exit: 0 ok; 2 [contract] args/arvore; 1 caso falhou.

pc_abref_main <- function(argv) {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
  own <- normalizePath(file.path(dirname(normalizePath(f, winslash = "/")), "..", ".."), winslash = "/")
  k <- sub("^--", "", argv[seq(1L, length(argv), by = 2L)]); v <- as.list(argv[seq(2L, length(argv), by = 2L)]); names(v) <- k
  lib0 <- file.path(own, "scripts", "lib")
  options(prevcarga.lib = lib0)
  source(file.path(lib0, "errors.R"))
  bad <- function(w) prevcarga_abort("tools.bad_args", where = w)
  if (length(argv) %% 2L != 0L || length(setdiff(k, c("cases", "data", "out", "ref", "ref-tag", "limit")))) bad("argv")
  for (n in c("cases", "data", "out")) if (is.null(v[[n]])) bad(paste0("--", n))
  if (xor(is.null(v$ref), is.null(v[["ref-tag"]]))) bad("--ref/--ref-tag")
  root <- own; commit <- NA_character_; tag <- NA_character_
  if (!is.null(v$ref)) {
    ge <- new.env(parent = globalenv())
    sys.source(file.path(own, "scripts", "tools", "plugin_golden.R"), envir = ge)
    for (n in ls(ge, all.names = TRUE)) if (is.function(ge[[n]])) environment(ge[[n]]) <- ge
    pre <- ge$pc_golden_preflight_reference(list(ref = v$ref, ref_tag = v[["ref-tag"]], out = v$out))
    root <- pre$oracle; commit <- pre$commit; tag <- pre$tag
  } else commit <- suppressWarnings(system2("git", c("-C", shQuote(own), "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE))[[1L]]
  lib <- file.path(root, "scripts", "lib")
  options(prevcarga.lib = lib)
  for (m in c("errors", "runtime")) source(file.path(lib, paste0(m, ".R")))
  invisible(prevcarga_runtime_enforce())
  for (m in c("config", "topology", "schema_long", "deck_reader", "dates", "features", "provenance", "registry", "validation", "evaluation",
              "reconcile", "combination", "verify_repro", "training", "inference", "build_long_output"))
    source(file.path(lib, paste0(m, ".R")))
  for (m in c("fixture_store", "ab_cases", "ab_relayout", "ab_stages")) source(file.path(root, "scripts", "tools", paste0(m, ".R")))
  cs <- pc_ab_cases_read(v$cases)
  cs <- cs[cs$executavel, , drop = FALSE]
  if (!is.null(v$limit)) cs <- utils::head(cs, as.integer(v$limit))
  work <- tempfile("abref-"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  store <- pc_ab_store_overlay(v$data, file.path(root, "config", "deck-manifest.store.json"), file.path(work, "store"))
  ctx <- list(data_dir = store, root = root, state_dir = file.path(work, "state"), tz = unique(cs$tz)[[1L]])
  dir.create(ctx$state_dir)
  dir.create(v$out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(v$out, winslash = "/")
  pc_ab_clean_run(cs, ctx, out)
  writeLines(jsonlite::toJSON(list(commit = commit, tag = tag, casos = nrow(cs)), auto_unbox = TRUE, null = "null", pretty = TRUE), file.path(out, "run.json"))
  invisible(list(out = out, casos = nrow(cs), commit = commit))
}

if (identical(environment(), globalenv()) && !interactive()) {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
  r <- tryCatch(pc_abref_main(commandArgs(trailingOnly = TRUE)), error = function(e) e)
  if (inherits(r, "error")) {
    cat("ab_reference_run: ", conditionMessage(r), "\n", sep = "", file = stderr())
    quit(save = "no", status = if (grepl("^\\[prevcarga/contract\\]", conditionMessage(r))) 2L else 1L)
  }
  cat(sprintf("ab_reference_run: %d caso(s) em %s (commit %s)\n", r$casos, r$out, r$commit))
}
