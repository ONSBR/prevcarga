#!/usr/bin/env Rscript
# =============================================================================
# accept_report.R -- relatorio da medicao do aceite em Docker local (spec RegDin Linear Refactor, E8-T3).
# Dev-only, roda no R do HOST, fora do bundle. So le <out> do aceite e o store (somente leitura); nao chama docker e nao le variavel de
# ambiente. Carrega do repo (pelo caminho do script) errors, topology, provenance, config, schema_long, dates, deck_reader, evaluation e backtest.
#
# Uso:
#   Rscript --vanilla accept_report.R --out-dir <out> --data <store>
#
# Entradas em <out>: index.tsv e units.medicao.tsv (obrigatorios), run.json (opcional: run_id e runtime_id) e, por unidade com rc 0, os
# LONGs listados em index.tsv (long_paths, relativos a units/<unit>/). A ultima linha de cada unidade em index.tsv vale (--resume anexa).
# Metrica. LONG por (versao, membro, area, origem): o membro vem do caminho state/backtest/<run>/members/<membro>/<folha>/<origem>/<corte>/
# long.parquet; so d_plus == 1; y_true e SEMPRE a verdade fresca de prevcarga_read_actual (por interval_end, igualdade exata), nunca a do LONG.
# So entram as chaves (membro, area, origem, corte, target_datetime) presentes nas DUAS versoes com y_true e y_pred finitos; o membro que so
# existe na nova (LIN_UNI) entra com wmape_ref/mape_ref '-'. Celula com LONG de um lado so e ausencia (nao entra na metrica).
# O score e SO prevcarga_scorecard(by = c(versao, membro, area)); nada e recalculado aqui.
#
# Saidas (ASCII puro, tmp + rename): report.tsv (membro area n_ref n_new wmape_ref wmape_new wmape_diff mape_ref mape_new mape_diff; '-' = sem
# valor), report.md (cabecalho RUN_ID/runtime_ids/unidades, tabela e secao Ausencias) e report.json.
# A listagem do store (nome, tamanho, sha256; .bench_data_listing) e tirada antes e depois: diferente => integrity.accept_store_changed.
#
# Exits (ids em scripts/lib/errors/accept.tsv): 0 ok; 2 contract.accept_report_input (index.tsv, units.medicao.tsv ou LONG listado
# ausente/ilegivel, ou --out-dir/--data invalidos); 9 integrity.accept_store_changed. stdout: um envelope JSON; stderr: mensagem ASCII.
# =============================================================================

.pc_acr_long_cols <- c("area", "forecast_origin", "target_datetime", "lead", "y_pred", "y_true", "d_plus", "granularity_min")
.pc_acr_member_re <- "(^|/)members/([^/]+)/([^/]+)/([^/]+)/([^/]+)/long\\.parquet$"
.pc_acr_tsv_cols <- c("membro", "area", "n_ref", "n_new", "wmape_ref", "wmape_new", "wmape_diff", "mape_ref", "mape_new", "mape_diff")

.pc_acr_bad <- function(where, reason) prevcarga_abort("contract.accept_report_input", where = where, details = list(reason = reason))

.pc_acr_fread <- function(path) {
  if (!file.exists(path)) .pc_acr_bad(path, "arquivo ausente")
  tryCatch(data.table::fread(path, sep = "\t", colClasses = "character", na.strings = NULL, data.table = TRUE),
           error = function(e) .pc_acr_bad(path, "TSV ilegivel"))
}

## ultima linha de cada unidade (index.tsv e anexado a cada execucao, inclusive --resume)
pc_accept_report_index <- function(out_dir) {
  ix <- .pc_acr_fread(file.path(out_dir, "index.tsv"))
  need <- c("unit", "versao", "rc", "error_id", "long_paths")
  if (!all(need %in% names(ix))) .pc_acr_bad(file.path(out_dir, "index.tsv"), "colunas ausentes")
  ix[, ord := seq_len(.N)]
  ix <- ix[ix[, .I[which.max(ord)], by = "unit"]$V1]
  ix[, ord := NULL]
  ix[]
}

pc_accept_report_units <- function(out_dir) {
  un <- .pc_acr_fread(file.path(out_dir, "units.medicao.tsv"))
  need <- c("unit", "versao", "origem", "area", "mdir")
  if (!all(need %in% names(un))) .pc_acr_bad(file.path(out_dir, "units.medicao.tsv"), "colunas ausentes")
  un
}

## LONG de d_plus == 1 das unidades rc 0 (versao, membro, corte e unit anexados). Atributo `cells`: (unit, versao, membro, folha, origem)
## de TODO LONG listado (qualquer d_plus). Arquivo listado ausente ou ilegivel => contract.accept_report_input.
pc_accept_report_long <- function(out_dir, units = pc_accept_report_units(out_dir), index = pc_accept_report_index(out_dir)) {
  ok <- merge(units[, c("unit", "versao"), with = FALSE], index[rc == "0", c("unit", "long_paths"), with = FALSE], by = "unit")
  data.table::setorderv(ok, "unit")
  parts <- list(); cells <- list()
  for (i in seq_len(nrow(ok))) {
    paths <- strsplit(ok$long_paths[[i]], ",", fixed = TRUE)[[1L]]
    paths <- paths[nzchar(paths) & paths != "-"]
    paths <- paths[grepl(.pc_acr_member_re, paths)]
    for (p in paths) {
      f <- file.path(out_dir, "units", ok$unit[[i]], p)
      if (!file.exists(f)) .pc_acr_bad(f, "LONG listado em index.tsv ausente")
      d <- tryCatch(data.table::as.data.table(as.data.frame(arrow::read_parquet(f))), error = function(e) .pc_acr_bad(f, "LONG ilegivel"))
      if (!all(.pc_acr_long_cols %in% names(d))) .pc_acr_bad(f, "LONG sem as colunas do schema")
      m <- regmatches(p, regexec(.pc_acr_member_re, p))[[1L]]
      cells[[length(cells) + 1L]] <- data.table::data.table(unit = ok$unit[[i]], versao = ok$versao[[i]], membro = m[[3L]],
                                                            folha = m[[4L]], origem = m[[5L]])
      d <- d[d_plus == 1L]
      if (!nrow(d)) next
      d <- d[, .pc_acr_long_cols, with = FALSE]
      d[, `:=`(forecast_origin = as.character(forecast_origin), versao = ok$versao[[i]], membro = m[[3L]], corte = m[[6L]], unit = ok$unit[[i]])]
      parts[[length(parts) + 1L]] <- d
    }
  }
  long <- if (length(parts)) data.table::rbindlist(parts, use.names = TRUE) else
    data.table::data.table(area = character(), forecast_origin = character(), target_datetime = as.POSIXct(character(), tz = "UTC"),
                           lead = integer(), y_pred = double(), y_true = double(), d_plus = integer(), granularity_min = integer(),
                           versao = character(), membro = character(), corte = character(), unit = character())
  attr(long, "cells") <- if (length(cells)) data.table::rbindlist(cells) else
    data.table::data.table(unit = character(), versao = character(), membro = character(), folha = character(), origem = character())
  long
}

## y_true fresco (prevcarga_read_actual via .bt_join_truth); o y_true embutido no LONG e descartado
pc_accept_report_truth <- function(long, data_dir) {
  cells <- attr(long, "cells")
  if (!nrow(long)) return(structure(long, cells = cells))
  out <- .bt_join_truth(list(data_dir = data_dir), long)
  attr(out, "cells") <- cells
  out
}

## membros que so existem na nova (nenhum LONG deles na ref); vazio quando a ref nao tem LONG algum
pc_accept_report_only_new <- function(cells) {
  mr <- unique(cells$membro[cells$versao == "ref"]); mn <- unique(cells$membro[cells$versao == "new"])
  if (!length(mr)) character() else sort(setdiff(mn, mr), method = "radix")
}

## chaves pareadas ref x nova (y_true e y_pred finitos); so-nova entra para o membro que so existe na nova
pc_accept_report_pair <- function(long, only_new = pc_accept_report_only_new(attr(long, "cells"))) {
  d <- data.table::as.data.table(long)[is.finite(y_true) & is.finite(y_pred)]
  d[, k := paste(membro, area, forecast_origin, corte, as.numeric(target_datetime), sep = "|")]
  both <- intersect(d[versao == "ref"]$k, d[versao == "new"]$k)
  d[k %in% both | (versao == "new" & membro %in% only_new)][, k := NULL][]
}

## scorecard pelo motor unico; interval_start pela borda do LONG (schema_long) no fuso da carga do store
pc_accept_report_score <- function(paired, tz) {
  tcols <- c("membro", "area", "n_ref", "n_new", "wmape_ref", "wmape_new", "mape_ref", "mape_new")
  if (!nrow(paired)) {
    z <- data.table::data.table(membro = character(), area = character(), n_ref = integer(), n_new = integer(), wmape_ref = double(),
                                wmape_new = double(), wmape_diff = double(), mape_ref = double(), mape_new = double(), mape_diff = double())
    return(z)
  }
  keep <- data.table::copy(paired)
  keep[, target_datetime := lubridate::with_tz(target_datetime, tz)]
  keep <- data.table::as.data.table(prevcarga_long_attach_interval_start(keep, tz = tz))
  sc <- data.table::as.data.table(prevcarga_scorecard(keep, by = c("versao", "membro", "area")))
  w <- data.table::dcast(sc, membro + area ~ versao, value.var = c("wmape", "mape", "n"))
  for (v in c("ref", "new")) for (k in c("wmape", "mape", "n")) {
    cn <- paste(k, v, sep = "_")
    if (!(cn %in% names(w))) w[, (cn) := if (k == "n") NA_integer_ else NA_real_]
  }
  w[, `:=`(wmape_diff = wmape_new - wmape_ref, mape_diff = mape_new - mape_ref)]
  w[is.na(n_ref), n_ref := 0L]; w[is.na(n_new), n_new := 0L]
  data.table::setorderv(w, c("membro", "area"))
  w[, .pc_acr_tsv_cols, with = FALSE][]
}

## unidades (da lista) por situacao: ok (rc 0), recusa (rc 2-15 com error_id), failed (o resto, inclusive sem linha no index)
pc_accept_report_counts <- function(units, index) {
  m <- merge(units[, "unit", with = FALSE], index[, c("unit", "rc", "error_id"), with = FALSE], by = "unit", all.x = TRUE)
  rc <- suppressWarnings(as.integer(m$rc))
  ok <- !is.na(rc) & rc == 0L
  rf <- !is.na(rc) & rc >= 2L & rc <= 15L & !is.na(m$error_id) & m$error_id != "-"
  list(total = nrow(m), ok = sum(ok), recusa = sum(rf), failed = sum(!ok & !rf))
}

## ausencias: (versao, origem, area, membro, error_id, rc) por unidade x membro esperado sem LONG. Esperado = membros do mdir na topologia
## (sem os so-nova na ref). Unidade recusada/failed herda error_id e rc do index; celula ausente numa unidade rc 0 fica com error_id '-'.
pc_accept_report_absent <- function(units, index, cells, members, only_new = pc_accept_report_only_new(cells)) {
  m <- merge(units, index[, c("unit", "rc", "error_id"), with = FALSE], by = "unit", all.x = TRUE)
  rows <- list()
  for (i in seq_len(nrow(m))) {
    u <- m[i]
    exp <- members$sigla[members$mdir == u$mdir]
    if (identical(u$versao, "ref")) exp <- setdiff(exp, only_new)
    have <- cells$membro[cells$unit == u$unit]
    miss <- sort(setdiff(exp, have), method = "radix")
    if (!length(miss)) next
    rows[[length(rows) + 1L]] <- data.table::data.table(versao = u$versao, origem = u$origem, area = u$area, membro = miss,
                                                        error_id = if (is.na(u$error_id)) "-" else u$error_id,
                                                        rc = if (is.na(u$rc)) "-" else u$rc)
  }
  out <- if (length(rows)) data.table::rbindlist(rows) else
    data.table::data.table(versao = character(), origem = character(), area = character(), membro = character(),
                           error_id = character(), rc = character())
  data.table::setorderv(out, c("versao", "origem", "area", "membro"))
  out[]
}

## ---- escrita (ASCII, tmp + rename) -------------------------------------------------------------------------------------
pc_accept_write_ascii <- function(lines, path) {
  bad <- grepl("[^\\x01-\\x7F]", lines, perl = TRUE, useBytes = TRUE)
  stopifnot(!any(bad))
  tmp <- tempfile(pattern = ".report-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  writeLines(lines, tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) stop("accept_report: rename falhou para ", path, call. = FALSE)
  invisible(path)
}

.pc_acr_num <- function(x) ifelse(is.na(x), "-", formatC(x, digits = 15L, format = "g"))

pc_accept_report_tsv_lines <- function(tbl) {
  f <- data.table::copy(tbl)
  for (k in c("wmape_ref", "wmape_new", "wmape_diff", "mape_ref", "mape_new", "mape_diff")) f[[k]] <- .pc_acr_num(f[[k]])
  c(paste(.pc_acr_tsv_cols, collapse = "\t"), if (nrow(f)) do.call(paste, c(lapply(f[, .pc_acr_tsv_cols, with = FALSE], as.character), sep = "\t")))
}

.pc_acr_md_table <- function(head, rows) {
  c(paste0("| ", paste(head, collapse = " | "), " |"), paste0("|", paste(rep("---", length(head)), collapse = "|"), "|"),
    if (length(rows)) paste0("| ", rows, " |"))
}

pc_accept_report_md_lines <- function(tbl, absent, counts, meta, sha) {
  tx <- tbl[, .pc_acr_tsv_cols, with = FALSE]
  rows <- if (nrow(tx)) do.call(paste, c(lapply(c(list(tx$membro, tx$area, tx$n_ref, tx$n_new),
                                                    lapply(tx[, 5:10], .pc_acr_num)), as.character), sep = " | ")) else character()
  ab <- if (nrow(absent)) do.call(paste, c(lapply(absent, as.character), sep = " | ")) else character()
  c("# Relatorio da medicao (D+1)", "",
    sprintf("RUN_ID: %s", meta$run_id),
    sprintf("runtime_id ref: %s", meta$runtime_id$ref), sprintf("runtime_id nova: %s", meta$runtime_id$new),
    sprintf("Unidades: total=%d ok=%d recusa=%d failed=%d", counts$total, counts$ok, counts$recusa, counts$failed),
    sprintf("sha256 de report.tsv: %s", sha), "Store: listagem identica antes e depois (somente leitura).", "",
    "Metrica: so prevcarga_scorecard sobre as chaves pareadas ref x nova com d_plus == 1 e verdade fresca; '-' = sem valor (LIN_UNI nao existe na ref).",
    "", "## Resultado por membro e area", "",
    .pc_acr_md_table(c("membro", "area", "n_ref", "n_new", "wmape_ref", "wmape_new", "wmape_diff", "mape_ref", "mape_new", "mape_diff"), rows),
    "", "## Ausencias", "",
    if (nrow(absent)) .pc_acr_md_table(c("versao", "origem", "area", "membro", "error_id", "rc"), ab) else "Nenhuma ausencia.")
}

pc_accept_report_json_lines <- function(tbl, absent, counts, meta, sha) {
  x <- list(run_id = meta$run_id, d_plus = 1L, runtime_id = meta$runtime_id, unidades = counts, linhas = as.data.frame(tbl),
            ausencias = as.data.frame(absent), sha256_report_tsv = sha, store_unchanged = TRUE)
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE))
}

.pc_acr_meta <- function(out_dir) {
  f <- file.path(out_dir, "run.json")
  j <- if (file.exists(f)) tryCatch(jsonlite::read_json(f), error = function(e) .pc_acr_bad(f, "run.json ilegivel")) else list()
  g <- function(x) if (is.null(x) || !nzchar(as.character(x))) "-" else as.character(x)
  list(run_id = g(j$run_id), runtime_id = list(ref = g(j$runtime_id$ref), new = g(j$runtime_id$new)))
}

## nucleo: `between` (so teste) roda depois do calculo e antes da segunda listagem do store
pc_accept_report_run <- function(out_dir, data_dir, root, between = NULL) {
  if (!dir.exists(out_dir)) .pc_acr_bad(out_dir, "--out-dir nao e diretorio")
  if (!dir.exists(data_dir)) .pc_acr_bad(data_dir, "--data nao e diretorio")
  before <- .bench_data_listing(data_dir)
  units <- pc_accept_report_units(out_dir); index <- pc_accept_report_index(out_dir)
  long <- pc_accept_report_truth(pc_accept_report_long(out_dir, units, index), data_dir)
  cells <- attr(long, "cells")
  paired <- pc_accept_report_pair(long, pc_accept_report_only_new(cells))
  tz <- if (nrow(paired)) prevcarga_deck_manifest(data_dir)$timezone$load else "UTC"
  tbl <- pc_accept_report_score(paired, tz)
  absent <- pc_accept_report_absent(units, index, cells, prevcarga_members(root = root))
  counts <- pc_accept_report_counts(units, index)
  meta <- .pc_acr_meta(out_dir)
  if (is.function(between)) between()
  after <- .bench_data_listing(data_dir)
  if (!identical(before, after))
    prevcarga_abort("integrity.accept_store_changed", where = data_dir, details = list(antes = nrow(before), depois = nrow(after)))
  p_tsv <- file.path(out_dir, "report.tsv")
  pc_accept_write_ascii(pc_accept_report_tsv_lines(tbl), p_tsv)
  sha <- digest::digest(file = p_tsv, algo = "sha256")
  pc_accept_write_ascii(pc_accept_report_md_lines(tbl, absent, counts, meta, sha), file.path(out_dir, "report.md"))
  pc_accept_write_ascii(pc_accept_report_json_lines(tbl, absent, counts, meta, sha), file.path(out_dir, "report.json"))
  list(report = p_tsv, rows = nrow(tbl), absent = nrow(absent), units = counts, sha256 = sha)
}

.pc_acr_opt <- function(args, key) { i <- which(args == key); if (length(i) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else NULL }

pc_accept_report_main <- function(args, root, between = NULL) {
  need <- function(key) { v <- .pc_acr_opt(args, key); if (is.null(v)) .pc_acr_bad(key, "opcao obrigatoria"); v }
  pc_accept_report_run(need("--out-dir"), need("--data"), root, between)
}

if (sys.nframe() == 0L) {
  fa <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  lib <- file.path(dirname(dirname(normalizePath(fa[[1L]]))), "lib")
  options(prevcarga.lib = lib)
  if (file.exists(file.path(lib, "runtime.R"))) source(file.path(lib, "runtime.R"))
  if (exists("prevcarga_runtime_fix", mode = "function")) prevcarga_runtime_fix()
  for (m in c("errors", "topology", "provenance", "config", "schema_long", "dates", "deck_reader", "evaluation", "backtest"))
    source(file.path(lib, paste0(m, ".R")))
  source(file.path(dirname(lib), "tools", "bench_forecast.R"))
  out <- prevcarga_with_errors(pc_accept_report_main(commandArgs(trailingOnly = TRUE), dirname(dirname(lib))))
  env <- prevcarga_envelope(verb = "accept_report", cond = out$cond, result = if (is.null(out$cond)) out$value else out$cond$result)
  env$error_id <- if (is.null(out$cond)) NULL else out$cond$id
  writeLines(as.character(jsonlite::toJSON(env, auto_unbox = TRUE, null = "null", digits = NA)), stdout())
  if (!is.null(out$cond)) writeLines(prevcarga_render_error_text(out$cond), stderr())
  quit(save = "no", status = if (is.null(out$cond)) 0L else as.integer(out$cond$exit_code))
}
