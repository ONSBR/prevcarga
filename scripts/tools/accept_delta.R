#!/usr/bin/env Rscript
# =============================================================================
# accept_delta.R -- comparacao ref x new dos LONGs e veredito do estagio aceite (spec RegDin Linear Refactor, E8-T2).
# Dev-only, roda no R do HOST, fora do bundle. So le <out> do aceite e o store ro; nao chama docker, nao le variavel de ambiente.
#
# Uso:
#   Rscript --vanilla accept_delta.R --ref <dir> --new <dir> --out delta.tsv
#   Rscript --vanilla accept_delta.R --verdict --index index.tsv --units units.aceite.tsv --out aceite.json
#
# Delta. <dir> e o diretorio de uma unidade (state/backtest/<unico run>/ dentro dele), o proprio diretorio do run, ou, no
# infer-model, o diretorio com long.parquet. LONGs localizados: members/<membro>/<folha>/<origem>/<corte>/long.parquet,
# combined/*.parquet, reconciled/subsistemas/*.parquet (artefato = caminho relativo ao run) ou long.parquet do infer-model.
# A chave e (area, forecast_origin, target_datetime, lead), com target_datetime comparado em numerico UTC (o parquet do store nao
# tem fuso; diferenca so de exibicao NAO e diferenca). No infer-model (colunas origin/target_datetime/y_pred) forecast_origin vem
# de origin e lead e o posto do alvo na origem. Artefato so de um lado (ex.: LIN_UNI na nova) vira chave exclusiva.
# delta.tsv (TAB, cabecalho, uma linha por artefato e area): artefato area n_ref n_new keys_only_ref keys_only_new max_abs
#   (max_abs = maior |y_pred ref - y_pred new| nas chaves comuns; NA so de um lado = Inf).
#
# Veredito (--verdict). index.tsv (do accept_docker.sh: unit versao image runtime_id origem area mdir rc error_id segundos ...) e
# units.aceite.tsv (de accept_cases.R). Feriado: unidade de backtest do dessem-linear na nova com error_id generic.unexpected.
# Tempo: infer-model (data_flag nao --data-inference) da nova com segundos >= os da ref (ou linha ausente). Grava aceite.json
# {verdict, error_id, culprits[{unit, error_id, detail}], infer_seconds{ref,new}, refusals[]}.
#
# Exits (ids em scripts/lib/errors/accept.tsv): 0 ok; 12 repro.accept_delta_nonzero | repro.accept_unexpected_error |
# repro.accept_infer_not_faster; 2 contract.accept_bad_args (uso, ou nenhum LONG em nenhum dos lados). stdout: um envelope JSON
# (error.id, error_id, result.delta {max_abs, keys_only}); stderr: mensagem ASCII.
# =============================================================================

.pc_acd_key <- c("area", "forecast_origin", "target_utc", "lead")

## localiza a raiz dos LONGs de <dir>: run unico de state/backtest, o proprio run, ou NULL (infer-model)
.pc_acd_run_root <- function(dir) {
  runs <- list.dirs(file.path(dir, "state", "backtest"), recursive = FALSE)
  if (length(runs) > 1L)
    prevcarga_abort("contract.accept_bad_args", where = dir, details = list(reason = "mais de um run em state/backtest"))
  if (length(runs) == 1L) return(runs)
  if (any(dir.exists(file.path(dir, c("members", "combined", "reconciled"))))) return(dir)
  NULL
}

## data.table(artefato, path): artefato = caminho relativo ao run (ou nome do arquivo no infer-model); ordem radix
pc_accept_long_index <- function(dir) {
  if (!dir.exists(dir)) return(data.table::data.table(artefato = character(), path = character()))
  rr <- .pc_acd_run_root(dir)
  if (is.null(rr)) {
    f <- c(file.path(dir, "long.parquet"), Sys.glob(file.path(dir, "*", "long.parquet")))
    f <- f[file.exists(f)]
    return(data.table::data.table(artefato = substring(f, nchar(dir) + 2L), path = f)[order(artefato, method = "radix")])
  }
  f <- c(list.files(file.path(rr, "members"), pattern = "^long\\.parquet$", recursive = TRUE, full.names = TRUE),
         list.files(file.path(rr, "combined"), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE),
         list.files(file.path(rr, "reconciled", "subsistemas"), pattern = "\\.parquet$", recursive = FALSE, full.names = TRUE))
  data.table::data.table(artefato = substring(f, nchar(rr) + 2L), path = f)[order(artefato, method = "radix")]
}

## normaliza uma tabela LONG (8 colunas) ou de infer-model para a chave de comparacao; target_utc e numerico (UTC)
.pc_acd_prep <- function(x) {
  x <- data.table::as.data.table(x)
  if (!("forecast_origin" %in% names(x)) && "origin" %in% names(x)) x[, forecast_origin := format(as.Date(origin), "%Y-%m-%d")]
  if (!("area" %in% names(x))) x[, area := NA_character_]
  td <- x$target_datetime
  x[, target_utc := if (inherits(td, "POSIXt")) as.numeric(td) else as.numeric(lubridate::ymd_hms(td, tz = "UTC", quiet = TRUE))]
  x[, forecast_origin := as.character(forecast_origin)]
  if (!("lead" %in% names(x))) {
    data.table::setorderv(x, c("area", "forecast_origin", "target_utc"))
    x[, lead := seq_len(.N), by = c("area", "forecast_origin")]
  }
  x[, lead := as.integer(lead)]
  x[, c(.pc_acd_key, "y_pred"), with = FALSE]
}

pc_accept_delta_one <- function(a, b, artefato) {
  a <- if (is.null(a)) NULL else .pc_acd_prep(a)
  b <- if (is.null(b)) NULL else .pc_acd_prep(b)
  if (is.null(a)) a <- b[0L]
  if (is.null(b)) b <- a[0L]
  a[, in_ref := TRUE]; b[, in_new := TRUE]
  m <- merge(a, b, by = .pc_acd_key, all = TRUE, suffixes = c(".ref", ".new"))
  m[is.na(in_ref), in_ref := FALSE]; m[is.na(in_new), in_new := FALSE]
  both <- m$in_ref & m$in_new
  d <- abs(m$y_pred.ref - m$y_pred.new)
  d[both & is.na(m$y_pred.ref) & is.na(m$y_pred.new)] <- 0
  d[both & is.na(d)] <- Inf
  m[, dif := ifelse(both, d, NA_real_)]
  r <- m[, list(n_ref = sum(in_ref), n_new = sum(in_new), keys_only_ref = sum(in_ref & !in_new), keys_only_new = sum(in_new & !in_ref),
                max_abs = if (any(in_ref & in_new)) max(dif, na.rm = TRUE) else 0), by = "area"]
  data.table::setorderv(r, "area")
  r[, artefato := artefato]
  r[, c("artefato", "area", "n_ref", "n_new", "keys_only_ref", "keys_only_new", "max_abs"), with = FALSE]
}

.pc_acd_read <- function(path) as.data.frame(arrow::read_parquet(path))

## ref e new: diretorios. Devolve a tabela do delta (uma linha por artefato e area)
pc_accept_delta <- function(ref, new) {
  ir <- pc_accept_long_index(ref); inew <- pc_accept_long_index(new)
  arte <- sort(union(ir$artefato, inew$artefato), method = "radix")
  if (!length(arte))
    prevcarga_abort("contract.accept_bad_args", where = ref, details = list(reason = "nenhum LONG em --ref nem em --new"))
  rows <- lapply(arte, function(a) {
    pr <- ir$path[match(a, ir$artefato)]; pn <- inew$path[match(a, inew$artefato)]
    pc_accept_delta_one(if (is.na(pr)) NULL else .pc_acd_read(pr), if (is.na(pn)) NULL else .pc_acd_read(pn), a)
  })
  data.table::rbindlist(rows)
}

pc_accept_delta_summary <- function(dt) {
  list(max_abs = if (nrow(dt)) max(dt$max_abs) else 0, keys_only = sum(dt$keys_only_ref) + sum(dt$keys_only_new), rows = nrow(dt))
}

.pc_acd_write_tsv <- function(dt, path) {
  tmp <- paste0(path, ".tmp")
  data.table::fwrite(dt, tmp, sep = "\t", quote = FALSE, eol = "\n", na = "-")
  if (!isTRUE(file.rename(tmp, path))) { unlink(tmp); prevcarga_abort("contract.accept_bad_args", where = path) }
}

pc_accept_delta_run <- function(ref, new, out) {
  dt <- pc_accept_delta(ref, new)
  .pc_acd_write_tsv(dt, out)
  s <- pc_accept_delta_summary(dt)
  if (s$max_abs > 0 || s$keys_only > 0)
    prevcarga_abort("repro.accept_delta_nonzero", where = out, details = s, result = list(delta = s, out = out))
  list(delta = s, out = out)
}

## veredito: culpados de feriado (generic.unexpected no dessem-linear da nova) e de tempo (infer-model MED da nova nao menor)
pc_accept_verdict <- function(index, units) {
  ix <- data.table::fread(index, sep = "\t", header = TRUE, colClasses = list(character = c("unit", "error_id")), na.strings = NULL)
  un <- data.table::fread(units, sep = "\t", header = TRUE, na.strings = NULL)
  culprits <- list()
  add <- function(unit, id, detail) culprits[[length(culprits) + 1L]] <<- list(unit = unit, error_id = id, detail = detail)
  hol <- un[versao == "new" & verbo == "backtest" & mdir != "-"]
  for (u in hol$unit) {
    r <- ix[unit == u]
    if (nrow(r) && identical(r$error_id[[1L]], "generic.unexpected"))
      add(u, "repro.accept_unexpected_error", sprintf("generic.unexpected em %s", r$area[[1L]]))
  }
  tm <- un[verbo == "infer-model" & data_flag != "--data-inference"]
  ur <- tm$unit[tm$versao == "ref"]; unw <- tm$unit[tm$versao == "new"]
  sec <- function(u) if (length(u) == 1L && nrow(ix[unit == u]) && ix[unit == u]$rc[[1L]] == 0L) as.numeric(ix[unit == u]$segundos[[1L]]) else NA_real_
  sr <- sec(ur); sn <- sec(unw)
  if (is.na(sr) || is.na(sn) || !(sn < sr))
    add(if (length(unw)) unw[[1L]] else "-", "repro.accept_infer_not_faster",
        sprintf("segundos ref=%s new=%s", format(sr), format(sn)))
  ref_ref <- ix[versao == "ref" & rc >= 2L & rc <= 15L & rc != 14L & error_id != "-"]
  list(verdict = if (length(culprits)) "FAIL" else "PASS",
       error_id = if (length(culprits)) culprits[[1L]]$error_id else NULL,
       culprits = culprits, infer_seconds = list(ref = if (is.na(sr)) NULL else sr, new = if (is.na(sn)) NULL else sn),
       refusals = as.list(ref_ref$unit))
}

pc_accept_verdict_run <- function(index, units, out) {
  v <- pc_accept_verdict(index, units)
  writeLines(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA), out)
  if (length(v$culprits)) {
    ids <- vapply(v$culprits, function(c) c$error_id, "")
    id <- if ("repro.accept_unexpected_error" %in% ids) "repro.accept_unexpected_error" else "repro.accept_infer_not_faster"
    prevcarga_abort(id, where = out, details = list(culprits = v$culprits), result = v)
  }
  v
}

.pc_acd_opt <- function(args, key) { i <- which(args == key); if (length(i) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else NULL }

pc_accept_delta_main <- function(args) {
  need <- function(key) { v <- .pc_acd_opt(args, key); if (is.null(v)) prevcarga_abort("contract.accept_bad_args", where = key); v }
  if ("--verdict" %in% args) return(pc_accept_verdict_run(need("--index"), need("--units"), need("--out")))
  pc_accept_delta_run(need("--ref"), need("--new"), need("--out"))
}

if (sys.nframe() == 0L) {
  fa <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  lib <- file.path(dirname(dirname(normalizePath(fa[[1L]]))), "lib")
  options(prevcarga.lib = lib)
  if (file.exists(file.path(lib, "runtime.R"))) source(file.path(lib, "runtime.R"))
  if (exists("prevcarga_runtime_fix", mode = "function")) prevcarga_runtime_fix()
  source(file.path(lib, "errors.R"))
  out <- prevcarga_with_errors(pc_accept_delta_main(commandArgs(trailingOnly = TRUE)))
  env <- prevcarga_envelope(verb = "accept_delta", cond = out$cond, result = if (is.null(out$cond)) out$value else out$cond$result)
  env$error_id <- if (is.null(out$cond)) NULL else out$cond$id
  writeLines(as.character(jsonlite::toJSON(env, auto_unbox = TRUE, null = "null", digits = NA)), stdout())
  if (!is.null(out$cond)) writeLines(prevcarga_render_error_text(out$cond), stderr())
  quit(save = "no", status = if (is.null(out$cond)) 0L else as.integer(out$cond$exit_code))
}
