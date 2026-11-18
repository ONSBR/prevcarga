#!/usr/bin/env Rscript
# =============================================================================
# accept_feriado.R -- pos-processamento da medicao do aceite (spec RegDin Linear Refactor, E8-T4, item 11 do pedido do Gabriel de
# 2026-11-17). Dev-only, roda no R do HOST, fora do bundle. Usa SO as previsoes (LONG) que a medicao ja gerou e a verdade fresca do store
# (somente leitura); nao roda motor, nao chama docker e nao le variavel de ambiente.
#
# Para cada (versao, membro DESSEM, area) separa MAPE e WMAPE em tres grupos de dia e informa o numero de dias de cada grupo:
#   feriado  : o dia (data de interval_start, fuso da carga) consta na tabela auxiliary/holidays da area;
#   vizinho  : nao e feriado e e o dia anterior ou o posterior (+-1 dia, os dias LAGDS) de um feriado da mesma area;
#   comum    : o resto.
# Duas coberturas: d1 (so d_plus == 1, a mesma do report.tsv) e horizonte (todos os d_plus do LONG).
# "dias" = pares distintos (forecast_origin, data). Marca as celulas de feriado que usaram warning.engine_tune_fallback, lidas do
# stderr.log da unidade (linha "[engine.dessem] warning.engine_tune_fallback: ... celula: <data, hora, tipo>").
#
# Uso: Rscript --vanilla accept_feriado.R --out-dir <out> --data <store>
# Saidas em <out> (ASCII, TAB): feriado.tsv, feriado_fallback.tsv, porigem.tsv (MAPE/WMAPE D+1 por membro, area e origem)
# =============================================================================

.pc_fer_member_re <- "(^|/)members/([^/]+)/([^/]+)/([^/]+)/([^/]+)/long\\.parquet$"
.pc_fer_dessem <- function(m) grepl("^(LIN|RGD)_", m)
.pc_fer_groups <- c("feriado", "vizinho", "comum")

pc_fer_holidays <- function(data_dir) {
  d <- file.path(data_dir, "auxiliary", "holidays")
  fs <- list.files(d, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  x <- data.table::rbindlist(lapply(fs, function(f) data.table::as.data.table(as.data.frame(arrow::read_parquet(f)))), fill = TRUE)
  x[, .(area = as.character(area_code), date = as.Date(date))][!is.na(date)]
}

## classe do dia por area: feriado / vizinho (+-1 dia de um feriado, nao feriado) / comum
pc_fer_class <- function(area, day, hol) {
  out <- rep("comum", length(day))
  for (a in unique(area)) {
    i <- area == a
    h <- hol$date[hol$area == a]
    nb <- setdiff(c(h - 1L, h + 1L), h)
    out[i & day %in% nb] <- "vizinho"
    out[i & day %in% h] <- "feriado"
  }
  out
}

## LONG de todos os membros DESSEM das unidades rc 0 (todos os d_plus), com versao, membro, origem e unit
pc_fer_long <- function(out_dir) {
  un <- data.table::fread(file.path(out_dir, "units.medicao.tsv"), sep = "\t", colClasses = "character", na.strings = NULL)
  ix <- data.table::fread(file.path(out_dir, "index.tsv"), sep = "\t", colClasses = "character", na.strings = NULL)
  ix[, ord := seq_len(.N)]; ix <- ix[ix[, .I[which.max(ord)], by = "unit"]$V1]
  ok <- merge(un[, c("unit", "versao"), with = FALSE], ix[rc == "0", c("unit", "long_paths"), with = FALSE], by = "unit")
  cols <- c("area", "forecast_origin", "target_datetime", "lead", "y_pred", "y_true", "d_plus", "granularity_min")
  parts <- list()
  for (i in seq_len(nrow(ok))) {
    ps <- strsplit(ok$long_paths[[i]], ",", fixed = TRUE)[[1L]]
    ps <- ps[nzchar(ps) & ps != "-" & grepl(.pc_fer_member_re, ps)]
    for (p in ps) {
      m <- regmatches(p, regexec(.pc_fer_member_re, p))[[1L]]
      if (!.pc_fer_dessem(m[[3L]])) next
      d <- data.table::as.data.table(as.data.frame(arrow::read_parquet(file.path(out_dir, "units", ok$unit[[i]], p))))
      d <- d[, cols, with = FALSE]
      d[, `:=`(forecast_origin = as.character(forecast_origin), versao = ok$versao[[i]], membro = m[[3L]], corte = m[[6L]], unit = ok$unit[[i]])]
      parts[[length(parts) + 1L]] <- d
    }
  }
  data.table::rbindlist(parts, use.names = TRUE)
}

pc_fer_score <- function(paired, tz, hol) {
  paired <- data.table::copy(paired)
  paired[, target_datetime := lubridate::with_tz(target_datetime, tz)]
  paired <- data.table::as.data.table(prevcarga_long_attach_interval_start(paired, tz = tz))
  paired[, dia := as.Date(lubridate::with_tz(interval_start, tz), tz = tz)]
  paired[, grupo := pc_fer_class(area, dia, hol)]
  dias <- paired[, .(dias = data.table::uniqueN(paste(forecast_origin, dia))), by = c("versao", "membro", "area", "grupo")]
  sc <- data.table::as.data.table(prevcarga_scorecard(paired, by = c("versao", "membro", "area", "grupo")))[, .(versao, membro, area, grupo, n, wmape, mape)]
  merge(sc, dias, by = c("versao", "membro", "area", "grupo"), all = TRUE)
}

## MAPE/WMAPE D+1 por (versao, membro, area, origem), sobre as mesmas chaves pareadas do report.tsv (pedido: "por area e por origem")
pc_fer_por_origem <- function(paired, tz) {
  paired <- data.table::copy(paired)
  paired[, target_datetime := lubridate::with_tz(target_datetime, tz)]
  paired <- data.table::as.data.table(prevcarga_long_attach_interval_start(paired, tz = tz))
  sc <- data.table::as.data.table(prevcarga_scorecard(paired, by = c("versao", "membro", "area", "forecast_origin")))
  o <- as.character(sc$forecast_origin)
  o <- ifelse(grepl("^[0-9]+$", o), format(as.Date(as.integer(o), origin = "1970-01-01")), substr(o, 1L, 10L))
  sc[, .(versao, membro, area, origem = o, n, wmape, mape)]
}

## celulas de feriado com warning.engine_tune_fallback, do stderr.log das unidades rc 0 (versao nova; a ref nao tem o fallback)
pc_fer_fallback <- function(out_dir) {
  un <- data.table::fread(file.path(out_dir, "units.medicao.tsv"), sep = "\t", colClasses = "character", na.strings = NULL)
  rows <- list()
  for (i in seq_len(nrow(un))) {
    f <- file.path(out_dir, "units", un$unit[[i]], "stderr.log")
    if (!file.exists(f)) next
    l <- grep("warning.engine_tune_fallback", readLines(f, warn = FALSE, encoding = "latin1"), value = TRUE, fixed = TRUE)
    if (!length(l)) next
    cell <- ifelse(grepl("celula: ", l, fixed = TRUE), sub("^.*celula: ", "", l), "-")
    rows[[length(rows) + 1L]] <- data.table::data.table(versao = un$versao[[i]], mdir = un$mdir[[i]], area = un$area[[i]],
                                                        origem = un$origem[[i]], celula = trimws(cell))
  }
  if (length(rows)) data.table::rbindlist(rows) else
    data.table::data.table(versao = character(), mdir = character(), area = character(), origem = character(), celula = character())
}

.pc_fer_num <- function(x) ifelse(is.na(x), "-", trimws(formatC(x, digits = 6L, format = "g")))

pc_fer_write <- function(lines, path) {
  stopifnot(!any(grepl("[^\\x01-\\x7F]", lines, perl = TRUE, useBytes = TRUE)))
  tmp <- tempfile(pattern = ".fer-", tmpdir = dirname(path)); on.exit(unlink(tmp), add = TRUE)
  writeLines(lines, tmp, useBytes = TRUE); file.rename(tmp, path); invisible(path)
}

pc_fer_run <- function(out_dir, data_dir, root) {
  before <- .bench_data_listing(data_dir)
  hol <- pc_fer_holidays(data_dir)
  long <- pc_fer_long(out_dir)
  tz <- prevcarga_deck_manifest(data_dir)$timezone$load
  long <- data.table::as.data.table(.bt_join_truth(list(data_dir = data_dir), long))
  long <- long[is.finite(y_true) & is.finite(y_pred)]
  res <- list()
  for (cob in c("d1", "horizonte")) {
    d <- if (cob == "d1") long[d_plus == 1L] else long
    d[, k := paste(membro, area, forecast_origin, corte, as.numeric(target_datetime), d_plus, sep = "|")]
    both <- intersect(d[versao == "ref"]$k, d[versao == "new"]$k)
    only_new <- setdiff(unique(d[versao == "new"]$membro), unique(d[versao == "ref"]$membro))
    p <- d[k %in% both | (versao == "new" & membro %in% only_new)][, k := NULL]
    s <- pc_fer_score(p, tz, hol); s[, cobertura := cob]; res[[cob]] <- s
    if (cob == "d1") por_origem <- pc_fer_por_origem(p, tz)
  }
  tab <- data.table::rbindlist(res)
  data.table::setcolorder(tab, c("cobertura", "versao", "membro", "area", "grupo", "dias", "n", "wmape", "mape"))
  data.table::setorderv(tab, c("cobertura", "versao", "membro", "area", "grupo"), order = c(1L, -1L, 1L, 1L, 1L))
  data.table::setorderv(por_origem, c("versao", "membro", "area", "origem"), order = c(-1L, 1L, 1L, 1L))
  po <- do.call(paste, c(lapply(por_origem, function(x) if (is.numeric(x) && !is.integer(x)) .pc_fer_num(x) else as.character(x)), sep = "\t"))
  pc_fer_write(c(paste(names(por_origem), collapse = "\t"), po), file.path(out_dir, "porigem.tsv"))
  fb <- pc_fer_fallback(out_dir)
  fb <- fb[, .(ocorrencias = .N), by = c("versao", "mdir", "area", "origem", "celula")]
  data.table::setorderv(fb, c("versao", "mdir", "area", "origem", "celula"))
  f <- do.call(paste, c(lapply(tab, function(x) if (is.numeric(x) && !is.integer(x)) .pc_fer_num(x) else as.character(x)), sep = "\t"))
  pc_fer_write(c(paste(names(tab), collapse = "\t"), f), file.path(out_dir, "feriado.tsv"))
  g <- if (nrow(fb)) do.call(paste, c(lapply(fb, as.character), sep = "\t")) else character()
  pc_fer_write(c(paste(names(fb), collapse = "\t"), g), file.path(out_dir, "feriado_fallback.tsv"))
  after <- .bench_data_listing(data_dir)
  if (!identical(before, after)) prevcarga_abort("integrity.accept_store_changed", where = data_dir, details = list())
  list(linhas = nrow(tab), fallback = nrow(fb))
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
  a <- commandArgs(trailingOnly = TRUE)
  opt <- function(k) { i <- which(a == k); if (length(i)) a[[i[[1L]] + 1L]] else stop("falta ", k) }
  r <- pc_fer_run(opt("--out-dir"), opt("--data"), dirname(dirname(lib)))
  writeLines(as.character(jsonlite::toJSON(r, auto_unbox = TRUE)), stdout())
}
