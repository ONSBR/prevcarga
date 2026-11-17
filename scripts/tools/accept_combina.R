#!/usr/bin/env Rscript
# =============================================================================
# accept_combina.R -- diagnostico da combina e da correlacao LIN x LIN_UNI sobre a medicao do aceite em Docker local (spec RegDin
# Linear Refactor, E8-T4; pedido do Gabriel em 2026-11-16). Dev-only, roda no R do HOST, fora do bundle. So le <out> e o store (somente
# leitura); nao chama docker e nao le variavel de ambiente. Complementa accept_report.R (nao altera report.tsv/md/json).
#
# Uso:
#   Rscript --vanilla accept_combina.R --out-dir <out> --data <store> [--n-min-sens 5]
#
# Entrada: index.tsv e units.medicao.tsv de <out> e os LONGs das unidades rc 0 da versao NOVA (dessem-linear e dessem-regdin).
# Os membros medidos sao os 6 do DESSEM (LIN_MED, LIN_MAX, LIN_MXMN, LIN_UNI, RGD_MED, RGD_MAX); RF, LGBM e HW nao fazem parte das 384
# unidades. "Com LIN_UNI" = esses 6; "sem LIN_UNI" = os 5 restantes (mesmas linhas, coluna f_LIN_UNI removida).
#
# (b) Correlacao: Pearson do erro (y_pred - y_true, y_true fresco do store) entre LIN_MED/LIN_MAX/LIN_MXMN e LIN_UNI, por intervalo
#     (chave area, origem, corte, alvo), por area e agregada (pool de todas as areas e media das correlacoes por area). D+1 e todos os D+.
# (a)/(c) Combina: prevcarga_combina_fit (o fit de producao) sobre o painel das 12 origens, por area e corte, nas duas configuracoes.
#     Producao (n_min 24) e sensibilidade (n_min = --n-min-sens, default 5; EXPLORATORIA, nao e a config de producao). Conta as celulas
#     (lead, corte, mes) nao-estacionarias (as que emitem warning.combina_nonstationary) e a distribuicao de iteracoes das celulas fitadas.
#     Origens esparsas (1 por mes) deixam cada celula com no maximo (meses anteriores) amostras: o relatorio traz n_max para deixar claro.
#
# Saidas (ASCII puro, tmp + rename) em <out>: combina.corr.tsv, combina.cells.tsv (todas as celulas), combina.summary.tsv, combina.nonstationary.tsv
# (lista das celulas nao-estacionarias por folha, lead e corte), combina.json e combina.md.
# Exits: 0 ok; 2 contract.accept_report_input; 9 integrity.accept_store_changed. stdout: envelope JSON; stderr: mensagem ASCII.
# =============================================================================

.pc_acm_lin <- c("LIN_MED", "LIN_MAX", "LIN_MXMN")

.pc_acm_bad <- function(where, reason) prevcarga_abort("contract.accept_report_input", where = where, details = list(reason = reason))

## LONG (TODOS os d_plus) das unidades rc 0 da versao nova, com membro, corte e area; y_true descartado (a verdade fresca vem depois)
pc_accept_combina_long <- function(out_dir, units = pc_accept_report_units(out_dir), index = pc_accept_report_index(out_dir)) {
  ok <- merge(units[versao == "new", c("unit"), with = FALSE], index[rc == "0", c("unit", "long_paths"), with = FALSE], by = "unit")
  data.table::setorderv(ok, "unit")
  parts <- list()
  for (i in seq_len(nrow(ok))) {
    paths <- strsplit(ok$long_paths[[i]], ",", fixed = TRUE)[[1L]]
    paths <- paths[nzchar(paths) & paths != "-" & grepl(.pc_acr_member_re, paths)]
    for (p in paths) {
      f <- file.path(out_dir, "units", ok$unit[[i]], p)
      if (!file.exists(f)) .pc_acm_bad(f, "LONG listado em index.tsv ausente")
      d <- tryCatch(data.table::as.data.table(as.data.frame(arrow::read_parquet(f))), error = function(e) .pc_acm_bad(f, "LONG ilegivel"))
      if (!all(.pc_acr_long_cols %in% names(d))) .pc_acm_bad(f, "LONG sem as colunas do schema")
      m <- regmatches(p, regexec(.pc_acr_member_re, p))[[1L]]
      d <- d[, .pc_acr_long_cols, with = FALSE]
      d[, `:=`(forecast_origin = as.character(forecast_origin), membro = m[[3L]], corte = m[[6L]])]
      parts[[length(parts) + 1L]] <- d
    }
  }
  if (!length(parts)) return(data.table::data.table())
  data.table::rbindlist(parts, use.names = TRUE)
}

## erro por intervalo: y_pred - y_true (y_true fresco). Chave (area, forecast_origin, corte, alvo).
pc_accept_combina_errors <- function(long, data_dir) {
  if (!nrow(long)) return(long)
  d <- .bt_join_truth(list(data_dir = data_dir), data.table::copy(long))
  d <- data.table::as.data.table(d)[is.finite(y_true) & is.finite(y_pred)]
  d[, erro := y_pred - y_true]
  d[, k := paste(area, forecast_origin, corte, as.numeric(target_datetime), sep = "|")]
  d[]
}

.pc_acm_cor <- function(x, y) if (length(x) >= 3L && stats::sd(x) > 0 && stats::sd(y) > 0) stats::cor(x, y) else NA_real_

## Pearson do erro LIN_x x LIN_UNI. escopo: "d1" (d_plus == 1) ou "todos". Linhas: area (cada uma), "AGREGADO" (pool) e "MEDIA_AREAS".
pc_accept_combina_corr <- function(err) {
  rows <- list()
  for (esc in c("d1", "todos")) {
    e <- if (esc == "d1") err[d_plus == 1L] else err
    uni <- e[membro == "LIN_UNI", c("k", "area", "erro"), with = FALSE]
    data.table::setnames(uni, "erro", "erro_uni")
    for (v in .pc_acm_lin) {
      x <- merge(e[membro == v, c("k", "erro"), with = FALSE], uni, by = "k")
      by_area <- x[, list(n = .N, pearson = .pc_acm_cor(erro, erro_uni)), by = "area"]
      data.table::setorderv(by_area, "area")
      for (i in seq_len(nrow(by_area)))
        rows[[length(rows) + 1L]] <- data.table::data.table(escopo = esc, variante = v, area = by_area$area[[i]], n = by_area$n[[i]], pearson = by_area$pearson[[i]])
      rows[[length(rows) + 1L]] <- data.table::data.table(escopo = esc, variante = v, area = "AGREGADO", n = nrow(x), pearson = .pc_acm_cor(x$erro, x$erro_uni))
      rows[[length(rows) + 1L]] <- data.table::data.table(escopo = esc, variante = v, area = "MEDIA_AREAS", n = sum(by_area$n),
                                                          pearson = if (any(is.finite(by_area$pearson))) mean(by_area$pearson, na.rm = TRUE) else NA_real_)
    }
  }
  r <- data.table::rbindlist(rows)
  if (!nrow(r)) return(data.table::data.table(escopo = character(), variante = character(), area = character(), n = integer(), pearson = double()))
  r[]
}

## painel largo (area, origem, lead, corte) com f_<membro>, y_true fresco, ym (mes da origem) e interval_start (borda do LONG)
pc_accept_combina_panel <- function(err_area, tz) {
  e <- data.table::copy(err_area)
  e[, target_datetime := lubridate::with_tz(target_datetime, tz)]
  tgt <- unique(e[, c("area", "forecast_origin", "lead", "corte", "target_datetime", "y_true", "granularity_min"), with = FALSE])
  tgt <- tgt[!duplicated(tgt, by = c("area", "forecast_origin", "lead", "corte"))]
  wide <- data.table::dcast(e, area + forecast_origin + lead + corte ~ membro, value.var = "y_pred", fill = NA_real_)
  data.table::setnames(wide, setdiff(names(wide), c("area", "forecast_origin", "lead", "corte")),
                       paste0("f_", setdiff(names(wide), c("area", "forecast_origin", "lead", "corte"))))
  p <- merge(tgt, wide, by = c("area", "forecast_origin", "lead", "corte"), sort = FALSE)
  p <- data.table::as.data.table(prevcarga_long_attach_interval_start(p, tz = tz))
  data.table::setnames(p, "corte", "cut")
  p[, ym := substr(forecast_origin, 1L, 7L)]
  data.table::setorderv(p, c("cut", "lead", "forecast_origin"))
  p[]
}

## uma configuracao (membros) e um n_min: celulas do fit de producao. `drop` = colunas f_* removidas (config sem LIN_UNI)
pc_accept_combina_cells <- function(panel, n_min, config, drop = character()) {
  p <- data.table::copy(panel)
  for (c in drop) if (c %in% names(p)) p[, (c) := NULL]
  ## o aviso de celula nao-estacionaria do fit_convex ja vira a coluna `nonstationary`; aqui nao se repete no stderr
  w <- withCallingHandlers(prevcarga_combina_fit(p, n_min = n_min), warning = function(w) {
    if (grepl("celula nao-estacionaria", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
  })
  if (!nrow(w)) return(data.table::data.table())
  out <- data.table::data.table(config = config, n_min = as.integer(n_min), lead = w$lead, cut = w$cut, ym = w$ym, fallback = as.character(w$fallback),
                                n = w$n, iters = w$iters, nonstationary = w$nonstationary %in% TRUE,
                                n_membros = vapply(w$w, length, integer(1L)))
  out[]
}

## todas as areas, configuracoes e n_min; o painel e por area (cada folha e uma combina)
pc_accept_combina_run_cells <- function(err, tz, n_mins = c(24L, 5L)) {
  areas <- sort(unique(err$area), method = "radix")
  cells <- list()
  for (a in areas) {
    p <- pc_accept_combina_panel(err[area == a], tz)
    for (nm in n_mins) {
      cells[[length(cells) + 1L]] <- cbind(data.table::data.table(area = a), pc_accept_combina_cells(p, nm, "com_LIN_UNI"))
      cells[[length(cells) + 1L]] <- cbind(data.table::data.table(area = a), pc_accept_combina_cells(p, nm, "sem_LIN_UNI", drop = "f_LIN_UNI"))
    }
  }
  r <- data.table::rbindlist(cells, use.names = TRUE, fill = TRUE)
  if (!nrow(r)) return(r)
  r[, cap := iters >= 500L]
  r[]
}

.pc_acm_bins <- c("0", "1-50", "51-100", "101-200", "201-499", "500")
.pc_acm_bin <- function(it) {
  cut(it, breaks = c(-Inf, 0, 50, 100, 200, 499, Inf), labels = .pc_acm_bins, right = TRUE)
}

pc_accept_combina_summary <- function(cells) {
  if (!nrow(cells)) return(data.table::data.table())
  s <- cells[, {
    fit <- is.na(fallback)
    it <- iters[fit]
    q <- if (length(it)) stats::quantile(it, c(0.5, 0.9), names = FALSE, type = 7) else c(NA_real_, NA_real_)
    list(celulas = .N, fitadas = sum(fit), fallback_n_min = sum(fallback %in% "n_min"), fallback_first_month = sum(fallback %in% "first_month"),
         fallback_no_member = sum(fallback %in% "no_member"), nonstationary = sum(nonstationary), n_max = max(n),
         iters_min = if (length(it)) min(it) else NA_integer_, iters_mediana = q[[1L]], iters_p90 = q[[2L]],
         iters_max = if (length(it)) max(it) else NA_integer_, no_cap = sum(it >= 500L))
  }, by = c("config", "n_min")]
  data.table::setorderv(s, c("n_min", "config"), order = c(-1L, -1L))
  s[]
}

pc_accept_combina_hist <- function(cells) {
  f <- cells[is.na(fallback)]
  if (!nrow(f)) return(data.table::data.table(config = character(), n_min = integer(), faixa = character(), celulas = integer()))
  f[, faixa := .pc_acm_bin(iters)]
  h <- f[, list(celulas = .N), by = c("config", "n_min", "faixa")]
  g <- data.table::CJ(config = unique(cells$config), n_min = unique(cells$n_min), faixa = factor(.pc_acm_bins, levels = .pc_acm_bins))
  h <- merge(g, h, by = c("config", "n_min", "faixa"), all.x = TRUE)
  h[is.na(celulas), celulas := 0L]
  h[, faixa := as.character(faixa)]
  h[order(-n_min, config, match(faixa, .pc_acm_bins))][]
}

pc_accept_combina_nonstat <- function(cells) {
  x <- cells[nonstationary == TRUE, c("config", "n_min", "area", "lead", "cut", "ym", "n", "iters"), with = FALSE]
  if (!nrow(x)) return(x)
  data.table::setorderv(x, c("n_min", "config", "area", "cut", "lead", "ym"), order = c(-1L, 1L, 1L, 1L, 1L, 1L))
  x[]
}

.pc_acm_fmt <- function(x) {
  if (is.numeric(x)) vapply(x, function(v) if (is.na(v)) "-" else if (is.integer(v) || v == round(v)) format(v, scientific = FALSE, trim = TRUE) else
    formatC(v, digits = 6, format = "g"), "") else ifelse(is.na(x), "-", as.character(x))
}
.pc_acm_tsv_lines <- function(dt) {
  if (!ncol(dt)) return("")
  c(paste(names(dt), collapse = "\t"), if (nrow(dt)) do.call(paste, c(lapply(dt, .pc_acm_fmt), sep = "\t")) else character())
}
.pc_acm_md_table <- function(dt) {
  if (!nrow(dt)) return("(sem linhas)")
  c(paste0("| ", paste(names(dt), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(dt)), collapse = "|"), "|"),
    paste0("| ", do.call(paste, c(lapply(dt, .pc_acm_fmt), sep = " | ")), " |"))
}

pc_accept_combina_md_lines <- function(corr, summ, hist, nonstat, meta) {
  nm <- function(cfg, nmin) { x <- summ[config == cfg & n_min == nmin]; if (nrow(x)) x$nonstationary[[1L]] else NA_integer_ }
  c("# Combina e correlacao LIN x LIN_UNI (medicao D+1, 12 origens x 8 areas)", "",
    sprintf("RUN_ID: %s", meta$run_id),
    "Membros medidos: os 6 do DESSEM (LIN_MED, LIN_MAX, LIN_MXMN, LIN_UNI, RGD_MED, RGD_MAX); RF, LGBM e HW nao estao nas 384 unidades.",
    "Com LIN_UNI = 6 membros; sem LIN_UNI = 5 membros (mesmas linhas, sem a coluna f_LIN_UNI). Nao e a contagem 9 x 8 do SIN.", "",
    "## Correlacao de Pearson do erro (y_pred - y_true) LIN_x x LIN_UNI, por intervalo", "", .pc_acm_md_table(corr), "",
    "## Combina: resumo por configuracao (n_min 24 = producao; menor n_min = sensibilidade exploratoria)", "", .pc_acm_md_table(summ), "",
    sprintf("Celulas nao-estacionarias (warning.combina_nonstationary): producao com LIN_UNI=%s, sem LIN_UNI=%s.", nm("com_LIN_UNI", 24L), nm("sem_LIN_UNI", 24L)), "",
    "## Distribuicao de iteracoes das celulas fitadas", "", .pc_acm_md_table(hist), "",
    "## Celulas nao-estacionarias (area, corte, lead, mes)", "", .pc_acm_md_table(nonstat))
}

pc_accept_combina_write_ascii <- function(lines, path) {
  txt <- paste0(paste(lines, collapse = "\n"), "\n")
  if (any(utf8ToInt(txt) > 127L)) prevcarga_abort("contract.accept_report_input", where = path, details = list(reason = "nao-ASCII"))
  tmp <- paste0(path, ".tmp"); writeLines(lines, tmp, useBytes = TRUE)
  if (!isTRUE(file.rename(tmp, path))) { unlink(tmp); prevcarga_abort("contract.accept_report_input", where = path) }
  invisible(path)
}

pc_accept_combina_run <- function(out_dir, data_dir, n_min_sens = 5L) {
  if (!dir.exists(out_dir)) .pc_acm_bad(out_dir, "--out-dir nao e diretorio")
  if (!dir.exists(data_dir)) .pc_acm_bad(data_dir, "--data nao e diretorio")
  before <- .bench_data_listing(data_dir)
  units <- pc_accept_report_units(out_dir); index <- pc_accept_report_index(out_dir)
  long <- pc_accept_combina_long(out_dir, units, index)
  err <- pc_accept_combina_errors(long, data_dir)
  tz <- if (nrow(err)) prevcarga_deck_manifest(data_dir)$timezone$load else "UTC"
  corr <- pc_accept_combina_corr(err)
  cells <- pc_accept_combina_run_cells(err, tz, unique(c(24L, as.integer(n_min_sens))))
  summ <- pc_accept_combina_summary(cells)
  hist <- pc_accept_combina_hist(cells)
  nonstat <- pc_accept_combina_nonstat(cells)
  meta <- .pc_acr_meta(out_dir)
  after <- .bench_data_listing(data_dir)
  if (!identical(before, after))
    prevcarga_abort("integrity.accept_store_changed", where = data_dir, details = list(antes = nrow(before), depois = nrow(after)))
  pc_accept_combina_write_ascii(.pc_acm_tsv_lines(corr), file.path(out_dir, "combina.corr.tsv"))
  pc_accept_combina_write_ascii(.pc_acm_tsv_lines(cells[, !"cap"]), file.path(out_dir, "combina.cells.tsv"))
  pc_accept_combina_write_ascii(.pc_acm_tsv_lines(summ), file.path(out_dir, "combina.summary.tsv"))
  pc_accept_combina_write_ascii(.pc_acm_tsv_lines(nonstat), file.path(out_dir, "combina.nonstationary.tsv"))
  pc_accept_combina_write_ascii(pc_accept_combina_md_lines(corr, summ, hist, nonstat, meta), file.path(out_dir, "combina.md"))
  js <- as.character(jsonlite::toJSON(list(run_id = meta$run_id, correlacao = as.data.frame(corr), resumo = as.data.frame(summ),
                                           iteracoes = as.data.frame(hist), nao_estacionarias = as.data.frame(nonstat)),
                                      auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE))
  pc_accept_combina_write_ascii(js, file.path(out_dir, "combina.json"))
  list(corr_rows = nrow(corr), cells = nrow(cells), nonstationary = nrow(nonstat))
}

.pc_acm_opt <- function(args, key) { i <- which(args == key); if (length(i) && i[[1L]] < length(args)) args[[i[[1L]] + 1L]] else NULL }

pc_accept_combina_main <- function(args) {
  need <- function(key) { v <- .pc_acm_opt(args, key); if (is.null(v)) .pc_acm_bad(key, "opcao obrigatoria"); v }
  ns <- .pc_acm_opt(args, "--n-min-sens")
  pc_accept_combina_run(need("--out-dir"), need("--data"), if (is.null(ns)) 5L else as.integer(ns))
}

if (sys.nframe() == 0L) {
  fa <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  lib <- file.path(dirname(dirname(normalizePath(fa[[1L]]))), "lib")
  options(prevcarga.lib = lib)
  if (file.exists(file.path(lib, "runtime.R"))) source(file.path(lib, "runtime.R"))
  if (exists("prevcarga_runtime_fix", mode = "function")) prevcarga_runtime_fix()
  for (m in c("errors", "topology", "provenance", "config", "schema_long", "dates", "deck_reader", "evaluation", "backtest", "combination"))
    source(file.path(lib, paste0(m, ".R")))
  source(file.path(dirname(lib), "tools", "bench_forecast.R"))
  source(file.path(dirname(lib), "tools", "accept_report.R"))
  out <- prevcarga_with_errors(pc_accept_combina_main(commandArgs(trailingOnly = TRUE)))
  env <- prevcarga_envelope(verb = "accept_combina", cond = out$cond, result = if (is.null(out$cond)) out$value else out$cond$result)
  env$error_id <- if (is.null(out$cond)) NULL else out$cond$id
  writeLines(as.character(jsonlite::toJSON(env, auto_unbox = TRUE, null = "null", digits = NA)), stdout())
  if (!is.null(out$cond)) writeLines(prevcarga_render_error_text(out$cond), stderr())
  quit(save = "no", status = if (is.null(out$cond)) 0L else as.integer(out$cond$exit_code))
}
