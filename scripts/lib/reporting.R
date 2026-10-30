## reporting.R -- primitivas HTML/SVG por string R (reusaveis pelo M7) e o relatorio DIAGNOSTICO do `forecast --report` (M6.5).
##
## O relatorio so descreve a rodada: curva do dia por nivel, available_until (global e por area), pesos dos membros, k-hat
## exatamente como veio do reconcile (versao, khat_origin, khat_age_days; nada de perfil lido ou recalculado), diferenca vs
## persistencia O-1 e vs sazonal O-7 (baselines ja calculados por evaluation.R sobre carga <= available_until) e proveniencia.
## Nunca le a verdade do painel nem emite metrica de acerto. Sem script, sem fonte externa, UTF-8 + LF, sem carimbo de hora:
## o mesmo insumo da o mesmo arquivo byte a byte. Todo texto interpolado passa por `prevcarga_html_escape` (escapador unico).
## Requer errors.R carregado antes (data.table para o heatmap).

## --- primitivas (M7 reusa) --------------------------------------------------------------------------------------

prevcarga_html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE); gsub("'", "&#39;", x, fixed = TRUE)
}

## Numero formatado de forma deterministica: `fmt` = "%.Nf" (N casas); NA vira "-".
.report_num <- function(x, fmt = "%.2f") {
  d <- suppressWarnings(as.integer(sub("^%\\.([0-9]+)f$", "\\1", fmt)))
  if (is.na(d) || !grepl("^%\\.[0-9]+f$", fmt)) prevcarga_abort("contract.report_input", where = "fmt")
  out <- formatC(as.numeric(x), format = "f", digits = d)
  out[!is.finite(as.numeric(x))] <- "-"
  trimws(out)
}

.report_css <- paste(
  "body{font-family:sans-serif;margin:1.5rem;color:#222}",
  "h1{font-size:1.4rem}h2{font-size:1.1rem;border-bottom:1px solid #ccc;padding-bottom:.2rem}",
  "table{border-collapse:collapse;margin:.5rem 0}th,td{border:1px solid #bbb;padding:.2rem .5rem;text-align:right}",
  "th:first-child,td:first-child{text-align:left}",
  ".destaque{background:#fff3b0;padding:.4rem .6rem;border-left:4px solid #d9a400;font-weight:bold}",
  ".nota{color:#555;font-size:.85rem}svg{display:block;margin:.3rem 0}", sep = "\n")

.report_titles <- c(curva = "Curva do dia por nivel", `available-until` = "available_until", pesos = "Pesos dos membros",
                    khat = "k-hat do reconcile", baselines = "Diferenca vs persistencia O-1 e sazonal O-7",
                    proveniencia = "Proveniencia")

## `sections`: lista nomeada (nome = id da secao) de fragmentos HTML ja escapados. Documento UTF-8, CSS inline, sem script.
prevcarga_html_page <- function(title, sections, css = .report_css) {
  ids <- names(sections)
  body <- vapply(seq_along(sections), function(i) {
    h <- if (ids[[i]] %in% names(.report_titles)) .report_titles[[ids[[i]]]] else ids[[i]]
    sprintf("<section id=\"%s\">\n<h2>%s</h2>\n%s\n</section>", prevcarga_html_escape(ids[[i]]), prevcarga_html_escape(h),
            paste(sections[[i]], collapse = "\n"))
  }, "")
  paste(c("<!doctype html>", "<html lang=\"pt-BR\">", "<head>", "<meta charset=\"utf-8\">",
          sprintf("<title>%s</title>", prevcarga_html_escape(title)), sprintf("<style>\n%s\n</style>", css), "</head>",
          "<body>", sprintf("<h1>%s</h1>", prevcarga_html_escape(title)), body, "</body>", "</html>"), collapse = "\n")
}

## Tabela linhas x colunas de `value`, com cor de fundo por faixa (min..max) calculada em R. `fmt` = "%.Nf".
prevcarga_html_heatmap <- function(dt, row, col, value, fmt = "%.2f") {
  dt <- as.data.frame(dt)
  rs <- unique(as.character(dt[[row]])); cs <- unique(as.character(dt[[col]]))
  v <- as.numeric(dt[[value]])
  fin <- v[is.finite(v)]
  lo <- if (length(fin)) min(fin) else 0; hi <- if (length(fin)) max(fin) else 0
  shade <- function(x) {
    if (!is.finite(x)) return("#eeeeee")
    t <- if (hi > lo) (x - lo) / (hi - lo) else 0
    sprintf("#%02x%02x%02x", as.integer(round(255 - t * 120)), as.integer(round(255 - t * 80)), 255L)
  }
  head <- paste0("<tr><th></th>", paste0("<th>", prevcarga_html_escape(cs), "</th>", collapse = ""), "</tr>")
  rows <- vapply(rs, function(r) {
    cells <- vapply(cs, function(cc) {
      i <- which(as.character(dt[[row]]) == r & as.character(dt[[col]]) == cc)
      x <- if (length(i)) v[[i[[1L]]]] else NA_real_
      sprintf("<td style=\"background:%s\">%s</td>", shade(x), .report_num(x, fmt))
    }, "")
    paste0("<tr><th>", prevcarga_html_escape(r), "</th>", paste(cells, collapse = ""), "</tr>")
  }, "")
  paste(c("<table>", head, rows, "</table>"), collapse = "\n")
}

## Curva SVG inline. `x` = posicao numerica (NULL = indice); `labels` = rotulos de x (so o primeiro e o ultimo aparecem).
prevcarga_svg_profile <- function(x = NULL, y, labels = NULL, width = 720, height = 220) {
  ok <- is.finite(y)
  if (is.null(x)) x <- seq_along(y)
  if (is.null(labels)) labels <- as.character(x)
  x <- as.numeric(x)[ok]; y <- as.numeric(y)[ok]; labels <- as.character(labels)[ok]
  n <- length(y)
  if (!n) return("<p class=\"nota\">sem dados</p>")
  ry <- range(y); rx <- range(x)
  xs <- if (n > 1L && diff(rx) > 0) (x - rx[1]) / diff(rx) * (width - 40) + 30 else rep(30, n)
  ys <- height - 20 - (y - ry[1]) / max(diff(ry), 1e-9) * (height - 40)
  pts <- paste(formatC(xs, format = "f", digits = 1), formatC(ys, format = "f", digits = 1), sep = ",", collapse = " ")
  esc <- prevcarga_html_escape
  sprintf(paste0("<svg width=\"%d\" height=\"%d\" role=\"img\" aria-label=\"%s\"><polyline fill=\"none\" stroke=\"#1f5f8b\" ",
                 "points=\"%s\"/><text x=\"30\" y=\"%d\" font-size=\"10\">%s</text><text x=\"%d\" y=\"%d\" font-size=\"10\" ",
                 "text-anchor=\"end\">%s</text><text x=\"2\" y=\"12\" font-size=\"10\">%s</text>",
                 "<text x=\"2\" y=\"%d\" font-size=\"10\">%s</text></svg>"),
          as.integer(width), as.integer(height), esc(paste(labels[1L], labels[n], sep = " - ")), pts,
          as.integer(height - 4L), esc(labels[1L]), as.integer(width - 4L), as.integer(height - 4L), esc(labels[n]),
          esc(.report_num(ry[2L], "%.1f")), as.integer(height - 22L), esc(.report_num(ry[1L], "%.1f")))
}

## --- blocos ------------------------------------------------------------------------------------------------

.report_table <- function(header, rows) {
  th <- paste0("<tr>", paste0("<th>", prevcarga_html_escape(header), "</th>", collapse = ""), "</tr>")
  tr <- vapply(rows, function(r) paste0("<tr>", paste0("<td>", prevcarga_html_escape(r), "</td>", collapse = ""), "</tr>"), "")
  paste(c("<table>", th, tr, "</table>"), collapse = "\n")
}

## Niveis: SIN, subsistemas (provenance$subsistemas) e folhas (o resto), nessa ordem; cada area em ordem radix.
.report_level <- function(areas, subsistemas) {
  ord <- function(x) x[order(x, method = "radix")]
  list(SIN = intersect("SIN", areas), subsistema = ord(intersect(areas, subsistemas)),
       folha = ord(setdiff(areas, c("SIN", subsistemas))))
}

## Dia O (d_plus == 0) de cada area, ordenado no tempo; uma linha por alvo.
.report_day <- function(long) {
  d <- as.data.frame(long)[, c("area", "target_datetime", "y_pred", "d_plus"), drop = FALSE]
  d <- d[!is.na(d$d_plus) & d$d_plus == 0L, , drop = FALSE]
  d <- d[order(d$area, as.numeric(d$target_datetime), method = "radix"), , drop = FALSE]
  d[!duplicated(d[c("area", "target_datetime")]), , drop = FALSE]
}

.report_clock <- function(t) format(t, "%d/%m %H:%M")

.report_curva <- function(long, subsistemas = character()) {
  d <- .report_day(long)
  lv <- .report_level(unique(as.character(d$area)), subsistemas)
  out <- character()
  for (nivel in names(lv)) for (a in lv[[nivel]]) {
    s <- d[d$area == a, , drop = FALSE]
    out <- c(out, sprintf("<h3>%s: %s</h3>", prevcarga_html_escape(nivel), prevcarga_html_escape(a)),
             prevcarga_svg_profile(as.numeric(s$target_datetime), s$y_pred, .report_clock(s$target_datetime)))
  }
  if (!length(out)) "<p class=\"nota\">sem previsao para o dia da origem</p>" else paste(out, collapse = "\n")
}

.report_available_until <- function(provenance) {
  por <- provenance$available_until_por_area
  glob <- sprintf("<p class=\"destaque\">available_until (global): %s</p>", prevcarga_html_escape(provenance$available_until))
  if (!length(por)) return(paste(glob, "<p class=\"nota\">sem corte por area</p>", sep = "\n"))
  nm <- names(por)[order(names(por), method = "radix")]
  paste(glob, .report_table(c("area", "available_until"), lapply(nm, function(a) c(a, as.character(por[[a]])))), sep = "\n")
}

.report_pesos <- function(weights) {
  w <- as.data.frame(weights)
  if (!nrow(w)) return("<p class=\"nota\">sem pesos</p>")
  long <- rbind(data.frame(area = w$area, coluna = paste0(w$member, " peso"), v = w$weight),
                data.frame(area = w$area, coluna = paste0(w$member, " intercepto"), v = w$intercept))
  long <- long[order(long$area, long$coluna, method = "radix"), , drop = FALSE]
  prevcarga_html_heatmap(long, "area", "coluna", "v", "%.4f")
}

## k-hat exatamente como veio do reconcile: cada campo e copiado como texto, sem ler perfil nem recalcular.
.report_khat <- function(khat) {
  if (!length(khat)) return("<p class=\"nota\">nenhum subsistema fechado: sem k-hat</p>")
  .report_table(c("subsistema", "versao", "khat_origin", "khat_age_days"),
                lapply(khat, function(k) c(k$ss, k$versao, format(k$khat_origin), as.character(k$khat_age_days))))
}

## Diferenca media do dia O do previsto contra cada baseline (ja calculado por evaluation.R sobre carga <= available_until).
## E descritiva (previsto - baseline), nunca contra a verdade.
.report_baselines <- function(long, baselines) {
  d <- .report_day(long)
  nomes <- c(persistence_o1 = "persistencia O-1", seasonal_o7 = "sazonal O-7")
  rows <- list()
  for (a in unique(as.character(d$area))) for (b in names(nomes)) {
    bb <- as.data.frame(baselines[[b]])
    s <- d[d$area == a, , drop = FALSE]
    base <- bb$y_base[match(paste(a, as.numeric(s$target_datetime)), paste(bb$area, as.numeric(bb$target_datetime)))]
    ok <- is.finite(base) & is.finite(s$y_pred)
    v <- if (any(ok) && mean(base[ok]) != 0) 100 * (mean(s$y_pred[ok]) - mean(base[ok])) / mean(base[ok]) else NA_real_
    rows[[length(rows) + 1L]] <- data.frame(area = a, baseline = nomes[[b]], dif = v)
  }
  if (!length(rows)) return("<p class=\"nota\">sem baselines</p>")
  paste(prevcarga_html_heatmap(do.call(rbind, rows), "area", "baseline", "dif", "%.2f"),
        "<p class=\"nota\">diferenca media percentual do previsto sobre o baseline, no dia da origem</p>", sep = "\n")
}

.report_proveniencia <- function(provenance) {
  fixed <- c("versao", "origem", "load_label", "timezone", "weather_issue_cutoff")
  skip <- c(fixed, "available_until", "available_until_por_area", "subsistemas")
  extra <- setdiff(names(provenance), skip)
  extra <- extra[order(extra, method = "radix")]
  extra <- extra[vapply(extra, function(k) is.atomic(provenance[[k]]) && length(provenance[[k]]) == 1L, NA)]
  keys <- c(fixed, extra)
  .report_table(c("campo", "valor"), lapply(keys, function(k) c(k, paste(format(provenance[[k]]), collapse = ","))))
}

## --- API publica --------------------------------------------------------------------------------------------

.report_check <- function(long, weights, khat, provenance, baselines) {
  bad <- function(w) prevcarga_abort("contract.report_input", where = w)
  if (!is.data.frame(long) || !all(c("area", "target_datetime", "y_pred", "d_plus") %in% names(long))) bad("long")
  if (!is.data.frame(weights) || !all(c("area", "member", "weight", "intercept") %in% names(weights))) bad("weights")
  if (!is.list(khat)) bad("khat")
  need <- c("versao", "origem", "available_until", "available_until_por_area", "load_label", "timezone", "weather_issue_cutoff")
  if (!is.list(provenance) || !all(need %in% names(provenance))) bad("provenance")
  if (!is.list(baselines) || !all(c("persistence_o1", "seasonal_o7") %in% names(baselines))) bad("baselines")
  invisible(TRUE)
}

.report_pdf <- function(long, weights, khat, provenance, baselines, out)
  prevcarga_abort("todo.not_implemented", where = "report --format pdf (M6.10)")

## Grava `<out>/report.html` (binario UTF-8, LF) e devolve o caminho. `provenance$subsistemas` (opcional) separa os niveis.
prevcarga_report <- function(long, weights, khat, provenance, baselines, format = c("html", "pdf"), out) {
  format <- match.arg(format)
  .report_check(long, weights, khat, provenance, baselines)
  if (format == "pdf") return(.report_pdf(long, weights, khat, provenance, baselines, out))
  sections <- list(curva = .report_curva(long, as.character(provenance$subsistemas)),
                   `available-until` = .report_available_until(provenance), pesos = .report_pesos(weights),
                   khat = .report_khat(khat), baselines = .report_baselines(long, baselines),
                   proveniencia = .report_proveniencia(provenance))
  html <- prevcarga_html_page(paste("prevcarga forecast", provenance$origem), sections)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out, "report.html")
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(html, "\n"))), con)
  path
}
