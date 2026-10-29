## scripts/lib/cli/score.R - verbo auto-registrado `score` (M3.3; descoberto por scripts/lib/cli.R, M1.4).
##
## Review D+1: pontua um LONG publicado de 8 colunas contra a carga realizada FRESCA do deck. A logica mora em
## evaluation.R (prevcarga_score; metrica so de la, principio 5); o deck so e lido por deck_reader.R. Este arquivo e o
## handler fino: modelo -> escopo de area -> gate-0 (prevcarga_validate_deck, salvo --skip-validate) -> leitor do
## LONG -> modulo -> result. Erros e exits vem do catalogo (ADR-0040) pelo handler de topo da fachada: sem quit(),
## sem cat de erro, sem env vars. Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve
## o `result` (a fachada o embrulha no envelope). Aviso nothing_to_score = prevcarga_warn (exit 0).
## Requer errors.R, config.R, topology.R, schema_long.R, provenance.R, deck_reader.R, dates.R, validation.R, evaluation.R.

.prevcarga_sc_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

## Gate-0: reprovado => abort com o id do primeiro erro da classe dominante do proprio validate-deck.
.prevcarga_sc_gate0 <- function(ctx, scope) {
  v <- prevcarga_validate_deck(ctx, ctx$origin, scope)
  if (isTRUE(v$ok)) return(invisible(v))
  f <- v$findings
  i <- which(f$level == "error" & f$class == v$class)[[1L]]
  n_err <- sum(f$level == "error")
  prevcarga_abort(paste0(f$class[[i]], ".", f$what[[i]]),
                  where = if (is.na(f$where[[i]]) || !nzchar(f$where[[i]])) "gate-0 validate-deck" else f$where[[i]],
                  details = list(findings = as.integer(n_err), gate = "validate-deck"))
}

## --- result do envelope (datas ISO; NA -> null; skill/baseline null quando ausentes; sem long/baseline_long) ---
.prevcarga_sc_iso_dt <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return(NULL)
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", format(x[[1L]], "%Y-%m-%dT%H:%M:%S%z", tz = attr(x, "tzone")))
}

.prevcarga_sc_rows <- function(df) {
  if (is.null(df) || !nrow(df)) return(list())
  as_int <- c("d_plus", "n")
  lapply(seq_len(nrow(df)), function(i) {
    out <- lapply(names(df), function(k) {
      x <- df[[k]][[i]]
      if (is.na(x)) NULL else if (k %in% as_int) as.integer(x) else if (is.numeric(x)) as.numeric(x) else x
    })
    stats::setNames(out, names(df))
  })
}

.prevcarga_score_result <- function(r) {
  list(schema_version = 1L,
       origins = as.list(format(r$origins, "%Y-%m-%d")),
       available_until = .prevcarga_sc_iso_dt(r$available_until),
       data_hash = r$data_hash,
       baseline = r$baseline,
       headline = list(d_plus = r$headline$d_plus, rows = .prevcarga_sc_rows(r$headline$rows)),
       scorecard = .prevcarga_sc_rows(r$scorecard))
}

## --- render texto ASCII (nenhuma metrica calculada aqui) --------------------------------------------------------
.prevcarga_sc_num <- function(x, d = 2L) {
  if (is.null(x) || !length(x) || is.na(x)) "-" else formatC(as.numeric(x), format = "f", digits = d)
}

.prevcarga_sc_table <- function(head, rows) {
  m <- if (length(rows)) rbind(head, do.call(rbind, rows)) else matrix(head, nrow = 1L)
  w <- apply(m, 2L, function(c) max(nchar(c)))
  apply(m, 1L, function(r) sub("\\s+$", "", paste(mapply(function(x, k) formatC(x, width = -k), r, w), collapse = " ")))
}

.prevcarga_sc_line <- function(r, lead, skill) {
  c(lead, as.character(r$n), .prevcarga_sc_num(r$wmape), .prevcarga_sc_num(r$mape), .prevcarga_sc_num(r$mae, 1L),
    .prevcarga_sc_num(r$rmse, 1L), .prevcarga_sc_num(r$bias, 1L), .prevcarga_sc_num(r$bias_pct), .prevcarga_sc_num(r$peak),
    if (skill) .prevcarga_sc_num(r$skill, 3L))
}

.render_score_text <- function(res) {
  hash <- if (is.null(res$data_hash)) "-" else substr(res$data_hash, 1L, 12L)
  head <- sprintf("score origins=%s available_until=%s data_hash=%s baseline=%s",
                  if (length(res$origins)) paste(unlist(res$origins), collapse = ",") else "-",
                  if (is.null(res$available_until)) "-" else res$available_until, hash,
                  if (is.null(res$baseline)) "-" else res$baseline$kind)
  skill <- !is.null(res$baseline)
  cols <- c("OBS", "WMAPE", "MAPE", "MAE", "RMSE", "BIAS", "BIAS%", "PEAK", if (skill) "SKILL")
  hl <- .prevcarga_sc_table(c("AREA", cols), lapply(res$headline$rows, function(r) .prevcarga_sc_line(r, r$area, skill)))
  sc <- .prevcarga_sc_table(c("AREA", "D_PLUS", cols), lapply(res$scorecard, function(r) {
    ln <- .prevcarga_sc_line(r, r$area, skill)
    c(ln[[1L]], if (identical(as.integer(r$d_plus), 0L)) "0 resto-do-dia" else as.character(r$d_plus), ln[-1L])
  }))
  c(head, "", "MANCHETE D+1", hl, "", "HORIZONTE", sc)
}

## --- Handler fino ------------------------------------------------------------------------------------------------
cmd_score <- function(ctx, args) {
  model <- .prevcarga_sc_opt(args, "--model")
  area <- .prevcarga_sc_opt(args, "--area")
  if (!is.null(model)) prevcarga_model_spec(model, root = ctx$root)
  prevcarga_require_data_dir(ctx)
  if (!is.null(area)) .prevcarga_score_areas(NULL, area, .prevcarga_score_topo(ctx))   # topology.area_unknown antes de ler o deck
  if (!isTRUE(args[["--skip-validate"]])) .prevcarga_sc_gate0(ctx, list(model = model, area = area))
  ctx$tz <- .prevcarga_eval_tz(ctx)
  long <- prevcarga_read_long(.prevcarga_sc_opt(args, "--long"), ctx)
  r <- prevcarga_score(long, ctx, area = area, baseline_kind = .prevcarga_sc_opt(args, "--baseline"), origin = ctx$origin)
  for (w in r$warnings) prevcarga_warn(paste0("warning.", w$type), details = list(type = w$type))
  res <- .prevcarga_score_result(r)
  if (identical(ctx$format, "json")) res else .render_score_text(res)
}

prevcarga_verb_score <- list(
  name    = "score",
  usage   = "prevcarga score --long=<f> [--area=<a>] [--baseline=<b>] [--skip-validate]",
  options = c(
    "--long=<f>              LONG publicado (parquet, 8 colunas) a pontuar",
    "--baseline=<b>          persistence ou seasonal_naive (skill opcional)",
    "--skip-validate         Pula o gate-0 prevcarga_validate_deck"),
  handler = function(ctx, args) cmd_score(ctx, args),
  owner   = "M3",
  exempt  = FALSE,
  modules = c("topology.R", "schema_long.R", "provenance.R", "deck_reader.R", "dates.R", "validation.R", "evaluation.R")
)
