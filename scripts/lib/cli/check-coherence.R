## scripts/lib/cli/check-coherence.R - verbo auto-registrado `check-coherence` (M3.4; descoberto por scripts/lib/cli.R, M1.4).
##
## Gate duro de entrega: prova a coerencia hierarquica de um LONG reconciliado (soma do SIN, k-hat por subsistema,
## completude estrutural, escopo de areas e alinhamento de grade). Toda a logica mora em evaluation.R
## (prevcarga_check_coherence; topologia como dado); este arquivo e so o handler fino: leitor unico do LONG ->
## modulo -> result. Reprovado = prevcarga_abort() com o id da classe-dona (3 completude / 4 topology / 11 coherence,
## ADR-0040) e os findings em `result`; quem emite o envelope e fixa o exit e o handler de topo da fachada.
## Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result`.
## Sem quit(), sem env vars, saida ASCII. Requer errors.R, config.R, topology.R, schema_long.R, evaluation.R
## (e deck_reader.R/provenance.R para o fuso do deck).

.prevcarga_cc_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

.prevcarga_cc_iso_dt <- function(x) {
  if (is.na(x)) return(NULL)
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", format(x, "%Y-%m-%dT%H:%M:%S%z", tz = attr(x, "tzone")))
}

## findings (data.frame) -> lista de registros JSON; NA -> null; datas ISO.
.prevcarga_cc_rows <- function(f) {
  if (is.null(f) || !nrow(f)) return(list())
  lapply(seq_len(nrow(f)), function(i) {
    num <- function(x) if (is.na(x)) NULL else as.numeric(x)
    list(check = f$check[[i]], area = f$area[[i]],
         forecast_origin = if (is.na(f$forecast_origin[[i]])) NULL else format(f$forecast_origin[[i]], "%Y-%m-%d"),
         target_datetime = .prevcarga_cc_iso_dt(f$target_datetime[[i]]),
         value = num(f$value[[i]]), expected = num(f$expected[[i]]),
         class = f$class[[i]], id = f$id[[i]], detail = f$detail[[i]])
  })
}

.prevcarga_cc_result <- function(r) list(schema_version = 1L, ok = isTRUE(r$ok), class = r$class,
                                         findings = .prevcarga_cc_rows(r$findings))

## --- render texto ASCII -----------------------------------------------------------------------------------------
.prevcarga_cc_dash <- function(x) if (is.null(x) || !length(x) || is.na(x)) "-" else as.character(x)

.render_check_coherence_text <- function(res) {
  rows <- lapply(res$findings, function(r) c(r$check, r$area, .prevcarga_cc_dash(r$forecast_origin),
    .prevcarga_cc_dash(r$target_datetime),
    if (is.null(r$value)) "-" else format(r$value, digits = 8), if (is.null(r$expected)) "-" else format(r$expected, digits = 8),
    r$id))
  head <- c("CHECK", "AREA", "ORIGEM", "INSTANTE", "VALOR", "ESPERADO", "ID")
  m <- if (length(rows)) rbind(head, do.call(rbind, rows)) else matrix(head, nrow = 1L)
  w <- apply(m, 2L, function(c) max(nchar(c)))
  tab <- apply(m, 1L, function(r) sub("\\s+$", "", paste(mapply(function(x, k) formatC(x, width = -k), r, w), collapse = " ")))
  c(tab, if (isTRUE(res$ok)) "OK" else sprintf("FALHA %s (%d findings)", res$class, length(res$findings)))
}

## --- Handler fino ---------------------------------------------------------------------------------------------
cmd_check_coherence <- function(ctx, args) {
  ## o verbo nao tem escopo de area: --area esta na gramatica global (score/validate-deck) e aqui e recusado
  if (!is.null(.prevcarga_cc_opt(args, "--area"))) prevcarga_abort("config.usage", where = "--area")
  path <- .prevcarga_cc_opt(args, "--long")
  ctx$tz <- .prevcarga_eval_tz(ctx)
  long <- prevcarga_read_long(path, ctx)
  r <- prevcarga_check_coherence(long, .prevcarga_score_topo(ctx), ctx = ctx)
  res <- .prevcarga_cc_result(r)
  if (!isTRUE(r$ok)) {
    if (!identical(ctx$format, "json"))
      writeLines(iconv(.render_check_coherence_text(res), from = "UTF-8", to = "ASCII", sub = "?"), con = stdout())
    prevcarga_abort(r$id, where = path, details = list(n_findings = nrow(r$findings)), result = res)
  }
  if (identical(ctx$format, "json")) res else .render_check_coherence_text(res)
}

prevcarga_verb_check_coherence <- list(
  name    = "check-coherence",
  usage   = "prevcarga check-coherence --long=<f>",
  options = character(0),
  handler = function(ctx, args) cmd_check_coherence(ctx, args),
  owner   = "M3",
  exempt  = FALSE,
  modules = c("topology.R", "schema_long.R", "provenance.R", "deck_reader.R", "evaluation.R")
)
