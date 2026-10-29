## scripts/lib/cli/validate-deck.R - verbo auto-registrado (M2.5; descoberto por scripts/lib/cli.R, M1.4).
##
## Gate-0 exposto na CLI: dispatch FINO sobre prevcarga_validate_deck (validation.R, M2.4). O construtor
## prevcarga_build_validate_deck monta o `result` do envelope (ADR-0040); o veredito reprovado vira
## prevcarga_abort() com o id do primeiro erro da classe dominante, e quem fixa o exit (2/3/4/5) e o envelope
## e o handler de topo da fachada. Sem quit()/stop() cru, sem env vars, saida de terminal ASCII.
##
## Convencao da fachada: handler(ctx, args). No texto o handler devolve as linhas ja renderizadas (a fachada
## so imprime); no JSON devolve o `result` (a fachada o embrulha no envelope). Os `data_lag_days` reais chegam
## com os descriptors (P.0b/M5); ate la `data_lag_source = default`.
## Requer errors.R, config.R, topology.R, provenance.R, deck_reader.R, dates.R e validation.R (campo `modules`).

.prevcarga_vd_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

.prevcarga_vd_ts <- function(x, tz) {
  if (is.null(x) || !length(x) || is.na(x)) return(NULL)
  format(lubridate::with_tz(x, tz), "%Y-%m-%d %H:%M")
}

.prevcarga_vd_iso <- function(x) if (is.null(x) || !length(x) || is.na(x)) NULL else format(x, "%Y-%m-%d")

.prevcarga_vd_rows <- function(dt, fmt = function(r) as.list(r)) {
  if (is.null(dt) || !nrow(dt)) return(list())
  lapply(seq_len(nrow(dt)), function(i) fmt(dt[i, ]))
}

prevcarga_build_validate_deck <- function(ctx, opts, data_lag_days = NULL) {
  scope <- list(model = .prevcarga_vd_opt(opts, "--model"), area = .prevcarga_vd_opt(opts, "--area"))
  v <- prevcarga_validate_deck(ctx, ctx$origin, scope, data_lag_days)
  tz <- if (!is.null(v$timezone)) v$timezone$load else NULL

  man <- tryCatch(prevcarga_deck_manifest(ctx$data_dir), prevcarga_error = function(e) NULL)
  hash <- if (!is.null(man) && !is.null(v$load_label))
    tryCatch(prevcarga_data_hash(ctx$data_dir, man, v$load_label), prevcarga_error = function(e) NULL) else NULL

  by_area <- if (!is.null(v$available_until_by_area) && !is.null(tz))
    stats::setNames(lapply(v$available_until_by_area$until, .prevcarga_vd_ts, tz = tz),
                    v$available_until_by_area$area_code) else NULL
  cuts <- .prevcarga_vd_rows(v$cuts, function(r) list(
    model = r$model, split = r$split, granularity_min = as.integer(r$granularity_min),
    horizon_days = as.integer(r$horizon_days), train_origin = .prevcarga_vd_iso(r$train_origin),
    last_data_day = .prevcarga_vd_iso(r$last_data_day), data_lag_days = as.integer(r$data_lag_days),
    data_lag_source = r$data_lag_source, input_cut = .prevcarga_vd_ts(r$input_cut, tz),
    first_delivery = .prevcarga_vd_ts(r$first_delivery, tz)))
  fnd <- .prevcarga_vd_rows(v$findings, function(r) lapply(as.list(r), function(x) if (is.na(x)) "" else x))

  list(schema_version = 1L, ok = isTRUE(v$ok), class = v$class,
       forecast_origin = .prevcarga_vd_iso(v$forecast_origin), origin_source = v$origin_source,
       available_until = .prevcarga_vd_ts(v$available_until, tz), available_until_by_area = by_area,
       load_label = v$load_label, load_label_source = v$load_label_source,
       weather_issue_cutoff = v$weather_issue_cutoff,
       weather_issue_limit = if (!is.null(tz)) .prevcarga_vd_ts(v$weather_issue_limit, tz) else NULL,
       timezone = v$timezone, data_hash = hash, scope = v$scope, cuts = cuts, findings = fnd)
}

## --- Render texto ASCII ---------------------------------------------------------------------------------
.prevcarga_vd_dash <- function(x) if (is.null(x) || !length(x) || is.na(x)) "-" else as.character(x)

.prevcarga_vd_table <- function(head, rows) {
  m <- if (length(rows)) rbind(head, do.call(rbind, rows)) else matrix(head, nrow = 1L)
  w <- apply(m, 2L, function(c) max(nchar(c)))
  apply(m, 1L, function(r) sub("\\s+$", "", paste(mapply(function(x, k) formatC(x, width = -k), r, w), collapse = " ")))
}

.render_validate_deck_text <- function(res) {
  au <- res$available_until
  hhmm <- if (is.null(au)) "-" else sub("^.* ", "", au)
  hash <- if (is.null(res$data_hash)) "-" else substr(res$data_hash, 1L, 12L)
  ll <- if (is.null(res$load_label)) "-" else sprintf("%s (%s)", res$load_label, .prevcarga_vd_dash(res$load_label_source))
  head <- sprintf("validate-deck origin=%s (%s) available_until=%s load_label=%s weather_issue_cutoff=%s data_hash=%s",
                  .prevcarga_vd_dash(res$forecast_origin), .prevcarga_vd_dash(res$origin_source), hhmm, ll,
                  .prevcarga_vd_dash(res$weather_issue_cutoff), hash)
  cuts <- if (length(res$cuts)) .prevcarga_vd_table(
    c("MODEL", "SPLIT", "GRAN", "TRAIN_ORIGIN", "LAST_DATA_DAY", "INPUT_CUT", "FIRST_DELIVERY"),
    lapply(res$cuts, function(r) c(r$model, r$split, as.character(r$granularity_min),
      .prevcarga_vd_dash(r$train_origin), .prevcarga_vd_dash(r$last_data_day),
      .prevcarga_vd_dash(r$input_cut), .prevcarga_vd_dash(r$first_delivery))))
  f <- res$findings
  if (length(f)) {
    lv <- vapply(f, function(r) r$level, "")
    f <- f[order(lv != "error", seq_along(f))]          # erros primeiro, ordem de insercao preservada
  }
  fnd <- if (length(f)) .prevcarga_vd_table(
    c("AREA", "DATASET", "WHAT", "CLASS", "LEVEL", "DETAIL"),
    lapply(f, function(r) c(r$area, r$dataset, r$what, r$class, r$level, .prevcarga_vd_dash(r$detail))))
  c(head, cuts, fnd, if (isTRUE(res$ok)) "OK" else paste("FALHA", res$class))
}

## --- Handler fino ----------------------------------------------------------------------------------------
cmd_validate_deck <- function(ctx, args = NULL) {
  res <- prevcarga_build_validate_deck(ctx, args)
  if (!isTRUE(res$ok)) {
    errs <- Filter(function(r) identical(r$level, "error") && identical(r$class, res$class), res$findings)
    first <- errs[[1L]]
    n_err <- sum(vapply(res$findings, function(r) identical(r$level, "error"), NA))
    if (!identical(ctx$format, "json")) writeLines(iconv(.render_validate_deck_text(res), from = "UTF-8", to = "ASCII", sub = "?"), con = stdout())
    prevcarga_abort(paste0(first$class, ".", first$what),
                    where = if (is.na(first$where) || !nzchar(first$where)) NULL else first$where,
                    details = list(findings = as.integer(n_err), items = res$findings, origin = res$forecast_origin))
  }
  for (w in unique(vapply(Filter(function(r) identical(r$level, "warning"), res$findings), function(r) r$what, "")))
    prevcarga_warn(paste0("warning.", w))
  if (identical(ctx$format, "json")) res else .render_validate_deck_text(res)
}

prevcarga_verb_validate_deck <- list(
  name    = "validate-deck",
  usage   = "prevcarga validate-deck [--area=<a>]",
  options = "--area=<a>              Restringe o escopo a uma area (subsistema expande para as folhas)",
  handler = function(ctx, args) cmd_validate_deck(ctx, args),
  owner   = "M2",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "deck_reader.R", "dates.R", "validation.R")
)
