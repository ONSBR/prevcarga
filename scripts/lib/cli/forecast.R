## scripts/lib/cli/forecast.R - verbo auto-registrado `forecast` (M6.9; dono M6; descoberto por scripts/lib/cli.R, barrel por descoberta).
##
## Handler fino sobre prevcarga_forecast (forecast.R): so valida enums, expande --subsistema em folhas pela topologia e entrega
## result/warnings ao envelope do ADR-0040. --origin e o unico flag de data humano (global do M1); nao ha flag de corte: o docopt
## gerado o recusa (config.usage, exit 8). --area/--subsistema/--skip-validate/--report/--report-format ja constam do bloco Options
## (validate-deck, train-models, score, backtest): a duplicata torna o docopt ambiguo, entao so a usage os cita.
## O relatorio roda DEPOIS do commit da entrega (M6.5); se falhar, a entrega fica e o verbo sai exit 14 (partial.report_failed).
## Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result`. Sem env vars.
## Requer errors.R, config.R (da fachada) e topology.R ... reporting.R (lista em `modules`).

.prevcarga_fc_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || all(is.na(v))) NULL else as.character(v)
}

## carimbo do run_id da fachada (relogio lido UMA vez em config.R); forecast.R/exports.R nao leem relogio
.prevcarga_fc_generated_at <- function(run_id) {
  t <- lubridate::ymd_hms(sub("-.*$", "", as.character(run_id)), tz = "UTC", quiet = TRUE)
  if (is.na(t)) NA_character_ else format(t, "%Y-%m-%dT%H:%M:%SZ")
}

.prevcarga_fc_topology <- function(ctx) {
  if (!is.null(ctx$topology_path)) prevcarga_topology(path = ctx$topology_path, root = ctx$root)
  else prevcarga_topology(root = ctx$root)
}

## --subsistema <ss>|all -> folhas (NULL = todas as folhas); subsistema fora da topologia => topology.area_undeclared (exit 4)
.prevcarga_fc_resolve_areas <- function(area, subsistema, topo) {
  if (!is.null(subsistema)) {
    ss <- subsistema[[1L]]
    if (identical(ss, "all")) return(NULL)
    if (!ss %in% names(topo$subsystems)) prevcarga_abort("topology.area_undeclared", where = ss)
    return(as.character(topo$subsystems[[ss]]$areas))
  }
  if (length(area)) unique(as.character(area)) else NULL
}

.prevcarga_fc_text <- function(r) {
  paths <- vapply(r$arquivos, function(a) a$path, "")
  c(sprintf("forecast  origem=%s  available_until=%s  versao=%s", r$origem, r$available_until, r$versao),
    sprintf("niveis=%s  folhas=%d  isoladas=%d  ja_publicado=%s", r$niveis, length(r$covered), length(r$stray),
            if (isTRUE(r$ja_publicado)) "sim" else "nao"),
    paste0("saida: ", r$saida),
    vapply(r$arquivos, function(a) sprintf("arquivo %-32s linhas=%d sha256=%s", a$path, a$linhas, a$sha256), ""),
    if (!identical(r$report$status, "skipped")) paste0("relatorio: ", r$report$status, if (!is.null(r$report$path)) paste0(" ", r$report$path)),
    if (length(paths)) "OK" else "OK (nada entregue)")
}

cmd_forecast <- function(ctx, args) {
  niveis <- .prevcarga_fc_opt(args, "--niveis")
  niveis <- if (is.null(niveis)) "subsistemas" else niveis[[1L]]
  if (!niveis %in% c("sin", "subsistemas", "tudo"))
    prevcarga_abort("config.usage", where = "--niveis", details = list(hint = "sin, subsistemas ou tudo"))
  fmt <- .prevcarga_fc_opt(args, "--report-format")
  fmt <- if (is.null(fmt)) "html" else fmt[[1L]]
  if (!fmt %in% c("html", "pdf"))
    prevcarga_abort("config.usage", where = "--report-format", details = list(hint = "html ou pdf"))
  area <- .prevcarga_fc_opt(args, "--area")
  ss <- .prevcarga_fc_opt(args, "--subsistema")
  if (length(area) && !is.null(ss))
    prevcarga_abort("config.usage", where = "--area/--subsistema", details = list(hint = "--area e --subsistema sao exclusivos"))
  prevcarga_require_data_dir(ctx)
  areas <- .prevcarga_fc_resolve_areas(area, ss, .prevcarga_fc_topology(ctx))
  ctx$generated_at <- .prevcarga_fc_generated_at(ctx$run_id)
  out <- prevcarga_forecast(ctx, origin = ctx$origin, areas = areas, niveis = niveis,
                            report = isTRUE(args[["--report"]]), report_format = fmt,
                            skip_validate = isTRUE(args[["--skip-validate"]]),
                            incluir_membros = isTRUE(args[["--incluir-membros"]]))
  for (w in out$warnings) prevcarga_warn(w$id, where = "forecast")      # a fachada coleta os avisos do catalogo no envelope
  res <- out$result
  ## entrega ja publicada e intacta; relatorio falho => exit 14 classificado, com o result no envelope
  if (identical(res$report$status, "failed"))
    prevcarga_abort("partial.report_failed", where = as.character(res$report$error), result = res)
  if (identical(ctx$format, "json")) res else .prevcarga_fc_text(res)
}

prevcarga_verb_forecast <- list(
  name    = "forecast",
  usage   = paste("prevcarga forecast [--area=<a>... | --subsistema=<s>] [--niveis=<n>] [--incluir-membros] [--report]",
                  "[--report-format=<f>] [--skip-validate]"),
  options = c(
    "--niveis=<n>            Teto dos agregados: sin, subsistemas (padrao) ou tudo; folha solta sempre entregue",
    "--incluir-membros       Inclui membros/<m>/<a>.parquet na entrega"),
  scoped  = c("--niveis", "--incluir-membros", "--report", "--report-format"),   # cli.R .prevcarga_check_scoped
  handler = function(ctx, args) cmd_forecast(ctx, args),
  owner   = "M6",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "schema_long.R", "evaluation.R", "deck_reader.R", "dates.R", "validation.R",
              "features.R", "registry.R", "training.R", "inference.R", "combination.R", "build_long_output.R", "reconcile.R",
              "exports.R", "reporting.R", "forecast.R")
)
