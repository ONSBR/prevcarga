## scripts/lib/cli/export.R - verbo auto-registrado `export` (M6.2; descoberto por scripts/lib/cli.R, barrel por descoberta).
##
## Passthrough fino para prevcarga_export (exports.R): so valida flag e compoe o result. O export so copia bytes do scratch.
## A chave imutavel (M6.7) e RECONSTRUIDA do run.json do `--scratch` (origem, available_until, versao, timezone) por
## prevcarga_export_key, nunca de flag: o mesmo scratch entrega na mesma chave que o forecast usou. `gerado_em` sai do carimbo
## do run_id da fachada (relogio lido UMA vez em config.R); o modulo exports.R nao le relogio. Sem env vars; erros e exits
## vem do catalogo (ADR-0040). Flags `scoped` (so valem aqui e no forecast; mecanismo do M7.1). Sem flags de data alem de --origin.
## Requer errors.R, config.R (da fachada) e schema_long.R, exports.R.

.prevcarga_ex_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

.prevcarga_ex_generated_at <- function(run_id) {
  stamp <- sub("-.*$", "", as.character(run_id))
  t <- lubridate::ymd_hms(stamp, tz = "UTC", quiet = TRUE)
  if (is.na(t)) NA_character_ else format(t, "%Y-%m-%dT%H:%M:%SZ")
}

.render_export_text <- function(r) {
  c(sprintf("export  niveis=%s  arquivos=%d", r$niveis, length(r$arquivos)),
    paste0("saida: ", r$saida),
    vapply(r$arquivos, function(a) sprintf("arquivo %-32s linhas=%d sha256=%s", a$path, a$linhas, a$sha256), ""),
    "OK")
}

cmd_export <- function(ctx, args) {
  from <- .prevcarga_ex_opt(args, "--scratch")
  if (is.null(from)) prevcarga_abort("config.usage", where = "--scratch")
  niveis <- .prevcarga_ex_opt(args, "--niveis")
  if (is.null(niveis)) niveis <- "subsistemas"
  if (!niveis %in% prevcarga_export_niveis)
    prevcarga_abort("config.usage", where = paste0("--niveis=", niveis), details = list(hint = "sin, subsistemas ou tudo"))
  scratch <- normalizePath(from, winslash = "/", mustWork = FALSE)
  if (!dir.exists(scratch))
    prevcarga_abort("state.nothing_to_deliver", where = "--scratch", details = list(reason = "scratch ausente", scratch = from))
  run <- .export_read_run(scratch)
  tz <- as.character(run$timezone)
  au <- lubridate::ymd_hm(as.character(run$available_until), tz = tz, quiet = TRUE)
  og <- lubridate::ymd(as.character(run$origem), quiet = TRUE)
  if (is.na(au) || is.na(og) || is.null(run$versao))
    prevcarga_abort("state.nothing_to_deliver", where = "--scratch",
                    details = list(reason = "run.json sem origem/available_until/versao/timezone", scratch = from))
  key <- prevcarga_export_key(og, au, as.character(run$versao), tz)
  ctx$generated_at <- .prevcarga_ex_generated_at(ctx$run_id)
  res <- prevcarga_export(scratch, key, niveis = niveis, incluir_membros = isTRUE(args[["--incluir-membros"]]), ctx = ctx)
  res$niveis <- niveis
  if (identical(ctx$format, "json")) res else .render_export_text(res)
}

prevcarga_verb_export <- list(
  name    = "export",
  usage   = "prevcarga export --scratch=<dir> [--niveis=<n>] [--incluir-membros]",
  options = c(
    "--scratch=<dir>         Scratch de uma rodada (run.json + parquets); a chave de entrega e reconstruida dele",
    "--niveis=<n>            Teto dos agregados: sin, subsistemas (padrao) ou tudo; folha solta sempre entregue",
    "--incluir-membros       Inclui membros/<m>/<a>.parquet na entrega"),
  scoped  = c("--scratch", "--niveis", "--incluir-membros"),   # so export e forecast (cli.R .prevcarga_check_scoped)
  handler = function(ctx, args) cmd_export(ctx, args),
  owner   = "M6",
  exempt  = FALSE,
  modules = c("schema_long.R", "exports.R")
)
