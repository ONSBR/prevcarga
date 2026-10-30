## scripts/lib/cli/plan.R - verbo auto-registrado `plan` (M7.1; descoberto por scripts/lib/cli.R, M1.4).
##
## Emite a matriz membro x folha x origem x corte (prevcarga_plan, orchestrate.R) sem treinar, inferir nem criar artefato.
## `backtest --dry-run` despacha para o mesmo handler. As flags --origin/--model/--area/--baseline/--dry-run/--jobs ja
## constam no bloco Options global ou de outros verbos (duplicata torna o docopt ambiguo); aqui entram so as do M7.
## Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result`.
## Requer errors.R, config.R, topology.R, provenance.R, deck_reader.R, dates.R, registry.R, training.R, cache.R, orchestrate.R.

cmd_plan <- function(ctx, args) {
  prevcarga_require_data_dir(ctx)
  res <- prevcarga_plan_result(prevcarga_plan_request(ctx, args))
  if (identical(ctx$format, "json")) res else .m7_plan_text(res)
}

prevcarga_verb_plan <- list(
  name    = "plan",
  usage   = paste("prevcarga plan --origin=<data> [--model=<m>] [--area=<a>] [--corte=<HH:MM,...>]",
                  "[--protocol=<p>] [--dplus]"),
  options = c(
    "--corte=<c>             Cortes intradiarios HH:00 ou HH:30, separados por virgula (padrao backtest.corte_default)",
    "--protocol=<p>          fixed, daily ou retrain_harness (padrao backtest.protocol_default do run.yaml)",
    "--dplus                 Inclui os horizontes D+ no relatorio do backtest"),
  scoped  = c("--corte", "--protocol", "--dplus"),
  handler = function(ctx, args) cmd_plan(ctx, args),
  owner   = "M7",
  exempt  = FALSE,
  modules = c("topology.R", "schema_long.R", "provenance.R", "deck_reader.R", "dates.R", "validation.R",
              "features.R", "registry.R", "training.R", "cache.R", "orchestrate.R")
)
