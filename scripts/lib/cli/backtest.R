## scripts/lib/cli/backtest.R - verbo auto-registrado `backtest` (M7.1; descoberto por scripts/lib/cli.R, M1.4).
##
## Roda a matriz do plano origem a origem (uma treina+infere por origem; nada fitado numa origem e reusado noutra),
## serial (--jobs 1; PSOCK e do M7.6), sob run.lock/, com ledger local (--resume), fail-fast ou --keep-going.
## Entrega parcial => exit 14 partial.backtest_delivery; lock ocupado => exit 13 state.lock_held (--break-lock retoma).
## `--dry-run` e identico a `plan` (mesmo handler). Flags --origin/--model/--area/--baseline/--dry-run/--jobs: ver plan.R.
## Requer errors.R, config.R, topology.R, provenance.R, deck_reader.R, dates.R, registry.R, training.R, inference.R, build_long_output.R, cache.R, orchestrate.R, backtest.R (M7.3: MAP real).

cmd_backtest <- function(ctx, args) {
  prevcarga_require_data_dir(ctx)
  if (isTRUE(args[["--dry-run"]])) {
    res <- prevcarga_plan_result(prevcarga_plan_request(ctx, args))
    return(if (identical(ctx$format, "json")) res else .m7_plan_text(res))
  }
  rf <- .prevcarga_tr_arg(args, "--report-format")
  if (!is.null(rf) && !rf %in% c("html", "pdf")) prevcarga_abort("config.usage", where = "--report-format")
  res <- prevcarga_backtest_run(ctx, args)
  if (identical(ctx$format, "json")) res else
    sprintf("backtest run_id=%s ok=%d/%d", res$run_id, res$summary$ok, res$summary$total)
}

prevcarga_verb_backtest <- list(
  name    = "backtest",
  usage   = paste("prevcarga backtest --origin=<data> [--model=<m>] [--area=<a>] [--corte=<HH:MM,...>]",
                  "[--protocol=<p>] [--dplus] [--baseline=<b>] [--report] [--report-format=<f>] [--resume]",
                  "[--keep-going] [--break-lock] [--jobs=<n>] [--dry-run]"),
  options = c(
    "--corte=<c>             Cortes intradiarios HH:00 ou HH:30, separados por virgula (padrao backtest.corte_default)",
    "--protocol=<p>          fixed, daily ou retrain_harness (padrao backtest.protocol_default do run.yaml)",
    "--dplus                 Inclui os horizontes D+ no relatorio do backtest",
    "--report                Gera o relatorio do backtest",
    "--report-format=<f>     Formato do relatorio: html ou pdf",
    "--resume                Pula as celulas ja concluidas (ledger por presenca)",
    "--keep-going            Continua apos a 1a falha (padrao: fail-fast)",
    "--break-lock            Retoma um run.lock/ deixado por outra execucao"),
  scoped  = c("--corte", "--protocol", "--dplus", "--report", "--report-format", "--resume", "--keep-going", "--break-lock"),
  handler = function(ctx, args) cmd_backtest(ctx, args),
  owner   = "M7",
  exempt  = FALSE,
  modules = c("topology.R", "schema_long.R", "provenance.R", "deck_reader.R", "dates.R", "validation.R",
              "features.R", "registry.R", "training.R", "inference.R", "build_long_output.R", "cache.R", "orchestrate.R",
              "backtest.R")
)
