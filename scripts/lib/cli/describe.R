## scripts/lib/cli/describe.R - so o registro; cmd_describe/prevcarga_build_describe vivem em introspect.R (carregado via modules)
prevcarga_verb_describe <- list(
  name    = "describe",
  usage   = "prevcarga describe [--origin=<data>] [--model=<m>]",
  options = character(),
  handler = function(ctx, args) cmd_describe(ctx, args),
  owner   = "M1",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "deck_reader.R", "dates.R", "introspect.R")
)
