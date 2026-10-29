## scripts/lib/cli/models.R - so o registro; cmd_models/prevcarga_build_models vivem em introspect.R (carregado via modules)
prevcarga_verb_models <- list(
  name    = "models",
  usage   = "prevcarga models",
  options = character(),
  handler = function(ctx, args) cmd_models(ctx, args),
  owner   = "M1",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "introspect.R")
)
