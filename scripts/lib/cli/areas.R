## scripts/lib/cli/areas.R - so o registro; cmd_areas/prevcarga_build_areas vivem em introspect.R (carregado via modules)
prevcarga_verb_areas <- list(
  name    = "areas",
  usage   = "prevcarga areas",
  options = character(),
  handler = function(ctx, args) cmd_areas(ctx, args),
  owner   = "M1",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "introspect.R")
)
