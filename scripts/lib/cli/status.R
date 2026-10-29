## scripts/lib/cli/status.R - so o registro; cmd_status/prevcarga_build_status vivem em introspect.R (carregado via modules)
prevcarga_verb_status <- list(
  name    = "status",
  usage   = "prevcarga status",
  options = character(),
  handler = function(ctx, args) cmd_status(ctx, args),
  owner   = "M1",
  exempt  = TRUE,
  modules = c("deck_reader.R", "introspect.R")
)
