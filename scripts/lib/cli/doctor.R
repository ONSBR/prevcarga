## scripts/lib/cli/doctor.R - so o registro; cmd_doctor/prevcarga_build_doctor vivem em introspect.R (carregado via modules)
prevcarga_verb_doctor <- list(
  name    = "doctor",
  usage   = "prevcarga doctor",
  options = character(),
  handler = function(ctx, args) cmd_doctor(ctx, args),
  owner   = "M1",
  exempt  = TRUE,
  modules = "introspect.R"
)
