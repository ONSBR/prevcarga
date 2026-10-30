## scripts/lib/cli/doctor.R - registro do verbo `doctor`; cmd_doctor/prevcarga_build_doctor vivem em introspect.R (carregado via modules).
## M8.7: o handler acrescenta ao resultado do construtor os itens de plataforma/bundle/artefatos e os campos JSON (runtime_id, version,
## platform, install_root, state_dir, artefatos, state_bytes) por prevcarga_doctor_extend (runtime.R). O construtor nao muda.
prevcarga_verb_doctor <- list(
  name    = "doctor",
  usage   = "prevcarga doctor",
  options = character(),
  handler = function(ctx, args) prevcarga_doctor_extend(cmd_doctor(ctx, args), ctx),
  owner   = "M1",
  exempt  = TRUE,
  modules = "introspect.R"
)
