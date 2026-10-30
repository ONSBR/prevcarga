## run_all_gates.R -- runner dos gates do plugin random-forest: casca fina sobre prevcarga_gates_run (P.0h).
## Uso: Rscript --vanilla models/random-forest/tests/run_all_gates.R [--tier fast|parity|parity-full] [--units <grupo>] [--format text|json]
## O runner chama SO prevcarga_gates_run, que fixa o runtime in-process antes de tudo, roda models/random-forest/tests/testthat
## e anexa os sub-gates descobertos em scripts/tools/gates/*.R (packs por descoberta: M5/M6/M7 nunca editam este arquivo).
## Mapa gate do oraculo -> teste (dono): gate01 corte_treino/corte_dia_o (test-train-cut, test-day-o-load); gate02 estagios
## (test-stages); gate05 freeze (test-freeze); gate06/gate07 no_env/determinismo (test-no-env, test-determinism); gate10
## weather_vintage/carry_forward (test-weather-locf); gate11 corte_treino (test-train-cut); paridade (test-parity, RF.4).
.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(dirname(dirname(dirname(normalizePath(.f, winslash = "/"))))), "scripts", "tools", "plugin_gates.R"))
gate_table <- data.frame(
  gate = c("contrato", "completude", "ingest_frame", "ingest_cut_check", "holidays", "weather_vintage", "carry_forward",
           "features_contract", "corte_treino", "determinismo", "freeze", "no_env",
           "corte_dia_o", "corte_horario", "sentinela", "spline_rule", "estagios", "completude_infer",
           "paridade", "paridade_control", "units_yaml", "gate04", "gate08", "gate12", "fachada", "cross_os"),
  owner = c(rep("PLUGINS", 21L), "DEFERRED M6", "DEFERRED M6", "DEFERRED M7", "DEFERRED M5", "DEFERRED M8"),
  tier = c(rep("fast", 18L), "parity", "parity", rep("fast", 6L)),
  stringsAsFactors = FALSE)
prevcarga_gates_run(gate_table, tier = "fast", model = "random-forest")
