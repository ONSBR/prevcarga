## run_all_gates.R -- runner dos gates do plugin random-forest: casca fina sobre prevcarga_gates_run (P.0h).
## Uso: Rscript --vanilla models/random-forest/tests/run_all_gates.R [--tier fast|parity|parity-full] [--units <grupo>] [--format text|json]
## O runner chama SO prevcarga_gates_run, que fixa o runtime in-process antes de tudo, roda models/random-forest/tests/testthat
## e anexa os sub-gates descobertos em scripts/tools/gates/*.R (packs por descoberta: M5/M6/M7 nunca editam este arquivo).
## Mapa gate do oraculo -> teste (dono): gate01 corte_treino/corte_dia_o (test-train-cut, test-day-o-load); gate02 estagios
## (test-stages); gate05 freeze (test-freeze); gate06/gate07 no_env/determinismo (test-no-env, test-determinism); gate10
## weather_vintage/carry_forward (test-weather-locf); gate11 corte_treino (test-train-cut); paridade (test-parity, RF.4);
## gate09/gate10 weather/weather_limit (test-weather, RF.5); aditividade/higiene/readme (test-additivity, RF.5).
## Cada teste de testthat/ carrega o rotulo `[PLUGINS] <gate>:` ou `[DEFERRED Mx] <gate>:` com uma linha desta tabela (senao tools.gate_unowned).
## `oracle` = gate do oraculo (documentacao; a fundacao le gate, owner e tier). DEFERRED vira SKIP com dono ate o pack existir.
.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(dirname(dirname(dirname(normalizePath(.f, winslash = "/"))))), "scripts", "tools", "plugin_gates.R"))
.rf_gates <- list(
  list("contrato", "gate02", "PLUGINS", "fast"), list("completude", "-", "PLUGINS", "fast"),
  list("ingest_frame", "-", "PLUGINS", "fast"), list("ingest_cut_check", "-", "PLUGINS", "fast"),
  list("holidays", "-", "PLUGINS", "fast"), list("weather_vintage", "gate10", "PLUGINS", "fast"),
  list("carry_forward", "gate10", "PLUGINS", "fast"), list("features_contract", "gate02", "PLUGINS", "fast"),
  list("corte_treino", "gate01/gate11", "PLUGINS", "fast"), list("determinismo", "gate07", "PLUGINS", "fast"),
  list("freeze", "gate05", "PLUGINS", "fast"), list("no_env", "gate06", "PLUGINS", "fast"),
  list("corte_dia_o", "gate01", "PLUGINS", "fast"), list("corte_horario", "-", "PLUGINS", "fast"),
  list("sentinela", "-", "PLUGINS", "fast"), list("spline_rule", "-", "PLUGINS", "fast"),
  list("estagios", "gate02", "PLUGINS", "fast"), list("completude_infer", "-", "PLUGINS", "fast"),
  list("weather", "gate09/gate10", "PLUGINS", "fast"), list("weather_limit", "gate09", "PLUGINS", "fast"),
  list("aditividade", "-", "PLUGINS", "fast"), list("higiene", "-", "PLUGINS", "fast"),
  list("readme", "-", "PLUGINS", "fast"), list("runner_controle", "-", "PLUGINS", "fast"),
  list("paridade", "gate03", "PLUGINS", "parity"), list("paridade_control", "gate03", "PLUGINS", "parity"),
  list("units_yaml", "-", "PLUGINS", "fast"),
  list("gate04", "gate04", "DEFERRED M6", "fast"), list("gate08", "gate08", "DEFERRED M6", "fast"),
  list("gate12", "gate12", "DEFERRED M7", "fast"), list("fachada", "-", "DEFERRED M5", "fast"),
  list("cross_os", "-", "DEFERRED M8", "fast"))
gate_table <- data.frame(
  gate = vapply(.rf_gates, `[[`, "", 1L), oracle = vapply(.rf_gates, `[[`, "", 2L),
  owner = vapply(.rf_gates, `[[`, "", 3L), tier = vapply(.rf_gates, `[[`, "", 4L), stringsAsFactors = FALSE)
prevcarga_gates_run(gate_table, tier = NULL, model = "random-forest")
