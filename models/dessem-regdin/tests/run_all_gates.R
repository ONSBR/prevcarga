#!/usr/bin/env Rscript
## run_all_gates.R -- runner dos gates do plugin dessem-regdin: casca fina sobre prevcarga_gates_run (P.0h). Uso:
##   Rscript --vanilla models/dessem-regdin/tests/run_all_gates.R [--tier fast|parity|parity-full] [--units smoke|foundation|fanout] [--format text|json]
## O runner chama SO prevcarga_gates_run, que fixa o runtime in-process antes de tudo, roda models/dessem-regdin/tests/testthat e anexa
## os sub-gates descobertos em scripts/tools/gates/*.R (sort radix; applies_to inclui dessem-regdin): M5/M6/M7 nunca editam este arquivo.
## Cada teste de testthat/ carrega o rotulo `[PLUGINS] <gate>:` ou `[DEFERRED Mx] <gate>:` com uma linha desta tabela (senao
## tools.gate_unowned). `oracle` = gate do oraculo (prevcargadessem3/models/dessem-regdin/tests/run_all_gates.R; so documentacao: a
## fundacao le gate, owner e tier). DEFERRED vira SKIP com dono ate o pack existir. Tiers: parity = grupo smoke do units.yaml;
## parity-full = smoke + foundation + fanout (`--units <grupo>` roda um grupo so). A tabela e DADO (nunca switch por gate).
.rd_runner <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]), winslash = "/")
.rd_repo <- dirname(dirname(dirname(dirname(.rd_runner))))
source(file.path(.rd_repo, "scripts", "tools", "plugin_gates.R"))

## gate, oracle (gate do oraculo), owner, tier. gate01/gate06 (LONG/y_true) = M6; gate03 (varias origens) = M7; gate12/gate13 = split_nowcast_*.
.rd_gates <- list(
  list("descritor", "-", "PLUGINS", "fast"), list("ingestao", "-", "PLUGINS", "fast"),
  list("nowcast_mm", "-", "PLUGINS", "fast"), list("treino_freeze", "-", "PLUGINS", "fast"),
  list("inferencia", "-", "PLUGINS", "fast"),
  list("contrato", "-", "PLUGINS", "fast"), list("params_schema", "-", "PLUGINS", "fast"),
  list("exogena_variante", "gate04", "PLUGINS", "fast"), list("janela_infer_igual_treino", "-", "PLUGINS", "fast"),
  list("arima_drift", "-", "PLUGINS", "fast"), list("treino_ate_o_menos_1", "-", "PLUGINS", "fast"),
  list("mm_head_rng", "-", "PLUGINS", "fast"), list("mm_head_corte", "-", "PLUGINS", "fast"),
  list("nowcast_so_no_infer", "-", "PLUGINS", "fast"), list("corte_horario", "-", "PLUGINS", "fast"),
  list("corte_clima", "gate05", "PLUGINS", "fast"), list("corte_carga", "-", "PLUGINS", "fast"),
  list("split_vs_acoplado", "-", "PLUGINS", "fast"), list("sentinela", "-", "PLUGINS", "fast"),
  list("artefato_puro", "-", "PLUGINS", "fast"), list("higiene", "-", "PLUGINS", "fast"), list("grep", "-", "PLUGINS", "fast"),
  list("escala_mw", "gate02", "PLUGINS", "fast"), list("contract_synthetic", "gate07", "PLUGINS", "fast"),
  list("contract_e2e", "gate08", "PLUGINS", "fast"), list("flag12_yearend", "gate09", "PLUGINS", "fast"),
  list("nowcast_parcial", "gate10,gate11", "PLUGINS", "fast"), list("split_nowcast_med", "gate12", "PLUGINS", "fast"),
  list("split_nowcast_max", "gate13", "PLUGINS", "fast"),
  list("units_yaml", "-", "PLUGINS", "fast"), list("runner_controle", "-", "PLUGINS", "fast"),
  list("requires_report", "-", "PLUGINS", "parity"),
  list("mm_head_oracle", "-", "PLUGINS", "parity"), list("paridade_smoke", "-", "PLUGINS", "parity"),
  list("paridade_control", "-", "PLUGINS", "parity"),
  list("paridade_foundation", "-", "PLUGINS", "parity-full"), list("paridade_fanout", "-", "PLUGINS", "parity-full"),
  list("schema_long", "gate01", "DEFERRED M6", "fast"), list("ytrue_real", "gate06", "DEFERRED M6", "fast"),
  list("sem_crash_harness", "gate03", "DEFERRED M7", "fast"), list("fachada", "-", "DEFERRED M5", "fast"),
  list("cross_os", "-", "DEFERRED M8", "fast"))
gate_table <- data.frame(
  gate = vapply(.rd_gates, `[[`, "", 1L), oracle = vapply(.rd_gates, `[[`, "", 2L),
  owner = vapply(.rd_gates, `[[`, "", 3L), tier = vapply(.rd_gates, `[[`, "", 4L), stringsAsFactors = FALSE)

prevcarga_gates_run(gate_table, tier = NULL, model = "dessem-regdin")
