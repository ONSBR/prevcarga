#!/usr/bin/env Rscript
## run_all_gates.R -- runner dos gates do plugin lgbm: casca fina sobre prevcarga_gates_run (P.0h). Uso:
##   Rscript --vanilla models/lgbm/tests/run_all_gates.R [--tier fast|parity|parity-full] [--units <grupo>] [--format text|json]
## O runner chama SO prevcarga_gates_run, que fixa o runtime in-process antes de tudo, roda models/lgbm/tests/testthat e anexa os
## sub-gates descobertos em scripts/tools/gates/*.R (sort radix; applies_to inclui lgbm): M5/M6/M7 nunca editam este arquivo.
## Cada teste de testthat/ carrega o rotulo `[PLUGINS] <gate>:` ou `[DEFERRED Mx] <gate>:` com uma linha desta tabela (senao
## tools.gate_unowned). `oracle` = gate do oraculo (prevcargadessem3/models/lgbm/tests; documentacao: a fundacao le gate, owner e tier).
## DEFERRED vira SKIP com dono ate o pack existir. Tiers: parity = grupo smoke do units.yaml; parity-full = smoke + foundation + fanout.
.lgbm_runner <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]), winslash = "/")
source(file.path(dirname(dirname(dirname(dirname(.lgbm_runner)))), "scripts", "tools", "plugin_gates.R"))

.lgbm_gates <- list(
  list("contrato", "-", "PLUGINS", "fast"), list("ingest_cortes", "-", "PLUGINS", "fast"),
  list("ingest_safra", "-", "PLUGINS", "fast"), list("ingest_contrato", "-", "PLUGINS", "fast"),
  list("ingest_feriados", "-", "PLUGINS", "fast"), list("ingest_efeitos", "-", "PLUGINS", "fast"),
  list("gate_cal", "gate_cal", "PLUGINS", "fast"), list("gate_saz", "gate_saz", "PLUGINS", "fast"),
  list("gate12_ondaletas_d1", "gate12", "PLUGINS", "fast"), list("fe_loess", "-", "PLUGINS", "fast"),
  list("gate_cut", "gate_cut", "PLUGINS", "fast"), list("fe_schema", "-", "PLUGINS", "fast"),
  list("fe_determinismo", "-", "PLUGINS", "fast"),
  list("gate01_perturbacao", "gate01", "PLUGINS", "fast"), list("gate02_escala_mw", "gate02", "PLUGINS", "fast"),
  list("gate03_norm_train_only", "gate03", "PLUGINS", "fast"), list("gate_cv_refit", "gate_cv_refit", "PLUGINS", "fast"),
  list("train_cortes", "-", "PLUGINS", "fast"),
  list("gate04_decoupling", "gate04", "PLUGINS", "fast"), list("thaw_integridade", "-", "PLUGINS", "fast"),
  list("higiene_artefato", "-", "PLUGINS", "fast"),
  list("loader_estagios", "-", "PLUGINS", "fast"), list("gate07_stage_a_48", "gate07", "PLUGINS", "fast"),
  list("gate05_sem_crash", "gate05", "PLUGINS", "fast"), list("parity_construcao_d1", "parity_construcao_d1", "PLUGINS", "fast"),
  list("infer_contrato", "-", "PLUGINS", "fast"), list("gate_inf_adequacao", "gate_inf_adequacao", "PLUGINS", "fast"),
  list("corte_intradiario", "-", "PLUGINS", "fast"), list("gate09_fuso", "gate09", "PLUGINS", "fast"),
  list("gate_nowcast_dplus0", "gate_nowcast_dplus0", "PLUGINS", "fast"),
  list("clima_honesto", "gate_cut", "PLUGINS", "fast"), list("clima_limite", "-", "PLUGINS", "fast"),
  list("clima_safra", "-", "PLUGINS", "fast"), list("gate10_determinismo", "gate10", "PLUGINS", "fast"),
  list("aditividade_epico", "-", "PLUGINS", "fast"), list("higiene", "-", "PLUGINS", "fast"),
  list("readme", "-", "PLUGINS", "fast"), list("runner_controle", "-", "PLUGINS", "fast"),
  list("units_yaml", "-", "PLUGINS", "fast"),
  list("paridade", "-", "PLUGINS", "parity"), list("paridade_control", "-", "PLUGINS", "parity"),
  list("gate_adequacao_add", "gate_adequacao_add", "PLUGINS", "parity"),
  list("gate11_ytrue_real", "gate11", "DEFERRED M6", "fast"), list("nowcast_dplus0_long", "gate_nowcast_dplus0", "DEFERRED M6", "fast"),
  list("fachada", "-", "DEFERRED M5", "fast"), list("cross_os", "-", "DEFERRED M8", "fast"))
gate_table <- data.frame(
  gate = vapply(.lgbm_gates, `[[`, "", 1L), oracle = vapply(.lgbm_gates, `[[`, "", 2L),
  owner = vapply(.lgbm_gates, `[[`, "", 3L), tier = vapply(.lgbm_gates, `[[`, "", 4L), stringsAsFactors = FALSE)

prevcarga_gates_run(gate_table, tier = NULL, model = "lgbm")
