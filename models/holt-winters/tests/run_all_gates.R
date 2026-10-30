#!/usr/bin/env Rscript
## Runner do plugin holt-winters: so compoe a biblioteca da fundacao (P.0h). Uso:
##   Rscript --vanilla models/holt-winters/tests/run_all_gates.R [--tier fast|parity|parity-full] [--units smoke|full] [--format text|json]
## `prevcarga_gates_run` fixa o runtime in-process primeiro (exit 7 se nao conseguir), roda tests/testthat, descobre os packs
## de scripts/tools/gates/*.R (sort radix; applies_to inclui holt-winters) e sai 0 sse todo gate [PLUGINS] do tier passou, senao 1.
## Os gates de M5/M6/M7 entram por packs descobertos: este arquivo nunca e editado por eles. `oracle` = gate do oraculo
## (documentacao; a fundacao le gate, owner e tier). Os gates do HW.1-HW.4 estao aqui porque o runner exige uma linha por
## teste rotulado; a tabela e a final do epico (HW.7): [PLUGINS] verdes, [DEFERRED M5|M6|M7|M8] como SKIP com dono.
.hw_runner <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]), winslash = "/")
source(file.path(dirname(dirname(dirname(dirname(.hw_runner)))), "scripts", "tools", "plugin_gates.R"))

## Tabela gate x dono final do epico (HW.7). `oracle` = gate do oraculo (documentacao). DEFERRED = SKIP com dono ate o pack existir.
.hw_gates <- list(
  list("registro", "-", "PLUGINS", "fast"), list("catalogo_hw", "-", "PLUGINS", "fast"),
  list("ingest_shape", "-", "PLUGINS", "fast"), list("ingest_contract", "-", "PLUGINS", "fast"),
  list("ingest_weather", "-", "PLUGINS", "fast"), list("ingest_puro", "-", "PLUGINS", "fast"),
  list("hv_lubridate", "-", "PLUGINS", "fast"), list("fe_ordem", "-", "PLUGINS", "fast"),
  list("fe_sementes", "-", "PLUGINS", "fast"), list("fe_params", "-", "PLUGINS", "fast"),
  list("fe_datas", "-", "PLUGINS", "fast"), list("fe_pureza", "-", "PLUGINS", "fast"),
  list("fe_perfil_st", "-", "PLUGINS", "fast"), list("helpers_oraculo", "-", "PLUGINS", "parity"),
  list("train_shape", "-", "PLUGINS", "fast"), list("ts_start", "-", "PLUGINS", "fast"),
  list("train_contract", "-", "PLUGINS", "fast"), list("train_cut", "-", "PLUGINS", "fast"),
  list("freeze_puro", "-", "PLUGINS", "fast"), list("freeze_det", "-", "PLUGINS", "fast"),
  list("thaw_contract", "-", "PLUGINS", "fast"), list("infer_envelope", "-", "PLUGINS", "fast"),
  list("infer_sem_refit", "-", "PLUGINS", "fast"), list("efeito_temp", "-", "PLUGINS", "fast"),
  list("semifer_loess", "-", "PLUGINS", "fast"), list("infer_clima", "-", "PLUGINS", "fast"),
  list("infer_forecast", "-", "PLUGINS", "fast"), list("infer_contract", "-", "PLUGINS", "fast"),
  list("infer_puro", "-", "PLUGINS", "fast"),
  list("paridade", "-", "PLUGINS", "parity"), list("paridade_controle", "-", "PLUGINS", "parity"),
  list("sem_crash_1o_mes", "gate03", "PLUGINS", "parity"), list("sem_deslocamento", "gate11", "PLUGINS", "parity"),
  list("corte_treino", "gate01", "PLUGINS", "fast"), list("contrato_entrada", "gate02", "PLUGINS", "fast"),
  list("corte_intradiario", "-", "PLUGINS", "fast"), list("mesmo_dia", "-", "PLUGINS", "fast"),
  list("sentinela", "-", "PLUGINS", "fast"), list("clima_honesto", "gate08", "PLUGINS", "fast"),
  list("clima_fallback", "gate09", "PLUGINS", "fast"), list("infer_igual_forecast", "-", "PLUGINS", "fast"),
  list("higiene_grep", "gate05", "PLUGINS", "fast"), list("higiene_env", "-", "PLUGINS", "fast"),
  list("determinismo", "gate06", "PLUGINS", "fast"), list("seed_medicao", "-", "PLUGINS", "fast"),
  list("sem_seed_literal", "-", "PLUGINS", "fast"), list("artefato_puro", "-", "PLUGINS", "fast"),
  list("aditividade", "-", "PLUGINS", "fast"), list("runner_controle", "-", "PLUGINS", "fast"),
  list("schema_long", "gate04", "DEFERRED M6", "fast"), list("ytrue_real", "gate07", "DEFERRED M6", "fast"),
  list("harness_parity", "gate10", "DEFERRED M7", "fast"), list("fachada", "-", "DEFERRED M5", "fast"),
  list("portabilidade_cross_os", "-", "DEFERRED M8", "fast"))
gate_table <- data.frame(
  gate = vapply(.hw_gates, `[[`, "", 1L), oracle = vapply(.hw_gates, `[[`, "", 2L),
  owner = vapply(.hw_gates, `[[`, "", 3L), tier = vapply(.hw_gates, `[[`, "", 4L), stringsAsFactors = FALSE)

prevcarga_gates_run(gate_table, tier = NULL, model = "holt-winters")
