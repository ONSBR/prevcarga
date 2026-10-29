#!/usr/bin/env Rscript
## Runner do plugin holt-winters: so compoe a biblioteca da fundacao (P.0h). Uso:
##   Rscript --vanilla models/holt-winters/tests/run_all_gates.R [--tier fast|parity|parity-full] [--units smoke|full] [--format text|json]
## `prevcarga_gates_run` fixa o runtime in-process primeiro (exit 7 se nao conseguir), roda tests/testthat, descobre os packs
## de scripts/tools/gates/*.R (sort radix; applies_to inclui holt-winters) e sai 0 sse todo gate [PLUGINS] do tier passou, senao 1.
## Os gates de M5/M6/M7 entram por packs descobertos: este arquivo nunca e editado por eles. `oracle` = gate do oraculo
## (documentacao; a fundacao le gate, owner e tier). Os gates do HW.1-HW.4 estao aqui porque o runner exige uma linha por
## teste rotulado; os demais gates do epico entram pelo ticket de fechamento (HW.7).
.hw_runner <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]), winslash = "/")
source(file.path(dirname(dirname(dirname(dirname(.hw_runner)))), "scripts", "tools", "plugin_gates.R"))

gate_table <- data.frame(
  gate = c("registro", "catalogo_hw", "ingest_shape", "ingest_contract", "ingest_weather", "ingest_puro", "hv_lubridate",
           "fe_ordem", "fe_sementes", "fe_params", "fe_datas", "fe_pureza", "fe_perfil_st", "helpers_oraculo",
           "train_shape", "ts_start", "train_contract", "train_cut", "freeze_puro", "freeze_det", "thaw_contract",
           "infer_envelope", "infer_sem_refit", "efeito_temp", "semifer_loess", "infer_clima", "infer_forecast",
           "infer_contract", "infer_puro",
           "paridade", "paridade_controle", "sem_crash_1o_mes", "sem_deslocamento"),
  oracle = c(rep("-", 31L), "gate03", "gate11"),
  owner = "PLUGINS",
  tier = c(rep("fast", 13L), "parity", rep("fast", 15L), rep("parity", 4L)),
  stringsAsFactors = FALSE)

prevcarga_gates_run(gate_table, tier = NULL, model = "holt-winters")
