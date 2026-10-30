## test-deferred.R -- gates do lgbm que pertencem a marcos posteriores (LGBM.6). Cada um aparece na tabela do runner como `SKIP`
## com o dono; nenhum e silenciado: o dono e o marco que o entrega.
test_that("[DEFERRED M6] gate11_ytrue_real: y_true real do oraculo contra a entrega do membro", { pc_gate_deferred("M6", "y_true real nasce no M6") })
test_that("[DEFERRED M6] nowcast_dplus0_long: saida LONG (schema_long) do nowcast D+0", { pc_gate_deferred("M6", "LONG nasce no M6") })
test_that("[DEFERRED M5] fachada: verbos da CLI e recusa de artefato sobre o plugin", { pc_gate_deferred("M5", "fachada da CLI e recusa de artefato nascem no M5") })
