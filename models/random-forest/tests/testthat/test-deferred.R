## test-deferred.R -- gates do random-forest que pertencem a epicos posteriores (RF.4). Cada um aparece na tabela do runner
## como `SKIP` com o dono; nenhum e silenciado: o dono e o marco que o entrega.
test_that("[DEFERRED M6] gate04: saida LONG (schema_long) do membro", { pc_gate_deferred("M6", "LONG nasce no M6") })
test_that("[DEFERRED M6] gate08: honestidade da combina com o membro", { pc_gate_deferred("M6", "combina nasce no M6") })
test_that("[DEFERRED M7] gate12: A/B de ponta a ponta contra o oraculo", { pc_gate_deferred("M7", "A/B nasce no M7") })
test_that("[DEFERRED M5] fachada: verbos da CLI sobre o plugin", { pc_gate_deferred("M5", "fachada da CLI nasce no M5") })
test_that("[DEFERRED M8] cross_os: paridade Linux x Windows", { pc_gate_deferred("M8", "cross-OS nasce no M8") })
