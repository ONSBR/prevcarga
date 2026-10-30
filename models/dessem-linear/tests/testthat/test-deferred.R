## test-deferred.R -- gates do dessem-linear que pertencem a marcos posteriores (DL.9). Cada um aparece na tabela do runner como
## `SKIP` com o dono; nenhum e silenciado: o dono e o marco que o entrega (M6 = LONG/y_true, M7 = sem_crash multiorigem pela
## CLI, M5 = fachada da CLI).
test_that("[DEFERRED M6] gate01_schema_long: saida LONG (schema_long) do dessem-linear", { pc_gate_deferred("M6", "LONG nasce no M6") })
test_that("[DEFERRED M6] gate06_ytrue_real: y_true real de uma origem passada na saida LONG", { pc_gate_deferred("M6", "y_true real nasce no M6") })
test_that("[DEFERRED M7] gate03_sem_crash: forecast sai 0 para varias origens pela CLI/backtest", { pc_gate_deferred("M7", "o laco multiorigem nasce no M7") })
test_that("[DEFERRED M5] fachada: verbos da CLI e recusa de artefato sobre o plugin", { pc_gate_deferred("M5", "fachada da CLI nasce no M5") })
