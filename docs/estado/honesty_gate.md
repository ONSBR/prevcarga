# honesty_gate.R (M3.2)

Molde in-process `prevcarga_honesty_gate(run_fn, panel, cut, horizon, tol = 1e-5, perturb)` em `scripts/lib/honesty_gate.R`.

- Perturba SO o canal `y_true`. FORBIDDEN: `interval_end` em `(cut, cut + horizon d]` (com controle de alcance `reach_ok`); CONTROL: `(cut - 1d, cut]`.
- `cut` e recebido pronto, nunca calculado aqui. Inferencia = `available_until`; treino = fim de `last_data_day = O-1-data_lag_days` (`cuts$last_data_day`, M2.3), igual para origin_agnostic e origin_scoped (ADR-0023). Corte = fim de `train_origin` faz o agnostico dar VAZA.
- Veredito em ordem fixa: CLEAN2>tol INDETERMINADO; FORBIDDEN>tol VAZA; !reach_ok INDETERMINADO; CONTROL<=tol HARNESS QUEBRADO; senao HONESTO. O gate nao aborta em VAZA (runtime converte em `honesty.leak`, exit 15).
- Delta estrutural (linhas, chaves, padrao de NA) = Inf. Erros de `run_fn` propagam. Sem subprocesso/disco/env/relogio [TBD: ADR-0029].
- Helpers `.prevcarga_hg_{check,regions,copy,arm,delta,verdict}` reusaveis pelo canal de clima (M3.5). Fixtures: `scripts/lib/tests/helper-run-fn.R`; suite: `test-honesty-gate.R`.
- Acoplamento: `last_data_day` vem de `prevcarga_resolve_cuts` (dates.R, M2.3), so como valor de fixture nos testes.
