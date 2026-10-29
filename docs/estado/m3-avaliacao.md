# Estado: M3 -- avaliacao fonte-unica e honesty-gate (fechamento, M3.V)

Epico M3 implementado. Detalhe por modulo em `docs/estado/evaluation.md`, `honesty_gate.md`, `score.md`,
`check_coherence.md`; este arquivo e o fechamento do epico (verificacao M3.V) e o ponto de entrada do indice.

## Implementado (VIVO, fora da lista de stubs)
- `scripts/lib/evaluation.R` -- VIVO. Fonte unica de metrica (`prevcarga_wmape` e a unica definicao; `sum(abs(` so aqui),
  pico por `interval_start` (janela 18-22h; [TBD: ADR-0028]), baselines que leem so `interval_end <= O 00:00` (< O),
  `prevcarga_scorecard`/`prevcarga_skill_score`, leitor unico do LONG (`prevcarga_read_long`), `prevcarga_score` e
  `prevcarga_check_coherence`. Sem `minutes(30)` (a conversao do rotulo e das bordas, `schema_long.R`/`deck_reader.R`).
- `scripts/lib/honesty_gate.R` -- VIVO. Honesty-gate IN-PROCESS falsificavel nos canais `y_true`
  (`prevcarga_honesty_gate`; inferencia com `cut` = `available_until`; treino com `cut` = fim de
  `last_data_day = O-1-data_lag_days`, igual para origin_agnostic e origin_scoped, ADR-0023) e clima previsto
  (`prevcarga_honesty_gate_weather`, as duas regras de `weather_issue_cutoff` com o limite pronto de
  `prevcarga_weather_issue_limit`). Quatro vereditos em ordem fixa: INDETERMINADO, VAZA, HARNESS QUEBRADO, HONESTO.
  Escopo do molde (forecast_origin == origin, reach por saida) [TBD: ADR-0029].
- Verbos em `scripts/lib/cli/`: `score.R` (exits 0/2/4/8) e `check-coherence.R` (0/2/3/4/11). Catalogo de erros em
  `scripts/lib/errors/evaluation.tsv` e `errors/score.tsv` (ADR-0040).

## Verificacao (M3.V)
- Bateria em tabela `tests/test-honesty-matrix.R`: honesto, vazador, spill so em d_plus=9, cego, so-membros,
  nao-deterministico, estilo-combina, regiao vazia, ruido 1e-10 e delta 1e-3; os 4 vereditos alcancados; spill com
  horizon 9 (VAZA) vs 8 (HONESTO); estilo-combina HONESTO no gate real (pin so-`y_true`, herdado pelo M4) e
  > 1e-5 no gate-de-referencia que perturba tambem os membros; treino por classe e clima nas duas regras.
- Processo: `tests/test-score-cli.R` e `tests/test-check-coherence-cli.R` (Rscript por `R.home('bin')`, matriz de
  exits x {text, json}, `exit_code` == status, stdout um unico JSON, stderr vazio, ASCII, traceback so com `--debug`,
  shell sujo TZ/LC_ALL/OPENBLAS == limpo com delta 0). `score` == `prevcarga_scorecard` in-process.
- Fonte: lote `grep_gates_m3` e prova de consumo por AST em `tests/test-static-greps.R`; gate `m3_docs_live`
  (este arquivo cita `evaluation.R` e `honesty_gate.R` como vivos). Sub-gate: `scripts/tools/gates/m3.R`.
- Bracos dependentes de TBD (ADR-0028 pico, ADR-0029 escopo do molde) ficam com `skip()` explicito e motivo.
