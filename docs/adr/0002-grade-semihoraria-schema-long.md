# ADR-0002 — Grade semi-horária canônica (48 slots, cubic-spline no ingest) + formato LONG de 8 colunas + re-ancoragem em O

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0001](0001-topologia-sin-map-reduce.md), [ADR-0003](0003-tempo-naive-brt-runtime-env.md), [ADR-0006](0006-backtest-honesto-corte-treino.md), [ADR-0013](0013-combina-convexa-metodo.md)

## Contexto
Precisa-se de um contrato único entre QUALQUER modelo e tudo a jusante (combinação, reconciliação, score), e de uma nomenclatura temporal à prova de off-by-one. Historicamente a convenção D+n do DESSEM e granularidades mistas (horário vs semi-horário) foram fonte de erros de um dia inteiro.

## Decisão
Grade canônica única = **semi-horária (48 slots/dia)**; dados horários são elevados por cubic-spline atravessando fronteiras de dia na borda de ingestão (CARGASHHIST é a verdade, CARGAHIST é ignorado), e o builder recusa `y_pred` ≠ 48 colunas. `build_long_output()` é o ÚNICO lugar que converte a previsão de qualquer modelo no schema **LONG de 8 colunas** `{area, forecast_origin(=O), target_datetime, lead, y_pred, y_true, d_plus, granularity_min}`, re-ancorado em O (d_plus=0 = dia da rodada; `lead = d_plus*slots + slot` derivados deterministicamente), com validador embutido que reprova d_plus negativo/não-contíguo, slot fora de 1..S e target_datetime incoerente.

## Consequências
As 8 colunas são o contrato congelado da fronteira combinação→reconciliação: mexer nelas quebra combina/reconcilia/painel de uma vez. A dimensão 48 fixa a aritmética de lead e a matriz de toda previsão. O validador é a guarda determinística onde honestidade temporal é imposta, não documentada. Carregar a convenção antiga (d_plus=0 = amanhã) erra por um dia em toda métrica.

## Alternativas consideradas
Cada modelo expor sua estrutura nativa/wide com adaptadores ponto-a-ponto; formato wide (1 coluna/slot); manter grade horária e cada modelo decidir upsample; interpolação linear em vez de spline; rotular a saída no vocabulário DESSEM D-1/D/D+1 (fonte histórica do off-by-one).

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/lib/build_long_output.R:19-21
- scripts/lib/build_long_output.R:68-72
- scripts/lib/build_long_output.R:93
- scripts/lib/build_long_output.R:150-179
- scripts/data-prep/migrate_legacy_data.py:17
- scripts/data-prep/migrate_legacy_data.py:113-197
- docs/ESTADO-ATUAL.md:639-649
- docs/ESTADO-ATUAL.md:871-872
