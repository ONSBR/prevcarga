# Estado: reconcile (R.1, nucleo puro)

- `scripts/lib/reconcile.R`: funcoes puras (sem I/O/leitor/registry). Requer errors, schema_long, evaluation
  (`prevcarga_assert_long`), data.table e lubridate.
  - `prevcarga_reconcile_plan(leaves_present, topology)` -> `list(closed, stray, sin_closed)` (ordem de `topology$order`).
  - `prevcarga_reconcile_bottomup(long_leaves, subsystem, topology, ctx)`: soma `y_pred` por
    (forecast_origin, target_datetime, lead, d_plus, granularity_min), so onde `n == length(areas)`;
    `attr(, "dropped")` = `data.table(d_plus, n)`; aviso `reconcile.coverage_dropped`. Folha ausente =>
    `completude.leaf_missing`; lead divergente/duplicata => `contract.reconcile_input`.
  - `prevcarga_khat_ratio(truth, subsystem, topology)` (SS_true/BU_true por interval_end, BU_true so com todas as folhas),
    `prevcarga_khat_fit(ratio, origin, window_days)` (janela `[O-W, O-1]` inclusiva, bucket hour x month x dow do
    `interval_start`, dow com `week_start = 7`, `glob` = media dos ratio da JANELA), `prevcarga_khat_apply(bu, profile, ctx)`
    (`y_pred = BU_pred * k`, sem clamp, `attr n_fallback`, aviso `reconcile.khat_fallback`),
    `prevcarga_reconcile_sin(long_subsystems, topology)` (`n == length(topology$subsystems)`),
    `prevcarga_reconcile_long(dt, area, ctx)` (LONG 8 colunas, `forecast_origin` texto, `y_true` NA).
- Regra: "soma por interval_end, bucket por interval_start". O Bottom-Up agrupa por `target_datetime` (= interval_end; bijecao com
  interval_start, mesmo agrupamento); o `interval_start` do bucket vem so de `prevcarga_long_interval_start` (schema_long.R).
  O modulo nao faz aritmetica de hora (gate `minutes30`).
- Divergencias deliberadas do oraculo: bucket por interval_start (o oraculo usa o rotulo de fim), glob da janela (o oraculo usa todo o historico).
- `scripts/lib/errors/reconcile.tsv` declara todos os ids do epico. Os avisos usam classe `completude` (errors.R nao aceita classe `ok`).
- Testes: `test-reconcile-core.R`, `test-reconcile-khat.R`, helper `helper-reconcile-fixtures.R`.
