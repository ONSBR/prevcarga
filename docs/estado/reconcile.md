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

## R.2 -- perfil k-hat como artefato do registry (unify, decisao do Gabriel 2026-10-29)
O perfil k-hat e mais um artefato no layout do registry: `<state_dir>/artifacts/reconcile/<ss>/{latest.txt,
v_<hash>/{artifact.rds, artifact.rds.manifest.json, run_manifest.json, SHA256SUMS}}` (mesmo prefixo do bucket;
copia byte a byte aplica sem conversao). `reconcile.R` guarda SO o que e especifico do k-hat; publicar/resolver/
carregar/mover o ponteiro sao do nucleo `prevcarga_registry_*` (M5.2), sem copia nem excecao no gate `registry_lifecycle`.

API (ctx = `list(state_dir=, tz=)`; a raiz sai de `prevcarga_registry_key`, nao e argumento `root`):
`prevcarga_khat_key(ss, ctx)`, `prevcarga_khat_data_hash(ratio)`, `prevcarga_khat_version(fit, meta)` (fingerprint de 8
chaves, model_id `reconcile/<ss>`, cutoff = O-1), `prevcarga_khat_freeze(fit, meta, ctx)` (no-op se mesmos bytes;
`integrity.khat_conflict` se o v_<hash> existe com bytes diferentes; staging `.tmp-*` do registry),
`prevcarga_khat_advance_latest(ss, versao, khat_origin, ctx)` (so para frente; menor => `reconcile.khat_not_promoted`;
escreve via `prevcarga_registry_promote_latest`), `prevcarga_khat_resolve(ctx, ss, khat_version)` (ausente =>
`state.khat_missing`, exit 13), `prevcarga_khat_thaw(ctx, ss, versao)` (SHA256SUMS do registry + schema =>
`contract.khat_profile`). `trained_at` e lateral: so no `run_manifest.json`, nunca no artefato nem no hash.

Desvios do plano original, por unify: (1) nao existe `prevcarga_khat_promote` (gate registry_lifecycle; vira
`prevcarga_khat_advance_latest` + core); (2) assinaturas recebem `ctx` em vez de `root`; (3) adulteracao no thaw sai
`integrity.artifact_hash` (exit 9, SHA256SUMS do core) e nao `integrity.freeze_sha_mismatch`; (4) manifest e
`run_manifest.json` (nome do core), com `arquivos[{path:'artifact.rds', sha256}]`. Consumidores (R.3, CLI) chamam esta API.

## R.3 -- orquestracao train/apply (`reconcile.R`, secao final)
- `prevcarga_reconcile_train(ctx, origin, subsystems = "all", window_days)`: `last_data_day` de `prevcarga_resolve_cuts(ctx, O)` (dates.R,
  sem conta de corte local); verdade SO por `prevcarga_read_load(ctx, areas = c(folhas, ss), from = O-W, to = O-1)`; ratio -> fit ->
  meta (convencao `load_label`/`tz` vem dos atributos do leitor) -> `prevcarga_khat_freeze` -> `prevcarga_khat_advance_latest` (unify R.2;
  nao existe `prevcarga_khat_promote`). Devolve por ss `{versao, path, khat_origin, n_ratio, promovido, k_hat_resumo{min,max,fora_faixa}}`;
  `fora_faixa` conta celulas do perfil fora de `khat_range` (so informa, nunca clampa).
- `prevcarga_khat_check_policy(prof, ctx, origin, pinned = FALSE)`: unico lugar da politica de uso do perfil. DECISAO (Gabriel, 2026-10-29):
  o k-hat e REAJUSTADO POR ORIGEM, como o oraculo (Delta = 0); nunca se reusa o `latest` de outra origem. Cada origem O usa o perfil com
  `khat_origin == O`: posterior => `honesty.khat_after_origin`; anterior sem pino => `state.khat_missing` (rode train para esta origem);
  convencao => `contract.khat_convention` (`where` = `load_label`|`timezone`). Com versao fixada de proposito (`pinned`, `khat_version`)
  vale `khat_origin <= O` e `khat_age_days` e informado. Devolve `khat_age_days`.
  `.prevcarga_khat_resolve_for_origin` acha a versao da origem (latest se for dela; senao varre as versoes da celula, pois o backtest
  treina para tras e o `latest.txt` so anda para frente). O registry/`latest.txt` (R.2) segue so armazenando/resolvendo o perfil de uma origem.
  Convencao efetiva = `ctx$load_label`/`ctx$tz`, ou o manifest do deck quando ausentes.
- `prevcarga_reconcile_apply(long_leaves, ctx, subsystems = "all", khat_version = NULL, eval = FALSE)`: valida a entrada (`assert_long`;
  uma unica `forecast_origin`), recusa subsistema pedido que nao fecha (`completude.leaf_missing`, `where` = folhas ausentes; nada devolvido),
  resolve+thaw+politica+khat_apply por ss, SIN so quando todos os subsistemas da topologia foram pedidos. Nada grava em disco.
  Retorno `{long{ss..., SIN|NULL}, khat, cobertura_descartada, wmape|NULL, scorecard|NULL, warnings}`.
- `need_truth = eval`: sem `eval` nenhum leitor e chamado e `y_true` e NA. Com `eval`, `prevcarga_read_actual` por area (a saida nao traz a
  coluna da area), casado por `target_datetime == interval_end`; `wmape{bottomup, reconciliado, teto}` por `prevcarga_wmape`
  (`teto` = BU_pred + (SS_true - BU_true)); `y_true` do SIN = soma dos `y_true` dos subsistemas; `y_pred` independe da verdade.
- `apply` e single-origin (uma `forecast_origin` por chamada, senao `contract.reconcile_input`): a rodada diaria e uma origem; o backtest
  roda muitas origens em SEQUENCIA para tras, um train+infer por origem, chamando train/apply uma vez por origem. Nada a relaxar.
- Testes: `test-reconcile-apply.R` (leitor espionado; FORBIDDEN/CONTROL).
