# Estado: cache (M7.2) -- cache content-addressed local

Codigo: `scripts/lib/cache.R`; catalogo `scripts/lib/errors/cache.tsv`; testes `scripts/lib/tests/test-cache.R` + `helper-m7-fixtures.R`.

- Store local sob `<ctx$state_dir>/cache/<kind>/<hh>/<key>/` (`hh` = 2 primeiros hex apos `v_`), kinds `features|artifact|member_long|khat`.
  Entrada = `object.rds` (saveRDS v3, sem compressao: bytes deterministicos) ou `object.parquet` (data.table) + `meta.json`
  `{key, kind, identity, lateral, sha256, format}`. Laterais (platform, bundle_id, trained_at, host) so no meta.json.
- Chave = `prevcarga_version_hash` do provenance.R (`v_<16 hex>`, NAO 64 hex como dizia o texto do ticket: reuso do hash e da identidade de 8 chaves
  do M5.3, sem hash paralelo). `model_id` = mdir; `feature_config` = {kind, exogena, split, feature_config, model_versions, seed, weather_issue_cutoff}
  (LIN_MED != LIN_MAX pela exogena); `cutoff` por classe (ADR-0023):
  `features`/`member_long` {origin, available_until} sempre; `artifact` origin_agnostic {split, train_origin, last_data_day}, origin_scoped
  {split, train_origin}, nunca available_until (o sweep de `--corte` nao re-treina); `khat` {subsistema, origin, window_days}.
  `train_origin`/`last_data_day` vem prontos da linha do membro de `prevcarga_resolve_cuts` (dates.R): o cache nao calcula corte nem le descriptor.
- `prevcarga_cache_put`: tmp `<dir>.tmp-<pid>` + rename atomico; chave existente com mesmo sha => idempotente; sha diferente =>
  `integrity.cache_put_conflict` (exit 9), entrada original intocada. `prevcarga_cache_get`: NULL se ausente (MISS); sha divergente ou
  meta ilegivel => move para `cache/_quarantine/<kind>/<key>/`, aviso `warning.cache_corrupt_quarantined`, MISS. `prevcarga_cache_stats(ctx)`
  = `list(hit, miss, corrupt)` por `state_dir`.
- Ids em `errors/cache.tsv` (dois segmentos, regra do errors.R): `contract.cache_{kind,key,member_label,split,identity}`,
  `integrity.cache_put_conflict`, `warning.cache_corrupt_quarantined` (o aviso do ticket, `cache_corrupt_quarantined`, ganhou o prefixo `warning.`).
- `weather_issue_cutoff` reservado na identidade; o uso ponta a ponta fecha no M7.8. GC fora do pacote.
- Interino: parquet nao preserva atributos de data.table (use rds via lista se precisar de attrs).
