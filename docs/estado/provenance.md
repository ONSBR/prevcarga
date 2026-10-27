# Estado: provenance (M1.3; ADR-0011)

- `scripts/lib/provenance.R`: `prevcarga_version_hash(fp)` exige EXATAMENTE 8 chaves (model_id, data_fingerprint, cutoff,
  feature_config, code_commit, renv_lock_hash, load_label, load_tz); lateral (trained_at/package_version/run_id) =>
  `contract.fingerprint_lateral`, ausente => `_missing_key`, extra => `_extra_key`. Canonicalizacao recursiva (radix),
  NULL/NA -> null, POSIXct em UTC (`...+0000`), sha256, id `v_<16 hex>`.
- `prevcarga_member_set_hash(members)`: sha256 completo das linhas (sigla, mdir, exogena) na ordem declarada; definicao unica.
- `prevcarga_run_manifest(...)`: chaves deterministicas + laterais (trained_at, package_version, run_id, extensiveis via `...`).
  Nunca `cutoff` run-level (`contract.run_manifest_forbidden_key`). Carimbos tz/lc/blas do `prevcarga_runtime_stamp()`
  (exige enforce/apply antes). Campo de fuso = `load_tz`. `delivery_days` escalar (membro) ou lista por sigla (rodada).
  `prevcarga_write_run_manifest(m, dir)` => `<dir>/run_manifest.json` atomico; `prevcarga_read_run_manifest(dir)`.
- `prevcarga_freeze/thaw`: saveRDS v3 atomico + `<p>.manifest.json` {sha256, class, nrows|length, format_version 1};
  thaw confere o sha ANTES do readRDS (`integrity.*`, exit 9). Integridade != autenticidade.
- `prevcarga_code_commit(root)` usa `git` (excecao dev) => `<40hex>[-dirty]` ou `unknown`; `prevcarga_renv_lock_hash(root)`.
- Erros: ids de integrity ja em `core.tsv`; novo `errors/provenance.tsv` (`integrity.freeze_file_missing`).
- Nota: `identical()` de data.table pos-thaw exige ignorar o ponteiro `.internal.selfref` (muda a cada readRDS; limite do data.table).
- Testes: `scripts/lib/tests/test-provenance.R`.
