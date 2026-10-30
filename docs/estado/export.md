# Estado: export (M6.2)

- `scripts/lib/exports.R`: `prevcarga_export(scratch, key, niveis = "subsistemas", incluir_membros = FALSE, ctx)` copia BYTES do
  scratch da rodada (`SIN.parquet`, `subsistemas/<ss>.parquet`, `folhas/<a>.parquet`, `membros/<m>/<a>.parquet`) para
  `file.path(ctx$state_dir, key$rel)`. Nunca recalcula k-hat, nunca reserializa parquet, nao le relogio, zero S3/HTTP, sem symlink.
  - Metadados da rodada vem do `run.json` do scratch (covered/stray/closed/sin_closed/members/khat/available_until/regra de clima/versao).
    `linhas` e `sha256` vem de `arquivos[]` do `run.json` (gravado pelo reconcile, R.5); o export recomputa o sha256 da fonte e aborta
    `integrity.copy_failed` se divergir. Nao abre parquet (leitor unico: deck_reader.R). Arquivo ausente ou nao registrado => aviso e omitido.
  - Niveis-teto: `sin` = SIN + folhas soltas; `subsistemas` = + subsistemas; `tudo` = + folhas cobertas; membros so com `incluir_membros`.
    A folha solta (stray) e sempre entregue. Arquivo listado e ausente no scratch => aviso `completude.export_file_missing`.
    Nada a entregar => `state.nothing_to_deliver` (exit 13) antes de criar qualquer diretorio.
  - `manifest.json` (chaves em ordem fixa; `gerado_em` = `ctx$generated_at`, unico campo nao deterministico; nao hasheado nem listado),
    binario com LF, UTF-8, indent 2. khat por subsistema `{ss, versao, khat_origin}` copiado do run.json.
  - Atomicidade: staging `.tmp-<run_id>` no mesmo volume + `file.rename`; erro remove o staging (`integrity.copy_failed`, exit 9).
- `scripts/lib/cli/export.R`: verbo `export --scratch=<dir> [--niveis=<n>] [--incluir-membros]` (flags `scoped`). Chave PROVISORIA
  `exports/<basename(scratch)>`; a chave imutavel e o layout `forecasts/` sao do M6.7 e a gramatica final do M6.9. `gerado_em` sai do
  carimbo do `run_id` da fachada (relogio lido uma vez em config.R).
- Erros: `scripts/lib/errors/export.tsv`. Testes: `scripts/lib/tests/test-exports.R` (scratch de fixture montado no proprio teste).
