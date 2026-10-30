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
  - `run_manifest.json` (identidade da rodada, escrito pelo forecast no scratch; ver `docs/estado/forecast.md`): copiado em bytes para a raiz da
    entrega quando existe, fora de `arquivos[]` e ignorado no `.export_key_check` como o `manifest.json` (rerun = no-op). O `verificar` le dele.
  - Atomicidade: staging `.tmp-<run_id>` no mesmo volume + `file.rename`; erro remove o staging (`integrity.copy_failed`, exit 9).
- `scripts/lib/cli/export.R` (M6.9): verbo `export --scratch=<dir> [--niveis=<n>] [--incluir-membros]` (flags `scoped`, dono M6). A chave de entrega e
  RECONSTRUIDA do `run.json` do `--scratch` (origem, available_until, versao, timezone) por `prevcarga_export_key`, nunca de flag (`--origin` e ignorado):
  mesma chave e mesmo manifest que o `forecast` produziu. Scratch ausente/sem run.json => `state.nothing_to_deliver` (exit 13). `gerado_em` sai do carimbo do run_id da fachada.
  Flag `--scratch` (nao `--from`, como dizia o ticket): `--from` casa o gate de flags de data e o docopt global o aceitaria em qualquer verbo; `--origin` segue o unico flag de data.
- Erros: `scripts/lib/errors/export.tsv`. Testes: `scripts/lib/tests/test-exports.R` (scratch de fixture montado no proprio teste).
- Nome da entrega: o manifest da entrega e `manifest.json` (fixado pelo ticket/blueprint). O guard do leitor unico (m2_single_reader*,
  test-deck-reader) mira REFERENCIAS ao manifest do DECK: o literal em linha de codigo, exceto a linha que e so ESCRITA (write*), com
  CONTROL de que a escrita do export nao casa e de que toda leitura/atribuicao do literal fora do deck_reader.R casa. Sem excecao por nome.

## M6.7 -- chave imutavel da entrega
- `exports.R`: `prevcarga_forecast_versao(parts)` (12 hex), `prevcarga_export_key(origin, available_until, versao, tz)` e `.export_key_check`.
  Layout `forecasts/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>` (mesmo prefixo `forecasts/` do bucket, ADR-0039): ASCII, sem `:`, separador `/`; data/hora do
  `available_until` no tz da carga (lubridate, sem relogio, sem conta de hora). `run_id` = 12 hex do sha256 da chave (nunca PID/relogio).
- `versao`: o `prevcarga_version_hash` (provenance.R) exige EXATAMENTE 8 chaves; os insumos da entrega sao mapeados nelas (model_id = "forecast",
  code_commit = versao do pacote, data_fingerprint = v_<hash> do k-hat por ss, feature_config = membros + pesos da combina + hash da topologia,
  cutoff = weather_issue_cutoff, load_label, load_tz = null). Ordem de membros/subsistemas e invariante. O `v_` do hash e removido (12 hex).
- Imutavel: o check roda no staging pronto, antes do `file.rename`. Chave ausente => publica; existente com mesmo conjunto de arquivos e sha256 =>
  no-op (`ja_publicado = TRUE`, bytes e manifest originais preservados, staging removido); qualquer diferenca (inclusive arquivo adulterado ou
  a mais) => `integrity` `export.key_conflict` (exit 9, `details$path` = 1o divergente em ordem radix), nada sobrescrito. O nome do manifest
  da entrega sai do retorno da escrita (sem literal em linha de leitura), para nao enfraquecer o gate do leitor unico.
- Catalogo: `scripts/lib/errors/export_key.tsv`. Testes: `test-exports_key.R`. O verbo `export` ainda usa a chave PROVISORIA (gramatica final: M6.9).
- Aberto: a chave de cache do M7.2 tem 16 hex (nao 64); a `versao` aqui e 12 hex (regex aceita 12 a 64). Nao tratado neste ticket.
- `code_commit` do fingerprint recebe `parts$package` (versao do pacote), como o ticket M6.7 fixa explicitamente ("versao do pacote"); NAO e o commit
  git de `prevcarga_code_commit` (M7.1/M7.2). Dois builds com a mesma versao e codigo diferente geram a mesma `versao`: o chamador que quiser
  distingui-los passa o commit em `parts$package`. Decisao a revisitar se o orquestrador (M6.9) tiver `ctx$code_commit`.
