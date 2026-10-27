# Estado: deck_reader (M2.1, parte 1; ADR-0035, ADR-0037, ADR-0040)

- `scripts/lib/deck_reader.R` e o unico leitor de `manifest.json` e o unico chamador de `arrow::open_dataset` (teste greppa o resto de `scripts/lib`).
  Requer `errors.R` e `config.R` (`prevcarga_require_data_dir`) carregados antes.
- `prevcarga_deck_manifest(data_dir)`: schema v1 fechado (`schema_version`, `load_label?`, `timezone`); chave desconhecida (inclusive `corte`) =>
  `contract.manifest_unknown_key` (`where` = chave, `timezone.<k>` no aninhado); fuso da carga sempre obrigatorio (`load.timestamp`), os de clima
  so se o dataset esta no store (`weather_observed.timestamp`, `weather_forecast.valid_time|issue_time`); ausente => `contract.tz_missing`, fora de
  `OlsonNames()` => `contract.tz_invalid`; manifest ausente/JSON ruim => `contract.manifest_missing|invalid` (`where = manifest.json`). Nunca devolve fuso default.
- `prevcarga_resolve_load_label(ctx, manifest)`: manifest > `ctx$load_label_flag` > `ctx$load_label_config` > `end`; manifest x flag diferentes =>
  `contract.load_label_conflict`. Devolve `list(value, source)` (`manifest|flag|config|default`).
- Fuso (`.prevcarga_apply_tz`): o arrow entrega `timestamp[ns]` ingenuo como POSIXct com tz do host; o leitor faz `with_tz(UTC)` (recupera os digitos) ->
  `force_tz(declarado)` -> `with_tz(tz da carga)`. Independe do fuso/locale da sessao. Parquet ja com fuso => `contract.tz_in_parquet`.
- `prevcarga_read_load(ctx, areas, from, to)` => data.table chaveada `(area_code, interval_start)` com `interval_start/interval_end/load_mwh` (sem `timestamp`);
  `end` => -30 min, `start` => identidade; attrs `load_label`, `load_label_source`, `tz`. `prevcarga_read_weather(ctx, kind, areas, from, to, select)`:
  clima chaveado por `area_code`, sem deslocamento. Filtro de particao `year` via `arrow::Scanner` + `Expression` (o ambiente nao tem dplyr; nao use verbos dplyr).
  Dataset ausente => `contract.dataset_missing`. Corte/`available_until`/emissao mais fresca: M2.2; feriados/painel: M2.7.
- `prevcarga_deck_probe(data_dir)`: por dataset (`load`, `weather_observed`, `weather_forecast`, `holidays`) `present`, `files`, `hive_ok`, `columns`, `types`,
  `tz_in_parquet`, `unreadable`; so metadados (sem `collect`).
- Catalogo: `scripts/lib/errors/deck_reader.tsv` (8 ids `contract.*`).
- Testes: `scripts/lib/tests/test-deck-reader.R`; deck sentinela compartilhado `scripts/lib/tests/helper-deck.R` (`pc_deck_sentinel`, `pc_deck_clean`,
  `pc_deck_manifest`, `pc_ctx_deck`; particoes Hive gravadas com `arrow::write_parquet`, pois `write_dataset` exige dplyr). `until` do sentinela = carga so com `interval_end <= until`.
- O store real em `data/` ainda nao tem `manifest.json` (o leitor aborta `contract.manifest_missing` ate o store ser regravado).
