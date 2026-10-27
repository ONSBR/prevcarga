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
  Dataset ausente => `contract.dataset_missing`. Feriados/painel: M2.7.
- `prevcarga_deck_probe(data_dir)`: por dataset (`load`, `weather_observed`, `weather_forecast`, `holidays`) `present`, `files`, `hive_ok`, `columns`, `types`,
  `tz_in_parquet`, `unreadable`; so metadados (sem `collect`).
- Catalogo: `scripts/lib/errors/deck_reader.tsv` (8 ids `contract.*`).
- Testes: `scripts/lib/tests/test-deck-reader.R`; deck sentinela compartilhado `scripts/lib/tests/helper-deck.R` (`pc_deck_sentinel`, `pc_deck_clean`,
  `pc_deck_manifest`, `pc_ctx_deck`; particoes Hive gravadas com `arrow::write_parquet`, pois `write_dataset` exige dplyr). `until` do sentinela = carga so com `interval_end <= until`.
- O store real em `data/` ainda nao tem `manifest.json` (o leitor aborta `contract.manifest_missing` ate o store ser regravado).

## M2.2 (parte 2): corte derivado do dado, filtro de saida, clima por regra
- `prevcarga_available_until(load, origin, areas = NULL)`: por area, `interval_end` do ultimo intervalo contiguo desde `O 00:00` (sem intervalo em O => `O 00:00`;
  48 => `O+1 00:00`); global = minimo; devolve `available_until`, `available_until_by_area` (`area_code, until, gap`), `spread`. Sem relogio, manifest nem flag.
- `prevcarga_read_load(ctx, areas, from, to, origin = NULL, truncate_at = NULL)`: com `origin` deriva o corte (a janela de derivacao sempre inclui O), entrega so
  `interval_end <= available_until` e carimba `available_until`, `available_until_by_area`, `available_until_spread`, `origin`. Buraco no dia O =>
  `completude.origin_day_gap` (`where = area:HH:MM`); sem linha => `completude.no_load`; areas divergentes => aviso `warning.available_until_spread`.
  `truncate_at` remove `interval_end > truncate_at` antes da derivacao (== deck fisicamente truncado). Sem `origin` o comportamento e o do M2.1.
- `prevcarga_last_load_day(ctx, areas = NULL)`: dia (`as_date(interval_start, tz)`) do ultimo intervalo; le so a particao `year` mais alta nao vazia.
- `prevcarga_weather_issue_limit(rule, origin, available_until, tz_load)`: `available_until` => ele mesmo; `origin_midnight_utc` => `O 00:00 UTC` no fuso da carga.
  Regra vem de `ctx$weather_issue_cutoff` (fonte `ctx$sources$weather_issue_cutoff`, default `default` se ausente). `prevcarga_freshest_issue(fc, limit)`: emissao mais fresca <= limite por area.
- `prevcarga_read_weather(..., origin, truncate_at)`: observado `<= available_until`; previsto so a emissao mais fresca permitida (escolhida antes do recorte de `valid_time`);
  attrs `weather_issue_cutoff`, `weather_issue_cutoff_source`, `weather_issue_limit`, `freshest_issue_by_area`. `available_until` vem da carga lida pelo proprio leitor.
- Catalogo: `scripts/lib/errors/deck_cut.tsv`. Testes: `test-deck-reader-cut.R`; helper ganhou `pc_deck_until`, `pc_deck_drop`, `pc_deck_perturb`, `pc_deck_issues`.
