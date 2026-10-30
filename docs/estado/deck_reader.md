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
  Dataset ausente => `contract.dataset_missing`.
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
  Corte da manha (FIX 5): a janela de derivacao vai de `min(from, O-1)` a `max(to, O)` (inclui a vespera), e `prevcarga_read_weather` repassa `from/to`
  a `.prevcarga_derive_cut` como `prevcarga_read_load`. `truncate_at = O 00:00` num store com o dia O, ou um deck sem linha no dia O, dao `available_until = O 00:00`
  (antes `completude.no_load`, porque a janela era so o dia O). Sem carga nem na vespera segue `completude.no_load`.
- `prevcarga_read_panel(..., truncate_at = NULL)` repassa o MESMO `truncate_at` aos tres leitores (carga, observado, previsto); o par e
  `prevcarga_resolve_cuts(..., truncate_at)` (dates.R), com o mesmo valor. Testes: `test-read-panel.R` (corte da manha, FORBIDDEN dia O, CONTROL vespera,
  truncate_at == deck fisicamente truncado) e `test-dates.R`.
- `prevcarga_last_load_day(ctx, areas = NULL)`: dia (`as_date(interval_start, tz)`) do ultimo intervalo; le so a particao `year` mais alta nao vazia.
- `prevcarga_weather_issue_limit(rule, origin, available_until, tz_load)`: `available_until` => ele mesmo; `origin_midnight_utc` => `O 00:00 UTC` no fuso da carga.
  Regra vem de `ctx$weather_issue_cutoff` (fonte `ctx$sources$weather_issue_cutoff`, default `default` se ausente). `prevcarga_freshest_issue(fc, limit)`: emissao mais fresca <= limite por area.
- `prevcarga_read_weather(..., origin, truncate_at)`: o observado sai com `valid_time` (F4: a coluna crua `timestamp` do store e renomeada na borda de entrada, logo
  apos o fuso; chave `(area_code, valid_time)`; nenhum consumidor ve `timestamp`); observado `<= available_until`; previsto so a emissao mais fresca permitida (escolhida antes do recorte de `valid_time`);
  attrs `weather_issue_cutoff`, `weather_issue_cutoff_source`, `weather_issue_limit`, `freshest_issue_by_area`. `available_until` vem da carga lida pelo proprio leitor.
- Catalogo: `scripts/lib/errors/deck_cut.tsv`. Testes: `test-deck-reader-cut.R`; helper ganhou `pc_deck_until`, `pc_deck_drop`, `pc_deck_perturb`, `pc_deck_issues`.

## M2.7 (parte 3): feriados e painel do leitor
- `prevcarga_read_holidays(ctx, areas, from = NULL, to = NULL)` (FIX 6: `from`/`to` NULL => todos os feriados da area, todas as particoes): `auxiliary/holidays` (Hive `area_code=/year=`), colunas `area_code, date, holiday_type_id, prevcarga_code, simplified_code`;
  `date` = data civil (`Date`) sem deslocamento e SEM corte de `available_until` (calendario conhecido de antemao); particao `year` exata; nao filtra codigos
  (`simplified_code = -1` sai como esta; o finding `code_negative` e do validate-deck, M2.4). Chaveado `(area_code, date)`.
- `prevcarga_read_panel(ctx, origin, areas, horizon_days, history_days)`: origem ja resolvida (o leitor nao depende de `dates.R`); valida `areas` contra
  `prevcarga_topology(path)$leaves` (`ctx$topology_path` ou `<ctx$root>/config/topology.yaml`) antes de ler => `topology.area_undeclared` (exit 4, `where` = a area).
  Devolve `load` (O-hist..O, cortada), `weather_observed` (O-hist..O, <= available_until), `weather_forecast` (O..O+horizon, emissao mais fresca permitida), `holidays`
  (FIX 6: o calendario INTEIRO da area, sem janela, como o `.read_holidays` do oraculo; a janela antiga perdia o feriado anterior ao inicio do historico e
  o `days_since_last_holiday` do RF divergia, ex. RS 2023-09-20 em rs-2024-10-01) e `attrs` (`origin`, `available_until`, `available_until_by_area`, `load_label`, `load_label_source`, `tz`, `weather_issue_cutoff`, `weather_issue_limit`).
  `pc_panel_attrs` copia os atributos sem recalcular e aborta `contract.constraint_violated` se `tz`/`available_until` divergirem. O mapeamento area -> serie de clima e do plugin.
- O limite inferior do filtro de particao `year` passou de `year(from)-1` para `year(from - 1 dia)` (nenhum ano fora da janela e coletado; o recorte fino segue em memoria).
- Catalogo: `topology.area_undeclared` mora em `scripts/lib/errors/topology.tsv` (o `panel.tsv` foi removido). Testes: `test-read-panel.R`; helper ganhou `pc_deck_panel`, `pc_ctx_panel`, `pc_deck_topology`, `pc_deck_holidays_default`
  e `pc_deck_perturb` cobre `holidays` (+1 em `prevcarga_code`).

## manifest.json do store atual (M2.6)
- Template versionado `config/deck-manifest.store.json` (schema v1, `load_label: start`, `Etc/GMT+3`; `issue_time` `UTC`; sem `corte`). Valores a confirmar pelo Gabriel antes do merge.
- `scripts/tools/write_store_manifest.R --target <copia>` (dev, fora do pacote) grava o template numa copia/overlay do store: valida pelo leitor, identico => no-op, diferente => `contract.manifest_exists_different` (exit 2; catalogo em `scripts/lib/errors/store_manifest.tsv`). O produto nunca escreve no `data_dir`. Nada foi gravado no store real (`data/`).
- Smoke `validate-deck` pendente (depende do verbo M2.5, nao integrado): rodar sobre a copia e registrar no PR.
