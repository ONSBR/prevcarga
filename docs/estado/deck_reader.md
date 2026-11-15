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
- **issue_time do clima previsto (F7.1, docs):** `issue_time` = `dat_referencia` 00:00 UTC do TEMPPREVHIST (nao `din_origemprevisaoutc`); excecao medida: de 2018-12-23 a 2021-03-27 o `din_origemprevisaoutc` esta +1 dia (824 emissoes de SP).
  O store historico e relogio de parede BRT com horario de verao ate 2019: lido como `Etc/GMT+3`, o `issue_time` do verao 2018-19 erra 1 h. Dia coberto do clima previsto = 48 instantes (00:00..23:30), sem tolerancia; faltando, o forecast corta o horizonte com `warning.forecast_weather_horizon_cut`.

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
- `prevcarga_weather_issue_limit(rule, origin, available_until, tz_load, lag_hours)`: `available_until` => `available_until - lag_hours` (no fuso da carga;
  lag de publicacao do deck, ver abaixo; ausente => `contract.weather_lag_missing`); `origin_midnight_utc` => `O 00:00 UTC` no fuso da carga, SEM lag (paridade do oraculo).
  Unico lugar com essa aritmetica: `prevcarga_freshest_issue`, `prevcarga_read_forecast_vintages`, `prevcarga_resolve_cuts` (dates.R) e os paineis de treino/infer herdam o limite pronto.
  `prevcarga_weather_effective_lag(rule, lag)` = lag efetivo para proveniencia (o do deck em `available_until`; 0 em `origin_midnight_utc`).
- **Lag de publicacao do clima previsto (Gabriel 2026-10-30, ADR-0024 atualizado):** uma emissao so esta DISPONIVEL `lag` horas depois do `issue_time`.
  Chave de topo `weather_forecast_publication_lag_hours` no `manifest.json` (como o fuso, ADR-0037): inteiro >= 0, OBRIGATORIA quando o deck tem
  `weather_forecast` (dataset ou fuso declarado) => ausente `contract.weather_lag_missing`, invalida (negativa, nao inteira, texto, vetor) `contract.manifest_invalid`
  (`where = manifest.json:weather_forecast_publication_lag_hours`); sem default nem variavel de ambiente. Store: 6 h (uma emissao/dia, 00Z): a rodada das 00h de O
  (`available_until = O 00:00 local = O 03Z`, limite O-1 21Z) usa a 00Z de O-1; a partir de O 03:00 local (06Z, fronteira inclusiva) usa a 00Z de O. Decks sinteticos
  (`fixture_store.R` `weather_lag_hours`, `helper-deck.R` `pc_deck_manifest(..., weather_lag_hours)`) declaram 0 por default. Proveniencia: attr
  `weather_publication_lag_hours` no previsto e no painel, `prevcarga_resolve_cuts()$weather_publication_lag_hours` (e nos `cuts` de treino/infer), linha
  `weather_forecast_publication_lag_hours` no `data_hash` (=> data_fingerprint da chave de cache do M7 e `data_hash` do run-manifest), `feature_config` do
  `run_id` do M7 e `run.json` da rodada diaria. Testes: `test-deck-reader.R` (manifest) e `test-deck-reader-cut.R` (00h, 02:30, 03:00, origin_midnight_utc, FORBIDDEN/CONTROL).
  Regra vem de `ctx$weather_issue_cutoff` (fonte `ctx$sources$weather_issue_cutoff`, default `default` se ausente). `prevcarga_freshest_issue(fc, limit)`: emissao mais fresca <= limite por `(area, valid_time)` (fix/facade-channels: os oraculos HW/dessem-linear/regdin/LGBM
  combinam por valid_time; a 06Z comeca as 03:00 e o inicio do dia O vem da 00Z). `prevcarga_read_forecast_vintages(ctx, areas, from, to, limit)`: TODAS as emissoes
  `<= limit` em `[from, to]` (insumo dos plugins com `weather_forecast_in_train`, no treino e no infer; usado por `training.R` e `inference.R`).
- `prevcarga_read_weather(..., origin, truncate_at)`: o observado sai com `valid_time` (F4: a coluna crua `timestamp` do store e renomeada na borda de entrada, logo
  apos o fuso; chave `(area_code, valid_time)`; nenhum consumidor ve `timestamp`); observado `<= available_until`; previsto so a emissao mais fresca permitida por `(area, valid_time)` (escolhida antes do recorte de `valid_time`);
  attrs `weather_issue_cutoff`, `weather_issue_cutoff_source`, `weather_issue_limit`, `weather_publication_lag_hours`, `freshest_issue_by_area` (a maior emissao usada por area). `available_until` vem da carga lida pelo proprio leitor.
- Catalogo: `scripts/lib/errors/deck_cut.tsv`. Testes: `test-deck-reader-cut.R`; helper ganhou `pc_deck_until`, `pc_deck_drop`, `pc_deck_perturb`, `pc_deck_issues`.

## M2.7 (parte 3): feriados e painel do leitor
- `prevcarga_read_holidays(ctx, areas, from = NULL, to = NULL)` (FIX 6: `from`/`to` NULL => todos os feriados da area, todas as particoes): `auxiliary/holidays` (Hive `area_code=/year=`), colunas `area_code, date, holiday_type_id, prevcarga_code, simplified_code`;
  `date` = data civil (`Date`) sem deslocamento e SEM corte de `available_until` (calendario conhecido de antemao); particao `year` exata; nao filtra codigos
  (`simplified_code = -1` sai como esta; o finding `code_negative` e do validate-deck, M2.4). Chaveado `(area_code, date)`.
- `prevcarga_read_panel(ctx, origin, areas, horizon_days, history_days)`: origem ja resolvida (o leitor nao depende de `dates.R`); valida `areas` contra
  `prevcarga_topology(path)$leaves` (`ctx$topology_path` ou `<ctx$root>/config/topology.yaml`) antes de ler => `topology.area_undeclared` (exit 4, `where` = a area).
  Devolve `load` (O-hist..O, cortada), `weather_observed` (O-hist..O, <= available_until), `weather_forecast` (O..O+horizon, emissao mais fresca permitida por valid_time), `holidays`
  (FIX 6: o calendario INTEIRO da area, sem janela, como o `.read_holidays` do oraculo; a janela antiga perdia o feriado anterior ao inicio do historico e
  o `days_since_last_holiday` do RF divergia, ex. RS 2023-09-20 em rs-2024-10-01) e `attrs` (`origin`, `available_until`, `available_until_by_area`, `load_label`, `load_label_source`, `tz`, `weather_issue_cutoff`, `weather_issue_limit`, `weather_publication_lag_hours`).
  `pc_panel_attrs` copia os atributos sem recalcular e aborta `contract.constraint_violated` se `tz`/`available_until` divergirem. O mapeamento area -> serie de clima e do plugin.
- O limite inferior do filtro de particao `year` passou de `year(from)-1` para `year(from - 1 dia)` (nenhum ano fora da janela e coletado; o recorte fino segue em memoria).
- Catalogo: `topology.area_undeclared` mora em `scripts/lib/errors/topology.tsv` (o `panel.tsv` foi removido). Testes: `test-read-panel.R`; helper ganhou `pc_deck_panel`, `pc_ctx_panel`, `pc_deck_topology`, `pc_deck_holidays_default`
  e `pc_deck_perturb` cobre `holidays` (+1 em `prevcarga_code`).

## manifest.json do store atual (M2.6)
- Template versionado `config/deck-manifest.store.json` (schema v1, `load_label: start`, `Etc/GMT+3`; `issue_time` `UTC`; `weather_forecast_publication_lag_hours: 6`; sem `corte`). O rotulo esta confirmado (a fonte rotula a carga pelo inicio; o conversor grava o carimbo + 30 min e declara `load_label: end`): o deck novo gerado pelo conversor (ex.: 20261104) tem `manifest.json` com `end`, `Etc/GMT+3`, `issue_time` UTC e atraso 6. O template versionado do store segue `start` e o store real em `data/` segue sem manifest. Falta so a ressalva do fuso: o store esta em relogio de parede BRT com horario de verao ate 2019 e `Etc/GMT+3` erra o `issue_time` em 1 h no verao 2018-19 (opcao de correcao do fuso pendente).
- `scripts/tools/write_store_manifest.R --target <copia>` (dev, fora do pacote) grava o template numa copia/overlay do store: valida pelo leitor, identico => no-op, diferente => `contract.manifest_exists_different` (exit 2; catalogo em `scripts/lib/errors/store_manifest.tsv`). O produto nunca escreve no `data_dir`. Nada foi gravado no store real (`data/`).
- Smoke `validate-deck` pendente (depende do verbo M2.5, nao integrado): rodar sobre a copia e registrar no PR.

## Cobertura do clima previsto no painel (F3.7)
- `prevcarga_read_panel` calcula, por area e sobre o previsto ja filtrado em `[O, O+horizon]`, `attrs$weather_forecast_until` (`data.table(area_code, until Date|NA)`: ultimo dia
  coberto em sequencia a partir de O, 48 instantes, predicado `prevcarga_forecast_coverage_end` de validation.R) e `attrs$weather_forecast_next` (`data.table(area_code, n_next)`:
  instantes consecutivos de until+1 a partir de 00:00, para o membro com offset). Toda conta de data fica no leitor; `pc_panel_attrs(load, obs, fc, origin = NULL)` so as calcula com `origin`.
- Depende de `validation.R` carregado (os chamadores de `prevcarga_read_panel` ja o carregam; `test-read-panel.R` passou a carrega-lo).

## E1-T1: kind, bloco inference e guarda unica do deck (regdin-linear-refactor)
- Manifest ganha `kind` (`train` | `inference`; ausente = `train`) e o bloco `inference` (`origin` AAAA-MM-DD, `available_until` AAAA-MM-DDTHH:MM:SS no fuso da carga,
  `train_deck_hash` sha256 64 hex minusculos, `windows` mapa dataset -> `{from, to}`, `from <= to`, vazio aceito). `prevcarga_deck_manifest` devolve `kind` e `inference`
  (NULL no train; `origin` Date, `available_until` POSIXct na tz da carga). Valor fora do enum, bloco ausente no `inference`, bloco no `train`, campo mal formado =>
  `contract.manifest_invalid` (`manifest.json:kind|inference|inference.<campo>`); chave estranha em `inference` => `contract.manifest_unknown_key` (`inference.<chave>`).
- `prevcarga_require_deck(ctx)`: `prevcarga_require_data_dir` + manifest + `contract.deck_kind_mismatch` (`where` = `ctx$data_role_option` ou `--data-train|--data-inference`;
  `details` kind/expected; `ctx$data_role` NULL = train ate o E1-T3) + `contract.deck_origin_mismatch` (deck inference com `ctx$origin`; sem `--origin` nao confere).
  Devolve `invisible(list(ctx, manifest))`; nenhuma leitura de dataset. Usada nos handlers forecast, train-models, backtest, plan, score, combine, reconcile,
  infer-model, features e em `prevcarga_validate_deck`; forecast.R (resolucao do manifest), orchestrate.R e as guardas internas do leitor seguem com `prevcarga_require_data_dir`.
- `.prevcarga_manifest_tz(tz, where, olson = OlsonNames)`: `olson()` vazio => `runtime.tzdata_missing` (exit 7), no lugar de `contract.tz_invalid`; nome fora da lista segue `contract.tz_invalid`.
- `prevcarga_deck_manifest_timezones(data_dir)`: fusos declarados (unicos), lidos do JSON cru sem validar; consumido pelo doctor (E1-T6).
- Catalogo: `scripts/lib/errors/deck_contract.tsv`. Testes: `scripts/lib/tests/test-deck-kind.R`.

## E2-T2: previsto lido pela regra que o plugin declara (regdin-linear-refactor)
- `prevcarga_read_forecast_by_rule(ctx, area, origin, segments, limit)`: le o previsto da area uma unica vez (`prevcarga_read_weather` kind forecast, de `O + min(from)` em
  diante), filtra `issue_time <= limit` e despacha cada segmento `{kind, from, to, lead}` ao seletor da sua regra; une, deduplica (`unique`) e ordena por
  `(area_code, issue_time, valid_time)` com `method = "radix"`. Nenhuma emissao `> limit` sai (honestidade, ADR-0024). Nenhum nome de modelo: so `kind`.
  `kind` desconhecido => `contract.inference_needs_invalid`. Dias por lubridate no tz do `valid_time` do store; o dia da emissao (`issue_day`) e sempre UTC.
- Segmentos vazios (`list()`) => `data.table` de 0 linhas com as colunas e os tipos do store (area_code, issue_time, valid_time, temperature, heat_index).
- Regras (seletores puros `.prevcarga_rule_*`), cada uma reproduz a escolha do plugin que a originou:
  - `freshest` (dessem-linear, dessem-regdin, default generico): por `(area, valid_time)` com dia valido em `[O+from, O+to]`, a mais fresca `<= limit` (`prevcarga_freshest_issue`).
  - `per_origin` (random-forest, `.rf_weather_vintage`): para cada origem nativa `T` em `[O+from, O+to]`, `limit(T) = limit - days(O - T)`; a emissao mais fresca em
    `(limit(T) - 1 dia, limit(T)]` e entregue INTEIRA (todas as suas linhas). Origem sem candidata nao entrega nada (o LOCF segue no plugin).
  - `min_lead` (lgbm, `lgbm_weather_vintage`): por `valid_time` com `valid_day` (tz da carga) em `[O+from, O+to]` e `valid_day - issue_day(UTC) >= lead`, a mais fresca.
- Delta 0: cada plugin escolhe a mais fresca que satisfaz a SUA regra entre as emissoes que recebe; as linhas selecionadas sao exatamente as que ele escolheria sobre todas
  as emissoes, entao entregar so elas preserva o LONG por membro. `weather_forecast_in_train` segue no descriptor e nas chaves de cache/backtest.
- Quem declara: `data_needs(spec, area)` do plugin (`infer$weather_forecast` e `train_weather_forecast`, `from = NA` = desde o inicio da janela de treino, trocado por `-span`
  por `prevcarga_inference_needs_train_rule`, inference_needs.R). LGBM: `min_lead [-n, 0] lead 0` e `[+1, +1] lead 1` na area `heat_area[area]`; RF: `per_origin [-n, 0]`
  e feriados `[-(no_holiday_days + 1), +H]`; dessem-linear e dessem-regdin: `freshest [0, +H]` (E3/E4 trocam depois); HW: sem `data_needs`, o default generico da previsto vazio.
  Plugin com `weather_forecast_in_train` e sem `data_needs` segue lendo todas as emissoes `<= limit` no treino (`prevcarga_read_forecast_vintages`).
- Consumidores: `prevcarga_infer_read_panel(..., needs)` (inference.R; troca o previsto do painel quando `needs$weather_forecast$segments` nao e vazio; `prevcarga_infer_cell`
  passa `art$inference_needs`, caindo no `prevcarga_inference_needs_of` num artefato anterior ao campo), `prevcarga_train_inputs` e `.prevcarga_tr_panel_memo` (training.R; a chave do memo
  ganha a assinatura da regra). Testes: `scripts/lib/tests/test-forecast-by-rule.R`.

## E3-T2: leitor do anexo de feriados e holiday_window no painel (regdin-linear-refactor; ADR-046)
- `prevcarga_read_holiday_annex(ctx, origin, areas, window, truncate_at = NULL)` (`deck_reader.R`; requer `holiday_islands.R` carregado antes): devolve `list(islands, load, weather_observed)`. `window` NULL ou sem `holiday_from` (modo `referencia`) => `NULL`.
  - `islands` no schema de `prevcarga_holiday_islands` (`area_code, island_id, from, to, holiday_dates`); `load` e `weather_observed` nos schemas e atributos (`tz`, `load_label`, `load_label_source`, `available_until`, `origin`) dos leitores atuais, com chave preservada.
  - Deck `train` (manifest sem `kind` ou `train`): le `auxiliary/holidays` em `[holiday_from, O-1]`, calcula as ilhas por area com `first_day` = primeiro dia do store da area (so a particao `year` mais antiga) e le carga e observado por `prevcarga_read_load`/`prevcarga_read_weather("observed")` com `origin = O` em `[min(from), max(to)]`.
  - Deck `inference`: le ilhas, carga e observado do subdeck apontado por `manifest$holiday_annex` (campo opcional do bloco `inference`; caminho relativo ao deck, sem `..` nem caminho absoluto, senao `contract.manifest_invalid`). O subdeck tem `manifest.json`, `load/`, `weather/observed/` e `islands/part-0.parquet` (uma linha por data de feriado: `area_code, island_id, from, to, holiday_date`). Sem `holiday_annex` com janela `ano_civil` => `contract.constraint_violated` (`where = holiday_annex`). Os atributos `available_until`/`origin` vem do manifest do deck de inferencia e de `O`.
  - Nos dois ramos o filtro final deixa so dias contidos em alguma ilha e NENHUM dia `>= O` (corte em `O-1`, FORBIDDEN); ilhas com `from >= O` somem e `to` e limitado a `O-1`.
- `prevcarga_read_panel(..., truncate_at = NULL, holiday_window = NULL)`: so com `holiday_window` e anexo nao `NULL` o painel ganha `holiday_annex` (depois de `holidays`, antes de `attrs`); sem o argumento, ou com janela `referencia`, o objeto e `identical()` ao de antes. `truncate_at` vai igual ao leitor do anexo.
- Testes: `tests/test-holiday-annex-reader.R` (deck sintetico train e inference, `identical()` do painel sem janela).
