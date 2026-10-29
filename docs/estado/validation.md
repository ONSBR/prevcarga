# Estado: validation (M2.3 predicado + M2.4 `prevcarga_validate_deck`; ADR-0040)

- `scripts/lib/validation.R`: validacao model-agnostica do deck, ANTES de qualquer compute, com veredito ACUMULATIVO (nunca aborta no 1o defeito).
  Requer `errors.R`, `topology.R`, `config.R`, `deck_reader.R`, `dates.R` carregados. Somente leitura sobre `data_dir`; nao devolve exit (o handler do M1 mapeia classe -> exit).
- `prevcarga_validate_deck(ctx, origin = NULL, scope = NULL, data_lag_days = NULL)` devolve
  `{ok, class, forecast_origin, origin_source, available_until, available_until_by_area, load_label, load_label_source, weather_issue_cutoff, weather_issue_limit, timezone, cuts, scope, findings}`.
  `scope = list(model, area)`; `scope$area` subsistema expande para folhas + ele mesmo; `scope` do veredito devolve as areas/modelos efetivamente checados.
  Campos que dependem de dado/manifest reprovado vem `NULL` (nunca um default de fuso; `available_until` nunca assume o inicio do buraco).
- `findings`: `data.table(area, dataset, what, detail, class, level, where)`; `what` = sufixo do `error.id`; `where` relativo ao `data_dir` (ou `<dataset>.<coluna>`).
  `ok`/`class` so consideram `level == "error"`; classe dominante = `prevcarga_exit_precedence()` do errors.R (sem vetor de precedencia no modulo).
- Ordem: sonda/contrato (`.prevcarga_deck_spec()` + `prevcarga_deck_probe`) -> manifest/rotulo (capturados) -> escopo/topologia -> `prevcarga_resolve_cuts` (aborts do leitor/dates viram finding;
  `warning.available_until_spread` vira warning) -> leitura enxuta do intervalo relevante -> completude -> sanidade -> janelas -> avisos; feriados (`prevcarga_read_holiday_codes`, sem manifest) independem de fuso.
  Dataset reprovado no contrato sai das checagens seguintes (sem cascata). Erro de uso (classe `config`/`runtime`) nao e capturado.
- Intervalo relevante: `[min sobre os membros do escopo de (last_data_day - train + 1, O-1 - infer + 1), O-1]` (tabela `cuts` do M2.3 + `history_days` do run.yaml; nenhum literal de janela).
  `day_incomplete` (!= 48 por dia de `interval_start`, `prevcarga_day_complete`) conta do primeiro dia presente de cada area em diante: dias anteriores ao inicio do deck sao deficit de JANELA, nao buraco.
- Classes: contract (`schema_missing`, `type_mismatch`, `tz_in_parquet`, `hive_layout`, `parquet_unreadable`, `dataset_missing`, `grid_off`, `duplicate_interval`, `load_nonpositive`,
  `temperature_out_of_range` -10..50 C, `code_negative`, manifest/fuso/`load_label_conflict`), completude (`day_incomplete`, `origin_day_gap`, `no_load`), topology (`area_undeclared`, `scope_undeclared`),
  window (`window_train_deficit` ate `last_data_day` do membro; `window_infer_deficit` ate O-1; um finding por membro, area e tipo). Avisos (level warning, nao reprovam): `outlier` (IQR 1.5x e |z| > 3), `flatline` (>= 6 identicos),
  `climate_after_cut` (emissao do dia O apos `weather_issue_limit`, nas duas regras), `available_until_spread`.
- Premissas: a referencia nao preenche com zero (carga NA/0 reprova); completude flat-48; sem faixa de MW por subsistema; sem monotonicidade.
- Catalogo: `scripts/lib/errors/validation.tsv` (15 ids proprios; os demais vem de `deck_reader.tsv`, `dates.tsv`, `deck_cut.tsv`, `panel.tsv`).
- `deck_reader.R` ganhou `prevcarga_read_holiday_codes(ctx, areas)` (codigos crus, sem manifest/origem) para o `code_negative` seguir acumulando sem manifest.
- Limite conhecido: a sonda do leitor (M2.1) so le o schema do PRIMEIRO parquet legivel por dataset; coluna ausente apenas em outras particoes nao e vista pelo contrato.
- Testes: `scripts/lib/tests/test-validation.R` (pares limpo/mutacao, md5 do data_dir antes/depois em toda chamada); `helper-deck.R` ganhou `pc_mut_load|drop_col|retype|flatten|corrupt|holiday|add_area|issue|weather`, `pc_deck_days`.
