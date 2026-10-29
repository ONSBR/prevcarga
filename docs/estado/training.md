# Estado: training (M5.3) -- fachada `prevcarga_train`

Codigo: `scripts/lib/training.R`; catalogo `scripts/lib/errors/training.tsv`; spec efetivo em `topology.R`; testes `scripts/lib/tests/test-training.R`
(plugins-brinquedo toy-30/toy-60 sobre o deck de `fixture_store.R`; os modelos reais ainda nao tem todas as etapas).

- `prevcarga_model_spec(model, root, overrides = list())`: run.yaml + overrides pontilhados (`horizon_days`, `params.<k>`) so sobre chaves DECLARADAS.
  Chave inexistente => `config.override_unknown` (exit 8); tipo diferente do declarado ou `horizon_days` fora de inteiro >= 1 =>
  `config.override_bad_value` (exit 8). Constraints reaplicadas apos o override. Lista devolvida ordenada por nome.
- `prevcarga_train_inputs(member, area, origin, ctx)`: plugin (`prevcarga_plugin_load(mdir, <root>/models)` + `prevcarga_plugin_check_signature`), spec efetivo
  (`ctx$spec_overrides` + variante do membro na chave `descriptor$variant_param`), cortes por `prevcarga_resolve_cuts(ctx, origin, model, data_lag_days = descriptor)` e
  `prevcarga_train_cut(spec EFETIVO, ...)` (o `resolve_cuts` le o run.yaml sem overrides; so o `train_origin` com H efetivo vem do `prevcarga_train_cut`). Linha de corte com
  `data_lag_days` != descriptor => `contract.cut_mismatch` (exit 2). O M5 nao soma/subtrai dias de corte (teste varre o corpo de `training.R`).
- Painel: `prevcarga_read_panel` com janela `history_days.train + (O - last_data_day)`; carga e clima observado cortados em `last_data_day` e limitados a
  `history_days.train` dias; `weather_observed$timestamp` renomeado para `valid_time` (adaptador do defeito de integracao do P.0i: o leitor entrega `timestamp`, o
  despachante e `to_hourly` exigem `valid_time`); plugin com `weather_forecast_in_train` recebe todas as emissoes `<= weather_issue_limit` (nao so a mais fresca).
  Dias distintos de carga < `history_days.train` => `window.history_short` (exit 5; `details`: `available_days`, `required_days`, `member`), antes de qualquer escrita.
  Granularidade 60 => `to_hourly`. O mapeamento area -> serie de clima (ex.: LGBM SECO -> SP) e do plugin: o painel traz o clima da propria area.
- Identidade: `prevcarga_train_fingerprint` preenche as 8 chaves do contrato de `provenance.R`: `model_id` = `member$mdir`; `data_fingerprint` = sha256 do CONTEUDO do painel ja
  cortado (carga, clima observado/previsto, feriados; `attrs` e `available_until` fora) -- NAO e o `prevcarga_data_hash` (esse hasheia os arquivos do deck e mudaria com o dia O);
  `cutoff` = origin_scoped `{origin}` | origin_agnostic `{train_origin, last_data_day}`; `feature_config` = `{spec (name, split, granularity_min, horizon_days, history_days, seed, params),
  exogena}`; `code_commit`/`renv_lock_hash` de `ctx` (ausentes => calculados); `load_label` e `load_tz` efetivos. Laterais (`trained_at`, `run_id`, `version`, plataforma) so no run_manifest.
- Artefato publicado (classe `prevcarga_train_artifact`, dado puro): `model_id, variant, area, split, spec_efetivo, cuts{train_origin,last_data_day}, load_label, timezone,
  weather_issue_cutoff, fit` (= artefato carimbado do despachante, serve a `prevcarga_plugin_infer`), `contract`; `origin` so no origin_scoped; nunca `available_until`.
  `run_manifest.json` sem `available_until`, com `weather_issue_cutoff` efetivo e o `fingerprint` canonico.
- `prevcarga_train(member, area, origin, ctx, force)`: `v_<hash>` existente e `!force` => `status = "reused"` (plugin nao chamado, pasta intocada); senao
  `prevcarga_plugin_train` -> `prevcarga_registry_publish` -> `prevcarga_registry_promote_latest` (M5.2). Devolve 1 linha
  `(model, variant, area, split, train_origin, last_data_day, version, status, path)`.
- Gate `registry_lifecycle` (`test-static-greps.R`): `training.R` foi acrescentado ao allow-list porque so CHAMA `prevcarga_registry_promote_latest` (nao reimplementa o ciclo de vida).
- Pendencia F1 (despachante): os estagios continuam com formals fixos; esta fachada NAO escolheu canal para repassar spec/cortes aos estagios (params e seed ja chegam por `spec$params`/`spec$seed`).
