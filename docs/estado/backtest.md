# Estado: backtest (M7.3) -- MAP por celula

Codigo: `scripts/lib/backtest.R`; catalogo `scripts/lib/errors/backtest.tsv`; testes `scripts/lib/tests/test-backtest.R` (toys das duas classes + deck do fixture_store).
O REDUCE (combina, pesos, reconcile, scores, relatorio) e do M7.7; `dplus`/`baseline`/`report*` sao aceitos por `prevcarga_backtest()` e nao consumidos pelo MAP.

- `prevcarga_backtest(ctx, origins, models, areas, cortes, protocol, ...)`: resolve `cortes` (flag > `ctx$backtest$corte_default` > 08:00), monta `prevcarga_plan()`
  (protocolo incompativel aborta ali, exit 2 `contract.protocol_incompatible`, antes de criar qualquer arquivo) e despacha `prevcarga_orchestrate()` com `.bt_make_run_cell()`.
  Devolve `list(run_id, status, summary, paths)`; `paths$members` = tabela (membro, folha, origem, corte, long_parquet). O verbo `backtest` liga o MAP real por
  `prevcarga_backtest_cell_runner(ctx, plan)` (gancho `.m7_run_cell_for(ctx, plan)` em orchestrate.R; o plano agora carrega `ident`).
- Truncagem: `.bt_cut_instant(tz, O, HH:MM)` (lubridate, fuso do manifest) vai como `truncate_at` ao leitor unico; o `available_until` nasce no leitor. Nenhum derivador aqui.
- Cortes de treino: nunca calculados aqui; a linha do plano (dates.R + `data_lag_days` do descriptor) manda e o treino confere (`contract.cut_mismatch` se divergir).
  `fixed` treina na menor origem (1x), `daily`/`retrain_harness` por origem. `retrain_harness` = `prevcarga_plugin_forecast` (so origin_scoped), painel com a historia do treino.
- Artefato: chave `artifact` do cache.R (folha e spec efetivo em `feature_config`; available_until nunca entra), so em `<state_dir>/cache/artifact/`; treino = `prevcarga_train_inputs` +
  `prevcarga_plugin_train` + `prevcarga_artifact_build` SEM publicar no registry (a fachada `prevcarga_train` sempre publica, por isso nao e usada). Infer = `prevcarga_infer_cell(artifact=, truncate_at=)`.
- LONG da celula: `prevcarga_build_long` (8 colunas, 1a entrega `available_until + 30`) + `prevcarga_schema_long_check` (so nomes/ordem) + `.bt_check_types` (forecast_origin texto) + NA em y_pred
  aborta `contract.backtest_long_na`. `run_cell` devolve o LONG; o orquestrador publica `long.parquet` + `.key` (chave esperada do plano). `member_long` NAO e cacheado: a chave do cache
  nao tem folha na identidade (repete entre folhas do mesmo membro/origem/corte) e colidiria.
- Alteracoes fora do backtest.R: `inference.R` (`prevcarga_infer_cell` aceita `artifact` e `truncate_at`; `prevcarga_infer_read_panel` aceita `truncate_at`, ambos opcionais) e `orchestrate.R`.
- M7.8 (clima previsto ponta a ponta): o M7 so repassa `ctx$weather_issue_cutoff` ao leitor e registra o limite (nenhum filtro de `issue_time`/`freshest` em backtest.R/cache.R; grep em `test-backtest-weather.R`).
  Chave do artefato (`.bt_key_args`): regra + `cell$train_weather_issue_limit` (limite do TREINO, vindo do dates.R no plano) + `weather_forecast_in_train` do descriptor. Chave `member_long` do plano: regra + limite da
  celula (`prevcarga_weather_issue_limit` do leitor sobre o corte simulado e o lag do manifest); regra diferente => `cache_key`/`.key` e `run_id` diferentes (celula `done` de outra regra recomputa no `--resume`).
  `prevcarga_backtest()$summary$weather_issue_cutoff`, `plan --format json` e o `result` do backtest expoem a regra efetiva (flag > config.yaml > default). Desvio: o ticket cita `spec$inputs$weather_forecast$train`;
  o run.yaml nao tem esse campo, a fonte e o descriptor `weather_forecast_in_train`. Testes: `test-backtest-weather.R` (FORBIDDEN/CONTROL por regra, lag, serial x `--jobs 2`, molde honesty_gate_weather), `test-cache.R`.
