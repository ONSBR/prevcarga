# Estado: orchestrate (M7.1) -- plan/backtest, ledger local, lock

Codigo: `scripts/lib/orchestrate.R`; verbos `scripts/lib/cli/plan.R` e `cli/backtest.R`; catalogo `scripts/lib/errors/orchestrate.tsv`;
testes `scripts/lib/tests/test-orchestrate.R` (+ fixture `tests/fixtures/m7/config.yaml`). Depende de topology/dates/provenance/cache/training (seletores).

- **Origens**: o dia (daily) e UMA origem; o backtest roda varias origens EM SEQUENCIA (despacho da mais recente para tras), uma treina+infere por origem;
  nada e carregado entre origens. `--origin A..B` so em backtest/plan (config.R: `prevcarga_parse_origin_range`; outros verbos => config.bad_origin, exit 8).
- **Matriz** `prevcarga_plan(ctx, origins, cortes, members, folhas, protocol, filters)`: produto membro x folha x origem x corte, tudo de topology.R
  (`--model`/`--area` pelos seletores do training.R; fora da topologia => exit 4/8). Nenhuma lista de modelos nem literal de contagem.
  Colunas: membro, mdir, exogena, folha, origem, corte, split, protocol, granularity_min, data_lag_days, train_origin, last_data_day, available_until, cache_key.
  Cortes de treino vem do `prevcarga_resolve_cuts` (dates.R) com o `data_lag_days` do descriptor; protocolo `fixed` usa o corte da 1a origem da janela.
  `available_until` = origem + `--corte` no fuso do deck; `cache_key` = `prevcarga_cache_key("member_long")` (M7.2) com `ctx$data_hash`/`code_commit`/`renv_lock_hash`
  injetaveis (ausentes => calculados; `prevcarga_data_hash` le o deck inteiro -- injete em lote). member_long nao tem folha na identidade: a chave se repete entre as folhas do mesmo membro/origem/corte.
- **Classe x protocolo** so em `.m7_class(spec, protocol)`: fixed so origin_agnostic, retrain_harness so origin_scoped, daily nas duas (contract.protocol_incompatible / run_yaml_protocol / run_yaml_split).
- **run_id** = `prevcarga_version_hash` sobre {janela, cortes, membros(sigla, mdir, exogena, split, protocolo), folhas, load_label, weather_issue_cutoff, tz, code_version, renv}; flags operacionais ficam fora.
- **Ledger por presenca** em `<state>/backtest/<run_id>/`: `plan.json` (atomico), `members/<sigla>/<FOLHA>/<AAAA-MM-DD>/<HHMM>/{long.parquet,.key}`, `ledger/failures.jsonl` (so o pai escreve; `ts` = prefixo de data do `ctx$run_id`, sem relogio).
  `done` <=> long.parquet + `.key` == cache_key esperada (a chave e publicada por ultimo); `*.tmp` ignorado. `run_cell(cell)` devolve a tabela LONG; o orquestrador publica.
- **Politica de falha**: fail-fast (restantes `skipped`) ou `--keep-going`; veredito ok|partial|failed; partial/failed => `partial.backtest_delivery` (exit 14) com o resumo no `result` do envelope e linhas `falha ...` em stderr (texto).
- **Lock**: `run.lock/` por `dir.create` exclusivo + `owner.json {pid, host}`; ocupado => `state.lock_held` (exit 13, antes de tocar plano/ledger); `--break-lock` retoma (aviso `warning.lock_broken`). Liberado no `on.exit`.
- **Pendente (outros tickets)**: o runner de celula real (`prevcarga_backtest_cell_runner(ctx)`, M7.3: treino via `prevcarga_train` + infer); sem ele toda celula falha com `todo.not_implemented`. `--jobs > 1` (PSOCK) e do M7.6. `--report`/`--report-format`/`--dplus`/`--baseline` sao aceitos e consumidos pelo M7.3+.
- **cli.R** (extensao minima): campo opcional `scoped` no registro do verbo (flags validas so nos verbos que as declaram; fora => config.usage, exit 8); `plan`/`backtest` saem de `prevcarga_verbs_planned`.
- **config**: `backtest.corte_default` valida `HH:00|HH:30`, default `08:00`.
