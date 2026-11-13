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
- **run_id** = `prevcarga_version_hash` sobre {janela, cortes, membros(sigla, mdir, exogena, split, protocolo), folhas, load_label, weather_issue_cutoff, tz, code_version, renv}; flags operacionais ficam fora. M7.8: o plano carrega `weather_issue_cutoff` (tambem em `plan.json` e no `result` de `plan`/`backtest`), `weather_issue_limit` e `weather_forecast_in_train` por celula e `train_weather_issue_limit` (dates.R) por (mdir, origem de corte); o `cache_key` member_long inclui regra + limite.
- **Ledger por presenca** em `<state>/backtest/<run_id>/`: `plan.json` (atomico), `members/<sigla>/<FOLHA>/<AAAA-MM-DD>/<HHMM>/{long.parquet,.key}`, `ledger/failures.jsonl` (so o pai escreve; `ts` = prefixo de data do `ctx$run_id`, sem relogio).
  `done` <=> long.parquet + `.key` == cache_key esperada (a chave e publicada por ultimo); `*.tmp` ignorado. `run_cell(cell)` devolve a tabela LONG; o orquestrador publica.
- **Politica de falha**: fail-fast (restantes `skipped`) ou `--keep-going`; veredito ok|partial|failed; partial/failed => `partial.backtest_delivery` (exit 14) com o resumo no `result` do envelope e linhas `falha ...` em stderr (texto).
- **Lock**: `run.lock/` por `dir.create` exclusivo + `owner.json {pid, host}`; ocupado => `state.lock_held` (exit 13, antes de tocar plano/ledger); `--break-lock` retoma (aviso `warning.lock_broken`). Liberado no `on.exit`.
- O runner de celula (`prevcarga_backtest_cell_runner`) e o REDUCE (`prevcarga_backtest_reduce`) moram em `scripts/lib/backtest.R` (M7.3, M7.7; ver `backtest.md`); o fallback `todo.not_implemented` so vale se `backtest.R` nao estiver carregado, e o verbo `backtest` o carrega. `--report`/`--report-format`/`--baseline` sao consumidos pelo REDUCE; `--dplus` so registra `dplus = TRUE` (ver as pendencias em `backtest.md`).
- **cli.R** (extensao minima): campo opcional `scoped` no registro do verbo (flags validas so nos verbos que as declaram; fora => config.usage, exit 8); `plan`/`backtest` saem de `prevcarga_verbs_planned`.
- **config**: `backtest.corte_default` valida `HH:00|HH:30`, default `08:00`.
- **`--jobs N` (M7.6, PSOCK)**: `.m7_dispatch()` e serial quando `jobs == 1` (nenhum cluster) e `.m7_dispatch_psock()` caso contrario; so `parallel::makePSOCKcluster`
  (sem fork; Linux e Windows nativo, ADR-0038), fechado em `on.exit`. Cada worker roda `prevcarga_worker_init` (training.R: `.m7_worker_modules` do MESMO `prevcarga.lib`/`ctx$lib_dir`,
  depois `prevcarga_runtime_enforce()` in-process, sem env). Init que falha => toda celula vira `failed` com a classe/id originais (`runtime.*`), nada roda.
  Celula = `.m7_cell_exec()` (serial e worker, o mesmo codigo): `.m7_seed_apply(seed do run.yaml)` imediatamente antes de `run_cell`, publica so no diretorio da propria celula e devolve
  um envelope pequeno (state/error_class/error_id/mensagem; o LONG fica em disco). Lotes do tamanho do cluster (`clusterApplyLB`); o pai recolhe na ordem de despacho, escreve
  `failures.jsonl` (so ele) e, sem `--keep-going`, nao despacha lote novo apos a 1a falha (resto `skipped`). `status`/resumo na ordem de `plan$cells`: byte-identicos entre jobs.
  A semente tambem e aplicada no caminho serial (e o que garante `--jobs 1 == --jobs N`). Hooks de teste: `ctx$worker_hooks` (pre/post_runtime), como no train-models.
  Testes: `scripts/lib/tests/test-orchestrate-psock.R` (script-filho com shell sujo/limpo; CONTROL sem semente no worker).
- **Cortes do plan por origem distinta (F2.4, fix v0.1.4)**: `prevcarga_plan` chama `prevcarga_resolve_cuts` 1x por `cut_origin` distinto (com todos os mdirs daquela origem e o `data_lag_days` nomeado do descriptor),
  em vez de 1x por (mdir, cut_origin): `prevcarga_read_load` roda 1x por origem. O `weather_issue_limit` da celula e calculado 1x por par (origem, corte_hm) distinto (leitor, deck_reader.R; o M7 nao faz conta de hora)
  e juntado as celulas. Celulas (train_origin, last_data_day, limites, cache_key) identicas ao caminho por celula. Testes: `test-orchestrate.R` ([F2.4]).
- **Progresso do backtest (F2.5, fix v0.1.4)**: sem `ctx$progress`, cada celula consolidada escreve em stderr
  `backtest: <done>/<total> falhas=<n> eta=<dur|?> ultima=<membro> <folha> <origem> <corte>` (`.m7_progress_line`, funcao pura, ASCII).
  `eta = elapsed / processed x (total - done)`, so sobre as celulas DESTA execucao (`processed`; o `done0` do `--resume` nao conta); `eta=?` com `processed = 0`.
  Com `--keep-going`, cada falha sai na hora em `falha <membro> <folha> <origem> <corte> <error_id>: <mensagem>`, antes da linha de progresso da propria celula.
  O relogio e so de apresentacao: `.m7_now(ctx)` (`ctx$clock` injetavel nos testes, senao `Sys.time()`) e a unica leitura de relogio do M7 (excecao fixada por linha exata em
  `test-m7-static.R`; a isca continua disparando a regra `clock`); nunca entra em chave, ledger, `plan.json` ou result. `ctx$progress(done, total)` mantem a assinatura e,
  quando definido, nada e escrito em stderr pelo progresso. Testes: `test-orchestrate.R` ([F2.5]).
