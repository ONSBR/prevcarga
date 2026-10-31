# Estado: M7 -- Orquestracao, cache honesto e backtest (fechamento do epico, M7.V)

Documento de estado do EPICO M7. O detalhe por modulo mora em `docs/estado/orchestrate.md` (M7.1/M7.6), `cache.md` (M7.2/M7.8) e `backtest.md`
(M7.3 MAP, M7.4 relatorio, M7.5 gates de harness dos plugins, M7.7 REDUCE, M7.8 clima); aqui ficam o mapa, o layout de `state_dir`, classes x protocolos,
a chave do cache, a tabela dos gates bash do oraculo e os gates de fechamento. `docs/ESTADO-ATUAL.md` e `scripts/lib/README.md` nao sao editados por este
ticket (indice mantido pelo M1.V); `docs/roadmap/` esta arquivado em `docs/_archive/roadmap/` (so leitura) e `.claude/` nao e tocado.

## Modulos vivos
| Modulo | Arquivo | Papel |
|---|---|---|
| orchestrate | `scripts/lib/orchestrate.R` | `prevcarga_plan` (matriz membro x folha x origem x corte), `prevcarga_orchestrate` (ledger por presenca, lock, fail-fast/`--keep-going`, `--jobs` PSOCK), verbo `backtest_run` |
| cache | `scripts/lib/cache.R` | store content-addressed local (`features|artifact|member_long|khat`), chave pelo `prevcarga_version_hash`, quarentena de entrada corrompida |
| backtest | `scripts/lib/backtest.R` | MAP por celula (truncagem pelo leitor, treino por classe, LONG 8 colunas) e REDUCE (combina honesta por mes, reconcile por origem, avaliacao com y_true fresco) |
| reporting (secao `.rpt_bt_`) | `scripts/lib/reporting.R` | relatorio do backtest HTML/PDF: so render sobre o painel avaliado |
| verbos | `scripts/lib/cli/plan.R`, `scripts/lib/cli/backtest.R` | registros auto-descobertos pelo `cli.R` (dono M7); a fachada `scripts/prevcarga` nao tem bloco por verbo |
| gates de harness dos plugins | `scripts/tools/gates/m7_harness.R` | substitui os slots `[DEFERRED M7]` dos `run_all_gates.R` sem editar nenhum runner (ver `backtest.md`) |

## Layout de `ctx$state_dir`
- `artifacts/<model>/<area>/...`: registry de PRODUCAO. Somente leitura no backtest: o MAP nunca o abre, o REDUCE nunca promove (greps + spy nos testes; sha256 de
  `cache/` e `backtest/` estavel ao `registry gc`).
- `cache/<kind>/<hh>/<key>/`: cache content-addressed (`object.rds|parquet` + `meta.json`); entrada com sha divergente vai para `cache/_quarantine/` com
  `warning.cache_corrupt_quarantined` e vira MISS. O artefato do backtest vive so aqui (treino sem publicar no registry).
- `backtest/<run_id>/`: `plan.json`, `members/<sigla>/<FOLHA>/<AAAA-MM-DD>/<HHMM>/{long.parquet,.key}`, `ledger/failures.jsonl` (so o pai escreve), `run.lock/`
  (liberado no `on.exit`), e do REDUCE `combined/<FOLHA>/<HHMM>/`, `weights/<AAAA-MM>/`, `reconciled/<AAAA-MM-DD>/<HHMM>/{SIN.parquet,subsistemas/}`, `scores/`, `report/`.

## Classe de split x protocolo
Decidido uma vez, em `.m7_class(spec, protocol)` (orchestrate.R); incompativel aborta no plano, antes de qualquer arquivo (`contract.protocol_incompatible`, exit 2).
| split | `fixed` | `daily` | `retrain_harness` |
|---|---|---|---|
| `origin_agnostic` | sim (1 treino na menor origem, K infer) | sim (1 treino por origem) | nao |
| `origin_scoped` | nao | sim (treino por origem + infer do artefato) | sim (forecast acoplado do plugin, sem artefato de cache) |

Cortes de treino nunca sao calculados no M7: a linha do plano (dates.R + `data_lag_days` do descriptor) manda (`train_origin`, `last_data_day`) e o treino confere
(`contract.cut_mismatch`). O `available_until` nasce no leitor unico a partir de `truncate_at = origem + --corte` no fuso do deck.

## Chave do cache
`prevcarga_cache_key(kind, ...)` = `prevcarga_version_hash` de {identidade da celula, `feature_config`, `cutoff` por classe}. O corte entra pela classe (ADR-0023):
`artifact` origin_agnostic leva `train_origin` e `last_data_day`, origin_scoped leva `train_origin`, e nenhum artefato leva `available_until` (o sweep de `--corte` nao
re-treina); `features` e `member_long` levam origem e `available_until`. O rotulo da carga (`load_label`) e a regra de clima (`weather_issue_cutoff` + limite de
emissao, M7.8) entram na chave onde o dado e lido; o `run_id` do backtest tambem. Regra de clima diferente => chave, `.key` e `run_id` diferentes (celula `done` de outra
regra recomputa no `--resume`). O cache nunca filtra emissao: so repassa a regra ao leitor.

## Gates bash do oraculo -> teste e epico dono
Referencia read-only: `jobs/tests/*.sh` de `/opt/ons-source/prevcargadessem3`. Os gates de orquestracao/harness foram reescritos em testthat portavel (Linux e Windows,
sem bash, sem Docker, sem S3) com FORBIDDEN delta 0 e CONTROL delta != 0; fixtures por formula em `tempdir()` (`helper-m7-fixtures.R`).
| script do oraculo | teste no pacote | epico dono |
|---|---|---|
| `determinism_gate.sh` | `scripts/lib/tests/test-harness-determinism.R` (2 rodadas e `--jobs 1` x `--jobs 4` PSOCK; `identical()` em membros, combinado e reconciliado) | M7 (M7.V) |
| `test_split_parity.sh`, `test_reduce_split_parity.sh` | `test-harness-split-parity.R` (fixed x daily; daily x retrain_harness; combina/reconcile train+apply x REDUCE) | M7 (M7.V) |
| `test_reduce_batch_parity.sh` | `test-harness-reduce-batch.R` (P1 invariancia causal, P2 lote == por-origem, CONTROL de mascara aberta) | M7 (M7.V) |
| `test_e2e_origin_cut.sh`, `test_prod_honesty_cut.sh`, `test_available_until.sh` | `test-harness-origin-cut.R` (spy `truncate_at`, `prevcarga_honesty_gate`, manha de O, SIN = soma) | M7 (M7.V) |
| `test_partial_upload.sh` (sem upload) | `test-harness-partial.R` (CLI real por `callr`: exit 14 `partial.backtest_delivery`, verdict partial/failed/ok) | M7 (M7.V) |
| `test_prevcarga_s3_canary.sh` | DESCARTADO: o pacote nao tem transporte S3 no M7 (nenhum caminho do backtest toca rede) | -- |
| `test_reconc_freeze_parity.sh` | `test-reconcile-*.R` (train/apply do k-hat) e o braco (c) do `test-harness-split-parity.R` | M5 (reconcile) / M7.V |
| `test_prevcarga_cli.sh`, `test_prevcarga_contract.sh`, `test_prevcarga_layout.sh`, `test_prevcarga_subset.sh`, `test_prevcarga_honesty.sh` | suites do forecast e2e / export / shell (`docs/estado/m6-forecast.md`) | M6 |
| `test_prevcarga_verificar.sh` | `docs/estado/verificar.md` | M8 (M8.7) |
| `equivalence_gate.sh`, `test_lock_pin_parity.sh`, `test_marmita_e2e.sh`, `test_prevcarga_gateB.sh` | bundle reprodutivel, instalador e e2e do bundle (`m8-bundle*.md`, `m8-e2e-bundle.md`) e A/B de cutover | M8 |
| `test_exogena_override.sh` | sem equivalente: a exogena e identidade do membro no plano (`sigla`/`exogena` na topologia e na chave do cache) | M5 / M7.1 |

## Gate estatico do M7 (`test-m7-static.R`)
- `pc_m7_static_rules` (regex por regra) aplicada por `pc_m7_scan(files, rules)` sobre `orchestrate.R`, `cache.R`, `backtest.R`, a secao `.rpt_bt_` de `reporting.R` (do
  marcador `## --- Relatorio do backtest (M7.4)` ao fim) e os registros `cli/plan.R` e `cli/backtest.R`. Comentarios saem pelo parser do R antes do scan.
- Regras: variavel de ambiente, fork (`mclapply`, `makeForkCluster`), shell (`system`, `system2`), `file.symlink`, literal `/tmp`, relogio (`Sys.time`, `Sys.Date`), datas
  base (`as.POSIXct`, `strptime`, `as.Date`), rede (`paws`, `s3://`, `download.file`, `httr`), `python3`, nomes legados de corte, literais de contagem e bytes > 127.
  O codigo do M7 e ASCII puro: as mensagens ao usuario vem do catalogo `errors/*.tsv`.
- CONTROL: `scripts/lib/tests/fixtures/m7/static-bait.R` tem um exemplo de cada padrao; o teste falha se alguma regra nao dispara nela.
- Excecao fixada (linha exata, nao a regra): `.rpt_bt_value(sf$sum_check, 8L)` em reporting.R; o `8L` e o numero de DIGITOS da conferencia SIN = soma, nao uma
  contagem de membros. Um `8L` em qualquer outra linha continua reprovando (coberto por teste).

## Gates de fechamento do epico
- Suite M7: `test-orchestrate*.R`, `test-cache.R`, `test-backtest*.R`, `test-harness-*.R` (inclui `test-harness-gates.R`, M7.5), `test-grammar-m7-siblings.R`, `test-m7-static.R`.
- Gates de harness dos plugins (3 gates + o de y_true real): `scripts/tools/gates/m7_harness.R`, por `models/<m>/tests/run_all_gates.R`.
- Execucao no Windows (Pester, PowerShell, `cross_os`) fica DEFERIDA ao gate windows-vm do M8; os testes do M7 nao dependem de bash e usam so `callr`/PSOCK, mas
  a prova em Windows nativo e daquele gate.
- Pendencia de fechamento: `.claude/workflows/implement-milestone.js` (`CHECKPOINTS['M7']`) e `docs/roadmap/CLI-MODULAR-OBJETIVO.md` citados no plano original nao existem no
  repositorio (o workflow nao versiona o checkpoint do M7; o roadmap esta arquivado em `docs/_archive/roadmap/`, so leitura). Registrado como discovery do ticket M7.V.
