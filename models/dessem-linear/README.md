# Plugin dessem-linear (membros `LIN_MED`, `LIN_MAX` e `LIN_MXMN` do clean-room)

Porte R-only do modelo DESSEM linear do oraculo `prevcargadessem3/models/dessem-linear`, sobre o motor unico `models/_dessem-engine`
(so primitivas `pkgfun_*`, serializador de SVM e `engine_manifest()`; README proprio em `models/_dessem-engine/README.md`). Split
`origin_scoped` (ADR-0023), 60 min, H = 9 dias, um modelo por (area, variante `exogena` MED|MAX|MXMN). O estado duravel do plugin mora
em `docs/estado/dessem_linear.md`; este README e o mapa de leitura e nao o duplica. So ASCII. O plugin nao le variavel de ambiente,
nao faz aritmetica de hora e nao conta corte: le tudo de `cuts` e de `run.yaml`.

## Mapa para o oraculo

| Estagio (clean-room `models/dessem-linear/R/`) | Oraculo (`prevcargadessem3/models/dessem-linear/`) |
|---|---|
| `ingestion.R` `plugin_ingest`, `dl_validate_deck`, `dl_assert_covers_days` | `R/ingestion/data_adapter.R:91-130` (`.resolve_available_until`), `:131-146` (`.drop_incomplete_hours`), `:147-209` (`.load_cargahist`), `:210-260` (`.load_exogenahist`), `:261-335` (`.load_exogenaprev`), `:369-420` (`.load_feriados`), `:421-545` (`load_dessem_input`), `:546-591` (`.validate_dessem_deck`) |
| `ingestion.R` horizonte do motor = entregues + 1 | `R/ingestion/data_adapter.R:456` (`eng_horizon <- horizon + 1L`) |
| `helpers_nowcast.R` `dl_morning_means`, `dl_dm_head`, `dl_dm_head_input`, `dl_normalize_mm` (cabeca `mm`) | `R/ingestion/data_adapter.R:336-368` (`.load_morning_means`) |
| `feature_engineering.R` `dl_build_series`, `dl_norm_constants`, `plugin_build_features` | `R/training/dessem_linear_fit.R:33-58` (`.dl_build_series`), `R/feature_engineering/dessem_linear_feature_engineering.R:19` |
| `training.R` `.dl_enforce_real_history` | `R/training/dessem_linear_fit.R:59-73` (`.dl_enforce_real_history`) |
| `training.R` `.dl_fit_pass` (RNG preservado, cabeca de referencia descartada), `dl_training` | `R/training/dessem_linear_fit.R:74-99` (`.dl_fit_pass`, `set.seed` na linha 85), `:100-163` (`fit_dessem_linear`) |
| `freeze.R` `dl_freeze`, `dl_thaw` (sem mable, serializador do motor) | `R/training/dessem_linear_training.R` (artefato em disco) |
| `inference_features.R` `dl_inference_features`, `dl_check_payload` | `R/inference_features/dessem_linear_inference_features.R:19` |
| `inference.R` `.dl_predict_pass`, `plugin_infer`, `.dl_envelope`, `dl_dm_o_fit` | `R/inference/dessem_linear_predict.R:54-100` (`.dl_predict_pass`), `:101-130` (`predict_dessem_linear`), `R/inference/dessem_linear_inference.R:51-134` (`dessem_linear_infer`) |
| `register.R` `plugin_descriptor`, `plugin_constants`, `dl_semihourly_rule` | `R/registry/plugin_registry.R` |
| despachante da fundacao (nao e do plugin) | `R/orchestrator/orchestrator.R:52-148` (`run_forecast`), `:178-200` (`run_train`), `:201-242` (`run_infer`) |

## Contrato do plugin

`plugin_descriptor()` + `run.yaml` (loader P.0b, Tier A nos 5 estagios):

- `run.yaml`: `split: origin_scoped`, `granularity_min: 60`, `horizon_days: 9`, `history_days {train 1825, infer 1825}`, `seed: 42`,
  `modes: [train, infer, forecast]`, `params: {exogena: MED}` (variante `MED`|`MAX`|`MXMN`; o valor do run.yaml e so o placeholder do
  loader, a variante real vem do membro).
- Descriptor: `delivery_days: 9` (entrega O..O+8), `data_lag_days: 0`, `model_time_offset_min: 60` (a UNICA declaracao do deslocamento
  de hora, aplicada pelo despachante), `weather_resample: hourly`, `engine: _dessem-engine`, `uses_rng: TRUE`, `rng_deviation: NULL`,
  `history_infer_equals_train: TRUE`, `weather_forecast_in_train: TRUE`, `native_layout: dessem_semihourly_48`.
- Erros: ids `plugin.dessem_linear_*` em `scripts/lib/errors/dessem_linear.tsv` (mensagens ASCII); o motor tem o seu `dessem_engine.tsv`.
- `run.yaml` `backtest.protocol_default: retrain_harness` e protocolo de backtest; a propriedade do modelo e o split `origin_scoped`.

## Layout dessem_matrix_nx48 e faixa O..O+8

`values` = matriz 9 x 48 (linha 1 = dia O completado, ate O+8; colunas = slots de 30 min no rotulo de INICIO do intervalo). O nome do
ticket e `dessem_matrix_nx48`; o loader so aceita `prevcarga_native_layouts`, entao o descriptor declara `dessem_semihourly_48`. A
entrega (`delivery`) e a grade da fundacao (`prevcarga_delivery_grid(cuts, 9L)`).

Faixa O..O+8: o motor roda `horizon_days + 1` = 10 dias (O..O+9), como o oraculo com corte intradiario (`eng_horizon = horizon + 1`,
`previsao15` 10 x 48); o spline `monoH.FC` do ultimo dia entregue ve o dia seguinte. O dia O+9 do oraculo alimenta o spline e NAO e
entregue (`fora_da_faixa` no A/B). O leitor tem de entregar clima previsto ate O+9.

## Cabeca de nowcast (mm por horas completas)

A cabeca de nowcast e do PLUGIN (`helpers_nowcast.R`), nao do motor: `dl_morning_means(hourly_load, available_until, origin_day)` entrega
as medias horarias de O no frame de modelagem e `dl_dm_head` re-treina a DM(O) com `mm` (so no infer; o treino so consome o RNG com uma
cabeca de referencia no corte 12:00, descartada). So hora cheia COMPLETA entra em `mm`: os cortes 00:00 e 00:30 ficam sem a linha de O
e o modo da DM(O) vem do DADO, nunca do relogio (`dl_dm_o_mode`: `nowcast` se `O %in% mm$Data`, senao `no_day_o`, que ajusta o
`SVM_DemandaMedia` puro do motor logo apos `set.seed`, o mesmo que o modo "paridade" do oraculo). 09:00 e 09:30 dao saidas identicas.

## RNG preservado e plano B

O oraculo ajusta, no mesmo processo, a cabeca de nowcast antes dos SVMs de perfil. O treino do plugin preserva a ordem:
`set.seed(seed)` -> por dia O..O+9: DM (em O a cabeca de referencia DESCARTADA, so posiciona o RNG) e 24 perfis = 10 x 25 = 250
ajustes de `tune_wsvm` (os 225 primeiros na mesma ordem do oraculo). A posicao do RNG e identica a cada ajuste (`test-mm-head.R`). Plano
B (so se algum dia a posicao divergir): `descriptor$rng_deviation` nao nulo => modo `reseed` (`set.seed` re-fixado antes do perfil de
O, 249 ajustes) e aviso `warning.dessem_linear_rng_deviation`; o golden do gate passaria a ser a composicao. Hoje
`rng_deviation = NULL` (modo `preserve`) e o plano B NAO esta acionado.

## Desvios conscientes (A/B do M8)

1. `mm` so com horas completas: o oraculo usa o prefixo de meias horas com `timestamp <= available_until` (inclui o carimbo do proprio
   corte e meia hora incompleta); o plugin so entra com hora cheia completa de O. Cortes 00:00 e 00:30 ficam sem cabeca de nowcast
   (DM(O) puro); no corte 12:00 as duas regras dao as mesmas horas.
2. O+9 nao entregue: o oraculo devolve 10 dias (`fora_da_faixa` 48 no A/B); o plugin entrega O..O+8 e usa O+9 so no spline.
3. Hora incompleta descartada em todas as series, o clima previsto incluido (decisao do Gabriel, 2026-10-30): uma emissao 00Z termina
   em O+9 23:00 com um slot, o dia O+9 fica curto e isso alcanca as ultimas ~3 h do ultimo dia entregue; essas ~3 h sao a divergencia
   NOMEADA por unidade (ver `foundation_report.json` e `fanout_report.json`); fora delas max|Delta| <= 1,3e-8 MW e o CONTROL com a
   media de hora parcial do oraculo fecha tudo.
4. Excecao numerica documentada: `sp-2024-11-15` (O = feriado) fica com residuo de 0,446 MW (3e-5 rel) por 1 ulp em `mm`
   (media de 9 medias horarias contra `mean` de 18 slots), amplificado pelo SVM; teto `max_abs <= 0.5` e `max_rel <= 5e-5` so nessa
   unidade. `tests/ab/ab_excecoes.yaml` NAO existe: so o Gabriel o cria.
5. Corte: o `available_until` do clean-room (09:00) e o rotulo de INICIO; o oraculo roda com `--available-until` 08:30 (`oracle_cut:
   start_label`). A rodada das 00h (nenhuma carga de O) e aceita pelo clean-room.
6. Plano B do RNG: nao acionado (`rng_deviation = NULL`); se for, o golden passa a ser a composicao e o relatorio declara
   `gate_golden: composition`.

A divergencia de versao de pacote (R 4.5.3 contra o runtime do oraculo) e da ADR-0036 e e medida no A/B do M8, nao aqui.

## Gates gate01..gate18 x dono

Cada teste de `tests/testthat` leva `[PLUGINS] <gate>:` ou `[DEFERRED Mx] <gate>:` com uma linha da tabela de `tests/run_all_gates.R`
(que e DADO; os sub-gates de `scripts/tools/gates/*.R` entram por descoberta). O `gate12` nao existe no oraculo.

| Gate | Gate do oraculo | Dono | Tier | Teste |
|---|---|---|---|---|
| `00h` | - | [PLUGINS] | fast | `test-dl-infer.R` |
| `descritor` | - | [PLUGINS] | fast | `test-dl-descriptor.R` |
| `ingestao` | - | [PLUGINS] | fast | `test-dl-ingest.R` |
| `nowcast_mm` | - | [PLUGINS] | fast | `test-dl-nowcast.R` |
| `treino_freeze` | - | [PLUGINS] | fast | `test-dl-train.R` |
| `inferencia` | - | [PLUGINS] | fast | `test-dl-infer.R` |
| `gate02_escala_mw` | `gate02` | [PLUGINS] | fast | `test-gates.R` |
| `gate04_exogena_params` | `gate04` | [PLUGINS] | fast | `test-contract.R` |
| `gate05_clima_honesto` | `gate05` | [PLUGINS] | fast | `test-weather.R` |
| `gate07_dataprev_o` | `gate07` | [PLUGINS] | fast | `test-gates.R` |
| `gate08_contrato` | `gate08` | [PLUGINS] | fast | `test-contract.R` |
| `gate09_dado_faltante` | `gate09` | [PLUGINS] | fast | `test-contract.R` |
| `gate10_modelo_vazio` | `gate10` | [PLUGINS] | fast | `test-robustez.R` |
| `gate11_clamp_cross` | `gate11` | [PLUGINS] | fast | `test-robustez.R` |
| `gate13_flag12` | `gate13` | [PLUGINS] | fast | `test-robustez.R` |
| `gate14_corte_intradiario` | `gate14` | [PLUGINS] | fast | `test-cut.R` |
| `gate15_sem_crash_13h` | `gate15` | [PLUGINS] | fast | `test-gates.R` |
| `gate16_cauda_curta` | `gate16` | [PLUGINS] | fast | `test-robustez.R` |
| `gate17_split` | `gate17` | [PLUGINS] | fast | `test-split.R` |
| `gate18_sem_treino_circular` | `gate18` | [PLUGINS] | fast | `test-train-honesty.R` |
| `corte_treino` | - | [PLUGINS] | fast | `test-train-honesty.R` |
| `mm_head` | - | [PLUGINS] | fast | `test-mm-head.R` |
| `robustez` | - | [PLUGINS] | fast | `test-robustez.R` |
| `sentinela` | - | [PLUGINS] | fast | `test-sentinel.R` |
| `grep` | - | [PLUGINS] | fast | `test-grep.R` |
| `no_env` | - | [PLUGINS] | fast | `test-no-env.R` |
| `portabilidade` | - | [PLUGINS] | fast | `test-portability.R` |
| `motor_incluso` | - | [PLUGINS] | fast | `test-motor.R` |
| `units_yaml` | - | [PLUGINS] | fast | `test-parity.R` |
| `runner_controle` | - | [PLUGINS] | fast | `test-runner.R` |
| `paridade` | - | [PLUGINS] | parity | `test-parity.R` |
| `paridade_control` | - | [PLUGINS] | parity | `test-parity.R` |
| `fanout_parity` | - | [PLUGINS] | parity-full | `test-fanout.R` |
| `exogena_oper` | - | [PLUGINS] | fast | `test-exogena-oper.R` |
| `aditividade_epico` | - | [PLUGINS] | fast | `test-additivity.R` |
| `readme` | - | [PLUGINS] | fast | `test-readme.R` |
| `gate01_schema_long` | `gate01` | [DEFERRED M6] | fast | `test-deferred.R` |
| `gate06_ytrue_real` | `gate06` | [DEFERRED M6] | fast | `test-deferred.R` |
| `gate03_sem_crash` | `gate03` | [DEFERRED M7] | fast | `test-deferred.R` |
| `fachada` | - | [DEFERRED M5] | fast | `test-deferred.R` |
| `cross_os` | - | [DEFERRED M8] | fast | `test-portability.R` |

Comandos:

```
Rscript --vanilla models/dessem-linear/tests/run_all_gates.R --tier fast
Rscript --vanilla models/dessem-linear/tests/run_all_gates.R --tier parity
Rscript --vanilla models/dessem-linear/tests/run_all_gates.R --tier parity-full
```

`fast` roda os testes de testthat, o motor e os sub-gates; `parity` = grupo `smoke` do `tests/parity/units.yaml`; `parity-full` = smoke +
foundation + fanout (28 unidades; `--units fanout` exige o relatorio da fundacao verde e integro). O golden e gerado por
`scripts/tools/plugin_golden.R --model dessem-linear` (gitignorado; so o `tests/parity/golden.lock` e versionado) a partir de um clone
limpo do oraculo.

## Relatorios de paridade (sha256)

sha256 dos arquivos em `tests/parity/` (o teste de documentacao os recalcula):

| Relatorio | sha256 do arquivo | campo `sha256` (bloco `units`) |
|---|---|---|
| `foundation_report.json` | `aa823f68dfabbb5f2dfbefcb9d0663d94e8565fb491f549b0fa96b506dd16507` | `474f9a9585c24a4bbe105d1605be1caf060ce3cf2d6e05140a92105229948e0e` |
| `fanout_report.json` | `3ffd718964eab52b84716f4f0549973abd887b6cef5fd070134557ff58ec4a50` | `81667f1123fa6c7a7fe145e331bbc5c999f71458ec480c5c9fe64a3aaa9c3af8` |

Regerar um relatorio muda o sha256 do arquivo e exige atualizar esta tabela e `docs/estado/dessem_linear.md`.
