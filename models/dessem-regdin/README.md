# Plugin dessem-regdin (membros `RGD_MED` e `RGD_MAX` do clean-room)

Porte R-only do modelo DESSEM regdin do oraculo `prevcargadessem3/models/dessem-regdin`, sobre o motor compartilhado
`models/_dessem-engine` (dono: dessem-linear). Split `origin_scoped` (ADR-0023), 60 min, H = 9 dias, um modelo por (area, variante
`exogena` MED|MAX). O estado duravel do plugin mora em `docs/estado/dessem_regdin.md`; este README e o mapa de leitura e nao o duplica.
So ASCII. O plugin nao le variavel de ambiente, nao faz aritmetica de hora e nao conta corte: le tudo de `cuts` e de `run.yaml`.

## Mapa funcao -> arquivo:linha do oraculo

| Estagio (clean-room `models/dessem-regdin/R/`) | Oraculo (`prevcargadessem3/models/dessem-regdin/`) |
|---|---|
| `helpers_series.R` `.rd_build_series`, `.rd_enforce_real_history`, `.rd_day_types` | `R/training/dessem_regdin_fit.R:45-93` |
| `training.R` `.rd_fit_pass` (RNG preservado, cabeca de referencia) / `rd_training` | `R/training/dessem_regdin_fit.R:95-139` (`.rd_fit_pass`), `R/training/dessem_regdin_fit.R:140-206` (`fit_dessem_regdin`) |
| `helpers_nowcast.R` `rd_dm_nowcast` (Peca A wSVM + Peca B ARIMA re-ancorado em O) | `vendor/_load_engine.R:300-370` (wrapper com estado `.rd_nowcast`) |
| `helpers_nowcast.R` `rd_morning_means` / `.rd_mm_core` (`mm` so com horas completas) | `R/ingestion/data_adapter.R:165-188` (`.load_morning_means_rd`) |
| `ingestion.R` `plugin_ingest`, `.rd_exo`, `.rd_map_holidays`, `.rd_assert_panel` | `R/ingestion/data_adapter.R:36-59` (`available_until`), `:60-164` (`.load_cargahist_rd`, `.load_exogenahist_rd`, `.load_exogenaprev_rd`), `:189-216` (`.load_feriados_rd`), `:217-363` (`load_model_input`, `.validate_dessem_deck_rd`) |
| `inference.R` `.rd_predict_pass`, `plugin_infer`, `.rd_envelope` | `R/inference/dessem_regdin_predict.R:50-114` (`.rd_predict_pass`), `:115-` (`predict_dessem_regdin`) |
| `inference.R` `.rd_check_window`, `.rd_check_fingerprint` (invariante de janela) | sem equivalente no oraculo (guarda nova do clean-room) |
| `freeze.R` `rd_freeze` / `rd_thaw` (sem mable, sha256 do artefato) | `R/training/dessem_regdin_fit.R:207-` (`.rd_save_dessem_artifact`) |
| `register.R` `plugin_descriptor`, `plugin_constants`, `rd_semihourly_rule` | `R/registry/plugin_registry.R`, `R/config.R` |
| despachante da fundacao (nao e do plugin) | `R/orchestrator/orchestrator.R` |

## Contrato do descriptor

`plugin_descriptor()` + `run.yaml` (loader P.0b, Tier A nos 5 estagios):

- `split: origin_scoped`, `granularity_min: 60`, `horizon_days: 9`, `history_days {train 1825, infer 1825}`, `seed: 42`,
  `modes: [train, infer, forecast]`, `params: {exogena: MED}` (variante `MED`|`MAX`; `MXMN` e recusado, ver desvios).
- `delivery_days: 9` (entrega O..O+8), `data_lag_days: 0`, `model_time_offset_min: 60` (a UNICA declaracao do deslocamento de hora,
  aplicada pelo despachante), `weather_resample: hourly`, `engine: _dessem-engine`, `uses_rng: TRUE`, `rng_deviation: NULL`,
  `history_infer_equals_train: TRUE`, `weather_forecast_in_train: TRUE`.
- Deck exigido: `data.load.load_mwh`, `data.weather_forecast.temperature`, `data.holidays.prevcarga_code`. Erros: ids `plugin.regdin_*` em
  `scripts/lib/errors/dessem_regdin.tsv` (mensagens ASCII).
- `run.yaml` `backtest.protocol_default: retrain_harness` e protocolo de backtest; a propriedade do modelo e o split `origin_scoped`.

## Layout `dessem_matrix_nx48` (`dessem_semihourly_48`)

`values` = matriz 9 x 48 (linha 1 = dia O completado, ate O+8; colunas = slots de 30 min no rotulo de INICIO do intervalo). O nome do
ticket e `dessem_matrix_nx48`; o loader so aceita `prevcarga_native_layouts`, entao o descriptor declara `dessem_semihourly_48` (o mesmo
layout do dessem-linear). A entrega (`delivery`) e a grade da fundacao (`prevcarga_delivery_grid(cuts, 9L)`): `interval_end` de
`available_until + 30 min` ate O+9 00:00 (413 intervalos com corte 09:00 ou 09:30).

## Faixa O..O+8

A entrega e O (apos o corte) ate O+8. O motor roda `horizon_days + 1` = 10 dias (O..O+9): o oraculo usa `n_forecast_days = horizon + 1`
e o spline `monoH.FC` de `desagregaModeloSH` do ultimo dia entregue ve o dia seguinte. O+9 alimenta o spline e NAO e entregue (`fora_da_faixa`
no A/B). O leitor tem de entregar clima previsto ate O+9.

## Invariante de janela (`history_days.infer == train`, tres camadas)

1. Declaracao: `constraints.infer_history_equals_train: true` no `run.yaml` e `history_infer_equals_train = TRUE` no descriptor
   (os dois `history_days` do `run.yaml` valem 1825).
2. Guarda de painel: `.rd_check_window` confere que o painel de infer cobre exatamente `train_window` do artefato; senao
   `plugin.regdin_window_mismatch` (window, exit 5), antes de qualquer ajuste.
3. Fingerprint do ARIMA: `.rd_check_fingerprint` refaz o ARIMA base sobre a serie <= O-1 e confere ordem, coeficientes (tolerancia 0) e
   `dm_digest`; deriva => `plugin.regdin_arima_drift` (contract, exit 2).

## RNG preservado e plano B

O oraculo ajusta, no mesmo processo, uma cabeca de nowcast de referencia (corte fixo 12:00) antes dos SVMs de perfil. O treino do
plugin preserva essa ordem: `set.seed(seed)` -> ARIMA base -> cabeca de referencia DESCARTADA (so posiciona o RNG) -> 24 SVMs por dia
normal. Os SVMs de perfil batem o oraculo com max|Delta| = 0 e a posicao do RNG e identica nos 49 ajustes. Plano B (so se algum dia a
posicao divergir): `descriptor$rng_deviation` nao nulo => `set.seed` re-fixado antes do perfil e aviso `warning.regdin_rng_deviation`;
o golden do gate passaria a ser a composicao. Hoje `rng_deviation = NULL` (o braco `composition` do RD.6 mede centenas de MW de
diferenca, entao o plano B nao e equivalente ao nativo).

## Tabela gate x dono

Cada teste de `tests/testthat` leva `[PLUGINS] <gate>:` ou `[DEFERRED Mx] <gate>:` com uma linha da tabela de `tests/run_all_gates.R`
(que e DADO; os sub-gates de `scripts/tools/gates/*.R` entram por descoberta).

| Gate | Dono | Arquivo |
|---|---|---|
| `descritor`, `ingestao`, `nowcast_mm`, `treino_freeze`, `inferencia` | [PLUGINS] | `test-rd1-plugin.R`, `test-rd2-*.R`, `test-rd3-infer.R` |
| `contrato`, `params_schema`, `exogena_variante` | [PLUGINS] | `test-contract.R` |
| `janela_infer_igual_treino`, `arima_drift` | [PLUGINS] | `test-window-invariant.R` |
| `treino_ate_o_menos_1` | [PLUGINS] | `test-train-honesty.R` |
| `mm_head_rng`, `mm_head_corte` (`mm_head_oracle` em parity) | [PLUGINS] | `test-mm-head.R` |
| `nowcast_so_no_infer`, `corte_horario`, `split_vs_acoplado` | [PLUGINS] | `test-nowcast.R`, `test-cut.R`, `test-split.R` |
| `corte_clima`, `corte_carga`, `sentinela`, `artefato_puro`, `higiene`, `grep` | [PLUGINS] | `test-weather.R`, `test-sentinel.R`, `test-portability.R`, `test-no-env.R` |
| `escala_mw`, `contract_synthetic`, `contract_e2e`, `flag12_yearend`, `nowcast_parcial`, `split_nowcast_med`, `split_nowcast_max` | [PLUGINS] | `test-oracle-gates.R` |
| `units_yaml`, `runner_controle`, `requires_report`, `paridade_*` | [PLUGINS] | `test-runner.R`, `test-parity.R` |
| `aditividade_epico`, `readme` | [PLUGINS] | `test-additivity.R` |
| `schema_long`, `ytrue_real` | [DEFERRED M6] | LONG / y_true |
| `sem_crash_harness` | [DEFERRED M7] | harness de varias origens |
| `fachada` | [DEFERRED M5] | verbos e recusa de artefato |
| `cross_os` | [DEFERRED M8] | Windows x Linux |

## Comandos

```
Rscript --vanilla models/dessem-regdin/tests/run_all_gates.R --tier fast
Rscript --vanilla models/dessem-regdin/tests/run_all_gates.R --tier parity
Rscript --vanilla models/dessem-regdin/tests/run_all_gates.R --tier parity-full
```

`fast` roda os testes de testthat e os sub-gates (Linux e Windows); `parity` = grupo `smoke` do `tests/parity/units.yaml` (3 unidades);
`parity-full` = smoke + foundation + fanout (13 unidades). O golden e gerado por `scripts/tools/plugin_golden.R --model dessem-regdin`
(gitignorado; so o `tests/parity/golden.lock` e versionado) a partir de um clone limpo do oraculo em R 4.5.3.

## Desvios conscientes do oraculo (para o A/B do M8)

1. `MXMN` recusado: o oraculo caia para `MED` com aviso; aqui a variante fora de MED/MAX e `plugin.regdin_bad_exogena`.
2. `mm` so com horas completas: o oraculo usa o prefixo de meias horas com `timestamp <= available_until` (inclui o carimbo do proprio
   corte e meia hora incompleta); o plugin so entra com hora cheia completa de O. Cortes 00:00 e 00:30 ficam sem cabeca de nowcast
   (ARIMA base); no corte 12:00 as duas regras dao as mesmas horas.
3. `available_until` = 00:00 (a corrida sem informacao do dia O) e aceito pelo clean-room; o oraculo no corte 00:00 contra o clean-room
   em 09:00 difere ~1,5 MW (~0,2%) no `d_plus = 0` (braco informativo `cut0000` do RD.6, origem 2024-10-15).
4. Hora incompleta descartada em todas as series, inclusive o clima previsto (decisao do Gabriel, 2026-10-30): a emissao 00Z termina em
   O+9 23:00 com um slot, o dia O+9 fica curto e isso alcanca as ultimas ~3 h de O+8; essas ~3 h sao a divergencia NOMEADA por unidade
   (1,2 a 49 MW); fora delas max|Delta| <= 1,35e-7 MW e o CONTROL com a media de hora parcial do oraculo fecha tudo.
5. O+9 nao entregue (so alimenta o spline de O+8).
6. Plano B do RNG: nao acionado (`rng_deviation = NULL`); se for, o golden do gate passa a ser a composicao e o relatorio declara
   `gate_golden: composition`.

A divergencia de versao de pacote (fable 0.5.0 contra 0.3.3 do `renv.lock` do oraculo) e da ADR-0036 (max_rel 1,3e-5 / max_abs 0,416 MW)
e e medida no A/B do M8, nao aqui.

## Contrato `requires` / `provides` (fechamento do porte; `R/register.R`, `stages`)

- `ingestion`: `requires` = `data.load.load_mwh`, `data.weather_observed.temperature`, `data.weather_forecast.temperature`, `data.holidays.prevcarga_code`; `provides` = `frame.deck`.
- `feature_engineering`: `frame.deck` -> `feat.series`. `training`: `feat.series` -> `artifact.regdin`.
- `inference_features`: `frame.deck`, `artifact.regdin` -> `inf.feat`. `inference` e `forecast`: `artifact.regdin`, `inf.feat` -> `forecast.y` / `forecast.f`.
