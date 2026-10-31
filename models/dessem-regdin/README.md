# Plugin dessem-regdin (`models/dessem-regdin`)

Porte do oraculo `prevcargadessem3/models/dessem-regdin` (somente leitura) para o contrato de plugin da CLI `prevcarga`.
Fornece 2 membros do topology: `RGD_MED` e `RGD_MAX` (variante = `params.exogena`; `MXMN` nao existe no regdin). Motor compartilhado com o
dessem-linear: `models/_dessem-engine`. O estado duravel mora em `docs/estado/dessem_regdin.md`; este arquivo e o mapa e o contrato.
Escrito no fechamento M5.12 (AC44) porque o porte nao o criou.

## Mapa funcao -> arquivo:linha do oraculo

Oraculo = `prevcargadessem3/models/dessem-regdin/R/` (HEAD `c4af737`).

| Funcao do plugin (`R/`) | Oraculo |
|---|---|
| `plugin_ingest`, `.rd_assert_panel`, `.rd_exo`, `.rd_hourly_to_deck` (`ingestion.R`) | `ingestion/ingestion_plugin.R`; `ingestion/data_adapter.R` (mesmo adapter do dessem-linear) |
| `.rd_build_series`, `.rd_day_types` (`helpers_series.R`) | `training/dessem_regdin_fit.R:45` (`.rd_build_series`), `:78` (`.rd_day_types`), `:69` (`.rd_enforce_real_history`) |
| `plugin_build_features` (`feature_engineering.R`) | `feature_engineering/dessem_regdin_feature_engineering.R` |
| `rd_training`, `.rd_fit_pass`, `rd_assert_train_cut` (`training.R`) | `training/dessem_regdin_fit.R:95-140` (`.rd_fit_pass`, `fit_dessem_regdin`); `training/dessem_regdin_training.R` |
| `rd_freeze`, `rd_thaw` (`freeze.R`) | `training/dessem_regdin_fit.R:207` (`.rd_save_dessem_artifact`) |
| `rd_inference_features` (`inference_features.R`) | `inference_features/dessem_regdin_inference_features.R` |
| `plugin_infer`, `.rd_predict_pass`, `rd_foundation` (`inference.R`) | `inference/dessem_regdin_inference.R`; `inference/dessem_regdin_predict.R:50-115` (`.rd_predict_pass`, `predict_dessem_regdin`) |
| `rd_dm_nowcast`, `rd_morning_means` (`helpers_nowcast.R`) | `vendor/_load_engine.R:300-370` (wrapper `.rd_nowcast`, substituido por funcao explicita) |
| primitivas do motor | `models/_dessem-engine/README.md` |

## Contrato

- **Descriptor** (`R/register.R`): `model_id dessem-regdin`, `contract_version 1`, `granularity_supported 60`, `native_layout dessem_semihourly_48`,
  `delivery_days 9` (motor de 10 dias), `data_lag_days 0`, `model_time_offset_min 60`, `uses_rng`, `history_infer_equals_train`, `weather_forecast_in_train`,
  `engine _dessem-engine`.
- **Estagios e `requires`/`provides`**: `ingestion` (requires `data.load.load_mwh`, `data.weather_observed.temperature`, `data.weather_forecast.temperature`,
  `data.holidays.prevcarga_code`; provides `frame.deck`); `feature_engineering` (`frame.deck` -> `feat.series`); `training` (`feat.series` ->
  `artifact.regdin`); `inference_features` (`frame.deck`, `artifact.regdin` -> `inf.feat`); `inference` e `forecast` (`artifact.regdin`, `inf.feat` -> `forecast.y` / `forecast.f`).
- **`run.yaml`**: `split origin_scoped`, 60 min, `horizon_days 9`, `history_days {train 1825, infer 1825}`, `seed 42`, `constraints {infer_history_equals_train: true}`,
  `params {exogena: MED}`. Infer = treino (refit do ARIMA com fingerprint conferido).
- **Hora**: nenhuma aritmetica de hora no plugin nem no motor.

## Layout nativo e hora

`dessem_semihourly_48`: `values` = 9 x 48 (O..O+8). Deslocamento de 60 min so do despachante.

## Desvios conscientes do oraculo

1. Hora incompleta descartada tambem na previsao de clima (mesma regra do dessem-linear, Gabriel 2026-10-30).
2. Versoes de pacote (R 4.5.3 + fable 0.5.0 contra fable 0.3.3 do oraculo): tolerancia do ADR-0036 (max_rel 1,3e-5), medida pelo A/B do M8.8, nao aqui.
3. `rd_abort`/`rd_warn`/`rd_foundation` por `globalenv()` interino (pinado por teste).

## Gates x dono

Runner: `Rscript --vanilla models/dessem-regdin/tests/run_all_gates.R --tier fast|parity|parity-full [--units smoke|foundation|fanout] [--format json]`.
O fan-out exige o `foundation_report.json` do dessem-linear (`requires_report`, sha256 conferido). Packs `[M5]`/`[M6]` por descoberta nao constam desta tabela.

Nota: as linhas `[DEFERRED M5]` e `[DEFERRED M6]` da `gate_table` sao substituidas, no runner, pelos packs `[M5]` (`m5_facades.R`, M5.8) e `[M6]` (`m6_long.R`, M6.6) do mesmo dono; so `[DEFERRED M7]` e `[DEFERRED M8]` seguem como SKIP com dono.

| Gate (`gate_table`) | Gate do oraculo | Teste | Dono | Tier |
|---|---|---|---|---|
| `descritor` | - | `test-rd1-plugin.R` | [PLUGINS] | fast |
| `ingestao` | - | `test-rd1-plugin.R` | [PLUGINS] | fast |
| `nowcast_mm` | - | `test-rd2-nowcast.R` | [PLUGINS] | fast |
| `treino_freeze` | - | `test-rd2-train.R` | [PLUGINS] | fast |
| `inferencia` | - | `test-rd3-infer.R` | [PLUGINS] | fast |
| `contrato` | - | `test-contract.R` | [PLUGINS] | fast |
| `params_schema` | - | `test-contract.R` | [PLUGINS] | fast |
| `exogena_variante` | `gate04` | `test-contract.R` | [PLUGINS] | fast |
| `janela_infer_igual_treino` | - | `test-window-invariant.R` | [PLUGINS] | fast |
| `arima_drift` | - | `test-window-invariant.R` | [PLUGINS] | fast |
| `treino_ate_o_menos_1` | - | `test-train-honesty.R` | [PLUGINS] | fast |
| `mm_head_rng` | - | `test-mm-head.R` | [PLUGINS] | fast |
| `mm_head_corte` | - | `test-mm-head.R` | [PLUGINS] | fast |
| `nowcast_so_no_infer` | - | `test-nowcast.R` | [PLUGINS] | fast |
| `corte_horario` | - | `test-cut.R` | [PLUGINS] | fast |
| `corte_clima` | `gate05` | `test-weather.R` | [PLUGINS] | fast |
| `corte_carga` | - | `test-weather.R` | [PLUGINS] | fast |
| `split_vs_acoplado` | - | `test-split.R` | [PLUGINS] | fast |
| `sentinela` | - | `test-sentinel.R` | [PLUGINS] | fast |
| `artefato_puro` | - | `test-portability.R` | [PLUGINS] | fast |
| `higiene` | - | `test-no-env.R` | [PLUGINS] | fast |
| `grep` | - | `test-rd1-plugin.R`, `test-rd2-train.R` | [PLUGINS] | fast |
| `escala_mw` | `gate02` | `test-oracle-gates.R` | [PLUGINS] | fast |
| `contract_synthetic` | `gate07` | `test-oracle-gates.R` | [PLUGINS] | fast |
| `contract_e2e` | `gate08` | `test-oracle-gates.R` | [PLUGINS] | fast |
| `flag12_yearend` | `gate09` | `test-oracle-gates.R` | [PLUGINS] | fast |
| `nowcast_parcial` | `gate10,gate11` | `test-oracle-gates.R` | [PLUGINS] | fast |
| `split_nowcast_med` | `gate12` | `test-oracle-gates.R` | [PLUGINS] | fast |
| `split_nowcast_max` | `gate13` | `test-oracle-gates.R` | [PLUGINS] | fast |
| `units_yaml` | - | `test-parity.R` | [PLUGINS] | fast |
| `runner_controle` | - | `test-runner.R` | [PLUGINS] | fast |
| `requires_report` | - | `test-parity.R` | [PLUGINS] | parity |
| `mm_head_oracle` | - | `test-mm-head.R` | [PLUGINS] | parity |
| `paridade_smoke` | - | `test-parity.R` | [PLUGINS] | parity |
| `paridade_control` | - | `test-parity.R` | [PLUGINS] | parity |
| `paridade_foundation` | - | `test-parity.R` | [PLUGINS] | parity-full |
| `paridade_fanout` | - | `test-parity.R` | [PLUGINS] | parity-full |
| `schema_long` | `gate01` | `test-oracle-gates.R` | [DEFERRED M6] | fast |
| `ytrue_real` | `gate06` | `test-oracle-gates.R` | [DEFERRED M6] | fast |
| `sem_crash_harness` | `gate03` | `test-oracle-gates.R` | [DEFERRED M7] | fast |
| `fachada` | - | `test-oracle-gates.R` | [DEFERRED M5] | fast |
| `cross_os` | - | `test-portability.R` | [DEFERRED M8] | fast |
