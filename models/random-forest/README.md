# Plugin random-forest (`models/random-forest`)

Porte do oraculo `prevcargadessem3/models/random-forest` (somente leitura) para o contrato de plugin da CLI `prevcarga`.
O estado duravel (decisoes, paridade com o hash do relatorio, pendencias) mora em `docs/estado/random_forest.md`; este arquivo e o mapa,
o contrato e a tabela de gates. Nao duplica o estado.

## Mapa funcao -> arquivo:linha do oraculo

Caminhos relativos a `prevcargadessem3/models/random-forest/`.

| Funcao do plugin (`R/`) | Oraculo |
|---|---|
| `plugin_ingest` e auxiliares (`ingestion.R`): janelas, carga horaria, conferencia do painel | `R/ingestion/data_adapter.R:444-776` (`load_model_input`), `:125-203` (`.read_hourly_load`) |
| `.rf_weather_vintage`, `.rf_weather_locf` (`helpers_weather.R`): safra por origem nativa, tmax/tmin, LOCF por `day_of_horizon` | `R/ingestion/data_adapter.R:204-319` (`.read_forecast_temp`, LOCF em `:285-289`) |
| `.rf_holidays` (`ingestion.R`), `plugin_constants()$holidays_legacy` | `R/ingestion/data_adapter.R:320-376` (`.read_holidays`) |
| `plugin_build_features` (`feature_engineering.R`): tabela de 1486 features + 216 alvos, ordem das colunas do `merge` | `R/feature_engineering/feature_engineering.R:166-460` (nomes e blocos) |
| `train_multioutput_rf`, `plugin_train` (`training.R`): 216 `randomForest`, uniao de features por lead | `R/training/training_plugin.R:229-300` (`.select_features_for_lead`), `:563-693` (`train_multioutput_rf`) |
| `rf_freeze` / `rf_thaw`, `plugin_freeze` / `plugin_thaw` (`freeze.R`): artefato de dado puro | `R/training/training_plugin.R:694-760` (`save_rf_artifact`/`load_rf_artifact`) |
| `plugin_inference_features` (`inference_features.R`), `predict_rf` (`inference.R`) | `R/inference/predict_rf.R:334-470` |
| `rf_semihourly_rule` (`inference.R`): spline monoH.FC 24 -> 48 | `run_pipeline.R:503-515` |
| `plugin_infer` / `.rf_blocks_to_delivery` (`inference.R`): emenda dos dois blocos e faixa O..O+8 | `R/orchestrator/orchestrator.R:258-495` (`run_pipeline`) |

## Contrato

- **Descriptor** (`R/register.R`): `model_id random-forest`, `contract_version 1`, `granularity_supported 60`, `native_layout rf_wide_hourly_216`,
  `delivery_days 9` (O..O+8), `data_lag_days 0` (`last_data_day = O-1`), `uses_rng`, `history_infer_equals_train` FALSE, pacotes
  `randomForest`/`data.table`/`lubridate`. Cinco estagios na assinatura canonica da fundacao (ingestion, feature_engineering, training,
  inference_features, inference).
- **`run.yaml`**: `split origin_agnostic` (1 treino serve a todas as origens; `train_origin = O-1-H`, ADR-0023), 60 min, `horizon_days 9`,
  `history_days {train 365, infer 45}`, `seed 123`, `modes [train, infer]`, `params {ntree: 100}`. `ntree` e `seed` nao tem default no codigo.
- **Cortes**: o plugin le `cuts$train_origin`, `cuts$last_data_day` e `cuts$weather_issue_limit` de `dates.R`; nunca calcula corte proprio.
  Confere o painel (`plugin.train_beyond_cut`, `plugin.infer_beyond_cut`, `plugin.weather_beyond_limit`, `plugin.raw_timestamp`) e nunca filtra.
- **Hora**: nenhuma aritmetica de hora no plugin (principio 7, sem excecoes). A carga horaria e de `to_hourly`, o carimbo de 30 min de `to_semihourly`.

## Contrato `requires` / `provides` (M5.12; `R/register.R`, `stages`)

Cada estagio declara os tokens que consome e produz (a fundacao encadeia e valida por eles):
- `ingestion`: `requires` = `data.load.load_mwh`, `data.weather_forecast.{issue_time,valid_time,temperature}`, `data.holidays.date`; `provides` = `rf_frame`.
- `feature_engineering`: `rf_frame` -> `rf_features`. `training`: `rf_features` -> `rf_artifact`.
- `inference_features`: `rf_frame`, `rf_artifact` -> `rf_inf_features`. `inference`: `rf_artifact`, `rf_inf_features` -> `member_envelope`.

## Layout `rf_wide_hourly_216`

`values` e uma matriz 2 x 216 (linhas = origens nativas O-1 e O, colunas `y_h001..y_h216`; `y_h001` = primeira hora do dia seguinte a origem).
O artefato guarda 216 sub-modelos `randomForest` com as features usadas por lead (nunca as regex do oraculo) e `train_origin`.

## Emenda do dia O

O RF nao le a carga do dia O. O dia O sai do bloco da origem nativa O-1 (carga ate O-2) e O+1..O+8 do bloco da origem O (carga ate O-1),
as duas origens inferidas com o MESMO artefato. Cada bloco passa inteiro pelo `to_semihourly` com `rf_semihourly_rule` e so depois e cortado;
a emenda concatena por `interval_end`, nenhum spline atravessa os blocos (convencao `day0_source: oracle_origin_minus_1` da paridade).

## Faixa O..O+8

A entrega cobre O..O+8 (8 dias de O+1 mais o dia O). O+9 (o 9o dia do bloco O) nao e entregue: o oraculo o emite e a paridade o conta como
`fora_da_faixa_long` (48 slots por unidade).

## Alvos ate train_origin + H

Os alvos de treino chegam ate `train_origin + H` = `last_data_day` = O-1: a origem de treino mais recente tem 216 horas de alvo que terminam em O-1 23:00.
`plugin_train` confere `max(dataOrigem) <= train_origin` e `top + H <= last_data_day`; um corte uniforme que vazasse o horizonte e proibido (principio 4).
A safra de clima de cada origem nativa T e a emissao mais fresca em `(limit(T) - 1 dia, limit(T)]`, com `limit(T) = weather_issue_limit - days(O - T)`
(regra `weather_issue_cutoff` da fundacao: `available_until` ou `origin_midnight_utc`).

## Desvios conscientes do oraculo

1. O dia O e entregue pelo bloco da origem O-1 (o RF nao le a carga do dia O).
2. O+9 nao e entregue (faixa O..O+8).
3. Hora incompleta e descartada (`to_hourly` da fundacao), nao interpolada.
4. Feature NA no infer aborta (`plugin.completude`, exit 3), sem o "degrade gracefully" por subconjunto de features do oraculo.
5. Feriados sem fallback silencioso: painel sem `holidays` aborta `plugin.missing_column`; a lista legada 2022-2025 e unida ao painel.

## Gates x dono

`Rscript --vanilla models/random-forest/tests/run_all_gates.R [--tier fast|parity|parity-full] [--units <grupo>] [--format text|json]` (so compoe
`prevcarga_gates_run`; descobre os packs de `scripts/tools/gates/*.R` por `applies_to`, nenhum epico externo edita o runner).

| Gate do oraculo | Gate | Dono | Tier |
|---|---|---|---|
| gate02 | `contrato`, `ingest_frame`, `ingest_cut_check`, `features_contract`, `estagios` | [PLUGINS] | fast |
| gate01 / gate11 | `corte_treino`, `corte_dia_o` | [PLUGINS] | fast |
| gate10 | `weather_vintage`, `carry_forward` | [PLUGINS] | fast |
| gate09 / gate10 | `weather`, `weather_limit` (canal de clima do honesty_gate, 2 regras) | [PLUGINS] | fast |
| gate05 | `freeze` | [PLUGINS] | fast |
| gate06 / gate07 | `no_env`, `determinismo` | [PLUGINS] | fast |
| - | `completude`, `completude_infer`, `holidays`, `corte_horario`, `sentinela`, `spline_rule`, `units_yaml`, `aditividade`, `higiene`, `readme`, `runner_controle` | [PLUGINS] | fast |
| gate03 | `paridade`, `paridade_control` | [PLUGINS] | parity |
| gate04 | `gate04` (schema LONG) | [DEFERRED M6] | - |
| gate08 | `gate08` (y_true real / combina) | [DEFERRED M6] | - |
| gate12 | `gate12` (harness LONG) | [DEFERRED M7] | - |
| - | `fachada` | [DEFERRED M5] | - |
| - | `cross_os` | [DEFERRED M8] | - |
