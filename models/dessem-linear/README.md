# Plugin dessem-linear (`models/dessem-linear`)

Porte do oraculo `prevcargadessem3/models/dessem-linear` (somente leitura) para o contrato de plugin da CLI `prevcarga`.
Fornece 3 membros do topology: `LIN_MED`, `LIN_MAX`, `LIN_MXMN` (variante = `params.exogena`). O motor (SVM/wSVM, perfil, DM) e o
`models/_dessem-engine` (compartilhado com o dessem-regdin; ver o README dele). O estado duravel (decisoes, paridade, ids de erro)
mora em `docs/estado/dessem_linear.md`; este arquivo e o mapa e o contrato. DL.9 escreveu o runner e a paridade; este README foi
escrito no fechamento M5.12 (AC44) porque o porte nao o criou.

## Mapa funcao -> arquivo:linha do oraculo

Oraculo = `prevcargadessem3/models/dessem-linear/R/` (linhas conferidas no HEAD `c4af737`).

| Funcao do plugin (`R/`) | Oraculo |
|---|---|
| `plugin_ingest` e `dl_validate_deck`, `dl_assert_*` (`ingestion.R`) | `ingestion/ingestion_plugin.R:31` (`dessem_linear_ingest`); `ingestion/data_adapter.R:91-595` (`load_dessem_input`:421, `.drop_incomplete_hours`:131, `.load_cargahist`:147, `.load_exogenahist`:210, `.load_exogenaprev`:261, `.load_morning_means`:336, `.load_feriados`:369, `.validate_dessem_deck`:546) |
| `plugin_build_features`, `dl_build_series`, `dl_norm_constants` (`feature_engineering.R`) | `feature_engineering/dessem_linear_feature_engineering.R:19`; `training/dessem_linear_fit.R:33` (`.dl_build_series`) |
| `dl_training`, `.dl_fit_pass`, `dl_assert_train_cut` (`training.R`) | `training/dessem_linear_training.R:214` (`train_dessem_linear`); `training/dessem_linear_fit.R:59-100` (`.dl_enforce_real_history`, `.dl_fit_pass`, `fit_dessem_linear`) |
| `dl_freeze`, `dl_thaw` (`freeze.R`) | `training/dessem_linear_training.R:254` (`.save_dessem_artifact`) |
| `dl_inference_features`, `dl_check_payload` (`inference_features.R`) | `inference_features/dessem_linear_inference_features.R:19` |
| `plugin_infer`, `.dl_predict_pass`, `dl_dm_o_fit`, `dl_foundation` (`inference.R`) | `inference/dessem_linear_inference.R:51` (`dessem_linear_infer`); `inference/dessem_linear_predict.R:54-101` (`.dl_predict_pass`, `predict_dessem_linear`) |
| `dl_morning_means`, `dl_dm_head` (`helpers_nowcast.R`) | `ingestion/data_adapter.R:336`; `vendor/_load_engine.R:242-370` (nowcast da DM(O)) |
| primitivas do motor | `models/_dessem-engine/README.md` (mapa `pkgfun_*.R:linha`) |

## Contrato

- **Descriptor** (`R/register.R`): `model_id dessem-linear`, `contract_version 1`, `granularity_supported 60`, `native_layout dessem_semihourly_48`,
  `delivery_days 9` (O..O+8; o motor roda 10 dias, `horizon_days + 1`), `data_lag_days 0`, `model_time_offset_min 60` (a UNICA declaracao do
  deslocamento de hora; aplicado pelo despachante), `weather_resample hourly`, `uses_rng`, `history_infer_equals_train`, `engine _dessem-engine`.
- **Estagios e `requires`/`provides`** (assinatura canonica da fundacao): `ingestion` (requires `data.load.load_mwh`, `data.weather_observed.temperature`,
  `data.weather_forecast.temperature`, `data.holidays.prevcarga_code`; provides `frame.deck`); `feature_engineering` (`frame.deck` -> `feat.series`);
  `training` (`feat.series` -> `artifact.svm`); `inference_features` (`frame.deck`, `artifact.svm` -> `inf.feat`); `inference` e `forecast`
  (`artifact.svm`, `inf.feat` -> `forecast.y` / `forecast.f`).
- **`run.yaml`**: `split origin_scoped` (1 re-treino por origem, `train_origin = O`, ADR-0023), 60 min, `horizon_days 9`, `history_days {train 1825, infer 1825}`,
  `seed 42`, `modes [train, infer, forecast]`, `params {exogena: MED}` (variantes `MED`/`MAX`/`MXMN`).
- **Hora**: nenhuma aritmetica de hora no plugin nem no motor (principio 7). Hora incompleta descartada em toda serie, clima previsto incluido
  (divergencia deliberada do oraculo, `docs/estado/dessem_linear.md` DL.9).

## Layout nativo e hora

`dessem_semihourly_48`: `values` = 9 x 48 (O..O+8; o dia O+9 do motor fica fora da faixa). O deslocamento de modelagem de 60 min e do
despachante (`to_model_frame`/`to_display_frame`); o plugin nunca desloca.

## Desvios conscientes do oraculo

1. Hora incompleta descartada tambem na previsao de clima (so as ultimas ~3 h do ultimo dia entregue; intervalos nomeados em `docs/estado/dessem_linear.md`).
2. `sp-2024-11-15`: residuo numerico documentado de 0,446 MW (3e-5 rel; teto 0,5 MW) por 1 ulp em `mm`; as outras 8 unidades da paridade sao exatas.
3. Fonte unica de erro/aviso/fundacao por `globalenv()` interino (`dl_abort`, `dl_warn`, `dl_foundation`; pinado por teste), ate o loader expor a API.

## Gates x dono

Runner: `Rscript --vanilla models/dessem-linear/tests/run_all_gates.R --tier fast|parity|parity-full [--units smoke|foundation|fanout] [--format json]`.
Todo gate [PLUGINS] do tier passa (exit 0); `[DEFERRED Mx]` aparece como SKIP com o dono. Os packs por descoberta
(`scripts/tools/gates/*.R`: `[M5]` fachadas, `[M6]` LONG) entram sem editar o runner e nao constam desta tabela.

Nota: as linhas `[DEFERRED M5]` e `[DEFERRED M6]` da `gate_table` sao substituidas, no runner, pelos packs `[M5]` (`m5_facades.R`, M5.8) e `[M6]` (`m6_long.R`, M6.6) do mesmo dono; so `[DEFERRED M7]` e `[DEFERRED M8]` seguem como SKIP com dono.

| Gate (`gate_table`) | Gate do oraculo | Teste | Dono | Tier |
|---|---|---|---|---|
| `00h` | - | `test-dl-infer.R` | [PLUGINS] | fast |
| `descritor` | - | `test-dl-descriptor.R` | [PLUGINS] | fast |
| `ingestao` | - | `test-dl-ingest.R` | [PLUGINS] | fast |
| `nowcast_mm` | - | `test-dl-nowcast.R` | [PLUGINS] | fast |
| `treino_freeze` | - | `test-dl-train.R` | [PLUGINS] | fast |
| `inferencia` | - | `test-dl-infer.R` | [PLUGINS] | fast |
| `gate02_escala_mw` | `gate02` | `test-gates.R` | [PLUGINS] | fast |
| `gate04_exogena_params` | `gate04` | `test-contract.R` | [PLUGINS] | fast |
| `gate05_clima_honesto` | `gate05` | `test-weather.R` | [PLUGINS] | fast |
| `gate07_dataprev_o` | `gate07` | `test-gates.R` | [PLUGINS] | fast |
| `gate08_contrato` | `gate08` | `test-contract.R` | [PLUGINS] | fast |
| `gate09_dado_faltante` | `gate09` | `test-contract.R` | [PLUGINS] | fast |
| `gate10_modelo_vazio` | `gate10` | `test-robustez.R` | [PLUGINS] | fast |
| `gate11_clamp_cross` | `gate11` | `test-robustez.R` | [PLUGINS] | fast |
| `gate13_flag12` | `gate13` | `test-robustez.R` | [PLUGINS] | fast |
| `gate14_corte_intradiario` | `gate14` | `test-cut.R` | [PLUGINS] | fast |
| `gate15_sem_crash_13h` | `gate15` | `test-gates.R` | [PLUGINS] | fast |
| `gate16_cauda_curta` | `gate16` | `test-robustez.R` | [PLUGINS] | fast |
| `gate17_split` | `gate17` | `test-split.R` | [PLUGINS] | fast |
| `gate18_sem_treino_circular` | `gate18` | `test-train-honesty.R` | [PLUGINS] | fast |
| `corte_treino` | - | `test-train-honesty.R` | [PLUGINS] | fast |
| `mm_head` | - | `test-mm-head.R` | [PLUGINS] | fast |
| `robustez` | - | `test-robustez.R` | [PLUGINS] | fast |
| `sentinela` | - | `test-sentinel.R` | [PLUGINS] | fast |
| `grep` | - | `test-grep.R` | [PLUGINS] | fast |
| `no_env` | - | `test-no-env.R` | [PLUGINS] | fast |
| `portabilidade` | - | `test-portability.R` | [PLUGINS] | fast |
| `motor_incluso` | - | `test-motor.R` | [PLUGINS] | fast |
| `units_yaml` | - | `test-parity.R` | [PLUGINS] | fast |
| `runner_controle` | - | `test-runner.R` | [PLUGINS] | fast |
| `paridade` | - | `test-parity.R` | [PLUGINS] | parity |
| `paridade_control` | - | `test-parity.R` | [PLUGINS] | parity |
| `gate01_schema_long` | `gate01` | `test-deferred.R` | [DEFERRED M6] | fast |
| `gate06_ytrue_real` | `gate06` | `test-deferred.R` | [DEFERRED M6] | fast |
| `gate03_sem_crash` | `gate03` | `test-deferred.R` | [DEFERRED M7] | fast |
| `fachada` | - | `test-deferred.R` | [DEFERRED M5] | fast |
| `cross_os` | - | `test-portability.R` | [DEFERRED M8] | fast |
