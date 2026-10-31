# Plugin LGBM (membro `lgbm` do clean-room)

Porte R-only do modelo LightGBM semi-horario do oraculo `prevcargadessem3/models/lgbm`. Um modelo por area, split `origin_agnostic`
(ADR-0023), 30 min, H = 1 dia. O estado duravel do plugin mora em `docs/estado/lgbm.md`; este README e o mapa de leitura.
So ASCII. O plugin nao le variavel de ambiente, nao faz aritmetica de hora e nao conta corte: le tudo de `cuts` e de `run.yaml`.

## Mapa funcao -> arquivo:linha do oraculo

| Estagio (clean-room `models/lgbm/R/`) | Oraculo (`prevcargadessem3/models/lgbm/R/`) |
|---|---|
| `ingestion.R` `lgbm_ingest` / `lgbm_load_grid` (grade de 48 slots/dia) | `data_adapter.R:513-743` (`load_data`), `data_adapter.R:169-228` (`.read_load_raw`) |
| `ingestion.R` `lgbm_weather_vintage` (safra de clima por lead) | `data_adapter.R:229-318` (`.read_temperature_raw`), `data_adapter.R:130-150` (`.heat_area`) |
| `ingestion.R` `lgbm_holidays`, `register.R` `plugin_constants` (feriados fixos) | `data_adapter.R:319-339` (`.read_holidays`) |
| `helpers_calendar.R` (TipoDia, rotulos de fim de ano, dummies) | `data_adapter.R:386-443` (`.add_calendar_cols`), `feature_engineering.R:349-424` |
| `feature_engineering.R` `lgbm_build_wavelets` / `lgbm_add_wavelets` (Haar, janela 32) | `feature_engineering.R:124-196`, `inference_features.R:93-176` |
| `feature_engineering.R` `lgbm_compute_sazonalidade*` / `lgbm_merge_sazonalidade` | `feature_engineering.R:197-348` |
| `feature_engineering.R` `lgbm_split_cutoffs` / `lgbm_build_features` | `orchestrator.R:383-416`, `feature_engineering.R:425-690` (`build_features_train`) |
| `helpers_norm.R` (min-max so do treino) | `train_lgbm.R:93-140` (`.compute_min_max`, `.normalise_dt`), `inference.R:90-116` |
| `training.R` `lgbm_train` (CV blocos-semana + refit, holdout) | `train_lgbm.R:189-424` (`train_lgbm`) |
| `freeze.R` `lgbm_freeze` / `lgbm_thaw` (booster como texto, sha256) | `utils.R:327-413` (`save_artifact`, `load_artifact`) |
| `helpers_loess.R`, `inference_features.R` `plugin_inference_features` | `inference_features.R:177-412`, `inference.R:118-142` |
| `inference.R` `lgbm_stage_a` / `lgbm_stage_b` / `lgbm_delivery` (BLF de duas passadas) | `inference.R:200-475` (`infer_single_origin`) |
| `register.R` `lgbm_params_schema` / `lgbm_check_params` | `utils.R:88-200` (flags `LGBM_*`), `config.R:153-174` |
| despachante da fundacao (nao e do plugin) | `orchestrator.R:165-380` (`run_pipeline`) |

## Contrato do descriptor

`plugin_descriptor()` + `run.yaml` (loader P.0b, Tier A nos 5 estagios):

- `split: origin_agnostic`, `granularity_min: 30`, `horizon_days: 1`, `history_days {train 730, infer 45}`, `seed: 123`, `modes: [train, infer]`.
- `data_lag_days: 2` => `last_data_day = O-3` (o LGBM le carga so ate O-3; o horizonte H = 1 faz `train_origin = O-2`).
- `delivery_days: 2` (entrega de `available_until` ate a meia-noite de O+2) e `native_layout: lgbm_day_48`.
- Deck exigido: `data.load.load_mwh`, `data.weather_forecast.temperature`, `data.holidays.prevcarga_code`.
- Erros: ids `lgbm.*` em `scripts/lib/errors/lgbm.tsv` (classes contract/completude/window/integrity, mensagens ASCII).

## Contrato `requires` / `provides` (M5.12; `R/register.R`, `stages`)

- `ingestion`: `requires` = `lgbm_deck_tokens()` (carga, clima observado e previsto, feriados); `provides` = `frame`.
- `feature_engineering`: `frame` -> `features`. `training`: `features` -> `model`.
- `inference_features`: `frame`, `model` -> `inf_feat`. `inference`: `model`, `inf_feat` -> `envelope`.

## Layout `lgbm_day_48`

`values` = matriz 2 x 48 (linha 1 = dia O completado, linha 2 = O+1; colunas = slots de 30 min no rotulo de INICIO do intervalo).
A entrega (`delivery`) e a grade da fundacao (`prevcarga_delivery_grid`): `interval_end` de `available_until + 30 min` ate O+2 00:00
(78 slots com corte 09:00, 77 com 09:30). A comparacao de paridade usa `values` e `delivery` (day0 `native`).

## Tabela env flag -> param

O oraculo le estas variaveis de ambiente; o clean-room as le de `params` do `run.yaml` (efetivos no job do oraculo: `job.json`).

| Env flag do oraculo (`utils.R`) | Param `run.yaml` | Valor efetivo |
|---|---|---|
| `LGBM_DISABLE_LOESS` (`utils.R:91`) | `disable_loess` | `true` |
| `LGBM_CALENDAR_YEAREND` (`utils.R:134`) | `calendar_yearend` | `true` |
| `LGBM_TRAIN_CV` (`utils.R:163`) | `train_cv` | `true` |
| `LGBM_TIPODIA_LEGACY` (`utils.R:125`) | `tipodia_legacy` | `false` |
| `LGBM_1230_PREVESPERA` (`utils.R:153`) | `prevespera_1230` | `false` |
| `LGBM_CUTOFF_SCHEME` (`utils.R:113`) | `cutoff_scheme` | `v2_old` |
| `LGBM_SAZONALIDADE_SCOPE` (`utils.R:141`) | `sazonalidade_scope` | `train` (`full_window` vaza o horizonte e nao e portado) |

O artefato congelado carrega os params do treino; o infer os confere (`lgbm_check_artifact_params`) e nunca herda flag de processo.

## Cortes por fase

- Treino: `train_origin = O-2` (O-1-H, H = 1) e `last_data_day = O-3` (`data_lag_days: 2`); a carga entra so ate `last_data_day`.
  Um corte uniforme vazaria horizonte: o plugin le `cuts$train_origin` e `cuts$last_data_day`, nunca refaz a conta.
- Infer: carga real ate `available_until` (derivado do dado: O 09:30 => meia hora 09:00-09:30 presente); o resto de O e O+1 e sintetico.
- Clima: so emissoes com `issue_time <= weather_issue_limit` (regras `available_until` e `origin_midnight_utc`); o plugin so CONFERE.
- Dia da emissao da safra: `issue_day_tz = "UTC"` (como `as.Date(issue_time)` do oraculo); dia valido = dia civil no fuso da carga.

## Entrega O..O+1

Stage A prediz as 48 meias horas de O+1 a partir do perfil completado de O; Stage B completa o resto de O (BLF de duas passadas).
A entrega e O (apos o corte) e O+1, casada por posicao com a grade da fundacao. Sem carga real em O: so as razoes do Stage A, com aviso.

## Paridade (golden.lock)

Delta = 0 contra o oraculo (limiar 1e-5 MW, mesma plataforma): `values` 2 x 48 e `delivery` por `prevcarga_parity_compare`.
`models/lgbm/tests/parity/golden.lock` (versionado; o golden em `.state/golden` e gitignored) tem sha256 56915e70ca9871df94ebc33d3473a2b964663112ffee5f0c28cd9438e1307703.
Oraculo travado em `c4af7378cc767c46f95ce0f09d961fe84fb7f141`. Resultado do `--tier parity-full` do fechamento (LGBM.6): `--tier parity-full` exit 0 (smoke + foundation + fanout + controls; `paridade`, `paridade_control` e `gate_adequacao_add` PASS, Delta <= 1e-5 MW), tambem apos a troca de `force_tz`/`with_tz` por `make_datetime`/`as_date(tz=)`.

## Desvios conscientes do oraculo

1. Sem auto-deteccao de corte: o oraculo infere o corte dos dados; o plugin recebe `cuts` (dates.R) e so confere.
2. Sem fallback silencioso de feriados: `holidays` ausente aborta (`lgbm.contract`); a lista fixa 2022-2025 e so a base declarada em `plugin_constants()`.
3. Sem heranca de flags por env: os 7 flags `LGBM_*` viram `params`; o artefato carrega os do treino (`.inherit_adequacao_flags` some).
4. Rotulos de calendario na FE: `TipoDia`, `DiaSemanaNum` e fim de ano sao calculados por mapa fixo, sem `weekdays()` nem locale.
5. Sem re-extracao de ondaletas em O+1 como o oraculo faz: o plugin nao repete a extracao; a paridade Delta = 0 (`parity_construcao_d1`, `gate12_ondaletas_d1`) prova a equivalencia.

## Gates x dono

Runner: `Rscript --vanilla models/lgbm/tests/run_all_gates.R --tier fast|parity|parity-full [--units <grupo>] [--format json]`.
Todo gate [PLUGINS] do tier tem de passar (exit 0); [DEFERRED Mx] aparece como SKIP com o dono. Gates de packs por descoberta
(`scripts/tools/gates/*.R`, M5/M6/M7) entram sem editar o runner.

| Gate (`gate_table`) | Gate do oraculo | Teste | Dono |
|---|---|---|---|
| `contrato` | - | `test-contract.R` | [PLUGINS] |
| `ingest_cortes` | - | `test-ingestion.R` | [PLUGINS] |
| `ingest_safra` | - | `test-ingestion.R` | [PLUGINS] |
| `ingest_contrato` | - | `test-ingestion.R` | [PLUGINS] |
| `ingest_feriados` | - | `test-ingestion.R` | [PLUGINS] |
| `ingest_efeitos` | - | `test-ingestion.R` | [PLUGINS] |
| `gate_cal` | gate_cal | `test-features.R` | [PLUGINS] |
| `gate_saz` | gate_saz | `test-features.R` | [PLUGINS] |
| `gate12_ondaletas_d1` | gate12 | `test-features.R` | [PLUGINS] |
| `fe_loess` | - | `test-features.R` | [PLUGINS] |
| `gate_cut` | gate_cut | `test-features.R` | [PLUGINS] |
| `fe_schema` | - | `test-features.R` | [PLUGINS] |
| `fe_determinismo` | - | `test-features.R` | [PLUGINS] |
| `gate01_perturbacao` | gate01 | `test-training.R` | [PLUGINS] |
| `gate02_escala_mw` | gate02 | `test-training.R` | [PLUGINS] |
| `gate03_norm_train_only` | gate03 | `test-training.R` | [PLUGINS] |
| `gate_cv_refit` | gate_cv_refit | `test-training.R` | [PLUGINS] |
| `train_cortes` | - | `test-training.R` | [PLUGINS] |
| `gate04_decoupling` | gate04 | `test-portability.R` | [PLUGINS] |
| `thaw_integridade` | - | `test-portability.R` | [PLUGINS] |
| `higiene_artefato` | - | `test-portability.R` | [PLUGINS] |
| `loader_estagios` | - | `test-inference.R` | [PLUGINS] |
| `gate07_stage_a_48` | gate07 | `test-inference.R` | [PLUGINS] |
| `gate05_sem_crash` | gate05 | `test-inference.R` | [PLUGINS] |
| `parity_construcao_d1` | parity_construcao_d1 | `test-inference.R` | [PLUGINS] |
| `infer_contrato` | - | `test-inference.R` | [PLUGINS] |
| `gate_inf_adequacao` | gate_inf_adequacao | `test-params.R` | [PLUGINS] |
| `corte_intradiario` | - | `test-cut.R` | [PLUGINS] |
| `gate09_fuso` | gate09 | `test-delivery.R` | [PLUGINS] |
| `gate_nowcast_dplus0` | gate_nowcast_dplus0 | `test-delivery.R` | [PLUGINS] |
| `clima_honesto` | - | `test-weather.R` | [PLUGINS] |
| `clima_limite` | - | `test-weather.R` | [PLUGINS] |
| `clima_safra` | - | `test-weather.R` | [PLUGINS] |
| `gate10_determinismo` | gate10 | `test-determinism.R` | [PLUGINS] |
| `aditividade_epico` | - | `test-additivity.R` | [PLUGINS] |
| `higiene` | - | `test-additivity.R` | [PLUGINS] |
| `readme` | - | `test-additivity.R` | [PLUGINS] |
| `runner_controle` | - | `test-additivity.R` | [PLUGINS] |
| `units_yaml` | - | `test-parity.R` | [PLUGINS] |
| `paridade` | - | `test-parity.R` (tier parity) | [PLUGINS] |
| `paridade_control` | - | `test-parity.R` (tier parity) | [PLUGINS] |
| `gate_adequacao_add` | gate_adequacao_add | `test-parity.R` (tier parity) | [PLUGINS] |
| `gate11_ytrue_real` | gate11 | `test-deferred.R` | [DEFERRED M6] |
| `nowcast_dplus0_long` | gate_nowcast_dplus0 | `test-deferred.R` | [DEFERRED M6] |
| `fachada` | - | `test-deferred.R` | [DEFERRED M5] |
| `cross_os` | - | `test-portability.R` | [DEFERRED M8] |
