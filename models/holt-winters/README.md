# Plugin Holt-Winters (`models/holt-winters`)

Porte do oraculo `prevcargadessem3/models/holt-winters` (somente leitura) para o contrato de plugin da CLI `prevcarga`.
O estado duravel (decisoes, paridade, ids de erro) mora em `docs/estado/holt_winters.md`; este arquivo e o mapa e o contrato.

## Mapa funcao -> arquivo:linha do oraculo

| Funcao do plugin (`R/`) | Oraculo (`prevcargadessem3/models/holt-winters/R/`) |
|---|---|
| `plugin_ingest` e auxiliares (`ingestion.R`): `CargaTempVer`/`CargaTempMed`/`TempPre`/`CDDPre`, flag HV | `data_adapter.R:138-673` |
| `plugin_build_features` (`feature_engineering.R`): 7 passos de FE | `feature_engineering.R:256-599` |
| `helpers_aux.R`: `Corrige.Dias.Faltantes`, `Corrige.Dados`, `MO1`, `classifica_kmeans`, `Substitui.Feriados`, `Suaviza.LOESS`/`LOESS1`, `SemiFer` | `funcoes_aux.R:53-717` (unica copia; as de `helpers/` e `preprocessing/` sao shims) |
| `plugin_train` (`training.R`): 1 `forecast::hw` diario + 48 intervalares | `hw_training.R:304-549` |
| `plugin_inference_features` (`inference_features.R`): `efeitoTemp`, carga/temperatura do horizonte | `hw_inference_features.R:122-460` |
| `plugin_infer` / `plugin_forecast` (`inference.R`): SemiFer, LOESS 0,13, entrega | `hw_inference.R:226-662` |
| `plugin_freeze` / `plugin_thaw` (`freeze.R`): artefato de dado puro | `.rds` do oraculo (`hw_training.R`) |
| corte/janela do treino e da inferencia (despachante + `cuts`) | `jobs/run_job.sh:454-480` |

## Contrato

- **Descriptor** (`R/register.R`): `model_id holt-winters`, `contract_version 1`, `granularity_supported 30`, `native_layout hw_matrix_nx48`,
  `delivery_days 9` (O..O+8), `reads_day_O`, `uses_rng`, `history_infer_equals_train` FALSE, sem motor. Assinatura canonica dos 5 estagios.
- **`run.yaml`**: `split origin_scoped` (1 treino por origem O com serie ate O-1; ADR-0023), 30 min, `horizon_days 8`, `history_days {train 730, infer 45}`,
  `seed 1234`, `modes [train, infer, forecast]` e os 7 `params` (`kmeans_seed`, `ic`, `perfil_mo`, `classes_mo`, `similar`, `feriado`, `suaviza`) sem default no codigo.
- **Envelope** (inferencia): `model_id`, `variant` NA, `layout hw_matrix_nx48`, `values` (8 x 48 = O+1..O+8), `delivery` (`interval_end`, `y_pred`) a partir de
  `available_until + 30 min` ate O+9 00:00, `meta`. O artefato carimba `load_label`, `timezone`, `weather_issue_cutoff`, `split`, `granularity_min`, `seed`,
  `params`, `train_origin`, `origin_day` (despachante P.0c/P.0d) e nao tem relogio, caminho, objeto `forecast`/`ets`, `call`, closure nem environment.
- **Layout `hw_matrix_nx48`**: linhas = dias, 48 colunas = H01..H48 (H01 = 00:00-00:30, por `interval_start`). O `mean_interval` do artefato e 9 x 48.

## Layout nativo e hora

O plugin nao faz aritmetica de hora (princ. 7). Unica excecao declarada: o `+ hours(6)` do flag historico de horario de verao em `.hw_compute_hv`
(rotulo, nao deslocamento de frame). Datas por lubridate com tz explicito; sem variaveis de ambiente.

## Desvios conscientes do oraculo

- O dia O entregue vem do passo `$mean[1]` que o oraculo descarta; o artefato guarda so `$mean` (9 passos), nunca objetos `forecast`.
- `datafimver` estrito (`last_data_day = O-1`) em vez do `align_first_idx` data-driven do oraculo.
- FE 1x (o oraculo rodava 2x); `h_fit` fixo em 9.
- `efeitoTemp` do dia O usa `qdata` proprio (nao contamina O+1).
- `efeitoFer = efeitoSFer = 0` (como o oraculo, explicito).
- As 3 copias de `funcoes_aux.R` viram um unico `helpers_aux.R`; `classifica_kmeans` semeia com `params.kmeans_seed` (o oraculo fixava 123).
- Ramo `perfil_mo = ST` sem gate de paridade (so teste de estrutura).

## Medicao do seed

`seed` 1234 -> 1235 e medido (`max|delta values|`) por `[PLUGINS] seed_medicao` e gravado em `tests/parity/seed_measure.json` (gerado, gitignored), sem
assercao de Delta: o `hw()` e MLE e o MO1 re-semeia o RNG com `params.kmeans_seed`. `kmeans_seed` 123 -> 7 muda o sha256 do artefato (CONTROL do `determinismo`).

## Gates x dono

`Rscript --vanilla models/holt-winters/tests/run_all_gates.R [--tier fast|parity|parity-full] [--format text|json]` (so compoe `prevcarga_gates_run`;
descobre os packs de `scripts/tools/gates/*.R` por `applies_to`).

| Gate do oraculo | Gate | Dono |
|---|---|---|
| gate01 | `corte_treino` | [PLUGINS] |
| gate02 | `contrato_entrada` | [PLUGINS] |
| gate03 | `sem_crash_1o_mes` (tier parity) | [PLUGINS] |
| gate04 | `schema_long` | [DEFERRED M6] |
| gate05 | `higiene_grep` (+ `higiene_env`) | [PLUGINS] |
| gate06 | `determinismo` (+ `seed_medicao`, `sem_seed_literal`) | [PLUGINS] |
| gate07 | `ytrue_real` | [DEFERRED M6] |
| gate08 | `clima_honesto` | [PLUGINS] |
| gate09 | `clima_fallback` | [PLUGINS] |
| gate10 | `harness_parity` | [DEFERRED M7] |
| gate11 | `sem_deslocamento` (tier parity) | [PLUGINS] |
| - | `fachada` | [DEFERRED M5] |
| - | `portabilidade_cross_os` | [DEFERRED M8] |
| - | `registro`, `catalogo_hw`, `ingest_*`, `fe_*`, `train_*`, `freeze_*`, `infer_*`, `paridade*`, `sentinela`, `mesmo_dia`, `corte_intradiario`, `infer_igual_forecast`, `artefato_puro`, `aditividade`, `runner_controle` | [PLUGINS] |
