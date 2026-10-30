# Estado: plugin LGBM (epico Plugin LGBM) -- LGBM.1

Escopo do LGBM.1: contrato (`run.yaml` + `register.R`) e ingestao. FE, treino e infer sao LGBM.2-LGBM.4.

- `models/lgbm/run.yaml`: split `origin_agnostic`, 30 min, H=1, `history_days {train 730, infer 45}`, seed 123, modes `[train, infer]` e os 7 `params` que
  substituem as env flags `LGBM_*` do oraculo (efetivos: disable_loess, calendar_yearend, train_cv = true; tipodia_legacy, prevespera_1230 = false;
  cutoff_scheme `v2_old`; sazonalidade_scope `train`, `full_window` nao portado porque vaza). O pacote nao le env; params sem default no codigo.
- `R/register.R` (so definicoes): `plugin_descriptor()` (`data_lag_days = 2` => `last_data_day = O-3`, ADR-0023; `delivery_days = 2`; `native_layout lgbm_day_48`),
  `lgbm_params_schema()`/`lgbm_check_params()` (erro `lgbm.params` nomeando a chave), `plugin_constants()` (hiperparametros, spans, wavelet 32, `issue_day_tz = "UTC"`,
  `heat_area`, `holidays_fixed` 2022-2025, `min_history_days_infer = 4`), `lgbm_abort()` (ids `lgbm.*` em `scripts/lib/errors/lgbm.tsv`).
  O loader exige funcoes de assinatura canonica e `freeze`/`thaw`: os estagios dos tickets seguintes sao despachados POR NOME
  (`plugin_build_features`, `plugin_train`, `plugin_inference_features`, `plugin_infer`, `lgbm_freeze`, `lgbm_thaw`); ate existirem, `lgbm.stage_missing`.
  O loader ja registra a cadeia Tier A dos 5 estagios (provado em `test-contract.R`).
- `R/ingestion.R`: `plugin_ingest(panel, cuts)` (assinatura do loader; o modo sai do painel) e `lgbm_ingest(panel, cuts, spec, mode, consts)` (forma completa).
  So CONFERE cortes (`lgbm_check_panel`): treino alem de `cuts$last_data_day`, infer alem de `cuts$available_until` ou emissao alem de `weather_issue_limit` => `lgbm.contract`.
  Grade de 48 slots/dia (`DataHora = interval_start`, sem aritmetica de rotulo; principio 7), dia incompleto => `lgbm.completude` (exit 3), historia curta => `lgbm.window` (exit 5).
  Infer: `is_synthetic` = sem intervalo ou `interval_end > available_until`; sem auto-deteccao. Safra de clima do oraculo (`lgbm_weather_vintage`: lead 0 no historico e em O,
  lead 1 em O+1, emissao mais recente por slot, dia da emissao em UTC). Feriados = `prevcarga_code >= 1` + lista fixa, em `attr(frame, "feriados")`; dataset ausente => contract.
- Acesso a errors.R: o env do plugin so ve base; `.lgbm_foundation()` busca `prevcarga_condition` nos envs encadeados e, na falta, no ambiente global da CLI (interino, como o `engine_abort` do DL.1).
- Testes: `models/lgbm/tests/testthat/` (`helper-lgbm.R`, `test-contract.R`, `test-ingestion.R`); `testthat::test_dir("models/lgbm/tests/testthat")`.
- Pacotes instalados no R 4.5.3 por este ticket: lightgbm 4.7.0, wavelets 0.3.0.2, zoo 1.9.1 (binarios do Posit PM).

## LGBM.2 -- feature engineering

- Arquivos: `R/helpers_calendar.R` (`lgbm_add_calendar_cols`, `lgbm_tipodia`, `lgbm_fimano_label`, `lgbm_add_fimano_dummies`, `lgbm_adequacao_extra_features`),
  `R/helpers_loess.R` (`lgbm_loess_daily` grau 2, `lgbm_loess_smooth` grau 1; `disable_loess` por argumento, nunca env), `R/feature_engineering.R`
  (ondaletas Haar 32, Sazonalidade + fallbacks + merge, `lgbm_split_cutoffs`, `lgbm_input_cols`, `lgbm_build_features(frame, spec, cuts)`, `plugin_build_features(frame, params)`).
- Os rotulos de calendario nascem na FE (nao na ingestao): o infer nao depende do spec. `FimDeSemana = DiaSemanaNum %in% c(1,2)` (mapa legado Dom=1, Sab=2), como o oraculo.
  Com `tipodia_legacy`, a chave rica exige `calendar_yearend` (senao `lgbm.params`).
- Assinatura do loader: o estagio recebe `(frame, params)`. Os cortes (`last_data_day`, `train_origin`) chegam em `attr(frame, "cuts")` (anexado por `lgbm_ingest`);
  o seed nao chega ao estagio (o treino recebe `spec$seed`); a FE nao consome RNG. `lgbm_build_features(frame, spec, cuts)` e a forma completa e faz `set.seed(spec$seed)`
  dentro de `prevcarga_preserve_rng`.
- Shift CarWav de 48 h = chave de calendario `(Data + carwav_shift_days, Hora)` (sem aritmetica de hora). Janela com NA vira linha NA (o oraculo errava); `rollapply(fill = NA)`.
- Sazonalidade so de `Data <= train_cutoff` com carga, e calculada sobre as linhas apos o descarte de lag (como o oraculo). Retorno: lista do oraculo (`feature_table`, `sazonalidade`,
  `sazonalidade_fallbacks`, `colunas_entrada_BLF`, `colunas_nao_normalizar`, `train_cutoff`, `val_cutoff`). Com `tipodia_legacy`, `Feriado` vira 0/1 no `feature_table`.
- `lgbm_split_cutoffs`: `train_cv` => ambos `last_data_day`; v2_old/legacy_month/rolling replicam `default_val/train_cutoff` (v2_old: treino = fim do mes anterior ao de `val`); corte > `last_data_day` => `lgbm.contract`.
- Custo: o `rollapply` de `dwt` e ~40 s por 36k linhas (igual ao oraculo); os testes usam deck de 120 dias.
- INTERIM (decisao do Gabriel): `attr(frame, "cuts")` (anexado por `lgbm_ingest`) e o canal provisorio dos cortes para a FE, ja que o loader fixa o estagio em `(frame, params)`; o seed fica so no treino. O canal definitivo (F1) sera decidido apos a implementacao; nao mexer no despachante nem em `register.R` ate la.

## LGBM.3 -- treino, normalizacao e freeze do booster

- Arquivos: `R/helpers_norm.R` (`lgbm_compute_min_max` so de `Data <= train_cutoff`, `lgbm_normalise_dt`, `lgbm_norm`/`lgbm_denorm` para o infer com o `min_max_BLF` do artefato),
  `R/training.R` (`lgbm_cv_folds`, `lgbm_train_cv`, `lgbm_train_holdout`, `lgbm_train(features, spec, cuts)` e o estagio `plugin_train(features, params, seed)`),
  `R/freeze.R` (`lgbm_freeze(model)`, `plugin_freeze(model, spec, cuts, ctx)`, `lgbm_thaw(payload)`, `plugin_thaw(artifact, cuts)`).
- Assinaturas: o loader fixa `training = (features, params, seed)`, `freeze = (model)`, `thaw = (payload)`. `plugin_train`/`lgbm_freeze`/`lgbm_thaw` sao as formas despachadas
  (por nome, via `register.R`); `lgbm_train`, `plugin_freeze` e `plugin_thaw` sao as formas completas (spec + cuts) do contrato do ticket. O despachante carimba o payload de `lgbm_freeze`;
  `plugin_freeze` ja devolve o artefato plano carimbado (`prevcarga_plugin_stamp`) e varre a pureza.
- INTERIM (F1 adiado, decisao do Gabriel): os cortes chegam ao treino por `attr(features, "cuts")` (anexado por `lgbm_build_features`, mesmo canal do `attr(frame, "cuts")` da FE). O plugin nao recalcula
  `last_data_day`: so confere que nenhuma linha da `feature_table` nem corte interno passa dele (`lgbm.contract`).
- `train_cv: true` => CV blocos-semana (4 folds de 7x48 linhas round-robin, index-based) + refit em 100% das linhas com `nrounds = best_iter`; `lgb.cv`/`lgb.train` com `seed = spec$seed`,
  `num_threads = 1`, `deterministic`, `force_row_wise`. `train_cv: false` => holdout pelos cortes internos de LGBM.2 (`valids` + early stopping 50; diverge do oraculo so no determinismo: seed, `num_threads = 1`, `deterministic`, `force_row_wise`, pois o OpenMP padrao nao e reproduzivel e ~150x mais lento sob contencao); sem linhas de validacao => contract.
  Holdout `v2_old` exige historia anterior ao corte de treino (2 meses antes da validacao): deck de teste de 170 dias.
- `set.seed(spec$seed)` na entrada, dentro de `prevcarga_preserve_rng`. gate02: lag com max <= 1 => `lgbm.double_norm` (contract). `min_max_BLF` inclui o alvo `CargaGlobal`.
- Freeze: `booster_txt = save_model_to_string(num_iteration = NULL)` (ASCII, LF), `booster_sha256` (digest sha256 do texto), tabelas como `data.frame`, cortes ISO, `engine_version$lightgbm`; sem `lgb.Booster`, caminho ou relogio.
  Thaw: versao do lightgbm (`lgbm.engine_version`, contract) -> sha256 -> `lgb.load(model_str)` -> arvores == `best_iter` (`lgbm.integrity`, exit 9). Cross-OS do booster: `[DEFERRED M8]`.
- Racional do holdout pinado (aprovado): o default de producao do oraculo e CV (TRAIN_CV_DEFAULT TRUE, ja pinado); o holdout e so o kill-switch, entao o pin nao toca o caminho de paridade. Principio 2 exige determinismo.
- Erros novos em `scripts/lib/errors/lgbm.tsv`: `lgbm.double_norm`, `lgbm.engine_version`, `lgbm.integrity`.
- Testes: `models/lgbm/tests/testthat/` (`helper-lgbm-train.R`, `test-training.R`, `test-portability.R`).

## LGBM.4 -- inferencia BLF de duas passadas (G-ART)

- Arquivos: `R/inference_features.R` (`lgbm_check_artifact_params`, `plugin_inference_features(frame, artifact, cuts = NULL)`), `R/inference.R` (`lgbm_stage_a`, `lgbm_stage_b`,
  `lgbm_predict_d1`, `lgbm_delivery`, `plugin_infer(artifact, inf_feat, cuts)`). Despachados por nome pelo `register.R` (nada mudou la); `lgbm.stage_missing` so se a definicao sumir.
- G-ART: o infer so le o artefato (params, seed, min_max_BLF, sazonalidade + fallbacks, best_iter, booster). Nenhum `params`/`spec` nas assinaturas, nenhum refit, nenhuma env var
  (o `Sys.setenv(LGBM_DISABLE_LOESS)` e o `.inherit_adequacao_flags` do oraculo viraram `lgbm_check_artifact_params`: params fora do schema => `lgbm.params`; `tipodia_legacy` sem
  fallbacks, extra de adequacao fora de `colunas_entrada_BLF` (ou o inverso), campo ausente => `[prevcarga/contract] lgbm: artefato inconsistente: ...`). `set.seed(artifact$seed)` dentro de
  `prevcarga_preserve_rng`, na posicao do oraculo (antes do 1o LOESS).
- `plugin_inference_features` devolve `list(dt_features, dt_D, dt_D1, n_real_D, area)`: calendario/LOESS/lag/ondaletas/dummies como o treino (mesmas funcoes de LGBM.2), Sazonalidade so do
  artefato, sem `na.omit` (as 48 linhas de O e O+1 sao preservadas; dia sem 48 => `lgbm.completude`; historia curta => `lgbm.window`). `area` e extra ao contrato do ticket: o envelope exige a area.
- Corte real: `is_synthetic` da ingestao (`interval_end > available_until`), sem somar minutos no plugin. Stage B: `real = !is.na(CargaGlobal) & !is_synthetic`.
  CORRECAO na ingestao (LGBM.1): `is_synthetic` agora vem do `interval_end` do slot = `DataHora` do slot seguinte (o ultimo = meia-noite civil seguinte); antes um intervalo AUSENTE antes
  do corte (`interval_end` NA) virava sintetico em silencio e o infer o completava por razao. Agora e furo => `lgbm.completude` (exit 3).
- Passada 1: Stage A (48 previsoes normalizadas; matriz != 48 linhas => contract gate07), Stage B (carga real normalizada pelo `min_max_BLF`, depois `prev * round(pred[i]/pmax(abs(pred[i-1]),1e-9), 7)`;
  sem carga real em O => so razoes + aviso ASCII; perfil MW suavizado por `lgbm_loess_daily(span_lag, disable_loess do artefato)`). Passada 2: so o lag `CargaGlobal_anterior_BLF` de O+1 e
  sobrescrito pelo dia O completado (CONSERTO A: ondaletas/Sazonalidade/dummies ficam como vieram); LOESS inline 0.1 grau 1 sobre o normalizado e 0.15 por dia sobre o MW.
  Feature do artefato ausente na matriz => contract (nunca 0); `y_pred` nao finito => contract.
- Entrega: `delivery = (interval_end, y_pred)` identica a `prevcarga_delivery_grid(cuts, 2)` (78/77/48 linhas para available_until 09:00/09:30/O+1 00:00). O `y_pred` de cada `interval_end` e casado
  por POSICAO: o `interval_end` do slot k e o `DataHora` do slot k+1 (o ultimo, a meia-noite de O+2); sem aritmetica de hora. `values`: 2x48 (linha 1 = dia O completado, linha 2 = O+1);
  com dia O cheio, 1x48 = so O+1 (o que e entregue). O trecho de O da entrega vem do perfil completado (com o mesmo LOESS do lag quando `disable_loess = false`; com o default do run.yaml, `true`, e cru).
  Envelope sem `forecast_origin`/`lead`/`d_plus`/`y_true` (M6); `meta = list(best_iter, params_source = "artifact", n_real_D)`.
- Canal de cortes: o estagio recebe `(frame, payload)` e `(payload, inf_features, cuts)`; nao ha `attr(frame, "cuts")` novo (F1 adiado). `cuts` opcional em `plugin_inference_features` so confere a origem.
- Fidelidade ao oraculo: `real_mask` `DataHora <= avail_ts` do oraculo equivale a `interval_end <= available_until` do novo contrato porque o oraculo recebia `avail_ts` = ultimo `DataHora` real
  (rotulo de inicio; ADR-0019/M5.10). A paridade A/B numerica do infer (Delta = 0 vs `prevcargadessem3/models/lgbm`) e o ticket LGBM.5.
- Testes: `test-inference.R` (registro Tier A x2/memo, CONTROL `params`, gate07/05/12, parity_construcao_d1, window/contract/completude, Stage B sem carga), `test-params.R`, `test-cut.R`
  (honesty_gate 4 bracos + CONTROL 09:00-09:30 + granularidade 09:00 x 09:30 + 12:00/16:00), `test-delivery.R` (grade e sentinela Hora 17). Helpers novos em `helper-lgbm-train.R`
  (`lgbm_with_cli`, `lgbm_fx_origin/infer/ragged/base/at`); `helper-lgbm.R` ganhou `sentinel` em `lgbm_fx_deck`. O ticket citava `prevcarga_plugin_infer("lgbm", panel, cuts, spec, ctx)`; a
  assinatura real e `prevcarga_plugin_infer(plugin, artifact, panel, cuts)` e o plugin vem do loader.

## LGBM.5 -- paridade vs oraculo (Delta = 0), units.yaml, oracle_driver, golden.lock

- Arquivos: `models/lgbm/tests/parity/{units.yaml, topology.yaml, oracle_driver.R, golden.lock}` e `tests/testthat/test-parity.R`. O golden (`.state/golden`, gitignored) so e regenerado por
  `Rscript --vanilla scripts/tools/plugin_golden.R --model lgbm --units models/lgbm/tests/parity/units.yaml --oracle <clone limpo do oraculo> --data <store> --out .state/golden`
  (R 4.5.3, lightgbm 4.7.0 nos dois lados; cada unidade roda 2x e as saidas tem de ser identicas, senao exit 12). `topology.yaml` declara SECO/SP/RS/PA/BAOE como folhas (so para o leitor da paridade; nao e a topologia do produto).
- Oraculo: `/opt/ons-source/prevcargadessem3` tem untracked em `docs/roadmap/` e `plugin_golden.R` recusa oraculo sujo (`tools.oracle_dirty`); o golden roda de um `git clone` limpo do HEAD (mesmo commit) em tempdir e o oraculo real fica intocado (`git status --porcelain` identico antes e depois).
- Unidades (`units.yaml`, esquema P.0f): treino do oraculo com `--origin = O-2` (= `train_origin` do clean-room, O-1-H, H=1) e `history_start = O-2-730`, lendo carga so ate O-3 = `last_data_day` (`data_lag_days: 2`); infer em O com `history_start = O-45` e
  `available_until = O 08:30`; `oracle_cut: start_label` => o gerador passa `--available-until = O 08:00` (rotulo de inicio do ultimo intervalo real); `day0_source: native`; `weather_issue_cutoff: origin_midnight_utc`; `oracle_env` = job.json do oraculo
  (`LGBM_DISABLE_LOESS/CALENDAR_YEAREND/TRAIN_CV = "1"`), so no processo filho; `data_areas` = area + heat_area. Grupos: `smoke` (SECO 2024-10-02, RS 2024-07-01 = 1o do mes, SP 2024-12-24 = dez/24), `foundation` (SECO x outubro/2024: 01, 08, 15, 22, 29),
  `fanout` (SP, RS, PA, BAOE x 2024-07-15) e `controls` (aditividade: `oracle_env LGBM_TRAIN_CV "0"` x clean-room `train_cv: false`). `rules` de topo confere O-2 / O-732 / O-45, `day0_source native` e `seed 123` em toda unidade.
- `oracle_driver.R` (filho do `plugin_golden.R`): `run_pipeline("train", ...)` e `run_pipeline("infer", ..., available_until)` do `orchestrator.R` do oraculo (nunca do `run_pipeline.R`, cujo `.main()` roda em sessao nao interativa), artefato em scratch; `long.parquet` = saida do CLI do
  oraculo (`run_pipeline.R --mode infer --out`) sobre o MESMO artefato. `native.rds = list(layout "lgbm_day_48", values 2x48 [linha O = attr(pred,"completed_D")$mw; linha O+1 = pred$y_pred], completed_D, d1, meta$artifact_tables)`.
  `meta$artifact_tables` (sazonalidade + fallbacks, min_max_BLF, colunas, best_iter e os rotulos de calendario do quadro de infer do oraculo) vive DENTRO de `native.rds` porque `plugin_golden.R` so publica `native.rds` e `long*.parquet`
  (o ticket citava um `artifact_tables.rds` a parte; nao existe no contrato do gerador). `PREVCARGA_MODEL_R_DIR` e o contrato do proprio oraculo e e setado so neste filho.
- `test-parity.R`: o clean-room roda EM PROCESSO pelo despachante (`prevcarga_plugin_train` -> freeze/stamp -> `prevcarga_plugin_infer` com thaw) com os cortes literais da unidade e o painel do leitor (overlay de symlinks do store real + `manifest.json`, `truncate_at` = available_until da unidade);
  compara por `prevcarga_parity_compare` (layout `lgbm_day_48`, `day0_source native`, limiar 1e-5 MW): `values` 2x48, `delivery` x `long.parquet` (`interval_end = target_datetime + 30 min`), `cut_ok`, gate02 (`min_max_BLF$CargaGlobal["max"] > 1000`) e, por unidade,
  G-CAL (rotulos de calendario do quadro de infer, identicos) e G-SAZ (Sazonalidade, fallbacks, min_max_BLF, colunas e `best_iter` do artefato, <= 1e-9). Golden ausente, digest de dados ou fingerprint divergente => `fail()` com
  "golden ausente ou divergente: regere com scripts/tools/plugin_golden.R --model lgbm" (nunca skip). CONTROLs com dente: `train_cv: false` contra o golden de CV acusa > 1e-5; aditividade (`oracle_env LGBM_TRAIN_CV "0"` x `train_cv: false`) <= 1e-5; o pai nunca ganha `LGBM_*`.
- Rotulos dos testes (linhas da tabela do `run_all_gates.R`, que e do LGBM.6): `units_yaml` (fast), `paridade`, `paridade_control`, `aditividade` (tier parity; foundation/fanout so em parity-full).
