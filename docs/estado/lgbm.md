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
