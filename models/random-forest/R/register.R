## register.R -- descriptor, schema de params e constantes do plugin random-forest (RF.1). SO definicoes (o loader recusa
## codigo de topo). Porte de prevcargadessem3/models/random-forest (somente leitura). O plugin NAO le variavel de ambiente
## nem relogio; `ntree` e `seed` vem do run.yaml (nenhum default aqui). Nenhuma aritmetica de hora: o rotulo da carga e do
## leitor unico e a agregacao horaria e de to_hourly (features.R). Mensagens e saidas: ASCII puro.

## --- Acesso as primitivas da fundacao (errors.R, features.R) ---------------------------------------------------
## O env do plugin so enxerga base + pacotes anexados (loader P.0b): as funcoes da CLI vivem no ambiente global da
## fachada. Busca primeiro nos envs encadeados ao plugin (testes) e, na falta, no global (CLI). Interino, como o
## engine_abort() do motor DESSEM (DL.1) e o .lgbm_foundation do LGBM.1; sai quando o loader expuser a API.
.rf_foundation <- function(name) {
  f <- get0(name, envir = environment(), mode = "function")
  if (is.null(f)) f <- get0(name, envir = .GlobalEnv, mode = "function", inherits = FALSE)
  if (is.null(f)) stop(sprintf("[random-forest] funcao da fundacao ausente: %s", name), call. = FALSE)
  f
}

## Aborto classificado pelo catalogo da fundacao (ids plugin.*); `where` padrao = estagio de ingestao.
.rf_abort <- function(id, details = NULL, where = "random-forest/ingestion")
  .rf_foundation("prevcarga_abort")(id, where = where, details = details)

## --- Constantes intrinsecas do oraculo (nao sao params): dado nomeado, nenhum numero solto nos estagios ----------
plugin_constants <- function() list(
  horizon_days_native = 9L, n_leads = 216L,
  ## horas do dia local (0..23) das marcas de pico do oraculo; `day_daily_peak` usa 9:17 (estatistica por dia)
  daily_peak_hours = 9:18, evening_peak_hours = 18:23, minimum_hours = 0:8, daily_peak_stat_hours = 9:17,
  importance = TRUE, no_holiday_days = 365,
  ## janelas de Natal (12-23..12-29) e de Ano-Novo (12-30..12-31 e 01-01..01-04) como (mes, dia) inicial e final
  christmas_window = list(start = c(12L, 23L), end = c(12L, 29L)),
  newyear_windows = list(list(start = c(12L, 30L), end = c(12L, 31L)), list(start = c(1L, 1L), end = c(1L, 4L))),
  ## feriados legados 2022-2025 (data_adapter.R:320-345, 69 datas); datas civis, sem deslocamento
  holidays_legacy = lubridate::ymd(c(
    "2022-01-01", "2022-02-28", "2022-03-01", "2022-04-15", "2022-04-21", "2022-05-01",
    "2022-06-16", "2022-06-17", "2022-09-07", "2022-10-12", "2022-11-02", "2022-11-15",
    "2022-12-24", "2022-12-25", "2022-12-26", "2022-12-31", "2023-01-01", "2023-02-20",
    "2023-02-21", "2023-02-22", "2023-06-08", "2023-06-09", "2023-09-07", "2023-09-08",
    "2023-10-12", "2023-10-13", "2023-10-28", "2023-11-02", "2023-11-03", "2023-11-15",
    "2023-12-24", "2023-12-25", "2023-12-26", "2023-12-31", "2024-01-01", "2024-02-12",
    "2024-02-13", "2024-02-14", "2024-03-29", "2024-04-21", "2024-05-01", "2024-05-30",
    "2024-05-31", "2024-09-07", "2024-10-12", "2024-11-02", "2024-11-15", "2024-11-20",
    "2024-12-24", "2024-12-25", "2024-12-26", "2024-12-31", "2025-01-01", "2025-03-03",
    "2025-03-04", "2025-03-05", "2025-04-18", "2025-04-21", "2025-05-01", "2025-06-19",
    "2025-06-20", "2025-09-07", "2025-10-12", "2025-11-02", "2025-11-15", "2025-11-20",
    "2025-12-24", "2025-12-25", "2025-12-31"), quiet = TRUE, tz = NULL))

## Schema do unico param (sem default: o run.yaml e a unica fonte).
rf_params_schema <- function() list(ntree = list(type = "integer", min = 1L))

## Tokens exigidos do deck pela ingestao (data.<dataset>.<coluna>, contrato do leitor; a carga chega com
## interval_start/interval_end do leitor, conferidos em .rf_check_panel).
rf_deck_tokens <- function() c("data.load.load_mwh", "data.weather_forecast.issue_time",
                               "data.weather_forecast.valid_time", "data.weather_forecast.temperature",
                               "data.holidays.date")

## Estagio de ingestao despachado pela fundacao: assinatura canonica (panel, cuts). O `spec` (history_days, horizon_days)
## e a fase vem em cuts$spec / cuts$phase; o despachante P.0c ainda nao os repassa (discovery do RF.1).
rf_stage_ingest <- function(panel, cuts) {
  spec <- cuts$spec
  if (is.null(spec)) .rf_abort("plugin.contract", details = list(reason = "cuts$spec ausente"))
  plugin_ingest(panel, cuts, spec)
}

plugin_descriptor <- function() list(
  model_id = "random-forest", contract_version = 1L,
  granularity_supported = 60L, modes_implemented = c("train", "infer"),
  params_schema = rf_params_schema(), variant_param = NA_character_,
  native_layout = "rf_wide_hourly_216", semihourly_rule = rf_semihourly_rule,
  weather_resample = "raw",                 # tmax/tmin diarios sobre as leituras de 30 min (oraculo data_adapter.R:262-275)
  delivery_days = 9L, data_lag_days = 0L,   # last_data_day = O-1 (ADR-0023)
  reads_day_O = FALSE,                      # carga so com dia < O (oraculo data_adapter.R:80)
  weather_forecast_in_train = TRUE,         # origens de treino usam a emissao da origem (oraculo data_adapter.R:84-85)
  uses_rng = TRUE, history_infer_equals_train = FALSE,
  packages = c("randomForest", "data.table", "lubridate"),
  freeze = rf_freeze, thaw = rf_thaw,
  stages = list(
    ingestion = list(fn = rf_stage_ingest, requires = rf_deck_tokens(), provides = "rf_frame"),
    feature_engineering = list(fn = rf_feature_engineering, requires = "rf_frame", provides = "rf_features"),
    training = list(fn = rf_training, requires = "rf_features", provides = "rf_artifact"),
    inference_features = list(fn = rf_inference_features, requires = c("rf_frame", "rf_artifact"),
                              provides = "rf_inf_features"),
    inference = list(fn = rf_inference, requires = c("rf_artifact", "rf_inf_features"),
                     provides = "member_envelope")))
