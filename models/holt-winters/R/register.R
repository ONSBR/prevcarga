## register.R -- descriptor, schema de params e constantes do plugin Holt-Winters (HW.1). SO definicoes (o loader recusa
## codigo de topo). Porte do oraculo prevcargadessem3/models/holt-winters (somente leitura). split, seed, horizonte e
## janelas vivem so no run.yaml; o descriptor nao os repete. O plugin nao le variavel de ambiente nem relogio.
## Ids de erro do plugin: scripts/lib/errors/holt_winters.tsv (prefixo `hw.`: o catalogo aceita so ids de 2 segmentos).
## Saidas e mensagens: ASCII puro.

## --- Acesso as primitivas da fundacao (errors.R) -----------------------------------------------------------------
## O env do plugin so enxerga base + pacotes anexados (loader P.0b): as funcoes da CLI vivem no ambiente global da
## fachada. Busca primeiro nos envs encadeados ao plugin (testes) e, na falta, no global (CLI). Interino, como o
## engine_abort() do motor DESSEM (DL.1) e o lgbm_abort() do LGBM.1.
.hw_foundation <- function(name) {
  f <- get0(name, envir = environment(), mode = "function")
  if (is.null(f)) f <- get0(name, envir = .GlobalEnv, mode = "function", inherits = FALSE)
  f
}

## Aborto classificado: id do catalogo; `reason` (ASCII) nomeia a coluna/intervalo concreto na mensagem.
hw_abort <- function(id, reason = NULL, details = NULL, where = "holt-winters/ingestion") {
  mk <- .hw_foundation("prevcarga_condition")
  if (is.null(mk)) stop(sprintf("[%s] holt-winters: %s", id, reason), call. = FALSE)
  cond <- mk(id, where = where, details = details)
  if (!is.null(reason)) cond$message <- paste0(cond$message, ": ", reason)
  stop(cond)
}

## Aviso classificado (nao interrompe): vira warning de classe prevcarga_warning.
hw_warn <- function(id, reason = NULL, details = NULL, where = "holt-winters/ingestion") {
  mk <- .hw_foundation("prevcarga_condition")
  if (is.null(mk)) return(warning(sprintf("[%s] holt-winters: %s", id, reason), call. = FALSE))
  cond <- mk(id, where = where, details = details, type = "prevcarga_warning")
  if (!is.null(reason)) cond$message <- paste0(cond$message, ": ", reason)
  warning(cond)
}

## --- Constantes intrinsecas do modelo (nao sao tunaveis) ----------------------------------------------------------
plugin_constants <- function() list(
  loess_span_out  = 0.13,                          # Suaviza.LOESS1 pos-infer (hw_inference.R:379)
  loess_span_temp = 0.3,                           # Suaviza.LOESS de T01..T48 (feature_engineering.R:537)
  semifer_floor   = lubridate::ymd("2019-01-01"),  # piso do arqCarga do SemiFer (hw_training.R)
  ts_frequency    = 7L,                            # frequency do ts() diario
  hv_tz           = "America/Sao_Paulo",           # so o flag historico HV (horario de verao)
  hv_shift_hours  = 6L,                            # o oraculo avalia o fuso as 06:00 UTC do dia (GetData2.R:51)
  cmed_bounds     = c(1, 999999),                  # .preconsist_ctm: Cmed valida
  tmed_bounds     = c(-10, 50))                    # .preconsist_ctm: Tmed valida

## --- Nomes de colunas e tokens (sem literal espalhado) -------------------------------------------------------------
hw_slot_cols <- function() sprintf("H%02d", 1:48)
hw_temp_cols <- function() sprintf("T%02d", 1:48)
## 105 colunas do CargaTempVer na ordem do oraculo (data_adapter.R:634-645)
hw_ctv_cols <- function() c("Data", "Feriado", "DiaSemana", hw_slot_cols(), "TempMedia", hw_temp_cols(),
                            "CargaMedia", "HV", "FHV15", "DHV15", "TIPO_FER")
## Tokens do deck/features; o registro os qualifica com `holt-winters::` (so `data.*` fica sem prefixo).
hw_deck_tokens <- function() paste0("deck.", c("CargaTempVer", "CargaTempMed", "TempPre", "CDDPre", "load_day_o",
                                               "n_steps", "weather_absent", "meta"))
hw_feature_tokens <- function() paste0("features.", c("hist_matrix", "temp_matrix", "calendar", "horizon_weather"))
## Colunas do painel exigidas na ingestao (tokens data.<dataset>.<coluna> do contrato do leitor)
hw_data_tokens <- function() c("data.load.load_mwh", "data.weather_observed.temperature",
  "data.weather_forecast.issue_time", "data.weather_forecast.valid_time", "data.weather_forecast.temperature",
  "data.holidays.date", "data.holidays.simplified_code", "data.holidays.holiday_type_id")

## --- Schema dos 7 params (sem default: o run.yaml e a unica fonte) ------------------------------------------------
hw_params_schema <- function() list(
  kmeans_seed = list(type = "integer"),
  ic          = list(type = "numeric"),
  perfil_mo   = list(type = "character", allowed = c("C", "ST")),
  classes_mo  = list(type = "integer", min = 1),
  similar     = list(type = "numeric", min = 0, max = 1),
  feriado     = list(type = "logical"),
  suaviza     = list(type = "logical"))

## --- Estagios dos tickets seguintes (HW.2, HW.3) ---------------------------------------------------------------
## O loader exige funcoes com a assinatura canonica e freeze/thaw no descriptor. Os estagios ainda nao portados sao
## despachados POR NOME para a definicao que o ticket seguinte acrescenta em R/ (plugin_build_features, plugin_train,
## plugin_inference_features, plugin_infer, hw_freeze, hw_thaw); enquanto nao existe, abortam plugin.stage_missing.
.hw_stage <- function(name) {
  f <- get0(name, envir = environment(.hw_stage), mode = "function", inherits = FALSE)
  if (is.null(f)) hw_abort("plugin.stage_missing", sprintf("estagio %s ainda nao implementado", name), where = "holt-winters")
  f
}
hw_stage_features <- function(frame, params) .hw_stage("plugin_build_features")(frame, params)
hw_stage_training <- function(features, params, seed) .hw_stage("plugin_train")(features, params, seed)
hw_stage_inference_features <- function(frame, payload) .hw_stage("plugin_inference_features")(frame, payload)
hw_stage_inference <- function(payload, inf_features, cuts) .hw_stage("plugin_infer")(payload, inf_features, cuts)
hw_stage_forecast <- function(payload, inf_features, cuts) .hw_stage("plugin_forecast")(payload, inf_features, cuts)
hw_freeze_model <- function(model) .hw_stage("hw_freeze")(model)
hw_thaw_payload <- function(payload) .hw_stage("hw_thaw")(payload)

plugin_descriptor <- function() list(
  model_id = "holt-winters", contract_version = 1L, granularity_supported = 30L,
  modes_implemented = c("train", "infer", "forecast"),
  params_schema = hw_params_schema(),
  variant_param = NA_character_, native_layout = "hw_matrix_nx48", semihourly_rule = NULL,
  delivery_days = 9L,                  # O..O+8
  reads_day_O = TRUE,                  # o infer le a carga do dia O (ate available_until) so para delimitar a entrega
  uses_rng = TRUE,                     # kmeans/MO1 com semente do run.yaml (o despachante isola o RNG)
  history_infer_equals_train = FALSE, engine = NULL,
  packages = c("forecast", "data.table", "lubridate", "stats"),
  freeze = hw_freeze_model, thaw = hw_thaw_payload,
  stages = list(
    ingestion           = list(fn = plugin_ingest, requires = hw_data_tokens(), provides = hw_deck_tokens()),
    feature_engineering = list(fn = hw_stage_features, requires = hw_deck_tokens(), provides = hw_feature_tokens()),
    training            = list(fn = hw_stage_training, requires = hw_feature_tokens(), provides = "model"),
    inference_features  = list(fn = hw_stage_inference_features, requires = c("model", hw_deck_tokens()),
                               provides = "inf_feat"),
    inference           = list(fn = hw_stage_inference, requires = c("model", "inf_feat"), provides = "envelope"),
    forecast            = list(fn = hw_stage_forecast, requires = c("model", "inf_feat"), provides = "envelope")))
