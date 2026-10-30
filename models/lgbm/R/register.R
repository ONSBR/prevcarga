## register.R -- descriptor, schema de params e constantes do plugin LGBM (LGBM.1). SO definicoes (o loader recusa codigo de topo).
## Porte do oraculo prevcargadessem3/models/lgbm (somente leitura). As 7 env flags LGBM_* do oraculo viram `params` do
## run.yaml; o plugin NAO le variavel de ambiente. Estagios de LGBM.2-LGBM.4 sao despachados por nome (ver .lgbm_stage).
## Mensagens e saidas: ASCII puro.

## --- Acesso as primitivas da fundacao (errors.R) ---------------------------------------------------------------
## O env do plugin so enxerga base + pacotes anexados (loader P.0b): as funcoes da CLI vivem no ambiente global da
## fachada. Busca primeiro nos envs encadeados ao plugin (testes) e, na falta, no global (CLI). Interino, como o
## engine_abort() do motor DESSEM (DL.1); ver discovery do LGBM.1.
.lgbm_foundation <- function(name) {
  f <- get0(name, envir = environment(), mode = "function")
  if (is.null(f)) f <- get0(name, envir = .GlobalEnv, mode = "function", inherits = FALSE)
  f
}

## Aborto classificado: id do catalogo scripts/lib/errors/lgbm.tsv, mensagem "[prevcarga/<classe>] lgbm: <motivo>".
lgbm_abort <- function(id, reason, details = NULL) {
  mk <- .lgbm_foundation("prevcarga_condition")
  if (is.null(mk)) stop(sprintf("[%s] lgbm: %s", id, reason), call. = FALSE)
  cond <- mk(id, where = "lgbm", details = details)
  cond$message <- sprintf("[prevcarga/%s] lgbm: %s", cond$class_name, reason)
  stop(cond)
}

## --- Schema dos 7 params (sem default: o run.yaml e a unica fonte) ---------------------------------------------
lgbm_params_schema <- function() list(
  disable_loess      = list(type = "logical"),
  calendar_yearend   = list(type = "logical"),
  train_cv           = list(type = "logical"),
  tipodia_legacy     = list(type = "logical"),
  prevespera_1230    = list(type = "logical"),
  cutoff_scheme      = list(type = "character", allowed = c("v2_old", "legacy_month", "rolling")),
  sazonalidade_scope = list(type = "character", allowed = "train"))

## Confere `params` contra o schema; ausente, tipo errado ou valor fora de `allowed` => lgbm.params nomeando a chave.
lgbm_check_params <- function(params) {
  schema <- lgbm_params_schema()
  params <- if (is.null(params)) list() else params
  bad <- function(why) lgbm_abort("lgbm.params", why)
  extra <- setdiff(names(params), names(schema))
  if (length(extra)) bad(sprintf("params$%s desconhecido", extra[[1L]]))
  for (n in names(schema)) {
    if (!(n %in% names(params))) bad(sprintf("params$%s ausente", n))
    v <- params[[n]]
    def <- schema[[n]]
    tipo <- if (is.character(v)) "character" else if (is.logical(v)) "logical" else class(v)[[1L]]
    if (length(v) != 1L || is.na(v) || !identical(tipo, def$type))
      bad(sprintf("params$%s tipo %s != %s", n, tipo, def$type))
    if (!is.null(def$allowed) && !(v %in% def$allowed))
      bad(sprintf("params$%s=%s fora do schema", n, v))
  }
  invisible(params)
}

## --- Constantes intrinsecas do oraculo (nao sao params) ---------------------------------------------------------
plugin_constants <- function() list(
  ## hiperparametros (config.R, train_lgbm.R:189-205)
  num_leaves = 16L, min_data_in_leaf = 50L, learning_rate = 0.1, num_iterations = 5000L,
  early_stopping_rounds = 50L, objective = "regression", metric = "mse",
  cv_nfolds = 4L, cv_week_block = 336L, rolling_val_days = 28L,
  ## suavizacao (spans do LOESS) e wavelets
  span_prep = 0.3, span_inline = 0.1, span_perday = 0.15, span_lag = 0.3, lag_degree = 2L,
  wavelet_width = 32L, carwav_shift_days = 2L, normalization_scope = "train",
  ## safra de clima: o dia da emissao e o dia UTC (as.Date(issue_time) do oraculo); lead 0 no treino e no dia O
  issue_day_tz = "UTC", lead_train = 0L, lead_target = 1L,
  ## historia minima do infer: janela wavelet (32 meias-horas) + shift CarWav (2 dias) + lag de O-1
  min_history_days_infer = 4L,
  ## area -> area da serie de clima (data_adapter.R:130-150); area fora da tabela => contract
  heat_area = c(MS = "MS", RS = "RS", PR = "PR", MG = "MG", S = "RS", SP = "SP", SECO = "SP",
                RJ = "RJ", ES = "ES", SC = "SC", GO = "GO", MT = "MT", DF = "DF", AC = "AC", RO = "RO",
                N = "AM", NE = "CE", AM = "AM", PA = "PA", MA = "MA", TON = "TON", AP = "AP",
                ALPE = "ALPE", PBRN = "PBRN", BAOE = "BAOE", CE = "CE", PI = "PI", RR = "RR"),
  ## feriados nacionais fixos 2022-2025 (data_adapter.R:321-339); datas civis, sem deslocamento
  holidays_fixed = lubridate::ymd(c(
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
    "2025-12-24", "2025-12-25", "2025-12-31"), quiet = TRUE))

## --- Estagios dos tickets seguintes (LGBM.2-LGBM.4) ---------------------------------------------------------------
## O loader exige funcoes com a assinatura canonica e freeze/thaw no descriptor. Os estagios ainda nao portados sao
## despachados POR NOME para a definicao que o ticket seguinte acrescenta em R/ (plugin_build_features, plugin_train,
## plugin_inference_features, plugin_infer, lgbm_freeze, lgbm_thaw); enquanto nao existe, abortam lgbm.stage_missing.
.lgbm_stage <- function(name) {
  f <- get0(name, envir = environment(.lgbm_stage), mode = "function", inherits = FALSE)
  if (is.null(f)) lgbm_abort("lgbm.stage_missing", sprintf("estagio %s ainda nao implementado", name))
  f
}
lgbm_stage_features <- function(frame, params) .lgbm_stage("plugin_build_features")(frame, params)
lgbm_stage_training <- function(features, params, seed) .lgbm_stage("plugin_train")(features, params, seed)
lgbm_stage_inference_features <- function(frame, payload) .lgbm_stage("plugin_inference_features")(frame, payload)
lgbm_stage_inference <- function(payload, inf_features, cuts) .lgbm_stage("plugin_infer")(payload, inf_features, cuts)
lgbm_freeze_model <- function(model) .lgbm_stage("lgbm_freeze")(model)
lgbm_thaw_payload <- function(payload) .lgbm_stage("lgbm_thaw")(payload)

## Tokens que o estagio de ingestao exige do deck (data.<dataset>.<coluna>, contrato do leitor).
lgbm_deck_tokens <- function() c("data.load.load_mwh", "data.weather_forecast.temperature", "data.holidays.prevcarga_code")

plugin_descriptor <- function() list(
  model_id = "lgbm", contract_version = 1L,
  granularity_supported = 30L, modes_implemented = c("train", "infer"),
  variant_param = NA_character_, native_layout = "lgbm_day_48",
  semihourly_rule = NULL, delivery_days = 2L,
  data_lag_days = 2L,              # last_data_day = origem menos 1 menos 2 dias (ADR-0023; o oraculo le carga so ate train_origin-1)
  history_infer_equals_train = FALSE, engine = NULL,
  packages = c("lightgbm", "data.table", "lubridate", "wavelets", "zoo"),
  params_schema = lgbm_params_schema(),
  freeze = lgbm_freeze_model, thaw = lgbm_thaw_payload,
  stages = list(
    ingestion           = list(fn = plugin_ingest, requires = lgbm_deck_tokens(), provides = "frame"),
    feature_engineering = list(fn = lgbm_stage_features, requires = "frame", provides = "features"),
    training            = list(fn = lgbm_stage_training, requires = "features", provides = "model"),
    inference_features  = list(fn = lgbm_stage_inference_features, requires = c("frame", "model"), provides = "inf_feat"),
    inference           = list(fn = lgbm_stage_inference, requires = c("model", "inf_feat"), provides = "envelope")))
