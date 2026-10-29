## dates.R -- camada de datas honesta (M2.3; ADR-0023, ADR-0032, ADR-0035, ADR-0037)
##
## Os TRES cortes de data, distintos e nunca um escalar unico:
##   1. forecast-origin  O = `--origin`, o UNICO flag humano de data (Date validada pelo M1.4a). Omitido =>
##                       dia do ultimo intervalo de carga presente no deck (prevcarga_last_load_day), nunca o relogio.
##   2. corte de treino  POR MODELO, pela classe de split do run.yaml (nunca por nome, nunca por
##                       backtest.protocol_default): origin_agnostic => train_origin = O-1-H (H lido do run.yaml);
##                       origin_scoped => train_origin = O (1 re-treino por dia; available_until nao entra).
##                       last_data_day = O-1-data_lag_days (teto O-1; data_lag_days vem do descriptor do plugin
##                       por argumento: RF 0 => O-1, LGBM 2 => O-3). Os plugins so consomem `cuts$last_data_day`.
##   3. corte de insumo  derivado do PROPRIO dado de carga (available_until, lido pelo leitor unico); a
##                       granularidade do membro decide o corte (30 => available_until; 60 => hora cheia).
## Sem chave `corte`, sem last_label_day/train_end, sem relogio. dates.R nao conhece nomes de modelo.
## Requer errors.R, topology.R, deck_reader.R carregados antes.

## --- 1. Origem da rodada ----------------------------------------------------------------------------
prevcarga_resolve_origin <- function(ctx) {
  if (!is.null(ctx$origin)) return(list(origin = ctx$origin, source = "flag"))
  list(origin = prevcarga_last_load_day(ctx), source = "deck")
}

## --- 2. Corte de treino por classe de split -----------------------------------------------------------
prevcarga_last_data_day <- function(origin, data_lag_days = 0L) {
  if (!is.numeric(data_lag_days) || length(data_lag_days) != 1L || is.na(data_lag_days) ||
      data_lag_days != as.integer(data_lag_days))
    prevcarga_abort("contract.data_lag_invalid", where = "dates.R")
  if (data_lag_days < 0L) prevcarga_abort("honesty.data_lag_negative", where = "dates.R")
  origin - 1L - as.integer(data_lag_days)
}

prevcarga_train_cut <- function(spec, origin, data_lag_days = 0L) {
  last_data_day <- prevcarga_last_data_day(origin, data_lag_days)
  train_origin <- switch(as.character(spec$split),
    origin_agnostic = origin - 1L - as.integer(spec$horizon_days),
    origin_scoped = origin,
    prevcarga_abort("contract.split_unknown", where = "dates.R"))
  list(train_origin = train_origin, last_data_day = last_data_day)
}

## --- 3. Corte de insumo e primeira entrega -------------------------------------------------------------
prevcarga_input_cut <- function(available_until, granularity_min) {
  switch(as.character(granularity_min)[[1L]],
    "30" = available_until,
    "60" = lubridate::floor_date(available_until, "hour"),
    prevcarga_abort("contract.granularity_invalid", where = "dates.R"))
}

prevcarga_first_delivery <- function(available_until) available_until + lubridate::minutes(30L)

## --- Composicao: campos run-level + tabela `cuts` por modelo ---------------------------------------------
## `data_lag_days`: named integer (nome = diretorio do modelo) vindo do descriptor do plugin; ausente => 0L.
prevcarga_resolve_cuts <- function(ctx, origin = NULL, model = NULL, data_lag_days = NULL) {
  o <- if (is.null(origin)) prevcarga_resolve_origin(ctx) else list(origin = origin, source = "flag")
  ld <- prevcarga_read_load(ctx, origin = o$origin, from = o$origin, to = o$origin)
  au <- attr(ld, "available_until")
  tz <- attr(ld, "tz")
  models <- if (is.null(model)) {
    mem <- if (is.null(ctx$topology_path)) prevcarga_members(root = ctx$root) else
      prevcarga_members(path = ctx$topology_path, root = ctx$root)
    unique(mem$mdir)
  } else model
  lag_of <- function(m) if (!is.null(data_lag_days) && m %in% names(data_lag_days)) data_lag_days[[m]] else NULL
  cuts <- data.table::rbindlist(lapply(models, function(m) {
    spec <- prevcarga_model_spec(m, root = ctx$root)
    declared <- lag_of(m)
    lag <- if (is.null(declared)) 0L else declared
    tc <- prevcarga_train_cut(spec, o$origin, lag)
    data.table::data.table(model = m, split = spec$split, granularity_min = spec$granularity_min,
      horizon_days = spec$horizon_days, data_lag_days = as.integer(lag),
      data_lag_source = if (is.null(declared)) "default" else "descriptor",
      train_origin = tc$train_origin, last_data_day = tc$last_data_day,
      input_cut = prevcarga_input_cut(au, spec$granularity_min),
      first_delivery = prevcarga_first_delivery(au))
  }))
  list(forecast_origin = o$origin, origin_source = o$source, available_until = au,
       available_until_by_area = attr(ld, "available_until_by_area"),
       load_label = attr(ld, "load_label"), tz = tz,
       weather_issue_cutoff = ctx$weather_issue_cutoff,
       weather_issue_limit = prevcarga_weather_issue_limit(ctx$weather_issue_cutoff, o$origin, au, tz),
       cuts = cuts)
}
