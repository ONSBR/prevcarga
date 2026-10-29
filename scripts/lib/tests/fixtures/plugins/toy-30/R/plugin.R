## Plugin-brinquedo toy-30 (origin_agnostic, 30 min): media movel de `k` dias por intervalo. So definicoes.
plugin_constants <- function() list(slots = 48L)

toy_ingestion <- function(panel, cuts) {
  ld <- panel$load
  list(load = ld, n = nrow(ld))
}

toy_feature_engineering <- function(frame, params) {
  v <- frame$load$load_mwh
  n <- plugin_constants()$slots
  days <- length(v) %/% n
  list(m = matrix(v[seq_len(days * n)], nrow = days, byrow = TRUE), k = params$k)
}

toy_training <- function(features, params, seed) {
  m <- features$m
  k <- min(params$k, nrow(m))
  colMeans(m[seq(nrow(m) - k + 1L, nrow(m)), , drop = FALSE])
}

toy_inference_features <- function(frame, payload) list(slots = length(payload))

toy_inference <- function(payload, inf_features, cuts) payload

toy_freeze <- function(model) list(model = model)
toy_thaw <- function(payload) payload$model

plugin_descriptor <- function() list(
  model_id = "toy-30", contract_version = 1L,
  granularity_supported = 30L, modes_implemented = c("train", "infer"),
  params_schema = list(k = list(type = "integer", min = 1L)), variant_param = NA_character_,
  native_layout = "lgbm_day_48", semihourly_rule = NULL,
  delivery_days = 2L, data_lag_days = 0L,
  packages = character(), attach = character(), engine = NULL,
  freeze = toy_freeze, thaw = toy_thaw,
  stages = list(
    ingestion = list(fn = toy_ingestion, requires = "data.load.load_mwh", provides = "frame.raw"),
    feature_engineering = list(fn = toy_feature_engineering, requires = "frame.raw", provides = "feat.x"),
    training = list(fn = toy_training, requires = "feat.x", provides = "artifact.model"),
    inference_features = list(fn = toy_inference_features, requires = c("frame.raw", "artifact.model"),
                              provides = "inf.feat"),
    inference = list(fn = toy_inference, requires = c("artifact.model", "inf.feat"), provides = "forecast.y")))
