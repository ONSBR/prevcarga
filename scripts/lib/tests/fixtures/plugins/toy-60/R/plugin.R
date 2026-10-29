## Plugin-brinquedo toy-60 (origin_scoped, 60 min, forecast, motor _toy-engine). So definicoes.
## model_time_offset_min = 60L: o deslocamento de modelagem e aplicado so pelo despachante; aqui nao ha hora.
toy_ingestion <- function(panel, cuts) list(load = panel$load)

toy_feature_engineering <- function(frame, params) list(x = frame$load$load_mwh)

toy_training <- function(features, params, seed) engine_fit(features$x)

toy_inference_features <- function(frame, payload) list(x = frame$load$load_mwh)

toy_inference <- function(payload, inf_features, cuts) engine_predict(payload, inf_features$x)

toy_forecast <- function(payload, inf_features, cuts) engine_predict(payload, inf_features$x)

toy_semihourly_rule <- function(x) rep(x$y_pred, each = 2L)

toy_freeze <- function(model) list(model = model)
toy_thaw <- function(payload) payload$model

plugin_descriptor <- function() list(
  model_id = "toy-60", contract_version = 1L,
  granularity_supported = 60L, modes_implemented = c("train", "infer", "forecast"),
  params_schema = list(), variant_param = NA_character_,
  native_layout = "hw_matrix_nx48", semihourly_rule = toy_semihourly_rule,
  delivery_days = 9L, model_time_offset_min = 60L,
  history_infer_equals_train = TRUE, engine = "_toy-engine",
  freeze = toy_freeze, thaw = toy_thaw,
  stages = list(
    ingestion = list(fn = toy_ingestion, requires = "data.load.load_mwh", provides = "frame.raw"),
    feature_engineering = list(fn = toy_feature_engineering, requires = "frame.raw", provides = "feat.x"),
    training = list(fn = toy_training, requires = "feat.x", provides = "artifact.model"),
    inference_features = list(fn = toy_inference_features, requires = c("frame.raw", "artifact.model"),
                              provides = "inf.feat"),
    inference = list(fn = toy_inference, requires = c("artifact.model", "inf.feat"), provides = "forecast.y"),
    forecast = list(fn = toy_forecast, requires = c("artifact.model", "inf.feat"), provides = "forecast.f")))
