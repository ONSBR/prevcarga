## register.R -- plugin dessem-linear (DL.5). So definicoes (o loader recusa codigo de topo).
## Descriptor no contrato da fundacao (registry.R). O env do plugin tem pai = env do motor _dessem-engine, que
## enxerga so base + pacotes de `attach`: stats/utils entram aqui (o motor e o plugin os usam).
## A hora NAO e tocada aqui: o deslocamento de 60 min (ADR-0012/0019) e declarado em `model_time_offset_min` e
## aplicado SO pelo despachante da fundacao (to_model_frame/to_display_frame). Nenhuma aritmetica de hora no plugin.
##
## Estagios training/inference_features/inference/forecast e freeze/thaw sao dos tickets DL.6/DL.7: este arquivo
## resolve cada um por NOME (dl_training, dl_inference_features, dl_inference, dl_forecast, dl_freeze, dl_thaw) no env
## do plugin e, enquanto o arquivo do estagio nao existe, usa um stub que recusa com plugin.stage_missing.

plugin_constants <- function() {
  list(model_key = c(MED = "Linear.TmpMedia", MAX = "Linear.TmpMaxima", MXMN = "Linear.TmpMaxMin"),
       hours_per_day = 24L, mm_ref_hour = 12L, max_tail_fill_days = 2L, horizon_days = 9L)
}

## Env do motor (pai do env do plugin). Nao ha estado: o motor e uma instancia por plugin carregado.
dl_engine <- function() parent.env(environment(dl_engine))

## --- Erros e avisos: interim ate o loader expor a API de erros (mesmo padrao de engine_abort do motor) ---------
## prevcarga_condition/prevcarga_warn vivem no globalenv da facade; sem a fundacao carregada cai em stop()/warning().
dl_abort <- function(id, where = NULL, reason = NULL, details = NULL) {
  mk <- get0("prevcarga_condition", envir = globalenv(), mode = "function", inherits = FALSE)
  if (is.null(mk)) stop(paste0("[", id, "] ", where, if (!is.null(reason)) paste0(": ", reason)), call. = FALSE)
  cond <- mk(id, where = where, details = details)
  if (!is.null(reason)) cond$message <- paste0(cond$message, ": ", reason)
  stop(cond)
}

dl_warn <- function(id, where = NULL, details = NULL) {
  f <- get0("prevcarga_warn", envir = globalenv(), mode = "function", inherits = FALSE)
  if (is.null(f)) warning(paste0("[", id, "] ", where), call. = FALSE) else f(id, where = where, details = details)
}

## --- Variante (exogena) --------------------------------------------------------------------------------------
## spec$params$exogena in {MED, MAX, MXMN}; ausente ou fora do conjunto aborta ANTES de qualquer treino.
dl_resolve_variant <- function(spec) {
  ex <- spec$params$exogena
  keys <- plugin_constants()$model_key
  if (!is.character(ex) || length(ex) != 1L || is.na(ex) || !(ex %in% names(keys)))
    dl_abort("plugin.dessem_linear_bad_exogena", where = "dessem-linear/params",
             reason = sprintf("params.exogena=%s; permitido {%s}",
                              if (is.null(ex) || !length(ex)) "ausente" else paste(format(ex), collapse = ","),
                              paste(names(keys), collapse = ", ")),
             details = list(field = "params.exogena", got = ex, allowed = names(keys)))
  list(exogena = ex, model_key = unname(keys[[ex]]))
}

## --- Regra semi-horaria: desagregaModeloSH do motor; falha => duplicacao :00/:30 + aviso -------------------------
## `x` = data.table(interval_end, y_pred) horario ordenado; devolve 2 valores por hora (meia hora anterior a ancora,
## ancora), como pede to_semihourly. O carimbo de 30 min e centralizado em to_semihourly (features.R).
dl_semihourly_rule <- function(x) {
  n <- nrow(x)
  d <- data.table::data.table(DataHora = x$interval_end, Carga = x$y_pred, Modelo = "M")
  out <- tryCatch(suppressMessages(dl_engine()$desagregaModeloSH("M", d)), error = function(e) NULL)
  ok <- is.data.frame(out) && nrow(out) == 2L * n && is.numeric(out$Carga) && all(is.finite(out$Carga))
  if (ok) return(out$Carga)
  dl_warn("warning.dessem_linear_desagrega_fallback", where = "dessem-linear/semihourly_rule")
  rep(x$y_pred, each = 2L)
}

## --- Estagios de DL.5 (as 2 formas: com spec, para os testes; com 2 formais, para o despachante) -----------------
dl_ingestion <- function(panel, cuts) plugin_ingest(panel, cuts, NULL)
dl_feature_engineering <- function(frame, params) plugin_build_features(frame, list(params = params))

## --- Stubs dos estagios de DL.6/DL.7 (resolvidos por nome; o arquivo do estagio os substitui) ------------------
dl_stub_abort <- function(stage)
  dl_abort("plugin.stage_missing", where = "dessem-linear", reason = paste0("estagio ", stage, " ainda nao implementado"))
dl_stub_training <- function(features, params, seed) dl_stub_abort("training")
dl_stub_inference_features <- function(frame, payload) dl_stub_abort("inference_features")
dl_stub_inference <- function(payload, inf_features, cuts) dl_stub_abort("inference")
dl_stub_forecast <- function(payload, inf_features, cuts) dl_stub_abort("forecast")
dl_stub_freeze <- function(model) dl_stub_abort("freeze")
dl_stub_thaw <- function(payload) dl_stub_abort("thaw")

dl_pick <- function(name, stub) {
  f <- get0(name, envir = environment(dl_pick), mode = "function", inherits = FALSE)
  if (is.null(f)) stub else f
}

plugin_descriptor <- function() {
  attach <- c("data.table", "lubridate", "jsonlite", "rlist", "WeightSVM", "e1071", "stats", "utils")
  list(
    model_id = "dessem-linear", contract_version = 1L,
    granularity_supported = 60L, modes_implemented = c("train", "infer", "forecast"),
    params_schema = list(exogena = list(type = "character", allowed = names(plugin_constants()$model_key))),
    variant_param = "exogena",
    native_layout = "dessem_semihourly_48", semihourly_rule = dl_semihourly_rule,
    delivery_days = 9L, data_lag_days = 0L, model_time_offset_min = 60L,
    reads_day_O = FALSE, weather_forecast_in_train = TRUE, uses_rng = TRUE,
    history_infer_equals_train = TRUE, rng_deviation = NULL,
    packages = attach, attach = attach, engine = "_dessem-engine",
    freeze = dl_pick("dl_freeze", dl_stub_freeze), thaw = dl_pick("dl_thaw", dl_stub_thaw),
    stages = list(
      ingestion = list(fn = dl_ingestion,
        requires = c("data.load.load_mwh", "data.weather_observed.temperature",
                     "data.weather_forecast.temperature", "data.holidays.prevcarga_code"),
        provides = "frame.deck"),
      feature_engineering = list(fn = dl_feature_engineering, requires = "frame.deck", provides = "feat.series"),
      training = list(fn = dl_pick("dl_training", dl_stub_training), requires = "feat.series",
                      provides = "artifact.svm"),
      inference_features = list(fn = dl_pick("dl_inference_features", dl_stub_inference_features),
                                requires = c("frame.deck", "artifact.svm"), provides = "inf.feat"),
      inference = list(fn = dl_pick("dl_inference", dl_stub_inference),
                       requires = c("artifact.svm", "inf.feat"), provides = "forecast.y"),
      forecast = list(fn = dl_pick("dl_forecast", dl_stub_forecast),
                      requires = c("artifact.svm", "inf.feat"), provides = "forecast.f")))
}
