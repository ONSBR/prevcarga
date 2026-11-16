## register.R -- plugin dessem-linear (DL.5). So definicoes (o loader recusa codigo de topo).
## Descriptor no contrato da fundacao (registry.R). O env do plugin tem pai = env do motor _dessem-engine, que
## enxerga so base + pacotes de `attach`: stats/utils entram aqui (o motor e o plugin os usam).
## A hora NAO e tocada aqui: o deslocamento de 60 min (ADR-0012/0019) e declarado em `model_time_offset_min` e
## aplicado SO pelo despachante da fundacao (to_model_frame/to_display_frame). Nenhuma aritmetica de hora no plugin.
##
## Estagios training e freeze/thaw: DL.6 (training.R: dl_training; freeze.R: dl_freeze/dl_thaw). inference_features/inference/forecast: DL.7 (inference_features.R: dl_inference_features; inference.R: dl_inference, dl_forecast, plugin_forecast).
## Cada um e resolvido por NOME (nao ha edicao aqui para trocar stubs): este arquivo
## resolve cada um por NOME (dl_training, dl_inference_features, dl_inference, dl_forecast, dl_freeze, dl_thaw) no env
## do plugin e, enquanto o arquivo do estagio nao existe, usa um stub que recusa com plugin.stage_missing.

plugin_constants <- function() {
  list(model_key = c(MED = "Linear.TmpMedia", MAX = "Linear.TmpMaxima", MXMN = "Linear.TmpMaxMin",
                     UNI = "Linear.Univariado"),
       hours_per_day = 24L, mm_ref_hour = 12L, max_tail_fill_days = 2L, horizon_days = 9L,
       reference_history_days = 1825L)
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
## spec$params$exogena in {MED, MAX, MXMN, UNI} (names do mapa model_key); ausente ou fora do conjunto aborta ANTES de qualquer treino.
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

## TRUE sse o config do motor agrega a exogena com SEM (Linear.Univariado): nenhuma coluna de clima entra no modelo.
## Decisao pelo motor, nunca pelo literal da variante.
dl_variant_weatherless <- function(model_key) {
  if (is.null(model_key) || length(model_key) != 1L || is.na(model_key)) return(FALSE)
  identical(dl_engine()$getConfigModel(model_key)$exogenaOper, "SEM")
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

## Janela do DESSEM (ADR-046; E3-T1): puro; delega a regra unica da fundacao (scripts/lib/holiday_islands.R), que o registry
## expoe a este plugin (prevcarga_dessem_window). Sem o param, ano_civil.
## history_days.train ABAIXO da janela de ano_civil (1095) encurta a janela comum nos dois modos (override de teste/gate; o
## run.yaml declara 1095): common_from = O - history_days.train. Em producao (1095) a regra da fundacao vale como esta.
dl_history_window <- function(origin, spec) {
  mode <- spec$params$janela_dessem
  w <- prevcarga_dessem_window(origin, if (is.null(mode)) "ano_civil" else mode)
  tr <- spec$history_days$train
  if (!is.null(tr) && is.finite(tr) &&
      as.integer(tr) < as.integer(lubridate::as_date(origin) - prevcarga_dessem_window(origin, "ano_civil")$common_from))
    w$common_from <- lubridate::as_date(origin) - as.integer(tr)
  w
}

## data_needs (E2-T2): o que a inferencia/treino precisa do deck, so como dado. O previsto e a emissao mais fresca por
## valid_time em [O, O + H] (freshest); carga/observado/feriados pela janela atual (n = history_days.infer, nunca literal).
## E3/E4 trocam esta declaracao depois.
dl_data_needs <- function(spec, area) {
  n <- as.integer(spec$history_days$infer); H <- as.integer(spec$horizon_days)
  mode <- if (is.null(spec$params$janela_dessem)) "ano_civil" else spec$params$janela_dessem
  wf <- list(area = area, segments = list(list(kind = "freshest", from = 0L, to = H, lead = NA_integer_)))
  list(infer = list(schema_version = 1L, load = list(days_before = n), weather_observed = list(days_before = n),
                    weather_forecast = wf, holidays = list(from = -n, to = H), holiday_annex = identical(mode, "ano_civil")),
       train_weather_forecast = wf)
}

plugin_descriptor <- function() {
  attach <- c("data.table", "lubridate", "jsonlite", "rlist", "WeightSVM", "e1071", "stats", "utils")
  list(
    model_id = "dessem-linear", contract_version = 1L,
    granularity_supported = 60L, modes_implemented = c("train", "infer", "forecast"),
    weather_resample = "hourly",              # EXOGENAHIST/EXOGENAPREV horarios (to_hourly da fundacao)
    params_schema = list(exogena = list(type = "character", allowed = names(plugin_constants()$model_key)),
                         janela_dessem = list(type = "character", allowed = c("ano_civil", "referencia"))),
    variant_param = "exogena",
    ## so a UNI declara requires proprio (sem data.weather_*); MED/MAX/MXMN seguem o requires do estagio (E5-T1)
    variants = list(UNI = list(requires = c("data.load.load_mwh", "data.holidays.prevcarga_code"))),
    native_layout = "dessem_semihourly_48", semihourly_rule = dl_semihourly_rule,
    weather_cut = "absent",   # F3.7: DESSEM so com horizonte inteiro; clima previsto curto => completude.weather_forecast_short (membro ausente)
    delivery_days = 9L, data_lag_days = 0L, model_time_offset_min = 60L,
    reads_day_O = TRUE, weather_forecast_in_train = TRUE, uses_rng = TRUE,   # mm do dia O ate available_until (oraculo data_adapter.R)
    history_infer_equals_train = FALSE, rng_deviation = NULL,   # E4-T2: a inferencia roda sobre o estado do artefato (deck minimo)
    packages = attach, attach = attach, engine = "_dessem-engine",
    data_needs = dl_data_needs,
    ## Janela do DESSEM (ADR-046; E3-T1): a fachada consome este campo sem switch por nome. Sem o param, ano_civil.
    history_window = dl_history_window,
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
