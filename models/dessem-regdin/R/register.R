## register.R -- plugin dessem-regdin (RD.1). So definicoes (o loader recusa codigo de topo).
## Descriptor no contrato da fundacao (registry.R). O env do plugin tem pai = env do motor _dessem-engine, que
## enxerga so base + pacotes de `attach`. A hora NAO e tocada aqui: o deslocamento de 60 min (ADR-0012/0019) e
## declarado em `model_time_offset_min` e aplicado SO pelo despachante da fundacao (to_model_frame/to_display_frame).
## Nenhuma aritmetica de hora no plugin nem no motor.
##
## Estagios training/inference_features/inference/forecast e freeze/thaw: RD.2/RD.3. Cada um e resolvido por NOME
## (rd_training, rd_inference_features, rd_inference, rd_forecast, rd_freeze, rd_thaw) no env do plugin; enquanto o
## arquivo do estagio nao existe, um stub recusa com plugin.stage_missing. register.R nao e editado para trocar stubs.

plugin_constants <- function() {
  list(model_key = c(MED = "RegDin.Radial.TmpMed", MAX = "RegDin.Radial.TmpMax"),
       tipo_entrada_dm = "TS_FACTOR", lags_exogena_dia = c(1L, 2L, 7L), reference_mm_hour = 12L,
       hours_per_day = 24L, max_tail_fill_days = 2L, horizon_days = 9L,
       reference_history_days = 1825L)
}

## Env do motor (pai do env do plugin). Sem estado: o motor e uma instancia por plugin carregado.
rd_engine <- function() parent.env(environment(rd_engine))

## --- Erros e avisos: interim ate o loader expor a API de erros (mesmo padrao do dessem-linear, divida DL8-GAP-2) ----
## prevcarga_condition/prevcarga_warn vivem no globalenv da facade; sem a fundacao carregada cai em stop()/warning().
rd_abort <- function(id, where = NULL, reason = NULL, details = NULL) {
  mk <- get0("prevcarga_condition", envir = globalenv(), mode = "function", inherits = FALSE)
  if (is.null(mk)) stop(paste0("[", id, "] ", where, if (!is.null(reason)) paste0(": ", reason)), call. = FALSE)
  cond <- mk(id, where = where, details = details)
  if (!is.null(reason)) cond$message <- paste0(cond$message, ": ", reason)
  stop(cond)
}

rd_warn <- function(id, where = NULL, details = NULL) {
  f <- get0("prevcarga_warn", envir = globalenv(), mode = "function", inherits = FALSE)
  if (is.null(f)) warning(paste0("[", id, "] ", where), call. = FALSE) else f(id, where = where, details = details)
}

## --- Variante (exogena) --------------------------------------------------------------------------------------
## Tabela fechada MED/MAX; qualquer outro valor (MXMN incluido: o oraculo caia para MED com aviso) aborta contract.
.rd_variant_key <- function(params) {
  e <- params$exogena
  mk <- plugin_constants()$model_key
  if (!is.character(e) || length(e) != 1L || is.na(e) || !e %in% names(mk))
    rd_abort("plugin.regdin_bad_exogena", where = "dessem-regdin/params",
             reason = sprintf("params.exogena=%s; permitido {%s}",
                              if (is.null(e) || !length(e)) "ausente" else paste(format(e), collapse = ","),
                              paste(names(mk), collapse = ", ")),
             details = list(field = "exogena", value = e, allowed = names(mk)))
  mk[[e]]
}

plugin_variant_meta <- function(params) {
  key <- .rd_variant_key(params)
  list(exogena = params$exogena, member = paste0("RGD_", params$exogena), model_key = key)
}

## Simbolos do motor que o plugin chama (conferidos pelos testes do RD.4 contra o env do motor do regdin).
plugin_required_functions <- function()
  c("TransformaDeck", "aggregaSerie", "getNormalizaCarga", "getNormalizaExogena", "normalizaSerieRef",
    "inclui_CargaMediaRef", "execDemandaMediaTS", "obterEntradaFormulaDM_Reg", "SVMhorario", "RL_perfilFeriado",
    "entradaPerfil", "obterDiaEspPerfilConfig", "addValuesDMRef", "desagregaModeloSH", "engine_svm_freeze",
    "engine_svm_thaw")

## --- Regra semi-horaria: desagregaModeloSH do motor; falha => duplicacao :00/:30 + aviso -------------------------
rd_semihourly_rule <- function(x) {
  n <- nrow(x)
  d <- data.table::data.table(DataHora = x$interval_end, Carga = x$y_pred, Modelo = "M")
  out <- tryCatch(suppressMessages(rd_engine()$desagregaModeloSH("M", d)), error = function(e) NULL)
  ok <- is.data.frame(out) && nrow(out) == 2L * n && is.numeric(out$Carga) && all(is.finite(out$Carga))
  if (ok) return(out$Carga)
  rd_warn("warning.regdin_desagrega_fallback", where = "dessem-regdin/semihourly_rule")
  rep(x$y_pred, each = 2L)
}

## --- Estagios de RD.1 (2 formais, como o despachante os chama) -------------------------------------------------
rd_ingestion <- function(panel, cuts) plugin_ingest(panel, cuts, NULL)
rd_feature_engineering <- function(frame, params) plugin_build_features(frame, list(params = params))

## --- Stubs dos estagios de RD.2/RD.3 (resolvidos por nome; o arquivo do estagio os substitui) -------------------
rd_stub_abort <- function(stage)
  rd_abort("plugin.stage_missing", where = "dessem-regdin", reason = paste0("estagio ", stage, " ainda nao implementado"))
rd_stub_training <- function(features, params, seed) rd_stub_abort("training")
rd_stub_inference_features <- function(frame, payload) rd_stub_abort("inference_features")
rd_stub_inference <- function(payload, inf_features, cuts) rd_stub_abort("inference")
rd_stub_forecast <- function(payload, inf_features, cuts) rd_stub_abort("forecast")
rd_stub_freeze <- function(model) rd_stub_abort("freeze")
rd_stub_thaw <- function(payload) rd_stub_abort("thaw")

rd_pick <- function(name, stub) {
  f <- get0(name, envir = environment(rd_pick), mode = "function", inherits = FALSE)
  if (is.null(f)) stub else f
}

## Janela do DESSEM (ADR-046; E3-T1): puro; delega a regra unica da fundacao (scripts/lib/holiday_islands.R), resolvida no
## globalenv como prevcarga_condition (o env do plugin so enxerga base + attach). Sem o param, ano_civil.
## history_days.train ABAIXO da janela de ano_civil (1095) encurta a janela comum nos dois modos (override de teste/gate; o
## run.yaml declara 1095): common_from = O - history_days.train. Em producao (1095) a regra da fundacao vale como esta.
rd_history_window <- function(origin, spec) {
  mode <- spec$params$janela_dessem
  f <- get0("prevcarga_dessem_window", envir = globalenv(), mode = "function", inherits = FALSE)
  w <- f(origin, if (is.null(mode)) "ano_civil" else mode)
  tr <- spec$history_days$train
  if (!is.null(tr) && is.finite(tr) && w$common_from < lubridate::as_date(origin) - as.integer(tr) &&
      as.integer(tr) < as.integer(lubridate::as_date(origin) - f(origin, "ano_civil")$common_from))
    w$common_from <- lubridate::as_date(origin) - as.integer(tr)
  w
}

## data_needs (E2-T2): o que a inferencia/treino precisa do deck, so como dado. O previsto e a emissao mais fresca por
## valid_time em [O, O + H] (freshest); carga/observado/feriados pela janela atual (n = history_days.infer, nunca literal).
## E3/E4 trocam esta declaracao depois.
rd_data_needs <- function(spec, area) {
  n <- as.integer(spec$history_days$infer); H <- as.integer(spec$horizon_days)
  mode <- if (is.null(spec$params$janela_dessem)) "ano_civil" else spec$params$janela_dessem
  wf <- list(area = area, segments = list(list(kind = "freshest", from = 0L, to = H, lead = NA_integer_)))
  list(infer = list(schema_version = 1L, load = list(days_before = n), weather_observed = list(days_before = n),
                    weather_forecast = wf, holidays = list(from = -n, to = H), holiday_annex = identical(mode, "ano_civil")),
       train_weather_forecast = wf)
}

plugin_descriptor <- function() {
  ## ordem do oraculo (library()): data.table, jsonlite, rlist, WeightSVM, e1071, lubridate, series temporais
  attach <- c("stats", "utils", "data.table", "jsonlite", "rlist", "WeightSVM", "e1071", "lubridate",
              "tsibble", "fabletools", "fable", "feasts", "urca")
  list(
    model_id = "dessem-regdin", contract_version = 1L,
    granularity_supported = 60L, modes_implemented = c("train", "infer", "forecast"),
    weather_resample = "hourly",              # EXOGENAHIST/EXOGENAPREV horarios (to_hourly da fundacao)
    params_schema = list(exogena = list(type = "character", allowed = names(plugin_constants()$model_key)),
                         janela_dessem = list(type = "character", allowed = c("ano_civil", "referencia"))),
    variant_param = "exogena",
    native_layout = "dessem_semihourly_48", semihourly_rule = rd_semihourly_rule,
    weather_cut = "absent",   # F3.7: DESSEM so com horizonte inteiro; clima previsto curto => completude.weather_forecast_short (membro ausente)
    delivery_days = 9L, data_lag_days = 0L, model_time_offset_min = 60L,
    reads_day_O = TRUE, weather_forecast_in_train = TRUE, uses_rng = TRUE,   # mm do dia O ate available_until (oraculo data_adapter.R)
    history_infer_equals_train = TRUE, rng_deviation = NULL,
    packages = attach, attach = attach, engine = "_dessem-engine",
    data_needs = rd_data_needs,
    ## Janela do DESSEM (ADR-046; E3-T1): a fachada consome este campo sem switch por nome. Sem o param, ano_civil.
    history_window = rd_history_window,
    freeze = rd_pick("rd_freeze", rd_stub_freeze), thaw = rd_pick("rd_thaw", rd_stub_thaw),
    stages = list(
      ingestion = list(fn = rd_ingestion,
        requires = c("data.load.load_mwh", "data.weather_observed.temperature",
                     "data.weather_forecast.temperature", "data.holidays.prevcarga_code"),
        provides = "frame.deck"),
      feature_engineering = list(fn = rd_feature_engineering, requires = "frame.deck", provides = "feat.series"),
      training = list(fn = rd_pick("rd_training", rd_stub_training), requires = "feat.series",
                      provides = "artifact.regdin"),
      inference_features = list(fn = rd_pick("rd_inference_features", rd_stub_inference_features),
                                requires = c("frame.deck", "artifact.regdin"), provides = "inf.feat"),
      inference = list(fn = rd_pick("rd_inference", rd_stub_inference),
                       requires = c("artifact.regdin", "inf.feat"), provides = "forecast.y"),
      forecast = list(fn = rd_pick("rd_forecast", rd_stub_forecast),
                      requires = c("artifact.regdin", "inf.feat"), provides = "forecast.f")))
}
