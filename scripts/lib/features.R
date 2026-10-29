## features.R -- dono unico dos transforms de grade (ADR-0012, ADR-0019, ADR-0035; ticket P.0a).
##
## to_hourly / to_semihourly / to_model_frame / to_display_frame / prevcarga_delivery_grid. Os M2..M7 e os
## 5 plugins so CONSOMEM estes transforms; to_model_frame/to_display_frame sao chamados SO pelo despachante da
## fundacao com o `model_time_offset_min` do descriptor (DESSEM 60, demais 0). O rotulo da carga (-30 min) e
## so do leitor unico e nao e tocado aqui. Datas so por lubridate com tz explicito; nenhum literal de segundos;
## sem variaveis de ambiente. A fachada de dispatch e acrescentada depois pelo M5.1.
## Erros: prevcarga_abort (errors.R) com ids declarados em scripts/lib/errors/plugin.tsv.
## Saida de terminal/mensagens: ASCII puro, sem acento.

## --- Validadores internos -------------------------------------------------------------------------------
.prevcarga_feat_check_gran <- function(g, where = "features") {
  if (!is.numeric(g) || length(g) != 1L || is.na(g) || !(g %in% c(30, 60)))
    prevcarga_abort("features.bad_granularity", where = where, details = list(granularity_min = g))
  invisible(as.integer(g))
}

.prevcarga_feat_check_offset <- function(o, where = "features") {
  ok <- is.numeric(o) && length(o) == 1L && !is.na(o) && is.finite(o) && o >= 0 && o %% 1 == 0
  if (!ok) prevcarga_abort("features.bad_offset", where = where, details = list(offset_min = o))
  invisible(as.integer(o))
}

.prevcarga_feat_check_time <- function(x, col, where = "features", tz = NULL) {
  ok <- is.character(col) && length(col) == 1L && col %in% names(x) &&
    inherits(x[[col]], "POSIXct") && nzchar(lubridate::tz(x[[col]])) &&
    (is.null(tz) || identical(lubridate::tz(x[[col]]), tz))
  if (!ok) prevcarga_abort("features.bad_time_col", where = where, details = list(column = col))
  invisible(TRUE)
}

## Atributo do painel: o leitor entrega `panel$attrs` (lista); aceita-se tambem attr(panel, nome).
.prevcarga_feat_panel_attr <- function(panel, name) {
  v <- panel$attrs[[name]]
  if (is.null(v)) v <- attr(panel, name)
  v
}

## Copia os atributos de usuario de `from` em `to` (available_until, load_label, tz, ...).
.prevcarga_feat_keep_attrs <- function(to, from) {
  skip <- c("names", "row.names", "class", ".internal.selfref", "index", "sorted")
  for (n in setdiff(names(attributes(from)), skip)) data.table::setattr(to, n, attr(from, n))
  to
}

## --- to_hourly ------------------------------------------------------------------------------------------
## Carga: media dos 2 intervalos H:00/H:30 por (area, hora); agrupa por interval_start, nunca por interval_end.
.prevcarga_hourly_load <- function(load) {
  dt <- data.table::copy(data.table::as.data.table(load))
  dt[, hour_start := lubridate::floor_date(interval_start, "hour")]
  dt[, `:=`(n_slots = .N, n_start = data.table::uniqueN(interval_start)), by = .(area_code, hour_start)]
  out <- dt[n_slots == 2L & n_start == 2L, .(load_mwh = mean(load_mwh)),
            by = .(area_code, interval_start = hour_start)]
  out[, interval_end := interval_start + lubridate::minutes(60)]
  data.table::setcolorder(out, c("area_code", "interval_start", "interval_end", "load_mwh"))
  data.table::setorder(out, area_code, interval_start)
  .prevcarga_feat_keep_attrs(out, load)
  out[]
}

## Exogenas instantaneas: media dos instantes valid_time H:00 e H:30 por (keys, hora); hora incompleta some.
.prevcarga_hourly_weather <- function(w, keys) {
  dt <- data.table::copy(data.table::as.data.table(w))
  dt[, hour_start := lubridate::floor_date(valid_time, "hour")]
  vals <- setdiff(names(dt)[vapply(dt, is.numeric, NA)], keys)
  dt[, `:=`(n_slots = .N, n_inst = data.table::uniqueN(valid_time)), by = c(keys, "hour_start")]
  out <- dt[n_slots == 2L & n_inst == 2L, lapply(.SD, mean), by = c(keys, "hour_start"), .SDcols = vals]
  data.table::setnames(out, "hour_start", "valid_time")
  data.table::setcolorder(out, c(keys, "valid_time", vals))
  data.table::setorderv(out, c(keys, "valid_time"))
  .prevcarga_feat_keep_attrs(out, w)
  out[]
}

to_hourly <- function(panel) {
  tz <- .prevcarga_feat_panel_attr(panel, "tz")
  if (is.null(tz)) tz <- lubridate::tz(panel$load$interval_start)
  .prevcarga_feat_check_time(panel$load, "interval_start", "to_hourly", tz)
  .prevcarga_feat_check_time(panel$load, "interval_end", "to_hourly", tz)
  .prevcarga_feat_check_time(panel$weather_observed, "valid_time", "to_hourly", tz)
  .prevcarga_feat_check_time(panel$weather_forecast, "valid_time", "to_hourly", tz)
  .prevcarga_feat_check_time(panel$weather_forecast, "issue_time", "to_hourly")
  out <- panel
  out$load <- .prevcarga_hourly_load(panel$load)
  out$weather_observed <- .prevcarga_hourly_weather(panel$weather_observed, "area_code")
  out$weather_forecast <- .prevcarga_hourly_weather(panel$weather_forecast, c("area_code", "issue_time"))
  out$holidays <- panel$holidays
  attr(out, "granularity_min") <- 60L
  out
}

## --- to_semihourly --------------------------------------------------------------------------------------
## `rule(x)` (do plugin) devolve 2 valores por hora de entrada, na ordem (meia hora anterior a ancora, ancora),
## como vetor, coluna y_pred ou data.table(interval_end, y_pred). O carimbo de 30 min e centralizado aqui.
to_semihourly <- function(x, rule, available_until, last_day) {
  if (!is.function(rule)) prevcarga_abort("features.semihourly_rule", where = "to_semihourly",
                                          details = list(reason = "rule nao e funcao"))
  .prevcarga_feat_check_time(x, "interval_end", "to_semihourly")
  tz <- lubridate::tz(x$interval_end)
  x <- data.table::as.data.table(x)
  x <- x[order(interval_end)]
  anchor <- x$interval_end
  stamp <- sort(c(anchor - lubridate::minutes(30), anchor))
  bad <- function(why) prevcarga_abort("features.semihourly_rule", where = "to_semihourly",
                                       details = list(reason = why))
  res <- rule(x)
  y <- if (is.data.frame(res)) res[["y_pred"]] else res
  if (!is.numeric(y) || length(y) != 2L * nrow(x)) bad("comprimento != 2 por hora")
  if (is.data.frame(res) && "interval_end" %in% names(res)) {
    ie <- res[["interval_end"]]
    if (!inherits(ie, "POSIXct") || length(ie) != length(stamp) || anyNA(ie) || !all(ie == stamp))
      bad("interval_end fora da ancora horaria")
  }
  out <- data.table::data.table(interval_end = stamp, y_pred = as.numeric(y))
  end_limit <- lubridate::force_tz(lubridate::as_datetime(last_day + 1), tz)
  out[interval_end > available_until & interval_end <= end_limit]
}

## --- to_model_frame / to_display_frame ------------------------------------------------------------------
.prevcarga_feat_shift <- function(x, offset_min, time_col, sign, where) {
  .prevcarga_feat_check_offset(offset_min, where)
  .prevcarga_feat_check_time(x, time_col, where)
  out <- if (data.table::is.data.table(x)) data.table::copy(x) else x
  if (offset_min == 0) return(out)
  shifted <- out[[time_col]] + sign * lubridate::minutes(offset_min)
  if (data.table::is.data.table(out)) data.table::set(out, j = time_col, value = shifted)
  else out[[time_col]] <- shifted
  out
}

to_model_frame <- function(x, offset_min, time_col = "DataHora")
  .prevcarga_feat_shift(x, offset_min, time_col, -1L, "to_model_frame")

to_display_frame <- function(x, offset_min, time_col = "DataHora")
  .prevcarga_feat_shift(x, offset_min, time_col, 1L, "to_display_frame")

## --- Grade de entrega -----------------------------------------------------------------------------------
## interval_end de available_until + 30 min ate 00:00 do dia (origem + delivery_days), em cuts$tz: os passos
## de 30 min a partir de available_until (1o interval_start), sem o proprio available_until.
prevcarga_delivery_grid <- function(cuts, delivery_days) {
  w <- "prevcarga_delivery_grid"
  bad <- function(field) prevcarga_abort("features.bad_delivery_args", where = w, details = list(field = field))
  au <- cuts$available_until
  if (!inherits(au, "POSIXct") || length(au) != 1L || is.na(au)) bad("available_until")
  if (!(lubridate::minute(au) %in% c(0, 30)) || lubridate::second(au) != 0) bad("available_until")
  if (!is.character(cuts$tz) || length(cuts$tz) != 1L || !identical(lubridate::tz(au), cuts$tz)) bad("tz")
  if (!is.numeric(delivery_days) || length(delivery_days) != 1L || is.na(delivery_days) ||
      delivery_days < 1 || delivery_days %% 1 != 0) bad("delivery_days")
  origin <- if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin
  if (!inherits(origin, "Date") || length(origin) != 1L || is.na(origin)) bad("origin_day")
  end <- lubridate::force_tz(lubridate::as_datetime(origin + delivery_days), cuts$tz)
  seq(au, end, by = "30 min")[-1L]
}

## --- Fachada de feature engineering (M5.1) --------------------------------------------------------------
## Despacho puro: a etapa vem do `stage` (tabela fixa) e o `requires` do descriptor; zero calculo de feature,
## zero decisao por nome de modelo (ADR-0001/ADR-0009). O frame chega como o despachante o entrega ao plugin.
prevcarga_fe_stage <- c(train = "feature_engineering", infer = "inference_features")

## Segunda camada do ADR-0010 (a 1a e o registro wire-time): tokens SEM prefixo qualificado (`data.*`, `frame.*`,
## `artifact.*`, ...) de `requires` sao colunas do frame e devem existir nele; lista TODAS as faltantes.
prevcarga_assert_stage_requires <- function(plugin, etapa, frame) {
  req <- plugin$descriptor$stages[[etapa]]$requires
  cols <- req[!grepl("^[a-z_]+[.]", req)]
  faltam <- setdiff(cols, names(frame))
  if (length(faltam) > 0L) {
    prevcarga_abort("contract.column_missing", where = etapa,
                    details = list(plugin = plugin$descriptor$model_id, missing = sort(faltam)))
  }
  invisible(TRUE)
}

## `artifact` e o que a etapa de inferencia recebe (o payload do artefato); um artefato carimbado e desembrulhado.
prevcarga_build_features <- function(plugin, frame, cuts, spec, ctx,
                                     stage = c("train", "infer"), artifact = NULL) {
  stage <- match.arg(stage)
  etapa <- prevcarga_fe_stage[[stage]]
  prevcarga_assert_stage_requires(plugin, etapa, frame)
  fn <- plugin$descriptor$stages[[etapa]]$fn
  if (stage == "train") {
    return(fn(frame, spec$params))
  }
  if (is.null(artifact)) {
    prevcarga_abort("contract.artifact_required", where = etapa,
                    details = list(plugin = plugin$descriptor$model_id))
  }
  if (inherits(artifact, "prevcarga_plugin_artifact")) artifact <- artifact$payload
  fn(frame, artifact)
}
