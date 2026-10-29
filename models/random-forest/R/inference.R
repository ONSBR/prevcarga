## inference.R -- estagio de inferencia do plugin random-forest (RF.3). SO definicoes.
## Le SO o artefato: nenhum refit, sem `params`, sem RNG (predict de randomForest e deterministico). Duas origens nativas:
## o bloco O-1 (216 horas a partir de O) cobre o dia O; o bloco O (216 horas a partir de O+1) cobre O+1..O+8 (O+9 fica fora
## da faixa de entrega). Cada bloco passa INTEIRO pelo to_semihourly da fundacao com a regra rf_semihourly_rule e so depois
## e cortado; a emenda so concatena por interval_end. Nenhuma aritmetica de hora aqui: a grade horaria e de calendario
## (seq por hora) e o carimbo de 30 min e do to_semihourly. Porte de predict_rf.R:334-470 e run_pipeline.R:503-515
## (somente leitura). Saida ASCII.

## --- predict_rf: features guardadas por lead, nunca as regex ---------------------------------------------------------
predict_rf <- function(artifact, rows) {
  payload <- .rf_payload(artifact)
  if (!requireNamespace("randomForest", quietly = TRUE))
    .rf_abort("plugin.contract", details = list(reason = "pacote randomForest ausente"), where = "random-forest/inference")
  tg <- all_target_names()
  out <- matrix(NA_real_, nrow = nrow(rows), ncol = length(tg), dimnames = list(format(rows$dataOrigem), tg))
  for (j in seq_along(tg)) {
    sub <- payload[[tg[[j]]]]
    X <- as.matrix(rows[, sub$features, with = FALSE])
    out[, j] <- unname(stats::predict(sub$model, newdata = X))
  }
  if (any(!is.finite(out)))
    .rf_abort("plugin.contract", details = list(reason = "predicao nao finita"), where = "random-forest/inference")
  out
}

## --- Regra 24 -> 48 do oraculo (run_pipeline.R:503-515) --------------------------------------------------------------
## 216 ancoras horarias -> 432 valores na ordem (ancora, interpolado, ancora, ...): monoH.FC na grade de meias horas, :30
## interiores presos ao intervalo das duas ancoras vizinhas, ultimo slot (sem ancora seguinte) por extrapolacao linear.
## Funcao pura (sem RNG, sem datas). to_semihourly a chama com o data.table do bloco (coluna `y`).
rf_semihourly_rule <- function(y_hourly) {
  if (is.data.frame(y_hourly)) y_hourly <- y_hourly[["y"]]
  n <- length(y_hourly)
  x <- seq.int(0L, n - 1L)
  grid <- seq(0, n - 0.5, by = 0.5)
  y <- stats::splinefun(x, y_hourly, method = "monoH.FC")(grid)
  even <- which(seq_along(grid) %% 2L == 0L & grid <= max(x))
  for (i in even) {
    lo <- min(y[i - 1L], y[i + 1L]); hi <- max(y[i - 1L], y[i + 1L])
    y[i] <- min(max(y[i], lo), hi)
  }
  beyond <- grid > max(x)
  y[beyond] <- y_hourly[n] + (grid[beyond] - max(x)) * (y_hourly[n] - y_hourly[n - 1L])
  y
}

## --- Emenda dos blocos nativos ---------------------------------------------------------------------------------------
## Bloco T = serie horaria de 216 horas que comeca em T+1 00:00 (interval_end = fim de cada hora; grade de calendario).
## Bloco O-1 so entrega o dia O (last_day = O); bloco O entrega O+1..O+8 (last_day = O+8). O corte e do to_semihourly,
## DEPOIS do spline: nenhum spline atravessa os blocos.
.rf_blocks_to_delivery <- function(values, origins, cuts) {
  O <- .rf_origin(cuts)
  one <- function(b, last_day) {
    t0 <- lubridate::force_tz(lubridate::as_datetime(origins[[b]] + 1L), tzone = cuts$tz)
    ends <- seq(t0, by = "hour", length.out = ncol(values) + 1L)[-1L]
    x <- data.table::data.table(interval_end = ends, y = unname(values[b, ]))
    to_semihourly <- .rf_foundation("to_semihourly")
    to_semihourly(x, rule = rf_semihourly_rule, available_until = cuts$available_until, last_day = last_day)
  }
  d <- data.table::rbindlist(list(one(1L, O), one(2L, O + 8L)))
  data.table::setorder(d, interval_end)
  d[]
}

.rf_check_train_origin <- function(train_origin, cuts) {
  if (!inherits(train_origin, "Date") || length(train_origin) != 1L || is.na(train_origin) ||
      train_origin > cuts$train_origin)
    .rf_abort("plugin.artifact_mismatch", where = "random-forest/inference",
              details = list(field = "train_origin", artifact = format(train_origin), expected = format(cuts$train_origin)))
}

## (artifact, inf_feat, cuts) -> envelope do membro. `artifact` carimbado e conferido contra o descriptor/cortes pelo
## prevcarga_plugin_check_artifact (integrity 9 / contract 2); o payload puro (despachante) ja foi conferido por ele.
plugin_infer <- function(artifact, inf_feat, cuts) {
  if (inherits(artifact, "prevcarga_plugin_artifact")) {
    spec <- if (!is.null(cuts$spec)) cuts$spec else list(granularity_min = 60L, split = "origin_agnostic")
    .rf_foundation("prevcarga_plugin_check_artifact")(artifact, list(descriptor = plugin_descriptor()), spec, cuts)
  }
  payload <- .rf_payload(artifact)
  .rf_check_train_origin(payload$train_origin, cuts)
  .rf_check_train_origin(inf_feat$train_origin, cuts)
  values <- predict_rf(payload, inf_feat$rows)
  delivery <- .rf_blocks_to_delivery(values, inf_feat$origins, cuts)
  if (anyDuplicated(delivery$interval_end) ||
      !identical(delivery$interval_end, .rf_foundation("prevcarga_delivery_grid")(cuts, plugin_constants()$horizon_days_native)))
    .rf_abort("plugin.output_misaligned", where = "random-forest/inference",
              details = list(field = "delivery", reason = "interval_end diferente de prevcarga_delivery_grid(cuts, 9)"))
  list(model_id = "random-forest", variant = NA_character_, area = inf_feat$area, origin_day = .rf_origin(cuts),
       available_until = cuts$available_until, weather_issue_cutoff = cuts$weather_issue_cutoff,
       layout = "rf_wide_hourly_216", granularity_min = 60L, values = values, delivery = delivery,
       meta = list(native_origins = inf_feat$origins, train_origin = payload$train_origin, blocks = inf_feat$blocks,
                   fora_da_faixa = "O+9"))
}

## Assinatura canonica do despachante (payload, inf_features, cuts): so os campos que o despachante aceita do plugin.
rf_inference <- function(payload, inf_features, cuts)
  plugin_infer(payload, inf_features, cuts)[c("area", "values", "delivery", "meta")]
