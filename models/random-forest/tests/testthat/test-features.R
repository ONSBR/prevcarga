## test-features.R -- RF.2: contrato de features do plugin random-forest contra uma TRANSCRICAO de referencia dos blocos de
## load_model_input (data_adapter.R:444-776 do oraculo, somente leitura), aplicada aos mesmos dados do rf_frame.
## A referencia segue o oraculo literalmente (laco por origem, merge/dcast, difftime trocado por inteiros de dia);
## o plugin e vetorizado: dois caminhos independentes sobre o mesmo frame.
library(testthat)
library(data.table)

## --- Transcricao de referencia -----------------------------------------------------------------------------------
ref_load_model_input <- function(frame, horizon = 9L) {
  hol <- frame$holidays
  dt_load <- data.table(dia = frame$load$dia, hora = frame$load$hora, mean_val_cargaglobalcons = frame$load$load_mw)
  w <- as.data.table(frame$weather)
  origin_days <- sort(unique(w$origin))
  wx <- w[rep(seq_len(nrow(w)), each = 24L)]
  wx[, hora := rep(0L:23L, times = nrow(w))]
  setnames(wx, "origin", "dataOrigem")
  wx[, target_date := dataOrigem + day_of_horizon]
  wx[, lead_hour := (day_of_horizon - 1L) * 24L + hora + 1L]
  dt_joined <- merge(wx, dt_load, by.x = c("target_date", "hora"), by.y = c("dia", "hora"), all.x = TRUE)
  dt_joined <- dt_joined[lead_hour >= 1L & lead_hour <= horizon * 24L]
  tw <- dcast(dt_joined, dataOrigem ~ lead_hour, value.var = "mean_val_cargaglobalcons")
  ex <- intersect(as.character(seq_len(horizon * 24L)), names(tw))
  setnames(tw, ex, paste0("y_h", sprintf("%03d", as.integer(ex))))
  temp_features <- dcast(dt_joined[hora == 0L], dataOrigem ~ day_of_horizon, value.var = c("tmax", "tmin"))
  origins_dt <- unique(dt_joined[lead_hour == 1L, .(dataOrigem)]); setorder(origins_dt, dataOrigem)
  cal <- origins_dt[, .(dataOrigem, origin_dow = lubridate::wday(dataOrigem, week_start = 1L),
                        origin_month = lubridate::month(dataOrigem),
                        origin_is_weekend = lubridate::wday(dataOrigem, week_start = 1L) %in% c(6L, 7L),
                        origin_is_feriado = dataOrigem %in% hol)]
  dowf <- copy(origins_dt)
  for (d in seq_len(horizon)) {
    td <- dowf$dataOrigem + d; tdow <- lubridate::wday(td, week_start = 1L)
    dowf[, paste0("day", d, "_dow") := tdow]
    dowf[, paste0("day", d, "_is_weekend") := tdow %in% c(6L, 7L)]
    for (k in 1:7) dowf[, paste0("day", d, "_is_", c("monday", "tuesday", "wednesday", "thursday", "friday",
                                                     "saturday", "sunday")[k]) := tdow == k]
    dowf[, paste0("day", d, "_is_feriado") := td %in% hol]
  }
  hourf <- copy(origins_dt)
  for (d in seq_len(horizon)) for (h in 0L:23L) {
    pfx <- paste0("h", sprintf("%03d", (d - 1L) * 24L + h + 1L))
    tm <- lubridate::month(hourf$dataOrigem + d)
    hourf[, paste0(pfx, "_is_daily_peak") := h %in% 9L:18L]
    hourf[, paste0(pfx, "_is_evening_peak") := h %in% 18L:23L]
    hourf[, paste0(pfx, "_is_minimum") := h %in% 0L:8L]
    hourf[, paste0(pfx, "_hour_sin") := sin(2 * pi * h / 24)]
    hourf[, paste0(pfx, "_hour_cos") := cos(2 * pi * h / 24)]
    hourf[, paste0(pfx, "_hour_month_interaction") := h * tm]
  }
  seas <- copy(origins_dt)
  for (d in seq_len(horizon)) {
    m <- lubridate::month(cal$dataOrigem + d)
    season <- ifelse(m %in% c(12L, 1L, 2L), "verao", ifelse(m %in% 3:5, "outono", ifelse(m %in% 6:8, "inverno", "primavera")))
    seas[, paste0("day", d, "_month") := m]
    seas[, paste0("day", d, "_quarter") := ceiling(m / 3L)]
    seas[, paste0("day", d, "_is_summer") := season == "verao"]
    seas[, paste0("day", d, "_is_winter") := season == "inverno"]
  }
  xm <- function(x) {  # janelas civis: natal 12-23..12-29; ano-novo 12-30..12-31 e 01-01..01-04
    md <- lubridate::month(x) * 100L + lubridate::mday(x)
    list(c = md >= 1223L & md <= 1229L, n = md >= 1230L | md <= 104L)
  }
  xmas <- copy(origins_dt)
  xmas[, origin_is_christmas := xm(dataOrigem)$c]; xmas[, origin_is_newyear := xm(dataOrigem)$n]
  for (d in seq_len(horizon)) {
    td <- xmas$dataOrigem + d
    xmas[, paste0("day", d, "_is_christmas") := xm(td)$c]; xmas[, paste0("day", d, "_is_newyear") := xm(td)$n]
  }
  hf <- copy(origins_dt); hs <- sort(hol)
  for (i in seq_len(nrow(hf))) {
    d0 <- hf$dataOrigem[i]
    fut <- hs[hs > d0]; pst <- hs[hs < d0]
    set(hf, i, "has_holiday_in_horizon", any((d0 + seq_len(horizon)) %in% hs))
    set(hf, i, "days_to_next_holiday", if (length(fut)) as.numeric(as.integer(min(fut)) - as.integer(d0)) else 365)
    set(hf, i, "days_since_last_holiday", if (length(pst)) as.numeric(as.integer(d0) - as.integer(max(pst))) else 365)
  }
  ds <- dt_load[, .(day_mean = mean(mean_val_cargaglobalcons), day_peak = max(mean_val_cargaglobalcons),
                    day_min = min(mean_val_cargaglobalcons),
                    day_daily_peak = max(mean_val_cargaglobalcons[hora %in% 9L:17L]),
                    day_evening_peak = max(mean_val_cargaglobalcons[hora %in% 18L:23L])), by = dia]
  ds[, day_dow := lubridate::wday(dia, week_start = 1L)]
  ds[, day_is_weekend := day_dow %in% c(6L, 7L)]
  ds[, day_is_feriado := dia %in% hol]
  prev <- function(lag, pfx) {
    p <- copy(ds); p[, join_date := dia + lag]
    o <- p[origins_dt, on = .(join_date = dataOrigem)]
    setnames(o, "join_date", "dataOrigem"); o[, dia := NULL]
    old <- c("day_mean", "day_peak", "day_min", "day_daily_peak", "day_evening_peak", "day_dow", "day_is_weekend",
             "day_is_feriado")
    setnames(o, old, paste0(pfx, c("mean", "peak", "min", "daily_peak", "evening_peak", "dow", "is_weekend", "is_feriado")))
    o
  }
  dm <- ds[, .(dia, daily_mean = day_mean)]
  rec <- copy(origins_dt)
  for (i in seq_len(nrow(rec))) {
    d0 <- rec$dataOrigem[i]
    l7 <- dm[dia >= (d0 - 7L) & dia <= (d0 - 1L), daily_mean]; l3 <- dm[dia >= (d0 - 3L) & dia <= (d0 - 1L), daily_mean]
    m7 <- if (length(l7)) mean(l7, na.rm = TRUE) else NA_real_
    m3 <- if (length(l3)) mean(l3, na.rm = TRUE) else NA_real_
    set(rec, i, "prev_7day_mean", m7); set(rec, i, "prev_3day_mean", m3)
    set(rec, i, "recent_vs_historical", if (!is.na(m3) && !is.na(m7) && m7 != 0) m3 / m7 else NA_real_)
  }
  fl <- list(temp_features, cal, dowf, hourf, seas, xmas, hf, prev(1L, "prev_day_"), prev(2L, "prev_2day_"), rec)
  fw <- Reduce(function(x, y) merge(x, y, by = "dataOrigem", all = TRUE), fl)
  res <- merge(fw, tw, by = "dataOrigem", all.x = TRUE); setorder(res, dataOrigem)
  res
}

## frame de treino sobre o deck de pc_fixture_store (O = 2026-10-01, train_origin = 2026-09-21), janela real de 365 dias
fx <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      p <- rf_plug()
      spec <- rf_fx_spec(5L)
      cuts <- rf_fx_cuts(); cuts$spec <- spec
      frame <- p$plugin_ingest(rf_fx_panel(cuts = cuts), cuts, spec)
      cache <<- list(p = p, spec = spec, cuts = cuts, frame = frame, feats = p$plugin_build_features(frame, spec))
    }
    cache
  }
})

test_that("[PLUGINS] features_contract: 1486 colunas e 216 alvos na ordem de colunas do merge do oraculo", {
  p <- rf_plug()
  fn <- p$all_feature_names()
  expect_length(fn, 1486L)
  expect_false(anyDuplicated(fn) > 0L)
  expect_identical(p$all_target_names(), sprintf("y_h%03d", 1:216))
  ## blocos na ordem do merge: temp, origem, dow x9, hora x216, sazonal x9, natal/ano-novo, feriados, prev x2, recentes
  expect_identical(fn[1:18], c(paste0("tmax_", 1:9), paste0("tmin_", 1:9)))
  expect_identical(fn[19:22], c("origin_dow", "origin_month", "origin_is_weekend", "origin_is_feriado"))
  expect_identical(fn[23:32], p$.rf_dow_names(1L))
  expect_identical(fn[113:118], p$.rf_hour_names(1L))
  expect_identical(fn[1409:1412], p$.rf_seasonal_names(1L))
  expect_identical(fn[1445:1448], c("origin_is_christmas", "origin_is_newyear", "day1_is_christmas", "day1_is_newyear"))
  expect_identical(fn[1465:1467], c("has_holiday_in_horizon", "days_to_next_holiday", "days_since_last_holiday"))
  expect_identical(fn[1468:1475], paste0("prev_day_", c("mean", "peak", "min", "daily_peak", "evening_peak", "dow", "is_weekend", "is_feriado")))
  expect_identical(tail(fn, 3L), c("prev_7day_mean", "prev_3day_mean", "recent_vs_historical"))
  expect_length(p$.rf_hour_names(7L), 6L)
  expect_length(p$.rf_dow_names(3L), 10L)
})

test_that("[PLUGINS] features_contract: tabela do plugin == transcricao de referencia (colunas na mesma ordem; num <= 1e-12; logicos identical)", {
  z <- fx(); tb <- z$feats$table
  ref <- ref_load_model_input(z$frame)
  expect_identical(names(tb), names(ref))
  expect_identical(names(tb), c("dataOrigem", z$feats$feature_cols, z$feats$target_cols))
  expect_identical(tb$dataOrigem, ref$dataOrigem)
  expect_identical(nrow(tb), length(z$frame$origins))
  expect_false(is.unsorted(tb$dataOrigem, strictly = TRUE))
  bad <- character()
  for (cn in names(ref)) {
    a <- tb[[cn]]; b <- ref[[cn]]
    ok <- if (is.logical(b)) identical(a, b) else
      identical(is.na(a), is.na(b)) && all(abs(as.numeric(a) - as.numeric(b)) <= 1e-12, na.rm = TRUE)
    if (!ok) bad <- c(bad, cn)
  }
  expect_identical(bad, character())
  expect_identical(z$feats$phase, "train")
  expect_identical(z$feats$n_test, 0L)
  expect_equal(z$feats$train_origin, RF_O - 10L)
})

test_that("[PLUGINS] features_contract: y_h216 da ultima origem = media horaria de 2026-09-30 23:00 (O-1); alvos NA so fora do frame", {
  z <- fx(); tb <- z$feats$table
  expect_equal(tail(tb$dataOrigem, 1L), lubridate::ymd("2026-09-21"))
  ## da carga de 30 min do leitor: media dos intervalos 23:00 e 23:30 de 2026-09-30
  ld <- rf_fx_panel(cuts = z$cuts, hourly = FALSE)$load
  slots <- ld[interval_start >= lubridate::ymd_hm("2026-09-30 23:00", tz = RF_TZ) &
              interval_start <= lubridate::ymd_hm("2026-09-30 23:30", tz = RF_TZ)]
  expect_identical(nrow(slots), 2L)
  expect_lt(abs(tail(tb$y_h216, 1L) - mean(slots$load_mwh)), 1e-9)
  expect_false(anyNA(tb[, c(z$feats$target_cols), with = FALSE]))
  expect_false(anyNA(tb[, c(z$feats$feature_cols), with = FALSE]))
  ## phase infer: sem alvos na carga => NA (all.x do oraculo)
  fr <- z$frame; fr$phase <- "infer"; fr$train_origin <- lubridate::as_date(NA)
  fr$origins <- tail(fr$origins, 2L)
  fr$load <- fr$load[dia <= max(dia) - 8L]
  ti <- z$p$plugin_build_features(fr, z$spec)$table
  expect_true(all(is.na(ti$y_h216)))
})

test_that("[PLUGINS] features_contract: constantes do oraculo (picos, natal/ano-novo, 365 sem feriado, week_start = 1)", {
  z <- fx(); tb <- z$feats$table
  expect_identical(tb$h010_is_daily_peak[[1L]], TRUE)    # hora 9 em 9:18
  expect_identical(tb$h001_is_daily_peak[[1L]], FALSE)
  expect_identical(tb$h019_is_evening_peak[[1L]], TRUE)  # hora 18
  expect_identical(tb$h001_is_minimum[[1L]], TRUE)
  expect_equal(tb$h007_hour_sin[[1L]], sin(2 * pi * 6 / 24))
  p <- rf_plug()
  d <- lubridate::ymd(c("2025-12-22", "2025-12-23", "2025-12-29", "2025-12-30", "2026-01-04", "2026-01-05"))
  expect_identical(p$.rf_is_christmas(d), c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(p$.rf_is_newyear(d), c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  ## sem feriado: 365
  b <- p$.rf_block_holiday(lubridate::ymd("2030-06-10"), lubridate::ymd("2030-06-10"))
  expect_identical(b$days_to_next_holiday, 365); expect_identical(b$days_since_last_holiday, 365)
  b2 <- p$.rf_block_holiday(lubridate::ymd("2026-09-03"), lubridate::ymd("2026-09-07"))
  expect_identical(b2$has_holiday_in_horizon, TRUE); expect_identical(b2$days_to_next_holiday, 4)
  ## segunda = 1, domingo = 7
  expect_equal(p$.rf_block_calendar(lubridate::ymd("2026-09-21"), lubridate::ymd(character()))$origin_dow, 1L, ignore_attr = TRUE)
})

test_that("[PLUGINS] features_contract: .select_features_for_lead (uniao na ordem legada) e dimensoes", {
  p <- rf_plug(); fc <- p$all_feature_names()
  s1 <- p$.select_features_for_lead(1L, fc); s216 <- p$.select_features_for_lead(216L, fc)
  expect_length(s1, 52L); expect_length(s216, 68L)
  expect_identical(s1[1:6], grep("^origin_", fc, value = TRUE))
  expect_identical(s1[7:8], c("tmax_1", "tmin_1"))
  expect_true(all(grepl("^day1_", s1[9:24])))
  expect_identical(s1[25:30], p$.rf_hour_names(1L))
  expect_true(all(c("has_holiday_in_horizon", "days_to_next_holiday", "days_since_last_holiday") %in% s1))
  expect_false(anyDuplicated(s216) > 0L)
  expect_true(all(c(paste0("tmax_", 1:9), paste0("tmin_", 1:9)) %in% s216))
  expect_true(all(grepl("^h216_", s216[grepl("^h[0-9]{3}_", s216)])))
  ## coluna ausente sai silenciosamente (intersect)
  expect_false("tmax_1" %in% p$.select_features_for_lead(1L, setdiff(fc, "tmax_1")))
})

test_that("[PLUGINS] features_contract: CONTROL validate_feature_table sem tmax_9 => [prevcarga/contract] exit 2 nomeando a coluna; nao reordena nem descarta", {
  z <- fx(); p <- z$p
  expect_identical(p$validate_feature_table(z$feats$table), invisible(z$feats$table))
  t2 <- data.table::copy(z$feats$table)[, tmax_9 := NULL]
  e <- fail(p$validate_feature_table(t2))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  expect_identical(e$details$column, "tmax_9")
  expect_identical(nrow(t2), nrow(z$feats$table))     # a guarda nao altera a tabela
  t3 <- data.table::copy(z$feats$table)[, dataOrigem := NULL]
  expect_identical(fail(p$validate_feature_table(t3))$details$column, "dataOrigem")
  ## frame de treino com carga alem de train_origin + H => contract
  fr <- z$frame
  fr$train_origin <- fr$train_origin - 1L
  e2 <- fail(p$plugin_build_features(fr, z$spec))
  expect_identical(e2$class_name, "contract")
})
