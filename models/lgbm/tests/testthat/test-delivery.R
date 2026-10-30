## test-delivery.R -- LGBM.4: entrega = prevcarga_delivery_grid(cuts, 2) (grade da fundacao), values 2x48/1x48 no layout lgbm_day_48 e
## sentinela de indexacao (cada carga codifica o proprio interval_start). Artefato treinado uma vez (lgbm_fx_base).
library(testthat)

P <- lgbm_source_plugin()
b <- lgbm_fx_base()
O <- LGBM_O
at_time <- function(s) lubridate::ymd_hm(s, tz = LGBM_TZ)

test_that("[PLUGINS] gate09_fuso: [AC4] available_until 09:00/09:30/O+1 00:00: delivery == grade da fundacao (78/77/48), 30 min sem buraco, values 2x48/2x48/1x48", {
  cases <- list(list("2026-10-01 09:00", 78L, "2026-10-01 09:30", 2L), list("2026-10-01 09:30", 77L, "2026-10-01 10:00", 2L),
                list("2026-10-02 00:00", 48L, "2026-10-02 00:30", 1L))
  for (cs in cases) {
    a <- lgbm_fx_at(at_time(cs[[1L]]))
    expect_identical(a$cuts$available_until, at_time(cs[[1L]]))
    env <- lgbm_fx_infer(b, a$panel, a$cuts)
    ie <- env$delivery$interval_end
    expect_identical(ie, lgbm_lib$prevcarga_delivery_grid(a$cuts, 2L), info = cs[[1L]])
    expect_identical(nrow(env$delivery), cs[[2L]], info = cs[[1L]])
    expect_identical(ie[[1L]], at_time(cs[[3L]])); expect_identical(ie[[length(ie)]], at_time("2026-10-03 00:00"))
    expect_true(all(diff(as.numeric(ie)) == 1800)); expect_false(anyDuplicated(ie) > 0L)
    expect_identical(lubridate::tz(ie), a$cuts$tz)
    expect_identical(dim(env$values), c(cs[[4L]], 48L), info = cs[[1L]])
    expect_identical(names(env$delivery), c("interval_end", "y_pred"))
    expect_identical(env$layout, "lgbm_day_48"); expect_identical(env$granularity_min, 30L)
    ## values <-> delivery: o trecho de O vem do dia O completado, o de O+1 da Passada 2, ligados por interval_end = DataHora + 30 min
    ends <- at_time("2026-10-01 00:00") + lubridate::minutes(30L * (1:96))
    flat <- if (cs[[4L]] == 2L) c(env$values[1L, ], env$values[2L, ]) else c(rep(NA_real_, 48L), env$values[1L, ])
    expect_identical(env$delivery$y_pred, flat[match(ie, ends)], info = cs[[1L]])
    ## sem forecast_origin/lead/d_plus/y_true (M6)
    expect_false(any(c("forecast_origin", "lead", "d_plus", "y_true", "target_datetime") %in% names(env$delivery)))
  }
})

test_that("[PLUGINS] gate_nowcast_dplus0: [AC4] sentinela: a carga real de 08:30-09:00 aparece no Stage B na posicao Hora 17 (interval_start); CONTROL: indexar por interval_end reprova", {
  deck <- lgbm_fx_deck(days = 120L, available_until = at_time("2026-10-01 09:00"), sentinel = TRUE)
  ctx <- lgbm_fx_ctx(deck); cuts <- lgbm_fx_cuts(ctx)
  panel <- lgbm_fx_dpanel(deck, ctx, "infer", cuts)
  expect_identical(cuts$available_until, at_time("2026-10-01 09:00"))
  minutes_of <- function(s) as.numeric(difftime(at_time(s), at_time("2026-09-01 00:00"), units = "mins"))
  ## o leitor entrega o sentinela de cada interval_start
  k <- which(panel$load$interval_start == at_time("2026-10-01 08:30"))
  expect_identical(panel$load$load_mwh[[k]], minutes_of("2026-10-01 08:30"))
  frame <- P$plugin_ingest(panel, cuts)
  inf <- P$plugin_inference_features(frame, b$art$payload)
  dD <- inf$dt_D[order(Hora)]
  expect_identical(dD[Hora == 17L, CargaGlobal], minutes_of("2026-10-01 08:30"))        # Hora 17 = interval_start 08:30
  expect_identical(dD[Hora == 17L, DataHora], at_time("2026-10-01 08:30"))
  expect_identical(inf$n_real_D, 18L)
  art <- P$plugin_thaw(b$art$payload, cuts)
  dn <- data.table::copy(dD); P$lgbm_normalise_dt(dn, art$min_max_BLF)
  pA <- P$lgbm_stage_a(art$booster, as.matrix(dn[, art$colunas_entrada_BLF, with = FALSE]), art$best_iter)
  comp <- P$lgbm_stage_b(pA, dD, art)
  want <- vapply(0:17, function(i) minutes_of(format(at_time("2026-10-01 00:00") + lubridate::minutes(30L * i), "%Y-%m-%d %H:%M")), 0)
  expect_lte(max(abs(comp[1:18] - want)), 1e-6)                                         # as 18 primeiras posicoes sao o sentinela
  sentinel_ok <- function(x) abs(x[[18L]] - minutes_of("2026-10-01 08:30")) <= 1e-6
  expect_true(sentinel_ok(comp))
  ## CONTROL: indexar por interval_end desloca o valor para a posicao seguinte (Hora 18) e o sentinela reprova
  dC <- data.table::copy(dD)
  dC[, `:=`(CargaGlobal = c(NA_real_, CargaGlobal[-48L]), is_synthetic = c(FALSE, is_synthetic[-48L]))]
  expect_false(sentinel_ok(P$lgbm_stage_b(pA, dC, art)))
  ## entrega pelo despachante: o y_pred de O (09:30 em diante) nao e sentinela; a grade comeca em 09:30
  env <- lgbm_fx_infer(b, panel, cuts)
  expect_identical(env$delivery$interval_end[[1L]], at_time("2026-10-01 09:30"))
  expect_identical(env$meta$n_real_D, 18L)
})
