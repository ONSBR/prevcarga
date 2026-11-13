## test-infer-sem-clima.R -- gate [PLUGINS] infer_sem_clima do Holt-Winters univariado: calendario do horizonte, SemiFer em todo dia
## util com FER 1/2 e n_steps = horizon_days, sobre o artefato FALSO hw_fx_fake_artifact() (sem plugin_train, sem forecast::hw).
env <- hw_env(); st <- hw_fx_store(); cuts <- hw_fx_cuts(st); O <- cuts$origin_day
art <- hw_fx_fake_artifact(env, O = O)
P0 <- art$mean_interval

pan <- function(...) { p <- hw_fx_panel(st, cuts, "infer", weather = FALSE); for (d in list(...)) p <- do.call(hv_flip_fer, c(list(p), d)); p }
run <- function(p, a = art) {
  deck <- env$plugin_ingest(p, cuts)
  list(deck = deck, out = env$plugin_infer(a, env$plugin_inference_features(deck, a, cuts), cuts))
}
sf <- function(k, tipo, type) env$SemiFer(art$carga_temp_ver_fer, tipo, 0.5, art$mean_daily[k + 1L], art$m_mes, type)[[1L]]

test_that("[PLUGINS] infer_sem_clima: painel sem clima => values 8x48, meta so day0_raw/day0/n_steps, entrega ate O+9 00:00, sem aviso", {
  w <- list()
  r <- withCallingHandlers(run(pan(list(date = O + 2L, type = 12L))),
    prevcarga_warning = function(cnd) { w[[length(w) + 1L]] <<- cnd; invokeRestart("muffleWarning") })
  expect_length(w, 0L)
  out <- r$out
  expect_identical(dim(out$values), c(8L, 48L)); expect_true(is.double(out$values) && all(is.finite(out$values)))
  expect_identical(out$meta$n_steps, 8L)
  expect_identical(sort(names(out$meta)), c("day0", "day0_raw", "n_steps"))
  expect_identical(out$meta$day0_raw, as.numeric(P0[1L, ]))
  expect_identical(r$deck$n_steps, 8L)
  g <- hw_lib$prevcarga_delivery_grid(cuts, 9)
  expect_identical(out$delivery$interval_end, g)
  expect_identical(out$delivery$interval_end[[length(g)]], lubridate::ymd_hm("2024-05-24 00:00", tz = HW_TZ))
  expect_identical(out$delivery$interval_end[[1L]], cuts$available_until + lubridate::minutes(30L))
  expect_false(anyNA(out$delivery$y_pred))
  expect_false(any(c("weather_absent", "no_weather_days") %in% names(out$meta)))
})

test_that("[PLUGINS] infer_sem_clima: SemiFer na terca FER 1 e na quinta FER 2 (p_alfa 0,5), diferente do cru; CONTROL domingo FER 1 cru", {
  p <- pan(list(date = O + 6L, type = 12L, code = 1L),      # terca
           list(date = O + 4L, type = 12L, code = 1L),      # domingo
           list(date = O + 1L, type = 12L, code = 2L))      # quinta, FER 2 (S)
  out <- run(p)$out
  expect_identical(as.integer(lubridate::wday(O + 6L)), 3L); expect_identical(as.integer(lubridate::wday(O + 4L)), 1L)
  expect_identical(out$values[6L, ], as.numeric(sf(6L, "F", 12L)))
  expect_false(identical(out$values[6L, ], P0[7L, ]))
  expect_identical(out$values[1L, ], as.numeric(sf(1L, "S", 12L)))
  expect_false(identical(out$values[1L, ], P0[2L, ]))
  expect_identical(out$values[4L, ], P0[5L, ])               # domingo: cru
  expect_identical(out$values[2L, ], P0[3L, ])               # dia sem feriado: cru
  expect_identical(out$meta$day0, out$meta$day0_raw)
})

test_that("[PLUGINS] infer_sem_clima: n_steps 0 ou 9 => hw.input_column; feriado sem historico do tipo => hw.semifer_failed e linha crua", {
  deck <- env$plugin_ingest(pan(), cuts)
  for (n in c(0L, 9L)) {
    d <- deck; d$n_steps <- n
    e <- hw_catch(env$plugin_inference_features(d, art, cuts))
    expect_identical(e$id, "hw.input_column"); expect_identical(e$class_name, "contract"); expect_true(hv_ascii(conditionMessage(e)))
  }
  w <- list()
  out <- withCallingHandlers(run(pan(list(date = O + 6L, type = 99L)))$out,
    prevcarga_warning = function(cnd) { w[[length(w) + 1L]] <<- cnd; invokeRestart("muffleWarning") })
  ids <- vapply(w, function(x) x$id, "")
  expect_true("hw.semifer_failed" %in% ids)
  expect_true(all(vapply(w, function(x) hv_ascii(conditionMessage(x)), NA)))
  expect_identical(out$values[6L, ], P0[7L, ])
})
