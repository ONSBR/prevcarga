## test-weather.R -- gate [PLUGINS] clima_cego do Holt-Winters (HW univariado; substitui clima_honesto/gate08 e
## clima_fallback/gate09 do oraculo, que so faziam sentido com clima). Propriedade honesta: o HW NAO le clima. Nada treina
## e nada chama forecast::hw: o infer roda sobre o artefato FALSO hw_fx_fake_artifact() (plugin_freeze do modelo falso).
##   FORBIDDEN: clima NULL, temperatura x1,3+1 e cobertura so ate O+3 => deck de plugin_ingest (a unica entrada do treino) e
##              envelope de plugin_inference_features + plugin_infer identical() (Delta 0).
##   CONTROL:   trocar o FER de um dia util do horizonte muda so a linha dele; alterar a carga de O-1 muda o deck (Delta != 0).
env <- hw_env(); st <- hw_fx_store(); cuts <- hw_fx_cuts(st)
art <- hw_fx_fake_artifact(env, O = cuts$origin_day)
O <- cuts$origin_day

variant <- function(kind) {
  p <- hw_fx_panel(st, cuts, "infer", weather = !identical(kind, "null"))
  if (identical(kind, "scaled")) p$weather_forecast$temperature <- p$weather_forecast$temperature * 1.3 + 1
  if (identical(kind, "cover3"))
    p$weather_forecast <- p$weather_forecast[lubridate::as_date(valid_time, tz = cuts$tz) <= O + 3L]
  p
}
run <- function(p) {
  deck <- env$plugin_ingest(p, cuts)
  inf <- env$plugin_inference_features(deck, art, cuts)
  list(deck = deck, inf = inf, out = env$plugin_infer(art, inf, cuts))
}

test_that("[PLUGINS] clima_cego: clima NULL / x1,3+1 / cobertura ate O+3 => deck e envelope identical, sem aviso de clima (FORBIDDEN)", {
  w <- list()
  res <- withCallingHandlers(lapply(c("full", "null", "scaled", "cover3"), function(k) run(variant(k))),
    prevcarga_warning = function(cnd) { w[[length(w) + 1L]] <<- cnd; invokeRestart("muffleWarning") })
  expect_length(w, 0L)
  expect_identical(dim(res[[1L]]$out$values), c(8L, 48L))
  for (r in res[-1L]) {
    expect_identical(r$deck, res[[1L]]$deck)
    expect_identical(r$inf, res[[1L]]$inf)
    expect_identical(r$out, res[[1L]]$out)
  }
})

test_that("[PLUGINS] clima_cego: FER de um dia util do horizonte muda so a linha dele; carga de O-1 muda o deck (CONTROL)", {
  p <- variant("full"); k <- 2L
  expect_true(lubridate::wday(O + k) %in% 2:6)
  p2 <- hv_flip_fer(p, O + k)
  a <- run(p); b <- run(p2)
  expect_identical(b$deck$horizon_calendar$FER[b$deck$horizon_calendar$DIA == O + k], 1L)
  d <- a$out$values - b$out$values
  expect_identical(which(rowSums(abs(d)) > 0), k)
  ## CONTROL do deck: a carga de O-1 e a unica entrada do treino que o clima nao toca
  p3 <- p
  i <- lubridate::as_date(p3$load$interval_start, tz = cuts$tz) == O - 1L
  p3$load <- data.table::copy(p3$load); p3$load$load_mwh[i] <- p3$load$load_mwh[i] * 1.3 + 1
  expect_false(identical(run(p3)$deck, a$deck))
  expect_false(identical(run(p3)$deck$CargaTempVer, a$deck$CargaTempVer))
})
