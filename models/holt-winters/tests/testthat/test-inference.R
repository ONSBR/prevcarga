## test-inference.R -- gates [PLUGINS] infer_* / efeito_temp / semifer_loess do Holt-Winters (HW.4; AC 1-7). Cadeia
## ingest -> FE -> train -> freeze sobre o store sintetico (P.0e) e infer SO sobre o artefato. Espioes: clone do env do
## plugin (FE) e local_mocked_bindings no forecast::hw. O oraculo (somente leitura) so e carregado para o efeitoTemp.

if (!exists("prevcarga_freeze", envir = hw_lib, inherits = FALSE))
  sys.source(file.path(hw_root(), "scripts", "lib", "provenance.R"), envir = hw_lib)

HI_O <- lubridate::ymd("2024-05-15")
hi_au <- function(hhmm) lubridate::ymd_hm(paste("2024-05-15", hhmm), tz = HW_TZ)
hi_spec <- function() { s <- hw_spec(); s$history_days$train <- 59L; s }

## fundacao no global so durante `code` (o plugin carregado pelo loader a procura la; interino como engine_abort)
hi_with_cli <- function(code) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_plugin_scan_pure", "prevcarga_delivery_grid")
  g <- globalenv(); had <- nm[vapply(nm, exists, NA, envir = g, inherits = FALSE)]; old <- mget(had, envir = g)
  for (n in nm) assign(n, hw_lib[[n]], envir = g)
  ## restaura o que o runner (plugin_gates.R) ja tinha no global (HW.5): rm() puro apagava prevcarga_abort do runner
  on.exit({ rm(list = nm, envir = g); for (n in had) assign(n, old[[n]], envir = g) }, add = TRUE)
  force(code)
}

## store/cuts/paineis/deck de uma origem com carga do dia O ate `hhmm`
hi_ctx <- function(hhmm = "09:00", days = 60L) {
  st <- hw_fx_store(load_until = hi_au(hhmm)); cuts <- hw_fx_cuts(st)
  list(store = st, cuts = cuts, tpan = hw_fx_panel(st, cuts, "train", history_days = days),
       ipan = hw_fx_panel(st, cuts, "infer", history_days = days))
}

hi_base <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      env <- hw_env(); spec <- hi_spec(); cx <- hi_ctx("09:00")
      tdeck <- env$plugin_ingest(cx$tpan, cx$cuts)
      feats <- suppressMessages(env$plugin_build_features(tdeck, spec))
      art <- env$plugin_freeze(env$plugin_train(feats, spec))
      deck <- suppressWarnings(env$plugin_ingest(cx$ipan, cx$cuts))
      cache <<- c(cx, list(env = env, spec = spec, art = art, deck = deck))
    }
    cache
  }
})

hi_run <- function(env, art, deck, cuts) {
  inf <- suppressWarnings(env$plugin_inference_features(deck, art, cuts))
  suppressWarnings(env$plugin_infer(art, inf, cuts))
}

hi_loess <- function(env, v) env$Suaviza.LOESS1(v, env$plugin_constants()$loess_span_out)[[1L]]

test_that("[PLUGINS] infer_envelope: forma, grade available_until+30 .. O+9 00:00, tzone, sem colunas do M6; passa o despachante", {
  b <- hi_base(); env <- b$env; cuts <- b$cuts
  out <- hi_run(env, b$art, b$deck, cuts)
  expect_identical(dim(out$values), c(8L, 48L)); expect_true(is.double(out$values) && all(is.finite(out$values)))
  expect_identical(out$meta$day0_raw, as.numeric(b$art$mean_interval[1L, ]))
  expect_length(out$meta$day0, 48L); expect_identical(out$meta$n_steps, 8L); expect_false(out$meta$weather_absent)
  expect_identical(out$area, "ZZ1")
  g <- hw_lib$prevcarga_delivery_grid(cuts, 9)
  expect_identical(out$delivery$interval_end, g)
  expect_identical(out$delivery$interval_end[[1L]], hi_au("09:30"))
  expect_identical(out$delivery$interval_end[[length(g)]], lubridate::ymd_hm("2024-05-24 00:00", tz = HW_TZ))
  expect_identical(lubridate::tz(out$delivery$interval_end), cuts$tz)
  expect_true(all(diff(as.numeric(g)) == 1800) && !anyNA(out$delivery$y_pred) && is.double(out$delivery$y_pred))
  expect_false(any(c("forecast_origin", "lead", "d_plus", "y_true", "target_datetime") %in%
                     c(names(out), names(out$meta), names(out$delivery))))
  ## y_pred = P9 achatado por posicao: O+1 H01 termina 00:30
  k <- match(lubridate::ymd_hm("2024-05-16 00:30", tz = HW_TZ), out$delivery$interval_end)
  expect_identical(out$delivery$y_pred[[k]], out$values[1L, 1L])
  ## despachante P.0c (loader): treino + infer pelo plugin real; envelope confere
  mr <- file.path(hw_root(), "models")
  hi_with_cli({
    p <- hw_lib$prevcarga_plugin_load("holt-winters", mr)
    ctx <- list(load_label = cuts$load_label, timezone = cuts$tz, weather_issue_cutoff = cuts$weather_issue_cutoff)
    art <- suppressWarnings(suppressMessages(hw_lib$prevcarga_plugin_train(p, b$tpan, cuts, p$spec, ctx)))
    env2 <- suppressWarnings(hw_lib$prevcarga_plugin_infer(p, art, b$ipan, cuts))
    expect_identical(env2$model_id, "holt-winters"); expect_true(is.na(env2$variant))
    expect_identical(env2$layout, "hw_matrix_nx48"); expect_identical(env2$granularity_min, 30L)
    expect_identical(env2$delivery$interval_end, g)
    expect_identical(env2$values, out$values)
  })
})

test_that("[PLUGINS] infer_sem_refit: 09:00/09:30/12:00 => valores identicos, 1o interval_end 09:30/10:00/12:30, forecast::hw = 0, sem params", {
  b <- hi_base(); env <- b$env
  expect_false("params" %in% names(formals(env$plugin_infer)))
  expect_false("params" %in% names(formals(env$plugin_inference_features)))
  n_hw <- 0L
  testthat::local_mocked_bindings(hw = function(...) { n_hw <<- n_hw + 1L; stop("forecast::hw chamado no infer") },
                                  .package = "forecast")
  res <- lapply(c("09:00", "09:30", "12:00"), function(h) {
    cx <- hi_ctx(h); deck <- suppressWarnings(env$plugin_ingest(cx$ipan, cx$cuts))
    list(cuts = cx$cuts, out = hi_run(env, b$art, deck, cx$cuts))
  })
  expect_identical(n_hw, 0L)
  expect_identical(vapply(res, function(r) format(r$out$delivery$interval_end[[1L]], "%H:%M"), ""),
                   c("09:30", "10:00", "12:30"))
  for (r in res[-1L]) {
    expect_identical(r$out$values, res[[1L]]$out$values)
    expect_identical(r$out$meta$day0_raw, res[[1L]]$out$meta$day0_raw)
    expect_identical(r$out$meta$day0, res[[1L]]$out$meta$day0)
    com <- intersect(as.numeric(r$out$delivery$interval_end), as.numeric(res[[1L]]$out$delivery$interval_end))
    expect_gt(length(com), 400L)
    a <- res[[1L]]$out$delivery; bb <- r$out$delivery
    expect_identical(a$y_pred[match(com, as.numeric(a$interval_end))], bb$y_pred[match(com, as.numeric(bb$interval_end))])
  }
  ## o plugin nao chama forecast:: nos arquivos de inferencia
  expect_length(grep("^inference", hw_grep_code(file.path(hw_root(), "models", "holt-winters", "R"), "forecast::"), value = TRUE), 0L)
})

test_that("[PLUGINS] efeito_temp: legado == oraculo em O+1..O+n; dia O com qdata proprio; CONTROL dia O no mesmo laco contamina O+1", {
  b <- hi_base(); env <- b$env
  ctv3 <- env$.build_CargaTempVer3(b$art$carga_temp_ver, HI_O)
  expect_true(all(ctv3$Feriado == 0) && all(lubridate::month(ctv3$Data) %in% c(4L, 5L)))
  cdd <- data.frame(DIA = HI_O + 0:8, TEMP = b$deck$CDDPre$TEMP[1:9], DS = b$deck$CDDPre$DS[1:9])
  ef <- env$.hw_efeito_temp(ctv3, cdd, HI_O)
  oracle <- file.path("/opt/ons-source/prevcargadessem3", "models/holt-winters/R/inference_features/hw_inference_features.R")
  if (file.exists(oracle)) {
    orc <- new.env(parent = globalenv()); suppressMessages(sys.source(oracle, envir = orc))
    expect_identical(ctv3, orc$.build_CargaTempVer3(b$art$carga_temp_ver, HI_O))
    expect_identical(ef[-1L], orc$.compute_efeitoTemp_legacy(ctv3, cdd$DS[-1L], cdd$TEMP[-1L]))
    expect_identical(env$.compute_efeitoTemp_legacy(ctv3, cdd$DS, cdd$TEMP),
                     orc$.compute_efeitoTemp_legacy(ctv3, cdd$DS, cdd$TEMP))
  }
  expect_identical(ef[1L], env$.compute_efeitoTemp_legacy(ctv3, cdd$DS[1L], cdd$TEMP[1L]))
  ## fixture sintetico: o dia O (quarta, DS 4) tem mais linhas do que o dia seguinte (quinta, DS 5)
  syn <- data.frame(DiaSemana = c(4L, 4L, 4L, 4L, 4L, 5L, 5L), CargaMedia = 1000,
                    TempMedia = c(10, 12, 14, 16, 18, 25, 27))
  sc <- data.frame(DIA = HI_O + 0:1, TEMP = c(20, 26), DS = c(4L, 5L))
  own <- env$.hw_efeito_temp(syn, sc, HI_O)
  expect_identical(own[2L], env$.compute_efeitoTemp_legacy(syn, 5L, 26))
  expect_identical(own[2L], 0.5)
  same_loop <- env$.compute_efeitoTemp_legacy(syn, sc$DS, sc$TEMP)           # CONTROL: O no mesmo laco
  expect_identical(same_loop[2L], 0.8)
  expect_false(identical(same_loop[2L], own[2L]))
})

## frame com feriados no horizonte (mesmo tipo 12 do historico do fixture): terca O+6 FER 1, quinta O+1 FER 2, domingo O+4 FER 1
hi_holiday_frame <- function(b) {
  d <- b$deck
  fer <- c(O1 = 2L, O4 = 1L, O6 = 1L)
  dias <- c(HI_O + 1L, HI_O + 4L, HI_O + 6L)
  d$TempPre$FER[] <- 0L
  i <- match(dias, d$TempPre$DIA); d$TempPre$FER[i[!is.na(i)]] <- unname(fer)[!is.na(i)]
  d$CDDPre$TIPO_FER[] <- 0L
  j <- match(dias, d$CDDPre$DIA); d$CDDPre$TIPO_FER[j[!is.na(j)]] <- 12L
  d
}

test_that("[PLUGINS] semifer_loess: terca/quinta pelo SemiFer, domingo cru, LOESS 0,13 em todas; CONTROL suaviza false", {
  b <- hi_base(); env <- b$env; cuts <- b$cuts
  deck <- hi_holiday_frame(b)
  inf <- env$plugin_inference_features(deck, b$art, cuts)
  hm <- inf$horizon_meta
  expect_identical(hm$FER[hm$DIA == HI_O + 6L], 1L); expect_identical(hm$mDom[hm$DIA == HI_O + 6L], 2L)
  expect_identical(hm$mDom[hm$DIA == HI_O + 4L], 0L); expect_identical(hm$mDom[hm$DIA == HI_O + 1L], 4L)
  expect_true(is.integer(hm$mDom))
  out <- env$plugin_infer(b$art, inf, cuts)
  P0 <- b$art$mean_interval
  sf <- function(k, tipo) env$SemiFer(b$art$carga_temp_ver_fer, tipo, hm$efeitoTemp[k], b$art$mean_daily[k] + 0,
                                      b$art$m_mes, 12L)[[1L]]
  ## values[k - 1, ] = linha k do P9
  expect_identical(out$values[6L, ], hi_loess(env, as.numeric(sf(7L, "F"))))      # terca (FER 1)
  expect_identical(out$values[1L, ], hi_loess(env, as.numeric(sf(2L, "S"))))      # quinta (FER 2)
  expect_identical(out$values[4L, ], hi_loess(env, P0[5L, ]))                     # domingo: $mean cru + LOESS
  expect_false(identical(out$values[6L, ], hi_loess(env, P0[7L, ])))
  expect_false(identical(out$values[1L, ], hi_loess(env, P0[2L, ])))
  expect_identical(out$meta$day0, hi_loess(env, P0[1L, ]))                         # dia O tambem suavizado
  expect_identical(out$values[2L, ], hi_loess(env, P0[3L, ]))
  ## CONTROL: suaviza false no artefato => linhas sem feriado == $mean cru; feriados seguem pelo SemiFer
  a2 <- b$art; a2$params$suaviza <- FALSE
  o2 <- env$plugin_infer(a2, env$plugin_inference_features(deck, a2, cuts), cuts)
  expect_identical(o2$values[2L, ], P0[3L, ]); expect_identical(o2$values[4L, ], P0[5L, ])
  expect_identical(o2$meta$day0, o2$meta$day0_raw)
  expect_identical(o2$values[6L, ], as.numeric(sf(7L, "F")))
  expect_false(identical(o2$values[2L, ], out$values[2L, ]))
})

test_that("[PLUGINS] infer_clima: sem clima => 8 linhas + aviso ASCII; cobertura O..O+5 => 5 linhas e entrega de 9 dias", {
  b <- hi_base(); env <- b$env; cuts <- b$cuts
  ## sem emissao valida
  p0 <- b$ipan; p0$weather_forecast <- p0$weather_forecast[0L]
  w <- NULL
  deck0 <- withCallingHandlers(env$plugin_ingest(p0, cuts),
                               prevcarga_warning = function(c) { w <<- c; invokeRestart("muffleWarning") })
  expect_identical(w$id, "hw.weather_absent"); expect_true(all(utf8ToInt(conditionMessage(w)) < 128L))
  expect_true(deck0$weather_absent)
  d0 <- hi_holiday_frame(list(deck = deck0))
  out0 <- env$plugin_infer(b$art, env$plugin_inference_features(d0, b$art, cuts), cuts)
  expect_identical(dim(out0$values), c(8L, 48L)); expect_false(anyNA(out0$values))
  expect_true(out0$meta$weather_absent)
  expect_identical(out0$meta$no_weather_days, HI_O + 1:8)
  expect_identical(out0$values[1L, ], hi_loess(env, b$art$mean_interval[2L, ]))   # FER 2 presente mas SemiFer pulado
  expect_identical(nrow(out0$delivery), 414L)
  ## cobertura parcial O..O+5
  p5 <- b$ipan
  p5$weather_forecast <- p5$weather_forecast[lubridate::as_date(valid_time, tz = cuts$tz) <= HI_O + 5L]
  deck5 <- suppressWarnings(env$plugin_ingest(p5, cuts))
  expect_identical(deck5$n_steps, 5L); expect_false(deck5$weather_absent)
  d5 <- hi_holiday_frame(list(deck = deck5))
  out5 <- env$plugin_infer(b$art, env$plugin_inference_features(d5, b$art, cuts), cuts)
  expect_identical(dim(out5$values), c(5L, 48L)); expect_identical(out5$meta$n_steps, 5L)
  expect_identical(out5$meta$no_weather_days, HI_O + 6:8)
  expect_identical(out5$delivery$interval_end, hw_lib$prevcarga_delivery_grid(cuts, 9))
  expect_identical(nrow(out5$delivery), 414L)
  ## dias sem clima: $mean + LOESS, sem SemiFer (O+6 e terca FER 1 no frame, mas sem clima)
  k <- match(lubridate::ymd_hm("2024-05-21 12:00", tz = HW_TZ), out5$delivery$interval_end)
  expect_identical(out5$delivery$y_pred[[k]], hi_loess(env, b$art$mean_interval[7L, ])[24L])
})

test_that("[PLUGINS] infer_forecast: infer(thaw(freeze(train))) identical a forecast; FE 1x; CONTROL history -7 difere", {
  b <- hi_base(); env <- b$env; cuts <- b$cuts; spec <- b$spec
  ## artefato por disco (freeze/thaw da fundacao) -> infer
  f <- tempfile("hw-art-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))), add = TRUE)
  hw_lib$prevcarga_freeze(b$art, f)
  art2 <- env$plugin_thaw(hw_lib$prevcarga_thaw(f))
  o_inf <- hi_run(env, art2, b$deck, cuts)
  log <- new.env(); log$n <- 0L
  e2 <- new.env(parent = parent.env(env))
  for (n in ls(env, all.names = TRUE)) assign(n, get(n, envir = env), envir = e2)
  for (n in ls(e2, all.names = TRUE)) { fn <- get(n, envir = e2); if (is.function(fn)) environment(fn) <- e2; assign(n, fn, envir = e2) }
  orig <- get("plugin_build_features", envir = e2)
  assign("plugin_build_features", function(deck, spec) { log$n <- log$n + 1L; orig(deck, spec) }, envir = e2)
  o_fc <- suppressWarnings(suppressMessages(e2$plugin_forecast(b$ipan, cuts, spec)))
  expect_identical(log$n, 1L)
  expect_identical(o_fc$values, o_inf$values)
  expect_identical(o_fc$meta$day0_raw, o_inf$meta$day0_raw); expect_identical(o_fc$meta$day0, o_inf$meta$day0)
  expect_identical(o_fc$delivery, o_inf$delivery)
  expect_identical(o_fc, o_inf)
  ## o treino nao ve o dia O: forecast com carga do dia O ate 12:00 => mesmos valores
  c12 <- hi_ctx("12:00")
  o12 <- suppressWarnings(suppressMessages(env$plugin_forecast(c12$ipan, c12$cuts, spec)))
  expect_identical(o12$values, o_fc$values); expect_identical(o12$meta$day0_raw, o_fc$meta$day0_raw)
  ## CONTROL: 7 dias a menos de historico muda o treino
  s7 <- spec; s7$history_days$train <- 52L
  p7 <- hw_fx_panel(b$store, cuts, "infer", history_days = 53L)
  o7 <- suppressWarnings(suppressMessages(env$plugin_forecast(p7, cuts, s7)))
  expect_gt(max(abs(o7$values - o_inf$values)), 1e-5)
})

test_that("[PLUGINS] infer_contract: artefato O-1 => hw.artifact_origin; intervalo alem do corte => contract; nada entregue", {
  b <- hi_base(); env <- b$env; cuts <- b$cuts
  a1 <- b$art; a1$origin_day <- HI_O - 1L
  e1 <- hw_catch(env$plugin_inference_features(b$deck, a1, cuts))
  expect_identical(e1$id, "hw.artifact_origin"); expect_identical(e1$class_name, "contract"); expect_identical(e1$exit_code, 2L)
  inf <- env$plugin_inference_features(b$deck, b$art, cuts)
  e1b <- hw_catch(env$plugin_infer(a1, inf, cuts))
  expect_identical(e1b$id, "hw.artifact_origin")
  ## o despachante confere a origem do artefato (carimbo) antes do painel
  ## painel de 12:00 com o corte de 09:00: confere, nao corta
  c12 <- hi_ctx("12:00")
  e2 <- hw_catch(env$plugin_ingest(c12$ipan, cuts))
  expect_identical(e2$id, "hw.train_day_o"); expect_identical(e2$class_name, "contract")
  expect_match(conditionMessage(e2), "fora do corte")
  d2 <- b$deck; d2$load_day_o <- suppressWarnings(env$plugin_ingest(c12$ipan, c12$cuts))$load_day_o
  e3 <- hw_catch(env$plugin_inference_features(d2, b$art, cuts))
  expect_identical(e3$id, "hw.train_day_o"); expect_match(conditionMessage(e3), "alem de available_until")
})

test_that("[PLUGINS] infer_puro: sem env/relogio/RNG e sem aritmetica de hora nos arquivos de inferencia", {
  rdir <- file.path(hw_root(), "models", "holt-winters", "R")
  for (pat in c("Sys\\.getenv", "Sys\\.time", "Sys\\.Date", "\\bnow\\(", "set\\.seed", "\\bminutes\\(", "\\bhours\\(", "as\\.POSIXct", "difftime"))
    expect_length(grep("^inference", hw_grep_code(rdir, pat), value = TRUE), 0L)
  for (f in c("inference.R", "inference_features.R"))
    expect_true(all(utf8ToInt(paste(readLines(file.path(rdir, f), warn = FALSE), collapse = "\n")) < 128L), info = f)
  b <- hi_base(); set.seed(7); r0 <- .Random.seed
  hi_run(b$env, b$art, b$deck, b$cuts)
  expect_identical(.Random.seed, r0)
})
