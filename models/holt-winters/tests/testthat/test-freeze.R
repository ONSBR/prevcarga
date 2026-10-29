## test-freeze.R -- gates [PLUGINS] freeze_puro / freeze_det / thaw_contract do Holt-Winters (HW.3; AC 5-7).
## Reusa a cadeia ingest -> FE -> train de test-training.R (helpers tr_*; carregados antes por ordem alfabetica nao: por isso
## este arquivo traz o seu proprio fixture minimo).
if (!exists("prevcarga_freeze", envir = hw_lib, inherits = FALSE))
  sys.source(file.path(hw_root(), "scripts", "lib", "provenance.R"), envir = hw_lib)

fz_spec <- function() { s <- hw_spec(); s$history_days$train <- 59L; s }

fz_base <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      env <- hw_env(); st <- hw_fx_store(); cuts <- hw_fx_cuts(st); spec <- fz_spec()
      deck <- env$plugin_ingest(hw_fx_panel(st, cuts, history_days = 60L), cuts)
      feats <- suppressMessages(env$plugin_build_features(deck, spec))
      cache <<- list(env = env, cuts = cuts, spec = spec, feats = feats, model = env$plugin_train(feats, spec))
    }
    cache
  }
})

fz_sha <- function(obj) {
  f <- tempfile("hw-frz-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))))
  hw_lib$prevcarga_freeze(obj, f)
}

fz_plugin <- function() list(descriptor = list(model_id = "holt-winters", contract_version = 1L, variant_param = NA_character_,
                                               uses_rng = TRUE))

FZ_FIELDS <- c("model_id", "origin_day", "train_origin", "mean_interval", "mean_daily", "datainiver", "datafimver",
               "align_first_idx", "h_fit", "n_steps_fit", "m_mes", "params", "seed",
               "carga_temp_ver_fer", "carga_temp_med1", "carga_temp_ver")

test_that("[PLUGINS] freeze_puro: campos exatos, scan_pure ok; CONTROL trained_at injetado acusado e sha256 diferente", {
  b <- fz_base(); art <- b$env$plugin_freeze(b$model)
  expect_identical(names(art), FZ_FIELDS)
  expect_silent(hw_lib$prevcarga_plugin_scan_pure(art))
  expect_identical(dim(art$mean_interval), c(9L, 48L)); expect_null(dimnames(art$mean_interval))
  expect_true(is.double(art$mean_interval) && is.double(art$mean_daily) && length(art$mean_daily) == 9L)
  expect_s3_class(art$origin_day, "Date"); expect_s3_class(art$datainiver, "Date")
  expect_true(all(art$carga_temp_ver_fer$Data >= lubridate::ymd("2019-01-01")))
  expect_identical(ncol(art$carga_temp_ver_fer), 105L); expect_identical(ncol(art$carga_temp_ver), 105L)
  expect_identical(ncol(art$carga_temp_med1), 6L)
  expect_identical(art$m_mes, 5L); expect_identical(art$seed, 1234L)
  for (cl in c("forecast", "ets", "ts")) expect_false(any(vapply(art, inherits, NA, what = cl)))
  expect_false(any(c("trained_at", "config", "call", "available_until", "model_var") %in% names(art)))
  expect_identical(art$origin_day, b$cuts$origin_day); expect_identical(art$train_origin, b$cuts$origin_day)
  ## CONTROL: relogio injetado
  dirty <- c(art, list(trained_at = Sys.time()))
  cnd <- hw_catch(hw_lib$prevcarga_plugin_scan_pure(dirty))
  expect_identical(cnd$id, "plugin.impure_artifact")
  expect_false(identical(fz_sha(dirty), fz_sha(art)))
  ## CONTROL: objeto forecast e closure no lugar da matriz sao acusados
  expect_identical(hw_catch(hw_lib$prevcarga_plugin_scan_pure(c(art, list(f = function() 1))))$id, "plugin.impure_artifact")
  bad <- b$model; bad$mean_interval <- NULL
  expect_identical(hw_catch(b$env$plugin_freeze(bad))$id, "hw.bad_artifact")
})

test_that("[PLUGINS] freeze_det: 2 treinos => sha256 igual; thaw(freeze) identical; .Random.seed do chamador inalterado", {
  b <- fz_base(); env <- b$env
  guard <- hw_lib$.prevcarga_plugin_guarded
  set.seed(4242); seed0 <- get(".Random.seed", envir = globalenv())
  m1 <- suppressMessages(guard(fz_plugin(), env$plugin_train(b$feats, b$spec)))
  m2 <- suppressMessages(guard(fz_plugin(), env$plugin_train(b$feats, b$spec)))
  expect_identical(get(".Random.seed", envir = globalenv()), seed0)
  a1 <- env$plugin_freeze(m1); a2 <- env$plugin_freeze(m2)
  expect_identical(fz_sha(a1), fz_sha(a2))
  expect_identical(fz_sha(a1), fz_sha(env$plugin_freeze(b$model)))
  f <- tempfile("hw-frz-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))), add = TRUE)
  hw_lib$prevcarga_freeze(a1, f)
  back <- hw_lib$prevcarga_thaw(f)
  expect_identical(env$plugin_thaw(back), a1)
  ## forma do despachante P.0c (params + seed no 3o argumento) da o mesmo modelo
  m3 <- env$plugin_train(b$feats, b$spec$params, b$spec$seed)
  expect_identical(env$plugin_freeze(m3)$mean_interval, a1$mean_interval)
  ## o artefato nao depende do available_until: o carimbo nunca o traz
  expect_false("available_until" %in% names(a1))
})

test_that("[PLUGINS] thaw_contract: forma 8x48 => contract nomeando mean_interval; 1 byte adulterado => integrity exit 9", {
  b <- fz_base(); env <- b$env; art <- env$plugin_freeze(b$model)
  expect_identical(dim(env$plugin_thaw(art)$mean_interval), c(9L, 48L))
  w <- art; w$mean_interval <- w$mean_interval[1:8, ]
  cnd <- hw_catch(env$plugin_thaw(w))
  expect_identical(cnd$id, "hw.bad_artifact"); expect_identical(cnd$class_name, "contract"); expect_match(conditionMessage(cnd), "mean_interval")
  w <- art; w$mean_daily <- w$mean_daily[1:8]
  expect_match(conditionMessage(hw_catch(env$plugin_thaw(w))), "mean_daily")
  w <- art; w$align_first_idx <- 1L
  expect_match(conditionMessage(hw_catch(env$plugin_thaw(w))), "align_first_idx")
  w <- art; w$carga_temp_med1 <- w$carga_temp_med1[, 1:5]
  expect_match(conditionMessage(hw_catch(env$plugin_thaw(w))), "carga_temp_med1")
  ## byte adulterado, pela fundacao (P.0d): stamp + freeze_bytes + thaw
  st <- hw_lib$prevcarga_plugin_stamp(art, fz_plugin(), c(b$spec, list(split = "origin_scoped")), b$cuts,
                                      list(load_label = "start", timezone = HW_TZ, weather_issue_cutoff = "available_until"))
  f <- tempfile("hw-frz-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))), add = TRUE)
  hw_lib$prevcarga_plugin_freeze_bytes(st, f)
  ok <- hw_lib$prevcarga_plugin_thaw(f)
  expect_identical(env$plugin_thaw(ok$payload), art)
  raw <- readBin(f, "raw", file.info(f)$size); raw[length(raw) %/% 2L] <- as.raw(bitwXor(as.integer(raw[length(raw) %/% 2L]), 1L))
  writeBin(raw, f)
  cnd <- hw_catch(hw_lib$prevcarga_plugin_thaw(f))
  expect_identical(cnd$id, "plugin.artifact_corrupt"); expect_identical(cnd$exit_code, 9L)
})
