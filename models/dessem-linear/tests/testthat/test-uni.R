## test-uni.R -- variante UNI do dessem-linear (Linear.Univariado, exogenaOper SEM; membro LIN_UNI; E5-T2). Sem golden do oraculo:
## o aceite e por INVARIANCIA AO CLIMA (decisao L2) com CONTROL (a MED sente a mesma perturbacao). Fixtures do helper-dl.R, sem store real.

## "shift" = +10 graus em weather_observed e weather_forecast (o do ticket); "spike" = +10 graus so em 1 de cada 7 linhas. O
## deslocamento puro e afim e o SVM linear com scale=TRUE do motor o absorve (Delta ~ 1e-11 ate na MED), por isso o CONTROL usa
## o spike, que muda a forma da serie diaria (media) e nao e absorvido.
dlu_hot <- function(pan, how = "shift") {
  pan <- data.table::copy(pan)
  for (nm in c("weather_observed", "weather_forecast")) {
    pan[[nm]] <- data.table::copy(pan[[nm]])
    pan[[nm]][, temperature := temperature + 10 * (if (how == "shift") 1 else as.numeric(seq_len(.N) %% 7L == 0L))]
  }
  pan
}
dlu_plugin <- function(ex, st) { p <- dl_plugin(); p$spec <- dl_fx_spec(ex, p, hist = st$hist); p }
dlu_exo <- function(x) grep("^Exo_", names(x), value = TRUE)

test_that("[PLUGINS] gate04_exogena_params: UNI invariante a temperatura (+10 graus) no treino e na inferencia; SERIECOMP sem Exo_", {
  st <- dl_fx_store("09:00"); ct <- dl_fx_cuts(st); spec <- dl_fx_spec("UNI", hist = st$hist)
  pt <- dl_fx_panel(st, ct, "train"); pi <- dl_fx_panel(st, ct, "infer")
  a <- dl_train(st, ct, spec, panel = pt)
  b <- dl_train(st, ct, spec, panel = dlu_hot(pt))
  c <- dl_train(st, ct, spec, panel = dlu_hot(pt, "spike"))
  expect_identical(a$payload$model_key, "Linear.Univariado")
  expect_identical(a$payload$variant, "UNI")
  expect_identical(a$stamp$variant, "UNI")
  expect_identical(a$payload, b$payload)
  expect_identical(a$payload, c$payload)
  p <- dlu_plugin("UNI", st)
  ya <- dl_infer(a, st, ct, p, panel = pi)
  yb <- dl_infer(b, st, ct, p, panel = dlu_hot(pi))
  expect_identical(dl_infer(a, st, ct, p, panel = dlu_hot(pi, "spike"))$delivery$y_pred, ya$delivery$y_pred)
  expect_identical(ya$values, yb$values)
  expect_identical(ya$delivery$y_pred, yb$delivery$y_pred)
  expect_identical(ya$variant, "UNI")
  ## SERIECOMP do treino e da inferencia sem coluna Exo_* (o perfil horario a usaria)
  for (mode in c("train", "infer")) {
    pan <- if (identical(mode, "train")) pt else pi
    m <- dl_model_panel(pan, ct); m$cuts$spec <- spec
    frame <- dl_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts)))
    expect_identical(frame$variant, "UNI"); expect_identical(frame$model_key, "Linear.Univariado")
    feats <- dl_with_cli(suppressMessages(p$env$plugin_build_features(frame, spec)))
    expect_identical(dlu_exo(feats$processado$SERIECOMP), character(), info = mode)
    if (identical(mode, "infer")) {
      inf <- dl_with_cli(suppressMessages(p$env$dl_inference_features(frame, a$payload)))
      expect_identical(dlu_exo(inf$processado$SERIECOMP), character())
    }
  }
})

test_that("[PLUGINS] gate04_exogena_params: CONTROL MED sente a perturbacao de clima (max|Delta y_pred| > 1e-5)", {
  st <- dl_fx_store("09:00"); ct <- dl_fx_cuts(st); spec <- dl_fx_spec("MED", hist = st$hist)
  pt <- dl_fx_panel(st, ct, "train"); pi <- dl_fx_panel(st, ct, "infer")
  a <- dl_train(st, ct, spec, panel = pt)
  b <- dl_train(st, ct, spec, panel = dlu_hot(pt, "spike"))
  p <- dlu_plugin("MED", st)
  ya <- dl_infer(a, st, ct, p, panel = pi)
  yb <- dl_infer(b, st, ct, p, panel = dlu_hot(pi, "spike"))
  expect_gt(max(abs(ya$delivery$y_pred - yb$delivery$y_pred)), 1e-5)
})

test_that("[PLUGINS] gate04_exogena_params: painel sem weather_* com UNI nao aborta (deck sintetico); MED aborta series_gap", {
  p <- dl_load(); cuts <- dl_cuts(); O <- cuts$origin_day
  panel <- dl_panel(days = 60L)
  for (empty in c("ausente", "zero_linhas")) {
    pn <- panel
    pn$weather_observed <- if (identical(empty, "ausente")) NULL else panel$weather_observed[0L]
    pn$weather_forecast <- if (identical(empty, "ausente")) NULL else panel$weather_forecast[0L]
    m <- dl_model_panel(panel, cuts)
    m$panel$weather_observed <- pn$weather_observed; m$panel$weather_forecast <- pn$weather_forecast
    m$cuts$spec <- dl_spec("UNI", 40L)
    deck <- dl_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts)))
    expect_identical(deck$model_key, "Linear.Univariado", info = empty)
    expect_identical(deck$EXOGENAHIST$DataHora, deck$CARGAHIST$DataHora, info = empty)
    days <- seq(O, O + deck$horizon - 1L, by = "day")
    expect_identical(nrow(deck$EXOGENAPREV), 24L * length(days), info = empty)
    expect_identical(as.integer(table(lubridate::as_date(deck$EXOGENAPREV$DataHora, tz = cuts$tz))), rep(24L, length(days)), info = empty)
    expect_identical(lubridate::tz(deck$EXOGENAPREV$DataHora), cuts$tz, info = empty)
    expect_true(all(deck$EXOGENAHIST$Exo_Temperatura == 0) && all(deck$EXOGENAPREV$Exo_Temperatura == 0), info = empty)
    ## CONTROL: o mesmo painel com MED aborta
    m$cuts$spec <- dl_spec("MED", 40L)
    expect_dl_error(p$env$plugin_ingest(m$panel, m$cuts), "plugin.dessem_linear_series_gap")
  }
})

test_that("[PLUGINS] gate04_exogena_params: exogena invalida aborta bad_exogena com 4 valores permitidos e hint citando UNI", {
  p <- dl_load()
  panel <- dl_panel(days = 60L); cuts <- dl_cuts(); m <- dl_model_panel(panel, cuts)
  sp <- dl_spec(); sp$params$exogena <- "XYZ"
  cond <- expect_dl_error(p$env$plugin_ingest(m$panel, m$cuts, sp), "plugin.dessem_linear_bad_exogena", "params.exogena")
  expect_identical(cond$exit_code, 2L)
  expect_identical(unlist(cond$details$allowed), c("MED", "MAX", "MXMN", "UNI"))
  expect_match(conditionMessage(cond), "MED, MAX, MXMN, UNI", fixed = TRUE)
  tsv <- readLines(file.path(dl_root(), "scripts", "lib", "errors", "dessem_linear.tsv"), warn = FALSE)
  expect_match(tsv[grepl("^plugin.dessem_linear_bad_exogena\t", tsv)], "UNI", fixed = TRUE)
  expect_true(p$env$dl_variant_weatherless("Linear.Univariado"))
  for (k in c("Linear.TmpMedia", "Linear.TmpMaxima", "Linear.TmpMaxMin")) expect_false(p$env$dl_variant_weatherless(k))
  expect_false(p$env$dl_variant_weatherless(NA_character_))
})
