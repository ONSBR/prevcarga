## test-clima-hist.R -- parametro clima_hist (observado | previsto_lead0) do dessem-regdin (E5-T4). Loader real, motor real,
## despachante real; painel sintetico rd3_panel (14 meses, sem RNG). O previsto do fixture so cobre O..O+H: os testes de lead 0
## injetam emissoes de lead 0 (issue = meia-noite do dia valido, +5 graus sobre o observado) a partir de um dia.
## Os treinos (~1,5 min cada) ficam em cache de sessao.

rdc_cache <- new.env()

rdc_inject <- function(pan, from_day) {
  pan <- data.table::copy(pan)
  wo <- data.table::copy(pan$weather_observed)
  wo <- wo[lubridate::as_date(valid_time, tz = RD_TZ) >= from_day]
  fc <- data.table::data.table(area_code = wo$area_code,
    issue_time = lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(wo$valid_time, tz = RD_TZ)), RD_TZ),
    valid_time = wo$valid_time, temperature = wo$temperature + 5)
  pan$weather_forecast <- data.table::rbindlist(list(fc, pan$weather_forecast), use.names = TRUE)
  pan
}

rdc_first_day <- function(pan) lubridate::as_date(min(pan$weather_observed$valid_time), tz = RD_TZ)

rdc_spec <- function(clima = NULL, exogena = "MED") {
  sp <- rd2_spec(exogena)
  if (!is.null(clima)) sp$params$clima_hist <- clima
  sp
}

## o despachante de infer le o spec de plugin$spec (a fachada o troca pelo spec_efetivo do artefato); aqui o troca o teste
rdc_plugin <- function(clima = NULL) {
  p <- rd_fx_plugin()
  p$spec$params$clima_hist <- clima
  p
}

## deck da ingestao com o spec no canal interino cuts$spec, como o despachante faz
rdc_deck <- function(pan, spec, cuts = rd_fx_cuts()) {
  m <- rd_model_panel(pan, cuts)
  m$cuts$spec <- spec
  rd_with_cli(suppressMessages(rd_fx_plugin()$env$plugin_ingest(m$panel, m$cuts, NULL)))
}

rdc_train <- function(key, pan, spec)
  if (!is.null(rdc_cache[[key]])) rdc_cache[[key]] else {
    rdc_cache[[key]] <- rd_train(pan, rd_fx_cuts(), spec)
    rdc_cache[[key]]
  }

test_that("[PLUGINS] corte_clima: sem a chave, sem bloco no payload e spec sem clima_hist; observado explicito = mesmos numeros", {
  a0 <- rd3_state()$med
  expect_null(a0$payload$clima_hist)
  expect_false("clima_hist" %in% names(a0$spec_efetivo$params))
  a1 <- rdc_train("obs", rd3_panel(), rdc_spec("observado"))
  expect_identical(a1$payload$clima_hist$modo, "observado")
  expect_identical(a1$payload$clima_hist$dias_completados, 0L)
  for (f in c("svm_perfil", "norm", "config_mdl", "day_types", "dias", "train_window", "arima_fingerprint", "serie_dm",
              "hol_profile", "columns", "inference_needs"))
    expect_identical(a1$payload[[f]], a0$payload[[f]], info = f)
})

test_that("[PLUGINS] corte_clima: clima_hist fora do conjunto aborta antes de qualquer treino", {
  spy <- rd_spy(".rd_fit_pass")
  for (bad in list("lead1", 1L, c("observado", "previsto_lead0"))) {
    e <- rd5_err(rd_train(rd3_panel(), rd_fx_cuts(), rdc_spec(bad)))
    expect_identical(e$id, "plugin.regdin_bad_clima_hist")
    expect_identical(e$class_name, "contract")
    expect_identical(e$exit_code, 2L)
    expect_identical(e$details$field, "params.clima_hist")
  }
  expect_identical(spy$count(), 0L)
})

test_that("[PLUGINS] corte_clima: previsto_lead0 com lead 0 desde o inicio => EXOGENAHIST == exo_hist_lead0, dias_completados 0", {
  pan <- rd3_panel(); first <- rdc_first_day(pan)
  pan <- rdc_inject(pan, first)
  sp <- rdc_spec("previsto_lead0")
  deck <- rdc_deck(pan, sp)
  m <- rd_model_panel(pan, rd_fx_cuts())
  d0 <- lubridate::as_date(min(deck$CARGAHIST$DataHora))
  days <- seq(d0, rd_fx_cuts()$last_data_day, by = "day")
  r <- parent.env(rd_fx_plugin()$env)$exo_hist_lead0(m$panel$weather_forecast, m$panel$weather_observed, "SE", days, RD_TZ)
  expect_identical(deck$EXOGENAHIST, r$dt)
  expect_identical(deck$clima_hist$modo, "previsto_lead0")
  expect_identical(deck$clima_hist$dias_completados, 0L)
  expect_identical(deck$clima_hist$primeiro_dia_lead0, d0)
  deck_o <- rdc_deck(pan, rdc_spec())
  expect_false(isTRUE(all.equal(deck$EXOGENAHIST$Exo_Temperatura, deck_o$EXOGENAHIST$Exo_Temperatura)))
  expect_identical(deck$EXOGENAPREV, deck_o$EXOGENAPREV)
  expect_identical(deck_o$clima_hist$modo, "observado")
  ## payload do treino
  art <- rdc_train("lead0", pan, sp)
  expect_identical(art$payload$clima_hist, list(modo = "previsto_lead0", dias_completados = 0L, primeiro_dia_lead0 = d0))
  expect_identical(art$payload$inference_needs$weather_forecast$segments[[2L]]$kind, "min_lead")
  expect_identical(art$payload$inference_needs$weather_forecast$segments[[2L]]$lead, 0L)
  a0 <- rd3_state()$med
  expect_false(identical(art$payload$norm, a0$payload$norm))      # CONTROL: o lead 0 muda a normalizacao
})

test_that("[PLUGINS] corte_clima: previsto desde um dia posterior => dias anteriores vem do observado e sao contados", {
  pan0 <- rd3_panel(); d0 <- rdc_first_day(pan0)
  from <- d0 + 20L
  pan <- rdc_inject(pan0, from)
  deck <- rdc_deck(pan, rdc_spec("previsto_lead0"))
  expect_identical(deck$clima_hist$dias_completados, 20L)
  expect_identical(deck$clima_hist$primeiro_dia_lead0, from)
  deck_o <- rdc_deck(pan, rdc_spec())
  early <- lubridate::as_date(deck$EXOGENAHIST$DataHora) < from
  expect_identical(deck$EXOGENAHIST[early], deck_o$EXOGENAHIST[early])
})

test_that("[PLUGINS] corte_clima: payload previsto_lead0 x deck observado => artifact_mismatch field clima_hist (e o inverso)", {
  au <- rd3_au(9L)
  pan <- rdc_inject(rd3_panel(until = au), rdc_first_day(rd3_panel()))
  pan$weather_forecast <- pan$weather_forecast[issue_time <= au]
  art <- rdc_train("lead0", rdc_inject(rd3_panel(), rdc_first_day(rd3_panel())), rdc_spec("previsto_lead0"))
  e <- rd5_err(rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_infer(rdc_plugin(NULL), art, pan, rd_fx_cuts()))))
  expect_identical(e$id, "plugin.regdin_artifact_mismatch")
  expect_identical(e$details$field, "clima_hist")
  e2 <- rd5_err(rd_with_cli(suppressMessages(
    rd_lib$prevcarga_plugin_infer(rdc_plugin("previsto_lead0"), rd3_state()$med, pan, rd_fx_cuts()))))
  expect_identical(e2$id, "plugin.regdin_artifact_mismatch")
  expect_identical(e2$details$field, "clima_hist")
})

test_that("[PLUGINS] corte_clima: infer previsto_lead0 com deck curto == deck longo (identical)", {
  au <- rd3_au(9L)
  art <- rdc_train("lead0", rdc_inject(rd3_panel(), rdc_first_day(rd3_panel())), rdc_spec("previsto_lead0"))
  mk <- function(days) {
    pan <- rd3_panel(until = au, days = days)
    pan <- rdc_inject(pan, rdc_first_day(pan))
    pan$weather_forecast <- pan$weather_forecast[issue_time <= au]
    pan
  }
  pl <- rdc_plugin("previsto_lead0")
  go <- function(pan) rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_infer(pl, art, pan, rd_fx_cuts())))
  expect_identical(go(mk(16L))$delivery, go(mk(RD2_D))$delivery)
})
