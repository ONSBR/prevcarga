## test-clima-hist.R -- parametro clima_hist (observado | previsto_lead0) do dessem-linear (E5-T4). Fixtures do helper-dl.R
## (deck sintetico, sem store real). O previsto do fixture so cobre O..O+H; os testes de lead 0 injetam emissoes de lead 0
## (issue = meia-noite do dia valido, +5 graus sobre o observado) a partir de um dia, para distinguir as fontes.

dlc_env <- new.env()   # treino previsto_lead0 compartilhado entre os testes

dlc_inject <- function(pan, from_day) {
  pan <- data.table::copy(pan)
  wo <- data.table::copy(pan$weather_observed)
  wo <- wo[lubridate::as_date(valid_time, tz = DL_TZ) >= from_day]
  fc <- data.table::data.table(area_code = wo$area_code,
    issue_time = lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(wo$valid_time, tz = DL_TZ)), DL_TZ),
    valid_time = wo$valid_time, temperature = wo$temperature + 5)
  pan$weather_forecast <- data.table::rbindlist(list(fc, pan$weather_forecast), use.names = TRUE)
  pan
}

dlc_spec <- function(st, exo = "MED", clima = NULL) {
  sp <- dl_fx_spec(exo, hist = st$hist)
  if (!is.null(clima)) sp$params$clima_hist <- clima
  sp
}

## deck da ingestao (frame de modelagem) com o spec no canal interino cuts$spec, como o despachante faz
dlc_deck <- function(pan, cuts, spec) {
  m <- dl_model_panel(pan, cuts)
  m$cuts$spec <- spec
  dl_with_cli(suppressWarnings(suppressMessages(dl_plugin()$env$plugin_ingest(m$panel, m$cuts, NULL))))
}

## o despachante de infer le o spec de plugin$spec (a fachada o troca pelo spec_efetivo do artefato); aqui o troca o teste
dlc_plugin <- function(clima = NULL) {
  p <- dl_plugin()
  p$spec$params$clima_hist <- clima
  p
}

dlc_first <- function(cuts, spec) dl_plugin()$env$dl_history_window(cuts$origin_day, spec)$common_from - 1L

test_that("[PLUGINS] gate05_clima_honesto: sem a chave, sem bloco no payload e spec sem clima_hist; observado explicito = mesmos numeros", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st)
  a0 <- dl_train(st, cuts, dlc_spec(st))
  expect_null(a0$payload$clima_hist)
  expect_false("clima_hist" %in% names(a0$spec_efetivo$params))
  a1 <- dl_train(st, cuts, dlc_spec(st, clima = "observado"))
  expect_identical(a1$payload$clima_hist$modo, "observado")
  expect_identical(a1$payload$clima_hist$dias_completados, 0L)
  for (f in c("svm_dm", "svm_perfil", "norm", "head_recipe", "dm_o_sample", "columns", "inference_needs"))
    expect_identical(a1$payload[[f]], a0$payload[[f]], info = f)
})

test_that("[PLUGINS] gate05_clima_honesto: clima_hist fora do conjunto aborta antes de qualquer treino", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st)
  spy <- dl_spy_tune(); on.exit(spy$restore(), add = TRUE)
  for (bad in list("lead1", 1L, c("observado", "previsto_lead0"))) {
    e <- dl_err(dl_train(st, cuts, dlc_spec(st, clima = bad)))
    expect_dl_class(e, "contract", 2L, "plugin.dessem_linear_bad_clima_hist")
    expect_identical(e$details$field, "params.clima_hist")
  }
  expect_identical(spy$log$n, 0L)
})

test_that("[PLUGINS] gate05_clima_honesto: previsto_lead0 com lead 0 desde o inicio da janela => EXOGENAHIST == exo_hist_lead0, dias_completados 0", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st); sp <- dlc_spec(st, clima = "previsto_lead0")
  first <- dlc_first(cuts, sp)
  pan <- dlc_inject(dl_fx_panel(st, cuts, "train"), first)
  deck <- dlc_deck(pan, cuts, sp)
  m <- dl_model_panel(pan, cuts)
  days <- seq(first, cuts$last_data_day, by = "day")
  r <- dl_engine_env()$exo_hist_lead0(m$panel$weather_forecast, m$panel$weather_observed, DL8_AREA, days, cuts$tz)
  expect_identical(deck$EXOGENAHIST, r$dt)
  expect_identical(deck$clima_hist$modo, "previsto_lead0")
  expect_identical(deck$clima_hist$dias_completados, 0L)
  expect_identical(deck$clima_hist$primeiro_dia_lead0, first)
  ## nao e o observado (o +5 do lead 0 aparece) e EXOGENAPREV nao muda
  deck_o <- dlc_deck(pan, cuts, dlc_spec(st))
  expect_false(isTRUE(all.equal(deck$EXOGENAHIST$Exo_Temperatura, deck_o$EXOGENAHIST$Exo_Temperatura)))
  expect_identical(deck$EXOGENAPREV, deck_o$EXOGENAPREV)
  expect_identical(deck_o$clima_hist$modo, "observado")
  ## payload: bloco com o modo e os dias do lead 0
  art <- dl_train(st, cuts, sp, panel = pan)
  expect_identical(art$payload$clima_hist, list(modo = "previsto_lead0", dias_completados = 0L, primeiro_dia_lead0 = first))
  expect_identical(art$payload$inference_needs$weather_forecast$segments[[2L]]$kind, "min_lead")
  assign("art_lead0", art, envir = dlc_env); assign("pan_lead0", pan, envir = dlc_env)
})

test_that("[PLUGINS] gate05_clima_honesto: previsto desde um dia posterior => dias anteriores vem do observado e sao contados", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st); sp <- dlc_spec(st, clima = "previsto_lead0")
  first <- dlc_first(cuts, sp)
  from <- first + 7L
  pan <- dlc_inject(dl_fx_panel(st, cuts, "train"), from)
  deck <- dlc_deck(pan, cuts, sp)
  expect_identical(deck$clima_hist$dias_completados, 7L)
  expect_identical(deck$clima_hist$primeiro_dia_lead0, from)
  deck_o <- dlc_deck(pan, cuts, dlc_spec(st))
  early <- lubridate::as_date(deck$EXOGENAHIST$DataHora) < from
  expect_identical(deck$EXOGENAHIST[early], deck_o$EXOGENAHIST[early])
})

test_that("[PLUGINS] gate05_clima_honesto: payload previsto_lead0 x deck observado => artifact_mismatch field clima_hist (e o inverso)", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st)
  skip_if(is.null(dlc_env$art_lead0), "depende do treino previsto_lead0 do teste anterior")
  art <- dlc_env$art_lead0
  pan_i <- dlc_inject(dl_fx_panel(st, cuts, "infer"), dlc_first(cuts, dlc_spec(st)))
  e <- dl_err(dl_infer(art, st, cuts, plugin = dlc_plugin(NULL), panel = pan_i))     # deck do infer resolve observado
  expect_identical(e$id, "plugin.dessem_linear_artifact_mismatch")
  expect_identical(e$details$field, "clima_hist")
  ## inverso: payload sem bloco (lido como observado) x deck previsto_lead0
  e2 <- dl_err(dl_infer(dl_base_art(), st, cuts, plugin = dlc_plugin("previsto_lead0"), panel = pan_i))
  expect_identical(e2$id, "plugin.dessem_linear_artifact_mismatch")
  expect_identical(e2$details$field, "clima_hist")
})

test_that("[PLUGINS] gate05_clima_honesto: infer previsto_lead0 com deck curto == deck longo (identical)", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st)
  skip_if(is.null(dlc_env$art_lead0), "depende do treino previsto_lead0 do teste anterior")
  art <- dlc_env$art_lead0
  first <- dlc_first(cuts, dlc_spec(st))
  long <- dlc_inject(dl_fx_panel(st, cuts, "infer"), first)
  short <- dlc_inject(dl_fx_panel(st, cuts, "infer", history_days = 16L), first)
  pl <- dlc_plugin("previsto_lead0")
  o_long <- dl_infer(art, st, cuts, plugin = pl, panel = long)
  o_short <- dl_infer(art, st, cuts, plugin = pl, panel = short)
  expect_identical(o_short$delivery, o_long$delivery)
})

test_that("[PLUGINS] gate05_clima_honesto: LIN_UNI com previsto_lead0 => nao_se_aplica e nenhum clima lido", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st)
  sp <- dlc_spec(st, "UNI", clima = "previsto_lead0")
  art <- dl_train(st, cuts, sp)
  expect_identical(art$payload$clima_hist$modo, "nao_se_aplica")
  expect_identical(art$payload$clima_hist$dias_completados, 0L)
  expect_identical(art$payload$inference_needs$weather_forecast$segments[[1L]]$kind, "freshest")
  expect_length(art$payload$inference_needs$weather_forecast$segments, 1L)
  ## UNI sem a chave tambem registra nao_se_aplica (a variante ignora o param)
  art_u <- dl_train(st, cuts, dlc_spec(st, "UNI"))
  expect_identical(art_u$payload$clima_hist$modo, "nao_se_aplica")
})
