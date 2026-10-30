## test-oracle-gates.R -- RD.6: os gates do oraculo (prevcargadessem3/models/dessem-regdin/tests/run_all_gates.R) portados como [PLUGINS].
## O oraculo roda cada gate por subprocesso sobre o dado real; aqui o equivalente e em PROCESSO pelo despachante real da fundacao sobre
## o deck sintetico do RD.5 (`scripts/tools/fixture_store.R`, nunca dado real) e o painel sintetico do RD.2/RD.3; o dado real fica no
## tier parity (test-parity.R). Mapa (gate do oraculo -> gate do runner):
##   gate02 escala_mw -> escala_mw        gate04 exogena_env -> exogena_variante (RD.4)    gate05 honesty -> corte_clima (RD.5)
##   gate07 contract_synthetic -> contract_synthetic   gate08 contract_e2e -> contract_e2e   gate09 flag12_yearend -> flag12_yearend
##   gate10/11 nowcast_partial/anyhour_1330 -> nowcast_parcial   gate12/13 split_nowcast_MED/MAX -> split_nowcast_med/split_nowcast_max
##   gate01 schema_long -> [DEFERRED M6]   gate03 sem_crash -> [DEFERRED M7]   gate06 ytrue_real -> [DEFERRED M6]
## Diferencas deliberadas do oraculo (documentadas em docs/estado/dessem_regdin.md, RD.6): o plugin entrega 9 dias (O..O+8; o oraculo
## emite 10 e o tier parity trata o O+9 como `fora_da_faixa`); hora incompleta e descartada em toda serie (gate07: a "carga com 1
## amostra a menos" vira dia de 23 horas => completude); a cauda curta do clima ate 2 dias e persistida (so lacuna maior e contract).
## Sem variaveis de ambiente; datas por lubridate com tz explicito.

## d_plus de cada intervalo da entrega (pelo inicio do intervalo, no fuso do painel)
og_dplus <- function(delivery, O) as.integer(lubridate::as_date(delivery$interval_end - lubridate::minutes(30L), tz = RD_TZ) - O)

## ---- gate07 contract_synthetic: ramos do contrato de entrada do painel ---------------------------------------------------
## Cada ramo roda o estagio de ingestao do plugin carregado sobre o painel de 60 dias do RD.1 (frame de modelagem) e devolve a classe
## do erro ("ok" quando nao ha erro).
og_ingest <- function(edit = identity, until = NULL) {
  pan <- rd_panel(origin = "2024-12-23", days = 60L, until = until)
  pan <- edit(pan)
  cu <- rd_cuts(origin = "2024-12-23", available_until = if (is.null(until)) NULL else until)
  cu$weather_issue_cutoff <- "available_until"
  m <- rd_model_panel(pan, cu)
  p <- rd_fx_plugin()
  rd_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
}
og_class <- function(expr) {
  e <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  if (is.null(e)) "ok" else paste(e$class_name, e$id, sep = ":")
}

test_that("[PLUGINS] contract_synthetic: ramos do contrato de entrada aboram classificados (contract/completude), dia de 23 horas => completude; painel bom passa", {
  O <- lubridate::ymd("2024-12-23")
  hours23 <- function(day) function(p) {            # tira UMA hora (as duas meias horas) do dia: o to_hourly descarta a hora incompleta
    s <- lubridate::force_tz(lubridate::as_datetime(day), RD_TZ) + lubridate::hours(5L)
    p$load <- p$load[!(interval_start >= s & interval_start < s + lubridate::hours(1L))]
    p
  }
  r <- c(
    good       = og_class(og_ingest()),
    prev_empty = og_class(og_ingest(function(p) { p$weather_forecast <- p$weather_forecast[0]; p })),
    ## o oraculo tira 1 dia do fim e exige erro; aqui a cauda curta de ATE 2 dias e persistida (fill_forecast_tail, DL.2): so uma
    ## lacuna de 3 dias vira contract (a 1 dia segue, de proposito)
    prev_short3 = og_class(og_ingest(function(p) { p$weather_forecast <- p$weather_forecast[lubridate::as_date(valid_time, tz = RD_TZ) < O + 6L]; p })),
    prev_short1 = og_class(og_ingest(function(p) { p$weather_forecast <- p$weather_forecast[lubridate::as_date(valid_time, tz = RD_TZ) < O + 8L]; p })),
    carga_gap  = og_class(og_ingest(function(p) { p$load <- p$load[lubridate::as_date(interval_start, tz = RD_TZ) != O - 20L]; p })),
    carga_23h  = og_class(og_ingest(hours23(O - 10L))),
    carga_na   = og_class(og_ingest(function(p) { p$load[10L, load_mwh := NA_real_]; p })),
    hist_exo_gap = og_class(og_ingest(function(p) { p$weather_observed <- p$weather_observed[lubridate::as_date(valid_time, tz = RD_TZ) != O - 7L]; p })),
    fer_col    = og_class(og_ingest(function(p) { p$holidays[, holiday_type_id := NULL]; p })))
  expect_identical(r[["good"]], "ok")
  expect_identical(r[["prev_short1"]], "ok")                                          # persistencia de cauda curta
  expect_identical(r[["prev_empty"]], "contract:plugin.regdin_panel_contract")
  expect_identical(r[["prev_short3"]], "contract:plugin.regdin_panel_contract")
  expect_identical(r[["carga_gap"]], "completude:plugin.regdin_series_gap")
  expect_identical(r[["carga_23h"]], "completude:plugin.regdin_series_gap")         # dia com 23 horas => [prevcarga/completude]
  ## RD6-GAP-1 (discovery): uma hora de carga com NA (o to_hourly a mantem como linha) NAO e recusada na ingestao: o oraculo a recusa
  ## (gate07 carga_na => contract). PINADO como esta; quando o plugin passar a recusar, este pino falha de proposito e vira a classe.
  expect_identical(r[["carga_na"]], "ok")
  expect_identical(r[["hist_exo_gap"]], "contract:plugin.regdin_panel_contract")
  expect_identical(r[["fer_col"]], "contract:plugin.regdin_panel_contract")
  ## a mensagem do dia de 23 horas nomeia o dia e a contagem; terminal so ASCII
  e <- tryCatch(og_ingest(hours23(O - 10L)), prevcarga_error = function(e) e)
  expect_match(conditionMessage(e), format(O - 10L), fixed = TRUE)
  expect_match(conditionMessage(e), "23/24", fixed = TRUE)
  expect_true(rd_ascii(conditionMessage(e)))
  expect_identical(e$exit_code, 3L)
  ## CONTROL: o mesmo painel sem a mutacao passa (o erro vem da mutacao, nao do fixture)
  expect_identical(og_class(og_ingest(hours23(O + 30L))), "ok")                       # hora tirada fora da janela: nada muda
})

## ---- gate08 contract_e2e: origem alem do dado => erro classificado sem crash ---------------------------------------------
test_that("[PLUGINS] contract_e2e: origem 2099-01-01 (alem do dado) aborta classificado pelo leitor e pelo despachante, sem crash de baixo nivel", {
  st <- rd5_store()
  far <- lubridate::ymd("2099-01-01")
  low_level <- "prettyNum|VALLAG|subscript out of bounds|object '.*' not found"
  ## leitor: cortes da origem futura => erro classificado (exit != 0, nunca erro nao tratado)
  e1 <- rd5_err(suppressWarnings(rd_lib$prevcarga_resolve_cuts(rd5_ctx(st), far, "dessem-regdin")))
  expect_s3_class(e1, "prevcarga_error")
  expect_gt(e1$exit_code, 0L)
  expect_false(grepl(low_level, conditionMessage(e1)))
  ## despachante: painel real do deck com os cortes de uma origem alem do dado => contract/completude do plugin (nunca crash do motor)
  cu <- rd5_cuts(st, "09:00")
  pan <- rd5_panel(st, cu, "infer")
  late <- utils::modifyList(cu, list(origin_day = far, forecast_origin = far, train_origin = far, last_data_day = far - 1L,
                                     available_until = lubridate::force_tz(lubridate::as_datetime(far), RD_TZ) + lubridate::hours(9L)))
  late$weather_issue_limit <- late$available_until
  e2 <- rd5_err(rd5_train(st, late, rd5_spec("MED"), panel = pan))
  expect_s3_class(e2, "prevcarga_error")
  expect_true(e2$class_name %in% c("contract", "completude", "window"))
  expect_gt(e2$exit_code, 0L)
  expect_false(grepl(low_level, conditionMessage(e2)))
  expect_true(rd_ascii(conditionMessage(e2)))
})

## ---- gate02 escala_mw ----------------------------------------------------------------------------------------------------
test_that("[PLUGINS] escala_mw: entrega finita, positiva e na escala da carga de entrada; CONTROL de escala errada (1000x) e rejeitada", {
  st <- rd5_store(); art <- rd5_art("MED"); cu <- rd5_cuts(st, "09:00")
  out <- rd5_infer(art, st, cu)
  expect_identical(dim(out$values), c(9L, 48L))
  expect_true(all(is.finite(out$values)) && all(out$values > 0))
  expect_true(all(is.finite(out$delivery$y_pred)) && all(out$delivery$y_pred > 0))
  pan <- rd5_panel(st, cu, "infer")
  recent <- pan$load[interval_start >= max(interval_start) - lubridate::days(7L), load_mwh]
  ratio <- stats::median(out$delivery$y_pred) / stats::median(recent)
  expect_gt(ratio, 0.5); expect_lt(ratio, 2)
  expect_false(stats::median(out$delivery$y_pred / 1000) / stats::median(recent) > 0.5)
})

## ---- gate09 flag12_yearend: origem de fim de ano com codigo 12 no horizonte completa 9x48 --------------------------------
## Mundo proprio (origem 2024-12-22, 14 meses desde 2023-11-01; o ARIMA exige os 12 niveis de `Mes`): a semana-ponte tem o codigo 12
## retrospectivo (holiday_type_id 12) em O+2 e O+3, que o motor nunca conhece na origem. O gate prova que completa 9x48 sem
## "subscript out of bounds" no perfil (RL_perfilFeriado sem entrada para 12).
OG9_O <- lubridate::ymd("2024-12-22")
og9_panel <- function(until = NULL) {
  D <- as.integer(OG9_O - lubridate::ymd("2023-11-01"))
  pan <- rd_panel(origin = format(OG9_O), days = D, until = until)
  ## historia com os codigos 3..6 e 1 (o RL_perfilFeriado do tipo 1 precisa de amostras); horizonte: O+2 = codigo 12 retrospectivo com
  ## simplified_code 3 (vira Tipo 1, dia especial) e O+3 = codigo 12 com simplified_code 0 (vira Tipo 0, dia normal)
  pan$holidays <- data.table::rbindlist(list(
    data.table::data.table(area_code = "SE", date = OG9_O - c(5L, 12L, 20L, 33L, 47L, 61L, 75L, 90L, 120L, 150L, 200L, 260L, 330L, 390L,
                                                            100L, 130L, 170L, 230L, 290L, 350L),
                           holiday_type_id = 1L, prevcarga_code = c(3L, 4L, 5L, 3L, 4L, 6L, 3L, 4L, 5L, 3L, 6L, 4L, 5L, 3L, 1L, 1L, 1L, 1L, 1L, 1L),
                           simplified_code = 3L),
    data.table::data.table(area_code = "SE", date = OG9_O + 2:3, holiday_type_id = 12L, prevcarga_code = 12L, simplified_code = c(3L, 0L))))
  dd <- as.numeric(lubridate::as_date(pan$load$interval_start) - (OG9_O - D))
  wd <- lubridate::wday(pan$load$interval_start)
  pan$load[, load_mwh := load_mwh * (1 + 0.04 * sin(dd * dd * 0.77 + 1.1)) - 900 * (wd %in% c(1L, 7L)) + 150 * sin(dd * 3.7)]
  ddo <- as.numeric(lubridate::as_date(pan$weather_observed$valid_time) - (OG9_O - D))
  pan$weather_observed[, temperature := temperature + 2.5 * sin(ddo * ddo * 1.31 + 0.4) + 1.5 * sin(ddo * 5.3)]
  pan
}
og9_run <- function() rd2_cached("og9-run", {
  au <- lubridate::force_tz(lubridate::as_datetime(OG9_O), RD_TZ) + lubridate::hours(9L)
  cu <- rd_cuts(origin = format(OG9_O), available_until = au)
  cu$weather_issue_cutoff <- "available_until"
  D <- as.integer(OG9_O - lubridate::ymd("2023-11-01"))
  spec <- rd_spec("MED", D - 1L)
  p <- rd_fx_plugin()
  art <- rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_train(p, og9_panel(), cu, spec, ctx = rd2_ctx())))
  pan <- og9_panel(until = au)
  pan$weather_forecast <- pan$weather_forecast[issue_time <= au]
  list(art = art, out = rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_infer(p, art, pan, cu))))
})

test_that("[PLUGINS] flag12_yearend: origem 2024-12-22 com codigo 12 no horizonte completa 9x48 sem crash; o 12 vira dia normal", {
  r <- og9_run()
  expect_identical(dim(r$out$values), c(9L, 48L))
  expect_false(anyNA(r$out$values))
  expect_identical(nrow(r$out$delivery), 9L * 48L - 18L)             # corte 09:00: O sem os slots 1..18
  expect_true(all(is.finite(r$out$delivery$y_pred)) && all(r$out$delivery$y_pred > 0))
  ## o codigo 12 retrospectivo e mapeado ANTES do motor (simplified_code 3 => Tipo 1 especial, 0 => Tipo 0 normal)
  dt <- r$art$payload$day_types
  expect_false(dt[[format(OG9_O + 2L)]] %in% c(0L, NA))               # dia especial: perfil recomputado (RL_perfilFeriado), sem crash
  expect_true(dt[[format(OG9_O + 3L)]] %in% c(0L, NA))
  expect_identical(r$art$stamp$origin_day, OG9_O)
})

## ---- gate10/11 nowcast_parcial: cortes 12:00 e 13:30 (rodar a qualquer hora) -----------------------------------------------
## Convencao do clean-room: o corte e o interval_end do ultimo intervalo real; o 1o intervalo entregue termina em available_until + 30 min
## (o oraculo, que rotula pelo INICIO, fala em 12:30 = slot 26 e 14:00 = slot 29; o mesmo intervalo, indice pelo fim = 25 e 28).
test_that("[PLUGINS] nowcast_parcial: corte 12:00 entrega a partir de 12:30 e corte 13:30 a partir de 14:00 do dia O; d_plus 1..8 com 48 cheios, sem crash", {
  st <- rd5_store(); art <- rd5_art("MED"); O <- st$O
  for (x in list(list(h = "12:00", first = "12:30", d0 = 25:48), list(h = "13:30", first = "14:00", d0 = 28:48))) {
    cu <- rd5_cuts(st, x$h)
    out <- NULL
    expect_no_error(out <- rd5_infer(art, st, cu))
    expect_identical(dim(out$values), c(9L, 48L))
    expect_false(anyNA(out$values))
    ie <- out$delivery$interval_end
    expect_identical(format(min(ie), "%H:%M"), x$first)
    expect_identical(lubridate::as_date(min(ie) - lubridate::minutes(30L), tz = RD_TZ), O)
    expect_identical(ie, rd_lib$prevcarga_delivery_grid(cu, 9L))
    dp <- og_dplus(out$delivery, O)
    expect_identical(sort(unique(dp)), 0:8)
    ## d_plus = 0: so o sufixo pos-corte (slot k = k-esimo intervalo do dia pelo FIM, k = 25..48 / 28..48); d_plus >= 1 com 48 cheios
    slot0 <- as.integer(round(as.numeric(difftime(ie[dp == 0L], rd5_au("00:00", O), units = "mins")) / 30))
    expect_identical(sort(slot0), as.integer(x$d0))
    expect_true(all(vapply(1:8, function(k) sum(dp == k) == 48L, NA)))
    expect_true(out$meta$head_used)
  }
  ## CONTROL: o corte 12:00 e o 13:30 dao o mesmo dia O a partir de 14:00 so se a cabeca fosse a mesma; a mudanca de corte muda a previsao
  a <- rd5_infer(art, st, rd5_cuts(st, "12:00"))$delivery; b <- rd5_infer(art, st, rd5_cuts(st, "13:30"))$delivery
  cm <- rd5_common(a, b)
  expect_gt(max(abs(cm$a - cm$b)), RD5_TOL)
})

## ---- gate12/13 split_nowcast: train + infer SEPARADOS x forecast ACOPLADO no corte intradiario (MED e MAX) ------------------
## Contrato do oraculo (AC1/AC2/AC7 do plano de split): d_plus = 0 delta = 0 EXATO, cauda (d_plus >= 1) WMAPE <= 1% e o infer NAO
## refaz o perfil caro (os SVMs de perfil vem do artefato; so a cabeca de nowcast ajusta SVM no infer: 1 ajuste). Origem 2024-10-07
## (feriado de 12/out em d_plus = 5), corte 12:00 do deck sintetico.
og_split <- function(exo) {
  st <- rd5_store(); art <- rd5_art(exo); O <- st$O; cu <- rd5_cuts(st, "12:00")
  eng <- parent.env(rd_fx_plugin()$env)
  spy <- rd2_spy(eng); on.exit(spy$restore(), add = TRUE)
  sep <- rd5_infer(art, st, cu)
  fits_infer <- spy$log$n                                   # ajustes de SVM no infer: so a cabeca de nowcast
  acp <- rd5_forecast(st, cu, rd5_spec(exo))
  dp <- og_dplus(sep$delivery, O); d0 <- dp == 0L
  w <- vapply(1:8, function(k) rd_lib$prevcarga_wmape(sep$delivery$y_pred[dp == k], acp$delivery$y_pred[dp == k]), 0)
  list(day0 = max(abs(sep$delivery$y_pred[d0] - acp$delivery$y_pred[d0])), wmape = w, fits_infer = fits_infer,
       same_grid = identical(sep$delivery$interval_end, acp$delivery$interval_end), art = art, O = O)
}
og_split_check <- function(exo) {
  if (!(exo %in% rd5_variants())) testthat::skip(sprintf("[tier] variante %s fora de options(rd5.variants)", exo))
  r <- og_split(exo)
  expect_true(r$same_grid)
  expect_lte(r$day0, RD5_TOL)
  expect_true(all(r$wmape <= 1))
  expect_identical(r$fits_infer, 1L)                                 # so a cabeca de nowcast ajusta SVM; os 216 do perfil ficam congelados
  expect_false(r$art$payload$day_types[[format(r$O + 5L)]] %in% c(0L, NA))   # o feriado em d_plus = 5 e recomputado (lm), nao SVM
  cat(sprintf("\n[RD.6 medida] split_nowcast %s: dia D max|Delta| = %g MW; WMAPE d_plus 1..8 = %s %%\n", exo, r$day0,
              paste(sprintf("%.3g", r$wmape), collapse = " ")))
}
test_that("[PLUGINS] split_nowcast_med: train+infer separados == forecast acoplado no corte 12:00 (dia D <= 1e-5, cauda WMAPE <= 1%) e o infer nao refaz o perfil (RGD_MED)", {
  og_split_check("MED")
})
test_that("[PLUGINS] split_nowcast_max: train+infer separados == forecast acoplado no corte 12:00 (dia D <= 1e-5, cauda WMAPE <= 1%) e o infer nao refaz o perfil (RGD_MAX)", {
  og_split_check("MAX")
})

## ---- gates do oraculo que pertencem a marcos posteriores --------------------------------------------------------------------
og_deferred <- function(owner, why)
  if (exists("pc_gate_deferred", mode = "function")) pc_gate_deferred(owner, why) else testthat::skip(sprintf("[DEFERRED %s] %s", owner, why))
test_that("[DEFERRED M6] schema_long: saida LONG (schema_long, lead 1..n*48, 48 por dia) do dessem-regdin", { og_deferred("M6", "LONG nasce no M6 (gate01 do oraculo)") })
test_that("[DEFERRED M6] ytrue_real: y_true real de uma origem passada na saida LONG", { og_deferred("M6", "y_true real nasce no M6 (gate06 do oraculo)") })
test_that("[DEFERRED M7] sem_crash_harness: forecast sai 0 para varias origens pela CLI/backtest", { og_deferred("M7", "o laco multiorigem nasce no M7 (gate03 do oraculo)") })
test_that("[DEFERRED M5] fachada: verbos da CLI e recusa de artefato sobre o plugin", { og_deferred("M5", "fachada da CLI nasce no M5") })
