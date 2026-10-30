## test-gates.R -- gates do oraculo (tests/run_all_gates.R do prevcargadessem3) sem teste proprio nas fatias DL.5-DL.8 (DL.9):
## gate02 escala_mw, gate07 dataPrev = O e gate15 corte as 13:00 (rodar a qualquer hora). Deck sintetico do DL.8 (leitor -> despachante
## -> plugin); a escala MW do deck REAL fica no tier parity (test-parity.R). Sem variaveis de ambiente; datas por lubridate com tz.

test_that("[PLUGINS] gate02_escala_mw: entrega finita, positiva e na escala da carga de entrada (fixture)", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art()
  out <- dl_infer(art, st, cuts)
  expect_identical(dim(out$values), c(9L, 48L))
  expect_true(all(is.finite(out$values)) && all(out$values > 0))
  expect_true(all(is.finite(out$delivery$y_pred)) && all(out$delivery$y_pred > 0))
  ## escala: a mediana da entrega esta a menos de um fator 2 da mediana da carga dos ultimos 7 dias do painel (MW da fixture)
  pan <- dl_fx_panel(st, cuts, "infer")
  recent <- pan$load[interval_start >= max(interval_start) - lubridate::days(7L), load_mwh]
  r <- stats::median(out$delivery$y_pred) / stats::median(recent)
  expect_gt(r, 0.5); expect_lt(r, 2)
  ## CONTROL: o oraculo (mediana 5000-120000 MW) rejeitaria uma escala errada de 1000x
  expect_false(stats::median(out$delivery$y_pred / 1000) / stats::median(recent) > 0.5)
})

test_that("[PLUGINS] gate07_dataprev_o: dataPrev = O (1o dia de previsao e o dia da rodada), entrega comeca em available_until + 30 min", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art()
  m <- dl_model_panel(dl_fx_panel(st, cuts, "infer"), cuts)
  deck <- dl_with_cli(suppressMessages(dl_plugin()$env$plugin_ingest(m$panel, m$cuts, NULL)))
  expect_identical(deck$dataPrev, cuts$origin_day)
  expect_identical(art$stamp$origin_day, cuts$origin_day)
  expect_identical(format(art$stamp$origin_day), format(DL8_O))
  out <- dl_infer(art, st, cuts)
  expect_identical(min(out$delivery$interval_end), cuts$available_until + lubridate::minutes(30))
  ## 1a linha de values = dia O: o fim da grade e O+8 as 24:00
  t0 <- lubridate::force_tz(lubridate::as_datetime(cuts$origin_day), DL_TZ)
  expect_identical(max(out$delivery$interval_end), t0 + lubridate::days(9L))
  ## CONTROL: um artefato de outra origem NAO serve (a origem e o dataPrev do deck)
  e <- dl_err(dl_lib$prevcarga_plugin_infer(dl_plugin(), art, dl_fx_panel(st, cuts, "infer"),
                                            modifyList(cuts, list(origin_day = cuts$origin_day - 1L))))
  expect_s3_class(e, "prevcarga_error")
})

test_that("[PLUGINS] gate15_sem_crash_13h: corte as 13:00 (rodar a qualquer hora) entrega 9 x 48 sem erro, a partir de 13:30", {
  st <- dl_fx_store("13:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art()
  expect_identical(cuts$available_until, dl_au("13:00"))
  out <- NULL
  expect_no_error(out <- dl_infer(art, st, cuts))
  expect_identical(dim(out$values), c(9L, 48L))
  expect_true(all(is.finite(out$values)))
  expect_identical(min(out$delivery$interval_end), dl_au("13:30"))
  expect_identical(nrow(out$delivery), 9L * 48L - 26L)         # O: slots 27..48 (13:30..24:00) + 8 dias cheios
})
