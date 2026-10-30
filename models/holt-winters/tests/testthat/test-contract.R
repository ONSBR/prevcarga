## test-contract.R -- gate [PLUGINS] contrato_entrada do Holt-Winters (HW.6; AC 1; porta do gate02 do oraculo). Os 4 paineis
## ruins saem do deck sintetico (P.0e) por mutadores em copia e entram pelo DESPACHANTE da fundacao
## (prevcarga_plugin_train): a classe e o exit certos, mensagem ASCII, nunca preenche com 0 nem entrega NA em silencio.

test_that("[PLUGINS] contrato_entrada: load_mwh/timestamp cru/dia O => contract (2); O-1 incompleto => completude (3); CONTROL entrega MW plausivel", {
  st <- hv_store("09:00"); cuts <- hv_cuts(st); pan <- hv_panel(st, cuts, "train")
  run <- function(p, ct = cuts) hv_err(hv_train(p, ct))

  ## (1) coluna load_mwh ausente: o despachante nomeia a coluna exigida pelo plugin
  p1 <- pan; p1$load <- data.table::copy(pan$load)[, load_mwh := NULL]
  e1 <- run(p1); expect_hv_error(e1, "contract", 2L)
  expect_match(conditionMessage(e1), "load_mwh")

  ## (2) timestamp cru em vez de interval_start/interval_end
  p2 <- pan; p2$load <- data.table::copy(pan$load)[, timestamp := interval_start]
  e2 <- run(p2); expect_hv_error(e2, "contract", 2L)
  expect_match(conditionMessage(e2), "timestamp")

  ## (3) intervalo do dia O no painel de treino (o painel de infer traz O ate 09:00)
  p3 <- hv_panel(st, cuts, "infer")
  e3 <- run(p3); expect_hv_error(e3, "contract", 2L)
  expect_match(conditionMessage(e3), "2024-05-15")
  expect_gt(sum(lubridate::as_date(p3$load$interval_start, tz = cuts$tz) == cuts$origin_day), 0L)

  ## (4) O-1 com 1 intervalo a menos (mutador do P.0e): completude, nunca preenche com 0
  d47 <- hw_lib$prevcarga_fixture_drop_slots(st$dir, HW_AREA, cuts$last_data_day, "12:00", 1L)
  st47 <- hv_with_dir(st, d47)
  c47 <- hv_cuts(st47)
  e4 <- run(hv_panel(st47, c47, "train"), c47); expect_hv_error(e4, "completude", 3L)
  expect_match(conditionMessage(e4), "47")

  ## CONTROL: o painel completo treina e entrega y_pred finito, > 0, em MW plausivel (0,5x..2x a media da carga)
  art <- hv_train(pan, cuts)
  env <- hv_infer(art, hv_panel(st, cuts, "infer"), cuts)
  mu <- mean(pan$load$load_mwh)
  y <- env$delivery$y_pred
  expect_false(anyNA(y)); expect_true(all(is.finite(y)) && all(y > 0))
  expect_true(all(y > 0.5 * mu & y < 2 * mu))
  expect_identical(env$delivery$interval_end, hw_lib$prevcarga_delivery_grid(cuts, 9))
})
