## test-split.R -- RD.5 (AC8 do epico, `split_vs_acoplado`): train + infer SEPARADOS == forecast ACOPLADO na mesma origem
## (ADR-0023, origin_scoped). Origem 2024-10-07, cuja janela de entrega tem o feriado de 12/out em d_plus = 5 (dia especial:
## `RL_perfilFeriado` recomputado no infer). Dia D (O): max|Delta| <= 1e-5 nos cortes 00:00 e 12:00 e nas duas variantes; cauda
## (d_plus >= 1): WMAPE por d_plus <= 1% [TBD: ADR-0031], registrada. CONTROL: forecast SEM a cabeca de referencia no treino
## desloca o RNG e da > 1e-5 no dia D contra o infer 12:00; artefato de outra origem => [prevcarga/contract].
## O forecast acoplado e o do despachante (`prevcarga_plugin_forecast`: treina so ate last_data_day e infere) sobre o MESMO
## painel do infer. Cada acoplado re-treina (~45 s): shardavel por `options(rd5.variants = "MED")`.

rd5_dplus <- function(delivery, O) {
  s <- delivery$interval_end - lubridate::minutes(30L)               # hora do inicio do intervalo
  as.integer(lubridate::as_date(s, tz = RD_TZ) - O)
}

for (e in rd5_variants()) {
  local({
    exo <- e
    test_that(sprintf("[PLUGINS] split_vs_acoplado: %s: infer == forecast acoplado no dia D (<= 1e-5) nos cortes 00:00 e 12:00; cauda WMAPE <= 1%%", exo), {
      st <- rd5_store(); art <- rd5_art(exo); spec <- rd5_spec(exo); O <- st$O
      ## a janela de entrega tem o feriado em d_plus = 5 e o artefato o trata como dia especial (sem SVM)
      expect_identical(art$payload$dias[[6L]], format(O + 5L))
      expect_false(art$payload$day_types[[format(O + 5L)]] %in% c(0L, NA))
      expect_null(art$payload$svm_perfil[[format(O + 5L)]])
      expect_identical(art$stamp$origin_day, O)
      for (h in c("00:00", "12:00")) {
        cu <- rd5_cuts(st, h)
        sep <- rd5_infer(art, st, cu)
        acp <- rd5_forecast(st, cu, spec)
        expect_identical(sep$delivery$interval_end, acp$delivery$interval_end)
        expect_identical(sep$delivery$interval_end, rd_lib$prevcarga_delivery_grid(cu, 9L))
        dp <- rd5_dplus(sep$delivery, O)
        d0 <- dp == 0L
        expect_true(any(d0)); expect_true(any(dp == 8L))
        dD <- max(abs(sep$delivery$y_pred[d0] - acp$delivery$y_pred[d0]))
        expect_lte(dD, RD5_TOL)                                          # dia D
        expect_lte(max(abs(sep$values[1L, ] - acp$values[1L, ]), na.rm = TRUE), RD5_TOL)
        expect_false(anyNA(sep$values)); expect_false(anyNA(acp$values))
        ## cauda: WMAPE (percentual) por d_plus, registrada [TBD: ADR-0031]
        w <- vapply(1:8, function(k) rd_lib$prevcarga_wmape(sep$delivery$y_pred[dp == k], acp$delivery$y_pred[dp == k]), 0)
        tail_delta <- max(abs(sep$delivery$y_pred[!d0] - acp$delivery$y_pred[!d0]))
        cat(sprintf("\n[RD.5 medida] split %s corte %s: dia D max|Delta| = %g MW; cauda max|Delta| = %g MW; WMAPE por d_plus 1..8 = %s %% [TBD: ADR-0031]\n",
                    exo, h, dD, tail_delta, paste(sprintf("%.3g", w), collapse = " ")))
        expect_true(all(w <= 1))
        expect_identical(sep$meta$head_used, h != "00:00")
        expect_identical(sep$meta$head_used, acp$meta$head_used)
      }
    })
  })
}

test_that("[PLUGINS] split_vs_acoplado: CONTROL forecast sem a cabeca de referencia no treino da > 1e-5 no dia D contra o infer 12:00", {
  st <- rd5_store(); exo <- rd5_variants()[[1L]]; art <- rd5_art(exo); O <- st$O
  cu <- rd5_cuts(st, "12:00")
  sep <- rd5_infer(art, st, cu)
  no_ref <- rd2_with_plugin_fn("rd_reference_mm", function(real) function(...) real(...)[0L], rd5_forecast(st, cu, rd5_spec(exo)))
  d0 <- rd5_dplus(sep$delivery, O) == 0L
  dD <- max(abs(sep$delivery$y_pred[d0] - no_ref$delivery$y_pred[d0]))
  cat(sprintf("\n[RD.5 medida] split CONTROL %s sem cabeca de referencia: dia D max|Delta| = %.3g MW\n", exo, dD))
  expect_gt(dD, RD5_TOL)
})

test_that("[PLUGINS] split_vs_acoplado: artefato de outra origem => [prevcarga/contract] antes de qualquer ajuste", {
  st <- rd5_store(); exo <- rd5_variants()[[1L]]; art <- rd5_art(exo); cu <- rd5_cuts(st, "09:00")
  pan <- rd5_panel(st, cu, "infer")
  eng <- parent.env(rd_fx_plugin()$env)
  sp <- rd2_spy(eng); on.exit(sp$restore(), add = TRUE)
  ## outra origem: o despachante recusa pelo carimbo (origin_day)
  prev <- utils::modifyList(cu, list(origin_day = cu$origin_day - 1L, forecast_origin = cu$origin_day - 1L,
                                     train_origin = cu$origin_day - 1L))
  e1 <- rd5_err(rd5_infer(art, st, prev, panel = pan))
  expect_rd5_class(e1, "contract", 2L, id = "plugin.artifact_mismatch")
  expect_match(conditionMessage(e1), "origin_day", fixed = TRUE)
  ## outro modelo no carimbo
  other <- art; other$stamp$model_id <- "holt-winters"
  expect_rd5_class(rd5_err(rd5_infer(other, st, cu, panel = pan)), "contract", 2L, id = "plugin.artifact_mismatch")
  ## a guarda do estagio (payload de outra origem, carimbo adulterado para passar o despachante)
  stale <- art; stale$payload$dias <- format(cu$origin_day - 1L + 0:8)
  e2 <- rd5_err(rd5_infer(stale, st, cu, panel = pan))
  expect_rd5_class(e2, "contract", 2L, id = "plugin.regdin_artifact_mismatch")
  expect_identical(sp$log$n, 0L)                                       # nada foi ajustado
  ## CONTROL: o artefato da origem certa segue
  expect_null(rd5_err(rd5_infer(art, st, cu, panel = pan)))
})
