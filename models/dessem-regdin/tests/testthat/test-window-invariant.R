## test-window-invariant.R -- RD.4 (AC2, AC3): invariante de janela do infer (history_days.infer == train, origin_scoped) e
## deriva do ARIMA. FORBIDDEN/CONTROL de cada um; os dois bracos rodam, sem skip. Loader real, motor real, despachante real,
## painel sintetico cru. Os artefatos MED/MAX sao os do cache de sessao (rd3_state; corte de treino 09:00).

test_that("[PLUGINS] janela_infer_igual_treino: FORBIDDEN painel exato (fingerprint bate, infer == forecast no dia D)", {
  st <- rd3_state(); p <- st$p; cuts <- rd_fx_cuts()
  ok <- rd_infer(st$med, cuts = cuts)
  expect_identical(dim(ok$values), c(9L, 48L)); expect_false(anyNA(ok$values))
  ## o fingerprint bate: o refit do ARIMA sobre a janela exata do infer reproduz ordem, coeficientes e digest do treino
  s <- rd3_stage_inf(p, st$med, rd_fx_panel(until = cuts$available_until), cuts$available_until)
  expect_identical(s$inf$panel_window$start, st$med$payload$train_window$start)
  expect_identical(s$inf$panel_window$end, st$med$payload$train_window$end)
  expect_true(rd_with_cli(suppressMessages(is.list(p$env$.rd_check_fingerprint(
    s$inf$features$series$serieDM, st$med$payload$config_mdl, st$med$payload$arima_fingerprint)))))
  ## infer == forecast acoplado no dia D
  fc <- rd_forecast(cuts = cuts)
  dD <- max(abs(ok$values[1L, ] - fc$values[1L, ]), na.rm = TRUE)
  cat(sprintf("\n[RD.4 medida] infer x forecast no dia D: max|Delta| = %.3g MW\n", dD))
  expect_lte(dD, 1e-5)
  expect_lte(rd_fx_ydelta(ok, fc), 1e-5)
})

test_that("[PLUGINS] janela_infer_igual_treino: painel 30 dias mais curto => window exit 5 antes de ajustar; CONTROL bypass > 50 MW nos dias entregues", {
  st <- rd3_state(); p <- st$p; cuts <- rd_fx_cuts(); au <- cuts$available_until
  ok <- rd_infer(st$med, cuts = cuts)
  short <- rd_fx_shorten(until = au, days = 30L)
  spy_pass <- rd_spy(".rd_predict_pass"); spy_fp <- rd_spy(".rd_check_fingerprint")
  cond <- tryCatch(rd_infer(st$med, short, cuts), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_window_mismatch")
  expect_identical(cond$exit_code, 5L); expect_identical(cond$class_name, "window")
  expect_true(rd_ascii(conditionMessage(cond)))
  expect_false(is.null(cond$details$expected)); expect_false(is.null(cond$details$got))
  expect_identical(spy_pass$count(), 0L); expect_identical(spy_fp$count(), 0L)    # nenhum ajuste, nem o refit do ARIMA
  spy_pass$restore(); spy_fp$restore()
  ## CONTROL com dente: com o bypass (so teste; ausente do descriptor) o mesmo painel curto entrega e a previsao muda > 50 MW
  s2 <- rd3_stage_inf(p, st$med, short, au)
  ctl <- rd_with_cli(suppressMessages(p$env$plugin_infer(st$med$payload, s2$inf, s2$cuts, .bypass_window_guard = TRUE)))
  expect_false(".bypass_window_guard" %in% names(formals(p$descriptor$stages$inference$fn)))
  expect_false(".bypass_window_guard" %in% names(p$descriptor))
  perday <- apply(abs(ctl$values - ok$values), 1L, max)                       # max|Delta| por dia entregue (D = O .. O+8)
  cat(sprintf("[RD.4 medida] bypass x janela exata, por dia (MW): %s\n", paste(sprintf("%.1f", perday), collapse = " ")))
  expect_gt(max(perday), 50)                                                   # dente: algum dia entregue muda > 50 MW
  expect_gt(rd_fx_ydelta(ctl, ok), 50)
  expect_gt(perday[[1L]], 1e-5)                                                # e o proprio dia O muda
  ## o painel curto sem bypass seguia abortando (os dois bracos rodaram)
  expect_error(rd_with_cli(p$env$plugin_infer(st$med$payload, s2$inf, s2$cuts)), class = "prevcarga_error")
})

test_that("[PLUGINS] arima_drift: coeficiente +1e-3, ordem e digest adulterados abortam nomeando o campo; CONTROL original infere", {
  st <- rd3_state(); cuts <- rd_fx_cuts()
  expect_true(is.list(rd_infer(st$med, cuts = cuts)))                       # CONTROL: o artefato original infere
  fp <- st$med$payload$arima_fingerprint
  nm <- names(fp$coef)[2L]
  bad <- st$med; bad$payload$arima_fingerprint$coef[[nm]] <- bad$payload$arima_fingerprint$coef[[nm]] + 1e-3
  cond <- tryCatch(rd_infer(bad, cuts = cuts), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_arima_drift"); expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L)
  expect_identical(cond$details$field, "coef"); expect_identical(cond$details$coef, nm)
  expect_match(conditionMessage(cond), nm, fixed = TRUE)                       # o nome do coeficiente na mensagem
  expect_true(rd_ascii(conditionMessage(cond)))
  swap <- st$med; swap$payload$arima_fingerprint$order[["p"]] <- swap$payload$arima_fingerprint$order[["p"]] + 1L
  c2 <- tryCatch(rd_infer(swap, cuts = cuts), prevcarga_error = function(e) e)
  expect_identical(c2$id, "plugin.regdin_arima_drift"); expect_identical(c2$details$field, "order")
  dig <- st$med; dig$payload$arima_fingerprint$dm_digest <- "0"
  c3 <- tryCatch(rd_infer(dig, cuts = cuts), prevcarga_error = function(e) e)
  expect_identical(c3$id, "plugin.regdin_arima_drift"); expect_identical(c3$details$field, "dm_digest")
})

test_that("[PLUGINS] arima_drift: mable NULL do fable (janela sem os 12 meses / regressores colineares) aborta arima_drift nomeando a causa", {
  st <- rd3_state(); p <- st$p; au <- rd_fx_cuts()$available_until
  short <- rd_fx_panel(until = au, days = 100L)                                # menos de 12 meses: fable devolve NULL model
  s <- rd3_stage_inf(p, st$med, short, au)
  eng <- parent.env(p$env)
  raw <- suppressWarnings(eng$execDemandaMediaTS(data.table::copy(s$inf$features$series$serieDM), st$med$payload$config_mdl))
  expect_true(inherits(raw$mdl$modeloTreinado[[1L]][[1L]]$fit, "null_mdl"))   # premissa: o motor nao avisa nem erra
  ## direto no passo de conferencia
  cond <- tryCatch(rd_with_cli(p$env$.rd_check_fingerprint(s$inf$features$series$serieDM, st$med$payload$config_mdl,
                                                            st$med$payload$arima_fingerprint)),
                   prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_arima_drift"); expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$details$field, "null_model")
  expect_match(conditionMessage(cond), "NULL", fixed = TRUE)
  expect_match(conditionMessage(cond), "12 meses", fixed = TRUE); expect_match(conditionMessage(cond), "colineares", fixed = TRUE)
  expect_true(rd_ascii(conditionMessage(cond)))
  ## ponta a ponta: com a janela do artefato igualada a do painel curto a guarda de janela passa e o NULL mable aborta no infer
  art <- st$med$payload; art$train_window <- s$inf$panel_window
  c2 <- tryCatch(rd_with_cli(p$env$plugin_infer(art, s$inf, s$cuts)), prevcarga_error = function(e) e)
  expect_identical(c2$id, "plugin.regdin_arima_drift"); expect_identical(c2$details$field, "null_model")
  ## CONTROL: a janela exata (14 meses) nao produz NULL model
  full <- rd3_stage_inf(p, st$med, rd_fx_panel(until = au), au)
  ok <- suppressWarnings(eng$execDemandaMediaTS(data.table::copy(full$inf$features$series$serieDM), st$med$payload$config_mdl))
  expect_false(inherits(ok$mdl$modeloTreinado[[1L]][[1L]]$fit, "null_mdl"))
})
