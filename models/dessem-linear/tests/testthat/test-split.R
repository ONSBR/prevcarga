## test-split.R -- DL.8 (AC11 do epico): train+infer SEPARADOS == forecast ACOPLADO na mesma origem (ADR-0023, origin_scoped).
## Dia D (O): max|Delta| <= 1e-5; cauda (O+1..O+8): WMAPE <= 1% [TBD: ADR-0031]; exatamente 1 `tune_wsvm` no infer (so a DM(O));
## artefato de outra origem/variante/modelo => [prevcarga/contract]. CONTROL: `seed` 43 no acoplado => > 1e-5 no dia D.

dl_day_d <- function(delivery, O = DL8_O) lubridate::as_date(delivery$interval_end - lubridate::minutes(1L), tz = DL_TZ) == O

test_that("[PLUGINS] gate17_split: infer == forecast acoplado no dia D (<= 1e-5), cauda WMAPE <= 1%, 1 ajuste de SVM no infer", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art()
  spy <- dl_spy_tune(); on.exit(spy$restore(), add = TRUE)
  split <- dl_infer(art, st, cuts)
  expect_identical(spy$log$n, 1L)                                     # so a DM(O); os SVMs vem congelados
  expect_match(spy$log$fml[[1L]], "+ mm", fixed = TRUE)
  spy$reset()
  coup <- dl_forecast(st, cuts)
  expect_identical(spy$log$n, 251L)                                   # o acoplado treina (250: cabeca de referencia + 249) e refaz a DM(O) (1)
  spy$restore()
  expect_identical(split$delivery$interval_end, coup$delivery$interval_end)
  d0 <- dl_day_d(split$delivery)
  expect_true(any(d0)); expect_true(any(!d0))
  expect_lte(max(abs(split$delivery$y_pred[d0] - coup$delivery$y_pred[d0])), DL8_TOL)
  expect_lte(dl_lib$prevcarga_wmape(split$delivery$y_pred[!d0], coup$delivery$y_pred[!d0]), 1)      # percentual [TBD: ADR-0031]
  expect_lte(max(abs(split$values - coup$values)), DL8_TOL)
  dl_cached("coupled-42", coup)
})

test_that("[PLUGINS] gate17_split: artefato de outra origem, variante ou modelo => contract antes de qualquer ajuste", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art(); p <- dl_plugin(); env <- p$env
  spy <- dl_spy_tune(); on.exit(spy$restore(), add = TRUE)
  ipan <- dl_fx_panel(st, cuts, "infer")
  ## outra origem: o despachante recusa pelo carimbo
  prev <- modifyList(cuts, list(origin_day = cuts$origin_day - 1L, forecast_origin = cuts$origin_day - 1L,
                                train_origin = cuts$origin_day - 1L))
  e <- dl_err(dl_infer(art, st, prev, panel = ipan))
  expect_dl_class(e, "contract", 2L, id = "plugin.artifact_mismatch")
  ## outro modelo
  other <- art; other$stamp$model_id <- "holt-winters"
  expect_dl_class(dl_err(dl_infer(other, st, cuts)), "contract", 2L, id = "plugin.artifact_mismatch")
  ## outra variante (o deck do membro MAX com o artefato MED) e outro model_key/origem/area: a guarda do estagio
  m <- dl_model_panel(ipan, cuts)
  frame <- dl_with_cli(suppressMessages(env$plugin_ingest(m$panel, m$cuts, NULL)))
  feat <- p$descriptor$stages$inference_features$fn
  pl <- art$payload
  fr_max <- dl_with_cli(suppressMessages(env$plugin_ingest(m$panel, m$cuts, dl_fx_spec("MAX"))))
  e1 <- dl_err(feat(fr_max, pl))
  expect_dl_class(e1, "contract", 2L, id = "plugin.dessem_linear_artifact_mismatch"); expect_match(conditionMessage(e1), "variant", fixed = TRUE)
  v1 <- pl; v1$variant <- "MAX"
  e2 <- dl_err(feat(frame, v1))
  expect_dl_class(e2, "contract", 2L, id = "plugin.dessem_linear_artifact_mismatch"); expect_match(conditionMessage(e2), "model_key", fixed = TRUE)
  o1 <- pl; o1$head_recipe$dias <- format(DL8_O - 1L + 0:8)
  expect_dl_class(dl_err(feat(frame, o1)), "contract", 2L, id = "plugin.dessem_linear_artifact_mismatch")
  a1 <- pl; a1$area <- "NE"
  expect_dl_class(dl_err(feat(frame, a1)), "contract", 2L, id = "plugin.dessem_linear_artifact_mismatch")
  expect_identical(spy$log$n, 0L)                                     # nada foi ajustado
  ## CONTROL: o artefato de O (variante MED) segue
  expect_null(dl_err(feat(frame, pl)))
})

test_that("[PLUGINS] gate17_split: CONTROL seed 43 no acoplado => y_pred do dia D difere (> 1e-5) do infer do artefato de seed 42 (deck sensivel ao CV)", {
  st <- dl_fx_cv_store(); cuts <- dl_fx_cuts(st)
  art <- dl_train_memo(st, cuts)
  split <- dl_infer(art, st, cuts)
  spy <- dl_spy_tune(); on.exit(spy$restore(), add = TRUE)
  spec43 <- dl_fx_spec(hist = st$hist); spec43$seed <- 43L
  coup43 <- dl_forecast(st, cuts, spec43)
  fresh43 <- local({ set.seed(43L); get(".Random.seed", globalenv()) })
  expect_identical(spy$log$seed0, fresh43)                             # a semente do spec chega ao gerador do acoplado
  d0 <- dl_day_d(split$delivery)
  expect_gt(max(abs(split$delivery$y_pred[d0] - coup43$delivery$y_pred[d0])), DL8_TOL)
})
