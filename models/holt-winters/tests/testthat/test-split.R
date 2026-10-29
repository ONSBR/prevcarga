## test-split.R -- gates [PLUGINS] mesmo_dia e infer_igual_forecast do Holt-Winters (HW.6; AC 3 e 7). Split origin_scoped
## (ADR-0023): 1 treino por origem O com serie ate O-1; o available_until novo (rodada das 12:00) nao re-treina e o infer
## nao refita nada. Tudo pelo leitor -> despachante/estagios sobre o deck sintetico (P.0e).

test_that("[PLUGINS] mesmo_dia: decks 09:00 e 12:00 => mesmo sha256 e y_pred identico em interval_end > 12:00; CONTROL origin_day O-1 => hw.artifact_origin", {
  s09 <- hv_store("09:00"); s12 <- hv_store("12:00")
  c09 <- hv_cuts(s09); c12 <- hv_cuts(s12)
  expect_identical(c09$available_until, hv_au("09:00")); expect_identical(c12$available_until, hv_au("12:00"))
  expect_identical(c09$origin_day, c12$origin_day)
  a09 <- hv_train(hv_panel(s09, c09, "train"), c09)
  a12 <- hv_train(hv_panel(s12, c12, "train"), c12)
  expect_identical(hv_sha(a09), hv_sha(a12))                                    # novo available_until nao re-treina
  expect_identical(a09$payload$mean_interval, a12$payload$mean_interval)
  ## o 2o deck roda infer com o artefato do 1o (despachante: confere o carimbo e nao refita)
  o09 <- hv_infer(a09, hv_panel(s09, c09, "infer"), c09)
  o12 <- hv_infer(a09, hv_panel(s12, c12, "infer"), c12)
  after12 <- function(o) o$delivery[interval_end > hv_au("12:00")]
  d09 <- after12(o09); d12 <- after12(o12)
  expect_gt(nrow(d12), 400L)
  expect_identical(d09$interval_end, d12$interval_end)
  expect_identical(d09$y_pred, d12$y_pred)                                      # max|delta| = 0
  expect_identical(o09$values, o12$values)
  expect_identical(format(o12$delivery$interval_end[[1L]], "%H:%M"), "12:30")
  ## o forecast acoplado (treina + infere) tambem nao depende da manha
  f09 <- hv_fc(hv_panel(s09, c09, "infer"), c09)$delivery; f12 <- hv_fc(hv_panel(s12, c12, "infer"), c12)$delivery
  expect_identical(f09[f09$interval_end > hv_au("12:00"), ], f12[f12$interval_end > hv_au("12:00"), ])
  expect_identical(max(abs(f09$y_pred[match(f12$interval_end[f12$interval_end > hv_au("12:00")], f09$interval_end)] -
                           f12$y_pred[f12$interval_end > hv_au("12:00")])), 0)

  ## CONTROL: artefato com origin_day re-carimbado para O-1 usado em O
  bad <- a09; bad$payload$origin_day <- c09$origin_day - 1L
  e <- hv_err(hv_infer(bad, hv_panel(s09, c09, "infer"), c09))
  expect_hv_error(e, "contract", 2L, id = "hw.artifact_origin")
  expect_match(conditionMessage(e), "2024-05-1[45]")
  ## o mesmo CONTROL nos estagios do plugin (sem despachante)
  env <- hv_env()
  deck <- suppressWarnings(env$plugin_ingest(hv_panel(s09, c09, "infer"), c09))
  e2 <- hw_catch(env$plugin_inference_features(deck, bad$payload, c09))
  expect_identical(e2$id, "hw.artifact_origin")
})

test_that("[PLUGINS] infer_igual_forecast: infer(thaw(freeze(train))) identical a forecast em values, day0_raw e delivery; CONTROL history -7 difere", {
  st <- hv_store("09:00"); cuts <- hv_cuts(st); env <- hv_env(); spec <- hv_spec(hv_plugin())
  tpan <- hv_panel(st, cuts, "train"); ipan <- hv_panel(st, cuts, "infer")
  deck <- suppressWarnings(env$plugin_ingest(tpan, cuts))
  feats <- suppressMessages(env$plugin_build_features(deck, spec))
  art <- env$plugin_thaw(env$plugin_freeze(env$plugin_train(feats, spec)))
  inf <- suppressWarnings(env$plugin_infer(art, env$plugin_inference_features(suppressWarnings(env$plugin_ingest(ipan, cuts)), art, cuts), cuts))
  fc <- hv_fc(ipan, cuts, spec)
  expect_identical(inf$values, fc$values)
  expect_identical(inf$meta$day0_raw, fc$meta$day0_raw)
  expect_identical(inf$delivery, fc$delivery)                                   # delta = 0 em toda a entrega, nao so no dia D
  expect_identical(inf, fc)
  expect_gt(nrow(fc$delivery), 400L)
  ## o despachante (treino + infer) entrega os mesmos y_pred
  de <- hv_infer(hv_train(tpan, cuts, spec), ipan, cuts)
  expect_identical(de$delivery$y_pred, fc$delivery$y_pred); expect_identical(de$values, fc$values)
  ## CONTROL: history_days.train -7 muda o treino e a regua tem dente
  s7 <- spec; s7$history_days$train <- spec$history_days$train - 7L
  p7 <- hw_fx_panel(st, cuts, "infer", history_days = HV_HIST - 7L)
  fc7 <- hv_fc(p7, cuts, s7)
  expect_gt(max(abs(fc7$values - inf$values)), 1e-5)
  expect_gt(hv_delta(fc7$delivery, inf$delivery), HV_TOL)
})
