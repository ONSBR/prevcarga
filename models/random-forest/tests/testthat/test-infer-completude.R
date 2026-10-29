## test-infer-completude.R -- RF.3, AC 6: feature exigida NA em O ou O-1 => [prevcarga/completude] exit 3 nomeando origem e
## feature (nunca predicao com subconjunto de features). CONTROL: frame completo devolve as 2 linhas.
library(testthat)

inf_setup <- function() {
  cuts <- rf_fx_cuts(dir = rf_inf_deck("09:30"), phase = "infer"); spec <- rf_tr_spec(); cuts$spec <- spec
  frame <- rf_plug()$plugin_ingest(rf_fx_panel(rf_inf_deck("09:30"), cuts), cuts, spec)
  list(p = rf_plug(), cuts = cuts, frame = frame, art = rf_inf_artifact())
}

test_that("[PLUGINS] completude_infer: CONTROL frame completo devolve as 2 linhas (O-1, O) sem NA nas features exigidas", {
  s <- inf_setup()
  f <- s$p$plugin_inference_features(s$frame, s$art, s$cuts)
  expect_identical(f$origins, c(RF_O - 1L, RF_O))
  expect_identical(f$rows$dataOrigem, f$origins)
  expect_identical(nrow(f$rows), 2L)
  expect_identical(ncol(f$rows), 1L + length(s$p$all_feature_names()))
  expect_false(anyNA(f$rows))
  expect_true(all(vapply(f$rows[, -1L], is.numeric, NA)))                    # logicos viraram 0/1
  expect_identical(f$blocks, c(dia_O = "O-1", resto = "O"))
  expect_identical(f$area, RF_AREA)
  expect_identical(f$train_origin, s$art$payload$train_origin)
})

test_that("[PLUGINS] completude_infer: tmax_3 NA em O ou tmax_1 NA em O-1 => completude exit 3 nomeando origem e feature", {
  s <- inf_setup()
  cases <- list(list(o = RF_O, day = 3L, col = "tmax", feat = "tmax_3"),
                list(o = RF_O - 1L, day = 1L, col = "tmax", feat = "tmax_1"),
                list(o = RF_O, day = 2L, col = "tmin", feat = "tmin_2"))
  for (cs in cases) {
    fr <- s$frame; fr$weather <- data.table::copy(fr$weather)
    fr$weather[origin == cs$o & day_of_horizon == cs$day, (cs$col) := NA_real_]
    e <- fail(s$p$plugin_inference_features(fr, s$art, s$cuts))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$class_name, "completude"); expect_identical(e$exit_code, 3L)
    expect_identical(e$details$origem, format(cs$o)); expect_identical(e$details$feature, cs$feat)
  }
})

test_that("[PLUGINS] completude_infer: dia O-3 ausente => prev_2day_* NA em O-1 => completude exit 3 (nunca subconjunto)", {
  s <- inf_setup()
  fr <- s$frame; fr$load <- fr$load[dia != RF_O - 3L]
  e <- fail(s$p$plugin_inference_features(fr, s$art, s$cuts))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "completude"); expect_identical(e$exit_code, 3L)
  expect_identical(e$details$origem, format(RF_O - 1L))
  expect_match(e$details$feature, "^prev_2day_")
})

test_that("[PLUGINS] completude_infer: feature do artefato ausente das features => contract; frame de treino recusado", {
  s <- inf_setup()
  art <- s$art; art$payload$y_h001$features <- c(art$payload$y_h001$features, "feature_inexistente")
  e <- fail(s$p$plugin_inference_features(s$frame, art, s$cuts))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  expect_identical(e$details$feature, "feature_inexistente")
  fr <- s$frame; fr$phase <- "train"
  e2 <- fail(s$p$plugin_inference_features(fr, s$art, s$cuts))
  expect_identical(e2$class_name, "contract")
})
