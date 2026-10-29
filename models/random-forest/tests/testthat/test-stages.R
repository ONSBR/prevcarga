## test-stages.R -- RF.3, AC 5: plugin completo (sem stubs), forma do envelope, formals sem `params`, o infer so le o artefato.
library(testthat)

test_that("[PLUGINS] estagios: loader P.0b resolve os 5 estagios para funcoes reais (sem stubs) e a assinatura canonica", {
  root <- rf_fx_plugin_copy(stubs = FALSE)
  expect_false(file.exists(file.path(root, "random-forest", "R", "zz_stubs.R")))
  rf_lib$prevcarga_plugin_unload_all()
  pl <- rf_lib$prevcarga_plugin_load("random-forest", root)
  st <- pl$descriptor$stages
  expect_identical(names(st), c("ingestion", "feature_engineering", "training", "inference_features", "inference"))
  for (s in st) expect_true(is.function(s$fn))
  expect_identical(st$inference_features$fn, pl$env$rf_inference_features)
  expect_identical(st$inference$fn, pl$env$rf_inference)
  expect_identical(pl$descriptor$semihourly_rule, pl$env$rf_semihourly_rule)
  expect_identical(names(formals(st$inference_features$fn)), c("frame", "payload"))
  expect_identical(names(formals(st$inference$fn)), c("payload", "inf_features", "cuts"))
  rf_lib$prevcarga_plugin_check_signature(pl)
  for (n in c("plugin_build_features", "plugin_train", "plugin_ingest", "plugin_infer", "plugin_inference_features",
              "rf_freeze", "rf_thaw", "rf_semihourly_rule"))
    expect_true(is.function(pl$env[[n]]), info = n)
})

test_that("[PLUGINS] estagios: formals sem params (tudo vem do artefato)", {
  p <- rf_plug()
  expect_identical(names(formals(p$plugin_inference_features)), c("frame", "artifact", "cuts"))
  expect_identical(names(formals(p$plugin_infer)), c("artifact", "inf_feat", "cuts"))
  expect_false("params" %in% c(names(formals(p$plugin_infer)), names(formals(p$plugin_inference_features)),
                               names(formals(p$rf_inference)), names(formals(p$rf_inference_features))))
})

test_that("[PLUGINS] estagios: envelope do membro (forma, values 2 x 216, delivery, meta sem campos proibidos)", {
  r <- rf_inf_run(rf_inf_deck("09:30"))
  e <- r$env; O <- RF_O
  expect_identical(e$model_id, "random-forest")
  expect_identical(e$layout, "rf_wide_hourly_216")
  expect_identical(e$granularity_min, 60L)
  expect_identical(e$area, RF_AREA)
  expect_identical(e$origin_day, O)
  expect_identical(e$available_until, r$cuts$available_until)
  expect_identical(dim(e$values), c(2L, 216L))
  expect_identical(rownames(e$values), format(c(O - 1L, O)))
  expect_identical(colnames(e$values), sprintf("y_h%03d", 1:216))
  expect_true(all(is.finite(e$values)))
  expect_identical(names(e$delivery), c("interval_end", "y_pred"))
  expect_identical(e$delivery$interval_end, rf_lib$prevcarga_delivery_grid(r$cuts, 9L))
  expect_identical(e$meta$native_origins, c(O - 1L, O))
  expect_identical(e$meta$train_origin, rf_inf_artifact()$payload$train_origin)
  expect_identical(e$meta$blocks, c(dia_O = "O-1", resto = "O"))
  expect_identical(e$meta$fora_da_faixa, "O+9")
  expect_null(rf_lib$.prevcarga_plugin_find_forbidden(e))
  ## o despachante P.0c confere o mesmo envelope (campos, layout, grade) sobre o plugin carregado pelo loader
  rf_lib$prevcarga_plugin_unload_all()
  pl <- rf_lib$prevcarga_plugin_load("random-forest", rf_fx_plugin_copy(stubs = FALSE))
  expect_true(rf_lib$.prevcarga_plugin_check_envelope(c(e, list()), pl, pl$spec, r$cuts))
  ## a forma despachada devolve so os campos que o despachante aceita
  p <- rf_plug()
  d <- p$rf_inference(rf_inf_artifact()$payload, r$feat, r$cuts)
  expect_identical(names(d), c("area", "values", "delivery", "meta"))
  expect_identical(d$values, e$values)
})

test_that("[PLUGINS] estagios: spy -- o infer nunca chama plugin_train nem randomForest()", {
  p <- rf_plug()
  r0 <- rf_inf_run(rf_inf_deck("09:30"))
  cnt <- new.env(); cnt$train <- 0L; cnt$rf <- 0L
  ns <- asNamespace("randomForest")
  suppressMessages({
    trace("plugin_train", where = p, print = FALSE, tracer = bquote(assign("train", get("train", envir = .(cnt)) + 1L, envir = .(cnt))))
    trace("train_multioutput_rf", where = p, print = FALSE, tracer = bquote(assign("train", get("train", envir = .(cnt)) + 1L, envir = .(cnt))))
    trace("randomForest", where = ns, print = FALSE, tracer = bquote(assign("rf", get("rf", envir = .(cnt)) + 1L, envir = .(cnt))))
  })
  on.exit(suppressMessages({
    untrace("plugin_train", where = p); untrace("train_multioutput_rf", where = p); untrace("randomForest", where = ns)
  }), add = TRUE)
  cuts <- r0$cuts
  e <- p$plugin_infer(rf_inf_artifact(), r0$feat, cuts)
  d <- p$rf_inference(rf_inf_artifact()$payload, r0$feat, cuts)
  expect_identical(e$values, r0$env$values)
  expect_identical(cnt$train, 0L); expect_identical(cnt$rf, 0L)
  ## CONTROL: o spy enxerga um treino de verdade
  rf_tr_run(rf_inf_deck("09:00"))
  expect_gte(cnt$train, 1L); expect_gte(cnt$rf, 1L)
})

test_that("[PLUGINS] estagios: CONTROL artefato com train_origin > cuts$train_origin => [prevcarga/contract] exit 2", {
  r <- rf_inf_run(rf_inf_deck("09:30"))
  art <- rf_inf_artifact()
  art$stamp$train_origin <- r$cuts$train_origin + 1L
  e <- fail(rf_plug()$plugin_infer(art, r$feat, r$cuts))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  ## payload puro (despachante) com train_origin alem do corte tambem aborta
  pay <- rf_inf_artifact()$payload; pay$train_origin <- r$cuts$train_origin + 1L
  e2 <- fail(rf_plug()$plugin_infer(pay, r$feat, r$cuts))
  expect_identical(e2$class_name, "contract"); expect_identical(e2$exit_code, 2L)
  ## artefato de outro model_id e recusado pela conferencia da fundacao
  art2 <- rf_inf_artifact(); art2$stamp$model_id <- "outro"
  expect_identical(fail(rf_plug()$plugin_infer(art2, r$feat, r$cuts))$class_name, "contract")
})
