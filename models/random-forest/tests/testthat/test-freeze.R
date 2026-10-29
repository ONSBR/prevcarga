## test-freeze.R -- RF.2: freeze/thaw puro sobre o contrato comum da fundacao (P.0d). Round-trip identical, predict
## max|d| = 0, scan_pure limpo, carimbos de prevcarga_plugin_stamp, sem trained_at/package_version; CONTROL: relogio
## acusado, bytes adulterados => integrity exit 9, outro model_id/granularidade/train_origin => contract. Porte do gate05.
library(testthat)

freeze_to <- function(art) {
  f <- tempfile("rf-frz-", fileext = ".rds")
  list(path = f, sha = rf_lib$prevcarga_plugin_freeze_bytes(art, f))
}
plugin_ref <- function() list(descriptor = rf_plug()$.rf_identity())

test_that("[PLUGINS] freeze: dois treinos byte-identicos => mesmo sha256; thaw identical ao objeto em memoria; predict max|d| = 0", {
  a <- rf_tr_run(); b <- rf_tr_run()
  fa <- freeze_to(a$art); fb <- freeze_to(b$art)
  on.exit(unlink(c(fa$path, fb$path, paste0(c(fa$path, fb$path), ".manifest.json"))), add = TRUE)
  expect_identical(fa$sha, fb$sha)
  back <- rf_lib$prevcarga_plugin_thaw(fa$path)
  expect_identical(back, a$art)
  th <- rf_plug()$plugin_thaw(back)
  expect_identical(th, a$art)
  row <- a$feats$table[nrow(a$feats$table)]
  d <- vapply(names(a$model), function(n) {
    x <- as.matrix(row[, a$model[[n]]$features, with = FALSE])
    abs(stats::predict(a$model[[n]]$model, x) - stats::predict(th$payload[[n]]$model, x))
  }, 0)
  expect_identical(max(d), 0)
  ## o hash nao depende de caminho, host nem relogio: refazer o freeze em outro arquivo da o mesmo sha
  fc <- freeze_to(a$art); on.exit(unlink(c(fc$path, paste0(fc$path, ".manifest.json"))), add = TRUE)
  expect_identical(fc$sha, fa$sha)
})

test_that("[PLUGINS] freeze: scan_pure limpo, carimbos presentes, sem trained_at/package_version, train_origin = O-1-H", {
  a <- rf_tr_run(); art <- a$art
  expect_silent(rf_lib$prevcarga_plugin_scan_pure(art))
  expect_s3_class(art, "prevcarga_plugin_artifact")
  s <- art$stamp
  expect_true(all(c("model_id", "split", "granularity_min", "load_label", "timezone", "weather_issue_cutoff", "params",
                    "seed", "train_origin", "contract_version", "variant") %in% names(s)))
  expect_identical(s$model_id, "random-forest"); expect_identical(s$split, "origin_agnostic")
  expect_identical(s$granularity_min, 60L); expect_identical(s$seed, 123L)
  expect_identical(s$params, list(ntree = 5L)); expect_identical(s$timezone, RF_TZ)
  expect_identical(s$weather_issue_cutoff, "available_until"); expect_identical(s$load_label, "start")
  expect_equal(s$train_origin, RF_O - 10L)
  expect_identical(a$cuts$train_origin, s$train_origin)
  pl <- art$payload
  expect_false(any(c("trained_at", "package_version", "created_at", "hostname") %in% c(names(pl), names(s))))
  expect_identical(names(pl), c(sprintf("y_h%03d", 1:216), "feature_cols", "target_cols", "train_origin", "n_train"))
  expect_length(pl$feature_cols, 1486L); expect_length(pl$target_cols, 216L)
  expect_equal(pl$train_origin, RF_O - 10L)
  ## nenhum environment no objeto randomForest (a chamada virou constante)
  expect_false(is.environment(attr(pl$y_h001$model$terms, ".Environment")))
  expect_identical(pl$y_h001$model$call, quote(randomForest()))
})

test_that("[PLUGINS] freeze CONTROL: trained_at = Sys.time() muda o sha256 e e acusado pelo scan; env/closure/caminho tambem", {
  a <- rf_tr_run(); art <- a$art
  bad <- art; bad$payload$trained_at <- Sys.time()
  e <- fail(rf_lib$prevcarga_plugin_scan_pure(bad))
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$id, "plugin.impure_artifact")
  expect_identical(e$class_name, "contract")
  ## sem o scan (prevcarga_freeze cru) o sha mudaria: o relogio entraria nos bytes
  f1 <- tempfile(fileext = ".rds"); f2 <- tempfile(fileext = ".rds")
  on.exit(unlink(c(f1, f2, paste0(c(f1, f2), ".manifest.json"))), add = TRUE)
  expect_false(identical(rf_lib$prevcarga_freeze(art, f1), rf_lib$prevcarga_freeze(bad, f2)))
  expect_identical(fail(rf_lib$prevcarga_plugin_freeze_bytes(bad, tempfile()))$id, "plugin.impure_artifact")
  for (mk in list(function(x) { x$payload$e <- new.env(); x }, function(x) { x$payload$f <- function() 1; x },
                  function(x) { x$payload$p <- "/home/usuario/modelo.rds"; x })) {
    e2 <- fail(rf_lib$prevcarga_plugin_scan_pure(mk(art)))
    expect_identical(e2$id, "plugin.impure_artifact")
  }
})

test_that("[PLUGINS] freeze CONTROL: bytes adulterados => [prevcarga/integrity] exit 9", {
  a <- rf_tr_run(); fz <- freeze_to(a$art)
  on.exit(unlink(c(fz$path, paste0(fz$path, ".manifest.json"))), add = TRUE)
  raw <- readBin(fz$path, "raw", file.size(fz$path))
  raw[length(raw) %/% 2L] <- as.raw(bitwXor(as.integer(raw[length(raw) %/% 2L]), 255L))
  writeBin(raw, fz$path)
  e <- fail(rf_lib$prevcarga_plugin_thaw(fz$path))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "integrity"); expect_identical(e$exit_code, 9L)
  expect_identical(e$id, "plugin.artifact_corrupt")
})

test_that("[PLUGINS] freeze CONTROL: outro model_id ou granularity_min 30 => contract em plugin_thaw; train_origin > cuts => contract", {
  a <- rf_tr_run(); art <- a$art; p <- rf_plug()
  o <- art; o$stamp$model_id <- "lightgbm"
  g <- art; g$stamp$granularity_min <- 30L
  for (x in list(o, g)) {
    e <- fail(p$plugin_thaw(x))
    expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  }
  c0 <- art; class(c0) <- "list"
  expect_identical(fail(p$plugin_thaw(c0))$class_name, "contract")
  m <- art; m$payload$y_h200 <- NULL                          # perde sub-modelo
  expect_identical(fail(p$plugin_thaw(m))$class_name, "contract")
  expect_identical(p$plugin_thaw(art), art)                   # CONTROL: o real passa
  ## train_origin do artefato > cuts$train_origin (O-1-H do infer) => plugin.artifact_mismatch (contract)
  cu <- a$cuts; cu$train_origin <- cu$train_origin - 1L
  e2 <- fail(rf_lib$prevcarga_plugin_check_artifact(art, plugin_ref(), a$cuts$spec, cu))
  expect_identical(e2$class_name, "contract"); expect_identical(e2$id, "plugin.artifact_mismatch")
  expect_identical(e2$details$field, "train_origin")
  expect_true(rf_lib$prevcarga_plugin_check_artifact(art, plugin_ref(), a$cuts$spec, a$cuts))
  ## congelar um modelo com train_origin alem do corte tambem aborta
  e3 <- fail(p$plugin_freeze(a$model, cu, a$cuts$spec))
  expect_identical(e3$class_name, "contract")
})

test_that("[PLUGINS] freeze: rf_freeze/rf_thaw canonicos (model)/(payload); codigo do plugin sem saveRDS/relogio/trained_at", {
  p <- rf_plug()
  expect_identical(names(formals(p$rf_freeze)), "model"); expect_identical(names(formals(p$rf_thaw)), "payload")
  a <- rf_tr_run()
  pl <- p$rf_freeze(a$model)
  expect_identical(p$rf_thaw(pl), pl)
  expect_identical(rf_grep_code(file.path(RF_PLUGIN_DIR(), "R"), "saveRDS|readRDS|RF_ARTIFACT_COMPRESS|trained_at\\s*=|package_version\\s*\\("),
                   character())
})
