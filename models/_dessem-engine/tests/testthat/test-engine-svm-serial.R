## test-engine-svm-serial.R -- DL.2: engine_svm_freeze/thaw como dado puro (AC4 do ticket).

sv_fixture <- function(eng) {
  set.seed(21)
  n <- 150L
  d <- data.table::data.table(LAG_1 = rnorm(n), LAG_7 = rnorm(n), Exo_Temperatura = rnorm(n),
                              F01 = rbinom(n, 1L, 0.5), F02 = rbinom(n, 1L, 0.4))
  d[, Carga := 5 + 2 * LAG_1 + LAG_7 - Exo_Temperatura + 0.1 * rnorm(n)]
  fml <- eng$getFormula(names(d))                  # formula com environment de closure (o caso que o freeze remove)
  tuned <- eng$tune_wsvm(fml, iter.max = 500, weight = rep(1, n), data = d,
                         ranges = list(cost = c(1, 2)), kernel = "linear")
  list(model = tuned$best.model, newdata = d[1:24])
}

test_that("ac-4 freeze e puro (scan_pure limpo, sem call/environment) e thaw prediz identico", {
  eng <- eng_env(eng_load(eng_plugin_root()))
  fx <- sv_fixture(eng)
  m <- fx$model
  expect_true(is.environment(environment(m$terms)) || !is.null(attr(m$terms, ".Environment")))
  fz <- eng$engine_svm_freeze(m)
  expect_true(eng_lib$prevcarga_plugin_scan_pure(fz))
  expect_null(fz[["call"]]); expect_null(fz[["terms"]]); expect_null(fz[["fitted"]])
  expect_type(fz$terms_text, "character")
  expect_length(fz$terms_text, 1L)
  expect_true(grepl("^Carga ~ ", fz$terms_text))
  expect_identical(fz$svm_class, class(m))
  has_env <- function(x) {
    if (is.environment(x) || is.function(x) || typeof(x) == "externalptr") return(TRUE)
    if (is.list(x) && any(vapply(x, has_env, NA))) return(TRUE)
    any(vapply(attributes(x)[setdiff(names(attributes(x)), c("names", "dim", "dimnames", "class"))], has_env, NA))
  }
  expect_false(has_env(fz))
  th <- eng$engine_svm_thaw(fz)
  expect_identical(class(th), class(m))
  expect_identical(predict(th, fx$newdata), predict(m, fx$newdata))
  expect_false(identical(environment(th$terms), globalenv()))
  ## passa pela serializacao de bytes da fundacao (P.0d) e continua identico
  if (!exists("prevcarga_freeze", envir = eng_lib, inherits = FALSE))
    sys.source(file.path(eng_root(), "scripts", "lib", "provenance.R"), envir = eng_lib)
  f <- tempfile(fileext = ".rds")
  art <- structure(list(stamp = list(model_id = "x"), payload = fz), class = "prevcarga_plugin_artifact")
  eng_lib$prevcarga_plugin_freeze_bytes(art, f)
  back <- eng_lib$prevcarga_plugin_thaw(f)
  expect_identical(predict(eng$engine_svm_thaw(back$payload), fx$newdata), predict(m, fx$newdata))
})

test_that("ac-4 duas congelacoes do mesmo modelo tem o mesmo sha256 de serialize; lista de 24 perfis", {
  eng <- eng_env(eng_load(eng_plugin_root()))
  fx <- sv_fixture(eng)
  h <- function(x) {
    f <- tempfile(); on.exit(unlink(f)); writeBin(serialize(x, NULL, version = 3L), f); unname(tools::sha256sum(f))
  }
  expect_identical(h(eng$engine_svm_freeze(fx$model)), h(eng$engine_svm_freeze(fx$model)))
  perfis <- list("2025-06-20" = rep(list(fx$model), 24L))
  fz <- eng$engine_svm_freeze(perfis)
  expect_true(eng_lib$prevcarga_plugin_scan_pure(fz))
  expect_length(fz[[1L]], 24L)
  th <- eng$engine_svm_thaw(fz)
  expect_identical(predict(th[[1L]][[24L]], fx$newdata), predict(fx$model, fx$newdata))
})

test_that("ac-4 CONTROL: SV removido => [prevcarga/integrity] exit 9 engine.dessem_svm_thaw_failed", {
  eng <- eng_env(eng_load(eng_plugin_root()))
  fz <- eng$engine_svm_freeze(sv_fixture(eng)$model)
  fz$SV <- NULL
  assign("prevcarga_abort", eng_lib$prevcarga_abort, envir = globalenv())
  on.exit(rm("prevcarga_abort", envir = globalenv()), add = TRUE)
  cond <- tryCatch(eng$engine_svm_thaw(fz), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "engine.dessem_svm_thaw_failed")
  expect_identical(cond$class_name, "integrity")
  expect_identical(cond$exit_code, 9L)
  expect_true(grepl("[prevcarga/integrity]", conditionMessage(cond), fixed = TRUE))
  bad <- eng$engine_svm_freeze(sv_fixture(eng)$model); bad$svm_class <- "lm"
  expect_error(eng$engine_svm_thaw(bad), "svm_thaw_failed|integrity")
})
