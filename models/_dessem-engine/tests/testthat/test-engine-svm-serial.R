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

## DL.4: o artefato de um dia do plugin = 1 SVM de demanda media + 24 SVMs de perfil, todos DISTINTOS (dado e semente diferentes)
sv_day <- function(eng) {
  mk <- function(seed, h) {
    set.seed(seed); n <- 120L
    d <- data.table::data.table(LAG_1 = rnorm(n), LAG_7 = rnorm(n), Exo_Temperatura = rnorm(n), F01 = rbinom(n, 1L, 0.5))
    d[, Carga := 5 + (1 + h / 24) * LAG_1 + LAG_7 - Exo_Temperatura + 0.1 * rnorm(n)]
    tn <- eng$tune_wsvm(eng$getFormula(names(d)), iter.max = 500, weight = rep(1, n), data = d,
                        ranges = list(cost = c(1, 2)), kernel = "linear")
    list(model = tn$best.model, newdata = d[1:24])
  }
  list(dm = mk(100L, 0L), perfil = lapply(0:23, function(h) mk(200L + h, h)))
}

test_that("ac-4 1 DM + 24 perfis: freeze puro (scan_pure limpo), sha256 de serialize estavel em 2 execucoes, predict identico apos thaw", {
  eng <- eng_env(eng_load(eng_plugin_root()))
  day <- sv_day(eng)
  h <- function(x) {
    f <- tempfile(); on.exit(unlink(f)); writeBin(serialize(x, NULL, version = 3L), f); unname(tools::sha256sum(f))
  }
  frz <- function() eng$engine_svm_freeze(list(dm = day$dm$model, perfil = lapply(day$perfil, `[[`, "model")))
  fz1 <- frz(); fz2 <- frz()
  expect_true(eng_lib$prevcarga_plugin_scan_pure(fz1))
  expect_length(fz1$perfil, 24L)
  expect_identical(h(fz1), h(fz2))                                   # sha256 estavel
  ## CONTROL: modelos distintos => sha256 distintos (o hash enxerga o conteudo)
  expect_false(identical(h(eng$engine_svm_freeze(day$perfil[[1L]]$model)), h(eng$engine_svm_freeze(day$perfil[[24L]]$model))))
  th <- eng$engine_svm_thaw(fz1)
  expect_identical(predict(th$dm, day$dm$newdata), predict(day$dm$model, day$dm$newdata))
  for (k in 1:24) expect_identical(predict(th$perfil[[k]], day$perfil[[k]]$newdata), predict(day$perfil[[k]]$model, day$perfil[[k]]$newdata), info = k)
  ## descongelar e congelar de novo e ponto fixo (mesmo sha256)
  expect_identical(h(eng$engine_svm_freeze(th)), h(fz1))
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
