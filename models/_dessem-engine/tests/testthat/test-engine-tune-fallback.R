## test-engine-tune-fallback.R -- E8-T4: fallback sem tune no perfil de dia especial (decisao do Gabriel de 2026-11-17, ADR-046).
## Fixture sintetica: temperatura quase constante (17, 21, 21, 21, ...) com n_eff 4 e 6 faz o tune cair em dobra constante;
## o clamp de cross e a guarda de modelo vazio nao resolvem; antes o motor abortava com engine.dessem_tune_small_sample.

rb_env_tf <- function(eng, nm = ".wsvm_notune_fallback") {
  e <- environment(eng$tune_wsvm)
  while (!exists(nm, envir = e, inherits = FALSE)) e <- parent.env(e)
  e
}

rb_env <- function(eng, nm = ".tune_wsvm_real") {
  e <- environment(eng$tune_wsvm)
  while (!exists(nm, envir = e, inherits = FALSE)) e <- parent.env(e)
  e
}
rb_stub <- function(eng, ...) {
  resp <- list(...); i <- 0L
  assign(".tune_wsvm_real", function(...) {
    i <<- i + 1L
    r <- resp[[min(i, length(resp))]]
    if (is.character(r)) stop(r, call. = FALSE)
    r
  }, envir = rb_env(eng))
  invisible(NULL)
}

tf_engine <- function() eng_env(eng_load(eng_plugin_root()))

tf_collect <- function(expr) {
  got <- list()
  val <- withCallingHandlers(expr, message = function(m) {
    if (inherits(m, "dessem_engine_guard")) { got[[length(got) + 1L]] <<- m; invokeRestart("muffleMessage") }
  })
  list(value = val, guards = got, codes = vapply(got, function(g) g$code, ""))
}

## perfil de feriado minimo: Carga ~ LAG_1 + Exo_Temperatura; serie inteira quase constante (17, 21, 21, ...): a dobra que
## deixa de fora o 17 treina com o alvo constante e o tune devolve modelo vazio ("Model is empty!")
tf_args <- function(n, kernel = "linear") {
  tp <- c(17, rep(21, n - 1L))
  d <- data.frame(Carga = tp, LAG_1 = seq_len(n) * 1.1, Exo_Temperatura = tp)
  list(Carga ~ LAG_1 + Exo_Temperatura, iter.max = 500, weight = rep(1, n), data = d,
       ranges = list(epsilon = c(0.001, 0.01, 0.1), cost = c(1, 10)), kernel = kernel)
}

test_that("fallback: dobra constante com n_eff 4 e 6 entrega modelo sem tune e o warning classificado", {
  eng <- tf_engine()
  for (n in c(4L, 6L)) {
    a <- tf_args(n)
    expect_error(suppressWarnings(do.call(WeightSVM::tune_wsvm, a)), "sampling size")
    r <- tf_collect(suppressWarnings(do.call(eng$tune_wsvm, a)))
    expect_s3_class(r$value, "tune")
    expect_s3_class(r$value$best.model, "wsvm")
    expect_identical(r$codes, "warning.engine_tune_fallback")
    g <- r$guards[[1L]]
    expect_identical(g$details$n_eff, n)
    expect_identical(g$details$cost, 1)
    expect_identical(g$details$epsilon, 0.1)
    expect_false(grepl("[^\\x01-\\x7F]", conditionMessage(g), perl = TRUE))
    ## o modelo e o wsvm direto com os padroes (cost 1, epsilon 0.1) sobre a mesma amostra, e prediz finito
    d <- a$data
    direct <- suppressWarnings(do.call(WeightSVM::wsvm, list(a[[1L]], weight = a$weight, data = d, cost = 1, epsilon = 0.1,
                                                             iter.max = 500, kernel = "linear")))
    expect_identical(eng$engine_svm_freeze(r$value$best.model), eng$engine_svm_freeze(direct))
    expect_true(all(is.finite(predict(r$value$best.model, d))))
  }
})

test_that("fallback: pesos zero saem da amostra como no tune; n_eff e o numero de linhas com peso", {
  eng <- tf_engine()
  a <- tf_args(4L)
  a$data <- rbind(a$data, data.frame(Carga = 0, LAG_1 = 0, Exo_Temperatura = 0)[rep(1L, 4L), ])
  a$weight <- c(rep(1, 4), rep(0, 4))
  r <- tf_collect(suppressWarnings(do.call(eng$tune_wsvm, a)))
  expect_s3_class(r$value, "tune")
  expect_identical(r$guards[[length(r$guards)]]$code, "warning.engine_tune_fallback")
  expect_identical(r$guards[[length(r$guards)]]$details$n_eff, 4L)
  expect_identical(nrow(r$value$best.model$SV) <= 4L, TRUE)
})

test_that("fallback: caminho direto (amostra >= cross) com dobra constante e guarda sem saida tambem entrega com o warning", {
  eng <- tf_engine()
  a <- tf_args(12L)
  msg <- tryCatch({ suppressWarnings(do.call(WeightSVM::tune_wsvm, a)); NA_character_ }, error = function(e) conditionMessage(e))
  expect_true(grepl("empty|constant|scale|Model fitting fails", msg, ignore.case = TRUE))
  r <- tf_collect(suppressWarnings(do.call(eng$tune_wsvm, a)))
  expect_s3_class(r$value, "tune")
  expect_identical(r$codes, "warning.engine_tune_fallback")
})

test_that("fallback com as mensagens de campo: 'Model is empty!' e dobra constante, guarda encadeada sem saida (stub do tune real)", {
  for (m in c("Model is empty!", "Variable(s) 'Exo_Temperatura' constant. Cannot scale data.")) {
    eng <- tf_engine()
    rb_stub(eng, "sampling size is greater", m)     # clamp -> retry falha com `m` -> guarda tambem (mesmo stub) -> fallback
    r <- tf_collect(do.call(eng$tune_wsvm, tf_args(5L)))
    expect_s3_class(r$value, "tune")
    expect_identical(r$codes, c("warning.engine_cross_clamped", "warning.engine_tune_fallback"))
    expect_identical(r$guards[[2L]]$details$n_eff, 5L)
    expect_true(grepl(substr(m, 1L, 8L), r$guards[[2L]]$details$reason, fixed = TRUE))
    ## caminho direto (sem `sampling size`): mesma entrega
    rb_stub(eng, m)
    r2 <- tf_collect(do.call(eng$tune_wsvm, tf_args(5L)))
    expect_identical(r2$codes, "warning.engine_tune_fallback")
  }
})

test_that("CONTROL: sem o fallback a mesma fixture aborta como antes (small_sample no clamp, erro cru no caminho direto)", {
  eng <- tf_engine()
  assign(".wsvm_notune_fallback", function(...) NULL, envir = rb_env_tf(eng))
  e <- tryCatch(suppressWarnings(do.call(eng$tune_wsvm, tf_args(4L))), error = function(e) e)
  expect_match(conditionMessage(e), "engine.dessem_tune_small_sample", fixed = TRUE)
  e2 <- tryCatch(suppressWarnings(do.call(eng$tune_wsvm, tf_args(12L))), error = function(e) e)
  expect_true(inherits(e2, "error"))
  expect_false(grepl("engine.dessem_tune_small_sample", conditionMessage(e2), fixed = TRUE))
})

test_that("fallback: se o treino sem tune tambem nao resolve (sem pesos) o aborto original permanece", {
  eng <- tf_engine()
  a <- tf_args(4L); a$weight <- NULL
  e <- tryCatch(suppressWarnings(do.call(eng$tune_wsvm, a)), error = function(e) e)
  expect_true(inherits(e, "error"))
  expect_false(inherits(e, "tune"))
  a <- tf_args(4L); a$weight <- c(1, 1, 1)   # tamanho errado: o fallback devolve NULL e o erro original segue
  e <- tryCatch(suppressWarnings(do.call(eng$tune_wsvm, a)), error = function(e) e)
  expect_true(inherits(e, "error"))
})

test_that("caminho normal inalterado: identical() ao WeightSVM::tune_wsvm direto e nenhuma guarda emitida", {
  eng <- tf_engine()
  set.seed(11); n <- 120L
  d <- data.frame(LAG_1 = rnorm(n), LAG_7 = rnorm(n), Exo_Temperatura = rnorm(n), F01 = rbinom(n, 1L, 0.5))
  d$Carga <- 5 + 2 * d$LAG_1 + d$LAG_7 - d$Exo_Temperatura + 0.1 * rnorm(n)
  a <- list(Carga ~ LAG_1 + LAG_7 + Exo_Temperatura + F01, iter.max = 500, weight = rep(1, n), data = d,
            ranges = list(epsilon = c(0.001, 0.01, 0.1), cost = c(1, 10)), kernel = "radial")
  set.seed(7); w <- tf_collect(do.call(eng$tune_wsvm, a))
  set.seed(7); direct <- do.call(WeightSVM::tune_wsvm, a)
  expect_length(w$guards, 0L)
  ## `call` e so o registro da chamada (o wrapper repassa `...`); o resto do objeto e identico bit a bit
  strip <- function(x) { x$best.model$call <- NULL; x }
  expect_identical(strip(w$value), strip(direct))
  expect_identical(eng$engine_svm_freeze(w$value$best.model), eng$engine_svm_freeze(direct$best.model))
  ## o caminho da guarda de cross (resolvido ANTES do fallback) tambem segue identico ao do real com cross clampado
  d2 <- data.frame(Carga = c(1.5, 2.1, 3.3, 3.9, 5.2, 0, 0, 0), LAG_1 = c(1, 2, 3, 4, 5, 0, 0, 0))
  a2 <- list(Carga ~ LAG_1, iter.max = 500, weight = c(rep(1, 5), 0, 0, 0), data = d2, ranges = list(cost = 1), kernel = "linear")
  set.seed(3); w2 <- tf_collect(do.call(eng$tune_wsvm, a2))
  expect_identical(w2$codes, "warning.engine_cross_clamped")
  a2c <- a2; a2c$tunecontrol <- WeightSVM::tune.control(cross = 5L)
  set.seed(3); expect_error(do.call(WeightSVM::tune_wsvm, a2), "sampling size")
  set.seed(3); direct2 <- do.call(WeightSVM::tune_wsvm, a2c)
  expect_identical(eng$engine_svm_freeze(w2$value$best.model), eng$engine_svm_freeze(direct2$best.model))
})

test_that("celula: .engine_with_cell enriquece so o fallback; as outras guardas passam intactas", {
  eng <- tf_engine()
  r <- tf_collect(eng$.engine_with_cell("data=2024-06-20 hora=13 tipo_dia_especial=5",
                                        suppressWarnings(do.call(eng$tune_wsvm, tf_args(4L)))))
  expect_identical(r$codes, "warning.engine_tune_fallback")
  expect_identical(r$guards[[1L]]$details$cell, "data=2024-06-20 hora=13 tipo_dia_especial=5")
  expect_true(grepl("celula: data=2024-06-20 hora=13", conditionMessage(r$guards[[1L]]), fixed = TRUE))
  ## outra guarda (cross clampado que resolve) passa intacta, sem `cell`
  d2 <- data.frame(Carga = c(1.5, 2.1, 3.3, 3.9, 5.2), LAG_1 = c(1, 2, 3, 4, 5))
  r2 <- tf_collect(eng$.engine_with_cell("x", do.call(eng$tune_wsvm, list(Carga ~ LAG_1, iter.max = 500, weight = rep(1, 5),
                                                                          data = d2, ranges = list(cost = 1), kernel = "linear"))))
  expect_identical(r2$codes, "warning.engine_cross_clamped")
  expect_null(r2$guards[[1L]]$details$cell)
})

test_that("defaults do fallback: sao os do wsvm e pertencem aos ranges de todas as chaves de model_parameters.json", {
  eng <- tf_engine()
  def <- eng$.wsvm_notune_defaults()
  fm <- formals(WeightSVM:::wsvm.default)
  expect_identical(def$cost, fm$cost)
  expect_identical(def$epsilon, fm$epsilon)
  mp <- jsonlite::read_json(file.path(eng_dir(), "model_parameters.json"), simplifyVector = TRUE)
  for (k in names(mp)) {
    rg <- mp[[k]]$ranges
    if (is.null(rg)) next
    expect_true(def$cost %in% rg$cost, info = k)
    expect_true(def$epsilon %in% rg$epsilon, info = k)
  }
})

test_that("catalogo: warning.engine_tune_fallback tem linha ASCII com classe contract e dica", {
  f <- file.path(eng_root(), "scripts", "lib", "errors", "dessem_engine.tsv")
  l <- grep("^warning.engine_tune_fallback\t", readLines(f, warn = FALSE), value = TRUE)
  expect_length(l, 1L)
  p <- strsplit(l, "\t")[[1L]]
  expect_length(p, 4L)
  expect_identical(p[2L], "contract")
  expect_true(all(nzchar(p)))
  expect_false(grepl("[^\x01-\x7F]", l, perl = TRUE))
})
