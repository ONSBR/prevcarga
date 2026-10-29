## test-engine-robustez.R -- DL.2: robustez do tune_wsvm com escopo no env do motor (AC1, AC2, AC6 do ticket).

rb_engine <- function() eng_env(eng_load(eng_plugin_root()))

## serie sintetica deterministica (semente fixa); dummies com minoria > 30; Carga ~ LAG_1 + LAG_7 + Exo_Temperatura
rb_serie <- function(n = 200L, rare = FALSE) {
  set.seed(11)
  d <- data.table::data.table(Data = lubridate::ymd("2020-01-01") + seq_len(n) - 1L,
                              LAG_1 = rnorm(n), LAG_7 = rnorm(n), Exo_Temperatura = rnorm(n),
                              F01 = rbinom(n, 1L, 0.5), F02 = rbinom(n, 1L, 0.4))
  d[, Carga := 5 + 2 * LAG_1 + LAG_7 - Exo_Temperatura + 0.1 * rnorm(n)]
  d[, HV := 0L]
  d
}

## coleta as condicoes dessem_engine_guard de uma expressao
rb_collect <- function(expr) {
  got <- list()
  val <- withCallingHandlers(expr, message = function(m) {
    if (inherits(m, "dessem_engine_guard")) { got[[length(got) + 1L]] <<- m; invokeRestart("muffleMessage") }
  })
  list(value = val, guards = got)
}

## o frozen e a forma comparavel: sem `call` nem environment (o `call` do best.model embute os argumentos)
rb_pure <- function(eng, tuned) eng$engine_svm_freeze(tuned$best.model)

test_that("ac-1 SVM_DemandaMedia e SVMhorario: wrapper == WeightSVM::tune_wsvm direto e nenhuma guarda emitida", {
  eng <- rb_engine()
  d <- rb_serie()
  param <- list(alfa = 0.1, ranges = list(cost = c(1, 2)), kernel = "linear", nomModelo = "fix",
                colunaRemoverPerfil = 1L, funcDadosPerfil = "fixEntrada")
  fml <- Carga ~ LAG_1 + LAG_7 + Exo_Temperatura + F01 + F02
  ## stubs no env do motor da instancia de teste (so as entradas; o SVM e o do motor)
  assign("obterEntradaFormulaDM", function(dataPrev, serie, param_entrada)
    list(formulaMDL = fml, dadosEntrada = list(serieDiariaModelo = d)), envir = eng)
  assign("obterDiaEspPerfilConfig", function(dataPrev, serie, param_entrada)
    list(tpEntrada = 1, ds = 1, diasAno = 1, hrlag = 1, lagds = 1, diaEsp = 0), envir = eng)
  assign("fixEntrada", function(...) d[, .(dummy = 0L, Carga, LAG_1, LAG_7, Exo_Temperatura, F01, F02)], envir = eng)
  serie <- data.table::data.table(Data = d$Data[1L], HV = 0L)

  ## caminho direto: o mesmo corpo com WeightSVM::tune_wsvm no lugar do wrapper
  direct_env <- new.env(parent = eng)
  assign("tune_wsvm", WeightSVM::tune_wsvm, envir = direct_env)
  for (fn in c("SVM_DemandaMedia", "SVMhorario")) {
    f <- get(fn, envir = eng); environment(f) <- direct_env
    assign(fn, f, envir = direct_env)
  }
  expect_false(identical(eng$tune_wsvm, WeightSVM::tune_wsvm))

  w1 <- rb_collect(eng$SVM_DemandaMedia(d$Data[1L], serie, param))
  d1 <- direct_env$SVM_DemandaMedia(d$Data[1L], serie, param)
  expect_length(w1$guards, 0L)
  expect_identical(eng$engine_svm_freeze(w1$value$modeloTreinado), eng$engine_svm_freeze(d1$modeloTreinado))
  expect_identical(predict(w1$value$modeloTreinado, d[1:24]), predict(d1$modeloTreinado, d[1:24]))

  w2 <- rb_collect(eng$SVMhorario(d$Data[1L], 1L, serie, param))
  d2 <- direct_env$SVMhorario(d$Data[1L], 1L, serie, param)
  expect_length(w2$guards, 0L)
  expect_identical(rb_pure(eng, w2$value$modeloTreinado), rb_pure(eng, d2$modeloTreinado))
  expect_identical(predict(w2$value$modeloTreinado$best.model, d[1:24]), predict(d2$modeloTreinado$best.model, d[1:24]))
})

test_that("ac-1 CONTROL: dummy de minoria 1 => direto falha 'Model is empty' e o wrapper conclui com a guarda", {
  eng <- rb_engine()
  set.seed(3)
  n <- 60L
  d <- data.frame(Carga = rnorm(n) * 0.01, LAG_1 = rnorm(n), F11 = 0)
  d$F11[5L] <- 1; d$Carga[5L] <- 50
  w <- rep(1, n)
  args <- list(Carga ~ LAG_1 + F11, iter.max = 500, weight = w, data = d, ranges = list(cost = c(1, 2)),
               kernel = "linear", pre.check = FALSE)
  msg <- tryCatch({ suppressWarnings(do.call(WeightSVM::tune_wsvm, args)); NA_character_ },
                  error = function(e) conditionMessage(e))
  expect_true(grepl("Model is empty", msg, fixed = TRUE))
  r <- rb_collect(suppressWarnings(do.call(eng$tune_wsvm, args)))
  expect_s3_class(r$value, "tune")
  expect_length(r$guards, 1L)
  g <- r$guards[[1L]]
  expect_s3_class(g, "message")
  expect_identical(g$code, "warning.engine_empty_model_guard")
  expect_identical(g$details$dropped, "F11")
  expect_identical(g$details$k, 3)
  expect_identical(attr(r$value$best.model$terms, "term.labels"), "LAG_1")
  expect_false(grepl("[^\\x01-\\x7F]", conditionMessage(g), perl = TRUE))
})

test_that("ac-1 outro erro do tune_wsvm e re-propagado inalterado", {
  eng <- rb_engine()
  expect_error(eng$tune_wsvm(Carga ~ x, data = data.frame(Carga = 1:3, x = 1:3), weight = 1:2, kernel = "linear"),
               "length of weight")
})

test_that("ac-2 cross > amostra: clampa em min(10, sum(w>0)) = 5 e emite a guarda; CONTROL 1 linha re-propaga", {
  eng <- rb_engine()
  set.seed(5)
  d <- data.frame(Carga = rnorm(10), LAG_1 = rnorm(10))
  d$Carga <- d$Carga + d$LAG_1
  w <- c(rep(1, 5), rep(0, 5))
  args <- list(Carga ~ LAG_1, iter.max = 500, weight = w, data = d, ranges = list(cost = 1), kernel = "linear")
  expect_error(do.call(WeightSVM::tune_wsvm, args), "sampling size")
  r <- rb_collect(do.call(eng$tune_wsvm, args))
  expect_s3_class(r$value, "tune")
  expect_length(r$guards, 1L)
  g <- r$guards[[1L]]
  expect_identical(g$code, "warning.engine_cross_clamped")
  expect_identical(g$details$from, 10L)
  expect_identical(g$details$to, 5L)
  expect_identical(g$details$n_eff, 5L)
  expect_identical(r$value$best.parameters$cost, 1)
  ## CONTROL: 1 linha com peso > 0 => new_cross < 2 => erro original, sem fallback silencioso
  args$weight <- c(1, rep(0, 9))
  expect_error(do.call(eng$tune_wsvm, args), "sampling size")
})

test_that("ac-6 env do motor tem o wrapper e o globalenv nao ganha tune_wsvm nem as guardas", {
  snap <- eng_globalenv_snapshot()
  eng <- rb_engine()
  expect_true(is.function(eng$tune_wsvm))
  expect_false(identical(eng$tune_wsvm, WeightSVM::tune_wsvm))
  expect_false(any(c("tune_wsvm", ".tune_wsvm_real", ".wsvm_guard_retry", ".wsvm_clamp_cross") %in% ls(globalenv(), all.names = TRUE)))
  expect_identical(eng_globalenv_snapshot(), snap)
  ## SVM_DemandaMedia/SVMhorario resolvem tune_wsvm no env do motor
  expect_identical(environment(eng$SVM_DemandaMedia), eng)
  expect_identical(environment(eng$tune_wsvm), eng)
})
