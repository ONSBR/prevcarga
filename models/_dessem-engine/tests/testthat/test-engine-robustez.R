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
  expect_error(do.call(eng$tune_wsvm, args), "engine.dessem_tune_small_sample")
})

## stub deterministico do tune real: sequencia de respostas (erro de mensagem ou valor)
rb_env <- function(eng, nm = ".tune_wsvm_real") {
  e <- environment(eng$tune_wsvm)
  while (!exists(nm, envir = e, inherits = FALSE)) e <- parent.env(e)
  e
}
rb_stub <- function(eng, ...) {
  resp <- list(...); i <- 0L
  f <- function(...) {
    i <<- i + 1L
    r <- resp[[min(i, length(resp))]]
    if (is.character(r)) stop(r, call. = FALSE)
    r
  }
  assign(".tune_wsvm_real", f, envir = rb_env(eng))
  invisible(NULL)
}
rb_args5 <- function() {
  d <- data.frame(Carga = c(1, 2, 3, 4, 5, 0, 0), LAG_1 = c(1, 2, 3, 4, 5, 0, 0))
  list(Carga ~ LAG_1, weight = c(rep(1, 5), 0, 0), data = d, kernel = "linear")
}
rb_guard_codes <- function(r) vapply(r$guards, function(g) g$code, "")

test_that("clamp_encadeado: sampling size -> empty no cross clampado -> guarda recupera, sem generic.unexpected", {
  eng <- rb_engine()
  rb_stub(eng, "sampling size is greater", "Model is empty!", "ok_guard")
  g <- function(.args, msg) {
    message(dessem_engine_guard("warning.engine_empty_model_guard", dropped = "F11", k = 3))
    "modelo_ok"
  }
  environment(g) <- eng
  assign(".wsvm_guard_retry", g, envir = rb_env(eng))
  r <- rb_collect(do.call(eng$tune_wsvm, rb_args5()))
  expect_identical(r$value, "modelo_ok")
  expect_identical(rb_guard_codes(r), c("warning.engine_cross_clamped", "warning.engine_empty_model_guard"))
  expect_identical(r$guards[[1L]]$details$from, 10L)
  expect_identical(r$guards[[1L]]$details$to, 5L)
})

test_that("clamp_encadeado: guarda falhando, n_eff = 1 e outra classe de erro no retry abortam small_sample", {
  eng <- rb_engine()
  env <- rb_env(eng)
  ## guarda tambem falha
  rb_stub(eng, "sampling size", "Model is empty!")
  assign(".wsvm_guard_retry", function(.args, msg) stop("Model is empty!", call. = FALSE), envir = env)
  e <- tryCatch(do.call(eng$tune_wsvm, rb_args5()), error = function(e) e)
  expect_match(conditionMessage(e), "engine.dessem_tune_small_sample", fixed = TRUE)
  ## n_eff = 1 (new_cross < 2)
  rb_stub(eng, "sampling size")
  a <- rb_args5(); a$weight <- c(1, rep(0, 6))
  e <- tryCatch(do.call(eng$tune_wsvm, a), error = function(e) e)
  expect_match(conditionMessage(e), "engine.dessem_tune_small_sample", fixed = TRUE)
  ## retry com outra classe de erro
  rb_stub(eng, "sampling size", "length of weight")
  e <- tryCatch(do.call(eng$tune_wsvm, rb_args5()), error = function(e) e)
  expect_match(conditionMessage(e), "engine.dessem_tune_small_sample", fixed = TRUE)
})

test_that("clamp_encadeado: .wsvm_small_sample passa details e retry_message ASCII ao abort", {
  eng <- rb_engine()
  env <- rb_env(eng)
  got <- NULL
  assign("engine_abort", function(id, where = NULL, details = NULL) { got <<- list(id = id, d = details); stop("abort") }, envir = env)
  try(env$.wsvm_small_sample(1, 10, 1, "erro \u00e7 acentuado"), silent = TRUE)
  expect_identical(got$id, "engine.dessem_tune_small_sample")
  expect_identical(names(got$d), c("n_eff", "cross_from", "cross_to", "retry_message"))
  expect_identical(got$d$n_eff, 1L)
  expect_false(grepl("[^\\x01-\\x7F]", got$d$retry_message, perl = TRUE))
})

test_that("clamp_encadeado: catalogo tem a linha ASCII com classe window", {
  f <- file.path(eng_root(), "scripts", "lib", "errors", "dessem_engine.tsv")
  l <- grep("^engine.dessem_tune_small_sample\t", readLines(f, warn = FALSE), value = TRUE)
  expect_length(l, 1L)
  expect_identical(strsplit(l, "\t")[[1L]][2L], "window")
  expect_false(grepl("[^\x01-\x7F]", l, perl = TRUE))
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

## DL.4 (AC4 do ticket): os quatro fixtures de robustez completam emitindo a condicao esperada, e os kill-switches de ambiente do
## oraculo (DESSEM_EXOG_TAIL_FILL=0 / DESSEM_NORMALIZE_CODES=0) NAO existem no motor: o resultado e identico com eles no ambiente.
rb_four_cases <- function(eng) {
  msgs <- function(expr) { m <- character(); v <- withCallingHandlers(expr, message = function(c) {
    m <<- c(m, if (inherits(c, "dessem_engine_guard")) c$code else conditionMessage(c)); invokeRestart("muffleMessage") }); list(value = v, msgs = m) }
  ## (1) dummy rara => modelo-vazio contornado
  set.seed(3); n <- 60L
  d <- data.frame(Carga = rnorm(n) * 0.01, LAG_1 = rnorm(n), F11 = 0); d$F11[5L] <- 1; d$Carga[5L] <- 50
  a1 <- list(Carga ~ LAG_1 + F11, iter.max = 500, weight = rep(1, n), data = d, ranges = list(cost = c(1, 2)),
             kernel = "linear", pre.check = FALSE)
  r1 <- msgs(suppressWarnings(do.call(eng$tune_wsvm, a1)))
  ## (2) perfil de dia especial com 5 linhas de peso > 0 => cross clampado
  set.seed(5); d2 <- data.frame(Carga = rnorm(10), LAG_1 = rnorm(10)); d2$Carga <- d2$Carga + d2$LAG_1
  r2 <- msgs(do.call(eng$tune_wsvm, list(Carga ~ LAG_1, iter.max = 500, weight = c(rep(1, 5), rep(0, 5)), data = d2,
                                         ranges = list(cost = 1), kernel = "linear")))
  ## (3) codigo 12 no horizonte
  fer <- data.table::data.table(Data = lubridate::ymd(c("2024-12-22", "2024-12-25")), Tipo = c(12L, 6L))
  r3 <- msgs(eng$normalize_special_day_codes(fer, "Tipo", 0:11, "SP"))
  ## (4) EXOGENAPREV com 2 dias curtos
  O <- lubridate::ymd("2025-06-20"); t0 <- lubridate::force_tz(lubridate::as_datetime(O), "Etc/GMT+3")
  tt <- do.call(c, lapply(0:8, function(i) t0 + lubridate::days(i) + lubridate::hours(seq_len(if (i %in% 7:8) 5L else 24L) - 1L)))
  prev <- data.table::data.table(DataHora = tt, Exo_Temperatura = as.numeric(seq_along(tt)) %% 17 + 20)
  r4 <- msgs(eng$fill_forecast_tail(prev, "DataHora", O, O + 8, 24L, "SP", O, max_fill_days = 2L))
  list(empty = list(msgs = r1$msgs, class = class(r1$value), dropped = attr(r1$value$best.model$terms, "term.labels")),
       cross = list(msgs = r2$msgs, cost = r2$value$best.parameters$cost),
       flag12 = list(msgs = r3$msgs, tipo = r3$value$Tipo),
       tail = list(msgs = r4$msgs, n = nrow(r4$value), finite = all(is.finite(r4$value$Exo_Temperatura))))
}

test_that("ac-4 modelo-vazio, clamp de cross, flag-12 e cauda curta completam com a condicao esperada; kill-switches de ENV sem efeito", {
  eng <- rb_engine()
  clean <- rb_four_cases(eng)
  expect_identical(clean$empty$msgs, "warning.engine_empty_model_guard")
  expect_true("tune" %in% clean$empty$class)
  expect_identical(clean$empty$dropped, "LAG_1")
  expect_identical(clean$cross$msgs, "warning.engine_cross_clamped")
  expect_true(any(grepl("nao-modelado (12)", clean$flag12$msgs, fixed = TRUE)))
  expect_identical(clean$flag12$tipo, c(0L, 6L))
  expect_identical(clean$tail$n, 216L)
  expect_true(clean$tail$finite)
  expect_length(clean$tail$msgs, 2L)
  expect_true(all(grepl("persistido do ultimo dia completo", clean$tail$msgs, fixed = TRUE)))
  withr::local_envvar(DESSEM_EXOG_TAIL_FILL = "0", DESSEM_NORMALIZE_CODES = "0")
  expect_identical(Sys.getenv("DESSEM_EXOG_TAIL_FILL"), "0")
  expect_identical(rb_four_cases(eng), clean)
})
