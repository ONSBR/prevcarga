## test-feature-engineering.R -- gates [PLUGINS] fe_* do feature engineering do Holt-Winters (HW.2; AC 1-3 e 5-7).
## Espioes: o plugin e clonado num env filho e as funcoes do oraculo sao trocadas por envoltorios que registram a
## chamada (equivalente funcional do trace()); o plugin real nunca e alterado.

## fixture: deck do plugin_ingest sobre o store sintetico (P.0e); 60 dias de historia => 59 linhas na CargaTempVer
fe_deck <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      env <- hw_env()
      st <- hw_fx_store()
      cuts <- hw_fx_cuts(st)
      cache <<- list(deck = env$plugin_ingest(hw_fx_panel(st, cuts, history_days = 60L), cuts), cuts = cuts)
    }
    cache
  }
})

## dias atipicos injetados em 6 dias uteis: carga x0,5 COM forma distorcida. So a escala nao basta: o MO1 normaliza cada
## dia pela propria media (x / mean(x)), logo x0,5 puro e invisivel ao cluster. A distorcao e deterministica (seno), sem RNG.
fe_atipico <- function(deck, rows = c(12L, 19L, 26L, 33L, 40L, 47L), k = 0.5) {
  H <- hw_env()$hw_slot_cols()
  for (r in rows) {
    v <- as.numeric(deck$CargaTempVer[r, H])
    deck$CargaTempVer[r, H] <- k * v * exp(0.4 * sin((1:48)^2 * (r + 0.7)))
  }
  deck
}

## clone do env do plugin com envoltorios de registro; `wrap` = nomes das funcoes a espiar (alem de set.seed).
fe_spy_env <- function(env, log, wrap) {
  e2 <- new.env(parent = parent.env(env))
  for (n in ls(env, all.names = TRUE)) assign(n, get(n, envir = env), envir = e2)
  for (n in ls(e2, all.names = TRUE)) {
    f <- get(n, envir = e2)
    if (is.function(f)) environment(f) <- e2
    assign(n, f, envir = e2)
  }
  for (n in wrap) local({
    nm <- n
    orig <- get(nm, envir = e2)
    assign(nm, function(...) {
      idx <- length(log$calls) + 1L
      log$calls[[idx]] <- list(name = nm, args = list(...))
      out <- orig(...)
      log$out[[idx]] <- out
      out
    }, envir = e2)
  })
  assign("set.seed", function(seed, ...) {
    log$calls[[length(log$calls) + 1L]] <- list(name = "set.seed", args = list(seed = seed))
    base::set.seed(seed, ...)
  }, envir = e2)
  e2
}

fe_new_log <- function() { l <- new.env(); l$calls <- list(); l$out <- list(); l }
fe_names <- function(log) vapply(log$calls, function(x) x$name, "")

FE_STEPS <- c("Corrige.Dias.Faltantes", ".preconsist_ctm", "Corrige.Dados", "MO1", "Substitui.Feriados",
              "Suaviza.LOESS", ".hw_recompute_ctm")

test_that("[PLUGINS] fe_ordem: os 7 passos na ordem do oraculo; snapshots pos-MO1; CONTROL feriado false", {
  env <- hw_env(); fx <- fe_deck(); spec <- hw_spec()
  expect_true(isTRUE(spec$params$feriado))
  log <- fe_new_log()
  e2 <- fe_spy_env(env, log, FE_STEPS)
  f <- suppressMessages(e2$plugin_build_features(fx$deck, spec))
  nm <- fe_names(log)
  expect_identical(nm[nm != "set.seed"],
                   c("Corrige.Dias.Faltantes", "Corrige.Dias.Faltantes", ".preconsist_ctm", "Corrige.Dados", "MO1",
                     "Substitui.Feriados", "Suaviza.LOESS", ".hw_recompute_ctm"))
  ## Corrige.Dados(p_ncol = 52, p_Negativos = TRUE, p_TrocaDias = FALSE, p_TabelaCargaMedia = FALSE)
  cd <- log$calls[[which(nm == "Corrige.Dados")]]$args
  expect_identical(cd$p_ncol, 52L); expect_true(cd$p_Negativos)
  expect_false(cd$p_TrocaDias); expect_false(cd$p_TabelaCargaMedia)
  ## LOESS: span 0,3 do plugin_constants
  expect_identical(log$calls[[which(nm == "Suaviza.LOESS")]]$args$p_span, 0.3)
  ## snapshots == estado logo apos o MO1 / pre-consistencia, antes da substituicao de feriados
  i_mo <- which(nm == "MO1")
  expect_identical(f$CargaTempVerFer, log$out[[i_mo]][[1L]])
  expect_identical(f$CargaTempMed1, log$out[[which(nm == ".preconsist_ctm")]])
  ## a substituicao de feriados tem efeito: O-5 e feriado do fixture (Feriado 1)
  H <- env$hw_slot_cols()
  expect_false(identical(f$CargaTempVer[, H], f$CargaTempVerFer[, H]))
  ## CONTROL: feriado false => Substitui.Feriados nao e chamado e H01..H48 == os do snapshot
  sp2 <- spec; sp2$params$feriado <- FALSE
  log2 <- fe_new_log()
  e3 <- fe_spy_env(env, log2, FE_STEPS)
  g <- suppressMessages(e3$plugin_build_features(fx$deck, sp2))
  expect_false("Substitui.Feriados" %in% fe_names(log2))
  expect_identical(g$CargaTempVer[, H], g$CargaTempVerFer[, H])
  ## CargaTempMed recomputado da CargaTempVer limpa
  expect_equal(f$CargaTempMed$Cmed, unname(rowMeans(f$CargaTempVer[, H])))
  expect_equal(f$CargaTempMed$Tmed, unname(rowMeans(f$CargaTempVer[, env$hw_temp_cols()])))
})

test_that("[PLUGINS] fe_sementes: set.seed(spec$seed) antes do MO1, p_seed = kmeans_seed; CONTROL kmeans_seed 7", {
  env <- hw_env(); fx <- fe_deck(); spec <- hw_spec()
  deck <- fe_atipico(fx$deck)
  a <- suppressMessages(env$plugin_build_features(deck, spec))
  b <- suppressMessages(env$plugin_build_features(deck, spec))
  expect_identical(a, b)
  ## espiao: set.seed(1234) imediatamente antes do MO1 e classifica_kmeans com p_seed = 123
  log <- fe_new_log()
  e2 <- fe_spy_env(env, log, c("MO1", "classifica_kmeans"))
  suppressMessages(e2$plugin_build_features(deck, spec))
  nm <- fe_names(log)
  i <- which(nm == "MO1")
  expect_length(i, 1L)
  expect_identical(nm[[i - 1L]], "set.seed")
  expect_identical(log$calls[[i - 1L]]$args$seed, spec$seed)
  expect_identical(log$calls[[i]]$args$p_kmeans_seed, spec$params$kmeans_seed)
  k <- which(nm == "classifica_kmeans")
  expect_length(k, 1L)
  expect_identical(log$calls[[k]]$args$p_seed, spec$params$kmeans_seed)
  expect_identical(log$calls[[k + 1L]]$args$seed, spec$params$kmeans_seed)
  ## CONTROL com dente: kmeans_seed 123 -> 7 muda a saida em pelo menos um dia imputado
  sp7 <- spec; sp7$params$kmeans_seed <- 7L
  c7 <- suppressMessages(env$plugin_build_features(deck, sp7))
  H <- env$hw_slot_cols()
  expect_gt(max(abs(as.matrix(a$CargaTempVer[, H]) - as.matrix(c7$CargaTempVer[, H]))), 1e-5)
  ## o dispatcher P.0c entrega so spec$params: mesma saida (o kmeans re-semeia o gerador)
  d <- suppressMessages(env$plugin_build_features(deck, spec$params))
  expect_identical(a, d)
  ## nenhum set.seed(<literal>) no plugin
  rdir <- file.path(hw_root(), "models", "holt-winters", "R")
  expect_identical(hw_grep_code(rdir, "set\\.seed\\([0-9]"), character())
  ## CONTROL do grep: um set.seed literal plantado e apanhado
  tmp <- tempfile("hw-grep-"); dir.create(tmp)
  writeLines("f <- function() set.seed(123)", file.path(tmp, "x.R"))
  expect_length(hw_grep_code(tmp, "set\\.seed\\([0-9]"), 1L)
})

test_that("[PLUGINS] fe_params: param ausente ou invalido aborta contract nomeando o campo, antes de qualquer passo", {
  env <- hw_env(); fx <- fe_deck(); spec <- hw_spec()
  chk <- function(mut, field) {
    sp <- spec; sp$params <- mut(sp$params)
    log <- fe_new_log()
    e2 <- fe_spy_env(env, log, FE_STEPS)
    cnd <- hw_catch(suppressMessages(e2$plugin_build_features(fx$deck, sp)))
    expect_false(is.null(cnd), info = field)
    expect_match(conditionMessage(cnd), field, fixed = TRUE)
    expect_length(log$calls, 0L)
  }
  chk(function(p) { p$ic <- NULL; p }, "params.ic")
  chk(function(p) { p$perfil_mo <- "X"; p }, "params.perfil_mo")
  chk(function(p) { p$similar <- 1.5; p }, "params.similar")
  chk(function(p) { p$similar <- 0; p }, "params.similar")
  chk(function(p) { p$classes_mo <- 0L; p }, "params.classes_mo")
  chk(function(p) { p$kmeans_seed <- NA_integer_; p }, "params.kmeans_seed")
  chk(function(p) { p$feriado <- "sim"; p }, "params.feriado")
  ## a classe do erro e contract (exit 2)
  sp <- spec; sp$params$ic <- NULL
  cnd <- hw_catch(env$plugin_build_features(fx$deck, sp))
  expect_s3_class(cnd, "prevcarga_error")
  expect_identical(cnd$class, "contract")
  ## sem default no codigo
  rdir <- file.path(hw_root(), "models", "holt-winters", "R")
  fe <- file.path(rdir, c("feature_engineering.R", "helpers_aux.R"))
  tmp <- tempfile("hw-def-"); dir.create(tmp); file.copy(fe, tmp)
  expect_identical(hw_grep_code(tmp, "%\\|\\|%|is\\.null\\(params\\$"), character())
  ## CONTROL: default plantado e apanhado
  writeLines("g <- function(params) if (is.null(params$ic)) 2 else params$ic", file.path(tmp, "plantado.R"))
  expect_length(hw_grep_code(tmp, "%\\|\\|%|is\\.null\\(params\\$"), 1L)
})

FE_DATA_PAT <- paste("as\\.Date\\(", "as\\.POSIXct", "strptime", "format\\(.*%Y", "format\\(.*%j", "paste\\(.*-01-01",
                     "Sys\\.time", "Sys\\.Date", "Sys\\.getenv", "<<-", "source\\(", sep = "|")

test_that("[PLUGINS] fe_datas: sem datas de base R nem relogio/ambiente; CONTROL plantado e apanhado", {
  rdir <- file.path(hw_root(), "models", "holt-winters", "R")
  tmp <- tempfile("hw-datas-"); dir.create(tmp)
  file.copy(file.path(rdir, c("feature_engineering.R", "helpers_aux.R")), tmp)
  expect_identical(hw_grep_code(tmp, FE_DATA_PAT), character())
  expect_identical(hw_grep_code(rdir, "Sys\\."), character())
  ## o porte usa lubridate
  txt <- paste(readLines(file.path(tmp, "helpers_aux.R"), warn = FALSE), collapse = "\n")
  for (tok in c("lubridate::year(", "lubridate::yday(", "lubridate::floor_date(", "lubridate::days(1)"))
    expect_true(grepl(tok, txt, fixed = TRUE), info = tok)
  ## CONTROL: copia com as.Date( plantado
  for (planted in c("x <- as.Date(y)", "z <- Sys.time()", "w <- format(d, '%Y')")) {
    writeLines(planted, file.path(tmp, "plantado.R"))
    expect_length(hw_grep_code(tmp, FE_DATA_PAT), 1L)
  }
})

test_that("[PLUGINS] fe_pureza: deck de entrada intacto, saida com chaves e dimensoes, RNG e globalenv preservados", {
  env <- hw_env(); fx <- fe_deck(); spec <- hw_spec()
  deck0 <- fx$deck
  guard <- hw_lib$.prevcarga_plugin_guarded
  plugin <- list(descriptor = list(model_id = "holt-winters", uses_rng = TRUE))
  set.seed(99); seed0 <- get(".Random.seed", envir = globalenv()); g0 <- ls(globalenv(), all.names = TRUE)
  f <- suppressMessages(guard(plugin, env$plugin_build_features(fx$deck, spec)))
  expect_identical(get(".Random.seed", envir = globalenv()), seed0)
  expect_identical(ls(globalenv(), all.names = TRUE), g0)
  expect_identical(fx$deck, deck0)
  expect_identical(names(f), c("CargaTempVer", "CargaTempMed", "CargaTempVerFer", "CargaTempMed1", "m_mes", "TempPre",
                               "CDDPre", "load_day_o", "n_steps", "weather_absent", "meta"))
  n <- nrow(fx$deck$CargaTempVer)
  expect_identical(dim(f$CargaTempVer), c(n, 105L))
  expect_identical(dim(f$CargaTempVerFer), c(n, 105L))
  expect_identical(dim(f$CargaTempMed), c(n, 6L))
  expect_identical(dim(f$CargaTempMed1), c(n, 6L))
  expect_identical(names(f$CargaTempVer), env$hw_ctv_cols())
  expect_identical(names(f$CargaTempMed), c("data", "feriado", "diasemana", "Cmed", "Tmed", "TIPO_FER"))
  expect_identical(f$m_mes, as.integer(lubridate::month(fx$cuts$origin_day)))
  expect_identical(f$m_mes, 5L)
  for (k in c("TempPre", "CDDPre", "load_day_o", "n_steps", "weather_absent", "meta"))
    expect_identical(f[[k]], fx$deck[[k]], info = k)
  expect_s3_class(f$CargaTempVer$Data, "Date")
  expect_true(all(f$CargaTempVer$Data <= fx$cuts$last_data_day))
})

test_that("[PLUGINS] fe_perfil_st: o ramo ST (ARIMA) roda e devolve as mesmas colunas e dimensoes do ramo C", {
  env <- hw_env(); fx <- fe_deck(); spec <- hw_spec()
  expect_identical(spec$params$perfil_mo, "C")
  c0 <- suppressMessages(env$plugin_build_features(fx$deck, spec))
  sp <- spec; sp$params$perfil_mo <- "ST"
  st <- suppressMessages(env$plugin_build_features(fx$deck, sp))
  expect_identical(names(st), names(c0))
  for (k in c("CargaTempVer", "CargaTempMed", "CargaTempVerFer", "CargaTempMed1"))
    expect_identical(dim(st[[k]]), dim(c0[[k]]), info = k)
  expect_identical(names(st$CargaTempVer), names(c0$CargaTempVer))
  expect_true(all(is.finite(as.matrix(st$CargaTempVer[, env$hw_slot_cols()]))))
})
