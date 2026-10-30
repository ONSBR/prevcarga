## helper-rd2.R -- fixtures do RD.2 (cabeca de nowcast, treino ate O-1, freeze puro). Auto-carregado por test_dir. Nao e
## codigo de produto. Painel sintetico deterministico SEM RNG (ruido por senos), com 14 meses de historia comecando num
## primeiro de mes (fable::ARIMA com Mes exige todos os niveis do fator na janela), feriados espalhados e temperatura
## nao colinear. Cada treino custa ~1 min (216 ajustes de SVM): os treinos compartilhados ficam em cache de sessao.
## determinismo e caixa compartilhada: BLAS/OpenMP em 1 thread dentro do processo (como o runtime.R), sem variavel de ambiente
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(1L); RhpcBLASctl::omp_set_num_threads(1L)
}
RD2_O <- lubridate::ymd("2024-12-23")
RD2_D <- as.integer(RD2_O - lubridate::ymd("2023-11-01"))
RD2_ORACLE_FIT <- "/opt/ons-source/prevcargadessem3/models/dessem-regdin/R/training/dessem_regdin_fit.R"
RD2_ORACLE_ENG <- "/opt/ons-source/prevcargadessem3/models/dessem-regdin/vendor/_load_engine.R"

rd2_panel <- function(until = NULL) {
  pan <- rd_panel(origin = format(RD2_O), days = RD2_D, until = until)
  pan$holidays <- data.table::data.table(
    area_code = "SE", date = RD2_O - c(5L, 12L, 20L, 33L, 47L, 61L, 75L, 90L, 120L, 150L, 200L, 260L, 330L, 390L),
    holiday_type_id = 1L, prevcarga_code = c(3L, 4L, 5L, 3L, 4L, 6L, 3L, 4L, 5L, 3L, 6L, 4L, 5L, 3L), simplified_code = 3L)
  dd <- as.numeric(lubridate::as_date(pan$load$interval_start) - (RD2_O - RD2_D))
  wd <- lubridate::wday(pan$load$interval_start)
  pan$load[, load_mwh := load_mwh * (1 + 0.04 * sin(dd * dd * 0.77 + 1.1)) - 900 * (wd %in% c(1L, 7L)) + 150 * sin(dd * 3.7)]
  ddo <- as.numeric(lubridate::as_date(pan$weather_observed$valid_time) - (RD2_O - RD2_D))
  pan$weather_observed[, temperature := temperature + 2.5 * sin(ddo * ddo * 1.31 + 0.4) + 1.5 * sin(ddo * 5.3)]
  pan
}

rd2_spec <- function(exogena = "MED") rd_spec(exogena, RD2_D - 1L)
rd2_ctx <- function() list(load_label = "end", timezone = RD_TZ, weather_issue_cutoff = "available_until")
rd2_au <- function(hour) lubridate::force_tz(lubridate::as_datetime(RD2_O), RD_TZ) + lubridate::hours(hour)

## stages de ingestao + features pelo plugin carregado (painel de exibicao -> to_hourly -> frame de modelagem)
rd2_feats <- function(p, pan, au_hour = 9L, exogena = "MED") {
  cu <- rd_cuts(available_until = rd2_au(au_hour))
  m <- rd_model_panel(pan, cu)
  frame <- rd_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  rd_with_cli(suppressMessages(p$descriptor$stages$feature_engineering$fn(frame, rd2_spec(exogena)$params)))
}

## treino pelo despachante da fundacao com o painel CRU (o despachante reamostra: to_hourly -> to_model_frame -> estagios ->
## freeze -> carimbo -> scan_pure; FIX 7)
rd2_train <- function(p, au_hour = 9L, pan = rd2_panel(), exogena = "MED") {
  rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_train(p, pan, rd_cuts(available_until = rd2_au(au_hour)),
                                                            rd2_spec(exogena), ctx = rd2_ctx())))
}

rd2_sha <- function(art) {
  if (!exists("prevcarga_freeze", envir = rd_lib, inherits = FALSE))
    sys.source(file.path(rd_root(), "scripts", "lib", "provenance.R"), envir = rd_lib)
  f <- tempfile(fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))), add = TRUE)
  rd_lib$prevcarga_freeze(art, f)
  jsonlite::read_json(paste0(f, ".manifest.json"), simplifyVector = TRUE)$sha256
}

## espiao em tune_wsvm do ENV DO MOTOR: conta ajustes, guarda formula e a posicao do RNG antes de cada um
rd2_spy <- function(eng) {
  log <- new.env(); log$n <- 0L; log$fml <- character(); log$state <- list(); log$data <- list()
  real <- get("tune_wsvm", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("tune_wsvm", eng)
  if (unlock) unlockBinding("tune_wsvm", eng)
  assign("tune_wsvm", function(formula, ..., data) {
    log$n <- log$n + 1L
    log$data[[log$n]] <- data
    log$fml[[log$n]] <- paste(deparse(formula, width.cutoff = 500L), collapse = "")
    log$state[[log$n]] <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    real(formula, ..., data = data)
  }, envir = eng)
  list(log = log, restore = function() { assign("tune_wsvm", real, envir = eng); if (unlock) lockBinding("tune_wsvm", eng) })
}

rd2_cached <- local({
  cache <- new.env(parent = emptyenv())
  function(key, expr) {
    if (!is.null(cache[[key]])) return(cache[[key]])
    val <- force(expr)
    assign(key, val, envir = cache)
    val
  }
})
rd2_plugin <- function() rd2_cached("plugin", rd_load())

## so os 2 primeiros dias-alvo (a ordem do RNG e sequencial: os 2 primeiros dias independem dos demais)
rd2_trunc <- function(f, k = 2L) { f$dias <- f$dias[seq_len(k)]; f$day_types <- f$day_types[seq_len(k)]; f }

## troca temporaria de uma funcao do ENV DO PLUGIN (o que o treino resolve por nome); restaura no on.exit do chamador
rd2_with_plugin_fn <- function(name, fn, code, env = rd2_plugin()$env) {
  orig <- get(name, envir = env, inherits = FALSE)
  assign(name, fn(orig), envir = env)
  on.exit(assign(name, orig, envir = env), add = TRUE)
  force(code)
}

rd2_stamped <- function(p, payload, au_hour = 9L)
  rd_lib$prevcarga_plugin_stamp(payload, p, rd2_spec(), rd_cuts(available_until = rd2_au(au_hour)), rd2_ctx())

## carrega so o trecho do wrapper (.rd_nowcast ... execDemandaMediaTS) num env filho do motor
rd2_oracle_wrapper <- function(eng) {
  src <- readLines(RD2_ORACLE_ENG, warn = FALSE)
  i <- grep("^\\.rd_nowcast <- new\\.env", src)[1L]
  j <- grep("^# .* Confirm required functions exist", src)[1L] - 1L
  e <- new.env(parent = eng)
  eval(parse(text = src[i:j]), envir = e)
  e
}


## estado compartilhado do treino completo (09:00) e treino truncado de 2 dias
rd2_state <- local({
  s <- NULL
  function() {
    if (!is.null(s)) return(s)
    p <- rd2_plugin(); eng <- parent.env(p$env)
    spy <- rd2_spy(eng)
    set.seed(7); caller0 <- get(".Random.seed", globalenv())
    a09 <- rd2_train(p, 9L)
    caller1 <- get(".Random.seed", globalenv())
    spy$restore()
    s <<- list(p = p, eng = eng, log = spy$log, a09 = a09, caller_same = identical(caller0, caller1))
    s
  }
})

## treino truncado (2 dias) direto no estagio do plugin (pre-freeze) a partir das features do painel dado
rd2_fit2 <- function(p, pan = rd2_panel(), au_hour = 9L) {
  f <- rd2_trunc(rd2_feats(p, pan, au_hour))
  rd_with_cli(suppressWarnings(suppressMessages(p$env$rd_training(f, rd2_spec()$params, 42L))))
}
rd2_sha2 <- function(p, model) rd2_sha(rd2_stamped(p, p$env$rd_freeze(model)))

