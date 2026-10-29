## test-dl-infer.R -- DL.7: infer (mm[O] ate available_until, DM(O) refeita, SVMs congelados) e forecast acoplado.
## Loader real, motor real, painel sintetico. Sem golden/paridade (o motor ainda faz %m-% hours(1) ate o DL.3).

DI_DAYS <- 40L
di_O <- function() lubridate::ymd("2024-12-23")
di_au <- function(h, m = 0L) lubridate::force_tz(lubridate::as_datetime(di_O()), DL_TZ) + lubridate::hours(h) + lubridate::minutes(m)
di_panel <- function(until = NULL, days = 60L, ...) {
  pan <- dl_panel(days = days, until = until, ...)
  pan$holidays <- pan$holidays[date < di_O()]
  pan
}
di_prep <- function() {
  if (!exists("to_semihourly", envir = globalenv(), inherits = FALSE)) assign("to_semihourly", dl_lib$to_semihourly, envir = globalenv())
}
## cuts do despachante: weather_issue_cutoff e a REGRA (texto), como no carimbo do artefato (o fixture dl_cuts traz POSIXct)
di_cuts <- function(...) { cu <- dl_cuts(...); cu$weather_issue_cutoff <- "available_until"; cu }
di_ctx <- list(load_label = "end", timezone = DL_TZ, weather_issue_cutoff = "available_until")

## espiao em tune_wsvm do ENV DO MOTOR
di_spy <- function(eng) {
  log <- new.env(); log$n <- 0L; log$seed0 <- list()
  real <- get("tune_wsvm", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("tune_wsvm", eng)
  if (unlock) unlockBinding("tune_wsvm", eng)
  assign("tune_wsvm", function(...) {
    log$n <- log$n + 1L
    if (log$n == 1L) log$seed0 <- get(".Random.seed", globalenv())
    real(...)
  }, envir = eng)
  list(log = log, restore = function() { assign("tune_wsvm", real, envir = eng); if (unlock) lockBinding("tune_wsvm", eng) })
}

di_train <- function(p, seed = 42L, exogena = "MED") {
  cuts <- di_cuts(available_until = di_au(9L))
  spec <- dl_spec(exogena, DI_DAYS); spec$seed <- seed
  dl_with_cli(suppressMessages(dl_lib$prevcarga_plugin_train(p, dl_lib$to_hourly(di_panel()), cuts, spec, ctx = di_ctx)))
}

di_infer <- function(p, art, au_h, au_m = 0L, perturb = NULL, panel = NULL) {
  di_prep()
  au <- di_au(au_h, au_m)
  pan <- if (is.null(panel)) di_panel(until = au) else panel
  if (!is.null(perturb)) pan$load[perturb(interval_start), load_mwh := load_mwh + 500]
  dl_with_cli(suppressMessages(dl_lib$prevcarga_plugin_infer(p, art, dl_lib$to_hourly(pan), di_cuts(available_until = au))))
}

## EMULACAO DO DL.3 (so no teste): o motor ainda faz `%m-% hours(1)` em TransformaDeck e, sem isso, a ultima hora de O-1
## some e o LAG_1 da hora 0 de O fica NA (predict impossivel). Enquanto o DL.3 nao remove a linha do motor, o env do
## motor deste teste recebe um TransformaDeck que devolve +1 h aos rotulos de CARGAHIST: o -1 h do motor os restaura,
## que e o estado pos-DL.3. O env e proprio deste teste (dl_load recarrega o motor).
di_emulate_dl3 <- function(eng) {
  orig <- get("TransformaDeck", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("TransformaDeck", eng)
  if (unlock) unlockBinding("TransformaDeck", eng)
  assign("TransformaDeck", function(deck, ...) {
    deck$CARGAHIST <- data.table::copy(deck$CARGAHIST)[, DataHora := DataHora + lubridate::hours(1L)]
    orig(deck = deck, ...)
  }, envir = eng)
  if (unlock) lockBinding("TransformaDeck", eng)
  invisible(NULL)
}

di_state <- local({
  s <- NULL
  function() {
    if (!is.null(s)) return(s)
    p <- dl_load(); eng <- parent.env(p$env)
    di_emulate_dl3(eng)
    art <- di_train(p)
    spy <- di_spy(eng)
    set.seed(11); r0 <- get(".Random.seed", globalenv())
    e09 <- di_infer(p, art, 9L)
    r1 <- get(".Random.seed", globalenv())
    spy$restore()
    s <<- list(p = p, eng = eng, art = art, e09 = e09, tune_calls = spy$log$n, rng_same = identical(r0, r1))
    s
  }
})

test_that("ac-2/ac-3 envelope: 9x48 finito, entrega = grade da fundacao, 1 chamada a tune_wsvm, RNG do chamador intacto", {
  st <- di_state(); e <- st$e09; O <- di_O()
  expect_identical(st$tune_calls, 1L)                                  # so a DM(O)
  expect_true(st$rng_same)
  expect_identical(dim(e$values), c(9L, 48L)); expect_true(all(is.finite(e$values)))
  expect_identical(e$layout, "dessem_semihourly_48")
  cuts <- di_cuts(available_until = di_au(9L))
  grid <- dl_lib$prevcarga_delivery_grid(cuts, 9L)
  expect_identical(e$delivery$interval_end, grid)
  expect_identical(e$delivery$interval_end[[1L]], di_au(9L, 30L)); expect_identical(lubridate::tz(e$delivery$interval_end), DL_TZ)
  expect_identical(e$delivery$interval_end[[length(grid)]],
                   lubridate::force_tz(lubridate::as_datetime(O + 9L), DL_TZ))
  expect_true(all(is.finite(e$delivery$y_pred)))
  ## a entrega e a matriz 9x48 sem a parte ja observada (ultimas n meias horas)
  v <- as.vector(t(e$values))
  expect_equal(e$delivery$y_pred, v[(length(v) - length(grid) + 1L):length(v)])
  expect_identical(e$meta$rng_mode, "preserve"); expect_true(e$meta$dm_head_refit)
  expect_identical(e$variant, "MED"); expect_identical(e$origin_day, O)
})

test_that("ac-1 FORBIDDEN carga > corte nao muda nada; CONTROL manha muda O e O+1 (LAG_1 da media prevista)", {
  st <- di_state(); p <- st$p; O <- di_O()
  cuts <- di_cuts(available_until = di_au(12L))
  m <- dl_model_panel(di_panel(until = di_au(12L)), cuts)
  frame <- dl_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  run <- function(fr) dl_with_cli(suppressWarnings(p$descriptor$stages$inference$fn(
    st$art$payload, p$descriptor$stages$inference_features$fn(fr, st$art$payload), m$cuts)))
  base <- run(frame)
  ## FORBIDDEN: horas de O com interval_end > corte entram no deck bruto, mas nao em mm
  late <- data.table::copy(frame)
  hl <- data.table::copy(late$hourly_load)
  last_h <- max(hl$interval_end)
  ## linhas sinteticas com rotulo apos o corte (no frame de modelagem): nao podem entrar em mm
  fut <- data.table::data.table(interval_end = last_h + lubridate::hours(1:3), Carga = 99999)
  late$hourly_load <- data.table::rbindlist(list(hl, fut))
  alt <- run(late)
  expect_lte(max(abs(alt$values - base$values)), 1e-5)
  ## CONTROL: perturbar a manha (08:00-09:00 de O, rotulo do frame de modelagem) muda O e O+1
  man <- data.table::copy(frame)
  hh <- man$hourly_load
  k <- which(lubridate::as_date(hh$interval_end) == O & lubridate::hour(hh$interval_end) %in% c(8L, 9L))
  expect_gt(length(k), 0L)
  hh[k, Carga := Carga + 800]
  man$hourly_load <- hh
  ctl <- run(man)
  expect_gt(max(abs(ctl$values[1L, ] - base$values[1L, ])), 1e-5)
  expect_gt(max(abs(ctl$values[2L, ] - base$values[2L, ])), 1e-5)
})

test_that("ac-2 available_until 09:00 x 09:30: saidas iguais de 10:00 em diante; entrega comeca em 09:30 e 10:00; CONTROL manha", {
  st <- di_state(); p <- st$p
  e30 <- di_infer(p, st$art, 9L, 30L)
  e09 <- st$e09
  expect_identical(e09$delivery$interval_end[[1L]], di_au(9L, 30L))
  expect_identical(e30$delivery$interval_end[[1L]], di_au(10L))
  a <- e09$delivery[interval_end >= di_au(10L)]; b <- e30$delivery
  expect_identical(a$interval_end, b$interval_end)
  expect_lte(max(abs(a$y_pred - b$y_pred)), 1e-5)
  ctl <- di_infer(p, st$art, 9L, perturb = function(s) lubridate::hour(s) == 8L & lubridate::as_date(s) == di_O())
  expect_gt(max(abs(ctl$values - e09$values)), 1e-5)
})

test_that("ac-3 sentinela: cada valor cai no intervalo da sua hora (interval_start); CONTROL agrupar por interval_end reprova", {
  st <- di_state(); p <- st$p; eng <- st$eng; di_prep()
  fb <- get("desagregaModeloSH", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("desagregaModeloSH", eng)
  if (unlock) unlockBinding("desagregaModeloSH", eng)
  assign("desagregaModeloSH", function(modelo, dadosMdl) stop("forcado"), envir = eng)
  pp <- get(".dl_predict_pass", envir = p$env, inherits = FALSE)
  assign(".dl_predict_pass", function(a, f, dm_head) {
    sp <- f$seriePF[Prev == TRUE, .(DataHora)]
    sp[, Carga := as.numeric(as.integer(lubridate::as_date(DataHora) - f$cuts$origin_day)) * 100 + lubridate::hour(DataHora)]
    sp[]
  }, envir = p$env)
  on.exit({
    assign(".dl_predict_pass", pp, envir = p$env)
    assign("desagregaModeloSH", fb, envir = eng); if (unlock) lockBinding("desagregaModeloSH", eng)
  }, add = TRUE)
  e <- di_infer(p, st$art, 9L)
  expect_true("warning.dessem_linear_desagrega_fallback" %in% e$meta$warnings)
  d <- data.table::copy(e$delivery)
  by_start <- function(x) { s <- x - lubridate::minutes(30L); as.numeric(as.integer(lubridate::as_date(s) - di_O())) * 100 + lubridate::hour(s) }
  by_end <- function(x) as.numeric(as.integer(lubridate::as_date(x) - di_O())) * 100 + lubridate::hour(x)
  expect_equal(d$y_pred, by_start(d$interval_end))
  expect_false(isTRUE(all.equal(d$y_pred, by_end(d$interval_end))))           # CONTROL
  expect_identical(dim(e$values), c(9L, 48L))
})

test_that("ac-4 artefato de outra origem/variante/modelo: aborta antes de qualquer ajuste; CONTROL segue", {
  st <- di_state(); p <- st$p; eng <- st$eng
  spy <- di_spy(eng); on.exit(spy$restore(), add = TRUE)
  ## outra origem: o despachante recusa (carimbo); o estagio recusa pelo proprio artefato
  cuts_prev <- di_cuts(origin = "2024-12-22", available_until = di_au(9L) - lubridate::days(1))
  cond <- tryCatch(dl_with_cli(dl_lib$prevcarga_plugin_infer(p, st$art, dl_lib$to_hourly(di_panel(until = di_au(9L))), cuts_prev)),
                   prevcarga_error = function(e) e)
  expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
  cuts <- di_cuts(available_until = di_au(9L))
  m <- dl_model_panel(di_panel(until = di_au(9L)), cuts)
  frame <- dl_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  feat <- p$descriptor$stages$inference_features$fn
  pl <- st$art$payload
  o1 <- pl; o1$head_recipe$dias <- format(di_O() - 1L + 0:8)
  expect_dl_error(feat(frame, o1), "plugin.dessem_linear_artifact_mismatch", "origin_day")
  v1 <- pl; v1$variant <- "MAX"
  cond <- expect_dl_error(feat(frame, v1), "plugin.dessem_linear_artifact_mismatch", "model_key")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
  ## variante do membro conhecida pelo deck (ingestao com spec): MAX x artefato MED
  fr_max <- dl_with_cli(suppressMessages(plugin_ingest_with <- p$env$plugin_ingest(m$panel, m$cuts, dl_spec("MAX", DI_DAYS))))
  expect_dl_error(feat(fr_max, pl), "plugin.dessem_linear_artifact_mismatch", "variant")
  a1 <- pl; a1$area <- "NE"
  expect_dl_error(feat(frame, a1), "plugin.dessem_linear_artifact_mismatch", "area")
  expect_identical(spy$log$n, 0L)                                      # nada foi ajustado
  ## CONTROL: artefato de O e variante MED segue
  ok <- dl_with_cli(suppressMessages(feat(frame, pl)))
  expect_named(ok, c("processado", "serieDM", "seriePF", "mm", "config_mdl", "norm", "cuts", "engine"))
  expect_true(da <- nrow(ok$mm) > 0L)
})

test_that("ac-5 infer separado == forecast acoplado no dia D; WMAPE da cauda; CONTROL seed 43", {
  st <- di_state(); p <- st$p
  di_prep()
  O <- di_O(); au <- di_au(9L)
  cuts <- di_cuts(available_until = au)
  m <- dl_model_panel(di_panel(until = au), cuts)
  spec <- dl_spec("MED", 60L)
  spy <- di_spy(st$eng)
  fc <- dl_with_cli(suppressMessages(suppressWarnings(p$env$plugin_forecast(m$panel, m$cuts, spec))))
  spy$restore(); seed42 <- spy$log$seed0
  sep <- st$e09
  ## em frame de modelagem: values nao leva deslocamento (fatiado por Passo)
  d_day <- max(abs(fc$values[1L, ] - sep$values[1L, ]))
  d_all <- max(abs(fc$values - sep$values))
  expect_lte(d_day, 1e-5)
  wm <- sum(abs(fc$values[-1L, ] - sep$values[-1L, ])) / sum(abs(sep$values[-1L, ]))
  expect_lte(wm, 0.01)                                                 # [TBD: ADR-0031]
  message(sprintf("DL.7 ac-5: max|d| dia D = %.3g; max|d| total = %.3g; WMAPE cauda = %.3g", d_day, d_all, wm))
  spec43 <- spec; spec43$seed <- 43L
  spy <- di_spy(st$eng)
  fc43 <- dl_with_cli(suppressMessages(suppressWarnings(p$env$plugin_forecast(m$panel, m$cuts, spec43))))
  spy$restore()
  ## CONTROL: a semente chega ao gerador (estado do 1o ajuste do acoplado difere de 42 para 43). Neste fixture o CV do
  ## tune_wsvm escolhe os mesmos hiperparametros nas duas sementes, entao a saida em si nao muda (medido: ver message).
  expect_false(identical(seed42, spy$log$seed0))
  set.seed(43L); expect_identical(spy$log$seed0, get(".Random.seed", globalenv()))
  message(sprintf("DL.7 ac-5 CONTROL seed 43: max|d| dia D = %.3g", max(abs(fc43$values[1L, ] - sep$values[1L, ]))))
})

test_that("ac-6 ENV ignorado, formals sem params, cauda de EXOGENAPREV curta completa 9x48", {
  st <- di_state(); p <- st$p
  expect_false("params" %in% names(formals(p$env$plugin_infer)))
  expect_false("params" %in% names(formals(p$env$dl_inference_features)))
  old <- Sys.getenv(c("EXOGENA", "DESSEM_EXOG_TAIL_FILL"), unset = NA)
  Sys.setenv(EXOGENA = "MAX", DESSEM_EXOG_TAIL_FILL = "0")
  on.exit({ for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(old[[n]]), n)) }, add = TRUE)
  e <- di_infer(p, st$art, 9L)
  expect_identical(e$values, st$e09$values); expect_identical(e$delivery, st$e09$delivery)
  ## CONTROL: tail de 2 dias sem previsao => preenchido (sempre ligado), ainda 9x48
  short <- di_infer(p, st$art, 9L, panel = di_panel(until = di_au(9L), fc_short = c(7L, 8L)))
  expect_identical(dim(short$values), c(9L, 48L)); expect_true(all(is.finite(short$values)))
  ## fonte: sem leitura de ambiente nem aritmetica de hora nem `<<-`
  for (f in c("inference.R", "inference_features.R")) {
    src <- sub("#.*$", "", readLines(file.path(dl_models(), "dessem-linear", "R", f), warn = FALSE))
    expect_false(any(grepl("Sys\\.(getenv|time|setenv)|[+-]\\s*3600|difftime|with_tz|as\\.POSIX|<<-|minutes\\(|hours\\(", src, perl = TRUE)))
  }
})
