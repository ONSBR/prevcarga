## test-dl-train.R -- DL.6: treino ate O-1 com RNG preservado, plano B e freeze puro (AC1..AC5 do ticket) + F5 (variante do membro).
## Loader real, motor real, painel sintetico. Cada treino (225 ajustes) e feito uma vez e reaproveitado pelos testes.

DT_DAYS <- 40L
dt_prep <- function() {
  if (!exists("prevcarga_freeze", envir = dl_lib, inherits = FALSE))
    sys.source(file.path(dl_root(), "scripts", "lib", "provenance.R"), envir = dl_lib)
}
dt_sha <- function(art) {
  dt_prep()
  f <- tempfile(fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))), add = TRUE)
  dl_lib$prevcarga_freeze(art, f)
  jsonlite::read_json(paste0(f, ".manifest.json"), simplifyVector = TRUE)$sha256
}

## espiao em tune_wsvm do ENV DO MOTOR (o mesmo que SVM_DemandaMedia/SVMhorario/dl_dm_head resolvem)
dt_spy <- function(eng, stop_at = Inf) {
  log <- new.env(); log$n <- 0L; log$fml <- character(); log$data <- list(); log$seed0 <- list()
  real <- get("tune_wsvm", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("tune_wsvm", eng)
  if (unlock) unlockBinding("tune_wsvm", eng)
  assign("tune_wsvm", function(formula, ..., data) {
    log$n <- log$n + 1L
    log$fml[[log$n]] <- paste(deparse(formula, width.cutoff = 500L), collapse = "")
    log$data[[log$n]] <- data
    log$seed0[[log$n]] <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    if (log$n >= stop_at) stop("dt_spy: parada pedida", call. = FALSE)
    real(formula, ..., data = data)
  }, envir = eng)
  list(log = log, restore = function() { assign("tune_wsvm", real, envir = eng); if (unlock) lockBinding("tune_wsvm", eng) })
}

## feriado em O+1 do fixture padrao nao tem amostra historica do mesmo tipo (o motor recusaria): fora do painel de treino
dt_panel <- function(...) {
  pan <- dl_panel(days = 60L, ...)
  pan$holidays <- pan$holidays[date < lubridate::ymd("2024-12-23")]
  pan
}

## perturbacao em O-2: enquanto o DL.3 nao remove o `%m-% hours(1)` do motor, o dia O-1 (23 horas uteis) ainda nao entra
## na serie diaria; com o DL.3 trocar por O-1 (o CONTROL do ticket).
dt_train <- function(p, au_hour = 9L, perturb_prev = FALSE, exogena = "MED", panel = NULL) {
  O <- lubridate::ymd("2024-12-23")
  au <- lubridate::force_tz(lubridate::as_datetime(O), DL_TZ) + lubridate::hours(au_hour)
  cuts <- dl_cuts(available_until = au)
  pan <- if (is.null(panel)) dt_panel() else panel
  if (perturb_prev) {
    k <- which(lubridate::as_date(pan$load$interval_start) == O - 2L & lubridate::hour(pan$load$interval_start) == 15L)
    pan$load[k, load_mwh := load_mwh + 400]
  }
  dl_with_cli(suppressMessages(dl_lib$prevcarga_plugin_train(p, dl_lib$to_hourly(pan), cuts, dl_spec(exogena, DT_DAYS),
                                                   ctx = list(load_label = "end", timezone = DL_TZ, weather_issue_cutoff = "available_until"))))
}

## sessao compartilhada: um plugin, o espiao e os treinos base
dt_state <- local({
  s <- NULL
  function() {
    if (!is.null(s)) return(s)
    p <- dl_load()
    eng <- parent.env(p$env)
    spy <- dt_spy(eng)
    set.seed(7); caller0 <- get(".Random.seed", globalenv())
    a09 <- dt_train(p, 9L)
    caller1 <- get(".Random.seed", globalenv())
    spy$restore()
    s <<- list(p = p, eng = eng, log = spy$log, a09 = a09, caller_same = identical(caller0, caller1))
    s
  }
})

test_that("ac-3 ordem do oraculo: 9 x (1 DM + 24 perfis) = 225 ajustes; DM(O) com mm e descartada; artefato O+1..O+8 / O..O+8", {
  st <- dt_state(); lg <- st$log; O <- lubridate::ymd("2024-12-23")
  expect_identical(lg$n, 225L)
  has_mm <- grepl("+ mm", lg$fml, fixed = TRUE)
  expect_identical(which(has_mm), 1L)                              # so a cabeca de referencia, 1o ajuste
  pl <- st$a09$payload
  expect_identical(names(pl$svm_dm), format(O + 1:8))
  expect_identical(names(pl$svm_perfil), format(O + 0:8))
  expect_true(all(vapply(pl$svm_perfil, length, 1L) == 24L))
  expect_identical(pl$rng_mode, "preserve"); expect_identical(pl$n_leak, 0L)
  expect_identical(pl$horizon_days, 9L); expect_identical(pl$seed, 42L)
  expect_setequal(names(pl), c("svm_dm", "svm_perfil", "norm", "head_recipe", "variant", "model_key", "area",
                               "horizon_days", "seed", "rng_mode", "n_leak"))
  expect_identical(names(pl$norm), c("dm_carga", "dm_exo", "pf_cargadm", "pf_exo"))
  expect_identical(names(pl$head_recipe), c("model_key", "config_mdl", "mm_norm", "dias"))
  ## a semente do 1o ajuste e a de set.seed(42): o gerador entra na posicao do oraculo
  set.seed(42L); fresh <- get(".Random.seed", globalenv())
  expect_identical(lg$seed0[[1L]], fresh)
  ## a cabeca consome o gerador: o estado antes do perfil de O (2o ajuste) difere do recem-semeado
  expect_false(identical(lg$seed0[[2L]], fresh))
  ## CONTROL: pular a cabeca deixa o perfil de O na posicao recem-semeada (RNG deslocado em relacao ao oraculo)
  p <- st$p
  orig <- p$env$dl_dm_head
  assign("dl_dm_head", function(...) NULL, envir = p$env)
  on.exit(assign("dl_dm_head", orig, envir = p$env), add = TRUE)
  spy <- dt_spy(st$eng, stop_at = 1L)
  suppressWarnings(try(dt_train(p, 9L), silent = TRUE))
  spy$restore()
  expect_identical(spy$log$seed0[[1L]], fresh)
  expect_false(identical(spy$log$seed0[[1L]], lg$seed0[[2L]]))
})

test_that("ac-2 sha256 invariante ao corte (09:00 x 12:00); FORBIDDEN dia O; CONTROL perturbar O-2 (O-1 apos o DL.3) muda", {
  st <- dt_state(); p <- st$p
  a12 <- dt_train(p, 12L)
  expect_identical(dt_sha(a12), dt_sha(st$a09))
  expect_identical(a12, st$a09)
  ## o treino nao enxerga o dia O: painel com carga de O no treino e recusado pelo despachante (nada e cortado)
  O <- lubridate::ymd("2024-12-23")
  pan <- dt_panel(until = lubridate::force_tz(lubridate::as_datetime(O), DL_TZ) + lubridate::hours(9L))
  cond <- tryCatch(dt_train(p, 9L, panel = pan), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.train_beyond_cut")
  ## CONTROL
  apert <- dt_train(p, 9L, perturb_prev = TRUE)
  expect_false(identical(dt_sha(apert), dt_sha(st$a09)))
})

test_that("ac-1 painel com linha do dia O: aborta train_sees_day_o (contract, exit 2) sem ajustar SVM; CONTROL treina", {
  st <- dt_state(); p <- st$p; O <- lubridate::ymd("2024-12-23")
  cuts <- dl_cuts(); m <- dl_model_panel(dt_panel(), cuts)
  frame <- dl_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  feats <- dl_with_cli(suppressMessages(p$descriptor$stages$feature_engineering$fn(frame, dl_spec("MED", DT_DAYS)$params)))
  spy <- dt_spy(st$eng); on.exit(spy$restore(), add = TRUE)
  bad <- feats
  row <- data.table::data.table(DataHora = lubridate::force_tz(lubridate::as_datetime(O), DL_TZ) + lubridate::hours(3L),
                                Carga = 9000)
  bad$deck$CARGAHIST <- data.table::rbindlist(list(bad$deck$CARGAHIST, row))
  cond <- expect_dl_error(p$descriptor$stages$training$fn(bad, dl_spec("MED", DT_DAYS)$params, 42L),
                          "plugin.dessem_linear_train_sees_day_o", format(O))
  expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
  bad2 <- feats
  bad2$deck$origin_load <- data.table::data.table(interval_end = row$DataHora, Carga = 9000)
  expect_dl_error(p$env$plugin_train(bad2, dl_spec("MED", DT_DAYS)), "plugin.dessem_linear_train_sees_day_o")
  expect_identical(spy$log$n, 0L)                                  # nenhum SVM ajustado
  ## a guarda anti-circular zera Prev == TRUE e e NO-OP na serie limpa (porta de dessem_linear_fit.R:59-63)
  sc <- data.table::copy(feats$processado$SERIECOMP)
  expect_identical(p$env$.dl_enforce_real_history(sc), 0L)
  sc[Prev == TRUE, Carga := 123][, Prev := Prev]
  n <- p$env$.dl_enforce_real_history(sc)
  expect_gt(n, 0L); expect_true(all(is.na(sc[Prev == TRUE, Carga])))
  ## CONTROL: o mesmo painel sem a linha de O treina (a base compartilhada ja o provou: 225 ajustes)
  expect_identical(st$log$n, 225L)
})

test_that("ac-4 plano B (rng_deviation): reseed antes do perfil de O, sem cabeca, aviso, rng_mode reseed; CONTROL preserve", {
  st <- dt_state(); p <- st$p
  ## descriptor-fixture com desvio
  expect_identical(p$env$dl_rng_mode(deviation = NULL), "preserve")
  w <- NULL
  mode <- withCallingHandlers(dl_with_cli(p$env$dl_rng_mode(deviation = "consumo de RNG diverge do oraculo")),
                              warning = function(x) { w <<- x; invokeRestart("muffleWarning") })
  expect_identical(mode, "reseed")
  expect_s3_class(w, "prevcarga_warning")
  expect_identical(w$id, "warning.dessem_linear_rng_deviation")
  ## preserve: nenhum aviso
  w2 <- NULL
  withCallingHandlers(dl_with_cli(p$env$dl_rng_mode()), warning = function(x) { w2 <<- x; invokeRestart("muffleWarning") })
  expect_null(w2)
  ## treino em reseed
  d0 <- p$env$plugin_descriptor
  assign("plugin_descriptor", function() { d <- d0(); d$rng_deviation <- "desvio de teste"; d }, envir = p$env)
  on.exit(assign("plugin_descriptor", d0, envir = p$env), add = TRUE)
  spy <- dt_spy(st$eng)
  ws <- NULL
  a_rs <- withCallingHandlers(dt_train(p, 9L), warning = function(x) { ws <<- c(ws, x$id); invokeRestart("muffleWarning") })
  spy$restore()
  expect_identical(a_rs$payload$rng_mode, "reseed")
  expect_true("warning.dessem_linear_rng_deviation" %in% ws)
  expect_identical(spy$log$n, 224L)                                # 8 DM + 9 x 24 perfis, sem cabeca
  expect_false(any(grepl("+ mm", spy$log$fml, fixed = TRUE)))
  set.seed(42L); fresh <- get(".Random.seed", globalenv())
  expect_identical(spy$log$seed0[[1L]], fresh)                     # set.seed re-fixado imediatamente antes do perfil de O
  expect_false(identical(dt_sha(a_rs), dt_sha(st$a09)))
  expect_identical(st$a09$payload$rng_mode, "preserve")
})

test_that("ac-5 freeze puro e deterministico; thaw prediz identico; carimbos; RNG do chamador intacto; CONTROL adulterado", {
  st <- dt_state(); p <- st$p; O <- lubridate::ymd("2024-12-23")
  a <- st$a09
  expect_true(st$caller_same)
  expect_true(dl_lib$prevcarga_plugin_scan_pure(a))
  s <- a$stamp
  expect_identical(s$train_origin, O); expect_identical(s$origin_day, O)
  expect_identical(s$variant, "MED"); expect_identical(s$seed, 42L); expect_identical(s$contract_version, 1L)
  expect_identical(s$model_id, "dessem-linear")
  expect_identical(a$payload$model_key, "Linear.TmpMedia")
  nm <- function(x) c(names(x), unlist(lapply(x, function(y) if (is.list(y) && !is.data.frame(y)) nm(y))))
  expect_false(any(c("available_until", "trained_at", "cut") %in% nm(a)))
  ## thaw: predicao identica a do modelo em memoria (modelos reais capturados pelo espiao e re-treinados sem congelar)
  eng <- st$eng
  th <- p$descriptor$thaw(a$payload)
  expect_true(all(vapply(th$svm_perfil, function(d) all(vapply(d, inherits, NA, what = "wsvm")), NA)))
  bm <- function(m) if (inherits(m, "tune")) m$best.model else m   # o motor devolve o objeto tune; o freeze guarda o best.model
  spy <- dt_spy(eng)
  feats_cuts <- dl_cuts(); m <- dl_model_panel(dt_panel(), feats_cuts)
  frame <- dl_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  feats <- dl_with_cli(suppressMessages(p$descriptor$stages$feature_engineering$fn(frame, dl_spec("MED", DT_DAYS)$params)))
  mem <- dl_with_cli(p$descriptor$stages$training$fn(feats, dl_spec("MED", DT_DAYS)$params, 42L))
  spy$restore()
  d2 <- spy$log$data[[2L]]                                          # 2o ajuste = perfil(O, h=0) em preserve
  nd <- d2[seq_len(min(24L, nrow(d2))), ]
  expect_identical(stats::predict(th$svm_perfil[[1L]][[1L]], nd), stats::predict(bm(mem$svm_perfil[[1L]][[1L]]), nd))
  fz1 <- p$descriptor$freeze(mem)
  expect_identical(dt_sha(fz1), dt_sha(p$descriptor$freeze(mem)))
  expect_identical(fz1, a$payload)                                  # o artefato do despachante = treino + freeze
  d27 <- spy$log$data[[26L]]
  expect_identical(stats::predict(p$descriptor$thaw(fz1)$svm_dm[[1L]], d27[1:5, ]),
                   stats::predict(bm(mem$svm_dm[[1L]]), d27[1:5, ]))
  ## CONTROL: SVM adulterado => integrity (exit 9)
  tam <- a$payload
  tam$svm_perfil[[1L]][[1L]]$SV <- NULL
  cond <- expect_dl_error(p$descriptor$thaw(tam), "engine.dessem_svm_thaw_failed")
  expect_identical(cond$exit_code, 9L); expect_identical(cond$class_name, "integrity")
})

test_that("F5 membro nao-MED: a variante do membro (LIN_* => MAX) chega a engenharia de features e ao treino, nao o MED do run.yaml", {
  st <- dt_state(); p <- st$p
  expect_identical(p$spec$params$exogena, "MED")                   # placeholder do run.yaml
  amax <- dt_train(p, 9L, exogena = "MAX")
  expect_identical(amax$stamp$variant, "MAX"); expect_identical(amax$stamp$params$exogena, "MAX")
  pl <- amax$payload
  expect_identical(pl$variant, "MAX"); expect_identical(pl$model_key, "Linear.TmpMaxima")
  expect_identical(pl$head_recipe$model_key, "Linear.TmpMaxima")
  expect_identical(pl$head_recipe$config_mdl, st$eng$getConfigModel("Linear.TmpMaxima"))
  expect_false(identical(pl$head_recipe$config_mdl, st$eng$getConfigModel("Linear.TmpMedia")))
  expect_false(identical(dt_sha(amax), dt_sha(st$a09)))
  ## variante invalida aborta antes de qualquer treino
  cond <- tryCatch(dt_train(p, 9L, exogena = "XYZ"), prevcarga_error = function(e) e)
  expect_identical(cond$id, "plugin.dessem_linear_bad_exogena")
})

test_that("fontes de DL.6: so definicoes, sem relogio nem aritmetica de hora (grep de fonte)", {
  for (f in c("training.R", "freeze.R")) {
    src <- sub("#.*$", "", readLines(file.path(dl_models(), "dessem-linear", "R", f), warn = FALSE))
    expect_false(any(grepl("Sys\\.time|Sys\\.getenv|[+-]\\s*3600|difftime|with_tz|as\\.POSIX|<<-", src, perl = TRUE)))
  }
})
