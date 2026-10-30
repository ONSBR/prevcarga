## test-rd3-infer.R -- RD.3: infer do dessem-regdin (guarda de janela, refit do ARIMA com fingerprint, cabeca mm apos
## set.seed, forecast acoplado). Loader real, motor real, painel sintetico, despachante real da fundacao.

test_that("[RD.3] estagios reais registrados e artefato carrega a semente (nada de stub)", {
  st <- rd3_state(); p <- st$p; d <- p$descriptor
  expect_identical(d$stages$inference$fn, p$env$rd_inference)
  expect_identical(d$stages$inference_features$fn, p$env$rd_inference_features)
  expect_identical(d$stages$forecast$fn, p$env$rd_forecast)
  expect_identical(d$stages$training$fn, p$env$rd_training)
  expect_identical(st$med$payload$seed, 42L)
  expect_null(st$med$payload$svm_perfil[[format(RD2_O + 2L)]])           # dia especial: sem SVM
  expect_identical(st$med$payload$day_types[[format(RD2_O + 2L)]], 3L)
})

test_that("[RD.3] AC1: guarda de janela, FORBIDDEN/CONTROL e infer == forecast acoplado no dia D", {
  st <- rd3_state(); p <- st$p; au <- rd3_au(9L)
  e <- rd3_infer(p, st$med, au)
  expect_identical(dim(e$values), c(9L, 48L)); expect_false(anyNA(e$values))
  ## painel 30 dias mais curto => window_mismatch (exit 5) antes de qualquer ajuste
  short <- rd3_panel(until = au, days = RD2_D - 30L)
  spy <- rd2_spy(st$eng); on.exit(spy$restore(), add = TRUE)
  cond <- tryCatch(rd3_infer(p, st$med, au, pan = short), prevcarga_error = function(c) c)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_window_mismatch")
  expect_identical(cond$exit_code, 5L); expect_identical(cond$class_name, "window")
  expect_identical(spy$log$n, 0L)
  spy$restore()
  ## CONTROL: com o bypass (so teste) o mesmo painel entrega e muda a previsao
  s2 <- rd3_stage_inf(p, st$med, short, au)
  expect_error(rd_with_cli(p$env$plugin_infer(st$med$payload, s2$inf, s2$cuts)), class = "prevcarga_error")
  y_b <- rd_with_cli(suppressMessages(p$env$plugin_infer(st$med$payload, s2$inf, s2$cuts, .bypass_window_guard = TRUE)))
  expect_false("bypass" %in% names(formals(p$descriptor$stages$inference$fn)))
  expect_false(".bypass_window_guard" %in% names(p$descriptor))
  cat(sprintf("\n[RD.3 medida] bypass x janela exata: max|Delta y_pred| = %.3f MW\n", rd3_ypos(e, y_b)))
  expect_gt(rd3_ypos(e, y_b), 50)
  ## infer == forecast no dia D (09:00): o forecast acoplado treina e infere numa passada
  f <- rd3_forecast(p, au)
  expect_lte(max(abs(e$delivery$y_pred - f$delivery$y_pred)), 1e-5)
  expect_identical(e$values, f$values)
  expect_identical(nrow(e$delivery), nrow(f$delivery))
})

test_that("[RD.3] AC2: arima_fingerprint divergente aborta nomeando o campo/coeficiente; CONTROL original infere", {
  st <- rd3_state(); p <- st$p; au <- rd3_au(9L)
  ok <- rd3_infer(p, st$med, au)
  expect_true(is.list(ok))
  art <- st$med; nm <- names(art$payload$arima_fingerprint$coef)[2L]
  art$payload$arima_fingerprint$coef[[nm]] <- art$payload$arima_fingerprint$coef[[nm]] + 1e-3
  cond <- tryCatch(rd3_infer(p, art, au), prevcarga_error = function(c) c)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_arima_drift"); expect_identical(cond$class_name, "contract")
  expect_identical(cond$details$field, "coef"); expect_identical(cond$details$coef, nm)
  art2 <- st$med; art2$payload$arima_fingerprint$order[["p"]] <- art2$payload$arima_fingerprint$order[["p"]] + 1L
  cond2 <- tryCatch(rd3_infer(p, art2, au), prevcarga_error = function(c) c)
  expect_identical(cond2$id, "plugin.regdin_arima_drift"); expect_identical(cond2$details$field, "order")
  art3 <- st$med; art3$payload$arima_fingerprint$dm_digest <- "0"
  cond3 <- tryCatch(rd3_infer(p, art3, au), prevcarga_error = function(c) c)
  expect_identical(cond3$id, "plugin.regdin_arima_drift"); expect_identical(cond3$details$field, "dm_digest")
})

test_that("[RD.3] AC3: cabeca so com mm (00:00 nao aciona), set.seed antes, FORBIDDEN/CONTROL de perturbacao, RNG do chamador", {
  st <- rd3_state(); p <- st$p
  spy <- rd2_spy(st$eng); on.exit(spy$restore(), add = TRUE)
  run <- function(au, ...) { spy$log$n <- 0L; spy$log$state <- list(); e <- rd3_infer(p, st$med, au, ...); list(e = e, n = spy$log$n, st = spy$log$state) }
  r00 <- run(rd3_au(0L)); r09 <- run(rd3_au(9L)); r12 <- run(rd3_au(12L))
  expect_identical(r00$n, 0L); expect_false(r00$e$meta$head_used)
  expect_identical(r09$n, 1L); expect_identical(r12$n, 1L)
  expect_true(r09$e$meta$head_used)
  set.seed(42L); fresh <- get(".Random.seed", globalenv())
  expect_identical(r09$st[[1L]], fresh)                                   # a cabeca parte de set.seed(artifact$seed)
  expect_gt(rd3_ymax(r00$e, r09$e), 1e-3); expect_gt(rd3_ymax(r09$e, r12$e), 1e-3); expect_gt(rd3_ymax(r00$e, r12$e), 1e-3)
  ## RNG do chamador
  set.seed(11); s0 <- get(".Random.seed", globalenv()); rd3_infer(p, st$med, rd3_au(9L)); s1 <- get(".Random.seed", globalenv())
  expect_identical(s0, s1)
  ## FORBIDDEN: a carga de O alem de available_until nunca chega (o despachante recusa); a meia hora 09:00-09:30 e ignorada
  cond <- tryCatch(rd3_infer(p, st$med, rd3_au(9L), pan = rd3_panel(until = rd3_au(10L))), prevcarga_error = function(c) c)
  expect_identical(cond$id, "plugin.infer_beyond_cut")
  expect_identical(rd3_infer(p, st$med, rd3_au(9L), pan = rd3_panel(until = rd3_au(9L, 30L)))$delivery, r09$e$delivery)
  base <- run(rd3_au(9L, 30L))$e
  pert <- run(rd3_au(9L, 30L), perturb = function(s) lubridate::as_date(s) == RD2_O & lubridate::hour(s) == 9L & lubridate::minute(s) == 0L)$e
  expect_lte(rd3_ymax(base, pert), 1e-5)
  ## CONTROL: perturbar 08:00-09:00 (hora completa) muda y_pred de O+1
  pc <- run(rd3_au(9L), perturb = function(s) lubridate::as_date(s) == RD2_O & lubridate::hour(s) == 8L)$e
  expect_gt(rd3_ymax(r09$e, pc), 1e-3)
  o1 <- as.Date(r09$e$delivery$interval_end - lubridate::minutes(1)) == RD2_O + 1L
  expect_gt(max(abs(r09$e$delivery$y_pred[o1] - pc$delivery$y_pred[o1])), 1e-3)
})

test_that("[RD.3] AC4: 09:00 x 09:30 identicos nos comuns; grade de entrega, matriz 9 x 48, RGD_MAX tambem", {
  st <- rd3_state(); p <- st$p
  for (v in c("med", "max")) {
    a <- rd3_infer(p, st[[v]], rd3_au(9L)); b <- rd3_infer(p, st[[v]], rd3_au(9L, 30L))
    expect_identical(a$values, b$values)
    com <- intersect(as.numeric(a$delivery$interval_end), as.numeric(b$delivery$interval_end))
    ia <- as.numeric(a$delivery$interval_end) %in% com; ib <- as.numeric(b$delivery$interval_end) %in% com
    expect_identical(a$delivery$y_pred[ia], b$delivery$y_pred[ib])
    expect_identical(a$delivery$interval_end[[1L]], rd3_au(9L, 30L))
    expect_identical(b$delivery$interval_end[[1L]], rd3_au(10L))
    expect_identical(a$delivery$interval_end[[nrow(a$delivery)]], lubridate::force_tz(lubridate::as_datetime(RD2_O + 9L), RD_TZ))
    expect_identical(a$delivery$interval_end, rd_lib$prevcarga_delivery_grid(rd3_cuts(rd3_au(9L)), 9L))
    expect_identical(dim(a$values), c(9L, 48L)); expect_true(is.numeric(a$values)); expect_false(anyNA(a$values))
    expect_identical(a$layout, p$descriptor$native_layout); expect_identical(a$granularity_min, 60L)
    ## CONTROL: perturbar a hora 08:00-09:00 muda a saida
    c1 <- rd3_infer(p, st[[v]], rd3_au(9L), perturb = function(s) lubridate::as_date(s) == RD2_O & lubridate::hour(s) == 8L)
    expect_gt(rd3_ymax(a, c1), 1e-3)
  }
})

test_that("[RD.3] AC5: dia especial recomputado, artefato alheio aborta, infer == forecast (MED 12:00 e MAX 09:00), CONTROL sem cabeca", {
  st <- rd3_state(); p <- st$p
  ## MED, corte 12:00: infer do artefato (treinado com corte 09:00) == forecast acoplado
  a12 <- rd3_infer(p, st$med, rd3_au(12L)); f12 <- rd3_forecast(p, rd3_au(12L))
  cat(sprintf("\n[RD.3 medida] MED infer x forecast: max|Delta| = %.3g\n", rd3_ypos(a12, f12)))
  expect_lte(rd3_ypos(a12, f12), 1e-5)
  expect_identical(a12$values, f12$values)
  amx <- rd3_infer(p, st$max, rd3_au(9L)); fmx <- rd3_forecast(p, rd3_au(9L), "MAX")
  cat(sprintf("[RD.3 medida] MAX infer x forecast: max|Delta| = %.3g\n", rd3_ypos(amx, fmx)))
  expect_lte(rd3_ypos(amx, fmx), 1e-5)
  ## dia especial (O+2): sem NA, perfil de RL_perfilFeriado
  expect_false(anyNA(a12$values)); expect_false(anyNA(amx$values))
  ## artefato de outra origem ou variante => contract via prevcarga_plugin_check_artifact
  other <- st$med; other$stamp$origin_day <- RD2_O - 1L
  cond <- tryCatch(rd3_infer(p, other, rd3_au(9L)), prevcarga_error = function(c) c)
  expect_identical(cond$id, "plugin.artifact_mismatch"); expect_identical(cond$class_name, "contract")
  alien <- st$med; alien$payload$dias <- format(RD2_O - 1L + 0:8)
  cond2 <- tryCatch(rd3_infer(p, alien, rd3_au(9L)), prevcarga_error = function(c) c)
  expect_identical(cond2$id, "plugin.regdin_artifact_mismatch"); expect_identical(cond2$class_name, "contract")
  ## variante do artefato: um artefato MAX com o carimbo do MED difere do MED
  expect_gt(rd3_ymax(rd3_infer(p, st$med, rd3_au(9L)), amx), 1e-5)
  ## CONTROL: treino SEM a cabeca de referencia desloca o RNG => infer com corte 12:00 diverge do forecast correto
  nohead <- rd2_with_plugin_fn("rd_reference_mm", function(real) function(...) { m <- real(...); m[0L] }, rd3_train(p, "MED"))
  c12 <- rd3_infer(p, nohead, rd3_au(12L))
  cat(sprintf("[RD.3 medida] sem cabeca de referencia: max|Delta| = %.3g MW\n", rd3_ypos(c12, f12)))
  expect_gt(rd3_ypos(c12, f12), 1e-5)
})

test_that("[RD.3] AC6: nenhuma leitura de ENV; variante so do artefato", {
  st <- rd3_state(); p <- st$p
  clean <- rd3_infer(p, st$med, rd3_au(9L))
  old <- Sys.getenv("EXOGENA", unset = NA)
  Sys.setenv(EXOGENA = "MAX")
  on.exit(if (is.na(old)) Sys.unsetenv("EXOGENA") else Sys.setenv(EXOGENA = old), add = TRUE)
  dirty <- rd3_infer(p, st$med, rd3_au(9L))
  expect_identical(dirty, clean)
  expect_identical(clean$variant, "MED"); expect_identical(clean$granularity_min, 60L)
  expect_identical(clean$layout, p$descriptor$native_layout)
  ## CONTROL: um artefato MAX produz values diferentes
  expect_gt(max(abs(rd3_infer(p, st$max, rd3_au(9L))$values - clean$values)), 1e-5)
  ## o codigo do plugin nao le ambiente
  src <- unlist(lapply(list.files(file.path(rd_models(), "dessem-regdin", "R"), full.names = TRUE), readLines, warn = FALSE))
  expect_false(any(grepl("Sys\\.getenv|Sys\\.setenv", sub("#.*$", "", src))))
})

test_that("[RD.3] meta$warnings recebe as guardas do motor emitidas na cabeca mm e no perfil (sem o gap DL8-GAP-1)", {
  st <- rd3_state(); p <- st$p; env <- p$env
  hit <- rd2_with_plugin_fn("rd_dm_nowcast", function(real) function(...) {
    message(st$eng$dessem_engine_guard("warning.engine_cross_clamped", from = 1L, to = 2L, n_eff = 3L)); real(...)
  }, rd3_infer(p, st$med, rd3_au(9L)))
  expect_true("warning.engine_cross_clamped" %in% hit$meta$warnings)
  none <- rd3_infer(p, st$med, rd3_au(9L))
  expect_false("warning.engine_cross_clamped" %in% none$meta$warnings)
})
