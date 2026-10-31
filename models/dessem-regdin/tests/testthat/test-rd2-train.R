## test-rd2-train.R -- RD.2: treino ate O-1 (ADR-0023), RNG na posicao do oraculo, freeze sem mable. Loader real, motor real,
## painel sintetico. O treino completo (240 SVMs de perfil: 10 dias do motor) e feito uma vez por corte e reaproveitado; os controles usam so os 2
## primeiros dias-alvo (a ordem do RNG e sequencial, logo a posicao do gerador nesses dias e a do treino completo).

test_that("[PLUGINS] treino_freeze: ordem do oraculo: 1 cabeca de referencia (com mm) + 10 x 24 perfis = 241 ajustes; RNG parte de set.seed(seed); sem DM no artefato", {
  st <- rd2_state(); lg <- st$log; O <- RD2_O
  expect_identical(lg$n, 241L)
  expect_identical(which(grepl("\\bmm\\b", lg$fml, perl = TRUE)), 1L)
  pl <- st$a09$payload
  expect_setequal(names(pl), c("svm_perfil", "norm", "config_mdl", "day_types", "dias", "train_window", "arima_fingerprint", "seed"))
  expect_identical(names(pl$svm_perfil), format(O + 0:9))
  expect_true(all(vapply(pl$svm_perfil, length, 1L) == 24L))
  set.seed(42L); fresh <- get(".Random.seed", globalenv())
  expect_identical(lg$state[[1L]], fresh)                    # a cabeca parte de set.seed(42), como o oraculo
  expect_false(identical(lg$state[[2L]], fresh))             # e consome o gerador antes do perfil de O
  expect_true(st$caller_same)                                # o despachante restaura o .Random.seed do chamador
  expect_identical(pl$train_window$end, O - 1L)
  expect_identical(pl$train_window$start, lubridate::ymd("2023-11-01"))
})

test_that("[PLUGINS] treino_freeze: AC4: sha256 invariante ao corte (09:00 x 12:00); dia O no painel aborta; CONTROL perturbar O-1 muda", {
  st <- rd2_state(); p <- st$p
  a12 <- rd2_train(p, 12L)
  expect_identical(rd2_sha(a12), rd2_sha(st$a09))
  expect_identical(a12, st$a09)
  ## o treino nao enxerga o dia O: o despachante recusa carga de O no painel de treino (nada e cortado)
  pan <- rd2_panel(until = rd2_au(9L))
  cond <- tryCatch(rd2_train(p, 9L, pan = pan), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error"); expect_identical(cond$id, "plugin.train_beyond_cut")
  ## e o proprio estagio recusa um deck com linha de O (contract, exit 2) sem ajustar nenhum SVM
  f <- rd2_feats(p, rd2_panel(until = rd2_au(9L)), 9L)
  spy <- rd2_spy(st$eng); on.exit(spy$restore(), add = TRUE)
  cond2 <- tryCatch(rd_with_cli(p$env$rd_training(f, rd2_spec()$params, 42L)), prevcarga_error = function(e) e)
  expect_identical(cond2$id, "plugin.regdin_train_panel_has_origin")
  expect_identical(cond2$exit_code, 2L); expect_identical(cond2$class_name, "contract")
  expect_identical(spy$log$n, 0L)
  spy$restore()
  ## FORBIDDEN (2 dias-alvo): perturbar a previsao de clima de O nao muda o sha256; CONTROL: perturbar uma hora de O-1 muda
  base <- rd2_sha2(p, rd2_fit2(p))
  pw <- rd2_panel(); pw$weather_forecast[lubridate::as_date(valid_time) == RD2_O, temperature := temperature + 3]
  expect_identical(rd2_sha2(p, rd2_fit2(p, pw)), base)
  pc <- rd2_panel()
  k <- which(lubridate::as_date(pc$load$interval_start) == RD2_O - 1L & lubridate::hour(pc$load$interval_start) == 15L)
  pc$load[k, load_mwh := load_mwh + 400]
  expect_false(identical(rd2_sha2(p, rd2_fit2(p, pc)), base))
})

test_that("[PLUGINS] treino_freeze: AC3: mm de referencia com valores trocados => mesmo sha256 e mesma posicao de RNG; CONTROL sem a cabeca desloca o RNG", {
  st <- rd2_state(); p <- st$p; env <- p$env
  expect_null(env$plugin_descriptor()$rng_deviation)
  spy <- rd2_spy(st$eng)
  a <- rd2_fit2(p); st_a <- spy$log$state; n_a <- spy$log$n
  expect_identical(n_a, 49L)
  expect_identical(st_a[1:2], st$log$state[1:2])             # o treino truncado repete a posicao do RNG do completo
  sha_a <- rd2_sha2(p, a)
  ## FORBIDDEN: mesmos numeros de linhas, valores diferentes
  spy$log$state <- list(); spy$log$n <- 0L
  n_ref <- 0L
  b <- rd2_with_plugin_fn("rd_reference_mm", function(real) function(...) {
    n_ref <<- n_ref + 1L; m <- real(...); m[, mm := rev(mm) * 1.7 + 321]; m[]
  }, rd2_fit2(p))
  expect_gt(n_ref, 0L)
  expect_identical(spy$log$state, st_a)                      # o gerador chega igual a cada um dos 49 ajustes
  if (!identical(rd2_sha2(p, b), sha_a))
    expect_false(is.null(env$plugin_descriptor()$rng_deviation),
                 label = "RNG depende de mm: ativar o plano B (descriptor$rng_deviation) e listar no A/B (M8)")
  else expect_identical(rd2_sha2(p, b), sha_a)
  ## CONTROL: sem a cabeca de referencia o gerador chega deslocado a todos os perfis
  spy$log$state <- list(); spy$log$n <- 0L
  c0 <- rd2_with_plugin_fn("rd_dm_nowcast", function(real) function(...)
    list(infer = data.table::data.table(Data = lubridate::as_date(character()), Carga.infer = numeric())), rd2_fit2(p))
  st_c <- spy$log$state
  spy$restore()
  expect_identical(spy$log$n, 48L)
  expect_false(any(grepl("\\bmm\\b", spy$log$fml, perl = TRUE)))
  expect_identical(st_c[[1L]], st_a[[1L]])
  expect_false(any(mapply(identical, st_c, st_a[-1L])))
  expect_true(is.list(c0$svm_perfil))
})

test_that("[PLUGINS] treino_freeze: AC3: SVMs de perfil de O e O+1 batem o .rd_fit_pass do oraculo (mm real, corte 12:00) com max|Delta| <= 1e-5", {
  skip_if_not(file.exists(RD2_ORACLE_FIT) && file.exists(RD2_ORACLE_ENG), "oraculo ausente")
  st <- rd2_state(); p <- st$p; eng <- st$eng; O <- RD2_O
  pan12 <- rd2_panel(until = rd2_au(12L))
  fe <- rd2_feats(p, pan12, 12L)
  mm <- p$env$rd_morning_means(rd_lib$to_hourly(pan12)$load, rd2_au(12L), RD_TZ)
  oe <- rd2_oracle_wrapper(eng)
  suppressPackageStartupMessages(sys.source(RD2_ORACLE_FIT, envir = oe))
  proc <- fe$processado; proc$DIASPREV <- proc$DIASPREV[1:2]
  spy_o <- rd2_spy(eng)
  ora <- suppressWarnings(suppressMessages(oe$.rd_fit_pass(proc, fe$config_mdl, seed = 42L, mm = mm, origin = O)))
  st_o <- spy_o$log$state; spy_o$restore()
  ## treino do plugin sobre o painel de TREINO (sem O), truncado nos mesmos 2 dias
  spy_m <- rd2_spy(eng)
  mine <- rd2_fit2(p)
  st_m <- spy_m$log$state; spy_m$restore()
  expect_identical(st_m, st_o)                                # a mesma posicao de RNG em cada um dos 49 ajustes
  bm <- function(m) if (inherits(m, "tune")) m$best.model else m
  dl <- c()
  for (d in format(O + 0:1)) for (h in 1:24) {
    a <- bm(mine$svm_perfil[[d]][[h]]); b <- bm(ora$svm_perfil[[d]][[h]])
    expect_identical(a$kernel, b$kernel)
    dl <- c(dl, max(abs(a$coefs - b$coefs)), abs(a$rho - b$rho), max(abs(a$fitted - b$fitted)))
  }
  expect_lte(max(dl), 1e-5)
  ## CONTROL: sem a cabeca de referencia o RNG chega deslocado e as posicoes deixam de bater com o oraculo
  spy_c <- rd2_spy(eng)
  ctl <- rd2_with_plugin_fn("rd_dm_nowcast", function(real) function(...)
    list(infer = data.table::data.table(Data = lubridate::as_date(character()), Carga.infer = numeric())), rd2_fit2(p))
  st_c <- spy_c$log$state; spy_c$restore()
  expect_false(identical(st_c[-1L], st_o[-1L]))
  dc <- c()
  for (d in format(O + 0:1)) for (h in 1:24)
    dc <- c(dc, max(abs(bm(ctl$svm_perfil[[d]][[h]])$fitted - bm(ora$svm_perfil[[d]][[h]])$fitted)))
  expect_gt(max(dc), 1e-5)
})

test_that("[PLUGINS] treino_freeze: plano B: com rng_deviation o modo e reseed (aviso), set.seed re-fixado antes do perfil e sem cabeca; CONTROL preserve", {
  st <- rd2_state(); p <- st$p; env <- p$env
  expect_identical(env$rd_rng_mode(deviation = NULL), "preserve")
  w <- NULL
  mode <- withCallingHandlers(rd_with_cli(env$rd_rng_mode(deviation = "consumo de RNG diverge do oraculo")),
                              warning = function(x) { w <<- x; invokeRestart("muffleWarning") })
  expect_identical(mode, "reseed")
  expect_identical(w$id, "warning.regdin_rng_deviation")
  w2 <- NULL
  withCallingHandlers(rd_with_cli(env$rd_rng_mode()), warning = function(x) { w2 <<- x; invokeRestart("muffleWarning") })
  expect_null(w2)
  ## treino em reseed: o gerador volta a set.seed(42) imediatamente antes do 1o perfil (que, com a cabeca de referencia
  ## ajustada antes, chegaria em outra posicao)
  d0 <- env$plugin_descriptor
  assign("plugin_descriptor", function() { d <- d0(); d$rng_deviation <- "desvio de teste"; d }, envir = env)
  on.exit(assign("plugin_descriptor", d0, envir = env), add = TRUE)
  spy <- rd2_spy(st$eng)
  rd_with_cli(suppressWarnings(suppressMessages(rd2_fit2(p))))
  spy$restore()
  set.seed(42L); fresh <- get(".Random.seed", globalenv())
  expect_identical(spy$log$n, 49L)                            # a cabeca de referencia ainda e ajustada (so o RNG e re-fixado)
  expect_identical(spy$log$state[[2L]], fresh)                # 1o perfil parte do gerador recem-semeado
  expect_false(identical(spy$log$state[[2L]], st$log$state[[2L]]))
})

test_that("[PLUGINS] treino_freeze: AC5: freeze puro e deterministico; thaw prediz identico; carimbos; CONTROL relogio e adulteracao", {
  st <- rd2_state(); p <- st$p; O <- RD2_O; a <- st$a09
  expect_true(rd_lib$prevcarga_plugin_scan_pure(a))
  s <- a$stamp
  expect_setequal(names(s), c("model_id", "variant", "split", "granularity_min", "load_label", "timezone", "weather_issue_cutoff",
                              "params", "seed", "contract_version", "train_origin", "origin_day"))
  expect_identical(s$train_origin, O); expect_identical(s$origin_day, O); expect_identical(s$variant, "MED")
  expect_identical(s$model_id, "dessem-regdin")
  nm <- function(x) c(names(x), unlist(lapply(x, function(y) if (is.list(y) && !is.data.frame(y)) nm(y))))
  expect_false(any(c("available_until", "trained_at", "cut", "mm_used", "arima_dm") %in% nm(a)))
  fp <- a$payload$arima_fingerprint
  expect_setequal(names(fp), c("order", "coef", "dm_digest"))
  expect_length(fp$order, 6L); expect_true(is.numeric(fp$coef) && !is.null(names(fp$coef))); expect_match(fp$dm_digest, "^[0-9a-f]{64}$")
  ## thaw: predict identico ao do modelo em memoria (treino truncado sob espiao, sem congelar)
  spy <- rd2_spy(st$eng)
  mem <- rd2_fit2(p)
  spy$restore()
  d2 <- spy$log$data[[2L]]
  nd <- d2[seq_len(min(24L, nrow(d2))), ]
  bm <- function(m) if (inherits(m, "tune")) m$best.model else m
  fz <- p$env$rd_freeze(mem)
  th <- p$env$rd_thaw(fz)
  expect_true(all(vapply(th$svm_perfil, function(d) all(vapply(d, inherits, NA, what = "wsvm")), NA)))
  expect_identical(stats::predict(th$svm_perfil[[1L]][[1L]], nd), stats::predict(bm(mem$svm_perfil[[1L]][[1L]]), nd))
  expect_identical(stats::predict(th$svm_perfil[[2L]][[24L]], spy$log$data[[49L]][1:5, ]),
                   stats::predict(bm(mem$svm_perfil[[2L]][[24L]]), spy$log$data[[49L]][1:5, ]))
  ## determinismo: dois freezes do mesmo modelo, mesmo sha256
  expect_identical(rd2_sha(rd2_stamped(p, fz)), rd2_sha(rd2_stamped(p, p$env$rd_freeze(mem))))
  ## o freeze nao carrega mable mesmo que o modelo em memoria o traga
  expect_false("arima_dm" %in% names(p$env$rd_freeze(c(mem, list(arima_dm = "mable")))))
  ## CONTROL: um campo de relogio muda o sha256
  clk <- fz; clk$trained_at <- lubridate::force_tz(lubridate::as_datetime(O), "UTC")
  expect_false(identical(rd2_sha(rd2_stamped(p, clk)), rd2_sha(rd2_stamped(p, fz))))
  ## CONTROL: artefato adulterado => integrity (exit 9)
  tam <- fz; tam$svm_perfil[[1L]][[1L]]$SV <- NULL
  c1 <- tryCatch(rd_with_cli(p$env$rd_thaw(tam)), prevcarga_error = function(e) e)
  expect_identical(c1$id, "engine.dessem_svm_thaw_failed"); expect_identical(c1$exit_code, 9L); expect_identical(c1$class_name, "integrity")
  tam2 <- fz; tam2$svm_perfil[[1L]] <- tam2$svm_perfil[[1L]][1:23]
  c2 <- tryCatch(rd_with_cli(p$env$rd_thaw(tam2)), prevcarga_error = function(e) e)
  expect_identical(c2$id, "plugin.regdin_artifact_tampered"); expect_identical(c2$exit_code, 9L)
  tam3 <- fz; tam3$arima_fingerprint <- NULL
  expect_identical(tryCatch(rd_with_cli(p$env$rd_thaw(tam3)), prevcarga_error = function(e) e)$id, "plugin.regdin_artifact_tampered")
})

test_that("[PLUGINS] treino_freeze: AC6: .rd_arima_fingerprint = ordem + coeficientes nomeados + sha256 da serie DM; repetivel; CONTROL perturbar O-1", {
  st <- rd2_state(); p <- st$p; eng <- st$eng
  fit_fp <- function(pan) {
    fe <- rd2_feats(p, pan)
    base <- suppressWarnings(eng$execDemandaMediaTS(data.table::copy(fe$series$serieDM), fe$config_mdl))
    list(fp = p$env$.rd_arima_fingerprint(base$mdl$modeloTreinado, fe$series$serieDM), serieDM = fe$series$serieDM, fit = base$mdl$modeloTreinado)
  }
  a <- fit_fp(rd2_panel())
  expect_identical(names(a$fp$order), c("p", "d", "q", "P", "D", "Q"))
  expect_identical(names(a$fp$coef), fabletools::tidy(a$fit)$term)
  expect_identical(a$fp, st$a09$payload$arima_fingerprint)
  hist <- a$serieDM[Prev == FALSE, c("Data", "Carga", grep("^Exo", names(a$serieDM), value = TRUE)), with = FALSE]
  expect_identical(a$fp$dm_digest, digest::digest(hist, algo = "sha256"))
  expect_identical(fit_fp(rd2_panel())$fp, a$fp)              # repetir o fit da o mesmo fingerprint
  pc <- rd2_panel()
  k <- which(lubridate::as_date(pc$load$interval_start) == RD2_O - 1L & lubridate::hour(pc$load$interval_start) == 15L)
  pc$load[k, load_mwh := load_mwh + 400]
  expect_false(identical(fit_fp(pc)$fp$dm_digest, a$fp$dm_digest))
})

test_that("[PLUGINS] grep: RD.2 fontes: so definicoes, sem relogio, env global, atribuicao global nem aritmetica de hora (grep de fonte)", {
  for (f in c("helpers_nowcast.R", "training.R", "freeze.R")) {
    src <- sub("#.*$", "", readLines(file.path(rd_models(), "dessem-regdin", "R", f), warn = FALSE))
    expect_false(any(grepl("Sys\\.time|Sys\\.Date|Sys\\.getenv|[+-]\\s*3600|difftime|with_tz|force_tz|as\\.POSIX|<<-|globalenv|saveRDS|\\.rd_nowcast",
                           src, perl = TRUE)), info = f)
    expect_false(any(grepl("[\\x80-\\xFF]", readLines(file.path(rd_models(), "dessem-regdin", "R", f), warn = FALSE),
                           perl = TRUE, useBytes = TRUE)), info = paste(f, "ascii"))
  }
})
