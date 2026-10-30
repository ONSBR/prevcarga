## test-mm-head.R -- RD.4 (AC5): premissa do RNG preservado e regra do `mm` por corte. O treino ajusta a cabeca DM(O) de
## REFERENCIA (mm no corte de referencia 12:00) so para consumir o gerador na posicao do oraculo e a descarta. Prova:
## (1) trocar os VALORES do mm de referencia (mesmas linhas) nao muda o sha256 nem a posicao do RNG; (2) CONTROL: o passe de
## ajuste sem a cabeca de referencia (mm_ref vazio, direto em .rd_fit_pass) muda o perfil de O+1; (3) se (1) cair, so passa
## com `rng_deviation` NAO-nulo (plano B). O 2o bloco cobre a regra de existencia do mm: decidida pelo dado (hora cheia de O
## com rotulo <= corte), nunca pelo relogio do corte no frame de modelagem: 00:00 e 00:30 sem cabeca; 01:00 e 01:30 com cabeca.
## Treinos truncados nos 2 primeiros dias-alvo (a ordem do RNG e sequencial: a posicao nesses dias e a do treino completo).

test_that("[PLUGINS] mm_head_rng: valores do mm de referencia trocados => mesmo sha256 e mesma posicao de RNG (plano B so se cair)", {
  st <- rd2_state(); p <- st$p; env <- p$env
  expect_null(env$plugin_descriptor()$rng_deviation)
  spy <- rd2_spy(st$eng); on.exit(spy$restore(), add = TRUE)
  a <- rd2_fit2(p); st_a <- spy$log$state
  expect_identical(spy$log$n, 49L)                                    # cabeca de referencia + 2 dias x 24 perfis
  expect_identical(grep("\\bmm\\b", spy$log$fml, perl = TRUE), 1L)    # so o 1o ajuste (a cabeca) leva o mm
  sha_a <- rd2_sha2(p, a)
  ## FORBIDDEN: mesmas linhas, valores diferentes (corte 09:00 em vez do 12:00 de referencia: outros valores, mesmo no de linhas)
  spy$log$state <- list(); spy$log$n <- 0L; n_ref <- 0L
  b <- rd2_with_plugin_fn("rd_reference_mm", function(real) function(...) {
    n_ref <<- n_ref + 1L; m <- real(...); m[, mm := rev(mm) * 1.7 + 321]; m[]
  }, rd2_fit2(p))
  expect_gt(n_ref, 0L)                                                # a troca valeu: o treino leu o mm de referencia
  expect_identical(spy$log$state, st_a)                               # o gerador chega igual a cada um dos 49 ajustes
  same <- identical(rd2_sha2(p, b), sha_a)
  cat(sprintf("\n[RD.4 medida] premissa do RNG: sha256 %s com mm de referencia trocado; plano B %s\n",
              if (same) "IGUAL" else "DIFERENTE", if (is.null(env$plugin_descriptor()$rng_deviation)) "nao acionado" else "acionado"))
  if (!same && is.null(env$plugin_descriptor()$rng_deviation))
    fail("premissa do RNG caiu: ative o plano B (rng_deviation) e liste o desvio no A/B do M8")
  expect_true(same || !is.null(env$plugin_descriptor()$rng_deviation))
  ## CONTROL: .rd_fit_pass direto sem a cabeca de referencia (mm_ref vazio) => o perfil de O+1 muda (> 1e-5)
  f <- rd2_trunc(rd2_feats(p, rd2_panel(), 9L))
  f$norm <- env$.rd_norm_constants(f$processado$SERIECOMP, f$config_mdl)
  mm_ref <- env$rd_reference_mm(env$.rd_deck_panel(f$deck$CARGAHIST), env$plugin_constants()$reference_mm_hour, f$deck$tz)
  expect_gt(nrow(mm_ref), 0L)
  spy$log$state <- list(); spy$log$n <- 0L
  with_head <- suppressWarnings(rd_with_cli(env$.rd_fit_pass(f, 42L, mm_ref)))
  expect_identical(spy$log$n, 49L)
  spy$log$state <- list(); spy$log$n <- 0L
  no_head <- suppressWarnings(rd_with_cli(env$.rd_fit_pass(f, 42L, mm_ref[0L])))
  expect_identical(spy$log$n, 48L)                                    # so a cabeca falta
  expect_false(any(grepl("\\bmm\\b", spy$log$fml, perl = TRUE)))
  bm <- function(m) if (inherits(m, "tune")) m$best.model else m
  d1 <- format(RD2_O + 1L)
  dl <- vapply(1:24, function(h) max(abs(bm(with_head$svm_perfil[[d1]][[h]])$fitted - bm(no_head$svm_perfil[[d1]][[h]])$fitted)), 1)
  cat(sprintf("[RD.4 medida] sem a cabeca de referencia: max|Delta| no perfil de O+1 = %.3g\n", max(dl)))
  expect_gt(max(dl), 1e-5)
})

test_that("[PLUGINS] mm_head_corte: o mm de O existe so com hora cheia de O (00:00 e 00:30 sem cabeca; 01:00 e 01:30 com cabeca)", {
  st <- rd3_state(); p <- st$p
  spy <- rd2_spy(st$eng); on.exit(spy$restore(), add = TRUE)
  O <- RD2_O
  run <- function(minute_of_day, perturb = NULL) {
    au <- rd2_au(0L) + lubridate::minutes(minute_of_day)
    cu <- rd_fx_cuts(available_until = au)
    pan <- rd_fx_panel(until = au)
    if (!is.null(perturb)) pan$load[perturb(interval_start), load_mwh := load_mwh + 500]
    spy$log$n <- 0L
    e <- rd_infer(st$med, pan, cu)
    s <- rd3_stage_inf(p, st$med, pan, au)
    list(e = e, n = spy$log$n, mm = s$inf$mm, au = au)
  }
  r00 <- run(0L); r30 <- run(30L); r60 <- run(60L); r90 <- run(90L)
  ## existencia da cabeca: so as horas COMPLETAS de O contam (decidido pelo dado, nao pelo relogio do frame de modelagem)
  expect_identical(c(r00$n, r30$n, r60$n, r90$n), c(0L, 0L, 1L, 1L))     # ajustes de SVM da cabeca por infer
  expect_identical(c(r00$e$meta$head_used, r30$e$meta$head_used, r60$e$meta$head_used, r90$e$meta$head_used),
                   c(FALSE, FALSE, TRUE, TRUE))
  expect_false(O %in% r00$mm$Data); expect_false(O %in% r30$mm$Data)
  expect_true(O %in% r60$mm$Data); expect_true(O %in% r90$mm$Data)
  ## mm[O] em 01:00/01:30 = a hora 00 de O (o unico dado completo da manha); 01:30 so adiciona meia hora isolada (descartada)
  h <- data.table::data.table(rd_lib$to_hourly(rd_fx_panel(until = rd2_au(0L) + lubridate::hours(2L)))$load)
  h0 <- h[interval_start == rd2_au(0L)]$load_mwh
  expect_length(h0, 1L)
  expect_equal(r60$mm[Data == O]$mm, h0); expect_equal(r90$mm[Data == O]$mm, h0)
  expect_identical(r60$mm, r90$mm)
  ## 00:00 x 00:30 sao identicos (nenhuma cabeca; a carga de O ainda nao tem hora completa); 01:00 x 01:30 identicos nos comuns
  expect_lte(rd3_ymax(r00$e, r30$e), 1e-5)
  expect_lte(rd3_ymax(r60$e, r90$e), 1e-5)
  expect_gt(rd3_ymax(r30$e, r60$e), 1e-3)                                # a cabeca muda a previsao
  ## FORBIDDEN: a meia hora 01:00-01:30 (hora incompleta) perturbada nao muda nada; CONTROL: a hora 00-01 completa muda
  fb <- run(90L, function(s) s == rd2_au(1L))
  expect_lte(rd3_ymax(r90$e, fb$e), 1e-5)
  ct <- run(90L, function(s) lubridate::as_date(s) == O & lubridate::hour(s) == 0L)
  expect_gt(rd3_ymax(r90$e, ct$e), 1e-3)
  ## CONTROL do corte 00:30: a meia hora 00:00-00:30 (incompleta) nem chega ao painel de infer quando perturbada
  fb30 <- run(30L, function(s) s == rd2_au(0L))
  expect_lte(rd3_ymax(r30$e, fb30$e), 1e-5)
  ## unitario, frame de modelagem: o mesmo painel horario decide pela hora cheia (corte 00:00 do modelo = 01:00 de exibicao)
  mk <- function(h_end) data.table::data.table(interval_end = rd2_au(0L) + lubridate::hours(h_end), load_mwh = 100 + h_end)
  f <- p$env$rd_morning_means
  m_model <- f(data.table::rbindlist(list(mk(-24:-1), mk(0L))), rd2_au(0L), RD_TZ)    # frame de modelagem, corte 00:00
  expect_true(O %in% m_model$Data); expect_equal(m_model[Data == O]$mm, 100)
  expect_identical(nrow(f(mk(-24:-1), rd2_au(0L), RD_TZ)[Data == O]), 0L)               # sem hora de O: sem mm de O
})

test_that("[PLUGINS] mm_head_oracle: perfil do artefato vs caminho de infer nativo do oraculo <= 1e-5 (tier parity, golden foundation)", {
  rd_gate_tier("parity")
  if (exists("pc_gate_require_golden", mode = "function")) pc_gate_require_golden("dessem-regdin", "foundation")
  skip_if_not(file.exists(RD2_ORACLE_FIT) && file.exists(RD2_ORACLE_ENG), "[tier] oraculo ausente")
  st <- rd2_state(); p <- st$p; eng <- st$eng; O <- RD2_O
  pan12 <- rd2_panel(until = rd2_au(12L))
  fe <- rd2_feats(p, pan12, 12L)
  mm <- p$env$rd_morning_means(rd_lib$to_hourly(pan12)$load, rd2_au(12L), RD_TZ)
  oe <- rd2_oracle_wrapper(eng)
  suppressPackageStartupMessages(sys.source(RD2_ORACLE_FIT, envir = oe))
  proc <- fe$processado; proc$DIASPREV <- proc$DIASPREV[1:2]
  ora <- suppressWarnings(suppressMessages(oe$.rd_fit_pass(proc, fe$config_mdl, seed = 42L, mm = mm, origin = O)))
  mine <- rd2_fit2(p)
  bm <- function(m) if (inherits(m, "tune")) m$best.model else m
  dl <- c()
  for (d in format(O + 0:1)) for (h in 1:24) {
    a <- bm(mine$svm_perfil[[d]][[h]]); b <- bm(ora$svm_perfil[[d]][[h]])
    dl <- c(dl, max(abs(a$coefs - b$coefs)), abs(a$rho - b$rho), max(abs(a$fitted - b$fitted)))
  }
  expect_lte(max(dl), 1e-5)
})
