## test-nowcast.R -- RD.5 (AC6 do epico, `nowcast_so_no_infer`): a cabeca de nowcast (DM de O com `mm`) so existe no INFER e so
## com hora cheia de O; o TREINO nunca a roda. Deck sintetico do fixture_store (460 dias, SECO-fixture, feriado fixo em O+5),
## leitor do M2 (`truncate_at` para o corte da manha), despachante da fundacao; o artefato de O e o MESMO nos tres cortes.
##  - 00:00: nenhuma hora cheia de O => sem cabeca (0 chamadas, 0 ajustes de SVM, `head_used` FALSE);
##  - 09:00 e 12:00: 1 cabeca cada (1 ajuste de SVM), previsoes distintas entre si e do 00:00;
##  - FORBIDDEN: carga de O com interval_end > available_until perturbada (x1.30 + 1) so no store => Delta <= 1e-5;
##  - CONTROL: carga 08:00-09:00 de O (corte 09:00) muda O e O+1; `.Random.seed` do chamador identical() antes e depois.

test_that("[PLUGINS] nowcast_so_no_infer: 00:00 sem cabeca; 09:00 e 12:00 com cabeca e previsoes distintas; RNG do chamador intacto", {
  st <- rd5_store(); art <- rd5_art("MED"); art0 <- art
  eng <- parent.env(rd_fx_plugin()$env)
  cu <- lapply(c("00:00", "09:00", "12:00"), function(h) rd5_cuts(st, h)); names(cu) <- c("00", "09", "12")
  expect_identical(cu[["00"]]$available_until, rd5_au("00:00"))
  hs <- rd5_head_spy(); sp <- rd2_spy(eng); on.exit(sp$restore(), add = TRUE)
  set.seed(7L); before <- get(".Random.seed", globalenv())
  e00 <- rd5_infer(art, st, cu[["00"]])
  expect_identical(hs$head(), 0L)                                      # espiao em rd_dm_nowcast: nenhuma cabeca
  expect_identical(sp$log$n, 0L)                                       # e nenhum ajuste de SVM
  expect_false(e00$meta$head_used)
  e09 <- rd5_infer(art, st, cu[["09"]])
  expect_identical(hs$head(), 1L); expect_identical(sp$log$n, 1L)
  e12 <- rd5_infer(art, st, cu[["12"]])
  expect_identical(hs$head(), 2L); expect_identical(sp$log$n, 2L)
  expect_true(e09$meta$head_used); expect_true(e12$meta$head_used)
  expect_identical(get(".Random.seed", globalenv()), before)           # o despachante restaura o gerador do chamador
  expect_identical(art, art0)                                          # o infer nao altera o artefato
  ## as tres previsoes diferem nos interval_end comuns
  d <- function(a, b) { x <- rd5_common(a$delivery, b$delivery); expect_gt(x$n, 400L); max(abs(x$a - x$b)) }
  expect_gt(d(e09, e12), RD5_TOL)
  expect_gt(d(e00, e09), RD5_TOL)
  expect_gt(d(e00, e12), RD5_TOL)
  cat(sprintf("\n[RD.5 medida] nowcast: max|Delta| 09:00 x 12:00 = %.3g; 00:00 x 09:00 = %.3g; 00:00 x 12:00 = %.3g MW\n",
              d(e09, e12), d(e00, e09), d(e00, e12)))
})

test_that("[PLUGINS] nowcast_so_no_infer: o treino so roda a cabeca de REFERENCIA (descartada) e o artefato nao depende do corte intradiario", {
  st <- rd5_store(); art09 <- rd5_art("MED")
  hs <- rd5_head_spy()
  cu12 <- rd5_cuts(st, "12:00")
  a12 <- rd5_train(st, cu12)                                           # treino real no corte 12:00 (sem cache)
  ## unica chamada do treino = a cabeca de referencia (mm no corte fixo 12:00, descartada: so posiciona o RNG, RD.2); a
  ## cabeca de nowcast do infer (mm do corte real) nunca entra no treino
  expect_identical(hs$calls(), 1L)
  expect_identical(rd5_sha(a12), rd5_sha(art09))                       # o corte intradiario nao entra no artefato
  expect_identical(a12, art09)
})

test_that("[PLUGINS] nowcast_so_no_infer: FORBIDDEN carga de O com interval_end > available_until perturbada => Delta <= 1e-5", {
  st <- rd5_store(); art <- rd5_art("MED"); cu <- rd5_cuts(st, "09:00"); au <- cu$available_until
  base <- rd5_infer(art, st, cu)
  ## alcance: a carga alem do corte existe no store e a perturbacao a muda (o leitor so nao a entrega)
  forb <- rd5_perturb(st, "load", after = au - 1)
  raw0 <- rd_lib$prevcarga_read_load(rd5_ctx(st), RD5_AREA, from = st$O, to = st$O)
  raw1 <- rd_lib$prevcarga_read_load(rd5_ctx(forb), RD5_AREA, from = st$O, to = st$O)
  late <- raw0$interval_end > au
  expect_gt(sum(late), 0L)
  expect_true(all(raw1$load_mwh[late] != raw0$load_mwh[late]))
  expect_identical(raw1$load_mwh[!late], raw0$load_mwh[!late])
  pan <- rd5_panel(forb, cu, "infer")
  expect_true(max(pan$load$interval_end) <= au)                        # o leitor corta; o plugin nunca a ve
  out <- rd5_infer(art, forb, cu)
  expect_lte(rd5_delta(base$delivery, out$delivery), RD5_TOL)
  expect_identical(out$delivery, base$delivery)
})

test_that("[PLUGINS] nowcast_so_no_infer: CONTROL carga 08:00-09:00 de O (corte 09:00) muda O e O+1", {
  st <- rd5_store(); art <- rd5_art("MED"); cu <- rd5_cuts(st, "09:00")
  base <- rd5_infer(art, st, cu)
  man <- rd5_perturb(st, "load", after = rd5_au("07:59"), at_or_before = rd5_au("08:30"))
  raw0 <- rd_lib$prevcarga_read_load(rd5_ctx(st), RD5_AREA, from = st$O, to = st$O, origin = st$O, truncate_at = cu$available_until)
  raw1 <- rd_lib$prevcarga_read_load(rd5_ctx(man), RD5_AREA, from = st$O, to = st$O, origin = st$O, truncate_at = cu$available_until)
  expect_identical(sum(raw0$load_mwh != raw1$load_mwh), 2L)            # as duas meias horas da hora 08:00-09:00
  ctl <- rd5_infer(art, man, cu)
  dd <- function(day) {
    ie <- base$delivery$interval_end
    k <- ie > rd5_day_start(st$O + day) & ie <= rd5_day_start(st$O + day + 1L)
    max(abs(base$delivery$y_pred[k] - ctl$delivery$y_pred[k]))
  }
  expect_gt(dd(0L), RD5_TOL)                                           # O (a hora entra no mm de O)
  expect_gt(dd(1L), RD5_TOL)                                           # O+1 (o ARIMA re-ancorado em O propaga)
  cat(sprintf("\n[RD.5 medida] nowcast CONTROL 08:00-09:00: max|Delta| O = %.3g; O+1 = %.3g MW\n", dd(0L), dd(1L)))
})
