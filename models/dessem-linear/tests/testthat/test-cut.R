## test-cut.R -- DL.8 (AC6 e AC7 do epico): corte intradiario do dessem-linear. Molde de 4 bracos do honesty_gate.R (M3.2) sobre
## o deck sintetico (leitor -> despachante -> plugin; artefato de O ja treinado, so o infer varia):
##  - FORBIDDEN: carga com interval_end > available_until (so no store, o leitor nao a entrega) => Delta <= 1e-5;
##  - CONTROL: carga da manha (08:00-09:00) de O muda a entrega de O e de O+1 (mm[O] e a media prevista de O como LAG_1);
##  - available_until 09:00 x 09:30 => saidas iguais em interval_end >= 10:00 (meia hora isolada nao forma hora completa).

test_that("[PLUGINS] corte_intradiario: FORBIDDEN carga com interval_end > 12:00 => Delta <= 1e-5; CONTROL; molde HONESTO", {
  rg <- dl_fx_store(ragged = "12:00"); cuts <- dl_fx_cuts(rg); O <- cuts$origin_day
  au <- cuts$available_until
  expect_identical(au, dl_au("12:00"))
  art <- dl_base_art()
  ## ZZ1 tem carga alem do corte so no store; o leitor nao a entrega
  ipan <- dl_fx_panel(rg, cuts, "infer")
  expect_true(max(ipan$load$interval_end) <= au)
  ctx <- dl_fx_ctx(rg)
  raw <- dl_lib$prevcarga_read_load(ctx, DL8_AREA, from = O - 9L, to = O)
  expect_gt(sum(raw$interval_end > au), 0L)

  run <- function(s) dl_infer(art, s, cuts)$delivery
  base <- run(rg); clean2 <- run(rg)
  expect_identical(base, clean2)
  ## FORBIDDEN: tudo que esta apos o corte, perturbado no store
  forb_st <- dl_fx_perturb(rg, "load", after = au - 1, areas = DL8_AREA)
  raw_f <- dl_lib$prevcarga_read_load(dl_fx_ctx(forb_st), DL8_AREA, from = O - 9L, to = O)
  expect_true(all(raw_f$load_mwh[raw_f$interval_end > au] != raw$load_mwh[raw$interval_end > au]))   # alcance
  expect_lte(dl_delta(base, run(forb_st)), DL8_TOL)
  expect_identical(run(forb_st), base)
  ## CONTROL: a manha de O (08:00-09:00) e os dias <= O-1 mudam a entrega
  man <- dl_fx_perturb(rg, "load", after = dl_au("07:59") , at_or_before = dl_au("08:30"), areas = DL8_AREA)
  ctrl <- dl_infer(art, man, cuts)
  expect_gt(dl_delta(base, ctrl$delivery), DL8_TOL)
  expect_gt(dl_delta(base, run(dl_fx_perturb(rg, "load", after = dl_day_start(O - 1L) - 1, at_or_before = dl_day_start(O) - 1,
                                              areas = DL8_AREA))), DL8_TOL)

  ## molde M3.2: cut = available_until; painel y_true = carga bruta de ZZ1 (inclui a regiao proibida, so no store)
  gp <- data.frame(area_code = raw$area_code, interval_start = raw$interval_start, interval_end = raw$interval_end,
                   y_true = raw$load_mwh, stringsAsFactors = FALSE)
  run_fn <- function(p) {
    ch <- which(abs(p$y_true - gp$y_true) > 0)
    s2 <- if (!length(ch)) rg else dl_fx_perturb(rg, "load", after = min(p$interval_start[ch]) - lubridate::minutes(30L),
                                                  at_or_before = max(p$interval_start[ch]), areas = DL8_AREA)
    run(s2)
  }
  g <- dl_lib$prevcarga_honesty_gate(run_fn, gp, au, cuts$horizon_days)
  expect_identical(g$verdict, "HONESTO")
  expect_true(g$reach_ok)
  expect_lte(g$clean2_delta, DL8_TOL); expect_lte(g$forbidden_delta, DL8_TOL); expect_gt(g$control_delta, DL8_TOL)
  expect_gt(g$arms$forbidden$n_perturbed, 0L); expect_gt(g$arms$control$n_perturbed, 0L)

  ## painel com intervalo alem do corte entregue ao despachante => contract (confere, nao corta)
  e <- dl_err(dl_infer(art, rg, cuts, panel = within(ipan, {
    load <- data.table::rbindlist(list(load, raw[interval_end > au & area_code == DL8_AREA, names(load), with = FALSE]))
  })))
  expect_dl_class(e, "contract", 2L, id = "plugin.infer_beyond_cut")
})

test_that("[PLUGINS] corte_intradiario: manha de O (08:00-09:00) muda O e O+1; CONTROL do LAG_1 da media prevista", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art()
  base <- dl_infer(art, st, cuts)
  man <- dl_fx_perturb(st, "load", after = dl_au("07:59"), at_or_before = dl_au("08:30"))
  ctrl <- dl_infer(art, man, cuts)
  expect_gt(max(abs(ctrl$values[1L, ] - base$values[1L, ])), DL8_TOL)           # O
  expect_gt(max(abs(ctrl$values[2L, ] - base$values[2L, ])), DL8_TOL)           # O+1 (LAG_1 = DM prevista de O)
  ## mas a manha de ONTEM nao deixa de entrar: perturbar a manha de O-1 tambem muda (nao e so a ultima hora)
  ant <- dl_fx_perturb(st, "load", after = dl_au("07:59", DL8_O - 1L), at_or_before = dl_au("08:30", DL8_O - 1L))
  expect_gt(dl_delta(base$delivery, dl_infer(art, ant, cuts)$delivery), DL8_TOL)
})

test_that("[PLUGINS] corte_intradiario: available_until 09:00 x 09:30 => saidas iguais em interval_end >= 10:00; grade sem buraco", {
  s09 <- dl_fx_store("09:00"); s0930 <- dl_fx_store("09:30")
  c09 <- dl_fx_cuts(s09); c0930 <- dl_fx_cuts(s0930)
  expect_identical(c09$available_until, dl_au("09:00")); expect_identical(c0930$available_until, dl_au("09:30"))
  art <- dl_base_art()
  expect_identical(dl_panel_key(dl_fx_panel(s0930, c0930, "train")), dl_panel_key(dl_fx_panel(s09, c09, "train")))   # a meia hora nova nao entra no treino
  o09 <- dl_infer(art, s09, c09); o0930 <- dl_infer(art, s0930, c0930)
  expect_identical(format(o09$delivery$interval_end[[1L]], "%H:%M"), "09:30")
  expect_identical(format(o0930$delivery$interval_end[[1L]], "%H:%M"), "10:00")
  for (x in list(list(o09, c09), list(o0930, c0930))) {
    ie <- x[[1L]]$delivery$interval_end
    expect_identical(ie, dl_lib$prevcarga_delivery_grid(x[[2L]], 9))
    expect_true(all(diff(as.numeric(ie)) == 1800))                              # 30 min sem buraco
    expect_identical(ie[[length(ie)]], dl_au("00:00", DL8_O + 9L))
    expect_identical(lubridate::tz(ie), x[[2L]]$tz)
  }
  com <- intersect(as.numeric(o09$delivery$interval_end), as.numeric(o0930$delivery$interval_end))
  expect_gt(length(com), 400L)
  m <- function(o) o$delivery$y_pred[match(com, as.numeric(o$delivery$interval_end))]
  expect_lte(max(abs(m(o09) - m(o0930))), DL8_TOL)
  expect_lte(max(abs(o09$values - o0930$values)), DL8_TOL)
  ## CONTROL: a hora cheia 09:00-10:00 so entra quando o corte chega a 10:00 (09:30 ainda nao a completa)
  s10 <- dl_fx_store("10:00"); c10 <- dl_fx_cuts(s10)
  o10 <- dl_infer(art, s10, c10)
  expect_gt(dl_delta(o09$delivery[interval_end >= dl_au("10:30")], o10$delivery[interval_end >= dl_au("10:30")]), DL8_TOL)
  ## painel CRU alem do corte entregue com o corte de 09:00 => contract. O despachante confere o painel cru ANTES de
  ## reamostrar (FIX 7): a meia hora 09:00-09:30 ja passa do corte, mesmo que o to_hourly fosse descarta-la depois
  for (st in list(list(s10, c10), list(s0930, c0930))) {
    e <- dl_err(dl_infer(art, s09, c09, panel = dl_fx_panel(st[[1L]], st[[2L]], "infer")))
    expect_dl_class(e, "contract", 2L, id = "plugin.infer_beyond_cut")
    expect_match(conditionMessage(e), "09:30", fixed = TRUE)
  }
  expect_null(dl_err(dl_infer(art, s09, c09, panel = dl_fx_panel(s09, c09, "infer"))))   # CONTROL: o painel do proprio corte
})
