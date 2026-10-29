## test-cut.R -- gates [PLUGINS] corte_treino (gate01 do oraculo, como MEDICAO) e corte_intradiario do Holt-Winters (HW.6;
## AC 2 e 4). Molde de 4 bracos do honesty_gate.R (M3.2) sobre o deck sintetico (P.0e): leitor -> despachante -> plugin.
## FORBIDDEN = perturbacao alem do corte => Delta 0; CONTROL = perturbacao dentro do corte => Delta > 1e-5.

test_that("[PLUGINS] corte_treino: FORBIDDEN dia O +20% => sha256 igual; CONTROL O-1 +20% => muda e max|d| > 1e-5; molde HONESTO", {
  st <- hv_store("09:00"); cuts <- hv_cuts(st); O <- cuts$origin_day
  expect_identical(cuts$train_origin, O); expect_identical(cuts$last_data_day, O - 1L)
  train <- function(s) hv_train(hv_panel(s, cuts, "train"), cuts)
  pert <- function(after, at_or_before = NULL)
    hv_with_dir(st, hw_lib$prevcarga_fixture_perturb(st$dir, "load", after = after, at_or_before = at_or_before,
                                                     perturb = function(v) v * 1.2))
  d0 <- hv_day_start(O); d1 <- hv_day_start(O - 1L)
  base <- train(st); sha0 <- hv_sha(base)
  expect_identical(hv_sha(train(st)), sha0)                                   # CLEAN2: determinismo
  ## sem deslocamento por horizon_days: a serie termina em O-1 e datafimver e carimbado
  expect_identical(base$payload$datafimver, O - 1L); expect_identical(base$payload$datafimver, cuts$last_data_day)
  expect_identical(base$payload$origin_day, O)
  ## FORBIDDEN: dia O inteiro +20% no store
  forb <- train(pert(after = d0 - 1))
  expect_identical(hv_sha(forb), sha0)
  expect_identical(forb$payload$mean_interval, base$payload$mean_interval)
  ## CONTROL: O-1 inteiro +20% no store
  ctrl <- train(pert(after = d1 - 1, at_or_before = d0 - 1))
  expect_false(identical(hv_sha(ctrl), sha0))
  expect_gt(max(abs(ctrl$payload$mean_interval - base$payload$mean_interval)), 1e-5)
  ## o proprio plugin (forecast acoplado, painel com o dia O ate 09:00) tambem ignora o dia O e reage a O-1
  ipan <- function(s) hv_panel(s, cuts, "infer")
  fb <- hv_fc(ipan(st), cuts)$delivery
  expect_identical(hv_fc(ipan(pert(after = d0 - 1)), cuts)$delivery, fb)
  expect_gt(hv_delta(fb, hv_fc(ipan(pert(after = d1 - 1, at_or_before = d0 - 1)), cuts)$delivery), 1e-5)

  ## molde M3.2 (4 bracos): cut = fim de last_data_day; treino pelo despachante, infer limpo
  clean_infer <- ipan(st)
  gp <- hv_gate_panel(clean_infer)
  run_fn <- function(p) {
    s2 <- hv_store_for_gate(st, gp, p)
    hv_infer(train(s2), clean_infer, cuts)$delivery
  }
  g <- hw_lib$prevcarga_honesty_gate(run_fn, gp, d0, cuts$horizon_days)
  expect_identical(g$verdict, "HONESTO")
  expect_true(g$reach_ok)
  expect_lte(g$clean2_delta, HV_TOL); expect_lte(g$forbidden_delta, HV_TOL); expect_gt(g$control_delta, HV_TOL)
  expect_gt(g$arms$forbidden$n_perturbed, 0L); expect_gt(g$arms$control$n_perturbed, 0L)
})


test_that("[PLUGINS] corte_intradiario: 09:00/09:30 => 1o interval_end 09:30/10:00 e grade sem buraco; FORBIDDEN alem do corte Delta 0; CONTROL y_true", {
  s09 <- hv_store("09:00"); s0930 <- hv_store("09:30")
  c09 <- hv_cuts(s09); c0930 <- hv_cuts(s0930)
  expect_identical(c09$available_until, hv_au("09:00")); expect_identical(c0930$available_until, hv_au("09:30"))
  a09 <- hv_train(hv_panel(s09, c09, "train"), c09); a0930 <- hv_train(hv_panel(s0930, c0930, "train"), c0930)
  expect_identical(hv_sha(a09), hv_sha(a0930))                                 # a meia hora nova nao re-treina
  o09 <- hv_infer(a09, hv_panel(s09, c09, "infer"), c09)
  o0930 <- hv_infer(a0930, hv_panel(s0930, c0930, "infer"), c0930)
  expect_identical(format(o09$delivery$interval_end[[1L]], "%H:%M"), "09:30")
  expect_identical(format(o0930$delivery$interval_end[[1L]], "%H:%M"), "10:00")
  for (x in list(list(o09, c09), list(o0930, c0930))) {
    ie <- x[[1L]]$delivery$interval_end
    expect_identical(ie, hw_lib$prevcarga_delivery_grid(x[[2L]], 9))
    expect_true(all(diff(as.numeric(ie)) == 1800))                              # 30 min sem buraco
    expect_identical(ie[[length(ie)]], lubridate::ymd_hm("2024-05-24 00:00", tz = HW_TZ))
    expect_identical(lubridate::tz(ie), x[[2L]]$tz)
  }
  com <- intersect(as.numeric(o09$delivery$interval_end), as.numeric(o0930$delivery$interval_end))
  expect_gt(length(com), 400L)
  m <- function(o) o$delivery$y_pred[match(com, as.numeric(o$delivery$interval_end))]
  expect_identical(m(o09), m(o0930)); expect_identical(o09$values, o0930$values)

  ## FORBIDDEN: ZZ1 tem carga alem do corte (interval_end > 09:00) so no store; o leitor nao a entrega
  rg <- hv_ragged(); crg <- hv_cuts(rg); au <- crg$available_until
  expect_identical(au, hv_au("09:00"))
  prg <- hv_ragged_panel(rg, crg, "infer")
  expect_true(max(prg$load$interval_end) <= au)
  raw <- hw_lib$prevcarga_read_load(hw_fx_ctx(rg), HW_AREA, from = HV_O, to = HV_O)
  expect_gt(sum(raw$interval_end > au), 0L)
  run <- function(s) hv_fc(hv_ragged_panel(s, crg, "infer"), crg)$delivery
  pert <- function(after, at_or_before = NULL)
    hv_with_dir(rg, hw_lib$prevcarga_fixture_perturb(rg$dir, "load", after = after, at_or_before = at_or_before,
                                                     areas = HW_AREA))
  base <- run(rg); clean2 <- run(rg)
  forb <- run(pert(after = au - 1))
  raw_f <- hw_lib$prevcarga_read_load(hw_fx_ctx(pert(after = au - 1)), HW_AREA, from = HV_O, to = HV_O)
  expect_true(all(raw_f$load_mwh[raw_f$interval_end > au] != raw$load_mwh[raw$interval_end > au]))   # alcance
  expect_lte(hv_delta(base, forb), HV_TOL)
  ## CONTROL (canal y_true): um dia <= O-1 dentro de history_days.train
  ctrl <- run(pert(after = hv_day_start(HV_O - 1L) - 1, at_or_before = hv_day_start(HV_O) - 1))
  expect_gt(hv_delta(base, ctrl), HV_TOL)
  expect_identical(hw_lib$.prevcarga_hg_verdict(hv_delta(base, clean2), hv_delta(base, forb), hv_delta(base, ctrl), TRUE, HV_TOL), "HONESTO")

  ## painel com intervalo alem do corte entregue ao despachante => contract (confere, nao corta)
  e <- hv_err(hv_infer(a09, hv_panel(s0930, c0930, "infer"), c09))
  expect_hv_error(e, "contract", 2L)
  expect_match(conditionMessage(e), "09:")
})
