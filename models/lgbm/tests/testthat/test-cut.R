## test-cut.R -- LGBM.4: corte intradiario do infer (gate01_perturbacao do oraculo como MEDICAO). available_until e derivado do dado
## (O 09:30 = meia hora 09:00-09:30 presente). Molde de 4 bracos prevcarga_honesty_gate (M3.2) sobre o deck ragged: o store
## TEM carga alem do corte e o leitor a esconde. FORBIDDEN = perturbacao alem do corte => Delta <= 1e-5; CONTROL = dentro => > 1e-5.
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
TOL <- 1e-5
if (!is.function(lgbm_lib$prevcarga_honesty_gate))
  sys.source(file.path(lgbm_root(), "scripts", "lib", "honesty_gate.R"), envir = lgbm_lib)
P <- lgbm_source_plugin()
b <- lgbm_fx_base()
O <- LGBM_O
au <- b$cuts$available_until
rag <- lgbm_fx_ragged()
at_time <- function(s) lubridate::ymd_hm(s, tz = LGBM_TZ)

## infer do despachante sobre um store (deck) perturbado; cuts e artefato fixos (o corte de treino nao muda)
run_on <- function(deck) {
  ctx <- lgbm_fx_ctx(deck)
  lgbm_fx_infer(b, lgbm_fx_dpanel(deck, ctx, "infer", b$cuts))$delivery
}
perturbed <- function(after, at_or_before = NULL) {
  out <- lgbm_lib$prevcarga_fixture_perturb(rag$dir, "load", after = after, at_or_before = at_or_before, areas = LGBM_HEAT)
  utils::modifyList(rag, list(dir = out))
}
## painel y_true do molde: a carga CRUA da area no store (inclui o que o leitor esconde), 10 dias ate O+1
gate_panel <- function() {
  ld <- lgbm_lib$prevcarga_read_load(lgbm_fx_ctx(rag), LGBM_HEAT, from = O - 9L, to = O + 1L)
  data.frame(interval_start = ld$interval_start, interval_end = ld$interval_end, y_true = ld$load_mwh, stringsAsFactors = FALSE)
}
## mapeia a perturbacao do molde (y_true) para o store: o intervalo [min, max] dos intervalos alterados
store_for <- function(base, p) {
  ch <- which(abs(p$y_true - base$y_true) > 0)
  if (!length(ch)) return(rag)
  s0 <- p$interval_start[ch]
  perturbed(after = min(s0) - lubridate::minutes(30L), at_or_before = max(s0))
}

test_that("[AC3] o corte e O 09:30 derivado do dado e o artefato e o mesmo para qualquer meia hora do dia O", {
  expect_identical(au, at_time("2026-10-01 09:30"))
  expect_identical(b$art$stamp$train_origin, LGBM_O - 2L)
  expect_identical(b$cuts$last_data_day, LGBM_O - 3L)
  expect_lte(max(b$panel$load$interval_end), au)
  raw <- gate_panel()
  expect_gt(sum(raw$interval_end > au), 0L)          # o store tem carga alem do corte; o leitor a esconde
})

test_that("[AC3] molde honesty_gate: FORBIDDEN (interval_end > 09:30 e O+1) Delta <= 1e-5; CONTROL (09:00-09:30 dentro do corte) > 1e-5 => HONESTO", {
  gp <- gate_panel()
  run_fn <- function(p) run_on(store_for(gp, p))
  g <- lgbm_lib$prevcarga_honesty_gate(run_fn, gp, cut = au, horizon = 2L, tol = TOL)
  expect_identical(g$verdict, "HONESTO")
  expect_true(g$reach_ok)
  expect_lte(g$clean2_delta, TOL); expect_lte(g$forbidden_delta, TOL); expect_gt(g$control_delta, TOL)
  expect_gt(g$arms$forbidden$n_perturbed, 0L); expect_gt(g$arms$control$n_perturbed, 0L)
  ## FORBIDDEN explicito: todo o store alem do corte (inclui O+1) +30% => saida byte a byte igual
  base <- run_on(rag)
  fz <- run_on(perturbed(after = au - 1))
  expect_identical(fz$y_pred, base$y_pred)
  raw_f <- lgbm_lib$prevcarga_read_load(lgbm_fx_ctx(perturbed(after = au - 1)), LGBM_HEAT, from = O, to = O + 1L)
  raw_b <- lgbm_lib$prevcarga_read_load(lgbm_fx_ctx(rag), LGBM_HEAT, from = O, to = O + 1L)
  expect_true(all(raw_f$load_mwh[raw_f$interval_end > au] != raw_b$load_mwh[raw_b$interval_end > au]))   # alcance
  ## CONTROL: so a meia hora 09:00-09:30 (interval_start 09:00) => o semi-horario usa a meia hora isolada
  half <- run_on(perturbed(after = at_time("2026-10-01 08:30"), at_or_before = at_time("2026-10-01 09:00")))
  expect_gt(max(abs(half$y_pred - base$y_pred)), TOL)
  ## CONTROL: a meia hora ANTERIOR (08:30-09:00) tambem pesa; e o resto do dia O ate 08:30 tambem
  early <- run_on(perturbed(after = at_time("2026-10-01 00:00"), at_or_before = at_time("2026-10-01 08:30")))
  expect_gt(max(abs(early$y_pred - base$y_pred)), TOL)
})

test_that("[AC3] CONTROL de granularidade: decks com available_until 09:00 e 09:30 dao previsoes diferentes para O+1", {
  a09 <- lgbm_fx_at(at_time("2026-10-01 09:00")); a0930 <- lgbm_fx_at(at_time("2026-10-01 09:30"))
  e09 <- lgbm_fx_infer(b, a09$panel, a09$cuts); e0930 <- lgbm_fx_infer(b, a0930$panel, a0930$cuts)
  com <- intersect(as.numeric(e09$delivery$interval_end), as.numeric(e0930$delivery$interval_end))
  expect_gt(length(com), 70L)
  m <- function(e) e$delivery$y_pred[match(com, as.numeric(e$delivery$interval_end))]
  o1 <- com >= as.numeric(at_time("2026-10-02 00:30"))                 # so O+1
  expect_gt(max(abs(m(e09)[o1] - m(e0930)[o1])), TOL)
  ## FORBIDDEN dentro do mesmo available_until: o mesmo painel duas vezes => identico (CLEAN2)
  expect_identical(lgbm_fx_infer(b, a0930$panel, a0930$cuts)$delivery, e0930$delivery)
})

test_that("[AC3] painel com interval_end > available_until entregue direto => [prevcarga/contract]; sem carga real em O => so razoes, finito", {
  a09 <- lgbm_fx_at(at_time("2026-10-01 09:00"))
  e <- fail(lgbm_fx_infer(b, b$panel, a09$cuts))                       # painel ate 09:30 com corte 09:00 (despachante)
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  expect_match(conditionMessage(e), "09:")
  e2 <- fail(P$plugin_ingest(b$panel, a09$cuts))                       # idem direto na ingestao do plugin
  expect_s3_class(e2, "prevcarga_error"); expect_identical(e2$class_name, "contract")
  ## sem carga real em O (available_until = O 00:00; o leitor nao deriva esse corte, entao o painel/cuts sao montados a mao)
  c00 <- b$cuts; c00$available_until <- at_time("2026-10-01 00:00")
  p00 <- b$panel; p00$load <- b$panel$load[interval_end <= c00$available_until]
  expect_lte(max(p00$load$interval_end), c00$available_until)
  w <- character()
  out <- withCallingHandlers({
    frame <- P$plugin_ingest(p00, c00)
    inf <- P$plugin_inference_features(frame, b$art$payload)
    P$plugin_infer(b$art$payload, inf, c00)
  }, warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("sem carga real no dia O", w))); expect_true(all(grepl("^[ -~]*$", w)))
  expect_identical(inf$n_real_D, 0L)
  expect_identical(nrow(out$delivery), 96L); expect_true(all(is.finite(out$delivery$y_pred)))
  expect_identical(dim(out$values), c(2L, 48L))
})

test_that("[AC3] cortes 12:00 e 16:00: entrega e values 2x48 finitos, grade da fundacao", {
  for (hhmm in c("12:00", "16:00")) {
    a <- lgbm_fx_at(at_time(paste("2026-10-01", hhmm)))
    env <- lgbm_fx_infer(b, a$panel, a$cuts)
    expect_identical(env$delivery$interval_end, lgbm_lib$prevcarga_delivery_grid(a$cuts, 2L), info = hhmm)
    expect_true(all(is.finite(env$delivery$y_pred)), info = hhmm)
    expect_identical(dim(env$values), c(2L, 48L), info = hhmm); expect_true(all(is.finite(env$values)), info = hhmm)
    expect_identical(env$meta$n_real_D, if (hhmm == "12:00") 24L else 32L)
  }
})
