## test-train-honesty.R -- RD.4 (AC4): honestidade do TREINO do dessem-regdin (ADR-0023, origin_scoped: o treino ve ate O-1).
## Molde de 4 bracos do honesty_gate.R (M3.2, canal de treino: cut = fim de last_data_day) sobre o painel sintetico cru:
## FORBIDDEN = perturbar (x1.30 + 1) todo intervalo do dia O (carga e clima observado) numa copia do store => sha256 do
## artefato IGUAL; CONTROL = perturbar uma hora de O-1 => sha256 muda. O corte intradiario (09:00 x 12:00) nao entra no
## artefato. O corte do painel de treino e o da fundacao (.prevcarga_plugin_train_slice), nunca recalculado aqui.

RD_TH_PERT <- function(y) y * 1.30 + 1

## o "store" do dia O: o painel de infer (carga de O ate 09:00 e clima observado de O) com o dia O perturbado
rd_th_store <- function(au = rd3_au(9L), perturb_day_o = FALSE, hour_o1 = NULL) {
  pan <- rd_fx_panel(until = au)
  if (perturb_day_o) {
    pan$load[lubridate::as_date(interval_start) == RD2_O, load_mwh := RD_TH_PERT(load_mwh)]
    pan$weather_observed[lubridate::as_date(valid_time) == RD2_O, temperature := RD_TH_PERT(temperature)]
  }
  if (!is.null(hour_o1))
    pan$load[lubridate::as_date(interval_start) == RD2_O - 1L & lubridate::hour(interval_start) == hour_o1,
             load_mwh := RD_TH_PERT(load_mwh)]
  pan
}

rd_th_slice <- function(pan, cuts = rd_fx_cuts()) rd_lib$.prevcarga_plugin_train_slice(pan, cuts)

test_that("[PLUGINS] treino_ate_o_menos_1: o corte de treino da fundacao entrega o painel ate O-1 e nada do dia O", {
  cuts <- rd_fx_cuts(); pan <- rd_th_store()
  expect_gt(sum(lubridate::as_date(pan$load$interval_start) == RD2_O), 0L)                 # o store tem carga de O
  sl <- rd_th_slice(pan, cuts)
  expect_identical(cuts$last_data_day, RD2_O - 1L); expect_identical(cuts$train_origin, RD2_O)
  expect_true(all(lubridate::as_date(sl$load$interval_start) <= RD2_O - 1L))
  expect_true(all(lubridate::as_date(sl$weather_observed$valid_time) <= RD2_O - 1L))
  ## perturbar o dia O no store nao muda o painel de treino; perturbar O-1 muda
  expect_identical(rd_th_slice(rd_th_store(perturb_day_o = TRUE), cuts)[c("load", "weather_observed")],
                   sl[c("load", "weather_observed")])
  expect_false(identical(rd_th_slice(rd_th_store(hour_o1 = 15L), cuts)$load, sl$load))
})

test_that("[PLUGINS] treino_ate_o_menos_1: FORBIDDEN dia O perturbado => sha256 igual; CONTROL hora de O-1 => muda; 09:00 x 12:00 iguais", {
  st <- rd3_state(); cuts <- rd_fx_cuts(); sha0 <- rd2_sha(st$med)
  ## FORBIDDEN: o dia O inteiro (carga e clima observado) perturbado; treino real sobre o painel cortado pela fundacao
  fb <- rd_train(rd_th_slice(rd_th_store(perturb_day_o = TRUE), cuts), cuts)
  expect_identical(rd2_sha(fb), sha0)
  expect_identical(fb, st$med)
  ## o artefato e dado puro: sem corte, relogio nem caminho; carimbo = origem do treino (nao o corte intradiario)
  expect_true(rd_lib$prevcarga_plugin_scan_pure(st$med))
  expect_identical(st$med$stamp$train_origin, RD2_O); expect_identical(st$med$payload$train_window$end, RD2_O - 1L)
  nm <- function(x) c(names(x), unlist(lapply(x, function(y) if (is.list(y) && !is.data.frame(y)) nm(y))))
  expect_false(any(c("available_until", "trained_at", "cut", "weather_issue_limit", "mm_used") %in% nm(st$med)))
  ## 09:00 x 12:00: o corte intradiario nao entra no artefato
  cuts12 <- rd_fx_cuts(available_until = rd3_au(12L))
  a12 <- rd_train(rd_th_slice(rd_th_store(au = rd3_au(12L)), cuts12), cuts12)
  expect_identical(rd2_sha(a12), sha0)
  expect_identical(a12, st$med)
  ## CONTROL: uma hora de O-1 perturbada muda o artefato
  ctl <- rd_train(rd_th_slice(rd_th_store(hour_o1 = 15L), cuts), cuts)
  expect_false(identical(rd2_sha(ctl), sha0))
  expect_false(identical(ctl$payload$arima_fingerprint$dm_digest, st$med$payload$arima_fingerprint$dm_digest))
})

test_that("[PLUGINS] treino_ate_o_menos_1: molde do honesty_gate (4 bracos) sobre o treino => HONESTO com FORBIDDEN e CONTROL executados", {
  st <- rd3_state(); au <- rd3_au(9L); cuts <- rd_fx_cuts(available_until = au)
  base_pan <- rd_th_store(au)
  d0 <- rd3_au(0L)                                                   # fim de last_data_day = O-1, recebido pronto
  gp <- data.frame(area_code = base_pan$load$area_code, interval_start = base_pan$load$interval_start,
                   interval_end = base_pan$load$interval_end, y_true = base_pan$load$load_mwh, stringsAsFactors = FALSE)
  memo <- new.env(parent = emptyenv()); n_train <- 0L
  train_memo <- function(tr) {                                       # um treino por painel de treino distinto
    key <- digest::digest(tr[c("load", "weather_observed")])
    if (is.null(memo[[key]])) { n_train <<- n_train + 1L; memo[[key]] <- rd_train(tr, cuts) }
    memo[[key]]
  }
  memo[[digest::digest(rd_th_slice(base_pan, cuts)[c("load", "weather_observed")])]] <- st$med   # o treino limpo (cache)
  run_fn <- function(p) {
    pan <- data.table::copy(base_pan)
    pan$load[, load_mwh := p$y_true]                                  # o store perturbado (so o canal y_true)
    art <- train_memo(rd_th_slice(pan, cuts))                         # o treino ve so o painel cortado pela fundacao
    ## o infer enxerga o MESMO historico (<= cut) e a carga de O limpa: a unica diferenca possivel e o treino
    inf <- data.table::copy(base_pan)
    inf$load <- data.table::rbindlist(list(pan$load[interval_end <= d0], base_pan$load[interval_end > d0]))
    data.frame(y_pred = rd_infer(art, inf, cuts)$delivery$y_pred)
  }
  if (!exists("prevcarga_honesty_gate", envir = rd_lib, inherits = FALSE))
    sys.source(file.path(rd_root(), "scripts", "lib", "honesty_gate.R"), envir = rd_lib)
  g <- rd_lib$prevcarga_honesty_gate(run_fn, gp, d0, 9L)
  cat(sprintf("\n[RD.4 medida] honesty gate treino: %s | clean2=%.3g forbidden=%.3g control=%.3f MW | treinos extras=%d\n",
              g$verdict, g$clean2_delta, g$forbidden_delta, g$control_delta, n_train))
  expect_identical(g$verdict, "HONESTO")                              # nunca INDETERMINADO / HARNESS QUEBRADO / VAZA
  expect_true(g$reach_ok)
  expect_lte(g$clean2_delta, 1e-5); expect_lte(g$forbidden_delta, 1e-5); expect_gt(g$control_delta, 1e-5)
  expect_gt(g$arms$forbidden$n_perturbed, 0L); expect_gt(g$arms$control$n_perturbed, 0L)
  expect_identical(n_train, 1L)                                       # so o painel do CONTROL exigiu treino novo
})

test_that("[PLUGINS] treino_ate_o_menos_1: painel de treino com o dia O aborta (despachante e estagio) sem ajustar nenhum SVM", {
  st <- rd3_state(); p <- st$p; au <- rd3_au(9L)
  spy_fit <- rd_spy(".rd_fit_pass")
  ## despachante: o painel de infer (carga de O ate 09:00) no treino e recusado (confere, nunca corta)
  e <- tryCatch(rd_train(rd_th_store(au), rd_fx_cuts(available_until = au)), prevcarga_error = function(c) c)
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$id, "plugin.train_beyond_cut")
  expect_match(conditionMessage(e), format(RD2_O), fixed = TRUE)
  ## estagio: a guarda propria do plugin (deck com linha de O) aborta contract exit 2 antes do 1o ajuste
  f <- rd2_feats(p, rd2_panel(until = au), 9L)
  e2 <- tryCatch(rd_with_cli(p$env$rd_training(f, rd2_spec()$params, 42L)), prevcarga_error = function(c) c)
  expect_identical(e2$id, "plugin.regdin_train_panel_has_origin")
  expect_identical(e2$exit_code, 2L); expect_identical(e2$class_name, "contract")
  expect_match(conditionMessage(e2), format(RD2_O), fixed = TRUE)
  expect_true(rd_ascii(conditionMessage(e2)))
  expect_identical(spy_fit$count(), 0L)
  ## CONTROL: o deck sem a linha de O passa na guarda
  f0 <- rd2_feats(p, rd2_panel(), 9L)
  expect_true(p$env$rd_assert_train_cut(f0$deck))
})
