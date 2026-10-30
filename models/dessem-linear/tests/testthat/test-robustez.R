## test-robustez.R -- DL.8 (AC15/AC16 do epico): robustez do infer do dessem-linear. Cauda de EXOGENAPREV curta (2 dias) completa
## 9x48 (preenchimento do motor, `max_tail_fill_days = 2`); 3 dias falta => completude. Fixtures de modelo-vazio, cross e flag-12
## completam o envelope; o aviso do motor (dessem_engine_guard) chega pelo canal de condicoes e o do plugin em meta$warnings.
##
## LACUNA CONHECIDA (DL8-GAP-1, discovery do DL.8): no infer a UNICA chamada a tune_wsvm e a cabeca DM(O) (`dl_dm_head`), que roda
## FORA do withCallingHandlers de plugin_infer; por isso as guardas do motor (cross/modelo-vazio) e o aviso de flag-12 (mensagem do
## normalize_special_day_codes, na ingestao) NAO chegam a meta$warnings. Os testes pinam o estado atual (marcados DL8-GAP-1): quando o
## plugin os propagar, os `expect_identical(..., character())` falham de proposito e viram `expect_true(code %in% warnings)`.

## coleta mensagens dessem_engine_guard (codigos) e mensagens simples do motor durante `expr`
dl_rb_collect <- function(expr) {
  codes <- character(); msgs <- character()
  val <- withCallingHandlers(expr, message = function(m) {
    if (inherits(m, "dessem_engine_guard")) codes <<- c(codes, m$code) else msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  list(value = val, codes = codes, msgs = msgs)
}

dl_rb_infer <- function(art, st, cuts, panel = NULL) {
  pan <- if (is.null(panel)) dl_fx_panel(st, cuts, "infer") else panel
  dl_rb_collect(dl_with_cli(suppressWarnings(dl_lib$prevcarga_plugin_infer(dl_plugin(), art, pan, cuts))))
}

dl_rb_complete <- function(e, cuts) {
  expect_identical(dim(e$values), c(9L, 48L)); expect_true(all(is.finite(e$values)))
  expect_identical(e$delivery$interval_end, dl_lib$prevcarga_delivery_grid(cuts, 9))
  expect_true(all(is.finite(e$delivery$y_pred)))
}

test_that("[PLUGINS] gate16_cauda_curta: cauda de EXOGENAPREV curta de 2 dias completa 9x48; 3 dias => completude exit 3", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art(); O <- cuts$origin_day
  ipan <- dl_fx_panel(st, cuts, "infer")
  short <- function(k) { p <- ipan; p$weather_forecast <- ipan$weather_forecast[lubridate::as_date(valid_time, tz = DL_TZ) < O + 10L - k]; p }
  base <- dl_infer(art, st, cuts)
  for (k in 1:2) {
    e <- dl_infer(art, st, cuts, panel = short(k))
    expect_identical(dim(e$values), c(9L, 48L)); expect_true(all(is.finite(e$values)))
    expect_identical(e$delivery$interval_end, base$delivery$interval_end)
    ## a cauda preenchida e o dia anterior repetido: o inicio do horizonte nao muda (so as ultimas exogenas)
    expect_identical(e$values[1L, ], base$values[1L, ])
  }
  e3 <- dl_err(dl_infer(art, st, cuts, panel = short(3L)))
  expect_dl_class(e3, "completude", 3L, id = "plugin.dessem_linear_series_gap")
  expect_match(conditionMessage(e3), "EXOGENAPREV", fixed = TRUE)
})

test_that("[PLUGINS] gate11_clamp_cross: fixture cross (amostra pequena do dia especial) completa o envelope; guarda do motor visivel; DL8-GAP-1", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art()
  r <- dl_rb_infer(art, st, cuts)
  dl_rb_complete(r$value, cuts)
  expect_true("warning.engine_cross_clamped" %in% r$codes)             # o tune_wsvm real clampou o cross da DM(O)
  expect_identical(r$value$meta$warnings, character())                  # DL8-GAP-1: a guarda nao chega a meta$warnings
})

test_that("[PLUGINS] gate10_modelo_vazio: fixture modelo-vazio (erro 'Model is empty' do libsvm no 1o ajuste) completa o envelope com a guarda do motor; DL8-GAP-1", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art(); eng <- dl_engine_env()
  real <- get(".tune_wsvm_real", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked(".tune_wsvm_real", eng)
  if (unlock) unlockBinding(".tune_wsvm_real", eng)
  n <- 0L
  ## 1a chamada: erro do libsvm; nas seguintes o ajuste real (com cross pequeno, que a amostra da DM(O) comporta)
  assign(".tune_wsvm_real", function(...) {
    n <<- n + 1L
    if (n == 1L) stop("Model is empty!", call. = FALSE)
    a <- list(...); a$tunecontrol <- WeightSVM::tune.control(cross = 3L)
    do.call(real, a)
  }, envir = eng)
  on.exit({ assign(".tune_wsvm_real", real, envir = eng); if (unlock) lockBinding(".tune_wsvm_real", eng) }, add = TRUE)
  r <- dl_rb_infer(art, st, cuts)
  dl_rb_complete(r$value, cuts)
  expect_true("warning.engine_empty_model_guard" %in% r$codes)         # o wrapper do motor dropou as dummies raras e re-ajustou
  expect_gt(n, 1L)
  expect_identical(r$value$meta$warnings, character())                  # DL8-GAP-1
})

test_that("[PLUGINS] gate13_flag12: fixture flag-12 (codigo 12 de feriado) completa o envelope; aviso do motor na ingestao; CONTROL sem flag-12 sem aviso; DL8-GAP-1", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art(); O <- cuts$origin_day
  ipan <- dl_fx_panel(st, cuts, "infer")
  expect_true(12L %in% ipan$holidays$prevcarga_code)                    # o deck sintetico traz codigo 12 em O-5
  r <- dl_rb_infer(art, st, cuts)
  dl_rb_complete(r$value, cuts)
  expect_true(any(grepl("codigo nao-modelado (12)", r$msgs, fixed = TRUE)))
  expect_identical(r$value$meta$warnings, character())                  # DL8-GAP-1
  ## CONTROL: sem o codigo 12 no deck nao ha aviso de normalizacao
  no12 <- ipan; no12$holidays <- data.table::copy(ipan$holidays)[prevcarga_code == 12L, prevcarga_code := 2L]
  c0 <- dl_rb_infer(art, st, cuts, panel = no12)
  dl_rb_complete(c0$value, cuts)
  expect_false(any(grepl("nao-modelado", c0$msgs, fixed = TRUE)))
})

test_that("[PLUGINS] robustez: aviso do plugin em meta$warnings (regra semi-horaria que falha => duplicacao :00/:30) e saida ASCII", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art(); eng <- dl_engine_env()
  fb <- get("desagregaModeloSH", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("desagregaModeloSH", eng)
  if (unlock) unlockBinding("desagregaModeloSH", eng)
  assign("desagregaModeloSH", function(modelo, dadosMdl) stop("forcado"), envir = eng)
  on.exit({ assign("desagregaModeloSH", fb, envir = eng); if (unlock) lockBinding("desagregaModeloSH", eng) }, add = TRUE)
  e <- dl_infer(art, st, cuts)
  dl_rb_complete(e, cuts)
  expect_identical(e$meta$warnings, "warning.dessem_linear_desagrega_fallback")
  expect_true(dl_ascii(e$meta$warnings))
  ## duplicacao :00/:30 do oraculo: cada par de meia horas de uma hora tem o mesmo valor
  v <- as.vector(t(e$values)); expect_identical(v[c(TRUE, FALSE)], v[c(FALSE, TRUE)])
})
