## test-weather.R -- gates [PLUGINS] clima_honesto (gate08 do oraculo) e clima_fallback (gate09) do Holt-Winters (HW.6; AC 6).
## Canal de clima do honesty_gate (M3.5, ADR-0024) nas DUAS regras de weather_issue_cutoff: `available_until` e
## `origin_midnight_utc`, sobre o deck sintetico (emissoes 00Z/06Z/18Z) com feriado util no horizonte (FER = 1, SemiFer ativo).
##
## Clima no HW e numericamente INERTE na saida: no oraculo p_alfa se cancela no SemiFer (feature_engineering/funcoes_aux.R:660,712),
## entao o clima so decide (a) o comprimento do horizonte e (b) se o feriado passa pelo SemiFer (dia com temperatura) ou fica
## cru. Por isso a perturbacao do canal e "tirar a temperatura" (NA): uma emissao usada que perde a temperatura do dia do
## feriado pula o SemiFer naquele dia (Delta > 0 so la). Uma perturbacao de VALOR (y*1.3+1, o default do molde M3.5) tem
## Delta = 0 mesmo na emissao usada: e o que o primeiro bloco documenta (o molde com o default daria HARNESS QUEBRADO).

## y_pred (delivery) do forecast acoplado do plugin sobre o store
hv_weather_run <- function(s, cuts) hv_fc(hv_panel(s, cuts, "infer"), cuts)$delivery

for (rule in c("available_until", "origin_midnight_utc")) {
  test_that(sprintf("[PLUGINS] clima_honesto: regra %s (gate08): FORBIDDEN emissao > limite Delta 0; CONTROL ultima emissao permitida no feriado > tol so la", rule), {
    st <- hv_holiday_store(); cuts <- hv_cuts(st, rule); lim <- cuts$weather_issue_limit
    expect_identical(cuts$weather_issue_cutoff, rule)
    expect_identical(lim, if (rule == "available_until") cuts$available_until else
      lubridate::with_tz(lubridate::force_tz(lubridate::as_datetime(cuts$origin_day), "UTC"), cuts$tz))
    pert <- function(...) hv_with_dir(st, hw_lib$prevcarga_fixture_perturb(st$dir, "weather_forecast", ...))
    base <- hv_weather_run(st, cuts); clean2 <- hv_weather_run(st, cuts)
    ## FER = 1 no feriado do horizonte e o SemiFer ativo (dia com temperatura)
    deck <- suppressWarnings(hv_env()$plugin_ingest(hv_panel(st, cuts, "infer"), cuts))
    expect_identical(deck$TempPre$FER[deck$TempPre$DIA == HV_HOL], 1L)
    expect_false(deck$weather_absent); expect_false(HV_HOL %in% hv_fc(hv_panel(st, cuts, "infer"), cuts)$meta$no_weather_days)

    ## FORBIDDEN: emissoes com issue_time > limite sem temperatura (e com valor perturbado) => Delta 0
    forb <- hv_weather_run(pert(issue_after = lim, perturb = hv_drop_temp), cuts)
    forb_v <- hv_weather_run(pert(issue_after = lim), cuts)
    expect_lte(hv_delta(base, forb), HV_TOL); expect_lte(hv_delta(base, forb_v), HV_TOL)

    ## CONTROL: a ultima emissao permitida (issue_time <= limite) so no dia do feriado perde a temperatura => Delta > tol so no feriado
    fresh <- max(unique(hw_lib$prevcarga_read_weather(hw_fx_ctx(st), "forecast", HW_AREA, cuts$origin_day,
                                                      cuts$origin_day + cuts$horizon_days)$issue_time[
      hw_lib$prevcarga_read_weather(hw_fx_ctx(st), "forecast", HW_AREA, cuts$origin_day, cuts$origin_day + cuts$horizon_days)$issue_time <= lim]))
    ctrl <- hv_weather_run(pert(issue_after = fresh - 1, issue_at_or_before = lim, after = hv_day_start(HV_HOL) - 1,
                                at_or_before = hv_day_start(HV_HOL + 1L) - 1, perturb = hv_drop_temp), cuts)
    d <- abs(ctrl$y_pred - base$y_pred)
    fer <- lubridate::as_date(base$interval_end - lubridate::minutes(30L), tz = cuts$tz) == HV_HOL
    expect_true(any(fer))
    expect_gt(max(d[fer]), HV_TOL)
    expect_equal(max(d[!fer]), 0)
    expect_identical(hw_lib$.prevcarga_hg_verdict(hv_delta(base, clean2), hv_delta(base, forb), hv_delta(base, ctrl), TRUE, HV_TOL), "HONESTO")
  })
}

test_that("[PLUGINS] clima_honesto: 06Z cruza as regras (CONTROL sob available_until, FORBIDDEN sob origin_midnight_utc); valor da temperatura e inerte", {
  st <- hv_holiday_store()
  O <- st$O
  z06 <- lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(6L)
  drop06 <- hw_lib$prevcarga_fixture_perturb(st$dir, "weather_forecast", issue_after = z06 - 1, issue_at_or_before = z06,
                                             after = hv_day_start(HV_HOL) - 1, at_or_before = hv_day_start(HV_HOL + 1L) - 1,
                                             perturb = hv_drop_temp)
  s06 <- hv_with_dir(st, drop06)
  d <- vapply(c("available_until", "origin_midnight_utc"), function(rule) {
    cuts <- hv_cuts(st, rule)
    hv_delta(hv_weather_run(st, cuts), hv_weather_run(s06, cuts))
  }, 0)
  expect_gt(d[["available_until"]], HV_TOL)           # 06Z permitida (03:00 local <= 09:00): CONTROL
  expect_lte(d[["origin_midnight_utc"]], HV_TOL)      # 06Z > limite 00Z: FORBIDDEN
  ## inercia numerica do oraculo: perturbar o VALOR da temperatura das emissoes usadas nao muda a saida
  cuts <- hv_cuts(st, "available_until")
  vp <- hv_with_dir(st, hw_lib$prevcarga_fixture_perturb(st$dir, "weather_forecast", issue_at_or_before = cuts$weather_issue_limit))
  expect_lte(hv_delta(hv_weather_run(st, cuts), hv_weather_run(vp, cuts)), HV_TOL)
})

test_that("[PLUGINS] clima_honesto: emissao alem do limite no painel => contract (confere, nao corta)", {
  st <- hv_holiday_store()
  for (rule in c("available_until", "origin_midnight_utc")) {
    cuts <- hv_cuts(st, rule)
    all_em <- hw_lib$prevcarga_read_weather(hw_fx_ctx(st), "forecast", HW_AREA, cuts$origin_day, cuts$origin_day + cuts$horizon_days)
    expect_gt(sum(all_em$issue_time > cuts$weather_issue_limit), 0L)
    ipan <- hv_panel(st, cuts, "infer"); ipan$weather_forecast <- all_em
    art <- hv_train(hv_panel(st, cuts, "train"), cuts)
    e <- hv_err(hv_infer(art, ipan, cuts))                                      # despachante
    expect_hv_error(e, "contract", 2L)
    expect_match(conditionMessage(e), "issue_time|emiss|weather", ignore.case = TRUE)
    e2 <- hw_catch(suppressWarnings(hv_env()$plugin_ingest(ipan, cuts)))        # plugin (ingestao)
    expect_hv_error(e2, "contract", 2L, id = "hw.weather_beyond_cut")
  }
})

test_that("[PLUGINS] clima_fallback: sem emissao valida => sucesso, horizon_days linhas nao-NA, SemiFer pulado e aviso hw.weather_absent ASCII (gate09)", {
  st <- hv_holiday_store(); cuts <- hv_cuts(st); env <- hv_env(); spec <- hv_spec()
  ipan <- hv_panel(st, cuts, "infer")
  ## store-fixture sem nenhuma emissao <= limite: o painel chega sem previsao
  p0 <- ipan; p0$weather_forecast <- ipan$weather_forecast[0L]
  w <- list()
  out <- withCallingHandlers(env$plugin_forecast(p0, cuts, spec),
    prevcarga_warning = function(cnd) { w[[length(w) + 1L]] <<- cnd; invokeRestart("muffleWarning") },
    message = function(m) invokeRestart("muffleMessage"))
  ids <- vapply(w, function(x) x$id, "")
  expect_true("hw.weather_absent" %in% ids)
  wa <- w[[match("hw.weather_absent", ids)]]
  expect_s3_class(wa, "prevcarga_warning"); expect_true(hv_ascii(conditionMessage(wa)))
  expect_identical(sum(ids == "hw.weather_absent"), 1L)                          # o aviso sai uma vez
  ## sai pelo canal de aviso do R (stderr): capturado por capture.output(type = "message")
  msg <- capture.output(invisible(withCallingHandlers(
    env$plugin_forecast(p0, cuts, spec), warning = function(cnd) { cat(conditionMessage(cnd), "\n", file = stderr()); invokeRestart("muffleWarning") })),
    type = "message")
  expect_true(any(grepl(conditionMessage(wa), msg, fixed = TRUE))); expect_true(all(hv_ascii(msg)))
  expect_true(out$meta$weather_absent)
  expect_identical(dim(out$values), c(cuts$horizon_days, 48L)); expect_false(anyNA(out$values)); expect_false(anyNA(out$delivery$y_pred))
  expect_identical(out$meta$no_weather_days, cuts$origin_day + 1:cuts$horizon_days)
  ## SemiFer pulado: a linha do feriado e o $mean suavizado (sem temperatura nao ha SemiFer)
  art <- hv_train(hv_panel(st, cuts, "train"), cuts)$payload
  k <- as.integer(HV_HOL - cuts$origin_day)                                       # O+2 => values[2, ] = mean_interval[3, ]
  expect_identical(out$values[k, ], env$Suaviza.LOESS1(art$mean_interval[k + 1L, ], env$plugin_constants()$loess_span_out)[[1L]])
  ## CONTROL: com a emissao presente o feriado passa pelo SemiFer e a linha difere do $mean suavizado
  base <- hv_fc(ipan, cuts)
  expect_false(base$meta$weather_absent)
  expect_false(identical(base$values[k, ], out$values[k, ]))
  ## o despachante entrega o mesmo caso: envelope valido, y_pred finito na grade
  de <- suppressWarnings(hv_infer(hv_train(hv_panel(st, cuts, "train"), cuts), p0, cuts))
  expect_true(all(is.finite(de$delivery$y_pred))); expect_true(de$meta$weather_absent)
  expect_identical(de$delivery$y_pred, out$delivery$y_pred)
})
