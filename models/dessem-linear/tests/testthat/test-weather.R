## test-weather.R -- DL.8 (AC9 do epico): honestidade do clima previsto do dessem-linear (ADR-0024), nas DUAS regras de
## `weather_issue_cutoff`: `available_until` (emissoes ate o corte intradiario) e `origin_midnight_utc` (ate 00:00 UTC de O).
## Deck sintetico com emissoes 00Z/06Z/18Z por dia. Canal de clima do molde (M3.5): FORBIDDEN = perturbar emissoes com
## issue_time > weather_issue_limit => Delta <= 1e-5; CONTROL = a emissao mais fresca permitida => Delta > 1e-5.
## O artefato de O e o de sempre; so o infer varia (o clima previsto entra nas exogenas de O..O+8).

DL_WX_RULES <- c("available_until", "origin_midnight_utc")

dl_wx_emissions <- function(st, cuts)
  dl_lib$prevcarga_read_weather(dl_fx_ctx(st, cuts$weather_issue_cutoff), "forecast", DL8_AREA, cuts$origin_day,
                                cuts$origin_day + cuts$horizon_days)

## infer com um previsto dado: `w` e a tabela de emissoes; o corte por weather_issue_limit e do chamador (o leitor), como
## no despachante, que RECUSA emissao alem do limite (conferido em outro teste)
## O artefato leva o carimbo da regra (weather_issue_cutoff) com que foi treinado: o infer sob `origin_midnight_utc` pede
## um artefato treinado sob essa regra (treino real; o de `available_until` e o artefato base).
dl_wx_art <- function(st, cuts)
  if (identical(cuts$weather_issue_cutoff, "available_until")) dl_base_art() else
    dl_cached(paste("art", cuts$weather_issue_cutoff), dl_train(st, cuts))

dl_wx_run <- function(w, st, cuts, art = dl_wx_art(st, cuts), ipan = dl_fx_panel(st, cuts, "infer")) {
  w <- data.table::as.data.table(w)                                  # o molde entrega data.frame
  ipan$weather_forecast <- w[issue_time <= cuts$weather_issue_limit]
  dl_infer(art, st, cuts, panel = ipan)$delivery
}

for (rule in DL_WX_RULES) {
  test_that(sprintf("[PLUGINS] gate05_clima_honesto: regra %s: FORBIDDEN emissao > limite Delta <= 1e-5; CONTROL ultima emissao permitida Delta > 1e-5; molde HONESTO", rule), {
    st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st, rule); lim <- cuts$weather_issue_limit; O <- cuts$origin_day
    expect_identical(cuts$weather_issue_cutoff, rule)
    expect_identical(lim, if (rule == "available_until") cuts$available_until else
      lubridate::with_tz(lubridate::force_tz(lubridate::as_datetime(O), "UTC"), cuts$tz))
    w <- dl_wx_emissions(st, cuts)
    ## emissoes 00Z/06Z/18Z (em UTC) e ha emissoes permitidas e proibidas
    expect_setequal(unique(lubridate::hour(lubridate::with_tz(w$issue_time, "UTC"))), c(0L, 6L, 18L))
    expect_gt(sum(w$issue_time <= lim), 0L); expect_gt(sum(w$issue_time > lim), 0L)
    ipan <- dl_fx_panel(st, cuts, "infer")
    expect_true(all(ipan$weather_forecast$issue_time <= lim))
    run_fn <- function(panel, weather) data.frame(y_pred = dl_wx_run(weather, st, cuts, ipan = ipan)$y_pred)
    g <- dl_lib$prevcarga_honesty_gate_weather(run_fn, data.frame(x = 1), as.data.frame(w), lim)
    expect_identical(g$verdict, "HONESTO")
    expect_true(g$reach_ok)
    expect_lte(g$clean2_delta, DL8_TOL); expect_lte(g$forbidden_delta, DL8_TOL); expect_gt(g$control_delta, DL8_TOL)
    expect_gt(g$arms$forbidden$n_perturbed, 0L); expect_gt(g$arms$control$n_perturbed, 0L)
    ## a emissao mais fresca permitida decide por valid_time; a mais velha so vale onde a fresca nao cobre (a de 06Z comeca
    ## em 03:00 local, deixando 00:00-03:00 de O para a de 00Z; sob a regra de meia-noite a fresca cobre O inteiro)
    base <- dl_wx_run(w, st, cuts)
    fresh <- max(w$issue_time[w$issue_time <= lim])
    old <- max(w$issue_time[w$issue_time < fresh])
    pert <- function(sel) { w2 <- data.table::copy(w); w2[sel, temperature := temperature * 1.3 + 1]; w2 }
    expect_gt(dl_delta(base, dl_wx_run(pert(w$issue_time == fresh), st, cuts)), DL8_TOL)
    fresh_gap <- min(w$valid_time[w$issue_time == fresh]) > dl_day_start(O)
    expect_identical(dl_delta(base, dl_wx_run(pert(w$issue_time == old), st, cuts)) > DL8_TOL, fresh_gap)
    expect_lte(dl_delta(base, dl_wx_run(pert(w$issue_time > lim), st, cuts)), DL8_TOL)
  })
}

test_that("[PLUGINS] gate05_clima_honesto: 06Z cruza as regras (CONTROL sob available_until, FORBIDDEN sob origin_midnight_utc)", {
  st <- dl_fx_store("09:00"); O <- st$O
  z06 <- lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(6L)
  d <- vapply(DL_WX_RULES, function(rule) {
    cuts <- dl_fx_cuts(st, rule)
    w <- dl_wx_emissions(st, cuts)
    w6 <- data.table::copy(w); w6[issue_time == z06, temperature := temperature * 1.3 + 1]
    dl_delta(dl_wx_run(w, st, cuts), dl_wx_run(w6, st, cuts))
  }, 0)
  expect_gt(d[["available_until"]], DL8_TOL)            # 06Z (03:00 local) <= 09:00: permitida e a mais fresca
  expect_lte(d[["origin_midnight_utc"]], DL8_TOL)       # 06Z > 00:00 UTC de O: proibida
})

test_that("[PLUGINS] gate05_clima_honesto: emissao alem do limite no painel => contract (confere, nao corta); via store a perturbacao nao chega", {
  st <- dl_fx_store("09:00")
  for (rule in DL_WX_RULES) {
    cuts <- dl_fx_cuts(st, rule); art <- dl_wx_art(st, cuts)
    w <- dl_wx_emissions(st, cuts)
    expect_gt(sum(w$issue_time > cuts$weather_issue_limit), 0L)
    ipan <- dl_fx_panel(st, cuts, "infer"); ipan$weather_forecast <- w
    e <- dl_err(dl_infer(art, st, cuts, panel = ipan))
    expect_dl_class(e, "contract", 2L, id = "plugin.weather_beyond_limit")
    expect_match(conditionMessage(e), "issue_time|emiss|weather", ignore.case = TRUE)
    ## o mesmo pelo store: emissoes > limite perturbadas no deck nao alteram a entrega (o leitor/corte as retira)
    forb <- dl_fx_perturb(st, "weather_forecast", issue_after = cuts$weather_issue_limit)
    expect_lte(dl_delta(dl_infer(art, st, cuts)$delivery, dl_infer(art, forb, cuts)$delivery), DL8_TOL)
  }
})
