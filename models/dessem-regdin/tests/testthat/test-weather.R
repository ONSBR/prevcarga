## test-weather.R -- RD.5 (AC9 do epico, `corte_clima` e `corte_carga`): honestidade do clima previsto (ADR-0024) e da carga
## (intradiario e treino) do dessem-regdin, nas DUAS variantes (MED, MAX) e nas DUAS regras de `weather_issue_cutoff`:
## `available_until` (emissoes ate o corte intradiario) e `origin_midnight_utc` (ate 00:00 UTC de O). Deck sintetico do
## fixture_store (emissoes 00Z/06Z/18Z por dia), leitor do M2, despachante e molde do honesty_gate.R (M3.2 carga/M3.5 clima).
##  - clima: FORBIDDEN = emissoes com issue_time > weather_issue_limit perturbadas => Delta <= 1e-5; CONTROL = a emissao mais
##    fresca permitida => Delta > 1e-5; veredito HONESTO;
##  - carga (intradiario): FORBIDDEN = carga de O com interval_end > available_until; CONTROL = a manha de O (corte - 1 dia, corte];
##  - carga (treino, ADR-0023): FORBIDDEN = tudo do dia O em diante nao entra no painel de treino (corte da fundacao,
##    `.prevcarga_plugin_train_slice`, nunca recalculado); CONTROL = uma hora de O-1 muda o painel de treino. O treino e uma
##    funcao pura do painel de treino (dois treinos identicos => mesmo sha256, test-portability.R; RD.4 mede o CONTROL do sha).
## Cada (variante, regra) pede o artefato treinado SOB a regra (o carimbo leva `weather_issue_cutoff`).

wx_ctx <- function(exo, rule) {
  rd2_cached(paste("rd5-wx", exo, rule), {
    st <- rd5_store(); cu <- rd5_cuts(st, "09:00", rule)
    list(st = st, cuts = cu, art = rd5_art(exo, rule), pan = rd5_panel(st, cu, "infer"),
         w = rd_lib$prevcarga_read_weather(rd5_ctx(st, rule), "forecast", st$areas, st$O, st$O + cu$horizon_days))
  })
}

wx_run <- function(x, w, rule_pan = x$pan) {
  pan <- rule_pan
  pan$weather_forecast <- data.table::as.data.table(w)[issue_time <= x$cuts$weather_issue_limit]
  rd5_infer(x$art, x$st, x$cuts, panel = pan)$delivery
}

for (exo in rd5_variants()) for (rule in RD5_RULES) {
  local({
    exo <- exo; rule <- rule
    tag <- sprintf("%s regra %s", exo, rule)

    test_that(sprintf("[PLUGINS] corte_clima: %s: FORBIDDEN emissao > limite Delta <= 1e-5; CONTROL ultima emissao permitida Delta > 1e-5; molde HONESTO", tag), {
      x <- wx_ctx(exo, rule); cu <- x$cuts; lim <- cu$weather_issue_limit; O <- x$st$O; w <- x$w
      expect_identical(cu$weather_issue_cutoff, rule)
      expect_identical(lim, if (rule == "available_until") cu$available_until else
        lubridate::with_tz(lubridate::force_tz(lubridate::as_datetime(O), "UTC"), cu$tz))
      expect_identical(x$art$stamp$weather_issue_cutoff, rule)
      ## emissoes 00Z/06Z/18Z (UTC), com emissoes permitidas e proibidas; o painel entregue so tem as permitidas
      expect_setequal(unique(lubridate::hour(lubridate::with_tz(w$issue_time, "UTC"))), c(0L, 6L, 18L))
      expect_gt(sum(w$issue_time <= lim), 0L); expect_gt(sum(w$issue_time > lim), 0L)
      expect_true(all(x$pan$weather_forecast$issue_time <= lim))
      run_fn <- function(panel, weather) data.frame(y_pred = wx_run(x, weather)$y_pred)
      g <- rd_lib$prevcarga_honesty_gate_weather(run_fn, data.frame(x = 1), as.data.frame(w), lim)
      cat(sprintf("\n[RD.5 medida] clima %s: %s | clean2=%.3g forbidden=%.3g control=%.3f MW\n", tag, g$verdict, g$clean2_delta,
                  g$forbidden_delta, g$control_delta))
      expect_identical(g$verdict, "HONESTO")                           # nunca INDETERMINADO / HARNESS QUEBRADO / VAZA
      expect_true(g$reach_ok)
      expect_lte(g$clean2_delta, RD5_TOL); expect_lte(g$forbidden_delta, RD5_TOL); expect_gt(g$control_delta, RD5_TOL)
      expect_gt(g$arms$forbidden$n_perturbed, 0L); expect_gt(g$arms$control$n_perturbed, 0L)
      ## a emissao mais fresca permitida decide por valid_time; a mais velha so vale onde a fresca nao cobre (a de 06Z comeca
      ## em 03:00 local, deixando 00:00-03:00 de O para a de 00Z; sob a regra de meia-noite a de 00Z cobre O inteiro)
      base <- wx_run(x, w)
      fresh <- max(w$issue_time[w$issue_time <= lim]); old <- max(w$issue_time[w$issue_time < fresh])
      pert <- function(sel) { w2 <- data.table::copy(w); w2[sel, temperature := temperature * 1.3 + 1]; w2 }
      expect_gt(rd5_delta(base, wx_run(x, pert(w$issue_time == fresh))), RD5_TOL)
      fresh_gap <- min(w$valid_time[w$issue_time == fresh]) > rd5_day_start(O)
      expect_identical(rd5_delta(base, wx_run(x, pert(w$issue_time == old))) > RD5_TOL, fresh_gap)
      expect_lte(rd5_delta(base, wx_run(x, pert(w$issue_time > lim))), RD5_TOL)
    })

    test_that(sprintf("[PLUGINS] corte_clima: %s: painel entregue com emissao alem do limite => contract (confere, nao corta); via store a perturbacao nao chega", tag), {
      x <- wx_ctx(exo, rule); cu <- x$cuts
      expect_gt(sum(x$w$issue_time > cu$weather_issue_limit), 0L)
      pan <- x$pan; pan$weather_forecast <- data.table::copy(x$w)
      e <- rd5_err(rd5_infer(x$art, x$st, cu, panel = pan))
      expect_rd5_class(e, "contract", 2L, id = "plugin.weather_beyond_limit")
      expect_match(conditionMessage(e), "issue_time", fixed = TRUE)
      ## o mesmo pelo store: emissoes > limite perturbadas no deck nao alteram a entrega (o leitor/corte as retira)
      base <- rd5_infer(x$art, x$st, cu, panel = x$pan)$delivery
      forb <- rd5_perturb(x$st, "weather_forecast", issue_after = cu$weather_issue_limit)
      expect_lte(rd5_delta(base, rd5_infer(x$art, forb, cu)$delivery), RD5_TOL)
    })

    test_that(sprintf("[PLUGINS] corte_carga: %s: FORBIDDEN carga com interval_end > available_until Delta <= 1e-5; CONTROL a manha de O; molde HONESTO", tag), {
      x <- wx_ctx(exo, rule); cu <- x$cuts; st <- x$st; au <- cu$available_until; O <- st$O
      raw <- rd_lib$prevcarga_read_load(rd5_ctx(st, rule), RD5_AREA, from = O, to = O)
      expect_gt(sum(raw$interval_end > au), 0L)                        # ha carga de O alem do corte, so no store
      gp <- data.frame(area_code = raw$area_code, interval_start = raw$interval_start, interval_end = raw$interval_end,
                       y_true = raw$load_mwh, stringsAsFactors = FALSE)
      run_fn <- function(p) {
        ch <- which(abs(p$y_true - gp$y_true) > 0)
        s2 <- if (!length(ch)) st else
          rd5_perturb(st, "load", after = min(p$interval_start[ch]) - lubridate::minutes(30L), at_or_before = max(p$interval_start[ch]))
        data.frame(y_pred = rd5_infer(x$art, s2, cu)$delivery$y_pred)
      }
      g <- rd_lib$prevcarga_honesty_gate(run_fn, gp, au, cu$horizon_days)
      cat(sprintf("\n[RD.5 medida] carga %s: %s | clean2=%.3g forbidden=%.3g control=%.3f MW\n", tag, g$verdict, g$clean2_delta,
                  g$forbidden_delta, g$control_delta))
      expect_identical(g$verdict, "HONESTO")
      expect_true(g$reach_ok)
      expect_lte(g$clean2_delta, RD5_TOL); expect_lte(g$forbidden_delta, RD5_TOL); expect_gt(g$control_delta, RD5_TOL)
      expect_gt(g$arms$forbidden$n_perturbed, 0L); expect_gt(g$arms$control$n_perturbed, 0L)
      ## painel CRU com carga alem do corte entregue ao despachante => contract (confere, nao corta)
      pan <- x$pan
      pan$load <- data.table::rbindlist(list(pan$load, rd_lib$prevcarga_read_load(rd5_ctx(st, rule), RD5_AREA, from = O, to = O)[interval_end > au]),
                                        use.names = TRUE, fill = TRUE)
      expect_rd5_class(rd5_err(rd5_infer(x$art, st, cu, panel = pan)), "contract", 2L, id = "plugin.infer_beyond_cut")
    })

    test_that(sprintf("[PLUGINS] corte_carga: %s: treino ate O-1: FORBIDDEN dia O em diante nao entra no painel de treino; CONTROL hora de O-1 entra", tag), {
      x <- wx_ctx(exo, rule); cu <- x$cuts; st <- x$st; O <- st$O
      base <- rd5_panel(st, cu, "train")
      expect_true(all(lubridate::as_date(base$load$interval_start, tz = cu$tz) <= cu$last_data_day))
      expect_true(all(lubridate::as_date(base$weather_observed$valid_time, tz = cu$tz) <= cu$last_data_day))
      same <- function(a, b) all(vapply(c("load", "weather_observed"), function(n) rd_lib$.prevcarga_reach_same(a[[n]], b[[n]]), NA))
      d_o <- rd5_day_start(O)
      forb <- rd5_perturb(rd5_perturb(st, "load", after = d_o - 1), "weather_observed", after = d_o - 1)   # dia O em diante (carga e clima)
      expect_true(same(base, rd5_panel(forb, cu, "train")))            # o treino nao enxerga o dia O: mesmo painel => mesmo artefato
      ctl <- rd5_perturb(st, "load", after = rd5_day_start(O - 1L) + lubridate::hours(15L) - 1, at_or_before = rd5_day_start(O - 1L) + lubridate::hours(16L))
      expect_false(same(base, rd5_panel(ctl, cu, "train")))            # uma hora de O-1 muda o painel de treino
    })
  })
}
