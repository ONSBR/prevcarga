## test-honesty-matrix.R -- bateria em tabela do honesty-gate (M3.V; M3.2 canal y_true, M3.5 canal de clima).
## Uma linha por run_fn sintetico com veredito esperado; todo braco FORBIDDEN (delta <= 1e-5) vem pareado com um
## CONTROL (delta > 1e-5). O `cut` chega pronto do fixture (nunca calculado no gate). Falha nomeia o caso (info).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "deck_reader", "dates", "honesty_gate"))
for (n in ls(env, all.names = TRUE)) assign(n, env[[n]])
prevcarga_runtime_enforce()

TZ <- "Etc/GMT+3"
O <- "2026-10-01"
tt <- function(s) lubridate::force_tz(lubridate::ymd_hms(s), TZ)
end_of <- function(day) lubridate::force_tz(lubridate::as_datetime(day + 1L), TZ)   # fim do dia = 00:00 do seguinte
TOL <- 1e-5
cut <- tt("2026-10-01 09:00:00")
panel_full <- pc_hg_panel(O, back = 8, horizon = 9)
panel_nofut <- pc_hg_panel_no_future(cut, O, back = 8, horizon = 9)

## ---- tabela de casos (fabrica(cut) -> run_fn; painel; veredito; faixa dos deltas) -----------------------------
hg_cases <- data.frame(
  name     = c("honesto", "vazador", "spill_d9", "cego", "so_membros", "nao_det",
               "combina", "regiao_vazia", "ruido_1e-10", "delta_1e-3"),
  expected = c("HONESTO", "VAZA", "VAZA", "HARNESS QUEBRADO", "HARNESS QUEBRADO",
               "INDETERMINADO", "HONESTO", "INDETERMINADO", "HONESTO", "VAZA"),
  reach_ok = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
  forb     = c("le", "gt", "gt", "le", "le", NA, "le", "le", "le", "gt"),   # forbidden_delta <= tol | > tol
  ctrl     = c("gt", NA, NA, "le", "le", NA, "gt", NA, "gt", NA),            # control_delta <= tol | > tol
  panel    = c("full", "full", "full", "full", "full", "full", "full", "no_future", "full", "full"),
  stringsAsFactors = FALSE)
hg_cases$make <- list(
  pc_rf_infer_honest, pc_rf_infer_peek, function(cut) pc_rf_spill(cut, 9L), function(cut) pc_rf_blind(),
  pc_rf_members_only, pc_rf_nondet, pc_rf_combina, pc_rf_infer_honest,
  function(cut) pc_rf_tiny(cut, 1e-10), function(cut) pc_rf_tiny(cut, 1e-3))
hg_panel <- function(kind) if (identical(kind, "no_future")) panel_nofut else panel_full
hg_run <- function(i, horizon = 9, cut_ = cut) {
  r <- hg_cases[i, ]
  prevcarga_honesty_gate(r$make[[1L]](cut_), hg_panel(r$panel), cut_, horizon = horizon)
}
hg_assert <- function(row, g) {
  expect_identical(g$verdict, row$expected, info = row$name)
  expect_identical(g$reach_ok, row$reach_ok, info = row$name)
  if (!is.na(row$forb)) {
    if (row$forb == "le") expect_lte(g$forbidden_delta, TOL) else expect_gt(g$forbidden_delta, TOL)
  }
  if (!is.na(row$ctrl)) {
    if (row$ctrl == "le") expect_lte(g$control_delta, TOL) else expect_gt(g$control_delta, TOL)
  }
}

for (i in seq_len(nrow(hg_cases))) local({
  i <- i
  test_that(paste0("bateria y_true [", hg_cases$name[[i]], "] -> ", hg_cases$expected[[i]]), {
    g <- hg_run(i)
    hg_assert(hg_cases[i, ], g)
    if (hg_cases$name[[i]] == "nao_det") expect_gt(g$clean2_delta, TOL) else expect_lte(g$clean2_delta, TOL)
  })
})

test_that("os 4 vereditos sao alcancados pela bateria (AC 11 do epico)", {
  got <- vapply(seq_len(nrow(hg_cases)), function(i) hg_run(i)$verdict, "")
  expect_setequal(got, c("HONESTO", "VAZA", "INDETERMINADO", "HARNESS QUEBRADO"))
  expect_identical(got, hg_cases$expected)
})

test_that("[CONTROL] uma linha divergente falha nomeando o caso", {
  bad <- hg_cases[hg_cases$name == "honesto", ]
  bad$expected <- "VAZA"; bad$name <- "caso-divergente-xyz"
  g <- hg_run(which(hg_cases$name == "honesto"))
  expect_failure(expect_identical(g$verdict, bad$expected, info = bad$name), "caso-divergente-xyz")
})

test_that("regiao vazia: INDETERMINADO com reach_ok FALSE e 0 alvos perturbados (nunca HONESTO vacuo)", {
  g <- hg_run(which(hg_cases$name == "regiao_vazia"))
  expect_false(g$reach_ok)
  expect_identical(g$arms$forbidden$n_perturbed, 0L)
  expect_gt(prevcarga_honesty_gate(pc_rf_infer_honest(cut), panel_full, cut, 9)$arms$forbidden$n_perturbed, 0L)
})

test_that("ruido 1e-10 e delta 1e-3 ficam em lados opostos do limiar de 1e-5 (par FORBIDDEN/CONTROL)", {
  lo <- hg_run(which(hg_cases$name == "ruido_1e-10")); hi <- hg_run(which(hg_cases$name == "delta_1e-3"))
  expect_gt(lo$forbidden_delta, 0); expect_lte(lo$forbidden_delta, TOL)
  expect_gt(hi$forbidden_delta, TOL)
})

## ---- dentes de cobertura -----------------------------------------------------------------------------------------
test_that("spill so em d_plus = 9: horizon 9 -> VAZA, horizon 8 -> HONESTO (so o horizonte cheio pega)", {
  i <- which(hg_cases$name == "spill_d9")
  v9 <- hg_run(i, horizon = 9); v8 <- hg_run(i, horizon = 8)
  expect_identical(v9$verdict, "VAZA"); expect_gt(v9$forbidden_delta, TOL)
  expect_identical(v8$verdict, "HONESTO"); expect_lte(v8$forbidden_delta, TOL)
  expect_gt(v8$control_delta, TOL); expect_true(v8$reach_ok)
  ## CONTROL do proprio run_fn: com d = 8 o horizon 8 pega
  expect_identical(prevcarga_honesty_gate(pc_rf_spill(cut, 8L), panel_full, cut, 8)$verdict, "VAZA")
})

## gate-de-referencia escrito NO TESTE: perturba y_true E os membros m1/m2 da regiao proibida inteira
ref_forbidden_delta <- function(run_fn, panel, cut, horizon = 9, perturb = function(y) y * 1.30 + 1) {
  ie <- panel$interval_end
  m <- ie > cut & ie <= cut + lubridate::days(horizon)
  p2 <- panel
  for (cl in c("y_true", "m1", "m2")) p2[[cl]][m] <- perturb(panel[[cl]][m])
  a <- run_fn(panel); b <- run_fn(p2)
  max(abs(a$y_pred - b$y_pred))
}

test_that("estilo combina: HONESTO no gate real (pin so-y_true) e > 1e-5 no gate-de-referencia (membros)", {
  g <- hg_run(which(hg_cases$name == "combina"))
  expect_identical(g$verdict, "HONESTO"); expect_lte(g$forbidden_delta, TOL)
  expect_gt(ref_forbidden_delta(pc_rf_combina(cut), panel_full, cut), TOL)
  ## CONTROL do gate-de-referencia: um run_fn honesto fica <= 1e-5 mesmo perturbando os membros
  expect_lte(ref_forbidden_delta(pc_rf_infer_honest(cut), panel_full, cut), TOL)
  ## e o vazador e pego pelos dois
  expect_gt(ref_forbidden_delta(pc_rf_infer_peek(cut), panel_full, cut), TOL)
})

test_that("determinismo do proprio gate: duas execucoes do mesmo caso sao identical", {
  for (nm in c("honesto", "combina", "spill_d9", "so_membros")) {
    i <- which(hg_cases$name == nm)
    expect_identical(hg_run(i), hg_run(i), info = nm)
  }
})

## ---- treino por classe: cut = fim de last_data_day = O-1-data_lag_days -------------------------------------------
o_d <- lubridate::ymd(O)
cut_of <- function(lag) end_of(prevcarga_last_data_day(o_d, lag))
tr_scoped <- prevcarga_train_cut(list(split = "origin_scoped", horizon_days = 9L), o_d)
tr_agn    <- prevcarga_train_cut(list(split = "origin_agnostic", horizon_days = 9L), o_d)

test_that("fixture dos cortes: last_data_day = O-1-lag; train_origin por classe (ADR-0023)", {
  expect_identical(format(cut_of(0L), "%Y-%m-%d %H:%M:%S"), "2026-10-01 00:00:00")
  expect_identical(format(cut_of(2L), "%Y-%m-%d %H:%M:%S"), "2026-09-29 00:00:00")
  expect_identical(tr_agn$train_origin, o_d - 10L); expect_identical(tr_scoped$train_origin, o_d)
  expect_identical(tr_agn$last_data_day, tr_scoped$last_data_day)
})

train_cases <- list(
  list(name = "origin_agnostic H=9", rf = pc_rf_train_agnostic(O, 9L), cut = cut_of(0L), expected = "HONESTO"),
  list(name = "origin_scoped", rf = pc_rf_train_scoped(O), cut = cut_of(0L), expected = "HONESTO"),
  list(name = "vazador (usa o dia O)", rf = pc_rf_train_leaky(O), cut = cut_of(0L), expected = "VAZA"),
  list(name = "CONTROL agnostic com cut = fim de train_origin", rf = pc_rf_train_agnostic(O, 9L),
       cut = end_of(tr_agn$train_origin), expected = "VAZA"),
  list(name = "atraso 2, cut = fim de O-3", rf = pc_rf_train_lagged(O, 2L), cut = cut_of(2L), expected = "HONESTO"),
  list(name = "CONTROL atraso 2, cut = fim de O-1", rf = pc_rf_train_lagged(O, 2L), cut = cut_of(0L),
       expected = "HARNESS QUEBRADO"),
  list(name = "vazador de atraso (usa O-3 com cut O-3)", rf = pc_rf_train_lagged(O, 1L), cut = cut_of(2L),
       expected = "VAZA"))
for (cs in train_cases) local({
  cs <- cs
  test_that(paste0("treino [", cs$name, "] -> ", cs$expected), {
    g <- prevcarga_honesty_gate(cs$rf, panel_full, cs$cut, horizon = 9)
    expect_identical(g$verdict, cs$expected, info = cs$name)
    if (cs$expected == "HONESTO") { expect_lte(g$forbidden_delta, TOL); expect_gt(g$control_delta, TOL) }
    if (cs$expected == "VAZA") expect_gt(g$forbidden_delta, TOL)
    if (cs$expected == "HARNESS QUEBRADO") { expect_lte(g$forbidden_delta, TOL); expect_lte(g$control_delta, TOL) }
  })
})

## ---- canal de clima: matriz nas duas regras de weather_issue_cutoff ------------------------------------------------
au <- tt("2026-10-01 09:00:00")
lim <- list(available_until = prevcarga_weather_issue_limit("available_until", o_d, au, TZ, 0L),
            origin_midnight_utc = prevcarga_weather_issue_limit("origin_midnight_utc", o_d, au, TZ))
weather <- pc_hg_weather(O)
hour_utc <- function(x) as.integer(format(lubridate::with_tz(x, "UTC"), "%H"))

wx_cases <- data.frame(
  rule = rep(c("available_until", "origin_midnight_utc"), each = 4L),
  run  = rep(c("honesto", "emissao_06Z", "emissao_18Z", "cego"), times = 2L),
  expected = c("HONESTO", "HONESTO", "VAZA", "HARNESS QUEBRADO",     # available_until (limite 12Z)
               "HONESTO", "VAZA", "VAZA", "HARNESS QUEBRADO"),       # origin_midnight_utc (limite 00Z)
  control_hour = c(6L, 6L, 6L, 6L, 0L, 0L, 0L, 0L),                  # emissao mais fresca <= limite
  stringsAsFactors = FALSE)
wx_run <- function(rule, run) {
  l <- lim[[rule]]
  switch(run, honesto = pc_wf_honest(l), emissao_06Z = pc_wf_issue(6), emissao_18Z = pc_wf_issue(18), cego = pc_wf_blind())
}
for (i in seq_len(nrow(wx_cases))) local({
  w <- wx_cases[i, ]
  test_that(paste0("clima [", w$rule, " / ", w$run, "] -> ", w$expected), {
    g <- prevcarga_honesty_gate_weather(wx_run(w$rule, w$run), panel_full, weather, lim[[w$rule]])
    expect_identical(g$verdict, w$expected, info = paste(w$rule, w$run))
    expect_identical(g$arms$channel, "weather")
    expect_lte(g$clean2_delta, TOL)
    if (w$expected == "HONESTO") { expect_lte(g$forbidden_delta, TOL); expect_gt(g$control_delta, TOL) }
    if (w$expected == "VAZA") expect_gt(g$forbidden_delta, TOL)
    if (w$expected == "HARNESS QUEBRADO") { expect_lte(g$forbidden_delta, TOL); expect_lte(g$control_delta, TOL) }
  })
})

test_that("clima: o limite vem pronto de prevcarga_weather_issue_limit e a emissao perturbada no CONTROL e a exata", {
  expect_identical(format(lubridate::with_tz(lim$available_until, "UTC"), "%H"), "12")
  expect_identical(format(lubridate::with_tz(lim$origin_midnight_utc, "UTC"), "%H"), "00")
  for (rule in names(lim)) {
    e <- new.env()
    prevcarga_honesty_gate_weather(pc_wf_spy(lim[[rule]], e), panel_full, weather, lim[[rule]])
    expect_length(e$calls, 4L)
    chg <- function(k) {
      w <- e$calls[[k]]$weather
      unique(hour_utc(w$issue_time[w$temp_c != e$calls[[1L]]$weather$temp_c]))
    }
    expect_identical(chg(4L), if (rule == "available_until") 6L else 0L, info = rule)   # CONTROL: mais fresca <= limite
    expect_setequal(chg(3L), if (rule == "available_until") 18L else c(6L, 18L))          # FORBIDDEN: > limite
  }
})

test_that("clima: o painel (y_true e membros) segue intocado em todos os bracos", {
  e <- new.env()
  prevcarga_honesty_gate_weather(pc_wf_spy(lim$available_until, e), panel_full, weather, lim$available_until)
  p0 <- data.table::as.data.table(panel_full)
  for (cl in e$calls) expect_identical(cl$panel, p0)
})

## ---- TBD: nao ha caso de pico ou de molde enquanto os ADRs nao fecham ------------------------------------------------
test_that("[TBD: ADR-0028] metrica de pico da bateria", { skip("TBD: ADR-0028 (metrica de pico) ainda nao decidida") })
test_that("[TBD: ADR-0029] escopo do molde (forecast_origin == origin, reach por saida)", {
  skip("TBD: ADR-0029 (escopo do molde do honesty-gate) ainda nao decidido")
})
