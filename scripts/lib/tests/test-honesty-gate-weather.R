## test-honesty-gate-weather.R -- canal de clima previsto do honesty_gate.R (M3.5, ADR-0024).
## Cada braco que afirma delta <= 1e-5 vem pareado com um que exige > 1e-5.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "deck_reader", "dates", "honesty_gate"))
for (n in ls(env, all.names = TRUE)) assign(n, env[[n]])
prevcarga_runtime_enforce()

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
expect_args_error <- function(expr, where) {
  e <- fail(expr)
  expect_false(is.null(e))
  expect_identical(e$id, "contract.honesty_gate_args")
  expect_match(conditionMessage(e), "^\\[prevcarga/contract\\]")
  expect_match(paste(e$where, collapse = " "), where, fixed = TRUE)
}

TZ <- "Etc/GMT+3"
O <- "2026-10-01"
au <- lubridate::force_tz(lubridate::ymd_hms("2026-10-01 09:00:00"), TZ)
lim_au  <- prevcarga_weather_issue_limit("available_until", lubridate::ymd(O), au, TZ)
lim_mid <- prevcarga_weather_issue_limit("origin_midnight_utc", lubridate::ymd(O), au, TZ)
panel <- pc_hg_panel(O, back = 8L, horizon = 9L)
weather <- pc_hg_weather(O)
utc <- function(x) format(lubridate::with_tz(x, "UTC"), "%H")

test_that("limites das duas regras (fixture) e emissoes por area", {
  expect_identical(format(lubridate::with_tz(lim_au, "UTC"), "%Y-%m-%d %H"), "2026-10-01 12")
  expect_identical(format(lubridate::with_tz(lim_mid, "UTC"), "%Y-%m-%d %H"), "2026-10-01 00")
  expect_identical(sort(unique(utc(weather$issue_time))), c("00", "06", "18"))
  expect_identical(lubridate::tz(weather$issue_time), TZ)
})

test_that("AC1 available_until: honesto HONESTO (CONTROL = 06Z); vazador 18Z VAZA", {
  h <- prevcarga_honesty_gate_weather(pc_wf_honest(lim_au), panel, weather, lim_au)
  expect_identical(h$verdict, "HONESTO")
  expect_lte(h$clean2_delta, 1e-5); expect_lte(h$forbidden_delta, 1e-5)
  expect_true(h$reach_ok); expect_gt(h$control_delta, 1e-5)
  expect_identical(h$arms$channel, "weather")
  expect_identical(h$arms$weather_issue_limit, "2026-10-01T12:00:00Z")
  n1 <- sum(weather$issue_time == min(weather$issue_time))
  expect_identical(h$arms$control$n_perturbed, n1)   # so a 06Z (uma emissao)
  expect_identical(h$arms$forbidden$n_perturbed, n1) # so a 18Z
  v <- prevcarga_honesty_gate_weather(pc_wf_issue(18), panel, weather, lim_au)
  expect_identical(v$verdict, "VAZA")
  expect_gt(v$forbidden_delta, 1e-5)
})

test_that("AC2 origin_midnight_utc: honesto HONESTO (CONTROL = 00Z); 06Z e 18Z VAZAM; 06Z legal na outra regra", {
  h <- prevcarga_honesty_gate_weather(pc_wf_honest(lim_mid), panel, weather, lim_mid)
  expect_identical(h$verdict, "HONESTO")
  expect_true(h$reach_ok); expect_gt(h$control_delta, 1e-5); expect_lte(h$forbidden_delta, 1e-5)
  n1 <- sum(weather$issue_time == min(weather$issue_time))
  expect_identical(h$arms$control$n_perturbed, n1)
  expect_identical(h$arms$forbidden$n_perturbed, 2L * n1)  # 06Z + 18Z
  expect_identical(prevcarga_honesty_gate_weather(pc_wf_issue(6), panel, weather, lim_mid)$verdict, "VAZA")
  expect_identical(prevcarga_honesty_gate_weather(pc_wf_issue(18), panel, weather, lim_mid)$verdict, "VAZA")
  ## a mesma 06Z e legal na 1a regra (honesta) e vazamento na 2a
  expect_identical(prevcarga_honesty_gate_weather(pc_wf_issue(6), panel, weather, lim_au)$verdict, "HONESTO")
})

test_that("multi-area: CONTROL e a mais fresca POR area", {
  w2 <- pc_hg_weather(O, areas = c("A1", "A2"))
  h <- prevcarga_honesty_gate_weather(pc_wf_honest(lim_au), panel, w2, lim_au)
  expect_identical(h$verdict, "HONESTO")
  n1 <- sum(w2$issue_time == min(w2$issue_time) & w2$area_code == "A1")
  expect_identical(h$arms$control$n_perturbed, 2L * n1)
})

test_that("AC3 cego -> HARNESS QUEBRADO; sem emissao acima do limite -> INDETERMINADO", {
  b <- prevcarga_honesty_gate_weather(pc_wf_blind(), panel, weather, lim_au)
  expect_identical(b$verdict, "HARNESS QUEBRADO")
  expect_lte(b$forbidden_delta, 1e-5); expect_lte(b$control_delta, 1e-5)
  none <- weather[weather$issue_time <= lim_au, ]
  i <- prevcarga_honesty_gate_weather(pc_wf_honest(lim_au), panel, none, lim_au)
  expect_false(i$reach_ok)
  expect_identical(i$verdict, "INDETERMINADO")
})

test_that("AC3 espiao: panel identico em todos os bracos; so colunas de valor das regioes mudam", {
  e <- new.env()
  prevcarga_honesty_gate_weather(pc_wf_spy(lim_au, e), panel, weather, lim_au)
  expect_length(e$calls, 4L)
  p0 <- data.table::as.data.table(panel); w0 <- data.table::as.data.table(weather)
  for (cl in e$calls) expect_identical(cl$panel, p0)
  expect_identical(e$calls[[1]]$weather, w0); expect_identical(e$calls[[2]]$weather, w0)
  rg <- .prevcarga_hgw_regions(weather, lim_au)
  for (k in 3:4) {
    w <- e$calls[[k]]$weather
    mask <- if (k == 3) rg$forbidden else rg$control
    for (cl in c("area_code", "issue_time", "valid_time")) expect_identical(w[[cl]], w0[[cl]])
    for (cl in c("temp_c", "rh_pct")) {
      expect_true(all(w[[cl]][!mask] == w0[[cl]][!mask]))
      expect_true(all(w[[cl]][mask] != w0[[cl]][mask]))
    }
  }
  ## entradas do chamador nao mutadas
  expect_identical(weather, pc_hg_weather(O))
})

test_that("AC4 entradas invalidas abortam contract.honesty_gate_args antes do run_fn", {
  called <- FALSE
  f <- function(panel, weather) { called <<- TRUE; pc_wf_blind()(panel, weather) }
  g <- function(...) prevcarga_honesty_gate_weather(f, panel, ...)
  expect_args_error(g(weather[, setdiff(names(weather), "issue_time")], lim_au), "weather")
  expect_args_error(g(weather, "2026-10-01 12:00:00"), "weather_issue_limit")
  expect_args_error(g(weather, c(lim_au, lim_mid)), "weather_issue_limit")
  expect_args_error(g(weather[, c("area_code", "issue_time", "valid_time")], lim_au), "weather")
  expect_args_error(g(weather, lim_au, tol = 0), "tol")
  expect_args_error(prevcarga_honesty_gate_weather("x", panel, weather, lim_au), "run_fn")
  expect_false(called)
  expect_false(any(c("ctx", "origin", "available_until") %in% names(formals(prevcarga_honesty_gate_weather))))
})
