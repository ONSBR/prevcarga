## test-reconcile-honesty.R -- reach-control das duas bordas da janela do k-hat (R.4). Leitor stub (nenhum deck
## real), state_dir novo por braco, tudo em tempdir. Janela correta = [O-W, O-1], W = 60 aqui.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long",
                       "deck_reader", "dates", "evaluation", "honesty_gate", "reconcile"))
Od <- lubridate::ymd("2026-01-15")
W <- 60L
SS <- "SS1"

rh_mock <- function(..., .frame = parent.frame()) {
  for (n in names(list(...))) {
    old <- env[[n]]
    withr::defer(assign(n, old, envir = env), envir = .frame)
    assign(n, list(...)[[n]], envir = env)
  }
}

rh_topo_path <- pc_topology_fixture(3L)
rh_topo <- env$prevcarga_topology(path = rh_topo_path, root = tempdir())

## verdade de entrada: O-120 .. O+5 do subsistema e das suas folhas (data.table, area_code/interval_*/load_mwh)
rh_truth <- function() {
  days <- seq(Od - 120L, Od + 5L, by = "day")
  data.table::as.data.table(do.call(rbind, lapply(days, function(d) pc_truth_load(rh_topo, SS, d))))
}

## O leitor devolve a verdade com folga de 1 dia nas duas pontas ([from-1, to+1]); o ajuste real recorta a janela.
## Assim a mutacao (janela errada injetada no ajuste) enxerga os dias das bordas e de O.
rh_reader <- function(truth) function(ctx, areas = NULL, from = NULL, to = NULL, ...) {
  d <- lubridate::as_date(truth$interval_start, tz = pc_rc_tz)
  out <- truth[truth$area_code %in% areas & d >= from - 1L & d <= to + 1L]
  data.table::setattr(out, "load_label", "start"); data.table::setattr(out, "tz", pc_rc_tz)
  out
}

## treina com state_dir NOVO e devolve o perfil congelado (hour, month, dow, k) + glob
rh_train <- function(truth) {
  d <- tempfile("rh-state-"); dir.create(file.path(d, "artifacts"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  ctx <- list(state_dir = d, tz = pc_rc_tz, load_label = "start", topology_path = rh_topo_path, root = tempdir())
  rh_mock(prevcarga_resolve_cuts = function(ctx, origin, ...) list(cuts = data.frame(last_data_day = origin - 1L)),
          prevcarga_read_load = rh_reader(truth))
  r <- suppressWarnings(env$prevcarga_reconcile_train(ctx, Od, SS, W))
  env$prevcarga_khat_thaw(ctx, SS, r[[SS]]$versao)
}

## BU_pred fixo: um intervalo (12:00) em cada dia de [O-W, O-1] => cobre todos os (hora, mes, dow) da janela
rh_bu <- function() {
  days <- seq(Od - W, Od - 1L, by = "day")
  data.frame(area = SS, forecast_origin = format(Od), target_datetime = lubridate::ymd_hm(paste(format(days), "12:30"), tz = pc_rc_tz),
             lead = 0L, BU_pred = 100, d_plus = 0L, granularity_min = 30L)
}

rh_fn_k <- function(truth) {
  art <- rh_train(truth)
  data.table::as.data.table(art$profile)[, c("hour", "month", "dow", "k"), with = FALSE]
}
rh_fn_y <- function(truth) {
  art <- rh_train(truth)
  ap <- env$prevcarga_khat_apply(rh_bu(), list(profile = art$profile, glob = art$glob), list(tz = pc_rc_tz))
  data.table::as.data.table(ap)[, c("target_datetime", "y_pred"), with = FALSE]
}

rh_arms <- function() env$prevcarga_khat_reach_arms(Od, W, SS)
rh_check <- function() {
  truth <- rh_truth()
  list(k = env$prevcarga_reach_control(rh_fn_k, truth, rh_arms(), list(value = "k", keys = c("hour", "month", "dow"))),
       y = env$prevcarga_reach_control(rh_fn_y, truth, rh_arms(), list(value = "y_pred", keys = "target_datetime")))
}

test_that("AC3 janela correta [O-W, O-1]: FORBIDDEN delta 0 e CONTROL delta > 1e-5 em k e em y_pred => HONESTO", {
  r <- rh_check()
  for (ch in r) {
    expect_identical(ch$veredito, "HONESTO")
    d <- ch$deltas
    expect_identical(sort(d$arm), sort(c("futuro", "antes_da_janela", "ultimo_dia", "primeiro_dia")))
    expect_true(all(d$changed_input))
    expect_true(all(d$delta[d$kind == "forbidden"] == 0))
    expect_true(all(d$delta[d$kind == "control"] > 1e-5))
  }
})

## mutacao injetada no ajuste: usa a janela absoluta [lo, hi] em vez de [O-W, O-1]
rh_mutant <- function(lo, hi, .frame = parent.frame()) {
  fit_orig <- env$prevcarga_khat_fit
  rh_mock(prevcarga_khat_fit = function(ratio, origin, window_days = 60L)
            fit_orig(ratio, hi + 1L, as.integer(hi - lo) + 1L), .frame = .frame)
}

test_that("AC4 mutacoes da janela (59 dias, 61 dias, incluindo O) => veredito != HONESTO", {
  casos <- list(m59 = list(lo = Od - W + 1L, hi = Od - 1L, esperado = "INDETERMINADO"),
                m61 = list(lo = Od - W - 1L, hi = Od - 1L, esperado = "VAZA"),
                mO  = list(lo = Od - W, hi = Od, esperado = "VAZA"))
  for (nm in names(casos)) {
    cs <- casos[[nm]]
    local({
      rh_mutant(cs$lo, cs$hi)
      r <- rh_check()
      for (ch in r) {
        expect_false(identical(ch$veredito, "HONESTO"), info = nm)
        expect_identical(ch$veredito, cs$esperado, info = nm)
      }
      d <- r$k$deltas
      if (nm == "m59") expect_equal(d$delta[d$arm == "primeiro_dia"], 0)
      if (nm == "m61") expect_gt(d$delta[d$arm == "antes_da_janela"], 1e-5)
      if (nm == "mO")  expect_gt(d$delta[d$arm == "futuro"], 1e-5)
    })
  }
})
