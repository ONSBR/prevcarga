## test-reconcile-khat.R -- k-hat (R.1): sentinela por interval_start, relabelador 8-col, janela do fit, glob, ratio
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "schema_long", "deck_reader", "evaluation", "reconcile"))
ctx <- pc_rc_ctx()
topo <- env$prevcarga_topology(path = pc_topology_fixture(3L), root = tempdir())
canon <- c("area", "forecast_origin", "target_datetime", "lead", "y_pred", "y_true", "d_plus", "granularity_min")
## origem no fim do mes: o ultimo intervalo (rotulo D+1 00:00) cai em outro mes e outro dow
O <- "2026-01-31"

sentinel_k <- function(start) {
  1 + lubridate::hour(start) / 100 + lubridate::month(start) / 1e4 + lubridate::wday(start, week_start = 7) / 1e6
}

test_that("R.1 AC2 - sentinela: y_pred = BU_pred * k(bucket de interval_start); borda D+1 00:00 usa hora 23 de D", {
  long <- pc_leaves_long(topo, O, ctx)
  bu <- env$prevcarga_reconcile_bottomup(long, "SS2", topo, ctx)
  ap <- env$prevcarga_khat_apply(bu, pc_khat_profile_sentinel(), ctx)
  start <- ap$target_datetime - lubridate::minutes(30)   # recalculo independente do teste
  expect_equal(ap$y_pred, ap$BU_pred * sentinel_k(start), tolerance = 1e-9)
  last <- ap[nrow(ap), ]
  expect_identical(format(last$target_datetime, "%Y-%m-%d %H:%M", tz = pc_rc_tz), "2026-02-01 00:00")
  expect_equal(last$y_pred, last$BU_pred * (1 + 23 / 100 + 1 / 1e4 + 7 / 1e6), tolerance = 1e-12)  # jan, sabado(7), 23h
  expect_identical(attr(ap, "n_fallback"), 0L)
  ## CONTROL: bucketizar por interval_end (hora 0, fev, domingo) reprova o mesmo teste
  k_end <- sentinel_k(ap$target_datetime)
  expect_gt(max(abs(ap$y_pred - ap$BU_pred * k_end)), 1e-6)
  ## sem clamp: k fora da faixa do topology e preservado
  expect_gt(max(ap$y_pred / ap$BU_pred), topo$subsystems$SS2$khat_range[[2L]] - 0.2)
})

test_that("R.1 AC2 - relabelador: 8 colunas na ordem canonica, forecast_origin texto, y_true NA; Date reprova", {
  long <- pc_leaves_long(topo, O, ctx)
  bu <- env$prevcarga_reconcile_bottomup(long, "SS1", topo, ctx)
  ap <- env$prevcarga_khat_apply(bu, pc_khat_profile_sentinel(), ctx)
  out <- env$prevcarga_reconcile_long(ap, "SS1", ctx)
  expect_identical(names(out), canon)
  expect_identical(vapply(as.data.frame(out), function(c) class(c)[[1L]], ""),
                   stats::setNames(c("character", "character", "POSIXct", "integer", "numeric", "numeric", "integer",
                                     "integer"), canon))
  expect_identical(out$forecast_origin, rep(O, 48L))
  expect_true(all(is.na(out$y_true)))
  expect_true(all(out$granularity_min == 30L))
  expect_identical(out$y_pred, ap$y_pred)
  expect_true(isTRUE(env$prevcarga_schema_long_check(out)))
  expect_identical(env$prevcarga_assert_long(out, ctx)$area, out$area)
  ## FORBIDDEN: forecast_origin como Date reprova (tipo contra prevcarga_schema_long)
  dd <- as.data.frame(ap); dd$forecast_origin <- lubridate::ymd(dd$forecast_origin)
  e <- tryCatch(env$prevcarga_reconcile_long(dd, "SS1", ctx), prevcarga_error = identity)
  expect_identical(e$id, "contract.long_schema")
  bad <- as.data.frame(out); bad$forecast_origin <- lubridate::ymd(bad$forecast_origin)
  expect_false(identical(class(bad$forecast_origin), "character"))
  expect_false(identical(class(bad$forecast_origin)[[1L]], env$prevcarga_schema_long[["forecast_origin"]]))
})

mk_ratio <- function(f = function(d) 1 + (as.integer(d) %% 97L) / 1000)
  pc_truth_ratio(seq(lubridate::ymd("2023-06-01"), lubridate::ymd("2026-05-31"), by = "day"), f)
OR <- lubridate::ymd("2026-01-15")
day_of <- function(x) lubridate::as_date(x, tz = pc_rc_tz)

test_that("R.1 AC4 - janela [O-730, O-1] e [O-30, O-1]; k = media da janela; glob da janela", {
  r <- mk_ratio()
  fit <- env$prevcarga_khat_fit(r, OR, 730L)
  expect_identical(fit$window_from, lubridate::ymd("2024-01-16"))
  expect_identical(fit$window_to, lubridate::ymd("2026-01-14"))
  d <- day_of(r$interval_start)
  w <- r[d >= fit$window_from & d <= fit$window_to, ]
  expect_equal(fit$n_ratio, nrow(w)); expect_equal(nrow(w), 730L)
  b <- paste(lubridate::month(w$interval_start), lubridate::wday(w$interval_start, week_start = 7))
  ref <- tapply(w$ratio, b, mean)
  got <- fit$profile
  expect_true(all(got$hour == 12L))
  expect_equal(as.numeric(ref[paste(got$month, got$dow)]), got$k, tolerance = 1e-12)
  expect_equal(fit$glob, mean(w$ratio), tolerance = 1e-12)
  expect_gt(abs(fit$glob - mean(r$ratio)), 1e-9)   # glob NAO e a media de todo o historico (corrige o oraculo)
  f30 <- env$prevcarga_khat_fit(r, OR, 30L)
  expect_identical(f30$window_from, lubridate::ymd("2025-12-16"))
  expect_identical(f30$window_to, lubridate::ymd("2026-01-14"))
  expect_equal(f30$n_ratio, 30L)
  expect_false(isTRUE(all.equal(f30$profile$k, fit$profile$k[seq_along(f30$profile$k)])))
  ## FORBIDDEN: ratio em O (2026-01-15) e em O-731 (2024-01-15) nao mexem no perfil; CONTROL: O-1 mexe
  bump <- function(day) { x <- r; x$ratio[day_of(x$interval_start) == lubridate::ymd(day)] <- 5; x }
  expect_identical(env$prevcarga_khat_fit(bump("2026-01-15"), OR, 730L), fit)
  expect_identical(env$prevcarga_khat_fit(bump("2024-01-15"), OR, 730L), fit)
  expect_false(identical(env$prevcarga_khat_fit(bump("2026-01-14"), OR, 730L)$profile, fit$profile))
  expect_false(identical(env$prevcarga_khat_fit(bump("2024-01-16"), OR, 730L)$profile, fit$profile))  # ponta inclusiva
  expect_identical(env$prevcarga_khat_fit(bump("2026-01-15"), OR, 30L), f30)
})

test_that("R.1 AC4 - bucket sem observacao cai no glob (n_fallback, aviso); janela vazia => completude.khat_no_truth", {
  r <- mk_ratio()
  f30 <- env$prevcarga_khat_fit(r, OR, 30L)           # so a hora 12 observada
  bu <- env$prevcarga_reconcile_bottomup(pc_leaves_long(topo, O, ctx), "SS1", topo, ctx)
  w <- NULL
  ap <- withCallingHandlers(env$prevcarga_khat_apply(bu, f30, ctx),
    prevcarga_warning = function(c) { w <<- c; invokeRestart("muffleWarning") })
  expect_identical(w$id, "reconcile.khat_fallback")
  nfb <- attr(ap, "n_fallback")
  expect_gt(nfb, 0L)
  expect_equal(ap$y_pred / ap$BU_pred, ap$k)
  expect_equal(sum(abs(ap$k - f30$glob) < 1e-15), nfb)
  e <- tryCatch(env$prevcarga_khat_fit(r, lubridate::ymd("2020-01-01"), 730L), prevcarga_error = identity)
  expect_identical(e$id, "completude.khat_no_truth"); expect_identical(e$exit_code, 3L)
  ## perfil malformado
  e <- tryCatch(env$prevcarga_khat_apply(bu, list(profile = data.frame(a = 1), glob = 1), ctx), prevcarga_error = identity)
  expect_identical(e$id, "contract.khat_profile")
})

test_that("R.1 - khat_ratio: SS/BU por interval_end so com todas as folhas; fit a partir da verdade do leitor", {
  truth <- pc_truth_load(topo, "SS2", "2026-01-10", factor = 1.1)
  r <- env$prevcarga_khat_ratio(truth, "SS2", topo)
  expect_equal(nrow(r), 48L)
  expect_equal(r$ratio, rep(1.1, 48L), tolerance = 1e-12)
  expect_identical(names(r), c("interval_start", "interval_end", "ratio"))
  leaf <- topo$subsystems[["SS2"]]$areas[[1L]]
  r2 <- env$prevcarga_khat_ratio(pc_truth_load(topo, "SS2", "2026-01-10", miss = list(leaf = leaf, idx = 1:2)), "SS2", topo)
  expect_equal(nrow(r2), 46L)   # BU_true so com todas as folhas; nada preenchido com 0
  fit <- env$prevcarga_khat_fit(r, lubridate::ymd("2026-01-11"), 30L)
  expect_equal(fit$glob, 1.1, tolerance = 1e-12)
})
