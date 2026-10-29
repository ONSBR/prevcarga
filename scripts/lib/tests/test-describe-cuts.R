## test-describe-cuts.R -- describe com cortes por membro derivados em dates.R (M2.5; re-wire do M1.5).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
dv <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli", "topology", "provenance", "deck_reader",
                      "dates", "introspect"))
mk <- function(origin = "2026-10-01", ...) dv$prevcarga_ctx(c(list(`--origin` = origin), list(...)), pc_root(), NULL)
row <- function(d, m) Filter(function(r) r$model == m, d$cuts$members)[[1L]]

test_that("LGBM com data_lag_days injetado: train 2026-09-29, last_data_day 2026-09-28 (descriptor); sem injecao 2026-09-30 (default)", {
  d <- dv$prevcarga_build_describe(mk(), model = "lgbm", data_lag_days = c(lgbm = 2L))
  r <- row(d, "lgbm")
  expect_identical(r$train_origin, "2026-09-29"); expect_identical(r$last_data_day, "2026-09-28")
  expect_identical(r$data_lag_days, 2L); expect_identical(r$data_lag_source, "descriptor")
  d0 <- dv$prevcarga_build_describe(mk(), model = "lgbm")
  r0 <- row(d0, "lgbm")
  expect_identical(r0$train_origin, "2026-09-29"); expect_identical(r0$last_data_day, "2026-09-30")
  expect_identical(r0$data_lag_days, 0L); expect_identical(r0$data_lag_source, "default")
  txt <- dv$prevcarga_render_describe(d)
  expect_true(any(grepl("lgbm .*train_origin=2026-09-29 last_data_day=2026-09-28 data_lag_days=2 \\(descriptor\\)", txt)))
  expect_length(d$cuts$members, 1L)
})

test_that("dessem-linear: train_origin 2026-10-01 e last_data_day 2026-09-30 (origin_scoped)", {
  r <- row(dv$prevcarga_build_describe(mk(), model = "dessem-linear"), "dessem-linear")
  expect_identical(r$split, "origin_scoped")
  expect_identical(r$train_origin, "2026-10-01"); expect_identical(r$last_data_day, "2026-09-30")
})

test_that("sem --model: tabela de 5 membros; available_until anotado; placeholder do M1.5 ausente", {
  d <- dv$prevcarga_build_describe(mk(), data_lag_days = c(lgbm = 2L))
  expect_length(d$cuts$members, 5L)
  expect_setequal(vapply(d$cuts$members, function(r) r$model, ""),
                  c("dessem-linear", "dessem-regdin", "holt-winters", "lgbm", "random-forest"))
  txt <- dv$prevcarga_render_describe(d)
  expect_true(any(grepl("available_until: derivado do deck (via validate-deck)", txt, fixed = TRUE)))
  expect_false(any(grepl("derivado no M2", txt, fixed = TRUE)))
  expect_identical(d$cuts$available_until_source, "derivado do deck (via validate-deck)")
  expect_false(any(grepl("[^ -~]", txt)))
  ## sem origem: sem tabela de cortes (nao ha data a derivar), nunca o relogio
  d1 <- dv$prevcarga_build_describe(dv$prevcarga_ctx(list(), pc_root(), NULL))
  expect_length(d1$cuts$members, 0L)
})

test_that("com deck legivel o available_until sai derivado do deck; deck ilegivel cai no corte de treino (so contract/completude)", {
  d <- pc_deck_clean(days = seq(lubridate::ymd("2026-09-29"), lubridate::ymd("2026-10-01"), by = "day"),
                     dir = withr::local_tempdir())
  ctx <- mk(`--data` = d)
  res <- dv$prevcarga_build_describe(ctx, model = "dessem-linear")
  expect_identical(res$cuts$available_until, "2026-10-02 00:00")
  expect_true(any(grepl("available_until: derivado do deck (via validate-deck) = 2026-10-02 00:00",
                        dv$prevcarga_render_describe(res), fixed = TRUE)))
  bad <- withr::local_tempdir()
  rb <- dv$prevcarga_build_describe(mk(`--data` = bad), model = "lgbm")
  expect_null(rb$cuts$available_until)
  expect_identical(row(rb, "lgbm")$train_origin, "2026-09-29")
  expect_true(grepl("indisponivel", rb$cuts$note))
})
