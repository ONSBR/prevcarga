## test-honesty_gate-reach_control.R -- prevcarga_reach_control (R.4): module_fn sinteticos, tudo in-memory.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "honesty_gate"))
O <- lubridate::ymd("2026-01-15")

rc_input <- function() {
  days <- seq(O - 5L, O + 5L, by = "day")
  data.table::data.table(day = days, value = as.numeric(seq_along(days)))
}
rc_sum <- function(sel) function(x) data.table::data.table(grp = "all", y_pred = sum(x$value[sel(x$day)]))
rc_bump <- function(sel) function(x) { x[sel(x$day), value := value * 1.3 + 1]; x }
rc_arms <- function() list(forbidden = list(futuro = rc_bump(function(d) d >= O)),
                           control = list(ultimo_dia = rc_bump(function(d) d == O - 1L)))
rc_cmp <- list(value = "y_pred", keys = "grp")
rc_run <- function(fn, arms = rc_arms(), input = rc_input())
  env$prevcarga_reach_control(fn, input, arms, rc_cmp, 1e-5)

test_that("AC1 ignora a entrada => INDETERMINADO; le o futuro => VAZA; honesto => HONESTO; deltas e entrada", {
  inp <- rc_input(); pristine <- data.table::copy(inp)
  ign <- rc_run(function(x) data.table::data.table(grp = "all", y_pred = 7), input = inp)
  expect_identical(ign$veredito, "INDETERMINADO")
  expect_true(all(ign$deltas$delta == 0))
  fut <- rc_run(rc_sum(function(d) rep(TRUE, length(d))), input = inp)
  expect_identical(fut$veredito, "VAZA")
  expect_gt(fut$deltas$delta[fut$deltas$arm == "futuro"], 1e-5)
  hon <- rc_run(rc_sum(function(d) d < O), input = inp)
  expect_identical(hon$veredito, "HONESTO")
  expect_equal(hon$deltas$delta[hon$deltas$arm == "futuro"], 0)
  expect_gt(hon$deltas$delta[hon$deltas$arm == "ultimo_dia"], 1e-5)
  expect_s3_class(hon$deltas, "data.table")
  expect_named(hon$deltas, c("arm", "kind", "changed_input", "delta"))
  expect_identical(hon$deltas$kind, c("forbidden", "control"))
  expect_identical(nrow(hon$deltas), 2L)
  expect_true(all(hon$deltas$changed_input))
  expect_identical(inp, pristine)   # copia profunda por braco: a entrada original sai intacta
})

test_that("AC2 braco inocuo => INDETERMINADO; chaves diferentes = Inf; erro propaga; nenhum arquivo criado", {
  before <- list.files(tempdir(), recursive = TRUE, all.files = TRUE)
  arms <- rc_arms(); arms$control$inocuo <- rc_bump(function(d) d > O + 100L)
  r <- rc_run(rc_sum(function(d) d < O), arms)
  expect_false(r$deltas$changed_input[r$deltas$arm == "inocuo"])
  expect_identical(r$veredito, "INDETERMINADO")
  ## saida com chaves diferentes da referencia
  keyed <- function(x) data.table::data.table(grp = if (any(x$value > 20)) "outra" else "all", y_pred = 1)
  big <- list(forbidden = list(futuro = function(x) { x[day >= O, value := value + 100]; x }),
              control = list(ultimo_dia = rc_bump(function(d) d == O - 1L)))
  v <- rc_run(keyed, big)
  expect_identical(v$deltas$delta[v$deltas$arm == "futuro"], Inf)
  expect_identical(v$veredito, "VAZA")
  expect_error(rc_run(function(x) stop("boom")), "boom")
  expect_identical(list.files(tempdir(), recursive = TRUE, all.files = TRUE), before)
  expect_error(env$prevcarga_reach_control("nao", rc_input(), rc_arms(), rc_cmp), class = "prevcarga_error")
})
