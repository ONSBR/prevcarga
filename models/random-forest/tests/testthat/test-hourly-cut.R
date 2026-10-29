## test-hourly-cut.R -- RF.3, AC 2: corte intradiario (available_until) e grade de entrega.
library(testthat)

test_that("[PLUGINS] corte_horario: decks 09:00 x 09:30 -> 1o interval_end 09:30 / 10:00, comuns identicos, grade da fundacao", {
  d0 <- rf_inf_deck("09:00"); d1 <- rf_inf_deck("09:30")
  a <- rf_inf_run(d0); b <- rf_inf_run(d1)
  expect_identical(format(a$env$delivery$interval_end[[1L]]), "2026-10-01 09:30:00")
  expect_identical(format(b$env$delivery$interval_end[[1L]]), "2026-10-01 10:00:00")
  expect_lte(rf_inf_maxdiff(a$env$delivery, b$env$delivery), 1e-5)
  for (r in list(a, b)) {
    ie <- r$env$delivery$interval_end
    expect_identical(ie, rf_lib$prevcarga_delivery_grid(r$cuts, 9L))
    expect_identical(lubridate::tz(ie), RF_TZ)
    expect_false(anyDuplicated(ie) > 0L)
    expect_true(all(diff(as.numeric(ie)) == 1800))
    expect_identical(format(ie[[length(ie)]], "%Y-%m-%d %H:%M:%S"), "2026-10-10 00:00:00")
    expect_true(all(ie > r$cuts$available_until))
    expect_true(all(is.finite(r$env$delivery$y_pred)))
  }
  expect_identical(nrow(a$env$delivery) - nrow(b$env$delivery), 1L)
})

test_that("[PLUGINS] corte_horario: o slot O+1 00:00 vem do bloco O-1 e O+1 00:30 do bloco O (emenda sem spline atravessando)", {
  r <- rf_inf_run(rf_inf_deck("09:30"))
  p <- rf_plug()
  y1 <- p$rf_semihourly_rule(unname(r$env$values[1L, ])); y2 <- p$rf_semihourly_rule(unname(r$env$values[2L, ]))
  at <- function(hm) r$env$delivery$y_pred[r$env$delivery$interval_end ==
                                           lubridate::ymd_hm(paste("2026-10-02", hm), tz = RF_TZ)]
  ## bloco O-1: 216 horas a partir de O 00:00; fim da hora 23 de O (O+1 00:00) = interpolado da 24a ancora = y1[48]
  expect_identical(at("00:00"), y1[[48L]])
  expect_identical(at("00:30"), y2[[1L]])
  expect_identical(at("01:00"), y2[[2L]])
  ## O+9 00:00 (ultimo entregue) = interpolado da hora 192 do bloco O; o resto do bloco (O+9) fica fora da faixa
  expect_identical(r$env$delivery$y_pred[[nrow(r$env$delivery)]], y2[[384L]])
})
