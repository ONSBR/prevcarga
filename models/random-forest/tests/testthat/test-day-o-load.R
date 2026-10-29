## test-day-o-load.R -- RF.3, AC 1: o RF nao le a carga do dia O; O-1 cobre o dia O e O cobre O+1..O+8.
library(testthat)

test_that("[PLUGINS] corte_dia_o: FORBIDDEN carga do dia O (inclusive 08:00-09:30) nao muda a entrega", {
  d_ref <- rf_inf_deck("09:30")
  d_pert <- rf_inf_perturb(d_ref, RF_O, RF_O)
  on.exit(unlink(d_pert, recursive = TRUE))
  a <- rf_inf_run(d_ref); b <- rf_inf_run(d_pert)
  expect_identical(format(a$cuts$available_until), "2026-10-01 09:30:00")
  ## a perturbacao existe no deck (sem ela o teste seria vazio): o leitor devolve carga diferente do dia O
  la <- rf_fx_panel(d_ref, a$cuts)$load; lb <- rf_fx_panel(d_pert, b$cuts)$load
  expect_gt(max(abs(la$load_mwh - lb$load_mwh)), 1)
  expect_lte(rf_inf_maxdiff(a$env$delivery, b$env$delivery), 1e-5)
  expect_identical(a$env$values, b$env$values)
  expect_identical(a$env$delivery, b$env$delivery)
})

test_that("[PLUGINS] corte_dia_o: CONTROL O-1 muda O+1..O+8 e deixa o dia O identico; O-2 muda o dia O", {
  d_ref <- rf_inf_deck("09:30")
  ref <- rf_inf_run(d_ref)
  O1 <- lubridate::force_tz(lubridate::as_datetime(RF_O + 1L), RF_TZ)
  dayO <- ref$env$delivery$interval_end <= O1
  d1 <- rf_inf_perturb(d_ref, RF_O - 1L, RF_O - 1L)
  d2 <- rf_inf_perturb(d_ref, RF_O - 2L, RF_O - 2L)
  on.exit(unlink(c(d1, d2), recursive = TRUE))
  p1 <- rf_inf_run(d1); p2 <- rf_inf_run(d2)
  ## carga de O-1: so o bloco O (O+1..O+8) muda; o bloco O-1 (dia O) fica identico
  expect_identical(p1$env$values[1L, ], ref$env$values[1L, ])
  expect_gt(max(abs(p1$env$values[2L, ] - ref$env$values[2L, ])), 1e-5)
  expect_identical(p1$env$delivery[dayO], ref$env$delivery[dayO])
  expect_gt(rf_inf_maxdiff(p1$env$delivery, ref$env$delivery, keep = !dayO), 1e-5)
  ## carga de O-2: o bloco O-1 (dia O) muda
  expect_gt(max(abs(p2$env$values[1L, ] - ref$env$values[1L, ])), 1e-5)
  expect_gt(rf_inf_maxdiff(p2$env$delivery, ref$env$delivery, keep = dayO), 1e-5)
})
