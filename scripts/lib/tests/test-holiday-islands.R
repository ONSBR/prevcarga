## test-holiday-islands.R -- regra de datas da janela e ilhas de feriado (ADR-046; E3-T1). Calendario sintetico, sem store.
library(testthat)
library(data.table)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

hol <- function(codes, dates, area = "SECO")
  data.table(area_code = area, prevcarga_code = as.integer(codes), date = lubridate::ymd(dates))
lib <- function() pc_source_lib(c("errors", "holiday_islands"))

test_that("janela ano_civil: datas para 2023-06-01 e 2024-01-01", {
  e <- lib()
  w <- e$prevcarga_dessem_window("2023-06-01", "ano_civil")
  expect_equal(w$common_from, lubridate::ymd("2020-06-01"))
  expect_equal(w$common_to, lubridate::ymd("2023-05-31"))
  expect_equal(w$holiday_from, lubridate::ymd("2018-01-01"))
  expect_identical(w$mode, "ano_civil")
  expect_equal(e$prevcarga_dessem_window("2024-01-01")$holiday_from, lubridate::ymd("2019-01-01"))
})

test_that("janela referencia e modo invalido", {
  e <- lib()
  w <- e$prevcarga_dessem_window("2023-06-01", "referencia")
  expect_equal(w$common_from, lubridate::ymd("2018-06-02"))
  expect_equal(w$common_to, lubridate::ymd("2023-05-31"))
  expect_null(w$holiday_from)
  expect_error(e$prevcarga_dessem_window("2023-06-01", "xpto"), class = "prevcarga_error", regexp = "contract")
  expect_error(e$prevcarga_dessem_window("2023-06-01", c("ano_civil", "referencia")), class = "prevcarga_error", regexp = "contract")
})

test_that("ilhas de Corpus Christi antes da janela comum; Natal e codigo 0 sem ilha", {
  e <- lib()
  w <- e$prevcarga_dessem_window("2023-06-01", "ano_civil")
  h <- hol(c(5L, 5L, 9L, 0L), c("2018-05-31", "2019-06-20", "2021-12-25", "2019-03-01"))
  il <- e$prevcarga_holiday_islands(h, "2023-06-01", w$common_from, lubridate::ymd("2018-01-01"),
                                    holiday_from = w$holiday_from)
  expect_equal(il$from, lubridate::ymd(c("2018-05-24", "2019-06-13")))
  expect_equal(il$to, lubridate::ymd(c("2018-05-31", "2019-06-20")))
  expect_equal(il$island_id, 1:2)
  expect_equal(il$holiday_dates, list(lubridate::ymd("2018-05-31"), lubridate::ymd("2019-06-20")))
  expect_true(all(il$to < w$common_from))
  expect_true(all(il$to < lubridate::ymd("2023-06-01")))
})

test_that("corte em first_day, juncao de ilhas que se encostam e determinismo", {
  e <- lib()
  h <- hol(c(1L, 5L, 5L), c("2018-01-01", "2019-06-17", "2019-06-20"))
  a <- e$prevcarga_holiday_islands(h, "2023-01-01", lubridate::ymd("2020-01-01"), lubridate::ymd("2018-01-01"))
  b <- e$prevcarga_holiday_islands(h, "2023-01-01", lubridate::ymd("2020-01-01"), lubridate::ymd("2018-01-01"))
  expect_identical(a, b)
  expect_equal(nrow(a), 2L)
  expect_equal(a$from[1], lubridate::ymd("2018-01-01"))
  expect_equal(a$to[1], lubridate::ymd("2018-01-01"))
  expect_equal(a$from[2], lubridate::ymd("2019-06-10"))
  expect_equal(a$to[2], lubridate::ymd("2019-06-20"))
  expect_equal(a$holiday_dates[[2]], lubridate::ymd(c("2019-06-17", "2019-06-20")))
})

test_that("ilha que cruza common_from e truncada; feriado >= O ignorado; varias areas ordenadas", {
  e <- lib()
  cf <- lubridate::ymd("2020-06-01")
  h <- rbind(hol(5L, "2020-06-03", "SUL"), hol(5L, "2020-05-20", "NE"), hol(5L, "2023-06-01", "NE"))
  il <- e$prevcarga_holiday_islands(h, "2023-06-01", cf, lubridate::ymd("2018-01-01"),
                                    holiday_from = lubridate::ymd("2018-01-01"))
  expect_equal(il$area_code, c("NE", "SUL"))
  expect_equal(il$to, lubridate::ymd(c("2020-05-20", "2020-05-31")))
  expect_equal(il$from, lubridate::ymd(c("2020-05-13", "2020-05-27")))
})

test_that("zero ilhas devolve data.table vazia com o schema", {
  e <- lib()
  il <- e$prevcarga_holiday_islands(hol(9L, "2021-12-25"), "2023-06-01", lubridate::ymd("2020-06-01"),
                                    lubridate::ymd("2018-01-01"))
  expect_equal(nrow(il), 0L)
  expect_identical(names(il), c("area_code", "island_id", "from", "to", "holiday_dates"))
  expect_identical(class(il$from), "Date")
  expect_identical(class(il$island_id), "integer")
  expect_identical(class(il$holiday_dates), "list")
  expect_equal(nrow(e$prevcarga_holiday_islands(hol(0L, "2019-03-01"), "2023-06-01", lubridate::ymd("2020-06-01"),
                                                lubridate::ymd("2018-01-01"))), 0L)
})
