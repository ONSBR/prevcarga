## test-day-complete.R -- predicado unico prevcarga_day_complete (M2.3). Matriz 48/47/NA/duplicata/:15 sobre a
## carga no formato do leitor; CONTROL: contar por interval_end reprova o dia D.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "validation"))
prevcarga_day_complete <- env$prevcarga_day_complete

TZ <- "Etc/GMT+3"
D <- lubridate::ymd("2026-10-01")
grid <- function(n = 48L, area = "A") {
  s <- lubridate::force_tz(lubridate::as_datetime(D), TZ) + lubridate::minutes(30L * (seq_len(n) - 1L))
  x <- data.table::data.table(area_code = area, interval_start = s, interval_end = s + lubridate::minutes(30L),
                              load_mwh = seq_len(n) + 0)
  data.table::setattr(x, "tz", TZ)
  x
}

test_that("matriz 48/47/NA/duplicata/:15", {
  expect_identical(env$prevcarga_slots_per_day, 48L)
  r48 <- prevcarga_day_complete(grid(48L))
  expect_identical(names(r48), c("area_code", "date", "n_slots", "complete"))
  expect_equal(r48$date, D); expect_identical(r48$n_slots, 48L); expect_true(r48$complete)
  r47 <- prevcarga_day_complete(grid(47L))
  expect_identical(r47$n_slots, 47L); expect_false(r47$complete)
  g <- grid(48L); g[10L, load_mwh := NA_real_]
  rna <- prevcarga_day_complete(g)
  expect_identical(rna$n_slots, 47L); expect_false(rna$complete)
  g <- rbind(grid(48L), grid(48L)[5L]); data.table::setattr(g, "tz", TZ)
  rdup <- prevcarga_day_complete(g)
  expect_identical(rdup$n_slots, 48L); expect_true(rdup$complete)
  g <- grid(48L)
  g[48L, `:=`(interval_start = interval_start - lubridate::minutes(15L))]   # um valido fora da grade (:15)
  g <- g[-1L]; data.table::setattr(g, "tz", TZ)
  rg <- prevcarga_day_complete(g)
  expect_identical(rg$n_slots, 46L); expect_false(rg$complete)
})

test_that("data pelo interval_start no fuso da carga; CONTROL por interval_end", {
  x <- rbind(grid(48L), {
    s <- lubridate::force_tz(lubridate::as_datetime(D + 1L), TZ)
    data.table::data.table(area_code = "A", interval_start = s, interval_end = s + lubridate::minutes(30L), load_mwh = 1)
  })
  data.table::setattr(x, "tz", TZ)
  r <- prevcarga_day_complete(x)
  expect_equal(r[date == D]$n_slots, 48L); expect_true(r[date == D]$complete)
  ## CONTROL: contar pelo fim do intervalo faz o dia D perder o primeiro intervalo e ganhar o 00:00 de D+1
  by_end <- x[, list(n = data.table::uniqueN(interval_end)), by = list(date = lubridate::as_date(interval_end, tz = TZ))]
  expect_identical(by_end[date == D]$n, 47L)
  expect_identical(by_end[date == D + 1L]$n, 2L)
})

test_that("varias areas e coluna ausente", {
  x <- rbind(grid(48L, "A"), grid(47L, "B")); data.table::setattr(x, "tz", TZ)
  r <- prevcarga_day_complete(x)
  expect_identical(r$area_code, c("A", "B")); expect_identical(r$complete, c(TRUE, FALSE))
  e <- tryCatch(prevcarga_day_complete(x[, list(area_code, interval_start)]), prevcarga_error = identity)
  expect_identical(e$id, "contract.schema_missing")
})
