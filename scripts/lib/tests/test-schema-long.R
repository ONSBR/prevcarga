## test-schema-long.R -- constante do painel LONG e conferencia de nome+ordem (M1.2b).
## Usa data.frame: a conferencia so le names(); data.table nao e dependencia do M1.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "schema_long"))
chk <- env$prevcarga_schema_long_check
canon <- c("area", "forecast_origin", "target_datetime", "lead", "y_pred", "y_true", "d_plus", "granularity_min")

mk <- function(nm) {
  d <- data.frame(area = "SE", forecast_origin = "2026-01-01",
                  target_datetime = as.POSIXct("2026-01-02 00:30:00", tz = "UTC"),
                  lead = 1L, y_pred = 1, y_true = 1, d_plus = 1L, granularity_min = 30L,
                  stringsAsFactors = FALSE)
  d[nm]
}
fail_id <- function(dt) tryCatch({ chk(dt); NULL }, prevcarga_error = identity)

test_that("nomes, ordem e tipos logicos da constante (ADR-0002/0032)", {
  s <- env$prevcarga_schema_long
  expect_identical(names(s), canon)
  expect_identical(unname(s), c("character", "character", "POSIXct", "integer", "double", "double", "integer", "integer"))
  expect_identical(s[["forecast_origin"]], "character")
  expect_false(identical(s[["forecast_origin"]], "date"))
})

test_that("sem nome PREVCARGA_MAIUSCULO no arquivo (grep do M1.V)", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "schema_long.R"), warn = FALSE)
  expect_false(any(grepl("PREVCARGA_[A-Z_]+", src)))
})

test_that("ordem canonica passa; lead/y_pred trocados e y_true ausente falham [CONTROL]", {
  ok <- mk(canon)
  ok0 <- ok
  expect_identical(withVisible(chk(ok))$visible, FALSE)
  expect_true(chk(ok))
  expect_identical(ok, ok0)

  sw <- mk(c("area", "forecast_origin", "target_datetime", "y_pred", "lead", "y_true", "d_plus", "granularity_min"))
  sw0 <- sw
  e <- fail_id(sw)
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$id, "contract.long_schema_constant")
  expect_identical(e$class_name, "contract")
  expect_identical(e$exit_code, 2L)
  expect_identical(e$where, "lead")
  expect_identical(sw, sw0)

  miss <- mk(setdiff(canon, "y_true"))
  miss0 <- miss
  e2 <- fail_id(miss)
  expect_identical(e2$id, "contract.long_schema_constant")
  expect_identical(e2$where, "y_true")
  expect_identical(miss, miss0)
})

test_that("coluna extra no fim tambem falha nomeando a extra", {
  ex <- mk(canon); ex$extra <- 1
  e <- fail_id(ex)
  expect_identical(e$id, "contract.long_schema_constant")
  expect_identical(e$where, "extra")
})
