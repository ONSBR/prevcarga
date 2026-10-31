## test-data-hash.R -- prevcarga_data_hash (M2.5; identidade do deck, model-agnostica). FORBIDDEN delta=0: ordem,
## copia em outro diretorio e modelo nao mudam o hash; CONTROL delta!=0: 1 byte, load_label e fuso do manifest mudam.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
hv <- pc_source_lib(c("errors", "provenance", "deck_reader"))

dh <- function(dir, label = "end", manifest = hv$prevcarga_deck_manifest(dir), ...)
  hv$prevcarga_data_hash(dir, manifest, label, ...)
copy_deck <- function(src, env = parent.frame()) {
  dst <- withr::local_tempdir(.local_envir = env)
  file.copy(list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)
  dst
}
hex64 <- "^[0-9a-f]{64}$"

test_that("data_hash: 64 hex e identico em 2x, lista embaralhada e copia em outro diretorio (FORBIDDEN)", {
  d <- pc_deck_panel(areas = c("A", "B"))
  h <- dh(d)
  expect_match(h, hex64)
  expect_identical(dh(d), h)
  fs <- unlist(lapply(c("load", "weather/observed", "weather/forecast", "auxiliary/holidays"), function(x)
    file.path(x, list.files(file.path(d, x), pattern = "[.]parquet$", recursive = TRUE))))
  expect_gt(length(fs), 4L)
  set.seed(1)
  for (i in 1:3) expect_identical(dh(d, files = sample(fs)), h)
  expect_identical(dh(d, files = rev(fs)), h)
  d2 <- copy_deck(d)
  expect_false(identical(d2, d))
  expect_identical(dh(d2), h)
})

test_that("data_hash: nao depende de modelo (model-agnostico; so a assinatura sem modelo)", {
  expect_false(any(c("model", "models", "scope") %in% names(formals(hv$prevcarga_data_hash))))
  d <- pc_deck_panel(areas = c("A", "B"))
  expect_identical(dh(d), dh(d))                      # --model lgbm x random-forest nao entra no calculo
})

test_that("data_hash CONTROL: 1 byte, load_label e fuso do manifest mudam o hash", {
  d <- pc_deck_panel(areas = c("A", "B"))
  h <- dh(d)
  d1 <- copy_deck(d)
  f <- list.files(file.path(d1, "load"), pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)[[1L]]
  b <- readBin(f, "raw", file.size(f)); b[length(b) %/% 2L] <- as.raw(bitwXor(as.integer(b[length(b) %/% 2L]), 1L))
  writeBin(b, f)
  h1 <- dh(d1)
  expect_match(h1, hex64); expect_false(identical(h1, h))
  expect_false(identical(dh(d, label = "start"), h))
  m <- hv$prevcarga_deck_manifest(d)
  m$timezone$weather_forecast$issue_time <- "America/Sao_Paulo"
  hm <- dh(d, manifest = m)
  expect_match(hm, hex64); expect_false(identical(hm, h))
  expect_false(identical(hm, dh(d, label = "start")))
  ml <- hv$prevcarga_deck_manifest(d)
  ml$weather_forecast_publication_lag_hours <- 6L                                    # lag de publicacao (ADR-0024)
  expect_false(identical(dh(d, manifest = ml), h))
})

test_that("data_hash: corte/arquivo nao parquet nao muda; data_dir sem parquet aborta contract", {
  d <- pc_deck_panel(areas = c("A", "B"))
  h <- dh(d)
  d2 <- copy_deck(d)
  writeLines("x", file.path(d2, "load", "lixo.txt")); writeLines("{}", file.path(d2, "extra.json"))
  expect_identical(dh(d2), h)
  e <- withr::local_tempdir()
  err <- tryCatch(hv$prevcarga_data_hash(e, list(timezone = list(load = "UTC")), "end"), prevcarga_error = identity)
  expect_s3_class(err, "prevcarga_error")
  expect_identical(err$id, "contract.dataset_missing"); expect_identical(err$exit_code, 2L)
})
