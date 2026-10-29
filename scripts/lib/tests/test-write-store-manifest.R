## test-write-store-manifest.R -- config/deck-manifest.store.json + scripts/tools/write_store_manifest.R (M2.6)
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "deck_reader"))
tpl  <- file.path(pc_root(), "config", "deck-manifest.store.json")
tool <- file.path(pc_root(), "scripts", "tools", "write_store_manifest.R")
raw_of <- function(p) readBin(p, "raw", file.size(p))
run_tool <- function(target, fmt = NULL) {
  tf <- tempfile("wsm-out")
  on.exit(unlink(tf), add = TRUE)
  st <- suppressWarnings(system2(pc_rscript_bin(), c("--vanilla", shQuote(tool), "--target", shQuote(target), fmt),
                                 stdout = tf, stderr = FALSE))
  list(status = as.integer(st), out = readLines(tf, warn = FALSE))
}

test_that("template valida pelo leitor com os valores do store; UTF-8 sem BOM, LF, sem corte", {
  d <- withr::local_tempdir()
  file.copy(tpl, file.path(d, "manifest.json"))
  m <- env$prevcarga_deck_manifest(d)
  expect_identical(m$load_label, "start")
  tz <- m$timezone
  expect_identical(tz$load, "Etc/GMT+3"); expect_identical(tz$weather_observed, "Etc/GMT+3")
  expect_identical(tz$holidays, "Etc/GMT+3")
  b <- raw_of(tpl)
  expect_false(identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf))))
  expect_false(as.raw(13L) %in% b)
  expect_true(all(b < as.raw(128L)))
  j <- jsonlite::read_json(tpl)
  expect_identical(j$schema_version, 1L)
  expect_identical(j$timezone$weather_forecast, list(valid_time = "Etc/GMT+3", issue_time = "UTC"))
  expect_false("corte" %in% names(j))
})

test_that("utilitario: alvo vazio grava, identico e no-op, diferente recusa exit 2 sem tocar", {
  d1 <- withr::local_tempdir()
  r1 <- run_tool(d1)
  expect_identical(r1$status, 0L)
  expect_identical(raw_of(file.path(d1, "manifest.json")), raw_of(tpl))
  expect_false(file.exists(file.path(d1, "manifest.json.tmp")))

  d2 <- withr::local_tempdir()
  file.copy(tpl, file.path(d2, "manifest.json"))
  Sys.setFileTime(file.path(d2, "manifest.json"), as.POSIXct("2020-01-01", tz = "UTC"))
  mt <- file.mtime(file.path(d2, "manifest.json"))
  r2 <- run_tool(d2)
  expect_identical(r2$status, 0L)
  expect_identical(file.mtime(file.path(d2, "manifest.json")), mt)
  expect_identical(raw_of(file.path(d2, "manifest.json")), raw_of(tpl))

  d3 <- withr::local_tempdir()
  writeLines('{"schema_version":1,"load_label":"end","timezone":{}}', file.path(d3, "manifest.json"))
  before <- tools::md5sum(file.path(d3, "manifest.json"))
  r3 <- run_tool(d3, c("--format", "json"))
  expect_identical(r3$status, 2L)
  expect_identical(jsonlite::fromJSON(paste(r3$out, collapse = ""))$error$id, "contract.manifest_exists_different")
  expect_identical(tools::md5sum(file.path(d3, "manifest.json")), before)
})

test_that("utilitario recusa template invalido sem gravar; sem symlink nem system()", {
  d <- withr::local_tempdir()
  bad <- tempfile(fileext = ".json"); writeLines('{"schema_version":1,"corte":"06:00"}', bad)
  st <- suppressWarnings(system2(pc_rscript_bin(), c("--vanilla", shQuote(tool), "--target", shQuote(d), "--template", shQuote(bad)),
                                 stdout = FALSE, stderr = FALSE))
  expect_identical(as.integer(st), 2L)
  expect_false(file.exists(file.path(d, "manifest.json")))
  code <- readLines(tool, warn = FALSE); code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl("file\\.symlink|Sys\\.readlink|system2?\\(", code)))
})

test_that("store_manifest.tsv carrega pelo loader sem duplicar id", {
  cat <- env$prevcarga_errors_load(refresh = TRUE)
  expect_identical(sum(cat$id == "contract.manifest_exists_different"), 1L)
  expect_false(anyDuplicated(cat$id) > 0L)
  expect_identical(cat$class[cat$id == "contract.manifest_exists_different"], "contract")
})

test_that("DECK-CANONICO documenta manifest.json e painel do leitor sem corte como chave valida", {
  doc <- readLines(file.path(pc_root(), "docs", "reference", "DECK-CANONICO.md"), warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("^## `manifest\\.json`", doc)))
  expect_true(any(grepl("^## Painel do leitor", doc)))
  expect_true(any(grepl("manifest > .*--load-label.* > .*config\\.yaml.* > .*end", doc)))
  expect_false(any(grepl("corte[^ ]* do manifest|ADR-0007.*manifest", doc)))
})
