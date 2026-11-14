## test-deck-kind.R -- kind e bloco inference do manifest + prevcarga_require_deck (E1-T1). Fixture sintetica em tempdir.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "deck_reader"))
for (n in c("prevcarga_deck_manifest", "prevcarga_require_deck", "prevcarga_deck_manifest_timezones"))
  assign(n, env[[n]])

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
HASH <- strrep("a1", 32L)
inf_block <- function(...) {
  b <- list(origin = "2023-06-01", available_until = "2023-06-01T08:00:00", train_deck_hash = HASH,
            windows = list(load = list(from = "2023-01-01", to = "2023-06-01")))
  mods <- list(...)
  for (k in names(mods)) b[k] <- list(mods[[k]])
  b
}
mk <- function(mutate = NULL) {
  d <- tempfile("dk"); dir.create(d)
  pc_deck_manifest(d, mutate = mutate)
  d
}
mk_inf <- function(...) { b <- inf_block(...); mk(function(m) { m$kind <- "inference"; m$inference <- b; m }) }

test_that("[AC0] kind ausente => train, inference NULL; require_deck passa com papel train e NULL", {
  d <- mk()
  m <- prevcarga_deck_manifest(d)
  expect_identical(m$kind, "train")
  expect_null(m$inference)
  expect_null(fail(prevcarga_require_deck(list(data_dir = d, data_role = "train"))))
  r <- prevcarga_require_deck(list(data_dir = d))
  expect_identical(r$ctx$data_dir, d)
})

test_that("[AC1] deck inference: origem divergente aborta, coincidente devolve Date", {
  d <- mk_inf()
  e <- fail(prevcarga_require_deck(list(data_dir = d, data_role = "inference", origin = "2023-06-02")))
  expect_identical(e$id, "contract.deck_origin_mismatch")
  expect_identical(e$class, "contract")
  expect_identical(e$details$deck_origin, "2023-06-01")
  expect_identical(e$details$requested_origin, "2023-06-02")
  r <- prevcarga_require_deck(list(data_dir = d, data_role = "inference", origin = "2023-06-01"))
  expect_s3_class(r$manifest$inference$origin, "Date")
  expect_identical(format(r$manifest$inference$available_until), "2023-06-01 08:00:00")
  expect_identical(r$manifest$inference$windows$load$from, as.Date("2023-01-01"))
  ## sem --origin: sem conferencia de origem
  expect_null(fail(prevcarga_require_deck(list(data_dir = d, data_role = "inference"))))
})

test_that("[AC2] kind x papel nos dois sentidos", {
  e <- fail(prevcarga_require_deck(list(data_dir = mk(), data_role = "inference")))
  expect_identical(e$id, "contract.deck_kind_mismatch")
  expect_identical(e$where, "--data-inference")
  expect_identical(e$details$kind, "train")
  expect_identical(e$details$expected, "inference")
  e2 <- fail(prevcarga_require_deck(list(data_dir = mk_inf(), data_role = "train")))
  expect_identical(e2$id, "contract.deck_kind_mismatch")
  expect_identical(e2$where, "--data-train")
  expect_identical(e2$details$kind, "inference")
  expect_identical(e2$details$expected, "train")
  e3 <- fail(prevcarga_require_deck(list(data_dir = mk_inf(), data_role_option = "--data")))
  expect_identical(e3$where, "--data")
})

test_that("[AC3] manifests invalidos e chave estranha", {
  chk <- function(d, id, where) {
    e <- fail(prevcarga_deck_manifest(d))
    expect_identical(e$id, id); expect_identical(e$where, where)
  }
  chk(mk(function(m) { m$kind <- "treino"; m }), "contract.manifest_invalid", "manifest.json:kind")
  chk(mk(function(m) { m$kind <- 1; m }), "contract.manifest_invalid", "manifest.json:kind")
  chk(mk(function(m) { m$kind <- "inference"; m }), "contract.manifest_invalid", "manifest.json:inference")
  chk(mk(function(m) { m$inference <- inf_block(); m }), "contract.manifest_invalid", "manifest.json:inference")
  chk(mk_inf(train_deck_hash = "abc1234567"), "contract.manifest_invalid", "manifest.json:inference.train_deck_hash")
  chk(mk_inf(train_deck_hash = toupper(HASH)), "contract.manifest_invalid", "manifest.json:inference.train_deck_hash")
  chk(mk_inf(origin = "2023-06-01T08:00:00"), "contract.manifest_invalid", "manifest.json:inference.origin")
  chk(mk_inf(available_until = "2023-06-01T08:00"), "contract.manifest_invalid", "manifest.json:inference.available_until")
  chk(mk_inf(windows = list(load = list(from = "2023-06-02", to = "2023-06-01"))),
      "contract.manifest_invalid", "manifest.json:inference.windows.load")
  chk(mk_inf(foo = 1), "contract.manifest_unknown_key", "inference.foo")
  ## windows vazio e aceito
  m <- prevcarga_deck_manifest(mk_inf(windows = structure(list(), names = character())))
  expect_length(m$inference$windows, 0L)
})

test_that("[AC4] tzdata ausente => runtime.tzdata_missing; CONTROL => tz_invalid; timezones sem validar", {
  e <- tryCatch(env$.prevcarga_manifest_tz("Etc/GMT+3", "load.timestamp", olson = function() character(0)),
                prevcarga_error = identity)
  expect_identical(e$id, "runtime.tzdata_missing")
  expect_identical(e$class, "runtime")
  expect_identical(unname(env$prevcarga_exit_codes[["runtime"]]), 7L)
  e2 <- tryCatch(env$.prevcarga_manifest_tz("Marte/Base", "load.timestamp"), prevcarga_error = identity)
  expect_identical(e2$id, "contract.tz_invalid")
  d <- mk(function(m) { m$timezone$load <- "Marte/Base"; m })
  expect_setequal(prevcarga_deck_manifest_timezones(d), c("Marte/Base", "Etc/GMT+3", "UTC"))
})

test_that("catalogo deck_contract carrega e e ASCII", {
  p <- file.path(pc_root(), "scripts", "lib", "errors")
  cat <- env$prevcarga_errors_load(p)
  expect_true(all(c("contract.deck_kind_mismatch", "contract.deck_origin_mismatch", "contract.deck_role_refused",
                    "runtime.tzdata_missing") %in% cat$id))
  expect_false(any(grepl("[^\x01-\x7f]", readLines(file.path(p, "deck_contract.tsv"), warn = FALSE), useBytes = TRUE)))
})
