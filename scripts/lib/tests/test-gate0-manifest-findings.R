## test-gate0-manifest-findings.R -- manifest invalido (sem timezone.load, sem weather_forecast_publication_lag_hours) e FINDING do gate-0,
## nao aborto antecipado do require_deck (regressao do E1-T1, restaurada no E1-T7). Pela fachada; fixture sintetica em tempdir; sem treino.
library(testthat)
skip_if_not_installed("docopt")
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
E <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli"), env = globalenv())

run <- function(argv) {
  err <- character(); code <- NULL
  out <- utils::capture.output(err <- utils::capture.output(
    code <- E$prevcarga_main(argv, root = pc_root(), runtime = function() NULL), type = "message"), type = "output")
  list(code = as.integer(code), json = jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE))
}
deck <- function(mutate, .frame = parent.frame()) {
  d <- pc_deck_clean("end", dir = withr::local_tempdir("g0-", .local_envir = .frame))
  if (!is.null(mutate)) pc_deck_manifest(d, mutate = mutate)
  d
}
CASOS <- list(
  tz_load = function(m) { m$timezone$load <- NULL; m },
  lag = function(m) { m$weather_forecast_publication_lag_hours <- NULL; m })

test_that("score: manifest sem timezone.load / sem lag => finding do gate-0 (details$gate = validate-deck), nao aborto do require_deck", {
  for (n in names(CASOS)) {
    d <- deck(CASOS[[n]])
    r <- run(c("score", "--long", file.path(d, "nao-existe.parquet"), paste0("--data-train=", d), "--origin", "2026-10-01", "--format=json"))
    expect_identical(r$code, 2L, info = n)                                   # classe contract do catalogo
    expect_identical(r$json$error$class, "contract", info = n)
    expect_identical(r$json$error$details$gate, "validate-deck", info = n)
  }
  ## CONTROL: --skip-validate pula o gate-0 (o erro seguinte e do --long inexistente, sem details$gate)
  d <- deck(CASOS$lag)
  ok <- run(c("score", "--long", file.path(d, "nao-existe.parquet"), paste0("--data-train=", d), "--origin", "2026-10-01", "--skip-validate", "--format=json"))
  expect_null(ok$json$error$details$gate)
})

test_that("validate-deck: os mesmos manifests viram findings no resultado", {
  for (n in names(CASOS)) {
    d <- deck(CASOS[[n]])
    r <- run(c("validate-deck", paste0("--data-train=", d), "--origin", "2026-10-01", "--format=json"))
    expect_false(isTRUE(r$json$ok), info = n)
    expect_true(is.list(r$json$result) || is.list(r$json$error), info = n)
  }
})

test_that("kind/origem do deck seguem abortando no validate_deck (nao viram finding)", {
  d <- deck(function(m) { m$kind <- "inference"; m$inference <- list(origin = "2026-10-01", available_until = "2026-10-01T08:00:00",
                                                                    train_deck_hash = strrep("a", 64L), windows = structure(list(), names = character())); m })
  r <- run(c("validate-deck", paste0("--data-train=", d), "--origin", "2026-10-01", "--format=json"))
  expect_identical(r$json$error$id, "contract.deck_kind_mismatch")
})
