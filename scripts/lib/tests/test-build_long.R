## test-build_long.R -- build_long_output.R (M6.1): painel LONG 8-col por interval_end. Cada test_that nomeia o AC
## e o braco FORBIDDEN/CONTROL. Assercoes por error.id.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "features", "schema_long", "build_long_output"))
env$prevcarga_runtime_enforce()
build <- env$prevcarga_build_long
validate <- env$.long_validate
SCHEMA <- env$prevcarga_schema_long

O <- lubridate::ymd("2026-01-15")
t0 <- lubridate::force_tz(lubridate::as_datetime(O), M6_TZ)
at <- function(x) lubridate::ymd_hm(x, tz = M6_TZ)
ctx <- m6_fixture_ctx()
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
rf <- m6_member("random-forest", 9L, 60L)
lg <- m6_member("lgbm", 2L, 60L)
panel <- function(member = rf, au = at("2026-01-15 09:00"), delivery = NULL, truth = NULL, ctx_ = ctx)
  build(if (is.null(delivery)) m6_fixture_delivery(member, O, au) else delivery, member, "A", O, au, truth, ctx_)

test_that("ac-1 schema 8-col, forecast_origin texto, rotulo de fim, lead e tz", {
  p <- panel()
  expect_identical(names(p), names(SCHEMA))
  expect_identical(p$forecast_origin[[1L]], "2026-01-15")
  expect_type(p$forecast_origin, "character")
  expect_type(p$lead, "integer"); expect_type(p$d_plus, "integer"); expect_type(p$y_pred, "double")
  expect_type(p$y_true, "double"); expect_identical(unique(p$granularity_min), 30L)
  expect_identical(p$target_datetime[[1L]], at("2026-01-15 09:30"))      # FORBIDDEN: rotular por interval_start (09:00)
  expect_identical(attr(p$target_datetime, "tzone"), M6_TZ)
  day2 <- p[p$d_plus == 1L]
  expect_identical(nrow(day2), 48L)
  expect_identical(day2$target_datetime[[1L]], at("2026-01-16 00:30"))
  expect_identical(day2$target_datetime[[48L]], at("2026-01-17 00:00"))
  expect_identical(p$lead, p$d_plus * 48L + as.integer((lubridate::hour(p$target_datetime - lubridate::minutes(30)) * 60L +
    lubridate::minute(p$target_datetime - lubridate::minutes(30))) %/% 30L + 1L))
  expect_identical(p$lead[[1L]], 19L)
  expect_identical(p$y_pred[[1L]], 540)                                  # sentinela do interval_start 09:00
  expect_identical(env$prevcarga_schema_long_check(p), invisible(TRUE))
  ## CONTROL: tz da entrega diferente do ctx
  d <- m6_fixture_delivery(rf, O, at("2026-01-15 09:00"))
  d$interval_end <- lubridate::with_tz(d$interval_end, "UTC")
  e <- fail(build(d, rf, "A", O, at("2026-01-15 09:00"), NULL, ctx))
  expect_identical(e$id, "contract.long_tz")
})

test_that("ac-2 validador embutido: off-by-one, buraco, lead duplicado, grade 47; CONTROL integro passa", {
  p <- panel(au = t0)
  p$interval_start <- p$target_datetime - lubridate::minutes(30)         # painel interno do conversor
  expect_silent(validate(data.table::copy(p), O, M6_TZ))                  # CONTROL
  bad <- data.table::copy(p); bad$target_datetime <- bad$target_datetime + lubridate::minutes(30)
  expect_identical(fail(validate(bad, O, M6_TZ))$id, "contract.long_offbyone")
  hole <- data.table::copy(p)[p$d_plus != 3L]
  expect_identical(fail(validate(hole, O, M6_TZ))$id, "contract.long_grid")
  dup <- data.table::copy(p); dup$lead[2L] <- dup$lead[1L]
  expect_identical(fail(validate(dup, O, M6_TZ))$id, "contract.long_duplicate_lead")
  short <- data.table::copy(p)[!(p$d_plus == 2L & p$lead == 2L * 48L + 10L)]
  expect_identical(fail(validate(short, O, M6_TZ))$id, "contract.long_grid")
  ## via prevcarga_build_long: carimbo deslocado nunca devolve painel
  d <- m6_fixture_delivery(rf, O, t0, shift_min = 30L)
  e <- fail(build(d, rf, "A", O, t0, NULL, ctx))
  expect_s3_class(e, "prevcarga_error")
})

test_that("ac-3 inicio comum + fim por membro via delivery_grid; desalinhados abortam nomeando o membro", {
  au <- at("2026-01-15 09:00")
  late <- m6_fixture_delivery(rf, O, au, first = at("2026-01-16 00:30"))
  e <- fail(build(late, rf, "A", O, au, NULL, ctx))
  expect_identical(e$id, "contract.member_misaligned"); expect_identical(e$details$member, "random-forest")
  short <- m6_fixture_delivery(rf, O, au, last = at("2026-01-23 00:00"))
  e <- fail(build(short, rf, "A", O, au, NULL, ctx))
  expect_identical(e$id, "contract.member_misaligned"); expect_identical(e$details$member, "random-forest")
  long <- m6_fixture_delivery(lg, O, au, last = at("2026-01-17 00:30"))
  e <- fail(build(long, lg, "A", O, au, NULL, ctx))
  expect_identical(e$id, "contract.member_misaligned"); expect_identical(e$details$member, "lgbm")
  ## CONTROL: faixas de fim diferentes passam, cada uma com o seu ultimo rotulo
  pl <- panel(lg, au); pr <- panel(rf, au)
  expect_identical(pl$target_datetime[[nrow(pl)]], at("2026-01-17 00:00"))
  expect_identical(pr$target_datetime[[nrow(pr)]], at("2026-01-24 00:00"))
  expect_identical(pl$target_datetime[[1L]], pr$target_datetime[[1L]])    # inicio comum
  expect_identical(nrow(pl), 30L + 48L)                                     # O 09:30..O+1 00:00 (30) + O+1 (48)
})

test_that("ac-4 horario: available_until 09:30 descarta o intervalo O 09:30 (regra unica, sem nowcast)", {
  hr <- m6_member("hw", 9L, 60L)
  d <- m6_fixture_delivery(hr, O, at("2026-01-15 09:00"))                 # entrega bruta inclui O 09:30
  p <- build(d, hr, "A", O, at("2026-01-15 09:30"), NULL, ctx)          # a regra unica descarta O 09:30
  expect_identical(p$target_datetime[[1L]], at("2026-01-15 10:00"))
  p0 <- build(d, hr, "A", O, at("2026-01-15 09:00"), NULL, ctx)
  expect_identical(p0$target_datetime[[1L]], at("2026-01-15 09:30"))
})

test_that("ac-5 y_true por interval_end; FORBIDDEN y_pred invariante; truth ate available_until => NA", {
  au <- at("2026-01-15 09:00")
  tr <- m6_fixture_truth(O, 10L)
  p <- panel(truth = tr)
  expect_identical(p$y_true, m6_minutes(p$target_datetime - lubridate::minutes(30), O))
  expect_false(anyNA(p$y_true))
  ## CONTROL: truth rotulado por interval_start desloca 1 slot
  ps <- panel(truth = m6_fixture_truth(O, 10L, label = "start"))
  expect_false(identical(ps$y_true, p$y_true))
  ## FORBIDDEN: perturbar todo o truth nao altera y_pred
  pp <- panel(truth = m6_fixture_truth(O, 10L, perturb = 1e6))
  expect_identical(pp$y_pred, p$y_pred)
  expect_false(identical(pp$y_true, p$y_true))
  ## truth so ate available_until => y_true todo NA, sem stop
  old <- tr[tr$interval_end <= au]
  pn <- panel(truth = old)
  expect_true(all(is.na(pn$y_true))); expect_identical(pn$y_pred, p$y_pred)
  expect_true(all(is.na(panel(truth = NULL)$y_true)))
})

test_that("ac-6 invariancia ao fuso de sessao; tzone e o da carga", {
  skip_if_not_installed("withr")
  a <- withr::with_timezone("UTC", panel(truth = m6_fixture_truth(O, 10L)))
  b <- withr::with_timezone("America/Sao_Paulo", panel(truth = m6_fixture_truth(O, 10L)))
  expect_identical(a, b)
  expect_identical(attr(a$target_datetime, "tzone"), M6_TZ)
})

test_that("ac-7 grep do conversor unico: sem byrow nem literal das 8 colunas fora do build_long_output.R", {
  lib <- file.path(pc_root(), "scripts", "lib")
  pc_grep_code <- function(pattern, files) {
    files <- files[file.exists(files)]
    Filter(function(f) any(grepl(pattern, sub("#.*$", "", readLines(f, warn = FALSE)), perl = TRUE)), files)
  }
  others <- file.path(lib, c("forecast.R", "exports.R", "reporting.R"))
  lit <- "forecast_origin[^\\n]*target_datetime[^\\n]*lead[^\\n]*y_pred[^\\n]*y_true[^\\n]*d_plus[^\\n]*granularity_min"
  expect_length(pc_grep_code("byrow\\s*=\\s*TRUE", c(others, file.path(lib, "build_long_output.R"))), 0L)
  expect_length(pc_grep_code(lit, others), 0L)
  src <- file.path(lib, "build_long_output.R")
  expect_length(pc_grep_code("prevcarga_schema_long\\s*(<-|=)", src), 0L)
  expect_length(pc_grep_code("prevcarga_schema_long", src), 1L)
  ## CONTROL: a isca casa
  bait <- tempfile(fileext = ".R"); on.exit(unlink(bait))
  writeLines("m <- matrix(1, byrow = TRUE)", bait)
  expect_length(pc_grep_code("byrow\\s*=\\s*TRUE", bait), 1L)
})
