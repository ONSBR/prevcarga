## test-deck-reader-cut.R -- deck_reader.R (M2.2): corte intradiario derivado do dado, filtro de saida e clima
## pela regra weather_issue_cutoff. FORBIDDEN (delta=0) e CONTROL (delta!=0). Assercoes por error.id/classe.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "deck_reader"))
for (n in c("prevcarga_available_until", "prevcarga_read_load", "prevcarga_read_weather", "prevcarga_last_load_day",
            "prevcarga_weather_issue_limit", "prevcarga_freshest_issue"))
  assign(n, env[[n]])

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
at <- function(x) lubridate::ymd_hm(x, tz = TZ)
O_08h <- at("2026-10-01 08:00"); O_09h <- at("2026-10-01 09:00"); O_0930 <- at("2026-10-01 09:30")
O_12h <- at("2026-10-01 12:00")
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
cut_ctx <- function(d, rule = "available_until", src = "flag") {
  c0 <- pc_ctx_deck(d)
  c0$weather_issue_cutoff <- rule
  c0$sources <- list(weather_issue_cutoff = src)
  c0
}
## carga do dia O ate `until` e dias seguintes presentes (deck historico): remove o resto do dia O
hist_deck <- function(until = O_09h, days = O + (-2:9), ..., dir = withr::local_tempdir(.local_envir = parent.frame())) {
  d <- pc_deck_sentinel("end", days = days, ..., dir = dir)
  rest <- O_09h + lubridate::minutes(30L * (0:((as.integer(difftime(at("2026-10-02 00:00"), until, units = "mins")) %/% 30L) - 1L)))
  pc_deck_drop(d, c("A", "B"), rest)
  d
}

test_that("catalogo do corte carrega pelo loader sem id duplicado com o deck_reader.tsv", {
  cat <- env$prevcarga_errors_load(file.path(pc_root(), "scripts", "lib", "errors"))
  ids <- c("completude.origin_day_gap", "completude.no_load", "warning.available_until_spread")
  expect_true(all(ids %in% cat$id))
  expect_false(anyDuplicated(cat$id) > 0L)
  expect_true(all(cat$class[cat$id %in% ids] == "completude"))
})

test_that("sem relogio e sem parametro de corte: 09:00 -> 09:30 so pela carga", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "deck_reader.R"), warn = FALSE)
  code <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("Sys\\.time|Sys\\.Date|now\\(\\)|Sys\\.getenv", code)))
  expect_identical(names(formals(prevcarga_available_until)), c("load", "origin", "areas"))
  d <- pc_deck_sentinel("end", days = O + (-1:0), until = O_09h)
  expect_null(env$prevcarga_deck_manifest(d)$corte)
  a <- prevcarga_read_load(pc_ctx_deck(d), from = O - 1, to = O, origin = O)
  expect_identical(attr(a, "available_until"), O_09h)
  expect_identical(prevcarga_available_until(a, O)$available_until, O_09h)
  d2 <- pc_deck_sentinel("end", days = O + (-1:0), until = O_0930)
  b <- prevcarga_read_load(pc_ctx_deck(d2), from = O - 1, to = O, origin = O)
  expect_identical(attr(b, "available_until"), O_0930)
  expect_identical(attr(b, "origin"), O)
})

test_that("minimo entre areas + aviso spread; O sem intervalo; O completo", {
  d <- pc_deck_sentinel("end", days = O + (-1:0), until = O_0930)
  pc_deck_drop(d, "B", O_09h)                          # B termina em 09:00 (09:00-09:30 removido)
  w <- NULL
  x <- withCallingHandlers(prevcarga_read_load(pc_ctx_deck(d), from = O - 1, to = O, origin = O),
                           prevcarga_warning = function(c) { w <<- c; invokeRestart("muffleWarning") })
  expect_identical(w$id, "warning.available_until_spread")
  expect_identical(attr(x, "available_until"), O_09h)
  by <- attr(x, "available_until_by_area")
  expect_identical(by$area_code, c("A", "B"))
  expect_identical(by$until, c(O_0930, O_09h)); expect_true(all(is.na(by$gap)))
  expect_true(attr(x, "available_until_spread"))
  expect_identical(max(x[area_code == "A"]$interval_end), O_09h)   # corte global: A tambem e cortada
  d0 <- pc_deck_sentinel("end", days = O - 1)
  z <- prevcarga_read_load(pc_ctx_deck(d0), from = O - 1, to = O, origin = O)
  expect_identical(attr(z, "available_until"), at("2026-10-01 00:00"))
  expect_false(attr(z, "available_until_spread"))
  d48 <- pc_deck_sentinel("end", days = O + (-1:0))
  f <- prevcarga_read_load(pc_ctx_deck(d48), from = O - 1, to = O, origin = O)
  expect_identical(attr(f, "available_until"), at("2026-10-02 00:00"))
})

test_that("buraco no dia O aborta completude.origin_day_gap com where; carga vazia aborta no_load", {
  d <- pc_deck_sentinel("end", days = O + (-1:0), until = O_0930)
  pc_deck_drop(d, "A", at("2026-10-01 07:00"))
  e <- fail(prevcarga_read_load(pc_ctx_deck(d), from = O - 1, to = O, origin = O))
  expect_identical(e$id, "completude.origin_day_gap"); expect_identical(e$class_name, "completude")
  expect_identical(e$exit_code, 3L); expect_identical(e$where, "A:07:00")
  e2 <- fail(prevcarga_read_load(pc_ctx_deck(d), areas = "Z", from = O - 1, to = O, origin = O))
  expect_identical(e2$id, "completude.no_load"); expect_identical(e2$exit_code, 3L)
})

test_that("[FORBIDDEN] depois de available_until nada muda (carga, observado, previsto); [CONTROL] o ultimo intervalo muda", {
  d <- hist_deck(O_09h)
  ctx <- cut_ctx(d)
  rd <- function() list(
    load = prevcarga_read_load(ctx, from = O - 2, to = O + 9, origin = O),
    obs = prevcarga_read_weather(ctx, "observed", from = O - 2, to = O + 9, origin = O),
    fc = prevcarga_read_weather(ctx, "forecast", from = O - 2, to = O + 9, origin = O))
  base <- rd()
  expect_identical(attr(base$load, "available_until"), O_09h)
  expect_true(all(base$load$interval_end <= O_09h)); expect_true(all(base$obs$valid_time <= O_09h))
  expect_true(all(base$fc$issue_time <= O_09h))
  expect_gt(pc_deck_perturb(d, "load", quote(interval_end > O_09h)), 0L)
  expect_gt(pc_deck_perturb(d, "weather_observed", quote(timestamp > O_09h)), 0L)
  expect_gt(pc_deck_perturb(d, "weather_forecast", quote(issue_time > O_09h)), 0L)
  expect_identical(rd(), base)
  pc_deck_perturb(d, "load", quote(interval_end == O_09h))
  pc_deck_perturb(d, "weather_observed", quote(timestamp == O_09h))
  last_issue <- max(base$fc$issue_time)
  pc_deck_perturb(d, "weather_forecast", quote(issue_time == last_issue))
  now <- rd()
  expect_false(identical(now$load, base$load)); expect_false(identical(now$obs, base$obs))
  expect_false(identical(now$fc, base$fc))
})

test_that("emissoes 00Z/06Z/18Z nas duas regras; fonte flag e config; perturbar 06Z", {
  d <- hist_deck(O_09h, days = O + (-2:0), issues = c(0L, 6L, 18L))
  rd <- function(rule, src) prevcarga_read_weather(cut_ctx(d, rule, src), "forecast", from = O - 1, to = O + 1, origin = O)
  a <- rd("available_until", "flag")
  ## por (area, valid_time) a mais fresca <= limite: 06Z onde ela cobre; 18Z (15:00) nunca sai
  expect_identical(unique(a[valid_time > at("2026-10-01 03:00")]$issue_time), at("2026-10-01 03:00"))
  expect_true(all(a$issue_time <= at("2026-10-01 03:00")))
  expect_false(anyDuplicated(a[, .(area_code, valid_time)]) > 0L)
  expect_identical(attr(a, "weather_issue_cutoff"), "available_until")
  expect_identical(attr(a, "weather_issue_cutoff_source"), "flag")
  expect_identical(attr(a, "weather_issue_limit"), O_09h)
  expect_identical(attr(a, "freshest_issue_by_area")$issue_time, rep(at("2026-10-01 03:00"), 2L))
  m <- rd("origin_midnight_utc", "flag")
  expect_identical(unique(m[valid_time > at("2026-09-30 21:00")]$issue_time), at("2026-09-30 21:00"))   # a 00Z onde cobre
  expect_true(all(m$issue_time <= at("2026-09-30 21:00")))                 # nada alem da 00Z
  expect_identical(attr(m, "weather_issue_limit"), at("2026-09-30 21:00"))
  expect_identical(attr(m, "weather_issue_cutoff"), "origin_midnight_utc")
  cfg <- rd("origin_midnight_utc", "config")
  expect_identical(attr(cfg, "weather_issue_cutoff_source"), "config")
  expect_identical(cfg$issue_time, m$issue_time); expect_identical(cfg$temperature, m$temperature)
  pc_deck_perturb(d, "weather_forecast", quote(issue_time == at("2026-10-01 03:00")), env = environment())
  m2 <- rd("origin_midnight_utc", "flag"); a2 <- rd("available_until", "flag")
  expect_identical(m2, m)                       # FORBIDDEN: 06Z fora da regra origin_midnight_utc
  expect_false(identical(a2, a))                # CONTROL: dentro da regra available_until
  expect_identical(prevcarga_weather_issue_limit("origin_midnight_utc", O, O_09h, TZ), at("2026-09-30 21:00"))
  expect_identical(prevcarga_weather_issue_limit("available_until", O, O_09h, TZ, 0L), O_09h)
  expect_identical(fail(prevcarga_weather_issue_limit("outra", O, O_09h, TZ, 0L))$id, "config.bad_weather_cutoff")
  expect_identical(attr(a, "weather_publication_lag_hours"), 0L)
  expect_identical(attr(m, "weather_publication_lag_hours"), 0L)
})

## --- Lag de publicacao (ADR-0024): emissao so disponivel issue_time + lag depois -------------------------------------
## Deck como o store: UMA emissao por dia, 00Z; lag 6 h. Corte do leitor (truncate_at) na rodada das 00h e vizinhas.
lag_deck <- function(lag = 6L, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  d <- pc_deck_sentinel("end", days = O + (-2:1), issues = 0L, dir = dir)
  pc_deck_manifest(d, weather_lag_hours = lag)
  d
}
lag_read <- function(d, cut, rule = "available_until")
  prevcarga_read_weather(cut_ctx(d, rule), "forecast", from = O - 1, to = O + 1, origin = O, truncate_at = cut)
utc_day <- function(x) format(lubridate::with_tz(x, "UTC"), "%Y-%m-%d %H")

test_that("limite available_until = available_until - lag (no fuso da carga); lag invalido/ausente aborta", {
  O_00h <- at("2026-10-01 00:00")
  expect_identical(prevcarga_weather_issue_limit("available_until", O, O_00h, TZ, 6L), at("2026-09-30 18:00"))
  expect_identical(lubridate::tz(prevcarga_weather_issue_limit("available_until", O, O_00h, TZ, 6L)), TZ)
  expect_identical(prevcarga_weather_issue_limit("origin_midnight_utc", O, O_00h, TZ, 6L),
                   prevcarga_weather_issue_limit("origin_midnight_utc", O, O_00h, TZ))
  expect_identical(fail(prevcarga_weather_issue_limit("available_until", O, O_00h, TZ))$id, "contract.weather_lag_missing")
  expect_identical(fail(prevcarga_weather_issue_limit("available_until", O, O_00h, TZ, NULL))$id,
                   "contract.weather_lag_missing")
  for (bad in list(-1L, "6", 2.5, NA_integer_))
    expect_identical(fail(prevcarga_weather_issue_limit("available_until", O, O_00h, TZ, bad))$id,
                     "contract.manifest_invalid")
})

test_that("rodada das 00h (available_until = O 00:00 = O 03Z) com lag 6 usa a 00Z de O-1, nao a 00Z de O", {
  d <- lag_deck(6L)
  w <- lag_read(d, at("2026-10-01 00:00"))
  expect_identical(attr(w, "available_until"), at("2026-10-01 00:00"))
  expect_identical(attr(w, "weather_issue_limit"), at("2026-09-30 18:00"))
  expect_identical(attr(w, "weather_publication_lag_hours"), 6L)
  expect_identical(unique(utc_day(w$issue_time)), "2026-09-30 00")
  expect_identical(utc_day(attr(w, "freshest_issue_by_area")$issue_time), rep("2026-09-30 00", 2L))
  ## CONTROL: o mesmo deck com lag 0 pega a 00Z de O nas horas que ela cobre
  pc_deck_manifest(d, weather_lag_hours = 0L)
  w0 <- lag_read(d, at("2026-10-01 00:00"))
  expect_true("2026-10-01 00" %in% utc_day(w0$issue_time))
  expect_identical(attr(w0, "weather_publication_lag_hours"), 0L)
})

test_that("fronteira inclusiva: O 03:00 (06Z) pega a 00Z de O; O 02:30 ainda a 00Z de O-1", {
  d <- lag_deck(6L)
  w3 <- lag_read(d, at("2026-10-01 03:00"))
  expect_identical(attr(w3, "weather_issue_limit"), at("2026-09-30 21:00"))       # = O 00Z, exatamente o issue_time
  expect_identical(utc_day(attr(w3, "freshest_issue_by_area")$issue_time), rep("2026-10-01 00", 2L))
  expect_true(all(utc_day(w3[valid_time > at("2026-09-30 21:00")]$issue_time) == "2026-10-01 00"))
  w230 <- lag_read(d, at("2026-10-01 02:30"))
  expect_identical(unique(utc_day(w230$issue_time)), "2026-09-30 00")
})

test_that("origin_midnight_utc (paridade do oraculo) ignora o lag: lag 6 == lag 0, sempre a 00Z de O", {
  d <- lag_deck(6L)
  m6 <- lag_read(d, at("2026-10-01 00:00"), "origin_midnight_utc")
  expect_identical(attr(m6, "weather_issue_limit"), at("2026-09-30 21:00"))
  expect_identical(attr(m6, "weather_publication_lag_hours"), 0L)
  expect_identical(utc_day(attr(m6, "freshest_issue_by_area")$issue_time), rep("2026-10-01 00", 2L))
  pc_deck_manifest(d, weather_lag_hours = 0L)
  m0 <- lag_read(d, at("2026-10-01 00:00"), "origin_midnight_utc")
  expect_identical(m0, m6)
})

test_that("[FORBIDDEN] emissao dentro da janela de lag (issue <= au < issue + lag) nao muda a leitura; [CONTROL] a ultima permitida muda", {
  d <- lag_deck(6L)
  base <- lag_read(d, at("2026-10-01 00:00"))
  pc_deck_perturb(d, "weather_forecast", quote(issue_time == at("2026-09-30 21:00")), env = environment())   # O 00Z
  expect_identical(lag_read(d, at("2026-10-01 00:00")), base)
  pc_deck_perturb(d, "weather_forecast", quote(issue_time == at("2026-09-29 21:00")), env = environment())   # O-1 00Z
  expect_false(identical(lag_read(d, at("2026-10-01 00:00")), base))
})

test_that("last_load_day no deck e no truncado; so o dia do interval_start", {
  d <- pc_deck_sentinel("end", days = lubridate::ymd(c("2026-09-29", "2026-09-30", "2026-10-01")), until = O_0930)
  expect_identical(prevcarga_last_load_day(pc_ctx_deck(d)), lubridate::ymd("2026-10-01"))
  pc_deck_until(d, at("2026-09-30 23:30"))
  expect_identical(prevcarga_last_load_day(pc_ctx_deck(d)), lubridate::ymd("2026-09-30"))
  expect_identical(prevcarga_last_load_day(pc_ctx_deck(d), areas = "B"), lubridate::ymd("2026-09-30"))
  y <- pc_deck_sentinel("end", days = lubridate::ymd(c("2025-12-31", "2026-01-01")), until = at("2026-01-01 00:30"))
  expect_identical(prevcarga_last_load_day(pc_ctx_deck(y)), lubridate::ymd("2026-01-01"))
})

test_that("[FORBIDDEN] truncate_at == deck fisicamente truncado; [CONTROL] sem truncar o corte e 12:00", {
  d12 <- pc_deck_sentinel("end", days = O + (-1:0), until = O_12h)
  d08 <- pc_deck_sentinel("end", days = O + (-1:0), until = O_08h)
  a <- prevcarga_read_load(pc_ctx_deck(d12), from = O - 1, to = O, origin = O, truncate_at = O_08h)
  b <- prevcarga_read_load(pc_ctx_deck(d08), from = O - 1, to = O, origin = O)
  expect_identical(a, b)
  expect_identical(attr(a, "available_until"), O_08h)
  c0 <- prevcarga_read_load(pc_ctx_deck(d12), from = O - 1, to = O, origin = O)
  expect_identical(attr(c0, "available_until"), O_12h)
  expect_false(identical(c0, b))
  ## clima: o corte vem da carga truncada
  w1 <- prevcarga_read_weather(cut_ctx(d12), "observed", from = O - 1, to = O, origin = O, truncate_at = O_08h)
  w2 <- prevcarga_read_weather(cut_ctx(d08), "observed", from = O - 1, to = O, origin = O)
  expect_identical(w1, w2)
})

test_that("pc_deck_issues regrava so as emissoes pedidas", {
  d <- pc_deck_sentinel("end", days = O + (-1:0))
  pc_deck_issues(d, c(0L, 6L))
  fc <- prevcarga_read_weather(pc_ctx_deck(d), "forecast", from = O - 1, to = O)
  expect_identical(sort(unique(format(lubridate::with_tz(fc$issue_time, "UTC"), "%H"))), c("00", "06"))
})
