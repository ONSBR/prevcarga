## test-features.R -- features.R (P.0a): to_hourly, to_semihourly, to_model_frame/to_display_frame, delivery_grid,
## catalogo plugin.tsv e higiene. Cada test_that nomeia o AC e o braco FORBIDDEN/CONTROL. Assercoes por error.id.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "features"))
env$prevcarga_runtime_enforce()
for (n in c("to_hourly", "to_semihourly", "to_model_frame", "to_display_frame", "prevcarga_delivery_grid"))
  assign(n, env[[n]])

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
at <- function(x) lubridate::ymd_hm(x, tz = TZ)
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

test_that("ac-8 to_hourly carga: media exata de H:00/H:30, meia hora isolada descartada, sem NA", {
  p <- pc_sentinel_panel(origin = O, days = 2, areas = "A", drop = at("2026-10-01 09:30"))
  before <- data.table::copy(p$load)
  h <- to_hourly(p)$load
  expect_identical(p$load, before)                                   # entrada nao mutada
  expect_false(any(is.na(h)))
  expect_identical(lubridate::tz(h$interval_start), TZ)
  expect_identical(lubridate::tz(h$interval_end), TZ)
  expect_true(all(h$interval_end == h$interval_start + lubridate::minutes(60)))
  expect_true(all(lubridate::minute(h$interval_start) == 0L))
  ## sentinela: media de m(H:00) e m(H:30) = m(H:00) + 15
  expect_equal(h$load_mwh, pc_sentinel_minutes(h$interval_start, TZ) + 15)
  expect_false(at("2026-10-01 09:00") %in% h$interval_start)         # hora isolada some
  expect_true(at("2026-10-01 08:00") %in% h$interval_start)
  expect_identical(nrow(h), 47L)
  expect_identical(attr(to_hourly(p), "granularity_min"), 60L)
  expect_identical(to_hourly(p)$attrs, p$attrs)
})

test_that("ac-8 CONTROL: variante por interval_end diverge do sentinela esperado", {
  p <- pc_sentinel_panel(origin = O, days = 2, areas = "A")
  good <- to_hourly(p)$load
  bad <- .pc_to_hourly_by_end(p$load)
  expect_equal(good$load_mwh, pc_sentinel_minutes(good$interval_start, TZ) + 15)
  expect_false(isTRUE(all.equal(bad$load_mwh, pc_sentinel_minutes(bad$interval_start, TZ) + 15)))
})

test_that("ac-9 to_hourly exogenas: emissoes separadas, hora incompleta por emissao, holidays identical", {
  p <- pc_sentinel_panel(origin = O, days = 2, areas = c("A", "B"))
  hole <- p$weather_forecast[area_code == "A" & lubridate::hour(issue_time) == 6L &
                               valid_time == at("2026-10-01 12:30"), which = TRUE]
  expect_length(hole, 1L)
  p$weather_forecast <- p$weather_forecast[-hole]
  r <- to_hourly(p)
  o <- r$weather_observed
  expect_equal(o$temperature, pc_sentinel_minutes(o$valid_time, TZ) + 15)
  f <- r$weather_forecast
  k <- as.integer(lubridate::hour(f$issue_time) == 6L)
  expect_equal(f$temperature, pc_sentinel_minutes(f$valid_time, TZ) + 15 + 1e6 * k)  # sem misturar emissoes
  gone <- f[area_code == "A" & lubridate::hour(issue_time) == 6L & valid_time == at("2026-10-01 12:00")]
  expect_identical(nrow(gone), 0L)                                    # some so na emissao incompleta
  expect_identical(nrow(f[area_code == "A" & lubridate::hour(issue_time) == 0L & valid_time == at("2026-10-01 12:00")]), 1L)
  expect_identical(nrow(f[area_code == "B" & lubridate::hour(issue_time) == 6L & valid_time == at("2026-10-01 12:00")]), 1L)
  expect_identical(r$holidays, p$holidays)
})

test_that("ac-9 CONTROL: embaralhar as linhas nao muda a saida", {
  p <- pc_sentinel_panel(origin = O, days = 2, areas = c("A", "B"))
  q <- p
  withr::local_seed(1)
  for (n in c("load", "weather_observed", "weather_forecast")) q[[n]] <- q[[n]][sample(.N)]
  expect_identical(to_hourly(q)[c("load", "weather_observed", "weather_forecast")],
                   to_hourly(p)[c("load", "weather_observed", "weather_forecast")])
})

test_that("ac-8/ac-9 to_hourly: tz divergente => features.bad_time_col", {
  p <- pc_sentinel_panel(origin = O, days = 1, areas = "A")
  p$attrs$tz <- "UTC"
  e <- fail(to_hourly(p))
  expect_identical(e$id, "features.bad_time_col")
})

hourly_x <- function() data.table::data.table(interval_end = seq(at("2026-10-01 10:00"), at("2026-10-10 00:00"), by = "1 hour"))

test_that("ac-10 to_semihourly: faixa, passo 30 min, 48/dia, nada antes de available_until nem apos last_day", {
  x <- hourly_x(); x[, y_pred := seq_len(.N)]
  r <- to_semihourly(x, pc_rule_split, at("2026-10-01 09:00"), O + 8)
  expect_identical(names(r), c("interval_end", "y_pred"))
  expect_true(all(r$interval_end > at("2026-10-01 09:00")))
  expect_identical(r$interval_end[[1L]], at("2026-10-01 09:30"))
  expect_identical(r$interval_end[[nrow(r)]], at("2026-10-10 00:00"))
  expect_true(all(r$interval_end[-1L] == r$interval_end[-nrow(r)] + lubridate::minutes(30)))
  per_day <- table(lubridate::as_date(r$interval_end - lubridate::minutes(30)))
  expect_true(all(per_day[as.character(O + 1:8)] == 48L))
  r2 <- to_semihourly(x, pc_rule_split, at("2026-10-01 09:00"), O + 4)
  expect_identical(r2$interval_end[[nrow(r2)]], at("2026-10-06 00:00"))
})

test_that("ac-10 CONTROL: available_until 09:30 descarta o intervalo 09:30; regra invalida => features.semihourly_rule", {
  x <- hourly_x(); x[, y_pred := seq_len(.N)]
  r <- to_semihourly(x, pc_rule_split, at("2026-10-01 09:30"), O + 8)
  expect_false(at("2026-10-01 09:30") %in% r$interval_end)
  expect_identical(r$interval_end[[1L]], at("2026-10-01 10:00"))
  one <- function(x) x$y_pred                                          # 1 linha por hora
  for (rl in list(one, pc_rule_shifted, 5)) {
    e <- fail(to_semihourly(x, rl, at("2026-10-01 09:00"), O + 8))
    expect_identical(e$id, "features.semihourly_rule")
    expect_identical(e$where, "to_semihourly")
  }
})

mk_x <- function(n, tz = TZ) {
  d <- data.frame(DataHora = lubridate::ymd_hm("2026-10-01 00:00", tz = tz) + lubridate::minutes(seq.int(0L, by = 30L, length.out = n)),
                  v = seq_len(n))
  d
}

test_that("ac-11 to_model_frame/to_display_frame: round-trip identical, deslocamento exato, sem mutar o chamador", {
  x30 <- mk_x(96L)
  x60 <- data.table::as.data.table(mk_x(48L)); x60[, DataHora := DataHora[1L] + lubridate::minutes(60L * (seq_len(.N) - 1L))]
  for (x in list(x30, x60)) for (o in c(0L, 60L)) {
    snap <- data.table::copy(x)
    expect_identical(to_display_frame(to_model_frame(x, o), o), x)
    expect_identical(x, snap)
  }
  addr <- data.table::address(x60)
  m <- to_model_frame(x60, 60L)
  expect_identical(data.table::address(x60), addr)
  expect_true(all(m$DataHora == x60$DataHora - lubridate::minutes(60L)))
  expect_identical(lubridate::tz(m$DataHora), TZ)
  expect_identical(m$v, x60$v)
  d <- to_display_frame(x60, 60L)
  expect_true(all(d$DataHora == x60$DataHora + lubridate::minutes(60L)))
  expect_identical(to_model_frame(x30, 0L), x30)                       # identidade
  expect_identical(to_model_frame(x60, 0L), x60)
})

test_that("ac-11 FORBIDDEN: to_model_frame 2x e voltar 1x NAO e igual a x", {
  x30 <- mk_x(96L)
  twice <- to_model_frame(to_model_frame(x30, 60L), 60L)
  expect_false(identical(to_display_frame(twice, 60L), x30))
})

test_that("ac-11 CONTROL: offset invalido => features.bad_offset; time_col invalido => features.bad_time_col", {
  x <- mk_x(4L)
  for (o in list(-60, 1.5, "60", NA_real_, c(0, 60), NULL)) {
    expect_identical(fail(to_model_frame(x, o))$id, "features.bad_offset")
    expect_identical(fail(to_display_frame(x, o))$id, "features.bad_offset")
  }
  naive <- x; naive$DataHora <- lubridate::force_tz(naive$DataHora, "")
  dd <- x; dd$DataHora <- as.Date(dd$DataHora)
  for (bad in list(x, dd, naive)) {
    col <- if (identical(bad, x)) "Ausente" else "DataHora"
    e <- fail(to_model_frame(bad, 60L, col))
    expect_identical(e$id, "features.bad_time_col")
    expect_identical(e$details$column, col)
  }
})

mk_cuts <- function(au, tz = TZ) list(origin_day = O, available_until = au, tz = tz)

test_that("ac-12 delivery_grid: 09:00/09:30 x 9/2 dias, comprimentos 414/77, passo exato, tzone", {
  g <- prevcarga_delivery_grid(mk_cuts(at("2026-10-01 09:00")), 9L)
  expect_length(g, 414L)
  expect_identical(g[[1L]], at("2026-10-01 09:30"))
  expect_identical(g[[length(g)]], at("2026-10-10 00:00"))
  expect_true(all(g[-1L] == g[-length(g)] + lubridate::minutes(30)))
  expect_false(anyDuplicated(g) > 0L)
  expect_identical(attr(g, "tzone"), TZ)
  g2 <- prevcarga_delivery_grid(mk_cuts(at("2026-10-01 09:30")), 2L)
  expect_length(g2, 77L)
  expect_identical(g2[[1L]], at("2026-10-01 10:00"))
  expect_identical(g2[[length(g2)]], at("2026-10-03 00:00"))
  expect_length(prevcarga_delivery_grid(mk_cuts(at("2026-10-01 09:30")), 9L), 413L)
  expect_length(prevcarga_delivery_grid(mk_cuts(at("2026-10-01 09:00")), 2L), 78L)
  ## forma real do resolve_cuts: forecast_origin no lugar de origin_day
  cr <- list(forecast_origin = O, available_until = at("2026-10-01 09:00"), tz = TZ)
  expect_identical(prevcarga_delivery_grid(cr, 9L), prevcarga_delivery_grid(mk_cuts(at("2026-10-01 09:00")), 9L))
})

test_that("ac-12 CONTROL: argumentos invalidos => features.bad_delivery_args nomeando o campo", {
  au <- at("2026-10-01 09:00")
  chk <- function(cuts, dd, field) {
    e <- fail(prevcarga_delivery_grid(cuts, dd))
    expect_identical(e$id, "features.bad_delivery_args")
    expect_identical(e$details$field, field)
  }
  chk(mk_cuts(at("2026-10-01 09:10")), 9L, "available_until")
  chk(mk_cuts(au), 0L, "delivery_days")
  chk(mk_cuts(au), 1.5, "delivery_days")
  chk(mk_cuts(au), "9", "delivery_days")
  chk(mk_cuts(lubridate::with_tz(au, "UTC")), 9L, "tz")
})

feat_file <- file.path(pc_root(), "scripts", "lib", "features.R")
.hygiene_hits <- function(path) {
  l <- readLines(path, warn = FALSE, encoding = "UTF-8")
  code <- !grepl("^\\s*#", l)
  pats <- c("3600", "hours\\(1\\)", "as\\.POSIXct", "strptime", "as\\.Date\\(", "difftime", "Sys\\.time",
            "Sys\\.Date", "Sys\\.getenv", "switch\\(", "grepl\\([^)]*(dessem|lgbm|rf|hw)")
  hits <- character()
  for (p in pats) for (i in which(code & grepl(p, l, ignore.case = TRUE))) hits <- c(hits, sprintf("%s:%d", basename(path), i))
  hits
}

test_that("ac-13 higiene: features.R sem aritmetica/relogio/env/switch; padrao plantado e acusado com arquivo:linha", {
  expect_identical(.hygiene_hits(feat_file), character())
  d <- withr::local_tempdir()
  cp <- file.path(d, "features.R")
  l <- readLines(feat_file, warn = FALSE)
  writeLines(c(l, "x <- as.POSIXct('2026-01-01')"), cp)
  expect_identical(.hygiene_hits(cp), sprintf("features.R:%d", length(l) + 1L))
})

test_that("ac-13 catalogo: plugin.tsv carrega na uniao, todo id chamado em features.R existe; CONTROLs reprovam", {
  cat <- env$prevcarga_errors_load()
  l <- readLines(feat_file, warn = FALSE)
  ids <- unique(unlist(regmatches(l, gregexpr("features\\.[a-z_]+", l))))
  ids <- ids[ids != "features.R"]
  expect_true(length(ids) >= 4L)
  expect_identical(setdiff(ids, cat$id), character())
  need <- c("plugin.contract", "plugin.rng", "plugin.artifact_corrupt", "tools.gate_unowned", "tools.oracle_nondeterministic")
  expect_identical(setdiff(need, cat$id), character())
  expect_identical(cat$class[cat$id == "plugin.window"], "window")
  expect_identical(cat$class[cat$id == "tools.oracle_nondeterministic"], "repro")
  ## CONTROLs: copia do diretorio com id acentuado / classe fora da tabela
  src <- file.path(pc_root(), "scripts", "lib", "errors")
  for (mut in list(function(t) sub("features.bad_offset\tcontract\toffset_min", "features.bad_offset\tcontract\toffset_mín", t, fixed = FALSE),
                   function(t) sub("plugin.rng\trepro", "plugin.rng\tinexistente", t, fixed = TRUE))) {
    d <- withr::local_tempdir()
    file.copy(list.files(src, full.names = TRUE), d)
    f <- file.path(d, "plugin.tsv")
    writeLines(mut(readLines(f, warn = FALSE, encoding = "UTF-8")), f, useBytes = FALSE)
    expect_identical(fail(env$prevcarga_errors_load(dir = d))$id, "contract.error_catalog_schema")
  }
  ## id removido do plugin.tsv mas ainda chamado: reprova a verificacao de sincronia
  d <- withr::local_tempdir()
  file.copy(list.files(src, full.names = TRUE), d)
  f <- file.path(d, "plugin.tsv")
  t <- readLines(f, warn = FALSE); writeLines(t[!startsWith(t, "features.bad_offset\t")], f)
  expect_true("features.bad_offset" %in% setdiff(ids, env$prevcarga_errors_load(dir = d)$id))
})
