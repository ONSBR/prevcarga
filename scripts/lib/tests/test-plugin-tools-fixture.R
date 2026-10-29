## test-plugin-tools-fixture.R -- scripts/tools/fixture_store.R (P.0e): deck sintetico deterministico, modo sentinela e
## mutadores em copia. Tudo em tempdir; nunca toca data/. Reusa helper-plugins.R (pc_decode_sentinel) e o leitor/validador do M2.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "deck_reader", "dates", "validation"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-10")
DAYS <- seq(lubridate::ymd("2026-08-01"), O, by = "day")
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
mk <- function(seed = 11, label = "start", ..., days = DAYS, areas = c("A", "B"),
               dir = withr::local_tempdir(.local_envir = parent.frame())) {
  env$prevcarga_fixture_deck(dir, seed, areas, days, load_label = label, ...)
}

## raiz-copia (config + models + lib) com historicos pequenos e topologia de areas A/B (membros reais); o deck sintetico
## de 71 dias nao cobre os historicos reais dos modelos (anos).
fx_root <- local({
  d <- tempfile("fx-root-"); dir.create(d)   # removido com o tempdir da sessao
  file.copy(file.path(pc_root(), c("config", "models")), d, recursive = TRUE)
  for (m in list.files(file.path(d, "models"))) {
    f <- file.path(d, "models", m, "run.yaml")
    if (!file.exists(f)) next
    y <- yaml::read_yaml(f); y$history_days <- list(train = 20, infer = if (grepl("^dessem", m)) 20 else 5); yaml::write_yaml(y, f)
  }
  tp <- file.path(d, "topology.yaml")
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [A, B], loss: PX}",
               "order: [X]", grep("^members:|^  - ", readLines(file.path(pc_root(), "config", "topology.yaml")), value = TRUE)), tp)
  list(root = d, topology = tp)
})
fx_ctx <- function(dir) list(data_dir = dir, root = fx_root$root, topology_path = fx_root$topology,
                             weather_issue_cutoff = "available_until")
fx_validate <- function(dir) env$prevcarga_validate_deck(fx_ctx(dir), origin = O)
fx_errors <- function(v) v$findings[v$findings$level == "error", ]

test_that("[ac-0] mesma semente => sha256 igual; outra semente => diferente; layout Hive naive; .Random.seed e arquivos do chamador intactos", {
  set.seed(5); before <- .Random.seed
  d1 <- mk(11); d2 <- mk(11); d3 <- mk(12)
  expect_identical(.Random.seed, before)
  g1 <- env$prevcarga_fixture_digest(d1); g2 <- env$prevcarga_fixture_digest(d2); g3 <- env$prevcarga_fixture_digest(d3)
  expect_identical(g1$aggregate, g2$aggregate)
  expect_identical(g1$files, g2$files)
  expect_false(identical(g1$aggregate, g3$aggregate))
  expect_identical(names(g1$files), sort(names(g1$files), method = "radix"))
  expect_true(all(c("manifest.json", "load/area_code=A/year=2026/part-0.parquet",
                    "weather/observed/area_code=B/year=2026/part-0.parquet",
                    "weather/forecast/area_code=A/year=2026/part-0.parquet",
                    "auxiliary/holidays/area_code=A/year=2026/part-0.parquet") %in% names(g1$files)))
  pr <- env$prevcarga_deck_probe(d1)
  expect_true(all(vapply(pr, function(p) isTRUE(p$present) && isTRUE(p$hive_ok), NA)))
  expect_identical(unname(pr$load$types[c("timestamp", "load_mwh")]), c("timestamp[ns]", "double"))
  expect_identical(unname(pr$weather_forecast$types[c("issue_time", "valid_time", "temperature")]),
                   c("timestamp[ns]", "timestamp[ns]", "double"))
  expect_identical(unname(pr$holidays$types), c("date32[day]", "int64", "int64", "int64"))
  for (p in pr) expect_false(any(p$tz_in_parquet))
  ## seed 11 em outro processo de sessao => mesmos bytes (nada de relogio): o sha de d1 e estavel numa regravacao
  d4 <- mk(11)
  expect_identical(env$prevcarga_fixture_digest(d4)$aggregate, g1$aggregate)
})

test_that("[ac-1] manifest, 3 emissoes por dia, clima por area, feriados, painel do leitor e validate-deck ok", {
  until <- lubridate::ymd_hm("2026-10-10 09:30", tz = TZ)
  d <- mk(11, load_until = until)
  m <- env$prevcarga_deck_manifest(d)
  expect_identical(m$load_label, "start")
  expect_identical(m$timezone$load, TZ); expect_identical(m$timezone$weather_observed, TZ)
  expect_identical(m$timezone$weather_forecast, list(valid_time = TZ, issue_time = "UTC"))
  raw <- jsonlite::read_json(file.path(d, "manifest.json"))
  expect_identical(names(raw), sort(names(raw), method = "radix"))

  fc <- data.table::as.data.table(as.data.frame(arrow::read_parquet(
    file.path(d, "weather", "forecast", "area_code=A", "year=2026", "part-0.parquet"))))
  it <- lubridate::with_tz(fc$issue_time, "UTC")
  per_day <- unique(data.table::data.table(day = lubridate::as_date(it), h = lubridate::hour(it)))
  expect_identical(sort(unique(per_day$h)), c(0L, 6L, 18L))
  expect_true(all(per_day[, .N, by = day]$N == 3L))
  expect_identical(length(unique(it[lubridate::as_date(it) == lubridate::ymd("2026-10-05")])), 3L)
  vt <- lubridate::with_tz(fc$valid_time, "UTC")
  one <- fc[it == lubridate::ymd_h("2026-10-05 06", tz = "UTC")]
  expect_identical(lubridate::as_date(max(lubridate::with_tz(one$valid_time, "UTC"))), lubridate::ymd("2026-10-15"))
  expect_identical(format(max(lubridate::with_tz(one$valid_time, "UTC")), "%H:%M"), "23:30")

  ctx <- fx_ctx(d)
  p <- env$prevcarga_read_panel(ctx, O, c("A", "B"), horizon_days = 10, history_days = 5)
  expect_identical(p$attrs$available_until, until)
  expect_identical(max(p$load$interval_end), until)
  expect_identical(max(p$weather_observed$timestamp), until)
  ta <- p$weather_observed[area_code == "A"]$temperature; tb <- p$weather_observed[area_code == "B"]$temperature
  expect_false(isTRUE(all.equal(ta, tb)))
  la <- p$load[area_code == "A"]$load_mwh; lb <- p$load[area_code == "B"]$load_mwh
  expect_false(isTRUE(all.equal(la, lb)))
  expect_identical(sort(unique(p$weather_forecast$area_code)), c("A", "B"))

  h <- env$prevcarga_read_holiday_codes(ctx)
  expect_identical(names(h), c("area_code", "date", "holiday_type_id", "prevcarga_code", "simplified_code"))
  for (a in c("A", "B")) {
    ha <- h[area_code == a]
    expect_true(any(ha$prevcarga_code == 12L & ha$holiday_type_id == 12L))
    expect_true((O + 3L) %in% ha$date)
    expect_true(lubridate::ymd("2026-09-07") %in% ha$date)
  }

  v <- fx_validate(d)
  expect_true(v$ok); expect_identical(nrow(fx_errors(v)), 0L)
  expect_identical(v$available_until, until)
})

test_that("[ac-2] sentinela decodifica ao interval_start/valid_time; CONTROL: rotulo declarado errado desloca 30 min", {
  days <- seq(lubridate::ymd("2026-09-28"), O, by = "day")
  sent <- function(dir) {
    ctx <- list(data_dir = dir)
    ld <- env$prevcarga_read_load(ctx, "A")
    ob <- env$prevcarga_read_weather(ctx, "observed", "A")
    fc <- env$prevcarga_read_weather(ctx, "forecast", "A")
    list(load = as.numeric(pc_decode_sentinel(ld$load_mwh, TZ)) - as.numeric(ld$interval_start),
         obs = as.numeric(pc_decode_sentinel(ob$temperature, TZ)) - as.numeric(ob$timestamp),
         fc = as.numeric(pc_decode_sentinel(fc$temperature, TZ)) - as.numeric(fc$valid_time))
  }
  ok_start <- sent(mk(11, "start", sentinel = TRUE, days = days))
  ok_end <- sent(mk(11, "end", sentinel = TRUE, days = days))
  for (s in list(ok_start, ok_end)) for (x in s) { expect_gt(length(x), 0L); expect_true(all(x == 0)) }
  ## CONTROL: gravado com rotulo de fim, manifest declara start => todo valor decodifica 30 min (1800 s) antes do entregue
  bad <- sent(mk(11, "end", sentinel = TRUE, manifest_label = "start", days = days))
  expect_true(all(bad$load == -1800)); expect_false(all(bad$load == 0))
  expect_true(all(bad$obs == 0))                                   # o clima e instantaneo: o rotulo da carga nao o desloca
  ## e o inverso: gravado start, declarado end => +30 min
  inv <- sent(mk(11, "start", sentinel = TRUE, manifest_label = "end", days = days))
  expect_true(all(inv$load == 1800))
  ## sentinela puro: valores = minutos desde 2026-09-01 00:00
  ld <- env$prevcarga_read_load(list(data_dir = mk(11, "start", sentinel = TRUE, days = days)), "A")
  expect_identical(ld$load_mwh[[1L]], 27 * 1440)
})

test_that("[ac-3] mutadores devolvem COPIA; original com o mesmo sha256; so a regiao pedida muda; buraco reprova o validate-deck", {
  d <- mk(11, load_until = lubridate::ymd_hm("2026-10-10 09:00", tz = TZ))
  g0 <- env$prevcarga_fixture_digest(d)
  ld0 <- env$prevcarga_read_load(list(data_dir = d), c("A", "B"))
  O0 <- lubridate::ymd_hm("2026-10-10 00:00", tz = TZ)

  ## perturb da carga apos O 00:00
  c1 <- env$prevcarga_fixture_perturb(d, "load", after = O0)
  expect_false(identical(c1, d)); expect_true(dir.exists(c1)); withr::defer(unlink(c1, recursive = TRUE))
  expect_identical(env$prevcarga_fixture_digest(d)$aggregate, g0$aggregate)
  ld1 <- env$prevcarga_read_load(list(data_dir = c1), c("A", "B"))
  expect_identical(nrow(ld1), nrow(ld0))
  hit <- ld0$interval_start > O0
  expect_true(any(hit)); expect_true(any(!hit))
  expect_equal(ld1$load_mwh[hit], ld0$load_mwh[hit] * 1.3 + 1)
  expect_identical(ld1$load_mwh[!hit], ld0$load_mwh[!hit])

  ## perturb do clima previsto por emissao (so issue_time > limite)
  lim <- lubridate::ymd_hm("2026-10-09 12:00", tz = "UTC")
  c2 <- env$prevcarga_fixture_perturb(d, "weather_forecast", issue_after = lim, areas = "A")
  withr::defer(unlink(c2, recursive = TRUE))
  rd <- function(dir, a) data.table::as.data.table(as.data.frame(arrow::read_parquet(
    file.path(dir, "weather", "forecast", paste0("area_code=", a), "year=2026", "part-0.parquet"))))
  f0 <- rd(d, "A"); f2 <- rd(c2, "A")
  late <- lubridate::with_tz(f0$issue_time, "UTC") > lim
  expect_true(any(late)); expect_true(any(!late))
  expect_equal(f2$temperature[late], f0$temperature[late] * 1.3 + 1)
  expect_identical(f2$temperature[!late], f0$temperature[!late])
  expect_identical(unname(tools::md5sum(file.path(c2, "weather", "forecast", "area_code=B", "year=2026", "part-0.parquet"))),
                   unname(tools::md5sum(file.path(d, "weather", "forecast", "area_code=B", "year=2026", "part-0.parquet"))))

  ## drop_col: contrato violado na copia
  c3 <- env$prevcarga_fixture_drop_col(d, "load", "load_mwh"); withr::defer(unlink(c3, recursive = TRUE))
  expect_false("load_mwh" %in% env$prevcarga_deck_probe(c3)$load$columns)
  v3 <- fx_validate(c3)
  expect_false(v3$ok); expect_identical(v3$class, "contract")

  ## drop_slots: buraco no meio do dia, nunca preenchido com 0
  day <- O - 3L
  c4 <- env$prevcarga_fixture_drop_slots(d, "A", day, at = "12:00", n = 2); withr::defer(unlink(c4, recursive = TRUE))
  ld4 <- env$prevcarga_read_load(list(data_dir = c4), "A")
  gone <- ld0[area_code == "A" & !interval_start %in% ld4$interval_start]
  expect_identical(format(gone$interval_start, "%Y-%m-%d %H:%M"), paste(format(day), c("12:00", "12:30")))
  expect_false(any(ld4$load_mwh == 0))
  v4 <- fx_validate(c4)
  expect_false(v4$ok); expect_identical(v4$class, "completude")
  expect_true("day_incomplete" %in% fx_errors(v4)$what)
  expect_true(fx_validate(d)$ok)
  expect_identical(env$prevcarga_fixture_digest(d)$aggregate, g0$aggregate)

  ## set_holiday: troca os codigos de um feriado em copia
  c5 <- env$prevcarga_fixture_set_holiday(d, "A", O + 3L, c(holiday_type_id = 5L, prevcarga_code = 7L, simplified_code = 2L))
  withr::defer(unlink(c5, recursive = TRUE))
  h5 <- env$prevcarga_read_holiday_codes(list(data_dir = c5), "A")[date == O + 3L]
  expect_identical(c(as.integer(h5$holiday_type_id), as.integer(h5$prevcarga_code), as.integer(h5$simplified_code)), c(5L, 7L, 2L))
  expect_identical(env$prevcarga_fixture_digest(d)$aggregate, g0$aggregate)
})

test_that("[ac-3] CONTROL: regiao vazia => tools.fixture_empty_region; areas/days invalidos => tools.fixture_bad_args", {
  d <- mk(11, days = seq(lubridate::ymd("2026-10-01"), O, by = "day"))
  g0 <- env$prevcarga_fixture_digest(d)
  e <- fail(env$prevcarga_fixture_perturb(d, "load", after = lubridate::ymd_hm("2030-01-01 00:00", tz = TZ)))
  expect_identical(e$id, "tools.fixture_empty_region"); expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  e <- fail(env$prevcarga_fixture_perturb(d, "weather_forecast", issue_after = lubridate::ymd_hm("2030-01-01 00:00", tz = "UTC")))
  expect_identical(e$id, "tools.fixture_empty_region")
  e <- fail(env$prevcarga_fixture_drop_slots(d, "A", O - 40L, at = "12:00", n = 2))
  expect_identical(e$id, "tools.fixture_empty_region")
  expect_identical(env$prevcarga_fixture_digest(d)$aggregate, g0$aggregate)
  bad <- function(...) fail(env$prevcarga_fixture_deck(withr::local_tempdir(), 1, ...))
  e <- bad(areas = character(), days = DAYS); expect_identical(e$id, "tools.fixture_bad_args")
  e <- bad(areas = "A", days = "2026-10-01"); expect_identical(e$id, "tools.fixture_bad_args")
  e <- bad(areas = c("A", "A"), days = DAYS); expect_identical(e$id, "tools.fixture_bad_args")
  e <- bad(areas = "A", days = DAYS, load_label = "meio"); expect_identical(e$id, "tools.fixture_bad_args")
  e <- bad(areas = "A", days = DAYS, tz = "Mars/Base"); expect_identical(e$id, "tools.fixture_bad_args")
  expect_identical(e$exit_code, 2L)
})

test_that("a ferramenta nao le env nem relogio nem RNG global; so escreve no dir recebido", {
  src <- readLines(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl("Sys\\.getenv|Sys\\.time|Sys\\.Date|as\\.POSIXct|set\\.seed|runif|rnorm|sample\\(", code)))
  expect_false(any(grepl("[^\\x01-\\x7f]", src, perl = TRUE)))
})
