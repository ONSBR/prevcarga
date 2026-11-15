## test-holiday-annex-reader.R -- leitor do anexo de feriados e holiday_window no painel (E3-T2; ADR-046).
## Deck sintetico pequeno (sentinela + feriados) em tempdir; deck inference com o anexo apontado pelo manifest.
library(testthat)
library(data.table)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "holiday_islands", "deck_reader", "validation"))

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2023-06-01")
O_09h <- lubridate::ymd_hm("2023-06-01 09:00", tz = TZ)
W <- env$prevcarga_dessem_window(O, "ano_civil")
## feriados: Corpus Christi 2018 e 2019 (ilhas antes da janela comum), Natal 2022 (dentro da janela comum) e um depois de O
HOL <- data.table(area_code = "A", date = lubridate::ymd(c("2018-05-31", "2019-06-20", "2022-12-25", "2023-06-08")),
                  holiday_type_id = bit64::as.integer64(1L), prevcarga_code = bit64::as.integer64(c(5L, 5L, 9L, 5L)),
                  simplified_code = bit64::as.integer64(1L))
ISLAND_DAYS <- c(seq(lubridate::ymd("2018-05-24"), lubridate::ymd("2018-05-31"), by = "day"),
                 seq(lubridate::ymd("2019-06-13"), lubridate::ymd("2019-06-20"), by = "day"))
DAYS <- c(ISLAND_DAYS, lubridate::ymd(c("2020-06-01", "2023-05-30", "2023-05-31", "2023-06-01")))
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

train_deck <- function(env_ = parent.frame())
  pc_deck_panel(areas = c("A", "B"), days = DAYS, until = O_09h, holidays = HOL,
                dir = withr::local_tempdir(.local_envir = env_))
annex_train <- function(d, window = W) env$prevcarga_read_holiday_annex(pc_ctx_panel(d), O, "A", window)
cal_days <- function(x, col) lubridate::as_date(x[[col]], tz = TZ)

## deck inference: anexo em <dir>/annex (subdeck com manifest, load, weather/observed e islands/part-0.parquet)
write_islands <- function(dir, isl) {
  rows <- rbindlist(lapply(seq_len(nrow(isl)), function(i)
    data.table(area_code = isl$area_code[i], island_id = isl$island_id[i], from = isl$from[i], to = isl$to[i],
               holiday_date = isl$holiday_dates[[i]])))
  dir.create(file.path(dir, "islands"), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(rows, file.path(dir, "islands", "part-0.parquet"))
}
inference_deck <- function(isl, annex_days, with_annex = TRUE, env_ = parent.frame()) {
  d <- pc_deck_sentinel("end", days = lubridate::ymd(c("2023-05-31", "2023-06-01")), areas = c("A", "B"),
                        dir = withr::local_tempdir(.local_envir = env_))
  if (with_annex) {
    ad <- file.path(d, "annex")
    pc_deck_sentinel("end", days = annex_days, areas = c("A", "B"), dir = ad)
    write_islands(ad, isl)
  }
  pc_deck_manifest(d, mutate = function(m) {
    m$kind <- "inference"
    m$inference <- list(origin = "2023-06-01", available_until = "2023-06-02T00:00:00",
                        train_deck_hash = strrep("a", 64), windows = setNames(list(), character()))
    if (with_annex) m$inference$holiday_annex <- "annex"
    m
  })
  d
}

test_that("[AC1] deck train: ilhas do T1, so dias de ilha, nenhum dia >= O e atributos dos leitores", {
  d <- train_deck()
  ax <- annex_train(d)
  ref <- env$prevcarga_holiday_islands(HOL[area_code == "A"], O, W$common_from, first_day = lubridate::ymd("2018-05-24"),
                                       holiday_from = W$holiday_from)
  expect_identical(ax$islands, ref)
  expect_equal(nrow(ax$islands), 2L)
  expect_identical(names(ax), c("islands", "load", "weather_observed"))
  expect_setequal(unique(cal_days(ax$load, "interval_start")), ISLAND_DAYS)
  expect_setequal(unique(cal_days(ax$weather_observed, "valid_time")), ISLAND_DAYS)
  expect_true(all(cal_days(ax$load, "interval_start") < O))
  expect_true(all(ax$islands$to < O))
  expect_true(all(ax$load$area_code == "A"))
  ## mesmos atributos dos leitores atuais (a carga de referencia, lida com a mesma origem)
  rl <- env$prevcarga_read_load(pc_ctx_panel(d), "A", from = min(ISLAND_DAYS), to = max(ISLAND_DAYS), origin = O)
  for (a in c("tz", "load_label", "load_label_source", "available_until"))
    expect_identical(attr(ax$load, a), attr(rl, a))
  ro <- env$prevcarga_read_weather(pc_ctx_panel(d), "observed", "A", min(ISLAND_DAYS), max(ISLAND_DAYS), origin = O)
  for (a in c("tz", "available_until"))
    expect_identical(attr(ax$weather_observed, a), attr(ro, a))
  expect_identical(names(ax$load), names(rl))
  expect_identical(names(ax$weather_observed), names(ro))
  ## so os dias de ilha: a janela comum e o dia 2020-06-01 (fora das ilhas) nao entram
  expect_false(any(cal_days(ax$load, "interval_start") %in% lubridate::ymd(c("2020-06-01", "2023-05-31"))))
})

test_that("[AC1] janela referencia, NULL e area sem feriado devolvem NULL ou tabelas vazias com schema", {
  d <- train_deck()
  expect_null(annex_train(d, env$prevcarga_dessem_window(O, "referencia")))
  expect_null(annex_train(d, NULL))
  z <- env$prevcarga_read_holiday_annex(pc_ctx_panel(d), O, "B", W)       # B sem feriado: zero ilhas
  expect_equal(nrow(z$islands), 0L)
  expect_equal(nrow(z$load), 0L)
  expect_equal(nrow(z$weather_observed), 0L)
  expect_identical(names(z$islands), c("area_code", "island_id", "from", "to", "holiday_dates"))
  expect_identical(names(z$load), c("area_code", "interval_start", "interval_end", "load_mwh"))
  expect_identical(attr(z$load, "tz"), TZ)
})

test_that("[AC2] deck inference: le o anexo do manifest, descarta a linha de 2023-06-01 e mantem o schema do train", {
  d <- train_deck()
  tr <- annex_train(d)
  inf <- inference_deck(tr$islands, c(ISLAND_DAYS, O))              # anexo com dia extra em O
  ctx <- pc_ctx_deck(inf)
  raw <- env$prevcarga_read_load(pc_ctx_deck(file.path(inf, "annex")), "A")
  expect_true(O %in% cal_days(raw, "interval_start"))               # a linha extra existe no anexo gravado
  ax <- env$prevcarga_read_holiday_annex(ctx, O, "A", W)
  expect_identical(ax$islands, tr$islands)
  expect_true(all(cal_days(ax$load, "interval_start") < O))
  expect_true(all(cal_days(ax$weather_observed, "valid_time") < O))
  expect_identical(names(ax), names(tr))
  expect_identical(names(ax$load), names(tr$load))
  expect_identical(names(ax$weather_observed), names(tr$weather_observed))
  for (n in names(tr$load)) expect_identical(ax$load[[n]], tr$load[[n]])
  for (n in names(tr$weather_observed)) expect_identical(ax$weather_observed[[n]], tr$weather_observed[[n]])
  expect_identical(attr(ax$load, "tz"), attr(tr$load, "tz"))
  expect_identical(attr(ax$load, "load_label"), attr(tr$load, "load_label"))
  expect_s3_class(attr(ax$load, "available_until"), "POSIXct")
})

test_that("[AC2] deck inference sem holiday_annex no manifest com janela ano_civil aborta nomeando holiday_annex", {
  inf <- inference_deck(NULL, NULL, with_annex = FALSE)
  e <- fail(env$prevcarga_read_holiday_annex(pc_ctx_deck(inf), O, "A", W))
  expect_identical(e$id, "contract.constraint_violated")
  expect_match(e$where, "holiday_annex", fixed = TRUE)
  ## janela referencia nao precisa do anexo
  expect_null(env$prevcarga_read_holiday_annex(pc_ctx_deck(inf), O, "A", env$prevcarga_dessem_window(O, "referencia")))
})

test_that("manifest rejeita holiday_annex absoluto ou com .. (contract.manifest_invalid)", {
  for (bad in c("/tmp/x", "../x", "a/../x")) {
    inf <- inference_deck(NULL, NULL, with_annex = FALSE)
    pc_deck_manifest(inf, mutate = function(m) {
      m$kind <- "inference"
      m$inference <- list(origin = "2023-06-01", available_until = "2023-06-02T00:00:00",
                          train_deck_hash = strrep("a", 64), windows = setNames(list(), character()), holiday_annex = bad)
      m
    })
    expect_identical(fail(env$prevcarga_deck_manifest(inf))$id, "contract.manifest_invalid")
  }
})

test_that("[AC3] painel sem holiday_window e com janela referencia e identical() a montagem de referencia", {
  d <- train_deck()
  ctx <- pc_ctx_panel(d)
  ld <- env$prevcarga_read_load(ctx, "A", from = O - 2L, to = O, origin = O)
  ob <- env$prevcarga_read_weather(ctx, "observed", "A", O - 2L, O, origin = O)
  fc <- env$prevcarga_read_weather(ctx, "forecast", "A", O, O + 1L, origin = O)
  ref <- list(load = ld, weather_observed = ob, weather_forecast = fc, holidays = env$prevcarga_read_holidays(ctx, "A"),
              attrs = c(list(origin = O), env$pc_panel_attrs(ld, ob, fc, origin = O)))
  p0 <- env$prevcarga_read_panel(ctx, O, "A", 1, 2)
  expect_identical(p0, ref)
  expect_null(p0$holiday_annex)
  expect_identical(env$prevcarga_read_panel(ctx, O, "A", 1, 2, holiday_window = env$prevcarga_dessem_window(O, "referencia")), ref)
  expect_identical(env$prevcarga_read_panel(ctx, O, "A", 1, 2, holiday_window = NULL), ref)
  ## com ano_civil o painel ganha holiday_annex depois de holidays e antes de attrs; o resto fica igual
  p1 <- env$prevcarga_read_panel(ctx, O, "A", 1, 2, holiday_window = W)
  expect_identical(names(p1), c("load", "weather_observed", "weather_forecast", "holidays", "holiday_annex", "attrs"))
  expect_identical(names(p1$holiday_annex), c("islands", "load", "weather_observed"))
  expect_identical(p1[c("load", "weather_observed", "weather_forecast", "holidays", "attrs")], ref)
  expect_identical(p1$holiday_annex, annex_train(d))
})
