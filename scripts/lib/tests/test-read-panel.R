## test-read-panel.R -- deck_reader.R (M2.7): feriados e painel do leitor. FORBIDDEN (delta=0) e CONTROL (delta!=0).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "deck_reader"))
for (n in c("prevcarga_read_load", "prevcarga_read_weather", "prevcarga_read_holidays", "prevcarga_read_panel"))
  assign(n, env[[n]])

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
O_09h <- lubridate::ymd_hm("2026-10-01 09:00", tz = TZ)
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
DAYS <- O + (-40:11)
panel_deck <- function(..., dir = withr::local_tempdir(.local_envir = parent.frame()))
  pc_deck_panel(days = DAYS, until = O_09h, ..., dir = dir)
panel <- function(d, areas = "A") prevcarga_read_panel(pc_ctx_panel(d), O, areas, horizon_days = 9, history_days = 30)

test_that("catalogo do painel carrega pelo loader sem id duplicado", {
  cat <- env$prevcarga_errors_load(file.path(pc_root(), "scripts", "lib", "errors"))
  expect_true("topology.area_undeclared" %in% cat$id)
  expect_false(anyDuplicated(cat$id) > 0L)
  expect_identical(cat$class[cat$id == "topology.area_undeclared"], "topology")
})

test_that("[AC1] painel de A nao ve o clima de B (FORBIDDEN) e ve o de A (CONTROL)", {
  d <- panel_deck()
  p0 <- panel(d)
  for (n in c("load", "weather_observed", "weather_forecast", "holidays")) {
    expect_gt(nrow(p0[[n]]), 0L)
    expect_true(all(p0[[n]]$area_code == "A"))
  }
  pc_deck_perturb(d, "weather_observed", quote(area_code == "B"))
  pc_deck_perturb(d, "weather_forecast", quote(area_code == "B"))
  expect_identical(panel(d), p0)
  pc_deck_perturb(d, "weather_observed", quote(area_code == "A"))
  p1 <- panel(d)
  expect_false(identical(p1$weather_observed, p0$weather_observed))
  expect_identical(p1$weather_forecast, p0$weather_forecast)
  pc_deck_perturb(d, "weather_forecast", quote(area_code == "A"))
  expect_false(identical(panel(d)$weather_forecast, p0$weather_forecast))
  ## series proprias por area (A ~ 16, B ~ 17 no sentinela): o painel de B difere do de A
  pb <- panel(d, "B")
  expect_false(isTRUE(all.equal(mean(pb$weather_observed$temperature), mean(p0$weather_observed$temperature))))
})

test_that("[AC2] feriados: O-10, O+3, O+9 sem O+12; date sem shift; calendario nao e cortado (CONTROL)", {
  d <- panel_deck()
  ctx <- pc_ctx_panel(d)
  h0 <- prevcarga_read_holidays(ctx, "A", O - 30, O + 9)
  expect_identical(names(h0), c("area_code", "date", "holiday_type_id", "prevcarga_code", "simplified_code"))
  expect_s3_class(h0$date, "Date")
  expect_identical(h0$date, O + c(-10, 3, 9))
  expect_true(all(h0$area_code == "A"))
  expect_identical(prevcarga_read_holidays(ctx, "A", O - 30, O + 12)$date, O + c(-10, 3, 9, 12))
  ## feriado futuro alem de available_until e entregue: o calendario nao e cortado
  expect_true(any(h0$date > as.Date(O_09h)))
  pc_deck_perturb(d, "holidays", quote(area_code == "A" & date == lubridate::ymd("2026-10-04")))
  h1 <- prevcarga_read_holidays(ctx, "A", O - 30, O + 9)
  expect_false(identical(h1, h0))
  expect_identical(h1$date, h0$date)
  expect_identical(as.numeric(h1$prevcarga_code[2L]), as.numeric(h0$prevcarga_code[2L]) + 1)
  pc_deck_perturb(d, "holidays", quote(area_code == "B"))
  expect_identical(prevcarga_read_holidays(ctx, "A", O - 30, O + 9), h1)
})

test_that("[AC6] simplified_code = -1 e entregue como esta", {
  d <- panel_deck()
  h <- prevcarga_read_holidays(pc_ctx_panel(d), "A", O - 30, O + 9)
  expect_equal(as.numeric(h$simplified_code[h$date == O - 10]), -1)
})

test_that("[AC3] attrs identicos aos dos leitores", {
  d <- panel_deck()
  ctx <- pc_ctx_panel(d)
  p <- panel(d)
  ld <- prevcarga_read_load(ctx, "A", O - 30, O, origin = O)
  fc <- prevcarga_read_weather(ctx, "forecast", "A", O, O + 9, origin = O)
  for (n in c("available_until", "available_until_by_area", "load_label", "load_label_source", "tz"))
    expect_identical(p$attrs[[n]], attr(ld, n), info = n)
  for (n in c("weather_issue_cutoff", "weather_issue_limit"))
    expect_identical(p$attrs[[n]], attr(fc, n), info = n)
  expect_identical(p$attrs$origin, O)
  expect_identical(p$attrs$available_until, O_09h)
  ## agregador aborta quando os leitores divergem
  obs <- prevcarga_read_weather(ctx, "observed", "A", O - 30, O, origin = O)
  data.table::setattr(obs, "available_until", O_09h + 1800)
  e <- fail(env$pc_panel_attrs(ld, obs, fc))
  expect_identical(e$id, "contract.constraint_violated")
})

test_that("[AC4] area fora da topologia aborta topology (exit 4) sem ler dataset", {
  d <- panel_deck()
  e <- fail(prevcarga_read_panel(pc_ctx_panel(d), O, c("A", "XX"), 9, 30))
  expect_identical(e$id, "topology.area_undeclared"); expect_identical(e$where, "XX")
  expect_identical(e$class_name, "topology")
  expect_identical(unname(env$prevcarga_exit_codes[[e$class_name]]), 4L)
  ## sem leitura: data_dir sem manifest nem datasets (qualquer leitura daria contract.manifest_missing)
  vazio <- withr::local_tempdir()
  topo <- file.path(vazio, "topology.yaml"); pc_deck_topology(topo, c("A", "B"))
  c0 <- pc_ctx_deck(vazio); c0$topology_path <- topo
  e2 <- fail(prevcarga_read_panel(c0, O, c("A", "XX"), 9, 30))
  expect_identical(e2$id, "topology.area_undeclared")
})

test_that("[AC5] janelas de carga/observado/previsto e filtro de particao year", {
  days <- O + (-399:11)
  d <- pc_deck_panel(days = days, until = O_09h)
  ## espiao do scan: projeta tambem a particao `year` e registra os anos realmente coletados
  seen <- integer()
  orig <- env$.prevcarga_scan
  env$.prevcarga_scan <- function(ds, cols, ...) {
    r <- orig(ds, c(cols, "year"), ...)
    seen <<- union(seen, unique(r$year))
    r[, year := NULL][]
  }
  withr::defer(env$.prevcarga_scan <- orig)
  p <- panel(d)
  expect_identical(sort(seen), 2026L)   # 2025 existe no deck (400 dias) e nao foi coletado
  at <- function(x) lubridate::ymd_hm(x, tz = TZ)
  expect_identical(min(p$load$interval_start), at("2026-09-01 00:00"))
  expect_identical(max(p$load$interval_end), O_09h)
  expect_identical(nrow(p$load), as.integer(30 * 48 + 18))
  expect_identical(min(p$weather_observed$valid_time), at("2026-09-01 00:00"))
  expect_lte(max(p$weather_observed$valid_time), O_09h)
  expect_length(unique(p$weather_forecast$issue_time), 1L)
  expect_lte(unique(p$weather_forecast$issue_time), O_09h)
  expect_lt(max(p$weather_forecast$valid_time), at("2026-10-10 00:00"))
  expect_true(all(p$weather_forecast$valid_time >= at("2026-10-01 00:00")))
  expect_identical(p$holidays$date, O + c(-10, 3, 9, 12))             # calendario inteiro da area (O+12 > O+horizon)
})

## ---- corte da manha (O 00:00) num store que ja tem o dia O: truncate_at unico para os tres leitores -----------------
O_00h <- lubridate::ymd_hm("2026-10-01 00:00", tz = TZ)
morning <- function(d, truncate_at = O_00h)
  prevcarga_read_panel(pc_ctx_panel(d), O, "A", horizon_days = 9, history_days = 30, truncate_at = truncate_at)

test_that("[corte da manha] truncate_at = O 00:00 num store com o dia O: available_until O 00:00, sem no_load", {
  d <- panel_deck()
  p <- morning(d)
  expect_identical(p$attrs$available_until, O_00h)
  expect_identical(max(p$load$interval_end), O_00h)
  expect_lte(max(p$weather_observed$valid_time), O_00h)
  expect_true(all(p$weather_forecast$issue_time <= p$attrs$weather_issue_limit))
  expect_identical(p$attrs$weather_issue_limit, O_00h)
  ## o leitor de clima sozinho tambem deriva o corte (antes: completude.no_load com a janela so do dia O)
  w <- prevcarga_read_weather(pc_ctx_panel(d), "observed", "A", O, O, origin = O, truncate_at = O_00h)
  expect_identical(attr(w, "available_until"), O_00h)
  ## CONTROL: sem truncate_at o mesmo store da 09:00
  expect_identical(panel(d)$attrs$available_until, O_09h)
})

test_that("[corte da manha] FORBIDDEN carga/clima do dia O nao chegam (delta 0); CONTROL a vespera chega", {
  d <- panel_deck()
  p0 <- morning(d)
  expect_gt(pc_deck_perturb(d, "load", quote(interval_end > O_00h & interval_end <= O_09h)), 0L)
  expect_gt(pc_deck_perturb(d, "weather_observed", quote(timestamp > O_00h)), 0L)
  expect_identical(morning(d), p0)
  pc_deck_perturb(d, "load", quote(interval_end == O_00h))                 # ultimo intervalo da vespera (23:30)
  expect_false(identical(morning(d)$load, p0$load))
})

test_that("[corte da manha] deck fisicamente truncado (sem linha no dia O) == truncate_at no store com o dia O", {
  full <- panel_deck()
  cut <- pc_deck_panel(days = DAYS, until = O_00h, dir = withr::local_tempdir())
  a <- morning(full)
  b <- prevcarga_read_panel(pc_ctx_panel(cut), O, "A", horizon_days = 9, history_days = 30)
  expect_identical(b$attrs$available_until, O_00h)
  expect_identical(a$load, b$load)
  expect_identical(a$weather_observed, b$weather_observed)
  expect_identical(a$attrs, b$attrs)
  ## CONTROL: a origem seguinte sem nenhuma carga continua no_load
  e <- fail(prevcarga_read_panel(pc_ctx_panel(cut), O + 2L, "A", horizon_days = 9, history_days = 1))
  expect_identical(e$id, "completude.no_load")
})

## ---- feriados: o painel traz o calendario INTEIRO da area (oraculo .read_holidays), nao a janela do painel ---------------
test_that("[feriados] RS 2023-09-20 antes do inicio do historico chega ao painel; CONTROL a janela antiga o perde", {
  O2 <- lubridate::ymd("2023-10-05")
  days <- O2 + (-3:0)
  hol <- data.table::data.table(area_code = "RS", date = lubridate::ymd(c("2023-09-08", "2023-09-20", "2023-10-12")),
                                holiday_type_id = bit64::as.integer64(1:3), prevcarga_code = bit64::as.integer64(1:3),
                                simplified_code = bit64::as.integer64(1:3))
  d <- pc_deck_panel(areas = "RS", days = days, until = lubridate::ymd_hm("2023-10-05 09:00", tz = TZ), holidays = hol,
                     dir = withr::local_tempdir())
  p <- prevcarga_read_panel(pc_ctx_panel(d), O2, "RS", horizon_days = 3, history_days = 3)
  expect_true(lubridate::ymd("2023-09-20") %in% p$holidays$date)       # 2023-09-20 < O2 - history_days (2023-10-02)
  expect_identical(p$holidays$date, hol$date)
  ## days_since_last_holiday na origem: 15 dias (2023-09-20), nunca 27 (2023-09-08)
  expect_identical(as.integer(O2 - max(p$holidays$date[p$holidays$date < O2])), 15L)
  ## CONTROL: a janela antiga [O - history_days, O + horizon] perde o feriado anterior ao inicio do historico
  old <- prevcarga_read_holidays(pc_ctx_panel(d), "RS", O2 - 3, O2 + 3)
  expect_false(lubridate::ymd("2023-09-20") %in% old$date)
  ## FORBIDDEN: feriado de outra area nao entra
  expect_true(all(p$holidays$area_code == "RS"))
})
