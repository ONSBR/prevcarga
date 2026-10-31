## helper-rd.R -- fixtures dos testes do plugin dessem-regdin (RD.1). Auto-carregado por test_dir. Nao e codigo de
## produto. Sem variaveis de ambiente; datas por lubridate com tz explicito; sem RNG nas fixtures.
suppressWarnings(suppressMessages(library(testthat)))

rd_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("rd_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}
rd_models <- function() file.path(rd_root(), "models")

options(prevcarga.lib = file.path(rd_root(), "scripts", "lib"))
## fundacao carregada num env de teste, nunca no globalenv
rd_lib <- local({
  e <- new.env(parent = globalenv())
  for (m in c("errors", "topology", "deck_reader", "validation", "registry", "features"))
    sys.source(file.path(rd_root(), "scripts", "lib", paste0(m, ".R")), envir = e)
  e
})

rd_load <- function(model = "dessem-regdin", root = rd_models()) {
  rd_lib$prevcarga_plugin_unload_all()
  rd_lib$prevcarga_registry_reset()
  rd_lib$prevcarga_plugin_load(model, root)
}

## a CLI fornece prevcarga_abort/_condition/_warn no globalenv; aqui entram so durante `code`
rd_with_cli <- function(code) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_warn", "to_semihourly")
  nm <- nm[!vapply(nm, exists, NA, envir = globalenv(), inherits = FALSE)]
  for (n in nm) assign(n, rd_lib[[n]], envir = globalenv())
  on.exit(rm(list = nm, envir = globalenv()), add = TRUE)
  force(code)
}

expect_rd_error <- function(expr, id, fragment = NULL) {
  cond <- tryCatch({ rd_with_cli(force(expr)); NULL }, prevcarga_error = function(e) e)
  testthat::expect_false(is.null(cond), info = "esperava prevcarga_error")
  if (!is.null(cond)) {
    testthat::expect_identical(cond$id, id)
    if (!is.null(fragment)) testthat::expect_match(conditionMessage(cond), fragment, fixed = TRUE)
  }
  invisible(cond)
}

RD_TZ <- "Etc/GMT+3"

## Painel de exibicao (meia hora; rotulo ja convertido pelo leitor) deterministico. `days` dias terminando em O,
## dia O parcial ate `until` (POSIXct) quando dado. area unica.
rd_panel <- function(origin = "2024-12-23", days = 60L, until = NULL, area = "SE", tz = RD_TZ, fc_days = 10L,
                     fc_short = integer(), hol_type = 12L, hol_simplified = 3L) {
  O <- lubridate::ymd(origin)
  t0 <- lubridate::force_tz(lubridate::as_datetime(O - days), tz)
  s_hist <- t0 + lubridate::minutes(30L * (seq_len(as.integer(days) * 48L) - 1L))
  tO <- lubridate::force_tz(lubridate::as_datetime(O), tz)
  s_o <- if (is.null(until)) s_hist[0] else tO + lubridate::minutes(30L * (seq_len(48L) - 1L))
  if (!is.null(until)) s_o <- s_o[s_o + lubridate::minutes(30) <= until]
  starts <- c(s_hist, s_o)
  hr <- lubridate::hour(starts) + lubridate::minute(starts) / 60
  dd <- as.numeric(lubridate::as_date(starts) - (O - days))
  temp <- function(d, h) 24 + 5 * sin(2 * pi * d / 365.25) + 4 * sin(2 * pi * (h - 9) / 24) + 0.5 * sin(d * 1.7)
  load_mwh <- 9000 + 600 * sin(2 * pi * (hr - 6) / 24) + 40 * (temp(dd, hr) - 24) + 25 * sin(dd * 0.37 + hr)
  ld <- data.table::data.table(area_code = area, interval_start = starts,
                               interval_end = starts + lubridate::minutes(30), load_mwh = load_mwh)
  obs <- data.table::data.table(area_code = area, valid_time = starts, temperature = temp(dd, hr))
  fs <- tO + lubridate::minutes(30L * (seq_len(48L * fc_days) - 1L))
  fs <- fs[!(lubridate::as_date(fs) %in% (O + fc_short))]
  fd <- as.numeric(lubridate::as_date(fs) - (O - days)); fh <- lubridate::hour(fs) + lubridate::minute(fs) / 60
  iss <- lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(c(0L, 6L))
  fc <- data.table::rbindlist(lapply(seq_along(iss), function(k) data.table::data.table(
    area_code = area, issue_time = iss[[k]], valid_time = fs, temperature = temp(fd, fh) + 0.4 + 100 * (k - 1L))))
  hol <- data.table::data.table(area_code = area, date = c(lubridate::ymd("2024-12-24"), O - 5L),
                                holiday_type_id = c(hol_type, 0L), simplified_code = c(hol_simplified, 0L))
  au <- if (is.null(until)) lubridate::force_tz(lubridate::as_datetime(O), tz) else until
  list(load = ld, weather_observed = obs, weather_forecast = fc, holidays = hol,
       attrs = list(origin = O, available_until = au, load_label = "end", tz = tz,
                    weather_issue_cutoff = "available_until", weather_issue_limit = au))
}

rd_cuts <- function(origin = "2024-12-23", available_until = NULL, tz = RD_TZ) {
  O <- lubridate::ymd(origin)
  au <- if (is.null(available_until)) lubridate::force_tz(lubridate::as_datetime(O), tz) +
    lubridate::hours(9L) else available_until
  list(origin_day = O, train_origin = O, last_data_day = O - 1L, tz = tz, available_until = au,
       weather_issue_cutoff = au, weather_issue_limit = au)
}

## painel horario no frame de modelagem, como o despachante entrega ao plugin (to_hourly + to_model_frame)
rd_model_panel <- function(panel, cuts, offset = 60L) {
  h <- rd_lib$to_hourly(panel)
  rd_lib$.prevcarga_plugin_to_model(h, cuts, offset)
}

rd_spec <- function(exogena = "MED", train_days = 59L) {
  list(name = "dessem-regdin", split = "origin_scoped", granularity_min = 60L, horizon_days = 9L,
       history_days = list(train = train_days, infer = train_days), seed = 42L,
       modes = c("train", "infer", "forecast"), params = list(exogena = exogena))
}

## run.yaml de fixture em diretorio temporario: copia o plugin (R/ + run.yaml) e o motor, aplica `edit` ao yaml.
rd_fixture_root <- function(edit = identity) {
  base <- tempfile("rd1-models-"); dir.create(base)
  for (m in c("dessem-regdin", "_dessem-engine")) {
    dir.create(file.path(base, m))
    file.copy(file.path(rd_models(), m, "R"), file.path(base, m), recursive = TRUE)
  }
  file.copy(file.path(rd_models(), "_dessem-engine", "model_parameters.json"), file.path(base, "_dessem-engine"))
  file.copy(file.path(rd_models(), "dessem-regdin", "run.yaml"), file.path(base, "dessem-regdin", "run.yaml"))
  yf <- file.path(base, "dessem-regdin", "run.yaml")
  writeLines(edit(readLines(yf)), yf)
  base
}
