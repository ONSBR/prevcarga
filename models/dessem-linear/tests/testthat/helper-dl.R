## helper-dl.R -- fixtures dos testes do plugin dessem-linear (DL.5). Auto-carregado por test_dir. Nao e codigo de
## produto. Sem variaveis de ambiente; datas por lubridate com tz explicito; sem RNG nas fixtures.
suppressWarnings(suppressMessages(library(testthat)))

dl_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("dl_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}
dl_models <- function() file.path(dl_root(), "models")

options(prevcarga.lib = file.path(dl_root(), "scripts", "lib"))
## fundacao carregada num env de teste, nunca no globalenv
dl_lib <- local({
  e <- new.env(parent = globalenv())
  for (m in c("errors", "topology", "deck_reader", "validation", "registry", "features"))
    sys.source(file.path(dl_root(), "scripts", "lib", paste0(m, ".R")), envir = e)
  e
})

dl_load <- function(root = dl_models()) {
  dl_lib$prevcarga_plugin_unload_all()
  dl_lib$prevcarga_registry_reset()
  dl_lib$prevcarga_plugin_load("dessem-linear", root)
}

## a CLI fornece prevcarga_abort/_condition/_warn no globalenv; aqui entram so durante `code`
dl_with_cli <- function(code) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_warn")
  for (n in nm) assign(n, dl_lib[[n]], envir = globalenv())
  on.exit(rm(list = nm, envir = globalenv()), add = TRUE)
  force(code)
}

expect_dl_error <- function(expr, id, fragment = NULL) {
  cond <- tryCatch({ dl_with_cli(force(expr)); NULL }, prevcarga_error = function(e) e)
  testthat::expect_false(is.null(cond), info = "esperava prevcarga_error")
  if (!is.null(cond)) {
    testthat::expect_identical(cond$id, id)
    if (!is.null(fragment)) testthat::expect_match(conditionMessage(cond), fragment, fixed = TRUE)
  }
  invisible(cond)
}

DL_TZ <- "Etc/GMT+3"

## Painel de exibicao (meia hora, rotulo ja convertido pelo leitor: interval_start/interval_end) que o leitor entrega,
## determinístico. `days` dias terminando em O (incl.), dia O parcial ate `until` (POSIXct) quando dado.
## `drop`: interval_start (display) a remover da carga. area unica.
dl_panel <- function(origin = "2024-12-23", days = 420L, until = NULL, drop = NULL, area = "SE", tz = DL_TZ,
                     fc_days = 9L, fc_short = integer()) {
  O <- lubridate::ymd(origin)
  t0 <- lubridate::force_tz(lubridate::as_datetime(O - days), tz)
  n_hist <- as.integer(days) * 48L
  s_hist <- t0 + lubridate::minutes(30L * (seq_len(n_hist) - 1L))
  tO <- lubridate::force_tz(lubridate::as_datetime(O), tz)
  s_o <- if (is.null(until)) s_hist[0] else tO + lubridate::minutes(30L * (seq_len(48L) - 1L))
  if (!is.null(until)) s_o <- s_o[s_o + lubridate::minutes(30) <= until]
  starts <- c(s_hist, s_o)
  hr <- lubridate::hour(starts) + lubridate::minute(starts) / 60
  dd <- as.numeric(lubridate::as_date(starts) - (O - days))
  temp <- function(d, h) 24 + 5 * sin(2 * pi * d / 365.25) + 4 * sin(2 * pi * (h - 9) / 24) + 0.5 * sin(d * 1.7)
  wd <- lubridate::wday(starts)
  load_mwh <- 9000 + 600 * sin(2 * pi * (hr - 6) / 24) + 700 * sin(2 * pi * (hr - 14) / 12) +
    40 * (temp(dd, hr) - 24) - 900 * (wd %in% c(1, 7)) + 25 * sin(dd * 0.37 + hr)
  ld <- data.table::data.table(area_code = area, interval_start = starts,
                               interval_end = starts + lubridate::minutes(30), load_mwh = load_mwh)
  if (!is.null(drop)) ld <- ld[!(interval_start %in% drop)]
  obs <- data.table::data.table(area_code = area, valid_time = starts, temperature = temp(dd, hr))
  fs <- tO + lubridate::minutes(30L * (seq_len(48L * fc_days) - 1L))
  keep <- !(lubridate::as_date(fs) %in% (O + fc_short))
  fs <- fs[keep]
  fd <- as.numeric(lubridate::as_date(fs) - (O - days)); fh <- lubridate::hour(fs) + lubridate::minute(fs) / 60
  iss <- lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(c(0L, 6L))
  fc <- data.table::rbindlist(lapply(seq_along(iss), function(k) data.table::data.table(
    area_code = area, issue_time = iss[[k]], valid_time = fs, temperature = temp(fd, fh) + 0.4 + 100 * (k - 1L))))
  fer_days <- O - c(10L, 40L, 100L, 200L, 300L, 399L)
  hol <- data.table::data.table(area_code = area, date = c(fer_days, O + 1L),
                                holiday_type_id = 1L, prevcarga_code = c(10L, 3L, 4L, 12L, 6L, 9L, 5L),
                                simplified_code = 3L)
  au <- if (is.null(until)) lubridate::force_tz(lubridate::as_datetime(O), tz) else until
  list(load = ld, weather_observed = obs, weather_forecast = fc, holidays = hol,
       attrs = list(origin = O, available_until = au, load_label = "end", tz = tz,
                    weather_issue_cutoff = "available_until", weather_issue_limit = au))
}

dl_cuts <- function(origin = "2024-12-23", available_until = NULL, tz = DL_TZ) {
  O <- lubridate::ymd(origin)
  au <- if (is.null(available_until)) lubridate::force_tz(lubridate::as_datetime(O), tz) +
    lubridate::hours(9L) else available_until
  list(origin_day = O, train_origin = O, last_data_day = O - 1L, tz = tz, available_until = au,
       weather_issue_cutoff = au, weather_issue_limit = au)
}

## painel horario no frame de modelagem, como o despachante entrega ao plugin (to_hourly + to_model_frame)
dl_model_panel <- function(panel, cuts, offset = 60L) {
  h <- dl_lib$to_hourly(panel)
  dl_lib$.prevcarga_plugin_to_model(h, cuts, offset)
}

dl_spec <- function(exogena = "MED", train_days = 400L) {
  list(name = "dessem-linear", split = "origin_scoped", granularity_min = 60L, horizon_days = 9L,
       history_days = list(train = train_days, infer = train_days), seed = 42L,
       modes = c("train", "infer", "forecast"), params = list(exogena = exogena))
}
