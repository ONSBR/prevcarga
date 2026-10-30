## fixture_store.R -- deck sintetico deterministico (P.0e). Ferramenta de DESENVOLVIMENTO, fora do bundle.
##
## E o deck dos testes da fundacao, dos 5 epicos de modelo e do M5: store Hive canonico (area_code=/year=) com
## `manifest.json` (esquema estrito do leitor M2.1), carga de 30 min por area, clima observado e previsto POR AREA
## (previsto com emissoes `issue_time` 00Z/06Z/18Z por dia, em UTC) e `auxiliary/holidays` (feriados fixos, um dia
## flag-12 e um feriado futuro em O+3, O = ultimo dia de `days`). Tudo por formula e semente: sem RNG global,
## sem relogio, sem variavel de ambiente; datas por lubridate com tz explicito. Escreve SO no `dir` recebido
## (tempdir nos testes; nunca em data/). Os mutadores trabalham em COPIA: o deck de entrada nunca e escrito.
##
## Uso (testes): env <- new.env(); sys.source("scripts/lib/errors.R", env); sys.source("scripts/tools/fixture_store.R", env)
##   (as funcoes chamam prevcarga_abort de errors.R no mesmo ambiente).
##
## Modo sentinela (`sentinel = TRUE`): cada valor codifica os MINUTOS desde 2026-09-01 00:00 (no tz do deck) do
## `interval_start` (carga) / `valid_time` (clima observado e previsto), decodificavel por pc_decode_sentinel
## (helper-plugins.R). `manifest_label` declara um rotulo diferente do realmente gravado (CONTROL: o rotulo
## declarado errado desloca todo intervalo em 30 min).
##
## Esquema (colunas de tempo `timestamp[ns]` ingenuas = digitos civis; area_code/year so no caminho Hive):
##   load/                 timestamp, load_mwh
##   weather/observed/     timestamp (valid_time), temperature
##   weather/forecast/     issue_time (UTC), valid_time, temperature
##   auxiliary/holidays/   date, holiday_type_id, prevcarga_code, simplified_code (int64)

.PC_FX_SENTINEL_BASE <- "2026-09-01 00:00"
.PC_FX_DATASETS <- c(load = "load", weather_observed = file.path("weather", "observed"),
                     weather_forecast = file.path("weather", "forecast"), holidays = file.path("auxiliary", "holidays"))
.PC_FX_TS_COLS <- list(load = "timestamp", weather_observed = "timestamp",
                       weather_forecast = c("issue_time", "valid_time"), holidays = character())
.PC_FX_YEAR_COL <- c(load = "timestamp", weather_observed = "timestamp", weather_forecast = "valid_time", holidays = "date")
.PC_FX_INT64 <- c("holiday_type_id", "prevcarga_code", "simplified_code")
.PC_FX_ISSUE_HOURS <- c(0L, 6L, 18L)

## --- formulas ------------------------------------------------------------------------------------------------
.pc_fx_hash <- function(area) sum(utf8ToInt(area))

.pc_fx_minutes <- function(t, tz) {
  lubridate::time_length(t - lubridate::ymd_hm(.PC_FX_SENTINEL_BASE, tz = tz), "minute")
}

## carga: base por area * (1 + 0.25 sin) * fator do dia da semana + ruido(seed, area, dia, slot)
.pc_fx_load <- function(area, days, seed, tz, sentinel = FALSE, load_label = "start") {
  grid <- data.table::CJ(day = days, slot = 0:47)
  grid[, interval_start := lubridate::force_tz(lubridate::as_datetime(day), tz) + lubridate::minutes(30L * slot)]
  h <- .pc_fx_hash(area)
  k <- as.integer(grid$day - lubridate::ymd("2026-09-01"))
  base <- 800 + (h %% 500)
  wk <- c(1.00, 1.03, 1.03, 1.03, 1.02, 0.92, 0.85)[lubridate::wday(grid$day, week_start = 1L)]
  val <- base * (1 + 0.25 * sin(2 * pi * (grid$slot - 30) / 48)) * wk + sin(seed * 1000 + k * 7.13 + grid$slot * 1.37 + h)
  if (sentinel) val <- .pc_fx_minutes(grid$interval_start, tz)
  stamp <- if (identical(load_label, "end")) grid$interval_start + lubridate::minutes(30L) else grid$interval_start
  data.table::data.table(area_code = area, interval_start = grid$interval_start, timestamp = stamp, load_mwh = as.double(val))
}

## temperatura observada: funcao pura de (area, instante local, seed); a previsao parte dela
.pc_fx_temp <- function(area, t, seed, tz) {
  m <- .pc_fx_minutes(t, tz); h <- .pc_fx_hash(area)
  hour <- (m %% 1440) / 60
  k <- m %/% 1440; slot <- (m %% 1440) %/% 30
  (15 + h %% 7) + 6 * sin(2 * pi * (hour - 9) / 24) + 0.3 * sin(seed * 1000 + k * 7.13 + slot * 1.37 + h)
}

.pc_fx_weather_observed <- function(area, days, seed, tz, sentinel = FALSE) {
  vt <- lubridate::force_tz(lubridate::as_datetime(rep(days, each = 48L)), tz) +
    lubridate::minutes(30L * rep(0:47, times = length(days)))
  tmp <- if (sentinel) .pc_fx_minutes(vt, tz) else .pc_fx_temp(area, vt, seed, tz)
  data.table::data.table(area_code = area, timestamp = vt, temperature = as.double(tmp))
}

## previsto: 3 emissoes por dia (UTC), valid_time de 30 min da emissao ate D+horizon_days 23:30 (D = dia UTC da emissao);
## temperatura = observado(valid_time) + 0.1 * lead_dias + 0.01 * hora_da_emissao.
.pc_fx_weather_forecast <- function(area, days, seed, tz, horizon_days = 10L, sentinel = FALSE) {
  em <- data.table::CJ(day = days, ih = .PC_FX_ISSUE_HOURS)
  em[, issue_time := lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::hours(ih)]
  last <- lubridate::force_tz(lubridate::as_datetime(em$day + horizon_days), tz) + lubridate::minutes(1410L)
  n <- as.integer(lubridate::time_length(last - em$issue_time, "minute") %/% 30L) + 1L
  fc <- em[rep(seq_len(nrow(em)), n)]
  fc[, valid_time := lubridate::with_tz(issue_time, tz) + lubridate::minutes(30L * sequence(n) - 30L)]
  lead <- lubridate::time_length(fc$valid_time - fc$issue_time, "day")
  tmp <- if (sentinel) .pc_fx_minutes(fc$valid_time, tz) else
    .pc_fx_temp(area, fc$valid_time, seed, tz) + 0.1 * lead + 0.01 * fc$ih
  data.table::data.table(area_code = area, issue_time = fc$issue_time, valid_time = fc$valid_time, temperature = as.double(tmp))
}

## feriados fixos nos anos cobertos, um dia flag-12 (O-5) e um feriado futuro em O+3; os tres codigos inteiros.
.pc_fx_holidays <- function(areas, days, horizon_days = 10L) {
  O <- max(days)
  yrs <- seq(lubridate::year(min(days)), lubridate::year(O + horizon_days))
  fixed <- lubridate::ymd(paste0(rep(yrs, each = 5L), c("-01-01", "-09-07", "-10-12", "-11-15", "-12-25")))
  fixed <- fixed[fixed >= min(days) & fixed <= O + horizon_days]
  d <- data.table::data.table(date = c(fixed, O - 5L, O + 3L), type = c(rep(1L, length(fixed)), 12L, 1L),
                              pc = c(rep(2L, length(fixed)), 12L, 3L), sc = 1L)
  d <- d[!duplicated(date)]
  data.table::setorder(d, date)
  data.table::rbindlist(lapply(areas, function(a) d[, list(area_code = a, date = date, holiday_type_id = type,
                                                            prevcarga_code = pc, simplified_code = sc)]))
}

## --- escrita Hive a mao (arrow::write_dataset exige dplyr, fora do ambiente) ------------------------------------
## digitos civis do instante `x` no fuso `tz` como POSIXct UTC (o arrow grava timestamp[ns] sem fuso)
.pc_fx_digits <- function(x, tz) lubridate::force_tz(lubridate::with_tz(x, tz), "UTC")

.pc_fx_array <- function(x, name, ts_cols) {
  if (name %in% ts_cols) return(arrow::Array$create(lubridate::force_tz(x, "UTC"), type = arrow::timestamp("ns")))
  if (name %in% .PC_FX_INT64) return(arrow::Array$create(bit64::as.integer64(x)))
  arrow::Array$create(x)
}

.pc_fx_write_file <- function(dt, file, ts_cols) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  cols <- lapply(names(dt), function(n) .pc_fx_array(dt[[n]], n, ts_cols))
  names(cols) <- names(dt)
  arrow::write_parquet(do.call(arrow::arrow_table, cols), file)
  invisible(file)
}

## `dt` com area_code e colunas de tempo ja em digitos UTC; particiona por area_code/year (year da coluna-chave).
.pc_fx_write_ds <- function(dt, dir, dataset) {
  rel <- .PC_FX_DATASETS[[dataset]]; ts <- .PC_FX_TS_COLS[[dataset]]
  yr <- as.integer(lubridate::year(dt[[.PC_FX_YEAR_COL[[dataset]]]]))
  unlink(file.path(dir, rel), recursive = TRUE)
  keys <- paste(dt$area_code, yr, sep = "\r")
  for (g in unique(keys)) {
    i <- which(keys == g)
    sub <- dt[i]
    part <- file.path(dir, rel, paste0("area_code=", sub$area_code[[1L]]), paste0("year=", yr[i[[1L]]]))
    .pc_fx_write_file(sub[, setdiff(names(sub), "area_code"), with = FALSE], file.path(part, "part-0.parquet"), ts)
  }
  invisible(NULL)
}

.pc_fx_manifest <- function(manifest_label, tz) {
  list(load_label = manifest_label, schema_version = 1L,
       timezone = list(load = tz, weather_forecast = list(issue_time = "UTC", valid_time = tz),
                       weather_observed = tz))
}

.pc_fx_check_args <- function(dir, seed, areas, days, load_label, sentinel, manifest_label, load_until, tz, horizon_days) {
  bad <- function(w) prevcarga_abort("tools.fixture_bad_args", where = w)
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) bad("dir")
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) bad("seed")
  if (!is.character(areas) || !length(areas) || anyNA(areas) || any(!nzchar(areas)) || anyDuplicated(areas)) bad("areas")
  if (!inherits(days, "Date") || !length(days) || anyNA(days) || anyDuplicated(days)) bad("days")
  if (!is.character(load_label) || length(load_label) != 1L || !load_label %in% c("start", "end")) bad("load_label")
  if (!is.character(manifest_label) || length(manifest_label) != 1L || !manifest_label %in% c("start", "end")) bad("manifest_label")
  if (!is.logical(sentinel) || length(sentinel) != 1L || is.na(sentinel)) bad("sentinel")
  if (!is.character(tz) || length(tz) != 1L || !tz %in% OlsonNames()) bad("tz")
  if (!is.null(load_until) && (!inherits(load_until, "POSIXct") || length(load_until) != 1L || is.na(load_until))) bad("load_until")
  if (!is.numeric(horizon_days) || length(horizon_days) != 1L || horizon_days < 0) bad("horizon_days")
  invisible(NULL)
}

## --- API publica -----------------------------------------------------------------------------------------------
## Gera o deck em `dir` e devolve `dir`. `load_until` (POSIXct): so ficam carga e clima observado com
## interval_end/valid_time <= load_until (o available_until que o leitor deriva fica igual a ele).
prevcarga_fixture_deck <- function(dir, seed, areas, days, load_label = "start", sentinel = FALSE,
                                   manifest_label = load_label, load_until = NULL, tz = "Etc/GMT+3",
                                   horizon_days = 10L) {
  .pc_fx_check_args(dir, seed, areas, days, load_label, sentinel, manifest_label, load_until, tz, horizon_days)
  days <- sort(days)
  areas <- sort(areas, method = "radix")
  ld <- data.table::rbindlist(lapply(areas, .pc_fx_load, days = days, seed = seed, tz = tz,
                                     sentinel = sentinel, load_label = load_label))
  ob <- data.table::rbindlist(lapply(areas, .pc_fx_weather_observed, days = days, seed = seed, tz = tz, sentinel = sentinel))
  fc <- data.table::rbindlist(lapply(areas, .pc_fx_weather_forecast, days = days, seed = seed, tz = tz,
                                     horizon_days = horizon_days, sentinel = sentinel))
  if (!is.null(load_until)) {
    ld <- ld[interval_start + lubridate::minutes(30L) <= load_until]
    ob <- ob[timestamp <= load_until]
    if (!nrow(ld) || !nrow(ob)) prevcarga_abort("tools.fixture_bad_args", where = "load_until")
  }
  ld[, interval_start := NULL]
  ld[, timestamp := .pc_fx_digits(timestamp, tz)]
  ob[, timestamp := .pc_fx_digits(timestamp, tz)]
  fc[, `:=`(issue_time = .pc_fx_digits(issue_time, "UTC"), valid_time = .pc_fx_digits(valid_time, tz))]
  data.table::setorder(ld, area_code, timestamp)
  data.table::setorder(ob, area_code, timestamp)
  data.table::setorder(fc, area_code, issue_time, valid_time)
  hol <- .pc_fx_holidays(areas, days, horizon_days)
  data.table::setorder(hol, area_code, date)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  .pc_fx_write_ds(ld, dir, "load")
  .pc_fx_write_ds(ob, dir, "weather_observed")
  .pc_fx_write_ds(fc, dir, "weather_forecast")
  .pc_fx_write_ds(hol, dir, "holidays")
  jsonlite::write_json(.pc_fx_manifest(manifest_label, tz), file.path(dir, "manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  invisible(dir)
}

## sha256 por arquivo (ordem radix, caminhos relativos) + agregado
prevcarga_fixture_digest <- function(dir) {
  fs <- list.files(dir, recursive = TRUE, all.files = TRUE)
  fs <- fs[order(fs, method = "radix")]
  h <- vapply(fs, function(f) digest::digest(file.path(dir, f), algo = "sha256", file = TRUE), "", USE.NAMES = FALSE)
  names(h) <- fs
  list(files = h, aggregate = digest::digest(paste0(fs, "\t", h, collapse = "\n"), algo = "sha256", serialize = FALSE))
}

## --- mutadores em copia ----------------------------------------------------------------------------------------
prevcarga_fixture_copy <- function(dir) {
  out <- tempfile("fixture-copy-")
  dir.create(out)
  src <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (!all(file.copy(src, out, recursive = TRUE))) prevcarga_abort("tools.fixture_bad_args", where = "dir")
  out
}

.pc_fx_manifest_read <- function(dir) jsonlite::read_json(file.path(dir, "manifest.json"), simplifyVector = TRUE)

## arquivos de um dataset com a area do caminho Hive
.pc_fx_files <- function(dir, dataset, areas = NULL) {
  fs <- list.files(file.path(dir, .PC_FX_DATASETS[[dataset]]), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  fs <- fs[order(fs, method = "radix")]
  a <- sub(".*area_code=([^/]+)/.*", "\\1", fs)
  if (!is.null(areas)) fs <- fs[a %in% areas]
  fs
}

## le um arquivo: colunas de tempo em digitos UTC; `date` como Date; int64 como integer
.pc_fx_read_file <- function(f, dataset) {
  x <- data.table::as.data.table(as.data.frame(arrow::read_parquet(f)))
  for (n in intersect(.PC_FX_TS_COLS[[dataset]], names(x))) data.table::set(x, j = n, value = lubridate::with_tz(x[[n]], "UTC"))
  x
}

## so as particoes afetadas (hit e uma funcao view -> lgl) sao reescritas
.pc_fx_rewrite_hits <- function(dir, dataset, areas, hit, edit) {
  n <- 0L
  for (f in .pc_fx_files(dir, dataset, areas)) {
    x <- .pc_fx_read_file(f, dataset)
    i <- which(hit(x))
    if (!length(i)) next
    edit(x, i)
    .pc_fx_write_file(x, f, .PC_FX_TS_COLS[[dataset]])
    n <- n + length(i)
  }
  n
}

## Multiplica/perturba a regiao pedida. Eixos: `after`/`at_or_before` valem para interval_start (carga, no
## fuso do deck, pelo rotulo do manifest) ou valid_time (clima); `issue_after`/`issue_at_or_before` para issue_time.
prevcarga_fixture_perturb <- function(dir, channel = c("load", "weather_observed", "weather_forecast"),
                                      after = NULL, at_or_before = NULL, issue_after = NULL,
                                      issue_at_or_before = NULL, areas = NULL, perturb = function(v) v * 1.3 + 1) {
  channel <- match.arg(channel)
  out <- prevcarga_fixture_copy(dir)
  m <- .pc_fx_manifest_read(out); tz <- m$timezone$load
  col <- if (channel == "load") "load_mwh" else "temperature"
  hit <- function(x) {
    ok <- rep(TRUE, nrow(x))
    if (channel == "load") {
      t <- lubridate::force_tz(x$timestamp, tz)
      if (identical(m$load_label, "end")) t <- t - lubridate::minutes(30L)
    } else {
      t <- lubridate::force_tz(if (channel == "weather_observed") x$timestamp else x$valid_time, tz)
    }
    if (!is.null(after)) ok <- ok & t > after
    if (!is.null(at_or_before)) ok <- ok & t <= at_or_before
    if (channel == "weather_forecast") {
      it <- lubridate::force_tz(x$issue_time, m$timezone$weather_forecast$issue_time)
      if (!is.null(issue_after)) ok <- ok & it > issue_after
      if (!is.null(issue_at_or_before)) ok <- ok & it <= issue_at_or_before
    }
    ok
  }
  n <- .pc_fx_rewrite_hits(out, channel, areas, hit,
                           function(x, i) data.table::set(x, i = i, j = col, value = perturb(x[[col]][i])))
  if (!n) { unlink(out, recursive = TRUE); prevcarga_abort("tools.fixture_empty_region", where = channel) }
  out
}

prevcarga_fixture_drop_col <- function(dir, dataset, col) {
  if (!dataset %in% names(.PC_FX_DATASETS)) prevcarga_abort("tools.fixture_bad_args", where = "dataset")
  out <- prevcarga_fixture_copy(dir)
  fs <- .pc_fx_files(out, dataset)
  if (!length(fs)) { unlink(out, recursive = TRUE); prevcarga_abort("tools.fixture_empty_region", where = dataset) }
  for (f in fs) {
    tb <- arrow::read_parquet(f, as_data_frame = FALSE)
    keep <- setdiff(tb$schema$names, col)
    cols <- stats::setNames(lapply(keep, function(n) tb$GetColumnByName(n)), keep)
    arrow::write_parquet(do.call(arrow::arrow_table, cols), f)
  }
  out
}

## Remove `n` intervalos de 30 min da carga de `area`, a partir de `at` ("HH:MM", fuso do deck) em `day`: buraco no
## meio do dia. Nunca grava 0.
prevcarga_fixture_drop_slots <- function(dir, area, day, at, n) {
  out <- prevcarga_fixture_copy(dir)
  m <- .pc_fx_manifest_read(out); tz <- m$timezone$load
  lo <- lubridate::ymd_hm(paste(format(lubridate::as_date(day)), at), tz = tz)
  hi <- lo + lubridate::minutes(30L * as.integer(n))
  drop <- integer()
  for (f in .pc_fx_files(out, "load", area)) {
    x <- .pc_fx_read_file(f, "load")
    t <- lubridate::force_tz(x$timestamp, tz)
    if (identical(m$load_label, "end")) t <- t - lubridate::minutes(30L)
    k <- which(t >= lo & t < hi)
    if (!length(k)) next
    drop <- c(drop, k)
    .pc_fx_write_file(x[-k], f, "timestamp")
  }
  if (!length(drop)) { unlink(out, recursive = TRUE); prevcarga_abort("tools.fixture_empty_region", where = "load") }
  out
}

## Troca (ou cria) o feriado de `area` em `date`; `codes` = c(holiday_type_id =, prevcarga_code =, simplified_code =).
prevcarga_fixture_set_holiday <- function(dir, area, date, codes) {
  need <- c("holiday_type_id", "prevcarga_code", "simplified_code")
  if (!is.numeric(codes) || !setequal(names(codes), need)) prevcarga_abort("tools.fixture_bad_args", where = "codes")
  d0 <- lubridate::as_date(date)
  out <- prevcarga_fixture_copy(dir)
  f <- file.path(out, .PC_FX_DATASETS[["holidays"]], paste0("area_code=", area),
                 paste0("year=", lubridate::year(d0)), "part-0.parquet")
  x <- if (file.exists(f)) .pc_fx_read_file(f, "holidays") else
    data.table::data.table(date = lubridate::as_date(character()), holiday_type_id = integer(), prevcarga_code = integer(),
                           simplified_code = integer())
  x <- rbind(x[!(x$date %in% d0)], data.table::data.table(date = d0, holiday_type_id = as.integer(codes[["holiday_type_id"]]),
                                                       prevcarga_code = as.integer(codes[["prevcarga_code"]]),
                                                       simplified_code = as.integer(codes[["simplified_code"]])))
  data.table::setorder(x, date)
  .pc_fx_write_file(x, f, character())
  out
}
