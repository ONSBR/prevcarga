## deck_reader.R -- leitor unico do deck (M2.1, parte 1 de 3; ADR-0035, ADR-0037).
##
## Unico lugar do sistema que abre manifest.json e que chama arrow::open_dataset. Requer errors.R
## carregado antes (prevcarga_abort). Fuso so por lubridate com tz explicito (force_tz + with_tz);
## nenhum Sys.time/Sys.getenv e nenhuma aritmetica de data-hora fora do leitor. Cortes (available_until,
## weather_issue_cutoff), feriados e painel entram no M2.2/M2.7. Saida de terminal: ASCII puro.
##
## Datasets canonicos (relativos a data_dir) e seus nomes no manifest/probe:
.PREVCARGA_DECK_DATASETS <- c(load = "load", weather_observed = file.path("weather", "observed"),
                              weather_forecast = file.path("weather", "forecast"),
                              holidays = file.path("auxiliary", "holidays"))

## --- Manifest estrito (schema v1 fechado) --------------------------------------------------------
.PREVCARGA_MANIFEST_KEYS <- c("schema_version", "load_label", "timezone")
.PREVCARGA_MANIFEST_TZ_KEYS <- c("load", "weather_observed", "weather_forecast", "holidays", "dst_periods")
.PREVCARGA_MANIFEST_FC_KEYS <- c("valid_time", "issue_time")

.prevcarga_manifest_tz <- function(tz, where) {
  if (is.null(tz) || !is.character(tz) || length(tz) != 1L || is.na(tz) || !nzchar(tz))
    prevcarga_abort("contract.tz_missing", where = where)
  if (!tz %in% OlsonNames())
    prevcarga_abort("contract.tz_invalid", where = where, details = list(tz = tz))
  tz
}

.prevcarga_manifest_read_json <- function(data_dir) {
  path <- file.path(data_dir, "manifest.json")
  if (!file.exists(path)) prevcarga_abort("contract.manifest_missing", where = "manifest.json")
  tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
           error = function(e) prevcarga_abort("contract.manifest_invalid", where = "manifest.json",
                                               details = list(reason = "json")))
}

prevcarga_deck_manifest <- function(data_dir) {
  m <- .prevcarga_manifest_read_json(data_dir)
  if (!is.list(m) || (length(m) && (is.null(names(m)) || any(!nzchar(names(m))))))
    prevcarga_abort("contract.manifest_invalid", where = "manifest.json", details = list(reason = "raiz"))
  bad <- setdiff(names(m), .PREVCARGA_MANIFEST_KEYS)
  if (length(bad)) prevcarga_abort("contract.manifest_unknown_key", where = bad[[1L]])

  sv <- m[["schema_version"]]
  if (!(is.numeric(sv) && length(sv) == 1L && !is.na(sv) && sv == 1))
    prevcarga_abort("contract.manifest_invalid", where = "manifest.json:schema_version",
                    details = list(expected = 1L))
  ll <- m[["load_label"]]
  if (!is.null(ll) && !(is.character(ll) && length(ll) == 1L && ll %in% c("end", "start")))
    prevcarga_abort("contract.manifest_invalid", where = "manifest.json:load_label",
                    details = list(allowed = c("end", "start")))

  tzm <- m[["timezone"]]
  if (is.null(tzm)) prevcarga_abort("contract.tz_missing", where = "timezone")
  if (!is.list(tzm) || (length(tzm) && is.null(names(tzm))))
    prevcarga_abort("contract.manifest_invalid", where = "timezone", details = list(reason = "mapa"))
  badtz <- setdiff(names(tzm), .PREVCARGA_MANIFEST_TZ_KEYS)
  if (length(badtz)) prevcarga_abort("contract.manifest_unknown_key", where = paste0("timezone.", badtz[[1L]]))

  fc <- tzm[["weather_forecast"]]
  if (!is.null(fc)) {
    if (!is.list(fc) || (length(fc) && is.null(names(fc))))
      prevcarga_abort("contract.manifest_invalid", where = "weather_forecast", details = list(reason = "mapa"))
    badfc <- setdiff(names(fc), .PREVCARGA_MANIFEST_FC_KEYS)
    if (length(badfc))
      prevcarga_abort("contract.manifest_unknown_key", where = paste0("timezone.weather_forecast.", badfc[[1L]]))
  }

  ## fuso exigido: a carga sempre (e o fuso de saida); os demais so se o dataset esta no store
  has <- function(ds) dir.exists(file.path(data_dir, .PREVCARGA_DECK_DATASETS[[ds]]))
  out_tz <- list(load = .prevcarga_manifest_tz(tzm[["load"]], "load.timestamp"))
  if (has("weather_observed") || !is.null(tzm[["weather_observed"]]))
    out_tz$weather_observed <- .prevcarga_manifest_tz(tzm[["weather_observed"]], "weather_observed.timestamp")
  if (has("weather_forecast") || !is.null(fc)) {
    out_tz$weather_forecast <- list(
      valid_time = .prevcarga_manifest_tz(fc[["valid_time"]], "weather_forecast.valid_time"),
      issue_time = .prevcarga_manifest_tz(fc[["issue_time"]], "weather_forecast.issue_time"))
  }
  if (!is.null(tzm[["holidays"]])) out_tz$holidays <- .prevcarga_manifest_tz(tzm[["holidays"]], "holidays.date")
  if (!is.null(tzm[["dst_periods"]]))
    out_tz$dst_periods <- .prevcarga_manifest_tz(tzm[["dst_periods"]], "dst_periods.date")

  list(schema_version = 1L, load_label = ll, timezone = out_tz)
}

## --- Precedencia do rotulo da carga: manifest > --load-label > config.yaml > end (ADR-0035) -----------
## ctx traz a flag e o config separados (config.R): load_label_flag / load_label_config.
prevcarga_resolve_load_label <- function(ctx, manifest) {
  flag <- ctx$load_label_flag
  cfg <- ctx$load_label_config
  if (!is.null(manifest$load_label)) {
    if (!is.null(flag) && !identical(flag, manifest$load_label))
      prevcarga_abort("contract.load_label_conflict", where = "manifest.json:load_label",
                      details = list(manifest = manifest$load_label, flag = flag))
    return(list(value = manifest$load_label, source = "manifest"))
  }
  if (!is.null(flag)) return(list(value = flag, source = "flag"))
  if (!is.null(cfg)) return(list(value = cfg, source = "config"))
  list(value = "end", source = "default")
}

## --- Fuso (ADR-0037): digitos crus do store ingenuo -> instante correto no fuso da carga ----------------
## x vem de timestamp[ns] SEM fuso; o arrow o entrega como POSIXct cujo epoch e o dos digitos lidos como UTC,
## com tzone do host. with_tz(UTC) recupera os digitos sem depender do fuso da sessao.
.prevcarga_apply_tz <- function(x, declared, tz_load) {
  digits <- lubridate::with_tz(x, tzone = "UTC")
  lubridate::with_tz(lubridate::force_tz(digits, tzone = declared), tzone = tz_load)
}

.prevcarga_check_naive <- function(schema, dataset, col) {
  f <- schema$GetFieldByName(col)
  if (is.null(f)) return(invisible(NULL))
  tz <- tryCatch(f$type$timezone(), error = function(e) "")
  if (is.character(tz) && length(tz) == 1L && nzchar(tz))
    prevcarga_abort("contract.tz_in_parquet", where = paste0(dataset, ".", col))
  invisible(NULL)
}

## --- Abertura unica do dataset Hive --------------------------------------------------------------------
.prevcarga_open <- function(data_dir, rel, dataset) {
  p <- file.path(data_dir, rel)
  if (!dir.exists(p)) prevcarga_abort("contract.dataset_missing", where = dataset)
  arrow::open_dataset(p, partitioning = arrow::hive_partition(area_code = arrow::utf8(),
                                                              year = arrow::int32()))
}

.prevcarga_scan <- function(ds, cols, from, to, areas) {
  fy <- arrow::Expression$field_ref("year")
  fa <- arrow::Expression$field_ref("area_code")
  flt <- NULL
  and <- function(a, b) if (is.null(a)) b else a & b
  if (!is.null(from)) flt <- and(flt, fy >= arrow::Expression$scalar(as.integer(lubridate::year(from)) - 1L))
  if (!is.null(to)) flt <- and(flt, fy <= arrow::Expression$scalar(as.integer(lubridate::year(to))))
  if (length(areas))
    flt <- and(flt, Reduce(`|`, lapply(areas, function(a) fa == arrow::Expression$scalar(as.character(a)))))
  sc <- if (is.null(flt)) arrow::Scanner$create(ds, projection = cols)
        else arrow::Scanner$create(ds, projection = cols, filter = flt)
  data.table::as.data.table(as.data.frame(sc$ToTable()))
}

## [from 00:00, to + 1 00:00) no fuso da carga; NULL = sem limite
.prevcarga_window <- function(from, to, tz) {
  lo <- if (is.null(from)) NULL else lubridate::force_tz(lubridate::as_datetime(from), tz)
  hi <- if (is.null(to)) NULL else lubridate::force_tz(lubridate::as_datetime(to), tz) + lubridate::days(1L)
  list(lo = lo, hi = hi)
}

.prevcarga_in_window <- function(x, w) {
  keep <- rep(TRUE, length(x))
  if (!is.null(w$lo)) keep <- keep & x >= w$lo
  if (!is.null(w$hi)) keep <- keep & x < w$hi
  keep
}

.prevcarga_deck_ctx <- function(ctx) {
  prevcarga_require_data_dir(ctx)
  man <- prevcarga_deck_manifest(ctx$data_dir)
  list(dir = ctx$data_dir, manifest = man, tz = man$timezone$load,
       label = prevcarga_resolve_load_label(ctx, man))
}

## --- Carga: area_code, interval_start, interval_end, load_mwh (sem timestamp) --------------------------
prevcarga_read_load <- function(ctx, areas = NULL, from = NULL, to = NULL) {
  d <- .prevcarga_deck_ctx(ctx)
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[["load"]], "load")
  .prevcarga_check_naive(ds$schema, "load", "timestamp")
  dt <- .prevcarga_scan(ds, c("area_code", "timestamp", "load_mwh"), from, to, areas)
  ts <- .prevcarga_apply_tz(dt$timestamp, d$manifest$timezone$load, d$tz)
  dt[, timestamp := NULL]
  dt[, interval_start := if (d$label$value == "end") ts - lubridate::minutes(30L) else ts]
  dt[, interval_end := interval_start + lubridate::minutes(30L)]
  w <- .prevcarga_window(from, to, d$tz)
  dt <- dt[.prevcarga_in_window(interval_start, w)]
  data.table::setcolorder(dt, c("area_code", "interval_start", "interval_end", "load_mwh"))
  data.table::setkey(dt, area_code, interval_start)
  data.table::setattr(dt, "load_label", d$label$value)
  data.table::setattr(dt, "load_label_source", d$label$source)
  data.table::setattr(dt, "tz", d$tz)
  dt[]
}

## --- Clima por area (exogenas instantaneas: nao deslocadas) --------------------------------------------
prevcarga_read_weather <- function(ctx, kind = c("observed", "forecast"), areas = NULL, from = NULL,
                                   to = NULL, select = NULL) {
  kind <- match.arg(kind)
  d <- .prevcarga_deck_ctx(ctx)
  dataset <- paste0("weather_", kind)
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[[dataset]], dataset)
  tzs <- d$manifest$timezone
  if (kind == "observed") {
    tcols <- c(timestamp = tzs$weather_observed)
    key <- "timestamp"; base <- c("timestamp", "temperature", "heat_index", "rh")
  } else {
    tcols <- c(issue_time = tzs$weather_forecast$issue_time, valid_time = tzs$weather_forecast$valid_time)
    key <- "valid_time"; base <- c("issue_time", "valid_time", "temperature", "heat_index")
  }
  for (col in names(tcols)) .prevcarga_check_naive(ds$schema, dataset, col)
  want <- if (is.null(select)) base else intersect(base, union(select, names(tcols)))
  cols <- c("area_code", intersect(want, ds$schema$names))
  dt <- .prevcarga_scan(ds, cols, from, to, areas)
  for (col in intersect(names(tcols), names(dt)))
    data.table::set(dt, j = col, value = .prevcarga_apply_tz(dt[[col]], tcols[[col]], d$tz))
  w <- .prevcarga_window(from, to, d$tz)
  dt <- dt[.prevcarga_in_window(dt[[key]], w)]
  data.table::setkeyv(dt, c("area_code", intersect(c("issue_time", key), names(dt))))
  data.table::setattr(dt, "tz", d$tz)
  dt[]
}

## --- Sonda: schema/tipos/Hive sem coletar linhas ---------------------------------------------------------
.prevcarga_probe_one <- function(data_dir, rel) {
  p <- file.path(data_dir, rel)
  if (!dir.exists(p)) return(list(present = FALSE))
  files <- list.files(p, pattern = "\\.parquet$", recursive = TRUE, full.names = FALSE)
  files <- files[order(files, method = "radix")]
  rels <- if (length(files)) file.path(rel, files) else character()
  hive_re <- "^[^/]+(/[^/]+)*/area_code=[^/]+/year=[0-9]+/[^/]+\\.parquet$"
  hive_ok <- length(rels) > 0L && all(grepl(hive_re, rels))
  schema <- NULL
  unreadable <- character()
  for (i in seq_along(rels)) {
    s <- tryCatch(arrow::ParquetFileReader$create(file.path(data_dir, rels[[i]]))$GetSchema(),
                  error = function(e) NULL)
    if (is.null(s)) unreadable <- c(unreadable, rels[[i]]) else if (is.null(schema)) schema <- s
  }
  cols <- if (is.null(schema)) character() else schema$names
  types <- if (is.null(schema)) character() else
    vapply(cols, function(cn) schema$GetFieldByName(cn)$type$ToString(), "")
  is_ts <- if (is.null(schema)) logical() else
    vapply(cols, function(cn) inherits(schema$GetFieldByName(cn)$type, "Timestamp"), NA)
  tzp <- if (is.null(schema)) logical() else
    vapply(cols[is_ts], function(cn) {
      tz <- tryCatch(schema$GetFieldByName(cn)$type$timezone(), error = function(e) "")
      is.character(tz) && length(tz) == 1L && nzchar(tz)
    }, NA)
  list(present = TRUE, files = rels, hive_ok = hive_ok, columns = cols, types = types,
       tz_in_parquet = tzp, unreadable = unreadable)
}

prevcarga_deck_probe <- function(data_dir) {
  lapply(.PREVCARGA_DECK_DATASETS, function(rel) .prevcarga_probe_one(data_dir, rel))
}
