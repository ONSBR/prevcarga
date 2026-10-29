## deck_reader.R -- leitor unico do deck (M2.1, parte 1 de 3; ADR-0035, ADR-0037).
##
## Unico lugar do sistema que abre manifest.json e que chama arrow::open_dataset. Requer errors.R
## carregado antes (prevcarga_abort). Fuso so por lubridate com tz explicito (force_tz + with_tz);
## nenhum Sys.time/Sys.getenv e nenhuma aritmetica de data-hora fora do leitor. M2.2: corte intradiario
## derivado do dado (available_until), filtro de saida e clima pela regra weather_issue_cutoff. M2.7: feriados
## (calendario conhecido de antemao, sem corte) e painel do leitor. Saida de terminal: ASCII puro.
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

.prevcarga_scan <- function(ds, cols, from, to, areas, years = NULL) {
  fy <- arrow::Expression$field_ref("year")
  fa <- arrow::Expression$field_ref("area_code")
  flt <- NULL
  and <- function(a, b) if (is.null(a)) b else a & b
  if (!is.null(from)) flt <- and(flt, fy >= arrow::Expression$scalar(as.integer(lubridate::year(from - lubridate::days(1L)))))
  if (!is.null(to)) flt <- and(flt, fy <= arrow::Expression$scalar(as.integer(lubridate::year(to))))
  if (length(years))
    flt <- and(flt, Reduce(`|`, lapply(years, function(y) fy == arrow::Expression$scalar(as.integer(y)))))
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

## --- Corte intradiario derivado do dado (nunca manifest, flag nem relogio) --------------------------------
## Por area: interval_end do ultimo intervalo CONTIGUO desde O 00:00 (sem intervalo em O => O 00:00; 48 => O+1 00:00).
## `gap` = inicio do primeiro intervalo faltante quando ainda existe intervalo presente depois do buraco.
.prevcarga_area_until <- function(istart, origin, tz) {
  o0 <- lubridate::force_tz(lubridate::as_datetime(origin), tz)
  s <- sort(unique(istart[istart >= o0 & istart < o0 + lubridate::days(1L)]))
  if (!length(s)) return(list(until = o0, gap = NA_real_))
  k <- as.integer(lubridate::time_length(s - o0, "minute") %/% 30L)
  run <- sum(cumprod(k == seq_along(k) - 1L))
  gap <- if (run < length(k)) as.numeric(o0 + lubridate::minutes(30L * run)) else NA_real_
  list(until = o0 + lubridate::minutes(30L * run), gap = gap)
}

prevcarga_available_until <- function(load, origin, areas = NULL) {
  tz <- attr(load, "tz")
  if (is.null(tz)) tz <- lubridate::tz(load$interval_start)
  if (is.null(areas)) areas <- unique(as.character(load$area_code))
  areas <- sort(unique(as.character(areas)), method = "radix")
  per <- lapply(areas, function(a) .prevcarga_area_until(load$interval_start[load$area_code == a], origin, tz))
  until <- lubridate::as_datetime(vapply(per, function(p) as.numeric(p$until), 0), tz = tz)
  gap <- lubridate::as_datetime(vapply(per, function(p) p$gap, 0), tz = tz)
  by_area <- data.table::data.table(area_code = areas, until = until, gap = gap)
  glob <- if (length(until)) min(until) else lubridate::force_tz(lubridate::as_datetime(origin), tz)
  list(available_until = glob, available_until_by_area = by_area,
       spread = length(unique(as.numeric(until))) > 1L)
}

## --- Carga bruta (sem corte): area_code, interval_start, interval_end, load_mwh -----------------------------
.prevcarga_load_raw <- function(d, areas, from, to) {
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
  dt
}

## Deriva o corte da carga (apos truncate_at) ou aborta: sem linha => completude.no_load; buraco no dia O =>
## completude.origin_day_gap (o corte nunca encurta por causa do buraco). A janela de derivacao inclui a vespera de O:
## corte da manha (truncate_at = O 00:00) ou deck sem linha no dia O => available_until = O 00:00, nao no_load.
.prevcarga_derive_cut <- function(ctx_d, areas, origin, truncate_at, from, to) {
  eve <- lubridate::as_date(origin) - 1L
  lo <- if (is.null(from)) eve else min(lubridate::as_date(from), eve)
  hi <- if (is.null(to)) origin else max(to, origin)
  ext <- .prevcarga_load_raw(ctx_d, areas, lo, hi)
  if (!is.null(truncate_at)) ext <- ext[interval_end <= truncate_at]
  if (!nrow(ext)) prevcarga_abort("completude.no_load", where = "load")
  data.table::setattr(ext, "tz", ctx_d$tz)
  scope <- if (is.null(areas)) unique(ext$area_code) else areas
  au <- prevcarga_available_until(ext, origin, scope)
  bad <- au$available_until_by_area[!is.na(gap)]
  if (nrow(bad))
    prevcarga_abort("completude.origin_day_gap",
                    where = sprintf("%s:%s", bad$area_code[1L], format(bad$gap[1L], "%H:%M")))
  list(load = ext, au = au)
}

prevcarga_read_load <- function(ctx, areas = NULL, from = NULL, to = NULL, origin = NULL, truncate_at = NULL) {
  d <- .prevcarga_deck_ctx(ctx)
  if (is.null(origin)) {
    dt <- .prevcarga_load_raw(d, areas, from, to)
    if (!is.null(truncate_at)) dt <- dt[interval_end <= truncate_at]
    au <- NULL
  } else {
    r <- .prevcarga_derive_cut(d, areas, origin, truncate_at, from, to)
    au <- r$au
    w <- .prevcarga_window(from, to, d$tz)
    dt <- r$load[.prevcarga_in_window(interval_start, w) & interval_end <= au$available_until]
    if (isTRUE(au$spread))
      prevcarga_warn("warning.available_until_spread",
                     where = paste(sprintf("%s:%s", au$available_until_by_area$area_code,
                                           format(au$available_until_by_area$until, "%H:%M")), collapse = ","))
  }
  data.table::setkey(dt, area_code, interval_start)
  data.table::setattr(dt, "load_label", d$label$value)
  data.table::setattr(dt, "load_label_source", d$label$source)
  data.table::setattr(dt, "tz", d$tz)
  if (!is.null(au)) {
    data.table::setattr(dt, "available_until", au$available_until)
    data.table::setattr(dt, "available_until_by_area", au$available_until_by_area)
    data.table::setattr(dt, "available_until_spread", au$spread)
    data.table::setattr(dt, "origin", origin)
  }
  dt[]
}

## Dia (no fuso da carga) do ultimo intervalo de carga presente: base do default de --origin. Le so as
## particoes `year` mais altas (area_code, timestamp); nunca o relogio, nunca o store inteiro.
prevcarga_last_load_day <- function(ctx, areas = NULL) {
  d <- .prevcarga_deck_ctx(ctx)
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[["load"]], "load")
  .prevcarga_check_naive(ds$schema, "load", "timestamp")
  yrs <- sort(unique(as.integer(sub(".*year=([0-9]+).*", "\\1", grep("year=[0-9]+", ds$files, value = TRUE)))),
              decreasing = TRUE)
  for (y in yrs) {
    dt <- .prevcarga_scan(ds, c("area_code", "timestamp"), NULL, NULL, areas, years = y)
    if (!nrow(dt)) next
    ts <- .prevcarga_apply_tz(dt$timestamp, d$manifest$timezone$load, d$tz)
    istart <- if (d$label$value == "end") ts - lubridate::minutes(30L) else ts
    return(lubridate::as_date(max(istart), tz = d$tz))
  }
  prevcarga_abort("completude.no_load", where = "load")
}

## --- Limite de emissao do clima previsto (ADR-0024); o dates.R so reexpoe este valor -------------------------
prevcarga_weather_issue_limit <- function(rule, origin, available_until, tz_load) {
  if (identical(rule, "available_until")) return(lubridate::with_tz(available_until, tz_load))
  if (identical(rule, "origin_midnight_utc"))
    return(lubridate::with_tz(lubridate::force_tz(lubridate::as_datetime(origin), "UTC"), tz_load))
  prevcarga_abort("config.bad_weather_cutoff", where = as.character(rule))
}

## Por area, a emissao mais fresca com issue_time <= limit.
prevcarga_freshest_issue <- function(fc, limit) {
  ok <- fc[issue_time <= limit]
  if (!nrow(ok)) return(ok)
  last <- ok[, .(issue_time = max(issue_time)), by = area_code]
  ok[last, on = .(area_code, issue_time), nomatch = NULL]
}

## --- Clima por area (exogenas instantaneas: nao deslocadas) --------------------------------------------
## Com `origin`: observado so <= available_until (derivado da carga lida pelo proprio leitor, mesmo
## origin/truncate_at); previsto so emissoes <= weather_issue_limit, a mais fresca por area.
## O observado sai com `valid_time` (a coluna crua `timestamp` do store so existe aqui dentro).
prevcarga_read_weather <- function(ctx, kind = c("observed", "forecast"), areas = NULL, from = NULL,
                                   to = NULL, select = NULL, origin = NULL, truncate_at = NULL) {
  kind <- match.arg(kind)
  d <- .prevcarga_deck_ctx(ctx)
  dataset <- paste0("weather_", kind)
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[[dataset]], dataset)
  tzs <- d$manifest$timezone
  if (kind == "observed") {
    tcols <- c(timestamp = tzs$weather_observed)
    base <- c("timestamp", "temperature", "heat_index", "rh")
  } else {
    tcols <- c(issue_time = tzs$weather_forecast$issue_time, valid_time = tzs$weather_forecast$valid_time)
    base <- c("issue_time", "valid_time", "temperature", "heat_index")
  }
  key <- "valid_time"
  for (col in names(tcols)) .prevcarga_check_naive(ds$schema, dataset, col)
  want <- if (is.null(select)) base else intersect(base, union(select, names(tcols)))
  cols <- c("area_code", intersect(want, ds$schema$names))
  au <- NULL
  if (!is.null(origin)) au <- .prevcarga_derive_cut(d, areas, origin, truncate_at, from, to)$au
  dt <- .prevcarga_scan(ds, cols, from, to, areas)
  for (col in intersect(names(tcols), names(dt)))
    data.table::set(dt, j = col, value = .prevcarga_apply_tz(dt[[col]], tcols[[col]], d$tz))
  if (kind == "observed" && "timestamp" %in% names(dt)) data.table::setnames(dt, "timestamp", "valid_time")
  extra <- list()
  if (!is.null(au)) {
    if (kind == "observed") {
      dt <- dt[valid_time <= au$available_until]
    } else {
      rule <- if (is.null(ctx$weather_issue_cutoff)) "available_until" else ctx$weather_issue_cutoff
      src <- ctx$sources$weather_issue_cutoff
      limit <- prevcarga_weather_issue_limit(rule, origin, au$available_until, d$tz)
      dt <- prevcarga_freshest_issue(dt, limit)
      extra <- list(weather_issue_cutoff = rule,
                    weather_issue_cutoff_source = if (is.null(src)) "default" else src,
                    weather_issue_limit = limit,
                    freshest_issue_by_area = unique(dt[, .(area_code, issue_time)])[order(area_code, method = "radix")])
    }
  }
  w <- .prevcarga_window(from, to, d$tz)
  dt <- dt[.prevcarga_in_window(dt[[key]], w)]
  data.table::setkeyv(dt, c("area_code", intersect(c("issue_time", key), names(dt))))
  data.table::setattr(dt, "tz", d$tz)
  if (!is.null(au)) {
    data.table::setattr(dt, "available_until", au$available_until)
    data.table::setattr(dt, "origin", origin)
  }
  for (n in names(extra)) data.table::setattr(dt, n, extra[[n]])
  dt[]
}

## --- Feriados (M2.7): calendario conhecido de antemao ---------------------------------------------------------
## `date` e a data civil no fuso da carga: sem deslocamento de 30 min e SEM filtro de available_until. Particao `year`
## exata (data civil nao desloca); nao filtra nem corrige codigos (code_negative e do validate-deck).
prevcarga_read_holidays <- function(ctx, areas, from, to) {
  d <- .prevcarga_deck_ctx(ctx)
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[["holidays"]], "holidays")
  yrs <- seq(lubridate::year(from), lubridate::year(to))
  h <- .prevcarga_scan(ds, c("area_code", "date", "holiday_type_id", "prevcarga_code", "simplified_code"),
                       NULL, NULL, areas, years = yrs)
  data.table::set(h, j = "date", value = lubridate::as_date(h$date))
  h <- h[date >= lubridate::as_date(from) & date <= lubridate::as_date(to)]
  data.table::setorderv(h, c("area_code", "date"))
  data.table::setkeyv(h, c("area_code", "date"))
  h[]
}

## Codigos de feriado crus de todas as particoes (validate-deck, M2.4). A data civil nao tem fuso: nao exige
## manifest nem origem; nao filtra nem corrige codigos. Leitura somente do dataset `auxiliary/holidays`.
prevcarga_read_holiday_codes <- function(ctx, areas = NULL) {
  prevcarga_require_data_dir(ctx)
  ds <- .prevcarga_open(ctx$data_dir, .PREVCARGA_DECK_DATASETS[["holidays"]], "holidays")
  h <- .prevcarga_scan(ds, c("area_code", "date", "holiday_type_id", "prevcarga_code", "simplified_code"),
                       NULL, NULL, areas)
  data.table::set(h, j = "date", value = lubridate::as_date(h$date))
  data.table::setorderv(h, c("area_code", "date"))
  h[]
}

## --- Painel do leitor (M2.7) ---------------------------------------------------------------------------------
## Agrega os atributos dos leitores sem recalcular; aborta se divergirem em tz ou available_until.
.PREVCARGA_PANEL_LOAD_ATTRS <- c("available_until", "available_until_by_area", "load_label", "load_label_source", "tz")
.PREVCARGA_PANEL_FC_ATTRS <- c("weather_issue_cutoff", "weather_issue_limit")

pc_panel_attrs <- function(load, weather_obs, weather_fc) {
  for (x in list(weather_obs, weather_fc)) {
    if (!identical(attr(x, "tz"), attr(load, "tz")))
      prevcarga_abort("contract.constraint_violated", where = "panel.tz")
    if (!identical(attr(x, "available_until"), attr(load, "available_until")))
      prevcarga_abort("contract.constraint_violated", where = "panel.available_until")
  }
  c(stats::setNames(lapply(.PREVCARGA_PANEL_LOAD_ATTRS, function(n) attr(load, n)), .PREVCARGA_PANEL_LOAD_ATTRS),
    stats::setNames(lapply(.PREVCARGA_PANEL_FC_ATTRS, function(n) attr(weather_fc, n)), .PREVCARGA_PANEL_FC_ATTRS))
}

## Origem ja resolvida pelo chamador (o leitor nao depende de dates.R). Topologia: ctx$topology_path ou
## <ctx$root>/config/topology.yaml; area fora dela => topology.area_undeclared antes de qualquer leitura.
## `truncate_at` (opcional) vai IGUAL aos tres leitores: carga, observado e previsto derivam o mesmo available_until.
prevcarga_read_panel <- function(ctx, origin, areas, horizon_days, history_days, truncate_at = NULL) {
  O <- lubridate::as_date(origin)
  tp <- if (!is.null(ctx$topology_path)) ctx$topology_path else prevcarga_topology_path(ctx$root)
  undeclared <- setdiff(areas, prevcarga_topology(path = tp)$leaves)
  if (length(undeclared)) prevcarga_abort("topology.area_undeclared", where = undeclared[[1L]])
  load <- prevcarga_read_load(ctx, areas, from = O - history_days, to = O, origin = O, truncate_at = truncate_at)
  obs <- prevcarga_read_weather(ctx, "observed", areas, O - history_days, O, origin = O, truncate_at = truncate_at)
  fc <- prevcarga_read_weather(ctx, "forecast", areas, O, O + horizon_days, origin = O, truncate_at = truncate_at)
  hol <- prevcarga_read_holidays(ctx, areas, O - history_days, O + horizon_days)
  list(load = load, weather_observed = obs, weather_forecast = fc, holidays = hol,
       attrs = c(list(origin = O), pc_panel_attrs(load, obs, fc)))
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
