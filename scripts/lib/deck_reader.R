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
.PREVCARGA_MANIFEST_KEYS <- c("schema_version", "load_label", "timezone", "weather_forecast_publication_lag_hours",
                              "kind", "inference")
.PREVCARGA_MANIFEST_INFERENCE_KEYS <- c("origin", "available_until", "train_deck_hash", "windows")
## Chaves opcionais do bloco inference (nao exigidas): holiday_annex = diretorio do anexo, relativo ao deck.
## inference_needs_source (E2-T4): proveniencia das necessidades do gerador (artifacts | topology | mixed).
.PREVCARGA_MANIFEST_INFERENCE_OPTIONAL <- c("holiday_annex", "inference_needs_source")
.PREVCARGA_MANIFEST_TZ_KEYS <- c("load", "weather_observed", "weather_forecast", "holidays", "dst_periods")
.PREVCARGA_MANIFEST_FC_KEYS <- c("valid_time", "issue_time")

## olson injetavel (teste). Base de fusos vazia e falha do ambiente (runtime.tzdata_missing), nao do deck.
.prevcarga_manifest_tz <- function(tz, where, olson = OlsonNames) {
  if (is.null(tz) || !is.character(tz) || length(tz) != 1L || is.na(tz) || !nzchar(tz))
    prevcarga_abort("contract.tz_missing", where = where)
  known <- olson()
  if (!length(known)) prevcarga_abort("runtime.tzdata_missing", where = where, details = list(tz = tz))
  if (!tz %in% known)
    prevcarga_abort("contract.tz_invalid", where = where, details = list(tz = tz))
  tz
}

## Fusos declarados no manifest, sem validar (insumo do doctor). Vetor unico, sem NA/vazio.
prevcarga_deck_manifest_timezones <- function(data_dir) {
  m <- .prevcarga_manifest_read_json(data_dir)
  tzm <- if (is.list(m)) m[["timezone"]] else NULL
  if (!is.list(tzm)) return(character())
  fc <- tzm[["weather_forecast"]]
  vals <- c(tzm[["load"]], tzm[["weather_observed"]],
            if (is.list(fc)) c(fc[["valid_time"]], fc[["issue_time"]]),
            tzm[["holidays"]], tzm[["dst_periods"]])
  vals <- unlist(vals[vapply(vals, function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v), logical(1))])
  unique(as.character(vals))
}

## Bloco inference do manifest (kind inference): origem, corte, hash do deck de treino e janelas por dataset.
.prevcarga_manifest_inference <- function(inf, tz_load) {
  w <- "manifest.json:inference"
  bad_ <- function(field, reason = NULL)
    prevcarga_abort("contract.manifest_invalid", where = paste0(w, ".", field),
                    details = if (is.null(reason)) list() else list(reason = reason))
  if (!is.list(inf) || (length(inf) && is.null(names(inf))))
    prevcarga_abort("contract.manifest_invalid", where = w, details = list(reason = "mapa"))
  badk <- setdiff(names(inf), c(.PREVCARGA_MANIFEST_INFERENCE_KEYS, .PREVCARGA_MANIFEST_INFERENCE_OPTIONAL))
  if (length(badk)) prevcarga_abort("contract.manifest_unknown_key", where = paste0("inference.", badk[[1L]]))
  for (k in .PREVCARGA_MANIFEST_INFERENCE_KEYS) if (is.null(inf[[k]])) bad_(k, "ausente")

  iso_date <- function(x, field) {
    if (!(is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))) bad_(field, "AAAA-MM-DD")
    d <- suppressWarnings(lubridate::ymd(x, tz = "UTC"))
    if (is.na(d)) bad_(field, "AAAA-MM-DD")
    lubridate::as_date(d)
  }
  origin <- iso_date(inf[["origin"]], "origin")

  au <- inf[["available_until"]]
  if (!(is.character(au) && length(au) == 1L && !is.na(au) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}$", au)))
    bad_("available_until", "AAAA-MM-DDTHH:MM:SS")
  au_t <- suppressWarnings(lubridate::ymd_hms(au, tz = tz_load, quiet = TRUE))
  if (is.na(au_t)) bad_("available_until", "AAAA-MM-DDTHH:MM:SS")

  h <- inf[["train_deck_hash"]]
  if (!(is.character(h) && length(h) == 1L && !is.na(h) && grepl("^[0-9a-f]{64}$", h)))
    bad_("train_deck_hash", "sha256 com 64 hex minusculos")

  win <- inf[["windows"]]
  if (!is.list(win) || (length(win) && (is.null(names(win)) || any(!nzchar(names(win))))))
    bad_("windows", "mapa dataset -> from/to")
  wout <- list()
  for (nm in names(win)) {
    ft <- win[[nm]]
    fld <- paste0("windows.", nm)
    if (!is.list(ft) || is.null(names(ft)) || !setequal(names(ft), c("from", "to"))) bad_(fld, "from/to")
    f <- iso_date(ft[["from"]], paste0(fld, ".from"))
    t <- iso_date(ft[["to"]], paste0(fld, ".to"))
    if (f > t) bad_(fld, "from > to")
    wout[[nm]] <- list(from = f, to = t)
  }
  ann <- inf[["holiday_annex"]]
  if (!is.null(ann) && !(is.character(ann) && length(ann) == 1L && !is.na(ann) && nzchar(ann) &&
                         !grepl("^(/|[A-Za-z]:|\\\\)", ann) && !any(strsplit(ann, "[/\\\\]")[[1L]] == "..")))
    bad_("holiday_annex", "caminho relativo ao deck, sem ..")
  src <- inf[["inference_needs_source"]]
  if (!is.null(src) && !(is.character(src) && length(src) == 1L && !is.na(src) && src %in% c("artifacts", "topology", "mixed")))
    bad_("inference_needs_source", "artifacts|topology|mixed")
  res <- list(origin = origin, available_until = au_t, train_deck_hash = h, windows = wout)
  if (!is.null(ann)) res$holiday_annex <- ann
  if (!is.null(src)) res$inference_needs_source <- src
  res
}

## Atraso de publicacao do clima previsto (ADR-0024): horas inteiras >= 0 entre issue_time e a emissao estar disponivel.
## Propriedade do deck (como o fuso): obrigatoria quando o deck tem weather_forecast; sem default nem variavel de ambiente.
.prevcarga_manifest_lag <- function(lag) {
  where <- "manifest.json:weather_forecast_publication_lag_hours"
  if (is.null(lag)) prevcarga_abort("contract.weather_lag_missing", where = where)
  if (!(is.numeric(lag) && length(lag) == 1L && is.finite(lag) && lag >= 0 && lag == floor(lag)))
    prevcarga_abort("contract.manifest_invalid", where = where, details = list(allowed = "inteiro >= 0 (horas)"))
  as.integer(lag)
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
  ## kind holiday_annex (E3-T2; ADR-044): manifest minimo do subdeck do anexo, so schema_version + kind. Fuso, load_label e
  ## train_deck_hash vem do deck de inferencia dono (prevcarga_annex_ctx). Nunca e deck principal (require_deck recusa).
  if (identical(m[["kind"]], "holiday_annex")) {
    badan <- setdiff(names(m), c("schema_version", "kind"))
    if (length(badan)) prevcarga_abort("contract.manifest_unknown_key", where = badan[[1L]])
    return(list(schema_version = 1L, kind = "holiday_annex"))
  }
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
  lag <- m[["weather_forecast_publication_lag_hours"]]
  if (has("weather_forecast") || !is.null(fc)) {
    out_tz$weather_forecast <- list(
      valid_time = .prevcarga_manifest_tz(fc[["valid_time"]], "weather_forecast.valid_time"),
      issue_time = .prevcarga_manifest_tz(fc[["issue_time"]], "weather_forecast.issue_time"))
    lag <- .prevcarga_manifest_lag(lag)
  } else if (!is.null(lag)) lag <- .prevcarga_manifest_lag(lag)
  if (!is.null(tzm[["holidays"]])) out_tz$holidays <- .prevcarga_manifest_tz(tzm[["holidays"]], "holidays.date")
  if (!is.null(tzm[["dst_periods"]]))
    out_tz$dst_periods <- .prevcarga_manifest_tz(tzm[["dst_periods"]], "dst_periods.date")

  kind <- m[["kind"]]
  if (is.null(kind)) kind <- "train"
  if (!(is.character(kind) && length(kind) == 1L && !is.na(kind) && kind %in% c("train", "inference")))
    prevcarga_abort("contract.manifest_invalid", where = "manifest.json:kind",
                    details = list(allowed = c("train", "inference")))
  inf <- m[["inference"]]
  if (identical(kind, "inference") && is.null(inf))
    prevcarga_abort("contract.manifest_invalid", where = "manifest.json:inference", details = list(reason = "ausente"))
  if (identical(kind, "train") && !is.null(inf))
    prevcarga_abort("contract.manifest_invalid", where = "manifest.json:inference", details = list(reason = "kind train"))

  out <- list(schema_version = 1L, load_label = ll, timezone = out_tz)
  if (!is.null(lag)) out$weather_forecast_publication_lag_hours <- lag
  out["kind"] <- list(kind)
  out["inference"] <- list(if (is.null(inf)) NULL else .prevcarga_manifest_inference(inf, out_tz$load))
  if (!is.null(out$inference$holiday_annex)) out$holiday_annex <- out$inference$holiday_annex
  out
}

## Guarda dos verbos que leem deck: data_dir + manifest + conferencia de kind (papel) e de origem (deck de inferencia).
## ctx$data_role NULL = train (ate o E1-T3 resolver o papel). Nenhuma leitura de dataset aqui.
prevcarga_require_deck <- function(ctx) {
  prevcarga_require_data_dir(ctx)
  man <- prevcarga_deck_manifest(ctx$data_dir)
  role <- if (is.null(ctx$data_role)) "train" else ctx$data_role
  opt <- if (is.null(ctx$data_role_option)) c(train = "--data-train", inference = "--data-inference")[[role]] else ctx$data_role_option
  if (!identical(man$kind, role))
    prevcarga_abort("contract.deck_kind_mismatch", where = opt, details = list(kind = man$kind, expected = role))
  if (identical(man$kind, "inference") && !is.null(ctx$origin)) {
    want <- lubridate::as_date(lubridate::ymd(format(ctx$origin), tz = "UTC"))
    if (!identical(man$inference$origin, want))
      prevcarga_abort("contract.deck_origin_mismatch", where = "--origin",
                      details = list(deck_origin = format(man$inference$origin, "%Y-%m-%d"),
                                     requested_origin = format(want, "%Y-%m-%d")))
  }
  invisible(list(ctx = ctx, manifest = man))
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
  man <- if (!is.null(ctx$annex_owner_manifest)) ctx$annex_owner_manifest else prevcarga_deck_manifest(ctx$data_dir)
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

## Rotulo da carga -> intervalo [start, end): unico lugar do rotulo de 30 min do deck (carga, geradores de deck derivados).
.prevcarga_load_interval <- function(ts, label) {
  start <- if (identical(label, "end")) ts - lubridate::minutes(30L) else ts
  list(start = start, end = start + lubridate::minutes(30L))
}

## --- Carga bruta (sem corte): area_code, interval_start, interval_end, load_mwh -----------------------------
.prevcarga_load_raw <- function(d, areas, from, to) {
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[["load"]], "load")
  .prevcarga_check_naive(ds$schema, "load", "timestamp")
  dt <- .prevcarga_scan(ds, c("area_code", "timestamp", "load_mwh"), from, to, areas)
  ts <- .prevcarga_apply_tz(dt$timestamp, d$manifest$timezone$load, d$tz)
  dt[, timestamp := NULL]
  iv <- .prevcarga_load_interval(ts, d$label$value)
  dt[, interval_start := iv$start]
  dt[, interval_end := iv$end]
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
## available_until: a emissao so esta disponivel `lag_hours` depois do issue_time (manifest do deck,
## weather_forecast_publication_lag_hours) => limite = available_until - lag, no fuso da carga. Unico lugar com essa
## aritmetica: treino, infer, painel e dates.R herdam o limite pronto. origin_midnight_utc (paridade do oraculo): sem lag.
prevcarga_weather_issue_limit <- function(rule, origin, available_until, tz_load, lag_hours) {
  if (identical(rule, "available_until")) {
    if (missing(lag_hours) || is.null(lag_hours))
      prevcarga_abort("contract.weather_lag_missing", where = "manifest.json:weather_forecast_publication_lag_hours")
    return(lubridate::with_tz(available_until, tz_load) - lubridate::hours(.prevcarga_manifest_lag(lag_hours)))
  }
  if (identical(rule, "origin_midnight_utc"))
    return(lubridate::with_tz(lubridate::force_tz(lubridate::as_datetime(origin), "UTC"), tz_load))
  prevcarga_abort("config.bad_weather_cutoff", where = as.character(rule))
}

## Lag efetivo da regra (proveniencia): o do deck em available_until; 0 em origin_midnight_utc (a regra nao usa lag).
prevcarga_weather_effective_lag <- function(rule, lag_hours)
  if (identical(rule, "origin_midnight_utc")) 0L else .prevcarga_manifest_lag(lag_hours)

## Por (area, valid_time), a emissao mais fresca com issue_time <= limit (como os oraculos HW, dessem-linear/regdin e
## LGBM: "for each valid_time keep the most recent issue"). Uma emissao que comeca depois de 00:00 do dia O (06Z, 18Z)
## nao apaga as primeiras horas do dia: elas vem da emissao anterior. Emissao > limit nunca entra.
prevcarga_freshest_issue <- function(fc, limit) {
  ok <- fc[issue_time <= limit]
  if (!nrow(ok)) return(ok)
  last <- ok[, .(issue_time = max(issue_time)), by = .(area_code, valid_time)]
  ok[last, on = .(area_code, valid_time, issue_time), nomatch = NULL]
}

## Todas as emissoes com issue_time <= limit em [from, to] (valid_time): o insumo dos plugins que montam a propria safra
## por origem/lead (descriptor `weather_forecast_in_train`), no treino e no infer. Honestidade: nada depois do limite.
prevcarga_read_forecast_vintages <- function(ctx, areas, from, to, limit) {
  fc <- prevcarga_read_weather(ctx, "forecast", areas, from, to)
  fc[issue_time <= limit]
}

## --- Leitura do previsto pela regra que o plugin declara (E2-T2; data_needs -> weather_forecast.segments) -----------
## Cada segmento {kind, from, to, lead} e um recorte relativo a O; a uniao deduplicada dos segmentos e o que o plugin recebe.
## Os tres seletores sao puros e reproduzem a escolha do proprio plugin (freshest: prevcarga_freshest_issue; per_origin:
## .rf_weather_vintage do random-forest; min_lead: lgbm_weather_vintage), entao entregar so as linhas selecionadas nao muda a
## escolha do plugin (delta 0 do LONG por membro). Dias por lubridate no tz do valid_time; issue_day sempre em UTC.
## Nenhum nome de modelo. Emissao > limit nunca entra (honestidade, ADR-0024).

.prevcarga_rule_valid_day <- function(fc) lubridate::as_date(fc$valid_time, tz = lubridate::tz(fc$valid_time))

## freshest: por (area, valid_time), a emissao mais fresca <= limit nos dias validos [O+from, O+to].
.prevcarga_rule_freshest <- function(fc, O, s, limit) {
  vd <- .prevcarga_rule_valid_day(fc)
  prevcarga_freshest_issue(fc[vd >= O + s$from & vd <= O + s$to], limit)
}

## per_origin: para cada origem nativa T em [O+from, O+to], limit(T) = limit - days(O - T); a emissao mais fresca em
## (limit(T) - 1 dia, limit(T)] e entregue inteira (todas as suas linhas). Origem sem candidata nao entrega nada.
.prevcarga_rule_per_origin <- function(fc, O, s, limit) {
  iss <- sort(unique(fc$issue_time))
  origins <- seq(O + s$from, O + s$to, by = "day")
  pick <- vapply(origins, function(Tn) {
    lim <- limit - lubridate::days(as.integer(O - Tn))
    cand <- iss[iss > lim - lubridate::days(1L) & iss <= lim]
    if (length(cand)) as.numeric(max(cand)) else NA_real_
  }, 0)
  fc[as.numeric(issue_time) %in% pick[!is.na(pick)]]
}

## min_lead: valid_day (tz da carga) em [O+from, O+to] e valid_day - issue_day(UTC) >= lead; por valid_time, a mais fresca.
.prevcarga_rule_min_lead <- function(fc, O, s, limit) {
  vd <- .prevcarga_rule_valid_day(fc)
  id <- lubridate::as_date(fc$issue_time, tz = "UTC")
  w <- fc[vd >= O + s$from & vd <= O + s$to & as.integer(vd - id) >= s$lead]
  w <- w[order(area_code, valid_time, -as.numeric(issue_time))]
  w[!duplicated(w, by = c("area_code", "valid_time"))]
}

prevcarga_read_forecast_by_rule <- function(ctx, area, origin, segments, limit) {
  O <- lubridate::as_date(origin)
  if (!length(segments)) return(prevcarga_read_weather(ctx, "forecast", area, O, O)[0L])
  from <- O + min(vapply(segments, function(s) as.integer(s$from), 0L))
  fc <- prevcarga_read_weather(ctx, "forecast", area, from, NULL)
  fc <- fc[issue_time <= limit]
  pick <- list(freshest = .prevcarga_rule_freshest, per_origin = .prevcarga_rule_per_origin,
               min_lead = .prevcarga_rule_min_lead)
  out <- data.table::rbindlist(lapply(segments, function(s) {
    f <- pick[[s$kind]]
    if (is.null(f)) prevcarga_abort("contract.inference_needs_invalid", where = "prevcarga_read_forecast_by_rule",
                                    details = list(field = "weather_forecast.segments.kind"))
    f(fc, O, s, limit)
  }))
  out <- unique(out)
  out[order(area_code, issue_time, valid_time, method = "radix")]
}

## --- Clima por area (exogenas instantaneas: nao deslocadas) --------------------------------------------
## Com `origin`: observado so <= available_until (derivado da carga lida pelo proprio leitor, mesmo
## origin/truncate_at); previsto so emissoes <= weather_issue_limit (= available_until - lag de publicacao do deck, na regra
## available_until), a mais fresca por (area, valid_time).
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
      lag <- d$manifest$weather_forecast_publication_lag_hours
      limit <- prevcarga_weather_issue_limit(rule, origin, au$available_until, d$tz, lag)
      dt <- prevcarga_freshest_issue(dt, limit)
      extra <- list(weather_issue_cutoff = rule,
                    weather_issue_cutoff_source = if (is.null(src)) "default" else src,
                    weather_issue_limit = limit,
                    weather_publication_lag_hours = prevcarga_weather_effective_lag(rule, lag),
                    freshest_issue_by_area = if (nrow(dt)) dt[, .(issue_time = max(issue_time)), by = area_code][
                      order(area_code, method = "radix")] else unique(dt[, .(area_code, issue_time)]))
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
## `from`/`to` NULL => todos os feriados da area (todas as particoes), como o oraculo le o calendario.
prevcarga_read_holidays <- function(ctx, areas, from = NULL, to = NULL) {
  d <- .prevcarga_deck_ctx(ctx)
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[["holidays"]], "holidays")
  yrs <- if (is.null(from) || is.null(to)) NULL else seq(lubridate::year(from), lubridate::year(to))
  h <- .prevcarga_scan(ds, c("area_code", "date", "holiday_type_id", "prevcarga_code", "simplified_code"),
                       NULL, NULL, areas, years = yrs)
  data.table::set(h, j = "date", value = lubridate::as_date(h$date))
  if (!is.null(from)) h <- h[date >= lubridate::as_date(from)]
  if (!is.null(to)) h <- h[date <= lubridate::as_date(to)]
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

## --- Anexo de feriados (E3-T2; ADR-046) -------------------------------------------------------------------
## Dias de ilha fora da janela comum, para o plugin que declara descriptor$history_window. Sai
## list(islands, load, weather_observed): islands no schema de prevcarga_holiday_islands; load e weather_observed nos
## schemas e atributos dos leitores atuais, SO com dias contidos em alguma ilha e NENHUM dia >= O (FORBIDDEN: corte em O-1).
## Deck train: calcula as ilhas sobre auxiliary/holidays e le carga/observado pelos leitores. Deck inference: le ilhas,
## carga e observado do subdeck apontado por manifest$holiday_annex (diretorio relativo ao deck, kind holiday_annex, com
## load/ e weather/observed/ em hive area_code=/year= e islands/area_code=<a>/part-0.parquet com uma linha por data de
## feriado: island_id, from, to, holiday_date; ADR-044). Janela NULL ou sem holiday_from (modo referencia) => NULL.
.PREVCARGA_ANNEX_LOAD_ATTRS <- c("load_label", "load_label_source", "tz", "available_until",
                                 "available_until_by_area", "available_until_spread", "origin")

.prevcarga_annex_keep_attrs <- function(new, old) {
  for (n in .PREVCARGA_ANNEX_LOAD_ATTRS)
    if (!is.null(attr(old, n))) data.table::setattr(new, n, attr(old, n))
  new
}

.prevcarga_annex_islands_schema <- function(x) {
  x <- data.table::as.data.table(x)
  x[, `:=`(area_code = as.character(area_code), island_id = as.integer(island_id),
           from = lubridate::as_date(from), to = lubridate::as_date(to))]
  data.table::setcolorder(x, c("area_code", "island_id", "from", "to", "holiday_dates"))
  data.table::setindex(x, NULL)
  x[]
}

## Primeiro dia (data civil do fuso da carga) do store por area: so a particao `year` mais antiga da area.
.prevcarga_annex_first_days <- function(d, areas) {
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[["load"]], "load")
  .prevcarga_check_naive(ds$schema, "load", "timestamp")
  scope <- if (length(areas)) areas else sub("^area_code=", "", basename(
    list.dirs(file.path(d$dir, .PREVCARGA_DECK_DATASETS[["load"]]), recursive = FALSE)))
  out <- lapply(scope, function(a) {
    ys <- sub("^year=", "", basename(list.dirs(file.path(d$dir, .PREVCARGA_DECK_DATASETS[["load"]],
                                                         paste0("area_code=", a)), recursive = FALSE)))
    ys <- suppressWarnings(as.integer(ys))
    if (!length(ys) || all(is.na(ys))) return(NULL)
    dt <- .prevcarga_scan(ds, c("area_code", "timestamp"), NULL, NULL, a, years = min(ys, na.rm = TRUE))
    if (!nrow(dt)) return(NULL)
    ts <- .prevcarga_apply_tz(dt$timestamp, d$manifest$timezone$load, d$tz)
    ist <- if (d$label$value == "end") ts - lubridate::minutes(30L) else ts
    list(area_code = a, first_day = lubridate::as_date(min(ist), tz = d$tz))
  })
  out[!vapply(out, is.null, NA)]
}

.prevcarga_annex_day_keep <- function(area, day, islands) {
  if (!nrow(islands) || !length(day)) return(rep(FALSE, length(day)))
  keys <- unlist(lapply(seq_len(nrow(islands)), function(i)
    paste(islands$area_code[[i]], as.integer(seq(islands$from[[i]], islands$to[[i]], by = "day")))))
  paste(as.character(area), as.integer(day)) %in% keys
}

## Filtro final: so dias de ilha e nenhum dia >= O, preservando chave e atributos.
.prevcarga_annex_cut <- function(ax, O) {
  isl <- data.table::copy(ax$islands)
  isl <- isl[from < O]
  if (nrow(isl)) isl[, to := pmin(to, O - 1L)]
  tz <- attr(ax$load, "tz")
  sel <- function(x, col) {
    day <- lubridate::as_date(x[[col]], tz = tz)
    keep <- .prevcarga_annex_day_keep(x$area_code, day, isl) & day < O
    r <- x[keep]
    k <- data.table::key(x)
    if (length(k)) data.table::setkeyv(r, k)
    data.table::setattr(r, "tz", tz)
    .prevcarga_annex_keep_attrs(r, x)
    r[]
  }
  list(islands = .prevcarga_annex_islands_schema(isl), load = sel(ax$load, "interval_start"),
       weather_observed = sel(ax$weather_observed, "valid_time"))
}

.prevcarga_annex_from_train_deck <- function(ctx, O, areas, window, truncate_at = NULL) {
  d <- .prevcarga_deck_ctx(ctx)
  hol <- prevcarga_read_holidays(ctx, areas, window$holiday_from, O - 1L)
  fds <- .prevcarga_annex_first_days(d, areas)
  parts <- lapply(fds, function(f)
    prevcarga_holiday_islands(hol[area_code == f$area_code], O, window$common_from, first_day = f$first_day,
                              holiday_from = window$holiday_from))
  isl <- if (length(parts)) data.table::rbindlist(parts) else .prevcarga_islands_empty()
  isl <- .prevcarga_annex_islands_schema(isl)
  if (nrow(isl)) {
    from <- min(isl$from); to <- max(isl$to)
  } else {
    from <- O - 1L; to <- O - 1L          # sem ilhas: leitura minima so para herdar schema e atributos
  }
  list(islands = isl,
       load = prevcarga_read_load(ctx, areas, from = from, to = to, origin = O, truncate_at = truncate_at),
       weather_observed = prevcarga_read_weather(ctx, "observed", areas, from, to, origin = O,
                                                 truncate_at = truncate_at))
}

## ctx do subdeck do anexo: data_dir = subdeck; o manifest do subdeck deve ser kind holiday_annex (unico contexto em que esse
## kind e aceito; no deck principal o require_deck o recusa); fuso e load_label vem do manifest do deck dono.
.prevcarga_annex_ctx <- function(ctx, man, rel) {
  adir <- file.path(ctx$data_dir, rel)
  if (!dir.exists(adir)) prevcarga_abort("contract.dataset_missing", where = "holiday_annex")
  am <- prevcarga_deck_manifest(adir)
  if (!identical(am$kind, "holiday_annex"))
    prevcarga_abort("contract.deck_kind_mismatch", where = "holiday_annex",
                    details = list(kind = am$kind, expected = "holiday_annex"))
  actx <- ctx
  actx$data_dir <- adir
  actx$annex_owner_manifest <- man
  actx
}

## islands/area_code=<a>/part-0.parquet (hive, uma linha por data de feriado: island_id, from, to, holiday_date).
.prevcarga_annex_read_islands <- function(adir, areas) {
  idir <- file.path(adir, "islands")
  if (!dir.exists(idir)) prevcarga_abort("contract.dataset_missing", where = "holiday_annex.islands")
  fs <- list.files(idir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  parts <- lapply(fs, function(f) {
    a <- sub("^area_code=", "", basename(dirname(f)))
    x <- data.table::as.data.table(as.data.frame(arrow::read_parquet(f)))
    x[, area_code := a]
    x
  })
  raw <- if (length(parts)) data.table::rbindlist(parts) else NULL
  if (is.null(raw) || !nrow(raw)) return(.prevcarga_islands_empty())
  if (length(areas)) raw <- raw[area_code %in% areas]
  if (!nrow(raw)) return(.prevcarga_islands_empty())
  data.table::set(raw, j = "holiday_date", value = lubridate::as_date(raw$holiday_date))
  isl <- raw[, list(holiday_dates = list(sort(unique(holiday_date)))),
             by = list(area_code, island_id, from = lubridate::as_date(from), to = lubridate::as_date(to))]
  data.table::setorderv(isl, c("area_code", "from"))
  isl
}

.prevcarga_annex_from_manifest <- function(ctx, man, O, areas) {
  rel <- man$holiday_annex
  if (is.null(rel)) prevcarga_abort("contract.constraint_violated", where = "holiday_annex")
  actx <- .prevcarga_annex_ctx(ctx, man, rel)
  isl <- .prevcarga_annex_islands_schema(.prevcarga_annex_read_islands(actx$data_dir, areas))
  ld <- prevcarga_read_load(actx, areas)
  obs <- prevcarga_read_weather(actx, "observed", areas)
  au <- man$inference$available_until
  for (x in list(ld, obs)) {
    data.table::setattr(x, "available_until", au)
    data.table::setattr(x, "origin", O)
  }
  list(islands = isl, load = ld, weather_observed = obs)
}

prevcarga_read_holiday_annex <- function(ctx, origin, areas, window, truncate_at = NULL) {
  if (is.null(window) || is.null(window$holiday_from)) return(NULL)
  O <- lubridate::as_date(origin)
  man <- prevcarga_deck_manifest(ctx$data_dir)
  ax <- if (identical(man$kind, "inference")) .prevcarga_annex_from_manifest(ctx, man, O, areas)
        else .prevcarga_annex_from_train_deck(ctx, O, areas, window, truncate_at)
  .prevcarga_annex_cut(ax, O)
}

## --- Painel do leitor (M2.7) ---------------------------------------------------------------------------------
## Agrega os atributos dos leitores sem recalcular; aborta se divergirem em tz ou available_until.
.PREVCARGA_PANEL_LOAD_ATTRS <- c("available_until", "available_until_by_area", "load_label", "load_label_source", "tz")
.PREVCARGA_PANEL_FC_ATTRS <- c("weather_issue_cutoff", "weather_issue_limit", "weather_publication_lag_hours")

pc_panel_attrs <- function(load, weather_obs, weather_fc, origin = NULL) {
  for (x in list(weather_obs, weather_fc)) {
    if (!identical(attr(x, "tz"), attr(load, "tz")))
      prevcarga_abort("contract.constraint_violated", where = "panel.tz")
    if (!identical(attr(x, "available_until"), attr(load, "available_until")))
      prevcarga_abort("contract.constraint_violated", where = "panel.available_until")
  }
  base <- c(stats::setNames(lapply(.PREVCARGA_PANEL_LOAD_ATTRS, function(n) attr(load, n)), .PREVCARGA_PANEL_LOAD_ATTRS),
            stats::setNames(lapply(.PREVCARGA_PANEL_FC_ATTRS, function(n) attr(weather_fc, n)), .PREVCARGA_PANEL_FC_ATTRS))
  if (is.null(origin)) return(base)
  c(base, .prevcarga_weather_forecast_cover(weather_fc, lubridate::as_date(origin), attr(load, "tz")))
}

## Cobertura do clima previsto da emissao vigente por area (F3.7), pelo predicado unico do validate-deck
## (prevcarga_forecast_coverage_end, validation.R): dia coberto = os 48 instantes de 30 min, sem tolerancia para o 23:30.
##   weather_forecast_until : data.table(area_code, until Date | NA)  ultimo dia coberto em sequencia a partir de O; NA = O incompleto
##   weather_forecast_next  : data.table(area_code, n_next int)       instantes consecutivos de until+1 a partir de 00:00
##                            (o membro com model_time_offset_min > 0 precisa do(s) 1o(s) instante(s) do dia seguinte)
## Toda conta de data fica aqui, no leitor: o despachante so le estas tabelas.
.prevcarga_weather_forecast_cover <- function(fc, origin, tz) {
  ce <- prevcarga_forecast_coverage_end(fc, origin, tz)
  areas <- sort(unique(as.character(fc$area_code)), method = "radix")
  until <- lubridate::as_date(ce$coverage_end[match(areas, ce$area_code)])
  nx <- vapply(seq_along(areas), function(i) {
    if (is.na(until[[i]])) return(0L)
    start <- lubridate::force_tz(lubridate::as_datetime(until[[i]] + 1L), tz)
    have <- as.numeric(fc$valid_time[fc$area_code == areas[[i]]])
    ok <- (as.numeric(start) + 1800 * (seq_len(prevcarga_slots_per_day) - 1L)) %in% have
    if (all(ok)) length(ok) else as.integer(which(!ok)[[1L]] - 1L)
  }, 0L)
  list(weather_forecast_until = data.table::data.table(area_code = areas, until = until),
       weather_forecast_next = data.table::data.table(area_code = areas, n_next = nx))
}

## Origem ja resolvida pelo chamador (o leitor nao depende de dates.R). Topologia: ctx$topology_path ou
## <ctx$root>/config/topology.yaml; area fora dela => topology.area_undeclared antes de qualquer leitura.
## `truncate_at` (opcional) vai IGUAL aos tres leitores: carga, observado e previsto derivam o mesmo available_until.
prevcarga_read_panel <- function(ctx, origin, areas, horizon_days, history_days, truncate_at = NULL,
                                 holiday_window = NULL) {
  O <- lubridate::as_date(origin)
  tp <- if (!is.null(ctx$topology_path)) ctx$topology_path else prevcarga_topology_path(ctx$root)
  undeclared <- setdiff(areas, prevcarga_topology(path = tp)$leaves)
  if (length(undeclared)) prevcarga_abort("topology.area_undeclared", where = undeclared[[1L]])
  load <- prevcarga_read_load(ctx, areas, from = O - history_days, to = O, origin = O, truncate_at = truncate_at)
  obs <- prevcarga_read_weather(ctx, "observed", areas, O - history_days, O, origin = O, truncate_at = truncate_at)
  fc <- prevcarga_read_weather(ctx, "forecast", areas, O, O + horizon_days, origin = O, truncate_at = truncate_at)
  hol <- prevcarga_read_holidays(ctx, areas)                    # calendario: todos os feriados da area, sem janela
  out <- list(load = load, weather_observed = obs, weather_forecast = fc, holidays = hol)
  ## Anexo de feriados (E3-T2): so com holiday_window e anexo nao NULL; sem ele o painel e identical() ao anterior.
  ax <- prevcarga_read_holiday_annex(ctx, O, areas, holiday_window, truncate_at = truncate_at)
  if (!is.null(ax)) out$holiday_annex <- ax
  c(out, list(attrs = c(list(origin = O), pc_panel_attrs(load, obs, fc, origin = O))))
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
