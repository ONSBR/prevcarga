## inference_needs.R -- necessidade de dados por membro como DADO (schema_version 1): o que o deck de inferencia precisa
## recortar, declarado pelo plugin (descriptor$data_needs) ou derivado por um default GENERICO so por campos.
## Nenhuma data absoluta (offsets inteiros relativos a O), nenhum nome de modelo, nenhuma variavel de ambiente, nenhum relogio.
## Requer errors.R; `resolve` requer topology.R, registry.R e inference.R.

.prevcarga_in_keys <- c("schema_version", "load", "weather_observed", "weather_forecast", "holidays", "holiday_annex")
.prevcarga_in_kinds <- c("freshest", "per_origin", "min_lead")

prevcarga_inference_needs_normalize <- function(x, where) {
  bad <- function(field) prevcarga_abort("contract.inference_needs_invalid", where = where,
                                         details = list(field = field))
  int1 <- function(v, f, min = -Inf) {
    if (!(is.numeric(v) && length(v) == 1L && !is.na(v) && is.finite(v) && v == round(v) && v >= min)) bad(f)
    as.integer(v)
  }
  closed <- function(l, keys, f) {
    if (!is.list(l) || (length(l) && is.null(names(l)))) bad(f)
    extra <- setdiff(names(l), keys)
    if (length(extra)) bad(if (identical(f, "inference_needs")) extra[[1L]] else paste0(f, ".", extra[[1L]]))
  }
  closed(x, .prevcarga_in_keys, "inference_needs")
  if (!is.null(x$schema_version) && !identical(as.integer(x$schema_version), 1L)) bad("schema_version")
  closed(x$load, "days_before", "load")
  closed(x$weather_observed, "days_before", "weather_observed")
  load <- list(days_before = int1(x$load$days_before, "load.days_before", 0))
  wobs <- list(days_before = int1(x$weather_observed$days_before, "weather_observed.days_before", 0))
  wf <- x$weather_forecast
  closed(wf, c("area", "segments"), "weather_forecast")
  if (!(is.character(wf$area) && length(wf$area) == 1L && !is.na(wf$area) && nzchar(wf$area)))
    bad("weather_forecast.area")
  sg <- wf$segments
  if (is.null(sg)) sg <- list()
  if (!is.list(sg)) bad("weather_forecast.segments")
  segs <- lapply(sg, function(s) {
    closed(s, c("kind", "from", "to", "lead"), "weather_forecast.segments")
    if (!(is.character(s$kind) && length(s$kind) == 1L && !is.na(s$kind) && s$kind %in% .prevcarga_in_kinds))
      bad("weather_forecast.segments.kind")
    from <- int1(s$from, "weather_forecast.segments.from"); to <- int1(s$to, "weather_forecast.segments.to")
    if (from > to) bad("weather_forecast.segments.from")
    lead <- NA_integer_
    if (identical(s$kind, "min_lead")) {
      if (is.null(s$lead) || is.na(s$lead)) bad("weather_forecast.segments.lead")
      lead <- int1(s$lead, "weather_forecast.segments.lead")
    } else if (!is.null(s$lead) && !is.na(s$lead)) bad("weather_forecast.segments.lead")
    list(kind = s$kind, from = from, to = to, lead = lead)
  })
  if (length(segs)) {
    key <- data.frame(kind = vapply(segs, `[[`, "", "kind"), from = vapply(segs, `[[`, 0L, "from"),
                      to = vapply(segs, `[[`, 0L, "to"), lead = vapply(segs, `[[`, 0L, "lead"),
                      stringsAsFactors = FALSE)
    key <- key[!duplicated(key), , drop = FALSE]
    segs <- segs[as.integer(rownames(key))][order(key$kind, key$from, key$to, key$lead, method = "radix")]
  }
  closed(x$holidays, c("from", "to"), "holidays")
  hf <- int1(x$holidays$from, "holidays.from"); ht <- int1(x$holidays$to, "holidays.to")
  if (hf > ht) bad("holidays.from")
  ann <- x$holiday_annex
  if (!(is.logical(ann) && length(ann) == 1L && !is.na(ann))) bad("holiday_annex")
  list(schema_version = 1L, load = load, weather_observed = wobs,
       weather_forecast = list(area = wf$area, segments = segs),
       holidays = list(from = hf, to = ht), holiday_annex = ann)
}

## Default GENERICO so por campos: mesmo predicado de .prevcarga_inf_weather_dependent (requires data.weather_forecast.*).
prevcarga_inference_needs_default <- function(descriptor, spec, area) {
  n <- as.integer(spec$history_days$infer); H <- as.integer(spec$horizon_days)
  fc <- any(vapply(descriptor$stages, function(s)
    any(startsWith(as.character(s$requires), "data.weather_forecast.")), NA))
  list(schema_version = 1L, load = list(days_before = n), weather_observed = list(days_before = n),
       weather_forecast = list(area = area,
                               segments = if (fc) list(list(kind = "freshest", from = 0L, to = H)) else list()),
       holidays = list(from = -n, to = H), holiday_annex = FALSE)
}

prevcarga_inference_needs_of <- function(plugin, spec, area) {
  d <- plugin$descriptor
  raw <- if (is.function(d$data_needs)) d$data_needs(spec, area)$infer else prevcarga_inference_needs_default(d, spec, area)
  prevcarga_inference_needs_normalize(raw, where = d$model_id)
}

## Uniao por dataset e area (independe da ordem das linhas). load/weather_observed/holidays por area da carga;
## weather_forecast por area de clima. Base R + data.table::data.table (sem NSE: nao depende de data.table anexado).
prevcarga_inference_needs_aggregate <- function(rows) {
  n <- rows$needs
  areas <- as.character(rows$area)
  ua <- sort(unique(areas), method = "radix")
  per_area <- function(f, v) vapply(ua, function(a) f(v[areas == a]), 0L, USE.NAMES = FALSE)
  days <- function(ds) data.table::data.table(
    area_code = ua, days_before = per_area(max, vapply(n, function(x) x[[ds]]$days_before, 0L)))
  seg <- list(area_code = character(), kind = character(), from = integer(), to = integer(), lead = integer())
  for (x in n) for (z in x$weather_forecast$segments) {
    seg$area_code <- c(seg$area_code, x$weather_forecast$area); seg$kind <- c(seg$kind, z$kind)
    seg$from <- c(seg$from, z$from); seg$to <- c(seg$to, z$to); seg$lead <- c(seg$lead, z$lead)
  }
  seg <- data.table::as.data.table(seg)
  seg <- seg[!duplicated(seg), ]
  seg <- seg[order(seg$area_code, seg$kind, seg$from, seg$to, seg$lead, method = "radix"), ]
  hf <- vapply(n, function(x) x$holidays$from, 0L); ht <- vapply(n, function(x) x$holidays$to, 0L)
  list(load = days("load"), weather_observed = days("weather_observed"), weather_forecast = seg,
       holidays = data.table::data.table(area_code = ua, from = per_area(min, hf), to = per_area(max, ht)),
       holiday_annex = any(vapply(n, function(x) isTRUE(x$holiday_annex), NA)))
}

## ctx: root, e (source = artifacts) o registro de artefatos (artifacts_dir/state_dir). `areas`: folhas da topologia.
## Uma linha por membro x area, ordem (area, member) radix.
prevcarga_inference_needs_resolve <- function(ctx, origin, areas, source = c("artifacts", "topology")) {
  source <- match.arg(source)
  members <- if (is.null(ctx$topology_path)) prevcarga_members(root = ctx$root) else
    prevcarga_members(path = ctx$topology_path, root = ctx$root)
  rows <- list()
  for (i in seq_len(nrow(members))) {
    m <- as.list(members[i, , drop = FALSE])
    label <- if (is.na(m$exogena)) m$mdir else paste(m$mdir, m$exogena, sep = "/")
    plugin <- if (source == "topology") prevcarga_plugin_load(m$mdir, prevcarga_models_root(ctx$root)) else NULL
    for (a in areas) {
      needs <- if (source == "artifacts") {
        art <- prevcarga_infer_resolve(m, a, ctx)$artifact
        if (is.null(art$inference_needs))
          prevcarga_abort("contract.inference_needs_missing", where = m$mdir,
                          details = list(member = label, area = a, origin = format(origin, "%Y-%m-%d")))
        art$inference_needs
      } else prevcarga_inference_needs_of(plugin, prevcarga_model_spec(m$mdir, ctx$root), a)
      rows[[length(rows) + 1L]] <- list(member = label, mdir = m$mdir, area = a, needs = needs)
    }
  }
  out <- data.table::data.table(member = vapply(rows, `[[`, "", "member"), mdir = vapply(rows, `[[`, "", "mdir"),
                                area = vapply(rows, `[[`, "", "area"), needs = lapply(rows, `[[`, "needs"),
                                source = rep(source, length(rows)))
  out[order(out$area, out$member, method = "radix"), ]
}
