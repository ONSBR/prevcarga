## deck_inference.R -- gerador do deck de inferencia minimo e deterministico (E2-T4; ADR-044, ADR-046).
##
## Recorta o deck de TREINO (ctx$data_dir) num deck kind `inference` para a origem O, guiado pelas necessidades gravadas nos
## artefatos (inference_needs, E2-T1) ou, na falta delas, pela topologia. As linhas sao copiadas CRUAS do store (colunas, tipos e
## timestamp[ns] sem fuso intactos; particao `year` da fonte preservada), nas janelas agregadas, em ordem radix. Escrita em
## <out>.partial-<hash> irmao + um unico file.rename; manifest.json por ultimo, chaves em ordem fixa, sem relogio, host nem caminho
## absoluto. Anexo de feriados (decisao do Gabriel, ADR-044 atualizada; discovery 01M7FGQ78H0ZQZPPYT652RGRFW): SUBDECK em
## <out>/holiday_annex com manifest.json proprio (kind holiday_annex), load/ e weather/observed/ em hive e islands/ em hive, o
## layout que prevcarga_read_holiday_annex (E3-T2) le; o manifest do deck aponta o caminho relativo em inference.holiday_annex.
## Nenhum nome de modelo, nenhum literal de janela, nenhuma variavel de ambiente. Requer errors.R, topology.R, provenance.R,
## deck_reader.R, dates.R, registry.R, inference.R, inference_needs.R e holiday_islands.R carregados antes.

.PREVCARGA_DI_ANNEX_DIR <- "holiday_annex"

## --- --out: ausente ou vazio (antes de qualquer leitura) ----------------------------------------------------------------
prevcarga_deck_inference_check_out <- function(out) {
  if (!(is.character(out) && length(out) == 1L && !is.na(out) && nzchar(out)))
    prevcarga_abort("config.usage", where = "--out")
  if (file.exists(out) && (!dir.exists(out) || length(list.files(out, all.files = TRUE, no.. = TRUE))))
    prevcarga_abort("contract.deck_out_not_empty", where = out)
  invisible(out)
}

## --- Leitura CRUA de um dataset do deck de treino ------------------------------------------------------------------------
## Devolve as colunas do arquivo (ordem do schema) + area_code + year (particoes da fonte) e os tipos arrow do arquivo.
.prevcarga_di_raw <- function(d, dataset, areas, from = NULL, to = NULL, years = NULL, rows = TRUE) {
  ds <- .prevcarga_open(d$dir, .PREVCARGA_DECK_DATASETS[[dataset]], dataset)
  cols <- setdiff(ds$schema$names, c("area_code", "year"))
  types <- stats::setNames(lapply(cols, function(n) ds$schema$GetFieldByName(n)$type), cols)
  for (n in cols)
    if (startsWith(types[[n]]$ToString(), "timestamp")) .prevcarga_check_naive(ds$schema, dataset, n)
  dt <- if (rows) .prevcarga_scan(ds, c(cols, "area_code", "year"), from, to, areas, years = years) else NULL
  list(dt = dt, types = types, cols = cols)
}

.prevcarga_di_secs <- function(x) as.numeric(x)

## --- Recorte (passo 3) ---------------------------------------------------------------------------------------------------
.prevcarga_di_win <- function(a, ds, from, to)
  list(area = a, dataset = ds, from = format(from, "%Y-%m-%d"), to = format(to, "%Y-%m-%d"))

.prevcarga_di_no_load <- function(a, from, to)
  prevcarga_abort("completude.no_load", where = "load",
                  details = list(area = a, from = format(from, "%Y-%m-%d"), to = format(to, "%Y-%m-%d")))

## carga e observado: [O - days_before, O] limitados a available_until (mesmo predicado do leitor).
.prevcarga_di_slice_series <- function(d, O, rc, ag_tab, dataset) {
  man <- d$manifest
  parts <- list(); wins <- list(); types <- NULL
  for (i in seq_len(nrow(ag_tab))) {
    a <- ag_tab$area_code[[i]]
    from <- O - ag_tab$days_before[[i]]
    r <- .prevcarga_di_raw(d, dataset, a, from, O)
    types <- r$types
    x <- r$dt
    if (nrow(x)) {
      ts <- .prevcarga_apply_tz(x$timestamp, if (dataset == "load") man$timezone$load else man$timezone$weather_observed, d$tz)
      iv <- if (dataset == "load") .prevcarga_load_interval(ts, d$label$value) else list(start = ts, end = ts)
      when <- iv$start; end <- iv$end
      keep <- .prevcarga_in_window(when, .prevcarga_window(from, O, d$tz)) & end <= rc$available_until
      x <- x[keep, ]
    }
    if (dataset == "load" && !nrow(x)) .prevcarga_di_no_load(a, from, O)
    if (nrow(x)) parts[[length(parts) + 1L]] <- x
    wins[[length(wins) + 1L]] <- .prevcarga_di_win(a, dataset, from, O)
  }
  list(dt = if (length(parts)) data.table::rbindlist(parts) else NULL, types = types, windows = wins,
       keys = c("area_code", "timestamp"))
}

## previsto: so as emissoes escolhidas por prevcarga_read_forecast_by_rule com o weather_issue_limit da origem.
.prevcarga_di_slice_forecast <- function(ctx, d, O, rc, wf) {
  man <- d$manifest
  parts <- list(); wins <- list(); types <- NULL
  for (a in sort(unique(wf$area_code), method = "radix")) {
    s <- wf[wf$area_code == a, ]
    segs <- lapply(seq_len(nrow(s)), function(i) list(kind = s$kind[[i]], from = s$from[[i]], to = s$to[[i]], lead = s$lead[[i]]))
    keep <- prevcarga_read_forecast_by_rule(ctx, a, O, segs, rc$weather_issue_limit)
    from <- O + min(s$from)
    r <- .prevcarga_di_raw(d, "weather_forecast", a, from, NULL)
    types <- r$types
    x <- r$dt
    if (nrow(x) && nrow(keep)) {
      k_raw <- paste(.prevcarga_di_secs(.prevcarga_apply_tz(x$issue_time, man$timezone$weather_forecast$issue_time, d$tz)),
                     .prevcarga_di_secs(.prevcarga_apply_tz(x$valid_time, man$timezone$weather_forecast$valid_time, d$tz)))
      k_keep <- paste(.prevcarga_di_secs(keep$issue_time), .prevcarga_di_secs(keep$valid_time))
      x <- x[k_raw %in% k_keep, ]
    } else x <- x[0L, ]
    if (nrow(x)) parts[[length(parts) + 1L]] <- x
    wins[[length(wins) + 1L]] <- .prevcarga_di_win(a, "weather_forecast", from, O + max(s$to))
  }
  list(dt = if (length(parts)) data.table::rbindlist(parts) else NULL, types = types, windows = wins,
       keys = c("area_code", "issue_time", "valid_time"))
}

## feriados: [O + from, O + to] por area (data civil, sem corte de available_until, como o leitor).
.prevcarga_di_slice_holidays <- function(d, O, ho) {
  parts <- list(); wins <- list(); types <- NULL
  for (i in seq_len(nrow(ho))) {
    a <- ho$area_code[[i]]
    from <- O + ho$from[[i]]; to <- O + ho$to[[i]]
    r <- .prevcarga_di_raw(d, "holidays", a, NULL, NULL, years = seq(lubridate::year(from), lubridate::year(to)))
    types <- r$types
    x <- r$dt
    if (nrow(x)) {
      dd <- lubridate::as_date(x$date)
      x <- x[dd >= from & dd <= to, ]
    }
    if (nrow(x)) parts[[length(parts) + 1L]] <- x
    wins[[length(wins) + 1L]] <- .prevcarga_di_win(a, "holidays", from, to)
  }
  list(dt = if (length(parts)) data.table::rbindlist(parts) else NULL, types = types, windows = wins,
       keys = c("area_code", "date"))
}

.prevcarga_di_slice <- function(ctx, O, rc, ag) {
  d <- .prevcarga_deck_ctx(ctx)
  out <- list(load = .prevcarga_di_slice_series(d, O, rc, ag$load, "load"),
              weather_observed = .prevcarga_di_slice_series(d, O, rc, ag$weather_observed, "weather_observed"))
  if (nrow(ag$weather_forecast)) out$weather_forecast <- .prevcarga_di_slice_forecast(ctx, d, O, rc, ag$weather_forecast)
  out$holidays <- .prevcarga_di_slice_holidays(d, O, ag$holidays)
  out
}

## --- Anexo de feriados (passo 4/5 conforme a decisao do Gabriel; ADR-044) ----------------------------------------------------
## Quando ag$holiday_annex e TRUE: ilhas pelo construtor do E3 (prevcarga_read_holiday_annex sobre o deck de treino, corte em O-1) e
## as linhas CRUAS de carga e observado dos dias de ilha (nenhum dia >= O). Sem need de anexo devolve NULL.
.prevcarga_di_annex <- function(ctx, O, areas, ag) {
  if (!isTRUE(ag$holiday_annex)) return(NULL)
  d <- .prevcarga_deck_ctx(ctx)
  ax <- prevcarga_read_holiday_annex(ctx, O, areas, prevcarga_dessem_window(O, "ano_civil"))
  isl <- ax$islands
  rows <- if (nrow(isl)) data.table::rbindlist(lapply(seq_len(nrow(isl)), function(i)
    data.table::data.table(area_code = isl$area_code[[i]], island_id = isl$island_id[[i]], from = isl$from[[i]],
                           to = isl$to[[i]], holiday_date = isl$holiday_dates[[i]]))) else NULL
  if (!is.null(rows)) {
    rows[, island_id := as.integer(island_id)]
    data.table::setorderv(rows, c("area_code", "island_id", "holiday_date"))
  }
  man <- d$manifest
  series <- function(dataset) {
    parts <- list(); types <- NULL
    for (a in sort(unique(isl$area_code), method = "radix")) {
      ia <- isl[isl$area_code == a, ]
      r <- .prevcarga_di_raw(d, dataset, a, min(ia$from), max(ia$to))
      types <- r$types
      x <- r$dt
      if (nrow(x)) {
        ts <- .prevcarga_apply_tz(x$timestamp, if (dataset == "load") man$timezone$load else man$timezone$weather_observed, d$tz)
        when <- if (dataset == "load") .prevcarga_load_interval(ts, d$label$value)$start else ts
        day <- lubridate::as_date(when, tz = d$tz)
        x <- x[.prevcarga_annex_day_keep(rep(a, length(day)), day, ia) & day < O, ]
      }
      if (nrow(x)) parts[[length(parts) + 1L]] <- x
    }
    if (length(parts))
      return(list(dt = data.table::rbindlist(parts), types = types, keys = c("area_code", "timestamp")))
    ## sem linhas (zero ilhas ou ilhas sem dado): tabela vazia com os tipos do store e uma particao so para o schema
    r <- .prevcarga_di_raw(d, dataset, character(), years = -1L)
    list(dt = r$dt, types = r$types, keys = c("area_code", "timestamp"),
         empty_part = list(area = areas[[1L]], year = lubridate::year(O - 1L)))
  }
  list(islands = list(dt = rows, types = NULL, keys = c("area_code", "island_id", "holiday_date")),
       load = series("load"), weather_observed = series("weather_observed"))
}

## --- Escrita ----------------------------------------------------------------------------------------------------------------
## Um parquet por (area, year) em <root>/<rel>/area_code=<a>/[year=<y>/]part-0.parquet; linhas em ordem radix das chaves;
## area_code e year so na particao. snappy, version 2.6, sem dicionario (determinismo pinado ao arrow da imagem do bundle).
.prevcarga_di_write_part <- function(x, file, types) {
  cols <- setdiff(names(x), c("area_code", "year"))
  arrs <- lapply(cols, function(n) {
    v <- x[[n]]
    if (inherits(v, "POSIXct")) v <- lubridate::with_tz(v, "UTC")      # epoch = digitos do store; o tipo e naive
    if (is.null(types)) arrow::Array$create(v) else arrow::Array$create(v, type = types[[n]])
  })
  names(arrs) <- cols
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(do.call(arrow::arrow_table, arrs), file, compression = "snappy", version = "2.6", use_dictionary = FALSE)
}

.prevcarga_di_write_table <- function(root, rel, tab) {
  dir.create(file.path(root, rel), recursive = TRUE, showWarnings = FALSE)
  x <- tab$dt
  files <- list()
  ## tabela vazia com `empty_part` (anexo sem nenhuma linha): um parquet de 0 linhas guarda o schema, senao o leitor nao abre o dataset
  if (!is.null(x) && !nrow(x) && !is.null(tab$empty_part)) {
    p <- paste(c(rel, paste0("area_code=", tab$empty_part$area), paste0("year=", tab$empty_part$year), "part-0.parquet"), collapse = "/")
    .prevcarga_di_write_part(x, file.path(root, p), tab$types)
    return(list(list(path = p, rows = 0L)))
  }
  if (is.null(x) || !nrow(x)) return(files)
  x <- data.table::copy(x)
  data.table::setorderv(x, tab$keys, order = 1L, na.last = TRUE)
  by_year <- "year" %in% names(x)
  grp <- if (by_year) paste(x$area_code, x$year, sep = "\r") else x$area_code
  for (g in unique(grp)) {
    s <- x[grp == g, ]
    p <- paste(c(rel, paste0("area_code=", s$area_code[[1L]]), if (by_year) paste0("year=", s$year[[1L]]), "part-0.parquet"),
               collapse = "/")
    .prevcarga_di_write_part(s, file.path(root, p), tab$types)
    files[[length(files) + 1L]] <- list(path = gsub("\\\\", "/", p), rows = nrow(s))
  }
  files
}

.prevcarga_di_write_annex <- function(root, annex) {
  dir.create(file.path(root, .PREVCARGA_DI_ANNEX_DIR), recursive = TRUE, showWarnings = FALSE)
  .prevcarga_di_write_manifest(file.path(root, .PREVCARGA_DI_ANNEX_DIR), list(schema_version = 1L, kind = "holiday_annex"))
  rel <- function(...) file.path(.PREVCARGA_DI_ANNEX_DIR, ...)
  c(.prevcarga_di_write_table(root, rel("load"), annex$load),
    .prevcarga_di_write_table(root, rel("weather", "observed"), annex$weather_observed),
    .prevcarga_di_write_table(root, rel("islands"), annex$islands))
}

.prevcarga_di_json <- function(x)
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"))

## gravacao do manifest.json (por ultimo); o leitor segue sendo o unico a le-lo
.prevcarga_di_write_manifest <- function(dir, x) writeLines(.prevcarga_di_json(x), file.path(dir, "manifest.json"))

## manifest.json: chaves em ordem fixa; fuso, load_label e atraso do clima copiados do deck de treino.
.prevcarga_di_manifest <- function(ctx, O, rc, ag, rows, train_hash, windows, annex) {
  d <- .prevcarga_deck_ctx(ctx)
  tzm <- d$manifest$timezone
  m <- list(schema_version = 1L, kind = "inference", timezone = tzm, load_label = d$label$value)
  if (!is.null(d$manifest$weather_forecast_publication_lag_hours))
    m$weather_forecast_publication_lag_hours <- as.integer(d$manifest$weather_forecast_publication_lag_hours)
  src <- unique(as.character(rows$source))
  src <- if (length(src) == 1L) src else "mixed"
  inf <- list(origin = format(O, "%Y-%m-%d"),
              available_until = format(lubridate::with_tz(rc$available_until, d$tz), "%Y-%m-%dT%H:%M:%S"),
              train_deck_hash = train_hash, windows = windows)
  if (!is.null(annex)) inf$holiday_annex <- .PREVCARGA_DI_ANNEX_DIR
  inf$inference_needs_source <- src
  m$inference <- inf
  list(manifest = m, source = src)
}

## windows do manifest: dataset -> {from, to} (uniao das areas).
.prevcarga_di_manifest_windows <- function(wins) {
  out <- list()
  for (ds in c("load", "weather_observed", "weather_forecast", "holidays")) {
    w <- Filter(function(x) identical(x$dataset, ds), wins)
    if (!length(w)) next
    out[[ds]] <- list(from = min(vapply(w, function(x) x$from, "")), to = max(vapply(w, function(x) x$to, "")))
  }
  out
}

.prevcarga_di_write <- function(tables, annex, out, manifest, partial) {
  files <- list()
  for (ds in names(tables))
    files <- c(files, .prevcarga_di_write_table(partial, .PREVCARGA_DECK_DATASETS[[ds]], tables[[ds]]))
  if (!is.null(annex)) files <- c(files, .prevcarga_di_write_annex(partial, annex))
  .prevcarga_di_write_manifest(partial, manifest)
  files
}

## --- Orquestracao -------------------------------------------------------------------------------------------------------------
prevcarga_deck_inference_build <- function(ctx, origin, areas, out, from_topology = FALSE) {
  prevcarga_deck_inference_check_out(out)
  O <- lubridate::as_date(origin)
  areas <- sort(unique(as.character(areas)), method = "radix")
  rc <- prevcarga_resolve_cuts(ctx, origin = O)
  d <- .prevcarga_deck_ctx(ctx)
  train_hash <- prevcarga_data_hash(ctx$data_dir, d$manifest, d$label$value)

  warns <- list()
  rows <- withCallingHandlers(
    prevcarga_inference_needs_resolve(ctx, O, areas, source = if (isTRUE(from_topology)) "topology" else "artifacts",
                                      on_missing = "topology"),
    prevcarga_warning = function(w) {
      warns[[length(warns) + 1L]] <<- list(id = w$id, where = w$where, details = w$details)
      invokeRestart("muffleWarning")
    })
  ag <- prevcarga_inference_needs_aggregate(rows)

  tables <- .prevcarga_di_slice(ctx, O, rc, ag)
  annex <- .prevcarga_di_annex(ctx, O, areas, ag)
  wins <- unlist(lapply(tables, function(t) t$windows), recursive = FALSE, use.names = FALSE)
  mf <- .prevcarga_di_manifest(ctx, O, rc, ag, rows, train_hash, .prevcarga_di_manifest_windows(wins), annex)

  tag <- substr(.prevcarga_sha256(paste(format(O), train_hash, jsonlite::toJSON(ag, digits = NA, dataframe = "rows", auto_unbox = TRUE),
                                        sep = "\n")), 1L, 16L)
  partial <- paste0(sub("[/\\\\]+$", "", out), ".partial-", tag)
  unlink(partial, recursive = TRUE)
  done <- FALSE
  on.exit(if (!done) unlink(partial, recursive = TRUE), add = TRUE)
  oops <- function(why) prevcarga_abort("io.out_unwritable", where = out, details = list(reason = why))
  if (!dir.exists(dirname(partial)) && !dir.create(dirname(partial), recursive = TRUE, showWarnings = FALSE)) oops("diretorio pai")
  if (!dir.create(partial, recursive = TRUE, showWarnings = FALSE)) oops("diretorio parcial")
  files <- tryCatch(.prevcarga_di_write(tables, annex, out, mf$manifest, partial),
                    prevcarga_error = function(e) stop(e),
                    error = function(e) oops(conditionMessage(e)))

  ## todos os arquivos do deck (manifest incluso), ordem radix; `rows` so nos parquet
  rows_of <- stats::setNames(vapply(files, function(f) as.integer(f$rows), 0L), vapply(files, function(f) f$path, ""))
  all_paths <- sort(list.files(partial, recursive = TRUE, all.files = TRUE), method = "radix")
  files <- lapply(all_paths, function(p) c(list(path = p, sha256 = .prevcarga_sha256_file(file.path(partial, p))),
                                            if (p %in% names(rows_of)) list(rows = unname(rows_of[[p]]))))
  data_hash <- prevcarga_data_hash(partial, d$manifest, d$label$value)

  if (dir.exists(out)) unlink(out, recursive = TRUE)
  if (!isTRUE(file.rename(partial, out))) oops("rename")
  done <- TRUE

  win_by <- list()
  for (w in wins) win_by[[w$dataset]][[w$area]] <- list(from = w$from, to = w$to)
  list(result = list(schema_version = 1L, out = out, origin = format(O, "%Y-%m-%d"),
                     available_until = mf$manifest$inference$available_until, train_deck_hash = train_hash,
                     data_hash = data_hash, inference_needs_source = mf$source, windows = win_by, files = files,
                     needs_from_topology = lapply(warns, function(w) list(member = w$details$member, area = w$details$area))),
       warnings = warns)
}
