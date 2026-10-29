## helper-deck.R -- deck sentinela compartilhado (M2.1; consumido por M2.2..M2.V, M3..M7, Reconcile e plugins).
## Nao e codigo de produto. Auto-carregado por testthat::test_dir. Nunca toca data/: tudo em tempdir.
##
## Deck sentinela: carga com valor = slot do dia (1..48) + 1000 * indice da area, gravada como
## timestamp[ns] SEM fuso (espelha o store real). Modo `end`: os digitos sao o FIM do intervalo (+30 min);
## modo `start`: o inicio. O mesmo deck nos dois modos tem interval_start/interval_end identicos apos a leitura.
## Clima: observado horario e previsto com uma emissao por hora UTC em `issues` por dia; digitos de
## issue_time em UTC, digitos de valid_time/timestamp no fuso da carga.
## M2.7: pc_deck_panel (duas areas, clima proprio por area, feriados por area, topologia-fixture).

.pc_deck_tz <- "Etc/GMT+3"

## timestamp[ns] sem fuso a partir de digitos civis (POSIXct em UTC cujos digitos sao os do store)
.pc_naive_ns <- function(x_digits_utc) {
  arrow::Array$create(lubridate::force_tz(x_digits_utc, "UTC"), type = arrow::timestamp("ns"))
}

## Grava particoes Hive area_code=/year= a mao (arrow::write_dataset exige dplyr, que o deck nao usa).
.pc_deck_write <- function(dt, dir, rel, ts_cols, year_col = ts_cols[[1L]]) {
  yr <- as.integer(lubridate::year(dt[[year_col]]))
  grp <- paste(dt$area_code, yr, sep = "\r")
  for (g in unique(grp)) {
    sub <- dt[grp == g]
    cols <- lapply(setdiff(names(sub), "area_code"), function(n) {
      if (n %in% ts_cols) .pc_naive_ns(sub[[n]]) else arrow::Array$create(sub[[n]])
    })
    names(cols) <- setdiff(names(sub), "area_code")
    part <- file.path(dir, rel, paste0("area_code=", sub$area_code[[1L]]),
                      paste0("year=", yr[grp == g][[1L]]))
    dir.create(part, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(do.call(arrow::arrow_table, cols), file.path(part, "part-0.parquet"))
  }
  invisible(NULL)
}

## Tabela do clima previsto (area_code, issue_time, valid_time, temperature, heat_index) com digitos do store.
.pc_deck_forecast <- function(days, areas, issues, tz, issue_tz) {
  em <- data.table::CJ(area_code = areas, day = days, issue_hour = as.integer(issues), sorted = FALSE)
  em[, issue_utc := lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::hours(issue_hour)]
  fc <- em[rep(seq_len(nrow(em)), each = 48L)]
  fc[, lead_h := rep(1:48, times = nrow(em))]
  fc[, valid_local := lubridate::with_tz(issue_utc, tz) + lubridate::hours(lead_h)]
  fc[, `:=`(issue_time = lubridate::force_tz(issue_utc, "UTC"),     # digitos UTC (issue_time declarado UTC)
            valid_time = lubridate::force_tz(valid_local, "UTC"))]  # digitos no fuso da carga
  if (!identical(issue_tz, "UTC")) {                                  # CONTROL: declarar o fuso errado nao muda os digitos
    fc[, issue_time := lubridate::force_tz(lubridate::with_tz(issue_utc, issue_tz), "UTC")]
  }
  fc[, temperature := 15 + match(area_code, areas) + issue_hour / 100 + lead_h / 1000]
  fc[, heat_index := temperature + 1]
  fc[, .(area_code, issue_time, valid_time, temperature, heat_index)]
}

## Manifest padrao do deck sentinela (ADR-0037); `load_label` so e gravado quando "start".
pc_deck_manifest_default <- function(load_label = NULL, issue_tz = "UTC") {
  m <- list(schema_version = 1L)
  if (!is.null(load_label)) m$load_label <- load_label
  m$timezone <- list(load = .pc_deck_tz, weather_observed = .pc_deck_tz,
                     weather_forecast = list(valid_time = .pc_deck_tz, issue_time = issue_tz))
  m
}

## Reescreve o manifest do deck (para mutacoes): parte do padrao e aplica `mutate(lista) -> lista`.
pc_deck_manifest <- function(data_dir, load_label = NULL, issue_tz = "UTC", mutate = NULL) {
  m <- pc_deck_manifest_default(load_label, issue_tz)
  if (is.function(mutate)) m <- mutate(m)
  jsonlite::write_json(m, file.path(data_dir, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(m)
}

pc_deck_default_days <- function() lubridate::ymd(c("2026-09-29", "2026-09-30", "2026-10-01", "2026-10-02"))

## pc_deck_sentinel(load_label, days, areas, until, issues)
##   until : NULL ou POSIXct no fuso da carga; a carga so tem intervalos com interval_end <= until
##           (o clima nao e cortado). Default: carga completa nos `days`.
##   issues: horas UTC das emissoes de previsao (uma por hora em `issues`, por dia).
pc_deck_sentinel <- function(load_label = c("end", "start"), days = pc_deck_default_days(),
                             areas = c("A", "B"), until = NULL, issues = c(0L, 6L, 12L, 18L),
                             tz = .pc_deck_tz, issue_tz = "UTC",
                             dir = withr::local_tempdir(.local_envir = parent.frame())) {
  load_label <- match.arg(load_label)
  days <- lubridate::as_date(days)
  grid <- data.table::CJ(area_code = areas, day = days, slot = 1:48, sorted = FALSE)
  grid[, area_idx := match(area_code, areas)]
  istart <- lubridate::force_tz(lubridate::as_datetime(grid$day), tz) + lubridate::minutes(30L * (grid$slot - 1L))
  iend <- istart + lubridate::minutes(30L)
  if (!is.null(until)) {
    keep <- iend <= until
    grid <- grid[keep]; istart <- istart[keep]; iend <- iend[keep]
  }
  stamp <- if (load_label == "end") iend else istart
  load <- data.table::data.table(
    area_code = grid$area_code,
    timestamp = lubridate::force_tz(lubridate::with_tz(stamp, tz), "UTC"),  # digitos civis do fuso da carga
    load_mwh = as.double(grid$slot + 1000L * grid$area_idx))
  .pc_deck_write(load, dir, "load", "timestamp")

  ## clima observado horario (digitos no fuso da carga)
  hours <- lubridate::force_tz(lubridate::as_datetime(rep(days, each = 24L)), tz) +
    lubridate::hours(rep(0:23, times = length(days)))
  obs <- data.table::CJ(area_code = areas, i = seq_along(hours), sorted = FALSE)
  obs[, `:=`(timestamp = lubridate::force_tz(hours[i], "UTC"), area_idx = match(area_code, areas))]
  obs[, temperature := 15 + area_idx + lubridate::hour(timestamp) / 100]
  obs[, heat_index := temperature + 1]
  .pc_deck_write(obs[, .(area_code, timestamp, temperature, heat_index)], dir,
                 file.path("weather", "observed"), "timestamp")

  ## clima previsto: uma emissao por hora UTC em `issues`; valid_time = emissao + 1..48 h (digitos no fuso da carga)
  fc <- .pc_deck_forecast(days, areas, issues, tz, issue_tz)
  .pc_deck_write(fc, dir, file.path("weather", "forecast"), c("valid_time", "issue_time"))

  pc_deck_manifest(dir, load_label = if (load_label == "start") "start")
  dir
}

## Deck sem defeito (alias de leitura: o sentinela e limpo por construcao).
pc_deck_clean <- function(load_label = c("end", "start"), ...) pc_deck_sentinel(match.arg(load_label), ...)

## ctx minimo sem config.yaml; `source` diz de onde veio o load_label: "flag" | "config" | "default".
pc_ctx_deck <- function(data_dir, load_label = NULL, source = "default") {
  list(data_dir = data_dir,
       load_label_flag = if (identical(source, "flag")) load_label else NULL,
       load_label_config = if (identical(source, "config")) load_label else NULL)
}

## --- Mutadores de corte (M2.2): reescrevem o deck em disco; digitos ingenuos como no sentinela --------------
.pc_deck_dsets <- c(load = "load", weather_observed = file.path("weather", "observed"),
                    weather_forecast = file.path("weather", "forecast"),
                    holidays = file.path("auxiliary", "holidays"))
.pc_deck_ts_cols <- list(load = "timestamp", weather_observed = "timestamp",
                         weather_forecast = c("valid_time", "issue_time"), holidays = character())

## Le todas as particoes do dataset: colunas de tempo viram POSIXct UTC com os digitos do store.
.pc_deck_read <- function(dir, dataset) {
  rel <- .pc_deck_dsets[[dataset]]
  fs <- list.files(file.path(dir, rel), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  parts <- lapply(fs, function(f) {
    x <- data.table::as.data.table(as.data.frame(arrow::read_parquet(f)))
    for (n in intersect(.pc_deck_ts_cols[[dataset]], names(x))) data.table::set(x, j = n, value = lubridate::with_tz(x[[n]], "UTC"))
    x[, area_code := sub(".*area_code=([^/]+)/.*", "\\1", f)]
    x
  })
  data.table::rbindlist(parts, use.names = TRUE)
}

.pc_deck_rewrite <- function(dt, dir, dataset) {
  rel <- .pc_deck_dsets[[dataset]]
  unlink(file.path(dir, rel), recursive = TRUE)
  dir.create(file.path(dir, rel), recursive = TRUE, showWarnings = FALSE)
  if (nrow(dt)) {
    ts <- .pc_deck_ts_cols[[dataset]]
    data.table::setcolorder(dt, c("area_code", setdiff(names(dt), "area_code")))
    .pc_deck_write(dt, dir, rel, intersect(ts, names(dt)), if (dataset == "holidays") "date" else ts[[1L]])
  }
  invisible(dir)
}

.pc_deck_manifest_read <- function(dir) jsonlite::read_json(file.path(dir, "manifest.json"), simplifyVector = TRUE)

## Colunas de tempo ja no instante correto (fuso da carga) + interval_start/interval_end para a carga.
.pc_deck_view <- function(dt, dataset, dir) {
  m <- .pc_deck_manifest_read(dir); tz <- m$timezone$load
  v <- data.table::copy(dt)
  fix <- function(x, declared) lubridate::with_tz(lubridate::force_tz(x, declared), tz)
  if (dataset == "load") {
    st <- fix(v$timestamp, tz)
    v[, interval_start := if (identical(m$load_label, "start")) st else st - lubridate::minutes(30L)]
    v[, interval_end := interval_start + lubridate::minutes(30L)]
  } else if (dataset == "holidays") {
    NULL
  } else if (dataset == "weather_observed") {
    v[, timestamp := fix(timestamp, m$timezone$weather_observed)]
  } else {
    v[, `:=`(issue_time = fix(issue_time, m$timezone$weather_forecast$issue_time),
             valid_time = fix(valid_time, m$timezone$weather_forecast$valid_time))]
  }
  v
}

## Corta fisicamente a carga: fica so interval_end <= until (POSIXct no fuso da carga).
pc_deck_until <- function(dir, until) {
  dt <- .pc_deck_read(dir, "load"); v <- .pc_deck_view(dt, "load", dir)
  .pc_deck_rewrite(dt[v$interval_end <= until], dir, "load")
}

## Remove da carga os intervalos (areas x interval_starts; vetores aceitos): cria buracos.
pc_deck_drop <- function(dir, area, interval_start) {
  dt <- .pc_deck_read(dir, "load"); v <- .pc_deck_view(dt, "load", dir)
  .pc_deck_rewrite(dt[!(v$area_code %in% area & v$interval_start %in% interval_start)], dir, "load")
}

## +1 no valor (load_mwh | temperature) das linhas onde `rows` (expressao quote()) e TRUE; a expressao ve
## interval_start/interval_end (carga), timestamp (observado) ou issue_time/valid_time (previsto) no fuso da carga.
pc_deck_perturb <- function(dir, dataset, rows, env = parent.frame()) {
  dt <- .pc_deck_read(dir, dataset); v <- .pc_deck_view(dt, dataset, dir)
  idx <- which(eval(rows, v, env))
  col <- switch(dataset, load = "load_mwh", holidays = "prevcarga_code", "temperature")
  data.table::set(dt, i = idx, j = col, value = dt[[col]][idx] + 1)
  .pc_deck_rewrite(dt, dir, dataset)
  invisible(length(idx))
}

## Regrava o clima previsto com as emissoes pedidas (horas UTC), nos mesmos dias/areas do deck.
pc_deck_issues <- function(dir, hours_utc) {
  fc <- .pc_deck_read(dir, "weather_forecast"); m <- .pc_deck_manifest_read(dir)
  days <- sort(unique(lubridate::as_date(fc$issue_time)))
  new <- .pc_deck_forecast(days, sort(unique(fc$area_code), method = "radix"), hours_utc,
                           m$timezone$load, m$timezone$weather_forecast$issue_time)
  .pc_deck_rewrite(new, dir, "weather_forecast")
}

## --- Painel (M2.7) ------------------------------------------------------------------------------------------------
## Feriados padrao (O = 2026-10-01) por area, com os tres codigos int64: O-10 (A com simplified_code = -1), O+3, O+9, O+12.
pc_deck_holidays_default <- function(areas = c("A", "B"), origin = lubridate::ymd("2026-10-01")) {
  off <- c(-10L, 3L, 9L, 12L)
  h <- data.table::CJ(area_code = areas, k = seq_along(off), sorted = FALSE)
  h[, date := origin + off[k]]
  h[, `:=`(holiday_type_id = bit64::as.integer64(1L + k), prevcarga_code = bit64::as.integer64(10L * match(area_code, areas) + k),
           simplified_code = bit64::as.integer64(k))]
  h[area_code == areas[[1L]] & k == 1L, simplified_code := bit64::as.integer64(-1L)]
  h[, .(area_code, date, holiday_type_id, prevcarga_code, simplified_code)]
}

## Topologia-fixture so com as areas dadas (mesmo schema do config/topology.yaml).
pc_deck_topology <- function(path, areas) {
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
               sprintf("  X: {areas: [%s], loss: PX}", paste(areas, collapse = ", ")),
               "order: [X]", "members: []"), path)
  invisible(path)
}

## Deck sentinela + feriados por area + topologia-fixture em <dir>/topology.yaml. Clima e carga ja tem serie propria por area.
pc_deck_panel <- function(areas = c("A", "B"), days = pc_deck_default_days(), until = NULL, holidays = NULL,
                          dir = withr::local_tempdir(.local_envir = parent.frame()), ...) {
  pc_deck_sentinel("end", days = days, areas = areas, until = until, dir = dir, ...)
  if (is.null(holidays)) holidays <- pc_deck_holidays_default(areas)
  .pc_deck_write(data.table::copy(holidays), dir, file.path("auxiliary", "holidays"), character(), "date")
  pc_deck_topology(file.path(dir, "topology.yaml"), areas)
  dir
}

pc_ctx_panel <- function(dir, ...) {
  c0 <- pc_ctx_deck(dir, ...)
  c0$topology_path <- file.path(dir, "topology.yaml")
  c0
}

## --- Mutadores do validate-deck (M2.4): UMA alteracao isolada cada, so sob o `dir` recebido (tempdir) -------------
## `fn(dt)` recebe a tabela crua do dataset (digitos ingenuos do store, `area_code` da particao) e devolve a tabela alterada.
## Colunas int64 do store voltam a int64 na regravacao (o arrow as entrega como integer).
.pc_mut_int64 <- function(dt) {
  for (n in intersect(c("holiday_type_id", "prevcarga_code", "simplified_code"), names(dt)))
    data.table::set(dt, j = n, value = bit64::as.integer64(dt[[n]]))
  dt
}

pc_mut_dataset <- function(dir, dataset, fn) {
  dt <- .pc_mut_int64(.pc_deck_read(dir, dataset))
  .pc_deck_rewrite(fn(dt), dir, dataset)
}

pc_mut_load <- function(dir, fn) pc_mut_dataset(dir, "load", fn)
pc_mut_weather <- function(dir, dataset, fn) pc_mut_dataset(dir, dataset, fn)
pc_mut_holiday <- function(dir, col, value, row = 1L) {
  pc_mut_dataset(dir, "holidays", function(dt) { data.table::set(dt, i = row, j = col, value = bit64::as.integer64(value)); dt })
}

## Reescreve cada parquet do dataset (opcionalmente so de uma `area`) trocando as colunas via `fn(lista_de_colunas)`.
.pc_mut_files <- function(dir, dataset, fn, area = NULL) {
  fs <- list.files(file.path(dir, .pc_deck_dsets[[dataset]]), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  if (!is.null(area)) fs <- fs[grepl(paste0("area_code=", area, "/"), fs, fixed = TRUE)]
  for (f in fs) {
    tb <- arrow::read_parquet(f, as_data_frame = FALSE)
    cols <- stats::setNames(lapply(tb$schema$names, function(n) tb$GetColumnByName(n)), tb$schema$names)
    arrow::write_parquet(do.call(arrow::arrow_table, fn(cols)), f)
  }
  invisible(length(fs))
}

pc_mut_drop_col <- function(dir, dataset, col, area = NULL) {
  .pc_mut_files(dir, dataset, function(cols) cols[setdiff(names(cols), col)], area)
}

## type: "string" | "timestamp_tz" (timestamp[ns] com fuso UTC no parquet)
pc_mut_retype <- function(dir, dataset, col, type = c("string", "timestamp_tz")) {
  type <- match.arg(type)
  to <- if (type == "string") arrow::utf8() else arrow::timestamp("ns", timezone = "UTC")
  .pc_mut_files(dir, dataset, function(cols) { cols[[col]] <- cols[[col]]$cast(to); cols })
}

## Parquet sem Hive: todos os arquivos do dataset vao para a raiz do dataset.
pc_mut_flatten <- function(dir, dataset) {
  root <- file.path(dir, .pc_deck_dsets[[dataset]])
  fs <- list.files(root, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  file.rename(fs, file.path(root, sprintf("flat-%03d.parquet", seq_along(fs))))
  unlink(list.dirs(root, recursive = FALSE), recursive = TRUE)
  invisible(length(fs))
}

## Trunca os bytes do arquivo `rel` (relativo a dir): o arrow nao consegue abri-lo.
pc_mut_corrupt <- function(dir, rel) {
  f <- file.path(dir, rel)
  n <- file.size(f)
  writeBin(readBin(f, "raw", n)[seq_len(n %/% 3L)], f)
  invisible(f)
}

## Copia as particoes de uma area existente para uma area nova `code` em todos os datasets do deck.
pc_mut_add_area <- function(dir, code, from = NULL) {
  for (rel in .pc_deck_dsets) {
    ps <- list.dirs(file.path(dir, rel), recursive = FALSE)
    ps <- ps[grepl("area_code=", basename(ps))]
    if (!length(ps)) next
    src <- if (is.null(from)) ps[[1L]] else file.path(dir, rel, paste0("area_code=", from))
    dst <- file.path(dir, rel, paste0("area_code=", code)); dir.create(dst)
    file.copy(list.files(src, full.names = TRUE), dst, recursive = TRUE)
  }
  invisible(code)
}

pc_mut_issue <- function(dir, hours_utc) pc_deck_issues(dir, hours_utc)

## Historico de `n` dias de carga e clima observado terminando no ultimo dia de carga do deck (mesma formula do
## sentinela: carga = slot + 1000 * indice da area; clima por hora). Mantem emissoes e feriados.
pc_deck_days <- function(dir, n) {
  dt <- .pc_deck_read(dir, "load"); v <- .pc_deck_view(dt, "load", dir)
  m <- .pc_deck_manifest_read(dir); tz <- m$timezone$load
  areas <- unique(dt$area_code)
  last <- lubridate::as_date(max(v$interval_start), tz = tz)
  days <- seq(last - n + 1L, last, by = "day")
  shift <- if (identical(m$load_label, "start")) 0L else 30L
  g <- data.table::CJ(area_code = areas, day = days, slot = 1:48, sorted = FALSE)
  g[, timestamp := lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::minutes(30L * (slot - 1L) + shift)]
  g[, load_mwh := as.double(slot + 1000L * match(area_code, areas))]
  .pc_deck_rewrite(g[, .(area_code, timestamp, load_mwh)], dir, "load")
  o <- data.table::CJ(area_code = areas, day = days, h = 0:23, sorted = FALSE)
  o[, timestamp := lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::hours(h)]
  o[, temperature := 15 + match(area_code, areas) + h / 100]
  o[, heat_index := temperature + 1]
  .pc_deck_rewrite(o[, .(area_code, timestamp, temperature, heat_index)], dir, "weather_observed")
  invisible(dir)
}
