## helper-deck.R -- deck sentinela compartilhado (M2.1; consumido por M2.2..M2.V, M3..M7, Reconcile e plugins).
## Nao e codigo de produto. Auto-carregado por testthat::test_dir. Nunca toca data/: tudo em tempdir.
##
## Deck sentinela: carga com valor = slot do dia (1..48) + 1000 * indice da area, gravada como
## timestamp[ns] SEM fuso (espelha o store real). Modo `end`: os digitos sao o FIM do intervalo (+30 min);
## modo `start`: o inicio. O mesmo deck nos dois modos tem interval_start/interval_end identicos apos a leitura.
## Clima: observado horario e previsto com uma emissao por hora UTC em `issues` por dia; digitos de
## issue_time em UTC, digitos de valid_time/timestamp no fuso da carga.
## Estende no M2.7: clima por area e feriados (auxiliary/holidays).

.pc_deck_tz <- "Etc/GMT+3"

## timestamp[ns] sem fuso a partir de digitos civis (POSIXct em UTC cujos digitos sao os do store)
.pc_naive_ns <- function(x_digits_utc) {
  arrow::Array$create(lubridate::force_tz(x_digits_utc, "UTC"), type = arrow::timestamp("ns"))
}

## Grava particoes Hive area_code=/year= a mao (arrow::write_dataset exige dplyr, que o deck nao usa).
.pc_deck_write <- function(dt, dir, rel, ts_cols) {
  yr <- as.integer(lubridate::year(dt[[ts_cols[[1L]]]]))
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
  .pc_deck_write(fc[, .(area_code, issue_time, valid_time, temperature, heat_index)], dir,
                 file.path("weather", "forecast"), c("valid_time", "issue_time"))

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
