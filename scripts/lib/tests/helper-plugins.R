## helper-plugins.R -- fixtures sentinela da fundacao de plugins (P.0a; P.0b/P.0c/P.0d/P.0j acrescentam).
## Auto-carregado por testthat::test_dir. Nao e codigo de produto. Tudo por lubridate com tz explicito, sem env vars.
##
## Sentinela: `load_mwh` e `temperature` codificam os MINUTOS desde 2026-09-01 00:00 (no tz do painel) do
## `interval_start`/`valid_time`. Assim qualquer deslocamento de rotulo ou agrupamento errado muda o valor.

.PC_SENTINEL_BASE <- "2026-09-01 00:00:00"

pc_sentinel_minutes <- function(t, tz = lubridate::tz(t)) {
  base <- lubridate::ymd_hms(.PC_SENTINEL_BASE, tz = tz)
  as.numeric(difftime(t, base, units = "mins"))
}

## v (minutos) -> instante no tz dado
pc_decode_sentinel <- function(v, tz = "Etc/GMT+3") {
  lubridate::ymd_hms(.PC_SENTINEL_BASE, tz = tz) + lubridate::minutes(as.integer(round(v)))
}

## Painel no formato do M2.7 (lista com `attrs`). Dias O .. O+days-1 em grade de 30 min.
## `drop`: vetor POSIXct de interval_start a remover da carga (ex.: deixa a meia hora isolada).
## Previsto: duas emissoes por area (00Z e 06Z em UTC); temperature = sentinela + 1e6 * indice da emissao.
pc_sentinel_panel <- function(origin, days, areas, tz = "Etc/GMT+3", drop = NULL) {
  O <- lubridate::as_date(origin)
  t0 <- lubridate::force_tz(lubridate::as_datetime(O), tz)
  starts <- t0 + lubridate::minutes(30L * (seq_len(48L * days) - 1L))
  load <- data.table::rbindlist(lapply(areas, function(a) data.table::data.table(
    area_code = a, interval_start = starts, interval_end = starts + lubridate::minutes(30),
    load_mwh = pc_sentinel_minutes(starts, tz))))
  if (!is.null(drop)) load <- load[!(interval_start %in% drop)]
  obs <- data.table::rbindlist(lapply(areas, function(a) data.table::data.table(
    area_code = a, valid_time = starts, temperature = pc_sentinel_minutes(starts, tz))))
  issues <- lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(c(0L, 6L))
  fc <- data.table::rbindlist(lapply(areas, function(a) data.table::rbindlist(lapply(seq_along(issues), function(k)
    data.table::data.table(area_code = a, issue_time = issues[[k]], valid_time = starts,
                           temperature = pc_sentinel_minutes(starts, tz) + 1e6 * (k - 1L))))))
  hol <- data.table::data.table(area_code = areas, date = rep(O, length(areas)),
                                holiday_type_id = 1L, prevcarga_code = 2L, simplified_code = 3L)
  au <- max(load$interval_end)
  list(load = load, weather_observed = obs, weather_forecast = fc, holidays = hol,
       attrs = list(origin = O, available_until = au, load_label = "end", tz = tz,
                    weather_issue_cutoff = "available_until", weather_issue_limit = au))
}

## Regra-brinquedo: cada hora vira 2 intervalos de 30 min com o mesmo valor (vetor y_pred de 2n).
pc_rule_split <- function(x) rep(x$y_pred, each = 2L)

## CONTROL: desloca o carimbo em 30 min (interval_end fora da ancora).
pc_rule_shifted <- function(x) {
  stamp <- sort(c(x$interval_end - lubridate::minutes(30), x$interval_end))
  data.table::data.table(interval_end = stamp + lubridate::minutes(30), y_pred = rep(x$y_pred, each = 2L))
}

## CONTROL: agrupa a carga por interval_end (errado) em vez de interval_start.
.pc_to_hourly_by_end <- function(load) {
  dt <- data.table::copy(data.table::as.data.table(load))
  dt[, hour_end := lubridate::floor_date(interval_end, "hour")]
  dt[, n_slots := .N, by = .(area_code, hour_end)]
  out <- dt[n_slots == 2L, .(load_mwh = mean(load_mwh)), by = .(area_code, interval_start = hour_end)]
  data.table::setorder(out, area_code, interval_start)
  out[]
}

## --- Fabrica de variantes dos plugins-brinquedo (P.0b) --------------------------------------------------------
pc_toy_root <- function() file.path(pc_root(), "scripts", "lib", "tests", "fixtures", "plugins")

.pc_dput <- function(x) paste(deparse(x, control = c("keepNA", "keepInteger", "niceNames", "showAttributes")),
                             collapse = "\n")

## Copia o brinquedo `base` (e o motor) para um tempdir e devolve o `root` novo.
##  run_yaml  : modifyList no run.yaml (NULL remove a chave);  descriptor : lista (NULL remove o campo) ou
##  function(d) -> d, aplicada sobre o descriptor original por R/zz_override.R;  files : nome relativo -> linhas;
##  engine    : modifyList no engine_manifest();  engine_files : nome em _toy-engine/R -> linhas;
##  name      : nome do plugin na copia (ex.: toy-60b); ajusta run.yaml e model_id.
pc_toy_variant <- function(base, run_yaml = list(), descriptor = list(), files = list(), engine = list(),
                           engine_files = list(), name = base) {
  root <- tempfile("toyroot-"); dir.create(root)
  dir.create(file.path(root, name))
  file.copy(list.files(file.path(pc_toy_root(), base), full.names = TRUE), file.path(root, name), recursive = TRUE)
  file.copy(file.path(pc_toy_root(), "_toy-engine"), root, recursive = TRUE)
  ry <- file.path(root, name, "run.yaml")
  y <- yaml::read_yaml(ry); y$name <- name
  yaml::write_yaml(modifyList(y, run_yaml), ry)
  ov <- descriptor
  if (name != base || length(ov)) {
    pf <- file.path(root, name, "R", "plugin.R")
    writeLines(sub("^plugin_descriptor <- function", ".plugin_descriptor_base <- function", readLines(pf)), pf)
    body <- if (is.function(ov)) paste0("f <- ", .pc_dput(ov), "\n  d <- f(d)") else paste0(
      "ov <- ", .pc_dput(ov), "\n  for (n in names(ov)) d[[n]] <- ov[[n]]")
    writeLines(c("plugin_descriptor <- function() {", "  d <- .plugin_descriptor_base()",
                 if (name != base) sprintf("  d$model_id <- \"%s\"", name), paste0("  ", body), "  d", "}"),
               file.path(root, name, "R", "zz_override.R"))
  }
  for (f in names(files)) {
    dir.create(dirname(file.path(root, name, f)), showWarnings = FALSE, recursive = TRUE)
    writeLines(files[[f]], file.path(root, name, f))
  }
  if (length(engine)) {
    e <- new.env(); sys.source(file.path(root, "_toy-engine", "R", "manifest.R"), envir = e)
    m <- modifyList(e$engine_manifest(), engine)
    writeLines(c("engine_manifest <- function()", paste0("  ", .pc_dput(m))), file.path(root, "_toy-engine", "R", "manifest.R"))
  }
  for (f in names(engine_files)) writeLines(engine_files[[f]], file.path(root, "_toy-engine", "R", f))
  root
}

pc_globalenv_snapshot <- function() list(globalenv = ls(globalenv(), all.names = TRUE), search = search())
