#!/usr/bin/env Rscript
## reference_driver.R -- driver clean-room do golden (ADR-0041; supera o oracle_driver.R do ADR-0034). ASCII.
## Ferramenta de DESENVOLVIMENTO (so Linux, fora do bundle), chamada UNICAMENTE por scripts/tools/plugin_golden.R
## (`--source reference`) em processo filho, com o `Rscript` da tag. Mesmo contrato do oracle_driver.R:
##   --oracle <checkout limpo da tag> --unit <unit.json> --out <dir> --available-until <ISO>   (este ultimo e aceito e ignorado:
## o corte intradiario vem do `available_until` da unidade, como em plugin_parity.R)
## O codigo que roda e o da PROPRIA tag: scripts/lib e scripts/tools/plugin_parity.R do checkout, plugin pelo loader
## (prevcarga_plugin_load) e fachada (treino em `last_data_day` + infer em O, com os cortes LITERAIS da unidade, como
## `.pc_parity_envelope`). Sem variavel de ambiente; o store (`data_dir` do unit.json) so e LIDO (overlay de symlinks em tempdir).
## Escrita por LAYOUT do plugin (nunca por nome de modelo):
##   native.rds   = list(layout, values, meta) -- `values`/`meta` do envelope; unidade com variante => meta$exogena
##   long.parquet = (timestamp = INICIO do intervalo, rotulo de parede naive, y_pred) das celulas de `delivery`
##   layout horario (rf_wide_hourly_216, 2 origens): long_O.parquet (O+1..) + long_Om1.parquet (dia O, d_plus = 1)
## Modelo sem `run.yaml` (motor isolado) tem driver proprio em <ref>/models/<m>/tests/parity/reference_driver.R.

local({
  av <- commandArgs(trailingOnly = TRUE)
  opt <- function(name) { i <- which(av == name); if (length(i) && i[[1L]] < length(av)) av[[i[[1L]] + 1L]] else NULL }
  ref <- opt("--oracle"); ujson <- opt("--unit"); out <- opt("--out")
  if (is.null(ref) || is.null(ujson) || is.null(out)) stop("[prevcarga/contract] driver: --oracle, --unit e --out sao obrigatorios")
  ref <- normalizePath(ref, winslash = "/", mustWork = TRUE)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(out, winslash = "/", mustWork = TRUE)
  if (identical(out, ref) || startsWith(paste0(out, "/"), paste0(ref, "/")))
    stop("[prevcarga/contract] golden: --out dentro da referencia")
  u <- jsonlite::read_json(ujson, simplifyVector = TRUE)
  if (is.null(u$data_dir) || !nzchar(u$data_dir)) stop("[prevcarga/contract] driver: informe --data ao plugin_golden.R")
  if (is.null(u$model) || !nzchar(u$model)) stop("[prevcarga/contract] driver: unit.json sem `model`")

  ## fundacao DA TAG (mesma ordem de pc_parity_cli)
  lib <- file.path(ref, "scripts", "lib")
  options(prevcarga.lib = lib)
  for (m in c("errors", "runtime")) source(file.path(lib, paste0(m, ".R")))
  invisible(prevcarga_runtime_enforce())
  for (m in c("config", "provenance", "topology", "deck_reader", "validation", "dates", "registry", "features"))
    source(file.path(lib, paste0(m, ".R")))
  ge <- new.env(parent = globalenv())
  sys.source(file.path(ref, "scripts", "tools", "plugin_golden.R"), envir = ge)
  sys.source(file.path(ref, "scripts", "tools", "plugin_parity.R"), envir = ge)
  for (n in ls(ge, all.names = TRUE)) if (is.function(ge[[n]])) environment(ge[[n]]) <- ge

  tz <- u$tz
  unit <- list(id = u$id, area = u$area, origin = lubridate::ymd(u$origin),
               available_until = lubridate::ymd_hm(u$available_until, tz = tz), weather_issue_cutoff = u$weather_issue_cutoff,
               day0_source = u$day0_source, variant = u$variant, history_days = u$history_days, cuts = u$cuts,
               params = u$params, seed = u$seed, data_areas = u$data_areas)

  ## overlay do store (symlinks + manifest do template da tag); nada e gravado em data/
  store <- tempfile("ref-store-"); dir.create(store)
  on.exit(unlink(store, recursive = TRUE), add = TRUE)
  for (d in c("load", "weather", "auxiliary")) file.symlink(normalizePath(file.path(u$data_dir, d)), file.path(store, d))
  file.copy(file.path(ref, "config", "deck-manifest.store.json"), file.path(store, "manifest.json"))
  mdir <- file.path(ref, "models")
  top <- file.path(mdir, u$model, "tests", "parity", "topology.yaml")
  if (!file.exists(top)) {   # modelo sem topologia propria (p.ex. HW): um subsistema com as areas do store de paridade
    top <- file.path(store, "topology.yaml")
    ar <- unique(c("SECO", "S", "NE", "N", unit$area))
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
                 sprintf("  X: {areas: [%s], loss: PX}", paste0("\"", ar, "\"", collapse = ", ")), "order: [X]", "members: []"), top)
  }

  env <- ge$.pc_parity_envelope(u$model, unit, mdir, store, top)

  meta <- env$meta
  if (!is.null(unit$variant)) meta$exogena <- unit$variant
  saveRDS(list(layout = env$layout, values = env$values, meta = meta), file.path(out, "native.rds"))

  ## long: rotulo de parede naive (UTC so como moldura) = INICIO do intervalo
  dl <- as.data.frame(env$delivery)
  start <- lubridate::force_tz(lubridate::with_tz(dl$interval_end, tz) - lubridate::minutes(30L), "UTC")
  long <- data.frame(timestamp = start, y_pred = dl$y_pred)
  if (ge$.pc_parity_layout(env$layout)$step_min == 60L) {
    ## 2 origens nativas: o dia O sai de long_Om1 (d_plus 1) e O+1.. de long_O (ver .pc_parity_read_dir)
    day <- lubridate::as_date(lubridate::force_tz(start, tz)) - unit$origin
    lm <- long[day == 0L, , drop = FALSE]; lm$d_plus <- 1L
    lo <- long[day >= 1L, , drop = FALSE]; lo$d_plus <- as.integer(day[day >= 1L])
    arrow::write_parquet(lo, file.path(out, "long_O.parquet"))
    arrow::write_parquet(lm, file.path(out, "long_Om1.parquet"))
  } else {
    arrow::write_parquet(long, file.path(out, "long.parquet"))
  }
  invisible(NULL)
})
