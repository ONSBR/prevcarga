#!/usr/bin/env Rscript
## reference_driver.R -- driver clean-room do golden do motor DESSEM isolado (_dessem-engine; ADR-0041, substitui o oracle_driver.R). ASCII.
## Ferramenta de DESENVOLVIMENTO (so Linux, fora do bundle), chamada UNICAMENTE por scripts/tools/plugin_golden.R (`--source reference`)
## em processo filho. O motor nao e plugin (sem run.yaml): o driver monta o deck pelo ingest do dessem-linear DA TAG, no modo paridade
## do motor (dataPrev = O + 1, historico ate O, 9 dias previstos, sem corte intradiario) e roda o motor como o despachante o faz:
## `to_model_frame` na entrada (ja aplicado pelo despachante), TransformaDeck, set.seed(42), processaModelosDMPerfil, desagregaModeloSH.
## Contrato: --oracle <checkout limpo da tag> --unit <unit.json> --out <dir> --available-until <ISO> (ignorado).
## Saidas: native.rds = list(deck = deck CRU no frame de exibicao (CARGAHIST/EXOGENAHIST/EXOGENAPREV/FERIADOS), proc = TransformaDeck,
## hourly, semihourly (DataHora = carimbo de 30 min do despachante, Passo, Carga), model_key, seed, area, origin, dataPrev, variant) e
## long.parquet (timestamp, Passo, Carga do semi-horario).

local({
  av <- commandArgs(trailingOnly = TRUE)
  opt <- function(name) { i <- which(av == name); if (length(i) && i[[1L]] < length(av)) av[[i[[1L]] + 1L]] else NULL }
  ref <- opt("--oracle"); ujson <- opt("--unit"); out <- opt("--out")
  if (is.null(ref) || is.null(ujson) || is.null(out)) stop("[prevcarga/contract] driver: --oracle, --unit e --out sao obrigatorios")
  ref <- normalizePath(ref, winslash = "/", mustWork = TRUE)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(out, winslash = "/", mustWork = TRUE)
  if (identical(out, ref) || startsWith(paste0(out, "/"), paste0(ref, "/"))) stop("[prevcarga/contract] golden: --out dentro da referencia")
  u <- jsonlite::read_json(ujson, simplifyVector = TRUE)
  if (is.null(u$data_dir) || !nzchar(u$data_dir)) stop("[prevcarga/contract] driver: informe --data ao plugin_golden.R")
  if (is.null(u$history_days)) stop("[prevcarga/contract] driver: history_days e obrigatorio")

  lib <- file.path(ref, "scripts", "lib")
  options(prevcarga.lib = lib)
  for (m in c("errors", "runtime")) source(file.path(lib, paste0(m, ".R")))
  invisible(prevcarga_runtime_enforce())
  for (m in c("config", "provenance", "topology", "deck_reader", "validation", "dates", "registry", "features"))
    source(file.path(lib, paste0(m, ".R")))

  tz <- u$tz
  O <- lubridate::ymd(u$origin)
  variant <- if (is.null(u$variant)) "MED" else u$variant
  store <- tempfile("ref-store-"); dir.create(store)
  on.exit(unlink(store, recursive = TRUE), add = TRUE)
  for (d in c("load", "weather", "auxiliary")) file.symlink(normalizePath(file.path(u$data_dir, d)), file.path(store, d))
  file.copy(file.path(ref, "config", "deck-manifest.store.json"), file.path(store, "manifest.json"))
  mdir <- file.path(ref, "models")
  ctx <- list(data_dir = store, root = ref, weather_issue_cutoff = u$weather_issue_cutoff,
              topology_path = file.path(mdir, "dessem-linear", "tests", "parity", "topology.yaml"))

  plugin <- prevcarga_plugin_load("dessem-linear", mdir)
  d <- plugin$descriptor
  sp <- plugin$spec
  sp$params$exogena <- variant
  sp$history_days$train <- as.integer(u$history_days)
  sp$horizon_days <- 9L - 1L        # o ingest roda horizon_days + 1 dias a partir de dataPrev: 9 dias previstos (O+1..O+9)
  ## modo paridade do motor: dataPrev = O + 1; historico e carga ate O inteiro (sem corte intradiario)
  P <- O + 1L
  lag <- stats::setNames(d$data_lag_days, "dessem-linear")
  rc <- suppressWarnings(prevcarga_resolve_cuts(ctx, origin = P, model = "dessem-linear", data_lag_days = lag))
  row <- rc$cuts[rc$cuts$model == "dessem-linear", ]
  cuts <- list(forecast_origin = rc$forecast_origin, origin_day = rc$forecast_origin, split = row$split, tz = rc$tz,
               load_label = rc$load_label, weather_issue_cutoff = rc$weather_issue_cutoff, available_until = rc$available_until,
               weather_issue_limit = rc$weather_issue_limit, train_origin = row$train_origin, last_data_day = row$last_data_day)
  hist <- as.integer(u$history_days) + 1L
  p <- suppressWarnings(prevcarga_read_panel(ctx, P, u$area, sp$horizon_days, hist))
  lim <- attr(p$weather_forecast, "weather_issue_limit")
  fc <- prevcarga_read_weather(ctx, "forecast", u$area, P - hist, P + sp$horizon_days + 2L)
  p$weather_forecast <- fc[issue_time <= lim]
  m <- .prevcarga_plugin_stage_input(p, cuts, d, sp, "train")
  deck <- suppressMessages(plugin$env$plugin_ingest(m$panel, m$cuts, sp))
  eng <- parent.env(plugin$env)

  dk <- suppressMessages(eng$TransformaDeck(deck = list(CARGAHIST = deck$CARGAHIST, EXOGENAHIST = deck$EXOGENAHIST,
                                                        EXOGENAPREV = deck$EXOGENAPREV, FERIADOS = deck$FERIADOS),
                                            dataInicioPrev = format(deck$dataPrev), horizonteprevisao = "10DIAS", tz = tz))
  cfg <- eng$getConfigModel(deck$model_key)
  seed <- 42L
  set.seed(seed)
  hor <- suppressMessages(eng$processaModelosDMPerfil(dk, cfg))
  sh <- suppressMessages(eng$desagregaModeloSH(deck$model_key, data.table::copy(hor)[, Modelo := deck$model_key]))
  anchor <- to_display_frame(data.table::data.table(DataHora = sh$DataHora), d$model_time_offset_min, "DataHora")$DataHora
  stamp <- anchor - lubridate::minutes(ifelse(sh$Passo %% 2L == 1L, 30L, 0L))
  semi <- data.table::data.table(DataHora = stamp, Passo = sh$Passo, Carga = sh$Carga)

  raw <- list(CARGAHIST = to_display_frame(data.table::copy(deck$CARGAHIST), d$model_time_offset_min, "DataHora"),
              EXOGENAHIST = deck$EXOGENAHIST, EXOGENAPREV = deck$EXOGENAPREV, FERIADOS = deck$FERIADOS)
  saveRDS(list(deck = raw, proc = dk, hourly = data.table::copy(hor), semihourly = semi, model_key = deck$model_key, seed = seed,
               area = u$area, origin = O, dataPrev = deck$dataPrev, variant = variant), file.path(out, "native.rds"))
  ## timestamp naive (rotulo de parede em UTC como moldura)
  long <- data.frame(timestamp = lubridate::force_tz(lubridate::with_tz(semi$DataHora, tz), "UTC"), Passo = semi$Passo, Carga = semi$Carga)
  arrow::write_parquet(long, file.path(out, "long.parquet"))
  invisible(NULL)
})
