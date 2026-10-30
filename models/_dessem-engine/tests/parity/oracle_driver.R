#!/usr/bin/env Rscript
## oracle_driver.R -- driver do ORACULO do motor DESSEM isolado (_dessem-engine) para scripts/tools/plugin_golden.R (DL.4). ASCII.
## Ferramenta de DESENVOLVIMENTO (so Linux, fora do bundle), chamada UNICAMENTE por plugin_golden.R em processo filho.
## Contrato do P.0f: --oracle <dir> --unit <unit.json> --out <dir> --available-until <ISO>. O oraculo e so lido/executado:
## tudo que ele grava cai sob --out (nunca na arvore do oraculo).
##
## Reproduz o caminho serial de um modelo do dessem-linear do oraculo (modo paridade: sem corte intradiario, dataPrev = O + 1):
## `load_dessem_input` -> `.build_dessem_deck` -> `TransformaDeck` -> `set.seed(42L)` -> `processaModelosDMPerfil` ->
## `desagregaModeloSH`. O argumento --available-until do contrato e aceito e ignorado (o oraculo roda no default = 23:30 de O).
## A variante (MED/MAX/MXMN) entra pela variavel EXOGENA do `oracle_env` da unidade (o oraculo a le em `.exogena_mode()`).
## FERIADOS e ordenado por (Data, Tipo) antes do TransformaDeck (a ordem de leitura do arrow no oraculo varia; a saida nao).
## Saidas em --out: native.rds = list(deck = deck CRU entregue ao TransformaDeck do oraculo, proc = deck processado do oraculo,
## hourly, semihourly, model_key, seed, area, origin, dataPrev, variant), long.parquet (semi-horario: timestamp, Passo, Carga).

local({
  av <- commandArgs(trailingOnly = TRUE)
  opt <- function(name) { i <- which(av == name); if (length(i) && i[[1L]] < length(av)) av[[i[[1L]] + 1L]] else NULL }
  oracle <- opt("--oracle"); ujson <- opt("--unit"); out <- opt("--out")
  if (is.null(oracle) || is.null(ujson) || is.null(out)) stop("[prevcarga/contract] driver: --oracle, --unit e --out sao obrigatorios")
  oracle <- normalizePath(oracle, winslash = "/", mustWork = TRUE)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(out, winslash = "/", mustWork = TRUE)
  if (identical(out, oracle) || startsWith(paste0(out, "/"), paste0(oracle, "/")))
    stop("[prevcarga/contract] golden: --out dentro do oraculo")
  unit <- jsonlite::read_json(ujson, simplifyVector = TRUE)
  if (is.null(unit$data_dir) || !nzchar(unit$data_dir)) stop("[prevcarga/contract] driver: informe --data ao plugin_golden.R")
  if (is.null(unit$history_days)) stop("[prevcarga/contract] driver: history_days e obrigatorio (O - history_start)")
  variant <- if (is.null(unit$variant)) "MED" else unit$variant
  seed <- 42L

  rdir <- file.path(oracle, "models", "dessem-linear", "R")
  Sys.setenv(PREVCARGA_MODEL_R_DIR = rdir)   # so neste processo filho: o oraculo localiza o proprio R/ por esta variavel
  suppressPackageStartupMessages({ library(lubridate); library(data.table); library(arrow); library(dplyr) })
  sys.source(file.path(rdir, "utils.R"), envir = globalenv())
  sys.source(file.path(rdir, "config.R"), envir = globalenv())
  for (f in c("validate_input.R", "normalize_holiday_codes.R", "fill_forecast_tail.R"))
    sys.source(file.path(oracle, "scripts", "lib", f), envir = globalenv())
  sys.source(file.path(rdir, "ingestion", "data_adapter.R"), envir = globalenv())
  sys.source(file.path(rdir, "training", "dessem_linear_training.R"), envir = globalenv())
  source(file.path(oracle, "models", "dessem-linear", "vendor", "_load_engine.R"), chdir = TRUE, local = FALSE)

  if (!identical(.exogena_mode(), variant)) stop("[prevcarga/contract] driver: EXOGENA do oracle_env diferente da variante da unidade")
  model_key <- dessem_linear_config()$model_key
  origin <- as.Date(unit$origin)
  raw <- load_dessem_input(area = unit$area, origin_day = origin, history_start = origin - as.integer(unit$history_days),
                           horizon = 9L, data_dir = unit$data_dir)
  ## `.load_feriados` do oraculo le o dataset do arrow em ordem NAO deterministica (linhas de FERIADOS embaralhadas entre rodadas
  ## com a saida numerica identica): a ordem de leitura e canonizada aqui (por Data, Tipo) para o golden ser reproduzivel.
  data.table::setorderv(raw$FERIADOS, c("Data", "Tipo"))
  deck <- .build_dessem_deck(raw)
  proc <- TransformaDeck(deck = deck, dataInicioPrev = raw$dataPrev, horizonteprevisao = "10DIAS")
  set.seed(seed)
  hor <- processaModelosDMPerfil(proc, getConfigModel(model_key))
  sh <- desagregaModeloSH(model_key, hor[, Modelo := model_key])

  saveRDS(list(deck = deck, proc = proc, hourly = hor, semihourly = sh, model_key = model_key, seed = seed,
               area = unit$area, origin = origin, dataPrev = raw$dataPrev, variant = variant),
          file.path(out, "native.rds"))
  long <- data.frame(timestamp = sh$DataHora, Passo = sh$Passo, Carga = sh$Carga)
  arrow::write_parquet(long, file.path(out, "long.parquet"))
  invisible(NULL)
})
