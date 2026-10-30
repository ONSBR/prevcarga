#!/usr/bin/env Rscript
## oracle_driver.R -- driver do ORACULO do holt-winters para scripts/tools/plugin_golden.R (HW.5). ASCII.
## Ferramenta de DESENVOLVIMENTO (so Linux, fora do bundle), chamada UNICAMENTE por plugin_golden.R em processo filho.
## Contrato do P.0f: --oracle <dir> --unit <unit.json> --out <dir> --available-until <ISO>. O oraculo e so lido/executado:
## tudo que ele grava cai sob --out (artefato com `artifact_dir` explicito; nunca o model-local do oraculo).
##
## Reproduz `jobs/run_job.sh` (retrain_harness) por unidade: `run_forecast(area, O, history_start = O - history_days,
## horizon = 8)`. O HW do oraculo NAO recebe --available-until (cut-invariant; o adaptador capa em O-1 e o clima em 00Z de
## O, data_adapter.R:328); o argumento do contrato e aceito e ignorado.
## Saidas em --out: native.rds = list(layout = "hw_matrix_nx48", values = Previsao15 n x 48 (O+1..O+n, sem arredondar),
## meta = list(day0_raw = $mean[1] das 48 series do artefato gravado = o passo cru do dia O que o oraculo descarta)),
## day0_raw.rds (o mesmo vetor), long.parquet (build_long_output do oraculo + coluna `timestamp` = target_datetime) e
## artifacts/ (artefato do oraculo).

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
  hist_days <- if (is.null(unit$history_days)) 730L else as.integer(unit$history_days)

  rdir <- file.path(oracle, "models", "holt-winters", "R")
  Sys.setenv(PREVCARGA_MODEL_R_DIR = rdir)   # so neste processo filho: o oraculo localiza o proprio R/ por esta variavel
  suppressPackageStartupMessages({ library(lubridate); library(data.table); library(arrow) })
  sys.source(file.path(rdir, "registry", "plugin_registry.R"), envir = globalenv())
  sys.source(file.path(rdir, "orchestrator", "orchestrator.R"), envir = globalenv())
  sys.source(file.path(oracle, "scripts", "lib", "build_long_output.R"), envir = globalenv())

  origin <- as.Date(unit$origin)
  art_dir <- file.path(out, "artifacts")
  res <- run_forecast(area = unit$area, origin_day = origin, history_start = origin - hist_days, horizon = 8L,
                      data_dir = unit$data_dir, family_id = "holt_winters", artifact_dir = art_dir, verbose = FALSE)
  p15 <- res$inference_result$previsao15

  rds <- list.files(art_dir, pattern = "^hw_artifact_.*[.]rds$", full.names = TRUE)
  if (length(rds) != 1L) stop("[prevcarga/contract] driver: esperado 1 artefato sob --out, achei ", length(rds))
  art <- readRDS(rds)
  day0_raw <- vapply(art$fits_interval, function(f) as.numeric(f$mean)[1L], numeric(1))
  if (length(day0_raw) != 48L) stop("[prevcarga/contract] driver: esperado 48 series em fits_interval")

  saveRDS(list(layout = "hw_matrix_nx48", values = p15, meta = list(day0_raw = day0_raw)), file.path(out, "native.rds"))
  saveRDS(day0_raw, file.path(out, "day0_raw.rds"))
  long <- build_long_output(p15, forecast_origin = origin, area = unit$area, data_dir = unit$data_dir, granularity_min = 30L)
  ## contrato do comparador P.0g: `timestamp` = INICIO do intervalo (naive); o build_long_output do oraculo o chama de
  ## `target_datetime` (mesmo valor): a coluna e acrescentada, as do oraculo ficam como estao
  long$timestamp <- long$target_datetime
  arrow::write_parquet(long, file.path(out, "long.parquet"))
  invisible(NULL)
})
