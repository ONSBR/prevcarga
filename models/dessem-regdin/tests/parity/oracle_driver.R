#!/usr/bin/env Rscript
## oracle_driver.R -- driver do ORACULO do dessem-regdin para scripts/tools/plugin_golden.R (RD.6). ASCII.
## Ferramenta de DESENVOLVIMENTO (so Linux, fora do bundle), chamada UNICAMENTE por plugin_golden.R em processo filho.
## Contrato do P.0f: --oracle <dir> --unit <unit.json> --out <dir> --available-until <ISO>; o oraculo e so lido/executado e tudo
## que ele grava cai sob --out (artefato com `artifact_dir` explicito em <out>/artifacts, apagado ao fim; nunca o models/ do oraculo).
##
## Reproduz o caminho SPLIT-NOWCAST de `jobs/run_job.sh:98-104,207-216` (dessem-*, retrain_harness) por unidade, como
## `run_pipeline.R --mode train` e `--mode infer`: `run_train(area, O, history_start = O - history_days, horizon = 9, available_until)`
## (a ingestao traz `mm` do prefixo de meias horas <= o corte; o fit usa a cabeca de nowcast com ele) seguido de `run_infer` com a
## MESMA janela e o MESMO corte (`mm` no predict). `available_until` e o do oraculo (`oracle_cut: start_label` => available_until
## - 30 min). A variante (MED/MAX) entra pela variavel EXOGENA do `oracle_env` (o oraculo a le em `.exogena_mode()`).
## Saidas em --out: native.rds = list(layout = "dessem_semihourly_48", values = `previsao15` (10 x 48, dias O..O+9), meta) e
## long.parquet (build_long_output do oraculo com first_forecast_day = dataPrev e origin_cut, + coluna `timestamp` = inicio do intervalo).
## Bracos INFORMATIVOS (`--arm`, fora do contrato do plugin_golden.R; nunca entram no golden.lock):
##   composition: o fit roda `.rd_fit_pass(mm = NULL)` (sem a cabeca de nowcast; o plano B do RNG) e o predict `.rd_predict_pass(mm)`.
##   cut0000:     o mesmo caminho nativo com o corte de O 00:00 (o prefixo do oraculo inclui o carimbo 00:00).

local({
  av <- commandArgs(trailingOnly = TRUE)
  opt <- function(name) { i <- which(av == name); if (length(i) && i[[1L]] < length(av)) av[[i[[1L]] + 1L]] else NULL }
  oracle <- opt("--oracle"); ujson <- opt("--unit"); out <- opt("--out"); cut <- opt("--available-until"); arm <- opt("--arm")
  if (is.null(oracle) || is.null(ujson) || is.null(out)) stop("[prevcarga/contract] driver: --oracle, --unit e --out sao obrigatorios")
  oracle <- normalizePath(oracle, winslash = "/", mustWork = TRUE)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(out, winslash = "/", mustWork = TRUE)
  if (identical(out, oracle) || startsWith(paste0(out, "/"), paste0(oracle, "/")))
    stop("[prevcarga/contract] golden: --out dentro do oraculo")
  if (!is.null(arm) && !(arm %in% c("composition", "cut0000"))) stop("[prevcarga/contract] driver: --arm composition|cut0000")
  unit <- jsonlite::read_json(ujson, simplifyVector = TRUE)
  if (is.null(unit$data_dir) || !nzchar(unit$data_dir)) stop("[prevcarga/contract] driver: informe --data ao plugin_golden.R")
  if (is.null(unit$history_days)) stop("[prevcarga/contract] driver: history_days e obrigatorio (O - history_start)")
  if (is.null(cut) || !nzchar(cut)) stop("[prevcarga/contract] driver: --available-until e obrigatorio (corte intradiario do oraculo)")
  origin <- as.Date(unit$origin)
  if (identical(arm, "cut0000")) cut <- paste0(format(origin), "T00:00")
  ## "YYYY-MM-DDTHH:MM" (contrato do P.0f) -> "YYYY-MM-DD HH:MM:00" (formato que o oraculo le)
  cut_txt <- paste0(sub("T", " ", cut, fixed = TRUE), ":00")
  variant <- if (is.null(unit$variant)) "MED" else unit$variant

  rdir <- file.path(oracle, "models", "dessem-regdin", "R")
  Sys.setenv(PREVCARGA_MODEL_R_DIR = rdir)          # so neste processo filho: o oraculo localiza o proprio R/ por esta variavel
  suppressPackageStartupMessages({ library(lubridate); library(data.table); library(arrow) })
  sys.source(file.path(rdir, "orchestrator", "orchestrator.R"), envir = globalenv())
  sys.source(file.path(oracle, "scripts", "lib", "build_long_output.R"), envir = globalenv())
  sys.source(file.path(oracle, "scripts", "lib", "validate_input.R"), envir = globalenv())
  sys.source(file.path(oracle, "scripts", "lib", "normalize_holiday_codes.R"), envir = globalenv())
  sys.source(file.path(oracle, "scripts", "lib", "fill_forecast_tail.R"), envir = globalenv())

  if (identical(arm, "composition")) {
    .orch_source_split_rd(rdir)                     # o fit/predict do split entram antes; o passe de fit e trocado por um wrapper
    .rd_fit_orig <- get(".rd_fit_pass", envir = globalenv())
    assign(".rd_fit_pass", function(processado, config_mdl, seed = 42L, mm = NULL, origin = NULL)
      .rd_fit_orig(processado, config_mdl, seed = seed, mm = NULL, origin = NULL), envir = globalenv())
  }

  hist_start <- origin - as.integer(unit$history_days)
  art_dir <- file.path(out, "artifacts")
  dir.create(art_dir, recursive = TRUE, showWarnings = FALSE)
  common <- list(area = unit$area, origin_day = origin, history_start = hist_start, horizon = 9L, data_dir = unit$data_dir,
                 artifact_dir = art_dir, config_overrides = list(available_until = cut_txt), verbose = FALSE)
  invisible(suppressMessages(do.call(run_train, common)))
  inf <- suppressMessages(do.call(run_infer, common))
  p15 <- inf$inference_result$previsao15
  if (!is.matrix(p15) || ncol(p15) != 48L || nrow(p15) != 10L)
    stop("[prevcarga/contract] driver: previsao15 deveria ser 10 x 48 (O..O+9), veio ", paste(dim(p15), collapse = "x"))
  if (!identical(as.Date(inf$deck$dataPrev), origin))
    stop("[prevcarga/contract] driver: o corte intradiario deveria ancorar dataPrev em O (modo nowcast)")

  saveRDS(list(layout = "dessem_semihourly_48", values = p15,
               meta = list(dataPrev = as.Date(inf$deck$dataPrev), n_steps = nrow(p15), origin_cut = format(inf$deck$origin_cut),
                           model_key = inf$config$model_key, exogena = inf$config$exogena, seed = 42L, variant = variant,
                           arm = if (is.null(arm)) "native" else arm)),
          file.path(out, "native.rds"))
  long <- build_long_output(p15, forecast_origin = origin, area = unit$area, data_dir = unit$data_dir, granularity_min = 30L,
                            first_forecast_day = inf$deck$dataPrev, origin_cut = inf$deck$origin_cut)
  ## contrato do comparador P.0g: `timestamp` = INICIO do intervalo (naive); o do oraculo e `target_datetime` (mesmo valor)
  long$timestamp <- long$target_datetime
  arrow::write_parquet(long, file.path(out, "long.parquet"))
  unlink(art_dir, recursive = TRUE)                  # o artefato do oraculo nao e golden (so previsao15 e long)
  invisible(NULL)
})
