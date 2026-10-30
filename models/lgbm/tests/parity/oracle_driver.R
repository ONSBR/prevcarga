## oracle_driver.R -- driver do ORACULO lgbm para o gerador de golden (plugin_golden.R, P.0f). Ferramenta de DESENVOLVIMENTO
## (so Linux): roda como processo FILHO do plugin_golden.R (que ja injetou o `oracle_env` da unidade), nunca faz parte do pacote.
##
##   Rscript --vanilla oracle_driver.R --unit <unit.json> --oracle <dir> --out <dir> --available-until <ISO>
##
## Protocolo daily do oraculo (jobs/run_job.sh): treino com `--origin = cuts$train$train_origin` (O-2) e history_start O-2-730,
## depois infer em O com history_start O-45 e `--available-until` (ja deslocado -30 min pelo gerador quando `oracle_cut:
## start_label`: O 08:00 = rotulo de INICIO do ultimo intervalo real). Treino e infer rodam em processo (run_pipeline do
## orchestrator.R, sem o `.main()` do run_pipeline.R); o long.parquet e a saida do CLI do oraculo (`run_pipeline.R --mode infer
## --out`) sobre o MESMO artefato. Grava SO sob --out: native.rds (layout, values 2x48 = linha O completada + O+1, completed_D,
## d1, meta$artifact_tables = sazonalidade/min_max_BLF/colunas do artefato e rotulos de calendario do quadro de infer),
## long.parquet e o artefato fica no scratch. O oraculo e so lido: cwd e scratch ficam em tempdir.
## Saida: 0 ok; 1 oraculo falhou; 2 argumento/unidade invalida ou --out dentro do oraculo (antes de qualquer execucao).

.lg_driver_fail <- function(msg, status) { message("prevcarga: driver: ", msg); quit(save = "no", status = status) }

.lg_driver_args <- function(argv = commandArgs(trailingOnly = TRUE)) {
  get <- function(k) { i <- match(k, argv); if (is.na(i) || i == length(argv)) NA_character_ else argv[i + 1L] }
  a <- list(unit = get("--unit"), oracle = get("--oracle"), out = get("--out"), cut = get("--available-until"))
  if (anyNA(unlist(a))) .lg_driver_fail("argumento ausente (--unit, --oracle, --out, --available-until)", 2L)
  if (!file.exists(a$unit) || !dir.exists(a$oracle)) .lg_driver_fail("--unit ou --oracle inexistente", 2L)
  oracle <- normalizePath(a$oracle, winslash = "/", mustWork = TRUE)
  dir.create(a$out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(a$out, winslash = "/", mustWork = TRUE)
  if (identical(out, oracle) || startsWith(paste0(out, "/"), paste0(oracle, "/")))
    .lg_driver_fail("--out dentro do oraculo", 2L)
  a$oracle <- oracle; a$out <- out
  a$unit <- tryCatch(jsonlite::read_json(a$unit, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(a$unit)) .lg_driver_fail("unit.json ilegivel", 2L)
  a
}

## unidade valida = os campos que o protocolo usa (cortes por fase literais, area, data_dir, available_until do oraculo)
.lg_driver_unit <- function(u, cut) {
  chr <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  day <- function(x) chr(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) && !is.na(as.Date(x, optional = TRUE))
  ok <- chr(u$area) && grepl("^[A-Za-z0-9_]+$", u$area) && day(u$origin) && chr(u$data_dir) &&
    day(u$cuts$train$train_origin) && day(u$cuts$train$history_start) && day(u$cuts$infer$history_start) &&
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}$", cut)
  if (!ok) .lg_driver_fail("unidade invalida (area/origin/data_dir/cuts/available-until)", 2L)
  u
}

## ordem do oraculo: treino em O-2, depois infer em O com o mesmo artefato (artifact_path explicito: nenhum ponteiro `latest`)
.lg_oracle_load <- function(a) {
  rdir <- file.path(a$oracle, "models", "lgbm", "R")
  suppressPackageStartupMessages({ library(arrow); library(data.table); library(lubridate); library(lightgbm); library(R6) })
  Sys.setenv(PREVCARGA_MODEL_R_DIR = rdir)   # so neste processo filho: o oraculo localiza o proprio R/ por esta variavel
  invisible(utils::capture.output(source(file.path(rdir, "utils.R"), local = FALSE)))
  invisible(utils::capture.output(source(file.path(rdir, "orchestrator", "orchestrator.R"), local = FALSE)))
}

.lg_driver_run <- function(a, scratch) {
  u <- a$unit
  .lg_oracle_load(a)
  art <- file.path(scratch, "artifact")
  au <- lubridate::ymd_hm(sub("T", " ", a$cut, fixed = TRUE), tz = "America/Sao_Paulo")   # so o relogio de parede conta (data_adapter.R)
  O <- as.Date(u$origin)
  hist_i <- as.Date(u$cuts$infer$history_start)
  quiet <- function(x) { r <- NULL; utils::capture.output(r <- x); r }
  quiet(run_pipeline("train", u$area, as.Date(u$cuts$train$train_origin), as.Date(u$cuts$train$history_start),
                     data_dir = u$data_dir, artifact_path = art))
  res <- quiet(run_pipeline("infer", u$area, O, hist_i, data_dir = u$data_dir, artifact_path = art, available_until = au))
  pred <- res$predictions
  cd <- attr(pred, "completed_D")
  if (is.null(cd) || length(cd$mw) != 48L) .lg_driver_fail("o oraculo nao devolveu completed_D (48): corte sem trecho sintetico em O", 1L)
  d1 <- pred$y_pred[order(pred$target_datetime)]   # mesma re-ordenacao do run_pipeline.R antes do build_long_output
  values <- rbind(as.numeric(cd$mw), as.numeric(d1))
  dimnames(values) <- NULL
  stopifnot(identical(dim(values), c(2L, 48L)))
  saveRDS(list(layout = "lgbm_day_48", values = values, completed_D = as.numeric(cd$mw), d1 = as.numeric(d1),
               meta = list(artifact_tables = .lg_artifact_tables(art, u, O, hist_i, au))), file.path(a$out, "native.rds"))
  .lg_long_cli(a, art)
}

## tabelas do artefato (G-SAZ) e rotulos de calendario do quadro de infer (G-CAL), pelas funcoes do proprio oraculo
.lg_artifact_tables <- function(art, u, O, hist_i, au) {
  ar <- load_artifact(art)
  desc <- model_registry$get("lgbm_v2")
  dt <- NULL
  utils::capture.output(dt <- desc$new_ingestion_plugin()$load(list(area = u$area, origin_day = O, history_start = hist_i,
                          available_until = au, data_dir = u$data_dir, mode = "infer")))
  cols <- intersect(c("Data", "DiaSemanaNum", "FimDeSemana", "Feriado", "DiaEspecial", "TipoDia", "DiaEspecialFimAno",
                      "PreFeriado", "PosFeriado"), names(dt))
  cal <- as.data.frame(unique(dt[, cols, with = FALSE], by = "Data"))
  cal <- cal[order(cal$Data), , drop = FALSE]
  rownames(cal) <- NULL
  plain <- function(x) if (is.null(x)) NULL else { x <- as.data.frame(x); attr(x, ".internal.selfref") <- NULL; x }
  fb <- ar$sazonalidade_fallbacks
  list(sazonalidade = plain(ar$sazonalidade),
       sazonalidade_fallbacks = if (is.null(fb)) NULL else lapply(fb, plain),
       min_max_BLF = ar$min_max_BLF, colunas_entrada_BLF = ar$colunas_entrada_BLF,
       colunas_nao_normalizar = ar$colunas_nao_normalizar, best_iter = as.integer(ar$best_iter), calendario = cal)
}

## long.parquet = saida do CLI do oraculo (`--out`), sobre o mesmo artefato; Rscript do proprio filho (mesmo R e mesmo env)
.lg_long_cli <- function(a, art) {
  u <- a$unit
  rp <- file.path(a$oracle, "models", "lgbm", "run_pipeline.R")
  long <- file.path(a$out, "long.parquet")
  args <- c("--vanilla", shQuote(rp), "--mode", "infer", "--area", u$area, "--origin", u$origin,
            "--history-start", u$cuts$infer$history_start, "--data-dir", shQuote(u$data_dir), "--artifact", shQuote(art),
            "--available-until", shQuote(sub("T", " ", paste0(a$cut, ":00"), fixed = TRUE)), "--out", shQuote(long))
  st <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), args, stdout = TRUE, stderr = TRUE))
  if ((!is.null(attr(st, "status")) && attr(st, "status") != 0L) || !file.exists(long)) {
    message(paste(utils::tail(st, 15L), collapse = "\n"))
    .lg_driver_fail("o CLI do oraculo falhou", 1L)
  }
}

.lg_driver_main <- function() {
  a <- .lg_driver_args()
  a$unit <- .lg_driver_unit(a$unit, a$cut)
  scratch <- tempfile("lgbm-oracle-scratch-")
  dir.create(scratch)
  if (startsWith(paste0(normalizePath(scratch, winslash = "/"), "/"), paste0(a$oracle, "/")))
    .lg_driver_fail("scratch dentro do oraculo", 2L)
  old <- setwd(scratch)
  on.exit({ setwd(old); unlink(scratch, recursive = TRUE) }, add = TRUE)
  tryCatch(.lg_driver_run(a, scratch), error = function(e) .lg_driver_fail(paste("o oraculo falhou:", conditionMessage(e)), 1L))
  quit(save = "no", status = 0L)
}

if (!interactive()) .lg_driver_main()
