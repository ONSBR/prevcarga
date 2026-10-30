## oracle_driver.R -- driver do ORACULO random-forest para o gerador de golden (plugin_golden.R, P.0f). Ferramenta de
## DESENVOLVIMENTO (so Linux): roda como processo FILHO do plugin_golden.R, nunca faz parte do pacote.
##
##   Rscript --vanilla oracle_driver.R --unit <unit.json> --oracle <dir> --out <dir> [--available-until <ISO>]
##
## Protocolo daily do oraculo (run_job.sh:341-399): treino em O-10 (= O-1-H) UMA vez; infer em O e em O-1 com o MESMO
## artefato e o MESMO history_start absoluto. `--available-until` e aceito e ignorado (o RF do oraculo nao o le,
## run_pipeline.R:238-241). Grava SO sob --out: artifact.rds, long_O.parquet, long_Om1.parquet, native.rds (matriz 2 x 216,
## linhas O-1 e O, colunas y_h001..y_h216) e invocation.json. O oraculo e so lido: cwd e scratch ficam em tempdir.
## Saida: 0 ok; 1 oraculo falhou; 2 argumento/unidade invalida ou --out dentro do oraculo (antes de qualquer execucao).

.rf_driver_fail <- function(msg, status) { message("prevcarga: driver: ", msg); quit(save = "no", status = status) }

.rf_driver_args <- function(argv = commandArgs(trailingOnly = TRUE)) {
  get <- function(k) { i <- match(k, argv); if (is.na(i) || i == length(argv)) NA_character_ else argv[i + 1L] }
  a <- list(unit = get("--unit"), oracle = get("--oracle"), out = get("--out"))
  if (anyNA(unlist(a))) .rf_driver_fail("argumento ausente (--unit, --oracle, --out)", 2L)
  if (!file.exists(a$unit) || !dir.exists(a$oracle)) .rf_driver_fail("--unit ou --oracle inexistente", 2L)
  oracle <- normalizePath(a$oracle, winslash = "/", mustWork = TRUE)
  dir.create(a$out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(a$out, winslash = "/", mustWork = TRUE)
  if (identical(out, oracle) || startsWith(paste0(out, "/"), paste0(oracle, "/")))
    .rf_driver_fail("--out dentro do oraculo", 2L)
  a$oracle <- oracle; a$out <- out
  a$unit <- tryCatch(jsonlite::read_json(a$unit, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(a$unit)) .rf_driver_fail("unit.json ilegivel", 2L)
  a
}

## unidade valida = os campos que o protocolo usa (cortes por fase literais, ntree, area, data_dir)
.rf_driver_unit <- function(u) {
  chr <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  day <- function(x) chr(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) && !is.na(as.Date(x, optional = TRUE))
  ok <- chr(u$area) && grepl("^[A-Za-z0-9_]+$", u$area) && day(u$origin) && chr(u$data_dir) &&
    day(u$cuts$train$train_origin) && day(u$cuts$train$history_start) && day(u$cuts$infer$history_start) &&
    is.numeric(u$params$ntree) && length(u$params$ntree) == 1L && u$params$ntree >= 1
  if (!ok) .rf_driver_fail("unidade invalida (area/origin/data_dir/cuts/params$ntree)", 2L)
  u
}

.rf_driver_run <- function(a) {
  u <- a$unit
  rp <- file.path(a$oracle, "models", "random-forest", "run_pipeline.R")
  rscript <- file.path(R.home("bin"), "Rscript")
  art <- file.path(a$out, "artifact.rds")
  O <- as.Date(u$origin)
  run <- function(args) {
    st <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(rp), args), stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(st, "status")) && attr(st, "status") != 0L) {
      message(paste(utils::tail(st, 15L), collapse = "\n"))
      .rf_driver_fail("o oraculo falhou", 1L)
    }
    invisible(st)
  }
  common <- c("--area", u$area, "--data-dir", shQuote(u$data_dir))
  run(c("--mode", "train", common, "--origin", u$cuts$train$train_origin, "--history-start", u$cuts$train$history_start,
        "--ntree", as.character(as.integer(u$params$ntree)), "--artifact", shQuote(art)))
  for (T in list(O, O - 1L))
    run(c("--mode", "infer", common, "--origin", format(T), "--history-start", u$cuts$infer$history_start,
          "--artifact", shQuote(art), "--out", shQuote(file.path(a$out, if (T == O) "long_O.parquet" else "long_Om1.parquet"))))
  saveRDS(.rf_oracle_native(a, art, O), file.path(a$out, "native.rds"))
}

## values 2 x 216 (linhas O-1 e O) = result$predictions do run_pipeline do oraculo (predict_rf do artefato salvo), carregado
## a partir de R/orchestrator/orchestrator.R. PREVCARGA_MODEL_R_DIR e o contrato do proprio oraculo (so este filho).
.rf_oracle_native <- function(a, art, O) {
  u <- a$unit
  mdir <- file.path(a$oracle, "models", "random-forest")
  suppressPackageStartupMessages({ library(arrow); library(data.table); library(lubridate); library(randomForest); library(R6); library(dplyr) })
  Sys.setenv(PREVCARGA_MODEL_R_DIR = file.path(mdir, "R"))
  invisible(utils::capture.output(source(file.path(mdir, "R", "orchestrator", "orchestrator.R"), local = FALSE)))
  rows <- lapply(list(O - 1L, O), function(T) {
    res <- NULL
    utils::capture.output(res <- run_pipeline(model_family = "random_forest", area = u$area, forecast_origin = T,
                          history_start = as.Date(u$cuts$infer$history_start), horizon = 9L, mode = "inference",
                          data_dir = u$data_dir, artifact_path = art))
    p <- as.data.frame(res$predictions)
    as.numeric(p[1L, grep("^y_h[0-9]{3}$", names(p))])
  })
  m <- do.call(rbind, rows)
  dimnames(m) <- list(format(c(O - 1L, O)), sprintf("y_h%03d", seq_len(ncol(m))))
  stopifnot(identical(dim(m), c(2L, 216L)))
  list(layout = "rf_wide_hourly_216", values = m, meta = list())
}

.rf_driver_main <- function() {
  a <- .rf_driver_args()
  a$unit <- .rf_driver_unit(a$unit)
  scratch <- tempfile("rf-oracle-scratch-")
  dir.create(scratch)
  if (startsWith(paste0(normalizePath(scratch, winslash = "/"), "/"), paste0(a$oracle, "/")))
    .rf_driver_fail("scratch dentro do oraculo", 2L)
  old <- setwd(scratch)
  on.exit({ setwd(old); unlink(scratch, recursive = TRUE) }, add = TRUE)
  .rf_driver_run(a)
  commit <- suppressWarnings(system2("git", c("--no-optional-locks", "-C", shQuote(a$oracle), "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE))
  inv <- list(unit = a$unit$id, oracle_commit = commit[[1L]], r_version = R.version.string,
              train = list(origin = a$unit$cuts$train$train_origin, history_start = a$unit$cuts$train$history_start,
                           ntree = a$unit$params$ntree),
              infer = list(origins = c(format(as.Date(a$unit$origin) - 1L), a$unit$origin),
                           history_start = a$unit$cuts$infer$history_start))
  writeLines(jsonlite::toJSON(inv, auto_unbox = TRUE, pretty = TRUE), file.path(a$out, "invocation.json"))
  quit(save = "no", status = 0L)
}

if (!interactive()) .rf_driver_main()
