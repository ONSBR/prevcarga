## ab_relayout.R -- relayout da referencia do oraculo para o LONG nativo de 8 colunas (M8.8; ADR-0002, ADR-0035). Dev-only, so Linux.
## Requer carregados: errors.R, schema_long.R, e arrow/jsonlite/digest/lubridate. Datas so por lubridate com tz explicito.
##
## O oraculo grava `ref/A/results/<mdir>/<AREA>/<RUN_ID>/long.parquet` com 8 colunas, forecast_origin como Date e target_datetime rotulado pelo
## INICIO do intervalo, em digitos civis ingenuos (sem fuso). O relayout (1) declara o fuso da carga, (2) re-rotula cada instante para
## `interval_end` (+granularity_min: +30 min nos semi-horarios, +60 nos horarios; a unica aritmetica de rotulo e a borda do LONG,
## schema_long.R), (3) recalcula `lead = d_plus * 48 + slot(interval_start)` (a mesma regra do build_long_output.R) e grava
## `<out>/<origem>/membros/<sigla>/<area>.parquet` com o manifest.json da arvore (arquivos[] = {path, area, linhas, sha256}).
## O layout do oraculo nunca entra no produto. `relabel = FALSE` (so para o CONTROL) mantem o rotulo original do oraculo.

pc_ab_relabel <- function(dt, tz_load, relabel = TRUE) {
  d <- as.data.frame(dt)
  gm <- as.integer(d$granularity_min)
  start <- lubridate::force_tz(as.POSIXct(d$target_datetime), tzone = tz_load)       # digitos civis do oraculo = inicio do intervalo
  fo <- as.character(lubridate::as_date(d$forecast_origin))
  d_plus <- as.integer(lubridate::as_date(start, tz = tz_load) - lubridate::as_date(fo))
  slot <- (lubridate::hour(start) * 60L + lubridate::minute(start)) %/% 30L + 1L
  ts <- start
  if (relabel) {
    ts <- start
    s30 <- gm == 30L
    if (any(s30)) ts[s30] <- prevcarga_long_target_datetime(start[s30], 30L, tz_load)
    if (any(!s30)) ts[!s30] <- start[!s30] + lubridate::minutes(gm[!s30])
  }
  out <- data.frame(area = as.character(d$area), forecast_origin = fo, target_datetime = ts, lead = as.integer(d_plus * 48L + slot),
                    y_pred = as.double(d$y_pred), y_true = as.double(d$y_true), d_plus = d_plus, granularity_min = gm,
                    stringsAsFactors = FALSE)
  prevcarga_schema_long_check(out)
  out
}

.pc_ab_write_parquet <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, path, compression = "snappy", version = "2.6", use_dictionary = FALSE)
}

## `manifest.json` da arvore: arquivos[] em ordem radix. Regerado a cada gravacao (o CONTROL "+0,2 MW com manifest regerado" o usa).
pc_ab_manifest_write <- function(dir) {
  fs <- sort(setdiff(list.files(dir, recursive = TRUE, all.files = FALSE), c("manifest.json", "run_manifest.json")), method = "radix")
  ent <- lapply(fs, function(p) list(path = p, area = sub("[.]parquet$", "", basename(p)),
                                     sha256 = digest::digest(file = file.path(dir, p), algo = "sha256")))
  jsonlite::write_json(list(schema_version = 1L, arquivos = ent), file.path(dir, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(fs)
}

## ref_dir = <ref_out>/A (index.tsv do runner); out_dir recebe uma arvore por origem. Linhas sem resultado (rc != 0 ou arquivo ausente) sao
## devolvidas em `faltando` (o comparador as reporta como FAIL de entrada, nunca como Delta = 0).
pc_ab_relayout <- function(ref_dir, out_dir, ctx, relabel = TRUE) {
  idx <- utils::read.delim(file.path(ref_dir, "index.tsv"), stringsAsFactors = FALSE, colClasses = "character", na.strings = character())
  tz <- ctx$tz
  if (!is.character(tz) || length(tz) != 1L) prevcarga_abort("contract.ab_stage_input", where = "ctx$tz")
  falt <- character()
  for (i in seq_len(nrow(idx))) {
    r <- idx[i, ]
    f <- file.path(ref_dir, r$result)
    if (!identical(r$rc, "0") || !nzchar(r$result) || !file.exists(f)) { falt <- c(falt, r$caso); next }
    long <- suppressWarnings(arrow::read_parquet(f))        # o arrow 11 do oraculo grava metadata R que o arrow novo nao desserializa (aviso)
    .pc_ab_write_parquet(pc_ab_relabel(long, tz, relabel), file.path(out_dir, r$origem, "membros", r$sigla, paste0(r$area, ".parquet")))
  }
  for (o in list.dirs(out_dir, recursive = FALSE, full.names = TRUE)) pc_ab_manifest_write(o)
  invisible(list(out = out_dir, faltando = falt, n = nrow(idx) - length(falt)))
}
