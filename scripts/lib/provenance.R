## provenance.R -- proveniencia + freeze (M1.3, CONTRATO; ADR-0011)
##
## Contrato que M4/M5/Reconcile/plugins herdam, sem `version = format(Sys.time())` ad-hoc.
##   - version CONTENT-ADDRESSED = `v_<16 hex>` do sha256 do JSON canonico de EXATAMENTE 8 chaves:
##     model_id, data_fingerprint, cutoff, feature_config, code_commit, renv_lock_hash, load_label, load_tz.
##     model_id + feature_config (incl. exogena) evitam a colisao RGD_MED x LIN_MED (ADR-0021). Chave ausente ou
##     extra => contract; laterais (trained_at/package_version/run_id) dentro do fingerprint => contract.
##   - canonicalizacao RECURSIVA: named lists ordenadas (radix) em todos os niveis; Date/POSIXct em ISO-8601
##     (POSIXct sempre em UTC); NULL e NA viram null.
##   - run-manifest: chaves deterministicas + laterais (trained_at, package_version, run_id) fora de qualquer hash.
##     `origin` != corte: NUNCA existe `cutoff` run-level (o corte de treino e por modelo, no fingerprint; ADR-0023).
##   - freeze: saveRDS v3 BINARIO (IEEE-754) atomico (tmp no mesmo dir + file.rename) + `<p>.manifest.json` com sha256.
##     thaw confere o sha256 ANTES de desserializar. Integridade != autenticidade (nao ha assinatura).
##
## Sem variaveis de ambiente: raiz sempre por argumento. Excecao dev documentada: `code_commit` usa `git`
## (system2) so quando ha repositorio; senao 'unknown'. Requer errors.R (e topology.R/runtime.R no run-manifest).

.prevcarga_fingerprint_keys <- c("model_id", "data_fingerprint", "cutoff", "feature_config",
  "code_commit", "renv_lock_hash", "load_label", "load_tz")
.prevcarga_fingerprint_laterals <- c("trained_at", "package_version", "run_id")

## --- Canonicalizacao e hash --------------------------------------------------------------------------
.prevcarga_canon <- function(x) {
  if (is.null(x)) return(NA)
  if (inherits(x, "POSIXct")) return(lubridate::format_ISO8601(lubridate::with_tz(x, "UTC"), usetz = TRUE))
  if (inherits(x, "Date")) return(lubridate::format_ISO8601(x))
  if (is.factor(x)) return(as.character(x))
  if (!is.list(x)) return(x)
  nm <- names(x)
  if (is.null(nm)) return(lapply(x, .prevcarga_canon))
  if (any(!nzchar(nm))) prevcarga_abort("contract.fingerprint_partially_named")
  x <- x[sort(nm, method = "radix")]
  lapply(x, .prevcarga_canon)
}

.prevcarga_canon_json <- function(x) {
  as.character(jsonlite::toJSON(.prevcarga_canon(x), auto_unbox = TRUE, digits = NA,
                                null = "null", na = "null"))
}

.prevcarga_sha256 <- function(txt) digest::digest(txt, algo = "sha256", serialize = FALSE)

prevcarga_version_hash <- function(fingerprint) {
  nm <- names(fingerprint)
  if (!is.list(fingerprint) || is.null(nm) || any(!nzchar(nm)))
    prevcarga_abort("contract.fingerprint_partially_named")
  lat <- intersect(.prevcarga_fingerprint_laterals, nm)
  if (length(lat)) prevcarga_abort("contract.fingerprint_lateral", where = lat[[1L]])
  miss <- setdiff(.prevcarga_fingerprint_keys, nm)
  if (length(miss)) prevcarga_abort("contract.fingerprint_missing_key", where = miss[[1L]])
  extra <- setdiff(nm, .prevcarga_fingerprint_keys)
  if (length(extra)) prevcarga_abort("contract.fingerprint_extra_key", where = extra[[1L]])
  paste0("v_", substr(.prevcarga_sha256(.prevcarga_canon_json(fingerprint)), 1L, 16L))
}

## --- Insumos de proveniencia (por argumento, sem env) -------------------------------------------------
prevcarga_code_commit <- function(root) {
  git <- function(args) tryCatch(suppressWarnings(
    system2("git", c("-C", shQuote(root), args), stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL)
  head <- git(c("rev-parse", "HEAD"))
  if (is.null(head) || !is.null(attr(head, "status")) || length(head) != 1L ||
      !grepl("^[0-9a-f]{40}$", head)) return("unknown")
  st <- git(c("status", "--porcelain"))
  if (!is.null(st) && is.null(attr(st, "status")) && length(st)) paste0(head, "-dirty") else head
}

prevcarga_renv_lock_hash <- function(root) {
  f <- file.path(root, "renv.lock")
  if (!file.exists(f)) return(NULL)
  digest::digest(file = f, algo = "sha256")
}

## Definicao unica do hash do conjunto de membros (consumida por M4): linhas (sigla, mdir, exogena) na ordem declarada.
prevcarga_member_set_hash <- function(members) {
  rows <- lapply(seq_len(nrow(members)), function(i)
    list(sigla = members$sigla[[i]], mdir = members$mdir[[i]],
         exogena = if (is.na(members$exogena[[i]])) NULL else members$exogena[[i]]))
  .prevcarga_sha256(.prevcarga_canon_json(rows))
}

## --- Run-manifest -------------------------------------------------------------------------------------
.prevcarga_manifest_reserved <- c("cutoff", "data_hash", "origin", "available_until",
  "load_label", "load_tz", "weather_issue_cutoff", "split", "delivery_days", "member_set_hash",
  "model_versions", "seeds", "code_commit", "renv_lock_hash", "platform", "blas_threads", "tz", "lc",
  "bundle_id")

.prevcarga_check_laterals <- function(extra) {
  bad <- intersect(names(extra), .prevcarga_manifest_reserved)
  if (length(bad)) prevcarga_abort("contract.run_manifest_forbidden_key", where = bad[[1L]])
  if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra)))))
    prevcarga_abort("contract.run_manifest_bad_value", where = "...")
  invisible(extra)
}

.prevcarga_bad <- function(key) prevcarga_abort("contract.run_manifest_bad_value", where = key)
.prevcarga_whole <- function(x, min = 1) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x) && x >= min
}
.prevcarga_chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.prevcarga_named_list <- function(x) is.list(x) && length(x) > 0L && !is.null(names(x)) && all(nzchar(names(x)))

prevcarga_run_manifest <- function(data_hash, origin, available_until = NULL, load_label, load_tz,
                                   weather_issue_cutoff, split, delivery_days, model_versions, seeds,
                                   bundle_id = NULL, root, trained_at = NULL, package_version = NULL,
                                   run_id = NULL, members = NULL, ...) {
  .prevcarga_check_laterals(list(...))
  if (!(is.character(data_hash) && length(data_hash) == 1L && grepl("^[0-9a-f]{64}$", data_hash)))
    .prevcarga_bad("data_hash")
  if (!(inherits(origin, "Date") && length(origin) == 1L && !is.na(origin))) .prevcarga_bad("origin")
  if (!is.null(available_until) &&
      !(inherits(available_until, "POSIXct") && length(available_until) == 1L && !is.na(available_until)))
    .prevcarga_bad("available_until")
  if (!(.prevcarga_chr1(load_label) && load_label %in% c("end", "start"))) .prevcarga_bad("load_label")
  if (!(.prevcarga_chr1(load_tz) && load_tz %in% OlsonNames())) .prevcarga_bad("load_tz")
  if (!(.prevcarga_chr1(weather_issue_cutoff) &&
        weather_issue_cutoff %in% c("available_until", "origin_midnight_utc")))
    .prevcarga_bad("weather_issue_cutoff")
  if (!(.prevcarga_chr1(split) && split %in% c("origin_agnostic", "origin_scoped"))) .prevcarga_bad("split")
  dd_ok <- if (is.list(delivery_days)) .prevcarga_named_list(delivery_days) &&
             all(vapply(delivery_days, .prevcarga_whole, NA)) else .prevcarga_whole(delivery_days)
  if (!dd_ok) .prevcarga_bad("delivery_days")
  if (!(.prevcarga_named_list(model_versions) &&
        all(vapply(model_versions, function(v) is.character(v) && length(v) == 1L &&
                     grepl("^v_[0-9a-f]{16}$", v), NA)))) .prevcarga_bad("model_versions")
  if (!(.prevcarga_named_list(seeds) && all(vapply(seeds, .prevcarga_whole, NA, min = -Inf)))) .prevcarga_bad("seeds")
  if (!is.null(bundle_id) && !(is.character(bundle_id) && length(bundle_id) == 1L &&
                               grepl("^[0-9a-f]{64}$", bundle_id))) .prevcarga_bad("bundle_id")
  if (!is.null(trained_at) && !(inherits(trained_at, "POSIXct") && length(trained_at) == 1L) &&
      !.prevcarga_chr1(trained_at)) .prevcarga_bad("trained_at")
  if (!is.null(package_version) && !.prevcarga_chr1(package_version)) .prevcarga_bad("package_version")
  if (!is.null(run_id) && !.prevcarga_chr1(run_id)) .prevcarga_bad("run_id")

  stamp <- prevcarga_runtime_stamp()
  if (is.null(members)) members <- prevcarga_members(root = root)
  iso <- function(x) if (is.null(x)) NULL else if (is.character(x)) x else .prevcarga_canon(x)
  as_int <- function(x) if (is.list(x)) lapply(x, as.integer) else as.integer(x)

  c(list(
    data_hash = data_hash,
    origin = .prevcarga_canon(origin),
    available_until = iso(available_until),
    load_label = load_label,
    load_tz = load_tz,
    weather_issue_cutoff = weather_issue_cutoff,
    split = split,
    delivery_days = as_int(delivery_days),
    member_set_hash = prevcarga_member_set_hash(members),
    model_versions = model_versions,
    seeds = lapply(seeds, as.integer),
    code_commit = prevcarga_code_commit(root),
    renv_lock_hash = prevcarga_renv_lock_hash(root),
    platform = R.version$platform,
    blas_threads = as.integer(stamp$blas_threads),
    tz = stamp$tz,
    lc = stamp$lc,
    bundle_id = bundle_id,
    trained_at = iso(trained_at),
    package_version = package_version,
    run_id = run_id),
    list(...))
}

.prevcarga_write_atomic <- function(txt, dest) {
  tmp <- tempfile(pattern = ".atomic-", tmpdir = dirname(dest), fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(txt, tmp, useBytes = TRUE)
  if (!file.rename(tmp, dest)) prevcarga_abort("contract.freeze_dir_missing", where = dest)
  invisible(dest)
}

prevcarga_write_run_manifest <- function(m, run_dir) {
  if (!dir.exists(run_dir)) prevcarga_abort("contract.freeze_dir_missing", where = run_dir)
  .prevcarga_write_atomic(.prevcarga_canon_json(m), file.path(run_dir, "run_manifest.json"))
}

prevcarga_read_run_manifest <- function(run_dir) {
  jsonlite::read_json(file.path(run_dir, "run_manifest.json"), simplifyVector = TRUE)
}

## --- freeze / thaw --------------------------------------------------------------------------------------
.prevcarga_freeze_manifest <- function(obj, sha) {
  n <- if (is.data.frame(obj)) list(nrows = nrow(obj)) else list(length = length(obj))
  .prevcarga_canon_json(c(list(sha256 = sha, class = class(obj)), n, list(format_version = 1L)))
}

prevcarga_freeze <- function(obj, path) {
  d <- dirname(path)
  if (!dir.exists(d)) prevcarga_abort("contract.freeze_dir_missing", where = d)
  tmp <- tempfile(pattern = ".freeze-", tmpdir = d, fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(obj, tmp, version = 3)
  sha <- digest::digest(file = tmp, algo = "sha256")
  if (!file.rename(tmp, path)) prevcarga_abort("contract.freeze_dir_missing", where = path)
  .prevcarga_write_atomic(.prevcarga_freeze_manifest(obj, sha), paste0(path, ".manifest.json"))
  invisible(sha)
}

prevcarga_thaw <- function(path) {
  if (!file.exists(path)) prevcarga_abort("integrity.freeze_file_missing", where = path)
  mf <- paste0(path, ".manifest.json")
  if (!file.exists(mf)) prevcarga_abort("integrity.freeze_manifest_missing", where = mf)
  want <- tryCatch(jsonlite::read_json(mf, simplifyVector = TRUE)$sha256, error = function(e) NULL)
  got <- digest::digest(file = path, algo = "sha256")
  if (!is.character(want) || length(want) != 1L || !identical(want, got))
    prevcarga_abort("integrity.freeze_sha_mismatch", where = path,
                    details = list(manifest = want, file = got))
  readRDS(path)
}

## --- Identidade do deck: data_hash (M2.5; model-agnostico; receita = default de trabalho, ADR pendente) ------------
## sha256 de linhas `<caminho Hive RELATIVO ao data_dir>\t<sha256 do arquivo>` de todos os parquet (carga, clima,
## feriados), reordenadas por radix (independe de locale e da ordem de listagem), mais `load_label\t<efetivo>` e
## `timezone.<chave>\t<IANA>` do manifest (+ `weather_forecast_publication_lag_hours\t<h>` quando declarado). Sem `--model`: o hash nao depende de modelo. `files` e a alavanca de
## teste (qualquer ordem); NULL enumera os datasets canonicos. Sem parquet => contract.dataset_missing.
.prevcarga_sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

.prevcarga_hash_datasets <- c("load", "weather/observed", "weather/forecast", "auxiliary/holidays")

prevcarga_data_hash <- function(data_dir, manifest, load_label, files = NULL) {
  if (is.null(files)) {
    files <- unlist(lapply(.prevcarga_hash_datasets, function(d) {
      rel <- list.files(file.path(data_dir, d), pattern = "[.]parquet$", recursive = TRUE)
      if (length(rel)) paste(d, rel, sep = "/") else character()
    }))
  }
  if (!length(files)) prevcarga_abort("contract.dataset_missing", where = "load")
  files <- sort(unique(files), method = "radix")
  sha <- vapply(file.path(data_dir, files), .prevcarga_sha256_file, character(1), USE.NAMES = FALSE)
  tz <- unlist(manifest$timezone)
  tz_lines <- if (length(tz)) paste0("timezone.", names(tz), "\t", tz)[order(names(tz), method = "radix")] else character()
  lag <- manifest$weather_forecast_publication_lag_hours           # declarado pelo deck como o fuso (ADR-0024/0037)
  lag_line <- if (is.null(lag)) character() else paste0("weather_forecast_publication_lag_hours\t", as.integer(lag))
  body <- c(paste0(files, "\t", sha), paste0("load_label\t", load_label), tz_lines, lag_line)
  .prevcarga_sha256(paste0(body, "\n", collapse = ""))
}
