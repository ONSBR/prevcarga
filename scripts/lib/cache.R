## cache.R -- cache content-addressed LOCAL honrando os dois cortes por classe de split (M7.2; ADR-0023, ADR-0040).
##
## Store sob `<ctx$state_dir>/cache/<kind>/<hh>/<key>/` com `object.rds|object.parquet` + `meta.json`.
## A chave REUSA o hash do provenance.R (prevcarga_version_hash: `v_<16 hex>` do JSON canonico de 8 chaves) e a
## identidade do M5.3 (model_id, data_fingerprint, cutoff, feature_config, code_commit, renv_lock_hash, load_label,
## load_tz); nao existe hash paralelo. O que muda por kind e o `cutoff` (ADR-0023):
##   features / member_long : {origin, available_until}   -- o corte intradiario SEMPRE entra
##   artifact               : origin_agnostic {split, train_origin, last_data_day}; origin_scoped {split, train_origin}
##                            -- available_until NUNCA entra (o sweep de --corte nao re-treina artefato)
##   khat                   : {subsistema, origin, window_days}
## Clima previsto (ADR-0024, M7.8): features/member_long levam SEMPRE a regra efetiva `weather_issue_cutoff` e o
## `weather_issue_limit` (POSIXct devolvido pelo leitor/dates.R) no feature_config; o artefato so os leva quando o
## descriptor do membro declara `weather_forecast_in_train` (sem over-key dos demais). O cache nunca filtra emissao.
## `train_origin`/`last_data_day` chegam prontos da linha do membro de prevcarga_resolve_cuts (dates.R): este modulo
## nunca calcula corte nem le o descriptor. Laterais (platform, bundle_id, trained_at, host) ficam so no meta.json.
## Sem relogio, sem variavel de ambiente, sem rede; coleta de lixo do cache fica fora do pacote.
## Requer errors.R e provenance.R. Ids no catalogo distribuido errors/cache.tsv.

.pc_cache_kinds <- c("features", "artifact", "member_long", "khat")
.pc_cache_splits <- c("origin_agnostic", "origin_scoped")
.pc_cache_key_re <- "^v_[0-9a-f]{16}$"

## --- Raiz e layout ---------------------------------------------------------------------------------------
.pc_cache_root <- function(ctx) file.path(ctx$state_dir, "cache")

.pc_cache_check_kind <- function(kind) {
  if (!is.character(kind) || length(kind) != 1L || !kind %in% .pc_cache_kinds)
    prevcarga_abort("contract.cache_kind", where = paste(kind, collapse = ","))
  invisible(kind)
}

.pc_cache_dir <- function(ctx, kind, key) {
  .pc_cache_check_kind(kind)
  if (!is.character(key) || length(key) != 1L || !grepl(.pc_cache_key_re, key))
    prevcarga_abort("contract.cache_key", where = paste(key, collapse = ","))
  file.path(.pc_cache_root(ctx), kind, substr(key, 3L, 4L), key)
}

## --- Identidade por classe -------------------------------------------------------------------------------
.pc_cache_need <- function(v, what, check) {
  if (is.null(v) || length(v) == 0L || !check(v)) prevcarga_abort("contract.cache_identity", where = what)
  invisible(v)
}
.pc_is_day <- function(v) inherits(v, "Date") && length(v) == 1L && !is.na(v)
.pc_is_instant <- function(v) inherits(v, "POSIXct") && length(v) == 1L && !is.na(v)
.pc_is_chr1 <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
.pc_weather_rules <- c("available_until", "origin_midnight_utc")

## Identidade do clima previsto por kind: list() quando o kind (ou o membro) nao consome previsto; senao regra + limite.
.pc_cache_weather <- function(kind, common) {
  if (!(kind %in% c("features", "member_long") || (kind == "artifact" && isTRUE(common$weather_forecast_in_train))))
    return(list())
  .pc_cache_need(common$weather_issue_cutoff, "weather_issue_cutoff", function(v) .pc_is_chr1(v) && v %in% .pc_weather_rules)
  .pc_cache_need(common$weather_issue_limit, "weather_issue_limit", .pc_is_instant)
  list(weather_issue_cutoff = common$weather_issue_cutoff, weather_issue_limit = common$weather_issue_limit)
}

.pc_cache_identity <- function(kind, member_label, model_spec, common, cuts, origin = NULL,
                               available_until = NULL, khat = NULL) {
  .pc_cache_check_kind(kind)
  if (is.null(member_label$mdir) || is.null(member_label$exogena) || !.pc_is_chr1(member_label$mdir))
    prevcarga_abort("contract.cache_member_label")
  split <- model_spec$split
  if (is.null(split) || !is.character(split) || length(split) != 1L || !split %in% .pc_cache_splits)
    prevcarga_abort("contract.cache_split")
  cutoff <- switch(kind,
    features = , member_long = {
      .pc_cache_need(origin, "origin", .pc_is_day)
      .pc_cache_need(available_until, "available_until", .pc_is_instant)
      list(origin = origin, available_until = available_until)
    },
    artifact = {
      .pc_cache_need(cuts$train_origin, "train_origin", .pc_is_day)
      if (identical(split, "origin_agnostic")) {
        .pc_cache_need(cuts$last_data_day, "last_data_day", .pc_is_day)
        list(split = split, train_origin = cuts$train_origin, last_data_day = cuts$last_data_day)
      } else list(split = split, train_origin = cuts$train_origin)
    },
    khat = {
      .pc_cache_need(khat$subsistema, "subsistema", .pc_is_chr1)
      .pc_cache_need(origin, "origin", .pc_is_day)
      .pc_cache_need(khat$window_days, "window_days", function(v) is.numeric(v) && length(v) == 1L && !is.na(v))
      list(subsistema = khat$subsistema, origin = origin, window_days = as.integer(khat$window_days))
    })
  ## as 8 chaves do contrato do provenance.R; o que e proprio do cache vai em feature_config
  list(model_id = member_label$mdir,
       data_fingerprint = common$data_fingerprint,
       cutoff = cutoff,
       feature_config = list(kind = kind, exogena = member_label$exogena, split = split,
                             feature_config = common$feature_config, model_versions = common$model_versions,
                             seed = common$seed, weather = .pc_cache_weather(kind, common)),
       code_commit = common$code_version,
       renv_lock_hash = common$lock_hash,
       load_label = common$load_label,
       load_tz = common$timezone)
}

## Identidade completa (8 chaves) a partir dos argumentos da chave; a mesma que vai no meta.json.
prevcarga_cache_identity <- function(kind, member_label, model_spec, data_fingerprint, load_label, timezone,
                                     weather_issue_cutoff = NULL, train_origin = NULL, last_data_day = NULL,
                                     origin = NULL, available_until = NULL, feature_config = NULL,
                                     model_versions = NULL, code_version, lock_hash, seed = NULL,
                                     subsistema = NULL, window_days = NULL, weather_issue_limit = NULL,
                                     weather_forecast_in_train = FALSE) {
  seed <- if (is.null(seed)) model_spec$seed else seed
  common <- list(data_fingerprint = data_fingerprint, load_label = load_label, timezone = timezone,
                 weather_issue_cutoff = weather_issue_cutoff, weather_issue_limit = weather_issue_limit,
                 weather_forecast_in_train = weather_forecast_in_train, feature_config = feature_config,
                 model_versions = model_versions, code_version = code_version, lock_hash = lock_hash, seed = seed)
  for (n in c("data_fingerprint", "load_label", "timezone", "code_version", "lock_hash"))
    .pc_cache_need(common[[n]], n, .pc_is_chr1)
  .pc_cache_need(seed, "seed", function(v) is.numeric(v) && length(v) == 1L && !is.na(v))
  .pc_cache_identity(kind, member_label, model_spec, common,
                     cuts = list(train_origin = train_origin, last_data_day = last_data_day),
                     origin = origin, available_until = available_until,
                     khat = list(subsistema = subsistema, window_days = window_days))
}

## --- Chave -------------------------------------------------------------------------------------------------
prevcarga_cache_key <- function(kind, member_label, model_spec, data_fingerprint, load_label, timezone,
                                weather_issue_cutoff = NULL, train_origin = NULL, last_data_day = NULL,
                                origin = NULL, available_until = NULL, feature_config = NULL,
                                model_versions = NULL, code_version, lock_hash, seed = NULL,
                                subsistema = NULL, window_days = NULL, weather_issue_limit = NULL,
                                weather_forecast_in_train = FALSE) {
  id <- prevcarga_cache_identity(kind, member_label, model_spec, data_fingerprint, load_label, timezone,
                                 weather_issue_cutoff, train_origin, last_data_day, origin, available_until,
                                 feature_config, model_versions, code_version, lock_hash, seed,
                                 subsistema, window_days, weather_issue_limit, weather_forecast_in_train)
  prevcarga_version_hash(id)
}

## --- Contadores por state_dir -------------------------------------------------------------------------------
.pc_cache_counters <- new.env(parent = emptyenv())
.pc_cache_cell <- function(ctx) {
  k <- normalizePath(ctx$state_dir, winslash = "/", mustWork = FALSE)
  if (is.null(.pc_cache_counters[[k]])) .pc_cache_counters[[k]] <- c(hit = 0L, miss = 0L, corrupt = 0L)
  k
}
.pc_cache_count <- function(ctx, what) {
  k <- .pc_cache_cell(ctx)
  v <- .pc_cache_counters[[k]]
  v[[what]] <- v[[what]] + 1L
  .pc_cache_counters[[k]] <- v
  invisible(NULL)
}
prevcarga_cache_stats <- function(ctx) as.list(.pc_cache_counters[[.pc_cache_cell(ctx)]])

## --- Objeto em disco ------------------------------------------------------------------------------------------
.pc_cache_write_object <- function(object, dir) {
  if (inherits(object, "data.table")) {
    f <- file.path(dir, "object.parquet")
    arrow::write_parquet(object, f)
  } else {
    f <- file.path(dir, "object.rds")
    saveRDS(object, f, version = 3L, compress = FALSE)
  }
  f
}

.pc_cache_read_object <- function(f) {
  if (grepl("[.]parquet$", f)) data.table::as.data.table(arrow::read_parquet(f)) else readRDS(f)
}

.pc_cache_sha <- function(f) digest::digest(file = f, algo = "sha256")

.pc_cache_meta <- function(dir) tryCatch({
  m <- jsonlite::read_json(file.path(dir, "meta.json"))
  if (is.character(m$sha256) && is.character(m$format)) m else NULL
}, error = function(e) NULL)

.pc_cache_quarantine <- function(ctx, kind, key, dir) {
  dest <- file.path(.pc_cache_root(ctx), "_quarantine", kind, key)
  unlink(dest, recursive = TRUE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.rename(dir, dest)
  invisible(dest)
}

## --- put / get -------------------------------------------------------------------------------------------------
## `identity`: a lista de prevcarga_cache_identity (vai no meta.json, canonizada); `lateral`: platform, bundle_id,
## trained_at, host (do run-manifest do M1.3; nunca de relogio aqui). A entrada e publicada por rename atomico.
prevcarga_cache_put <- function(ctx, key, kind, object, identity, lateral = list()) {
  dir <- .pc_cache_dir(ctx, kind, key)
  tmp <- paste0(dir, ".tmp-", Sys.getpid())
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  f <- .pc_cache_write_object(object, tmp)
  sha <- .pc_cache_sha(f)
  if (dir.exists(dir)) {
    old <- .pc_cache_meta(dir)
    if (is.null(old) || !identical(old$sha256, sha))
      prevcarga_abort("integrity.cache_put_conflict", where = key,
                      details = list(kind = kind, sha256_new = sha,
                                     sha256_old = if (is.null(old)) NA_character_ else old$sha256))
    return(invisible(dir))
  }
  jsonlite::write_json(list(key = key, kind = kind, identity = .prevcarga_canon(identity),
                            lateral = .prevcarga_canon(lateral), sha256 = sha, format = basename(f)),
                       file.path(tmp, "meta.json"), auto_unbox = TRUE, digits = NA, null = "null", na = "null")
  dir.create(dirname(dir), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(tmp, dir)) prevcarga_abort("integrity.cache_put_conflict", where = key,
                                              details = list(kind = kind, reason = "rename"))
  invisible(dir)
}

prevcarga_cache_get <- function(ctx, key, kind) {
  dir <- .pc_cache_dir(ctx, kind, key)
  if (!dir.exists(dir)) { .pc_cache_count(ctx, "miss"); return(NULL) }
  meta <- .pc_cache_meta(dir)
  f <- if (is.null(meta)) NA_character_ else file.path(dir, meta$format)
  if (is.null(meta) || !file.exists(f) || !identical(.pc_cache_sha(f), meta$sha256)) {
    .pc_cache_quarantine(ctx, kind, key, dir)
    prevcarga_warn("warning.cache_corrupt_quarantined", where = paste(kind, key, sep = "/"))
    .pc_cache_count(ctx, "corrupt"); .pc_cache_count(ctx, "miss")
    return(NULL)
  }
  .pc_cache_count(ctx, "hit")
  .pc_cache_read_object(f)
}
