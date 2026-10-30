## reconcile.R -- Reconcile: nucleo PURO (R.1) + perfil k-hat como artefato do registry (R.2).
## Bottom-Up por interval_end, k-hat por interval_start (ADR-0002/0035). O nucleo nao faz I/O nem leitor.
## Requer errors.R, schema_long.R e evaluation.R (prevcarga_assert_long) carregados, alem de data.table e lubridate.
## A secao final (R.2) so especializa o k-hat (versao, freeze, thaw, regra de avanco do ponteiro); o ciclo de vida
## (publicar/resolver/carregar/mover o ponteiro) e do nucleo do registry (registry.R, M5.2), chamado como API.
## Requer tambem provenance.R e registry.R carregados.
##
## Regra: o Bottom-Up SOMA por `target_datetime` (= interval_end; bijecao com interval_start, mesmo agrupamento);
## o k-hat e BUCKETIZADO por `interval_start`, obtido so da borda do LONG (prevcarga_long_interval_start em
## schema_long.R). Este modulo nao faz aritmetica de hora (gate minutes30). Hierarquia, perdas e faixa vem so de
## prevcarga_topology(); nenhum literal de regiao, perda ou contagem. Datas so por lubridate com tz explicito.

prevcarga_khat_window_days <- 730L
prevcarga_khat_keys <- c("hour", "month", "dow")
prevcarga_khat_week_start <- 7L   # dow: domingo = 1 (receita do oraculo)

.prevcarga_reconcile_dt <- function(x) data.table::as.data.table(as.data.frame(x))

.prevcarga_reconcile_dropped <- function(d_plus_dropped) {
  if (!length(d_plus_dropped))
    return(data.table::data.table(d_plus = integer(), n = integer()))
  out <- data.table::data.table(d_plus = as.integer(d_plus_dropped))[, list(n = .N), by = "d_plus"]
  data.table::setorderv(out, "d_plus")
  out
}

## --- Planejador de fechamento ---------------------------------------------------------------------------
## Subsistema fecha sse todas as suas folhas estao presentes; folhas de subsistema que nao fecha vao para stray.
prevcarga_reconcile_plan <- function(leaves_present, topology) {
  leaves_present <- unique(as.character(leaves_present))
  ok <- vapply(topology$order, function(ss) all(topology$subsystems[[ss]]$areas %in% leaves_present), logical(1))
  closed <- topology$order[ok]
  in_closed <- unlist(lapply(closed, function(ss) topology$subsystems[[ss]]$areas), use.names = FALSE)
  list(closed = closed, stray = setdiff(leaves_present, in_closed), sin_closed = all(ok))
}

## --- Bottom-Up ------------------------------------------------------------------------------------------
## long_leaves: um LONG (coluna `area`) ou lista de LONGs de folhas. Soma y_pred por (forecast_origin,
## target_datetime, lead, d_plus) so onde TODAS as folhas do subsistema existem; nunca preenche com 0.
prevcarga_reconcile_bottomup <- function(long_leaves, subsystem, topology, ctx) {
  if (!is.character(subsystem) || length(subsystem) != 1L || !subsystem %in% topology$order)
    prevcarga_abort("contract.reconcile_input", where = as.character(subsystem)[[1L]])
  areas <- topology$subsystems[[subsystem]]$areas
  parts <- if (is.data.frame(long_leaves)) list(long_leaves) else long_leaves
  if (!is.list(parts) || !length(parts)) prevcarga_abort("contract.reconcile_input", where = "long_leaves")
  checked <- lapply(parts, function(p) .prevcarga_reconcile_dt(prevcarga_assert_long(p, ctx)))
  lv <- data.table::rbindlist(checked, use.names = TRUE)
  lv <- lv[lv$area %in% areas]
  absent <- setdiff(areas, unique(lv$area))
  if (length(absent)) prevcarga_abort("completude.leaf_missing", where = paste(absent, collapse = ","))
  dup <- lv[, list(n = .N), by = c("area", "forecast_origin", "target_datetime")][n > 1L]
  if (nrow(dup)) prevcarga_abort("contract.reconcile_input", where = paste(unique(dup$area), collapse = ","),
                                 details = list(reason = "folha duplicada no mesmo instante"))
  keys <- c("forecast_origin", "target_datetime")
  chk <- lv[, list(n_lead = data.table::uniqueN(lead), n_dplus = data.table::uniqueN(d_plus)), by = keys]
  bad <- chk[n_lead > 1L | n_dplus > 1L]
  if (nrow(bad))
    prevcarga_abort("contract.reconcile_input",
                    where = paste(unique(lv[bad, on = keys]$area), collapse = ","),
                    details = list(reason = "lead ou d_plus divergente entre folhas"))
  bu <- lv[, list(BU_pred = sum(y_pred), n = .N),
           by = c("forecast_origin", "target_datetime", "lead", "d_plus", "granularity_min")]
  n_areas <- length(areas)
  dropped <- .prevcarga_reconcile_dropped(bu$d_plus[bu$n < n_areas])
  if (nrow(dropped))
    prevcarga_warn("reconcile.coverage_dropped", where = subsystem,
                   details = list(subsystem = subsystem, dropped = as.data.frame(dropped)))
  bu <- bu[bu$n == n_areas][, n := NULL]
  data.table::setorderv(bu, c("forecast_origin", "target_datetime"))
  data.table::setattr(bu, "dropped", dropped)
  bu
}

## --- k-hat: razao SS_true / BU_true ----------------------------------------------------------------------
## truth = saida do leitor unico (area_code, interval_start, interval_end, load_mwh). Soma por interval_end.
prevcarga_khat_ratio <- function(truth, subsystem, topology) {
  if (!is.character(subsystem) || length(subsystem) != 1L || !subsystem %in% topology$order)
    prevcarga_abort("contract.reconcile_input", where = as.character(subsystem)[[1L]])
  areas <- topology$subsystems[[subsystem]]$areas
  tr <- .prevcarga_reconcile_dt(truth)
  need <- c("area_code", "interval_start", "interval_end", "load_mwh")
  if (!all(need %in% names(tr)))
    prevcarga_abort("contract.reconcile_input", where = paste(setdiff(need, names(tr)), collapse = ","))
  dup <- tr[tr$area_code %in% c(subsystem, areas), list(n = .N), by = c("area_code", "interval_end")][n > 1L]
  if (nrow(dup)) prevcarga_abort("contract.reconcile_input", where = paste(unique(dup$area_code), collapse = ","),
                                 details = list(reason = "carga duplicada no mesmo interval_end"))
  ss <- tr[tr$area_code == subsystem & is.finite(tr$load_mwh),
           list(interval_start, interval_end, SS_true = load_mwh)]
  bu <- tr[tr$area_code %in% areas & is.finite(tr$load_mwh),
           list(BU_true = sum(load_mwh), n = .N), by = "interval_end"]
  bu <- bu[bu$n == length(areas) & bu$BU_true > 0][, n := NULL]
  m <- merge(ss, bu, by = "interval_end")
  out <- m[, list(interval_start, interval_end, ratio = SS_true / BU_true)]
  data.table::setorderv(out, "interval_start")
  out[is.finite(out$ratio)]
}

## --- k-hat: ajuste ---------------------------------------------------------------------------------------
## Janela = os `window_days` dias de interval_start terminando em O-1: [O - W, O - 1], inclusiva nas duas pontas
## (receita do oraculo seq(O - W, O - 1)). glob = media dos ratio DA JANELA (o oraculo usa todo o historico).
prevcarga_khat_fit <- function(ratio, origin, window_days = prevcarga_khat_window_days) {
  if (!is.numeric(window_days) || length(window_days) != 1L || !is.finite(window_days) ||
      window_days < 1 || window_days != floor(window_days))
    prevcarga_abort("contract.reconcile_input", where = "window_days")
  origin <- lubridate::as_date(origin)
  if (length(origin) != 1L || is.na(origin)) prevcarga_abort("contract.reconcile_input", where = "origin")
  r <- .prevcarga_reconcile_dt(ratio)
  tz <- lubridate::tz(r$interval_start)
  d <- lubridate::as_date(r$interval_start, tz = tz)
  from <- origin - as.integer(window_days)
  to <- origin - 1L
  w <- r[d >= from & d <= to & is.finite(r$ratio)]
  if (!nrow(w)) prevcarga_abort("completude.khat_no_truth", where = as.character(origin))
  st <- w$interval_start
  w[, `:=`(hour = lubridate::hour(st), month = lubridate::month(st),
           dow = lubridate::wday(st, week_start = prevcarga_khat_week_start))]
  prof <- w[, list(k = mean(ratio), n = .N), by = prevcarga_khat_keys]
  data.table::setorderv(prof, prevcarga_khat_keys)
  list(profile = prof, glob = mean(w$ratio), n_ratio = nrow(w), window_from = from, window_to = to,
       window_days = as.integer(window_days))
}

## --- k-hat: aplicacao ------------------------------------------------------------------------------------
## y_pred = BU_pred * k(bucket de interval_start); bucket ausente usa glob; sem clamp (coerencia e do check).
prevcarga_khat_apply <- function(bu, profile, ctx) {
  p <- profile$profile
  if (!is.data.frame(p) || !all(c(prevcarga_khat_keys, "k") %in% names(p)) ||
      !is.numeric(profile$glob) || length(profile$glob) != 1L || !is.finite(profile$glob))
    prevcarga_abort("contract.khat_profile", where = "profile")
  x <- data.table::copy(.prevcarga_reconcile_dt(bu))
  st <- prevcarga_long_interval_start(x$target_datetime, x$granularity_min, ctx$tz)
  x[, `:=`(hour = lubridate::hour(st), month = lubridate::month(st),
           dow = lubridate::wday(st, week_start = prevcarga_khat_week_start))]
  pk <- data.table::as.data.table(p)[, c(prevcarga_khat_keys, "k"), with = FALSE]
  x <- pk[x, on = prevcarga_khat_keys]
  n_fb <- sum(is.na(x$k))
  if (n_fb) prevcarga_warn("reconcile.khat_fallback", details = list(n = n_fb))
  x[is.na(k), k := profile$glob]
  x[, y_pred := BU_pred * k]
  x[, c(prevcarga_khat_keys) := NULL]
  data.table::setcolorder(x, setdiff(names(x), c("k", "y_pred")))
  data.table::setattr(x, "n_fallback", n_fb)
  x
}

## --- SIN = soma dos subsistemas --------------------------------------------------------------------------
## long_subsystems: LONG (coluna `area` = subsistema) ou lista deles. Mantem so n == length(topology$subsystems).
prevcarga_reconcile_sin <- function(long_subsystems, topology) {
  parts <- if (is.data.frame(long_subsystems)) list(long_subsystems) else long_subsystems
  if (!is.list(parts) || !length(parts)) prevcarga_abort("contract.reconcile_input", where = "long_subsystems")
  ls <- data.table::rbindlist(lapply(parts, .prevcarga_reconcile_dt), use.names = TRUE, fill = TRUE)
  ls <- ls[ls$area %in% topology$order]
  dup <- ls[, list(n = .N), by = c("area", "forecast_origin", "target_datetime")][n > 1L]
  if (nrow(dup)) prevcarga_abort("contract.reconcile_input", where = paste(unique(dup$area), collapse = ","),
                                 details = list(reason = "subsistema duplicado no mesmo instante"))
  keys <- c("forecast_origin", "target_datetime")
  chk <- ls[, list(n_lead = data.table::uniqueN(lead), n_dplus = data.table::uniqueN(d_plus)), by = keys]
  bad <- chk[n_lead > 1L | n_dplus > 1L]
  if (nrow(bad))
    prevcarga_abort("contract.reconcile_input", where = paste(unique(ls[bad, on = keys]$area), collapse = ","),
                    details = list(reason = "lead ou d_plus divergente entre subsistemas"))
  sin <- ls[, list(y_pred = sum(y_pred), n = .N),
            by = c("forecast_origin", "target_datetime", "lead", "d_plus", "granularity_min")]
  n_ss <- length(topology$subsystems)
  dropped <- .prevcarga_reconcile_dropped(sin$d_plus[sin$n < n_ss])
  if (nrow(dropped))
    prevcarga_warn("reconcile.coverage_dropped", where = "SIN",
                   details = list(dropped = as.data.frame(dropped)))
  sin <- sin[sin$n == n_ss][, n := NULL]
  data.table::setorderv(sin, c("forecast_origin", "target_datetime"))
  data.table::setattr(sin, "dropped", dropped)
  sin
}

## --- Relabelador fino: monta o LONG de 8 colunas ----------------------------------------------------------
## forecast_origin TEXTO yyyy-MM-dd (ADR-0032; Date e proibido); y_true NA por padrao; sem reancorar.
prevcarga_reconcile_long <- function(dt, area, ctx) {
  if (!is.character(area) || length(area) != 1L || is.na(area) || !nzchar(area))
    prevcarga_abort("contract.reconcile_input", where = "area")
  x <- .prevcarga_reconcile_dt(dt)
  need <- c("forecast_origin", "target_datetime", "lead", "d_plus", "y_pred")
  if (!all(need %in% names(x)))
    prevcarga_abort("contract.reconcile_input", where = paste(setdiff(need, names(x)), collapse = ","))
  if (!is.character(x$forecast_origin))
    prevcarga_abort("contract.long_schema", where = "forecast_origin")
  n <- nrow(x)
  out <- data.table::data.table(
    area = rep(area, n), forecast_origin = x$forecast_origin, target_datetime = x$target_datetime,
    lead = as.integer(x$lead), y_pred = as.double(x$y_pred),
    y_true = if ("y_true" %in% names(x)) as.double(x$y_true) else rep(NA_real_, n),
    d_plus = as.integer(x$d_plus),
    granularity_min = if ("granularity_min" %in% names(x)) as.integer(x$granularity_min) else rep(30L, n))
  data.table::setorderv(out, c("forecast_origin", "target_datetime"))
  prevcarga_schema_long_check(out)
  invisible(prevcarga_assert_long(out, ctx))
  out
}

## =====================================================================================================
## R.2 -- perfil k-hat como artefato oficial do registry (ADR-0039/ADR-0030). Layout (o do bucket):
##   <state_dir>/artifacts/reconcile/<ss>/{latest.txt, v_<hash>/{artifact.rds, run_manifest.json, SHA256SUMS}}
## Aqui so mora o que e especifico do k-hat; publicar/resolver/carregar/ponteiro passam por prevcarga_registry_*.
## =====================================================================================================
prevcarga_khat_schema_version <- 1L
.prevcarga_khat_mdir <- "reconcile"
.prevcarga_khat_meta_keys <- c("ss", "khat_origin", "window_days", "window_from", "window_to", "profile_keys",
                               "week_start", "areas", "loss", "n_ratio", "load_label", "timezone", "data_hash",
                               "code_commit", "renv_lock_hash")

.prevcarga_khat_bad <- function(what) prevcarga_abort("contract.khat_profile", where = what)

## chave da celula <state_dir>/artifacts/reconcile/<ss>
prevcarga_khat_key <- function(ss, ctx) {
  if (!is.character(ss) || length(ss) != 1L || is.na(ss) || !nzchar(ss)) .prevcarga_khat_bad("ss")
  prevcarga_registry_key(list(mdir = .prevcarga_khat_mdir, exogena = NA_character_), ss, ctx)
}

## sha256 do ratio em forma canonica (ordenado por interval_end, colunas fixas, instantes em UTC ISO-8601)
prevcarga_khat_data_hash <- function(ratio) {
  r <- .prevcarga_reconcile_dt(ratio)
  if (!all(c("interval_start", "interval_end", "ratio") %in% names(r))) .prevcarga_khat_bad("ratio")
  r <- r[order(r$interval_end)]
  txt <- paste0(vapply(r$interval_start, .prevcarga_canon, ""), "\t", vapply(r$interval_end, .prevcarga_canon, ""),
                "\t", sprintf("%.17g", r$ratio), "\n", collapse = "")
  digest::digest(txt, algo = "sha256", serialize = FALSE)
}

## Artefato congelado (sem trained_at: lateral, so no manifest). `meta` completo ou contract.khat_profile.
.prevcarga_khat_artifact <- function(fit, meta) {
  miss <- setdiff(.prevcarga_khat_meta_keys, names(meta))
  if (length(miss)) .prevcarga_khat_bad(miss[[1L]])
  m <- meta[.prevcarga_khat_meta_keys]
  art <- list(schema_version = prevcarga_khat_schema_version,
              profile = data.table::as.data.table(fit$profile), glob = fit$glob, meta = m)
  .prevcarga_khat_check(art)
  art
}

.prevcarga_khat_check <- function(art) {
  if (!is.list(art) || !identical(art$schema_version, prevcarga_khat_schema_version)) .prevcarga_khat_bad("schema_version")
  p <- art$profile
  need <- c(prevcarga_khat_keys, "k", "n")
  if (!is.data.frame(p) || !all(need %in% names(p))) {
    .prevcarga_khat_bad(paste(setdiff(need, names(p)), collapse = ","))
  }
  if (!all(vapply(as.list(p)[c(prevcarga_khat_keys, "n")], is.numeric, NA)) || !is.numeric(p$k)) .prevcarga_khat_bad("profile")
  if (!is.numeric(art$glob) || length(art$glob) != 1L || !is.finite(art$glob)) .prevcarga_khat_bad("glob")
  m <- art$meta
  miss <- setdiff(.prevcarga_khat_meta_keys, names(m))
  if (!is.list(m) || length(miss)) .prevcarga_khat_bad(if (length(miss)) miss[[1L]] else "meta")
  if (!inherits(m$khat_origin, "Date") || length(m$khat_origin) != 1L || is.na(m$khat_origin)) .prevcarga_khat_bad("khat_origin")
  if (!(is.character(m$load_label) && length(m$load_label) == 1L && m$load_label %in% c("start", "end")))
    .prevcarga_khat_bad("load_label")
  if (!(is.character(m$timezone) && length(m$timezone) == 1L && m$timezone %in% OlsonNames())) .prevcarga_khat_bad("timezone")
  if (!(is.numeric(m$window_days) && length(m$window_days) == 1L && m$window_days >= 1)) .prevcarga_khat_bad("window_days")
  invisible(TRUE)
}

## --- versao: v_<16hex> do fingerprint de 8 chaves ----------------------------------------------------------
prevcarga_khat_version <- function(fit, meta) {
  art <- .prevcarga_khat_artifact(fit, meta)
  m <- art$meta
  prevcarga_version_hash(list(
    model_id = paste0(.prevcarga_khat_mdir, "/", m$ss),
    data_fingerprint = m$data_hash,
    cutoff = m$khat_origin - 1L,
    feature_config = list(window_days = as.integer(m$window_days), keys = m$profile_keys,
                          week_start = as.integer(m$week_start), areas = sort(m$areas, method = "radix"),
                          loss = m$loss),
    code_commit = m$code_commit,
    renv_lock_hash = m$renv_lock_hash,
    load_label = m$load_label,
    load_tz = m$timezone))
}

## sha256 dos bytes que o freeze gravaria (serializacao deterministica saveRDS v3), sem tocar o registry
.prevcarga_khat_sha_of <- function(art) {
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(art, f, version = 3)
  digest::digest(file = f, algo = "sha256")
}

## --- freeze: publica v_<hash>/ pelo registry (staging .tmp-* + rename); mesmo perfil => no-op ---------------
## `meta$trained_at` (opcional, lateral) so entra no run_manifest.json. Devolve list(version, path, reused).
prevcarga_khat_freeze <- function(fit, meta, ctx) {
  art <- .prevcarga_khat_artifact(fit, meta)
  v <- prevcarga_khat_version(fit, meta)
  key <- prevcarga_khat_key(art$meta$ss, ctx)
  sha <- .prevcarga_khat_sha_of(art)
  final <- file.path(key, v)
  if (dir.exists(final)) {
    disk <- file.path(final, "artifact.rds")
    mf <- file.path(final, "run_manifest.json")
    want <- tryCatch(prevcarga_read_run_manifest(final)$arquivos$sha256[[1L]], error = function(e) NA_character_)
    got <- if (file.exists(disk)) digest::digest(file = disk, algo = "sha256") else NA_character_
    if (!file.exists(mf) || !identical(want, sha) || !identical(got, sha))
      prevcarga_abort("integrity.khat_conflict", where = final, details = list(new = sha, manifest = want, disk = got))
  }
  m <- art$meta
  manifest <- list(produto = "reconcile", ss = m$ss, versao = v, khat_origin = m$khat_origin,
                   window_days = as.integer(m$window_days), load_label = m$load_label, timezone = m$timezone,
                   schema_version = art$schema_version, areas = sort(m$areas, method = "radix"),
                   arquivos = list(list(path = "artifact.rds", sha256 = sha)),
                   trained_at = meta$trained_at)
  prevcarga_registry_publish(art, manifest, key, v, ctx = ctx)
}

## --- avanco do ponteiro: so para frente (khat_origin menor grava a versao mas nao move o latest) -------------
## Delega a escrita ao nucleo (prevcarga_registry_promote_latest). TRUE = moveu; FALSE = manteve (com aviso).
prevcarga_khat_advance_latest <- function(ss, versao, khat_origin, ctx) {
  key <- prevcarga_khat_key(ss, ctx)
  prevcarga_resolve_version(key, "version", versao, ctx = ctx)   # a versao precisa existir
  cur <- tryCatch(prevcarga_resolve_version(key, "latest", ctx = ctx), prevcarga_error = function(e) NULL)
  if (!is.null(cur)) {
    cur_o <- prevcarga_read_run_manifest(file.path(key, cur))$khat_origin
    if (is.character(cur_o) && length(cur_o) == 1L && khat_origin < lubridate::ymd(cur_o)) {
      prevcarga_warn("reconcile.khat_not_promoted", where = ss)
      return(FALSE)
    }
  }
  prevcarga_registry_promote_latest(key, versao, ctx = ctx)
  TRUE
}

## --- resolve / thaw ------------------------------------------------------------------------------------------
## khat_version dado => essa versao (sem latest.txt); NULL => latest.txt. Ausente => state.khat_missing (exit 13).
prevcarga_khat_resolve <- function(ctx, ss, khat_version = NULL) {
  key <- prevcarga_khat_key(ss, ctx)
  mode <- if (is.null(khat_version)) "latest" else "version"
  tryCatch(prevcarga_resolve_version(key, mode, khat_version, ctx = ctx),
           prevcarga_error = function(e) {
             if (!identical(e$id, "state.artifact_missing") && !identical(e$id, "state.pin_missing")) stop(e)
             prevcarga_abort("state.khat_missing",
                             where = if (is.null(khat_version)) file.path(key, "latest.txt") else khat_version)
           })
}

## thaw pelo registry (SHA256SUMS antes do readRDS; exit 9) + schema do perfil (contract.khat_profile)
prevcarga_khat_thaw <- function(ctx, ss, versao) {
  key <- prevcarga_khat_key(ss, ctx)
  v <- prevcarga_khat_resolve(ctx, ss, versao)
  art <- prevcarga_load_artifact(key, v, ctx = ctx)
  .prevcarga_khat_check(art)
  art
}
