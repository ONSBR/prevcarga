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

## =====================================================================================================
## R.3 -- orquestracao train/apply (compoe o nucleo R.1 e o perfil-artefato R.2). Nada e gravado em --out aqui;
## o verbo (R.5) grava. O leitor so e chamado em train (verdade ate O-1) e em apply com eval = TRUE.
## Requer tambem dates.R e topology.R carregados (prevcarga_resolve_cuts, prevcarga_topology).
## =====================================================================================================
.prevcarga_reconcile_topo <- function(ctx) .prevcarga_score_topo(ctx)

.prevcarga_reconcile_origin_date <- function(origin) {
  o <- lubridate::as_date(origin)
  if (length(o) != 1L || is.na(o)) prevcarga_abort("contract.reconcile_input", where = "origin")
  o
}

## "all" ou NULL = todos os subsistemas da topologia; nomes fora dela => contract.reconcile_input
.prevcarga_reconcile_subsystems <- function(subsystems, topo) {
  if (is.null(subsystems) || identical(subsystems, "all")) return(topo$order)
  if (!is.character(subsystems) || !length(subsystems) || anyNA(subsystems))
    prevcarga_abort("contract.reconcile_input", where = "subsystems")
  bad <- setdiff(subsystems, topo$order)
  if (length(bad)) prevcarga_abort("contract.reconcile_input", where = bad[[1L]])
  topo$order[topo$order %in% subsystems]
}

## Convencao efetiva da carga: rotulo (ctx$load_label ou o resolvido do deck) e fuso (ctx$tz)
.prevcarga_reconcile_convention <- function(ctx) {
  ll <- ctx$load_label
  tz <- ctx$tz
  if (is.null(ll) || is.null(tz)) {
    d <- .prevcarga_deck_ctx(ctx)
    if (is.null(ll)) ll <- d$label$value
    if (is.null(tz)) tz <- d$tz
  }
  list(load_label = ll, tz = tz)
}

.prevcarga_khat_summary <- function(profile, khat_range) {
  k <- profile$k
  list(min = min(k), max = max(k), fora_faixa = as.integer(sum(k < khat_range[[1L]] | k > khat_range[[2L]])))
}

## --- train: ajusta, congela e promove o perfil de cada subsistema (khat_origin = O) -----------------------------
## Verdade SO pelo leitor unico (prevcarga_read_load), janela [O-W, O-1] inclusiva; o corte vem de dates.R.
prevcarga_reconcile_train <- function(ctx, origin, subsystems = "all", window_days = prevcarga_khat_window_days) {
  origin <- .prevcarga_reconcile_origin_date(origin)
  topo <- .prevcarga_reconcile_topo(ctx)
  sss <- .prevcarga_reconcile_subsystems(subsystems, topo)
  cuts <- prevcarga_resolve_cuts(ctx, origin)
  last_day <- unique(cuts$cuts$last_data_day)
  if (length(last_day) != 1L) prevcarga_abort("contract.reconcile_input", where = "last_data_day")
  from <- origin - as.integer(window_days)
  out <- lapply(sss, function(ss) {
    areas <- topo$subsystems[[ss]]$areas
    truth <- prevcarga_read_load(ctx, areas = c(areas, ss), from = from, to = last_day)
    ratio <- prevcarga_khat_ratio(truth, ss, topo)
    fit <- prevcarga_khat_fit(ratio, origin, window_days)
    conv <- list(load_label = attr(truth, "load_label"), tz = attr(truth, "tz"))
    if (is.null(conv$load_label) || is.null(conv$tz)) conv <- .prevcarga_reconcile_convention(ctx)
    renv <- if (is.null(ctx$root)) NULL else prevcarga_renv_lock_hash(ctx$root)
    meta <- list(ss = ss, khat_origin = origin, window_days = fit$window_days, window_from = fit$window_from,
                 window_to = fit$window_to, profile_keys = prevcarga_khat_keys,
                 week_start = prevcarga_khat_week_start, areas = areas, loss = topo$subsystems[[ss]]$loss,
                 n_ratio = fit$n_ratio, load_label = conv$load_label, timezone = conv$tz,
                 data_hash = prevcarga_khat_data_hash(ratio),
                 code_commit = if (is.null(ctx$root)) "unknown" else prevcarga_code_commit(ctx$root),
                 renv_lock_hash = if (is.null(renv)) "none" else renv)
    fz <- prevcarga_khat_freeze(fit, meta, ctx)
    list(versao = fz$version, path = fz$path, khat_origin = origin, n_ratio = fit$n_ratio,
         promovido = prevcarga_khat_advance_latest(ss, fz$version, origin, ctx),
         k_hat_resumo = .prevcarga_khat_summary(fit$profile, topo$subsystems[[ss]]$khat_range))
  })
  stats::setNames(out, sss)
}

## --- Politica de uso do perfil (unico lugar): khat_origin == O SEMPRE, com ou sem pino (Gabriel, 2026-10-29) ---
## Como o oraculo (reajusta o k-hat por origem; Delta = 0), sem reuso de perfil de outra origem: posterior a O =>
## honesty.khat_after_origin; anterior a O => state.khat_missing (rode train para esta origem). O pino so escolhe entre
## versoes ajustadas EM O. Devolve khat_age_days, hoje sempre 0L (mantido por contrato de saida do result$khat).
prevcarga_khat_check_policy <- function(prof, ctx, origin) {
  m <- prof$meta
  conv <- .prevcarga_reconcile_convention(ctx)
  if (!identical(m$load_label, conv$load_label))
    prevcarga_abort("contract.khat_convention", where = "load_label",
                    details = list(perfil = m$load_label, ctx = conv$load_label))
  if (!identical(m$timezone, conv$tz))
    prevcarga_abort("contract.khat_convention", where = "timezone",
                    details = list(perfil = m$timezone, ctx = conv$tz))
  origin <- .prevcarga_reconcile_origin_date(origin)
  ko <- lubridate::as_date(m$khat_origin)
  if (ko > origin)
    prevcarga_abort("honesty.khat_after_origin",
                    details = list(khat_origin = format(ko), origem = format(origin)))
  if (ko < origin)
    prevcarga_abort("state.khat_missing", where = paste0(m$ss, "@", format(origin)),
                    details = list(khat_origin = format(ko), origem = format(origin)))
  as.integer(origin - ko)
}

## Versao do perfil ajustado para a origem O: o latest se for dele; senao varre as versoes da celula (o backtest
## treina em sequencia para tras e o latest.txt so anda para frente). Sem correspondencia devolve o latest, e a
## politica decide o erro (futuro => honesty; antigo => state.khat_missing). Sem latest => state.khat_missing.
.prevcarga_khat_resolve_for_origin <- function(ctx, ss, origin) {
  latest <- prevcarga_khat_resolve(ctx, ss, NULL)
  key <- prevcarga_khat_key(ss, ctx)
  ko <- function(v) tryCatch(lubridate::ymd(prevcarga_read_run_manifest(file.path(key, v))$khat_origin),
                             error = function(e) lubridate::NA_Date_)
  if (identical(ko(latest), origin)) return(latest)
  vs <- sort(list.files(key, pattern = "^v_[0-9a-f]{16}$"), method = "radix")
  hit <- vs[vapply(vs, function(v) identical(ko(v), origin), NA)]
  if (length(hit)) hit[[length(hit)]] else latest
}

## verdade por area pelo leitor unico (um read_actual por area: a saida nao traz a coluna da area)
.prevcarga_reconcile_truth <- function(ctx, area, from, to) {
  tr <- prevcarga_read_actual(ctx, area, from = from, to = to)
  stats::setNames(tr$y_true, as.numeric(tr$interval_end))
}

## --- apply: Bottom-Up x k-hat por subsistema (+ SIN) --------------------------------------------------------
prevcarga_reconcile_apply <- function(long_leaves, ctx, subsystems = "all", khat_version = NULL, eval = FALSE) {
  topo <- .prevcarga_reconcile_topo(ctx)
  sss <- .prevcarga_reconcile_subsystems(subsystems, topo)
  parts <- if (is.data.frame(long_leaves)) list(long_leaves) else long_leaves
  if (!is.list(parts) || !length(parts)) prevcarga_abort("contract.reconcile_input", where = "long_leaves")
  checked <- lapply(parts, function(p) .prevcarga_reconcile_dt(prevcarga_assert_long(p, ctx)))
  all_leaves <- data.table::rbindlist(checked, use.names = TRUE)
  origins <- unique(all_leaves$forecast_origin)
  if (length(origins) != 1L) prevcarga_abort("contract.reconcile_input", where = "forecast_origin")
  origin <- lubridate::ymd(origins, tz = ctx$tz)
  origin <- lubridate::as_date(origin)

  plan <- prevcarga_reconcile_plan(unique(all_leaves$area), topo)
  open <- setdiff(sss, plan$closed)
  if (length(open)) {
    absent <- unlist(lapply(open, function(ss) setdiff(topo$subsystems[[ss]]$areas, all_leaves$area)))
    prevcarga_abort("completude.leaf_missing", where = paste(absent, collapse = ","))
  }

  warns <- list()
  long <- list(); khat <- list(); cob <- list(); wm <- list(); dets <- list()
  withCallingHandlers({
    for (ss in sss) {
      areas <- topo$subsystems[[ss]]$areas
      bu <- prevcarga_reconcile_bottomup(all_leaves, ss, topo, ctx)
      ver <- if (is.null(khat_version)) .prevcarga_khat_resolve_for_origin(ctx, ss, origin)
             else prevcarga_khat_resolve(ctx, ss, khat_version)
      prof <- prevcarga_khat_thaw(ctx, ss, ver)
      age <- prevcarga_khat_check_policy(prof, ctx, origin)
      ap <- prevcarga_khat_apply(bu, list(profile = prof$profile, glob = prof$glob), ctx)
      m <- prof$meta
      khat[[ss]] <- list(versao = ver, khat_origin = lubridate::as_date(m$khat_origin), khat_age_days = age,
                         window_from = m$window_from, window_to = m$window_to, n_ratio = m$n_ratio,
                         n_fallback = as.integer(attr(ap, "n_fallback")))
      dr <- attr(bu, "dropped")
      cob[[ss]] <- if (nrow(dr)) stats::setNames(as.list(as.integer(dr$n)), paste0("d_plus_", dr$d_plus)) else list()
      dets[[ss]] <- ap
      long[[ss]] <- prevcarga_reconcile_long(ap, ss, ctx)
    }
  }, warning = function(w) {
    warns[[length(warns) + 1L]] <<- list(id = if (inherits(w, "prevcarga_warning")) w$id else NA_character_,
                                         message = conditionMessage(w))
  })

  if (isTRUE(eval)) {
    for (ss in sss) {
      ap <- dets[[ss]]
      areas <- topo$subsystems[[ss]]$areas
      lo <- min(ap$target_datetime) - lubridate::days(1L)
      hi <- max(ap$target_datetime)
      key <- as.character(as.numeric(ap$target_datetime))
      ss_true <- unname(.prevcarga_reconcile_truth(ctx, ss, lo, hi)[key])
      leaf_true <- lapply(areas, function(a) unname(.prevcarga_reconcile_truth(ctx, a, lo, hi)[key]))
      bu_true <- Reduce(`+`, leaf_true)
      l_true <- ss_true - bu_true
      ok <- is.finite(ss_true) & is.finite(bu_true)
      wm[[ss]] <- list(bottomup = prevcarga_wmape(ss_true[ok], ap$BU_pred[ok]),
                       reconciliado = prevcarga_wmape(ss_true[ok], ap$y_pred[ok]),
                       teto = prevcarga_wmape(ss_true[ok], ap$BU_pred[ok] + l_true[ok]))
      ap$y_true <- ss_true
      long[[ss]] <- prevcarga_reconcile_long(ap, ss, ctx)
    }
  }

  sin <- NULL
  if (setequal(sss, topo$order)) {
    ls <- prevcarga_reconcile_sin(long, topo)
    if (isTRUE(eval)) {
      yt <- Reduce(`+`, lapply(sss, function(ss) dets[[ss]]$y_true[match(ls$target_datetime, dets[[ss]]$target_datetime)]))
      ls$y_true <- yt
    }
    sin <- prevcarga_reconcile_long(ls, "SIN", ctx)
  }
  scorecard <- NULL
  if (isTRUE(eval)) {
    allv <- data.table::rbindlist(c(unname(long), if (!is.null(sin)) list(sin)), use.names = TRUE)
    scorecard <- prevcarga_scorecard(prevcarga_assert_long(allv, ctx), by = "area")
  }
  list(long = c(long, list(SIN = sin)), khat = khat, cobertura_descartada = cob,
       wmape = if (isTRUE(eval)) wm else NULL, scorecard = scorecard, warnings = warns)
}

## --- Reach-control das duas bordas da janela do k-hat (R.4) -----------------------------------------------------
## Bracos para prevcarga_reach_control (honesty_gate.R): perturbam SS_true (x factor + shift) so nas linhas do
## subsistema `ss` e so nos dias de interval_start do braco. Janela [O - W, O - 1]: FORBIDDEN = dias >= O e
## dias <= O - W - 1; CONTROL = O - 1 e O - W. `truth` = saida do leitor (area_code, interval_start, load_mwh).
prevcarga_khat_reach_arms <- function(origin, window_days, ss, factor = 1.3, shift = 1) {
  origin <- lubridate::as_date(origin)
  bump <- function(sel) function(tr) {
    d <- lubridate::as_date(tr$interval_start, tz = lubridate::tz(tr$interval_start))
    hit <- which(tr$area_code == ss & sel(d))
    if (length(hit)) data.table::set(tr, i = hit, j = "load_mwh", value = tr$load_mwh[hit] * factor + shift)
    tr
  }
  first <- origin - as.integer(window_days)
  list(forbidden = list(futuro = bump(function(d) d >= origin),
                        antes_da_janela = bump(function(d) d <= first - 1L)),
       control = list(ultimo_dia = bump(function(d) d == origin - 1L),
                      primeiro_dia = bump(function(d) d == first)))
}

## =====================================================================================================
## R.5 -- borda de I/O do verbo `reconcile` (cli/reconcile.R so compoe): ler o --leaves-dir, run_id, escrever no scratch.
## Requer tambem provenance.R (.prevcarga_canon_json/.prevcarga_sha256) e arrow/digest.
## =====================================================================================================

## --subsistema contra topology$order, antes de qualquer leitura de verdade ou perfil (topology.area_undeclared, exit 4)
prevcarga_reconcile_check_subsystem <- function(subsystem, topology) {
  if (is.null(subsystem) || identical(subsystem, "all")) return(invisible(topology$order))
  bad <- setdiff(subsystem, topology$order)
  if (length(bad)) prevcarga_abort("topology.area_undeclared", where = bad[[1L]])
  invisible(topology$order[topology$order %in% subsystem])
}

## Um parquet LONG por area em `dir` (ordem estavel radix); leitor unico do LONG (prevcarga_read_long). Area fora da
## topologia => topology.area_undeclared; arquivo com mais de uma area => contract.reconcile_input.
## Devolve list(leaves = list de data.frame nomeada pelo arquivo, sha256 = c(arquivo = sha256)).
prevcarga_reconcile_read_from <- function(dir, ctx, topology) {
  if (!is.character(dir) || length(dir) != 1L || !dir.exists(dir))
    prevcarga_abort("config.usage", where = paste0("--leaves-dir=", paste(dir, collapse = "")))
  fs <- list.files(dir, pattern = "\\.parquet$", full.names = FALSE)
  fs <- fs[order(fs, method = "radix")]
  if (!length(fs)) prevcarga_abort("contract.reconcile_input", where = dir,
                                   details = list(reason = "nenhum parquet no --leaves-dir"))
  leaves <- lapply(fs, function(f) {
    long <- prevcarga_read_long(file.path(dir, f), ctx)
    areas <- unique(long$area)
    if (length(areas) != 1L)
      prevcarga_abort("contract.reconcile_input", where = f, details = list(reason = "uma area por arquivo"))
    if (!areas %in% c(topology$leaves, topology$order))
      prevcarga_abort("topology.area_undeclared", where = areas)
    long
  })
  names(leaves) <- fs
  sha <- vapply(fs, function(f) digest::digest(file = file.path(dir, f), algo = "sha256"), "")
  list(leaves = leaves, sha256 = sha)
}

## run_id = 16 hex do sha256 do JSON canonico da chave; nunca PID nem relogio.
prevcarga_reconcile_run_id <- function(key) substr(.prevcarga_sha256(.prevcarga_canon_json(key)), 1L, 16L)

.reconcile_write_parquet <- function(long, path) {
  df <- as.data.frame(long)
  df <- df[order(df$target_datetime, df$lead, df$forecast_origin, method = "radix"), , drop = FALSE]
  rownames(df) <- NULL
  arrow::write_parquet(df, path, compression = "snappy", version = "2.6", use_dictionary = FALSE)
  nrow(df)
}

.reconcile_scratch_layout <- function(out) {
  fs <- list.files(out, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  all(grepl("^(SIN\\.parquet|subsistemas/[^/]+\\.parquet)$", fs))
}

## Escreve o scratch (`<out>/subsistemas/<ss>.parquet` + `<out>/SIN.parquet`) por diretorio `.tmp-<run_id>` irmao e
## file.rename so depois de todos os arquivos prontos; erro antes disso deixa `out` intocado e remove o `.tmp-*`.
## Devolve arquivos[] = list(path, area, linhas, sha256).
prevcarga_reconcile_write <- function(res, out, run_id) {
  parent <- dirname(out)
  if (!dir.exists(parent))
    prevcarga_abort("io.out_unwritable", where = out, details = list(reason = "diretorio ausente"))
  if (dir.exists(out) && length(list.files(out, all.files = TRUE, no.. = TRUE)) && !.reconcile_scratch_layout(out))
    prevcarga_abort("io.out_unwritable", where = out, details = list(reason = "diretorio nao vazio"))
  if (file.exists(out) && !dir.exists(out))
    prevcarga_abort("io.out_unwritable", where = out, details = list(reason = "ja existe como arquivo"))
  tmp <- file.path(parent, paste0(".tmp-", run_id))
  unlink(tmp, recursive = TRUE)
  if (!dir.create(file.path(tmp, "subsistemas"), recursive = TRUE, showWarnings = FALSE))
    prevcarga_abort("io.out_unwritable", where = out, details = list(reason = "nao criou o diretorio temporario"))
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  rows <- list()
  put <- function(long, rel, area) {
    p <- file.path(tmp, rel)
    n <- .reconcile_write_parquet(long, p)
    rows[[length(rows) + 1L]] <<- list(path = rel, area = area, linhas = as.integer(n),
                                       sha256 = digest::digest(file = p, algo = "sha256"))
  }
  for (ss in setdiff(names(res$long), "SIN"))
    if (!is.null(res$long[[ss]])) put(res$long[[ss]], file.path("subsistemas", paste0(ss, ".parquet")), ss)
  if (!is.null(res$long$SIN)) put(res$long$SIN, "SIN.parquet", "SIN")
  if (dir.exists(out)) unlink(out, recursive = TRUE)
  if (!file.rename(tmp, out))
    prevcarga_abort("io.out_unwritable", where = out, details = list(reason = "rename falhou"))
  rows
}
