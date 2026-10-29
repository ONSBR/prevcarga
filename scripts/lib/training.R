## training.R -- fachada de treino de UMA celula modelo x variante x area x origem (M5.3; ADR-0023, ADR-0039, ADR-0040)
##
## prevcarga_train(member, area, origin, ctx, force) compoe, sem calcular corte nem hora:
##   spec EFETIVO (topology.R: run.yaml + overrides) -> plugin (registry.R) -> painel pelo leitor unico (deck_reader.R)
##   -> cortes (dates.R: prevcarga_resolve_cuts + prevcarga_train_cut, classe de split) -> identidade content-addressed
##   v_<hash> (provenance.R) -> prevcarga_plugin_train (despachante) -> artefato auto-descritivo -> registry (M5.2).
## O M5 nao soma nem subtrai dias de corte: train_origin / last_data_day vem do dates.R com o data_lag_days do
## descriptor. Nenhum plugin faz conta de corte. O rotulo da carga e o deslocamento de modelagem sao do leitor e do
## despachante. `ctx` (alem do ctx da config): `spec_overrides` (lista de chaves pontilhadas: horizon_days, params.<k>),
## `code_commit`, `renv_lock_hash` (injetados; ausentes => calculados), `trained_at`, `run_id`, `version` (laterais).
## Requer errors.R, topology.R, provenance.R, deck_reader.R, dates.R, features.R e registry.R carregados antes.

## --- Variante e overrides -------------------------------------------------------------------------------
.prevcarga_tr_variant <- function(member) {
  exo <- member$exogena
  if (is.null(exo) || length(exo) != 1L || is.na(exo) || !nzchar(exo)) NA_character_ else as.character(exo)
}

## Overrides do ctx (--horizon, ...) mais a variante do membro na chave que o descriptor declara (`variant_param`).
.prevcarga_tr_overrides <- function(plugin, member, ctx) {
  ov <- if (is.null(ctx$spec_overrides)) list() else ctx$spec_overrides
  vp <- plugin$descriptor$variant_param
  exo <- .prevcarga_tr_variant(member)
  if (is.character(vp) && length(vp) == 1L && !is.na(vp) && !is.na(exo)) ov[[paste0("params.", vp)]] <- exo
  ov
}

## --- Painel e cortes da celula ------------------------------------------------------------------------------
## Devolve plugin, spec efetivo, cuts (formato do despachante) e o painel de TREINO ja cortado em last_data_day.
prevcarga_train_inputs <- function(member, area, origin, ctx) {
  origin <- lubridate::as_date(origin)
  if (is.null(ctx$weather_issue_cutoff)) ctx$weather_issue_cutoff <- "available_until"
  plugin <- prevcarga_plugin_load(member$mdir, prevcarga_models_root(ctx$root))
  prevcarga_plugin_check_signature(plugin)
  d <- plugin$descriptor
  spec <- prevcarga_model_spec(member$mdir, ctx$root, .prevcarga_tr_overrides(plugin, member, ctx))
  prevcarga_plugin_check_params(spec$params, d$params_schema, member$mdir)

  rc <- prevcarga_resolve_cuts(ctx, origin, model = member$mdir,
                               data_lag_days = stats::setNames(d$data_lag_days, member$mdir))
  row <- rc$cuts[rc$cuts$model == member$mdir, ]
  if (nrow(row) != 1L || !identical(as.integer(row$data_lag_days), as.integer(d$data_lag_days)))
    prevcarga_abort("contract.cut_mismatch", where = member$mdir,
      details = list(member = member$mdir, cut_data_lag_days = as.integer(row$data_lag_days),
                     descriptor_data_lag_days = as.integer(d$data_lag_days)))
  tc <- prevcarga_train_cut(spec, origin, row$data_lag_days)
  cuts <- list(forecast_origin = origin, origin_day = origin, origin = origin, tz = rc$tz,
               load_label = rc$load_label, available_until = rc$available_until,
               weather_issue_cutoff = rc$weather_issue_cutoff, weather_issue_limit = rc$weather_issue_limit,
               split = spec$split, train_origin = tc$train_origin, last_data_day = tc$last_data_day,
               data_lag_days = as.integer(row$data_lag_days))

  hist <- spec$history_days$train
  span <- hist + as.integer(origin - cuts$last_data_day)
  p <- prevcarga_read_panel(ctx, origin, area, spec$horizon_days, span)
  if (isTRUE(d$weather_forecast_in_train)) {
    fc <- prevcarga_read_weather(ctx, "forecast", area, origin - span, origin + spec$horizon_days)
    p$weather_forecast <- fc[issue_time <= cuts$weather_issue_limit]
  }
  day_of <- function(x) lubridate::as_date(x, tz = cuts$tz)
  first_day <- cuts$last_data_day - hist
  in_window <- function(x) day_of(x) <= cuts$last_data_day & day_of(x) > first_day
  panel <- p
  panel$load <- p$load[in_window(p$load$interval_start)]
  obs <- p$weather_observed[in_window(p$weather_observed$timestamp)]
  data.table::setnames(obs, "timestamp", "valid_time")          # adaptador: o leitor entrega `timestamp`; o despachante exige `valid_time`
  panel$weather_observed <- obs
  have <- length(unique(day_of(panel$load$interval_start)))
  if (have < hist)
    prevcarga_abort("window.history_short", where = member$mdir,
      details = list(available_days = have, required_days = hist, member = member$mdir))
  if (spec$granularity_min == 60L) panel <- to_hourly(panel)
  list(plugin = plugin, spec = spec, cuts = cuts, panel = panel)
}

## --- Identidade content-addressed -------------------------------------------------------------------------------
.prevcarga_tr_table_text <- function(x) {
  x <- as.data.frame(x)
  head <- paste(names(x), collapse = ",")
  if (!nrow(x)) return(head)
  cols <- lapply(x, function(v) {
    if (inherits(v, "POSIXct")) sprintf("%.3f", as.numeric(v))
    else if (inherits(v, "Date")) as.character(as.integer(v))
    else if (is.numeric(v)) sprintf("%.17g", v)
    else as.character(v)
  })
  paste(c(head, do.call(paste, c(unname(cols), sep = "\t"))), collapse = "\n")
}

## sha256 do conteudo do painel JA cortado (so dados; `attrs`, com available_until do deck, fica de fora).
.prevcarga_tr_panel_hash <- function(panel) {
  ds <- c("load", "weather_observed", "weather_forecast", "holidays")
  txt <- vapply(ds, function(n) .prevcarga_tr_table_text(panel[[n]]), "")
  digest::digest(paste(ds, txt, sep = "\n", collapse = "\n"), algo = "sha256", serialize = FALSE)
}

## Os 8 campos do contrato do provenance.R: cutoff = cortes efetivos da classe de split (origin_scoped: so a origem;
## available_until nunca entra); feature_config = spec efetivo + variante. Laterais ficam fora (run_manifest).
prevcarga_train_fingerprint <- function(member, spec, cuts, panel, ctx) {
  cortes <- if (identical(spec$split, "origin_scoped")) list(origin = cuts$origin) else
    list(train_origin = cuts$train_origin, last_data_day = cuts$last_data_day)
  fp <- list(
    model_id = member$mdir,
    data_fingerprint = .prevcarga_tr_panel_hash(panel),
    cutoff = cortes,
    feature_config = list(
      spec = spec[c("name", "split", "granularity_min", "horizon_days", "history_days", "seed", "params")],
      exogena = .prevcarga_tr_variant(member)),
    code_commit = if (is.null(ctx$code_commit)) prevcarga_code_commit(ctx$root) else ctx$code_commit,
    renv_lock_hash = if (is.null(ctx$renv_lock_hash)) prevcarga_renv_lock_hash(ctx$root) else ctx$renv_lock_hash,
    load_label = cuts$load_label,
    load_tz = cuts$tz)
  list(fingerprint = fp, version = prevcarga_version_hash(fp))
}

## --- Artefato e manifesto -------------------------------------------------------------------------------------
## `fit` e o artefato carimbado do despachante (stamp + payload de plugin$freeze). Dado puro: sem relogio, sem
## available_until. origin_scoped => `origin` = chave do artefato; origin_agnostic => sem o campo.
prevcarga_artifact_build <- function(member, area, inp, fit) {
  d <- inp$plugin$descriptor
  a <- list(model_id = member$mdir, variant = .prevcarga_tr_variant(member), area = area, split = inp$spec$split,
            spec_efetivo = inp$spec,
            cuts = list(train_origin = inp$cuts$train_origin, last_data_day = inp$cuts$last_data_day),
            load_label = inp$cuts$load_label, timezone = inp$cuts$tz,
            weather_issue_cutoff = inp$cuts$weather_issue_cutoff,
            fit = fit,
            contract = lapply(d$stages, function(s) list(requires = s$requires, provides = s$provides)))
  if (identical(inp$spec$split, "origin_scoped")) a$origin <- inp$cuts$origin
  prevcarga_plugin_scan_pure(a)
  structure(a, class = "prevcarga_train_artifact")
}

prevcarga_train_manifest <- function(id, member, inp, ctx) {
  exo <- .prevcarga_tr_variant(member)
  label <- if (is.na(exo)) member$mdir else paste(member$mdir, exo, sep = "/")
  sigla <- if (is.null(member$sigla)) member$mdir else as.character(member$sigla)
  m <- prevcarga_run_manifest(
    data_hash = id$fingerprint$data_fingerprint, origin = inp$cuts$origin, available_until = NULL,
    load_label = inp$cuts$load_label, load_tz = inp$cuts$tz,
    weather_issue_cutoff = inp$cuts$weather_issue_cutoff, split = inp$spec$split,
    delivery_days = inp$plugin$descriptor$delivery_days,
    model_versions = stats::setNames(list(id$version), label),
    seeds = stats::setNames(list(inp$spec$seed), label), root = ctx$root,
    trained_at = ctx$trained_at, package_version = ctx$version, run_id = ctx$run_id,
    members = data.frame(sigla = sigla, mdir = member$mdir, exogena = exo, stringsAsFactors = FALSE),
    fingerprint = id$fingerprint)
  m$code_commit <- id$fingerprint$code_commit
  m$renv_lock_hash <- id$fingerprint$renv_lock_hash
  m[setdiff(names(m), "available_until")]
}

prevcarga_cell_row <- function(member, area, cuts, version, status, path) {
  data.frame(model = member$mdir, variant = .prevcarga_tr_variant(member), area = area, split = cuts$split,
             train_origin = cuts$train_origin, last_data_day = cuts$last_data_day, version = version,
             status = status, path = path, stringsAsFactors = FALSE)
}

## --- Fachada --------------------------------------------------------------------------------------------------
prevcarga_train <- function(member, area, origin, ctx, force = FALSE) {
  inp <- prevcarga_train_inputs(member, area, origin, ctx)
  id <- prevcarga_train_fingerprint(member, inp$spec, inp$cuts, inp$panel, ctx)
  key <- prevcarga_registry_key(member, area, ctx)
  dest <- file.path(key, id$version)
  if (dir.exists(dest) && !isTRUE(force))
    return(prevcarga_cell_row(member, area, inp$cuts, id$version, "reused", dest))
  pctx <- list(load_label = inp$cuts$load_label, timezone = inp$cuts$tz,
               weather_issue_cutoff = inp$cuts$weather_issue_cutoff)
  fit <- prevcarga_plugin_train(inp$plugin, inp$panel, inp$cuts, inp$spec, pctx)
  art <- prevcarga_artifact_build(member, area, inp, fit)
  pub <- prevcarga_registry_publish(art, prevcarga_train_manifest(id, member, inp, ctx), key, id$version,
                                    force = isTRUE(force), ctx = ctx)
  prevcarga_registry_promote_latest(key, id$version, ctx = ctx)
  prevcarga_cell_row(member, area, inp$cuts, id$version, "trained", pub$path)
}

## =====================================================================================================
## M5.4 -- matriz de celulas, plano (--dry-run) e execucao serial/PSOCK de prevcarga_train (verbo train-models)
## =====================================================================================================
## A matriz = membros (topology.R: prevcarga_members) x areas (prevcarga_resolve_areas, topologia, nenhuma sigla literal).
## --jobs > 1: parallel::makePSOCKcluster (sem fork/mclapply; ADR-0038, Windows nativo). Cada worker e um R novo:
## carrega a lib pelo caminho do script, fixa o runtime NO PROCESSO (prevcarga_runtime_enforce; o pacote nao le env;
## sobrescreve TZ/locale/BLAS que o shell passou) e a semente vem do run.yaml (spec$seed), aplicada pelo plugin/despachante
## dentro do worker. Resultado reordenado pela chave (model, variant, area): independe de --jobs e da ordem de chegada.

.prevcarga_tr_worker_modules <- c("errors.R", "runtime.R", "config.R", "topology.R", "schema_long.R", "provenance.R",
                                  "deck_reader.R", "dates.R", "validation.R", "features.R", "registry.R", "training.R")

.prevcarga_tr_topo <- function(ctx) {
  if (!is.null(ctx$topology_path)) prevcarga_topology(path = ctx$topology_path, root = ctx$root)
  else prevcarga_topology(root = ctx$root)
}

## Seletores exclusivos: --area <a> | --subsistema <s> | --areas <a,b,c>; nenhum => todas as folhas.
## `--area`/`--areas` aceitam folha ou no de subsistema (a propria chave); `--subsistema` expande para as folhas-filhas.
## Mais de um seletor => contract.area_selector_conflict; fora da topologia => topology.area_undeclared.
prevcarga_resolve_areas <- function(area = NULL, subsistema = NULL, areas = NULL, ctx) {
  topo <- .prevcarga_tr_topo(ctx)
  given <- c(`--area` = !is.null(area), `--subsistema` = !is.null(subsistema), `--areas` = !is.null(areas))
  if (sum(given) > 1L)
    prevcarga_abort("contract.area_selector_conflict", where = paste(names(given)[given], collapse = " "))
  valid <- unique(c(topo$order, topo$leaves))
  chk <- function(x, flag) {
    bad <- setdiff(x, valid)
    if (length(bad)) prevcarga_abort("topology.area_undeclared", where = bad[[1L]], details = list(flag = flag))
    x
  }
  if (!is.null(area)) return(chk(as.character(area)[[1L]], "--area"))
  if (!is.null(areas)) {
    a <- trimws(unlist(strsplit(as.character(areas), ",", fixed = TRUE)))
    a <- a[nzchar(a)]
    if (!length(a)) prevcarga_abort("topology.area_undeclared", where = "--areas")
    return(unique(chk(a, "--areas")))
  }
  if (!is.null(subsistema)) {
    s <- as.character(subsistema)[[1L]]
    if (!(s %in% topo$order)) prevcarga_abort("topology.area_undeclared", where = s, details = list(flag = "--subsistema"))
    return(topo$subsystems[[s]]$areas)
  }
  topo$leaves
}

.prevcarga_tr_arg <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else v
}

## Membros filtrados por --model (lista; `mdir` ou `mdir/exogena`) e --exogena; identidade = (mdir, exogena).
.prevcarga_tr_select_members <- function(ctx, model = NULL, exogena = NULL) {
  mem <- if (!is.null(ctx$topology_path)) prevcarga_members(path = ctx$topology_path, root = ctx$root)
         else prevcarga_members(root = ctx$root)
  keep <- rep(TRUE, nrow(mem))
  if (!is.null(model)) {
    tok <- trimws(unlist(strsplit(as.character(model), ",", fixed = TRUE)))
    tok <- tok[nzchar(tok)]
    ident <- ifelse(is.na(mem$exogena), mem$mdir, paste(mem$mdir, mem$exogena, sep = "/"))
    hit <- vapply(seq_len(nrow(mem)), function(i) mem$mdir[[i]] %in% tok || ident[[i]] %in% tok, NA)
    for (t in tok) if (!(t %in% mem$mdir || t %in% ident))
      prevcarga_abort("config.unknown_model", where = t)
    keep <- keep & hit
  }
  if (!is.null(exogena)) {
    keep <- keep & !is.na(mem$exogena) & mem$exogena == as.character(exogena)[[1L]]
    if (!any(keep)) prevcarga_abort("config.unknown_model", where = as.character(exogena)[[1L]],
                                    details = list(flag = "--exogena"))
  }
  mem[keep, , drop = FALSE]
}

## Matriz de celulas: (sigla, model, variant, area), ordenada por (model, variant, area).
prevcarga_train_matrix <- function(args, ctx) {
  areas <- prevcarga_resolve_areas(.prevcarga_tr_arg(args, "--area"), .prevcarga_tr_arg(args, "--subsistema"),
                                   .prevcarga_tr_arg(args, "--areas"), ctx)
  mem <- .prevcarga_tr_select_members(ctx, .prevcarga_tr_arg(args, "--model"), .prevcarga_tr_arg(args, "--exogena"))
  g <- expand.grid(i = seq_len(nrow(mem)), area = areas, stringsAsFactors = FALSE)
  cells <- data.frame(sigla = mem$sigla[g$i], model = mem$mdir[g$i], variant = mem$exogena[g$i], area = g$area,
                      stringsAsFactors = FALSE)
  cells[order(cells$model, ifelse(is.na(cells$variant), "", cells$variant), cells$area, method = "radix"), , drop = FALSE]
}

.prevcarga_tr_member <- function(cells, i)
  list(sigla = cells$sigla[[i]], mdir = cells$model[[i]], exogena = cells$variant[[i]])

.prevcarga_tr_sort <- function(df) {
  df <- df[order(df$model, ifelse(is.na(df$variant), "", df$variant), df$area, method = "radix"), , drop = FALSE]
  rownames(df) <- NULL
  df
}

## --dry-run: cortes e versao-alvo por celula, sem gravar nada (status = "planned"). A leitura do deck e a mesma do treino.
prevcarga_train_plan <- function(cells, origin, ctx) {
  rows <- lapply(seq_len(nrow(cells)), function(i) {
    m <- .prevcarga_tr_member(cells, i)
    inp <- prevcarga_train_inputs(m, cells$area[[i]], origin, ctx)
    id <- prevcarga_train_fingerprint(m, inp$spec, inp$cuts, inp$panel, ctx)
    key <- prevcarga_registry_key(m, cells$area[[i]], ctx)
    prevcarga_cell_row(m, cells$area[[i]], inp$cuts, id$version, "planned", file.path(key, id$version))
  })
  .prevcarga_tr_sort(do.call(rbind, rows))
}

## Uma celula sem propagar a condicao (o worker devolve dado serializavel; o pai re-levanta com a classe original).
.prevcarga_tr_cell_error <- function(e) {
  structure(list(error_id = if (!is.null(e$id)) e$id else "generic.unexpected",
                 where = e$where, details = if (!is.null(e$id)) e$details else list(message = conditionMessage(e)),
                 message = conditionMessage(e)),
            class = "prevcarga_cell_error")
}

prevcarga_train_cell_safe <- function(cells, i, ctx, origin, force = FALSE) {
  tryCatch(prevcarga_train(.prevcarga_tr_member(cells, i), cells$area[[i]], origin, ctx, force = force),
           error = .prevcarga_tr_cell_error)
}

## Roda no worker (R novo): carrega a lib, executa hooks de teste (so ctx$worker_hooks) e fixa o runtime in-process.
## Devolve lista serializavel (nunca levanta): ok, ou o erro original (id/where/details) para o pai re-levantar.
prevcarga_worker_init <- function(lib, modules, hooks = list()) {
  tryCatch({
    options(prevcarga.lib = lib)
    for (m in modules) sys.source(file.path(lib, m), envir = globalenv())
    if (is.function(hooks$pre_runtime)) hooks$pre_runtime()
    prevcarga_runtime_enforce()
    if (is.function(hooks$post_runtime)) hooks$post_runtime()
    list(ok = TRUE)
  }, error = function(e)
    list(ok = FALSE, error_id = if (!is.null(e$id)) e$id else NA_character_, where = e$where,
         details = e$details, message = conditionMessage(e)))
}

.prevcarga_tr_reraise <- function(bad) {
  if (is.null(bad$error_id) || is.na(bad$error_id))
    prevcarga_abort("runtime.worker_unfixable", where = "worker", details = list(message = bad$message))
  prevcarga_abort(bad$error_id, where = bad$where, details = bad$details)
}

prevcarga_train_models <- function(cells, ctx, origin = NULL, jobs = 1L, force = FALSE) {
  if (is.null(origin)) origin <- prevcarga_resolve_origin(ctx)$origin
  jobs <- as.integer(jobs)
  idx <- seq_len(nrow(cells))
  if (!length(idx))
    return(data.frame(model = character(), variant = character(), area = character(), split = character(),
                      train_origin = as.Date(character()), last_data_day = as.Date(character()),
                      version = character(), status = character(), path = character(), stringsAsFactors = FALSE))
  res <- list()
  if (jobs <= 1L) {
    for (i in idx) {
      res[[i]] <- prevcarga_train_cell_safe(cells, i, ctx, origin, force)
      if (inherits(res[[i]], "prevcarga_cell_error")) break
    }
  } else {
    lib <- if (!is.null(ctx$lib_dir)) ctx$lib_dir else getOption("prevcarga.lib")
    mods <- if (!is.null(ctx$worker_modules)) ctx$worker_modules else .prevcarga_tr_worker_modules
    cl <- parallel::makePSOCKcluster(min(jobs, length(idx)))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    init <- parallel::clusterCall(cl, prevcarga_worker_init, lib, mods,
                                  if (is.null(ctx$worker_hooks)) list() else ctx$worker_hooks)
    for (r in init) if (!isTRUE(r$ok)) .prevcarga_tr_reraise(r)
    res <- parallel::parLapplyLB(cl, idx, function(i, cells, ctx, origin, force)
      prevcarga_train_cell_safe(cells, i, ctx, origin, force), cells = cells, ctx = ctx, origin = origin, force = force)
  }
  bad <- Find(function(r) inherits(r, "prevcarga_cell_error"), res)
  if (!is.null(bad)) prevcarga_abort(bad$error_id, where = bad$where, details = bad$details)
  .prevcarga_tr_sort(do.call(rbind, res))
}
