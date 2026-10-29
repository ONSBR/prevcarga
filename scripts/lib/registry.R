## registry.R -- fronteira de contrato Tier A (ADR-0010), wire-time. Fronteira=M1; ciclo de vida do registry=M5.
## Tier A = contract-verified; Tier B reservado e vazio. Estado so em memoria da sessao (sem disco, sem env).
## Requer errors.R (prevcarga_abort/prevcarga_condition) e topology.R (prevcarga_members) ja carregados.
## Expoe: prevcarga_register(spec, root), prevcarga_registry(), prevcarga_registry_reset(base).
## P.0b acrescenta o loader de plugins (fim do arquivo): prevcarga_plugin_load(model, root) e acessores;
## requer tambem validation.R (.prevcarga_deck_spec, base `data.*`) e deck_reader.R.
##
## spec <- list(id = chr(1), tier = "A", requires = chr, provides = chr, member = chr(1) | NULL)
## base de capacidades: prevcarga_registry_reset(base = chr); vocabulario concreto no M5; default vazio no M1.

.prevcarga_registry_state <- new.env()
.prevcarga_registry_state$entries <- list()
.prevcarga_registry_state$base <- character()

prevcarga_registry_reset <- function(base = character()) {
  if (!is.character(base)) prevcarga_abort("contract.registry_malformed", where = "base")
  .prevcarga_registry_state$entries <- list()
  .prevcarga_registry_state$base <- base
  invisible(NULL)
}

prevcarga_registry <- function() .prevcarga_registry_state$entries

## O catalogo traz a mensagem generica; o motivo concreto (capacidade, tier) e anexado a ela.
.prevcarga_registry_abort <- function(id, where, reason = NULL, details = NULL) {
  cond <- prevcarga_condition(id, where = where, details = details)
  if (!is.null(reason)) cond$message <- paste0(cond$message, ": ", reason)
  stop(cond)
}

.prevcarga_registry_validate <- function(spec) {
  if (!is.list(spec)) prevcarga_abort("contract.registry_malformed", where = "spec")
  if (!is.character(spec$id) || length(spec$id) != 1L || is.na(spec$id) || !nzchar(spec$id))
    prevcarga_abort("contract.registry_malformed", where = "id")
  for (f in c("requires", "provides")) {
    if (is.null(spec[[f]])) spec[[f]] <- character()
    if (!is.character(spec[[f]])) prevcarga_abort("contract.registry_malformed", where = f)
  }
  if (!identical(spec$tier, "A"))
    .prevcarga_registry_abort("contract.registry_tier_invalid", where = spec$id,
      reason = if (identical(spec$tier, "B")) "tier B reservado (ADR-0010)" else "tier deve ser A (ADR-0010)")
  if (spec$id %in% names(.prevcarga_registry_state$entries))
    prevcarga_abort("contract.registry_duplicate_id", where = spec$id)
  spec
}

.prevcarga_registry_check_member <- function(spec, root) {
  siglas <- prevcarga_members(root = root)$sigla
  if (!(spec$member %in% siglas))
    .prevcarga_registry_abort("contract.registry_unknown_member", where = spec$member, reason = spec$member)
  invisible(TRUE)
}

prevcarga_register <- function(spec, root = NULL) {
  spec <- .prevcarga_registry_validate(spec)
  if (!is.null(spec$member)) .prevcarga_registry_check_member(spec, root)
  st <- .prevcarga_registry_state
  available <- union(st$base, unlist(lapply(st$entries, `[[`, "provides"), use.names = FALSE))
  missing <- setdiff(spec$requires, available)
  if (length(missing))
    .prevcarga_registry_abort("contract.registry_requires_unmet", where = spec$id,
      reason = paste(missing, collapse = ", "), details = list(missing = missing))
  st$entries[[spec$id]] <- spec
  invisible(spec)
}

## =====================================================================================================
## Loader de plugins (P.0b; ADR-0001/0009/0010). `root` = diretorio dos plugins (ctx$models_root), nunca env var.
## Cada plugin vive em <root>/<model>/{run.yaml, R/}. Tudo e carregado em ENV ISOLADO: nada vai ao
## globalenv()/search(); pacotes via requireNamespace (sem attach); motor encadeado por engine_manifest().
## Nenhuma regra olha o nome do modelo: tudo e guiado pelo descriptor e pelo run.yaml (config-driven).
## =====================================================================================================

prevcarga_plugin_stages <- c("ingestion", "feature_engineering", "training", "inference_features", "inference")

## Assinaturas despachadas pelas fachadas (consumidas por P.0c e M5.1). `forecast` segue a forma de `inference`.
prevcarga_plugin_signatures <- list(
  ingestion = c("panel", "cuts"), feature_engineering = c("frame", "params"),
  training = c("features", "params", "seed"), inference_features = c("frame", "payload"),
  inference = c("payload", "inf_features", "cuts"), forecast = c("payload", "inf_features", "cuts"),
  freeze = "model", thaw = "payload")

## Layouts nativos de saida aceitos no descriptor (conjunto provisorio, ver discovery do P.0b).
prevcarga_native_layouts <- c("lgbm_day_48", "hw_matrix_nx48", "rf_horizon_list", "dessem_hourly_24",
                              "dessem_semihourly_48")

.prevcarga_run_yaml_keys <- c("name", "split", "granularity_min", "horizon_days", "history_days", "seed",
                              "modes", "backtest", "constraints", "params")
.prevcarga_plugin_fields <- c("model_id", "contract_version", "granularity_supported", "modes_implemented",
  "params_schema", "variant_param", "native_layout", "semihourly_rule", "delivery_days", "data_lag_days",
  "model_time_offset_min", "reads_day_O", "weather_forecast_in_train", "uses_rng", "history_infer_equals_train",
  "rng_deviation", "packages", "attach", "engine", "freeze", "thaw", "stages")
.prevcarga_plugin_param_types <- c("integer", "numeric", "logical", "character")

.prevcarga_plugin_cache <- new.env(parent = emptyenv())

## Aborto classificado com o motivo concreto (campo, arquivo:linha, token) anexado a mensagem do catalogo.
.prevcarga_plugin_abort <- function(id, where = NULL, reason = NULL, details = NULL)
  .prevcarga_registry_abort(id, where = where, reason = reason, details = details)

.prevcarga_plg_int1 <- function(x, min = 1) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x) && x >= min
}
.prevcarga_plg_lgl1 <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
.prevcarga_plg_chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.prevcarga_plg_chrs <- function(x) is.character(x) && length(x) >= 1L && !anyNA(x) && all(nzchar(x))

## --- 1. Caminhos e run.yaml ------------------------------------------------------------------------
.prevcarga_plugin_paths <- function(model, root) {
  if (!.prevcarga_plg_chr1(model) || grepl("[/\\\\]", model) || !.prevcarga_plg_chr1(root))
    .prevcarga_plugin_abort("plugin.not_found", where = as.character(model)[[1L]], reason = as.character(model)[[1L]])
  dir <- file.path(root, model)
  p <- list(dir = dir, run_yaml = file.path(dir, "run.yaml"), r_dir = file.path(dir, "R"))
  if (!file.exists(p$run_yaml) || !dir.exists(p$r_dir))
    .prevcarga_plugin_abort("plugin.not_found", where = model,
      reason = p$run_yaml, details = list(expected = p$run_yaml))
  p
}

.prevcarga_plugin_read_spec <- function(model, paths) {
  bad <- function(field) .prevcarga_plugin_abort("plugin.bad_run_yaml", where = model, reason = field,
                                                 details = list(model = model, field = field))
  raw <- .prevcarga_read_yaml(paths$run_yaml, "plugin.bad_run_yaml")
  if (!is.list(raw) || is.null(names(raw))) bad("run.yaml")
  for (k in names(raw)) if (!k %in% .prevcarga_run_yaml_keys)
    .prevcarga_plugin_abort("plugin.unknown_key", where = model, reason = k, details = list(model = model, key = k))
  if (!identical(raw[["name"]], model)) bad("name")
  if (!.prevcarga_plg_chr1(raw[["split"]]) || !raw[["split"]] %in% c("origin_agnostic", "origin_scoped")) bad("split")
  if (!.prevcarga_plg_int1(raw[["granularity_min"]]) || !raw[["granularity_min"]] %in% c(30, 60)) bad("granularity_min")
  if (!.prevcarga_plg_int1(raw[["horizon_days"]])) bad("horizon_days")
  hd <- raw[["history_days"]]
  if (!is.list(hd)) bad("history_days")
  for (k in c("train", "infer")) if (!.prevcarga_plg_int1(hd[[k]])) bad(paste0("history_days.", k))
  if (!is.numeric(raw[["seed"]]) || length(raw[["seed"]]) != 1L || !is.finite(raw[["seed"]]) ||
      raw[["seed"]] != floor(raw[["seed"]])) bad("seed")
  modes <- unlist(raw[["modes"]])
  if (!is.character(modes) || !all(c("train", "infer") %in% modes) ||
      !all(modes %in% c("train", "infer", "forecast")) || anyDuplicated(modes)) bad("modes")
  par <- if (is.null(raw[["params"]])) list() else raw[["params"]]
  if (!is.list(par) || (length(par) && is.null(names(par)))) bad("params")
  list(name = model, split = raw[["split"]], granularity_min = as.integer(raw[["granularity_min"]]),
       horizon_days = as.integer(raw[["horizon_days"]]),
       history_days = list(train = as.integer(hd[["train"]]), infer = as.integer(hd[["infer"]])),
       seed = as.integer(raw[["seed"]]), modes = modes, backtest = raw[["backtest"]],
       constraints = if (is.null(raw[["constraints"]])) list() else raw[["constraints"]], params = par)
}

## --- 2. So-definicoes e fonte isolada --------------------------------------------------------------
.prevcarga_plugin_defs_only <- function(file) {
  exprs <- parse(file, keep.source = TRUE)
  refs <- attr(exprs, "srcref")
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    ok <- is.call(e) && is.symbol(e[[1L]]) && as.character(e[[1L]]) %in% c("<-", "=") &&
      is.symbol(e[[2L]]) && is.call(e[[3L]]) && identical(e[[3L]][[1L]], as.name("function"))
    if (!ok) {
      where <- sprintf("%s:%d", basename(file), refs[[i]][1L])
      .prevcarga_plugin_abort("plugin.toplevel_code", where = where, reason = where)
    }
  }
  invisible(TRUE)
}

.prevcarga_plugin_r_files <- function(dir) {
  f <- list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)
  f[order(basename(f), method = "radix")]
}

.prevcarga_plugin_source_dir <- function(dir, env) {
  files <- .prevcarga_plugin_r_files(dir)
  for (f in files) .prevcarga_plugin_defs_only(f)
  for (f in files) sys.source(f, envir = env, keep.source = FALSE, toplevel.env = env)
  assign(".plugin_dir", normalizePath(dir, winslash = "/", mustWork = FALSE), envir = env)
  invisible(files)
}

## --- 3. Pacotes sem efeito global ------------------------------------------------------------------
.prevcarga_plugin_check_packages <- function(pkgs, model) {
  pkgs <- unique(as.character(pkgs))
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss))
    .prevcarga_plugin_abort("plugin.package_missing", where = model, reason = paste(miss, collapse = ", "),
                            details = list(packages = miss))
  invisible(TRUE)
}

## Copia as exportacoes dos pacotes `attach` para o env-imports (no lugar de library(); search() intacto).
.prevcarga_plugin_imports_env <- function(imports, attach) {
  for (p in attach) for (n in getNamespaceExports(p))
    assign(n, getExportedValue(p, n), envir = imports)
  invisible(imports)
}

## --- 4. Motor encadeado por instancia --------------------------------------------------------------
.prevcarga_plugin_engine_env <- function(engine, root, imports, model) {
  dir <- file.path(root, engine, "R")
  mfile <- file.path(dir, "manifest.R")
  if (!dir.exists(dir) || !file.exists(mfile))
    .prevcarga_plugin_abort("plugin.engine_missing", where = model, reason = dir, details = list(expected = dir))
  e <- new.env(parent = imports)
  .prevcarga_plugin_defs_only(mfile)
  sys.source(mfile, envir = e, keep.source = FALSE, toplevel.env = e)
  if (!exists("engine_manifest", envir = e, inherits = FALSE))
    .prevcarga_plugin_abort("plugin.engine_function_missing", where = model, reason = "engine_manifest")
  m <- get("engine_manifest", envir = e, inherits = FALSE)()
  bad <- function(field) .prevcarga_plugin_abort("plugin.bad_descriptor", where = model,
                                                 reason = paste0("engine_manifest$", field))
  if (!is.list(m)) bad("manifest")
  if (!.prevcarga_plg_int1(m[["contract_version"]])) bad("contract_version")
  if (!.prevcarga_plg_chrs(m[["files"]])) bad("files")
  if (!.prevcarga_plg_chrs(m[["required_functions"]])) bad("required_functions")
  pk <- m[["packages"]]
  if (!is.null(pk) && !is.character(pk)) bad("packages")
  .prevcarga_plugin_check_packages(pk, model)
  req <- m[["file_requires"]]
  for (f in m[["files"]]) {
    path <- file.path(dir, f)
    if (!file.exists(path))
      .prevcarga_plugin_abort("plugin.engine_missing", where = model, reason = path, details = list(expected = path))
    for (fn in req[[f]]) if (!exists(fn, envir = e, inherits = FALSE))
      .prevcarga_plugin_abort("plugin.engine_function_missing", where = model,
                              reason = paste0(fn, " (needed by ", f, ")"), details = list(file = f, fn = fn))
    .prevcarga_plugin_defs_only(path)
    sys.source(path, envir = e, keep.source = FALSE, toplevel.env = e)
  }
  assign(".engine_dir", normalizePath(dir, winslash = "/", mustWork = FALSE), envir = e)
  for (fn in m[["required_functions"]]) if (!exists(fn, envir = e, inherits = FALSE))
    .prevcarga_plugin_abort("plugin.engine_function_missing", where = model, reason = fn, details = list(fn = fn))
  e
}

## --- 5. Descriptor normalizado ---------------------------------------------------------------------
.prevcarga_plugin_check_signature <- function(fn, key, model) {
  got <- names(formals(fn))
  if (is.null(got)) got <- character()
  if (key %in% c("inference_features", "inference", "forecast") && "params" %in% got)
    .prevcarga_plugin_abort("plugin.params_formal", where = model, reason = key, details = list(stage = key))
  if (!identical(got, prevcarga_plugin_signatures[[key]]))
    .prevcarga_plugin_abort("plugin.bad_signature", where = model, reason = key, details = list(stage = key))
  invisible(TRUE)
}

.prevcarga_plugin_check_schema <- function(schema, model) {
  bad <- function(field) .prevcarga_plugin_abort("plugin.bad_descriptor", where = model, reason = field)
  if (!is.list(schema) || (length(schema) && is.null(names(schema)))) bad("params_schema")
  for (n in names(schema)) {
    def <- schema[[n]]
    f <- paste0("params_schema$", n)
    if (!is.list(def) || !.prevcarga_plg_chr1(def[["type"]]) ||
        !def[["type"]] %in% .prevcarga_plugin_param_types) bad(f)
    if (!is.null(def[["default"]])) bad(paste0(f, "$default"))
    for (k in setdiff(names(def), c("type", "allowed", "min", "max"))) bad(paste0(f, "$", k))
    for (k in c("min", "max")) if (!is.null(def[[k]]) && !(is.numeric(def[[k]]) && length(def[[k]]) == 1L)) bad(paste0(f, "$", k))
  }
  invisible(TRUE)
}

.prevcarga_plugin_normalize_descriptor <- function(d, model) {
  bad <- function(field) .prevcarga_plugin_abort("plugin.bad_descriptor", where = model, reason = field,
                                                 details = list(model = model, field = field))
  if (!is.list(d) || is.null(names(d))) bad("descriptor")
  for (k in setdiff(names(d), .prevcarga_plugin_fields))
    .prevcarga_plugin_abort("plugin.unknown_key", where = model, reason = paste0("descriptor$", k),
                            details = list(model = model, key = k))
  pick <- function(f, default) if (is.null(d[[f]])) default else d[[f]]
  if (!identical(d[["model_id"]], model)) bad("model_id")
  if (!.prevcarga_plg_int1(d[["contract_version"]])) bad("contract_version")
  gs <- d[["granularity_supported"]]
  if (!is.numeric(gs) || !length(gs) || anyNA(gs) || !all(gs %in% c(30, 60))) bad("granularity_supported")
  mi <- d[["modes_implemented"]]
  if (!is.character(mi) || !length(mi) || !all(mi %in% c("train", "infer", "forecast")) || anyDuplicated(mi))
    bad("modes_implemented")
  .prevcarga_plugin_check_schema(d[["params_schema"]], model)
  schema <- if (is.null(d[["params_schema"]])) list() else d[["params_schema"]]
  vp <- pick("variant_param", NA_character_)
  if (!(is.character(vp) && length(vp) == 1L && (is.na(vp) || vp %in% names(schema)))) bad("variant_param")
  if (!.prevcarga_plg_chr1(d[["native_layout"]]) || !d[["native_layout"]] %in% prevcarga_native_layouts) bad("native_layout")
  rule <- d[["semihourly_rule"]]
  if (xor(is.function(rule), 60 %in% gs) || (!is.null(rule) && !is.function(rule))) bad("semihourly_rule")
  if (!.prevcarga_plg_int1(d[["delivery_days"]], 1)) bad("delivery_days")
  lag <- pick("data_lag_days", 0L)
  if (!.prevcarga_plg_int1(lag, 0)) bad("data_lag_days")
  off <- pick("model_time_offset_min", 0L)
  if (!.prevcarga_plg_int1(off, 0)) bad("model_time_offset_min")
  flags <- list(reads_day_O = FALSE, weather_forecast_in_train = FALSE, uses_rng = FALSE,
                history_infer_equals_train = FALSE)
  for (f in names(flags)) {
    flags[[f]] <- pick(f, FALSE)
    if (!.prevcarga_plg_lgl1(flags[[f]])) bad(f)
  }
  rd <- d[["rng_deviation"]]
  if (!is.null(rd) && (!.prevcarga_plg_chr1(rd) || !isTRUE(flags$uses_rng))) bad("rng_deviation")
  pk <- pick("packages", character()); at <- pick("attach", character())
  if (!is.character(pk) || anyNA(pk)) bad("packages")
  if (!is.character(at) || anyNA(at)) bad("attach")
  eng <- d[["engine"]]
  if (!is.null(eng) && !.prevcarga_plg_chr1(eng)) bad("engine")
  for (f in c("freeze", "thaw")) {
    if (!is.function(d[[f]])) bad(f)
    .prevcarga_plugin_check_signature(d[[f]], f, model)
  }
  stages <- d[["stages"]]
  if (!is.list(stages) || is.null(names(stages))) bad("stages")
  extra <- setdiff(names(stages), c(prevcarga_plugin_stages, "forecast"))
  if (length(extra)) bad(paste0("stages$", extra[[1L]]))
  for (s in prevcarga_plugin_stages) if (is.null(stages[[s]])) bad(paste0("stages$", s))
  if (xor("forecast" %in% mi, !is.null(stages[["forecast"]]))) bad("stages$forecast")
  for (s in names(stages)) {
    st <- stages[[s]]
    if (!is.list(st) || !is.function(st[["fn"]])) bad(paste0("stages$", s, "$fn"))
    for (f in c("requires", "provides")) if (!.prevcarga_plg_chrs(st[[f]])) bad(paste0("stages$", s, "$", f))
    .prevcarga_plugin_check_signature(st[["fn"]], s, model)
  }
  out <- list(model_id = model, contract_version = as.integer(d[["contract_version"]]),
    granularity_supported = as.integer(gs), modes_implemented = mi, params_schema = schema,
    variant_param = vp, native_layout = d[["native_layout"]], semihourly_rule = rule,
    delivery_days = as.integer(d[["delivery_days"]]), data_lag_days = as.integer(lag),
    model_time_offset_min = as.integer(off), reads_day_O = flags$reads_day_O,
    weather_forecast_in_train = flags$weather_forecast_in_train, uses_rng = flags$uses_rng,
    history_infer_equals_train = flags$history_infer_equals_train, rng_deviation = rd,
    packages = pk, attach = at, engine = eng, freeze = d[["freeze"]], thaw = d[["thaw"]], stages = stages)
  out
}

## --- 6. Cruzamento descriptor x run.yaml -----------------------------------------------------------
.prevcarga_plugin_param_ok <- function(v, def) {
  ok <- switch(def[["type"]],
    integer = .prevcarga_plg_int1(v, -Inf),
    numeric = is.numeric(v) && length(v) == 1L && is.finite(v),
    logical = .prevcarga_plg_lgl1(v),
    character = is.character(v) && length(v) == 1L && !is.na(v), FALSE)
  if (!ok) return(FALSE)
  if (!is.null(def[["allowed"]]) && !(v %in% unlist(def[["allowed"]]))) return(FALSE)
  if (!is.null(def[["min"]]) && v < def[["min"]]) return(FALSE)
  if (!is.null(def[["max"]]) && v > def[["max"]]) return(FALSE)
  TRUE
}

prevcarga_plugin_check_params <- function(params, schema, model) {
  params <- if (is.null(params)) list() else params
  bad <- function(n) .prevcarga_plugin_abort("plugin.bad_run_yaml", where = model, reason = paste0("params$", n),
                                             details = list(model = model, field = paste0("params$", n)))
  extra <- setdiff(names(params), names(schema))
  if (length(extra)) bad(extra[[1L]])
  miss <- setdiff(names(schema), names(params))
  if (length(miss)) bad(miss[[1L]])
  for (n in names(schema)) if (!.prevcarga_plugin_param_ok(params[[n]], schema[[n]])) bad(n)
  invisible(TRUE)
}

.prevcarga_plugin_check_spec <- function(d, spec) {
  model <- d$model_id
  bad <- function(field) .prevcarga_plugin_abort("plugin.bad_run_yaml", where = model, reason = field,
                                                 details = list(model = model, field = field))
  if (!(spec$granularity_min %in% d$granularity_supported)) bad("granularity_min")
  if (!all(spec$modes %in% d$modes_implemented)) bad("modes")
  if (xor("forecast" %in% spec$modes, identical(spec$split, "origin_scoped"))) bad("modes")
  if (isTRUE(d$history_infer_equals_train) && !identical(spec$history_days$infer, spec$history_days$train))
    .prevcarga_plugin_abort("plugin.history_window", where = model, reason = "history_days.infer",
                            details = list(model = model, field = "history_days.infer"))
  prevcarga_plugin_check_params(spec$params, d$params_schema, model)
}

## --- 7. Registro Tier A atomico --------------------------------------------------------------------
## Tokens `data.<dataset>.<coluna>` do contrato canonico do deck (validation.R, consumido do leitor M2.1).
.prevcarga_plugin_base_tokens <- function() {
  fn <- get0(".prevcarga_deck_spec", mode = "function")
  if (is.null(fn)) prevcarga_abort("plugin.contract", where = "deck_spec")
  sp <- fn()
  unique(c(paste0("data.", sp$dataset, ".", sp$column), paste0("data.", unique(sp$dataset), ".area_code")))
}

## Simula a cadeia inteira contra `available`; nada e gravado.
.prevcarga_plugin_simulate_chain <- function(entries, base) {
  available <- base
  for (e in entries) {
    miss <- setdiff(e$requires, available)
    if (length(miss))
      .prevcarga_plugin_abort("plugin.provides_gap", where = e$id, reason = paste0(miss[[1L]], " (", e$id, ")"),
                              details = list(token = miss[[1L]], stage = e$id, missing = miss))
    available <- union(available, e$provides)
  }
  invisible(TRUE)
}

.prevcarga_plugin_register <- function(d, model) {
  q <- function(tok) ifelse(startsWith(tok, "data."), tok, paste0(model, "::", tok))
  stages <- intersect(c(prevcarga_plugin_stages, "forecast"), names(d$stages))
  entries <- lapply(stages, function(s) list(id = paste0(model, "/", s), tier = "A",
    requires = q(d$stages[[s]]$requires), provides = q(d$stages[[s]]$provides)))
  st <- .prevcarga_registry_state
  base <- .prevcarga_plugin_base_tokens()
  .prevcarga_plugin_simulate_chain(entries, union(st$base, base))
  st$base <- union(st$base, base)
  st$entries[intersect(vapply(entries, `[[`, "", "id"), names(st$entries))] <- NULL
  for (e in entries) prevcarga_register(e)
  invisible(entries)
}

## --- 8. Orquestracao publica e memoizacao ----------------------------------------------------------
.prevcarga_plugin_build <- function(model, root) {
  paths <- .prevcarga_plugin_paths(model, root)
  spec <- .prevcarga_plugin_read_spec(model, paths)
  imports <- new.env(parent = baseenv())
  env <- new.env(parent = imports)
  .prevcarga_plugin_source_dir(paths$r_dir, env)
  if (!exists("plugin_descriptor", envir = env, inherits = FALSE))
    .prevcarga_plugin_abort("plugin.bad_descriptor", where = model, reason = "plugin_descriptor")
  raw <- tryCatch(get("plugin_descriptor", envir = env, inherits = FALSE)(),
    prevcarga_error = function(e) stop(e),
    error = function(e) .prevcarga_plugin_abort("plugin.bad_descriptor", where = model,
                                                reason = "plugin_descriptor() falhou"))
  d <- .prevcarga_plugin_normalize_descriptor(raw, model)
  .prevcarga_plugin_check_packages(c(d$packages, d$attach), model)
  .prevcarga_plugin_imports_env(imports, d$attach)
  if (!is.null(d$engine)) {
    eng <- .prevcarga_plugin_engine_env(d$engine, root, imports, model)
    parent.env(env) <- eng
  }
  .prevcarga_plugin_check_spec(d, spec)
  structure(list(model = model, root = root, env = env, descriptor = d, spec = spec), class = "prevcarga_plugin")
}

prevcarga_plugin_load <- function(model, root) {
  key <- paste(normalizePath(root, winslash = "/", mustWork = FALSE), model)
  hit <- .prevcarga_plugin_cache[[key]]
  if (!is.null(hit)) {
    if (!paste0(model, "/ingestion") %in% names(prevcarga_registry()))
      .prevcarga_plugin_register(hit$descriptor, model)
    return(hit)
  }
  p <- .prevcarga_plugin_build(model, root)
  .prevcarga_plugin_register(p$descriptor, model)
  assign(key, p, envir = .prevcarga_plugin_cache)
  p
}

prevcarga_plugin_env <- function(model, root) prevcarga_plugin_load(model, root)$env
prevcarga_plugin_descriptor <- function(model, root) prevcarga_plugin_load(model, root)$descriptor
prevcarga_plugin_unload_all <- function() {
  rm(list = ls(.prevcarga_plugin_cache, all.names = TRUE), envir = .prevcarga_plugin_cache)
  invisible(NULL)
}
