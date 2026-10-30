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
                              "dessem_semihourly_48",
                              "rf_wide_hourly_216")

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

## --- Verificacao wire-time da assinatura canonica (M5.1) ----------------------------------------------
## Reconfere, sobre o plugin que o loader registrou, a existencia e os formals de cada etapa, de freeze e de thaw
## contra `prevcarga_plugin_signatures` (fonte unica da forma canonica). Acumula TODAS as lacunas e aborta uma vez
## com `contract.plugin_signature` nomeando plugin e etapa(s); e a 1a acao de train-models/infer-model, antes de
## qualquer treino. Sem memoizacao: a conferencia e barata e uma copia adulterada do plugin nunca herda um aceite.
prevcarga_plugin_check_signature <- function(plugin) {
  d <- plugin$descriptor
  fns <- c(lapply(d$stages, function(s) s$fn), d[c("freeze", "thaw")])
  etapas <- unique(c(prevcarga_plugin_stages, names(d$stages), "freeze", "thaw"))
  gaps <- character()
  for (etapa in etapas) {
    fn <- fns[[etapa]]
    if (!is.function(fn)) { gaps <- c(gaps, sprintf("%s: ausente", etapa)); next }
    got <- names(formals(fn))
    if (is.null(got)) got <- character()
    if (etapa %in% c("inference_features", "inference", "forecast") && "params" %in% got)
      gaps <- c(gaps, sprintf("%s: formal params proibido (tudo vem do artefato)", etapa))
    else if (!identical(got, prevcarga_plugin_signatures[[etapa]]))
      gaps <- c(gaps, sprintf("%s: formals (%s)", etapa, paste(got, collapse = ", ")))
  }
  if (length(gaps)) {
    prevcarga_abort("contract.plugin_signature", where = paste(sub(":.*$", "", gaps), collapse = ","),
                    details = list(plugin = d$model_id, gaps = gaps))
  }
  invisible(TRUE)
}

prevcarga_plugin_env <- function(model, root) prevcarga_plugin_load(model, root)$env
prevcarga_plugin_descriptor <- function(model, root) prevcarga_plugin_load(model, root)$descriptor
prevcarga_plugin_unload_all <- function() {
  rm(list = ls(.prevcarga_plugin_cache, all.names = TRUE), envir = .prevcarga_plugin_cache)
  invisible(NULL)
}

## =====================================================================================================
## Contrato comum de freeze/thaw dos plugins (P.0d; ADR-0023). O plugin devolve DADO PURO em `freeze`; o
## despachante o carimba (stamp), varre a pureza e grava com prevcarga_freeze (M1.3, serializacao v3). Nada de
## `available_until`, relogio, caminho ou host no artefato: dois treinos identicos tem o mesmo sha256.
## `cuts` aqui = lista com origin_day, train_origin (Date) e tz; `ctx` = load_label/timezone/weather_issue_cutoff.
## =====================================================================================================

.prevcarga_plg_or <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

prevcarga_plugin_stamp <- function(artifact, plugin, spec, cuts, ctx = list()) {
  d <- plugin$descriptor
  vp <- d$variant_param
  variant <- NA_character_
  if (is.character(vp) && length(vp) == 1L && !is.na(vp) && !is.null(spec$params[[vp]]))
    variant <- as.character(spec$params[[vp]])
  scoped <- identical(spec$split, "origin_scoped")
  stamp <- list(
    model_id = d$model_id, variant = variant, split = spec$split,
    granularity_min = as.integer(spec$granularity_min),
    load_label = .prevcarga_plg_or(ctx$load_label, .prevcarga_plg_or(cuts$load_label, NA_character_)),
    timezone = .prevcarga_plg_or(ctx$timezone, .prevcarga_plg_or(cuts$tz, NA_character_)),
    weather_issue_cutoff = .prevcarga_plg_or(ctx$weather_issue_cutoff,
                                             .prevcarga_plg_or(cuts$weather_issue_cutoff, NA_character_)),
    params = spec$params, seed = as.integer(spec$seed), contract_version = as.integer(d$contract_version),
    train_origin = cuts$train_origin)
  if (scoped) stamp$origin_day <- cuts$origin_day
  structure(list(stamp = stamp, payload = artifact), class = "prevcarga_plugin_artifact")
}

.prevcarga_plg_clock_name <- "^(trained_at|created_at|created|timestamp|time_stamp|sys_time|now|hostname|host)$"

## Varredura recursiva (listas, atributos, slots S4). O primeiro achado aborta com o caminho no objeto.
prevcarga_plugin_scan_pure <- function(x, path = "$", allow_times = character()) {
  bad <- function(why) .prevcarga_plugin_abort("plugin.impure_artifact", where = path,
                                               reason = paste0(path, " (", why, ")"), details = list(reason = why, path = path))
  if (is.environment(x)) bad("environment")
  if (typeof(x) == "externalptr") bad("externalptr")
  if (is.function(x) && !is.primitive(x)) {
    e <- environment(x)
    if (!(identical(e, baseenv()) || identical(e, emptyenv()) || isNamespace(e) || is.null(e))) bad("closure")
  }
  if (inherits(x, "POSIXct") && !path %in% allow_times) bad("clock-like POSIXct")
  if (is.character(x) && any(grepl("^(/|[A-Za-z]:[\\\\/])", x))) bad("absolute path")
  if (isS4(x) && !is.environment(x)) {
    for (s in methods::slotNames(x))
      prevcarga_plugin_scan_pure(methods::slot(x, s), sprintf("slot(%s, '%s')", path, s), allow_times)
  }
  at <- attributes(x)
  for (a in setdiff(names(at), c("names", "dim", "dimnames", "class", "row.names", "levels")))
    if (!isS4(x)) prevcarga_plugin_scan_pure(at[[a]], sprintf("attr(%s, '%s')", path, a), allow_times)
  if (is.list(x)) {
    nm <- names(x)
    for (i in seq_along(x)) {
      lab <- if (!is.null(nm) && !is.na(nm[i]) && nzchar(nm[i])) nm[i] else as.character(i)
      sub <- paste0(path, "$", lab)
      if (grepl(.prevcarga_plg_clock_name, lab)) {
        path <- sub
        bad("clock field")
      }
      prevcarga_plugin_scan_pure(x[[i]], sub, allow_times)
    }
  }
  invisible(TRUE)
}

## Chamada pelo despachante de infer (P.0c). Cada divergencia => plugin.artifact_mismatch (contract, exit 2).
prevcarga_plugin_check_artifact <- function(artifact, plugin, spec, cuts) {
  fmt <- function(v) if (is.null(v)) "NULL" else paste(format(v), collapse = ",")
  chk <- function(ok, field, got, exp) if (!isTRUE(ok))
    .prevcarga_plugin_abort("plugin.artifact_mismatch", where = plugin$descriptor$model_id,
      reason = sprintf("%s: artefato=%s esperado=%s", field, fmt(got), fmt(exp)),
      details = list(field = field, artifact = got, expected = exp))
  chk(inherits(artifact, "prevcarga_plugin_artifact") && is.list(artifact$stamp), "class",
      class(artifact)[[1L]], "prevcarga_plugin_artifact")
  s <- artifact$stamp
  d <- plugin$descriptor
  chk(identical(s$model_id, d$model_id), "model_id", s$model_id, d$model_id)
  chk(identical(s$contract_version, as.integer(d$contract_version)), "contract_version",
      s$contract_version, d$contract_version)
  chk(identical(s$granularity_min, as.integer(spec$granularity_min)), "granularity_min",
      s$granularity_min, spec$granularity_min)
  chk(identical(s$split, spec$split), "split", s$split, spec$split)
  if (identical(spec$split, "origin_scoped"))
    chk(identical(s$origin_day, cuts$origin_day), "origin_day", s$origin_day, cuts$origin_day)
  ok <- inherits(s$train_origin, "Date") && length(s$train_origin) == 1L && !is.na(s$train_origin) &&
    isTRUE(s$train_origin <= cuts$train_origin)
  chk(ok, "train_origin", s$train_origin, cuts$train_origin)
  invisible(TRUE)
}

## Bytes portaveis: varre a pureza e congela (sha256 do arquivo). Persistencia/layout v_<hash>: M5.2.
prevcarga_plugin_freeze_bytes <- function(artifact, path) {
  prevcarga_plugin_scan_pure(artifact)
  prevcarga_freeze(artifact, path)
}

## Qualquer falha de leitura/integridade/classe vira plugin.artifact_corrupt (integrity, exit 9).
prevcarga_plugin_thaw <- function(path, plugin = NULL) {
  corrupt <- function(why) .prevcarga_plugin_abort("plugin.artifact_corrupt", where = path, reason = why)
  a <- tryCatch(prevcarga_thaw(path), error = function(e) corrupt(conditionMessage(e)))
  if (!inherits(a, "prevcarga_plugin_artifact")) corrupt("classe diferente de prevcarga_plugin_artifact")
  a
}

## =====================================================================================================
## Despachantes dos plugins (P.0c; ADR-0012/0019/0023, principio 7). prevcarga_plugin_train/_infer/_forecast
## compoem os estagios na ordem do blueprint, sem switch por nome de modelo. Conferem os insumos ANTES (nunca
## cortam nem preenchem: abortam [prevcarga/contract] nomeando intervalo/emissao/coluna), aplicam o deslocamento
## de modelagem do descriptor (to_model_frame/to_display_frame de features.R; unico ponto do repo), isolam RNG e
## globalenv e conferem o envelope DEPOIS contra prevcarga_delivery_grid. O plugin recebe o frame ja deslocado.
## `cuts` = lista no frame de EXIBICAO com origin_day (ou forecast_origin), train_origin, last_data_day (Date),
## tz, available_until (POSIXct), weather_issue_cutoff e weather_issue_limit (POSIXct). Requer features.R.
## =====================================================================================================

## Campos do envelope (constante em minusculas: o grep do M1 reserva o prefixo maiusculo para variaveis de ambiente).
prevcarga_envelope_fields <- c("model_id", "variant", "area", "origin_day", "available_until",
  "weather_issue_cutoff", "layout", "granularity_min", "values", "delivery", "meta")
.prevcarga_plugin_out_fields <- c("area", "values", "delivery", "meta")
.prevcarga_plugin_forbidden_fields <- c("forecast_origin", "lead", "d_plus", "y_true", "target_datetime")
.prevcarga_plugin_raw_cols <- c("timestamp", "load_label")

.prevcarga_plg_origin <- function(cuts) if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin

.prevcarga_plg_fmt <- function(x) if (!length(x)) "NA" else paste(format(x), collapse = ",")

## tz da coluna de tempo precisa ser o de cuts (todas as datas sao comparadas no fuso de cuts).
.prevcarga_plg_check_tz <- function(x, col, cuts) {
  v <- x[[col]]
  if (!inherits(v, "POSIXct") || !identical(lubridate::tz(v), cuts$tz))
    .prevcarga_plugin_abort("plugin.raw_timestamp", where = col,
      reason = sprintf("%s fora do fuso de cuts (%s)", col, cuts$tz), details = list(column = col, tz = cuts$tz))
  invisible(TRUE)
}

## --- Conferencia do painel de treino -----------------------------------------------------------------
.prevcarga_plugin_check_train_panel <- function(panel, cuts, d) {
  origin <- .prevcarga_plg_origin(cuts)
  expect_last <- origin - 1L - d$data_lag_days
  got <- cuts$last_data_day
  if (!inherits(got, "Date") || length(got) != 1L || is.na(got) || as.numeric(got) != as.numeric(expect_last))
    .prevcarga_plugin_abort("plugin.cuts_mismatch", where = d$model_id,
      reason = sprintf("last_data_day=%s esperado=%s (origem %s, data_lag_days %d)", .prevcarga_plg_fmt(got),
                       format(expect_last), format(origin), d$data_lag_days),
      details = list(last_data_day = got, expected = expect_last))
  over <- function(x, col, dataset) {
    if (!NROW(x)) return(invisible(TRUE))
    .prevcarga_plg_check_tz(x, col, cuts)
    day <- lubridate::as_date(x[[col]])
    bad <- which(day > got)
    if (length(bad))
      .prevcarga_plugin_abort("plugin.train_beyond_cut", where = dataset,
        reason = sprintf("%s.%s=%s depois de last_data_day=%s", dataset, col, format(min(x[[col]][bad])), format(got)),
        details = list(dataset = dataset, interval_start = format(min(x[[col]][bad])), last_data_day = got))
    invisible(TRUE)
  }
  over(panel$load, "interval_start", "load")
  over(panel$weather_observed, "valid_time", "weather_observed")
  invisible(TRUE)
}

## Colunas `data.<dataset>.<coluna>` dos `requires` dos estagios dados.
.prevcarga_plugin_require_data_cols <- function(panel, d, stages) {
  toks <- unique(unlist(lapply(intersect(stages, names(d$stages)), function(s) d$stages[[s]]$requires)))
  toks <- toks[grepl("^data\\.[^.]+\\..+$", toks)]
  for (tk in toks) {
    ds <- sub("^data\\.([^.]+)\\..+$", "\\1", tk)
    col <- sub("^data\\.[^.]+\\.(.+)$", "\\1", tk)
    x <- panel[[ds]]
    if (!is.data.frame(x) || !(col %in% names(x)))
      .prevcarga_plugin_abort("plugin.missing_column", where = ds,
        reason = sprintf("coluna %s ausente em %s (requires %s)", col, ds, tk),
        details = list(column = col, dataset = ds, token = tk))
  }
  invisible(TRUE)
}

.prevcarga_plugin_check_raw_cols <- function(panel) {
  for (ds in names(panel)) {
    x <- panel[[ds]]
    if (!is.data.frame(x)) next
    raw <- intersect(.prevcarga_plugin_raw_cols, names(x))
    if (length(raw))
      .prevcarga_plugin_abort("plugin.raw_timestamp", where = ds,
        reason = sprintf("coluna %s em %s: so o leitor converte rotulo de tempo", raw[[1L]], ds),
        details = list(column = raw[[1L]], dataset = ds))
  }
  invisible(TRUE)
}

## --- Conferencia do painel de infer -------------------------------------------------------------------
.prevcarga_plugin_check_infer_panel <- function(panel, cuts, d) {
  .prevcarga_plugin_check_raw_cols(panel)
  .prevcarga_plugin_require_data_cols(panel, d, c("ingestion", "inference_features", "inference"))
  au <- cuts$available_until
  ld <- panel$load
  if (NROW(ld)) {
    .prevcarga_plg_check_tz(ld, "interval_end", cuts)
    bad <- which(ld$interval_end > au)
    if (length(bad))
      .prevcarga_plugin_abort("plugin.infer_beyond_cut", where = "load",
        reason = sprintf("interval_end=%s depois de available_until=%s", format(min(ld$interval_end[bad])), format(au)),
        details = list(interval_end = format(min(ld$interval_end[bad])), available_until = format(au)))
  }
  wf <- panel$weather_forecast
  if (NROW(wf)) {
    lim <- if (!is.null(cuts$weather_issue_limit)) cuts$weather_issue_limit else panel$attrs$weather_issue_limit
    if (!inherits(lim, "POSIXct") || length(lim) != 1L || is.na(lim))
      .prevcarga_plugin_abort("plugin.cuts_mismatch", where = "weather_forecast",
        reason = "weather_issue_limit ausente em cuts", details = list(field = "weather_issue_limit"))
    bad <- which(wf$issue_time > lim)
    if (length(bad))
      .prevcarga_plugin_abort("plugin.weather_beyond_limit", where = "weather_forecast",
        reason = sprintf("issue_time=%s depois de weather_issue_limit=%s", format(max(wf$issue_time[bad])), format(lim)),
        details = list(issue_time = format(max(wf$issue_time[bad])), weather_issue_limit = format(lim)))
  }
  wo <- panel$weather_observed
  if (NROW(wo)) {
    .prevcarga_plg_check_tz(wo, "valid_time", cuts)
    bad <- which(wo$valid_time > au)
    if (length(bad))
      .prevcarga_plugin_abort("plugin.weather_beyond_limit", where = "weather_observed",
        reason = sprintf("valid_time=%s depois de available_until=%s", format(max(wo$valid_time[bad])), format(au)),
        details = list(valid_time = format(max(wo$valid_time[bad])), available_until = format(au)))
  }
  invisible(TRUE)
}

## --- Conferencia do envelope --------------------------------------------------------------------------
## Primeiro caminho (campo$campo) com nome proibido, em qualquer nivel (listas e colunas de data.frame).
.prevcarga_plugin_find_forbidden <- function(x, path = "envelope") {
  nm <- names(x)
  hit <- which(nm %in% .prevcarga_plugin_forbidden_fields)
  if (length(hit)) return(paste0(path, "$", nm[[hit[[1L]]]]))
  if (is.list(x) && !is.data.frame(x)) {
    for (i in seq_along(x)) {
      r <- .prevcarga_plugin_find_forbidden(x[[i]], paste0(path, "$", if (!is.null(nm) && nzchar(nm[[i]])) nm[[i]] else i))
      if (!is.null(r)) return(r)
    }
  }
  NULL
}

.prevcarga_plugin_misaligned <- function(model, why, details = NULL)
  .prevcarga_plugin_abort("plugin.output_misaligned", where = model, reason = why, details = details)

.prevcarga_plugin_check_delivery <- function(delivery, grid, model) {
  bad <- function(why, ...) .prevcarga_plugin_misaligned(model, why, details = list(...))
  if (!is.data.frame(delivery) || !identical(names(delivery), c("interval_end", "y_pred")))
    bad("delivery deve ser data.table(interval_end, y_pred)", field = "delivery")
  ie <- delivery$interval_end
  if (!inherits(ie, "POSIXct")) bad("delivery$interval_end nao e POSIXct", field = "delivery$interval_end")
  if (!is.numeric(delivery$y_pred)) bad("delivery$y_pred nao e numerico", field = "delivery$y_pred")
  if (!identical(ie, grid)) {
    n <- min(length(ie), length(grid))
    diff <- if (n) which(is.na(ie[seq_len(n)]) | ie[seq_len(n)] != grid[seq_len(n)]) else integer()
    i <- if (length(diff)) diff[[1L]] else n + 1L
    got <- if (i <= length(ie)) format(ie[[i]]) else "ausente"
    want <- if (i <= length(grid)) format(grid[[i]]) else "alem da grade"
    if (!length(diff) && length(ie) == length(grid)) {
      got <- paste0("tzone ", lubridate::tz(ie)); want <- paste0("tzone ", lubridate::tz(grid))
    }
    bad(sprintf("interval_end divergente na posicao %d: entregue=%s esperado=%s (n=%d, grade=%d)", i, got, want,
                length(ie), length(grid)), field = "delivery$interval_end", position = i, got = got, expected = want)
  }
  fin <- is.finite(delivery$y_pred)
  if (!all(fin)) {
    i <- which(!fin)[[1L]]
    bad(sprintf("y_pred nao finito em interval_end=%s", format(ie[[i]])), field = "delivery$y_pred",
        interval_end = format(ie[[i]]))
  }
  invisible(TRUE)
}

.prevcarga_plugin_check_envelope <- function(env, plugin, spec, cuts) {
  d <- plugin$descriptor
  m <- d$model_id
  bad <- function(why, ...) .prevcarga_plugin_misaligned(m, why, details = list(...))
  if (!is.list(env)) bad("envelope nao e lista", field = "envelope")
  fb <- .prevcarga_plugin_find_forbidden(env)
  if (!is.null(fb)) bad(sprintf("campo proibido %s", fb), field = fb)
  extra <- setdiff(names(env), prevcarga_envelope_fields)
  miss <- setdiff(prevcarga_envelope_fields, names(env))
  if (length(extra) || length(miss) || anyDuplicated(names(env)))
    bad(sprintf("campos do envelope: extra=[%s] ausentes=[%s]", paste(extra, collapse = ","), paste(miss, collapse = ",")),
        extra = extra, missing = miss)
  if (!identical(env$layout, d$native_layout)) bad("layout diferente de descriptor$native_layout", field = "layout")
  if (!identical(env$granularity_min, as.integer(spec$granularity_min))) bad("granularity_min diferente de spec", field = "granularity_min")
  if (!identical(env$available_until, cuts$available_until)) bad("available_until diferente de cuts", field = "available_until")
  if (!is.null(cuts$weather_issue_cutoff) && !identical(env$weather_issue_cutoff, cuts$weather_issue_cutoff))
    bad("weather_issue_cutoff diferente de cuts", field = "weather_issue_cutoff")
  if (!.prevcarga_plg_chr1(env$area)) bad("area deve ser chr escalar", field = "area")
  if (is.null(env$values)) bad("values ausente", field = "values")
  if (!is.list(env$meta)) bad("meta deve ser lista", field = "meta")
  .prevcarga_plugin_check_delivery(env$delivery, prevcarga_delivery_grid(cuts, d$delivery_days), m)
  invisible(TRUE)
}

## --- Guarda de efeitos: RNG isolado + snapshot de globalenv()/search() -----------------------------------
prevcarga_preserve_rng <- function(expr) {
  g <- globalenv()
  had <- exists(".Random.seed", envir = g, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = g, inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = g)
    else if (exists(".Random.seed", envir = g, inherits = FALSE)) rm(".Random.seed", envir = g)
  }, add = TRUE)
  force(expr)
}

.prevcarga_plg_rng_state <- function() {
  g <- globalenv()
  if (exists(".Random.seed", envir = g, inherits = FALSE)) get(".Random.seed", envir = g, inherits = FALSE) else NULL
}

## Unico ponto do repo onde a fundacao toca globalenv(). Simbolo novo ou pacote anexado e removido e acusado.
.prevcarga_plugin_guarded <- function(plugin, expr) {
  g <- globalenv()
  before <- c(ls(g, all.names = TRUE), search())
  state0 <- .prevcarga_plg_rng_state()
  rng_moved <- FALSE
  failed <- TRUE
  on.exit({
    now <- ls(g, all.names = TRUE)
    planted <- setdiff(now, c(before, ".Random.seed"))
    if (length(planted)) rm(list = planted, envir = g)
    for (p in setdiff(search(), before)) try(detach(p, character.only = TRUE), silent = TRUE)
  }, add = TRUE)
  out <- prevcarga_preserve_rng({
    r <- force(expr)
    rng_moved <- !identical(.prevcarga_plg_rng_state(), state0)
    r
  })
  new <- setdiff(c(ls(g, all.names = TRUE), search()), c(before, ".Random.seed"))
  if (length(new))
    .prevcarga_plugin_abort("plugin.global_side_effect", where = plugin$descriptor$model_id,
      reason = sprintf("simbolo ou pacote novo no ambiente global: %s", new[[1L]]), details = list(symbol = new[[1L]]))
  if (rng_moved && !isTRUE(plugin$descriptor$uses_rng))
    .prevcarga_plugin_abort("plugin.rng", where = plugin$descriptor$model_id,
      reason = "o plugin consumiu o gerador sem declarar uses_rng", details = list(model = plugin$descriptor$model_id))
  out
}

## --- Deslocamento de modelagem centralizado (ADR-0012/0019) --------------------------------------------
## So a carga (interval_start/interval_end) e o available_until de cuts; clima, dias e holidays intocados.
.prevcarga_plugin_to_model <- function(panel, cuts, o) {
  if (identical(as.integer(o), 0L)) return(list(panel = panel, cuts = cuts))
  pm <- panel
  for (col in c("interval_start", "interval_end")) pm$load <- to_model_frame(pm$load, o, col)
  cm <- cuts
  cm$available_until <- to_model_frame(data.table::data.table(available_until = cuts$available_until), o,
                                       "available_until")$available_until
  list(panel = pm, cuts = cm)
}

.prevcarga_plugin_check_mode <- function(plugin, spec, mode) {
  if (!(mode %in% spec$modes) || !(mode %in% plugin$descriptor$modes_implemented) ||
      (identical(mode, "forecast") && !identical(spec$split, "origin_scoped")))
    .prevcarga_plugin_abort("plugin.mode_unavailable", where = plugin$descriptor$model_id,
      reason = sprintf("modo %s indisponivel", mode), details = list(mode = mode))
  invisible(TRUE)
}

## --- Despachantes --------------------------------------------------------------------------------------
prevcarga_plugin_train <- function(plugin, panel, cuts, spec, ctx = list()) {
  d <- plugin$descriptor
  .prevcarga_plugin_check_mode(plugin, spec, "train")
  .prevcarga_plugin_check_train_panel(panel, cuts, d)
  .prevcarga_plugin_check_raw_cols(panel)
  .prevcarga_plugin_require_data_cols(panel, d, c("ingestion", "feature_engineering", "training"))
  m <- .prevcarga_plugin_to_model(panel, cuts, d$model_time_offset_min)
  st <- d$stages
  payload <- .prevcarga_plugin_guarded(plugin, {
    frame <- st$ingestion$fn(m$panel, m$cuts)
    feats <- st$feature_engineering$fn(frame, spec$params)
    fit <- st$training$fn(feats, spec$params, spec$seed)
    d$freeze(fit)
  })
  art <- prevcarga_plugin_stamp(payload, plugin, spec, cuts, ctx)
  prevcarga_plugin_scan_pure(art)
  art
}

.prevcarga_plugin_infer_impl <- function(plugin, artifact, panel, cuts, spec) {
  d <- plugin$descriptor
  prevcarga_plugin_check_artifact(artifact, plugin, spec, cuts)
  .prevcarga_plugin_check_infer_panel(panel, cuts, d)
  m <- .prevcarga_plugin_to_model(panel, cuts, d$model_time_offset_min)
  st <- d$stages
  payload <- artifact$payload
  raw <- .prevcarga_plugin_guarded(plugin, {
    frame <- st$ingestion$fn(m$panel, m$cuts)
    inf <- st$inference_features$fn(frame, payload)
    st$inference$fn(payload, inf, m$cuts)
  })
  mid <- d$model_id
  if (!is.list(raw)) .prevcarga_plugin_misaligned(mid, "saida de inference nao e lista", list(field = "envelope"))
  fb <- .prevcarga_plugin_find_forbidden(raw)
  if (!is.null(fb)) .prevcarga_plugin_misaligned(mid, sprintf("campo proibido %s", fb), list(field = fb))
  extra <- setdiff(names(raw), .prevcarga_plugin_out_fields)
  if (length(extra))
    .prevcarga_plugin_misaligned(mid, sprintf("campo inesperado %s na saida de inference", extra[[1L]]),
                                 list(field = extra[[1L]]))
  dl <- raw$delivery
  if (!is.data.frame(dl) || !("interval_end" %in% names(dl)) || !inherits(dl$interval_end, "POSIXct"))
    .prevcarga_plugin_misaligned(mid, "delivery sem interval_end POSIXct", list(field = "delivery"))
  dl <- to_display_frame(data.table::as.data.table(dl), d$model_time_offset_min, "interval_end")
  meta <- if (is.list(raw$meta)) raw$meta else list()
  meta$rng_deviation <- d$rng_deviation
  env <- list(model_id = d$model_id, variant = artifact$stamp$variant, area = raw$area,
    origin_day = .prevcarga_plg_origin(cuts), available_until = cuts$available_until,
    weather_issue_cutoff = artifact$stamp$weather_issue_cutoff, layout = d$native_layout,
    granularity_min = as.integer(spec$granularity_min), values = raw$values, delivery = dl, meta = meta)
  .prevcarga_plugin_check_envelope(env, plugin, spec, cuts)
  env
}

prevcarga_plugin_infer <- function(plugin, artifact, panel, cuts, ctx = list()) {
  .prevcarga_plugin_check_mode(plugin, plugin$spec, "infer")
  .prevcarga_plugin_infer_impl(plugin, artifact, panel, cuts, plugin$spec)
}

prevcarga_plugin_forecast <- function(plugin, panel, cuts, spec, ctx = list()) {
  .prevcarga_plugin_check_mode(plugin, spec, "forecast")
  art <- prevcarga_plugin_train(plugin, panel, cuts, spec, ctx)
  .prevcarga_plugin_infer_impl(plugin, art, panel, cuts, spec)
}

## =====================================================================================================
## M5.2 -- nucleo do registry local no layout do bucket (ADR-0039/ADR-0030). Requer provenance.R (freeze/thaw/
## run_manifest) alem de errors.R. Layout:
##   <state_dir>/artifacts/<mdir>/[<exogena>/]<area>/{latest.txt, .pin, .history, .tmp-*/, v_<hash>/}
##   v_<hash>/ = {artifact.rds, artifact.rds.manifest.json, run_manifest.json, SHA256SUMS} imutavel.
## So locais: .pin, .history, .tmp-*. Texto UTF-8 com LF. O pacote nunca fala rede: os oficiais chegam pelo
## `prevcarga-sync` (M8) neste mesmo layout e sao tratados igual aos proprios, sem conversao.
## Erros declarados em errors/registry_core.tsv. Publicacao/promocao atomicas = tempfile irmao + file.rename.
## =====================================================================================================
.prevcarga_reg_version_rx <- "^v_[0-9a-f]+$"

## normalizePath nao colapsa `..` em caminho inexistente: colapso lexical depois (links existentes ja resolvidos).
.prevcarga_reg_norm <- function(p) {
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  m <- regmatches(p, regexec("^([A-Za-z]:/|/)(.*)$", p))[[1L]]
  head <- if (length(m)) m[[2L]] else ""
  parts <- strsplit(if (length(m)) m[[3L]] else p, "/", fixed = TRUE)[[1L]]
  out <- character()
  for (s in parts[nzchar(parts) & parts != "."]) {
    if (identical(s, "..")) out <- out[-length(out)] else out <- c(out, s)
  }
  paste0(head, paste(out, collapse = "/"))
}

prevcarga_registry_root <- function(ctx) {
  if (!is.character(ctx$state_dir) || length(ctx$state_dir) != 1L || is.na(ctx$state_dir) || !nzchar(ctx$state_dir))
    prevcarga_abort("contract.path_outside_root", where = "state_dir ausente no ctx")
  .prevcarga_reg_norm(file.path(ctx$state_dir, "artifacts"))
}

## Aborta se o caminho normalizado nao estiver contido na raiz (recusa `..` e links que saiam dela).
prevcarga_registry_contain <- function(path, ctx) {
  root <- prevcarga_registry_root(ctx)
  p <- .prevcarga_reg_norm(path)
  if (!(identical(p, root) || startsWith(p, paste0(root, "/"))))
    prevcarga_abort("contract.path_outside_root", where = path, details = list(root = root))
  p
}

## Segmento de variante sse `member$exogena` nao for NA/vazia: decisao so pelo membro, nunca pelo nome do modelo.
prevcarga_registry_key <- function(member, area, ctx) {
  if (is.null(member$mdir) || is.null(member$exogena))
    prevcarga_abort("contract.variant_required", where = "membro sem mdir/exogena")
  exo <- member$exogena
  seg <- c(member$mdir, if (!is.na(exo) && nzchar(exo)) exo, area)
  prevcarga_registry_contain(do.call(file.path, as.list(c(prevcarga_registry_root(ctx), seg))), ctx)
}

.prevcarga_reg_read_lines <- function(path) {
  if (!file.exists(path)) return(character())
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  x <- trimws(x)
  x[nzchar(x)]
}

## Resolve uma versao existente da celula; nunca devolve NULL/vazio (todo caminho sem resposta aborta).
prevcarga_resolve_version <- function(key, mode = c("latest", "version", "pin"), version = NULL, ctx = NULL) {
  mode <- match.arg(mode)
  if (!is.null(ctx)) key <- prevcarga_registry_contain(key, ctx)
  v <- switch(mode,
    latest  = {
      lp <- file.path(key, "latest.txt")
      if (!file.exists(lp)) prevcarga_abort("state.artifact_missing", where = lp)
      utils::head(.prevcarga_reg_read_lines(lp), 1L)
    },
    version = version,
    pin     = {
      pl <- .prevcarga_reg_read_lines(file.path(key, ".pin"))
      if (!length(pl)) prevcarga_abort("state.pin_missing", where = file.path(key, ".pin"))
      utils::tail(pl, 1L)
    })
  if (!is.character(v) || length(v) != 1L || is.na(v) || !grepl(.prevcarga_reg_version_rx, v))
    prevcarga_abort("contract.version_malformed", where = key,
                    details = list(version = if (is.character(v) && length(v)) v[[1L]] else NA_character_))
  dir <- file.path(key, v)
  if (!is.null(ctx)) prevcarga_registry_contain(dir, ctx)
  if (!dir.exists(dir)) prevcarga_abort("state.artifact_missing", where = dir, details = list(version = v))
  v
}

.prevcarga_reg_write_bytes <- function(path, txt) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(txt)), con)
  invisible(path)
}

## SHA256SUMS: '<sha256>  <arquivo>' por linha (radix), LF, cobrindo todos os arquivos da pasta exceto ele mesmo.
.prevcarga_reg_sums_write <- function(dir) {
  fs <- setdiff(list.files(dir, all.files = TRUE, no.. = TRUE), "SHA256SUMS")
  fs <- fs[order(fs, method = "radix")]
  sha <- vapply(file.path(dir, fs), function(f) digest::digest(file = f, algo = "sha256"), "", USE.NAMES = FALSE)
  .prevcarga_reg_write_bytes(file.path(dir, "SHA256SUMS"), paste0(sha, "  ", fs, "\n", collapse = ""))
}

## Publica `v_<hash>/` de forma atomica (staging .tmp-<rand>/ irmao + file.rename). Devolve list(version, path, reused).
prevcarga_registry_publish <- function(artifact, manifest, key, version, force = FALSE, ctx = NULL) {
  if (!is.character(version) || length(version) != 1L || is.na(version) || !grepl(.prevcarga_reg_version_rx, version))
    prevcarga_abort("contract.version_malformed", where = key, details = list(version = as.character(version)))
  if (!is.null(ctx)) key <- prevcarga_registry_contain(key, ctx)
  final <- file.path(key, version)
  if (dir.exists(final) && !isTRUE(force))
    return(list(version = version, path = final, reused = TRUE))
  dir.create(key, recursive = TRUE, showWarnings = FALSE)
  stg <- tempfile(".tmp-", tmpdir = key)
  dir.create(stg)
  ok <- FALSE
  on.exit(if (!ok) unlink(stg, recursive = TRUE), add = TRUE)
  prevcarga_freeze(artifact, file.path(stg, "artifact.rds"))
  prevcarga_write_run_manifest(manifest, stg)
  .prevcarga_reg_sums_write(stg)
  fail <- function() prevcarga_abort("state.promote_failed", where = final)
  if (dir.exists(final)) {                                  # force: o antigo so sai depois de o novo entrar
    old <- tempfile(".tmp-old-", tmpdir = key)
    if (!isTRUE(file.rename(final, old))) fail()
    if (!isTRUE(file.rename(stg, final))) {
      file.rename(old, final)                               # restaura o anterior
      fail()
    }
    unlink(old, recursive = TRUE)
  } else if (!isTRUE(file.rename(stg, final))) fail()
  ok <- TRUE
  list(version = version, path = final, reused = FALSE)
}

## Carga: confere cada linha de SHA256SUMS ANTES do thaw; divergencia => integrity.artifact_hash (exit 9).
prevcarga_load_artifact <- function(key, version, ctx = NULL) {
  v <- prevcarga_resolve_version(key, "version", version, ctx = ctx)
  dir <- file.path(key, v)
  sp <- file.path(dir, "SHA256SUMS")
  if (!file.exists(sp)) prevcarga_abort("integrity.artifact_hash", where = sp, details = list(reason = "SHA256SUMS ausente"))
  ln <- .prevcarga_reg_read_lines(sp)
  m <- regmatches(ln, regexec("^([0-9a-f]{64})  (.+)$", ln))
  if (!length(ln) || any(lengths(m) != 3L))
    prevcarga_abort("integrity.artifact_hash", where = sp, details = list(reason = "SHA256SUMS malformado"))
  for (e in m) {
    f <- e[[3L]]
    fp <- file.path(dir, f)
    got <- if (!grepl("[/\\\\]", f) && file.exists(fp)) digest::digest(file = fp, algo = "sha256") else NA_character_
    if (is.na(got) || !identical(got, e[[2L]]))
      prevcarga_abort("integrity.artifact_hash", where = fp, details = list(file = f, want = e[[2L]], got = got))
  }
  prevcarga_thaw(file.path(dir, "artifact.rds"))
}

## Promove `latest.txt` (tempfile irmao + file.rename; nunca escrita direta sobre o ponteiro) e registra em .history.
prevcarga_registry_promote_latest <- function(key, version, ctx = NULL) {
  v <- prevcarga_resolve_version(key, "version", version, ctx = ctx)
  tmp <- tempfile(".tmp-latest-", tmpdir = key)
  .prevcarga_reg_write_bytes(tmp, paste0(v, "\n"))
  if (!isTRUE(file.rename(tmp, file.path(key, "latest.txt"))))   # o .tmp-* pode sobrar (interrupcao): e do `registry gc`
    prevcarga_abort("state.promote_failed", where = file.path(key, "latest.txt"))
  hp <- file.path(key, ".history")
  con <- file(hp, open = "ab")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(paste0(v, "\n")), con)
  invisible(v)
}

## =====================================================================================================
## M5.7 -- ciclo de vida do registry (promote/rollback/pin/unpin/gc) e leitura (ls/show). Unico dono do ciclo de vida
## (gate registry_lifecycle); o verbo `registry` (cli/registry.R) so despacha para estas funcoes. Erros novos em
## errors/registry_lifecycle.tsv; pin_missing/promote_failed vem do registry_core.tsv. Higiene local apenas (nunca S3);
## sem lock entre processos. Toda escrita de ponteiro/.pin = tempfile `.tmp-*` irmao + file.rename (staging unico, ADR-0030).
## =====================================================================================================
prevcarga_registry_subcommands <- c("ls", "show", "promote", "rollback", "pin", "unpin", "gc")
prevcarga_registry_usage <- paste0("prevcarga registry (", paste(prevcarga_registry_subcommands, collapse = "|"),
                                   ") [<model>] [<area>] [<version>] [--dry-run]")

.prevcarga_reg_is_tmp <- function(x) grepl("^[.]tmp-", x)

## link = o caminho resolvido difere do pai resolvido + nome (portavel: sem API de link; junction do Windows tambem)
.prevcarga_reg_is_link <- function(p) vapply(p, function(x)
  !identical(normalizePath(x, winslash = "/", mustWork = FALSE),
             file.path(normalizePath(dirname(x), winslash = "/", mustWork = FALSE), basename(x))), NA, USE.NAMES = FALSE)

## diretorio real (nao-link) -- o gc e as listagens nunca seguem links
.prevcarga_reg_real_dir <- function(p) dir.exists(p) & !.prevcarga_reg_is_link(p)

.prevcarga_reg_pins <- function(key) unique(.prevcarga_reg_read_lines(file.path(key, ".pin")))

.prevcarga_reg_versions <- function(key) {
  ents <- list.files(key, all.files = TRUE, no.. = TRUE)
  v <- ents[grepl(.prevcarga_reg_version_rx, ents) & .prevcarga_reg_real_dir(file.path(key, ents))]
  v[order(v, method = "radix")]
}

## latest.txt da celula, ou NA se ausente/vazio (leitura tolerante: ls/gc nao abortam por celula sem ponteiro)
.prevcarga_reg_latest_or_na <- function(key) {
  lp <- file.path(key, "latest.txt")
  l <- if (file.exists(lp)) utils::head(.prevcarga_reg_read_lines(lp), 1L) else character()
  if (length(l)) l else NA_character_
}

## .pin reescrito atomicamente (vazio => remove o arquivo)
.prevcarga_reg_pins_write <- function(key, pins) {
  pp <- file.path(key, ".pin")
  if (!length(pins)) {
    unlink(pp)
    return(invisible(pp))
  }
  tmp <- tempfile(".tmp-pin-", tmpdir = key)
  .prevcarga_reg_write_bytes(tmp, paste0(pins, "\n", collapse = ""))
  if (!isTRUE(file.rename(tmp, pp))) prevcarga_abort("state.promote_failed", where = pp)
  invisible(pp)
}

## khat_origin do perfil do Reconcile (NA para qualquer outro produto): o latest do k-hat so anda para frente
.prevcarga_reg_khat_origin <- function(key, version) {
  m <- tryCatch(prevcarga_read_run_manifest(file.path(key, version)), error = function(e) NULL)
  if (is.null(m) || !identical(m$produto, "reconcile") || !is.character(m$khat_origin) || length(m$khat_origin) != 1L)
    return(NA_character_)
  m$khat_origin
}

.prevcarga_reg_check_forward <- function(key, version) {
  to <- .prevcarga_reg_khat_origin(key, version)
  cur <- .prevcarga_reg_latest_or_na(key)
  if (is.na(to) || is.na(cur)) return(invisible(NULL))
  from <- .prevcarga_reg_khat_origin(key, cur)
  if (!is.na(from) && lubridate::ymd(to) < lubridate::ymd(from))
    prevcarga_abort("state.registry_khat_backward", where = key, details = list(latest = cur, to = version))
  invisible(NULL)
}

.prevcarga_reg_need_cell <- function(key) {
  if (!dir.exists(key)) prevcarga_abort("state.cell_missing", where = key)
  invisible(key)
}

## Promove `version` (deve existir) a latest.txt; delega a escrita atomica ao nucleo (M5.2).
prevcarga_registry_promote <- function(key, version, ctx = NULL) {
  if (!is.null(ctx)) key <- prevcarga_registry_contain(key, ctx)
  .prevcarga_reg_need_cell(key)
  v <- prevcarga_resolve_version(key, "version", version, ctx = ctx)
  .prevcarga_reg_check_forward(key, v)
  prevcarga_registry_promote_latest(key, v, ctx = ctx)
  list(key = key, latest = v)
}

## Volta ao penultimo latest distinto da .history que ainda existe em disco.
prevcarga_registry_rollback <- function(key, ctx = NULL) {
  if (!is.null(ctx)) key <- prevcarga_registry_contain(key, ctx)
  .prevcarga_reg_need_cell(key)
  hist <- unique(rev(.prevcarga_reg_read_lines(file.path(key, ".history"))))
  atual <- .prevcarga_reg_latest_or_na(key)
  prev <- setdiff(hist, atual)
  prev <- prev[grepl(.prevcarga_reg_version_rx, prev) & .prevcarga_reg_real_dir(file.path(key, prev))]
  if (!length(prev)) prevcarga_abort("state.registry_no_previous", where = key)
  prevcarga_registry_promote(key, prev[[1L]], ctx = ctx)
}

prevcarga_registry_pin <- function(key, version, ctx = NULL) {
  if (!is.null(ctx)) key <- prevcarga_registry_contain(key, ctx)
  .prevcarga_reg_need_cell(key)
  if (!is.character(version) || length(version) != 1L || is.na(version) || !grepl(.prevcarga_reg_version_rx, version))
    prevcarga_abort("contract.version_malformed", where = key, details = list(version = as.character(version)))
  if (!.prevcarga_reg_real_dir(file.path(key, version)))
    prevcarga_abort("state.pin_missing", where = file.path(key, version))
  pins <- union(.prevcarga_reg_pins(key), version)
  .prevcarga_reg_pins_write(key, pins)
  list(key = key, pinned = pins)
}

prevcarga_registry_unpin <- function(key, version, ctx = NULL) {
  if (!is.null(ctx)) key <- prevcarga_registry_contain(key, ctx)
  .prevcarga_reg_need_cell(key)
  pins <- .prevcarga_reg_pins(key)
  if (!is.character(version) || length(version) != 1L || is.na(version) || !(version %in% pins))
    prevcarga_abort("state.pin_missing", where = file.path(key, ".pin"))
  pins <- setdiff(pins, version)
  .prevcarga_reg_pins_write(key, pins)
  list(key = key, pinned = pins)
}

## Percorre `start` sem seguir links. Devolve list(cells = data.frame(path, latest, pinned, versions), tmp = chr).
## Celula = diretorio com latest.txt, .pin ou v_<hex>/; nunca desce em v_*/ nem em .tmp-*/. Entradas `.tmp-*` (arquivo
## ou diretorio, qualquer nivel) sao staging de algum escritor do registry (publish/promote/pin, combina, k-hat).
.prevcarga_reg_walk <- function(start) {
  rec <- function(d) {
    ents <- list.files(d, all.files = TRUE, no.. = TRUE)
    full <- file.path(d, ents)
    link <- .prevcarga_reg_is_link(full)
    isdir <- dir.exists(full) & !link
    isver <- grepl(.prevcarga_reg_version_rx, ents)
    vers <- ents[isver & isdir]
    out <- list(cells = list(), tmp = full[.prevcarga_reg_is_tmp(ents) & !link])
    if (length(vers) || any(ents %in% c("latest.txt", ".pin")))
      out$cells <- list(list(path = d, versions = vers[order(vers, method = "radix")]))
    for (s in ents[isdir & !isver & !.prevcarga_reg_is_tmp(ents)]) {
      sub <- rec(file.path(d, s))
      out$cells <- c(out$cells, sub$cells)
      out$tmp <- c(out$tmp, sub$tmp)
    }
    out
  }
  if (.prevcarga_reg_real_dir(start)) rec(start) else list(cells = list(), tmp = character())
}

## model (sigla ou mdir de prevcarga_members, ou nome de pasta do registry como combina/reconcile) -> segmentos
.prevcarga_reg_model_segs <- function(model, ctx) {
  mem <- tryCatch(prevcarga_members(root = ctx$root), error = function(e) NULL)
  if (!is.null(mem)) {
    i <- which(mem$sigla == model)
    if (length(i) == 1L) return(c(mem$mdir[[i]], if (!is.na(mem$exogena[[i]]) && nzchar(mem$exogena[[i]])) mem$exogena[[i]]))
  }
  model
}

.prevcarga_reg_rel <- function(path, root) substring(path, nchar(root) + 2L)

.prevcarga_reg_scope <- function(ctx, model = NULL, area = NULL) {
  root <- prevcarga_registry_root(ctx)
  segs <- if (is.null(model)) character() else .prevcarga_reg_model_segs(model, ctx)
  start <- do.call(file.path, as.list(c(root, segs)))
  prevcarga_registry_contain(start, ctx)
  list(root = root, start = start, area = area)
}

## Lista as celulas: list de list(key, latest, pinned, versions); `key` relativa a <state>/artifacts.
prevcarga_registry_ls <- function(ctx, model = NULL, area = NULL) {
  sc <- .prevcarga_reg_scope(ctx, model, area)
  w <- .prevcarga_reg_walk(sc$start)
  rows <- lapply(w$cells, function(cl) {
    list(key = .prevcarga_reg_rel(cl$path, sc$root), latest = .prevcarga_reg_latest_or_na(cl$path),
         pinned = intersect(.prevcarga_reg_pins(cl$path), cl$versions), versions = cl$versions)
  })
  if (!is.null(area)) rows <- Filter(function(r) identical(basename(r$key), area), rows)
  rows[order(vapply(rows, function(r) r$key, ""), method = "radix")]
}

## Campos do run_manifest.json de uma versao (default latest); read-only.
prevcarga_registry_show <- function(key, version = NULL, ctx = NULL) {
  if (!is.null(ctx)) key <- prevcarga_registry_contain(key, ctx)
  .prevcarga_reg_need_cell(key)
  v <- if (is.null(version)) prevcarga_resolve_version(key, "latest", ctx = ctx)
       else prevcarga_resolve_version(key, "version", version, ctx = ctx)
  m <- prevcarga_read_run_manifest(file.path(key, v))
  pick <- function(...) for (x in list(...)) if (!is.null(x)) return(x)
  fp <- m$fingerprint
  cuts <- pick(m$cuts, fp$cutoff)
  list(key = basename(key), version = v, model_id = pick(m$model_id, fp$model_id),
       cuts = list(train_origin = pick(cuts$train_origin, cuts$origin), last_data_day = cuts$last_data_day),
       load_label = m$load_label, timezone = pick(m$timezone, m$load_tz),
       weather_issue_cutoff = m$weather_issue_cutoff, code_commit = m$code_commit)
}

## Poda local: orfas v_<hex> (nem latest nem pinada) e `.tmp-*` orfaos de qualquer escritor, sob <state>/artifacts.
## Celula sem ponteiro algum (sem latest.txt nem .pin) nao tem v_* podada (nada a preservar-por-referencia); so seus .tmp-*.
prevcarga_registry_gc <- function(ctx, model = NULL, dry_run = FALSE) {
  sc <- .prevcarga_reg_scope(ctx, model)
  w <- .prevcarga_reg_walk(sc$start)
  rm_paths <- w$tmp
  kept <- list()
  for (cl in w$cells) {
    key <- .prevcarga_reg_rel(cl$path, sc$root)
    cur <- .prevcarga_reg_latest_or_na(cl$path)
    pins <- .prevcarga_reg_pins(cl$path)
    keep <- c(if (!is.na(cur)) cur, pins)
    if (!length(keep)) {
      if (length(cl$versions)) kept[[length(kept) + 1L]] <- data.frame(key = key, version = cl$versions, reason = "no_pointer")
      next
    }
    why <- ifelse(cl$versions %in% cur, "latest", ifelse(cl$versions %in% pins, "pinned", NA_character_))
    if (any(!is.na(why)))
      kept[[length(kept) + 1L]] <- data.frame(key = key, version = cl$versions[!is.na(why)], reason = why[!is.na(why)])
    rm_paths <- c(rm_paths, file.path(cl$path, cl$versions[is.na(why)]))
  }
  rm_paths <- sort(unique(rm_paths), method = "radix")
  contained <- vapply(rm_paths, function(p) prevcarga_registry_contain(p, ctx), "", USE.NAMES = FALSE)
  if (!isTRUE(dry_run)) for (p in contained) unlink(p, recursive = TRUE, force = TRUE)
  kdt <- if (length(kept)) data.table::as.data.table(do.call(rbind, kept))
         else data.table::data.table(key = character(), version = character(), reason = character())
  list(removed = .prevcarga_reg_rel(contained, sc$root), kept = kdt, dry_run = isTRUE(dry_run))
}

## Chave da celula a partir de <model> (sigla do membro, mdir ou pasta do registry) e <area>; celula inexistente => 13.
prevcarga_registry_cell_key <- function(ctx, model, area) {
  if (!is.character(model) || length(model) != 1L || !is.character(area) || length(area) != 1L)
    prevcarga_abort("config.usage", where = "<model> <area>")
  segs <- .prevcarga_reg_model_segs(model, ctx)
  key <- prevcarga_registry_key(list(mdir = segs[[1L]], exogena = if (length(segs) > 1L) segs[[2L]] else NA_character_),
                                area, ctx)
  if (!dir.exists(key)) prevcarga_abort("state.cell_missing", where = key)
  key
}

## Despacho unico do verbo: subcomando -> funcao do ciclo de vida/leitura (o handler da CLI nao nomeia nenhuma delas).
prevcarga_registry_run <- function(sub, ctx, model = NULL, area = NULL, version = NULL, dry_run = FALSE) {
  if (!(is.character(sub) && length(sub) == 1L && sub %in% prevcarga_registry_subcommands))
    prevcarga_abort("config.usage", where = "registry")
  cell <- function() prevcarga_registry_cell_key(ctx, model, area)
  switch(sub,
    ls       = prevcarga_registry_ls(ctx, model, area),
    show     = prevcarga_registry_show(cell(), version, ctx = ctx),
    promote  = prevcarga_registry_promote(cell(), version, ctx = ctx),
    rollback = prevcarga_registry_rollback(cell(), ctx = ctx),
    pin      = prevcarga_registry_pin(cell(), version, ctx = ctx),
    unpin    = prevcarga_registry_unpin(cell(), version, ctx = ctx),
    gc       = prevcarga_registry_gc(ctx, model = model, dry_run = dry_run))
}
