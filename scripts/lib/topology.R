## topology.R -- fonte unica da topologia e do contrato por-modelo (M1.2; ADR-0001/0009/0021)
##
## Responsabilidade: SO carregar e validar `config/topology.yaml` (arvore do SIN, perdas, khat_range,
## membros) e `models/<m>/run.yaml` (split, granularidade, horizonte, historico, seed, modos, params).
## Nenhuma topologia, faixa ou granularidade e literal neste arquivo: tudo vem do YAML.
## Caminho so por argumento (path/root); o pacote nao le variaveis de ambiente.
## Qualquer violacao => prevcarga_abort() classe contract, sem objeto parcial. Requer errors.R carregado.
##
## Expoe: prevcarga_topology(path, root), prevcarga_members(path, root), prevcarga_model_spec(model, root).

.prevcarga_read_yaml <- function(file, missing_id) {
  if (!file.exists(file)) prevcarga_abort(missing_id, where = file)
  tryCatch(yaml::read_yaml(file, eval.expr = FALSE),
    error = function(e) prevcarga_abort(missing_id, where = file,
      details = list(parse_error = conditionMessage(e))))
}
prevcarga_topology_path <- function(root) file.path(root, "config", "topology.yaml")
prevcarga_models_root  <- function(root) file.path(root, "models")

.prevcarga_is_chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.prevcarga_is_int1 <- function(x, min = 1) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x) && x >= min
}

.prevcarga_khat_effective <- function(ss_name, ss, default) {
  kr <- if (is.null(ss$khat_range)) default else ss$khat_range
  ok <- is.numeric(kr) && length(kr) == 2L && all(is.finite(kr)) &&
        kr[[1L]] > 0 && kr[[1L]] < kr[[2L]]
  if (!ok) {
    where <- if (is.null(ss$khat_range)) "khat_range" else paste0("subsystems.", ss_name, ".khat_range")
    prevcarga_abort("contract.khat_range_invalid", where = where)
  }
  as.numeric(kr)
}

.prevcarga_topology_keys <- c("schema_version", "khat_range", "subsystems", "order", "members")
.prevcarga_subsystem_keys <- c("areas", "loss", "khat_range")

prevcarga_topology <- function(path = prevcarga_topology_path(root), root) {
  raw <- .prevcarga_read_yaml(path, "contract.topology_file_missing")
  if (!is.list(raw) || is.null(names(raw)))
    prevcarga_abort("contract.topology_bad_value", where = path)
  for (k in setdiff(names(raw), .prevcarga_topology_keys))
    prevcarga_abort("contract.topology_unknown_key", where = k)
  for (k in setdiff(.prevcarga_topology_keys, names(raw)))
    prevcarga_abort("contract.topology_bad_value", where = k)
  if (!identical(as.integer(raw$schema_version), 1L) || length(raw$schema_version) != 1L)
    prevcarga_abort("contract.topology_schema_version", where = "schema_version")

  ss <- raw$subsystems
  if (!is.list(ss) || !length(ss) || is.null(names(ss)) || anyNA(names(ss)) || any(!nzchar(names(ss))))
    prevcarga_abort("contract.topology_bad_value", where = "subsystems")
  seen_areas <- character(); seen_loss <- character()
  for (s in names(ss)) {
    x <- ss[[s]]
    if (!is.list(x)) prevcarga_abort("contract.topology_bad_value", where = paste0("subsystems.", s))
    for (k in setdiff(names(x), .prevcarga_subsystem_keys))
      prevcarga_abort("contract.topology_unknown_key", where = paste0("subsystems.", s, ".", k))
    if (is.null(x$loss)) prevcarga_abort("contract.topology_loss_missing", where = s)
    if (!.prevcarga_is_chr1(x$loss)) prevcarga_abort("contract.topology_bad_value", where = paste0(s, ".loss"))
    if (x$loss %in% seen_loss) prevcarga_abort("contract.topology_loss_duplicated", where = x$loss)
    seen_loss <- c(seen_loss, x$loss)
    ar <- unlist(x$areas)
    if (!is.character(ar) || !length(ar) || anyNA(ar) || any(!nzchar(ar)))
      prevcarga_abort("contract.topology_bad_value", where = paste0(s, ".areas"))
    for (a in ar) {
      if (a %in% seen_areas) prevcarga_abort("contract.topology_area_duplicated", where = a)
      seen_areas <- c(seen_areas, a)
    }
  }
  ord <- unlist(raw$order)
  if (!is.character(ord)) prevcarga_abort("contract.topology_bad_value", where = "order")
  bad <- c(setdiff(names(ss), ord), setdiff(ord, names(ss)))
  if (length(bad)) prevcarga_abort("contract.topology_order_mismatch", where = bad[[1L]])
  if (!identical(ord, names(ss)) || anyDuplicated(ord))
    prevcarga_abort("contract.topology_order_mismatch", where = "order")

  default <- raw$khat_range
  out_ss <- lapply(stats::setNames(names(ss), names(ss)), function(s) {
    list(areas = unlist(ss[[s]]$areas, use.names = FALSE), loss = ss[[s]]$loss,
         khat_range = .prevcarga_khat_effective(s, ss[[s]], default))
  })
  .prevcarga_khat_effective("default", list(), default)
  list(subsystems = out_ss,
       leaves = unlist(lapply(ord, function(s) out_ss[[s]]$areas), use.names = FALSE),
       order = ord, sin = list(children = ord),
       khat_range_default = as.numeric(default), members_raw = raw$members)
}

prevcarga_members <- function(path = prevcarga_topology_path(root), root) {
  topo <- prevcarga_topology(path = path, root = root)
  mr <- topo$members_raw
  if (!is.list(mr) || !length(mr)) prevcarga_abort("contract.topology_bad_value", where = "members")
  rows <- lapply(mr, function(m) {
    for (k in setdiff(names(m), c("sigla", "mdir", "exogena")))
      prevcarga_abort("contract.topology_unknown_key", where = paste0("members.", k))
    if (!.prevcarga_is_chr1(m$sigla)) prevcarga_abort("contract.topology_member_invalid", where = "members.sigla")
    if (!.prevcarga_is_chr1(m$mdir)) prevcarga_abort("contract.topology_member_invalid", where = m$sigla)
    if (!is.null(m$exogena) && !.prevcarga_is_chr1(m$exogena))
      prevcarga_abort("contract.topology_member_invalid", where = m$sigla)
    data.frame(sigla = m$sigla, mdir = m$mdir,
               exogena = if (is.null(m$exogena)) NA_character_ else m$exogena,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  dup <- out$sigla[duplicated(out$sigla)]
  if (length(dup)) prevcarga_abort("contract.topology_member_invalid", where = dup[[1L]])
  for (i in seq_len(nrow(out))) {
    if (!file.exists(file.path(prevcarga_models_root(root), out$mdir[[i]], "run.yaml")))
      prevcarga_abort("contract.topology_member_invalid", where = paste0(out$sigla[[i]], ": ", out$mdir[[i]]))
  }
  rownames(out) <- NULL
  out
}

## --- contrato por modelo (run.yaml) -----------------------------------------------------------------
.prevcarga_run_keys <- c("name", "split", "granularity_min", "horizon_days", "history_days", "seed",
                         "modes", "backtest", "constraints", "params")
.prevcarga_run_forbidden <- c("resources", "vcpu", "mem_gb", "r_deps", "entrypoint", "train_args", "cut_invariant")

.prevcarga_constraints <- list(
  infer_history_equals_train = function(spec)
    identical(as.integer(spec$history_days$infer), as.integer(spec$history_days$train))
)
.prevcarga_apply_constraints <- function(spec) {
  for (k in names(spec$constraints)) {
    rule <- .prevcarga_constraints[[k]]
    if (is.null(rule)) prevcarga_abort("contract.run_yaml_unknown_key", where = paste0("constraints.", k))
    if (isTRUE(spec$constraints[[k]]) && !rule(spec))
      prevcarga_abort("contract.constraint_violated", where = paste0(spec$name, ": ", k))
  }
  spec
}

prevcarga_model_spec <- function(model, root) {
  if (!.prevcarga_is_chr1(model) || grepl("[/\\\\]", model))
    prevcarga_abort("contract.run_yaml_missing", where = as.character(model)[[1L]])
  file <- file.path(prevcarga_models_root(root), model, "run.yaml")
  raw <- .prevcarga_read_yaml(file, "contract.run_yaml_missing")
  if (!is.list(raw) || is.null(names(raw))) prevcarga_abort("contract.run_yaml_bad_value", where = file)
  for (k in names(raw)) {
    if (k %in% .prevcarga_run_forbidden) prevcarga_abort("contract.run_yaml_forbidden_key", where = k)
    if (!k %in% .prevcarga_run_keys) prevcarga_abort("contract.run_yaml_unknown_key", where = k)
  }
  bad <- function(k) prevcarga_abort("contract.run_yaml_bad_value", where = k)
  for (k in setdiff(.prevcarga_run_keys, c("seed", "constraints", "params")))
    if (is.null(raw[[k]])) bad(k)
  if (!identical(raw$name, model)) prevcarga_abort("contract.run_yaml_name_mismatch", where = as.character(raw$name)[[1L]])
  if (is.null(raw$seed)) prevcarga_abort("contract.seed_missing", where = "seed")
  if (!.prevcarga_is_int1(raw$seed, min = -.Machine$integer.max)) bad("seed")
  if (!.prevcarga_is_chr1(raw$split) || !raw$split %in% c("origin_agnostic", "origin_scoped")) bad("split")
  if (!.prevcarga_is_int1(raw$granularity_min) || !raw$granularity_min %in% c(30, 60)) bad("granularity_min")
  if (!.prevcarga_is_int1(raw$horizon_days)) bad("horizon_days")
  hd <- raw$history_days
  if (!is.list(hd)) bad("history_days")
  for (k in setdiff(names(hd), c("train", "infer")))
    prevcarga_abort("contract.run_yaml_unknown_key", where = paste0("history_days.", k))
  for (k in c("train", "infer"))
    if (!.prevcarga_is_int1(hd[[k]])) bad(paste0("history_days.", k))
  modes <- unlist(raw$modes)
  if (!is.character(modes) || !all(c("train", "infer") %in% modes) ||
      !all(modes %in% c("train", "infer", "forecast")) || anyDuplicated(modes)) bad("modes")
  bt <- raw$backtest
  if (!is.list(bt)) bad("backtest")
  for (k in setdiff(names(bt), "protocol_default"))
    prevcarga_abort("contract.run_yaml_unknown_key", where = paste0("backtest.", k))
  if (!.prevcarga_is_chr1(bt$protocol_default) ||
      !bt$protocol_default %in% c("fixed", "daily", "retrain_harness")) bad("backtest.protocol_default")
  cons <- if (is.null(raw$constraints)) list() else raw$constraints
  par <- if (is.null(raw$params)) list() else raw$params
  if (!is.list(cons) || (length(cons) && is.null(names(cons)))) bad("constraints")
  if (!is.list(par) || (length(par) && is.null(names(par)))) bad("params")
  spec <- list(name = raw$name, split = raw$split,
               granularity_min = as.integer(raw$granularity_min),
               horizon_days = as.integer(raw$horizon_days),
               history_days = list(train = as.integer(hd$train), infer = as.integer(hd$infer)),
               seed = as.integer(raw$seed), modes = modes,
               backtest = list(protocol_default = bt$protocol_default),
               constraints = cons, params = par)
  .prevcarga_apply_constraints(spec)
}
