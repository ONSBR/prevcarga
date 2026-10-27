## registry.R -- fronteira de contrato Tier A (ADR-0010), wire-time. Fronteira=M1; ciclo de vida do registry=M5.
## Tier A = contract-verified; Tier B reservado e vazio. Estado so em memoria da sessao (sem disco, sem env).
## Requer errors.R (prevcarga_abort/prevcarga_condition) e topology.R (prevcarga_members) ja carregados.
## Expoe: prevcarga_register(spec, root), prevcarga_registry(), prevcarga_registry_reset(base).
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
