## robustez.R -- guardas do tune_wsvm com escopo no env do motor (porta de _load_engine.R:264-341 do oraculo).
## O oraculo aplicava estas funcoes como patch pos-source no globalenv; aqui vivem no env do motor
## (SVM_DemandaMedia/SVMhorario chamam `tune_wsvm` sem `::`, que resolve aqui antes do pacote anexado).
## O wrapper chama o tune_wsvm REAL primeiro (bit-identico no caminho feliz) e so age no erro.
## O motor nunca guarda estado: a robustez sinaliza uma condicao classificada (dessem_engine_guard,
## subclasse de `message`) que o despachante do plugin coleta em meta$warnings via withCallingHandlers.

## Condicao classificada (nao e erro). `code` usa os ids do catalogo errors/dessem_engine.tsv.
dessem_engine_guard <- function(code, ...) {
  d <- list(...)
  txt <- switch(code,
    "warning.engine_empty_model_guard" = sprintf(
      "modelo-vazio contornado: dummies dropadas [%s] (minoria<=%s)",
      paste(d$dropped, collapse = ","), as.character(d$k)),
    "warning.engine_cross_clamped" = sprintf(
      "cross clampado %s->%s (amostra=%s)", as.character(d$from), as.character(d$to), as.character(d$n_eff)),
    paste(names(d), unlist(lapply(d, as.character)), sep = "=", collapse = " "))
  structure(class = c("dessem_engine_guard", "message", "condition"),
            list(message = paste0("[engine.dessem] ", code, ": ", txt, "\n"), call = NULL,
                 code = code, details = d))
}

## _load_engine.R:264
.tune_wsvm_real <- function(...) WeightSVM::tune_wsvm(...)

## _load_engine.R:265 -- guarda de modelo-vazio (dummies de calendario ultra-raras, ex.: GO 2024-10-05).
.wsvm_guard_retry <- function(.args, msg) {
  fpos <- which(vapply(.args, inherits, logical(1L), "formula"))
  if (!length(fpos)) stop(msg, call. = FALSE)
  fml <- .args[[fpos[1L]]]; dat <- .args$data
  if (is.null(dat)) stop(msg, call. = FALSE)
  labs <- attr(stats::terms(fml), "term.labels"); lhs <- all.vars(fml)[1L]
  minority <- vapply(labs, function(v) {
    x <- dat[[v]]
    if (is.null(x) || !all(x %in% c(0, 1))) return(Inf)   # nao-binaria: nunca dropa
    as.numeric(min(sum(x == 1), sum(x == 0)))
  }, numeric(1L))
  try_keep <- function(keep) {
    if (!length(keep)) return(NULL)
    a2 <- .args; a2[[fpos[1L]]] <- stats::reformulate(keep, lhs)
    tryCatch(do.call(.tune_wsvm_real, a2), error = function(e) NULL)
  }
  for (K in c(3, 10, 30)) {
    drop <- names(minority)[is.finite(minority) & minority <= K]
    if (!length(drop)) next
    r <- try_keep(setdiff(labs, drop))
    if (!is.null(r)) {
      message(dessem_engine_guard("warning.engine_empty_model_guard", dropped = drop, k = K))
      return(r)
    }
  }
  r <- try_keep(names(minority)[!is.finite(minority)])   # ultimo recurso: so continuas
  if (!is.null(r)) {
    message(dessem_engine_guard("warning.engine_empty_model_guard",
                                dropped = names(minority)[is.finite(minority)], k = Inf))
    return(r)
  }
  stop(msg, call. = FALSE)
}

## _load_engine.R:311 -- clamp de cross (amostra pequena de dia especial, caso TON).
.wsvm_clamp_cross <- function(.args, msg) {
  dat <- .args$data
  if (is.null(dat)) stop(msg, call. = FALSE)
  w <- .args$weight
  n_eff <- if (!is.null(w)) sum(w > 0) else nrow(dat)
  ctl <- if (!is.null(.args$tunecontrol)) .args$tunecontrol else WeightSVM::tune.control()
  cur <- if (is.null(ctl$cross)) 10L else as.integer(ctl$cross)
  new_cross <- min(cur, as.integer(n_eff))
  if (new_cross < 2L) stop(msg, call. = FALSE)           # amostra degenerada: re-propaga o erro original
  ctl$cross <- new_cross
  a2 <- .args; a2$tunecontrol <- ctl
  r <- tryCatch(do.call(.tune_wsvm_real, a2), error = function(e) NULL)
  if (is.null(r)) stop(msg, call. = FALSE)
  message(dessem_engine_guard("warning.engine_cross_clamped", from = cur, to = new_cross, n_eff = as.integer(n_eff)))
  r
}

## _load_engine.R:331 -- despachante. Sucesso devolve o objeto do real sem tocar.
tune_wsvm <- function(...) {
  .args <- list(...)
  out <- tryCatch(do.call(.tune_wsvm_real, .args),
                  error = function(e) structure(list(m = conditionMessage(e)), class = "dessem_tune_err"))
  if (!inherits(out, "dessem_tune_err")) return(out)
  if (grepl("sampling size", out$m, ignore.case = TRUE)) return(.wsvm_clamp_cross(.args, out$m))
  if (!grepl("empty|constant|scale", out$m, ignore.case = TRUE)) stop(out$m, call. = FALSE)
  .wsvm_guard_retry(.args, out$m)
}
