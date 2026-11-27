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
      "modelo-vazio contornado: variaveis dropadas [%s] (minoria<=%s)",
      paste(d$dropped, collapse = ","), as.character(d$k)),
    "warning.engine_cross_clamped" = sprintf(
      "cross clampado %s->%s (amostra=%s)", as.character(d$from), as.character(d$to), as.character(d$n_eff)),
    "warning.engine_tune_fallback" = sprintf(
      "tune sem solucao (%s); treino sem tune cost=%s epsilon=%s (amostra=%s)%s",
      as.character(d$reason), as.character(d$cost), as.character(d$epsilon), as.character(d$n_eff),
      if (is.null(d$cell)) "" else paste0(" celula: ", d$cell)),
    paste(names(d), unlist(lapply(d, as.character)), sep = "=", collapse = " "))
  structure(class = c("dessem_engine_guard", "message", "condition"),
            list(message = paste0("[engine.dessem] ", code, ": ", txt, "\n"), call = NULL,
                 code = code, details = d))
}

## _load_engine.R:264
.tune_wsvm_real <- function(...) WeightSVM::tune_wsvm(...)

## _load_engine.R:265 -- guarda de modelo-vazio (dummies de calendario ultra-raras, ex.: GO 2024-10-05). Desde a 0.1.10 tambem
## remove a variavel constante na amostra (minoria 0), binaria ou nao: ex. Exo_Temperatura = 27 C nas 34 amostras de CE as 18h
## (origem 2025-08-19, LIN_MXMN), que o wsvm nao escala e devolve sem vetor de suporte ("Model is empty!").
.wsvm_guard_retry <- function(.args, msg) {
  fpos <- which(vapply(.args, inherits, logical(1L), "formula"))
  if (!length(fpos)) stop(msg, call. = FALSE)
  fml <- .args[[fpos[1L]]]; dat <- .args$data
  if (is.null(dat)) return(.wsvm_small_sample(NA_integer_, 10L, NA_integer_, msg))
  labs <- attr(stats::terms(fml), "term.labels"); lhs <- all.vars(fml)[1L]
  minority <- vapply(labs, function(v) {
    x <- dat[[v]]
    if (is.null(x)) return(Inf)
    if (length(unique(x)) < 2L) return(0)                 # constante na amostra (binaria ou nao): o wsvm nao escala (0.1.10)
    if (!all(x %in% c(0, 1))) return(Inf)                 # nao-binaria com variacao: nunca dropa
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

## Fallback sem tune (decisao do Gabriel de 2026-11-17, ADR-046): quando o tune devolve modelo vazio ou dobra constante e a
## guarda encadeada (clamp de cross + .wsvm_guard_retry) tambem nao resolve, o perfil treina SEM tune com os hiperparametros
## padrao do wsvm (cost = 1, epsilon = 0.1; ambos pertencem aos `ranges` de todas as chaves de model_parameters.json), no
## lugar de abortar. So atua onde hoje a falha termina (aborto ou erro cru); onde o tune resolve, nada muda.
.wsvm_notune_defaults <- function() list(cost = 1, epsilon = 0.1)

## Argumentos do tune_wsvm que NAO seguem para o wsvm (o resto, p.ex. iter.max e kernel, o tune ja repassa ao wsvm).
.wsvm_tune_only <- function() c("train.x", "train.y", "weight", "use_zero_weight", "pre.check", "data", "validation.x",
                                "validation.y", "validation.weight", "weigthed.error", "ranges", "predict.func", "tunecontrol")

## Devolve um objeto de classe `tune` (best.model) ou NULL se o treino sem tune tambem falha. Emite a condicao classificada.
.wsvm_notune_fallback <- function(.args, msg, n_eff = NA_integer_) {
  fpos <- which(vapply(.args, inherits, logical(1L), "formula"))
  dat <- .args$data; w <- .args$weight
  if (!length(fpos) || is.null(dat) || is.null(w) || length(w) != NROW(dat)) return(NULL)
  keep <- if (isTRUE(.args$use_zero_weight)) rep(TRUE, length(w)) else w > 0
  rest <- .args[setdiff(names(.args), c(.wsvm_tune_only(), names(.args)[fpos]))]
  rest <- rest[nzchar(names(rest))]
  fit <- tryCatch(suppressWarnings(do.call(WeightSVM::wsvm,
           c(list(.args[[fpos[1L]]], weight = w[keep], data = dat[keep, , drop = FALSE]), .wsvm_notune_defaults(), rest))),
         error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  if (is.na(n_eff)) n_eff <- sum(keep)
  message(dessem_engine_guard("warning.engine_tune_fallback", reason = iconv(msg, to = "ASCII//TRANSLIT"),
                              cost = .wsvm_notune_defaults()$cost, epsilon = .wsvm_notune_defaults()$epsilon,
                              n_eff = as.integer(n_eff), kernel = .args$kernel))
  structure(list(best.parameters = as.data.frame(.wsvm_notune_defaults()), best.performance = NA_real_,
                 method = "wsvm", nparcomb = 0L, train.ind = NULL, sampling = "sem tune (fallback)",
                 performances = NULL, best.model = fit), class = c("tune_wsvm", "tune"))
}

## Identificacao da celula (data, hora, tipo de dia especial) nos chamadores do motor: o fallback e re-sinalizado com `cell`.
.engine_with_cell <- function(cell, expr) {
  withCallingHandlers(expr, dessem_engine_guard = function(g) {
    if (identical(g$code, "warning.engine_tune_fallback") && is.null(g$details$cell)) {
      d <- g$details; d$cell <- cell
      message(do.call(dessem_engine_guard, c(list("warning.engine_tune_fallback"), d)))
      invokeRestart("muffleMessage")
    }
  })
}

## Aborto terminal da amostra pequena: clamp e guarda de modelo vazio esgotados.
.wsvm_small_sample <- function(n_eff, from, to, msg) {
  engine_abort("engine.dessem_tune_small_sample", where = "_dessem-engine/tune_wsvm",
               details = list(n_eff = as.integer(n_eff), cross_from = as.integer(from),
                              cross_to = as.integer(to),
                              retry_message = iconv(msg, to = "ASCII//TRANSLIT")))
}

## _load_engine.R:311 -- clamp de cross (amostra pequena de dia especial, caso TON).
## Encadeia a guarda de modelo vazio quando o retry clampado falha com empty|constant|scale.
.wsvm_clamp_cross <- function(.args, msg) {
  dat <- .args$data
  if (is.null(dat)) return(.wsvm_small_sample(NA_integer_, 10L, NA_integer_, msg))
  w <- .args$weight
  n_eff <- if (!is.null(w)) sum(w > 0) else nrow(dat)
  ctl <- if (!is.null(.args$tunecontrol)) .args$tunecontrol else WeightSVM::tune.control()
  cur <- if (is.null(ctl$cross)) 10L else as.integer(ctl$cross)
  new_cross <- min(cur, as.integer(n_eff))
  if (new_cross < 2L) return(.wsvm_small_sample(n_eff, cur, new_cross, msg))
  ctl$cross <- new_cross
  a2 <- .args; a2$tunecontrol <- ctl
  r <- tryCatch(do.call(.tune_wsvm_real, a2),
                error = function(e) structure(list(m = conditionMessage(e)), class = "dessem_tune_err"))
  if (!inherits(r, "dessem_tune_err")) {
    message(dessem_engine_guard("warning.engine_cross_clamped", from = cur, to = new_cross, n_eff = as.integer(n_eff)))
    return(r)
  }
  if (!grepl("empty|constant|scale", r$m, ignore.case = TRUE)) {
    ## pre.check do tune: dobra sem vetor de suporte (alvo constante na dobra) = mesma familia do modelo vazio; sem guarda de dummies
    fb <- if (grepl("Model fitting fails", r$m, fixed = TRUE)) .wsvm_notune_fallback(.args, r$m, n_eff) else NULL
    return(if (is.null(fb)) .wsvm_small_sample(n_eff, cur, new_cross, r$m) else fb)
  }
  message(dessem_engine_guard("warning.engine_cross_clamped", from = cur, to = new_cross, n_eff = as.integer(n_eff)))
  g <- tryCatch(.wsvm_guard_retry(a2, r$m),
                error = function(e) structure(list(m = conditionMessage(e)), class = "dessem_tune_err"))
  if (!inherits(g, "dessem_tune_err")) return(g)
  fb <- .wsvm_notune_fallback(.args, g$m, n_eff)
  if (is.null(fb)) .wsvm_small_sample(n_eff, cur, new_cross, g$m) else fb
}

## _load_engine.R:331 -- despachante. Sucesso devolve o objeto do real sem tocar.
tune_wsvm <- function(...) {
  .args <- list(...)
  out <- tryCatch(do.call(.tune_wsvm_real, .args),
                  error = function(e) structure(list(m = conditionMessage(e)), class = "dessem_tune_err"))
  if (!inherits(out, "dessem_tune_err")) return(out)
  if (grepl("sampling size", out$m, ignore.case = TRUE)) return(.wsvm_clamp_cross(.args, out$m))
  if (grepl("Model fitting fails", out$m, fixed = TRUE)) {
    fb <- .wsvm_notune_fallback(.args, out$m)
    if (is.null(fb)) stop(out$m, call. = FALSE) else return(fb)
  }
  if (!grepl("empty|constant|scale", out$m, ignore.case = TRUE)) stop(out$m, call. = FALSE)
  g <- tryCatch(.wsvm_guard_retry(.args, out$m), error = function(e) e)
  if (!inherits(g, "error")) return(g)
  fb <- .wsvm_notune_fallback(.args, out$m)
  if (is.null(fb)) stop(g) else fb
}
