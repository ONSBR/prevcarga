## combination.R -- nucleo numerico da combina (M4.1): simplex, FISTA, produtores e mascara de treino
##
## Responsabilidade: modulo numerico in-process (base + data.table + lubridate, sem dependencia nova).
## Pipeline da combina: discover -> assert_members_aligned -> build_panel -> join_fresh_truth ->
## fit por (lead, cut, ym) -> produce. Este arquivo cobre so o fit e o produce; os demais passos
## vivem nos tickets seguintes do M4.
##
## Leak-fix: o treino de peso de um mes-de-ORIGEM m so usa alvos ja conhecidos em inicio(m); o filtro e
## UMA funcao (prevcarga_combina_train_mask) e o eixo de agrupamento continua sendo o mes-de-ORIGEM.
## A/B por propriedade+qualidade (ADR-0014), nao por delta=0: a saida diverge do legado de proposito.
## Gatear em y_pred, nunca em w (pesos de problema mal-condicionado nao sao unicos).
##
## Proibidos aqui: leitura de ambiente, relogio, conversoes de data fora do lubridate, RNG,
## sigla literal de membro. Erros por prevcarga_abort (errors.R; ids em errors/combination.tsv).
## Saida de terminal/mensagens: ASCII puro, sem acento.

prevcarga_combina_defaults <- list(n_min = 24L, iters = 500L, grad_tol = 1e-6)

## --- validacoes internas -------------------------------------------------------------------------
.prevcarga_cmb_need <- function(panel, cols, fn) {
  if (!is.data.frame(panel))
    prevcarga_abort("contract.combina_input", where = sprintf("%s:panel", fn))
  miss <- setdiff(cols, names(panel))
  if (length(miss))
    prevcarga_abort("contract.combina_input", where = sprintf("%s:%s", fn, miss[[1L]]))
  invisible(panel)
}

.prevcarga_cmb_fcols <- function(panel, fn) {
  if (!is.data.frame(panel))
    prevcarga_abort("contract.combina_input", where = sprintf("%s:panel", fn))
  fcols <- grep("^f_", names(panel), value = TRUE)
  if (!length(fcols))
    prevcarga_abort("contract.combina_input", where = sprintf("%s:f_*", fn))
  fcols
}

.prevcarga_cmb_check_xy <- function(X, y) {
  if (!is.matrix(X) || !is.numeric(X) || nrow(X) < 1L || ncol(X) < 1L)
    prevcarga_abort("contract.combina_input", where = "fit_convex:X")
  if (anyNA(X))
    prevcarga_abort("contract.combina_input", where = "fit_convex:X com NA")
  if (!is.numeric(y) || anyNA(y))
    prevcarga_abort("contract.combina_input", where = "fit_convex:y")
  if (length(y) != nrow(X))
    prevcarga_abort("contract.combina_input", where = "fit_convex:length(y) != nrow(X)")
  invisible(TRUE)
}

## --- projecao no simplex (Duchi 2008) ------------------------------------------------------------
prevcarga_proj_simplex <- function(v) {
  if (!is.numeric(v) || length(v) < 1L || anyNA(v))
    prevcarga_abort("contract.combina_input", where = "proj_simplex:v")
  if (length(v) == 1L) return(stats::setNames(1, names(v)))
  u   <- sort(v, decreasing = TRUE)
  css <- cumsum(u) - 1
  ind <- seq_along(u)
  rho <- utils::tail(which(u - css / ind > 0), 1L)   # ULTIMO TRUE, nunca which.max
  theta <- css[rho] / rho
  pmax(v - theta, 0)                                  # preserva names(v)
}

## --- estacionariedade e FISTA --------------------------------------------------------------------
.prevcarga_cmb_center <- function(X, y) {
  xm <- colMeans(X)
  ym <- mean(y)
  Xc <- sweep(X, 2L, xm, "-")
  yc <- y - ym
  G  <- crossprod(Xc)
  b  <- drop(crossprod(Xc, yc))
  L  <- if (ncol(X) == 1L) 2 * G[1L, 1L] + 1e-9
        else 2 * eigen(G, symmetric = TRUE, only.values = TRUE)$values[1L] + 1e-9
  list(xm = xm, ym = ym, Xc = Xc, yc = yc, G = G, b = b, L = L)
}

## norma do gradient mapping no simplex: L * || w - P(w - g/L) ||
.prevcarga_cmb_grad_map <- function(g, w, L) {
  w <- unname(w)
  sqrt(sum((L * (w - unname(prevcarga_proj_simplex(w - g / L))))^2))
}

## medida unica de estacionariedade, direto do objetivo BRUTO ||y - (a + Xw)||^2; o solver para por ela
## e o gate do M4.V a reusa. L vem do sistema centrado (mesmo passo do FISTA).
.prevcarga_cmb_gn <- function(X, y, w, a, L) {
  r <- y - a - drop(X %*% w)
  sqrt(.prevcarga_cmb_grad_map(-2 * drop(crossprod(X, r)), w, L)^2 + (2 * sum(r))^2)
}

prevcarga_proj_grad_norm <- function(X, y, w, a = NULL) {
  .prevcarga_cmb_check_xy(X, y)
  if (length(w) != ncol(X))
    prevcarga_abort("contract.combina_input", where = "proj_grad_norm:length(w) != ncol(X)")
  if (is.null(a)) a <- mean(y) - sum(colMeans(X) * w)
  .prevcarga_cmb_gn(X, y, unname(w), a, .prevcarga_cmb_center(X, y)$L)
}

## Em carga real (~1000 MW, n~1152) o criterio ABSOLUTO bate o cap; normalizar por ||b|| da falsa
## parada [TBD: ADR-0026]. Os fixtures dos testes ficam em escala O(1).
prevcarga_fit_convex <- function(X, y, w0 = NULL, iters = 500L, grad_tol = 1e-6) {
  .prevcarga_cmb_check_xy(X, y)
  k <- ncol(X)
  if (k == 1L)
    return(list(w = stats::setNames(1, colnames(X)), a = mean(y) - mean(X),
                iters = 0L, grad_norm = 0, nonstationary = FALSE))
  cz <- .prevcarga_cmb_center(X, y)
  w <- if (length(w0) == k) unname(w0) else rep(1 / k, k)
  z <- w; t <- 1; gn <- Inf; it <- 0L
  while (it < iters) {
    it <- it + 1L
    g <- 2 * (drop(cz$G %*% z) - cz$b)
    w_new <- unname(prevcarga_proj_simplex(z - g / cz$L))
    t_new <- (1 + sqrt(1 + 4 * t^2)) / 2
    z <- w_new + ((t - 1) / t_new) * (w_new - w)
    w <- w_new; t <- t_new
    gn <- .prevcarga_cmb_gn(X, y, w, cz$ym - sum(cz$xm * w), cz$L)
    if (gn < grad_tol) break
  }
  if (gn >= grad_tol)
    warning(sprintf("[prevcarga/combina] fit_convex: cap de %d iteracoes sem grad < %g (grad=%.3g), celula nao-estacionaria",
                    as.integer(iters), grad_tol, gn), call. = FALSE)
  a <- cz$ym - sum(cz$xm * w)
  list(w = stats::setNames(w, colnames(X)), a = a, iters = it, grad_norm = gn,
       nonstationary = gn >= grad_tol)
}

## --- leak-fix: unica definicao do filtro de treino de peso ---------------------------------------
prevcarga_combina_month_start <- function(ym, tz) {
  if (!is.character(ym) || !all(grepl("^[0-9]{4}-[0-9]{2}$", ym)))
    prevcarga_abort("contract.combina_input", where = "month_start:ym")
  if (!is.character(tz) || length(tz) != 1L || is.na(tz) || !nzchar(tz))
    prevcarga_abort("contract.combina_input", where = "month_start:tz")
  out <- suppressWarnings(lubridate::ymd(paste0(ym, "-01"), tz = tz))
  if (anyNA(out))
    prevcarga_abort("contract.combina_input", where = "month_start:ym")
  out
}

## O horizonte por membro so decide quais colunas f_* sao nao-NA; o corte de treino dos MEMBROS (M2)
## nao se aplica ao peso. target_datetime e interval_end (ADR-0035): o intervalo cujo fim e inicio(m)
## entra, o do dia da 1a origem de m nao. Releitura do corte do ADR-0026 sob rotulo de fim [TBD: ADR-0026].
prevcarga_combina_train_mask <- function(panel, m) {
  .prevcarga_cmb_need(panel, c("target_datetime", "y_true"), "train_mask")
  if (!inherits(panel$target_datetime, "POSIXct"))
    prevcarga_abort("contract.combina_input", where = "train_mask:target_datetime sem POSIXct")
  tz <- attr(panel$target_datetime, "tzone")
  if (is.null(tz) || !nzchar(tz[1L]))
    prevcarga_abort("contract.combina_input", where = "train_mask:target_datetime sem tzone")
  istart <- panel$target_datetime - lubridate::minutes(30)   # interval_start (ADR-0035)
  istart < prevcarga_combina_month_start(m, tz[1L]) & !is.na(panel$y_true)
}

## --- fit expansivel por celula (lead, cut, ym) ---------------------------------------------------
.prevcarga_cmb_empty_weights <- function() {
  data.table::data.table(lead = integer(), cut = character(), ym = character(), a = numeric(),
                         w = list(), fallback = character(), n = integer(), iters = integer(),
                         grad_norm = numeric(), nonstationary = logical())
}

prevcarga_combina_fit <- function(panel, n_min = 24L, iters = 500L, grad_tol = 1e-6) {
  .prevcarga_cmb_need(panel, c("lead", "cut", "ym", "target_datetime", "y_true"), "combina_fit")
  fcols <- .prevcarga_cmb_fcols(panel, "combina_fit")
  dt <- data.table::as.data.table(panel)
  keys <- unique(dt[, c("lead", "cut"), with = FALSE])
  data.table::setorderv(keys, c("lead", "cut"))
  out <- list()
  for (gi in seq_len(nrow(keys))) {
    l  <- keys$lead[gi]
    ct <- keys$cut[gi]
    grp <- dt[dt$lead == l & dt$cut == ct]
    months <- sort(unique(grp$ym), method = "radix")
    Fm <- as.matrix(grp[, fcols, with = FALSE])
    w_prev <- NULL
    for (m in months) {
      in_cell <- grp$ym == m
      present <- fcols[colSums(!is.na(Fm[in_cell, , drop = FALSE])) > 0L]
      fb <- if (m == months[1L]) "first_month" else if (!length(present)) "no_member" else NA_character_
      n <- 0L
      if (is.na(fb)) {
        ok <- prevcarga_combina_train_mask(grp, m) &
          stats::complete.cases(Fm[, present, drop = FALSE])
        n <- sum(ok)
        if (n < n_min) fb <- "n_min"
      }
      if (is.na(fb)) {
        w0  <- if (identical(names(w_prev), present)) w_prev else NULL
        fit <- prevcarga_fit_convex(Fm[ok, present, drop = FALSE], grp$y_true[ok],
                                    w0 = w0, iters = iters, grad_tol = grad_tol)
        w_prev <- fit$w
      } else {
        k   <- max(1L, length(present))
        fit <- list(w = stats::setNames(rep(1 / k, length(present)), present),
                    a = 0, iters = 0L, grad_norm = NA_real_, nonstationary = FALSE)
      }
      out[[length(out) + 1L]] <- data.table::data.table(
        lead = l, cut = ct, ym = m, a = fit$a, w = list(fit$w), fallback = fb,
        n = as.integer(n), iters = as.integer(fit$iters), grad_norm = fit$grad_norm,
        nonstationary = fit$nonstationary)
    }
  }
  if (!length(out)) return(.prevcarga_cmb_empty_weights())
  res <- data.table::rbindlist(out)
  data.table::setorderv(res, c("lead", "cut", "ym"))
  res
}

## --- produtores ----------------------------------------------------------------------------------
.prevcarga_cmb_produce_cell <- function(M, w, a, fallback) {
  base <- if (is.na(fallback)) a else 0
  vapply(seq_len(nrow(M)), function(j) {
    o <- !is.na(M[j, ])
    if (!any(o)) return(NA_real_)
    s <- sum(w[o])
    if (s == 0) return(mean(M[j, o]))
    if (all(o)) return(base + sum(M[j, ] * w))
    base + sum(M[j, o] * w[o]) / s          # a FIXO, so w renormaliza
  }, numeric(1))
}

prevcarga_produce_combined <- function(panel, weights) {
  .prevcarga_cmb_need(panel, c("lead", "cut", "ym"), "produce_combined")
  .prevcarga_cmb_fcols(panel, "produce_combined")
  .prevcarga_cmb_need(weights, c("lead", "cut", "ym", "a", "w", "fallback"), "produce_combined")
  pk <- paste(panel$lead, panel$cut, panel$ym, sep = "\r")
  wk <- paste(weights$lead, weights$cut, weights$ym, sep = "\r")
  idx <- match(pk, wk)
  if (anyNA(idx)) {
    i <- which(is.na(idx))[1L]
    prevcarga_abort("contract.combina_input",
      where = sprintf("produce_combined:lead=%s cut=%s ym=%s", panel$lead[i], panel$cut[i], panel$ym[i]))
  }
  out <- rep(NA_real_, nrow(panel))
  for (ci in unique(idx)) {
    rows <- which(idx == ci)
    w <- weights$w[[ci]]
    miss <- setdiff(names(w), names(panel))
    if (length(miss))
      prevcarga_abort("contract.combina_input", where = sprintf("produce_combined:%s", miss[[1L]]))
    M <- as.matrix(as.data.frame(panel)[rows, names(w), drop = FALSE])
    out[rows] <- .prevcarga_cmb_produce_cell(M, unname(w), weights$a[ci], weights$fallback[ci])
  }
  out
}

prevcarga_produce_eqw <- function(panel) {
  fcols <- .prevcarga_cmb_fcols(panel, "produce_eqw")
  r <- rowMeans(as.matrix(as.data.frame(panel)[, fcols, drop = FALSE]), na.rm = TRUE)
  r[is.nan(r)] <- NA_real_
  r
}
