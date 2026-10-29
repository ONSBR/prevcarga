## combination.R -- nucleo numerico da combina (M4.1): simplex, FISTA, produtores e mascara de treino
##
## Responsabilidade: modulo numerico in-process (base + data.table + lubridate, sem dependencia nova).
## Pipeline da combina: discover -> assert_members_aligned -> build_panel -> join_fresh_truth ->
## fit por (lead, cut, ym) -> produce. Cobre fit/produce (M4.1) e a camada de IO (M4.2: discover,
## build_panel, join_fresh_truth, combina_load, panel_hash) e o alinhamento (M4.2b: assert_members_aligned).
## Requer (alem de errors.R): topology.R, provenance.R, schema_long.R, deck_reader.R, evaluation.R; features.R (so para a faixa de saida do M4.2c);
## registry.R (so prevcarga_plugin_scan_pure, na varredura de pureza do freeze do M4.3).
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
## nao se aplica ao peso. Corte pelo `interval_start` do painel (anexado pela borda do LONG, ADR-0035): o
## intervalo cujo fim e inicio(m) entra, o do dia da 1a origem de m nao. Releitura do ADR-0026 [TBD: ADR-0026].
prevcarga_combina_train_mask <- function(panel, m) {
  .prevcarga_cmb_need(panel, c("target_datetime", "interval_start", "y_true"), "train_mask")
  if (!inherits(panel$target_datetime, "POSIXct"))
    prevcarga_abort("contract.combina_input", where = "train_mask:target_datetime sem POSIXct")
  tz <- attr(panel$target_datetime, "tzone")
  if (is.null(tz) || !nzchar(tz[1L]))
    prevcarga_abort("contract.combina_input", where = "train_mask:target_datetime sem tzone")
  panel$interval_start < prevcarga_combina_month_start(m, tz[1L]) & !is.na(panel$y_true)
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
    months <- sort(unique(grp$ym))
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

## =================================================================================================
## M4.2 -- IO, descoberta de membros, painel por corte e join fresco por interval_end
## =================================================================================================

## --- localizador de run por identidade (mdir, EXOGENA) + _cHHMM opcional --------------------------
## Layout PROVISORIO <root>/<mdir>/<area>/<from>_<to>_<run_mode>[_<EXO>][_cHHMM]/{long.parquet, run_manifest.json}
## [TBD: ADR novo de layout de run, M6]. Nunca le `split`/`protocol` do run.yaml.
prevcarga_combina_run_modes <- c("infer", "fixed", "daily", "retrain_harness")

.prevcarga_cmb_run_regex <- function(exogena) {
  exo <- if (is.na(exogena)) "" else paste0("_", exogena)
  sprintf("_(%s)%s(_c[0-9]{4})?$", paste(prevcarga_combina_run_modes, collapse = "|"), exo)
}

.prevcarga_cmb_empty_runs <- function()
  data.table::data.table(run_id = character(), run_mode = character(), cut_suffix = character(),
                         path = character())

.prevcarga_cmb_member_runs <- function(root, area, mdir, exogena) {
  base <- file.path(root, mdir, area)
  if (!dir.exists(base)) return(.prevcarga_cmb_empty_runs())
  ids <- sort(basename(list.dirs(base, recursive = FALSE)))
  rx <- .prevcarga_cmb_run_regex(exogena)
  ids <- ids[grepl(rx, ids)]
  if (!length(ids)) return(.prevcarga_cmb_empty_runs())
  paths <- file.path(base, ids)
  miss <- !file.exists(file.path(paths, "long.parquet"))
  if (any(miss))
    prevcarga_abort("contract.combina_input",
                    where = sprintf("discover:long.parquet ausente em %s", paths[miss][[1L]]))
  data.table::data.table(run_id = ids, run_mode = sub(paste0(".*", rx), "\\1", ids),
                         cut_suffix = sub(paste0(".*", rx), "\\2", ids), path = paths)
}

## --- proveniencia por run (run-manifest M1.3 ao lado do LONG) --------------------------------------
## Arquivo/campo ausente vira NA (quem recusa e o alinhamento M4.2b). available_until e gravado em UTC
## ISO-8601 pelo M1.3; ymd_hms(tz = load_tz) o devolve no fuso da carga do proprio manifest (HHMM do corte).
.prevcarga_cmb_read_provenance <- function(run_path, member, run_id, run_mode, cut_suffix) {
  mf <- file.path(run_path, "run_manifest.json")
  m <- if (file.exists(mf)) jsonlite::read_json(mf, simplifyVector = TRUE) else list()
  chr <- function(k) {
    v <- m[[k]]
    if (is.character(v) && length(v) == 1L && !is.na(v)) v else NA_character_
  }
  load_tz <- chr("load_tz")
  au_raw <- m[["available_until"]]
  au <- lubridate::as_datetime(NA_real_)
  cut <- NA_character_
  if (file.exists(mf) && "available_until" %in% names(m) && is.null(au_raw)) {
    cut <- ""                                    # available_until nulo = corte unico
  } else if (is.character(au_raw) && length(au_raw) == 1L && !is.na(au_raw) && !is.na(load_tz) &&
             load_tz %in% OlsonNames()) {
    au <- suppressMessages(lubridate::ymd_hms(au_raw, tz = load_tz, quiet = TRUE))   # "Z" -> fuso da carga
    if (!is.na(au)) cut <- sprintf("%02d%02d", lubridate::hour(au), lubridate::minute(au))
  }
  dd <- m[["delivery_days"]]
  dd <- if (is.numeric(dd) && length(dd) == 1L && is.finite(dd)) as.integer(dd) else NA_integer_
  data.table::data.table(member = member, run_id = run_id, run_mode = run_mode, cut_suffix = cut_suffix,
                         origin = chr("origin"), available_until = au, cut = cut,
                         load_label = chr("load_label"), load_tz = load_tz,
                         weather_issue_cutoff = chr("weather_issue_cutoff"), delivery_days = dd)
}

## --- descoberta -------------------------------------------------------------------------------------
prevcarga_combina_discover <- function(root, area, ctx, members = prevcarga_members(root = ctx$root)) {
  topo <- prevcarga_topology(root = ctx$root)
  if (!is.character(area) || length(area) != 1L || is.na(area) || !area %in% c(topo$leaves, topo$order))
    prevcarga_abort("topology.area_undeclared", where = as.character(area)[1L])
  if (!is.character(root) || length(root) != 1L || is.na(root) || !dir.exists(root))
    prevcarga_abort("contract.combina_input",
                    where = sprintf("discover:root inexistente %s", as.character(root)[1L]))
  if (is.null(ctx$tz)) ctx$tz <- .prevcarga_eval_tz(ctx)
  parts <- list(); prov <- list(); present <- character()
  for (i in seq_len(nrow(members))) {
    sg <- members$sigla[[i]]
    runs <- .prevcarga_cmb_member_runs(root, area, members$mdir[[i]], members$exogena[[i]])
    if (!nrow(runs)) next
    modes <- sort(unique(runs$run_mode))
    if (length(modes) > 1L)
      prevcarga_abort("contract.combina_run_mode_dup",
                      where = sprintf("%s: %s/%s EXOGENA=%s {%s}", sg, members$mdir[[i]], area,
                                      if (is.na(members$exogena[[i]])) "-" else members$exogena[[i]],
                                      paste(modes, collapse = ",")))
    for (j in seq_len(nrow(runs))) {
      lp <- file.path(runs$path[[j]], "long.parquet")
      d <- tryCatch(prevcarga_read_long(lp, ctx), prevcarga_error = function(e)   # mesmo id/classe, com o run
        prevcarga_abort(e$id, where = sprintf("%s [%s]", lp, paste(e$where, collapse = ",")),
                        details = e$details))
      if (!all(d$area == area))
        prevcarga_abort("contract.combina_input",
                        where = sprintf("discover:area divergente em %s", runs$path[[j]]))
      d <- data.table::as.data.table(d)
      d[, `:=`(member = sg, run_id = runs$run_id[[j]])]
      parts[[length(parts) + 1L]] <- d
      prov[[length(prov) + 1L]] <- .prevcarga_cmb_read_provenance(
        runs$path[[j]], sg, runs$run_id[[j]], runs$run_mode[[j]], runs$cut_suffix[[j]])
    }
    present <- c(present, sg)
  }
  if (!length(parts))
    prevcarga_abort("contract.combina_input",
                    where = sprintf("discover:nenhum membro com run em %s/*/%s", root, area))
  out <- data.table::rbindlist(parts)
  data.table::setattr(out, "members_declared", members$sigla)
  data.table::setattr(out, "members_present", present)
  data.table::setattr(out, "provenance", data.table::rbindlist(prov))
  out
}

## --- cobertura por lead (M4.2c, ADR-0032 c) ------------------------------------------------------------
## Cada membro entrega de available_until + 30 ate O + delivery_days (00:00); a faixa vem SO do
## `delivery_days` do run-manifest (proveniencia M4.2), nunca do nome do modelo. Devolve 1 linha por
## (member, forecast_origin, cut, lead) com `covered` e `last_lead` (ultimo lead coberto por (member, cut)).
prevcarga_member_coverage <- function(provenance, panel) {
  .prevcarga_cmb_need(provenance, c("member", "cut", "delivery_days"), "member_coverage")
  .prevcarga_cmb_need(panel, c("forecast_origin", "cut", "lead", "target_datetime"), "member_coverage")
  dd <- unique(data.table::as.data.table(provenance)[, c("member", "cut", "delivery_days"), with = FALSE])
  if (anyNA(dd$delivery_days)) {
    r <- dd[is.na(dd$delivery_days)][1L]
    .prevcarga_cmb_misaligned(r$member, "delivery_days", "inteiro do run-manifest", "NA")
  }
  bad <- dd[, list(N = .N), by = c("member", "cut")][N > 1L]
  if (nrow(bad))
    prevcarga_abort("contract.member_misaligned",
      where = sprintf("%s:delivery_days inconsistente no corte %s", bad$member[1L], bad$cut[1L]))
  tz <- attr(panel$target_datetime, "tzone")
  if (is.null(tz) || !nzchar(tz[1L]))
    prevcarga_abort("contract.combina_input", where = "member_coverage:target_datetime sem tzone")
  key <- unique(data.table::as.data.table(panel)[, c("forecast_origin", "cut", "lead", "target_datetime"),
                                                 with = FALSE])
  cov <- merge(key, dd, by = "cut", allow.cartesian = TRUE)
  cov[, covered := target_datetime <=
        lubridate::ymd(forecast_origin, tz = tz[1L]) + lubridate::days(delivery_days)]
  cov[, last_lead := if (any(covered)) max(lead[covered]) else NA_integer_, by = c("member", "cut")]
  data.table::setcolorder(cov, c("forecast_origin", "cut", "lead", "target_datetime", "member",
                                 "delivery_days", "covered", "last_lead"))
  data.table::setorderv(cov, c("member", "cut", "forecast_origin", "lead"))
  cov[]
}

## --- painel por corte --------------------------------------------------------------------------------
prevcarga_build_panel <- function(long, members = NULL) {
  .prevcarga_cmb_need(long, c("member", "run_id", "area", "forecast_origin", "target_datetime", "interval_start",
                              "lead", "y_pred", "d_plus", "granularity_min"), "build_panel")
  prov <- attr(long, "provenance")
  if (is.null(prov) || !all(c("member", "run_id", "cut") %in% names(prov)))
    prevcarga_abort("contract.combina_input", where = "build_panel:provenance")
  decl <- attr(long, "members_declared")
  pres <- attr(long, "members_present")
  if (is.null(members)) {            # so a sigla ordena as colunas; a ordem vem da topologia via discover
    if (is.null(decl)) prevcarga_abort("contract.combina_input", where = "build_panel:members")
    members <- data.frame(sigla = decl, stringsAsFactors = FALSE)
  }
  long <- data.table::as.data.table(long)
  long <- merge(long, data.table::as.data.table(prov)[, c("member", "run_id", "cut"), with = FALSE],
                by = c("member", "run_id"), sort = FALSE)
  if (anyNA(long$cut))
    prevcarga_abort("contract.combina_input",
                    where = sprintf("build_panel:corte ausente na proveniencia de %s", long$member[[1L]]))
  key <- c("member", "area", "forecast_origin", "lead", "cut")
  if (anyDuplicated(long, by = key)) {
    r <- long[duplicated(long, by = key)][1L]
    prevcarga_abort("contract.combina_input",
                    where = sprintf("build_panel:linha duplicada de membro %s em (%s, %s, %s)",
                                    r$member, r$forecast_origin, r$lead, r$cut))
  }
  kk <- c("area", "forecast_origin", "lead", "cut")
  tgt <- unique(long[, c(kk, "target_datetime", "interval_start", "d_plus", "granularity_min"), with = FALSE])
  if (anyDuplicated(tgt, by = kk)) {
    r <- tgt[duplicated(tgt, by = kk)][1L]
    prevcarga_abort("contract.combina_input",
                    where = sprintf("build_panel:target_datetime divergente entre membros em (%s, %s, %s)",
                                    r$forecast_origin, r$lead, r$cut))
  }
  long[, member := factor(member, levels = members$sigla)]
  if (anyNA(long$member))
    prevcarga_abort("contract.combina_input", where = "build_panel:membro fora do conjunto declarado")
  wide <- data.table::dcast(long, area + forecast_origin + lead + cut ~ member, value.var = "y_pred",
                            fill = NA_real_, drop = c(TRUE, FALSE))
  data.table::setnames(wide, members$sigla, paste0("f_", members$sigla))
  panel <- merge(tgt, wide, by = kk, sort = FALSE)
  panel[, ym := substr(forecast_origin, 1L, 7L)]       # mes-de-ORIGEM
  panel[, y_true := NA_real_]                           # o y_true do LONG do membro e descartado
  data.table::setcolorder(panel, c("area", "forecast_origin", "target_datetime", "interval_start", "lead", "d_plus",
                                   "granularity_min", "cut", "ym", "y_true", paste0("f_", members$sigla)))
  data.table::setorderv(panel, c("area", "cut", "lead", "forecast_origin"))
  if ("delivery_days" %in% names(prov)) {   # mascara de faixa: fora de delivery_days = AUSENTE (NA), nunca 0
    cov <- prevcarga_member_coverage(prov, panel)
    for (s in unique(as.character(cov$member))) {
      col <- paste0("f_", s)
      off <- cov[cov$member == s & !cov$covered, c("forecast_origin", "cut", "lead"), with = FALSE]
      if (nrow(off) && col %in% names(panel)) {
        idx <- panel[off, on = c("forecast_origin", "cut", "lead"), which = TRUE, nomatch = NULL]
        data.table::set(panel, i = idx, j = col, value = NA_real_)
      }
    }
    data.table::setattr(panel, "coverage",
                        unique(cov[, c("member", "cut", "delivery_days", "last_lead"), with = FALSE]))
  }
  data.table::setattr(panel, "members_declared", decl)
  data.table::setattr(panel, "members_present", pres)
  data.table::setattr(panel, "provenance", prov)
  panel
}

## --- verdade fresca por interval_end ------------------------------------------------------------------
prevcarga_join_fresh_truth <- function(panel, ctx, area) {
  .prevcarga_cmb_need(panel, c("target_datetime"), "join_fresh_truth")
  at <- attributes(panel)[c("members_declared", "members_present", "provenance", "coverage")]
  panel <- data.table::copy(panel)
  panel[, y_true := NA_real_]
  if (nrow(panel)) {
    ## read_actual e (from, to]: from recua 1 s para incluir o 1o alvo (sem aritmetica de hora de intervalo)
    act <- prevcarga_read_actual(ctx, area, from = min(panel$target_datetime) - lubridate::seconds(1),
                                 to = max(panel$target_datetime))
    idx <- match(as.numeric(panel$target_datetime), as.numeric(act$interval_end))   # exato, sem round
    panel[, y_true := act$y_true[idx]]
  }
  for (n in names(at)) if (!is.null(at[[n]])) data.table::setattr(panel, n, at[[n]])
  panel[]
}

## =================================================================================================
## M4.2b -- recusa de membro desalinhado: INICIO e GRADE comuns; o FIM e por membro (delivery_days)
## =================================================================================================
## A combina nunca agrega nem desloca tempo: membros horarios chegam ja em 48 intervalos (to_semihourly, a
## montante). A faixa de entrega vem so do `delivery_days` do run-manifest do membro (ADR-0032 c); faixas
## diferentes ENTRE membros nao sao desalinhamento. Unico ponto de abort: .prevcarga_cmb_misaligned.
.prevcarga_cmb_misaligned <- function(member, field, expected, got) {
  prevcarga_abort("contract.member_misaligned",
    where = sprintf("%s:%s esperado=%s obtido=%s", member, field,
                    paste(expected, collapse = ","), paste(got, collapse = ",")))
}

## convencoes esperadas do ctx; o rotulo vem de ctx$load_label ou, sem ele, do deck (manifest > flag > config)
.prevcarga_cmb_ctx_conv <- function(ctx) {
  ll <- ctx$load_label
  if (is.null(ll)) ll <- .prevcarga_deck_ctx(ctx)$label$value
  list(load_label = ll, tz = .prevcarga_eval_tz(ctx),
       weather_issue_cutoff = if (is.null(ctx$weather_issue_cutoff)) "available_until" else ctx$weather_issue_cutoff)
}

.prevcarga_cmb_check_convention <- function(p, conv) {
  need <- c("origin", "available_until", "load_label", "load_tz", "weather_issue_cutoff", "delivery_days")
  miss <- need[!need %in% names(p) | vapply(need, function(k) !k %in% names(p) || anyNA(p[[k]]), logical(1))]
  if (length(miss))
    .prevcarga_cmb_misaligned(p$member, "proveniencia", "run_manifest.json completo",
                              paste0("ausente:", paste(miss, collapse = "+")))
  if (p$load_label != conv$load_label)
    .prevcarga_cmb_misaligned(p$member, "load_label vs ctx", conv$load_label, p$load_label)
  if (p$load_tz != conv$tz)
    .prevcarga_cmb_misaligned(p$member, "load_tz vs ctx", conv$tz, p$load_tz)
  if (p$weather_issue_cutoff != conv$weather_issue_cutoff)
    .prevcarga_cmb_misaligned(p$member, "weather_issue_cutoff vs ctx", conv$weather_issue_cutoff,
                              p$weather_issue_cutoff)
  invisible(TRUE)
}

## fim da faixa declarada (delivery_days) e grade completa: n passos de 30 min de available_until (1o
## interval_start) ate o fim da faixa (ultimo interval_end)
.prevcarga_cmb_check_band <- function(t_o, first_start, last_end, p) {
  if (max(t_o) != last_end)
    .prevcarga_cmb_misaligned(p$member, "delivery_days", format(last_end), format(max(t_o)))
  n_exp <- length(seq(first_start, last_end, by = "30 min")) - 1L
  if (length(unique(t_o)) != n_exp)
    .prevcarga_cmb_misaligned(p$member, "grade", paste0(n_exp, " alvos de 30 min"), length(unique(t_o)))
  invisible(TRUE)
}

## Compara o LONG pelo `interval_start` anexado no leitor: o 1o esperado e o proprio available_until.
## Deslocamento = faixa inteira 1 passo fora (rotulo por inicio ou dobrado), lido so pelas bordas do LONG.
.prevcarga_cmb_check_times <- function(d, p, tz) {
  .prevcarga_cmb_need(d, c("target_datetime", "interval_start"), "members_aligned")
  td <- d$target_datetime
  if (!identical(lubridate::tz(td), tz))
    .prevcarga_cmb_misaligned(p$member, "tzone", tz, lubridate::tz(td))
  if (any(!lubridate::minute(td) %in% c(0L, 30L) | lubridate::second(td) != 0))
    .prevcarga_cmb_misaligned(p$member, "grade", "minuto em {0,30}", "fora da grade")
  if (nzchar(p$cut_suffix) && !identical(p$cut_suffix, paste0("_c", p$cut)))
    .prevcarga_cmb_misaligned(p$member, "available_until", paste0("_c", p$cut), p$cut_suffix)
  hh <- as.integer(substr(p$cut, 1L, 2L)); mm <- as.integer(substr(p$cut, 3L, 4L))
  for (o in sort(unique(d$forecast_origin), method = "radix")) {
    sel <- d$forecast_origin == o
    t_o <- td[sel]; s_o <- d$interval_start[sel]
    au_o <- lubridate::ymd(o, tz = tz) + lubridate::hours(hh) + lubridate::minutes(mm)
    if (identical(o, p$origin) && au_o != p$available_until)
      .prevcarga_cmb_misaligned(p$member, "available_until", format(au_o), format(p$available_until))
    first_start <- au_o
    last_end    <- lubridate::ymd(o, tz = tz) + lubridate::days(p$delivery_days)
    if (min(s_o) != first_start) {
      first_end  <- prevcarga_long_target_datetime(first_start, 30L)
      last_start <- prevcarga_long_interval_start(last_end, 30L)
      shifted <- (min(t_o) == first_start || min(s_o) == first_end) &&
                 (max(t_o) == last_start || max(s_o) == last_end)
      .prevcarga_cmb_misaligned(p$member, if (shifted) "deslocamento" else "primeira_entrega",
                                format(first_end), format(min(t_o)))
    }
    .prevcarga_cmb_check_band(t_o, first_start, last_end, p)
  }
  invisible(TRUE)
}

## Ordem fixa (veredito deterministico): membros na ordem declarada, runs por run_id; por run, convencao
## (proveniencia, load_label, load_tz, weather_issue_cutoff) e depois tempo (tzone, grade, available_until,
## primeira entrega, fim da faixa). A primeira falha e a reportada.
prevcarga_assert_members_aligned <- function(long, ctx) {
  prov <- attr(long, "provenance")
  if (is.null(prov) || !is.data.frame(prov) || !nrow(prov) ||
      !all(c("member", "run_id", "cut_suffix", "cut") %in% names(prov)))
    .prevcarga_cmb_misaligned("-", "proveniencia", "run_manifest.json completo", "ausente")
  conv <- .prevcarga_cmb_ctx_conv(ctx)
  prov <- data.table::as.data.table(prov)
  decl <- attr(long, "members_declared")
  ord <- if (is.null(decl)) unique(prov$member) else c(intersect(decl, prov$member), setdiff(prov$member, decl))
  prov <- prov[order(match(prov$member, ord), prov$run_id, method = "radix")]
  for (i in seq_len(nrow(prov))) {
    p <- prov[i]
    .prevcarga_cmb_check_convention(p, conv)
    d <- long[long$member == p$member & long$run_id == p$run_id, ]
    if (!nrow(d)) .prevcarga_cmb_misaligned(p$member, "grade", "alvos do run", "nenhum")
    .prevcarga_cmb_check_times(d, p, conv$tz)
  }
  invisible(TRUE)
}

## unico pipeline de montagem (M4.4): discover -> assert_members_aligned -> build_panel -> join_fresh_truth
prevcarga_combina_load <- function(root, area, ctx, members = prevcarga_members(root = ctx$root)) {
  long <- prevcarga_combina_discover(root, area, ctx, members)
  prevcarga_assert_members_aligned(long, ctx)
  panel <- prevcarga_build_panel(long, members)
  prevcarga_join_fresh_truth(panel, ctx, area)
}

## sha256 (64 hex) do conteudo do painel pelo canonicalizador do M1.3 (sem digest/toJSON proprio). Linhas em ordem
## canonica (area, cut, lead, forecast_origin); `f_*` all-NA entram como null (o hash enxerga a presenca de membro).
prevcarga_combina_panel_hash <- function(panel) {
  .prevcarga_cmb_need(panel, c("area", "forecast_origin", "target_datetime", "lead", "d_plus",
                               "granularity_min", "cut", "ym", "y_true"), "panel_hash")
  fcols <- .prevcarga_cmb_fcols(panel, "panel_hash")
  cols <- c("area", "forecast_origin", "target_datetime", "lead", "d_plus", "granularity_min", "cut",
            "ym", fcols, "y_true")
  ord <- order(panel$area, panel$cut, panel$lead, panel$forecast_origin, method = "radix")
  body <- lapply(stats::setNames(cols, cols), function(k) panel[[k]][ord])
  prov <- attr(panel, "provenance")
  one <- function(k) {
    v <- if (is.null(prov) || !k %in% names(prov)) character() else sort(unique(stats::na.omit(prov[[k]])))
    if (length(v)) paste(v, collapse = "+") else "unknown"
  }
  .prevcarga_sha256(.prevcarga_canon_json(list(
    model_id = "combina_panel", columns = cols, body = body,
    load_label = one("load_label"), load_tz = one("load_tz"))))
}

## =================================================================================================
## M4.2c -- combinacao por lead: so membros que cobrem o lead entram no fit e no produce
## =================================================================================================
## O fit (prevcarga_combina_fit) ja monta o X de cada celula (lead, cut, ym) so com as colunas f_* nao-NA
## (k variavel por lead, simplex sobre os presentes, fallback EQW 1/k_presentes) e o produce usa so names(w);
## a mascara de faixa do build_panel garante que o membro fora da faixa e AUSENTE, nunca zero.

## conjunto de siglas presentes por (lead, cut), uniao sobre os meses; consumido pelo freeze (M4.3) e pelo JSON (M4.4)
prevcarga_combina_members_by_lead <- function(weights) {
  if (!is.data.frame(weights) || !all(c("lead", "cut", "w") %in% names(weights)))
    prevcarga_abort("contract.combina_input", where = "members_by_lead:weights")
  weights <- data.table::as.data.table(weights)
  out <- weights[, list(members = list(sort(unique(sub("^f_", "", unlist(lapply(w, names)))),
                                            method = "radix"))), by = c("lead", "cut")]
  out[, n_members := lengths(members)]
  out <- out[n_members > 0L]                      # lead sem membro cobrindo nao e entregue
  data.table::setorderv(out, c("cut", "lead"))
  out[]
}

## Faixa de saida: de available_until + 30 ate o fim da MAIOR faixa delivery_days presente no corte. Remove as
## linhas sem nenhum membro cobrindo o lead; aborta se sobrar y_pred NA dentro da faixa ou se a grade de alvos
## da origem divergir da grade de entrega (prevcarga_delivery_grid, features.R; buraco nunca preenchido).
.prevcarga_cmb_trim_output <- function(panel, y_pred) {
  .prevcarga_cmb_need(panel, c("forecast_origin", "cut", "lead", "target_datetime"), "trim_output")
  fcols <- .prevcarga_cmb_fcols(panel, "trim_output")
  cov <- attr(panel, "coverage")
  if (is.null(cov) || !all(c("member", "cut", "delivery_days") %in% names(cov)))
    prevcarga_abort("contract.combina_input", where = "trim_output:coverage")
  if (!is.numeric(y_pred) || length(y_pred) != nrow(panel))
    prevcarga_abort("contract.combina_input", where = "trim_output:y_pred")
  dt <- data.table::copy(data.table::as.data.table(panel))
  dt[, y_pred := y_pred]
  any_member <- rowSums(!is.na(as.matrix(as.data.frame(panel)[, fcols, drop = FALSE]))) > 0L
  out <- dt[any_member]
  if (anyNA(out$y_pred))
    prevcarga_abort("contract.combina_input",
      where = sprintf("trim_output:y_pred NA em lead=%s cut=%s", out$lead[is.na(out$y_pred)][1L],
                      out$cut[is.na(out$y_pred)][1L]))
  tz <- attr(panel$target_datetime, "tzone")[1L]
  cov <- data.table::as.data.table(cov)
  for (ct in unique(out$cut)) {
    maxdd <- max(cov$delivery_days[cov$cut == ct])
    hh <- if (nzchar(ct)) as.integer(substr(ct, 1L, 2L)) else 0L
    mm <- if (nzchar(ct)) as.integer(substr(ct, 3L, 4L)) else 0L
    for (o in sort(unique(out$forecast_origin[out$cut == ct]), method = "radix")) {
      au_o <- lubridate::ymd(o, tz = tz) + lubridate::hours(hh) + lubridate::minutes(mm)
      grid <- prevcarga_delivery_grid(list(available_until = au_o, tz = tz, origin_day = lubridate::ymd(o)), maxdd)
      got <- out$target_datetime[out$cut == ct & out$forecast_origin == o]
      if (!identical(as.numeric(sort(unique(got))), as.numeric(grid)))
        prevcarga_abort("contract.combina_input",
          where = sprintf("trim_output:faixa de saida diverge da grade de entrega (origem %s, corte %s)", o, ct))
    }
  }
  out[]
}

## =================================================================================================
## M4.3 -- fit -> freeze -> apply content-addressed + saida LONG
## =================================================================================================
## Layout (ADR-0030/0039): <artifact_dir>/<area>/v_<16hex>/{weights.rds, weights.rds.manifest.json} + <area>/latest.txt
## (1 linha, sem symlink). Reusa o contrato do M1.3 (prevcarga_version_hash/run_manifest/freeze/thaw) e a varredura de
## pureza do registry (P.0d); nenhum 2o mecanismo. Staging unico `.tmp-*` irmao do destino + file.rename; orfaos
## `.tmp-*` sao do `registry gc` (M5). A saida LONG passa pela borda de escrita de schema_long.R (sem aritmetica de hora aqui).

## --- selecao de corte -----------------------------------------------------------------------------------
.prevcarga_cmb_select_cut <- function(panel, t_cut = NULL) {
  .prevcarga_cmb_need(panel, c("cut"), "select_cut")
  cuts <- sort(unique(as.character(panel$cut)), method = "radix")
  if (is.null(t_cut)) {
    if (length(cuts) != 1L)
      prevcarga_abort("contract.combina_input",
                      where = sprintf("corte ambiguo (%s): passe t_cut/--cut", paste(cuts, collapse = ", ")))
    ct <- cuts
  } else {
    if (!is.character(t_cut) || length(t_cut) != 1L || is.na(t_cut))
      prevcarga_abort("contract.combina_input", where = "select_cut:t_cut")
    ct <- sub("^c", "", t_cut)
    if (!ct %in% cuts)
      prevcarga_abort("contract.combina_input",
                      where = sprintf("corte %s inexistente; disponiveis: %s", t_cut, paste(cuts, collapse = ", ")))
  }
  keep <- as.character(panel$cut) == ct
  at <- attributes(panel)[c("members_declared", "members_present", "provenance", "coverage")]
  rows <- data.table::as.data.table(panel)[keep]
  for (n in names(at)) if (!is.null(at[[n]])) data.table::setattr(rows, n, at[[n]])
  rows
}

## --- LONG 8 col (ADR-0002): so linhas cobertas; target_datetime pela borda de escrita de schema_long.R ----------
.prevcarga_cmb_to_long <- function(rows, y_pred, ctx) {
  .prevcarga_cmb_need(rows, c("area", "forecast_origin", "interval_start", "lead", "d_plus", "granularity_min",
                              "y_true"), "to_long")
  tz <- if (!is.null(ctx$tz)) ctx$tz else attr(rows$interval_start, "tzone")[1L]
  kept <- .prevcarga_cmb_trim_output(rows, y_pred)
  out <- data.table::data.table(
    area = as.character(kept$area),
    forecast_origin = as.character(kept$forecast_origin),
    target_datetime = prevcarga_long_target_datetime(kept$interval_start, kept$granularity_min, tz),
    lead = as.integer(kept$lead),
    y_pred = as.numeric(kept$y_pred),
    y_true = as.numeric(kept$y_true),
    d_plus = as.integer(kept$d_plus),
    granularity_min = as.integer(kept$granularity_min))
  prevcarga_schema_long_check(out)
  prevcarga_assert_long(out, list(tz = tz))
  out <- out[order(out$forecast_origin, out$lead, method = "radix")]
  data.table::setattr(out, "sorted", NULL)
  out[]
}

## --- full: fit + produce num passo, sem artefato ------------------------------------------------------------
## Consome `panel$y_true` como recebido (nao junta verdade fresca, nao le disco): o honesty-gate perturba y_true.
prevcarga_combina_full <- function(panel, ctx, t_cut = NULL, n_min = 24L, iters = 500L, grad_tol = 1e-6,
                                   detail = FALSE) {
  rows    <- .prevcarga_cmb_select_cut(panel, t_cut)
  weights <- prevcarga_combina_fit(rows, n_min = n_min, iters = iters, grad_tol = grad_tol)
  y       <- prevcarga_produce_combined(rows, weights)
  long    <- .prevcarga_cmb_to_long(rows, y, ctx)
  if (!detail) return(long)
  list(long = long, weights = weights, members_by_lead = prevcarga_combina_members_by_lead(weights))
}

## --- identidade do artefato -----------------------------------------------------------------------------------
.prevcarga_cmb_prov1 <- function(panel, key, fn) {
  prov <- attr(panel, "provenance")
  v <- if (is.null(prov) || !key %in% names(prov)) character() else unique(prov[[key]])
  if (length(v) != 1L || is.na(v) || !nzchar(v))
    prevcarga_abort("contract.combina_input", where = sprintf("%s:provenance$%s", fn, key))
  as.character(v)
}

.prevcarga_cmb_plain_table <- function(x) {
  x <- as.data.frame(data.table::copy(data.table::as.data.table(x)))
  attr(x, ".internal.selfref") <- NULL
  x
}

.prevcarga_cmb_fingerprint <- function(area, panel, weights, cfg, members, root) {
  prov   <- attr(panel, "provenance")
  fitted <- weights$ym[is.na(weights$fallback)]
  tz     <- attr(panel$target_datetime, "tzone")[1L]
  last_m <- if (length(fitted)) max(fitted) else min(weights$ym)
  prevcarga_combina_month_start(last_m, tz)                      # valida ym/tz; o corte ISO e o 1o dia do mes
  dd <- unique(data.table::as.data.table(prov)[, c("member", "cut", "delivery_days"), with = FALSE])
  dd <- dd[order(dd$member, dd$cut, method = "radix")]
  mbl <- prevcarga_combina_members_by_lead(weights)
  list(
    model_id         = "combination",
    data_fingerprint = prevcarga_combina_panel_hash(panel),
    cutoff           = paste0(last_m, "-01"),
    feature_config   = list(area = area, n_min = cfg$n_min, iters = cfg$iters, grad_tol = cfg$grad_tol,
                            cuts = sort(unique(as.character(panel$cut)), method = "radix"),
                            member_set_hash = prevcarga_member_set_hash(members),
                            delivery_days = as.list(stats::setNames(as.integer(dd$delivery_days),
                                                                    paste0(dd$member, "@", dd$cut))),
                            members_by_lead = lapply(stats::setNames(names(mbl), names(mbl)),
                                                     function(k) mbl[[k]])),
    code_commit      = prevcarga_code_commit(root),
    renv_lock_hash   = prevcarga_renv_lock_hash(root),
    load_label       = .prevcarga_cmb_prov1(panel, "load_label", "fingerprint"),
    load_tz          = .prevcarga_cmb_prov1(panel, "load_tz", "fingerprint"))
}

## latest.txt: 1 linha, tmp `.tmp-*` no mesmo diretorio + file.rename (sem symlink)
.prevcarga_cmb_write_latest <- function(area_dir, vh) {
  tmp <- tempfile(".tmp-", tmpdir = area_dir)
  on.exit(unlink(tmp), add = TRUE)
  writeLines(vh, tmp)
  if (!file.rename(tmp, file.path(area_dir, "latest.txt")))
    prevcarga_abort("contract.combina_input", where = paste0("rename ", file.path(area_dir, "latest.txt")))
  invisible(file.path(area_dir, "latest.txt"))
}

## --- train: fit de todos os cortes e leads + freeze content-addressed -------------------------------------------
prevcarga_combina_train <- function(area, panel, artifact_dir, root, members = prevcarga_members(root = root),
                                    n_min = 24L, iters = 500L, grad_tol = 1e-6, trained_at = NULL) {
  if (!is.character(area) || length(area) != 1L || is.na(area) || !nzchar(area))
    prevcarga_abort("contract.combina_input", where = "train:area")
  if (!is.character(artifact_dir) || length(artifact_dir) != 1L || is.na(artifact_dir) || !nzchar(artifact_dir))
    prevcarga_abort("contract.combina_input", where = "train:artifact_dir")
  .prevcarga_cmb_need(panel, c("area", "forecast_origin", "cut"), "train")
  weights <- prevcarga_combina_fit(panel, n_min = n_min, iters = iters, grad_tol = grad_tol)
  cfg <- list(n_min = as.integer(n_min), iters = as.integer(iters), grad_tol = grad_tol)
  vh  <- prevcarga_version_hash(.prevcarga_cmb_fingerprint(area, panel, weights, cfg, members, root))
  area_dir <- file.path(artifact_dir, area)
  final <- file.path(area_dir, vh)
  if (dir.exists(final)) {                         # versao imutavel: nao reescreve; so reaponta o latest
    .prevcarga_cmb_write_latest(area_dir, vh)
    return(list(version_hash = vh, path = final, reused = TRUE, cells = weights))
  }
  prov <- attr(panel, "provenance")
  dd <- unique(data.table::as.data.table(prov)[, c("member", "delivery_days"), with = FALSE])
  dd <- dd[, list(d = max(delivery_days)), by = "member"]
  mbl <- prevcarga_combina_members_by_lead(weights)
  meta <- prevcarga_run_manifest(
    data_hash = prevcarga_combina_panel_hash(panel), origin = lubridate::ymd(max(panel$forecast_origin)),
    load_label = .prevcarga_cmb_prov1(panel, "load_label", "train"),
    load_tz = .prevcarga_cmb_prov1(panel, "load_tz", "train"),
    weather_issue_cutoff = .prevcarga_cmb_prov1(panel, "weather_issue_cutoff", "train"),
    split = "origin_scoped", delivery_days = as.list(stats::setNames(as.integer(dd$d), dd$member)),
    model_versions = list(combination = "v_0000000000000000"),   # placeholder so para passar a validacao do M1.3
    seeds = list(combination = 0L), root = root, trained_at = trained_at, members = members,
    area = area, version_hash = vh, members_by_lead = .prevcarga_cmb_plain_table(mbl))
  meta$model_versions <- structure(list(), names = character())    # provisorio (decisao do Gabriel): lista nomeada vazia
  obj <- list(meta = meta, weights = .prevcarga_cmb_plain_table(weights))
  pure <- list(meta = meta[setdiff(names(meta), "trained_at")], weights = obj$weights)
  prevcarga_plugin_scan_pure(pure)                                 # pureza (P.0d); trained_at e lateral declarada
  dir.create(area_dir, recursive = TRUE, showWarnings = FALSE)
  stg <- tempfile(".tmp-", tmpdir = area_dir)                      # staging unico do registry, irmao do destino
  dir.create(stg)
  prevcarga_freeze(obj, file.path(stg, "weights.rds"))
  if (!file.rename(stg, final)) {
    unlink(stg, recursive = TRUE)
    prevcarga_abort("contract.combina_input", where = paste0("rename ", stg))
  }
  .prevcarga_cmb_write_latest(area_dir, vh)                        # POR ULTIMO
  list(version_hash = vh, path = final, reused = FALSE, cells = weights)
}

## --- apply: aplica os pesos congelados, nunca re-fita --------------------------------------------------------
prevcarga_combina_apply <- function(area, artifact_dir, panel, ctx, t_cut = NULL,
                                    members = prevcarga_members(root = ctx$root)) {
  lp <- file.path(artifact_dir, area, "latest.txt")
  if (!file.exists(lp))
    prevcarga_abort("state.combina_not_trained", where = sprintf("%s em %s", area, artifact_dir))
  vh <- readLines(lp, n = 1L, warn = FALSE)
  if (!length(vh) || !grepl("^v_[0-9a-f]{16}$", vh))
    prevcarga_abort("contract.combina_input", where = paste0("latest.txt invalido: ", lp))
  obj <- prevcarga_thaw(file.path(artifact_dir, area, vh, "weights.rds"))      # integrity (9) antes do readRDS
  if (!is.list(obj) || !all(c("meta", "weights") %in% names(obj)))
    prevcarga_abort("contract.combina_artifact_mismatch", where = file.path(area, vh))
  if (!identical(obj$meta$member_set_hash, prevcarga_member_set_hash(members)) ||
      !identical(obj$meta$load_label, .prevcarga_cmb_prov1(panel, "load_label", "apply")) ||
      !identical(obj$meta$load_tz, .prevcarga_cmb_prov1(panel, "load_tz", "apply")))
    prevcarga_abort("contract.combina_artifact_mismatch", where = file.path(area, vh))
  rows <- .prevcarga_cmb_select_cut(panel, t_cut)
  .prevcarga_cmb_to_long(rows, prevcarga_produce_combined(rows, obj$weights), ctx)
}
