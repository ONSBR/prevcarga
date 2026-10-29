## combination.R -- nucleo numerico da combina (M4.1): simplex, FISTA, produtores e mascara de treino
##
## Responsabilidade: modulo numerico in-process (base + data.table + lubridate, sem dependencia nova).
## Pipeline da combina: discover -> assert_members_aligned -> build_panel -> join_fresh_truth ->
## fit por (lead, cut, ym) -> produce. Cobre fit/produce (M4.1) e a camada de IO (M4.2: discover,
## build_panel, join_fresh_truth, combina_load, panel_hash) e o alinhamento (M4.2b: assert_members_aligned).
## Requer (alem de errors.R): topology.R, provenance.R, schema_long.R, deck_reader.R, evaluation.R.
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

## --- painel por corte --------------------------------------------------------------------------------
prevcarga_build_panel <- function(long, members = NULL) {
  .prevcarga_cmb_need(long, c("member", "run_id", "area", "forecast_origin", "target_datetime", "lead",
                              "y_pred", "d_plus", "granularity_min"), "build_panel")
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
  tgt <- unique(long[, c(kk, "target_datetime", "d_plus", "granularity_min"), with = FALSE])
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
  data.table::setcolorder(panel, c("area", "forecast_origin", "target_datetime", "lead", "d_plus",
                                   "granularity_min", "cut", "ym", "y_true", paste0("f_", members$sigla)))
  data.table::setorderv(panel, c("area", "cut", "lead", "forecast_origin"))
  data.table::setattr(panel, "members_declared", decl)
  data.table::setattr(panel, "members_present", pres)
  data.table::setattr(panel, "provenance", prov)
  panel
}

## --- verdade fresca por interval_end ------------------------------------------------------------------
prevcarga_join_fresh_truth <- function(panel, ctx, area) {
  .prevcarga_cmb_need(panel, c("target_datetime"), "join_fresh_truth")
  at <- attributes(panel)[c("members_declared", "members_present", "provenance")]
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

## fim da faixa declarada (delivery_days) e grade completa entre o primeiro e o ultimo alvo esperados
.prevcarga_cmb_check_band <- function(t_o, first_exp, last_exp, p) {
  if (max(t_o) != last_exp)
    .prevcarga_cmb_misaligned(p$member, "delivery_days", format(last_exp), format(max(t_o)))
  n_exp <- length(seq(first_exp, last_exp, by = "30 min"))
  if (length(unique(t_o)) != n_exp)
    .prevcarga_cmb_misaligned(p$member, "grade", paste0(n_exp, " alvos de 30 min"), length(unique(t_o)))
  invisible(TRUE)
}

.prevcarga_cmb_check_times <- function(d, p, tz) {
  td <- d$target_datetime
  if (!identical(lubridate::tz(td), tz))
    .prevcarga_cmb_misaligned(p$member, "tzone", tz, lubridate::tz(td))
  if (any(!lubridate::minute(td) %in% c(0L, 30L) | lubridate::second(td) != 0))
    .prevcarga_cmb_misaligned(p$member, "grade", "minuto em {0,30}", "fora da grade")
  if (nzchar(p$cut_suffix) && !identical(p$cut_suffix, paste0("_c", p$cut)))
    .prevcarga_cmb_misaligned(p$member, "available_until", paste0("_c", p$cut), p$cut_suffix)
  hh <- as.integer(substr(p$cut, 1L, 2L)); mm <- as.integer(substr(p$cut, 3L, 4L))
  for (o in sort(unique(d$forecast_origin), method = "radix")) {
    t_o  <- td[d$forecast_origin == o]
    au_o <- lubridate::ymd(o, tz = tz) + lubridate::hours(hh) + lubridate::minutes(mm)
    if (identical(o, p$origin) && au_o != p$available_until)
      .prevcarga_cmb_misaligned(p$member, "available_until", format(au_o), format(p$available_until))
    first_exp <- au_o + lubridate::minutes(30)
    last_exp  <- lubridate::ymd(o, tz = tz) + lubridate::days(p$delivery_days)
    if (min(t_o) != first_exp) {
      shifted <- any(min(t_o) == first_exp + lubridate::minutes(c(-30, 30))) &&
                 any(max(t_o) == last_exp + lubridate::minutes(c(-30, 30)))
      .prevcarga_cmb_misaligned(p$member, if (shifted) "deslocamento" else "primeira_entrega",
                                format(first_exp), format(min(t_o)))
    }
    .prevcarga_cmb_check_band(t_o, first_exp, last_exp, p)
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

## hash do painel pelo hash do M1.3 (version_hash exige as 8 chaves; o conteudo vai em data_fingerprint)
prevcarga_combina_panel_hash <- function(panel) {
  .prevcarga_cmb_need(panel, c("area", "forecast_origin", "target_datetime", "lead", "d_plus",
                               "granularity_min", "cut", "ym", "y_true"), "panel_hash")
  fcols <- .prevcarga_cmb_fcols(panel, "panel_hash")
  cols <- c("area", "forecast_origin", "target_datetime", "lead", "d_plus", "granularity_min", "cut",
            "ym", fcols, "y_true")
  body <- lapply(stats::setNames(cols, cols), function(k) panel[[k]])
  prov <- attr(panel, "provenance")
  one <- function(k) {
    v <- if (is.null(prov) || !k %in% names(prov)) character() else sort(unique(stats::na.omit(prov[[k]])))
    if (length(v)) paste(v, collapse = "+") else "unknown"
  }
  prevcarga_version_hash(list(
    model_id = "combina_panel",
    data_fingerprint = .prevcarga_sha256(.prevcarga_canon_json(body)),
    cutoff = "none",
    feature_config = list(columns = cols),
    code_commit = "none", renv_lock_hash = "none",
    load_label = one("load_label"), load_tz = one("load_tz")))
}
