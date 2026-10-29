## introspect.R -- comandos de introspeccao sobre o ctx (M1.5): doctor, status, models, areas, describe.
##
## Padrao por verbo: construtor puro prevcarga_build_<verbo>(ctx) (testavel in-process), renderizador texto
## ASCII prevcarga_render_<verbo>(result) e cmd_<verbo>(ctx, args) fino, que SO devolve o resultado: quem
## imprime (texto/JSON, banner, exit) e prevcarga_emit() na fachada (cli.R). Os registros dos verbos moram em
## scripts/lib/cli/<verbo>.R (barrel por descoberta). Nenhum valor/contagem local: topologia, specs e
## runtime vem de topology.R/runtime.R/provenance.R; cortes de data vem de dates.R (M2.5). Sem variaveis de ambiente.
##
## Requer errors.R, runtime.R, config.R carregados; topology.R/provenance.R nos verbos guardados.

.prevcarga_doctor_status <- c("PASS", "WARN", "FAIL", "N/A")
.prevcarga_doctor_required <- c("docopt", "yaml", "jsonlite", "digest", "lubridate", "RhpcBLASctl")
.prevcarga_doctor_optional <- c("arrow", "withr")
.prevcarga_doctor_r_expected <- "4.5.3"

.prevcarga_chr_or <- function(x, na = "-") {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) na else as.character(x[[1L]])
}

.prevcarga_doctor_row <- function(check, status, found = "-", effective = "-") {
  data.frame(check = check, status = status, found = found, effective = effective,
             stringsAsFactors = FALSE)
}

## --- linhas de runtime: found x effective do check injetado ------------------------------------------
.prevcarga_doctor_runtime_rows <- function(chk) {
  f <- chk$found; e <- chk$effective; off <- chk$offenders
  spec <- list(
    list("TZ", f$tz, e$tz, "TZ"),
    list("locale", f$lc, e$lc, c("LC_CTYPE", "LC_COLLATE", "LC_NUMERIC")),
    list("OMP_NUM_THREADS", f$omp, e$omp, "OMP_NUM_THREADS"),
    list("OPENBLAS_NUM_THREADS", f$openblas, e$openblas, "OPENBLAS_NUM_THREADS"),
    list("MKL_NUM_THREADS", f$mkl, e$mkl, "MKL_NUM_THREADS"),
    list("VECLIB_MAXIMUM_THREADS", f$veclib, e$veclib, "VECLIB_MAXIMUM_THREADS"),
    list("pool BLAS", f$blas_threads, e$blas_threads, "BLAS"))
  do.call(rbind, lapply(spec, function(s)
    .prevcarga_doctor_row(s[[1L]], if (any(s[[4L]] %in% off)) "FAIL" else "PASS",
                          .prevcarga_chr_or(s[[2L]]), .prevcarga_chr_or(s[[3L]]))))
}

.prevcarga_doctor_pkg_rows <- function(has_pkg) {
  one <- function(p, miss) {
    ok <- isTRUE(has_pkg(p))
    .prevcarga_doctor_row(p, if (ok) "PASS" else miss, if (ok) "instalado" else "ausente",
                          if (ok) "instalado" else "ausente")
  }
  do.call(rbind, c(lapply(.prevcarga_doctor_required, one, miss = "FAIL"),
                   lapply(.prevcarga_doctor_optional, one, miss = "WARN")))
}

.prevcarga_doctor_config_row <- function(ctx) {
  err <- ctx$config_error
  if (!is.null(err)) {
    id <- if (!is.null(err$id)) err$id else "config.bad_type"
    return(.prevcarga_doctor_row("config.yaml", "FAIL", id, "defaults + flags"))
  }
  .prevcarga_doctor_row("config.yaml", "PASS",
                        if (is.null(ctx$config_path)) "nenhum (defaults)" else ctx$config_path, "valido")
}

.prevcarga_doctor_data_row <- function(ctx) {
  if (is.null(ctx$data_dir)) .prevcarga_doctor_row("data_dir", "WARN", "ausente", "-")
  else .prevcarga_doctor_row("data_dir", "PASS", ctx$data_dir, ctx$data_dir)
}

## Constroi TODAS as linhas antes de calcular o exit (nunca aborta no meio): o numero de linhas e constante.
prevcarga_build_doctor <- function(ctx, check = prevcarga_runtime_apply,
                                   has_pkg = function(p) requireNamespace(p, quietly = TRUE),
                                   r_version = as.character(getRversion())) {
  chk <- check()
  rv <- .prevcarga_doctor_row("versao do R", if (identical(r_version, .prevcarga_doctor_r_expected)) "PASS" else "WARN",
                              r_version, .prevcarga_doctor_r_expected)
  runtime_rows <- .prevcarga_doctor_runtime_rows(chk)
  pkg_rows <- .prevcarga_doctor_pkg_rows(has_pkg)
  cfg_row <- .prevcarga_doctor_config_row(ctx)
  checks <- rbind(runtime_rows, rv, pkg_rows, cfg_row, .prevcarga_doctor_data_row(ctx))
  rownames(checks) <- NULL
  counts <- stats::setNames(lapply(.prevcarga_doctor_status, function(s) sum(checks$status == s)),
                            .prevcarga_doctor_status)
  counts <- lapply(counts, as.integer)
  classes <- c(if (any(runtime_rows$status == "FAIL")) "runtime",
               if (any(pkg_rows$status == "FAIL")) "bundle",
               if (cfg_row$status == "FAIL") "config")
  code <- if (length(classes)) unname(prevcarga_exit_codes[[prevcarga_exit_precedence(classes)]]) else 0L
  res <- list(schema_version = 1L, checks = checks, counts = counts,
              offenders = checks$check[checks$status == "FAIL"])
  structure(res, exit_code = as.integer(code))
}

prevcarga_render_doctor <- function(result) {
  ck <- result$checks
  w <- max(nchar(ck$check))
  lines <- sprintf("[%-4s] %-*s  found=%s  effective=%s", ck$status, w, ck$check, ck$found, ck$effective)
  cn <- result$counts
  c("doctor: diagnostico do ambiente", lines,
    sprintf("resumo: PASS=%d WARN=%d FAIL=%d N/A=%d", cn$PASS, cn$WARN, cn$FAIL, cn[["N/A"]]),
    if (length(result$offenders)) paste("ofensores:", paste(result$offenders, collapse = ", ")))
}

cmd_doctor <- function(ctx, args = NULL) prevcarga_build_doctor(ctx)

## --- status ------------------------------------------------------------------------------------------
## Best-effort: deteccao real do deck e do M2. O manifest e lido pelo leitor unico (deck_reader.R), se carregado.
.prevcarga_status_deck <- function(ctx, reader = get0("prevcarga_deck_manifest", mode = "function")) {
  if (is.null(ctx$data_dir)) return(list(note = "sem deck"))
  if (!dir.exists(ctx$data_dir)) return(list(path = ctx$data_dir, note = "sem deck"))
  if (!is.function(reader)) return(list(path = ctx$data_dir, note = "leitor do deck indisponivel"))
  m <- tryCatch(reader(ctx$data_dir), error = function(e)
    if (identical(e$id, "contract.manifest_missing")) "sem deck" else NULL)
  if (identical(m, "sem deck")) return(list(path = ctx$data_dir, note = "sem deck"))
  if (is.null(m)) return(list(path = ctx$data_dir, note = "manifest ilegivel"))
  list(path = ctx$data_dir, load_label = .prevcarga_chr_or(m$load_label, NA_character_),
       timezone = .prevcarga_chr_or(m$timezone$load, NA_character_))
}

prevcarga_build_status <- function(ctx, check = prevcarga_runtime_apply,
                                   has_pkg = function(p) requireNamespace(p, quietly = TRUE),
                                   deck_reader = get0("prevcarga_deck_manifest", mode = "function")) {
  d <- prevcarga_build_doctor(ctx, check = check, has_pkg = has_pkg)
  res <- list(schema_version = 1L, version = ctx$version, counts = d$counts, offenders = d$offenders,
              deck = .prevcarga_status_deck(ctx, deck_reader), bundle = "N/A")
  structure(res, exit_code = 0L)
}

prevcarga_render_status <- function(result) {
  dk <- result$deck
  deck <- if (!is.null(dk$load_label) || !is.null(dk$timezone))
    sprintf("deck: %s load_label=%s timezone=%s", dk$path, dk$load_label, dk$timezone)
  else sprintf("deck: %s", dk$note)
  cn <- result$counts
  c(sprintf("status: prevcarga %s", .prevcarga_chr_or(result$version)),
    sprintf("ambiente: PASS=%d WARN=%d FAIL=%d N/A=%d", cn$PASS, cn$WARN, cn$FAIL, cn[["N/A"]]),
    if (length(result$offenders)) paste("ofensores:", paste(result$offenders, collapse = ", ")),
    deck, sprintf("bundle: %s", result$bundle))
}

cmd_status <- function(ctx, args = NULL) prevcarga_build_status(ctx)

## --- models ------------------------------------------------------------------------------------------
prevcarga_build_models <- function(ctx) {
  mem <- prevcarga_members(root = ctx$root)
  specs <- list()
  for (m in unique(mem$mdir)) specs[[m]] <- prevcarga_model_spec(m, root = ctx$root)
  rows <- lapply(seq_len(nrow(mem)), function(i) {
    s <- specs[[mem$mdir[[i]]]]
    list(sigla = mem$sigla[[i]], mdir = mem$mdir[[i]],
         exogena = if (is.na(mem$exogena[[i]])) NULL else mem$exogena[[i]],
         split = s$split, granularity_min = s$granularity_min, horizon_days = s$horizon_days,
         history_days = s$history_days, seed = s$seed, modes = s$modes,
         protocol_default = s$backtest$protocol_default)
  })
  list(schema_version = 1L, members = rows)
}

prevcarga_render_models <- function(result) {
  lines <- vapply(result$members, function(r)
    sprintf("%-9s %-14s split=%s gran=%dmin horizonte=%dd historico=%d/%d seed=%d modos=%s protocolo=%s exogena=%s",
            r$sigla, r$mdir, r$split, r$granularity_min, r$horizon_days, r$history_days$train,
            r$history_days$infer, r$seed, paste(r$modes, collapse = ","), r$protocol_default,
            .prevcarga_chr_or(r$exogena)), "")
  c(sprintf("models: %d membros", length(result$members)), lines)
}

cmd_models <- function(ctx, args = NULL) prevcarga_build_models(ctx)

## --- areas -------------------------------------------------------------------------------------------
prevcarga_build_areas <- function(ctx) {
  t <- prevcarga_topology(root = ctx$root)
  list(schema_version = 1L, sin = "SIN", order = t$order,
       subsystems = lapply(t$order, function(s) list(
         name = s, loss = t$subsystems[[s]]$loss,
         khat_range = t$subsystems[[s]]$khat_range,
         areas = t$subsystems[[s]]$areas)))
}

prevcarga_render_areas <- function(result) {
  c(sprintf("%s", result$sin),
    unlist(lapply(result$subsystems, function(s)
      c(sprintf("  %s  perda=%s  khat_range=%.2f-%.2f", s$name, s$loss, s$khat_range[[1L]], s$khat_range[[2L]]),
        sprintf("    areas: %s", paste(s$areas, collapse = " "))))))
}

cmd_areas <- function(ctx, args = NULL) prevcarga_build_areas(ctx)

## --- describe ----------------------------------------------------------------------------------------
## M2.5: os cortes saem de dates.R. Com origem e deck legivel, prevcarga_resolve_cuts (inclui available_until,
## derivado do deck via validate-deck); sem deck legivel so o que nao depende do dado (corte de treino por
## membro, prevcarga_train_cut) e available_until fica indisponivel. `data_lag_days` (named integer) vem dos
## descriptors quando existirem (P.0b/M5); ate la a fonte e `default`. Requer dates.R e deck_reader.R.
.prevcarga_describe_au_label <- "derivado do deck (via validate-deck)"

.prevcarga_describe_fmt_date <- function(x) if (is.null(x) || is.na(x)) NULL else format(x, "%Y-%m-%d")

.prevcarga_describe_fallback <- function(ctx, mdirs, data_lag_days) {
  O <- ctx$origin
  rows <- lapply(mdirs, function(m) {
    spec <- prevcarga_model_spec(m, root = ctx$root)
    declared <- if (!is.null(data_lag_days) && m %in% names(data_lag_days)) data_lag_days[[m]] else NULL
    lag <- if (is.null(declared)) 0L else as.integer(declared)
    tc <- prevcarga_train_cut(spec, O, lag)
    list(model = m, split = spec$split, horizon_days = as.integer(spec$horizon_days),
         train_origin = .prevcarga_describe_fmt_date(tc$train_origin),
         last_data_day = .prevcarga_describe_fmt_date(tc$last_data_day),
         data_lag_days = lag, data_lag_source = if (is.null(declared)) "default" else "descriptor")
  })
  list(rows = rows, available_until = NULL)
}

.prevcarga_describe_cuts <- function(ctx, mdirs, data_lag_days) {
  O <- if (inherits(ctx$origin, "Date")) ctx$origin else NULL
  if (is.null(O)) return(list(rows = list(), available_until = NULL, note = "sem --origin: cortes nao calculados"))
  deck <- !is.null(ctx$data_dir) && dir.exists(ctx$data_dir)
  cv <- if (deck) tryCatch(prevcarga_resolve_cuts(ctx, O, mdirs, data_lag_days),
                           prevcarga_error = function(e) if (e$class_name %in% c("contract", "completude")) NULL else stop(e)) else NULL
  if (is.null(cv)) {
    out <- .prevcarga_describe_fallback(ctx, mdirs, data_lag_days)
    out$note <- if (deck) "deck ilegivel: available_until indisponivel" else "sem deck: available_until indisponivel"
    return(out)
  }
  ct <- cv$cuts
  rows <- lapply(seq_len(nrow(ct)), function(i) list(
    model = ct$model[[i]], split = ct$split[[i]], horizon_days = as.integer(ct$horizon_days[[i]]),
    train_origin = .prevcarga_describe_fmt_date(ct$train_origin[[i]]),
    last_data_day = .prevcarga_describe_fmt_date(ct$last_data_day[[i]]),
    data_lag_days = as.integer(ct$data_lag_days[[i]]), data_lag_source = ct$data_lag_source[[i]]))
  list(rows = rows, note = NULL,
       available_until = format(lubridate::with_tz(cv$available_until, cv$tz), "%Y-%m-%d %H:%M"))
}

prevcarga_build_describe <- function(ctx, model = ctx$model, data_lag_days = NULL) {
  mem <- prevcarga_members(root = ctx$root)
  mdirs <- unique(mem$mdir)
  if (!is.null(model) && !identical(model, FALSE)) {
    if (!(is.character(model) && length(model) == 1L && model %in% mdirs))
      prevcarga_abort("config.unknown_model", where = as.character(model)[[1L]])
    mdirs <- model
  }
  specs <- list()
  for (m in mdirs) specs[[m]] <- prevcarga_model_spec(m, root = ctx$root)
  origin <- if (is.null(ctx$origin)) NULL else
    if (inherits(ctx$origin, "Date")) format(ctx$origin, "%Y-%m-%d") else NULL
  cu <- .prevcarga_describe_cuts(ctx, mdirs, data_lag_days)
  list(schema_version = 1L, origin = origin, topology = prevcarga_build_areas(ctx), models = specs,
       member_set_hash = prevcarga_member_set_hash(mem),
       cuts = list(members = cu$rows, available_until = cu$available_until,
                   available_until_source = .prevcarga_describe_au_label, note = cu$note))
}

prevcarga_render_describe <- function(result) {
  specs <- unlist(lapply(result$models, function(s)
    sprintf("  %s: split=%s gran=%dmin horizonte=%dd historico=%d/%d seed=%d modos=%s protocolo=%s",
            s$name, s$split, s$granularity_min, s$horizon_days, s$history_days$train, s$history_days$infer,
            s$seed, paste(s$modes, collapse = ","), s$backtest$protocol_default)))
  cu <- result$cuts
  cuts <- if (length(cu$members)) c("cortes por membro:",
    vapply(cu$members, function(r)
      sprintf("  %-14s split=%s horizonte=%dd train_origin=%s last_data_day=%s data_lag_days=%d (%s)",
              r$model, r$split, r$horizon_days, r$train_origin, r$last_data_day, r$data_lag_days,
              r$data_lag_source), ""))
  au <- sprintf("available_until: %s%s", cu$available_until_source,
                if (!is.null(cu$available_until)) paste0(" = ", cu$available_until) else
                  paste0(" [indisponivel", if (!is.null(cu$note)) paste0(": ", cu$note), "]"))
  c("describe: topologia e contratos",
    if (!is.null(result$origin)) paste("origin:", result$origin),
    prevcarga_render_areas(result$topology), "modelos:", specs,
    paste("member_set_hash:", result$member_set_hash),
    cuts, au)
}

cmd_describe <- function(ctx, args = NULL) {
  ctx$model <- if (!is.null(args)) args[["--model"]] else NULL
  prevcarga_build_describe(ctx)
}
