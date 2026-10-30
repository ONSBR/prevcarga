## scripts/lib/cli/reconcile.R - verbo auto-registrado `reconcile` (R.5; descoberto por scripts/lib/cli.R, M1.4).
##
## Passthrough fino para prevcarga_reconcile_train / prevcarga_reconcile_apply (R.3). A borda de I/O (ler o --leaves-dir,
## run_id, escrever no scratch) mora em reconcile.R; aqui so se valida flag e se compoe o result. Daily = uma origem;
## o backtest laca origens fora deste verbo. khat_origin == O sempre: --khat-version so escolhe entre versoes ajustadas
## EM O. Erros e exits vem do catalogo (ADR-0040) pelo handler de topo da fachada: sem quit(), sem env vars.
## Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result`. O run_id do envelope e o
## hash da chave (attr "run_id" do result, honrado por cli.R), nunca PID nem relogio.
## --mode/--out (combine) e --subsistema (train-models) ja constam do bloco Options (a duplicata torna o docopt ambiguo).
## Requer errors.R, config.R (da fachada) e topology.R, schema_long.R, provenance.R, deck_reader.R, dates.R, evaluation.R,
## registry.R, reconcile.R.

.prevcarga_rc_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

.prevcarga_rc_fmt <- function(x) if (is.null(x) || (length(x) == 1L && is.na(x))) NULL else as.character(lubridate::as_date(x))

## flags validadas ANTES de qualquer IO; devolve list(mode, subsystem, window_days)
.prevcarga_rc_flags <- function(args) {
  mode <- .prevcarga_rc_opt(args, "--mode")
  if (is.null(mode)) prevcarga_abort("config.usage", where = "--mode")
  if (!mode %in% c("train", "apply")) prevcarga_abort("config.bad_mode", where = paste0("--mode=", mode))
  if (identical(mode, "train"))
    for (f in c("--eval", "--leaves-dir", "--khat-version", "--out"))
      if (!is.null(.prevcarga_rc_opt(args, f)) && !(f == "--eval" && !isTRUE(args[[f]])))
        prevcarga_abort("config.usage", where = f, details = list(hint = paste(f, "so vale com --mode=apply")))
  wd <- .prevcarga_rc_opt(args, "--khat-window")
  window <- prevcarga_khat_window_days
  if (!is.null(wd)) {
    v <- suppressWarnings(as.numeric(wd))
    if (length(v) != 1L || is.na(v) || v != floor(v) || v < 1)
      prevcarga_abort("config.usage", where = "--khat-window", details = list(hint = "inteiro > 0"))
    window <- as.integer(v)
  }
  ss <- .prevcarga_rc_opt(args, "--subsistema")
  list(mode = mode, subsystem = if (is.null(ss)) "all" else ss, window_days = window)
}

.prevcarga_rc_train <- function(ctx, fl, topo) {
  origin <- prevcarga_resolve_origin(ctx)$origin
  tr <- prevcarga_reconcile_train(ctx, origin, fl$subsystem, fl$window_days)
  khat <- lapply(names(tr), function(ss) {
    r <- tr[[ss]]
    list(ss = ss, versao = r$versao, khat_origin = .prevcarga_rc_fmt(r$khat_origin), window_days = fl$window_days,
         n_ratio = as.integer(r$n_ratio), promovido = isTRUE(r$promovido), k_hat_resumo = r$k_hat_resumo)
  })
  list(mode = "train", origem = .prevcarga_rc_fmt(origin), subsistemas = as.list(names(tr)), sin = FALSE,
       window_days = fl$window_days, khat = khat)
}

.prevcarga_rc_apply <- function(ctx, args, fl, topo) {
  from <- .prevcarga_rc_opt(args, "--leaves-dir")
  if (is.null(from)) prevcarga_abort("config.usage", where = "--leaves-dir")
  rd <- prevcarga_reconcile_read_from(from, ctx, topo)
  origins <- unique(unlist(lapply(rd$leaves, function(l) l$forecast_origin)))
  if (length(origins) == 1L && !is.null(ctx$origin) && !identical(origins, .prevcarga_rc_fmt(ctx$origin)))
    prevcarga_abort("contract.reconcile_input", where = "--origin",
                    details = list(reason = "--origin difere da forecast_origin do --leaves-dir"))
  pin <- .prevcarga_rc_opt(args, "--khat-version")
  ev <- isTRUE(args[["--eval"]])
  ap <- prevcarga_reconcile_apply(unname(rd$leaves), ctx, fl$subsystem, khat_version = pin, eval = ev)
  sss <- setdiff(names(ap$long), "SIN")
  khat <- lapply(sss, function(ss) {
    k <- ap$khat[[ss]]
    prof <- prevcarga_khat_thaw(ctx, ss, k$versao)
    list(ss = ss, versao = k$versao, khat_origin = .prevcarga_rc_fmt(k$khat_origin), khat_age_days = k$khat_age_days,
         window_from = .prevcarga_rc_fmt(k$window_from), window_to = .prevcarga_rc_fmt(k$window_to),
         n_ratio = as.integer(k$n_ratio), n_fallback = k$n_fallback,
         k_hat_resumo = .prevcarga_khat_summary(prof$profile, topo$subsystems[[ss]]$khat_range), promovido = NULL)
  })
  key <- list(verb = "reconcile", mode = "apply", origem = origins, subsistemas = sss, window_days = fl$window_days,
              from = as.list(rd$sha256), khat = lapply(khat, function(k) k$versao), eval = ev)
  run_id <- prevcarga_reconcile_run_id(key)
  out <- .prevcarga_rc_opt(args, "--out")
  if (is.null(out)) {
    scratch <- file.path(ctx$state_dir, "scratch")
    dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
    out <- file.path(scratch, run_id)
  }
  files <- prevcarga_reconcile_write(ap, out, run_id)
  res <- list(mode = "apply", origem = origins, subsistemas = as.list(sss), sin = !is.null(ap$long$SIN),
              window_days = fl$window_days, khat = khat, cobertura_descartada = ap$cobertura_descartada,
              out = out, arquivos = files, wmape = ap$wmape)
  attr(res, "run_id") <- run_id
  res
}

.render_reconcile_text <- function(r) {
  c(sprintf("reconcile  mode=%s  origem=%s  subsistemas=%s", r$mode, r$origem, paste(unlist(r$subsistemas), collapse = ",")),
    vapply(r$khat, function(k) sprintf("khat %-6s versao=%s khat_origin=%s", k$ss, k$versao, k$khat_origin), ""),
    if (!is.null(r$out)) paste0("saida: ", r$out),
    vapply(r$arquivos, function(a) sprintf("arquivo %-24s linhas=%d sha256=%s", a$path, a$linhas, a$sha256), ""),
    "OK")
}

cmd_reconcile <- function(ctx, args) {
  fl <- .prevcarga_rc_flags(args)
  topo <- .prevcarga_reconcile_topo(ctx)
  prevcarga_reconcile_check_subsystem(if (identical(fl$subsystem, "all")) NULL else fl$subsystem, topo)
  prevcarga_require_data_dir(ctx)
  ctx$tz <- .prevcarga_eval_tz(ctx)
  res <- if (identical(fl$mode, "train")) .prevcarga_rc_train(ctx, fl, topo) else .prevcarga_rc_apply(ctx, args, fl, topo)
  if (identical(ctx$format, "json")) res else .render_reconcile_text(res)
}

prevcarga_verb_reconcile <- list(
  name    = "reconcile",
  usage   = paste("prevcarga reconcile --mode=<mode> [--subsistema=<s>] [--khat-window=<n>] [--eval] [--leaves-dir=<dir>]",
                  "[--out=<path>] [--khat-version=<v>]"),
  ## --mode/--out (combine) e --subsistema (train-models) ja constam no bloco Options; so as novas sao declaradas aqui
  options = c(
    "--khat-window=<n>       Janela do k-hat em dias (train; inteiro > 0; padrao 730)",
    "--eval                  Calcula y_true e wmape no apply (le a verdade do deck)",
    "--leaves-dir=<dir>            Diretorio com um parquet LONG por folha (apply)",
    "--khat-version=<v>      v_<hash> do k-hat ajustado EM O (apply; senao o latest)"),
  scoped  = c("--khat-window", "--eval", "--leaves-dir", "--khat-version"),   # so reconcile (cli.R .prevcarga_check_scoped)
  handler = function(ctx, args) cmd_reconcile(ctx, args),
  owner   = "Reconcile",
  exempt  = FALSE,
  modules = c("topology.R", "schema_long.R", "provenance.R", "deck_reader.R", "dates.R", "evaluation.R", "registry.R",
              "reconcile.R")
)
