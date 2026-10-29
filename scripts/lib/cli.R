## cli.R -- logica da fachada da CLI (M1.4): descoberta de verbos, usage docopt gerado, dispatch runtime-first.
##
## scripts/prevcarga e so um shim; tudo que e logica mora aqui. Verbos entram na gramatica criando
## scripts/lib/cli/<verbo>.R (barrel por descoberta, ADR-0040/0018): ninguem edita este arquivo nem o shim.
## Requer errors.R, config.R, banner.R e runtime.R carregados antes. Saida de terminal: ASCII puro.
## Sem variaveis de ambiente.

.prevcarga_cli_state <- new.env(parent = emptyenv())
.prevcarga_cli_state$lib <- local({
  of <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (is.character(of) && length(of) == 1L && file.exists(of))
    dirname(normalizePath(of, winslash = "/"))
  else NULL
})

prevcarga_cli_dir <- function() {
  lib <- getOption("prevcarga.lib", .prevcarga_cli_state$lib)
  if (is.null(lib)) lib <- file.path("scripts", "lib")
  file.path(lib, "cli")
}

## --- Verbos planejados de outros donos (verbo -> dono). Sem arquivo em cli/ viram stub. ---------------
prevcarga_verbs_planned <- c(
  `validate-deck` = "M2", score = "M3", `check-coherence` = "M3", combine = "M4",
  reconcile = "epico Reconcile", `train-models` = "M5", `infer-model` = "M5", registry = "M5",
  features = "M6", forecast = "M6", export = "M6", backtest = "M7", plan = "M7",
  verificar = "M8", `release-pack` = "M8")

## --- Flags globais (bloco Options do docopt). Default e do config.R, nunca do docopt. ------------------
.prevcarga_global_options <- function() c(
  "  --origin=<data>          Forecast-origin YYYY-MM-DD (unico flag de data humano; A..B so em backtest/plan)",
  "  --data=<dir>             Diretorio do store de dados",
  "  --state=<dir>            Diretorio de estado",
  "  --config=<arquivo>       Arquivo config.yaml",
  "  --load-label=<l>         Rotulo da carga: end ou start",
  "  --weather-cutoff=<w>     Corte do tempo: available_until ou origin_midnight_utc",
  "  --format=<fmt>           Saida: text ou json",
  "  --color=<c>              Cor: auto, always ou never",
  "  --model=<m>              Seletor de modelo",
  "  --debug                  Mostra o traceback em erro inesperado",
  "  -h --help                Mostra esta ajuda",
  "  --version                Mostra a versao")

## --- Descoberta -----------------------------------------------------------------------------------------
.prevcarga_verb_row <- function(rec, planned_owner = NULL) {
  if (is.null(rec)) {
    list(verb = NA_character_, owner = planned_owner, exempt = FALSE, modules = character(),
         usage = NA_character_, options = character(), handler = NULL)
  } else {
    list(verb = rec$name, owner = as.character(rec$owner), exempt = isTRUE(rec$exempt),
         modules = as.character(rec$modules), usage = rec$usage,
         options = as.character(rec$options), handler = rec$handler)
  }
}

.prevcarga_verbs_table <- function(rows) {
  n <- length(rows)
  df <- data.frame(
    verb = vapply(rows, function(r) r$verb, ""),
    owner = vapply(rows, function(r) r$owner, ""),
    exempt = vapply(rows, function(r) r$exempt, NA),
    usage = vapply(rows, function(r) r$usage, ""),
    stringsAsFactors = FALSE)
  df$modules <- lapply(rows, function(r) r$modules)
  df$options <- lapply(rows, function(r) r$options)
  df$handler <- lapply(rows, function(r) r$handler)
  if (n == 0L) df <- df[0, , drop = FALSE]
  rownames(df) <- NULL
  df
}

.prevcarga_load_verb_file <- function(f) {
  verb <- sub("\\.R$", "", basename(f))
  env <- new.env(parent = globalenv())
  tryCatch(sys.source(f, envir = env),
           error = function(e) stop("cli.R: falha ao carregar ", basename(f), " (bug do pacote): ",
                                    conditionMessage(e), call. = FALSE))
  objs <- grep("^prevcarga_verb_", ls(env, all.names = FALSE), value = TRUE)
  if (length(objs) != 1L)
    stop("cli.R: ", basename(f), " deve definir exatamente um objeto prevcarga_verb_<verbo> (bug do pacote)",
         call. = FALSE)
  rec <- get(objs, envir = env, inherits = FALSE)
  if (!is.list(rec) || !is.character(rec$name) || length(rec$name) != 1L || !identical(rec$name, verb))
    stop("cli.R: ", basename(f), ": campo name diverge do nome do arquivo (bug do pacote)", call. = FALSE)
  if (!is.function(rec$handler))
    stop("cli.R: ", basename(f), ": handler nao e funcao (bug do pacote)", call. = FALSE)
  if (!(is.character(rec$usage) && length(rec$usage) == 1L && nzchar(rec$usage)))
    stop("cli.R: ", basename(f), ": usage ausente (bug do pacote)", call. = FALSE)
  if (is.null(rec$owner)) rec$owner <- NA_character_
  rec
}

prevcarga_verbs_discover <- function(dir = prevcarga_cli_dir(), planned = prevcarga_verbs_planned) {
  files <- if (dir.exists(dir)) list.files(dir, pattern = "\\.R$", full.names = TRUE) else character()
  files <- files[order(basename(files), method = "radix")]
  rows <- lapply(files, function(f) .prevcarga_verb_row(.prevcarga_load_verb_file(f)))
  found <- vapply(rows, function(r) r$verb, "")
  for (v in setdiff(names(planned), found)) {
    r <- .prevcarga_verb_row(NULL, planned_owner = unname(planned[[v]]))
    r$verb <- v
    r$usage <- sprintf("prevcarga %s [<args>...]", v)
    rows[[length(rows) + 1L]] <- r
  }
  tab <- .prevcarga_verbs_table(rows)
  tab <- tab[order(tab$verb, method = "radix"), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

## --- Usage docopt ---------------------------------------------------------------------------------------
prevcarga_usage <- function(verbs) {
  vlines <- vapply(verbs$usage, function(u) {
    if (grepl("[options]", u, fixed = TRUE)) u else paste(u, "[options]")
  }, "", USE.NAMES = FALSE)
  own <- unique(unlist(verbs$options))
  paste(c("prevcarga - CLI modular de previsao de carga", "", "Usage:",
          paste0("  ", vlines),
          "  prevcarga (-h | --help) [options]", "  prevcarga --version [options]", "  prevcarga [options]", "",
          "Options:", .prevcarga_global_options(), if (length(own)) paste0("  ", sub("^\\s+", "", own))),
        collapse = "\n")
}

## Tabela de flags conhecidas: nome -> toma argumento? (para localizar o token rejeitado).
.prevcarga_opt_table <- function(verbs) {
  lines <- c(.prevcarga_global_options(), unlist(verbs$options))
  nm <- character(); takes <- logical()
  for (l in lines) {
    head <- sub("\\s{2,}.*$", "", sub("^\\s+", "", l))
    m <- regmatches(head, gregexpr("--[A-Za-z0-9-]+(=<[^>]+>)?", head))[[1L]]
    for (x in m) { nm <- c(nm, sub("=.*$", "", x)); takes <- c(takes, grepl("=", x, fixed = TRUE)) }
  }
  stats::setNames(takes, nm)
}

.prevcarga_usage_culprit <- function(argv, verbs) {
  opts <- .prevcarga_opt_table(verbs)
  verb <- NULL
  i <- 1L
  while (i <= length(argv)) {
    t <- argv[[i]]
    if (startsWith(t, "-")) {
      name <- sub("=.*$", "", t)
      if (!(name %in% c(names(opts), "-h"))) return(t)
      if (name %in% names(opts) && isTRUE(opts[[name]]) && !grepl("=", t, fixed = TRUE)) i <- i + 1L
    } else if (is.null(verb)) {
      if (!(t %in% verbs$verb)) return(t)
      verb <- t
    } else {
      return(t)
    }
    i <- i + 1L
  }
  if (length(argv)) paste(argv, collapse = " ") else "-"
}

## --- Versao / ctx tolerante ------------------------------------------------------------------------------
.prevcarga_version <- function(root) {
  vf <- file.path(root, "config", "VERSION")
  if (file.exists(vf)) trimws(readLines(vf, n = 1L, warn = FALSE)) else "unknown"
}

## ctx de defaults + flags (sem ler config.yaml): usado por doctor/status e pelo topo (--help/--version)
## quando o config.yaml esta quebrado. A falha original vai em ctx$config_error.
.prevcarga_ctx_tolerant <- function(args, root, verb, error = NULL) {
  nofile <- file.path(tempdir(), "prevcarga-no-root-", basename(tempfile()))
  a <- args
  a[["--config"]] <- NULL
  ctx <- prevcarga_ctx(a, nofile, verb)
  ctx$root <- root
  ctx$version <- .prevcarga_version(root)
  ctx$config_path <- NULL
  if (is.null(args[["--state"]]) || identical(args[["--state"]], FALSE))
    ctx$state_dir <- normalizePath(file.path(root, ".state"), winslash = "/", mustWork = FALSE)
  ctx$config_error <- error
  ctx
}

## --- Render ----------------------------------------------------------------------------------------------
.prevcarga_ascii <- function(x) iconv(x, from = "UTF-8", to = "ASCII", sub = "?")

.prevcarga_text_lines <- function(result, verb) {
  fn <- get0(paste0("prevcarga_render_", gsub("-", "_", verb)), mode = "function")
  lines <- if (is.function(fn)) fn(result)
           else if (is.null(result)) character()
           else if (is.character(result)) result
           else utils::capture.output(print(unclass(result)))
  .prevcarga_ascii(as.character(lines))
}

prevcarga_emit <- function(out, ctx, verb, warnings = list(), exit_code = 0L) {
  if (identical(ctx$format, "json")) {
    env <- prevcarga_envelope(verb = verb, result = out, warnings = warnings,
                              run_id = ctx$run_id, version = ctx$version)
    env$exit_code <- as.integer(exit_code)
    env$ok <- identical(as.integer(exit_code), 0L)
    writeLines(as.character(jsonlite::toJSON(env, auto_unbox = TRUE, null = "null", digits = NA,
                                             force = TRUE)), con = stdout())
  } else {
    if (prevcarga_banner_wanted(ctx, verb)) writeLines(prevcarga_banner(ctx), con = stdout())
    lines <- .prevcarga_text_lines(out, verb)
    if (length(lines)) writeLines(lines, con = stdout())
    for (w in warnings) writeLines(prevcarga_render_warning_text(w), con = stderr())
  }
  invisible(exit_code)
}

## --- Dispatch ---------------------------------------------------------------------------------------------
.prevcarga_pick_handler <- function(verb, row, handlers) {
  h <- if (!is.null(handlers)) handlers[[verb]] else NULL
  if (is.null(h)) h <- row$handler[[1L]]
  h
}

.prevcarga_call_handler <- function(h, ctx, args) {
  fm <- names(formals(h))
  if (length(fm) >= 2L || "..." %in% fm) h(ctx, args) else h(ctx)
}

.prevcarga_run_verb <- function(verb, row, args, root, runtime, handlers, st) {
  if (!isTRUE(row$exempt)) runtime()                       # 1a acao do corpo (ADR-0022)
  ctx <- if (isTRUE(row$exempt)) {
    tryCatch(prevcarga_ctx(args, root, verb),
             prevcarga_error = function(e) .prevcarga_ctx_tolerant(args, root, verb, error = e))
  } else prevcarga_ctx(args, root, verb)
  st$ctx <- ctx
  h <- .prevcarga_pick_handler(verb, row, handlers)
  if (is.null(h)) {                                         # planejado, sem scripts/lib/cli/<verbo>.R
    prevcarga_abort("todo.not_implemented", where = sprintf("%s -> %s", verb, row$owner),
                    details = list(hint = sprintf("implementado no %s", row$owner)))
  }
  for (m in row$modules[[1L]]) sys.source(file.path(root, "scripts", "lib", m), envir = globalenv())
  result <- .prevcarga_call_handler(h, ctx, args)
  list(ctx = ctx, result = result)
}

.prevcarga_top_level <- function(argv, verbs, root, args) {
  ctx <- tryCatch(prevcarga_ctx(if (is.null(args)) list() else args, root, NULL),
                  error = function(e) .prevcarga_ctx_tolerant(if (is.null(args)) list() else args, root, NULL, e))
  show_version <- identical(argv[1L], "--version") || isTRUE(args[["--version"]])
  if (prevcarga_banner_wanted(ctx, if (show_version) "--version" else "--help"))
    writeLines(prevcarga_banner(ctx), con = stdout())
  if (show_version) writeLines(paste("prevcarga", ctx$version), con = stdout())
  else writeLines(prevcarga_usage(verbs), con = stdout())
  0L
}

.prevcarga_parse <- function(argv, verbs) {
  usage <- prevcarga_usage(verbs)
  res <- NULL
  utils::capture.output(
    res <- tryCatch(docopt::docopt(usage, args = argv, help = FALSE, strip_names = FALSE),
                    error = function(e) NULL),
    type = "output")
  res
}

prevcarga_main <- function(argv, verbs = prevcarga_verbs_discover(), root, runtime = prevcarga_runtime_enforce,
                           handlers = NULL) {
  argv <- as.character(argv)
  st <- new.env(parent = emptyenv())
  st$ctx <- NULL
  st$verb <- NULL
  debug <- "--debug" %in% argv
  warns <- list()
  body <- function() {
    args <- .prevcarga_parse(argv, verbs)
    hit <- if (is.null(args)) character() else verbs$verb[vapply(verbs$verb, function(v) isTRUE(args[[v]]), NA)]
    toplevel <- !length(argv) || any(argv %in% c("-h", "--help")) || identical(argv[1L], "--version") ||
      (!is.null(args) && !length(hit))
    if (toplevel) return(list(top = TRUE, code = .prevcarga_top_level(argv, verbs, root, args)))
    if (is.null(args))
      prevcarga_abort("config.usage", where = .prevcarga_usage_culprit(argv, verbs))
    if (length(hit) != 1L)
      prevcarga_abort("config.usage", where = .prevcarga_usage_culprit(argv, verbs))
    st$verb <- hit
    row <- verbs[verbs$verb == hit, , drop = FALSE]
    withCallingHandlers(
      .prevcarga_run_verb(hit, row, args, root, runtime, handlers, st),
      prevcarga_warning = function(w) {
        warns[[length(warns) + 1L]] <<- c(list(id = w$id, message = prevcarga_error_lookup(w$id)$message),
                                             if (!is.null(w$details$type)) list(type = w$details$type))
        invokeRestart("muffleWarning")
      })
  }
  out <- prevcarga_with_errors(body(), debug = debug || isTRUE(st$ctx$debug))
  if (is.null(out$cond)) {
    v <- out$value
    if (isTRUE(v$top)) return(as.integer(v$code))
    code <- attr(v$result, "exit_code", exact = TRUE)
    code <- if (is.null(code)) 0L else as.integer(code)
    prevcarga_emit(v$result, v$ctx, st$verb, warnings = warns, exit_code = code)
    return(code)
  }
  ctx <- st$ctx
  fmt <- if (!is.null(ctx)) ctx$format else
    if (any(argv == "--format=json") || any(argv[-length(argv)] == "--format" & argv[-1L] == "json")) "json" else "text"
  lines <- prevcarga_render_error(out$cond, format = fmt, verb = st$verb, warnings = NULL,
                                  run_id = if (!is.null(ctx)) ctx$run_id else prevcarga_run_id(),
                                  version = if (!is.null(ctx)) ctx$version else .prevcarga_version(root))
  if (identical(fmt, "json")) writeLines(lines, con = stdout()) else writeLines(.prevcarga_ascii(lines), con = stderr())
  as.integer(out$exit_code)
}
