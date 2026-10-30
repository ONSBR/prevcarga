## scripts/lib/cli/train-models.R - verbo auto-registrado `train-models` (M5.4; descoberto por scripts/lib/cli.R, M1.4).
##
## Treina o subset pedido (membros x areas) sobre prevcarga_train (M5.3, training.R) e publica no registry pelo core
## (prevcarga_registry_publish + promote latest; so registry.R define o ciclo de vida). A logica (matriz, plano, execucao
## serial/PSOCK) mora em training.R; este arquivo e o handler fino: valida flags -> matriz -> plano ou execucao -> result.
## Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result` (a fachada o embrulha no
## envelope do ADR-0040). Erros e exits vem do catalogo (errors/train_models.tsv + core): sem quit(), sem env vars.
## Requer errors.R, config.R, topology.R, provenance.R, deck_reader.R, dates.R, features.R, registry.R, training.R.

.prevcarga_tm_int <- function(x, flag, min) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1L || is.na(v) || v != floor(v) || v < min)
    prevcarga_abort("config.bad_type", where = flag, details = list(got = as.character(x), expected = paste0("inteiro >= ", min)))
  as.integer(v)
}

## Linhas do result: datas ISO, variant NA -> null.
.prevcarga_tm_records <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    r <- lapply(names(df), function(k) {
      x <- df[[k]][[i]]
      if (is.na(x)) NULL else if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x)
    })
    stats::setNames(r, names(df))
  })
}

.prevcarga_tm_iso <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return(NULL)
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", format(x[[1L]], "%Y-%m-%dT%H:%M:%S%z", tz = attr(x, "tzone")))
}

.prevcarga_tm_text <- function(res) {
  c(sprintf("train-models origin=%s dry_run=%s cells=%d", res$origin, tolower(as.character(res$dry_run)), length(res$cells)),
    vapply(res$cells, function(r)
      sprintf("%-8s %-14s %-6s %-14s %s %s", r$status, r$model, if (is.null(r$variant)) "-" else r$variant, r$area,
              r$version, r$path), ""))
}

cmd_train_models <- function(ctx, args) {
  jobs <- if (is.null(.prevcarga_tr_arg(args, "--jobs"))) 1L else .prevcarga_tm_int(args[["--jobs"]], "--jobs", 1)
  hz <- .prevcarga_tr_arg(args, "--horizon")
  if (!is.null(hz)) {
    h <- suppressWarnings(as.integer(hz))
    ctx$spec_overrides <- list(horizon_days = if (is.na(h)) as.character(hz) else h)   # tipo errado => config.override_bad_value
  }
  cells <- prevcarga_train_matrix(args, ctx)                  # seletores e modelos validados antes de qualquer leitura do deck
  prevcarga_require_data_dir(ctx)
  ctx$lib_dir <- getOption("prevcarga.lib")
  origin <- prevcarga_resolve_origin(ctx)$origin
  rc <- prevcarga_resolve_cuts(ctx, origin)
  dry <- isTRUE(args[["--dry-run"]])
  out <- if (dry) prevcarga_train_plan(cells, origin, ctx)
         else prevcarga_train_models(cells, ctx, origin = origin, jobs = jobs, force = isTRUE(args[["--force"]]))
  res <- list(schema_version = 1L, dry_run = dry, origin = format(origin, "%Y-%m-%d"),
              available_until = .prevcarga_tm_iso(rc$available_until), load_label = rc$load_label,
              weather_issue_cutoff = ctx$weather_issue_cutoff, cells = .prevcarga_tm_records(out))
  if (identical(ctx$format, "json")) res else .prevcarga_tm_text(res)
}

prevcarga_verb_train_models <- list(
  name    = "train-models",
  usage   = paste("prevcarga train-models [--model=<m>] [--area=<a>] [--subsistema=<s>] [--areas=<a,b>]",
                  "[--origin=<data>] [--horizon=<h>] [--exogena=<e>] [--dry-run] [--jobs=<n>] [--force]"),
  ## --model/--area/--origin ja constam no bloco Options global / de validate-deck (duplicata torna o docopt ambiguo)
  options = c(
    "--subsistema=<s>        Treina as folhas-filhas do subsistema (exclusivo com --area/--areas)",
    "--areas=<a,b>           Lista de areas separadas por virgula (exclusivo com --area/--subsistema)",
    "--horizon=<h>           Override de horizon_days do run.yaml (inteiro >= 1)",
    "--exogena=<e>           So membros com esta variante exogena",
    "--dry-run               Emite a matriz (cortes e versao-alvo) sem gravar nada",
    "--jobs=<n>              Workers PSOCK (padrao 1; sem fork)",
    "--force                 Retreina mesmo se a versao v_<hash> ja existe"),
  scoped  = c("--horizon", "--exogena"),   # so train-models; nos demais verbos => config.usage (cli.R .prevcarga_check_scoped)
  handler = function(ctx, args) cmd_train_models(ctx, args),
  owner   = "M5",
  exempt  = FALSE,
  modules = c("topology.R", "schema_long.R", "provenance.R", "deck_reader.R", "dates.R", "validation.R",
              "features.R", "registry.R", "training.R")
)
