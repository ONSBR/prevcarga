## scripts/lib/cli/registry.R - verbo auto-registrado `registry` (M5.7; descoberto por scripts/lib/cli.R, M1.4).
##
## Fachada fina: despacha o subcomando para as funcoes prevcarga_registry_* de registry.R (unico dono do ciclo de vida; gate
## registry_lifecycle). A lista de subcomandos e o usage vem de registry.R (carregado aqui so para ler esses dois valores).
## Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result` (a fachada o embrulha no
## envelope do ADR-0040). `--dry-run` ja consta no bloco Options de train-models (duplicata torna o docopt ambiguo).
## Requer errors.R, config.R, topology.R, provenance.R, registry.R.

.prevcarga_rg_def <- local({
  lib <- getOption("prevcarga.lib")
  if (is.null(lib)) lib <- file.path("scripts", "lib")
  env <- new.env(parent = globalenv())
  sys.source(file.path(lib, "registry.R"), envir = env)
  list(subs = env$prevcarga_registry_subcommands, usage = env$prevcarga_registry_usage)
})

## vetores de tamanho 1 viram array no JSON (auto_unbox); I() preserva a forma de lista
.prevcarga_rg_arr <- function(x) I(as.character(x))

.prevcarga_rg_ls_json <- function(rows)
  lapply(rows, function(r) list(key = r$key, latest = if (is.na(r$latest)) NULL else r$latest,
                                pinned = .prevcarga_rg_arr(r$pinned), versions = .prevcarga_rg_arr(r$versions)))

.prevcarga_rg_gc_json <- function(res) {
  k <- res$kept
  list(dry_run = res$dry_run, removed = .prevcarga_rg_arr(res$removed),
       kept = lapply(seq_len(nrow(k)), function(i) list(key = k$key[[i]], version = k$version[[i]], reason = k$reason[[i]])))
}

## O resultado se reconhece pela forma (lista sem nomes = ls; `removed` = poda; `model_id` = show; resto = ponteiro/pin).
.prevcarga_rg_text <- function(res) {
  if (is.null(names(res)))
    return(vapply(res, function(r)
      sprintf("%-32s latest=%s pinned=%s versions=%s", r$key, if (is.na(r$latest)) "-" else r$latest,
              if (length(r$pinned)) paste(r$pinned, collapse = ",") else "-", paste(r$versions, collapse = ",")), ""))
  if (!is.null(res$removed))
    return(c(sprintf("%s: %d removed, %d kept", if (isTRUE(res$dry_run)) "dry-run" else "done", length(res$removed), nrow(res$kept)),
             res$removed))
  if ("model_id" %in% names(res))
    return(unlist(lapply(c("version", "model_id", "load_label", "timezone", "weather_issue_cutoff", "code_commit"), function(k)
      sprintf("%s: %s", k, if (is.null(res[[k]])) "-" else res[[k]])), use.names = FALSE))
  sprintf("%s: %s", basename(res$key), if (!is.null(res$latest)) paste0("latest=", res$latest) else paste0("pinned=", paste(res$pinned, collapse = ",")))
}

.prevcarga_rg_json <- function(res) {
  if (is.null(names(res))) return(.prevcarga_rg_ls_json(res))
  if (!is.null(res$removed)) return(.prevcarga_rg_gc_json(res))
  if (!is.null(res$pinned)) res$pinned <- .prevcarga_rg_arr(res$pinned)
  res$key <- basename(res$key)
  res
}

cmd_registry <- function(ctx, args) {
  sub <- .prevcarga_rg_def$subs[vapply(.prevcarga_rg_def$subs, function(s) isTRUE(args[[s]]), logical(1))]
  if (length(sub) != 1L) prevcarga_abort("config.usage", where = "registry")
  res <- prevcarga_registry_run(sub, ctx, model = args[["<model>"]], area = args[["<area>"]],
                                version = args[["<version>"]], dry_run = isTRUE(args[["--dry-run"]]))
  if (identical(ctx$format, "json")) .prevcarga_rg_json(res) else .prevcarga_rg_text(res)
}

prevcarga_verb_registry <- list(
  name    = "registry",
  usage   = .prevcarga_rg_def$usage,
  options = character(),
  handler = function(ctx, args) cmd_registry(ctx, args),
  owner   = "M5",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "registry.R")
)
