## scripts/lib/cli/deck-inference.R - verbo auto-registrado `deck-inference` (E2-T4; descoberto por scripts/lib/cli.R, barrel por descoberta).
##
## Recorta o deck de treino (--data-train) num deck kind `inference` minimo e deterministico para a origem (--origin), guiado pelas
## necessidades gravadas nos artefatos (--data-artifacts). Handler FINO sobre prevcarga_deck_inference_build (deck_inference.R):
## valida --out/--origin, resolve as areas pela topologia e repassa os avisos. --origin, --area e --out ja constam das opcoes globais
## e do bloco Options de outros verbos (a duplicata torna o docopt ambiguo), entao so a usage os cita; --from-topology e novo.
## Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result` (a fachada o embrulha). Sem env vars.
## Requer errors.R, config.R (da fachada) e topology.R ... deck_inference.R (lista em `modules`).

.prevcarga_dki_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || all(is.na(v))) NULL else as.character(v)
}

## --area (folha ou subsistema, que expande para as folhas); sem --area => todas as folhas da topologia. Fora da topologia =>
## topology.area_undeclared (exit 4).
.prevcarga_dki_areas <- function(ctx, area) {
  topo <- if (!is.null(ctx$topology_path)) prevcarga_topology(path = ctx$topology_path, root = ctx$root)
          else prevcarga_topology(root = ctx$root)
  if (!length(area)) return(topo$leaves)
  out <- unlist(lapply(unique(area), function(a) {
    if (a %in% topo$leaves) a
    else if (a %in% names(topo$subsystems)) topo$subsystems[[a]]$areas
    else prevcarga_abort("topology.area_undeclared", where = a)
  }), use.names = FALSE)
  topo$leaves[topo$leaves %in% out]
}

.prevcarga_dki_text <- function(r) {
  area_of <- function(p) sub("^.*area_code=([^/]+)/.*$", "\\1", p)
  rows_of <- function(rel, a) sum(vapply(r$files, function(f)
    if (startsWith(f$path, paste0(rel, "/")) && identical(area_of(f$path), a) && !is.null(f$rows)) f$rows else 0L, 0L))
  rel <- c(load = "load", weather_observed = "weather/observed", weather_forecast = "weather/forecast", holidays = "auxiliary/holidays")
  tab <- unlist(lapply(names(r$windows), function(ds) lapply(sort(names(r$windows[[ds]]), method = "radix"), function(a) {
    w <- r$windows[[ds]][[a]]
    sprintf("%-18s %-6s %s %s %d", ds, a, w$from, w$to, rows_of(rel[[ds]], a))
  })), use.names = FALSE)
  c(sprintf("deck-inference origin=%s available_until=%s needs=%s", r$origin, r$available_until, r$inference_needs_source),
    paste0("out: ", r$out),
    sprintf("train_deck_hash=%s data_hash=%s", substr(r$train_deck_hash, 1L, 12L), substr(r$data_hash, 1L, 12L)),
    sprintf("%-18s %-6s %-10s %-10s %s", "DATASET", "AREA", "FROM", "TO", "ROWS"), tab,
    sprintf("arquivos=%d", length(r$files)), "OK")
}

cmd_deck_inference <- function(ctx, args) {
  out <- .prevcarga_dki_opt(args, "--out")
  if (is.null(out)) prevcarga_abort("config.usage", where = "--out")
  if (is.null(ctx$origin)) prevcarga_abort("config.usage", where = "--origin")
  prevcarga_deck_inference_check_out(out[[1L]])            # contract.deck_out_not_empty antes de qualquer leitura do deck
  prevcarga_require_deck(ctx)
  areas <- .prevcarga_dki_areas(ctx, .prevcarga_dki_opt(args, "--area"))
  b <- prevcarga_deck_inference_build(ctx, ctx$origin, areas, out[[1L]], isTRUE(args[["--from-topology"]]))
  for (w in b$warnings) prevcarga_warn(w$id, where = w$where, details = w$details)
  if (identical(ctx$format, "json")) b$result else .prevcarga_dki_text(b$result)
}

prevcarga_verb_deck_inference <- list(
  name    = "deck-inference",
  usage   = "prevcarga deck-inference [--area=<a>...] [--from-topology] --out=<dir>",
  options = "--from-topology         Necessidades pela topologia, sem ler artefatos",
  handler = function(ctx, args) cmd_deck_inference(ctx, args),
  deck_roles = "train",
  owner   = "E2",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "holiday_islands.R", "deck_reader.R", "dates.R", "validation.R", "features.R", "registry.R",
              "inference.R", "inference_needs.R", "deck_inference.R")
)
