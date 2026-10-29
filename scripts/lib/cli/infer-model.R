## scripts/lib/cli/infer-model.R - verbo auto-registrado `infer-model` (M5.5; descoberto por scripts/lib/cli.R, M1.4).
##
## 1 modelo x 1 area x 1 origem. Params 100% do artefato congelado: o bloco Usage NAO declara --horizon/--exogena (o
## docopt recusa => exit 8). A logica mora em inference.R (prevcarga_infer_cell); este arquivo e o handler fino.
## Erros e exits vem do catalogo (ADR-0040) pelo handler de topo da fachada: sem quit(), sem cat de erro, sem env vars.
## Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result` (a fachada o embrulha).
## --model e --origin sao flags globais; --area e --out ja constam do bloco Options de validate-deck/combine (a duplicata
## torna o docopt ambiguo), a usage os exige. O LONG de 8 colunas e do M6.
## Requer errors.R, config.R (da fachada) e topology.R, provenance.R, deck_reader.R, dates.R, features.R, registry.R, inference.R.

.prevcarga_im_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

## result do envelope: o que e interno (available_until_posix) sai; predictions vira registros com datas ISO.
.prevcarga_im_result <- function(res) {
  tz <- attr(res$predictions$target_datetime, "tzone")
  iso <- vapply(seq_len(nrow(res$predictions)), function(i)
    .prevcarga_inf_iso(structure(res$predictions$target_datetime[[i]], tzone = tz)), "")
  out <- res[setdiff(names(res), c("predictions", "available_until_posix"))]
  out$predictions <- data.frame(target_datetime = iso, y_pred = res$predictions$y_pred, stringsAsFactors = FALSE)
  out
}

.prevcarga_im_text <- function(r) {
  head <- sprintf("infer-model model=%s%s area=%s origin=%s version=%s resolved_by=%s",
                  r$model, if (is.null(r$variant)) "" else paste0("/", r$variant), r$area, r$origin, r$version, r$resolved_by)
  c(head, sprintf("available_until=%s first_target=%s", r$available_until, r$first_target), "",
    "TARGET_DATETIME           Y_PRED",
    sprintf("%-25s %s", r$predictions$target_datetime, formatC(r$predictions$y_pred, format = "f", digits = 3)))
}

cmd_infer_model <- function(ctx, args) {
  model <- .prevcarga_im_opt(args, "--model")
  area <- .prevcarga_im_opt(args, "--area")
  if (is.null(model)) prevcarga_abort("config.usage", where = "--model")
  if (is.null(area)) prevcarga_abort("config.usage", where = "--area")
  prevcarga_model_spec(model, root = ctx$root)                       # config.unknown_model antes de qualquer IO
  prevcarga_require_data_dir(ctx)
  out <- .prevcarga_im_opt(args, "--out")
  if (!is.null(out) && !dir.exists(dirname(out)))
    prevcarga_abort("io.out_unwritable", where = out, details = list(reason = "diretorio ausente"))
  member <- list(sigla = model, mdir = model, exogena = NA_character_)
  res <- prevcarga_infer_cell(member, area, ctx$origin, ctx,
                              variant = .prevcarga_im_opt(args, "--variant"),
                              version = .prevcarga_im_opt(args, "--artifact-version"),
                              pin = isTRUE(args[["--pin"]]))
  if (!is.null(out)) prevcarga_infer_write_out(res, out, ctx)
  r <- .prevcarga_im_result(res)
  if (identical(ctx$format, "json")) r else .prevcarga_im_text(r)
}

prevcarga_verb_infer_model <- list(
  name    = "infer-model",
  usage   = "prevcarga infer-model --model=<m> --area=<a> [--variant=<e>] [--artifact-version=<v> | --pin] [--out=<arquivo>]",
  options = c(
    "--variant=<e>           EXOGENA do membro (obrigatoria se o membro declara exogena)",
    "--artifact-version=<v>  v_<hash> explicito (senao latest.txt)",
    "--pin                   Usa a versao pinada (.pin) da celula"),
  handler = function(ctx, args) cmd_infer_model(ctx, args),
  owner   = "M5",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "deck_reader.R", "dates.R", "features.R", "registry.R", "inference.R")
)
