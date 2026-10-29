## scripts/lib/cli/combine.R - verbo auto-registrado `combine` (M4.4; descoberto por scripts/lib/cli.R, M1.4).
##
## Liga a combina (combination.R, M4.1-M4.3) a fachada. Handler FINO: valida flags ANTES de qualquer IO, monta o
## painel por prevcarga_combina_load e despacha train/apply/full; nenhuma conta numerica aqui (a logica mora em
## combination.R). Erros e exits vem do catalogo (ADR-0040) pelo handler de topo da fachada: sem quit(), sem cat de
## erro, sem env vars. Convencao da fachada: handler(ctx, args); texto devolve linhas ASCII, JSON devolve o `result`
## (a fachada o embrulha no envelope). Aviso de celula nao-estacionaria = prevcarga_warn (stderr; exit 0).
## O encadeamento no `forecast` e o flip de jobs/run_reduce.sh sao do M6.
## Requer errors.R e config.R (ja da fachada) e topology.R, schema_long.R, provenance.R, deck_reader.R, evaluation.R, features.R,
## registry.R, combination.R.

.prevcarga_cb_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

## escrita atomica: parquet em tmp irmao + file.rename (nenhum temporario fica ao lado do --out)
.prevcarga_write_parquet_atomic <- function(long, out) {
  tmp <- tempfile(".combine-", tmpdir = dirname(out), fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  arrow::write_parquet(as.data.frame(long), tmp)
  if (!file.rename(tmp, out)) prevcarga_abort("config.usage", where = paste0("--out=", out))
  invisible(out)
}

## --origin (unico flag de data) restringe as linhas de saida do apply/full; origem sem linhas = uso invalido
.prevcarga_cb_restrict_origin <- function(long, origin) {
  if (is.null(origin)) return(long)
  fo <- lubridate::ymd(long$forecast_origin, quiet = TRUE)
  keep <- if (is.list(origin)) fo >= origin$from & fo <= origin$to else fo == origin
  if (!any(keep))
    prevcarga_abort("config.usage", where = "--origin", details = list(hint = "nenhuma origem do resultado casa com --origin"))
  long[keep]
}

.prevcarga_cb_uniq <- function(panel, key) {
  prov <- attr(panel, "provenance")
  v <- if (is.null(prov) || !key %in% names(prov)) character() else sort(unique(stats::na.omit(as.character(prov[[key]]))))
  if (length(v)) paste(v, collapse = ",") else NULL
}

## tabela members_by_lead -> lista de registros JSON-friendly
.prevcarga_cb_mbl <- function(mbl) {
  lapply(seq_len(nrow(mbl)), function(i)
    list(lead = as.integer(mbl$lead[[i]]), cut = as.character(mbl$cut[[i]]),
         members = as.list(mbl$members[[i]]), n_members = as.integer(mbl$n_members[[i]])))
}

## dispatch train/apply/full; `warns` acumula os avisos de nao-estacionariedade (muffled aqui, emitidos 1x)
.prevcarga_cb_dispatch <- function(mode, area, panel, ctx, artifact_dir, t_cut, members) {
  switch(mode,
    train = {
      r <- prevcarga_combina_train(area, panel, artifact_dir, root = ctx$root, members = members)
      list(weights = r$cells, version_hash = r$version_hash, artifact_path = file.path(r$path, "weights.rds"), long = NULL)
    },
    apply = {
      r <- prevcarga_combina_apply(area, artifact_dir, panel, ctx, t_cut = t_cut, members = members, detail = TRUE)
      list(weights = r$weights, version_hash = r$version_hash, artifact_path = r$path, long = r$long)
    },
    full = {
      r <- prevcarga_combina_full(panel, ctx, t_cut = t_cut, detail = TRUE)
      list(weights = r$weights, version_hash = NULL, artifact_path = NULL, long = r$long)
    })
}

prevcarga_build_combine <- function(ctx, opts) {
  mode <- .prevcarga_cb_opt(opts, "--mode")
  if (is.null(mode) || !mode %in% c("train", "apply", "full"))
    prevcarga_abort("config.bad_mode", where = paste0("--mode=", if (is.null(mode)) "" else mode))
  area <- .prevcarga_cb_opt(opts, "--area")
  .prevcarga_score_areas(NULL, area, .prevcarga_score_topo(ctx))          # topology.area_undeclared antes de qualquer IO
  out <- .prevcarga_cb_opt(opts, "--out")
  if (mode %in% c("apply", "full")) {
    if (is.null(out)) prevcarga_abort("config.usage", where = "--out")
    if (!dir.exists(dirname(out))) prevcarga_abort("config.usage", where = paste0("--out=", out))
  }
  prevcarga_require_data_dir(ctx)
  root <- .prevcarga_cb_opt(opts, "--root")
  if (is.null(root)) prevcarga_abort("config.usage", where = "--root")
  artifact_dir <- .prevcarga_cb_opt(opts, "--artifact-dir")
  if (is.null(artifact_dir)) artifact_dir <- file.path(ctx$state_dir, "artifacts", "combina")
  t_cut <- .prevcarga_cb_opt(opts, "--cut")
  ctx$tz <- .prevcarga_eval_tz(ctx)
  members <- prevcarga_members(root = ctx$root)
  n_cap <- 0L
  r <- withCallingHandlers({
    panel <- prevcarga_combina_load(root, area, ctx, members)
    if (!is.null(t_cut) && identical(mode, "train")) panel <- .prevcarga_cmb_select_cut(panel, t_cut)
    .prevcarga_cb_dispatch(mode, area, panel, ctx, artifact_dir, t_cut, members)
  }, warning = function(w) {
    if (!inherits(w, "prevcarga_warning") && grepl("celula nao-estacionaria", conditionMessage(w), fixed = TRUE)) {
      n_cap <<- n_cap + 1L
      invokeRestart("muffleWarning")
    }
  })
  out_path <- NULL
  if (!is.null(r$long)) {
    .prevcarga_write_parquet_atomic(.prevcarga_cb_restrict_origin(r$long, ctx$origin), out)
    out_path <- out
  }
  w <- r$weights
  mbl <- prevcarga_combina_members_by_lead(w)
  if (n_cap > 0L) prevcarga_warn("warning.combina_nonstationary", details = list(cells = n_cap))
  list(mode = mode, area = area, version_hash = r$version_hash, artifact_path = r$artifact_path,
       out_path = out_path,
       n_cells_fitted = as.integer(length(which(is.na(w$fallback)))),
       n_cells_eqw = as.integer(length(which(!is.na(w$fallback)))),
       n_cells_nonstationary = as.integer(length(which(w$nonstationary %in% TRUE))),
       members_present = as.list(attr(panel, "members_present")),
       members_by_lead = .prevcarga_cb_mbl(mbl),
       load_label = .prevcarga_cb_uniq(panel, "load_label"), timezone = .prevcarga_cb_uniq(panel, "load_tz"),
       weather_issue_cutoff = .prevcarga_cb_uniq(panel, "weather_issue_cutoff"),
       cuts = paste(sort(unique(as.character(panel$cut)), method = "radix"), collapse = ","))
}

## --- render texto ASCII (so formata campos do result) ----------------------------------------------------------
.render_combine_text <- function(res) {
  dash <- function(x) if (is.null(x)) "-" else as.character(x)
  c(sprintf("combine  mode=%s  area=%s  version=%s", res$mode, res$area, dash(res$version_hash)),
    sprintf("celulas: fitadas=%d eqw=%d nao-estacionarias=%d", res$n_cells_fitted, res$n_cells_eqw,
            res$n_cells_nonstationary),
    paste0("membros: ", paste(unlist(res$members_present), collapse = " ")),
    if (!is.null(res$artifact_path)) paste0("artefato: ", res$artifact_path),
    if (!is.null(res$out_path)) paste0("saida: ", res$out_path),
    "OK")
}

## --- Handler fino ----------------------------------------------------------------------------------------------
cmd_combine <- function(ctx, args) {
  res <- prevcarga_build_combine(ctx, args)
  if (identical(ctx$format, "json")) res else .render_combine_text(res)
}

prevcarga_verb_combine <- list(
  name    = "combine",
  usage   = "prevcarga combine --mode=<mode> --area=<a> --root=<dir> [--artifact-dir=<dir>] [--cut=<HHMM>] [--out=<path>] [--origin=<date>]",
  ## --area=<a> ja e declarada no bloco Options por validate-deck (a duplicata torna o docopt ambiguo); a usage a exige
  options = c(
    "--mode=<mode>          train | apply | full (validado pelo handler; sem default)",
    "--root=<dir>           root dos runs de membro (_infer_c<HHMM>)",
    "--artifact-dir=<dir>   pesos content-addressed (padrao <state_dir>/artifacts/combina, resolvido no handler)",
    "--cut=<HHMM>           corte HH:MM do available_until a combinar",
    "--out=<path>           LONG de saida (parquet) em apply/full"),
  handler = function(ctx, args) cmd_combine(ctx, args),
  owner   = "M4",
  exempt  = FALSE,
  modules = c("topology.R", "schema_long.R", "provenance.R", "deck_reader.R", "dates.R",
              "evaluation.R", "features.R", "registry.R", "combination.R")
)
