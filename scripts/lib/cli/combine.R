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

## pack (0.1.8): junta o pacote anterior (--runs) e a arvore (--root) num pacote unico da area em --out
.prevcarga_cb_pack <- function(ctx, area, root, runs, out, members) {
  t0 <- prevcarga_t0()
  old <- if (is.null(runs)) NULL else prevcarga_combina_pack_read(runs, area, ctx, members)
  new <- if (is.null(root)) NULL else if (is.null(old)) prevcarga_combina_gather(root, area, ctx, members) else {
    pv <- attr(old, "provenance")
    prevcarga_combina_discover(root, area, ctx, members, skip = paste0(pv$member, "/", pv$run_id), allow_empty = TRUE)
  }
  long <- prevcarga_combina_pack_merge(old, new, members)
  prevcarga_assert_members_aligned(long, ctx)                     # pacote nunca carrega run que o train recusaria
  w <- prevcarga_combina_pack_write(long, out)
  prov <- attr(long, "provenance")
  prevcarga_say(ctx, "combine %s: pacote %s gravado: %d runs (%d do pacote anterior + %d da arvore), %d linhas, em %s", area,
                out, w$n_runs, if (is.null(old)) 0L else nrow(attr(old, "provenance")),
                if (is.null(new)) 0L else nrow(attr(new, "provenance")), w$n_rows, prevcarga_since(t0))
  origins <- sort(unique(prov$origin), method = "radix")
  list(mode = "pack", area = area, out_path = out, n_runs = as.integer(w$n_runs), n_rows = as.integer(w$n_rows),
       members_present = as.list(attr(long, "members_present")),
       n_runs_by_member = as.list(table(factor(prov$member, levels = attr(long, "members_present")))),
       origin_from = if (length(origins)) origins[[1L]] else NULL,
       origin_to = if (length(origins)) origins[[length(origins)]] else NULL)
}

## train --origin=<data> (0.1.9): o mes da origem e o mes alvo; o treino usa so origens ate o mes anterior
.prevcarga_cb_target_ym <- function(origin) {
  if (is.null(origin)) return(NULL)
  sprintf("%04d-%02d", lubridate::year(origin), lubridate::month(origin))
}

## dispatch train/apply/full; `warns` acumula os avisos de nao-estacionariedade (muffled aqui, emitidos 1x)
.prevcarga_cb_dispatch <- function(mode, area, panel, ctx, artifact_dir, t_cut, members) {
  cc <- ctx[["combina"]]                          # [[ ]]: `$` casaria parcialmente com ctx$combina_all_months
  wm <- if (is.null(cc[["window_months"]])) prevcarga_combina_defaults$window_months else cc[["window_months"]]
  switch(mode,
    train = {
      r <- prevcarga_combina_train(area, panel, artifact_dir, root = ctx$root, members = members, window_months = wm,
                                   say = function(fmt, ...) prevcarga_say(ctx, paste0("combine ", area, ": ", fmt), ...),
                                   only_next = !isTRUE(ctx$combina_all_months),   # 0.1.8: so o mes que o apply le
                                   target_ym = .prevcarga_cb_target_ym(ctx$origin))  # 0.1.9: --origin = mes alvo
      list(weights = r$cells, version_hash = r$version_hash, artifact_path = file.path(r$path, "weights.rds"), long = NULL,
           target_month = r$next_ym, latest = r$latest)
    },
    apply = {
      r <- prevcarga_combina_apply(area, artifact_dir, panel, ctx, t_cut = t_cut, members = members, detail = TRUE)
      list(weights = r$weights, version_hash = r$version_hash, artifact_path = r$path, long = r$long)
    },
    full = {
      r <- prevcarga_combina_full(panel, ctx, t_cut = t_cut, detail = TRUE, window_months = wm)
      list(weights = r$weights, version_hash = NULL, artifact_path = NULL, long = r$long)
    })
}

prevcarga_build_combine <- function(ctx, opts) {
  mode <- .prevcarga_cb_opt(opts, "--mode")
  if (is.null(mode) || !mode %in% c("train", "apply", "full", "pack"))
    prevcarga_abort("config.bad_mode", where = paste0("--mode=", if (is.null(mode)) "" else mode))
  area <- .prevcarga_cb_opt(opts, "--area")
  .prevcarga_score_areas(NULL, area, .prevcarga_score_topo(ctx))          # topology.area_undeclared antes de qualquer IO
  out <- .prevcarga_cb_opt(opts, "--out")
  if (mode %in% c("apply", "full", "pack")) {
    if (is.null(out)) prevcarga_abort("config.usage", where = "--out")
    if (!dir.exists(dirname(out))) prevcarga_abort("config.usage", where = paste0("--out=", out))
  }
  prevcarga_require_deck(ctx)
  root <- .prevcarga_cb_opt(opts, "--root")
  runs <- .prevcarga_cb_opt(opts, "--runs")
  if (identical(mode, "pack")) {                 # pack: --root (runs novas) e/ou --runs (pacote anterior) -> --out
    if (is.null(root) && is.null(runs))
      prevcarga_abort("config.usage", where = "--root", details = list(hint = "pack precisa de --root e/ou --runs"))
  } else {
    if (is.null(root) && is.null(runs)) prevcarga_abort("config.usage", where = "--root")
    if (!is.null(root) && !is.null(runs))
      prevcarga_abort("config.usage", where = "--runs", details = list(hint = "use --root (arvore) ou --runs (pacote da area), nao os dois"))
  }
  artifact_dir <- .prevcarga_cb_opt(opts, "--artifact-dir")
  if (is.null(artifact_dir)) artifact_dir <- file.path(prevcarga_registry_root(ctx), "combina")
  t_cut <- .prevcarga_cb_opt(opts, "--cut")
  if (isTRUE(opts[["--all-months"]]) && !identical(mode, "train"))
    prevcarga_abort("config.usage", where = "--all-months", details = list(hint = "--all-months so vale com --mode=train"))
  ctx$combina_all_months <- isTRUE(opts[["--all-months"]])
  ctx$tz <- .prevcarga_eval_tz(ctx)
  members <- prevcarga_members(root = ctx$root)
  if (identical(mode, "pack")) return(.prevcarga_cb_pack(ctx, area, root, runs, out, members))
  n_cap <- 0L
  t0 <- prevcarga_t0()
  prevcarga_say(ctx, "combine %s: inicio mode=%s %s cut=%s", area, mode,
                if (is.null(runs)) paste0("root=", root) else paste0("runs=", runs), if (is.null(t_cut)) "-" else t_cut)
  r <- withCallingHandlers({
    panel <- prevcarga_combina_load(root, area, ctx, members, runs = runs)
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
  prevcarga_say(ctx, "combine %s: fim mode=%s em %s (celulas fitadas=%d eqw=%d)%s", area, mode, prevcarga_since(t0),
                length(which(is.na(w$fallback))), length(which(!is.na(w$fallback))),
                if (is.null(out_path)) "" else paste0(" saida=", out_path))
  mbl <- prevcarga_combina_members_by_lead(w)
  if (n_cap > 0L) prevcarga_warn("warning.combina_nonstationary", details = list(cells = n_cap))
  list(mode = mode, area = area, version_hash = r$version_hash, artifact_path = r$artifact_path,
       out_path = out_path, target_month = r$target_month, latest_updated = r$latest,
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
  if (identical(res$mode, "pack"))
    return(c(sprintf("combine  mode=pack  area=%s  runs=%d  linhas=%d  origens=%s..%s", res$area, res$n_runs, res$n_rows,
                     dash(res$origin_from), dash(res$origin_to)),
             paste0("membros: ", paste(unlist(res$members_present), collapse = " ")),
             paste0("saida: ", res$out_path), "OK"))
  c(sprintf("combine  mode=%s  area=%s  version=%s", res$mode, res$area, dash(res$version_hash)),
    sprintf("celulas: fitadas=%d eqw=%d nao-estacionarias=%d", res$n_cells_fitted, res$n_cells_eqw,
            res$n_cells_nonstationary),
    paste0("membros: ", paste(unlist(res$members_present), collapse = " ")),
    if (!is.null(res$target_month))
      sprintf("mes alvo: %s (latest.txt %s)", res$target_month, if (isTRUE(res$latest_updated)) "atualizado" else "mantido"),
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
  usage   = "prevcarga combine --mode=<mode> --area=<a> [--root=<dir>] [--runs=<file>] [--artifact-dir=<dir>] [--all-months] [--cut=<HHMM>] [--out=<path>] [--origin=<date>]",
  ## --area=<a> ja e declarada no bloco Options por validate-deck (a duplicata torna o docopt ambiguo); a usage a exige
  options = c(
    "--mode=<mode>          train | apply | full | pack (validado pelo handler; sem default)",
    "--root=<dir>           root dos runs de membro: <dir>/<mdir>/<area>/<run_id>/{long.parquet,run_manifest.json}",
    ## sem --runs, <root>/<AREA>.runs.parquet (se existir) tem prioridade e a arvore completa o que faltar
    "--runs=<file>          pacote da area (1 parquet com todas as runs; gerado por --mode=pack), no lugar de --root",
    "--all-months           train: ajusta todos os meses do painel (como ate a 0.1.7); sem ela, so o mes seguinte",
    "--artifact-dir=<dir>   pesos content-addressed (padrao <raiz de artefatos>/combina (--data-artifacts ou <state_dir>/artifacts), resolvido no handler)",
    "--cut=<HHMM>           corte HH:MM do available_until a combinar",
    "--out=<path>           LONG de saida (parquet) em apply/full"),
  handler = function(ctx, args) cmd_combine(ctx, args),
  deck_roles = function(args) {
    mode <- args[["--mode"]]
    if (isTRUE(any(mode %in% c("train", "full", "pack")))) "train" else c("train", "inference")
  },
  owner   = "M4",
  exempt  = FALSE,
  modules = c("topology.R", "schema_long.R", "provenance.R", "deck_reader.R", "dates.R",
              "evaluation.R", "features.R", "registry.R", "combination.R")
)
