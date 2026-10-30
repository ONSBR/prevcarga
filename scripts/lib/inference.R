## inference.R -- fachada de inferencia de UMA celula modelo x variante x area x origem (M5.5; ADR-0023, ADR-0039, ADR-0040)
##
## prevcarga_infer_cell compoe, sem calcular corte nem hora: plugin (registry.R) -> artefato (registry core M5.2:
## prevcarga_resolve_version/prevcarga_load_artifact; integridade SHA256SUMS -> exit 9) -> spec DO ARTEFATO
## (artifact$spec_efetivo; nunca o run.yaml atual nem flag de param) -> painel pelo leitor unico (deck_reader.R) -> cortes
## (dates.R: prevcarga_resolve_cuts + prevcarga_train_cut/prevcarga_first_delivery) -> despachante
## prevcarga_plugin_infer (P.0c; HW/RF/LGBM so leem o artefato, dessem refaz a cabeca do dia O dentro do plugin).
## Esta fachada NUNCA chama plugin_train/prevcarga_train e NUNCA escreve no registry (so le). O M5 nao soma nem subtrai
## dias de corte nem faz aritmetica de hora: os cortes vem do dates.R, o deslocamento de modelagem e do despachante.
## O LONG de 8 colunas e do M6; aqui a saida e (model, variant, area, origin, available_until, target_datetime, y_pred).
## Requer errors.R, topology.R, provenance.R, deck_reader.R, dates.R, features.R e registry.R carregados antes.

.prevcarga_inf_or <- function(x, y) if (is.null(x) || !length(x)) y else x

.prevcarga_inf_variant <- function(member) {
  exo <- member$exogena
  if (is.null(exo) || length(exo) != 1L || is.na(exo) || !nzchar(exo)) NA_character_ else as.character(exo)
}

.prevcarga_inf_iso <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return(NULL)
  sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", format(x[[1L]], "%Y-%m-%dT%H:%M:%S%z", tz = attr(x, "tzone")))
}

## --- Resolucao da celula ---------------------------------------------------------------------------------------
## A variante e exigida quando o membro declara EXOGENA na topologia (qualquer linha do mdir) ou quando o descriptor
## do plugin declara `variant_param` (quem chama ja carregou o plugin). Sem topologia legivel decide so o descriptor.
.prevcarga_inf_topology_exogenas <- function(member, ctx) {
  mem <- tryCatch(
    if (is.null(ctx$topology_path)) prevcarga_members(root = ctx$root) else
      prevcarga_members(path = ctx$topology_path, root = ctx$root),
    prevcarga_error = function(e) NULL)
  if (is.null(mem)) return(character())
  ex <- mem$exogena[mem$mdir == member$mdir]
  ex[!is.na(ex) & nzchar(ex)]
}

prevcarga_infer_resolve <- function(member, area, ctx, variant = NULL, version = NULL, pin = FALSE,
                                    needs_variant = FALSE) {
  if (!is.null(variant) && (!is.character(variant) || length(variant) != 1L || is.na(variant) || !nzchar(variant)))
    prevcarga_abort("config.usage", where = "--variant")
  declared <- .prevcarga_inf_variant(member)
  if (!is.null(variant) && !is.na(declared) && !identical(declared, variant))
    prevcarga_abort("config.usage", where = "--variant",
                    details = list(hint = "a variante do flag diverge do exogena do membro"))
  topo <- .prevcarga_inf_topology_exogenas(member, ctx)
  needs <- isTRUE(needs_variant) || length(topo) > 0L
  chosen <- if (!is.null(variant)) variant else declared
  if (needs && is.na(chosen))
    prevcarga_abort("contract.variant_required", where = member$mdir,
                    details = list(model = member$mdir, area = area))
  if (!needs && !is.na(chosen))
    prevcarga_abort("config.usage", where = "--variant",
                    details = list(hint = "o modelo nao declara variante (exogena)"))
  if (length(topo) && !(chosen %in% topo))
    prevcarga_abort("config.usage", where = "--variant",
                    details = list(hint = "variante fora das exogenas declaradas na topologia", variants = as.list(topo)))
  member$exogena <- chosen
  key <- prevcarga_registry_key(member, area, ctx)
  mode <- if (!is.null(version)) "version" else if (isTRUE(pin)) "pin" else "latest"
  v <- prevcarga_resolve_version(key, mode, version)
  list(artifact = prevcarga_load_artifact(key, v), version = v, resolved_by = mode, key = key, member = member)
}

## --- Fachada sobre o despachante ---------------------------------------------------------------------------------
## `artifact` = artefato publicado (prevcarga_train_artifact, M5.3). O spec vem SEMPRE dele; `spec` (opcional) so e
## conferido: diferente do efetivo congelado => contract.infer_spec_divergent. A janela e history_days$infer do artefato.
prevcarga_infer <- function(plugin, artifact, panel, cuts, ctx = list(), spec = NULL) {
  frozen <- artifact$spec_efetivo
  if (is.null(frozen) || is.null(artifact$fit))
    prevcarga_abort("contract.artifact_spec_missing", where = plugin$descriptor$model_id,
                    details = list(missing = c(if (is.null(frozen)) "spec_efetivo", if (is.null(artifact$fit)) "fit")))
  if (!is.null(spec) && !identical(spec, frozen))
    prevcarga_abort("contract.infer_spec_divergent", where = plugin$descriptor$model_id,
                    details = list(model = plugin$descriptor$model_id))
  plugin$spec <- frozen          # INTERINO (F1): o despachante de infer nao recebe `spec`; ver docs/estado/inference.md
  tz <- cuts$tz
  since <- .prevcarga_inf_origin(cuts) - as.integer(frozen$history_days$infer)
  day_of <- function(x) lubridate::as_date(x, tz = tz)
  win <- panel
  win$load <- panel$load[day_of(panel$load$interval_start) >= since]
  if (NROW(panel$weather_observed))
    win$weather_observed <- panel$weather_observed[day_of(panel$weather_observed$valid_time) >= since]
  env <- prevcarga_plugin_infer(plugin, artifact$fit, win, cuts, ctx)
  data.table::data.table(target_datetime = env$delivery$interval_end, y_pred = env$delivery$y_pred)
}

.prevcarga_inf_origin <- function(cuts) if (!is.null(cuts$origin_day)) cuts$origin_day else cuts$forecast_origin

## --- Celula ---------------------------------------------------------------------------------------------------
## Cortes da celula: linha do dates.R com o data_lag_days DO descriptor (default 0), conferida; train_origin/last_data_day
## pelo prevcarga_train_cut do spec do artefato. Devolve a lista `cuts` do despachante.
.prevcarga_inf_cuts <- function(plugin, spec, origin, ctx, mdir) {
  d <- plugin$descriptor
  lag <- as.integer(.prevcarga_inf_or(d$data_lag_days, 0L))
  rc <- prevcarga_resolve_cuts(ctx, origin, model = mdir, data_lag_days = stats::setNames(lag, mdir))
  row <- rc$cuts[rc$cuts$model == mdir, ]
  if (nrow(row) != 1L || !identical(as.integer(row$data_lag_days), lag))
    prevcarga_abort("contract.cut_mismatch", where = mdir,
      details = list(member = mdir, cut_data_lag_days = as.integer(row$data_lag_days), descriptor_data_lag_days = lag))
  tc <- prevcarga_train_cut(spec, origin, row$data_lag_days)
  list(forecast_origin = origin, origin_day = origin, origin = origin, tz = rc$tz, load_label = rc$load_label,
       available_until = rc$available_until, weather_issue_cutoff = rc$weather_issue_cutoff,
       weather_issue_limit = rc$weather_issue_limit, split = spec$split, train_origin = tc$train_origin,
       last_data_day = tc$last_data_day, data_lag_days = as.integer(row$data_lag_days),
       deck_areas = intersect(as.character(rc$available_until_by_area$area_code),
                              (if (is.null(ctx$topology_path)) prevcarga_topology(root = ctx$root)
                               else prevcarga_topology(path = ctx$topology_path, root = ctx$root))$leaves))
}

## O available_until e o MINIMO entre as areas do deck (M2.2): o painel e lido com todas as areas do deck para que o corte
## seja o global e a area pedida nao veja carga alem dele; depois so se SELECIONA a area (nenhum corte e calculado aqui).
.prevcarga_inf_select_area <- function(panel, area) {
  pick <- function(x) {
    if (!is.data.frame(x) || !"area_code" %in% names(x)) return(x)
    keep <- attributes(x)[setdiff(names(attributes(x)), c("names", "row.names", "class", ".internal.selfref", "sorted", "index"))]
    out <- x[x$area_code == area, ]
    for (a in names(keep)) data.table::setattr(out, a, keep[[a]])
    out
  }
  for (nm in c("load", "weather_observed", "weather_forecast", "holidays")) panel[[nm]] <- pick(panel[[nm]])
  panel
}

## O artefato nao pode ter visto dado alem do teto desta origem: origin_scoped => last_data_day do treino da MESMA regra
## (dates.R, a partir da origem do artefato); origin_agnostic => nunca depois do last_data_day desta origem.
.prevcarga_inf_check_artifact_cut <- function(artifact, spec, cuts, mdir) {
  alag <- cuts$data_lag_days
  want <- if (identical(spec$split, "origin_scoped") && !is.null(artifact$origin))
    prevcarga_train_cut(spec, artifact$origin, alag)$last_data_day else NULL
  got <- artifact$cuts$last_data_day
  bad <- if (!inherits(got, "Date") || length(got) != 1L || is.na(got)) TRUE
         else if (!is.null(want)) !identical(got, want) else got > cuts$last_data_day
  if (bad)
    prevcarga_abort("contract.cut_mismatch", where = mdir,
      details = list(member = mdir, artifact_last_data_day = if (inherits(got, "Date")) format(got) else NA_character_,
                     data_lag_days = alag))
  invisible(TRUE)
}

## --- Conferencias de convencao e de alinhamento (M5.6; ADR-0023, ADR-0024, ADR-0032, ADR-0040) ------------------------
## Tabela de conferencias artefato x efetivo; `available_until` nunca entra (o mesmo artefato serve todas as rodadas de O).
.prevcarga_inf_checks <- list(
  list(field = "load_label",           eff = function(c, x) x$load_label,           id = "contract.load_label_mismatch"),
  list(field = "timezone",             eff = function(c, x) c$tz,                   id = "contract.timezone_mismatch"),
  list(field = "weather_issue_cutoff", eff = function(c, x) c$weather_issue_cutoff, id = "contract.weather_cutoff_mismatch",
       when = "isTRUE(descriptor$weather_forecast_in_train)"),
  list(field = "origin",               eff = function(c, x) c$origin,               id = "contract.origin_mismatch",
       when = "identical(spec$split, 'origin_scoped')"))

## Todas as divergencias acumulam e abortam uma vez (exit 2); o id do aborto e o da 1a divergencia da tabela.
prevcarga_infer_check_conventions <- function(artifact, cuts, ctx, descriptor) {
  spec <- artifact$spec_efetivo
  gaps <- list()
  for (ck in .prevcarga_inf_checks) {
    if (!is.null(ck$when) && !isTRUE(eval(parse(text = ck$when)))) next
    got <- format(artifact[[ck$field]])
    eff <- format(ck$eff(cuts, ctx))
    if (!identical(got, eff)) gaps[[ck$id]] <- list(convencao = ck$field, artefato = got, efetivo = eff)
  }
  if (length(gaps))
    prevcarga_abort(names(gaps)[[1L]], where = descriptor$model_id, details = gaps)
  invisible(TRUE)
}

## O painel ja chega do leitor unico cortado em available_until (nenhum corte e calculado aqui): so confere que nada
## passou do corte. Segue CRU (30 min): o despachante reamostra por granularity_min/weather_resample (FIX 7).
prevcarga_infer_panel <- function(panel, cuts, spec) {
  ie <- panel$load$interval_end
  if (length(ie) && any(ie > cuts$available_until))
    prevcarga_abort("contract.panel_beyond_cut", where = "prevcarga_infer_panel",
                    details = list(available_until = format(cuts$available_until, "%Y-%m-%d %H:%M")))
  panel
}

## A entrega tem que ser identica a grade de entrega: 1o = available_until + 30, passo 30, sem buraco, ate O + delivery_days.
prevcarga_infer_check_alignment <- function(pred, cuts, descriptor) {
  esperado <- prevcarga_delivery_grid(cuts, descriptor$delivery_days)
  got <- pred$target_datetime
  if (!identical(got, esperado)) {
    rng <- function(x) if (length(x)) format(range(x), "%Y-%m-%d %H:%M") else c(NA_character_, NA_character_)
    prevcarga_abort("contract.output_misaligned", where = descriptor$model_id,
      details = list(esperado = rng(esperado), obtido = rng(got), n_esperado = length(esperado), n_obtido = length(got)))
  }
  invisible(TRUE)
}

## Painel de infer da celula pelo leitor unico (unica montagem; a celula e os testes a reusam): le com todas as areas do
## deck (corte global, M2.2), adota available_until/weather_issue_limit do painel, seleciona a area e, para plugin com
## `weather_forecast_in_train`, troca o previsto por TODAS as emissoes <= limite em [O - history_days.infer, O + H] (o mesmo
## insumo do treino, training.R). Devolve list(panel, cuts). Nenhum corte e calculado aqui.
prevcarga_infer_read_panel <- function(plugin, spec, cuts, area, origin, ctx, truncate_at = NULL) {
  panel <- prevcarga_read_panel(ctx, origin, union(cuts$deck_areas, area), spec$horizon_days, spec$history_days$infer,
                                truncate_at = truncate_at)
  au <- panel$attrs$available_until
  cuts$available_until <- if (inherits(au, "POSIXct") && length(au) == 1L && !is.na(au)) au else cuts$available_until
  wl <- panel$attrs$weather_issue_limit
  if (inherits(wl, "POSIXct") && length(wl) == 1L && !is.na(wl)) cuts$weather_issue_limit <- wl
  panel <- .prevcarga_inf_select_area(panel, area)
  if (isTRUE(plugin$descriptor$weather_forecast_in_train))
    panel$weather_forecast <- prevcarga_read_forecast_vintages(ctx, area, origin - spec$history_days$infer,
                                                               origin + spec$horizon_days, cuts$weather_issue_limit)
  list(panel = prevcarga_infer_panel(panel, cuts, spec), cuts = cuts)
}

## `artifact` (M7.3, backtest): artefato em memoria (do cache local), no lugar da resolucao do registry; `truncate_at`: corte
## intradiario da celula, repassado AO LEITOR (o available_until continua derivado do dado, nunca calculado aqui).
prevcarga_infer_cell <- function(member, area, origin, ctx, variant = NULL, version = NULL, pin = FALSE,
                                 artifact = NULL, truncate_at = NULL) {
  if (is.null(origin)) origin <- prevcarga_resolve_origin(ctx)$origin
  origin <- lubridate::as_date(origin)
  if (is.null(ctx$weather_issue_cutoff)) ctx$weather_issue_cutoff <- "available_until"
  plugin <- prevcarga_plugin_load(member$mdir, prevcarga_models_root(ctx$root))
  prevcarga_plugin_check_signature(plugin)
  vp <- plugin$descriptor$variant_param
  needs <- is.character(vp) && length(vp) == 1L && !is.na(vp)
  res <- if (is.null(artifact)) prevcarga_infer_resolve(member, area, ctx, variant, version, pin, needs_variant = needs)
         else list(artifact = artifact, version = NA_character_, resolved_by = "memory", member = member)
  member <- res$member
  art <- res$artifact
  spec <- art$spec_efetivo
  want_variant <- .prevcarga_inf_variant(member)
  got_variant <- .prevcarga_inf_variant(list(exogena = art$variant))
  if (!identical(got_variant, want_variant))
    prevcarga_abort("contract.variant_mismatch", where = member$mdir,
      details = list(artifact_variant = got_variant, member_variant = want_variant))

  cuts <- .prevcarga_inf_cuts(plugin, spec, origin, ctx, member$mdir)
  prevcarga_infer_check_conventions(art, cuts, list(load_label = cuts$load_label), plugin$descriptor)
  .prevcarga_inf_check_artifact_cut(art, spec, cuts, member$mdir)
  rp <- prevcarga_infer_read_panel(plugin, spec, cuts, area, origin, ctx, truncate_at = truncate_at)
  panel <- rp$panel
  cuts <- rp$cuts

  pred <- prevcarga_infer(plugin, art, panel, cuts, list(), spec = NULL)
  prevcarga_infer_check_alignment(pred, cuts, plugin$descriptor)
  list(model = member$mdir, variant = if (is.na(want_variant)) NULL else want_variant, area = area,
       origin = format(origin, "%Y-%m-%d"), available_until = .prevcarga_inf_iso(cuts$available_until),
       weather_issue_cutoff = cuts$weather_issue_cutoff, version = res$version, resolved_by = res$resolved_by,
       first_target = .prevcarga_inf_iso(prevcarga_first_delivery(cuts$available_until)),
       split = spec$split, train_origin = format(art$cuts$train_origin, "%Y-%m-%d"),
       last_data_day = format(art$cuts$last_data_day, "%Y-%m-%d"),
       predictions = pred, available_until_posix = cuts$available_until)
}

## --- Saida em parquet (fora do registry) ---------------------------------------------------------------------
## 7 colunas (model, variant, area, origin, available_until, target_datetime, y_pred); escrita atomica (tmp irmao + rename).
prevcarga_infer_write_out <- function(res, out, ctx) {
  oops <- function(why) prevcarga_abort("io.out_unwritable", where = out, details = list(reason = why))
  if (!is.character(out) || length(out) != 1L || is.na(out) || !nzchar(out)) oops("caminho vazio")
  dst <- normalizePath(out, winslash = "/", mustWork = FALSE)
  root <- prevcarga_registry_root(ctx)
  if (identical(dst, root) || startsWith(dst, paste0(root, "/"))) oops("dentro do registry")
  if (!dir.exists(dirname(dst))) oops("diretorio ausente")
  p <- res$predictions
  df <- data.frame(model = res$model, variant = if (is.null(res$variant)) NA_character_ else res$variant,
                   area = res$area, origin = lubridate::as_date(res$origin),
                   available_until = res$available_until_posix, target_datetime = p$target_datetime,
                   y_pred = p$y_pred, stringsAsFactors = FALSE)
  tmp <- tempfile(".infer-", tmpdir = dirname(dst), fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({ arrow::write_parquet(df, tmp); TRUE }, error = function(e) FALSE)
  if (!ok) oops("falha ao escrever")
  if (!isTRUE(file.rename(tmp, dst))) oops("falha ao mover")
  invisible(dst)
}
