## backtest.R -- MAP do backtest: uma celula (membro x folha x origem x corte) por vez (M7.3; ADR-0023, ADR-0032, ADR-0040).
##
## O backtest e VARIAS origens em sequencia; cada celula faz o treino + infer com o codigo de producao e o harness so
## trunca o tempo: `--corte` vira `truncate_at = O + HH:MM` (no fuso da carga, do manifest) entregue ao LEITOR UNICO, que deriva
## o available_until igual a producao (nenhum derivador de corte aqui). Os cortes de treino NUNCA sao calculados aqui: a linha
## do plano (orchestrate.R) ja traz train_origin/last_data_day de dates.R (classe de split + data_lag_days do descriptor);
## o protocolo so decide QUANDO treinar: `fixed` 1x (corte da menor origem), `daily` por origem, `retrain_harness` = `forecast`
## acoplado do plugin (so origin_scoped). A classe/protocolo vem da linha do plano (validada por .m7_class()).
## Artefatos vao so para `<state_dir>/cache/artifact/` (cache.R), nunca para `<state_dir>/artifacts/` (registry) e nunca se le
## `latest`. `run_cell(cell)` devolve o LONG de 8 colunas da celula; o orquestrador (M7.1) publica long.parquet + .key.
## O REDUCE (M7.7, secao 8) roda depois do MAP: combina honesta por mes (M4), reconcile por origem (k-hat refeito na origem O) e
## avaliacao com verdade FRESCA do leitor; so cache/backtest, nunca o registry de producao (`<state_dir>/artifacts/`).
## Sem variaveis de ambiente, sem relogio, sem rede.
## Requer errors.R, topology.R, provenance.R, schema_long.R, deck_reader.R, dates.R, features.R, registry.R, training.R,
## inference.R, build_long_output.R, cache.R e orchestrate.R carregados antes. Ids em errors/backtest.tsv.

## --- 1. Caminhos -----------------------------------------------------------------------------------------------
## Arvore `<state_dir>/backtest/<run_id>/` (so caminhos; nada e criado aqui).
.bt_paths <- function(ctx, run_id, plan = NULL) {
  root <- .m7_run_dir(ctx, run_id)
  members <- if (is.null(plan)) file.path(root, "members") else {
    cells <- plan$cells
    data.table::data.table(
      membro = cells$membro, folha = cells$folha, origem = cells$origem, corte_hm = cells$corte_hm,
      long_parquet = file.path(root, "members", cells$membro, cells$folha, format(cells$origem, "%Y-%m-%d"),
                               gsub(":", "", cells$corte_hm, fixed = TRUE), "long.parquet"))
  }
  list(root = root, members = members, combined = file.path(root, "combined"), weights = file.path(root, "weights"),
       reconciled = file.path(root, "reconciled"), scores = file.path(root, "scores"), report = file.path(root, "report"))
}

## --- 2. Entrada do verbo --------------------------------------------------------------------------------------
## `dplus` e `baseline` alimentam o REDUCE (M7.7); `report`/`report_format` sao do relatorio (M6.5+), aceitos e nao consumidos aqui.
prevcarga_backtest <- function(ctx, origins, models = NULL, areas = NULL, cortes = NULL, protocol = NULL, dplus = NULL,
                               baseline = "todos", resume = FALSE, keep_going = FALSE, break_lock = FALSE, jobs = 1L,
                               report = FALSE, report_format = "html") {
  default <- if (is.null(ctx$backtest$corte_default)) "08:00" else ctx$backtest$corte_default
  cortes <- .m7_parse_cortes(cortes, default)
  ## o plano valida a matriz classe x protocolo: incompativel aborta AQUI, antes de qualquer celula ou arquivo
  plan <- prevcarga_plan(ctx, origins, cortes, protocol = protocol, filters = list(model = models, area = areas))
  map <- prevcarga_orchestrate(ctx, plan, .bt_make_run_cell(ctx, plan), jobs = jobs, keep_going = keep_going,
                               resume = resume, break_lock = break_lock)
  map$summary$weather_issue_cutoff <- plan$weather_issue_cutoff     # regra efetiva (flag > config.yaml > default), so registrada
  paths <- .bt_paths(ctx, plan$run_id, plan)
  red <- .bt_reduce(ctx, plan, map, paths, dplus, baseline)
  map$summary <- .bt_merge_summary(ctx, map$summary, red)
  list(run_id = plan$run_id, status = map$status, summary = map$summary, paths = paths, scores = red$scores)
}

## Gancho do verbo (orchestrate.R: prevcarga_backtest_run): mesmo REDUCE sobre o resultado do MAP; devolve o resumo ja completo.
prevcarga_backtest_reduce <- function(ctx, plan, map, dplus = NULL, baseline = "todos") {
  paths <- .bt_paths(ctx, plan$run_id, plan)
  red <- .bt_reduce(ctx, plan, map, paths, dplus, baseline)
  list(summary = .bt_merge_summary(ctx, map$summary, red), scores = red$scores, paths = paths)
}

## Gancho do orquestrador (orchestrate.R: .m7_run_cell_for) para o verbo `backtest`.
prevcarga_backtest_cell_runner <- function(ctx, plan) .bt_make_run_cell(ctx, plan)

## --- 3. Truncagem pelo leitor ------------------------------------------------------------------------------------
## O + HH:MM no fuso declarado pelo deck (ADR-0037). E so o argumento `truncate_at` do leitor; o available_until nasce la.
.bt_cut_instant <- function(tz, origem, corte) {
  hm <- as.integer(strsplit(corte, ":", fixed = TRUE)[[1L]])
  lubridate::make_datetime(lubridate::year(origem), lubridate::month(origem), lubridate::day(origem), hm[[1L]], hm[[2L]],
                           tz = tz)
}

.bt_member <- function(cell) list(sigla = cell$membro, mdir = cell$mdir, exogena = cell$exogena)

.bt_cell_id <- function(cell) .m7_cell_id(cell$membro, cell$folha, cell$origem, cell$corte_hm)

## --- 4. Artefato por classe e protocolo ----------------------------------------------------------------------------
## Origem do treino: `fixed` treina na menor origem da janela (a linha do plano ja traz os cortes dessa origem); os demais, na da celula.
.bt_train_origin <- function(plan, cell) if (identical(cell$protocol, "fixed")) min(plan$origins) else cell$origem

## Chave `artifact` (cache.R): corte pela classe (origin_agnostic {train_origin, last_data_day}; origin_scoped {train_origin});
## a folha e o spec efetivo entram no feature_config (o cache nao tem folha na identidade). available_until nunca entra.
.bt_key_args <- function(ctx, plan, cell, spec) {
  ident <- plan$ident
  weather <- if (is.null(ctx$weather_issue_cutoff)) "available_until" else ctx$weather_issue_cutoff
  ## o limite do treino ja vem pronto do plano (dates.R); so entra na chave se o descriptor usa previsto no treino
  fc_train <- isTRUE(prevcarga_plugin_load(cell$mdir, prevcarga_models_root(ctx$root))$descriptor$weather_forecast_in_train)
  list(kind = "artifact", member_label = list(mdir = cell$mdir, exogena = cell$exogena), model_spec = spec,
       data_fingerprint = ident$data_fingerprint, load_label = ident$load_label, timezone = ident$tz,
       weather_issue_cutoff = weather, weather_issue_limit = cell$train_weather_issue_limit,
       weather_forecast_in_train = fc_train, train_origin = cell$train_origin, last_data_day = cell$last_data_day,
       feature_config = list(area = cell$folha,
                             spec = spec[c("name", "split", "granularity_min", "horizon_days", "history_days", "seed", "params")]),
       code_version = ident$code_version, lock_hash = ident$lock_hash)
}

.bt_spec <- function(ctx, cell) {
  plugin <- prevcarga_plugin_load(cell$mdir, prevcarga_models_root(ctx$root))
  prevcarga_model_spec(cell$mdir, ctx$root, .prevcarga_tr_overrides(plugin, .bt_member(cell), ctx))
}

.bt_artifact <- function(ctx, plan, cell) {
  spec <- .bt_spec(ctx, cell)
  args <- .bt_key_args(ctx, plan, cell, spec)
  key <- do.call(prevcarga_cache_key, args)
  art <- prevcarga_cache_get(ctx, key, "artifact")
  if (!is.null(art)) return(art)
  member <- .bt_member(cell)
  inp <- prevcarga_train_inputs(member, cell$folha, .bt_train_origin(plan, cell), ctx)
  ## o treino deriva os mesmos cortes do dates.R que o plano: qualquer divergencia e bug, nunca um recalculo
  if (!identical(inp$cuts$train_origin, cell$train_origin) || !identical(inp$cuts$last_data_day, cell$last_data_day))
    prevcarga_abort("contract.cut_mismatch", where = cell$mdir,
                    details = list(plan_train_origin = format(cell$train_origin), train_origin = format(inp$cuts$train_origin),
                                   plan_last_data_day = format(cell$last_data_day),
                                   last_data_day = format(inp$cuts$last_data_day)))
  pctx <- list(load_label = inp$cuts$load_label, timezone = inp$cuts$tz, weather_issue_cutoff = inp$cuts$weather_issue_cutoff)
  fit <- prevcarga_plugin_train(inp$plugin, inp$panel, inp$cuts, inp$spec, pctx)
  art <- prevcarga_artifact_build(member, cell$folha, inp, fit)
  prevcarga_cache_put(ctx, key, "artifact", art, identity = do.call(prevcarga_cache_identity, args))
  art
}

## --- 5. Inferencia por protocolo ----------------------------------------------------------------------------------
## fixed/daily: infer da fachada sobre o artefato do cache, com o corte entregue ao leitor. retrain_harness: `forecast` acoplado
## do plugin (treina so ate last_data_day dentro do despachante e infere no mesmo painel; so origin_scoped).
## Devolve list(delivery = data.table(interval_end, y_pred), available_until, descriptor).
.bt_infer <- function(ctx, plan, cell, cut) {
  member <- .bt_member(cell)
  if (!.m7_class(.bt_spec(ctx, cell), cell$protocol)$coupled_forecast) {
    art <- .bt_artifact(ctx, plan, cell)
    res <- prevcarga_infer_cell(member, cell$folha, cell$origem, ctx, artifact = art, truncate_at = cut)
    plugin <- prevcarga_plugin_load(cell$mdir, prevcarga_models_root(ctx$root))
    p <- res$predictions
    return(list(delivery = data.table::data.table(interval_end = p$target_datetime, y_pred = p$y_pred),
                available_until = res$available_until_posix, descriptor = plugin$descriptor))
  }
  plugin <- prevcarga_plugin_load(cell$mdir, prevcarga_models_root(ctx$root))
  prevcarga_plugin_check_signature(plugin)
  spec <- .bt_spec(ctx, cell)
  cuts <- .prevcarga_inf_cuts(plugin, spec, cell$origem, ctx, cell$mdir)
  ## o painel do forecast carrega a historia do treino E a do infer (o despachante fatia o treino em last_data_day)
  rspec <- spec
  rspec$history_days$infer <- max(spec$history_days$train, spec$history_days$infer)
  rp <- prevcarga_infer_read_panel(plugin, rspec, cuts, cell$folha, cell$origem, ctx, truncate_at = cut)
  pctx <- list(load_label = rp$cuts$load_label, timezone = rp$cuts$tz, weather_issue_cutoff = rp$cuts$weather_issue_cutoff)
  env <- prevcarga_plugin_forecast(plugin, rp$panel, rp$cuts, spec, pctx)
  list(delivery = data.table::data.table(interval_end = env$delivery$interval_end, y_pred = env$delivery$y_pred),
       available_until = rp$cuts$available_until, descriptor = plugin$descriptor)
}

## --- 6. LONG da celula -------------------------------------------------------------------------------------------
## prevcarga_schema_long_check confere nomes e ordem; os tipos finos (forecast_origin TEXTO, nunca Date) ficam aqui.
.bt_check_types <- function(long) {
  for (n in names(prevcarga_schema_long)) {
    ok <- switch(prevcarga_schema_long[[n]], character = is.character(long[[n]]), POSIXct = inherits(long[[n]], "POSIXct"),
                 integer = is.integer(long[[n]]), double = is.double(long[[n]]), FALSE)
    if (!ok) prevcarga_abort("contract.long_schema", where = n, details = list(expected = prevcarga_schema_long[[n]]))
  }
  invisible(TRUE)
}

## 8 colunas (M6.1), entrega a partir de available_until + 30; NA no meio da grade aborta (nunca preenche com 0).
.bt_member_long <- function(plan, cell, inf) {
  member <- list(name = cell$membro, delivery_days = as.integer(inf$descriptor$delivery_days))
  long <- prevcarga_build_long(inf$delivery, member, cell$folha, cell$origem, inf$available_until, truth = NULL,
                               ctx = list(tz = plan$tz))
  prevcarga_schema_long_check(long)
  .bt_check_types(long)
  if (anyNA(long$y_pred))
    prevcarga_abort("contract.backtest_long_na", where = .bt_cell_id(cell), details = list(n_na = sum(is.na(long$y_pred))))
  if (any(long$target_datetime <= inf$available_until))
    prevcarga_abort("contract.backtest_long_na", where = .bt_cell_id(cell), details = list(reason = "entrega anterior ao corte"))
  data.table::as.data.table(long)
}

## --- 7. Composicao da celula --------------------------------------------------------------------------------------
## Escreve so no cache (chave propria) e devolve o LONG; o orquestrador publica dentro do diretorio da propria celula.
.bt_make_run_cell <- function(ctx, plan) {
  force(ctx); force(plan)
  function(cell) {
    cut <- .bt_cut_instant(plan$tz, cell$origem, cell$corte_hm)
    .bt_member_long(plan, cell, .bt_infer(ctx, plan, cell, cut))
  }
}

## --- 8. REDUCE (M7.7) ------------------------------------------------------------------------------------------------
## Depois do MAP, sobre os long.parquet `done`: (folha x corte) -> combina honesta por mes (M4); (origem x corte) -> reconcile com
## k-hat refeito em O; avaliacao com y_true fresco. Escreve so em `<state_dir>/backtest/<run_id>/` (combined, weights, reconciled,
## scores); o registry de producao nao e aberto e nenhum ponteiro `latest` e lido. A mascara de treino e do M4 (nao redefinida aqui).

## ctx do REDUCE: fuso e rotulo da carga do plano (o LONG da celula so e aceito nessa convencao).
.bt_rctx <- function(ctx, plan) {
  rctx <- ctx
  rctx$tz <- plan$tz
  rctx$load_label <- plan$load_label
  rctx
}

.bt_hhmm <- function(corte_hm) gsub(":", "", corte_hm, fixed = TRUE)

## Painel da combina de uma (folha, corte): LONG das celulas `done` (leitor unico) + proveniencia montada do plano, alinhamento
## recusado pelo M4 (prevcarga_assert_members_aligned), painel do M4 (build_panel) e y_true FRESCO por interval_end (nunca o do membro).
## Membro ausente numa origem e tolerado: a coluna f_* fica NA e a combina renormaliza. NULL se a folha/corte nao tem celula `done`.
.bt_reduce_panel <- function(ctx, plan, status, paths, folha, corte) {
  sel <- which(status$state == "done" & status$folha == folha & status$corte_hm == corte)
  if (!length(sel)) return(NULL)
  rctx <- .bt_rctx(ctx, plan)
  members <- prevcarga_members(root = ctx$root)
  dd <- list()
  parts <- vector("list", length(sel)); prov <- vector("list", length(sel))
  for (k in seq_along(sel)) {
    i <- sel[[k]]
    mdir <- status$mdir[[i]]
    if (is.null(dd[[mdir]]))
      dd[[mdir]] <- as.integer(prevcarga_plugin_load(mdir, prevcarga_models_root(ctx$root))$descriptor$delivery_days)
    origem <- format(status$origem[[i]], "%Y-%m-%d")
    d <- data.table::as.data.table(prevcarga_read_long(paths$members$long_parquet[[i]], rctx))
    d[, `:=`(member = status$membro[[i]], run_id = origem)]
    parts[[k]] <- d
    prov[[k]] <- data.table::data.table(
      member = status$membro[[i]], run_id = origem, run_mode = status$protocol[[i]], cut_suffix = "", origin = origem,
      available_until = status$available_until[[i]], cut = .bt_hhmm(corte), load_label = plan$load_label, load_tz = plan$tz,
      weather_issue_cutoff = plan$weather_issue_cutoff, delivery_days = dd[[mdir]])
  }
  long <- data.table::rbindlist(parts)
  data.table::setattr(long, "members_declared", members$sigla)
  data.table::setattr(long, "members_present", unique(status$membro[sel]))
  data.table::setattr(long, "provenance", data.table::rbindlist(prov))
  prevcarga_assert_members_aligned(long, rctx)
  prevcarga_join_fresh_truth(prevcarga_build_panel(long, members), rctx, folha)
}

## Combina honesta por mes: so `prevcarga_combina_full` (fit por (lead, corte, mes) com a mascara do M4, sem artefato). Grava
## combined/<FOLHA>/<HHMM>/combined.parquet e weights/<mes>/<FOLHA>_<HHMM>.rds (atomicos); devolve o LONG e os meses em fallback n_min.
.bt_combine <- function(ctx, panel, folha, corte, paths) {
  rctx <- ctx
  n_min <- if (is.null(ctx$combina$n_min)) 24L else as.integer(ctx$combina$n_min)
  d <- prevcarga_combina_full(panel, rctx, n_min = n_min, detail = TRUE)
  tag <- .bt_hhmm(corte)
  .m7_write_atomic(file.path(paths$combined, folha, tag, "combined.parquet"),
                   function(tmp) arrow::write_parquet(as.data.frame(d$long), tmp))
  w <- data.table::as.data.table(d$weights)
  for (mes in sort(unique(w$ym), method = "radix"))
    .m7_write_atomic(file.path(paths$weights, mes, paste0(folha, "_", tag, ".rds")),
                     function(tmp) saveRDS(w[w$ym == mes, ], tmp, version = 3))
  fb <- sort(unique(w$ym[!is.na(w$fallback)]), method = "radix")
  list(long = data.table::as.data.table(d$long),
       fallback = data.table::data.table(folha = rep(folha, length(fb)), corte_hm = rep(corte, length(fb)), mes = fb))
}

## k-hat da origem O: ratio da verdade (leitor unico) e janela [O-W, O-1] inclusiva (khat_fit). Puro sobre `truth`; nunca resolve
## nem promove perfil do registry. Devolve BU x k-hat do subsistema (data.table com BU_pred e y_pred).
.bt_khat_apply <- function(bu, truth, ss, topo, origem, window_days, rctx) {
  fit <- prevcarga_khat_fit(prevcarga_khat_ratio(truth, ss, topo), origem, window_days = window_days)
  prevcarga_khat_apply(bu, list(profile = fit$profile, glob = fit$glob), rctx)
}

.bt_window_days <- function(ctx) if (is.null(ctx$khat_window_days)) prevcarga_khat_window_days else as.integer(ctx$khat_window_days)

## Reconcile de uma (origem, corte). `combined` = lista folha -> LONG combinado. Folha sem linha na origem => `incompleto`
## (sem subsistema nem SIN); completo => bottom-up -> k-hat de O -> SIN. Escreve reconciled/<AAAA-MM-DD>/<HHMM>/.
.bt_reconcile <- function(ctx, plan, combined, origem, corte, available_until, paths, truth_fn = NULL) {
  topo <- prevcarga_topology(root = ctx$root)
  rctx <- .bt_rctx(ctx, plan)
  o <- format(origem, "%Y-%m-%d")
  rows <- lapply(stats::setNames(topo$leaves, topo$leaves), function(f) {
    l <- combined[[f]]
    if (is.null(l)) NULL else l[l$forecast_origin == o, ]
  })
  faltam <- topo$leaves[vapply(rows, function(r) is.null(r) || !nrow(r), NA)]
  if (length(faltam))
    return(list(long = NULL, incompleto = data.table::data.table(
      origem = origem, corte_hm = corte, available_until = available_until, folhas_faltando = list(faltam))))
  w <- .bt_window_days(ctx)
  if (is.null(truth_fn))
    truth_fn <- function(areas) prevcarga_read_load(rctx, areas = areas, from = origem - w, to = origem - 1L)
  sub <- list()
  withCallingHandlers({
    for (ss in topo$order) {
      areas <- topo$subsystems[[ss]]$areas
      bu <- prevcarga_reconcile_bottomup(unname(rows), ss, topo, rctx)
      ap <- .bt_khat_apply(bu, truth_fn(c(areas, ss)), ss, topo, origem, w, rctx)
      sub[[ss]] <- prevcarga_reconcile_long(ap, ss, rctx)
    }
    sin <- prevcarga_reconcile_long(prevcarga_reconcile_sin(sub, topo), "SIN", rctx)
  }, prevcarga_warning = function(w) invokeRestart("muffleWarning"))
  tag <- .bt_hhmm(corte)
  dir <- file.path(paths$reconciled, o, tag)
  for (ss in names(sub))
    .m7_write_atomic(file.path(dir, "subsistemas", paste0(ss, ".parquet")),
                     function(tmp) arrow::write_parquet(as.data.frame(sub[[ss]]), tmp))
  .m7_write_atomic(file.path(dir, "SIN.parquet"), function(tmp) arrow::write_parquet(as.data.frame(sin), tmp))
  list(long = data.table::rbindlist(c(unname(sub), list(sin))), incompleto = NULL)
}

## y_true FRESCO por interval_end (igualdade exata) por area; o y_true embutido no LONG e descartado.
.bt_join_truth <- function(ctx, long) {
  long <- data.table::copy(data.table::as.data.table(long))
  long[, y_true := NA_real_]
  for (a in unique(long$area)) {
    i <- which(long$area == a)
    act <- prevcarga_read_actual(ctx, a, from = min(long$target_datetime[i]) - lubridate::seconds(1),
                                 to = max(long$target_datetime[i]))
    data.table::set(long, i = i, j = "y_true", value = act$y_true[match(as.numeric(long$target_datetime[i]),
                                                                        as.numeric(act$interval_end))])
  }
  long
}

## SIN nao existe no deck: a verdade e a soma dos subsistemas no mesmo instante (so se todos presentes e finitos).
.bt_sin_truth <- function(long, topo) {
  ss <- long[long$area %in% topo$order, ]
  agg <- ss[, list(yt = if (.N == length(topo$order) && all(is.finite(y_true))) sum(y_true) else NA_real_),
            by = c("forecast_origin", "target_datetime")]
  i <- which(long$area == "SIN")
  data.table::set(long, i = i, j = "y_true",
                  value = agg$yt[match(paste(long$forecast_origin[i], as.numeric(long$target_datetime[i])),
                                       paste(agg$forecast_origin, as.numeric(agg$target_datetime)))])
  long
}

.bt_baseline_kinds <- function(baseline) {
  b <- if (is.null(baseline)) "todos" else as.character(baseline)[[1L]]
  switch(b, todos = c(skill_persistencia_O1 = "persistence", skill_sazonal_O7 = "seasonal_naive"),
         persistence = c(skill_persistencia_O1 = "persistence"), seasonal_naive = c(skill_sazonal_O7 = "seasonal_naive"),
         none = , nenhum = character(),
         prevcarga_abort("contract.baseline_kind", where = "--baseline", details = list(kind = b)))
}

## Baseline honesto (< O, evaluation.R) para as linhas distintas (area, origem, alvo, lead) do conjunto avaliado.
.bt_baselines <- function(rctx, long, kinds) {
  key <- paste(long$area, long$forecast_origin, as.numeric(long$target_datetime), long$lead)
  u <- as.data.frame(long[!duplicated(key), ])
  lapply(kinds, function(kd) {
    parts <- list()
    for (a in unique(u$area)) for (o in unique(u$forecast_origin[u$area == a]))
      parts[[length(parts) + 1L]] <- prevcarga_baseline(rctx, a, o, kd, u)
    do.call(rbind, parts)
  })
}

## Scorecard (so evaluation.R) com a verdade fresca; skill por baseline como coluna propria.
.bt_scorecard <- function(rctx, long, by, kinds) {
  if (!nrow(long)) return(data.frame())
  long <- as.data.frame(long)
  sc <- prevcarga_scorecard(long, by = by)
  if (length(kinds) && nrow(sc)) {
    bl <- .bt_baselines(rctx, long, kinds)
    for (nm in names(kinds)) sc[[nm]] <- prevcarga_scorecard(long, by = by, baseline_long = bl[[nm]])$skill
  }
  sc
}

## Avaliacao dia a dia. members/combined/reconciled: data.tables LONG com `corte` (e `member` nos membros). Origem sem verdade =>
## awaiting = TRUE e wmape NA (nunca 0). Grava scores/scores.rds e scores/scores.json atomicos.
.bt_evaluate <- function(ctx, plan, members, combined, reconciled, dplus, baseline, paths, origins) {
  rctx <- .bt_rctx(ctx, plan)
  topo <- prevcarga_topology(root = ctx$root)
  kinds <- .bt_baseline_kinds(baseline)
  fresh <- function(d) if (is.null(d) || !nrow(d)) NULL else .bt_check_long(.bt_join_truth(rctx, d), rctx)
  mem <- fresh(members); com <- fresh(combined); rec <- fresh(reconciled)
  if (!is.null(rec)) rec <- .bt_sin_truth(rec, topo)
  sc <- list(
    member = if (is.null(mem)) data.frame() else .bt_scorecard(rctx, mem, c("corte_hm", "member", "d_plus", "area"), kinds),
    combined = if (is.null(com)) data.frame() else .bt_scorecard(rctx, com, c("corte_hm", "d_plus", "area"), kinds),
    sin = if (is.null(rec)) data.frame() else .bt_scorecard(rctx, rec, c("corte_hm", "d_plus", "area"), kinds))
  sc$peak_fixed_18_22 <- if (nrow(sc$sin)) data.frame(sc$sin[c("corte_hm", "area", "d_plus")], wmape = sc$sin$peak,
                                                      bias_signed = sc$sin$bias) else data.frame()
  sin <- if (is.null(rec)) NULL else rec[rec$area == "SIN", ]
  ok <- if (is.null(sin)) logical() else is.finite(sin$y_true) & is.finite(sin$y_pred)
  sub_sum <- if (is.null(rec)) numeric() else {
    s <- rec[rec$area %in% topo$order, list(tot = sum(y_pred)), by = c("forecast_origin", "target_datetime")]
    s$tot[match(paste(sin$forecast_origin, as.numeric(sin$target_datetime)),
                paste(s$forecast_origin, as.numeric(s$target_datetime)))]
  }
  sc$sin_fresh <- list(wmape = if (any(ok)) prevcarga_wmape(sin$y_true[ok], sin$y_pred[ok]) else NA_real_,
                       sum_check = if (length(sub_sum)) max(abs(sin$y_pred - sub_sum), 0) else NA_real_)
  sc$awaiting_truth <- .bt_awaiting(plan, origins, rec, com, mem)
  sc$by_available_until <- TRUE
  sc$dplus <- isTRUE(dplus)
  .m7_write_atomic(file.path(paths$scores, "scores.rds"), function(tmp) saveRDS(sc, tmp, version = 3))
  .m7_write_atomic(file.path(paths$scores, "scores.json"), function(tmp)
    jsonlite::write_json(.bt_scores_json(sc), tmp, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
  sc
}

## so confere o contrato do LONG (nomes/ordem/tipos) preservando as colunas extras (corte/member); y_true ja e o fresco.
.bt_check_long <- function(d, rctx) {
  prevcarga_assert_long(as.data.frame(d)[names(prevcarga_schema_long)], rctx)
  if (!"interval_start" %in% names(d)) {
    d <- data.table::copy(d)
    d[, interval_start := prevcarga_long_interval_start(d$target_datetime, d$granularity_min, rctx$tz)]
  }
  d
}

## uma linha por (origem, corte) do plano: awaiting = nenhuma verdade finita em nenhuma linha avaliada da origem.
.bt_awaiting <- function(plan, origins, ...) {
  pools <- Filter(Negate(is.null), list(...))
  cells <- unique(plan$cells[, c("origem", "corte_hm", "available_until")])
  data.table::setorderv(cells, c("origem", "corte_hm"))
  aw <- vapply(seq_len(nrow(cells)), function(i) {
    o <- format(cells$origem[[i]], "%Y-%m-%d")
    !any(vapply(pools, function(d) any(d$forecast_origin == o & d$corte_hm == cells$corte_hm[[i]] & is.finite(d$y_true)), NA))
  }, NA)
  data.table::data.table(origem = cells$origem, corte_hm = cells$corte_hm, available_until = cells$available_until, awaiting = aw)
}

.bt_scores_json <- function(sc) {
  fmt <- function(x) {
    if (is.data.frame(x)) {
      x <- as.data.frame(x)
      for (n in names(x)) if (inherits(x[[n]], "POSIXct")) x[[n]] <- format(x[[n]], "%Y-%m-%dT%H:%M:%S%z", tz = lubridate::tz(x[[n]]))
        else if (inherits(x[[n]], "Date")) x[[n]] <- format(x[[n]], "%Y-%m-%d")
      x
    } else if (is.list(x)) lapply(x, fmt) else x
  }
  fmt(sc)
}

## Orquestra o REDUCE sobre as celulas `done` do MAP. Nunca abre o registry de producao.
.bt_reduce <- function(ctx, plan, map, paths, dplus = NULL, baseline = "todos") {
  status <- map$status
  empty <- list(incompleto = NULL, fallback = NULL, failures = NULL, scores = NULL)
  if (!any(status$state == "done")) return(empty)
  topo <- prevcarga_topology(root = ctx$root)
  rctx <- .bt_rctx(ctx, plan)
  folhas <- sort(unique(status$folha), method = "radix")
  cortes <- sort(unique(status$corte_hm), method = "radix")
  combined <- list(); fallback <- list(); mem <- list()
  for (cr in cortes) for (f in folhas) {
    panel <- .bt_reduce_panel(ctx, plan, status, paths, f, cr)
    if (is.null(panel)) next
    cb <- .bt_combine(ctx, panel, f, cr, paths)
    combined[[cr]][[f]] <- cb$long
    fallback[[length(fallback) + 1L]] <- cb$fallback
    for (i in which(status$state == "done" & status$folha == f & status$corte_hm == cr)) {
      d <- data.table::as.data.table(prevcarga_read_long(paths$members$long_parquet[[i]], rctx))
      d[, `:=`(corte_hm = cr, member = status$membro[[i]])]
      mem[[length(mem) + 1L]] <- d[, c(names(prevcarga_schema_long), "interval_start", "corte_hm", "member"), with = FALSE]
    }
  }
  ## so ha reconcile quando o plano cobre TODAS as folhas da topologia (um --area parcial nao e origem incompleta)
  can_reconcile <- setequal(folhas, topo$leaves)
  incompleto <- list(); rec <- list(); fails <- list()
  if (can_reconcile) for (cr in cortes) {
    cells <- unique(status[status$corte_hm == cr, c("origem", "available_until")])
    cells <- cells[order(cells$origem), ]
    for (k in seq_len(nrow(cells))) {
      r <- tryCatch(.bt_reconcile(ctx, plan, combined[[cr]], cells$origem[[k]], cr, cells$available_until[[k]], paths),
                    prevcarga_error = function(e) e)
      if (inherits(r, "prevcarga_error")) {
        fails[[length(fails) + 1L]] <- data.table::data.table(origem = cells$origem[[k]], corte_hm = cr,
          error_id = r$id, mensagem = conditionMessage(r))
      } else if (!is.null(r$incompleto)) incompleto[[length(incompleto) + 1L]] <- r$incompleto
      else { r$long[, corte_hm := cr]; rec[[length(rec) + 1L]] <- r$long }
    }
  }
  cbl <- data.table::rbindlist(lapply(cortes, function(cr) {
    l <- combined[[cr]]
    if (is.null(l)) NULL else data.table::rbindlist(lapply(l, function(d) data.table::copy(d)[, corte_hm := cr]))
  }))
  if (nrow(cbl)) cbl <- data.table::as.data.table(.bt_check_long(cbl, rctx))
  rcl <- if (length(rec)) data.table::as.data.table(.bt_check_long(data.table::rbindlist(rec), rctx)) else NULL
  scores <- .bt_evaluate(ctx, plan, data.table::rbindlist(mem), if (nrow(cbl)) cbl else NULL, rcl, dplus, baseline, paths,
                         plan$origins)
  scores$fallback_months <- data.table::rbindlist(fallback)
  list(incompleto = data.table::rbindlist(incompleto), fallback = data.table::rbindlist(fallback),
       failures = data.table::rbindlist(fails), scores = scores)
}

## Completa o resumo do MAP: incompleto, fallback_months, falhas de reconcile, cache e o veredito (partial se falha ou incompleto).
.bt_merge_summary <- function(ctx, s, red) {
  empty_inc <- data.table::data.table(origem = lubridate::ymd(character()), corte_hm = character(),
                                      available_until = lubridate::ymd_hms(character(), tz = "UTC"), folhas_faltando = list())
  s$incompleto <- if (is.null(red$incompleto) || !nrow(red$incompleto)) empty_inc else red$incompleto
  s$fallback_months <- if (is.null(red$fallback)) data.table::data.table(folha = character(), corte_hm = character(), mes = character())
                       else red$fallback
  s$reconcile_failures <- if (is.null(red$failures)) data.table::data.table() else red$failures
  s$cache <- prevcarga_cache_stats(ctx)
  if (identical(s$verdict, "ok") && (nrow(s$incompleto) || nrow(s$reconcile_failures))) s$verdict <- "partial"
  s
}
