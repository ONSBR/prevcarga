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
## O REDUCE (combina, pesos, reconcile, scores, relatorio) e do M7.7. Sem variaveis de ambiente, sem relogio, sem rede.
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
## `dplus`, `baseline`, `report`, `report_format` sao do REDUCE (M7.7): aceitos aqui e nao consumidos pelo MAP.
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
  list(run_id = plan$run_id, status = map$status, summary = map$summary, paths = .bt_paths(ctx, plan$run_id, plan))
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
