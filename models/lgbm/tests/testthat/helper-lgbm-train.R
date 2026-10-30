## helper-lgbm-train.R -- fixtures do treino/freeze do plugin LGBM (LGBM.3). Auto-carregado por test_dir. Nao e codigo de produto.
## Encadeia leitor -> ingestao -> FE -> treino -> freeze com `cuts` de prevcarga_resolve_cuts (dates.R). Tudo em tempdir.

## Deck "irmao" com a carga de `days` (Date contiguo, em ordem) somada de `delta` MW na area dos testes (mutador do fixture_store).
## days = NULL e `after` => todo dia depois de `after` (Date). Devolve o mesmo formato de lgbm_fx_deck.
lgbm_fx_perturb_days <- function(deck, days = NULL, delta = 500, after = NULL, area = LGBM_AREA) {
  ts <- function(d, hms) lubridate::ymd_hms(paste(format(d), hms), tz = LGBM_TZ)
  lo <- if (!is.null(days)) ts(min(days) - 1L, "23:59:59") else ts(after, "23:59:59")
  hi <- if (!is.null(days)) ts(max(days), "23:59:59") else NULL
  dir <- lgbm_lib$prevcarga_fixture_perturb(deck$dir, "load", after = lo, at_or_before = hi, areas = area,
                                            perturb = function(v) v + delta)
  utils::modifyList(deck, list(dir = dir))
}

## Cadeia completa. `fit_cfg`: params (substitui chaves do run.yaml), seed. Devolve frame, features, model e o artefato plano.
lgbm_fx_train <- function(deck, O = deck$O, params = list(), seed = NULL, history_days = 115L, P = lgbm_source_plugin(),
                          panel_fun = NULL) {
  spec <- lgbm_lib$prevcarga_model_spec("lgbm", lgbm_root())
  for (n in names(params)) spec$params[[n]] <- params[[n]]
  if (!is.null(seed)) spec$seed <- as.integer(seed)
  K <- lgbm_consts(P)
  ctx <- lgbm_fx_ctx(deck, O)
  cuts <- lgbm_fx_cuts(ctx, O)
  panel <- if (is.null(panel_fun)) lgbm_fx_panel(deck, ctx, "train", cuts, history_days = history_days) else panel_fun(deck, ctx, cuts)
  frame <- P$lgbm_ingest(panel, cuts, NULL, "train", K)
  feats <- P$lgbm_build_features(frame, spec, cuts, K)
  model <- P$lgbm_train(feats, spec, cuts, K)
  art <- P$plugin_freeze(model, spec, cuts, list())
  list(P = P, spec = spec, cuts = cuts, frame = frame, features = feats, model = model, artifact = art)
}

lgbm_fx_sha <- function(art) digest::digest(art, algo = "sha256")

## --- LGBM.4: infer pelo despachante real (loader P.0b + prevcarga_plugin_train/_infer) -------------------------------
## O env do plugin so enxerga base: as primitivas da fundacao vivem no global da CLI. Injeta-as so durante `code` e
## restaura o que o runner ja tinha (como hi_with_cli do HW.5). Nunca deixa simbolo no global.
lgbm_with_cli <- function(code) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_plugin_scan_pure", "prevcarga_plugin_stamp",
          "prevcarga_preserve_rng", "prevcarga_delivery_grid")
  g <- globalenv()
  had <- nm[vapply(nm, exists, NA, envir = g, inherits = FALSE)]
  old <- mget(had, envir = g)
  for (n in nm) assign(n, lgbm_lib[[n]], envir = g)
  on.exit({ rm(list = nm, envir = g); for (n in had) assign(n, old[[n]], envir = g) }, add = TRUE)
  force(code)
}

## Painel do despachante: area real (SP, tem heat_area); o leitor ja entrega `weather_observed$valid_time` (F4).
lgbm_fx_dpanel <- function(deck, ctx, mode, cuts, history_days = if (mode == "infer") 45L else 115L, area = LGBM_HEAT) {
  ## le TODAS as areas da topologia (o corte global e o minimo delas, como o despachante real) e restringe a `area`
  p <- suppressWarnings(lgbm_fx_panel(deck, ctx, mode, cuts, area = c(LGBM_AREA, LGBM_HEAT), history_days = history_days))
  one <- function(x) if (is.data.frame(x) && "area_code" %in% names(x)) x[x$area_code == area] else x
  for (n in c("load", "weather_observed", "weather_forecast", "holidays")) p[[n]] <- one(p[[n]])
  p
}

lgbm_fx_plugin <- function(params = list(), root = file.path(lgbm_root(), "models")) {
  lgbm_lib$prevcarga_plugin_unload_all()
  p <- lgbm_with_cli(lgbm_lib$prevcarga_plugin_load("lgbm", root))
  for (n in names(params)) p$spec$params[[n]] <- params[[n]]
  p
}

## Origem completa pelo despachante: plugin, cuts, artefato carimbado (treino ate last_data_day) e painel de infer.
lgbm_fx_origin <- function(deck, params = list(), history_days = 115L) {
  ctx <- lgbm_fx_ctx(deck)
  cuts <- lgbm_fx_cuts(ctx)
  plugin <- lgbm_fx_plugin(params)
  tp <- lgbm_fx_dpanel(deck, ctx, "train", cuts, history_days)
  stamp_ctx <- list(load_label = cuts$load_label, timezone = cuts$tz, weather_issue_cutoff = cuts$weather_issue_cutoff)
  art <- suppressWarnings(lgbm_with_cli(lgbm_lib$prevcarga_plugin_train(plugin, tp, cuts, plugin$spec, stamp_ctx)))
  list(plugin = plugin, ctx = ctx, cuts = cuts, art = art, panel = lgbm_fx_dpanel(deck, ctx, "infer", cuts))
}

lgbm_fx_infer <- function(o, panel = o$panel, cuts = o$cuts, art = o$art)
  suppressWarnings(lgbm_with_cli(lgbm_lib$prevcarga_plugin_infer(o$plugin, art, panel, cuts)))

## Deck "ragged": carga das duas areas ate O+1 no store; ZZ1 perde 09:30..O 24:00, entao o available_until do leitor e O 09:30
## e SP (area do despachante) TEM carga alem do corte no store (o leitor a esconde). Mesmo seed/dias do lgbm_fx_deck.
.lgbm_cache <- new.env(parent = emptyenv())
lgbm_fx_ragged <- function(days = 120L, seed = 11, O = LGBM_O) {
  key <- paste("rag", days, seed, format(O))
  if (!is.null(.lgbm_cache[[key]])) return(.lgbm_cache[[key]])
  dir <- tempfile("lgbm-rag-")
  lgbm_lib$prevcarga_fixture_deck(dir, seed, c(LGBM_AREA, LGBM_HEAT), O - (days - 1L):-1L, load_label = "start",
                                  tz = LGBM_TZ, horizon_days = 2L)
  d2 <- lgbm_lib$prevcarga_fixture_drop_slots(dir, LGBM_AREA, O, "09:30", 29L)
  d3 <- lgbm_lib$prevcarga_fixture_drop_slots(d2, LGBM_AREA, O + 1L, "00:00", 48L)
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
               sprintf("  X: {areas: [%s, %s], loss: PX}", LGBM_AREA, LGBM_HEAT), "order: [X]", "members: []"),
             file.path(d3, "topology.yaml"))
  out <- list(dir = d3, O = O, available_until = lubridate::ymd_hm("2026-10-01 09:30", tz = LGBM_TZ), days = days)
  assign(key, out, envir = .lgbm_cache)
  out
}

## Origem base (artefato treinado uma vez por processo): ragged, au = O 09:30; `loess` = disable_loess do artefato.
lgbm_fx_base <- function(loess_off = TRUE) {
  key <- paste("base", loess_off)
  if (is.null(.lgbm_cache[[key]]))
    assign(key, lgbm_fx_origin(lgbm_fx_ragged(), params = list(disable_loess = loess_off)), envir = .lgbm_cache)
  .lgbm_cache[[key]]
}

## Painel de infer e cuts de um deck com carga do dia O ate `until` (POSIXct no fuso do deck): mesmo seed/dias => mesmo treino.
lgbm_fx_at <- function(until, days = 120L) {
  deck <- lgbm_fx_deck(days = days, available_until = until)
  ctx <- lgbm_fx_ctx(deck)
  cuts <- lgbm_fx_cuts(ctx)
  list(deck = deck, ctx = ctx, cuts = cuts, panel = lgbm_fx_dpanel(deck, ctx, "infer", cuts))
}
