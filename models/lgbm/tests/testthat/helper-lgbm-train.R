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
