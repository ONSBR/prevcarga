## helper-train.R -- fixtures do treino/freeze do plugin random-forest (RF.2). Auto-carregado apos helper-fixtures.R.
## Nao e codigo de produto. Sem variaveis de ambiente; tudo em tempdir; nada toca data/.
for (m in c("provenance", "honesty_gate")) sys.source(file.path(rf_root(), "scripts", "lib", paste0(m, ".R")), envir = rf_lib)

## Plugin de producao carregado num env cujo pai e a fundacao (sem loader: os estagios de infer sao do RF.3/RF.4)
rf_plug <- local({ e <- NULL; function() { if (is.null(e)) e <<- rf_source_plugin(); e } })

## spec rapido: ntree = 5 e historico de treino curto (a janela real e coberta pelo test-features/test-ingest)
rf_tr_spec <- function(ntree = 5L, seed = 123L, train_days = 40L) {
  s <- rf_fx_spec(ntree)
  s$history_days$train <- as.integer(train_days)
  s$seed <- as.integer(seed)
  s
}

## Cadeia ingestao -> features -> treino -> freeze sobre um deck. Devolve list(art, model, feats, frame, cuts, panel).
rf_tr_run <- function(dir = rf_fx_store(), spec = rf_tr_spec(), panel_edit = identity) {
  p <- rf_plug()
  cuts <- rf_fx_cuts(dir = dir); cuts$spec <- spec
  panel <- rf_lib$to_hourly(panel_edit(rf_fx_panel(dir, cuts, hourly = FALSE)), weather = FALSE)   # edita o cru; reamostra como o despachante
  frame <- p$plugin_ingest(panel, cuts, spec)
  feats <- p$plugin_build_features(frame, spec)
  model <- p$plugin_train(feats, spec, cuts)
  list(art = p$plugin_freeze(model, cuts, spec), model = model, feats = feats, frame = frame, cuts = cuts, panel = panel)
}

## sha256 do artefato congelado (bytes de prevcarga_freeze, serializacao v3); o arquivo fica em tempdir
rf_tr_sha <- function(art) {
  f <- tempfile("rf-art-", fileext = ".rds")
  on.exit(unlink(c(f, paste0(f, ".manifest.json"))))
  rf_lib$prevcarga_plugin_freeze_bytes(art, f)
}
