## test-contract.R -- DL.8 (AC4 do epico): contrato do plugin dessem-linear. `exogena` ausente ou fora do conjunto e
## `history_days.infer != train` abortam [prevcarga/contract] (exit 2) ANTES de treinar (espiao em tune_wsvm: 0 chamadas);
## hora faltando no historico => [prevcarga/completude] (exit 3), tambem antes do primeiro ajuste. CONTROL: os 3 membros
## (MED/MAX/MXMN) resolvem e MED x MAX dao y_pred diferente. Deck sintetico de fixture_store.R pelo leitor e pelo despachante.

## raiz de modelos descartavel com o plugin (R/ + run.yaml mutado) e o motor, para o loader real
dl_root_mut <- function(mutate = identity) {
  root <- tempfile("dl8-root-"); dir.create(file.path(root, "dessem-linear"), recursive = TRUE)
  file.copy(file.path(dl_models(), "dessem-linear", "R"), file.path(root, "dessem-linear"), recursive = TRUE)
  file.copy(file.path(dl_models(), "_dessem-engine"), root, recursive = TRUE)
  y <- yaml::read_yaml(file.path(dl_models(), "dessem-linear", "run.yaml"))
  yaml::write_yaml(mutate(y), file.path(root, "dessem-linear", "run.yaml"))
  root
}

test_that("[PLUGINS] contract: exogena ausente ou 'XYZ' => contract exit 2 antes de treinar (0 ajustes de SVM)", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st)
  spy <- dl_spy_tune(); on.exit(spy$restore(), add = TRUE)
  for (bad in list(NULL, "XYZ", "med", c("MED", "MAX"), NA_character_)) {
    spec <- dl_fx_spec(); spec$params$exogena <- bad
    e <- dl_err(dl_train(st, cuts, spec))
    expect_dl_class(e, "contract", 2L, id = "plugin.dessem_linear_bad_exogena")
    expect_identical(e$details$allowed, c("MED", "MAX", "MXMN"))
  }
  expect_identical(spy$log$n, 0L)
  ## o mesmo nos estagios de treino e de ingestao com spec (variante validada ANTES de tocar o painel)
  spec <- dl_fx_spec(); spec$params$exogena <- "XYZ"
  m <- dl_model_panel(dl_fx_panel(st, cuts, "train"), cuts)
  e2 <- dl_err(dl_plugin()$env$plugin_ingest(m$panel, m$cuts, spec))
  expect_dl_class(e2, "contract", 2L, id = "plugin.dessem_linear_bad_exogena")
  expect_identical(spy$log$n, 0L)
  ## run.yaml sem exogena, ou com valor fora do conjunto: o loader recusa (contract) antes de qualquer estagio
  for (mut in list(function(y) { y$params$exogena <- NULL; y }, function(y) { y$params$exogena <- "XYZ"; y })) {
    root <- dl_root_mut(mut)
    e3 <- dl_err(dl_lib$prevcarga_plugin_load("dessem-linear", root))
    expect_dl_class(e3, "contract", 2L, id = "plugin.bad_run_yaml")
  }
})

test_that("[PLUGINS] contract: history_days.infer != train => contract exit 2 no loader; CONTROL infer == train carrega", {
  root <- dl_root_mut(function(y) { y$history_days$infer <- y$history_days$train + 1L; y })
  e <- dl_err(dl_lib$prevcarga_plugin_load("dessem-linear", root))
  expect_dl_class(e, "contract", 2L, id = "plugin.history_window")
  expect_true(dl_plugin()$descriptor$history_infer_equals_train)
  ok <- dl_err(dl_lib$prevcarga_plugin_load("dessem-linear", dl_root_mut()))
  expect_null(ok)
  p <- dl_lib$prevcarga_plugin_load("dessem-linear", dl_root_mut())
  expect_identical(p$spec$history_days$infer, p$spec$history_days$train)
})

test_that("[PLUGINS] contract: hora faltando no historico => completude exit 3 antes do 1o ajuste; CONTROL deck completo segue", {
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st)
  dir <- dl_lib$prevcarga_fixture_drop_slots(st$dir, DL8_AREA, DL8_O - 8L, "10:00", 2L)    # a hora 10:00-11:00 inteira
  gap <- dl_fx_with_dir(st, dir)
  spy <- dl_spy_tune(); on.exit(spy$restore(), add = TRUE)
  e <- dl_err(dl_train(gap, cuts))
  expect_dl_class(e, "completude", 3L)
  expect_identical(e$id, "plugin.dessem_linear_series_gap")
  expect_match(conditionMessage(e), format(DL8_O - 8L), fixed = TRUE)
  expect_identical(spy$log$n, 0L)
  ## meia hora isolada (hora incompleta) tambem nao forma hora: o to_hourly a descarta e o plugin acusa a hora
  dir2 <- dl_lib$prevcarga_fixture_drop_slots(st$dir, DL8_AREA, DL8_O - 8L, "10:30", 1L)
  e2 <- dl_err(dl_train(dl_fx_with_dir(st, dir2), cuts))
  expect_dl_class(e2, "completude", 3L)
  expect_identical(spy$log$n, 0L)
  ## CONTROL: o mesmo deck sem o buraco monta o deck do treino (ingestao completa, nenhum ajuste necessario para provar)
  m <- dl_model_panel(dl_fx_panel(st, cuts, "train"), cuts)
  deck <- dl_with_cli(suppressMessages(dl_plugin()$env$plugin_ingest(m$panel, m$cuts, dl_fx_spec())))
  expect_identical(nrow(deck$CARGAHIST) %% 24L, 0L)
})

test_that("[PLUGINS] contract CONTROL: os 3 membros resolvem a variante e MED x MAX dao y_pred diferente (> 1e-5)", {
  p <- dl_plugin(); env <- p$env
  keys <- env$plugin_constants()$model_key
  expect_identical(names(keys), c("MED", "MAX", "MXMN"))
  for (ex in names(keys)) {
    v <- env$dl_resolve_variant(dl_fx_spec(ex))
    expect_identical(v$exogena, ex); expect_identical(v$model_key, unname(keys[[ex]]))
  }
  st <- dl_fx_store(); cuts <- dl_fx_cuts(st)
  ## MXMN: variante e config do motor resolvem ate a engenharia de features (sem treino)
  m <- dl_model_panel(dl_fx_panel(st, cuts, "train"), cuts)
  frame <- dl_with_cli(suppressMessages(env$plugin_ingest(m$panel, m$cuts, dl_fx_spec("MXMN"))))
  feats <- dl_with_cli(suppressMessages(env$plugin_build_features(frame, dl_fx_spec("MXMN"))))
  expect_identical(feats$deck$variant, "MXMN"); expect_identical(feats$deck$model_key, "Linear.TmpMaxMin")
  ## MED x MAX: artefatos e entregas diferentes
  med <- dl_base_art()
  max_ <- dl_train_memo(st, cuts, dl_fx_spec("MAX"))
  expect_identical(med$stamp$variant, "MED"); expect_identical(max_$stamp$variant, "MAX")
  expect_identical(max_$payload$model_key, "Linear.TmpMaxima")
  expect_false(identical(dl_sha(med), dl_sha(max_)))
  o_med <- dl_infer(med, st, cuts)
  o_max <- dl_infer(max_, st, cuts)
  expect_gt(dl_delta(o_med$delivery, o_max$delivery), DL8_TOL)
  expect_identical(o_med$variant, "MED"); expect_identical(o_max$variant, "MAX")
  ## artefato de uma variante nao serve a outra: o spec do infer e o do artefato (contract)
  e <- dl_err(dl_lib$prevcarga_plugin_infer(p, med, dl_fx_panel(st, cuts, "infer"),
                                            modifyList(cuts, list(origin_day = cuts$origin_day - 1L))))
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$exit_code, 2L)
})
