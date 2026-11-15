## test-combination-eqw.R -- F3.4: combina pela media simples (EQW) quando a area nao tem pesos treinados. O fallback so liga
## pelo argumento fallback_eqw (forecast); combine --mode=apply segue abortando. Painel sintetico + fixture m6f em tempdir.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "inference_needs", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

members <- env$prevcarga_members(root = pc_root())
ctx <- pc_cmb_ctx_freeze()
panel <- pc_cmb_panel_joined(env, members)
catch_pc <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
warn_ids <- function(expr) {
  ids <- character()
  val <- withCallingHandlers(expr, prevcarga_warning = function(w) {
    ids <<- c(ids, w$id); invokeRestart("muffleWarning")
  })
  list(val = val, ids = ids)
}

test_that("[AC1] sem latest.txt e fallback_eqw: y_pred == produce_eqw, aviso com where = area, EQW sem pesos", {
  art <- withr::local_tempdir("eqw-")
  r <- warn_ids(env$prevcarga_combina_apply("SECO", art, panel, ctx, members = members, detail = TRUE, fallback_eqw = TRUE))
  expect_identical(r$ids, "warning.combina_eqw")
  expect_identical(r$val$version_hash, "EQW")
  expect_null(r$val$weights); expect_null(r$val$members_by_lead)
  expect_true(is.na(r$val$path))
  rows <- env$.prevcarga_cmb_select_cut(panel, NULL)
  k <- match(paste(rows$forecast_origin, rows$lead), paste(r$val$long$forecast_origin, r$val$long$lead))
  expect_equal(r$val$long$y_pred[k], env$prevcarga_produce_eqw(rows))   # o LONG sai ordenado; casa por (origem, lead)
  expect_gt(nrow(r$val$long), 0L)
  expect_identical(list.files(art, recursive = TRUE), character())     # nunca grava pesos
  ## sem detail devolve so o LONG
  r2 <- warn_ids(env$prevcarga_combina_apply("SECO", art, panel, ctx, members = members, fallback_eqw = TRUE))
  expect_equal(r2$val$y_pred, r$val$long$y_pred)
  ## o aviso carrega a area em where
  w <- tryCatch(withCallingHandlers(
    env$prevcarga_combina_apply("SECO", art, panel, ctx, members = members, fallback_eqw = TRUE),
    prevcarga_warning = function(w) stop(w)), prevcarga_warning = identity, error = identity)
  expect_identical(w$where, "SECO")
})

test_that("[AC2] sem fallback aborta 13; latest.txt invalido segue abortando com fallback; outro conjunto de membros vira EQW so com fallback (E6-T3)", {
  art <- withr::local_tempdir("eqw-")
  e <- catch_pc(env$prevcarga_combina_apply("SECO", art, panel, ctx, members = members))
  expect_identical(e$id, "state.combina_not_trained"); expect_identical(e$exit_code, 13L)
  e0 <- catch_pc(env$prevcarga_combina_apply("SECO", art, panel, ctx, members = members, fallback_eqw = FALSE))
  expect_identical(e0$id, "state.combina_not_trained")
  ## latest.txt invalido (CONTROL: a mesma area sem o arquivo cai no EQW)
  dir.create(file.path(art, "SECO")); writeLines("lixo", file.path(art, "SECO", "latest.txt"))
  e1 <- catch_pc(env$prevcarga_combina_apply("SECO", art, panel, ctx, members = members, fallback_eqw = TRUE))
  expect_identical(e1$id, "contract.combina_input")
  ## artefato de outro conjunto de membros: com fallback vira EQW (E6-T3); sem fallback aborta como antes
  t3 <- pc_cmb_trained(env, panel, pc_cmb_members_without_last(members))
  r3 <- warn_ids(env$prevcarga_combina_apply("SECO", t3$dir, panel, ctx, members = members, detail = TRUE, fallback_eqw = TRUE))
  expect_identical(r3$ids, "warning.combina_member_set")
  expect_identical(r3$val$version_hash, "EQW"); expect_identical(r3$val$eqw_reason, "member_set")
  e2 <- catch_pc(env$prevcarga_combina_apply("SECO", t3$dir, panel, ctx, members = members, fallback_eqw = FALSE))
  expect_identical(e2$id, "contract.combina_artifact_mismatch")
  ## CONTROL: com pesos treinados o fallback nao liga (sem aviso, versao v_<hash>)
  tr <- pc_cmb_trained(env, panel, members)
  r <- warn_ids(env$prevcarga_combina_apply("SECO", tr$dir, panel, ctx, members = members, detail = TRUE, fallback_eqw = TRUE))
  expect_identical(r$ids, character())
  expect_match(r$val$version_hash, "^v_[0-9a-f]{16}$")
  expect_false(is.null(r$val$weights))
})

test_that("[AC3] forecast em escopo area: A1 sem pesos sai EQW (manifest e run.json), A2 com pesos inalterada (CONTROL)", {
  skip_if_not(exists("m6f_deck"), "helper m6f ausente")
  O <- M6F_ORIGIN
  base <- withr::local_tempdir("eqw-base-")
  DECK <- m6f_deck(env, file.path(base, "deck"))
  TOPO <- m6f_topology_file(base)
  FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
  dir.create(FX$state_dir, recursive = TRUE)
  m6f_prepare_origin(env, FX)
  run <- function(ctx) {
    log <- new.env(); log$infer <- list(); log$train <- list()
    old <- list(i = env$prevcarga_infer_cell, t = env$prevcarga_train)
    on.exit({ assign("prevcarga_infer_cell", old$i, envir = env); assign("prevcarga_train", old$t, envir = env) })
    assign("prevcarga_infer_cell", m6f_infer_spy(env, log), envir = env)
    assign("prevcarga_train", m6f_train_spy(env, log), envir = env)
    env$prevcarga_forecast(ctx, skip_validate = TRUE, escopo = "area", areas = c("A1", "A2"))
  }
  copy_state <- function() {
    st <- withr::local_tempdir("eqw-state-", .local_envir = parent.frame())
    file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
    m6f_ctx(DECK, st, TOPO)
  }
  yp <- function(r, f) arrow::read_parquet(file.path(r$result$saida, "folhas", paste0(f, ".parquet")))
  ctl <- run(copy_state())                                               # CONTROL: ambas com pesos
  ctx <- copy_state()
  unlink(file.path(env$prevcarga_registry_root(ctx), "combina", "A1"), recursive = TRUE)
  r <- run(ctx)
  man <- jsonlite::read_json(file.path(r$result$saida, "run_manifest.json"))
  expect_identical(man$model_versions[["combina/A1"]], "EQW")
  expect_match(man$model_versions[["combina/A2"]], "^v_[0-9a-f]{16}$")
  expect_identical(man$model_versions[["combina/A2"]],
                   jsonlite::read_json(file.path(ctl$result$saida, "run_manifest.json"))$model_versions[["combina/A2"]])
  w <- Filter(function(x) identical(x$id, "warning.combina_eqw"), r$warnings)
  expect_length(w, 1L)
  expect_identical(w[[1L]]$where, "A1")
  expect_length(Filter(function(x) identical(x$id, "warning.combina_eqw"), ctl$warnings), 0L)
  expect_equal(yp(r, "A2")$y_pred, yp(ctl, "A2")$y_pred)               # A2 inalterada
  run_json <- jsonlite::read_json(list.files(file.path(dirname(dirname(dirname(dirname(r$result$saida)))), "scratch"),
                                             pattern = "^run.json$", recursive = TRUE, full.names = TRUE)[[1L]])
  expect_identical(run_json$combina_pesos$A1, "EQW")
})
