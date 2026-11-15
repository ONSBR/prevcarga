## test-combination-member-set.R -- E6-T3: EQW por troca do conjunto de membros. Na rodada (fallback_eqw = TRUE) pesos de outro
## conjunto de membros viram EQW (1/k dos presentes) com warning.combina_member_set; sem fallback e no combine --mode=apply
## seguem abortando; load_label/load_tz divergentes abortam SEMPRE (nunca EQW). Painel sintetico + fixture m6f em tempdir.
## O coletor de avisos do cli.R guarda so id/mensagem/where; os campos do aviso sobem pelo condition (details) e, na rodada,
## o AC confere o motivo no run_manifest.json e no run.json (combina_motivo).
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
warn_conds <- function(expr) {
  conds <- list()
  val <- withCallingHandlers(expr, prevcarga_warning = function(w) {
    conds[[length(conds) + 1L]] <<- w; invokeRestart("muffleWarning")
  })
  list(val = val, conds = conds, ids = vapply(conds, function(w) w$id, ""))
}
## pesos de uma topologia sem o ultimo membro: o painel de treino tambem nao o tem (o artefato lista os membros pelos pesos)
members_red <- pc_cmb_members_without_last(members)
panel_red <- pc_cmb_panel_joined(env, members_red)
last_sigla <- as.character(members$sigla[[nrow(members)]])

test_that("[AC1] pesos sem o ultimo membro: EQW, um warning.combina_member_set com incluidos = ultimo membro", {
  t3 <- pc_cmb_trained(env, panel_red, members_red)
  before <- list.files(t3$dir, recursive = TRUE)
  r <- warn_conds(env$prevcarga_combina_apply("SECO", t3$dir, panel, ctx, members = members, detail = TRUE, fallback_eqw = TRUE))
  expect_identical(r$ids, "warning.combina_member_set")
  expect_identical(r$conds[[1L]]$where, "SECO")
  expect_identical(r$conds[[1L]]$details$folha, "SECO")
  expect_identical(r$conds[[1L]]$details$incluidos, last_sigla)
  expect_length(r$conds[[1L]]$details$retirados, 0L)
  expect_identical(r$val$version_hash, "EQW"); expect_identical(r$val$eqw_reason, "member_set")
  expect_null(r$val$weights); expect_null(r$val$members_by_lead); expect_null(r$val$cell_months)
  expect_true(is.na(r$val$path))
  rows <- env$.prevcarga_cmb_select_cut(panel, NULL)
  k <- match(paste(rows$forecast_origin, rows$lead), paste(r$val$long$forecast_origin, r$val$long$lead))
  expect_equal(r$val$long$y_pred[k], env$prevcarga_produce_eqw(rows))
  expect_gt(nrow(r$val$long), 0L)
  expect_identical(list.files(t3$dir, recursive = TRUE), before)         # nada gravado em artifact_dir
  ## sem detail devolve so o LONG
  r2 <- warn_conds(env$prevcarga_combina_apply("SECO", t3$dir, panel, ctx, members = members, fallback_eqw = TRUE))
  expect_equal(r2$val$y_pred, r$val$long$y_pred)
  ## CONTROL: mesmo conjunto => sem aviso e pesos reais
  tr <- pc_cmb_trained(env, panel, members)
  q <- warn_conds(env$prevcarga_combina_apply("SECO", tr$dir, panel, ctx, members = members, detail = TRUE, fallback_eqw = TRUE))
  expect_identical(q$ids, character()); expect_true(is.na(q$val$eqw_reason))
})

test_that("[AC1] bordas: so retirado (incluidos vazio) e so incluido (retirados vazio) sao ambos EQW", {
  ## artefato com todos os membros; topologia atual sem o ultimo => so retirado
  tr <- pc_cmb_trained(env, panel, members)
  cur <- members_red
  r <- warn_conds(env$prevcarga_combina_apply("SECO", tr$dir, panel, ctx, members = cur, detail = TRUE, fallback_eqw = TRUE))
  expect_identical(r$ids, "warning.combina_member_set")
  expect_length(r$conds[[1L]]$details$incluidos, 0L)
  expect_identical(r$conds[[1L]]$details$retirados, last_sigla)
  expect_identical(r$val$eqw_reason, "member_set")
  rows <- env$.prevcarga_cmb_select_cut(panel, NULL)
  k <- match(paste(rows$forecast_origin, rows$lead), paste(r$val$long$forecast_origin, r$val$long$lead))
  expect_equal(r$val$long$y_pred[k], env$prevcarga_produce_eqw(rows))
  ## so incluido
  t3 <- pc_cmb_trained(env, panel_red, members_red)
  q <- warn_conds(env$prevcarga_combina_apply("SECO", t3$dir, panel, ctx, members = members, detail = TRUE, fallback_eqw = TRUE))
  expect_identical(q$conds[[1L]]$details$incluidos, last_sigla)
  expect_length(q$conds[[1L]]$details$retirados, 0L)
})

test_that("[AC2] sem fallback e no combine --mode=apply aborta contract.combina_artifact_mismatch (exit 2)", {
  t3 <- pc_cmb_trained(env, panel_red, members_red)
  e <- catch_pc(env$prevcarga_combina_apply("SECO", t3$dir, panel, ctx, members = members, fallback_eqw = FALSE, detail = TRUE))
  expect_identical(e$id, "contract.combina_artifact_mismatch"); expect_identical(e$exit_code, 2L)
  e0 <- catch_pc(env$prevcarga_combina_apply("SECO", t3$dir, panel, ctx, members = members))
  expect_identical(e0$id, "contract.combina_artifact_mismatch")
  ## CLI por processo: pesos treinados sem o ultimo membro em <state>/artifacts/combina; o apply do CLI usa a topologia completa
  skip_if_not_installed("docopt")
  fx <- pc_cmb_cli_fixture(dir = withr::local_tempdir())
  adir <- file.path(fx$state, "artifacts", "combina"); dir.create(adir, recursive = TRUE)
  env$prevcarga_combina_train("SECO", panel_red, adir, root = pc_root(), members = members_red,
                              trained_at = "2024-01-01T00:00:00Z")
  out <- file.path(withr::local_tempdir(), "apply.parquet")
  ap <- pc_run_cli(c("combine", "--mode=apply", "--area=SECO", paste0("--root=", fx$root), paste0("--data-train=", fx$data),
                     paste0("--state=", fx$state), "--format=json", paste0("--out=", out)))
  expect_identical(ap$status, 2L)
  j <- jsonlite::fromJSON(paste(ap$stdout, collapse = "\n"), simplifyVector = FALSE)
  expect_identical(j$error$id, "contract.combina_artifact_mismatch")
  expect_false(file.exists(out))
})

test_that("[AC3] load_label ou load_tz divergente aborta sempre, com fallback, sem EQW nem aviso (mesmos membros)", {
  tr <- pc_cmb_trained(env, panel, members)
  with_prov <- function(col, value) {
    p <- data.table::copy(panel)
    prov <- data.table::copy(data.table::as.data.table(attr(panel, "provenance")))
    data.table::set(prov, j = col, value = value)
    data.table::setattr(p, "provenance", prov)
    p
  }
  for (p in list(with_prov("load_label", "end"), with_prov("load_tz", "UTC"))) {
    ids <- character()
    e <- withCallingHandlers(
      catch_pc(env$prevcarga_combina_apply("SECO", tr$dir, p, ctx, members = members, detail = TRUE, fallback_eqw = TRUE,
                                           stale_ok = TRUE)),
      prevcarga_warning = function(w) { ids <<- c(ids, w$id); invokeRestart("muffleWarning") })
    expect_identical(e$id, "contract.combina_artifact_mismatch"); expect_identical(e$exit_code, 2L)
    expect_identical(ids, character())
  }
  ## a convencao vence a troca de membros: outro conjunto E rotulo divergente => aborta (nunca EQW)
  t3 <- pc_cmb_trained(env, panel_red, members_red)
  ids <- character()
  e <- withCallingHandlers(
    catch_pc(env$prevcarga_combina_apply("SECO", t3$dir, with_prov("load_label", "end"), ctx, members = members,
                                         fallback_eqw = TRUE)),
    prevcarga_warning = function(w) { ids <<- c(ids, w$id); invokeRestart("muffleWarning") })
  expect_identical(e$id, "contract.combina_artifact_mismatch")
  expect_identical(ids, character())
})

test_that("[AC4] forecast em escopo area: A1 member_set e A2 not_trained no run_manifest e no run.json", {
  skip_if_not(exists("m6f_deck"), "helper m6f ausente")
  base <- withr::local_tempdir("mset-base-")
  DECK <- m6f_deck(env, file.path(base, "deck"))
  TOPO <- m6f_topology_file(base)
  FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
  dir.create(FX$state_dir, recursive = TRUE)
  ## A1 treinada sem o ultimo membro (painel completo, membros reduzidos)
  orig_train <- env$prevcarga_combina_train
  reduced <- function(area, panel, dir, ..., members) {
    if (identical(area, "A1")) {                      # painel e membros sem o ultimo: o artefato nao lista o membro novo
      lastm <- as.character(members$sigla[[nrow(members)]])
      p <- data.table::copy(panel)
      p[, (paste0("f_", lastm)) := NULL]
      for (at in c("members_declared", "members_present")) data.table::setattr(p, at, setdiff(attr(panel, at), lastm))
      data.table::setattr(p, "provenance", attr(panel, "provenance")[attr(panel, "provenance")$member != lastm, ])
      panel <- p; members <- pc_cmb_members_without_last(members)
    }
    orig_train(area, panel, dir, ..., members = members)
  }
  assign("prevcarga_combina_train", reduced, envir = env)
  tryCatch(m6f_prepare_origin(env, FX), finally = assign("prevcarga_combina_train", orig_train, envir = env))
  unlink(file.path(env$prevcarga_registry_root(FX), "combina", "A2"), recursive = TRUE)   # A2 sem latest.txt
  log <- new.env(); log$infer <- list(); log$train <- list()
  old <- list(i = env$prevcarga_infer_cell, t = env$prevcarga_train)
  on.exit({ assign("prevcarga_infer_cell", old$i, envir = env); assign("prevcarga_train", old$t, envir = env) })
  assign("prevcarga_infer_cell", m6f_infer_spy(env, log), envir = env)
  assign("prevcarga_train", m6f_train_spy(env, log), envir = env)
  r <- env$prevcarga_forecast(FX, skip_validate = TRUE, escopo = "area", areas = c("A1", "A2"))
  man <- jsonlite::read_json(file.path(r$result$saida, "run_manifest.json"))
  expect_identical(man$model_versions[["combina/A1"]], "EQW")
  expect_identical(man$model_versions[["combina/A2"]], "EQW")
  expect_identical(man$combina_motivo$A1, "member_set")
  expect_identical(man$combina_motivo$A2, "not_trained")
  expect_length(Filter(function(x) identical(x$id, "warning.combina_member_set"), r$warnings), 1L)
  expect_identical(Filter(function(x) identical(x$id, "warning.combina_member_set"), r$warnings)[[1L]]$where, "A1")
  expect_length(Filter(function(x) identical(x$id, "warning.combina_eqw"), r$warnings), 1L)
  run_json <- jsonlite::read_json(list.files(file.path(dirname(dirname(dirname(dirname(r$result$saida)))), "scratch"),
                                             pattern = "^run.json$", recursive = TRUE, full.names = TRUE)[[1L]])
  expect_identical(run_json$combina_motivo, man$combina_motivo)
  expect_identical(run_json$combina_pesos$A1, "EQW"); expect_identical(run_json$combina_pesos$A2, "EQW")
})
