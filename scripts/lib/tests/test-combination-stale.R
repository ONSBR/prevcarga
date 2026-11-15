## test-combination-stale.R -- E6-T2: celula stale na rodada. A rodada (forecast) aplica a celula de maior ym anterior do mesmo
## (lead, cut) quando falta a do mes pedido, com warning.combina_stale; apply estrito (default e combine --mode=apply) segue abortando.
## Painel sintetico + fixture m6f em tempdir. Os campos folha/mes_pedido/mes_usado do aviso sobem pelo condition (details);
## o coletor do cli.R guarda so id/mensagem/where, entao o AC da rodada confere os campos no run_manifest.json e no run.json.
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
catch_pc <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
warn_ids <- function(expr) {
  ids <- character(); conds <- list()
  val <- withCallingHandlers(expr, prevcarga_warning = function(w) {
    ids <<- c(ids, w$id); conds[[length(conds) + 1L]] <<- w; invokeRestart("muffleWarning")
  })
  list(val = val, ids = ids, conds = conds)
}
## pesos SECO treinados com origens 2023-01-01..2023-03-31 (celulas 2023-01..2023-03 e a do mes seguinte, 2023-04)
tr <- pc_cmb_trained(env, pc_cmb_panel_joined(env, members, origins = pc_cmb_lead_origins("2023-01-01", "2023-03-31")), members)
panel_at <- function(o) pc_cmb_panel_joined(env, members, origins = o)

test_that("[AC1] origem 2023-05-02 usa a celula 2023-04 com warning.combina_stale e cell_months", {
  p5 <- panel_at("2023-05-02")
  r <- warn_ids(env$prevcarga_combina_apply("SECO", tr$dir, p5, ctx, members = members, detail = TRUE, stale_ok = TRUE))
  expect_identical(r$ids, "warning.combina_stale")
  expect_identical(r$conds[[1L]]$where, "SECO")
  expect_identical(r$conds[[1L]]$details$folha, "SECO")
  expect_identical(r$conds[[1L]]$details$mes_pedido, "2023-05")
  expect_identical(r$conds[[1L]]$details$mes_usado, "2023-04")
  expect_identical(r$val$cell_months$mes_pedido, "2023-05")
  expect_identical(r$val$cell_months$mes_usado, "2023-04")
  expect_true(r$val$cell_months$stale)
  expect_true(is.na(r$val$eqw_reason))
  ## y_pred == produce_combined com a celula 2023-04 re-chaveada para 2023-05
  w <- as.data.frame(r$val$weights)
  w5 <- w[w$ym == "2023-04", , drop = FALSE]; w5$ym <- "2023-05"
  rows <- env$.prevcarga_cmb_select_cut(p5, NULL)
  exp_y <- env$prevcarga_produce_combined(rows, rbind(w, w5))
  k <- match(paste(rows$forecast_origin, rows$lead), paste(r$val$long$forecast_origin, r$val$long$lead))
  expect_equal(r$val$long$y_pred[k], exp_y)
  expect_gt(nrow(r$val$long), 0L)
})

test_that("[AC2] default estrito e combine --mode=apply abortam contract.combina_input; nenhum LONG", {
  p5 <- panel_at("2023-05-02")
  e <- catch_pc(env$prevcarga_combina_apply("SECO", tr$dir, p5, ctx, members = members))
  expect_identical(e$id, "contract.combina_input"); expect_identical(e$exit_code, 2L)
  expect_match(e$where, "produce_combined:lead=.* cut=.* ym=2023-05")
  e2 <- catch_pc(env$prevcarga_combina_apply("SECO", tr$dir, p5, ctx, members = members, stale_ok = FALSE, detail = TRUE))
  expect_identical(e2$id, "contract.combina_input")
  ## CLI por processo: pesos treinados so com janeiro; a deck tem origens de fevereiro e marco => celula ausente
  skip_if_not_installed("docopt")
  early <- pc_cmb_cli_fixture(origins = format(seq(as.Date("2024-01-01"), as.Date("2024-01-31"), by = 1)),
                              dir = withr::local_tempdir())
  full <- pc_cmb_cli_fixture(dir = withr::local_tempdir())
  tt <- pc_run_cli(c("combine", "--mode=train", "--area=SECO", paste0("--root=", early$root), paste0("--data=", early$data),
                     paste0("--state=", early$state), "--format=json"))
  expect_identical(tt$status, 0L)
  out <- file.path(withr::local_tempdir(), "apply.parquet")
  ap <- pc_run_cli(c("combine", "--mode=apply", "--area=SECO", paste0("--root=", full$root), paste0("--data=", full$data),
                     paste0("--state=", early$state), "--format=json", paste0("--out=", out)))
  expect_identical(ap$status, 2L)
  j <- jsonlite::fromJSON(paste(ap$stdout, collapse = "\n"), simplifyVector = FALSE)
  expect_identical(j$error$id, "contract.combina_input")
  expect_match(j$error$where, "ym=2024-0[23]")
  expect_false(file.exists(out))
})

test_that("[AC3] mes anterior a toda celula aborta contract.combina_input mesmo com stale_ok; nunca EQW silencioso", {
  p0 <- panel_at("2022-12-15")
  ids <- character()
  e <- withCallingHandlers(
    catch_pc(env$prevcarga_combina_apply("SECO", tr$dir, p0, ctx, members = members, stale_ok = TRUE, fallback_eqw = TRUE)),
    prevcarga_warning = function(w) { ids <<- c(ids, w$id); invokeRestart("muffleWarning") })
  expect_identical(e$id, "contract.combina_input")
  expect_match(e$where, "ym=2022-12")
  expect_identical(ids, character())
})

test_that("[AC4] CONTROL: celula exata 2023-04 nao emite aviso; stale FALSE; eqw_reason NA", {
  p4 <- panel_at("2023-04-01")
  r <- warn_ids(env$prevcarga_combina_apply("SECO", tr$dir, p4, ctx, members = members, detail = TRUE, stale_ok = TRUE))
  expect_identical(r$ids, character())
  expect_identical(r$val$cell_months$mes_pedido, "2023-04")
  expect_identical(r$val$cell_months$mes_usado, "2023-04")
  expect_false(r$val$cell_months$stale)
  expect_true(is.na(r$val$eqw_reason))
  ## ramo EQW (sem latest.txt): cell_months NULL e eqw_reason not_trained
  art <- withr::local_tempdir("stale-eqw-")
  q <- warn_ids(env$prevcarga_combina_apply("SECO", art, p4, ctx, members = members, detail = TRUE, fallback_eqw = TRUE))
  expect_null(q$val$cell_months)
  expect_identical(q$val$eqw_reason, "not_trained")
})

test_that("[AC5] forecast em escopo area: A1 stale (aviso, combina_celula true) e A2 exata (stale false) no manifest e no run.json", {
  skip_if_not(exists("m6f_deck"), "helper m6f ausente")
  base <- withr::local_tempdir("stale-base-")
  DECK <- m6f_deck(env, file.path(base, "deck"))
  TOPO <- m6f_topology_file(base)
  FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
  dir.create(FX$state_dir, recursive = TRUE)
  ## A1 treinada com o painel do m6f com ym regravado 2 meses atras (ultimo ym < mes da origem; celula do mes seguinte < mes da origem)
  orig_train <- env$prevcarga_combina_train
  shifted <- function(area, panel, ...) {
    if (identical(area, "A1")) {
      panel <- data.table::copy(panel)
      cal <- format(seq(as.Date("2024-01-01"), by = "month", length.out = 60L), "%Y-%m")
      data.table::set(panel, j = "ym", value = cal[match(panel$ym, cal) - 2L])
    }
    orig_train(area, panel, ...)
  }
  assign("prevcarga_combina_train", shifted, envir = env)
  tryCatch(m6f_prepare_origin(env, FX), finally = assign("prevcarga_combina_train", orig_train, envir = env))
  log <- new.env(); log$infer <- list(); log$train <- list()
  old <- list(i = env$prevcarga_infer_cell, t = env$prevcarga_train)
  on.exit({ assign("prevcarga_infer_cell", old$i, envir = env); assign("prevcarga_train", old$t, envir = env) })
  assign("prevcarga_infer_cell", m6f_infer_spy(env, log), envir = env)
  assign("prevcarga_train", m6f_train_spy(env, log), envir = env)
  r <- env$prevcarga_forecast(FX, skip_validate = TRUE, escopo = "area", areas = c("A1", "A2"))
  w <- Filter(function(x) identical(x$id, "warning.combina_stale"), r$warnings)
  expect_length(w, 1L)
  expect_identical(w[[1L]]$where, "A1")
  man <- jsonlite::read_json(file.path(r$result$saida, "run_manifest.json"))
  expect_true(man$combina_celula$A1$stale)
  expect_false(man$combina_celula$A2$stale)
  expect_identical(man$combina_celula$A2$mes_pedido, man$combina_celula$A2$mes_usado)
  expect_lt(man$combina_celula$A1$mes_usado, man$combina_celula$A1$mes_pedido)
  run_json <- jsonlite::read_json(list.files(file.path(dirname(dirname(dirname(dirname(r$result$saida)))), "scratch"),
                                             pattern = "^run.json$", recursive = TRUE, full.names = TRUE)[[1L]])
  expect_identical(run_json$combina_celula, man$combina_celula)
  expect_true(!is.null(run_json$combina_pesos$A1))
  txt <- c(readLines(file.path(r$result$saida, "run_manifest.json"), warn = FALSE))
  expect_false(any(grepl("[^ -~]", txt)))
})
