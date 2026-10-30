## test-forecast_verificar.R -- a entrega REAL do forecast (M6.4 -> export M6.2) carrega a identidade (run_manifest.json) que o
## verbo `verificar` (M8.7) exige. Rodada de verdade sobre o deck sintetico (helper-m6-fixtures.R: 3 membros reais, k-hat e pesos
## treinados no setup, espiao so no infer/train) e `verificar` contra outra rodada e contra si mesma.
## Cada FORBIDDEN (exit 0, delta 0) tem o seu CONTROL (parquet adulterado => 12; sem run_manifest.json => 2). Tudo em tempdir.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast", "verify_repro"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
V <- new.env(parent = env)
sys.source(file.path(pc_root(), "scripts", "lib", "cli", "verificar.R"), envir = V)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("fv-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)

fx_copy <- function(.frame = parent.frame()) {
  st <- withr::local_tempdir("fv-state-", .local_envir = .frame)
  file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
  m6f_ctx(DECK, st, TOPO)
}
## rodada completa com os espioes do infer/train (o resto -- LONG, combina, reconcile, scratch, export -- e o codigo de producao)
forecast <- function(ctx) {
  log <- new.env(); log$infer <- list(); log$train <- list()
  old <- list(i = env$prevcarga_infer_cell, t = env$prevcarga_train)
  on.exit({ assign("prevcarga_infer_cell", old$i, envir = env); assign("prevcarga_train", old$t, envir = env) }, add = TRUE)
  assign("prevcarga_infer_cell", m6f_infer_spy(env, log), envir = env)
  assign("prevcarga_train", m6f_train_spy(env, log), envir = env)
  env$prevcarga_forecast(ctx, skip_validate = TRUE, niveis = "tudo")$result
}
verificar <- function(saida, ref = NULL, state = base) {
  args <- list(`--saida` = saida, `--referencia` = if (is.null(ref)) FALSE else ref, `--oficial` = is.null(ref))
  tryCatch(list(exit = 0L, res = V$cmd_verificar(list(state_dir = state, format = "json"), args)),
           prevcarga_error = function(e) list(exit = e$exit_code, id = e$id, err = e))
}
tree <- function(d) {
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
  stats::setNames(vapply(f, function(x) digest::digest(file = file.path(d, x), algo = "sha256"), ""), f)
}
copy_delivery <- function(src, .frame = parent.frame()) {
  d <- file.path(withr::local_tempdir("fv-copy-", .local_envir = .frame), "entrega")
  dir.create(d)
  file.copy(list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE), d, recursive = TRUE)
  d
}

CTX1 <- fx_copy(); R1 <- forecast(CTX1)
CTX2 <- fx_copy(); R2 <- forecast(CTX2)

test_that("[FORBIDDEN] entrega real tem run_manifest.json com a identidade do verificar, fora de arquivos[]", {
  rmf <- file.path(R1$saida, "run_manifest.json")
  expect_true(file.exists(rmf))
  rm <- env$prevcarga_read_run_manifest(R1$saida)
  expect_match(rm$data_hash, "^[0-9a-f]{64}$"); expect_match(rm$bundle_id, "^[0-9a-f]{64}$")
  expect_identical(rm$platform, R.version$platform)
  expect_identical(rm$bundle_id, env$prevcarga_bundle_id(pc_root()))
  mv <- unlist(rm$model_versions)
  expect_true(all(grepl("^v_[0-9a-f]{16}$", mv)))
  expect_true(all(c(paste0(rep(M6F_MEMBERS, each = 3L), "/", M6F_LEAVES), paste0("combina/", M6F_LEAVES), paste0("khat/", M6F_SS))
                  %in% names(mv)))
  expect_identical(unlist(rm$split)[M6F_MEMBERS], c(RF = "origin_agnostic", LGBM = "origin_agnostic", HW = "origin_scoped"))
  expect_identical(rm$origin, format(O)); expect_identical(rm$versao, R1$versao); expect_identical(rm$load_tz, M6_TZ)
  ## o mesmo data_hash do evaluation/orchestrate (receita unica do provenance.R)
  man <- env$prevcarga_deck_manifest(DECK)
  expect_identical(rm$data_hash, env$prevcarga_data_hash(DECK, man, env$prevcarga_resolve_load_label(CTX1, man)$value))
  ## os bytes do scratch, fora de arquivos[] (integridade do verificar exclui manifest.json e run_manifest.json)
  paths <- vapply(jsonlite::read_json(file.path(R1$saida, "manifest.json"))$arquivos, function(e) e$path, "")
  expect_false("run_manifest.json" %in% paths)
  expect_identical(sort(setdiff(names(tree(R1$saida)), c("manifest.json", "run_manifest.json")), method = "radix"),
                   sort(paths, method = "radix"))
})

test_that("[FORBIDDEN] verificar entrega real x outra rodada e x si mesma => exit 0, mesma-plataforma, delta 0; rerun = no-op", {
  v <- verificar(R1$saida, R2$saida)
  expect_identical(v$exit, 0L, info = v$id)
  expect_identical(v$res$resultado, "PASS"); expect_identical(v$res$trilho, "mesma-plataforma")
  expect_true(nrow(v$res$arquivos) >= 3L); expect_true(all(v$res$arquivos$pior_delta == 0))
  expect_length(v$res$avisos, 0L)
  vs <- verificar(R1$saida, R1$saida)
  expect_identical(vs$exit, 0L); expect_true(all(vs$res$arquivos$pior_delta == 0))
  ## --oficial resolve a mesma chave (origem, available_until, versao) do run_manifest no state da outra rodada
  vo <- verificar(R1$saida, state = CTX2$state_dir)
  expect_identical(vo$exit, 0L, info = vo$id); expect_identical(normalizePath(vo$res$referencia), normalizePath(R2$saida))
  ## rerun da mesma chave: ja_publicado, nenhum byte muda (run_manifest.json incluso)
  t1 <- tree(R1$saida)
  r1b <- forecast(CTX1)
  expect_true(r1b$ja_publicado); expect_identical(r1b$versao, R1$versao); expect_identical(tree(R1$saida), t1)
})

test_that("[CONTROL] parquet adulterado (manifest coerente) => exit 12; adulterado cru => exit 9; sem run_manifest.json => exit 2", {
  d <- copy_delivery(R1$saida)
  p <- file.path(d, "SIN.parquet")
  x <- arrow::read_parquet(p)
  x$y_pred[[1L]] <- x$y_pred[[1L]] + 1
  arrow::write_parquet(x, p)
  e9 <- verificar(d, R2$saida)                                # sha256 do manifest nao bate: integridade antes dos valores
  expect_identical(e9$exit, 9L); expect_identical(e9$id, "integrity.manifest_sha")
  mf <- jsonlite::read_json(file.path(d, "manifest.json"))
  for (i in seq_along(mf$arquivos)) if (identical(mf$arquivos[[i]]$path, "SIN.parquet"))
    mf$arquivos[[i]]$sha256 <- digest::digest(file = p, algo = "sha256")
  jsonlite::write_json(mf, file.path(d, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  e12 <- verificar(d, R2$saida)
  expect_identical(e12$exit, 12L); expect_identical(e12$id, "repro.value_out_of_tol")
  a <- e12$err$result$arquivos
  expect_identical(a$n_fora[a$path == "SIN.parquet"], 1L); expect_true(all(a$n_fora[a$path != "SIN.parquet"] == 0L))
  expect_equal(a$pior_delta[a$path == "SIN.parquet"], 1)

  d2 <- copy_delivery(R1$saida)
  unlink(file.path(d2, "run_manifest.json"))
  e2 <- verificar(d2, R2$saida)
  expect_identical(e2$exit, 2L); expect_identical(e2$id, "contract.identity_mismatch")
  expect_identical(e2$err$details$campo, "run_manifest.json")
})

test_that("[CONTROL] bundle_id acompanha o codigo: arvore de dev muda um byte => outro id; RUNTIME.json => runtime_id", {
  root <- withr::local_tempdir("fv-root-")
  dir.create(file.path(root, "scripts", "lib"), recursive = TRUE)
  writeLines("x <- 1", file.path(root, "scripts", "lib", "a.R"))
  writeLines("so teste", file.path(root, "scripts", "lib", "LEIA.md"))
  id1 <- env$prevcarga_bundle_id(root)
  writeLines("mudou", file.path(root, "scripts", "lib", "LEIA.md"))     # fora da selecao do code/ do bundle
  expect_identical(env$prevcarga_bundle_id(root), id1)
  writeLines("x <- 2", file.path(root, "scripts", "lib", "a.R"))
  expect_false(identical(env$prevcarga_bundle_id(root), id1))
  jsonlite::write_json(list(version = "9.9.9", files = list()), file.path(root, "RUNTIME.json"), auto_unbox = TRUE)
  expect_identical(env$prevcarga_bundle_id(root), env$prevcarga_runtime_id(file.path(root, "RUNTIME.json")))
})
