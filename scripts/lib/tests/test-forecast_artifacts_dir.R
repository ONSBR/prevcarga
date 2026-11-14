## test-forecast_artifacts_dir.R -- forecast com artefatos apontados (F3.6): --artifacts-dir / artifacts_dir do config.yaml.
## Raiz apontada = so leitura (lista + sha256 identicos antes/depois, <state_dir>/artifacts nao nasce); origin_scoped sem versao de O
## vira membro ausente sem treino implicito; CONTROL sem a raiz apontada treina. Fixture sintetica m6f em tempdir; sem env vars.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "inference_needs", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("fad-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)                       # combina + k-hat no registry do FX
MEM <- env$prevcarga_members(path = TOPO, root = pc_root())
HW <- MEM[MEM$sigla == "HW", ][1L, ]

mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- env[[nm]]; withr::defer(assign(nm, old, envir = env), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
}
snap <- function(d) {
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
  stats::setNames(vapply(file.path(d, f), digest::digest, "", file = TRUE, algo = "sha256"), f)
}
## raiz apontada: copia dos artefatos do FX + versao de O do HW em todas as folhas, salvo `sem` (folhas sem versao de O)
make_art <- function(sem = character(), .frame = parent.frame()) {
  art <- withr::local_tempdir("fad-art-", .local_envir = .frame)
  file.copy(list.files(file.path(FX$state_dir, "artifacts"), full.names = TRUE), art, recursive = TRUE)
  actx <- m6f_ctx(DECK, file.path(base, "unused"), TOPO, list(artifacts_dir = art))
  for (a in setdiff(M6F_LEAVES, sem)) m6f_fake_artifact(env, HW, a, O, actx)
  art
}
run_fc <- function(extra, .frame = parent.frame()) {
  st <- withr::local_tempdir("fad-state-", .local_envir = .frame)
  ctx <- m6f_ctx(DECK, st, TOPO, extra)
  log <- new.env(); log$infer <- list(); log$train <- list()
  mock(prevcarga_infer_cell = m6f_infer_spy(env, log), prevcarga_train = m6f_train_spy(env, log), .frame = .frame)
  r <- env$prevcarga_forecast(ctx, skip_validate = TRUE, escopo = "sin")
  list(r = r$result, log = log, ctx = ctx, run = jsonlite::read_json(file.path(r$result$saida, "manifest.json")))
}
expect_pc <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (!is.null(cond)) { expect_identical(cond$id, id); expect_identical(cond$exit_code, as.integer(exit)) }
  invisible(cond)
}

test_that("[FORBIDDEN] raiz apontada: le dela, nada muda nela e <state_dir>/artifacts nao nasce; run.json e result trazem a origem", {
  art <- make_art()
  before <- snap(art)
  s <- run_fc(list(artifacts_dir = env$.prevcarga_reg_norm(art)))
  expect_identical(snap(art), before)
  expect_false(dir.exists(file.path(s$ctx$state_dir, "artifacts")))
  expect_length(s$log$train, 0L)
  expect_identical(s$r$artefatos$modo, "apontado")
  expect_identical(s$r$artefatos$raiz, env$.prevcarga_reg_norm(art))
  expect_length(s$r$membros_ausentes, 0L)
  expect_true(any(vapply(s$log$infer, function(x) identical(x$member, "HW") && !is.null(x$version), NA)))
  rj <- jsonlite::read_json(list.files(file.path(s$ctx$state_dir, "scratch"), pattern = "^run.json$", recursive = TRUE,
                                       full.names = TRUE)[[1L]])
  expect_identical(rj$artefatos$modo, "apontado")
  expect_identical(rj$artefatos$raiz, env$.prevcarga_reg_norm(art))
})

test_that("origin_scoped sem versao de O na raiz apontada: zero treino e membro ausente; CONTROL sem raiz apontada treina", {
  art <- make_art(sem = "A1")
  before <- snap(art)
  s <- run_fc(list(artifacts_dir = env$.prevcarga_reg_norm(art)))
  expect_length(s$log$train, 0L)
  ab <- vapply(s$r$membros_ausentes, function(x) paste0(x$membro, "/", x$folha), "")
  expect_true("HW/A1" %in% ab)
  expect_identical(s$r$membros_ausentes[[match("HW/A1", ab)]]$error_id, "state.artifact_missing")
  expect_identical(snap(art), before)
  ## CONTROL: registry proprio (FX copiado) sem versao de O do HW => treina (espiao chamado) e grava o artefato falso
  st <- withr::local_tempdir("fad-ctl-")
  file.copy(list.files(file.path(FX$state_dir, "artifacts"), full.names = TRUE), file.path(st, "artifacts") |> (\(d) { dir.create(d); d })(),
            recursive = TRUE)
  log <- new.env(); log$infer <- list(); log$train <- list()
  mock(prevcarga_infer_cell = m6f_infer_spy(env, log), prevcarga_train = m6f_train_spy(env, log))
  ctl <- env$prevcarga_forecast(m6f_ctx(DECK, st, TOPO), skip_validate = TRUE, escopo = "sin")$result
  expect_identical(ctl$artefatos$modo, "registry")
  expect_true(any(vapply(log$train, function(x) identical(x$member, "HW") && identical(x$area, "A1"), NA)))
  expect_length(ctl$membros_ausentes, 0L)
})

test_that("precedencia flag > config.yaml > NULL de artifacts_dir; modo e raiz do registry", {
  root <- withr::local_tempdir("fad-root-")
  dir.create(file.path(root, "config")); writeLines("0.0.1", file.path(root, "config", "VERSION"))
  dA <- withr::local_tempdir("fad-A-"); dB <- withr::local_tempdir("fad-B-")
  cfg <- file.path(root, "config.yaml")
  writeLines(c("schema_version: 1", paste0("artifacts_dir: '", dA, "'")), cfg)
  n <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  c1 <- env$prevcarga_ctx(list("--artifacts-dir" = dB), root)
  expect_identical(c1$artifacts_dir, n(dB))
  c2 <- env$prevcarga_ctx(list(), root)
  expect_identical(c2$artifacts_dir, n(dA))
  expect_identical(env$prevcarga_registry_mode(c2), "apontado")
  expect_identical(env$prevcarga_registry_root(c2), env$.prevcarga_reg_norm(dA))
  unlink(cfg)
  c3 <- env$prevcarga_ctx(list(), root)
  expect_null(c3$artifacts_dir)
  expect_identical(env$prevcarga_registry_mode(c3), "registry")
  expect_identical(env$prevcarga_registry_root(c3), env$.prevcarga_reg_norm(file.path(c3$state_dir, "artifacts")))
})

test_that("--artifacts-dir inexistente => config.artifacts_dir_missing (exit 8) com where = o caminho", {
  root <- withr::local_tempdir("fad-root-")
  ghost <- file.path(root, "nao", "existe")
  cond <- expect_pc(env$prevcarga_ctx(list("--artifacts-dir" = ghost), root), "config.artifacts_dir_missing", 8L)
  expect_identical(cond$where, normalizePath(ghost, winslash = "/", mustWork = FALSE))
})
