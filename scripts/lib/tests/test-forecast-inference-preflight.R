## test-forecast-inference-preflight.R -- deck de inferencia sem treino implicito (E1-T4). Fixture sintetica m6f em tempdir.
## Preflight so leitura; origin_scoped exige a versao de O, origin_agnostic o latest; papel train segue delta 0; erro fatal na celula.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("fip-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO, list(data_role = "train"))
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)
MEM <- env$prevcarga_members(path = TOPO, root = pc_root())
HW <- MEM[MEM$sigla == "HW", ][1L, ]

mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- env[[nm]]; withr::defer(assign(nm, old, envir = env), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
}
lst <- function(d) sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
## raiz apontada com a combina/k-hat do FX; HW com versao de O nas folhas, menos `sem_hw`; RF/LGBM com latest, menos `sem_ag` ("SIGLA/AREA")
make_root <- function(sem_hw = character(), sem_ag = character(), .frame = parent.frame()) {
  art <- withr::local_tempdir("fip-art-", .local_envir = .frame)
  file.copy(list.files(file.path(FX$state_dir, "artifacts"), full.names = TRUE), art, recursive = TRUE)
  actx <- m6f_ctx(DECK, file.path(base, "unused"), TOPO, list(artifacts_dir = art))
  for (a in setdiff(M6F_LEAVES, sem_hw)) m6f_fake_artifact(env, HW, a, O, actx)
  for (sg in c("RF", "LGBM")) for (a in M6F_LEAVES) if (!paste0(sg, "/", a) %in% sem_ag) {
    m <- MEM[MEM$sigla == sg, ][1L, ]
    v <- m6f_fake_artifact(env, m, a, O, actx)
    writeLines(v, file.path(env$prevcarga_registry_key(m, a, actx), "latest.txt"))
  }
  art
}
run_fc <- function(art, role, .frame = parent.frame()) {
  ctx <- m6f_ctx(DECK, withr::local_tempdir("fip-st-", .local_envir = .frame), TOPO,
                 list(data_role = role, artifacts_dir = if (!is.null(art)) env$.prevcarga_reg_norm(art)))
  log <- new.env(); log$infer <- list(); log$train <- list()
  ## fixture sem inference_needs gravado nos artefatos falsos: o gate de necessidade (E2-T5) tem teste proprio
  ## (test-validate-deck-inference.R); aqui so o preflight de artefatos importa
  mock(prevcarga_infer_cell = m6f_infer_spy(env, log), prevcarga_train = m6f_train_spy(env, log),
       .forecast_inference_needs_gate = function(...) invisible(NULL), .frame = .frame)
  r <- tryCatch(env$prevcarga_forecast(ctx, skip_validate = TRUE, escopo = "sin"), prevcarga_error = identity)
  list(r = r, log = log, ctx = ctx)
}

test_that("[AC0] inference sem versao de O de origin_scoped: exit 13, zero treino, raiz intacta", {
  art <- make_root(sem_hw = "A1")
  antes <- lst(art)
  s <- run_fc(art, "inference")
  expect_s3_class(s$r, "prevcarga_error")
  expect_identical(s$r$id, "state.inference_artifact_missing")
  expect_identical(s$r$exit_code, 13L)
  expect_identical(s$r$details$membro, "HW")
  expect_identical(s$r$details$area, "A1")
  expect_identical(s$r$details$origem, format(O, "%Y-%m-%d"))
  expect_identical(s$r$details$split, "origin_scoped")
  expect_identical(s$r$details$raiz, env$.prevcarga_reg_norm(art))
  expect_length(s$log$train, 0L)
  expect_length(s$log$infer, 0L)
  expect_identical(lst(art), antes)
})

test_that("[AC1] origin_agnostic sem latest aborta; com tudo presente o preflight passa e infere sem treinar", {
  art <- make_root(sem_ag = "RF/A2")
  antes <- lst(art)
  s <- run_fc(art, "inference")
  expect_identical(s$r$id, "state.inference_artifact_missing")
  expect_identical(s$r$details$membro, "RF")
  expect_identical(s$r$details$area, "A2")
  expect_identical(s$r$details$split, "origin_agnostic")
  expect_identical(lst(art), antes)
  ok <- make_root()
  s2 <- run_fc(ok, "inference")
  expect_false(inherits(s2$r, "error"))
  expect_length(s2$log$train, 0L)
  expect_gt(length(s2$log$infer), 0L)
})

test_that("[AC2] papel train: delta 0 (raiz default treina origin_scoped faltante; raiz apontada vira membro ausente)", {
  st <- withr::local_tempdir("fip-ctl-")
  dir.create(file.path(st, "artifacts"))
  file.copy(list.files(file.path(FX$state_dir, "artifacts"), full.names = TRUE), file.path(st, "artifacts"), recursive = TRUE)
  log <- new.env(); log$infer <- list(); log$train <- list()
  mock(prevcarga_infer_cell = m6f_infer_spy(env, log), prevcarga_train = m6f_train_spy(env, log))
  r <- env$prevcarga_forecast(m6f_ctx(DECK, st, TOPO, list(data_role = "train")), skip_validate = TRUE, escopo = "sin")$result
  expect_identical(sum(vapply(log$train, function(x) identical(x$member, "HW"), NA)), length(M6F_LEAVES))
  expect_length(r$membros_ausentes, 0L)
  art <- make_root(sem_hw = "A1")
  s <- run_fc(art, "train")
  expect_length(s$log$train, 0L)
  ab <- vapply(s$r$result$membros_ausentes, function(x) paste0(x$membro, "/", x$folha), "")
  expect_true("HW/A1" %in% ab)
})

test_that("[AC3] erro de inferencia levantado na celula e fatal: nao vira membro ausente", {
  art <- make_root()
  ctx <- m6f_ctx(DECK, withr::local_tempdir("fip-cell-"), TOPO, list(data_role = "inference", artifacts_dir = env$.prevcarga_reg_norm(art)))
  n <- 0L
  mock(prevcarga_train = function(...) { n <<- n + 1L; stop("nao deveria treinar") })
  g <- list(origin = O)
  ## versao removida depois do preflight: a celula do HW em A1 nao acha a versao de O
  unlink(env$prevcarga_registry_key(HW, "A1", ctx), recursive = TRUE)
  e <- tryCatch(env$.forecast_cell_safe(ctx, HW, "A1", g), prevcarga_error = identity)
  expect_identical(e$id, "state.inference_artifact_missing")
  expect_identical(n, 0L)
  expect_true(grepl(env$.FORECAST_FATAL, "state.inference_artifact_missing"))
  expect_false(grepl(env$.FORECAST_FATAL, "state.artifact_missing"))
})
