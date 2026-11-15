## test-forecast_jobs.R -- forecast --jobs=N (F3.5): inferencia por (folha, membro) em PSOCK, resultado reordenado por chave radix.
## FORBIDDEN: --jobs=1 e --jobs=2 dao a mesma versao e o mesmo sha256 de cada arquivo e do run_manifest; CONTROL: sabotar UMA celula
## muda o versao. Falha de init do worker => runtime.worker_unfixable sem scratch e sem conexao orfa; --jobs invalido => exit 8.
## Fixture m6f pequeno; a fachada real roda em instalacao-copia cujo forecast.R recebe o espiao do infer (carregado tambem pelos workers,
## que sourceiam forecast.R). Tudo em tempdir; sem variaveis de ambiente.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "inference_needs", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- lubridate::ymd("2026-10-15")
work <- withr::local_tempdir("fjobs-")
topo <- m6f_topology_file(work)
deck <- file.path(work, "deck")
env$prevcarga_fixture_deck(deck, seed = 11, areas = c(M6F_LEAVES, M6F_SS), days = seq(O - 39L, O, by = "day"), sentinel = TRUE,
                           load_until = lubridate::ymd_hm("2026-10-15 09:00", tz = M6_TZ), tz = M6_TZ)

## espiao (mesma formula do m6f_delivery); `bump` = soma na celula "SIGLA/AREA" (sabotagem do CONTROL)
shim <- function(bump = "", kill_worker = FALSE) c(
  "",
  "## ESPIAO DE TESTE (so nesta copia, em tempdir): o membro entrega o sentinela; o resto da rodada e REAL.",
  if (kill_worker) "if (any(grepl('workRSOCK|slaveRSOCK', commandArgs()))) stop('worker sem runtime')",
  "prevcarga_infer_cell <- function(member, area, origin, ctx, variant = NULL, version = NULL, pin = FALSE, ...) {",
  "  cuts <- prevcarga_resolve_cuts(ctx, origin = origin)",
  "  dd <- as.integer(prevcarga_plugin_descriptor(member$mdir, prevcarga_models_root(ctx$root))$delivery_days)",
  "  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), cuts$tz)",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), t0 + lubridate::days(dd), by = '30 min')",
  "  y <- lubridate::time_length((ie - lubridate::minutes(30L)) - t0, 'minute') + 10 * match(as.character(member$sigla), c('RF', 'LGBM', 'HW')) +",
  "       100 * match(area, c('A1', 'A2', 'B1'))",
  sprintf("  if (paste0(member$sigla, '/', area) == '%s') y <- y + 1", bump),
  "  list(predictions = data.table::data.table(target_datetime = ie, y_pred = y),",
  "       version = if (is.null(version)) paste0('v_', substr(digest::digest(paste(member$sigla, area), algo = 'sha256'), 1L, 16L)) else version,",
  "       available_until_posix = cuts$available_until)",
  "}")
make_root <- function(...) {
  sh <- shim(...)
  pc_install_copy(plugins = TRUE, mutate = function(r) {
    file.copy(topo, file.path(r, "config", "topology.yaml"), overwrite = TRUE)
    f <- file.path(r, "scripts", "lib", "forecast.R")
    writeLines(c(readLines(f, warn = FALSE), sh), f)
  })
}
root_ok <- make_root()
root_bump <- make_root(bump = "RF/A1")
root_dead <- make_root(kill_worker = TRUE)

## estado preparado uma vez (k-hat latest, pesos da combina, artefatos do HW origin_scoped) e copiado por rodada
ctx0 <- m6f_ctx(deck, file.path(work, "state0"), topo)
dir.create(ctx0$state_dir, recursive = TRUE)
env$prevcarga_infer_cell <- m6f_infer_spy(env, new.env())
m6f_prepare_origin(env, ctx0, O)
mem <- env$prevcarga_members(path = topo, root = pc_root())
for (a in M6F_LEAVES) m6f_fake_artifact(env, mem[mem$sigla == "HW", ], a, O, ctx0)
fresh_state <- function(.frame = parent.frame()) {
  st <- withr::local_tempdir("fjobs-st-", .local_envir = .frame)
  file.copy(list.files(ctx0$state_dir, full.names = TRUE), st, recursive = TRUE)
  st
}
run_cli <- function(root, jobs, .frame = parent.frame()) {
  st <- fresh_state(.frame)
  r <- pc_cli(root, c("forecast", "--origin=2026-10-15", "--skip-validate", "--incluir-membros", jobs,
                      paste0("--data-train=", deck), paste0("--state=", st), "--format=json"))
  j <- if (length(r$stdout) == 1L) jsonlite::fromJSON(r$stdout, simplifyVector = FALSE) else NULL
  list(raw = r, json = j, state = st)
}
sha <- function(f) digest::digest(file = f, algo = "sha256")
## o que a entrega fixa: versao, sha256 de cada entrada de arquivos[] e do run_manifest.json
deliv <- function(x) list(versao = x$json$result$versao, arq = vapply(x$json$result$arquivos, function(a) paste(a$path, a$sha256), ""),
                          manifest = sha(file.path(x$json$result$saida, "run_manifest.json")))
ok <- function(x) expect_identical(x$raw$status, 0L, info = paste(c(x$raw$stderr, x$raw$stdout), collapse = "\n"))

J1 <- run_cli(root_ok, "--jobs=1")        # referencia serial, reaproveitada pelo CONTROL
test_that("[AC1] FORBIDDEN: --jobs=1 e --jobs=2 => mesma versao e mesmo sha256 de cada arquivo e do run_manifest", {
  j1 <- J1
  j2 <- run_cli(root_ok, "--jobs=2")
  ok(j1); ok(j2)
  expect_gt(length(deliv(j1)$arq), 12L)
  expect_identical(deliv(j2), deliv(j1))
  expect_identical(j2$json$result$jobs, 2L)         # o jobs real so entra no result do envelope
  expect_identical(j1$json$result$jobs, 1L)
  man <- readLines(file.path(j2$json$result$saida, "run_manifest.json"), warn = FALSE)
  expect_false(any(grepl("jobs", man)))
})

test_that("[AC2] CONTROL: sabotar uma celula muda o versao com jobs = 2", {
  j1 <- J1
  jb <- run_cli(root_bump, "--jobs=2")
  ok(jb)
  expect_false(identical(deliv(jb)$versao, deliv(j1)$versao))
  expect_false(identical(deliv(jb)$manifest, deliv(j1)$manifest))
})

test_that("[AC3] worker que nao fixa o runtime => runtime.worker_unfixable (exit da classe runtime), sem scratch nem entrega", {
  d <- run_cli(root_dead, "--jobs=2")
  expect_false(identical(d$raw$status, 0L))
  expect_identical(d$json$error$id, "runtime.worker_unfixable")
  expect_false(dir.exists(file.path(d$state, "forecasts")) && length(list.files(file.path(d$state, "forecasts"))) > 0L)
  expect_true(!dir.exists(file.path(d$state, "scratch")) || !length(list.files(file.path(d$state, "scratch"))))
})

test_that("[AC3] init com falha no cluster: erro runtime.worker_unfixable e nenhuma conexao orfa", {
  hook <- function() stop("sem runtime"); environment(hook) <- globalenv()
  ctx <- list(lib_dir = file.path(pc_root(), "scripts", "lib"), worker_modules = c("errors.R", "runtime.R"),
              worker_hooks = list(pre_runtime = hook))
  n0 <- nrow(showConnections(all = TRUE))
  e <- tryCatch(env$.forecast_cluster_start(ctx, 2L), prevcarga_error = function(e) e)
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$id, "runtime.worker_unfixable")
  expect_identical(nrow(showConnections(all = TRUE)), n0)
})

test_that("[AC4] --jobs=0 e --jobs=x => config.bad_type (exit 8) com where --jobs; --jobs=1 nao cria cluster", {
  for (v in c("--jobs=0", "--jobs=x")) {
    r <- run_cli(root_ok, v)
    expect_identical(r$raw$status, 8L)
    expect_identical(r$json$error$id, "config.bad_type")
    expect_identical(r$json$error$where, "--jobs")
  }
  ## jobs = 1 em processo: .forecast_cluster_start nunca e chamado
  old <- env$.forecast_cluster_start
  withr::defer(assign(".forecast_cluster_start", old, envir = env))
  assign(".forecast_cluster_start", function(...) stop("cluster criado com jobs = 1"), envir = env)
  plan <- list(leaves = c("A1", "B1"))
  withr::defer(assign(".forecast_cell_safe", env$.forecast_cell_safe, envir = env))
  assign(".forecast_cell_safe", function(ctx, m, a, g) list(status = "ok", folha = a, sigla = as.character(m$sigla)), envir = env)
  out <- env$.forecast_members(list(), plan, list(), mem, jobs = 1L)
  expect_identical(names(out), c("A1", "B1"))
  expect_identical(names(out$A1), as.character(mem$sigla))
})

test_that("[estrutural] forecast.R: sem fork/mclapply/Sys.getenv/Sys.setenv; cluster fechado em on.exit", {
  src <- sub("\\s*##.*$", "", readLines(file.path(pc_root(), "scripts", "lib", "forecast.R"), warn = FALSE))
  expect_false(any(grepl("mclapply|mcparallel|makeForkCluster|Sys\\.getenv|Sys\\.setenv", src)))
  expect_true(any(grepl("on.exit\\(parallel::stopCluster\\(cl\\)", src)))
})
