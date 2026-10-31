## test-forecast_timing.R -- tempo por passo da rodada `forecast` (M8.10, NFR de 30 min). Mesmo setup do test-forecast_compose.R
## (deck sintetico, membros com espioes). O relogio e INJETADO (`ctx$step_clock`): sem ele nao ha campos de tempo; com ele o `result` traz
## duracao_s, passos[] (validate-deck, infer, combina, reconcile, export, report), jobs, platform, runtime_id. A entrega NAO muda com o
## relogio (byte-deterministica) e o tempo nunca vai para run_manifest.json. CONTROL: espera injetada no infer reprova o orcamento de 2 s.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
source(file.path(pc_root(), "tests", "ab", "helper-nfr.R"), local = TRUE)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("ft-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)

fx_copy <- function(.frame = parent.frame(), ...) {
  st <- withr::local_tempdir("ft-state-", .local_envir = .frame)
  file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
  m6f_ctx(DECK, st, TOPO, list(...))
}
mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- env[[nm]]; withr::defer(assign(nm, old, envir = env), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
}
log <- new.env(); log$infer <- list(); log$train <- list()
mock(prevcarga_infer_cell = m6f_infer_spy(env, log), prevcarga_train = m6f_train_spy(env, log))
tick_clock <- function(step = 1) { t <- 0; function() { t <<- t + step; t } }      # relogio de teste: +step a cada leitura (nunca o relogio real)

test_that("[AC1] com relogio injetado o result traz duracao_s e os passos em ordem fixa; a soma bate com o total", {
  r <- env$prevcarga_forecast(fx_copy(step_clock = tick_clock()), skip_validate = TRUE, report = FALSE)$result
  expect_identical(vapply(r$passos, function(p) p$passo, ""), c("validate-deck", "infer", "combina", "reconcile", "export"))
  expect_false("score" %in% vapply(r$passos, function(p) p$passo, ""))
  expect_equal(r$duracao_s, 5)
  expect_identical(r$jobs, 1L)
  expect_match(r$platform, "^(linux|windows)-")
  g <- pc_nfr_budget(r, 1800)
  expect_true(g$ok)
})

test_that("[FORBIDDEN] sem relogio nao ha campos de tempo; a entrega e byte-identica com e sem relogio e sem tempo no run_manifest", {
  a <- env$prevcarga_forecast(fx_copy(), skip_validate = TRUE)$result
  b <- env$prevcarga_forecast(fx_copy(step_clock = tick_clock()), skip_validate = TRUE)$result
  expect_null(a$duracao_s); expect_null(a$passos)
  tree <- function(d) { f <- sort(list.files(d, recursive = TRUE), method = "radix"); f <- f[f != "manifest.json"]
    stats::setNames(vapply(f, function(x) digest::digest(file = file.path(d, x), algo = "sha256"), ""), f) }
  expect_identical(tree(b$saida), tree(a$saida))
  expect_false(any(grepl("duracao", readLines(file.path(b$saida, "run_manifest.json"), warn = FALSE))))
})

test_that("[AC3] CONTROL: espera injetada no infer reprova o orcamento de 2 s apontando `infer`; com 1800 s passa", {
  ## relogio de parede fake que avanca pelo sleep injetado: a espera entra no passo infer sem o teste dormir de verdade
  clock_env <- new.env(); clock_env$t <- 0
  clock <- function() { clock_env$t <- clock_env$t + 0.01; clock_env$t }
  run <- function(sleep_s) {
    wait <- m6f_infer_spy(env, log)
    ## restauracao explicita no on.exit de run(): o defer do mock() (frame lazy) vazava a espera para a rodada base quando o
    ## test-forecast_compose.R rodava antes no mesmo processo
    old <- env$prevcarga_infer_cell
    on.exit(assign("prevcarga_infer_cell", old, envir = env), add = TRUE)
    assign("prevcarga_infer_cell", function(...) { clock_env$t <- clock_env$t + sleep_s / 12; wait(...) }, envir = env)   # 12 celulas (membro x folha) => ~sleep_s no infer
    env$prevcarga_forecast(fx_copy(step_clock = clock), skip_validate = TRUE)$result
  }
  r <- pc_nfr_injected_round(run, sleep_s = 3)
  n_cells <- length(log$infer)
  expect_gt(n_cells, 0L)
  tight <- pc_nfr_budget(r, budget_s = 2)
  expect_false(tight$ok)
  expect_identical(tight$mais_lento, "infer")
  expect_true(pc_nfr_budget(r, budget_s = 1800)$ok)
  base_run <- env$prevcarga_forecast(fx_copy(step_clock = clock), skip_validate = TRUE)$result    # sem a espera injetada: passa com 2 s
  expect_true(pc_nfr_budget(base_run, budget_s = 2)$ok)
})
