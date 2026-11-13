## test-forecast_scope.R -- escopo do forecast (F3.1): area (so folhas, nunca reconcilia), subsistema (filhas de S + S) e sin
## (folhas + subsistemas + SIN); o export entrega exatamente o que o run.json declara. Topologia sintetica do fixture m6f
## (SS1 = A1,A2; SS2 = B1), 3 membros reais com espioes de infer/train e espiao de prevcarga_reconcile_apply.
## Cada FORBIDDEN tem o seu CONTROL. Tudo em tempdir; sem variaveis de ambiente.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("fs-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)

fx_copy <- function(.frame = parent.frame()) {
  st <- withr::local_tempdir("fs-state-", .local_envir = .frame)
  file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
  m6f_ctx(DECK, st, TOPO)
}
mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- env[[nm]]; withr::defer(assign(nm, old, envir = env), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
}
## espioes: infer/train + prevcarga_reconcile_apply (conta chamadas e repassa ao real)
run_scope <- function(ctx, ..., .frame = parent.frame()) {
  log <- new.env(); log$infer <- list(); log$train <- list(); log$reconcile <- 0L
  real <- env$prevcarga_reconcile_apply
  mock(prevcarga_infer_cell = m6f_infer_spy(env, log), prevcarga_train = m6f_train_spy(env, log),
       prevcarga_reconcile_apply = function(...) { log$reconcile <- log$reconcile + 1L; real(...) }, .frame = .frame)
  r <- env$prevcarga_forecast(ctx, skip_validate = TRUE, ...)
  list(r = r$result, log = log, run = jsonlite::read_json(file.path(r$result$saida, "manifest.json")))
}
arq <- function(r) sort(vapply(r$arquivos, function(a) a$path, ""), method = "radix")
srt <- function(x) sort(x, method = "radix")
expect_pc <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (!is.null(cond)) { expect_identical(cond$id, id); expect_identical(cond$exit_code, as.integer(exit)) }
  invisible(cond)
}

test_that("[FORBIDDEN] escopo area com as folhas de SS1 inteiro: so folhas, reconcile nao chamado, closed vazio", {
  s <- run_scope(fx_copy(), escopo = "area", areas = c("A1", "A2"))
  expect_identical(arq(s$r), c("folhas/A1.parquet", "folhas/A2.parquet"))
  expect_identical(s$log$reconcile, 0L)
  expect_identical(s$r$escopo, "area")
  expect_null(s$r$niveis)
  expect_identical(s$run$escopo, "area")
  expect_false(any(grepl("^SIN|^subsistemas/", arq(s$r))))
  ## CONTROL: escopo subsistema com o mesmo SS1 reconcilia (1 chamada) e entrega SS1
  c1 <- run_scope(fx_copy(), escopo = "subsistema", subsistema = "SS1")
  expect_identical(c1$log$reconcile, 1L)
})

test_that("[CONTROL] escopo subsistema SS1: folhas A1,A2 + subsistemas/SS1, sem SIN nem folha de SS2", {
  s <- run_scope(fx_copy(), escopo = "subsistema", subsistema = "SS1")
  expect_identical(arq(s$r), srt(c("folhas/A1.parquet", "folhas/A2.parquet", "subsistemas/SS1.parquet")))
  expect_identical(s$r$escopo, "subsistema"); expect_identical(s$r$subsistema, "SS1")
  expect_false(any(grepl("SIN|B1|SS2", arq(s$r))))
  expect_identical(unique(vapply(s$log$infer, function(x) x$area, "")), c("A1", "A2"))
})

test_that("[CONTROL] escopo sin: todas as folhas + SS1, SS2 + SIN", {
  s <- run_scope(fx_copy(), escopo = "sin")
  expect_identical(arq(s$r), srt(c(paste0("folhas/", M6F_LEAVES, ".parquet"), paste0("subsistemas/", M6F_SS, ".parquet"), "SIN.parquet")))
  expect_identical(s$r$escopo, "sin")
  ## o export entrega exatamente arquivos[] do run.json do scratch
  rj <- jsonlite::read_json(list.files(file.path(fx_copy_state <- dirname(dirname(dirname(dirname(s$r$saida)))), "scratch"),
                                       pattern = "^run.json$", recursive = TRUE, full.names = TRUE)[[1L]])
  expect_identical(srt(vapply(rj$arquivos, function(a) a$path, "")), arq(s$r))
  expect_true(isTRUE(rj$sin_closed)); expect_identical(unlist(rj$closed), M6F_SS)
  expect_identical(rj$escopo, "sin")
})

test_that("plano: subsistema/area fora da topologia => exit 4; escopo invalido => exit 8; --niveis nao existe mais", {
  ctx <- fx_copy()
  expect_pc(env$.forecast_plan(ctx, "subsistema", subsistema = "XX"), "topology.area_undeclared", 4L)
  expect_pc(env$.forecast_plan(ctx, "area", areas = "ZZ"), "topology.area_undeclared", 4L)
  expect_pc(env$.forecast_plan(ctx, "tudo"), "config.usage", 8L)
  expect_false("niveis" %in% names(formals(env$prevcarga_forecast)))
  expect_false("niveis" %in% names(formals(env$prevcarga_export)))
  p <- env$.forecast_plan(ctx, "area", areas = c("A1", "A2"))
  expect_identical(p$closed, character()); expect_false(p$sin_closed); expect_identical(p$stray, c("A1", "A2"))
  ## CONTROL: sin fecha tudo
  q <- env$.forecast_plan(ctx, "sin")
  expect_identical(q$closed, M6F_SS); expect_true(q$sin_closed)
})
