## test-orchestrate-psock.R -- M7.6: --jobs N por cluster PSOCK (ADR-0038; ADR-0023). Cada rodada com --jobs N roda em
## SCRIPT-FILHO (R novo, modulos em globalenv como a CLI), com shell sujo (TZ/LC_ALL/OPENBLAS por pc_with_env) ou limpo.
## Backtest de fixture: plugins-brinquedo das duas classes que consomem o RNG (runif) + deck sintetico (fixture_store.R).
## Cada FORBIDDEN (delta 0) tem o seu CONTROL (delta != 0). Tudo em tempdir; o pacote nao le variaveis de ambiente.
library(testthat)
lib <- file.path(pc_root(), "scripts", "lib")
options(prevcarga.lib = lib)

fx <- new.env()
sys.source(file.path(lib, "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

PO1 <- lubridate::ymd("2026-10-05")
PORIGINS <- PO1 + 0:2
AREAS <- c("A", "B")

## y_pred = nivel do artefato + ultima carga vista + sorteio SEM semente propria (a semente vem do run.yaml, pelo orquestrador)
RNG_30 <- c(
  "toy_inference_features <- function(frame, payload) list(area = frame$load$area_code[1L], last = utils::tail(frame$load$load_mwh, 1L))",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 2L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = payload$model$level[toy_slots(ie)] + inf_features$last + stats::runif(length(ie))))",
  "}")
RNG_60 <- c(
  "toy_inference_features <- function(frame, payload) list(area = frame$area, last = utils::tail(frame$load$load_mwh, 1L))",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 9L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = rep(payload$model$level, length(ie)) + inf_features$last + stats::runif(length(ie))))",
  "}")
toy_dir <- function(base, name, rng, run_yaml = list(), descriptor = list())
  pc_toy_variant(base, run_yaml = run_yaml, descriptor = descriptor, name = name,
                 files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines(base, 0L, 0L, character(), "ok"), "R/zz_rng.R" = rng))

## toy-a: origin_agnostic 30 min; toy-s: origin_scoped 60 min (as duas classes)
ROOT <- pc_install_copy(mutate = function(r) {
  tvs <- list(
    toy_dir("toy-30", "toy-a", RNG_30, run_yaml = list(seed = 11L, backtest = list(protocol_default = "daily")),
            descriptor = list(uses_rng = TRUE)),
    toy_dir("toy-60", "toy-s", RNG_60, run_yaml = list(seed = 23L, backtest = list(protocol_default = "daily")),
            descriptor = list(model_time_offset_min = 0L, uses_rng = TRUE)))
  for (tv in tvs) for (n in setdiff(list.files(tv), "_toy-engine")) file.copy(file.path(tv, n), file.path(r, "models"), recursive = TRUE)
  file.copy(file.path(tvs[[1L]], "_toy-engine"), file.path(r, "models"), recursive = TRUE)
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [A, B], loss: PX}",
               "order: [X]", "members:",
               "  - {sigla: MA, mdir: toy-a, exogena: null}",
               "  - {sigla: MS, mdir: toy-s, exogena: null}"),
             file.path(r, "config", "topology.yaml"))
})
DECK <- local({
  d <- tempfile("psock-deck-")
  fx$prevcarga_fixture_deck(d, seed = 5, areas = c(AREAS, "X"), days = seq(PO1 - 30L, max(PORIGINS), by = "day"))
  d
})

MODULES <- c("errors", "runtime", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
             "features", "registry", "training", "inference", "build_long_output", "evaluation", "combination", "reconcile", "cache", "orchestrate", "backtest")

## roda `body` (linhas R) num R novo com a lib em globalenv, runtime fixado e `ctx` pronto. env = vetor nomeado do shell.
child <- function(body, env = character(), hooks = "NULL") {
  state <- tempfile("psock-state-"); dir.create(state)
  out <- tempfile("psock-out-", fileext = ".rds")
  s <- tempfile("psock-child-", fileext = ".R")
  writeLines(c(
    sprintf("lib <- %s; options(prevcarga.lib = lib)", deparse(lib)),
    sprintf("for (m in %s) source(file.path(lib, paste0(m, '.R')))", paste(deparse(MODULES), collapse = "")),
    "prevcarga_runtime_enforce()",
    sprintf("ctx <- prevcarga_ctx(list(`--data` = %s, `--state` = %s), %s, 'backtest')", deparse(DECK), deparse(state), deparse(ROOT)),
    "ctx$lib_dir <- lib; ctx$code_commit <- 'c0ffee'; ctx$data_hash <- 'deck-fixed'; ctx$progress <- function(done, total) NULL",
    sprintf("ctx$worker_hooks <- %s", hooks),
    sprintf("OUT <- %s", deparse(out)),
    "conns0 <- nrow(showConnections())",
    body), s)
  r <- if (length(env)) pc_with_env(as.list(env), pc_rscript(s)) else pc_rscript(s)
  unlink(s)
  r$out <- if (file.exists(out)) readRDS(out) else NULL
  r$state <- state; r$outfile <- out
  r
}

bt_body <- function(jobs, keep_going = TRUE)
  c(sprintf("res <- prevcarga_backtest(ctx, origins = lubridate::ymd(%s), cortes = '08:00', jobs = %dL, keep_going = %s)",
            paste(deparse(as.character(PORIGINS)), collapse = ""), as.integer(jobs), if (keep_going) "TRUE" else "FALSE"),
    "p <- res$paths$members",
    "preds <- lapply(p$long_parquet, function(f) arrow::read_parquet(f)$y_pred)",
    "saveRDS(list(id = paste(p$membro, p$folha, p$origem, p$corte), preds = preds, verdict = res$summary$verdict,",
    "             ok = res$summary$ok, conns = nrow(showConnections()) - conns0), OUT)")

dirty <- pc_dirty_env()
clean <- c(TZ = NA, LC_ALL = NA, OMP_NUM_THREADS = NA, OPENBLAS_NUM_THREADS = NA)
N_CELLS <- 2L * length(AREAS) * length(PORIGINS)

noseed_hook <- "list(post_runtime = function() assign('.m7_seed_apply', function(seed) NULL, envir = globalenv()))"

## ---------------------------------------------------------------------------------------------------------------------
test_that("[AC1] FORBIDDEN: y_pred identico (delta 0) entre --jobs 1 e --jobs 4, shell sujo e limpo, mesma plataforma", {
  skip_if(is.na(dirty[["LC_ALL"]]), "host sem locale UTF-8 alternativo para sujar LC_ALL")
  a <- child(bt_body(1L), env = dirty)
  b <- child(bt_body(4L), env = dirty)
  c0 <- child(bt_body(1L), env = clean)
  for (r in list(a, b, c0)) { expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n")); expect_identical(r$out$verdict, "ok") }
  expect_length(a$out$preds, N_CELLS)
  expect_identical(b$out$id, a$out$id)
  expect_identical(b$out$preds, a$out$preds)                       # identical(), nao all.equal
  expect_identical(c0$out$preds, a$out$preds)
  ## o toy realmente consome o RNG: celulas distintas dao sorteios distintos (o teste tem dentes)
  expect_false(identical(a$out$preds[[1L]], a$out$preds[[2L]]))
})

test_that("[AC1] CONTROL: worker sem set.seed (sabotagem no fixture) diverge de --jobs 1", {
  a <- child(bt_body(1L))
  b <- child(bt_body(4L), hooks = noseed_hook)
  expect_identical(b$out$verdict, "ok")
  expect_false(identical(b$out$preds, a$out$preds))
})

test_that("[AC2] cluster fechado no retorno (sem conexao orfa); --jobs 1 nao cria cluster; sem fork/env no codigo", {
  b <- child(bt_body(4L))
  expect_identical(b$out$conns, 0L)
  src <- sub("\\s*##.*$", "", readLines(file.path(lib, "orchestrate.R"), warn = FALSE))
  expect_length(grep("mclapply|makeForkCluster|mc\\.cores|fork|Sys\\.getenv|detectCores", src), 0L)
  expect_gte(length(grep("parallel::makePSOCKcluster", src)), 1L)
  ## CONTROL do grep: o padrao pega codigo com fork
  expect_length(grep("mclapply|fork", "x <- parallel::mclapply(1:2, f)"), 1L)
  ## spy: --jobs 1 => 0 chamadas a makePSOCKcluster; --jobs 2 => >= 1 (o spy tem dentes)
  spy <- "trace(parallel::makePSOCKcluster, tracer = quote(assign('SPY_N', get0('SPY_N', globalenv(), ifnotfound = 0L) + 1L, envir = globalenv())), print = FALSE)"
  calls <- function(j) {
    r <- child(c(spy, bt_body(j), "saveRDS(get0('SPY_N', globalenv(), ifnotfound = 0L), paste0(OUT, '.spy'))"))
    expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
    n <- readRDS(sub("\\.rds$", ".rds.spy", r$outfile))
    n
  }
  expect_identical(calls(1L), 0L)
  expect_gte(calls(2L), 1L)
})

## ---------------------------------------------------------------------------------------------------------------------
## orquestrador direto com run_cell sabotavel (envelope pequeno); o script-filho devolve estado, ledger e JSON do resumo
orch_body <- function(jobs, keep_going, fail = "MA|B|2026-10-06", slow = TRUE)
  c(sprintf("plan <- prevcarga_plan(ctx, lubridate::ymd(%s), '08:00')", paste(deparse(as.character(PORIGINS)), collapse = "")),
    "mk_cell <- function(FAIL, SLOW) function(cell) {          # fabrica: o closure leva FAIL/SLOW para o worker",
    "  if (SLOW && cell$folha == 'A') Sys.sleep(0.4)              # termina DEPOIS da folha B: ordem de termino != ordem do plano",
    "  if (identical(paste(cell$membro, cell$folha, format(cell$origem, '%Y-%m-%d'), sep = '|'), FAIL))",
    "    prevcarga_abort('window.history_short', where = 'fixture')",
    "  data.frame(x = 1L)",
    "}",
    sprintf("run_cell <- mk_cell(%s, %s)", deparse(fail), if (slow) "TRUE" else "FALSE"),
    sprintf("out <- prevcarga_orchestrate(ctx, plan, run_cell, jobs = %dL, keep_going = %s)", as.integer(jobs),
            if (keep_going) "TRUE" else "FALSE"),
    "fj <- file.path(ctx$state_dir, 'backtest', out$run_id, 'ledger', 'failures.jsonl')",
    "s <- out$summary",
    "js <- as.character(jsonlite::toJSON(list(ok = s$ok, total = s$total, failed = s$failed, skipped = s$skipped, verdict = s$verdict,",
    "  failures = lapply(seq_len(nrow(s$failures)), function(i) as.list(s$failures[i, c('membro', 'folha', 'classe', 'error_id', 'mensagem')]))),",
    "  auto_unbox = TRUE))",
    "st <- out$status",
    "pq <- Sys.glob(file.path(ctx$state_dir, 'backtest', out$run_id, 'members', '*', '*', '*', '*', 'long.parquet'))",
    "saveRDS(list(order = paste(st$membro, st$folha, st$origem, st$corte_hm), state = st$state, error_id = st$error_id,",
    "             mensagem = st$mensagem, json = js, jsonl = if (file.exists(fj)) readLines(fj) else character(), n_pq = length(pq),",
    "             conns = nrow(showConnections()) - conns0), OUT)")

test_that("[AC4] falha em worker sob --jobs 4 --keep-going: classe e error.id originais, 1 linha no failures.jsonl, demais celulas completam", {
  r <- child(orch_body(4L, TRUE))
  expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  o <- r$out
  expect_identical(sum(o$state == "failed"), 1L)
  expect_identical(sum(o$state == "done"), N_CELLS - 1L)
  expect_identical(o$n_pq, N_CELLS - 1L)
  expect_identical(o$error_id[o$state == "failed"], "window.history_short")
  expect_match(o$mensagem[o$state == "failed"], "^\\[prevcarga/window\\]")
  expect_length(o$jsonl, 1L)                                                  # so o pai escreve; uma linha, sem append concorrente
  row <- jsonlite::fromJSON(o$jsonl[[1L]])
  expect_identical(row$error_class, "window"); expect_identical(row$error_id, "window.history_short")
  expect_identical(row$state, "failed")
  expect_identical(o$conns, 0L)                                               # cluster fechado mesmo com falha
})

test_that("[AC4] FORBIDDEN: sem --keep-going nenhuma celula nova e despachada depois da 1a falha recolhida (CONTROL: com keep-going despacha)", {
  ff <- "MA|A|2026-10-07"                                                     # 1a celula na ordem de despacho (origem mais recente)
  a <- child(orch_body(2L, FALSE, fail = ff, slow = FALSE))
  expect_identical(sum(a$out$state == "failed"), 1L)
  expect_lte(a$out$n_pq, 1L)                                                  # so o resto do lote do cluster (2 workers) chegou a rodar
  expect_identical(sum(a$out$state == "skipped"), N_CELLS - 2L)
  s1 <- child(orch_body(1L, FALSE, fail = ff, slow = FALSE))
  expect_identical(s1$out$n_pq, 0L)
  expect_identical(sum(s1$out$state == "skipped"), N_CELLS - 1L)
  b <- child(orch_body(2L, TRUE, fail = ff, slow = FALSE))
  expect_identical(sum(b$out$state == "skipped"), 0L)
  expect_identical(b$out$n_pq, N_CELLS - 1L)
})

test_that("[AC5] status na ordem de plan$cells e resumo JSON byte-identico entre --jobs 1 e --jobs 4 (terminos fora de ordem)", {
  a <- child(orch_body(1L, TRUE)); b <- child(orch_body(4L, TRUE))
  expect_identical(b$out$order, a$out$order)
  expect_identical(b$out$state, a$out$state)
  expect_identical(b$out$json, a$out$json)
  expect_identical(sub("^\\{\"ts\":\"[^\"]*\",", "", b$out$jsonl), sub("^\\{\"ts\":\"[^\"]*\",", "", a$out$jsonl))   # ts = relogio do ctx$run_id
  expect_identical(b$out$mensagem, a$out$mensagem)
  ## a ordem e a do plano (membro, folha, origem, corte), nao a de termino
  expect_identical(a$out$order, a$out$order[order(match(a$out$order, a$out$order))])
  expect_identical(a$out$order[[1L]], paste("MA", "A", format(PORIGINS[[1L]]), "08:00"))
})

test_that("[AC3] worker novo: mesmo lib e runtime fixado in-process (TZ/locale/BLAS do pai, sobrescrevendo o shell)", {
  skip_if(is.na(dirty[["LC_ALL"]]), "host sem locale UTF-8 alternativo para sujar LC_ALL")
  body <- c(
    "probe <- function() list(tz = format(Sys.time(), '%Z'), lc = Sys.getlocale('LC_COLLATE'), blas = Sys.getenv('OPENBLAS_NUM_THREADS'),",
    "                         lib = getOption('prevcarga.lib'), has_orch = exists('prevcarga_orchestrate', mode = 'function'))",
    "st <- .m7_cluster_start(ctx, 2L)",
    "w <- parallel::clusterCall(st$cl, probe)",
    "parallel::stopCluster(st$cl)",
    "saveRDS(list(parent = probe(), workers = w, bad = is.null(st$bad)), OUT)")
  r <- child(body, env = dirty)
  expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  expect_true(r$out$bad)
  expect_identical(r$out$parent$tz, "UTC")
  for (w in r$out$workers) {
    expect_identical(w$tz, r$out$parent$tz)
    expect_identical(w$lc, r$out$parent$lc)
    expect_identical(w$blas, r$out$parent$blas)
    expect_identical(w$lib, lib)
    expect_true(w$has_orch)
  }
})

test_that("[AC3] runtime nao fixavel no worker => toda celula vira failed com [prevcarga/runtime], nada publicado, nunca silencio", {
  hook <- "list(pre_runtime = function() assign('prevcarga_runtime_enforce', function(...) prevcarga_abort('runtime.tz_unfixable', where = 'TZ'), envir = globalenv()))"
  r <- child(c(orch_body(2L, TRUE, fail = "-"), "saveRDS(c(readRDS(OUT), list(none = TRUE)), OUT)"), hooks = hook)
  expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  o <- r$out
  expect_true(all(o$state == "failed"))
  expect_identical(unique(o$error_id), "runtime.tz_unfixable")
  expect_true(all(grepl("^\\[prevcarga/runtime\\]", o$mensagem)))
  expect_identical(o$n_pq, 0L)
  expect_identical(o$conns, 0L)
  ## CONTROL: sem o hook as mesmas celulas rodam
  ok <- child(orch_body(2L, TRUE, fail = "-"))
  expect_true(all(ok$out$state == "done"))
})

test_that("[AC1] --jobs invalido (0, NA) => config (exit 8) antes de criar cluster", {
  for (j in list(0L, NA_integer_, -2L)) {
    b <- child(c(sprintf("e <- tryCatch(prevcarga_orchestrate(ctx, NULL, NULL, jobs = %s), prevcarga_error = function(e) e)", deparse(j)),
                 "saveRDS(list(id = e$id, exit = e$exit_code), OUT)"))
    expect_identical(b$out$id, "config.bad_type")
    expect_identical(b$out$exit, 8L)
  }
})
