## test-orchestrate.R -- M7.1: matriz count-agnostica, ledger, --resume, fail-fast x --keep-going, lock e gramatica dos
## verbos plan/backtest (ADR-0023, ADR-0040). Gates de PROCESSO (`Rscript --vanilla <copia>/scripts/prevcarga ...`) sobre
## instalacao-copia em tempdir com plugins-brinquedo + deck sintetico (fixture_store.R); orquestrador in-process com
## run_cell sabotavel. Tudo em tempdir (nunca data/). Cada FORBIDDEN tem o seu CONTROL. Sem variaveis de ambiente.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
lib <- file.path(pc_root(), "scripts", "lib")

fx <- new.env()
sys.source(file.path(lib, "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

TZ <- "Etc/GMT+3"
O1 <- lubridate::ymd("2026-10-01")
O2 <- lubridate::ymd("2026-10-02")
WINDOW <- "2026-10-01..2026-10-02"
AREAS <- c("A", "B")

## --- instalacao-copia: topologia {X: A, B}; 3 membros-brinquedo (agnostic, scoped, agnostic com data_lag_days 2) -------
mk_root <- function(leaves = AREAS, extra = function(r) NULL) {
  pc_install_copy(mutate = function(r) {
    bt <- list(backtest = list(protocol_default = "daily"))
    tvs <- list(
      pc_toy_variant("toy-30", run_yaml = bt, name = "toy-a"),
      pc_toy_variant("toy-60", run_yaml = bt, name = "toy-s"),
      pc_toy_variant("toy-30", run_yaml = bt, descriptor = list(data_lag_days = 2L), name = "toy-l"))
    for (tv in tvs) for (n in c(setdiff(list.files(tv), "_toy-engine")))
      file.copy(file.path(tv, n), file.path(r, "models"), recursive = TRUE)
    file.copy(file.path(tvs[[1L]], "_toy-engine"), file.path(r, "models"), recursive = TRUE)
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
                 sprintf("  X: {areas: [%s], loss: PX}", paste(leaves, collapse = ", ")),
                 "order: [X]", "members:",
                 "  - {sigla: MA, mdir: toy-a, exogena: null}",
                 "  - {sigla: MS, mdir: toy-s, exogena: null}",
                 "  - {sigla: ML, mdir: toy-l, exogena: null}"),
               file.path(r, "config", "topology.yaml"))
    extra(r)
  })
}
root_small <- mk_root()
N_MEMBERS <- 3L

mk_deck <- function(areas = AREAS, label = "start")
  local({
    d <- tempfile("m7-deck-")
    fx$prevcarga_fixture_deck(d, seed = 5, areas = areas, days = seq(O1 - 14L, O2, by = "day"), load_label = label)
    d
  })
deck <- mk_deck()
deck_end <- mk_deck(label = "end")

cli <- function(args, root = root_small, state = pc5_state_tmp(parent.frame()), data = deck, fmt = "json") {
  a <- c(args, paste0("--state=", state), if (!is.null(data)) paste0("--data=", data), paste0("--format=", fmt))
  r <- pc_cli(root, a)
  r$state <- state
  r$json <- if (identical(fmt, "json") && length(r$stdout))
    jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE) else NULL
  r
}

## --- orquestrador in-process --------------------------------------------------------------------------------------
mod <- new.env()
for (m in c("errors", "runtime", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
            "features", "registry", "training", "cache", "orchestrate")) sys.source(file.path(lib, paste0(m, ".R")), envir = mod)

ictx <- function(root = root_small, data = deck, state = pc5_state_tmp(parent.frame()), weather = NULL, label_flag = NULL) {
  a <- list("--data" = data, "--state" = state)
  if (!is.null(weather)) a[["--weather-cutoff"]] <- weather
  if (!is.null(label_flag)) a[["--load-label"]] <- label_flag
  ctx <- mod$prevcarga_ctx(a, root, "backtest")
  ctx$code_commit <- "c0ffee"
  ctx$progress <- function(done, total) NULL
  ctx
}
mkplan <- function(ctx, origins = c(O1, O2), cortes = "08:00", ...) mod$prevcarga_plan(ctx, origins, cortes, ...)
ok_cell <- function(cell) data.frame(x = 1L)

## ---------------------------------------------------------------------------------------------------------------------
test_that("[AC1] plan: total = |membros| x folhas x origens x cortes, pela CLI real, sem criar artefato", {
  r <- cli(c("plan", paste0("--origin=", WINDOW), "--corte=08:00,09:30"))
  expect_identical(r$status, 0L)
  expect_identical(r$json$verb, "plan")
  expect_true(r$json$ok)
  res <- r$json$result
  expect_identical(res$total, N_MEMBERS * length(AREAS) * 2L * 2L)
  expect_setequal(names(res$by_member), c("MA", "MS", "ML"))
  expect_identical(res$by_member$MA$split, "origin_agnostic")
  expect_identical(res$by_member$MS$split, "origin_scoped")
  expect_identical(res$by_member$MS$granularity_min, 60L)
  expect_identical(res$by_member$MA$granularity_min, 30L)
  expect_identical(res$by_member$MA$protocol, "daily")
  expect_length(list.files(r$state, recursive = TRUE, all.files = TRUE), 0L)          # nenhum plano/long/artefato/treino
  ## FORBIDDEN: enumerar models/ em vez de prevcarga_members() quebra a cardinalidade
  n_dirs <- length(setdiff(list.files(file.path(root_small, "models")), "_toy-engine"))
  expect_false(res$total == n_dirs * length(AREAS) * 2L * 2L)
  ## CONTROL: uma folha a mais na topologia soma |membros| x origens x cortes
  r2 <- cli(c("plan", paste0("--origin=", WINDOW), "--corte=08:00,09:30"), root = mk_root(c(AREAS, "C")),
            data = mk_deck(c(AREAS, "C")))
  expect_identical(r2$json$result$total - res$total, N_MEMBERS * 2L * 2L)
})

test_that("[AC1] seletores --model/--area so pela topologia; fora dela => exit 4 / 8", {
  r <- cli(c("plan", paste0("--origin=", WINDOW), "--model=toy-a", "--area=A"))
  expect_identical(r$json$result$total, 1L * 1L * 2L * 1L)
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW), "--area=ZZ"))$status, 4L)
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW), "--model=nao-existe"))$status, 8L)
})

test_that("[AC5] gramatica: backtest --dry-run == plan; janela e --corte so em backtest/plan; 08:15 => 8", {
  a <- cli(c("plan", paste0("--origin=", WINDOW), "--corte=08:00"))
  b <- cli(c("backtest", paste0("--origin=", WINDOW), "--corte=08:00", "--dry-run"))
  expect_identical(a$status, 0L); expect_identical(b$status, 0L)
  expect_identical(a$json$result, b$json$result)
  expect_identical(cli(c("forecast", "--corte=08:00"))$status, 8L)
  expect_identical(cli(c("forecast", paste0("--origin=", WINDOW)))$status, 8L)
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW), "--corte=08:15"))$status, 8L)
  expect_identical(cli(c("plan", "--corte=08:00"))$status, 8L)                      # janela obrigatoria
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW), "--protocol=weekly"))$status, 8L)
  ## CONTROL: sem scripts/lib/cli/plan.R o verbo some da descoberta (exit 8 config.usage)
  r2 <- mk_root(extra = function(r) unlink(file.path(r, "scripts", "lib", "cli", "plan.R")))
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW)), root = r2)$status, 8L)
  expect_identical(cli(c("backtest", paste0("--origin=", WINDOW), "--dry-run"), root = r2)$status, 0L)   # backtest segue de pe
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW), "--resume"))$status, 8L)                  # --resume e do backtest
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW), "--break-lock"))$status, 8L)
  ## texto ASCII
  t <- cli(c("plan", paste0("--origin=", WINDOW)), fmt = "text")
  expect_identical(t$status, 0L)
  pc_assert_ascii(t$raw, "plan text")
})

test_that("[AC6] classe x protocolo num helper unico; incompativel => contract (2)", {
  sp <- function(split, pd = "daily") list(name = "x", split = split, backtest = list(protocol_default = pd))
  expect_identical(mod$.m7_class(sp("origin_agnostic"))$protocol, "daily")
  expect_identical(mod$.m7_class(sp("origin_scoped", "retrain_harness"))$coupled_forecast, TRUE)
  expect_false(mod$.m7_class(sp("origin_agnostic"), "fixed")$trains_per_origin)
  expect_abort <- function(expr, id) {
    e <- tryCatch(expr, prevcarga_error = function(e) e)
    expect_s3_class(e, "prevcarga_error"); expect_identical(e$id, id); e
  }
  e <- expect_abort(mod$.m7_class(sp("origin_scoped"), "fixed"), "contract.protocol_incompatible")
  expect_identical(e$exit_code, 2L)
  expect_abort(mod$.m7_class(sp("origin_agnostic"), "retrain_harness"), "contract.protocol_incompatible")
  expect_abort(mod$.m7_class(list(name = "x", split = "origin_agnostic", backtest = list())), "contract.run_yaml_protocol")
  expect_abort(mod$.m7_class(list(name = "x", split = NULL)), "contract.run_yaml_split")
  ## pela CLI: --protocol fixed com membro origin_scoped aborta ANTES de qualquer celula
  r <- cli(c("plan", paste0("--origin=", WINDOW), "--protocol=fixed"))
  expect_identical(r$status, 2L)
  expect_identical(r$json$error$id, "contract.protocol_incompatible")
  expect_length(list.files(r$state, recursive = TRUE), 0L)
  ## CONTROL: so o membro agnostic com fixed monta o plano; daily monta todos
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW), "--model=toy-a", "--protocol=fixed"))$status, 0L)
  expect_identical(cli(c("plan", paste0("--origin=", WINDOW), "--protocol=daily"))$status, 0L)
})

test_that("cortes de treino vem do dates.R por membro: last_data_day = O-1-data_lag_days; train_origin pela classe", {
  p <- mkplan(ictx())
  c1 <- p$cells[membro == "MA" & folha == "A" & origem == O2]
  expect_identical(c1$last_data_day, O2 - 1L)
  expect_identical(c1$train_origin, O2 - 1L - 1L)                          # origin_agnostic: O-1-H (H=1 no run.yaml)
  cl <- p$cells[membro == "ML" & folha == "A" & origem == O2]
  expect_identical(cl$last_data_day, O2 - 3L)                              # data_lag_days 2 do descriptor
  cs <- p$cells[membro == "MS" & folha == "A" & origem == O2]
  expect_identical(cs$train_origin, O2)                                    # origin_scoped: O
  expect_identical(cs$last_data_day, O2 - 1L)
  ## protocolo fixed: treino unico na 1a origem da janela
  pf <- mkplan(ictx(), protocol = "fixed", members = mod$.m7_members(ictx(), "toy-a"))
  expect_true(all(pf$cells$train_origin == O1 - 1L - 1L))
  ## ordem canonica e colunas
  expect_identical(p$cells$membro[[1L]], "MA")
  expect_true(all(c("membro", "mdir", "exogena", "folha", "origem", "corte_hm", "split", "protocol", "train_origin",
                    "last_data_day", "cache_key", "available_until") %in% names(p$cells)))
  expect_true(all(grepl("^v_[0-9a-f]{16}$", p$cells$cache_key)))
  ## member_long e por membro x origem x corte (todas as folhas): uma chave por trio
  expect_identical(length(unique(p$cells$cache_key)), N_MEMBERS * 2L)
})

test_that("[AC7] run_id: estavel sob flags operacionais; muda com janela, corte, protocolo, membros, folhas, label, clima", {
  base <- mkplan(ictx())$run_id
  expect_match(base, "^v_[0-9a-f]{16}$")
  c2 <- ictx(); c2$format <- "text"; c2$jobs <- 4L; c2$run_id <- "outro-run"; c2$debug <- TRUE
  expect_identical(mkplan(c2)$run_id, base)
  ## CONTROL: cada ingrediente de identidade muda o run_id
  expect_false(identical(mkplan(ictx(), origins = c(O1))$run_id, base))
  expect_false(identical(mkplan(ictx(), cortes = "09:30")$run_id, base))
  expect_false(identical(mkplan(ictx(), protocol = "daily", members = mod$.m7_members(ictx(), "toy-a"))$run_id, base))
  expect_false(identical(mkplan(ictx(), members = mod$.m7_members(ictx(), "toy-a"))$run_id, base))
  expect_false(identical(mkplan(ictx(), folhas = "A")$run_id, base))
  expect_false(identical(mkplan(ictx(data = deck_end))$run_id, base))                       # load_label do manifest
  expect_false(identical(mkplan(ictx(weather = "origin_midnight_utc"))$run_id, base))
  expect_false(identical(mkplan({x <- ictx(); x$code_commit <- "outro"; x})$run_id, base))
})

test_that("run_id e cache_key carregam o lag de publicacao do clima (ADR-0024); origin_midnight_utc nao usa lag no run_id", {
  d6 <- tempfile("m7-deck6-")
  fx$prevcarga_fixture_deck(d6, seed = 5, areas = AREAS, days = seq(O1 - 14L, O2, by = "day"), load_label = "start",
                            weather_lag_hours = 6L)
  p0 <- mkplan(ictx()); p6 <- mkplan(ictx(data = d6))
  expect_false(identical(p6$run_id, p0$run_id))                                               # CONTROL: lag efetivo 6 x 0
  expect_false(any(p6$cells$cache_key %in% p0$cells$cache_key))                               # data_hash leva o lag
  expect_identical(mkplan(ictx(data = d6, weather = "origin_midnight_utc"))$run_id,          # FORBIDDEN: regra sem lag
                   mkplan(ictx(weather = "origin_midnight_utc"))$run_id)
})

test_that("[AC2] --resume: so pending/failed rodam; .key divergente e .tmp; CONTROL: apagar long.parquet recomputa so a celula", {
  ctx <- ictx()
  p <- mkplan(ctx)
  total <- nrow(p$cells)
  calls <- new.env(); calls$n <- character()
  rc <- function(cell) { calls$n <- c(calls$n, mod$.m7_cell_id(cell$membro, cell$folha, cell$origem, cell$corte_hm)); data.frame(x = 1L) }
  seen <- list()
  ctx$progress <- function(done, total) seen[[length(seen) + 1L]] <<- c(done, total)
  r1 <- mod$prevcarga_orchestrate(ctx, p, rc)
  expect_identical(r1$summary$ok, total); expect_identical(r1$summary$verdict, "ok")
  expect_length(calls$n, total)
  expect_identical(seen[[length(seen)]], c(total, total))
  ## nada a fazer: resume nao chama run_cell (contador por celula das done == 0)
  calls$n <- character()
  r2 <- mod$prevcarga_orchestrate(ctx, p, rc, resume = TRUE)
  expect_length(calls$n, 0L); expect_identical(r2$summary$ok, total)
  ## CONTROL: apagar o long.parquet de UMA celula recomputa exatamente ela
  cell <- p$cells[2L]
  unlink(file.path(mod$.m7_cell_dir(ctx, p$run_id, cell), "long.parquet"))
  mod$prevcarga_orchestrate(ctx, p, rc, resume = TRUE)
  expect_identical(calls$n, mod$.m7_cell_id(cell$membro, cell$folha, cell$origem, cell$corte_hm))
  ## FORBIDDEN: .key divergente (outro available_until / label / clima) nao conta done
  calls$n <- character()
  cell3 <- p$cells[3L]
  writeLines("v_0000000000000000", file.path(mod$.m7_cell_dir(ctx, p$run_id, cell3), ".key"))
  mod$prevcarga_orchestrate(ctx, p, rc, resume = TRUE)
  expect_identical(calls$n, mod$.m7_cell_id(cell3$membro, cell3$folha, cell3$origem, cell3$corte_hm))
  ## plano com outro available_until/label/clima: chave esperada muda => nada e done
  for (alt in list(ictx(data = deck_end), ictx(weather = "origin_midnight_utc"))) {
    pa <- mkplan(alt)
    expect_false(any(pa$cells$cache_key %in% p$cells$cache_key))
  }
  p9 <- mkplan(ictx(), cortes = "09:30")
  expect_false(any(p9$cells$cache_key %in% p$cells$cache_key))
  ## .tmp deixado por kill e ignorado
  calls$n <- character()
  cell4 <- p$cells[4L]
  d4 <- mod$.m7_cell_dir(ctx, p$run_id, cell4)
  unlink(file.path(d4, c("long.parquet", ".key")))
  writeLines("lixo", file.path(d4, "long.parquet.tmp"))
  expect_identical(mod$.m7_cell_state(ctx, p$run_id, cell4), "pending")
  mod$prevcarga_orchestrate(ctx, p, rc, resume = TRUE)
  expect_identical(calls$n, mod$.m7_cell_id(cell4$membro, cell4$folha, cell4$origem, cell4$corte_hm))
  expect_true(file.exists(file.path(d4, "long.parquet")))
})

test_that("[AC2] resume apos falha parcial: so as failed rodam e o progresso termina em total/total", {
  ctx <- ictx(); p <- mkplan(ctx); total <- nrow(p$cells)
  bad <- function(cell) cell$origem == O2 && cell$membro == "MA"
  rc1 <- function(cell) if (bad(cell)) stop("[prevcarga/window] historico curto") else data.frame(x = 1L)
  r1 <- mod$prevcarga_orchestrate(ctx, p, rc1, keep_going = TRUE)
  nbad <- nrow(p$cells[origem == O2 & membro == "MA"])
  expect_identical(r1$summary$failed, nbad)
  calls <- new.env(); calls$n <- 0L
  seen <- list(); ctx$progress <- function(done, total) seen[[length(seen) + 1L]] <<- c(done, total)
  r2 <- mod$prevcarga_orchestrate(ctx, p, function(cell) { calls$n <- calls$n + 1L; data.frame(x = 1L) }, resume = TRUE)
  expect_identical(calls$n, nbad)
  expect_identical(r2$summary$verdict, "ok")
  expect_identical(seen[[length(seen)]], c(total, total))
})

test_that("[AC3] fail-fast x --keep-going: observaveis diferem; exit 14; failures.jsonl so do pai", {
  boom <- function(cell) if (cell$origem == O1 && cell$membro == "MA" && cell$folha == "A") stop("[prevcarga/window] historico curto") else data.frame(x = 1L)
  ctx1 <- ictx(); p <- mkplan(ctx1); total <- nrow(p$cells)
  r1 <- mod$prevcarga_orchestrate(ctx1, p, boom)
  expect_identical(r1$summary$failed, 1L)
  expect_gt(r1$summary$skipped, 0L)
  expect_identical(r1$summary$ok + 1L + r1$summary$skipped, total)
  expect_identical(r1$summary$verdict, "partial")
  ctx2 <- ictx(); p2 <- mkplan(ctx2)
  r2 <- mod$prevcarga_orchestrate(ctx2, p2, boom, keep_going = TRUE)
  expect_identical(r2$summary$failed, 1L); expect_identical(r2$summary$skipped, 0L)
  expect_identical(r2$summary$ok, total - 1L)
  expect_false(identical(r1$summary$ok, r2$summary$ok))                                  # os observaveis DIFEREM
  f <- r2$summary$failures
  expect_identical(c(f$membro, f$folha), c("MA", "A"))
  expect_match(f$mensagem, "[prevcarga/window]", fixed = TRUE)
  expect_true(all(c("membro", "folha", "origem", "available_until", "classe", "mensagem") %in% names(f)))
  lines <- readLines(mod$.m7_failures_path(ctx2, p2$run_id))
  expect_length(lines, 1L)
  j <- jsonlite::fromJSON(lines[[1L]])
  expect_identical(j$state, "failed"); expect_identical(j$error_class, "window"); expect_identical(j$attempt, 1L)
  ## o skipped nunca escreve linha de falha
  expect_length(readLines(mod$.m7_failures_path(ctx1, p$run_id)), 1L)
  ## CONTROL: tentativa seguinte do mesmo pedido incrementa attempt
  mod$prevcarga_orchestrate(ctx2, p2, boom, keep_going = TRUE, resume = TRUE)
  l2 <- readLines(mod$.m7_failures_path(ctx2, p2$run_id))
  expect_identical(jsonlite::fromJSON(l2[[2L]])$attempt, 2L)
})

test_that("[AC3] pela CLI: celulas sem runner falham classificadas; exit 14 e resumo; fail-fast x --keep-going", {
  w <- c("backtest", paste0("--origin=", WINDOW), "--corte=08:00")
  total <- N_MEMBERS * length(AREAS) * 2L
  r1 <- cli(w)
  expect_identical(r1$status, 14L)
  expect_false(r1$json$ok)
  expect_identical(r1$json$error$id, "partial.backtest_delivery")
  expect_identical(r1$json$error$class, "partial")
  s1 <- r1$json$result$summary
  expect_identical(s1$total, total); expect_identical(s1$ok, 0L)
  expect_identical(s1$failed, 1L); expect_identical(s1$skipped, total - 1L)
  expect_match(r1$json$error$where, sprintf("ok=0/%d", total), fixed = TRUE)
  rid <- r1$json$result$run_id
  expect_length(readLines(file.path(r1$state, "backtest", rid, "ledger", "failures.jsonl")), 1L)
  expect_false(dir.exists(file.path(r1$state, "backtest", rid, "run.lock")))             # liberado no on.exit
  r2 <- cli(c(w, "--keep-going"))
  expect_identical(r2$status, 14L)
  expect_identical(r2$json$result$summary$failed, total); expect_identical(r2$json$result$summary$skipped, 0L)
  expect_length(readLines(file.path(r2$state, "backtest", rid, "ledger", "failures.jsonl")), total)
  expect_identical(r1$json$result$run_id, r2$json$result$run_id)
  ## texto: resumo duro em stderr, ASCII
  t <- cli(w, fmt = "text")
  expect_identical(t$status, 14L)
  expect_true(any(grepl(sprintf("backtest ok=0/%d", total), t$stderr, fixed = TRUE)))
  expect_true(any(grepl("falha MA A", t$stderr, fixed = TRUE)))
  pc_assert_ascii(t$raw, "backtest text")
})

test_that("[AC4] lock: run.lock/ ocupado => exit 13 sem tocar ledger/plano; --break-lock retoma (CONTROL)", {
  w <- c("backtest", paste0("--origin=", WINDOW), "--corte=08:00")
  st <- pc5_state_tmp()
  rid <- cli(c("plan", paste0("--origin=", WINDOW), "--corte=08:00"), state = st)$json$result$run_id
  lock <- file.path(st, "backtest", rid, "run.lock")
  dir.create(lock, recursive = TRUE)
  r <- cli(w, state = st)
  expect_identical(r$status, 13L)
  expect_identical(r$json$error$id, "state.lock_held")
  expect_identical(r$json$error$class, "state")
  expect_match(r$json$error$hint, "--break-lock", fixed = TRUE)
  expect_false(file.exists(file.path(st, "backtest", rid, "plan.json")))
  expect_false(dir.exists(file.path(st, "backtest", rid, "ledger")))
  expect_true(dir.exists(lock))
  ## CONTROL: --break-lock retoma e a execucao segue
  r2 <- cli(c(w, "--break-lock"), state = st)
  expect_identical(r2$status, 14L)
  expect_true(file.exists(file.path(st, "backtest", rid, "plan.json")))
  ## in-process: owner.json durante a execucao; lock criado por dir.create exclusivo (sem pid-file solto), liberado ao fim
  ctx <- ictx(); p <- mkplan(ctx)
  seen <- new.env(); seen$owner <- NULL; seen$files <- NULL
  mod$prevcarga_orchestrate(ctx, p, function(cell) {
    lk <- file.path(mod$.m7_run_dir(ctx, p$run_id), "run.lock")
    seen$owner <- jsonlite::read_json(file.path(lk, "owner.json")); seen$files <- list.files(lk, all.files = TRUE, no.. = TRUE)
    data.frame(x = 1L)
  })
  expect_identical(seen$owner$pid, Sys.getpid())
  expect_identical(seen$files, "owner.json")
  expect_false(dir.exists(file.path(mod$.m7_run_dir(ctx, p$run_id), "run.lock")))
})

test_that("[AC7] --resume com plan.json de run_id diferente aborta contract.plan_run_id_mismatch", {
  ctx <- ictx(); p <- mkplan(ctx)
  d <- mod$.m7_run_dir(ctx, p$run_id)
  dir.create(d, recursive = TRUE)
  jsonlite::write_json(list(run_id = "v_ffffffffffffffff"), file.path(d, "plan.json"), auto_unbox = TRUE)
  e <- tryCatch(mod$prevcarga_orchestrate(ctx, p, ok_cell, resume = TRUE), prevcarga_error = function(e) e)
  expect_identical(e$id, "contract.plan_run_id_mismatch"); expect_identical(e$exit_code, 2L)
  expect_false(dir.exists(file.path(d, "run.lock")))                                      # lock liberado mesmo no abort
  ## CONTROL: sem --resume o plano e regravado
  mod$prevcarga_orchestrate(ctx, p, ok_cell)
  expect_identical(jsonlite::read_json(file.path(d, "plan.json"))$run_id, p$run_id)
})

test_that("run_cell que nao devolve LONG falha a celula; jobs > 1 e do M7.6", {
  ctx <- ictx(); p <- mkplan(ctx)
  r <- mod$prevcarga_orchestrate(ctx, p, function(cell) NULL, keep_going = TRUE)
  expect_identical(r$summary$failed, nrow(p$cells))
  expect_true(all(r$status$error_id == "contract.run_cell_result"))
  e <- tryCatch(mod$prevcarga_orchestrate(ictx(), p, ok_cell, jobs = 2L), prevcarga_error = function(e) e)
  expect_identical(e$id, "todo.not_implemented")
})

test_that("config: --origin A..B so em backtest/plan; corte_default na grade de 30 min com default 08:00", {
  r <- mod$prevcarga_parse_origin_range("2025-01-01..2025-01-07", "plan")
  expect_identical(r$from, lubridate::ymd("2025-01-01")); expect_identical(r$to, lubridate::ymd("2025-01-07"))
  expect_identical(mod$prevcarga_parse_origin_range("2025-01-01", "backtest")$to, lubridate::ymd("2025-01-01"))
  for (v in c("forecast", "train-models")) {
    e <- tryCatch(mod$prevcarga_parse_origin_range("2025-01-01..2025-01-07", v), prevcarga_error = function(e) e)
    expect_identical(e$id, "config.bad_origin"); expect_identical(e$exit_code, 8L)
  }
  expect_error(mod$prevcarga_parse_origin_range("2025-01-07..2025-01-01", "plan"), class = "prevcarga_error")
  yf <- function(l) { f <- tempfile(fileext = ".yaml"); writeLines(l, f); f }
  expect_identical(mod$prevcarga_config_load(yf("schema_version: 1"))$backtest$corte_default, "08:00")
  expect_identical(mod$prevcarga_config_load(file.path(lib, "tests", "fixtures", "m7", "config.yaml"))$backtest$corte_default, "08:00")
  e <- tryCatch(mod$prevcarga_config_load(yf(c("schema_version: 1", "backtest:", "  corte_default: '08:15'"))),
                prevcarga_error = function(e) e)
  expect_identical(e$id, "config.bad_type")
  expect_identical(mod$prevcarga_config_load(yf(c("schema_version: 1", "backtest:", "  corte_default: '09:30'")))$backtest$corte_default, "09:30")
})

test_that("catalogo distribuido errors/orchestrate.tsv: ids, classes/exit, ASCII e LF", {
  cat <- mod$prevcarga_errors_load(dir = file.path(lib, "errors"), refresh = TRUE)
  want <- c(partial.backtest_delivery = "partial", state.lock_held = "state", contract.protocol_incompatible = "contract",
            contract.run_yaml_protocol = "contract", contract.plan_run_id_mismatch = "contract")
  for (id in names(want)) expect_identical(cat$class[cat$id == id], unname(want[[id]]), info = id)
  expect_identical(unname(mod$prevcarga_exit_codes[["partial"]]), 14L)
  expect_identical(unname(mod$prevcarga_exit_codes[["state"]]), 13L)
  raw <- readBin(file.path(lib, "errors", "orchestrate.tsv"), "raw", 1e5)
  expect_true(all(as.integer(raw) < 128L)); expect_false(13L %in% as.integer(raw))
})

test_that("[AC6][AC7] greps: sem lista inline de modelos, sem literal de contagem/atraso em orchestrate.R, sem env", {
  src <- readLines(file.path(lib, "orchestrate.R"), warn = FALSE)
  code <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("c\\(\"holt-winters\"|refit_per_origin|random-forest|lgbm|dessem", code)))
  expect_false(any(grepl("\\b192\\b|\\b8L\\b|\\b5L\\b|== ?24\\b", code)))
  expect_false(any(grepl("Sys.getenv|Sys.setenv|Sys.time|Sys.Date|lubridate::now|quit\\(|mclapply|system2|symlink|Sys.readlink", code)))
  ## CONTROL: o grep acusa uma lista inline plantada
  expect_true(grepl("c\\(\"holt-winters\"", "x <- c(\"holt-winters\", \"lgbm\")"))
})
