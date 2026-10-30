## test-forecast_compose.R -- prevcarga_forecast (M6.4): a rodada operacional so COMPOE os modulos dos outros epicos.
## Deck sintetico (fixture_store.R) + 3 membros reais (2 origin_agnostic + 1 origin_scoped) com espioes em
## prevcarga_infer_cell / prevcarga_train; perfil k-hat e pesos da combina treinados no setup (nunca por forecast.R).
## Cada FORBIDDEN (delta 0) tem o seu CONTROL. Tudo em tempdir; sem variaveis de ambiente.
## [DEFERRED M6.6/M6.11] o ponta a ponta com os 8 membros reais roda em processo filho (test-forecast_e2e.R).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("ff-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)

## copia do estado preparado (perfil k-hat, pesos, artefatos) para um teste; `ctx` com overrides
fx_copy <- function(deck = DECK, topo = TOPO, .frame = parent.frame(), ...) {
  st <- withr::local_tempdir("ff-state-", .local_envir = .frame)
  file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
  m6f_ctx(deck, st, topo, list(...))
}

## troca binding do env de teste e restaura ao fim do teste
mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({
    nm <- n; old <- env[[nm]]
    withr::defer(assign(nm, old, envir = env), envir = .frame)
  })
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
}

expect_pc <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (!is.null(cond)) { expect_identical(cond$id, id); expect_identical(cond$exit_code, as.integer(exit)) }
  invisible(cond)
}

new_log <- function() { l <- new.env(); l$infer <- list(); l$train <- list(); l }
spies <- function(log, perturb = list(), fail = character(), .frame = parent.frame()) {
  mock(prevcarga_infer_cell = m6f_infer_spy(env, log, perturb, fail), prevcarga_train = m6f_train_spy(env, log), .frame = .frame)
}
run <- function(ctx, ...) env$prevcarga_forecast(ctx, skip_validate = TRUE, ...)
tree <- function(d) {
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
  stats::setNames(vapply(f, function(x) digest::digest(file = file.path(d, x), algo = "sha256"), ""), f)
}
no_delivery_dirs <- function(ctx) {
  expect_false(dir.exists(file.path(ctx$state_dir, "forecasts")))
  expect_false(dir.exists(file.path(ctx$state_dir, "scratch")))
}
src <- function() paste(readLines(file.path(pc_root(), "scripts", "lib", "forecast.R"), warn = FALSE), collapse = "\n")

test_that("[AC1] forecast compoe: reconcile_apply 1x (eval FALSE, khat_version NULL), train/fit nunca; result$khat do reconcile", {
  ctx <- fx_copy(); log <- new_log(); spies(log)
  calls <- new.env(); calls$apply <- list(); calls$bad <- 0L; calls$rec <- NULL
  real <- env$prevcarga_reconcile_apply
  mock(prevcarga_reconcile_apply = function(long_leaves, ctx, subsystems = "all", khat_version = NULL, eval = FALSE) {
         calls$apply[[length(calls$apply) + 1L]] <- list(subsystems = subsystems, khat_version = khat_version, eval = eval,
                                                           leaves = names(long_leaves))
         calls$rec <- real(long_leaves, ctx, subsystems, khat_version, eval)
       },
       prevcarga_reconcile_train = function(...) { calls$bad <- calls$bad + 1L; stop("proibido") },
       prevcarga_khat_fit = function(...) { calls$bad <- calls$bad + 1L; stop("proibido") },
       prevcarga_combina_fit = function(...) { calls$bad <- calls$bad + 1L; stop("proibido") })
  r <- run(ctx)
  expect_length(calls$apply, 1L)
  a <- calls$apply[[1L]]
  expect_false(a$eval); expect_null(a$khat_version); expect_identical(a$subsystems, M6F_SS)
  expect_identical(a$leaves, M6F_LEAVES)
  expect_identical(calls$bad, 0L)
  k <- calls$rec$khat
  expect_identical(vapply(r$result$khat, function(x) x$ss, ""), names(k))
  expect_identical(vapply(r$result$khat, function(x) x$versao, ""), unname(vapply(k, function(x) x$versao, "")))
  expect_identical(vapply(r$result$khat, function(x) x$khat_origin, ""), unname(vapply(k, function(x) format(x$khat_origin), "")))
  expect_identical(vapply(r$result$khat, function(x) x$khat_age_days, 0L), unname(vapply(k, function(x) x$khat_age_days, 0L)))
  expect_identical(r$result$cobertura_descartada, calls$rec$cobertura_descartada)
  expect_identical(r$result$report, list(status = "skipped", path = NULL))
  ## SIN + subsistemas entregues; o infer so foi chamado para os 3 membros x 3 folhas
  expect_setequal(vapply(r$result$arquivos, function(x) x$path, ""), c("SIN.parquet", "subsistemas/SS1.parquet", "subsistemas/SS2.parquet"))
  expect_length(log$infer, 9L)
  ## grep: forecast.R nao fita, nao treina o reconcile, nao promove, nao le o registry do k-hat
  s <- src()
  for (tok in c("khat_fit", "reconcile_train", "combina_fit", "registry_promote", "artifacts/reconcile"))
    expect_false(grepl(tok, s, fixed = TRUE), info = tok)
  ## CONTROL: o proprio fixture treina (o grep tem dente): o helper de setup chama reconcile_train
  expect_true(grepl("reconcile_train", paste(readLines(file.path(testthat::test_path(), "helper-m6-fixtures.R")), collapse = "\n"), fixed = TRUE))
})

test_that("[AC2] dois perfis k-hat => duas versoes/chaves preservadas; mesmo perfil => sha256 identical (FORBIDDEN) ", {
  ctx <- fx_copy(); log <- new_log(); spies(log)
  r1 <- run(ctx)
  t1 <- tree(r1$result$saida)
  r1b <- run(ctx)                                             # mesmo perfil: no-op na chave
  expect_identical(r1b$result$versao, r1$result$versao)
  expect_true(r1b$result$ja_publicado)
  expect_identical(tree(r1b$result$saida), t1)
  ## CONTROL: outro perfil latest (janela diferente) => outra versao; as duas chaves ficam
  suppressWarnings(env$prevcarga_reconcile_train(ctx, O, "SS1", window_days = 25L))
  r2 <- run(ctx)
  expect_false(identical(r2$result$versao, r1$result$versao))
  expect_false(identical(r2$result$saida, r1$result$saida))
  expect_true(dir.exists(r1$result$saida)); expect_true(dir.exists(r2$result$saida))
  expect_identical(tree(r1$result$saida), t1)
  expect_false(r2$result$ja_publicado)
})

test_that("[AC3] khat_origin = O+1 => exit 15; sem latest => exit 13; nenhum diretorio de entrega; CONTROL khat_origin = O entrega", {
  ctx <- fx_copy(); log <- new_log(); spies(log)
  unlink(env$prevcarga_khat_key("SS1", ctx), recursive = TRUE)  # SS1 so tem perfil posterior a O (sem versao ajustada em O)
  m6f_profile(env, ctx, "SS1", O + 1L)
  c1 <- expect_pc(run(ctx), "honesty.khat_after_origin", 15L)
  expect_identical(c1$class_name, "honesty")
  no_delivery_dirs(ctx)
  ## SS2 sem latest: state.khat_missing (o SS1 de O+1 ja foi corrigido por um latest de O)
  ctx2 <- fx_copy()
  unlink(file.path(env$prevcarga_khat_key("SS2", ctx2), "latest.txt"))
  c2 <- expect_pc(run(ctx2), "state.khat_missing", 13L)
  no_delivery_dirs(ctx2)
  ## CONTROL: khat_origin = O entrega, exit 0
  ctx3 <- fx_copy()
  r <- run(ctx3)
  expect_identical(vapply(r$result$khat, function(x) x$khat_origin, ""), rep(format(O), 2L))
  expect_true(dir.exists(r$result$saida))
})

test_that("[AC4] determinismo: 2 rodadas = sha256 identical e no-op; perturbar 1 membro move o y_pred e a chave", {
  ctx <- fx_copy(); log <- new_log(); spies(log)
  r1 <- run(ctx, incluir_membros = TRUE, niveis = "tudo")
  r2 <- run(ctx, incluir_membros = TRUE, niveis = "tudo")
  expect_true(r2$result$ja_publicado)
  expect_identical(r2$result$versao, r1$result$versao)
  expect_identical(vapply(r2$result$arquivos, function(a) a$sha256, ""), vapply(r1$result$arquivos, function(a) a$sha256, ""))
  expect_identical(r2$result$saida, r1$result$saida)
  ## todos os arquivos do scratch (nao so os da entrega) sao identical entre as rodadas: mesma run_id, recriado
  sc <- file.path(ctx$state_dir, "scratch")
  s1 <- tree(sc)
  run(ctx, incluir_membros = TRUE, niveis = "tudo")
  expect_identical(tree(sc), s1)
  ## CONTROL: perturbar 1 membro (mesmos artefatos) muda o y_pred composto, a versao e a chave
  spies(log, perturb = list(RF = 25))
  r3 <- run(ctx, incluir_membros = TRUE, niveis = "tudo")
  expect_false(identical(r3$result$versao, r1$result$versao))
  expect_false(identical(r3$result$saida, r1$result$saida))
  p <- function(r) arrow::read_parquet(file.path(r$result$saida, "folhas", "A1.parquet"))$y_pred
  expect_false(isTRUE(all.equal(p(r3), p(r1))))
  m <- function(r, sg) arrow::read_parquet(file.path(r$result$saida, "membros", sg, "A1.parquet"))$y_pred
  expect_identical(m(r3, "LGBM"), m(r1, "LGBM"))                      # o membro nao perturbado e identical
  expect_false(identical(m(r3, "RF"), m(r1, "RF")))
  ## grep: nenhum relogio na rodada
  s <- src()
  for (tok in c("Sys.time", "Sys.Date", "now(", "Sys.getenv"))
    expect_false(grepl(tok, s, fixed = TRUE), info = tok)
})

test_that("[AC5] origin_scoped treina so sem artefato de O (1x por folha); origin_agnostic ausente => exit 13 sem treino", {
  ctx <- fx_copy(); log <- new_log()
  ## origin_agnostic sem artefato: aborta exit 13 e NUNCA treina (RF e LGBM nao entram no train)
  spies(log, fail = "RF")
  expect_pc(run(ctx), "state.artifact_missing", 13L)
  expect_length(Filter(function(x) x$member %in% c("RF", "LGBM"), log$train), 0L)
  expect_length(log$train, 0L)                                          # RF e o 1o membro: nada treinou antes
  ## 1o deck: HW treinado 1x por folha, com a origem O
  log <- new_log(); spies(log)
  r1 <- run(ctx)
  expect_length(log$train, length(M6F_LEAVES))
  expect_true(all(vapply(log$train, function(x) identical(x$member, "HW") && identical(x$origin, O), NA)))
  spec <- env$prevcarga_model_spec("holt-winters", pc_root())
  tc <- env$prevcarga_train_cut(spec, O, 0L)                           # os cortes quem deriva e o dates.R: O e O-1
  expect_identical(tc$train_origin, O); expect_identical(tc$last_data_day, O - 1L)
  v1 <- vapply(Filter(function(x) identical(x$member, "HW"), log$infer), function(x) x$version, "")
  expect_length(v1, length(M6F_LEAVES)); expect_true(all(grepl("^v_[0-9a-f]{16}$", v1)))
  ## 2o deck do mesmo O (available_until maior): sem re-treino, mesmo v_<hash>, so o infer muda
  deck2 <- m6f_deck(env, file.path(base, "deck2"), until = "09:30")
  ctx2 <- ctx; ctx2$data_dir <- deck2
  m6f_prepare_origin(env, ctx2)                                          # k-hat/pesos do corte novo (setup)
  log2 <- new_log(); spies(log2)
  r2 <- run(ctx2)
  expect_length(log2$train, 0L)
  v2 <- vapply(Filter(function(x) identical(x$member, "HW"), log2$infer), function(x) x$version, "")
  expect_identical(sort(v2), sort(v1))
  expect_false(identical(r2$result$available_until, r1$result$available_until))
  expect_false(identical(r2$result$saida, r1$result$saida))
  ## artefato oficial sincronizado de O (mesmo layout, copiado de fora) tambem evita o treino; CONTROL: sem ele treina
  ctx3 <- fx_copy(); log3 <- new_log(); spies(log3)
  mem <- env$prevcarga_members(path = ctx3$topology_path, root = ctx3$root)
  for (a in M6F_LEAVES) m6f_fake_artifact(env, mem[mem$sigla == "HW", ], a, O, ctx3, tag = "oficial")
  run(ctx3)
  expect_length(log3$train, 0L)
  ctx4 <- fx_copy(); log4 <- new_log(); spies(log4)
  for (a in M6F_LEAVES) m6f_fake_artifact(env, mem[mem$sigla == "HW", ], a, O - 1L, ctx4, tag = "ontem")   # artefato de outra origem
  run(ctx4)
  expect_length(log4$train, length(M6F_LEAVES))
})

test_that("[AC6] origem default = dia do ultimo intervalo de carga do deck (sem relogio); origem explicita vence", {
  ctx <- fx_copy(); log <- new_log(); spies(log)
  r <- run(ctx)
  expect_identical(r$result$origem, "2026-01-15")
  expect_match(r$result$saida, "/forecasts/2026-01-15/20260115T0900/[0-9a-f]{12}$")
  ## deck que termina em 01-14: o default e 01-14 (ultimo dia de carga); origem explicita 01-15 = corte da manha (O 00:00)
  deck3 <- m6f_deck(env, file.path(base, "deck3"), origin = O - 1L)
  ctx3 <- fx_copy(deck = deck3)
  m6f_prepare_origin(env, ctx3, c(O - 1L, O))
  r3 <- run(ctx3)
  expect_identical(r3$result$origem, "2026-01-14")
  expect_match(r3$result$saida, "/forecasts/2026-01-14/20260114T0900/")
  r4 <- run(ctx3, origin = O)
  expect_identical(r4$result$origem, "2026-01-15")
  expect_match(r4$result$saida, "/forecasts/2026-01-15/20260115T0000/")
  ## origem em texto vale; ctx$origin (flag) e o default quando o argumento e NULL
  expect_identical(run(ctx3, origin = "2026-01-14")$result$origem, "2026-01-14")
  ctx3$origin <- O
  expect_identical(run(ctx3)$result$origem, "2026-01-15")
  expect_identical(run(ctx3, origin = O - 1L)$result$origem, "2026-01-14")      # o argumento vence o flag
  ## grep: nenhum relogio (a origem nunca vem de Sys.Date/Sys.time/now)
  for (tok in c("Sys.time", "Sys.Date", "now("))
    expect_false(grepl(tok, src(), fixed = TRUE), info = tok)
})

test_that("[AC7] reach-control: folha solta sem reconcile, SS1 so, todas = SIN + subsistemas; CONTROL topologia; area fora => exit 4", {
  ctx <- fx_copy(); log <- new_log(); spies(log)
  n_apply <- 0L
  real <- env$prevcarga_reconcile_apply
  mock(prevcarga_reconcile_apply = function(...) { n_apply <<- n_apply + 1L; real(...) })
  paths <- function(r) vapply(r$result$arquivos, function(a) a$path, "")
  r1 <- run(ctx, areas = "A1")                                         # A1 sozinha nao fecha SS1: solta, entregue sem reconcile
  expect_identical(n_apply, 0L)
  expect_identical(r1$result$stray, "A1"); expect_identical(r1$result$covered, character())
  expect_identical(paths(r1), "folhas/A1.parquet")
  expect_length(r1$result$khat, 0L)
  r2 <- run(ctx, areas = c("A1", "A2"))                                # fecha SS1 (SS2 aberto): so SS1, sem SIN
  expect_identical(n_apply, 1L)
  expect_identical(r2$result$covered, c("A1", "A2")); expect_identical(r2$result$stray, character())
  expect_identical(paths(r2), "subsistemas/SS1.parquet")
  r3 <- run(ctx)                                                       # todas: SIN + subsistemas
  expect_identical(n_apply, 2L)
  expect_setequal(paths(r3), c("SIN.parquet", "subsistemas/SS1.parquet", "subsistemas/SS2.parquet"))
  expect_identical(r3$result$covered, M6F_LEAVES); expect_identical(r3$result$stray, character())
  r4 <- run(ctx, areas = c("A1", "B1"))                                # mistura: SS2 fecha (B1), A1 solta
  expect_identical(r4$result$covered, "B1"); expect_identical(r4$result$stray, "A1")
  expect_setequal(paths(r4), c("subsistemas/SS2.parquet", "folhas/A1.parquet"))
  ## CONTROL: mover A2 para SS2 no topology.yaml muda covered/stray da MESMA chamada (areas = A1, A2)
  dir2 <- withr::local_tempdir("ff-topo-")
  ctx5 <- ctx; ctx5$topology_path <- m6f_topology_file(dir2, move_a2 = TRUE)
  r5 <- run(ctx5, areas = c("A1", "A2"))
  expect_identical(r5$result$covered, "A1"); expect_identical(r5$result$stray, "A2")
  expect_false(identical(r5$result$stray, r2$result$stray))
  ## area fora da topologia: exit 4 antes do infer
  log6 <- new_log(); spies(log6)
  expect_pc(run(ctx, areas = c("A1", "ZZ")), "topology.area_undeclared", 4L)
  expect_length(log6$infer, 0L)
})

test_that("[AC8] buraco no dia O: gate-0 exit 3 antes do infer e sem scratch; skip_validate pula a validacao, o corte vem do leitor", {
  ## deck com buraco no meio do dia O (a validacao e o corte do leitor o recusam: completude)
  holed <- env$prevcarga_fixture_drop_slots(DECK, "A1", O, "03:00", 4L)
  ctx <- fx_copy(deck = holed); log <- new_log(); spies(log)
  c1 <- expect_pc(env$prevcarga_forecast(ctx), "completude.origin_day_gap", 3L)
  expect_identical(c1$class_name, "completude")
  expect_length(log$infer, 0L); expect_length(log$train, 0L)
  expect_false(dir.exists(file.path(ctx$state_dir, "scratch"))); expect_false(dir.exists(file.path(ctx$state_dir, "forecasts")))
  ## com skip_validate o leitor ainda deriva o corte: o buraco continua abortando exit 3 (nunca um corte inventado)
  expect_pc(run(ctx), "completude.origin_day_gap", 3L)
  expect_length(log$infer, 0L)
  ## CONTROL: deck limpo + skip_validate = TRUE nao chama a validacao, e o available_until sai do leitor
  n_val <- 0L
  real <- env$prevcarga_validate_deck
  mock(prevcarga_validate_deck = function(...) { n_val <<- n_val + 1L; real(...) })
  ctx2 <- fx_copy(); log2 <- new_log(); spies(log2)
  r <- run(ctx2)
  expect_identical(n_val, 0L)
  expect_identical(r$result$available_until, "2026-01-15T09:00")
  expect_length(log2$infer, 9L)
  ## CONTROL: sem skip_validate a validacao roda (o deck curto reprova na janela de historico: aborta antes do infer)
  log3 <- new_log(); spies(log3)
  expect_error(env$prevcarga_forecast(fx_copy()), class = "prevcarga_error")
  expect_gt(n_val, 0L)
  expect_length(log3$infer, 0L)
})
