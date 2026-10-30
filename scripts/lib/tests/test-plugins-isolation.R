## test-plugins-isolation.R -- isolamento e portabilidade CONJUNTOS dos 5 plugins (M5.11; AC40, AC41). So VERIFICA.
## Os 5 plugins na MESMA sessao `Rscript --vanilla` limpa (filho; JSON em tempdir), treino 1x cada pela fachada prevcarga_train
## sobre o deck de fixture_store.R com params rapidos, sem tocar globalenv()/search()/.Random.seed (FORBIDDEN Delta=0). CONTROLs:
## sementes 123 e 124, plugin-fixture que suja o globalenv, artefato com campo de relogio. Sem variavel de ambiente (o ENV sujo
## do AC41 e injetado so no filho, por withr). Linux e Windows.

PCC_FORMAL_BAD <- "params"

## o cenario de isolamento (5 treinos) roda 1x por arquivo e e reaproveitado pelos dois testes que o leem
.pcc_iso <- local({
  res <- NULL
  function() {
    if (is.null(res)) res <<- pcc_run_child("isolation")
    res
  }
})

test_that("[ac-40] 5 plugins na mesma sessao limpa: globalenv, search() e .Random.seed identicos antes/depois (FORBIDDEN Delta=0)", {
  r <- .pcc_iso()
  expect_identical(r$status, 0L, info = paste(utils::tail(r$stderr, 12L), collapse = "\n"))
  expect_false(is.null(r$json))
  j <- r$json
  expect_identical(j$before$globalenv, j$after$globalenv)
  expect_identical(j$before$search, j$after$search)
  expect_identical(j$before$seed, j$after$seed)
  expect_length(j$before$seed, 626L)                             # .Random.seed apos set.seed(7)
  expect_identical(unlist(j$before$seed)[1:2], c(10403L, 624L))    # kind Mersenne-Twister/Inversion/Rejection; posicao inicial
  expect_length(j$mdirs, 5L)
  expect_setequal(unlist(j$mdirs), pcc_all_plugins())
  ## CONTROL do proprio instrumento: um assign no globalenv muda o snapshot
  s0 <- pcc_snapshot()
  assign("pcc_leak_probe", 1, envir = globalenv())
  withr::defer(rm("pcc_leak_probe", envir = globalenv()))
  expect_false(identical(s0$globalenv, pcc_snapshot()$globalenv))
})

test_that("[ac-40] os 5 descriptors vivem em envs distintos; os motores de dessem-linear e dessem-regdin sao instancias distintas", {
  r <- .pcc_iso()
  expect_identical(r$status, 0L, info = paste(utils::tail(r$stderr, 12L), collapse = "\n"))
  j <- r$json
  expect_equal(length(unique(unlist(j$descriptor_env))), 5L)
  expect_equal(length(unique(unlist(j$plugin_env))), 5L)
  eng <- j$engine_env
  expect_false(is.null(eng[["dessem-linear"]])); expect_false(is.null(eng[["dessem-regdin"]]))
  expect_false(identical(eng[["dessem-linear"]], eng[["dessem-regdin"]]))
  ## so os dois DESSEM tem motor
  for (m in c("random-forest", "lgbm", "holt-winters")) expect_true(is.null(eng[[m]]) || is.na(eng[[m]]), info = m)
})

test_that("[ac-40][CONTROL] RF com sementes 123 e 124 no spec: max|Delta| > 1e-5; 123 x 123 e Delta = 0", {
  r <- pcc_run_child("seeds")
  expect_identical(r$status, 0L, info = paste(utils::tail(r$stderr, 12L), collapse = "\n"))
  expect_false(is.null(r$json))
  expect_gt(r$json$max_diff_123_124, 1e-5)
  expect_equal(r$json$max_diff_123_123, 0)
  expect_gt(r$json$n, 100)
})

## plugin-fixture em tempdir (toy da fundacao) cujo treino faz assign('x', 1, envir = globalenv())
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("runtime", "errors", "provenance", "topology", "deck_reader", "validation", "dates", "registry", "features"),
              env = environment())
prevcarga_registry_reset(); prevcarga_plugin_unload_all()

test_that("[ac-40][CONTROL] plugin-fixture que faz assign(globalenv()) e apanhado: o despachante aborta e o snapshot acusa a mudanca", {
  O <- lubridate::ymd("2026-10-01"); tz <- "Etc/GMT+3"
  cuts <- pc_toy_cuts(O, lubridate::ymd_hm("2026-10-01 09:00", tz = tz), split = "origin_agnostic")
  full <- pc_sentinel_panel(O - 12L, 13L, "SE"); full$attrs$origin <- O
  p <- pc_toy_leaky()
  g0 <- pcc_snapshot()
  cond <- tryCatch(prevcarga_plugin_train(p, pc_train_slice(full, cuts), cuts, p$spec, list()), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.global_side_effect")
  expect_match(conditionMessage(cond), "[prevcarga/contract]", fixed = TRUE)
  expect_identical(cond$exit_code, 2L)
  ## o despachante remove o simbolo plantado: o snapshot depois do abort e identico (guarda de efeitos funcionou)
  expect_identical(g0$globalenv, pcc_snapshot()$globalenv)
  ## CONTROL: a mesma escrita FORA do despachante e vista pelo snapshot (o plugin-fixture, rodado cru, suja)
  body <- p$descriptor$stages$training$fn
  expect_true(any(grepl("globalenv", deparse(body), fixed = TRUE)))
  feats <- list(m = matrix(1, 3L, 3L))
  suppressWarnings(try(body(feats, p$spec$params, 1L), silent = TRUE))
  withr::defer(if (exists("x", envir = globalenv(), inherits = FALSE)) rm("x", envir = globalenv()))
  expect_false(identical(g0$globalenv, pcc_snapshot()$globalenv))
})

test_that("[ac-41] freeze 2x em treinos identicos: sha256 igual nos 5; thaw -> infer identical; ENV sujo identical; formals sem params", {
  r <- pcc_run_child("portability")
  expect_identical(r$status, 0L, info = paste(utils::tail(r$stderr, 12L), collapse = "\n"))
  expect_false(is.null(r$json))
  pl <- r$json$plugins
  expect_setequal(names(pl), pcc_all_plugins())
  expect_length(pl, 5L)
  for (m in names(pl)) {
    x <- pl[[m]]
    expect_match(x$sha1, "^[0-9a-f]{64}$", info = m)
    expect_identical(x$sha1, x$sha2, info = m)                     # 2 treinos identicos => bytes identicos
    expect_true(isTRUE(x$thaw_identical), info = m)               # LGBM: a partir do booster em texto
    expect_true(isTRUE(x$dirty_identical), info = m)              # ENV sujo sem efeito (FORBIDDEN Delta=0)
    expect_gt(x$n, 0)
    expect_false(PCC_FORMAL_BAD %in% unlist(x$features_formals), info = m)
    expect_false(PCC_FORMAL_BAD %in% unlist(x$infer_formals), info = m)
  }
  ## plugins diferentes => bytes diferentes (o sha256 nao e trivial)
  expect_equal(length(unique(vapply(pl, function(x) x$sha1, ""))), 5L)
})

test_that("[ac-41][CONTROL] artefato-fixture com campo de relogio da sha256 diferente entre 2 freezes; sem o campo, igual", {
  env <- pc_source_lib(c("errors", "provenance"))
  d <- withr::local_tempdir()
  fz <- function(a, tag) env$prevcarga_freeze(a, file.path(d, paste0(tag, ".rds")))
  t1 <- lubridate::ymd_hms("2026-10-30 10:00:00", tz = "UTC")
  a1 <- list(payload = c(1, 2, 3), trained_at = t1)
  a2 <- list(payload = c(1, 2, 3), trained_at = t1 + 1)
  expect_identical(fz(a1, "a"), fz(a1, "b"))                      # mesmo conteudo => mesmo sha
  expect_false(identical(fz(a1, "c"), fz(a2, "d")))               # relogio diferente => sha diferente (CONTROL)
  expect_identical(fz(a1[1], "e"), fz(a2[1], "f"))                # sem o campo de relogio => igual
})
