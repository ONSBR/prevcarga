## test-combine.R -- M4.4: verbo `combine` (fachada, ADR-0040) + prevcarga_combina_gate. Tudo in-process
## (prevcarga_main chamado direto, runtime real fixado); so o braco docopt faz skip_if_not_installed.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

lib_dir <- file.path(pc_root(), "scripts", "lib")
E <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli"))
verbs <- E$prevcarga_verbs_discover()
crow <- verbs[verbs$verb == "combine", , drop = FALSE]

fx <- pc_cmb_cli_fixture(dir = withr::local_tempdir())
cli <- function(mode, extra = character(), fmt = "json", with_data = TRUE, state = fx$state)
  pc_cmb_run_cli(c("combine", paste0("--mode=", mode), "--area=SECO", paste0("--root=", fx$root),
                   if (with_data) paste0("--data=", fx$data), paste0("--state=", state),
                   paste0("--format=", fmt), extra))
j1 <- function(r) jsonlite::fromJSON(paste(r$out, collapse = "\n"), simplifyVector = FALSE)
ascii <- function(x) !any(as.integer(unlist(lapply(x, charToRaw))) > 127L)
ev <- pc_source_lib(c("errors", "schema_long", "topology", "provenance", "deck_reader", "evaluation"))
rd <- function(p) ev$prevcarga_read_long(p, list(tz = pc_cmb_tz))

## ---- ac-0: registro por descoberta, gramatica gerada, nao isento -----------------------------------------------------
test_that("M4.4 - ac-0 verbo descoberto: impl/M4, usage gerado, --origin unico flag de data, nao isento", {
  expect_identical(nrow(crow), 1L)
  expect_identical(crow$owner, "M4")
  expect_true(is.function(crow$handler[[1L]]))                      # deixou de ser stub (handler NULL)
  expect_false(crow$exempt)
  expect_false(any(verbs$verb[verbs$exempt] == "combine"))
  usage <- E$prevcarga_usage(verbs)
  expect_true(grepl(paste0("prevcarga combine --mode=<mode> --area=<a> --root=<dir> [--artifact-dir=<dir>] ",
                           "[--cut=<HHMM>] [--out=<path>] [--origin=<date>]"), usage, fixed = TRUE))
  expect_false(grepl("--from|--to=|--date|--until|--since", crow$usage))
  expect_identical(nrow(verbs), length(union(names(E$prevcarga_verbs_planned), verbs$verb)))   # total de linhas inalterado
  stub <- E$prevcarga_verbs_discover(tempfile("sem-cli-"))                                      # CONTROL: sem arquivo => stub
  expect_true(is.null(stub$handler[[which(stub$verb == "combine")]]))
  src <- sub("\\s*##.*$", "", readLines(file.path(lib_dir, "cli", "combine.R"), warn = FALSE))   # so codigo, sem comentario
  expect_false(any(grepl("todo.not_implemented|quit\\(|Sys.getenv", src)))
  expect_false(any(grepl("exit_code\\s*(<-|=)\\s*[0-9]", src)))     # classes do catalogo, nunca numero literal
})

test_that("M4.4 - ac-0 docopt parseia combine (braco com skip)", {
  skip_if_not_installed("docopt")
  o <- E$.prevcarga_parse(c("combine", "--mode=full", "--area=SECO", "--root=r", "--data=d", "--out=o.parquet"), verbs)
  expect_true(isTRUE(o$combine))
  expect_identical(o[["--mode"]], "full")
  o2 <- E$.prevcarga_parse(c("combine", "--mode=full", "--area=SECO", "--root=r", "--until=2024-01-01"), verbs)
  expect_null(o2)                                                    # CONTROL: nenhum flag de data alem de --origin
})

## ---- ac-1: validacao antes de qualquer IO ---------------------------------------------------------------------------
test_that("M4.4 - ac-1 config.bad_mode antes do espiao; flags faltantes; topologia; classes do catalogo", {
  sv <- pc_cmb_spy_verb()
  ctx <- E$prevcarga_ctx(list(), pc_root(), "combine")
  args <- function(...) c(list("--area" = "SECO", "--root" = "/x"), list(...))
  run <- function(a) tryCatch(sv$env$prevcarga_build_combine(ctx, a), error = identity)
  chk <- function(e, id, cls, where = NULL) {
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, id)
    expect_identical(e$exit_code, unname(get("prevcarga_exit_codes", envir = globalenv())[[cls]]))
    if (!is.null(where)) expect_match(paste(e$where, collapse = " "), where, fixed = TRUE)
  }
  chk(run(args("--mode" = "xyz")), "config.bad_mode", "config", "--mode=xyz")
  chk(run(args("--mode" = NULL)), "config.bad_mode", "config")
  expect_identical(sv$spy$calls, 0L)                                 # nada de IO
  chk(run(args("--mode" = "train")), "config.data_dir_missing", "config", "--data")
  chk(run(args("--mode" = "apply")), "config.usage", "config", "--out")
  chk(run(args("--mode" = "full", "--out" = file.path(tempfile("nao-existe"), "o.parquet"))), "config.usage", "config", "--out=")
  chk(run(list("--mode" = "train", "--area" = "XX", "--root" = "/x")), "topology.area_undeclared", "topology")
  expect_identical(sv$spy$calls, 0L)
  ctx$data_dir <- fx$data                                            # CONTROL: com tudo valido o espiao e acionado
  e <- run(args("--mode" = "train"))
  expect_false(inherits(e, "prevcarga_error"))
  expect_match(conditionMessage(e), "espiao")
  expect_identical(sv$spy$calls, 1L)
})

## ---- ac-2: train em JSON ---------------------------------------------------------------------------------------------
tr <- cli("train")
test_that("M4.4 - ac-2 train: JSON unico, artefato no layout do bucket, latest.txt, ASCII", {
  expect_identical(tr$code, 0L)
  expect_length(tr$out, 1L)
  expect_identical(tr$err, character(0))
  j <- j1(tr)
  expect_true(j$ok)
  expect_identical(j$verb, "combine")
  expect_identical(j$exit_code, 0L)
  expect_null(j$error)
  expect_true(all(c("warnings", "result", "run_id", "version") %in% names(j)))
  r <- j$result
  expect_identical(r$mode, "train")
  expect_match(r$version_hash, "^v_[0-9a-f]{16}$")
  want <- file.path(normalizePath(fx$state, winslash = "/"), "artifacts", "combina", "SECO", r$version_hash, "weights.rds")
  expect_identical(r$artifact_path, want)
  expect_true(file.exists(r$artifact_path))
  expect_gt(r$n_cells_fitted, 0L)
  expect_gt(r$n_cells_eqw, 0L)
  expect_setequal(unlist(r$members_present), fx$members$sigla)
  n_of <- function(lead) unique(vapply(Filter(function(m) pc_cmb_lead_day(m$lead) %in% lead, r$members_by_lead),
                                       function(m) m$n_members, 0L))
  expect_identical(n_of(0:1), 8L)                                    # O..O+1: os 8 membros
  expect_identical(n_of(2:8), 7L)                                    # O+2..O+8: o curto ja saiu
  expect_identical(r$load_label, "start")
  expect_identical(r$timezone, pc_cmb_tz)
  expect_identical(r$weather_issue_cutoff, "available_until")
  expect_identical(r$cuts, "0900")
  expect_identical(readLines(file.path(fx$state, "artifacts", "combina", "SECO", "latest.txt")), r$version_hash)
  expect_true(ascii(tr$out))
})

## ---- ac-3: apply / full / --origin ----------------------------------------------------------------------------------
fa <- file.path(fx$dir, "a.parquet"); ff <- file.path(fx$dir, "f.parquet"); fo <- file.path(fx$dir, "o.parquet")
ra <- cli("apply", paste0("--out=", fa), fmt = "text")
rf <- cli("full", paste0("--out=", ff))
ro <- cli("apply", c(paste0("--out=", fo), "--origin=2024-03-10"), fmt = "text")
test_that("M4.4 - ac-3 apply e full gravam LONG de 8 colunas; full == train+apply; --origin restringe", {
  for (r in list(ra, rf, ro)) expect_identical(r$code, 0L)
  expect_true(all(file.exists(c(fa, ff, fo))))
  a <- rd(fa); f <- rd(ff); o <- rd(fo)
  expect_identical(setdiff(names(a), "interval_start"), c("area", "forecast_origin", "target_datetime", "lead", "y_pred", "y_true", "d_plus",
                               "granularity_min"))
  expect_identical(lubridate::tz(a$target_datetime), pc_cmb_tz)
  expect_gt(nrow(a), 0L)
  expect_identical(a$y_pred, f$y_pred)                               # full == train + apply, bit a bit
  expect_identical(unique(o$forecast_origin), "2024-03-10")
  expect_identical(o$y_pred, a$y_pred[a$forecast_origin == "2024-03-10"])
  expect_gt(length(unique(a$forecast_origin)), 1L)                   # CONTROL: o apply sem --origin tem varias origens
  expect_identical(list.files(fx$dir, pattern = "^\\.combine-", all.files = TRUE), character(0))
  jf <- j1(rf)$result
  expect_identical(jf$mode, "full")
  expect_null(jf$version_hash)
  expect_identical(jf$out_path, ff)
  expect_true(ascii(c(ra$out, ro$out)))
  expect_match(ra$out[[1L]], "^combine  mode=apply  area=SECO  version=v_[0-9a-f]{16}$")
})

## ---- ac-4: prevcarga_combina_gate ---------------------------------------------------------------------------------
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                       "evaluation", "features", "registry", "combination"))
env$prevcarga_runtime_enforce()
qp <- pc_cmb_quality_panel()

test_that("M4.4 - ac-4 gate: pass so por propriedade, qualidade reportada, sem eps", {
  expect_false("eps" %in% names(formals(env$prevcarga_combina_gate)))
  g <- env$prevcarga_combina_gate(qp)
  expect_true(g$property$simplex_ok)
  expect_gt(g$property$n_fitted_converged, 0L)
  expect_lt(g$property$max_grad_norm, 1e-6)
  expect_true(g$pass)
  expect_identical(names(g$quality), c("area", "d_plus", "n", "wmape_combina", "wmape_eqw", "wmape_best",
                                       "best_member", "le_best", "le_eqw"))
  expect_true(all(g$quality$le_best & g$quality$le_eqw))
  expect_identical(g$quality$best_member[g$quality$d_plus <= 1L], c("S", "S"))   # o curto so concorre em d_plus 0..1
  expect_false(any(g$quality$best_member[g$quality$d_plus >= 2L] == "S"))
  ## veredito = f(propriedade): a tabela de qualidade nao entra no pass
  expect_true(env$prevcarga_combina_gate(qp, t_cut = "c0900")$pass)
})

test_that("M4.4 - ac-4 CONTROL degenerado reprova pela propriedade; qualidade so reportada", {
  w <- suppressWarnings(env$prevcarga_combina_fit(qp))
  gd <- env$prevcarga_combina_gate(qp, weights = pc_cmb_degenerate_weights(w, "D"))
  expect_true(gd$property$simplex_ok)
  expect_gte(gd$property$max_grad_norm, 1e-6)
  expect_false(gd$pass)
  expect_true(any(!gd$quality$le_eqw))
  ## combina perde para o EQW em WMAPE, pesos otimos: pass == TRUE (sem gate de dominancia)
  gl <- env$prevcarga_combina_gate(pc_cmb_loses_panel())
  expect_true(gl$pass)
  expect_true(any(!gl$quality$le_eqw))
  expect_false(gl$quality$le_eqw[[1L]])
})

test_that("M4.4 - ac-4 a qualidade consome prevcarga_scorecard (nao redefine WMAPE)", {
  nm <- c(all.names(body(env$prevcarga_combina_gate)), all.names(body(env$.prevcarga_cmb_quality_report)))
  expect_true("prevcarga_scorecard" %in% nm)
  expect_false(any(c("abs", "sum") %in% all.names(body(env$.prevcarga_cmb_quality_report))))
  expect_false(any(c("abs", "sum") %in% all.names(body(env$prevcarga_combina_gate))))
  expect_false("prevcarga_wmape" %in% nm)
})

## ---- ac-5: render text/json de sucesso e de erro -------------------------------------------------------------------
test_that("M4.4 - ac-5 text de sucesso (train, full), ASCII, aviso so em stderr", {
  rt <- cli("train", fmt = "text")
  rx <- cli("full", paste0("--out=", file.path(fx$dir, "t.parquet")), fmt = "text")
  for (r in list(rt, rx)) {
    expect_identical(r$code, 0L)
    expect_true(ascii(c(r$out, r$err)))
    expect_match(r$out[[2L]], "^celulas: fitadas=[0-9]+ eqw=[0-9]+ nao-estacionarias=[0-9]+$")
    expect_identical(r$out[[length(r$out)]], "OK")
  }
  expect_match(rt$out[[1L]], "^combine  mode=train  area=SECO  version=v_[0-9a-f]{16}$")
  expect_match(rx$out[[1L]], "^combine  mode=full  area=SECO  version=-$")
  expect_identical(rt$err, character(0))
})

test_that("M4.4 - ac-5 area nao treinada: exit 13 em text e em json; xml e config exit 8", {
  empty <- file.path(fx$dir, "estado-vazio"); dir.create(empty)
  oo <- file.path(fx$dir, "n.parquet")
  rt <- cli("apply", paste0("--out=", oo), fmt = "text", state = empty)
  expect_identical(rt$code, 13L)
  expect_identical(rt$out, character(0))
  expect_identical(rt$err[[1L]], "prevcarga: erro [state] (exit 13)")
  expect_true(all(vapply(c("o que:", "onde:", "como corrigir:"), function(k) any(startsWith(rt$err, k)), NA)))
  expect_true(ascii(rt$err))
  expect_false(any(grepl("Traceback", rt$err)))
  rj <- cli("apply", paste0("--out=", oo), fmt = "json", state = empty)
  expect_identical(rj$code, 13L)
  expect_length(rj$out, 1L)
  expect_identical(rj$err, character(0))
  j <- j1(rj)
  expect_false(j$ok)
  expect_identical(j$exit_code, 13L)
  expect_identical(j$error$id, "state.combina_not_trained")
  expect_false(file.exists(oo))
  rx <- cli("train", fmt = "xml")
  expect_identical(rx$code, 8L)
  expect_match(paste(rx$err, collapse = "\n"), "[config]", fixed = TRUE)
})
