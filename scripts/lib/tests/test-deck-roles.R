## test-deck-roles.R -- deck_roles nos verbos e recusa de papel (E1-T3). Fixtures sinteticas em tempdir; runtime injetado vazio.
library(testthat)
skip_if_not_installed("docopt")
lib_dir <- file.path(pc_root(), "scripts", "lib")
withr::local_options(prevcarga.lib = lib_dir)
E <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli"))

abort_of <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
mk_ctx <- function(train = NULL, inf = NULL, verb = "forecast")
  list(data_train_dir = train, data_inference_dir = inf, data_dir = train, verb = verb)
BOTH <- c("train", "inference")
quiet <- function(x) { r <- NULL; utils::capture.output(r <- force(x), type = "output"); suppressMessages(r) }

test_that("[AC0] sem flag: duas chaves => train (delta 0); so inferencia => inference; nenhuma => train com data_dir NULL", {
  x <- E$prevcarga_resolve_deck_role(mk_ctx("/t", "/i"), BOTH, list())
  expect_identical(x$data_role, "train")
  expect_identical(x$data_dir, "/t")
  expect_identical(x$data_role_option, "--data-train")
  y <- E$prevcarga_resolve_deck_role(mk_ctx(NULL, "/i"), BOTH, list())
  expect_identical(y$data_role, "inference")
  expect_identical(y$data_dir, "/i")
  expect_identical(y$data_role_option, "--data-inference")
  z <- E$prevcarga_resolve_deck_role(mk_ctx(), "train", list())
  expect_identical(z$data_role, "train")
  expect_null(z$data_dir)
  ## CONTROL: a flag vence a chave do config
  w <- E$prevcarga_resolve_deck_role(mk_ctx("/t", "/i"), BOTH, list(`--data-inference` = "/i"))
  expect_identical(w$data_role, "inference")
})

test_that("[AC1] train-models com --data-inference: deck_role_refused antes do handler", {
  n <- 0L
  spy <- function(ctx, args) { n <<- n + 1L; list() }
  code <- quiet(E$prevcarga_main(c("train-models", paste0("--data-inference=", tempdir()), "--origin", "2023-06-01"),
                         root = pc_root(), runtime = function() NULL, handlers = list(`train-models` = spy)))
  expect_identical(code, 2L)
  expect_identical(n, 0L)
  ## erro estruturado
  e <- abort_of(E$prevcarga_resolve_deck_role(mk_ctx(NULL, "/i", "train-models"), "train", list(`--data-inference` = "/i")))
  expect_identical(e$id, "contract.deck_role_refused")
  expect_identical(e$where, "--data-inference")
  expect_identical(e$details$verb, "train-models")
  expect_identical(unlist(e$details$accepted), "train")
  expect_identical(e$exit_code, 2L)
  ## CONTROL: forecast (os dois papeis) aceita o mesmo deck de inferencia
  n2 <- 0L
  spy2 <- function(ctx, args) { n2 <<- n2 + 1L; list() }
  quiet(E$prevcarga_main(c("forecast", paste0("--data-inference=", tempdir())),
                 root = pc_root(), runtime = function() NULL, handlers = list(forecast = spy2)))
  expect_identical(n2, 1L)
})

test_that("[AC2] --data-train junto com --data-inference: config.usage; CONTROL so inferencia resolve", {
  e <- abort_of(E$prevcarga_resolve_deck_role(mk_ctx(verb = "forecast"), BOTH,
                list(`--data-train` = "/t", `--data-inference` = "/i")))
  expect_identical(e$id, "config.usage")
  expect_identical(e$exit_code, 8L)
  expect_identical(e$where, "--data-train x --data-inference")
  e2 <- abort_of(E$prevcarga_resolve_deck_role(mk_ctx(), BOTH, list(`--data` = "/t", `--data-inference` = "/i")))
  expect_identical(e2$id, "config.usage")
  x <- E$prevcarga_resolve_deck_role(mk_ctx(NULL, "/i"), BOTH, list(`--data-inference` = "/i"))
  expect_identical(x$data_role, "inference")
  expect_identical(x$data_dir, "/i")
})

test_that("[AC3] deck_roles dos registros descobertos: funcao em reconcile/combine, vetor nos demais", {
  tab <- E$prevcarga_verbs_discover()
  dr <- function(v) tab$deck_roles[[match(v, tab$verb)]]
  for (v in c("forecast", "infer-model", "validate-deck")) expect_identical(dr(v), BOTH, info = v)
  rec <- dr("reconcile"); cmb <- dr("combine")
  expect_true(is.function(rec)); expect_true(is.function(cmb))
  expect_identical(rec(list(`--mode` = "train")), "train")
  expect_identical(rec(list(`--mode` = "apply", `--eval` = TRUE)), "train")
  expect_identical(rec(list(`--mode` = "apply")), BOTH)
  expect_identical(cmb(list(`--mode` = "train")), "train")
  expect_identical(cmb(list(`--mode` = "full")), "train")
  expect_identical(cmb(list(`--mode` = "apply")), BOTH)
  ## CONTROL: verbo sem o campo = NULL (train)
  expect_null(dr("train-models"))
})

test_that("[AC3] .prevcarga_load_verb_file recusa deck_roles invalido", {
  d <- withr::local_tempdir()
  f <- file.path(d, "xx.R")
  writeLines('prevcarga_verb_xx <- list(name = "xx", usage = "prevcarga xx", handler = function(ctx) NULL, deck_roles = "foo")', f)
  expect_error(E$.prevcarga_load_verb_file(f), "deck_roles")
  writeLines('prevcarga_verb_xx <- list(name = "xx", usage = "prevcarga xx", handler = function(ctx) NULL, deck_roles = "inference")', f)
  expect_identical(E$.prevcarga_load_verb_file(f)$deck_roles, "inference")
})

test_that("[AC4] default do --artifact-dir do combine: <raiz de artefatos>/combina; sem --data-artifacts delta 0", {
  cb <- pc_source_lib(c("errors", "runtime", "registry"))
  sys.source(file.path(lib_dir, "cli", "combine.R"), envir = cb)
  cb$.prevcarga_score_areas <- function(...) NULL
  cb$.prevcarga_score_topo <- function(...) NULL
  cb$prevcarga_require_deck <- function(ctx) invisible(ctx)
  cb$prevcarga_members <- function(...) list()
  cb$.prevcarga_eval_tz <- function(ctx) "UTC"
  cb$prevcarga_combina_load <- function(...) data.frame()
  cb$prevcarga_combina_train <- function(area, panel, artifact_dir, ...)
    stop(structure(class = c("spy_stop", "error", "condition"), list(message = "spy", call = NULL, dir = artifact_dir)))
  got <- function(ctx) tryCatch(cb$prevcarga_build_combine(ctx, list(`--mode` = "train", `--area` = "SECO", `--root` = "/r")),
                                spy_stop = function(e) e$dir)
  st <- withr::local_tempdir("st-"); ar <- withr::local_tempdir("ar-")
  nrm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  expect_identical(got(list(state_dir = st, artifacts_dir = ar)), file.path(nrm(ar), "combina"))
  expect_identical(got(list(state_dir = st, artifacts_dir = NULL)), file.path(nrm(st), "artifacts", "combina"))
})
