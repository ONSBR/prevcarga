## test-engine-loader.R -- DL.1: o loader da fundacao monta o env do motor _dessem-engine (AC1, AC2, AC3, AC6).

expect_contract <- function(expr, id, token = NULL) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (is.null(cond)) return(invisible(NULL))
  expect_identical(cond$id, id)
  expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L)
  if (!is.null(token)) expect_true(grepl(token, conditionMessage(cond), fixed = TRUE))
  invisible(cond)
}

manifest_of <- function() { e <- new.env(); sys.source(file.path(eng_dir(), "R", "manifest.R"), envir = e); e$engine_manifest() }

test_that("ac-8 o loader monta o env do motor com todas as required_functions e o globalenv intacto", {
  snap <- eng_globalenv_snapshot()
  root <- eng_plugin_root()
  p <- eng_load(root)
  eng <- eng_env(p)
  m <- manifest_of()
  expect_identical(m$engine_id, "_dessem-engine")
  expect_identical(m$contract_version, 1L)
  expect_length(m$required_functions, 12L)
  expect_false(file.exists(file.path(eng_dir(), "run.yaml")))
  for (fn in m$required_functions) expect_true(is.function(get0(fn, envir = eng, inherits = FALSE)), info = fn)
  expect_true(all(c("desagregaModeloSH", "getConfig", "TransformaDeck") %in% m$required_functions))
  expect_identical(eng_globalenv_snapshot(), snap)
  expect_false(any(m$required_functions %in% ls(globalenv())))
})

test_that("ac-8 CONTROL: motor sem desagregaModeloSH => contract nomeando a funcao", {
  src <- readLines(file.path(eng_dir(), "R", "funcoes_apoio.R"))
  i <- grep("^desagregaModeloSH <- function", src)
  expect_length(i, 1L)
  nxt <- grep("^[A-Za-z_.][A-Za-z0-9_.]* <- function", src)
  nxt <- nxt[nxt > i]
  keep <- if (length(nxt)) src[-(i:(nxt[[1L]] - 1L))] else src[seq_len(i - 1L)]
  root <- eng_plugin_root(engine_files = list("funcoes_apoio.R" = keep))
  expect_contract(eng_load(root), "plugin.engine_function_missing", "desagregaModeloSH")
})

test_that("ac-9 cada arquivo do motor so define funcoes e o grep de estado/nowcast sai vazio", {
  files <- list.files(file.path(eng_dir(), "R"), pattern = "[.]R$", full.names = TRUE)
  expect_true(length(files) >= 9L)
  for (f in files) expect_true(eng_lib$.prevcarga_plugin_defs_only(f), info = basename(f))
  pat <- "<<-|assign[(]|[.]dessem_nowcast|[.]rd_nowcast|nowcast"
  hit <- unlist(lapply(files, function(f) { l <- readLines(f, warn = FALSE); basename(f)[any(grepl(pat, l))] }))
  expect_length(hit, 0L)
  m <- manifest_of()
  expect_true(all(m$files %in% basename(files)))
  expect_identical(m$files[[length(m$files)]], "engine.R")
})

test_that("ac-9 CONTROL: arquivo com read.csv no topo => contract do loader nomeando o arquivo", {
  root <- eng_plugin_root(engine_files = list("zz_bad.R" = "x <- read.csv('a.csv')"),
                          manifest = list(files = c(manifest_of()$files, "zz_bad.R")))
  expect_contract(eng_load(root), "plugin.toplevel_code", "zz_bad.R")
})

test_that("ac-10 model_parameters.json byte-identico ao do oraculo e getConfigModel le do diretorio do motor", {
  mine <- file.path(eng_dir(), "model_parameters.json")
  expect_identical(unname(tools::md5sum(mine)), "56f8fc5ed7b8db4275827a3a8122c0b0")
  orc <- "/opt/ons-source/prevcargadessem3/models/dessem-linear/vendor/model_parameters.json"
  if (file.exists(orc)) {
    expect_identical(unname(tools::sha256sum(mine)), unname(tools::sha256sum(orc)))
    expect_identical(readBin(mine, "raw", 1e6), readBin(orc, "raw", 1e6))
  }
  p <- eng_load(eng_plugin_root())
  eng <- eng_env(p)
  cfg <- eng$getConfigModel("Linear.TmpMedia")
  expect_true(all(c("exogenaOper", "normalizacao", "ranges") %in% names(cfg)))
  expect_identical(cfg$exogenaOper, "MED")
  expect_identical(eng$getConfig(), jsonlite::fromJSON(mine, simplifyDataFrame = FALSE, simplifyMatrix = FALSE))
  expect_true(startsWith(basename(dirname(eng$engine_config_path())), "_dessem-engine"))
})

test_that("ac-10 CONTROL: chave inexistente => [prevcarga/contract] engine.dessem_unknown_model_key", {
  p <- eng_load(eng_plugin_root())
  eng <- eng_env(p)
  ## o motor chama prevcarga_abort (fornecido pela CLI no globalenv); aqui entra so durante a chamada
  assign("prevcarga_abort", eng_lib$prevcarga_abort, envir = globalenv())
  on.exit(rm("prevcarga_abort", envir = globalenv()), add = TRUE)
  cond <- expect_contract(eng$getConfigModel("Linear.Inexistente"), "engine.dessem_unknown_model_key",
                          "[prevcarga/contract]")
  expect_identical(cond$details$model_key, "Linear.Inexistente")
})

test_that("ac-13 duas instancias do motor nao compartilham estado", {
  root <- eng_plugin_root(name = c("dessem-a", "dessem-b"))
  eng_lib$prevcarga_plugin_unload_all(); eng_lib$prevcarga_registry_reset()
  pa <- eng_lib$prevcarga_plugin_load("dessem-a", root)
  pb <- eng_lib$prevcarga_plugin_load("dessem-b", root)
  ea <- eng_env(pa); eb <- eng_env(pb)
  expect_false(identical(ea, eb))
  direct <- jsonlite::fromJSON(file.path(eng_dir(), "model_parameters.json"), simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  assign("getConfig", function() list(sobrescrito = TRUE), envir = ea)
  expect_identical(ea$getConfig(), list(sobrescrito = TRUE))          # CONTROL
  expect_identical(eb$getConfig(), direct)
  expect_false(exists("getConfig", envir = globalenv()))
})
