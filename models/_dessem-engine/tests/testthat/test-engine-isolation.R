## test-engine-isolation.R -- DL.4 (AC3 do ticket): o motor e carregado POR INSTANCIA para o dessem-linear (real) e para um
## descriptor-fixture do regdin na mesma sessao; nenhuma instancia enxerga ou altera a outra nem o globalenv.

## Raiz temporaria com o motor, o dessem-linear REAL e um descriptor-fixture `dessem-regdin` (engine = _dessem-engine)
iso_root <- function() {
  root <- eng_plugin_root(name = "dessem-regdin")
  file.copy(file.path(eng_root(), "models", "dessem-linear"), root, recursive = TRUE)
  root
}
iso_load <- function(root) {
  eng_lib$prevcarga_plugin_unload_all(); eng_lib$prevcarga_registry_reset()
  list(linear = eng_lib$prevcarga_plugin_load("dessem-linear", root), regdin = eng_lib$prevcarga_plugin_load("dessem-regdin", root))
}
iso_manifest <- function() { e <- new.env(); sys.source(file.path(eng_dir(), "R", "manifest.R"), envir = e); e$engine_manifest() }

test_that("ac-3 duas instancias distintas do motor (dessem-linear real + descriptor do regdin) com todas as required_functions", {
  snap <- eng_globalenv_snapshot()
  p <- iso_load(iso_root())
  ea <- eng_env(p$linear); eb <- eng_env(p$regdin)
  expect_true(is.environment(ea) && is.environment(eb))
  expect_false(identical(ea, eb))
  expect_false(identical(p$linear$env, p$regdin$env))
  for (fn in iso_manifest()$required_functions) {
    expect_true(is.function(get0(fn, envir = ea, inherits = FALSE)), info = paste("A", fn))
    expect_true(is.function(get0(fn, envir = eb, inherits = FALSE)), info = paste("B", fn))
    ## funcao por instancia: o mesmo codigo, mas cada closure no seu env
    expect_false(identical(get(fn, envir = ea), get(fn, envir = eb)), info = fn)
  }
  expect_identical(eng_globalenv_snapshot(), snap)                       # ls(globalenv()) antes == depois
  expect_false(any(iso_manifest()$required_functions %in% ls(globalenv(), all.names = TRUE)))
})

test_that("ac-3 grep: nowcast, .rd_nowcast, .dessem_nowcast e <<- ausentes do motor (pc_grep_code da fundacao)", {
  if (!exists("pc_grep_code", envir = eng_lib, inherits = FALSE))
    sys.source(file.path(eng_root(), "scripts", "lib", "tests", "helper-plugins.R"), envir = eng_lib)
  fs <- list.files(file.path(eng_dir(), "R"), pattern = "[.]R$", full.names = TRUE)
  pat <- list(nowcast = "nowcast", rd_nowcast = "[.]rd_nowcast", dessem_nowcast = "[.]dessem_nowcast", superassign = "<<-")
  expect_identical(nrow(eng_lib$pc_grep_code(fs, pat)), 0L)
  ## CONTROL: o mesmo grep acusa o que for plantado
  f <- tempfile(fileext = ".R")
  writeLines(c("a <- 1", ".rd_nowcast <- new.env()", "n <<- n + 1L", "x <- nowcast_head(1)"), f)
  expect_setequal(eng_lib$pc_grep_code(f, pat)$pattern, c("rd_nowcast", "superassign", "nowcast"))
})

test_that("ac-3 sobrescrita local na instancia A nao muda a saida da B (delta = 0)", {
  p <- iso_load(iso_root())
  ea <- eng_env(p$linear); eb <- eng_env(p$regdin)
  deck <- eng_model_deck(eng_deck())
  run_b <- function() {
    dk <- suppressMessages(eb$TransformaDeck(deck, "2019-02-28", "10DIAS", tz = "Etc/GMT+3"))
    list(serie = dk$SERIECOMP, cfg = eb$getConfigModel("Linear.TmpMedia"), conf = eb$getConfig())
  }
  before <- run_b()
  ## sobrescreve em A: config, tune_wsvm e TransformaDeck
  assign("getConfig", function() list(sobrescrito = TRUE), envir = ea)
  assign("TransformaDeck", function(...) stop("A sobrescrita"), envir = ea)
  assign("tune_wsvm", function(...) stop("A sobrescrita"), envir = ea)
  expect_identical(ea$getConfig(), list(sobrescrito = TRUE))                          # CONTROL: a mudanca pegou em A
  expect_error(ea$TransformaDeck(deck, "2019-02-28", "10DIAS"), "A sobrescrita")
  after <- run_b()
  expect_identical(after, before)                                                     # B: delta = 0
  expect_false(identical(eb$tune_wsvm, ea$tune_wsvm))
  expect_false(exists("getConfig", envir = globalenv()))
})

test_that("ac-3 CONTROL: motor sem desagregaModeloSH => [prevcarga/contract] plugin.engine_function_missing nomeando a funcao", {
  src <- readLines(file.path(eng_dir(), "R", "funcoes_apoio.R"))
  i <- grep("^desagregaModeloSH <- function", src)
  expect_length(i, 1L)
  nxt <- grep("^[A-Za-z_.][A-Za-z0-9_.]* <- function", src); nxt <- nxt[nxt > i]
  keep <- if (length(nxt)) src[-(i:(nxt[[1L]] - 1L))] else src[seq_len(i - 1L)]
  root <- eng_plugin_root(name = c("dessem-regdin", "dessem-linear"), engine_files = list("funcoes_apoio.R" = keep))
  eng_lib$prevcarga_plugin_unload_all(); eng_lib$prevcarga_registry_reset()
  cond <- tryCatch(eng_lib$prevcarga_plugin_load("dessem-regdin", root), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.engine_function_missing")
  expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L)
  expect_true(grepl("[prevcarga/contract]", conditionMessage(cond), fixed = TRUE))
  expect_true(grepl("desagregaModeloSH", conditionMessage(cond), fixed = TRUE))
})
