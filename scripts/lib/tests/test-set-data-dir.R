## test-set-data-dir.R -- verbo set-data-dir (config.R: prevcarga_config_set_data_dir): alvo, forma da chave, preservacao,
## recusas. CONTROL: sem o verbo, o ctx da instalacao nao tem data_dir; depois dele, tem.
library(testthat)
local({
  e <- pc_source_lib(c("errors", "runtime", "config"))
  for (n in ls(e, all.names = TRUE)) assign(n, get(n, e), envir = globalenv())
})
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

sdd_install <- function(yaml = NULL) {   # <R>/versions/<v>/code com config/VERSION; config.yaml opcional em <R>
  r <- normalizePath(tempfile("R"), winslash = "/", mustWork = FALSE)
  code <- file.path(r, "versions", "9.9.9", "code")
  dir.create(file.path(code, "config"), recursive = TRUE)
  writeLines("9.9.9", file.path(code, "config", "VERSION"))
  if (!is.null(yaml)) writeLines(yaml, file.path(r, "config.yaml"))
  list(R = r, code = code)
}
sdd_abort <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

test_that("cria <R>/config.yaml quando falta; caminho absoluto com /; CONTROL: antes do verbo o ctx nao tem data_dir", {
  i <- sdd_install(); deck <- tempfile("deck"); dir.create(deck)
  ctx <- prevcarga_ctx(list(), i$code)
  expect_null(ctx$data_dir)                                                          # CONTROL
  res <- prevcarga_config_set_data_dir(ctx, deck)
  d <- normalizePath(deck, winslash = "/")
  expect_identical(res$data_dir, d)
  expect_identical(res$config_path, normalizePath(file.path(i$R, "config.yaml"), winslash = "/"))
  expect_null(res$previous)
  expect_identical(prevcarga_ctx(list(), i$code)$data_dir, d)
  expect_true("schema_version: 1" %in% readLines(file.path(i$R, "config.yaml")))
})

test_that("troca o data_dir e preserva as outras linhas (comentario, state_dir, format); uma so chave data_dir", {
  i <- sdd_install(c("# meu comentario", "schema_version: 1", "format: json", "data_dir: '/velho'", "state_dir: '/estado'"))
  deck <- tempfile("deck"); dir.create(deck)
  res <- prevcarga_config_set_data_dir(prevcarga_ctx(list(), i$code), deck)
  expect_identical(res$previous, "/velho")
  y <- readLines(file.path(i$R, "config.yaml"))
  expect_identical(sum(startsWith(y, "data_dir:")), 1L)
  expect_true(all(c("# meu comentario", "format: json", "state_dir: '/estado'") %in% y))
  cfg <- prevcarga_config_load(file.path(i$R, "config.yaml"))
  expect_identical(cfg$state_dir, "/estado"); expect_identical(cfg$format, "json")
})

test_that("aspas simples no caminho: YAML com '' (a forma que o instalador le e preserva)", {
  i <- sdd_install(); deck <- file.path(tempfile("d"), "it's um deck"); dir.create(deck, recursive = TRUE)
  res <- prevcarga_config_set_data_dir(prevcarga_ctx(list(), i$code), deck)
  y <- readLines(file.path(i$R, "config.yaml"))
  expect_identical(y[startsWith(y, "data_dir:")], paste0("data_dir: '", gsub("'", "''", res$data_dir), "'"))
  expect_identical(prevcarga_ctx(list(), i$code)$data_dir, res$data_dir)
})

test_that("--config explicito e o alvo; o config.yaml da instalacao fica intacto", {
  i <- sdd_install(c("schema_version: 1", "data_dir: '/inst'"))
  alt <- tempfile("alt", fileext = ".yaml"); writeLines("schema_version: 1", alt)
  deck <- tempfile("deck"); dir.create(deck)
  res <- prevcarga_config_set_data_dir(prevcarga_ctx(list(`--config` = alt), i$code), deck)
  expect_identical(res$config_path, normalizePath(alt, winslash = "/"))
  expect_true("data_dir: '/inst'" %in% readLines(file.path(i$R, "config.yaml")))
})

test_that("recusas: diretorio inexistente (data_dir_missing) e fora da instalacao sem config.yaml (usage); nada escrito", {
  i <- sdd_install()
  e1 <- sdd_abort(prevcarga_config_set_data_dir(prevcarga_ctx(list(), i$code), file.path(i$R, "nao-existe")))
  expect_identical(e1$id, "config.data_dir_missing"); expect_identical(e1$exit_code, 8L)
  expect_false(file.exists(file.path(i$R, "config.yaml")))
  dev <- tempfile("dev"); dir.create(file.path(dev, "config"), recursive = TRUE)
  deck <- tempfile("deck"); dir.create(deck)
  e2 <- sdd_abort(prevcarga_config_set_data_dir(prevcarga_ctx(list(), dev), deck))
  expect_identical(e2$id, "config.usage")
  expect_length(list.files(dev, all.files = TRUE, no.. = TRUE, recursive = TRUE), 0L)
  expect_length(list.files(i$R, pattern = "^\\.config", all.files = TRUE), 0L)              # sem temporario largado
})

test_that("o verbo e descoberto pela fachada com <dir> posicional e o id config.write_failed esta no catalogo", {
  e <- pc_source_lib(c("errors", "config", "banner", "runtime", "cli"))
  v <- e$prevcarga_verbs_discover(file.path(pc_root(), "scripts", "lib", "cli"))
  expect_identical(v$usage[v$verb == "set-data-dir"], "prevcarga set-data-dir <dir>")
  expect_false(v$exempt[v$verb == "set-data-dir"])
  expect_identical(prevcarga_error_lookup("config.write_failed")$class, "config")
})
