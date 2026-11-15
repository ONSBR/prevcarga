## test-set-data-dir.R -- verbo set-data-dir (config.R: prevcarga_config_set_keys e o wrapper prevcarga_config_set_data_dir): alvo, forma da chave, preservacao,
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
  expect_identical(prevcarga_ctx(list(), i$code)$data_train_dir, d)
  expect_identical(res$config_path, normalizePath(file.path(i$R, "config.yaml"), winslash = "/"))
  expect_null(res$previous)
  expect_identical(prevcarga_ctx(list(), i$code)$data_dir, d)
  expect_true("schema_version: 1" %in% readLines(file.path(i$R, "config.yaml")))
})

test_that("troca o data_train_dir anterior e preserva as outras linhas; nunca grava nem le data_dir", {
  i <- sdd_install(c("# meu comentario", "schema_version: 1", "format: json", "data_train_dir: '/velho'", "state_dir: '/estado'"))
  deck <- tempfile("deck"); dir.create(deck)
  res <- prevcarga_config_set_data_dir(prevcarga_ctx(list(), i$code), deck)
  expect_identical(res$previous, "/velho")
  y <- readLines(file.path(i$R, "config.yaml"))
  expect_identical(sum(startsWith(y, "data_dir:")), 0L)
  expect_identical(sum(startsWith(y, "data_train_dir:")), 1L)
  expect_silent(prevcarga_ctx(list(), i$code))
  expect_true(all(c("# meu comentario", "format: json", "state_dir: '/estado'") %in% y))
  ## clean break: um config.yaml com a chave legada data_dir e recusado (nao ha leitura silenciosa nem mapeamento)
  j <- sdd_install(c("schema_version: 1", "data_dir: '/velho'"))
  e <- tryCatch(prevcarga_ctx(list(), j$code), prevcarga_error = identity)
  expect_identical(e$id, "config.unknown_key"); expect_identical(e$where, "data_dir")
  expect_error(prevcarga_config_set_data_dir(prevcarga_ctx(list(), i$code), file.path(tempdir(), "nao-existe-xyz")), class = "prevcarga_error")
  cfg <- prevcarga_config_load(file.path(i$R, "config.yaml"))
  expect_identical(cfg$state_dir, "/estado"); expect_identical(cfg$format, "json")
})

test_that("aspas simples no caminho: YAML com '' (a forma que o instalador le e preserva)", {
  i <- sdd_install(); deck <- file.path(tempfile("d"), "it's um deck"); dir.create(deck, recursive = TRUE)
  res <- prevcarga_config_set_data_dir(prevcarga_ctx(list(), i$code), deck)
  y <- readLines(file.path(i$R, "config.yaml"))
  expect_identical(y[startsWith(y, "data_train_dir:")], paste0("data_train_dir: '", gsub("'", "''", res$data_dir), "'"))
  expect_identical(prevcarga_ctx(list(), i$code)$data_dir, res$data_dir)
})

test_that("--config explicito e o alvo; o config.yaml da instalacao fica intacto", {
  i <- sdd_install(c("schema_version: 1", "data_train_dir: '/inst'"))
  alt <- tempfile("alt", fileext = ".yaml"); writeLines("schema_version: 1", alt)
  deck <- tempfile("deck"); dir.create(deck)
  res <- prevcarga_config_set_data_dir(prevcarga_ctx(list(`--config` = alt), i$code), deck)
  expect_identical(res$config_path, normalizePath(alt, winslash = "/"))
  expect_true("data_train_dir: '/inst'" %in% readLines(file.path(i$R, "config.yaml")))
  expect_true("data_train_dir" %in% vapply(prevcarga_config_set_keys(prevcarga_ctx(list(`--config` = alt), i$code),
                                            list(data_train_dir = deck))$changes, function(x) x$key, ""))
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
  expect_identical(v$usage[v$verb == "set-data-dir"],
                   "prevcarga set-data-dir [<dir>] [--train=<dir>] [--inference=<dir>] [--artifacts=<dir>]")
  expect_false(v$exempt[v$verb == "set-data-dir"])
  expect_identical(prevcarga_error_lookup("config.write_failed")$class, "config")
})

test_that("[AC3] set_keys parcial: so as chaves dadas mudam, data_inference_dir fica intacta, changes {key, old, new}", {
  va <- tempfile("va"); dir.create(va); va <- normalizePath(va, winslash = "/")
  i <- sdd_install(c("# c", "schema_version: 1", "data_inference_dir: '/inf'", paste0("artifacts_dir: '", va, "'")))
  t <- tempfile("t"); a <- tempfile("a"); dir.create(t); dir.create(a)
  res <- prevcarga_config_set_keys(prevcarga_ctx(list(), i$code), list(data_train_dir = t, artifacts_dir = a))
  expect_identical(vapply(res$changes, function(x) x$key, ""), c("data_train_dir", "artifacts_dir"))
  expect_identical(res$changes[[2]]$old, va)
  expect_null(res$changes[[1]]$old)
  y <- readLines(file.path(i$R, "config.yaml"))
  expect_true(all(c("# c", "data_inference_dir: '/inf'") %in% y))
  expect_identical(prevcarga_config_load(file.path(i$R, "config.yaml"))$artifacts_dir, normalizePath(a, winslash = "/"))
  e <- sdd_abort(prevcarga_config_set_keys(prevcarga_ctx(list(), i$code), list(data_train_dir = file.path(t, "nao"))))
  expect_identical(e$id, "config.data_dir_missing"); expect_identical(e$exit_code, 8L)
})

test_that("[AC3] pela fachada: --train/--artifacts com --format=json grava so as duas chaves; --train inexistente = exit 8", {
  skip_if_not_installed("processx")
  i <- sdd_install(); cfg <- file.path(i$R, "config.yaml")
  t <- tempfile("t"); a <- tempfile("a"); dir.create(t); dir.create(a)
  writeLines(c("schema_version: 1", "data_inference_dir: '/inf'"), cfg)
  run <- function(...) processx::run(file.path(R.home("bin"), "Rscript"),
           c("--vanilla", file.path(pc_root(), "scripts", "prevcarga"), "set-data-dir", paste0("--config=", cfg), ...),
           error_on_status = FALSE, stdin = "")
  r <- run(paste0("--train=", t), paste0("--artifacts=", a), "--format=json")
  expect_identical(r$status, 0L)
  j <- jsonlite::fromJSON(r$stdout, simplifyVector = FALSE)$result
  expect_identical(j$schema_version, 1L); expect_true(j$gravado)
  expect_identical(vapply(j$changes, function(x) x$key, ""), c("data_train_dir", "artifacts_dir"))
  expect_true("data_inference_dir: '/inf'" %in% readLines(cfg))
  expect_identical(run(paste0("--train=", file.path(t, "nao")))$status, 8L)
  r2 <- run()                                                   # sem chave e sem terminal: exit 2, sem tocar no arquivo
  expect_identical(r2$status, 2L); expect_match(r2$stderr, "contract.set_data_dir_noninteractive|exige terminal")
})

test_that("nenhum readline() em scripts/lib (a entrada e readLines(input, n = 1L))", {
  fs <- list.files(file.path(pc_root(), "scripts", "lib"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  fs <- fs[!grepl("/tests/", fs, fixed = TRUE)]
  hits <- unlist(lapply(fs, function(f) grep("(^|[^A-Za-z_.])readline\\(", readLines(f, warn = FALSE), value = TRUE)))
  expect_length(hits, 0L)
})
