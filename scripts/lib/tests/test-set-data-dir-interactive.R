## test-set-data-dir-interactive.R -- modo interativo do set-data-dir (config.R: prevcarga_set_data_dir_interactive).
## Entrada em textConnection, saida em textConnection de escrita, is_tty injetado. CONTROL: com 's' grava; com 'N'/EOF nao.
library(testthat)
local({
  e <- pc_source_lib(c("errors", "runtime", "config"))
  for (n in ls(e, all.names = TRUE)) assign(n, get(n, e), envir = globalenv())
})
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

sdi_install <- function() {
  r <- normalizePath(tempfile("R"), winslash = "/", mustWork = FALSE)
  code <- file.path(r, "versions", "9.9.9", "code")
  dir.create(file.path(code, "config"), recursive = TRUE)
  writeLines("9.9.9", file.path(code, "config", "VERSION"))
  writeLines(c("# meu comentario", "schema_version: 1", "state_dir: '/s'", "data_dir: '/velho'"), file.path(r, "config.yaml"))
  base <- normalizePath(tempfile("d"), winslash = "/", mustWork = FALSE)
  dir.create(file.path(base, "train"), recursive = TRUE)
  list(R = r, code = code, base = base, cfg = file.path(r, "config.yaml"))
}
sdi_run <- function(i, lines, is_tty = TRUE) {
  out <- character()
  con <- textConnection("out", "w", local = TRUE); on.exit(close(con))
  inp <- textConnection(lines); on.exit(close(inp), add = TRUE)
  res <- prevcarga_set_data_dir_interactive(suppressWarnings(prevcarga_ctx(list(), i$code)), base = i$base, input = inp, is_tty = is_tty, output = con)
  res
}

test_that("[AC0] linhas vazias aceitam as sugestoes; 's' grava tres chaves, remove data_dir, cria artefatos e inferencia", {
  i <- sdi_install()
  res <- sdi_run(i, c("", "", "", "s"))
  expect_true(res$gravado); expect_length(res$changes, 3L)
  y <- readLines(i$cfg)
  expect_true(all(c("# meu comentario", "state_dir: '/s'") %in% y))
  expect_false(any(startsWith(y, "data_dir:")))
  cfg <- prevcarga_config_load(i$cfg)
  expect_identical(cfg$data_train_dir, file.path(i$base, "train"))
  expect_identical(cfg$artifacts_dir, file.path(i$base, "artifacts"))
  expect_identical(cfg$data_inference_dir, file.path(i$base, "inference"))
  expect_true(dir.exists(file.path(i$base, "artifacts")) && dir.exists(file.path(i$base, "inference")))
  expect_silent(prevcarga_ctx(list(), i$code))                       # o ctx seguinte nao aborta nem avisa alias
})

test_that("o resumo 'chave: antigo -> novo' vai para a saida e so mostra o que mudou", {
  i <- sdi_install()
  out <- character(); con <- textConnection("out", "w", local = TRUE)
  inp <- textConnection(c("", "", "", "N"))
  prevcarga_set_data_dir_interactive(suppressWarnings(prevcarga_ctx(list(), i$code)), base = i$base, input = inp, is_tty = TRUE, output = con)
  close(con); close(inp)
  txt <- paste(out, collapse = "\n")
  expect_match(txt, paste0("data_train_dir: /velho -> ", file.path(i$base, "train")), fixed = TRUE)
  expect_match(txt, "artifacts_dir: - -> ", fixed = TRUE)
  expect_false(grepl("[^\x01-\x7f]", txt))                           # ASCII
})

test_that("[AC1] 'N' ou EOF antes da confirmacao: config.yaml byte a byte igual, nada criado, gravado FALSE", {
  i <- sdi_install(); antes <- tools::md5sum(i$cfg)
  for (ent in list(c("", "", "", "N"), c("", "", "", "qualquer"), c("", "", ""), character(), c("", ""))) {
    res <- sdi_run(i, ent)
    expect_false(res$gravado)
    expect_identical(unname(tools::md5sum(i$cfg)), unname(antes))
    expect_false(dir.exists(file.path(i$base, "artifacts"))); expect_false(dir.exists(file.path(i$base, "inference")))
  }
})

test_that("[AC2] sem terminal aborta contract.set_data_dir_noninteractive (exit 2) sem ler a entrada nem tocar no arquivo", {
  i <- sdi_install(); antes <- tools::md5sum(i$cfg)
  inp <- textConnection(c("", "", "", "s")); on.exit(close(inp))
  e <- tryCatch(prevcarga_set_data_dir_interactive(suppressWarnings(prevcarga_ctx(list(), i$code)), base = i$base, input = inp, is_tty = FALSE,
                                                   output = textConnection("o", "w", local = TRUE)),
                prevcarga_error = identity)
  expect_identical(e$id, "contract.set_data_dir_noninteractive"); expect_identical(e$exit_code, 2L)
  expect_length(readLines(inp, n = 1L), 1L)                          # a entrada continua intacta (nada lido)
  expect_identical(unname(tools::md5sum(i$cfg)), unname(antes))
  expect_identical(prevcarga_error_lookup("contract.set_data_dir_noninteractive")$class, "contract")
})
