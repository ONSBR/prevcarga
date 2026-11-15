## test-data-options.R -- opcoes globais de dados por papel (E1-T2): --data-train, --data-inference, --data-artifacts, apelidos
## sem apelidos (ADR-044, clean break): --data, --artifacts-dir e a chave data_dir deixam de existir (opcao desconhecida / config.unknown_key).
## Prefixo docopt. Fixture sintetica em tempdir.
library(testthat)
lib_dir <- file.path(pc_root(), "scripts", "lib")
withr::local_options(prevcarga.lib = lib_dir)
E <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli"))

nrm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
ctx_w <- function(args, cfg = NULL) {
  r <- withr::local_tempdir("do-root-")
  if (!is.null(cfg)) writeLines(c("schema_version: 1", cfg), file.path(r, "config.yaml"))
  ws <- list()
  ctx <- withCallingHandlers(E$prevcarga_ctx(args, r),
    prevcarga_warning = function(w) { ws[[length(ws) + 1L]] <<- w; invokeRestart("muffleWarning") })
  list(ctx = ctx, ws = ws)
}
abort_of <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

test_that("[AC0] nomes novos resolvem os tres papeis sem warning; data_dir (ativo) = data_train_dir", {
  ad <- withr::local_tempdir("art-")
  x <- ctx_w(list(`--data-train` = "/t", `--data-inference` = "/i", `--data-artifacts` = ad))
  expect_identical(x$ctx$data_train_dir, nrm("/t"))
  expect_identical(x$ctx$data_inference_dir, nrm("/i"))
  expect_identical(x$ctx$artifacts_dir, nrm(ad))
  expect_identical(x$ctx$data_dir, x$ctx$data_train_dir)
  expect_length(x$ws, 0L)
  ## CONTROL: sem nada, os papeis ficam NULL
  y <- ctx_w(list())
  expect_null(y$ctx$data_train_dir); expect_null(y$ctx$data_inference_dir); expect_null(y$ctx$artifacts_dir)
})

test_that("[AC1] sem apelidos: --data, --artifacts-dir e data_dir nao resolvem nada; a CLI recusa a opcao antiga", {
  skip_if_not_installed("docopt")
  d <- withr::local_tempdir("dt-"); ad <- withr::local_tempdir("art-")
  verbs <- E$prevcarga_verbs_discover()
  for (old in list(paste0("--data=", d), paste0("--artifacts-dir=", ad))) {
    utils::capture.output(r <- E$prevcarga_main(c("forecast", old, "--format=json"), root = pc_root(), runtime = function() NULL))
    expect_identical(r, 8L, info = old)                   # config.usage (catalogo): opcao desconhecida
    expect_null(E$.prevcarga_parse(c("forecast", old), verbs), info = old)
  }
  ## CONTROL: o nome novo parseia
  expect_false(is.null(E$.prevcarga_parse(c("forecast", paste0("--data-train=", d), paste0("--data-artifacts=", ad)), verbs)))
  ## o ctx ignora as chaves antigas
  x <- ctx_w(list(`--data` = d, `--artifacts-dir` = ad))
  expect_null(x$ctx$data_train_dir); expect_null(x$ctx$artifacts_dir)
  expect_length(x$ws, 0L)
})

test_that("[AC2] nome novo vale por papel, sem conflito possivel com apelido; flag vence o config", {
  x1 <- withr::local_tempdir("x-")
  y <- ctx_w(list(`--data-train` = "/a", `--data-artifacts` = x1), cfg = c("data_train_dir: /b", "artifacts_dir: /nao/usado"))
  expect_identical(y$ctx$data_train_dir, nrm("/a")); expect_identical(y$ctx$artifacts_dir, nrm(x1))
  expect_length(y$ws, 0L)
})

test_that("[AC3] config.yaml com a chave legada data_dir => config.unknown_key (exit 8); chaves novas validadas", {
  e <- abort_of(ctx_w(list(), cfg = "data_dir: /velho"))
  expect_identical(e$id, "config.unknown_key"); expect_identical(e$exit_code, 8L); expect_identical(e$where, "data_dir")
  x <- ctx_w(list(), cfg = c("data_train_dir: /novo", "data_inference_dir: /inf"))     # CONTROL
  expect_identical(x$ctx$data_train_dir, nrm("/novo"))
  expect_identical(x$ctx$data_inference_dir, nrm("/inf"))
  expect_length(x$ws, 0L)
  expect_identical(ctx_w(list(`--data-train` = "/f"), cfg = "data_train_dir: /novo")$ctx$data_train_dir, nrm("/f"))
  f <- tempfile(fileext = ".yaml"); writeLines(c("schema_version: 1", "data_train_dir: 3"), f)
  e <- abort_of(E$prevcarga_config_load(f))
  expect_identical(e$id, "config.bad_type")
  writeLines(c("schema_version: 1", "data_inference_dir: [a, b]"), f)
  expect_identical(abort_of(E$prevcarga_config_load(f))$id, "config.bad_type")
  ## --data-artifacts inexistente
  m <- abort_of(ctx_w(list(`--data-artifacts` = file.path(tempdir(), "nao-existe-xyz"))))
  expect_identical(m$id, "config.artifacts_dir_missing"); expect_identical(m$exit_code, 8L)
  ## o catalogo nao tem mais o aviso de apelido
  expect_false("warning.option_alias" %in% E$prevcarga_errors_load(file.path(pc_root(), "scripts", "lib", "errors"))$id)
})

test_that("[AC4] docopt: cada opcao casa o proprio nome; a usage nao cita mais --data nem --artifacts-dir", {
  skip_if_not_installed("docopt")
  verbs <- E$prevcarga_verbs_discover()
  usage <- E$prevcarga_usage(verbs)
  expect_false(grepl("--data=<dir>", usage, fixed = TRUE))
  expect_false(grepl("--artifacts-dir", usage, fixed = TRUE))
  expect_false(grepl("apelido", usage, fixed = TRUE))
  p <- docopt::docopt(usage, args = c("forecast", "--data-train=/t", "--data-artifacts=/r"),
                      help = FALSE, strip_names = FALSE)
  expect_identical(p[["--data-train"]], "/t")
  expect_identical(p[["--data-artifacts"]], "/r")
  expect_true(is.null(p[["--data-inference"]]) || identical(p[["--data-inference"]], FALSE) || is.na(p[["--data-inference"]]))
  ## nenhuma opcao NOVA e prefixo estrito de outra, nem tem outra como prefixo
  fl <- unique(E$.prevcarga_opt_lines(verbs)$flag)
  for (n in c("--data-train", "--data-inference", "--data-artifacts")) {
    expect_identical(sum(startsWith(fl, n) & fl != n), 0L, info = n)
    expect_identical(fl[startsWith(n, fl) & fl != n], character(), info = n)
  }
  ## CONTROL: --data-a* (prefixo de --data-artifacts) nao colide com --artifact-dir do combine
  expect_false(any(startsWith("--artifact-dir", "--data-a")))
})
