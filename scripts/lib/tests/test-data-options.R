## test-data-options.R -- opcoes globais de dados por papel (E1-T2): --data-train, --data-inference, --data-artifacts, apelidos
## (--data, --artifacts-dir, data_dir) com warning.option_alias, conflito => config.usage, prefixo docopt. Fixture sintetica em tempdir.
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

test_that("[AC1] apelidos --data, --artifacts-dir e data_dir valem como os nomes novos e emitem warning.option_alias", {
  d <- withr::local_tempdir("dt-"); ad <- withr::local_tempdir("art-")
  x <- ctx_w(list(`--data` = d, `--artifacts-dir` = ad))
  expect_identical(x$ctx$data_train_dir, nrm(d))
  expect_identical(x$ctx$artifacts_dir, nrm(ad))
  expect_identical(vapply(x$ws, function(w) w$id, ""), rep("warning.option_alias", 2L))
  al <- lapply(x$ws, function(w) w$details[c("alias", "novo")])
  expect_true(any(vapply(al, identical, NA, list(alias = "--data", novo = "--data-train"))))
  expect_true(any(vapply(al, identical, NA, list(alias = "--artifacts-dir", novo = "--data-artifacts"))))
  z <- ctx_w(list(), cfg = sprintf("data_dir: %s", d))
  expect_identical(z$ctx$data_train_dir, nrm(d))
  expect_identical(z$ws[[1L]]$details$alias, "data_dir")
  expect_identical(z$ws[[1L]]$details$novo, "data_train_dir")
})

test_that("[AC2] apelido x nome novo com valores diferentes => config.usage (exit 8); mesmo valor nao aborta", {
  a <- abort_of(ctx_w(list(`--data` = "/a", `--data-train` = "/b")))
  expect_identical(a$id, "config.usage"); expect_identical(a$exit_code, 8L)
  expect_match(a$where, "--data", fixed = TRUE); expect_match(a$where, "--data-train", fixed = TRUE)
  x1 <- withr::local_tempdir("x-"); y1 <- withr::local_tempdir("y-")
  b <- abort_of(ctx_w(list(`--artifacts-dir` = x1, `--data-artifacts` = y1)))
  expect_identical(b$id, "config.usage"); expect_identical(b$exit_code, 8L)
  expect_match(b$where, "--artifacts-dir", fixed = TRUE); expect_match(b$where, "--data-artifacts", fixed = TRUE)
  ## CONTROL
  expect_null(abort_of(ctx_w(list(`--data` = "/a", `--data-train` = "/a"))))
  expect_null(abort_of(ctx_w(list(`--artifacts-dir` = x1, `--data-artifacts` = x1))))
})

test_that("[AC3] config.yaml com data_dir e data_train_dir diferentes: vence data_train_dir com aviso; chaves novas validadas", {
  x <- ctx_w(list(), cfg = c("data_dir: /velho", "data_train_dir: /novo", "data_inference_dir: /inf"))
  expect_identical(x$ctx$data_train_dir, nrm("/novo"))
  expect_identical(x$ctx$data_inference_dir, nrm("/inf"))
  expect_identical(x$ws[[1L]]$id, "warning.option_alias")
  ## CONTROL: so data_train_dir => sem aviso
  expect_length(ctx_w(list(), cfg = "data_train_dir: /novo")$ws, 0L)
  ## flag vence o config
  expect_identical(ctx_w(list(`--data-train` = "/f"), cfg = "data_train_dir: /novo")$ctx$data_train_dir, nrm("/f"))
  f <- tempfile(fileext = ".yaml"); writeLines(c("schema_version: 1", "data_train_dir: 3"), f)
  e <- abort_of(E$prevcarga_config_load(f))
  expect_identical(e$id, "config.bad_type")
  writeLines(c("schema_version: 1", "data_inference_dir: [a, b]"), f)
  expect_identical(abort_of(E$prevcarga_config_load(f))$id, "config.bad_type")
  ## --data-artifacts inexistente
  m <- abort_of(ctx_w(list(`--data-artifacts` = file.path(tempdir(), "nao-existe-xyz"))))
  expect_identical(m$id, "config.artifacts_dir_missing"); expect_identical(m$exit_code, 8L)
})

test_that("[AC4] docopt: cada opcao casa o proprio nome; usage marca --data e --artifacts-dir como apelido", {
  skip_if_not_installed("docopt")
  verbs <- E$prevcarga_verbs_discover()
  usage <- E$prevcarga_usage(verbs)
  expect_match(usage, "--data=<dir>             (apelido de --data-train", fixed = TRUE)
  expect_match(usage, "--artifacts-dir=<dir>    (apelido de --data-artifacts", fixed = TRUE)
  p <- docopt::docopt(usage, args = c("forecast", "--data-train=/t", "--data=/u", "--data-artifacts=/r"),
                      help = FALSE, strip_names = FALSE)
  expect_identical(p[["--data-train"]], "/t")
  expect_identical(p[["--data"]], "/u")
  expect_identical(p[["--data-artifacts"]], "/r")
  expect_true(is.null(p[["--data-inference"]]) || identical(p[["--data-inference"]], FALSE) || is.na(p[["--data-inference"]]))
  ## nenhuma opcao NOVA e prefixo estrito de outra, nem tem outra como prefixo, salvo o apelido --data (exato, ver acima)
  fl <- unique(E$.prevcarga_opt_lines(verbs)$flag)
  for (n in c("--data-train", "--data-inference", "--data-artifacts")) {
    expect_identical(sum(startsWith(fl, n) & fl != n), 0L, info = n)
    expect_identical(setdiff(fl[startsWith(n, fl) & fl != n], "--data"), character(), info = n)
  }
  ## CONTROL: --data-a* (prefixo de --data-artifacts) nao colide com --artifact-dir do combine
  expect_false(any(startsWith("--artifact-dir", "--data-a")))
})
