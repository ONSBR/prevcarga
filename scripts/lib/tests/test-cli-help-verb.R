## test-cli-help-verb.R -- ajuda por verbo (F2.3): `<verbo> --help` lista so as opcoes do verbo; a gramatica docopt
## (prevcarga_usage) nao muda. Parte in-process com verbos-fixture; parte de processo sobre instalacao-copia.

lib_dir <- file.path(pc_root(), "scripts", "lib")
withr::local_options(prevcarga.lib = lib_dir)
new_env <- function() pc_source_lib(c("errors", "runtime", "config", "banner", "cli"))

write_fx <- function(dir, verb, usage, options, scoped = character()) {
  q <- function(x) if (length(x)) sprintf("c(%s)", paste0("'", x, "'", collapse = ",")) else "character()"
  fn <- paste0("cmd_", gsub("-", "_", verb))
  writeLines(c(sprintf("%s <- function(ctx, args) NULL", fn),
               sprintf("prevcarga_verb_%s <- list(name = '%s', usage = '%s', options = %s, scoped = %s,",
                       gsub("-", "_", verb), verb, usage, q(options), q(scoped)),
               sprintf("  handler = %s, owner = 'fixture', exempt = FALSE, modules = character())", fn)),
             file.path(dir, paste0(verb, ".R")))
}

fx_verbs <- function(E) {
  d <- tempfile("help-fx-"); dir.create(d)
  write_fx(d, "alfa", "prevcarga alfa [--shared] [--only-a=<n>]",
           c("--shared              Opcao compartilhada (linha so no verbo A)", "--only-a=<n>          So do verbo A"),
           scoped = "--only-a")
  write_fx(d, "beta", "prevcarga beta [--origin=<data>] [--shared] [--own=<n>]",
           c("--own=<n>             Opcao propria de B"), scoped = "--shared")
  E$prevcarga_verbs_discover(d, planned = character())
}

test_that("prevcarga_usage_verb: opcao compartilhada aparece em B, comuns = globais menos --origin/--model, opcoes de A nao citadas ficam fora", {
  E <- new_env(); v <- fx_verbs(E)
  h <- E$prevcarga_usage_verb(v, "beta")
  expect_match(h, "Usage:\n  prevcarga beta [--origin=<data>] [--shared] [--own=<n>] [options]", fixed = TRUE)
  expect_true(grepl("--shared  ", h, fixed = TRUE))
  expect_true(grepl("--own=<n>", h, fixed = TRUE))
  expect_true(grepl("--data=<dir>", h, fixed = TRUE))            # comum
  expect_true(grepl("--origin=<data>  ", h, fixed = TRUE))       # citada na usage de B
  expect_false(grepl("--model=<m>", h, fixed = TRUE))            # comum excluida e nao citada
  expect_false(grepl("--only-a", h, fixed = TRUE))
  ln <- strsplit(h, "\n", fixed = TRUE)[[1L]]
  expect_identical(anyDuplicated(ln[nzchar(ln)]), 0L)            # sem linha repetida
  ha <- E$prevcarga_usage_verb(v, "alfa")
  expect_true(grepl("--only-a=<n>", ha, fixed = TRUE)); expect_false(grepl("--own", ha, fixed = TRUE))
})

test_that(".prevcarga_opt_lines: global primeiro, depois verbos; opt_table derivada com o mesmo resultado", {
  E <- new_env(); v <- fx_verbs(E)
  ol <- E$.prevcarga_opt_lines(v)
  expect_true(all(c("flag", "line", "source") %in% names(ol)))
  expect_identical(ol$source[[1L]], "global")
  expect_identical(unique(ol$source), c("global", "alfa", "beta"))
  expect_identical(ol$source[ol$flag == "--shared"], "alfa")
  tb <- E$.prevcarga_opt_table(v)
  expect_true(isTRUE(tb[["--data"]])); expect_false(isTRUE(tb[["--shared"]])); expect_true(isTRUE(tb[["--own"]]))
})

test_that("prevcarga_usage (gramatica docopt) identico ao texto de referencia: o filtro e so de apresentacao", {
  E <- new_env(); v <- fx_verbs(E)
  vl <- vapply(v$usage, function(u) if (grepl("[options]", u, fixed = TRUE)) u else paste(u, "[options]"), "", USE.NAMES = FALSE)
  own <- unique(unlist(v$options))
  ref <- paste(c("prevcarga - CLI modular de previsao de carga", "", "Usage:", paste0("  ", vl),
                 "  prevcarga (-h | --help) [options]", "  prevcarga --version [options]", "  prevcarga [options]", "",
                 "Options:", E$.prevcarga_global_options(), paste0("  ", sub("^\\s+", "", own))), collapse = "\n")
  expect_identical(E$prevcarga_usage(v), ref)
})

test_that(".prevcarga_help_verb pula o valor de flag que toma argumento", {
  E <- new_env(); v <- fx_verbs(E)
  expect_identical(E$.prevcarga_help_verb(c("--data", "beta", "alfa", "--help"), v), "alfa")
  expect_identical(E$.prevcarga_help_verb(c("--data=beta", "alfa", "-h"), v), "alfa")
  expect_identical(E$.prevcarga_help_verb(c("beta", "--help"), v), "beta")
  expect_null(E$.prevcarga_help_verb(c("--help"), v))
  expect_null(E$.prevcarga_help_verb(c("desconhecido", "--help"), v))
})

test_that("processo: forecast --help so com as opcoes do forecast; train-models --help com as do train-models", {
  skip_if_not_installed("docopt")
  root <- pc_install_copy()
  f <- pc_cli(root, c("forecast", "--help"))
  expect_identical(f$status, 0L)
  expect_true(all(as.integer(f$raw_stdout) < 128L))
  expect_match(f$stdout[grepl("^  prevcarga ", f$stdout)][[1L]], "prevcarga forecast", fixed = TRUE)
  for (x in c("--protocol", "--corte", "--scratch", "--keep-going")) expect_false(any(grepl(x, f$stdout, fixed = TRUE)), info = x)
  t <- pc_cli(root, c("train-models", "--help"))
  expect_identical(t$status, 0L)
  expect_true(all(as.integer(t$raw_stdout) < 128L))
  for (x in c("--dry-run", "--jobs")) expect_true(any(grepl(x, t$stdout, fixed = TRUE)), info = x)
  expect_identical(pc_cli(root, c("train-models", "-h"))$stdout, t$stdout)
  j <- pc_cli(root, c("--format=json", "forecast", "--help"))
  expect_identical(j$status, 0L)
  expect_false(any(grepl("^\\s*\\{", j$stdout)))
  expect_identical(j$stdout, f$stdout)
})

test_that("processo: prevcarga --help sem verbo e byte a byte o prevcarga_usage(verbs)", {
  skip_if_not_installed("docopt")
  root <- pc_install_copy()
  E <- new_env()
  verbs <- E$prevcarga_verbs_discover(file.path(root, "scripts", "lib", "cli"))
  h <- pc_cli(root, "--help")
  expect_identical(h$status, 0L)
  expect_identical(h$stdout, strsplit(E$prevcarga_usage(verbs), "\n", fixed = TRUE)[[1L]])
})
