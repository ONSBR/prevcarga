## test-ascii-bytes.R -- toda saida de terminal e ASCII puro (ADR-0040; M1.V AC 13) e o banner aparece so
## onde deve (--help/--version/sem verbo/doctor texto; nunca em models nem em JSON); ESC so com --color=always.

fx_names <- c("accent")
root <- pc_install_copy(fixtures = fx_names)
cli_dir <- file.path(root, "scripts", "lib", "cli")
withr::local_options(prevcarga.lib = file.path(root, "scripts", "lib"))
E <- pc_source_lib("errors")
sys.source(file.path(root, "scripts", "lib", "cli.R"), envir = E)
real_verbs <- sub("\\.R$", "", list.files(cli_dir, pattern = "\\.R$"))
real_verbs <- setdiff(real_verbs, paste0("fixture-", fx_names))
stubs <- setdiff(names(E$prevcarga_verbs_planned), real_verbs)
banner_head <- "prevcarga - previsao de carga (CLI modular)"

test_that("pc_assert_ascii: aceita ASCII e reprova qualquer byte > 127", {
  expect_true(pc_assert_ascii(charToRaw("ok ascii\n")))
  expect_error(pc_assert_ascii(as.raw(c(0x61, 0xc3, 0xa7)), "x"), "byte\\(s\\) > 127 \\(1o na posicao 2\\)")
})

test_that("[CONTROL] fixture-accent: mensagem acentuada reprova pc_assert_ascii", {
  r <- pc_cli(root, "fixture-accent")
  expect_identical(r$status, 0L)
  expect_true(any(as.integer(r$raw) > 127L))
  expect_error(pc_assert_ascii(r$raw), "byte\\(s\\) > 127")
})

test_that("[FORBIDDEN] --help, --version, sem verbo, verbos do M1 e stubs: so ASCII", {
  cases <- c(list("--help", "--version", character()), lapply(real_verbs, identity), lapply(stubs, identity))
  for (a in cases) for (extra in list(character(), "--format=json", "--color=always")) {
    r <- pc_cli(root, c(a, extra))
    expect_no_error(pc_assert_ascii(r$raw, paste(c(a, extra), collapse = " ")))
  }
})

test_that("[FORBIDDEN] todo id do catalogo renderizado (texto e JSON) e ASCII; o TSV tambem", {
  cat <- E$prevcarga_errors_load(dir = file.path(root, "scripts", "lib", "errors"))
  for (i in seq_len(nrow(cat))) {
    cond <- E$prevcarga_condition(cat$id[[i]], where = "x")
    for (fmt in c("text", "json"))
      expect_no_error(pc_assert_ascii(charToRaw(paste(E$prevcarga_render_error(cond, fmt), collapse = "\n")), cat$id[[i]]))
  }
  for (f in Sys.glob(file.path(root, "scripts", "lib", "errors", "*.tsv")))
    expect_no_error(pc_assert_ascii(readBin(f, "raw", file.info(f)$size), basename(f)))
})

test_that("banner: presente em --help/--version/sem verbo/doctor texto; ausente em models e em qualquer JSON", {
  has <- function(r) banner_head %in% r$stdout
  for (a in list("--help", "--version", character(), "doctor")) {
    r <- pc_cli(root, c(a, "--color=never"))
    expect_identical(r$status, 0L); expect_true(has(r), info = paste(a, collapse = " "))
  }
  expect_false(has(pc_cli(root, c("models", "--color=never"))))
  for (a in list("doctor", "models", "areas", "status")) expect_false(has(pc_cli(root, c(a, "--format=json"))))
})

test_that("ESC (0x1b) so com --color=always; never/auto sem TTY nao emitem cor", {
  esc <- function(c1) any(pc_cli(root, c("--help", paste0("--color=", c1)))$raw_stdout == as.raw(27L))
  expect_true(esc("always"))
  expect_false(esc("never"))
  expect_false(esc("auto"))
  expect_no_error(pc_assert_ascii(pc_cli(root, c("--help", "--color=always"))$raw, "banner always"))
})
