## test-ascii-bytes.R -- toda saida de terminal e ASCII puro (ADR-0040; M1.V AC 13) e o banner aparece so
## onde deve (--help/--version/sem verbo/doctor texto, com stdout em terminal; nunca em models, em JSON nem com stdout
## redirecionado -- blueprint "Banner da CLI"); ESC so com o banner num terminal e --color=always (ou auto fora do Windows).

fx_names <- c("accent")
root <- pc_install_copy(fixtures = fx_names)
cli_dir <- file.path(root, "scripts", "lib", "cli")
withr::local_options(prevcarga.lib = file.path(root, "scripts", "lib"))
E <- pc_source_lib("errors")
sys.source(file.path(root, "scripts", "lib", "cli.R"), envir = E)
real_verbs <- sub("\\.R$", "", list.files(cli_dir, pattern = "\\.R$"))
real_verbs <- setdiff(real_verbs, paste0("fixture-", fx_names))
stubs <- setdiff(names(E$prevcarga_verbs_planned), real_verbs)
banner_head <- "  Previsao de carga do SIN - ONS"

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

test_that("[FORBIDDEN] stdout redirecionado: sem banner nem ESC, nem com --color=always; models e JSON nunca", {
  has <- function(r) banner_head %in% r$stdout
  for (a in list("--help", "--version", character(), "doctor")) for (c1 in c("never", "always")) {
    r <- pc_cli(root, c(a, paste0("--color=", c1)))
    expect_identical(r$status, 0L); expect_false(has(r), info = paste(c(a, c1), collapse = " "))
    expect_false(any(r$raw_stdout == as.raw(27L)), info = paste(c(a, c1), collapse = " "))
  }
  expect_true(any(grepl("^prevcarga [0-9]", pc_cli(root, c("--version", "--color=never"))$stdout)))   # a versao continua
  expect_false(has(pc_cli(root, c("models", "--color=never"))))
  for (a in list("doctor", "models", "areas", "status")) expect_false(has(pc_cli(root, c(a, "--format=json"))))
})

test_that("[CONTROL] sob pty: banner em --help/--version/sem verbo/doctor texto; ausente em models e JSON; ESC so com always/auto", {
  probe <- pc_cli_pty(root, "--version")
  if (is.null(probe)) skip(if (identical(.Platform$OS.type, "windows")) "nao-windows: sem pty no Windows para o CONTROL do banner em terminal"
                           else "util-linux script ausente: sem pty para o CONTROL do banner em terminal")
  has <- function(r) banner_head %in% r$lines
  sgr <- function(r) grepl(paste0(intToUtf8(27L), "\\[[0-9;]*m"), rawToChar(r$raw))   # cor ANSI (SGR); o pty pode ecoar ESC[?25h
  for (a in list("--help", "--version", character(), "doctor")) {
    r <- pc_cli_pty(root, c(a, "--color=never"))
    expect_true(has(r), info = paste(a, collapse = " "))
    expect_false(sgr(r), info = paste(a, collapse = " "))
  }
  expect_false(has(pc_cli_pty(root, c("models", "--color=never"))))
  for (a in list("doctor", "--help")) expect_false(has(pc_cli_pty(root, c(a, "--format=json"))))
  al <- pc_cli_pty(root, c("--help", "--color=always"))
  expect_true(sgr(al))
  expect_no_error(pc_assert_ascii(al$raw, "banner always"))
  expect_identical(sgr(pc_cli_pty(root, c("--help", "--color=auto"))), !identical(.Platform$OS.type, "windows"))
})
