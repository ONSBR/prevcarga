## test-errors.R -- contrato de erros ADR-0040 (M1.4b). Sourcing direto de scripts/lib/errors.R.
library(testthat)
local({
  here <- normalizePath(file.path("..", "errors.R"), winslash = "/", mustWork = TRUE)
  source(here, local = FALSE)
})
lib_dir <- normalizePath("..", winslash = "/")
options(prevcarga.lib = lib_dir)

write_tsv <- function(dir, name, rows, header = "id\tclass\tmessage\thint") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(paste(c(header, rows), collapse = "\n"), "\n"))),
           file.path(dir, name))
}
abort_id <- function(id, ...) tryCatch(prevcarga_abort(id, ...), prevcarga_error = identity)

test_that("tabela de exit-codes 0-15 identica ao ADR-0040", {
  expect_identical(prevcarga_exit_codes,
    c(ok = 0L, generic = 1L, contract = 2L, completude = 3L, topology = 4L, window = 5L,
      bundle = 6L, runtime = 7L, config = 8L, integrity = 9L, network = 10L,
      coherence = 11L, repro = 12L, state = 13L, partial = 14L, honesty = 15L))
  expect_true(all(prevcarga_error_catalog()$class %in% names(prevcarga_exit_codes)))
})

fixture_ids <- c("generic.unexpected", "contract.khat_range_invalid", "completude.unclassified",
  "topology.unclassified", "window.unclassified", "bundle.package_missing", "runtime.tz_unfixable",
  "config.usage", "integrity.freeze_sha_mismatch", "network.unclassified", "coherence.unclassified",
  "repro.unclassified", "state.official_set_incomplete", "partial.unclassified", "honesty.unclassified")

test_that("um abort por classe 1-15 renderiza texto e JSON coerentes", {
  expect_length(unique(sub("\\..*", "", fixture_ids)), 15L)
  for (id in fixture_ids) {
    cond <- abort_id(id, where = "x", details = list(k = 1))
    cls <- sub("\\..*", "", id)
    if (id == "bundle.package_missing") cls <- "bundle"
    expect_s3_class(cond, "prevcarga_error")
    expect_match(conditionMessage(cond), paste0("^\\[prevcarga/", cond$class_name, "\\] "))
    expect_identical(cond$exit_code, unname(prevcarga_exit_codes[[cond$class_name]]))
    txt <- prevcarga_render_error(cond, "text")
    expect_identical(txt[[1]], sprintf("prevcarga: erro [%s] (exit %d)", cond$class_name, cond$exit_code))
    expect_match(txt[[2]], "^o que: "); expect_identical(txt[[3]], "onde: x")
    expect_match(txt[[4]], "^como corrigir: ")
    js <- prevcarga_render_error(cond, "json", verb = "fx")
    expect_length(js, 1L)
    p <- jsonlite::fromJSON(js)
    expect_true(all(c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version") %in% names(p)))
    expect_true(all(c("class", "id", "message", "where", "hint", "details") %in% names(p$error)))
    expect_identical(p$error$class, cond$class_name)
    expect_identical(p$exit_code, cond$exit_code)
    expect_false(p$ok)
  }
})

test_that("id desconhecido => generic.unknown_error_id", {
  id <- paste0("nao", ".", "existe")
  cond <- abort_id(id)
  expect_identical(cond$id, "generic.unknown_error_id")
})

test_that("precedencia nos 6 conjuntos, nos dois sentidos [CONTROL de ordem]", {
  sets <- list(c("runtime", "config", "contract"), c("config", "contract", "window"),
               c("contract", "completude"), c("completude", "topology", "window"),
               c("window", "state"), c("integrity", "coherence"))
  want <- c("runtime", "config", "contract", "completude", "window", "integrity")
  for (i in seq_along(sets)) {
    expect_identical(prevcarga_exit_precedence(sets[[i]]), want[[i]])
    expect_identical(prevcarga_exit_precedence(rev(sets[[i]])), want[[i]])
  }
  expect_identical(unname(prevcarga_exit_codes[want]), c(7L, 8L, 2L, 3L, 5L, 9L))
  expect_error(prevcarga_exit_precedence("inventada"), "fora da tabela")
  # CONTROL: uma precedencia ingenua por ordem de emissao reprovaria
  expect_false(identical(sets[[1]][[1]], want[[1]]) && identical(rev(sets[[1]])[[1]], want[[1]]))
})

test_that("coletor poe as perdedoras em details/warnings", {
  col <- prevcarga_collect_errors()
  expect_false(col$has())
  col$add("window.unclassified", where = "w"); col$add("config.usage"); col$add("contract.seed_missing")
  expect_true(col$has())
  cond <- tryCatch(col$finish(), prevcarga_error = identity)
  expect_identical(cond$class_name, "config")
  expect_length(cond$details$other_errors, 2L)
  js <- jsonlite::fromJSON(prevcarga_render_error(cond, "json"), simplifyVector = FALSE)
  expect_length(js$warnings, 2L)
  expect_match(prevcarga_render_error(cond, "text")[[5]], "^prevcarga: aviso \\[")
})

test_that("stop('boom') => generic.unexpected; traceback so em debug", {
  f <- function() stop("boom")
  err0 <- capture.output(r0 <- prevcarga_with_errors(f(), debug = FALSE), type = "message")
  expect_identical(r0$exit_code, 1L); expect_identical(r0$cond$id, "generic.unexpected")
  expect_length(err0, 0L)
  for (fmt in c("text", "json")) {
    out <- prevcarga_render_error(r0$cond, fmt)
    expect_false(any(grepl("traceback|Error in", out)))
  }
  err1 <- capture.output(r1 <- prevcarga_with_errors(f(), debug = TRUE), type = "message")
  expect_identical(r1$exit_code, 1L)
  expect_true(any(grepl("f()", err1, fixed = TRUE)))
  ok <- prevcarga_with_errors(42)
  expect_identical(ok$exit_code, 0L); expect_identical(ok$value, 42)
  ce <- prevcarga_with_errors(prevcarga_abort("config.usage"))
  expect_identical(ce$exit_code, 8L)
})

test_that("catalogo real: ASCII, ids validos, ordem radix, sem duplicata", {
  cat <- prevcarga_errors_load()
  for (col in c("id", "message", "hint"))
    for (s in cat[[col]]) expect_false(any(as.integer(charToRaw(s)) > 127L))
  expect_true(all(grepl("^[a-z]+\\.[a-z_]+$", cat$id)))
  expect_false(anyDuplicated(cat$id) > 0)
  expect_identical(cat$id, sort(cat$id, method = "radix"))
  expect_identical(prevcarga_error_catalog(refresh = TRUE), cat)
  expect_true(all(c("contract.khat_range_invalid", "contract.long_schema_constant",
                    "bundle.package_missing", "state.official_set_incomplete") %in% cat$id))
  expect_false("partial.report_failed" %in% cat$id)
})

test_that("CONTROL: TSV acentuado reprova com contract.error_catalog_schema", {
  d <- file.path(tempfile("acc"))
  msg <- paste0("configura", intToUtf8(c(231, 227)), "o")
  write_tsv(d, "a.tsv", paste("config.usage", "config", msg, "dica", sep = "\t"))
  expect_error(prevcarga_catalog_assert_ascii(data.frame(id = "a.b", message = msg, hint = "x")),
               class = "prevcarga_error")
  expect_identical(tryCatch(prevcarga_errors_load(d), prevcarga_error = function(e) e$id),
                   "contract.error_catalog_schema")
})

test_that("CONTROL: id duplicado entre TSV, classe invalida e coluna faltando", {
  d <- tempfile("dup")
  row <- "config.usage\tconfig\tm\th"
  write_tsv(d, "a.tsv", row); write_tsv(d, "b.tsv", row)
  e <- tryCatch(prevcarga_errors_load(d), prevcarga_error = identity)
  expect_identical(e$id, "contract.error_catalog_dup")
  expect_identical(e$details$id, "config.usage")
  expect_setequal(e$details$files, c("a.tsv", "b.tsv"))
  d2 <- tempfile("cls"); write_tsv(d2, "a.tsv", "x.y\tinexistente\tm\th")
  expect_identical(tryCatch(prevcarga_errors_load(d2), prevcarga_error = function(e) e$id),
                   "contract.error_catalog_schema")
  d3 <- tempfile("col"); write_tsv(d3, "a.tsv", "x.y\tconfig\tm", header = "id\tclass\tmessage")
  expect_identical(tryCatch(prevcarga_errors_load(d3), prevcarga_error = function(e) e$id),
                   "contract.error_catalog_schema")
  d4 <- tempfile("bad"); write_tsv(d4, "a.tsv", "Bad-Id\tconfig\tm\th")
  expect_identical(tryCatch(prevcarga_errors_load(d4), prevcarga_error = function(e) e$id),
                   "contract.error_catalog_schema")
})

test_that("export gera a uniao ordenada por id", {
  tf <- tempfile(fileext = ".tsv")
  prevcarga_export_error_catalog(tf)
  raw <- readBin(tf, "raw", file.size(tf))
  expect_false(as.integer(raw[1]) == 0xEF); expect_false(as.raw(13L) %in% raw)
  g <- utils::read.delim(tf, quote = "", colClasses = "character", comment.char = "")
  expect_identical(names(g), c("id", "class", "exit_code", "message", "hint"))
  cat <- prevcarga_errors_load()
  expect_identical(g$id, cat$id)
  expect_identical(g$message, cat$message)
  expect_identical(as.integer(g$exit_code), unname(prevcarga_exit_codes[cat$class]))
})
