## test-errors-catalog-sync.R -- todo prevcarga_abort("<id>") do codigo consta no catalogo gerado (M1.4b)
library(testthat)
source(normalizePath(file.path("..", "errors.R"), winslash = "/", mustWork = TRUE))
repo <- normalizePath(file.path("..", "..", ".."), winslash = "/")
options(prevcarga.lib = file.path(repo, "scripts", "lib"))

rscript <- file.path(R.home("bin"), "Rscript")
gen <- tempfile(fileext = ".tsv")
res <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(file.path(repo, "scripts", "tools", "export_error_catalog.R")),
                                           "--out", shQuote(gen)), stdout = TRUE, stderr = TRUE))
generated <- utils::read.delim(gen, quote = "", colClasses = "character", comment.char = "")

referenced_ids <- function(extra_dirs = character()) {
  files <- unlist(lapply(c(file.path(repo, "scripts"), file.path(repo, "models"), extra_dirs),
                         function(d) if (dir.exists(d)) list.files(d, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)))
  ids <- unlist(lapply(files, function(f) {
    m <- regmatches(l <- readLines(f, warn = FALSE, encoding = "UTF-8"),
                    gregexpr("prevcarga_abort\\(\"([a-z_.]+)\"", l))
    sub("^prevcarga_abort\\(\"([a-z_.]+)\"$", "\\1", unlist(m))
  }))
  unique(ids)
}

test_that("tool gera catalogo identico a uniao dos TSV", {
  expect_true(any(grepl("^error-catalog: [0-9]+ linhas", res)))
  expect_identical(names(generated), c("id", "class", "exit_code", "message", "hint"))
  cat <- prevcarga_errors_load()
  expect_identical(generated$id, cat$id)
  expect_identical(generated$message, cat$message)
  expect_identical(generated$hint, cat$hint)
  expect_identical(nrow(generated), nrow(cat))
})

test_that("todo error.id referido no codigo consta no catalogo; CONTROL: id inventado reprova", {
  missing <- setdiff(referenced_ids(), generated$id)
  expect_identical(missing, character())
  d <- tempfile("ctl"); dir.create(d)
  writeLines(paste0("prevcarga_abort(", "\"", "nao.existe_mesmo", "\")"), file.path(d, "x.R"))
  missing2 <- setdiff(referenced_ids(extra_dirs = d), generated$id)
  expect_identical(missing2, "nao.existe_mesmo")
})

test_that("catalogo gerado nao e versionado", {
  gi <- readLines(file.path(repo, ".gitignore"), warn = FALSE, encoding = "UTF-8")
  expect_true("/install/error-catalog.tsv" %in% gi)
  tracked <- suppressWarnings(system2("git", c("-C", shQuote(repo), "ls-files", "install/error-catalog.tsv"), stdout = TRUE))
  expect_length(tracked, 0L)
})
