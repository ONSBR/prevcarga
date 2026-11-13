## test-errors-hint-verbs.R -- todo verbo `prevcarga <verbo>` citado num hint do catalogo existe em scripts/lib/cli/
## (ou e um flag global). Um hint que manda rodar um verbo inexistente (ex.: `prevcarga train`) reprova aqui.
library(testthat)
source(normalizePath(file.path("..", "errors.R"), winslash = "/", mustWork = TRUE))
lib_dir <- normalizePath("..", winslash = "/")
options(prevcarga.lib = lib_dir)

verbs <- sub("\\.R$", "", list.files(file.path(lib_dir, "cli"), pattern = "\\.R$"))
cited_verbs <- function(texts) {
  m <- regmatches(texts, gregexpr("prevcarga[ ]+[a-z][a-z-]*", texts))
  unique(sub("^prevcarga[ ]+", "", unlist(m)))
}
## palavras que seguem "prevcarga " em prosa e nao sao verbo
not_verbs <- c("instalado")

test_that("[FORBIDDEN] nenhum hint ou mensagem cita verbo inexistente", {
  cat <- prevcarga_errors_load()
  cited <- setdiff(cited_verbs(c(cat$hint, cat$message)), not_verbs)
  expect_true(length(cited) > 0L)
  expect_identical(setdiff(cited, verbs), character(0))
})

test_that("[CONTROL] o detector pega um verbo inexistente e aceita os reais", {
  expect_identical(setdiff(cited_verbs("rode `prevcarga train` agora"), verbs), "train")
  expect_identical(setdiff(cited_verbs("rode `prevcarga train-models` e `prevcarga registry ls`"), verbs), character(0))
  expect_identical(cited_verbs("use o prevcarga-sync"), character(0))
})
