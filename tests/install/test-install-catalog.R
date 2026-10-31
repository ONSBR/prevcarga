## test-install-catalog.R -- install.sh le o catalogo GERADO (install/error-catalog.tsv: id,class,exit_code,message,hint) pelo
## NOME das colunas. Nao precisa de bundle em dist/: extrai so pc_catalog_row do install.sh e o roda em /bin/sh contra o catalogo
## gerado por scripts/tools/export_error_catalog.R.

pcc_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)

pcc_catalog <- function() {
  out <- tempfile(fileext = ".tsv")
  st <- system2(file.path(R.home("bin"), "Rscript"),
                c("--vanilla", shQuote(file.path(pcc_root, "scripts", "tools", "export_error_catalog.R")), "--out", shQuote(out)),
                stdout = FALSE, stderr = FALSE)
  testthat::expect_identical(as.integer(st), 0L)
  out
}

## so a funcao pc_catalog_row do install.sh (do cabecalho ate o `esac; }`), num arquivo a parte
pcc_row_fn <- function(src = file.path(pcc_root, "install", "install.sh")) {
  l <- readLines(src)
  i <- grep("^pc_catalog_row\\(\\) \\{", l); j <- grep("^  esac; \\}", l); j <- j[j > i][1L]
  f <- tempfile(fileext = ".sh"); writeLines(l[i:j], f); f
}

pcc_row <- function(fn, catalog, id) {
  out <- system2("/bin/sh", c("-c", shQuote(sprintf(". %s; PC_CATALOG=%s; pc_catalog_row %s", shQuote(fn), shQuote(catalog), id))),
                 stdout = TRUE, stderr = FALSE)
  strsplit(out[[1L]], "\t", fixed = TRUE)[[1L]]
}

test_that("[AC7] install.sh le message e hint do catalogo GERADO pelo nome da coluna (nao a coluna exit_code)", {
  cat_file <- pcc_catalog()
  gen <- utils::read.delim(cat_file, sep = "\t", quote = "", comment.char = "", colClasses = "character")
  expect_true(all(c("id", "class", "exit_code", "message", "hint") %in% names(gen)))
  fn <- pcc_row_fn()
  for (id in grep("^install\\.", gen$id, value = TRUE)) {
    row <- pcc_row(fn, cat_file, id)
    g <- gen[gen$id == id, ]
    expect_identical(row, c(g$id, g$class, g$message, g$hint), info = id)
    expect_false(grepl("^[0-9]+$", row[[3L]]), info = id)                      # a mensagem nunca e o exit_code
  }
})

test_that("[CONTROL] ler por posicao (cut -f3) no catalogo gerado devolve o exit_code como mensagem", {
  cat_file <- pcc_catalog()
  gen <- utils::read.delim(cat_file, sep = "\t", quote = "", comment.char = "", colClasses = "character")
  id <- "install.sha_mismatch"
  pos <- system2("/bin/sh", c("-c", shQuote(sprintf("awk -F'\\t' -v id=%s '$1==id {print; exit}' %s | cut -f3", id, shQuote(cat_file)))),
                 stdout = TRUE)
  expect_identical(pos, gen$exit_code[gen$id == id])
  expect_true(grepl("^[0-9]+$", pos))
})
