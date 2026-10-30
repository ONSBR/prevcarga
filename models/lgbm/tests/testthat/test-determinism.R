## test-determinism.R -- LGBM.6 (gate10_determinismo): duas cadeias train + freeze + infer com entradas identicas, na mesma
## sessao e em duas sessoes R limpas (`Rscript --vanilla`, argumentos por vetor, sem env), dao o mesmo sha256 do artefato e a
## mesma entrega. O ambiente do chamador (globalenv, search, .Random.seed) fica intacto. CONTROL: seed 124 numa copia do run.yaml.
library(testthat)

LGD_DECK <- lgbm_fx_deck(days = 120L)

lgd_state <- function() list(ls = ls(globalenv(), all.names = TRUE), search = search(),
                             seed = if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()))

## sessao R limpa: o script carrega os helpers do teste e grava a cadeia em `out` (so argumentos, nenhuma variavel de ambiente)
lgd_clean_session <- function(out) {
  script <- tempfile("lgbm-chain-", fileext = ".R")
  writeLines(c("a <- commandArgs(trailingOnly = TRUE)",
               "setwd(a[2])",
               "source('helper-lgbm.R'); source('helper-lgbm-train.R')",
               "saveRDS(lgbm_fx_chain(), a[1])"), script)
  st <- system2(file.path(R.home("bin"), "Rscript"),
                c("--vanilla", shQuote(script), shQuote(out), shQuote(normalizePath(testthat::test_path(), winslash = "/"))),
                stdout = TRUE, stderr = TRUE)
  expect_true(file.exists(out), info = paste(st, collapse = "\n"))
  readRDS(out)
}

test_that("[PLUGINS] gate10_determinismo: duas cadeias na mesma sessao => sha256 do artefato, delivery e values identicos; chamador intacto", {
  set.seed(42L)
  before <- lgd_state()
  a <- lgbm_fx_chain(LGD_DECK)
  b <- lgbm_fx_chain(LGD_DECK)
  after <- lgd_state()
  expect_identical(a$sha, b$sha)
  expect_identical(a$booster_sha256, b$booster_sha256)
  expect_identical(a$delivery, b$delivery)
  expect_identical(a$values, b$values)
  expect_identical(nrow(a$delivery), 77L)        # grade da fundacao: available_until O 09:30 ate O+2 00:00
  expect_identical(after$ls, before$ls)
  expect_identical(after$search, before$search)
  expect_identical(after$seed, before$seed)
  ## o RNG do chamador tambem nao nasce quando nao existia
  rm(".Random.seed", envir = globalenv())
  lgbm_fx_chain(LGD_DECK)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  set.seed(1L)   # devolve um estado valido ao runner
})

test_that("[PLUGINS] gate10_determinismo: duas sessoes R limpas (Rscript --vanilla) => sha256, delivery e values identicos entre si e a sessao do teste", {
  a <- lgd_clean_session(tempfile(fileext = ".rds"))
  b <- lgd_clean_session(tempfile(fileext = ".rds"))
  expect_identical(a$sha, b$sha)
  expect_identical(a$delivery, b$delivery)
  expect_identical(a$values, b$values)
  m <- lgbm_fx_chain(LGD_DECK)
  expect_identical(a$sha, m$sha)
  expect_identical(a$delivery, m$delivery)
})

test_that("[PLUGINS] gate10_determinismo: CONTROL seed 124 numa copia do run.yaml muda o sha256 do artefato", {
  root <- lgbm_fx_runyaml(function(y) { y$seed <- 124L; y })
  expect_identical(yaml::read_yaml(file.path(root, "lgbm", "run.yaml"))$seed, 124L)
  base <- lgbm_fx_chain(LGD_DECK)
  ctl <- lgbm_fx_chain(LGD_DECK, runyaml_root = root)
  expect_false(identical(ctl$sha, base$sha))
  expect_false(identical(ctl$booster_sha256, base$booster_sha256))
  ## (a seed entra no artefato e nos parametros do booster; a entrega pode coincidir quando nao ha bagging/feature_fraction)
})
