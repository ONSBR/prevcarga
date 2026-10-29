## test-no-env.R -- RF.2: o plugin nao le variavel de ambiente nem relogio e nao suja o ambiente global. Um ambiente do
## processo com RF_ARTIFACT_COMPRESS/PREVCARGA_* nao muda o sha256 do artefato; nenhum arquivo fora do diretorio pedido.
library(testthat)

test_that("[PLUGINS] no_env: RF_ARTIFACT_COMPRESS/PREVCARGA_* no ambiente nao mudam o sha256; globalenv()/search() intactos", {
  clean <- rf_tr_sha(rf_tr_run()$art)
  vars <- c(RF_ARTIFACT_COMPRESS = "none", PREVCARGA_ARTIFACT_DIR = "/nao/existe/artefatos", PREVCARGA_DATA_DIR = "/nao/existe/data")
  old <- Sys.getenv(names(vars), unset = NA)
  do.call(Sys.setenv, as.list(vars))
  on.exit({
    for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(old[[n]]), n))
  }, add = TRUE)
  g <- globalenv()
  ls0 <- ls(g, all.names = TRUE); sr0 <- search()
  wd0 <- list.files(getwd(), all.files = TRUE, recursive = FALSE)
  dirty <- rf_tr_sha(rf_tr_run()$art)
  expect_identical(dirty, clean)
  expect_identical(ls(g, all.names = TRUE), ls0)
  expect_identical(search(), sr0)
  expect_identical(list.files(getwd(), all.files = TRUE, recursive = FALSE), wd0)
})

test_that("[PLUGINS] no_env: freeze escreve so no diretorio pedido (artefato + manifest) e nada em outro lugar", {
  a <- rf_tr_run()
  dir <- tempfile("rf-out-"); dir.create(dir)
  other <- tempfile("rf-other-"); dir.create(other)
  on.exit(unlink(c(dir, other), recursive = TRUE), add = TRUE)
  f <- file.path(dir, "rf.rds")
  rf_lib$prevcarga_plugin_freeze_bytes(a$art, f)
  expect_setequal(list.files(dir, all.files = TRUE, no.. = TRUE), c("rf.rds", "rf.rds.manifest.json"))
  expect_identical(list.files(other, all.files = TRUE, no.. = TRUE), character())
})

test_that("[PLUGINS] no_env: CONTROL seed 124 no run.yaml efetivo da sha256 diferente", {
  expect_false(identical(rf_tr_sha(rf_tr_run(spec = rf_tr_spec(seed = 123L))$art),
                         rf_tr_sha(rf_tr_run(spec = rf_tr_spec(seed = 124L))$art)))
})

test_that("[PLUGINS] no_env: o codigo do estagio nao referencia ambiente, relogio, globalenv nem setwd; spy do despacho nao toca o ambiente", {
  r <- file.path(RF_PLUGIN_DIR(), "R")
  expect_identical(rf_grep_code(r, "Sys[.](getenv|setenv|time|Date|getlocale)|setwd[(]|globalenv[(]|proc[.]time|Sys[.]sleep"),
                   character())
  expect_identical(rf_grep_code(r, "options[(]|library[(]|require[(]|attachNamespace|<<-"), character())
  ## o despachante (P.0c) isola RNG e globalenv: treinar via estagios canonicos nao deixa simbolo novo
  g <- globalenv(); ls0 <- ls(g, all.names = TRUE)
  p <- rf_plug(); base <- rf_tr_run()
  m <- p$rf_training(base$feats, list(ntree = 5L), 123L)
  expect_identical(ls(g, all.names = TRUE), ls0)
  expect_length(m, 216L)
})
