## test-bundle-windows-engines.R -- engines do bundle Windows (M8.2; AC 5 e 6). Gate [BUNDLE] windows_engines.
## Trilho entre-plataformas: o booster LGBM congelado em TEXTO no Linux (model.txt) e carregado pelo Rscript.exe do bundle no Windows,
## |delta| <= 0,001*|ref| (ADR-0034/ADR-0038). Aqui valem no Linux: a geracao da fixture (dist/win-engines-fixture/), a recarga do texto
## (trilho do proprio Linux, referencia), e o CONTROL do RDS binario. Os bracos que EXECUTAM o bundle (lgb.load pelo Rscript.exe, forecast --report
## com PATH minimo, CONTROL bundle.package_missing) so rodam num host Windows com o zip em dist/: skip `nao-windows`.

root <- bb_root()
lib <- file.path(root, "scripts", "lib")
on_windows <- identical(.Platform$OS.type, "windows")
fix_dir <- file.path(root, "dist", "win-engines-fixture")

## Booster pequeno e determinista (nthread = 1, seed fixa) -> model.txt (save_model_to_string, como models/lgbm/R/freeze.R), panel.csv e ref.csv.
bwe_make_fixture <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  set.seed(20261030)
  x <- matrix(stats::rnorm(600 * 4), ncol = 4, dimnames = list(NULL, paste0("f", 1:4)))
  y <- 3 * x[, 1] - 2 * x[, 2]^2 + 0.5 * x[, 3] + stats::rnorm(600, sd = 0.1)
  b <- lightgbm::lgb.train(params = list(objective = "regression", num_threads = 1L, seed = 7L, verbose = -1L, min_data_in_leaf = 10L,
                                         deterministic = TRUE, force_row_wise = TRUE),
                           data = lightgbm::lgb.Dataset(x, label = y), nrounds = 25L, verbose = -1L)
  txt <- gsub("\r\n", "\n", paste(b$save_model_to_string(num_iteration = NULL), collapse = "\n"), fixed = TRUE)
  con <- file(file.path(dir, "model.txt"), open = "wb"); writeBin(charToRaw(txt), con); close(con)
  panel <- x[1:40, , drop = FALSE]
  utils::write.table(panel, file.path(dir, "panel.csv"), sep = ",", row.names = FALSE, quote = FALSE)
  ref <- predict(b, panel)
  utils::write.table(data.frame(pred = format(ref, digits = 17)), file.path(dir, "ref.csv"), row.names = FALSE, quote = FALSE)
  invisible(dir)
}

bwe_predict <- function(dir) {
  b <- lightgbm::lgb.load(model_str = paste(readLines(file.path(dir, "model.txt"), warn = FALSE), collapse = "\n"))
  panel <- as.matrix(utils::read.csv(file.path(dir, "panel.csv")))
  as.numeric(predict(b, panel))
}
bwe_ref <- function(dir) as.numeric(utils::read.csv(file.path(dir, "ref.csv"))$pred)

test_that("[FORBIDDEN] [BUNDLE] windows_engines: booster em texto (ASCII, LF, sem RDS) recarrega e reproduz a referencia no trilho entre-plataformas", {
  skip_if_not_installed("lightgbm")
  d <- withr::local_tempdir()
  bwe_make_fixture(d)
  txt <- readBin(file.path(d, "model.txt"), "raw", 1e8)
  expect_true(all(as.integer(txt) < 128L)); expect_false(as.raw(13L) %in% txt)
  expect_match(rawToChar(txt), "^tree\nversion=")
  ref <- bwe_ref(d); got <- bwe_predict(d)
  expect_length(got, 40L)
  expect_true(all(abs(got - ref) <= 0.001 * abs(ref)))
  if (!on_windows) bwe_make_fixture(fix_dir)                 # entrega ao host Windows (dist/ e gitignored): copie win-engines-fixture/ junto do zip
  ## regeneracao determinista: mesma fixture, mesmo texto (o que o Linux entrega ao Windows e estavel)
  d2 <- withr::local_tempdir(); bwe_make_fixture(d2)
  expect_identical(unname(tools::md5sum(file.path(d, "model.txt"))), unname(tools::md5sum(file.path(d2, "model.txt"))))
})

test_that("[CONTROL] [BUNDLE] windows_engines: booster serializado em RDS binario e recusado como artefato, classe integrity (exit 9)", {
  skip_if_not_installed("lightgbm")
  options(prevcarga.lib = lib)
  env <- new.env(parent = globalenv())
  for (m in c("runtime", "errors", "provenance", "topology", "deck_reader", "validation", "registry", "features"))
    sys.source(file.path(lib, paste0(m, ".R")), envir = env)
  d <- withr::local_tempdir(); bwe_make_fixture(d)
  b <- lightgbm::lgb.load(model_str = paste(readLines(file.path(d, "model.txt"), warn = FALSE), collapse = "\n"))
  rds <- file.path(d, "booster.rds"); saveRDS(b, rds)               # nao portavel: o lgb.Booster guarda um ponteiro nativo
  cond <- tryCatch(env$prevcarga_plugin_thaw(rds), prevcarga_error = function(c) c)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.artifact_corrupt")
  expect_identical(cond$class_name, "integrity")
  expect_identical(cond$exit_code, 9L)
})

## --- bracos que EXECUTAM o bundle (host Windows) -----------------------------------------------------------------------------
bwe_bundle <- function(env = parent.frame()) {
  zf <- sort(c(Sys.glob(file.path(root, "dist", "prevcarga-*-windows-x86_64.zip")),
               Sys.glob(file.path(root, "dist", "repro-1", "prevcarga-*-windows-x86_64.zip"))), method = "radix")
  skip_if(!length(zf), "sem dist/prevcarga-<v>-windows-x86_64.zip")
  d <- withr::local_tempdir(.local_envir = env); utils::unzip(zf[length(zf)], exdir = d)
  file.path(d, "prevcarga")
}

test_that("[FORBIDDEN] [BUNDLE] windows_engines: Rscript.exe do bundle carrega o model.txt do Linux (lgb.load) e prediz dentro de 0,001*|ref|", {
  skip_if_not(on_windows, "nao-windows")
  skip_if_not(file.exists(file.path(fix_dir, "model.txt")), "sem dist/win-engines-fixture/ (rode este teste no Linux: ele grava dist/win-engines-fixture/; copie-o para o host Windows)")
  rt <- bwe_bundle()
  drv <- withr::local_tempfile(fileext = ".R")
  writeLines(c("a <- commandArgs(TRUE)",
               "b <- lightgbm::lgb.load(model_str = paste(readLines(file.path(a[1], 'model.txt'), warn = FALSE), collapse = '\\n'))",
               "p <- as.matrix(read.csv(file.path(a[1], 'panel.csv')))",
               "cat(format(as.numeric(predict(b, p)), digits = 17), sep = '\\n')"), drv)
  out <- system2(file.path(rt, "runtime", "R", "bin", "Rscript.exe"), c("--vanilla", shQuote(drv), shQuote(fix_dir)), stdout = TRUE)
  got <- as.numeric(out); ref <- bwe_ref(fix_dir)
  expect_length(got, length(ref))
  expect_true(all(abs(got - ref) <= 0.001 * abs(ref)))
})

test_that("[FORBIDDEN] [BUNDLE] windows_engines: prevcarga forecast --report com PATH minimo gera HTML+PDF pelo motor portavel do M6.10, exit 0", {
  skip_if_not(on_windows, "nao-windows")
  skip("[DEFERRED M8.4/M8.6] precisa do host Windows, do zip em dist/ e de um deck de fixture; o launcher Windows (prevcarga.cmd) e do M8.4")
})

test_that("[CONTROL] [BUNDLE] windows_engines: motor removido do stage => exit 6 bundle.package_missing antes de qualquer escrita", {
  skip_if_not(on_windows, "nao-windows")
  skip("[DEFERRED M8.4/M8.6] depende do forecast --report no Windows; prevcarga_bundle_require ainda nao e chamado pelo verbo forecast")
})
