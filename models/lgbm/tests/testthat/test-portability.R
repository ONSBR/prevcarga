## test-portability.R -- LGBM.3: artefato congelado = dado puro. scan_pure, booster_txt ASCII+LF e round-trip byte-identico,
## thaw em sessao R nova (Rscript --vanilla) == booster em memoria, adulteracoes => integrity (exit 9) / contract (exit 2).
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
P <- lgbm_source_plugin()
K <- lgbm_consts(P)
deck <- lgbm_fx_deck(days = 120L)
base <- lgbm_fx_train(deck, P = P)
art <- base$artifact
NEWER <- paste0("9", art$engine_version$lightgbm)

## matriz normalizada de 200 linhas do treino (o que o booster ve)
X <- local({
  dn <- data.table::copy(base$features$feature_table)
  P$lgbm_normalise_dt(dn, art$min_max_BLF)
  as.matrix(utils::tail(dn[Data <= base$cuts$last_data_day], 200L)[, art$colunas_entrada_BLF, with = FALSE])
})

test_that("[PLUGINS] gate04_decoupling: [AC4] artefato puro: scan_pure limpo; sem lgb.Booster, trained_at, package_version, caminho ou externalptr", {
  expect_true(lgbm_lib$prevcarga_plugin_scan_pure(art))
  expect_false(any(c("booster", "trained_at", "package_version", "area") %in% names(art)))
  expect_false(any(vapply(art, function(x) inherits(x, "lgb.Booster") || typeof(x) == "externalptr" || is.environment(x), NA)))
  expect_identical(art$model_id, "lgbm"); expect_identical(art$split, "origin_agnostic")
  expect_identical(art$granularity_min, 30L); expect_identical(art$contract_version, 1L)
  expect_identical(art$train_origin, base$cuts$train_origin)
  expect_identical(art$seed, 123L)
  expect_identical(art$engine_version, list(lightgbm = as.character(utils::packageVersion("lightgbm"))))
  expect_true(is.data.frame(art$sazonalidade) && !inherits(art$sazonalidade, "data.table"))
  expect_identical(art$train_cutoff, "2026-09-28")
})

test_that("[PLUGINS] gate04_decoupling: [AC4] booster_txt: character(1) ASCII so com LF; round-trip lgb.load(model_str) -> save_model_to_string byte-identico", {
  txt <- art$booster_txt
  expect_true(is.character(txt) && length(txt) == 1L)
  expect_false(is.na(iconv(txt, "UTF-8", "ASCII")))
  expect_false(grepl("\r", txt, fixed = TRUE))
  expect_identical(art$booster_sha256, digest::digest(txt, algo = "sha256", serialize = FALSE))
  b <- lightgbm::lgb.load(model_str = txt)
  expect_identical(b$save_model_to_string(num_iteration = NULL), txt)
  expect_identical(P$lgbm_n_trees(txt), art$best_iter)
})

test_that("[PLUGINS] gate04_decoupling: [AC4] thaw em sessao R nova (Rscript --vanilla) prediz identico ao booster em memoria (max|D| = 0)", {
  tmp <- tempfile("lgbm-thaw-"); dir.create(tmp)
  saveRDS(art, file.path(tmp, "art.rds")); saveRDS(X, file.path(tmp, "x.rds"))
  script <- file.path(tmp, "thaw.R")
  writeLines(c("a <- commandArgs(trailingOnly = TRUE)",
               "e <- new.env()",
               "for (f in a[4:5]) sys.source(f, envir = e, keep.source = FALSE, toplevel.env = e)",
               "art <- readRDS(a[1]); x <- readRDS(a[2])",
               "th <- e$lgbm_thaw(art)",
               "saveRDS(predict(th$booster, x), a[3])"), script)
  out <- file.path(tmp, "pred.rds")
  rs <- file.path(R.home("bin"), "Rscript")
  rf <- file.path(lgbm_root(), "models", "lgbm", "R")
  st <- system2(rs, c("--vanilla", script, file.path(tmp, "art.rds"), file.path(tmp, "x.rds"), out,
                      file.path(rf, "register.R"), file.path(rf, "freeze.R")), stdout = TRUE, stderr = TRUE)
  expect_true(file.exists(out), info = paste(st, collapse = "\n"))
  mem <- predict(lightgbm::lgb.load(model_str = art$booster_txt), X)
  expect_identical(max(abs(readRDS(out) - mem)), 0)
  th <- P$plugin_thaw(art, base$cuts)
  expect_identical(max(abs(predict(th$booster, X) - mem)), 0)
  expect_identical(max(abs(predict(base$model$booster, X) - mem)), 0)
})

test_that("[PLUGINS] gate04_decoupling: [AC4] dois freezes de treinos identicos => sha256 igual; CONTROL Sys.time() injetado muda o sha e o scan acusa", {
  m2 <- P$lgbm_train(base$features, base$spec, base$cuts, K)
  a2 <- P$plugin_freeze(m2, base$spec, base$cuts, list())
  expect_identical(lgbm_fx_sha(a2), lgbm_fx_sha(art))
  expect_identical(a2$booster_sha256, art$booster_sha256)
  bad <- c(art, list(trained_at = Sys.time()))
  expect_false(identical(lgbm_fx_sha(bad), lgbm_fx_sha(art)))
  e <- fail(lgbm_lib$prevcarga_plugin_scan_pure(bad))
  expect_s3_class(e, "prevcarga_error")
})

## ---- AC5: adulteracoes ---------------------------------------------------------------------------------------------
reseal <- function(a, txt) { a$booster_txt <- txt; a$booster_sha256 <- digest::digest(txt, algo = "sha256", serialize = FALSE); a }

test_that("[PLUGINS] thaw_integridade: [AC5] artefato intacto descongela sem erro; limiar alterado ou texto truncado => [prevcarga/integrity] (exit 9)", {
  th <- P$lgbm_thaw(art)
  expect_s3_class(th$booster, "lgb.Booster")
  expect_identical(th$best_iter, art$best_iter)
  ## limiar de arvore alterado (sha gravado nao confere)
  lines <- strsplit(art$booster_txt, "\n", fixed = TRUE)[[1L]]
  i <- grep("^threshold=", lines)[[1L]]
  lines[[i]] <- sub("^threshold=([0-9])", "threshold=9\\1", lines[[i]])
  tam <- art; tam$booster_txt <- paste(lines, collapse = "\n")
  e1 <- fail(P$lgbm_thaw(tam))
  expect_identical(e1$id, "lgbm.integrity"); expect_identical(e1$class_name, "integrity"); expect_identical(e1$exit_code, 9L)
  expect_match(conditionMessage(e1), "^\\[prevcarga/integrity\\] lgbm: ")
  ## truncado
  tr <- art; tr$booster_txt <- substr(art$booster_txt, 1L, nchar(art$booster_txt) %/% 2L)
  e2 <- fail(P$lgbm_thaw(tr))
  expect_identical(e2$class_name, "integrity"); expect_identical(e2$exit_code, 9L)
  ## truncado COM sha regravado: a leitura ou a contagem de arvores acusa
  e3 <- fail(P$lgbm_thaw(reseal(art, substr(art$booster_txt, 1L, nchar(art$booster_txt) %/% 2L))))
  expect_identical(e3$class_name, "integrity"); expect_identical(e3$exit_code, 9L)
})

test_that("[PLUGINS] thaw_integridade: [AC5] best_iter != numero de arvores do texto => integrity; versao do lightgbm diferente => contract nomeando as duas", {
  bi <- art; bi$best_iter <- art$best_iter + 1L
  e <- fail(P$lgbm_thaw(bi))
  expect_identical(e$class_name, "integrity"); expect_match(conditionMessage(e), "best_iter")
  ev <- art; ev$engine_version$lightgbm <- NEWER
  e2 <- fail(P$lgbm_thaw(ev))
  expect_identical(e2$class_name, "contract"); expect_identical(e2$exit_code, 2L)
  expect_match(conditionMessage(e2), NEWER, fixed = TRUE)
  expect_match(conditionMessage(e2), as.character(utils::packageVersion("lightgbm")), fixed = TRUE)
  ## carimbos: train_origin alem do da conferencia e outro modelo => contract
  late <- base$cuts; late$train_origin <- art$train_origin - 1L
  expect_identical(fail(P$plugin_thaw(art, late))$class_name, "contract")
  om <- art; om$model_id <- "outro"
  expect_identical(fail(P$plugin_thaw(om, base$cuts))$class_name, "contract")
})

test_that("[PLUGINS] higiene_artefato: [AC5] sem saveRDS, lgb.save, trained_at, Sys.time ou variavel de ambiente no codigo do plugin", {
  r <- file.path(lgbm_root(), "models", "lgbm", "R")
  expect_identical(pc_grep_code(r, "saveRDS|lgb\\.save\\s*\\(|trained_at|Sys\\.time|Sys\\.getenv|package_version"), character())
})

test_that("[DEFERRED M8] cross_os: [AC5] cross-OS do booster (Windows x Linux <= 0,1%)", {
  skip("[DEFERRED M8] cross-OS do booster")
})
