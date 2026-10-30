## test-params.R -- LGBM.4 (G-ART): o infer le os params SO do artefato. FORBIDDEN = processo sujo (LGBM_*) e spec$params alterado =>
## saida identica; CONTROL = artefato treinado com disable_loess=false => saida difere. Artefatos inconsistentes abortam [contract].
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
P <- lgbm_source_plugin()
b <- lgbm_fx_base()

## Sys.setenv so dentro do teste, restaurado por on.exit (sem withr)
with_dirty_env <- function(code) {
  nm <- c("LGBM_DISABLE_LOESS", "LGBM_CALENDAR_YEAREND", "LGBM_TRAIN_CV", "LGBM_TIPODIA_LEGACY", "LGBM_CUTOFF_SCHEME")
  old <- Sys.getenv(nm, unset = NA_character_)
  on.exit({ for (n in nm) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(old[[n]]), n)) },
          add = TRUE)
  Sys.setenv(LGBM_DISABLE_LOESS = "0", LGBM_CALENDAR_YEAREND = "0", LGBM_TRAIN_CV = "0", LGBM_TIPODIA_LEGACY = "1",
             LGBM_CUTOFF_SCHEME = "rolling")
  force(code)
}

test_that("[PLUGINS] gate_inf_adequacao: [AC2] artefato disable_loess/calendar_yearend/train_cv = true; processo com LGBM_* sujo e spec alterado => y_pred e values identicos", {
  expect_identical(b$art$payload$params[c("disable_loess", "calendar_yearend", "train_cv")],
                   list(disable_loess = TRUE, calendar_yearend = TRUE, train_cv = TRUE))
  clean <- lgbm_fx_infer(b)
  expect_identical(clean$meta$params_source, "artifact")
  dirty <- with_dirty_env(lgbm_fx_infer(b))
  expect_identical(dirty$delivery$y_pred, clean$delivery$y_pred)
  expect_identical(dirty$values, clean$values)
  expect_identical(Sys.getenv("LGBM_TRAIN_CV", unset = "unset"), "unset")
  b2 <- b; b2$plugin$spec$params$disable_loess <- FALSE; b2$plugin$spec$params$calendar_yearend <- FALSE
  spec2 <- lgbm_fx_infer(b2)
  expect_identical(spec2$delivery$y_pred, clean$delivery$y_pred)
  expect_identical(spec2$values, clean$values)
  expect_identical(spec2$meta$params_source, "artifact")
  ## sem refit: o infer nao muda o artefato (sha do objeto identico)
  expect_identical(lgbm_fx_sha(b$art), lgbm_fx_sha(lgbm_fx_base()$art))
})

test_that("[PLUGINS] gate_inf_adequacao: [AC2] CONTROL: artefato treinado com disable_loess=false da max|D y_pred| > 1e-5 sobre o mesmo painel", {
  bl <- lgbm_fx_base(loess_off = FALSE)
  expect_false(bl$art$payload$params$disable_loess)
  out <- lgbm_fx_infer(bl)
  base <- lgbm_fx_infer(b)
  expect_identical(out$delivery$interval_end, base$delivery$interval_end)
  expect_gt(max(abs(out$delivery$y_pred - base$delivery$y_pred)), 1e-5)
  expect_true(all(is.finite(out$delivery$y_pred)))
  ## o infer do artefato com loess usa o flag DO ARTEFATO mesmo com spec dizendo true
  bl2 <- bl; bl2$plugin$spec$params$disable_loess <- TRUE
  expect_identical(lgbm_fx_infer(bl2)$delivery$y_pred, out$delivery$y_pred)
})

test_that("[PLUGINS] gate_inf_adequacao: [AC2] artefatos inconsistentes abortam [prevcarga/contract] lgbm: artefato inconsistente", {
  pay <- b$art$payload
  chk <- function(p, what) {
    e <- fail(P$lgbm_check_artifact_params(p))
    expect_s3_class(e, "prevcarga_error"); expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
    expect_match(conditionMessage(e), "artefato inconsistente", fixed = TRUE)
    expect_match(conditionMessage(e), what, fixed = TRUE)
    expect_true(grepl("^[ -~]*$", conditionMessage(e)))
  }
  expect_silent(P$lgbm_check_artifact_params(pay))
  legacy <- pay; legacy$params$tipodia_legacy <- TRUE; legacy$sazonalidade_fallbacks <- NULL
  chk(legacy, "tipodia_legacy sem sazonalidade_fallbacks")
  nocal <- pay; nocal$colunas_entrada_BLF <- setdiff(pay$colunas_entrada_BLF, "PreFeriado")
  chk(nocal, "PreFeriado")
  over <- pay; over$params$calendar_yearend <- FALSE
  chk(over, "sem o flag em params")
  ## o estagio de features e o infer recusam o mesmo artefato (nada roda com params incoerentes)
  frame <- P$plugin_ingest(b$panel, b$cuts)
  expect_identical(fail(P$plugin_inference_features(frame, nocal))$class_name, "contract")
  inf <- P$plugin_inference_features(frame, pay)
  expect_identical(fail(P$plugin_infer(nocal, inf, b$cuts))$class_name, "contract")
  ## params fora do schema continuam sendo lgbm.params (nomeando a chave)
  bad <- pay; bad$params$disable_loess <- "1"
  e <- fail(P$lgbm_check_artifact_params(bad)); expect_identical(e$id, "lgbm.params")
})

test_that("[PLUGINS] gate_inf_adequacao: [AC2] G-ART estatico: nenhum Sys.getenv/Sys.setenv em models/lgbm/R", {
  expect_identical(pc_grep_code(file.path(lgbm_root(), "models", "lgbm", "R"), "Sys\\.(getenv|setenv|unsetenv)"), character())
  ## CONTROL do detector: ele acusa uma linha com Sys.setenv em codigo (e ignora comentario)
  d <- tempfile("lgbm-grep-"); dir.create(d)
  writeLines(c("# Sys.setenv(A = 1)", "x <- 1", "Sys.setenv(LGBM_DISABLE_LOESS = '1')"), file.path(d, "a.R"))
  expect_identical(pc_grep_code(d, "Sys\\.(getenv|setenv|unsetenv)"), "a.R:3")
})
