## test-history-guard.R -- guarda de history_days em tempo de execucao (decisao do Gabriel, 2026-11-16; ADR-045 e ADR-046, accepted):
##  (1) .prevcarga_apply_constraints recusa history_days.infer > history_days.train (contract.history_infer_gt_train, exit 2);
##  (2) os plugins DESSEM recusam history_days.train != 1095 (contract.dessem_history_train_fixed, exit 2), salvo pelo caminho EXPLICITO de
##      teste (`options(prevcarga.test_short_history = TRUE)`), que nem o run.yaml nem a CLI definem.
## Escopado: unidade/propriedade, sem deck, sem treino; CONTROLs de cada recusa.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "holiday_islands", "deck_reader", "validation", "dates",
                       "registry"))
O <- lubridate::ymd("2024-06-10")
models_root <- function() env$prevcarga_models_root(pc_root())
spec_of <- function(m, ...) env$prevcarga_model_spec(m, pc_root(), list(...))
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

## os plugins DESSEM chamam prevcarga_condition no globalenv (a fundacao carregada no processo real); aqui so durante o teste
local_condition_api <- function(.env = parent.frame()) {
  assign("prevcarga_condition", env$prevcarga_condition, envir = globalenv())
  withr::defer(rm("prevcarga_condition", envir = globalenv()), envir = .env)
}
dessem <- c("dessem-linear", "dessem-regdin")
hw <- function(m) env$prevcarga_plugin_load(m, models_root())$descriptor$history_window

test_that("infer > train recusa (contract.history_infer_gt_train, exit 2); infer == train e infer < train seguem aceitos", {
  for (m in c(dessem, "lgbm")) {
    tr <- spec_of(m)$history_days$train
    c1 <- fail(spec_of(m, history_days.infer = tr + 1L))
    expect_identical(c1$id, "contract.history_infer_gt_train", info = m)
    expect_identical(c1$class_name, "contract"); expect_identical(c1$exit_code, 2L)
    expect_match(c1$where, paste0(m, ": history_days.infer"), fixed = TRUE)
    expect_false(grepl("[^\\x20-\\x7E]", conditionMessage(c1), perl = TRUE))
    expect_identical(spec_of(m, history_days.infer = tr)$history_days$infer, tr)            # infer == train
    expect_identical(spec_of(m, history_days.infer = tr - 1L)$history_days$infer, tr - 1L)  # infer < train
    ## encurtar so o train abaixo do infer entregue tambem recusa
    expect_identical(fail(spec_of(m, history_days.train = 1L))$id, "contract.history_infer_gt_train", info = m)
  }
  ## o catalogo declara os dois ids com classe contract
  cat <- env$prevcarga_error_catalog()
  expect_identical(cat$class[match(c("contract.history_infer_gt_train", "contract.dessem_history_train_fixed"), cat$id)], c("contract", "contract"))
})

test_that("DESSEM com train 1095 passa; train != 1095 por spec de usuario recusa (contract.dessem_history_train_fixed, exit 2)", {
  local_condition_api()
  withr::local_options(prevcarga.test_short_history = NULL)
  for (m in dessem) {
    f <- hw(m)
    ok <- spec_of(m)
    expect_identical(ok$history_days$train, 1095L, info = m)
    w <- f(O, ok)
    expect_true(inherits(w$common_from, "Date"), info = m)
    for (bad in c(1094L, 730L, 1096L, 1825L)) {
      c1 <- fail(f(O, spec_of(m, history_days.train = bad, history_days.infer = 15L)))
      expect_identical(c1$id, "contract.dessem_history_train_fixed", info = paste(m, bad))
      expect_identical(c1$class_name, "contract"); expect_identical(c1$exit_code, 2L)
      expect_false(grepl("[^\\x20-\\x7E]", conditionMessage(c1), perl = TRUE))
    }
    ## run.yaml adulterado (spec com train != 1095 vindo do arquivo) recusa do mesmo modo
    tmp <- withr::local_tempdir()
    dir.create(file.path(tmp, "models", m), recursive = TRUE); dir.create(file.path(tmp, "config"))
    file.copy(file.path(pc_root(), "config", "topology.yaml"), file.path(tmp, "config"))
    for (mm in unique(env$prevcarga_members(root = pc_root())$mdir)) {
      dir.create(file.path(tmp, "models", mm), showWarnings = FALSE)
      file.copy(file.path(pc_root(), "models", mm, "run.yaml"), file.path(tmp, "models", mm, "run.yaml"))
    }
    y <- yaml::read_yaml(file.path(tmp, "models", m, "run.yaml")); y$history_days$train <- 800L
    yaml::write_yaml(y, file.path(tmp, "models", m, "run.yaml"))
    edited <- env$prevcarga_model_spec(m, tmp)
    expect_identical(edited$history_days$train, 800L)
    expect_identical(fail(f(O, edited))$id, "contract.dessem_history_train_fixed", info = m)
  }
})

test_that("[CONTROL] o caminho explicito de teste continua encurtando a janela comum; a opcao nao vem do run.yaml nem da CLI", {
  local_condition_api()
  for (m in dessem) {
    f <- hw(m)
    withr::local_options(prevcarga.test_short_history = TRUE)
    short <- spec_of(m, history_days.train = 40L, history_days.infer = 15L, params.janela_dessem = "ano_civil")
    w <- f(O, short)
    expect_identical(w$common_from, O - 40L, info = m)                               # encurtou
    expect_true(w$common_from > f(O, spec_of(m))$common_from, info = m)               # e a janela de producao e mais longa
    withr::local_options(prevcarga.test_short_history = NULL)
    expect_identical(fail(f(O, short))$id, "contract.dessem_history_train_fixed", info = m)   # sem a opcao, recusa de novo
  }
  ## nenhum caminho de usuario define a opcao: nem run.yaml (chave desconhecida) nem a CLI/config do produto
  expect_identical(fail(spec_of("dessem-linear", test_short_history = TRUE))$id, "config.override_unknown")
  src <- unlist(lapply(c(list.files(file.path(pc_root(), "scripts", "lib"), pattern = "[.]R$", full.names = TRUE, recursive = TRUE),
                         file.path(pc_root(), "scripts", "prevcarga")), function(p) if (grepl("/tests/", p)) character() else readLines(p, warn = FALSE)))
  expect_false(any(grepl("test_short_history", src, fixed = TRUE)))
})
