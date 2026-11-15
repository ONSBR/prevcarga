## test-contract.R -- RD.4 (AC1): contrato do plugin dessem-regdin. O run.yaml real registra Tier A; fixtures de run.yaml
## invalidas (origin_agnostic, granularity_min 30, seed ausente, exogena MXMN) abortam
## [prevcarga/contract] (exit 2) nomeando o campo ANTES de qualquer ajuste (espiao em .rd_fit_pass = 0); exogena fora de
## MED/MAX tambem no despachante. CONTROL: MED e MAX carregam e dao y_pred diferente (> 1e-5). Loader real, motor real,
## painel sintetico (nunca dado real do store); paineis crus ao despachante.

test_that("[PLUGINS] contrato: o run.yaml real registra Tier A (modes, delivery_days 9, history_infer_equals_train FALSE desde o E4-T3, motor proprio)", {
  p <- rd_fx_plugin(); d <- p$descriptor
  expect_setequal(p$spec$modes, c("train", "infer", "forecast"))
  expect_identical(d$modes_implemented, c("train", "infer", "forecast"))
  expect_identical(d$delivery_days, 9L); expect_identical(p$spec$horizon_days, 9L)
  expect_false(d$history_infer_equals_train)                                  # E4-T3: a inferencia roda sobre o estado do artefato
  expect_identical(p$spec$history_days$infer, 15L); expect_identical(p$spec$history_days$train, 1095L)
  expect_false("infer_history_equals_train" %in% names(p$spec$constraints))
  expect_identical(p$spec$split, "origin_scoped"); expect_identical(p$spec$granularity_min, 60L)
  expect_false(is.null(p$spec$seed))
  expect_identical(d$engine, "_dessem-engine")
  lin <- rd_lib$prevcarga_plugin_load("dessem-linear", rd_models())
  rd <- rd_lib$prevcarga_plugin_load("dessem-regdin", rd_models())
  expect_false(identical(parent.env(rd$env), parent.env(lin$env)))          # cada plugin com o SEU env de motor
  reg <- names(rd_lib$prevcarga_registry())
  expect_true(all(paste0("dessem-regdin/", names(d$stages)) %in% reg))
})

test_that("[PLUGINS] contrato: 4 fixtures de run.yaml abortam contract (exit 2) nomeando o campo, com 0 ajustes", {
  cases <- list(
    split = list(edit = function(x) sub("split: origin_scoped", "split: origin_agnostic", x), id = "plugin.bad_run_yaml"),
    granularity_min = list(edit = function(x) sub("granularity_min: 60", "granularity_min: 30", x), id = "plugin.bad_run_yaml"),
    seed = list(edit = function(x) x[!grepl("^seed:", x)], id = "plugin.bad_run_yaml"),
    exogena = list(edit = function(x) sub("exogena: MED", "exogena: MXMN", x), id = "plugin.bad_run_yaml"))
  field_in_msg <- c(split = "modes", granularity_min = "granularity_min", seed = "seed", exogena = "params$exogena")
  spy_fit <- rd_spy(".rd_fit_pass"); spy_train <- rd_spy("rd_training")
  for (field in names(cases)) {
    cond <- tryCatch(rd_load("dessem-regdin", rd_fx_root(cases[[field]]$edit)), prevcarga_error = function(e) e)
    expect_s3_class(cond, "prevcarga_error")
    expect_identical(cond$id, cases[[field]]$id, info = field)
    expect_identical(cond$exit_code, 2L, info = field); expect_identical(cond$class_name, "contract", info = field)
    expect_match(conditionMessage(cond), field_in_msg[[field]], fixed = TRUE, info = field)
    expect_true(rd_ascii(conditionMessage(cond)), info = field)
    expect_identical(spy_fit$count(), 0L); expect_identical(spy_train$count(), 0L)
  }
  ## CONTROL: o run.yaml sem mutacao carrega
  expect_s3_class(rd_load("dessem-regdin", rd_fx_root()), "prevcarga_plugin")
})

test_that("[PLUGINS] contrato: history_days.infer != train carrega (flag FALSE, E4-T3); CONTROL: com a flag TRUE o loader recusa exit 2", {
  root <- rd_fx_root(function(x) sub("infer: 15", "infer: 45", x))
  expect_s3_class(rd_load("dessem-regdin", root), "prevcarga_plugin")
  ## CONTROL: a mesma run.yaml com o descriptor no contrato antigo (flag TRUE) => plugin.history_window, contract, exit 2
  root2 <- rd_fx_root(function(x) sub("infer: 15", "infer: 45", x))
  rg <- file.path(root2, "dessem-regdin", "R", "register.R")
  writeLines(sub("history_infer_equals_train = FALSE", "history_infer_equals_train = TRUE", readLines(rg), fixed = TRUE), rg)
  cond <- tryCatch(rd_load("dessem-regdin", root2), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.history_window"); expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
  expect_match(conditionMessage(cond), "history_days.infer", fixed = TRUE)
})

test_that("[PLUGINS] params_schema: exogena fora de {MED, MAX} (MXMN incluido) aborta contract no despachante antes de qualquer ajuste", {
  d <- rd_fx_plugin()$descriptor
  expect_identical(d$params_schema$exogena$allowed, c("MED", "MAX"))
  expect_null(d$params_schema$exogena$default)
  expect_identical(d$variant_param, "exogena")
  spy_fit <- rd_spy(".rd_fit_pass"); spy_train <- rd_spy("rd_training")
  for (bad in list("MXMN", "med", NA_character_, c("MED", "MAX"), NULL)) {
    spec <- rd_fx_spec("MED"); spec$params$exogena <- bad
    cond <- tryCatch(rd_train(spec = spec), prevcarga_error = function(e) e)
    expect_s3_class(cond, "prevcarga_error")
    expect_identical(cond$id, "plugin.regdin_bad_exogena")
    expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
    expect_identical(cond$details$allowed, c("MED", "MAX"))
    expect_match(conditionMessage(cond), "exogena", fixed = TRUE)
    expect_true(rd_ascii(conditionMessage(cond)))
    expect_identical(spy_fit$count(), 0L); expect_identical(spy_train$count(), 0L)
  }
  ## o id consta no catalogo de erros do modulo
  tsv <- utils::read.delim(file.path(rd_root(), "scripts", "lib", "errors", "dessem_regdin.tsv"), stringsAsFactors = FALSE)
  expect_true(all(c("plugin.regdin_bad_exogena", "plugin.regdin_window_mismatch", "plugin.regdin_arima_drift",
                    "plugin.regdin_train_panel_has_origin") %in% tsv$id))
})

test_that("[PLUGINS] exogena_variante: CONTROL MED e MAX carregam e dao y_pred diferente (> 1e-5); artefatos carimbados por variante", {
  st <- rd3_state()
  keys <- rd_fx_plugin()$env$plugin_constants()$model_key
  expect_identical(names(keys), c("MED", "MAX"))
  for (ex in names(keys)) {
    v <- rd_fx_plugin()$env$plugin_variant_meta(list(exogena = ex))
    expect_identical(v$member, paste0("RGD_", ex)); expect_identical(v$model_key, unname(keys[[ex]]))
  }
  expect_identical(st$med$stamp$variant, "MED"); expect_identical(st$max$stamp$variant, "MAX")
  expect_false(identical(st$med$payload$config_mdl, st$max$payload$config_mdl))
  med <- rd_infer(st$med); max_ <- rd_infer(st$max)
  expect_identical(med$variant, "MED"); expect_identical(max_$variant, "MAX")
  d <- rd_fx_ydelta(med, max_)
  cat(sprintf("\n[RD.4 medida] MED x MAX: max|Delta y_pred| = %.3f MW\n", d))
  expect_gt(d, 1e-5)
})
