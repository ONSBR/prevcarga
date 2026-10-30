## test-dates.R -- dates.R (M2.3): origem, corte de treino por classe de split (ADR-0023), corte de insumo e
## composicao `cuts`. Cada braco FORBIDDEN (delta=0) pareado a um CONTROL (delta!=0). Assercoes por error.id/classe.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "deck_reader", "dates"))
env$prevcarga_runtime_enforce()
for (n in c("prevcarga_resolve_origin", "prevcarga_last_data_day", "prevcarga_train_cut", "prevcarga_input_cut",
            "prevcarga_first_delivery", "prevcarga_resolve_cuts", "prevcarga_model_spec"))
  assign(n, env[[n]])

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
at <- function(x) lubridate::ymd_hm(x, tz = TZ)
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

## copia do repo real (config + models) em tempdir; os 5 run.yaml sao editados no proprio teste
fx_root <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  file.copy(file.path(pc_root(), "config"), d, recursive = TRUE)
  file.copy(file.path(pc_root(), "models"), d, recursive = TRUE)
  d
}
edit_run <- function(root, model, fn) {
  f <- file.path(root, "models", model, "run.yaml")
  yaml::write_yaml(fn(yaml::read_yaml(f)), f)
}
spec_of <- function(root, m) prevcarga_model_spec(m, root = root)
mk_ctx <- function(deck, root, origin = NULL) {
  c0 <- pc_ctx_deck(deck)
  c0$root <- root; c0$origin <- origin; c0$weather_issue_cutoff <- "available_until"
  c0
}
days <- O + (-2:0)
lag_lgbm <- c(lgbm = 2L)

test_that("catalogo dates.tsv carrega pelo loader sem id duplicado", {
  cat <- env$prevcarga_errors_load(file.path(pc_root(), "scripts", "lib", "errors"))
  ids <- c("contract.data_lag_invalid", "contract.split_unknown", "contract.granularity_invalid",
           "contract.schema_missing", "honesty.data_lag_negative")
  expect_true(all(ids %in% cat$id))
  expect_false(anyDuplicated(cat$id) > 0L)
  expect_identical(cat$class[match(ids, cat$id)], c(rep("contract", 4L), "honesty"))
})

test_that("AC 12: input_cut 30/60 e first_delivery; 45 aborta", {
  a <- at("2026-10-01 09:30"); b <- at("2026-10-01 09:00")
  expect_identical(prevcarga_input_cut(a, 30L), a)
  expect_identical(prevcarga_input_cut(a, 60L), b)
  expect_identical(prevcarga_first_delivery(a), at("2026-10-01 10:00"))
  expect_identical(prevcarga_input_cut(b, 30L), b)
  expect_identical(prevcarga_input_cut(b, 60L), b)
  expect_identical(prevcarga_first_delivery(b), at("2026-10-01 09:30"))
  e <- fail(prevcarga_input_cut(a, 45L))
  expect_identical(e$id, "contract.granularity_invalid")
  expect_identical(e$class_name, "contract")
})

test_that("AC 13/14: train_origin e last_data_day por modelo; FORBIDDEN RF identico + CONTROL H do LGBM", {
  root <- fx_root()
  rf <- spec_of(root, "random-forest"); lg <- spec_of(root, "lgbm")
  expect_identical(c(rf$split, rf$horizon_days, lg$split, lg$horizon_days),
                   c("origin_agnostic", "9", "origin_agnostic", "1"))
  r1 <- prevcarga_train_cut(rf, O, 0L)
  l1 <- prevcarga_train_cut(lg, O, lag_lgbm[["lgbm"]])
  expect_equal(r1$train_origin, lubridate::as_date("2026-09-21"))
  expect_equal(l1$train_origin, lubridate::as_date("2026-09-29"))
  expect_equal(r1$last_data_day, lubridate::as_date("2026-09-30"))
  expect_equal(l1$last_data_day, lubridate::as_date("2026-09-28"))
  expect_equal(as.integer(r1$train_origin - l1$train_origin), -8L)
  expect_equal(prevcarga_train_cut(lg, O)$last_data_day, lubridate::as_date("2026-09-30"))  # sem atraso: teto O-1
  expect_identical(names(l1), c("train_origin", "last_data_day"))
  ## FORBIDDEN: editar o horizonte do LGBM nao move o RF
  edit_run(root, "lgbm", function(x) { x$horizon_days <- 3L; x })
  r2 <- prevcarga_train_cut(spec_of(root, "random-forest"), O, 0L)
  expect_identical(r2, r1)
  ## CONTROL: o LGBM le o horizonte do run.yaml; last_data_day nao depende de H
  l2 <- prevcarga_train_cut(spec_of(root, "lgbm"), O, 2L)
  expect_equal(l2$train_origin, lubridate::as_date("2026-09-27"))
  expect_equal(l2$last_data_day, lubridate::as_date("2026-09-28"))
})

test_that("AC 15: origin_scoped = O sem shift por H; protocolo trocado delta=0; split desconhecido aborta", {
  root <- fx_root()
  ms <- c("holt-winters", "dessem-linear", "dessem-regdin")
  base <- lapply(ms, function(m) prevcarga_train_cut(spec_of(root, m), O, 0L))
  for (x in base) {
    expect_equal(x$train_origin, O)
    expect_equal(x$last_data_day, lubridate::as_date("2026-09-30"))
  }
  for (m in ms) edit_run(root, m, function(x) { x$backtest$protocol_default <- "fixed"; x })
  again <- lapply(ms, function(m) prevcarga_train_cut(spec_of(root, m), O, 0L))
  expect_identical(again, base)                       # FORBIDDEN: protocolo nao entra na conta
  ## CONTROL: mudar a classe de split move o corte
  s <- spec_of(root, "holt-winters"); s$split <- "origin_agnostic"
  expect_false(identical(prevcarga_train_cut(s, O, 0L)$train_origin, O))
  s$split <- "inventado"
  expect_identical(fail(prevcarga_train_cut(s, O, 0L))$id, "contract.split_unknown")
  ## braco retrain_harness O-1: pendente de decisao (ADR-0023)
  skip("[TBD: ADR-0023] retrain_harness com corte O-1 ainda nao decidido; nunca como expect")
})

test_that("AC 16: atraso negativo => honesty (15); fracionario/NA/vetor => contract", {
  root <- fx_root(); lg <- spec_of(root, "lgbm")
  e1 <- fail(prevcarga_train_cut(lg, O, -1L))
  expect_identical(e1$id, "honesty.data_lag_negative")
  expect_identical(e1$class_name, "honesty"); expect_identical(e1$exit_code, 15L)
  for (bad in list(1.5, NA_integer_, c(1L, 2L), "2")) {
    e <- fail(prevcarga_train_cut(lg, O, bad))
    expect_identical(e$id, "contract.data_lag_invalid")
    expect_identical(e$class_name, "contract")
  }
})

test_that("AC 17: origem default pelo deck, flag vence; CONTROL: remover o ultimo dia; sem relogio", {
  root <- fx_root()
  d <- pc_deck_sentinel("end", days = days, until = at("2026-10-01 09:30"))
  r <- prevcarga_resolve_origin(mk_ctx(d, root))
  expect_identical(r, list(origin = O, source = "deck"))
  expect_identical(prevcarga_resolve_origin(mk_ctx(d, root)), r)           # determinismo
  f <- prevcarga_resolve_origin(mk_ctx(d, root, origin = lubridate::as_date("2026-09-28")))
  expect_identical(f, list(origin = lubridate::as_date("2026-09-28"), source = "flag"))
  d2 <- pc_deck_sentinel("end", days = O + (-2:-1))                         # truncado em 2026-09-30 23:30
  expect_equal(prevcarga_resolve_origin(mk_ctx(d2, root))$origin, lubridate::as_date("2026-09-30"))
  expect_identical(prevcarga_resolve_origin(mk_ctx(d2, root))$source, "deck")
  src <- readLines(file.path(pc_root(), "scripts", "lib", "dates.R"), warn = FALSE)
  code <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("Sys\\.time|Sys\\.Date|now\\(|today\\(|Sys\\.getenv|corte|last_label_day|train_end", code)))
  expect_false(any(grepl("\\b(365|730|1825)\\b|horizon_days *(=|<-) *[0-9]", code)))
})

test_that("AC 18: resolve_cuts devolve tabela de 5 linhas por modelo + campos run-level do leitor", {
  root <- fx_root()
  d <- pc_deck_sentinel("end", days = days, until = at("2026-10-01 09:00"))
  ctx <- mk_ctx(d, root, origin = O)
  res <- prevcarga_resolve_cuts(ctx, O, data_lag_days = lag_lgbm)
  expect_identical(res$forecast_origin, O); expect_identical(res$origin_source, "flag")
  expect_identical(res$available_until, at("2026-10-01 09:00"))
  ld <- env$prevcarga_read_load(ctx, origin = O, from = O, to = O)
  expect_identical(res$available_until_by_area, attr(ld, "available_until_by_area"))
  expect_identical(res$load_label, "end"); expect_identical(res$tz, TZ)
  expect_identical(res$weather_issue_cutoff, "available_until")
  expect_identical(res$weather_issue_limit, env$prevcarga_weather_issue_limit("available_until", O, res$available_until, TZ))
  cu <- res$cuts
  expect_s3_class(cu, "data.table")
  expect_identical(nrow(cu), 5L)
  expect_identical(names(cu), c("model", "split", "granularity_min", "horizon_days", "data_lag_days",
                                "data_lag_source", "train_origin", "last_data_day", "input_cut", "first_delivery"))
  expect_false(any(c("last_label_day", "train_end") %in% names(cu)))
  g <- function(m, col) cu[model == m][[col]]
  expect_false(identical(g("random-forest", "train_origin"), g("lgbm", "train_origin")))
  expect_equal(g("lgbm", "last_data_day"), lubridate::as_date("2026-09-28"))
  expect_identical(g("lgbm", "data_lag_source"), "descriptor")
  expect_identical(g("random-forest", "data_lag_source"), "default")
  expect_equal(g("random-forest", "last_data_day"), lubridate::as_date("2026-09-30"))
  expect_identical(g("holt-winters", "input_cut"), at("2026-10-01 09:00"))
  expect_identical(g("random-forest", "first_delivery"), at("2026-10-01 09:30"))
  one <- prevcarga_resolve_cuts(ctx, O, model = "lgbm")
  expect_identical(nrow(one$cuts), 1L)
})

test_that("AC 15 (module): origin_scoped iguais em decks 09:00/12:00; input_cut difere; protocolo delta=0", {
  root <- fx_root()
  ms <- c("holt-winters", "dessem-linear", "dessem-regdin")
  d9 <- pc_deck_sentinel("end", days = days, until = at("2026-10-01 09:00"))
  d12 <- pc_deck_sentinel("end", days = days, until = at("2026-10-01 12:30"))
  c9 <- prevcarga_resolve_cuts(mk_ctx(d9, root, O), O)$cuts
  c12 <- prevcarga_resolve_cuts(mk_ctx(d12, root, O), O)$cuts
  for (m in ms) {
    expect_equal(c9[model == m]$train_origin, O)
    expect_equal(c9[model == m]$last_data_day, lubridate::as_date("2026-09-30"))
  }
  cols <- c("model", "split", "granularity_min", "horizon_days", "data_lag_days", "data_lag_source", "train_origin", "last_data_day")
  expect_identical(c9[model %in% ms, cols, with = FALSE], c12[model %in% ms, cols, with = FALSE])
  expect_false(identical(c9$input_cut, c12$input_cut))                      # CONTROL: o insumo acompanha o dado
  for (m in ms) edit_run(root, m, function(x) { x$backtest$protocol_default <- "daily"; x })
  expect_identical(as.data.frame(prevcarga_resolve_cuts(mk_ctx(d9, root, O), O)$cuts), as.data.frame(c9))  # FORBIDDEN: protocolo
})

test_that("corte da manha: truncate_at = O 00:00 num store com o dia O => available_until O 00:00 (== deck truncado)", {
  root <- fx_root()
  full <- pc_deck_sentinel("end", days = days, until = at("2026-10-01 09:00"))
  cut <- pc_deck_sentinel("end", days = days, until = at("2026-10-01 00:00"))
  m <- prevcarga_resolve_cuts(mk_ctx(full, root, O), O, truncate_at = at("2026-10-01 00:00"))
  expect_identical(m$available_until, at("2026-10-01 00:00"))
  expect_identical(m$weather_issue_limit, at("2026-10-01 00:00"))
  expect_identical(m$cuts[model == "holt-winters"]$input_cut, at("2026-10-01 00:00"))
  expect_identical(m$cuts[model == "random-forest"]$first_delivery, at("2026-10-01 00:30"))
  ## FORBIDDEN: deck fisicamente sem o dia O da os mesmos cortes (antes: completude.no_load)
  p <- prevcarga_resolve_cuts(mk_ctx(cut, root, O), O)
  expect_identical(p[names(p) != "cuts"], m[names(m) != "cuts"])
  expect_identical(as.data.frame(p$cuts), as.data.frame(m$cuts))
  ## CONTROL: sem truncate_at o store com o dia O deriva 09:00; o corte de treino nao muda
  c9 <- prevcarga_resolve_cuts(mk_ctx(full, root, O), O)
  expect_identical(c9$available_until, at("2026-10-01 09:00"))
  expect_identical(c9$cuts$last_data_day, m$cuts$last_data_day)
  expect_identical(c9$cuts$train_origin, m$cuts$train_origin)
})
