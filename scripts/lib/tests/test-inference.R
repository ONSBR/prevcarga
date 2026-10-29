## test-inference.R -- cortes intradiarios e conferencias de convencao da inferencia (M5.6; ADR-0023, ADR-0024, ADR-0032,
## ADR-0040). Molde FORBIDDEN Delta=0 / CONTROL Delta != 0 do honesty_gate: cada braco proibido tem o seu controle.
## Deck sintetico (fixture_store.R) -> treino M5.3 -> registry (M5.2) -> inferencia. Plugins-brinquedo com estagio de
## inferencia real que LE a carga do dia O (membro tipo LGBM/DL), nao le o dia O (tipo RF) ou le o clima previsto.
## Tudo em tempdir; sem variaveis de ambiente; datas por lubridate com tz explicito.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                       "registry", "features", "training", "inference"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
AREAS <- c("A", "B")
at <- function(hm, day = "2026-10-01", tz = TZ) lubridate::ymd_hm(paste(day, hm), tz = tz)
utc <- function(hm, day = "2026-10-01") lubridate::ymd_hm(paste(day, hm), tz = "UTC")

expect_pc <- function(expr, id, exit = 2L) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$exit_code, as.integer(exit))
  invisible(cond)
}

## --- plugins-brinquedo --------------------------------------------------------------------------------------------
## 30 min tipo LGBM: y = nivel + media das 4 ultimas leituras de carga (le o dia O) + media do clima previsto (se houver).
IC_READ30 <- c(
  "toy_ingestion <- function(panel, cuts) list(load = panel$load, n = nrow(panel$load), weather_forecast = panel$weather_forecast)",
  "toy_inference_features <- function(frame, payload) {",
  "  ld <- frame$load; w <- frame$weather_forecast",
  "  list(area = ld$area_code[1L], today = mean(utils::tail(ld$load_mwh, 4L)), wx = if (NROW(w)) mean(w$temperature) else 0)",
  "}",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 2L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie,",
  "         y_pred = payload$model$level[toy_slots(ie)] + inf_features$today + inf_features$wx))",
  "}")
## 60 min tipo dessem-linear (le o dia O pelas 4 ultimas horas completas) e tipo RF (le so o dia O-1, nunca o dia O).
IC_ING60 <- "toy_ingestion <- function(panel, cuts) list(load = panel$load, area = panel$load$area_code[1L])"
IC_CORE60 <- c(
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 9L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = rep(payload$model$level, length(ie)) + inf_features$today))",
  "}",
  "toy_forecast <- function(payload, inf_features, cuts) toy_inference_core(payload, inf_features, cuts)")
IC_FEAT60_DL <- "toy_inference_features <- function(frame, payload) list(area = frame$area, today = mean(utils::tail(frame$load$load_mwh, 4L)))"
IC_FEAT60_RF <- c(
  "toy_inference_features <- function(frame, payload) {",
  "  ld <- frame$load; d <- lubridate::as_date(ld$interval_start, tz = lubridate::tz(ld$interval_start))",
  "  list(area = frame$area, today = mean(ld$load_mwh[d == max(d) - 1L]))",
  "}")

ic_def <- function(base, run_yaml = list(), descriptor = list(), files = list(), kind = "ok") {
  list(base = base, run_yaml = run_yaml, descriptor = descriptor, files = files, kind = kind,
       native = if (identical(base, "toy-60")) 60L else 0L)
}
IC_DEFS <- list(
  "toy-30" = ic_def("toy-30", descriptor = list(uses_rng = TRUE), files = list("R/zz_read.R" = IC_READ30)),
  "toy-30w" = ic_def("toy-30", descriptor = list(uses_rng = TRUE, weather_forecast_in_train = TRUE),
                     files = list("R/zz_read.R" = IC_READ30)),
  "toy-60dl" = ic_def("toy-60", files = list("R/zz_read.R" = c(IC_ING60, IC_FEAT60_DL, IC_CORE60))),
  "toy-60rf" = ic_def("toy-60", files = list("R/zz_read.R" = c(IC_ING60, IC_FEAT60_RF, IC_CORE60))),
  "toy-60" = ic_def("toy-60"),
  "toy-60mis" = ic_def("toy-60", kind = "from_0900"))

.ic_roots <- new.env()
ic_models <- function() {
  if (!is.null(.ic_roots$r)) return(.ic_roots$r)
  rt <- tempfile("ic-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
  for (nm in names(IC_DEFS)) {
    d <- IC_DEFS[[nm]]
    files <- c(list("R/zz_dispatch.R" = .pc_toy_dispatch_lines(d$base, native_min = d$native, kind = d$kind)), d$files)
    vr <- pc_toy_variant(d$base, run_yaml = modifyList(list(backtest = list(protocol_default = "fixed")), d$run_yaml),
                         descriptor = d$descriptor, files = files, name = nm)
    file.copy(file.path(vr, nm), file.path(rt, "models"), recursive = TRUE)
  }
  file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
  .ic_roots$r <- rt
  rt
}

## decks: `base` = SECO-like A com carga ate 09:30 e B ate 09:00 (available_until global 09:00 = minimo entre areas)
.ic_decks <- new.env()
ic_deck <- function(key = "base", load_until = at("09:00"), tz = TZ, load_label = "start", drop_b = FALSE) {
  if (!is.null(.ic_decks[[key]])) return(.ic_decks[[key]])
  dir <- tempfile("ic-deck-")
  env$prevcarga_fixture_deck(dir, seed = 11, areas = AREAS, days = seq(O - 13L, O, by = "day"),
                             load_until = load_until, tz = tz, load_label = load_label)
  pc_deck_topology(file.path(dir, "topology.yaml"), AREAS)
  if (drop_b) {
    d2 <- env$prevcarga_fixture_drop_slots(dir, "B", O, "09:00", 1L)
    file.copy(file.path(dir, "topology.yaml"), d2)
    dir <- d2
  }
  .ic_decks[[key]] <- dir
  dir
}
ic_spread <- function() ic_deck("spread", load_until = at("09:30"), drop_b = TRUE)
ic_bump <- function(dir, ...) env$prevcarga_fixture_perturb(dir, "load", ..., perturb = function(v) v + 1000)
ic_bump_wx <- function(dir, ...) env$prevcarga_fixture_perturb(dir, "weather_forecast", ..., perturb = function(v) v + 100)

ic_ctx <- function(dir, root, state, ...)
  modifyList(list(root = root, data_dir = dir, topology_path = file.path(dir, "topology.yaml"),
                  weather_issue_cutoff = "available_until", origin = O, state_dir = state,
                  code_commit = "c0ffee", renv_lock_hash = "ab12"), list(...))
ic_setup <- function(mdir, deck = ic_deck(), ...) {
  st <- pc5_state_tmp(.local_envir = parent.frame())
  list(mem = list(sigla = toupper(mdir), mdir = mdir, exogena = NA_character_),
       ctx = ic_ctx(deck, ic_models(), st, ...), deck = deck)
}
ic_train <- function(s, origin = O) suppressWarnings(env$prevcarga_train(s$mem, "A", origin, s$ctx))
ic_infer <- function(s, area = "A") suppressWarnings(env$prevcarga_infer_cell(s$mem, area, O, s$ctx))
ic_on <- function(s, deck, ...) { s$ctx$data_dir <- deck; s$ctx$topology_path <- file.path(deck, "topology.yaml"); s$ctx <- modifyList(s$ctx, list(...)); s }
ic_delta <- function(a, b) {
  expect_identical(a$predictions$target_datetime, b$predictions$target_datetime)
  max(abs(a$predictions$y_pred - b$predictions$y_pred))
}

## =====================================================================================================
## AC1 -- corte por granularidade: available_until = minimo entre areas; o 09:00-09:30 de A nunca entra
## =====================================================================================================
test_that("AC1: deck com A ate 09:30 e B ate 09:00 => available_until 09:00 e 1a entrega 09:30", {
  s <- ic_setup("toy-30", ic_spread())
  ic_train(s)
  r <- ic_infer(s)
  expect_identical(r$available_until, "2026-10-01T09:00:00-03:00")
  expect_identical(r$first_target, "2026-10-01T09:30:00-03:00")
})

for (m in c("toy-30", "toy-60dl", "toy-60rf")) {
  test_that(sprintf("AC1 [%s]: perturbar o 09:00-09:30 de A => Delta=0 (FORBIDDEN)", m), {
    s <- ic_setup(m, ic_spread())
    ic_train(s)
    base <- ic_infer(s)
    pert <- ic_infer(ic_on(s, ic_bump(s$deck, after = at("08:45"), at_or_before = at("09:00"), areas = "A")))
    expect_identical(ic_delta(pert, base), 0)
  })
}

test_that("AC1 [CONTROL]: LGBM-like (30) perturbar o 08:30-09:00 => Delta != 0", {
  s <- ic_setup("toy-30", ic_spread())
  ic_train(s)
  base <- ic_infer(s)
  pert <- ic_infer(ic_on(s, ic_bump(s$deck, after = at("08:15"), at_or_before = at("08:30"), areas = "A")))
  expect_gt(ic_delta(pert, base), 1)
})

test_that("AC1 [CONTROL]: dessem-linear-like (60) perturbar o 08:00-09:00 => Delta != 0", {
  s <- ic_setup("toy-60dl", ic_spread())
  ic_train(s)
  base <- ic_infer(s)
  pert <- ic_infer(ic_on(s, ic_bump(s$deck, after = at("07:45"), at_or_before = at("08:30"), areas = "A")))
  expect_gt(ic_delta(pert, base), 1)
})

test_that("AC1: RF-like (nao le o dia O): perturbar 08:00-09:00 => Delta=0; CONTROL = perturbar O-1 => Delta != 0", {
  s <- ic_setup("toy-60rf", ic_spread())
  ic_train(s)
  base <- ic_infer(s)
  expect_identical(ic_delta(ic_infer(ic_on(s, ic_bump(s$deck, after = at("07:45"), at_or_before = at("08:30"), areas = "A"))), base), 0)
  prev <- ic_bump(s$deck, after = at("00:00", "2026-09-30") - lubridate::minutes(1L), at_or_before = at("23:30", "2026-09-30"), areas = "A")
  expect_gt(ic_delta(ic_infer(ic_on(s, prev)), base), 1)
})

## =====================================================================================================
## AC2 -- modelo horario: hora incompleta ignorada, 1a entrega = available_until + 30
## =====================================================================================================
test_that("AC2: dois decks iguais salvo o 09:00-09:30 => Delta=0 nos alvos comuns; 1o alvo 09:30 vs 10:00", {
  d09 <- ic_deck("d09", at("09:00"))
  d0930 <- ic_deck("d0930", at("09:30"))
  s <- ic_setup("toy-60dl", d09)
  ic_train(s)
  a <- ic_infer(s)
  b <- ic_infer(ic_on(s, d0930))
  expect_identical(a$predictions$target_datetime[[1L]], at("09:30"))
  expect_identical(b$predictions$target_datetime[[1L]], at("10:00"))
  common <- intersect(as.numeric(a$predictions$target_datetime), as.numeric(b$predictions$target_datetime))
  ya <- a$predictions$y_pred[as.numeric(a$predictions$target_datetime) %in% common]
  yb <- b$predictions$y_pred[as.numeric(b$predictions$target_datetime) %in% common]
  expect_identical(ya, yb)
  expect_identical(a$available_until, "2026-10-01T09:00:00-03:00")
  expect_identical(b$available_until, "2026-10-01T09:30:00-03:00")
})

test_that("AC2 [CONTROL]: o modelo de 30 min usa a meia hora a mais (Delta != 0 entre os dois decks)", {
  s <- ic_setup("toy-30", ic_deck("d09"))
  ic_train(s)
  a <- ic_infer(s)
  b <- ic_infer(ic_on(s, ic_deck("d0930")))
  common <- intersect(as.numeric(a$predictions$target_datetime), as.numeric(b$predictions$target_datetime))
  expect_gt(max(abs(a$predictions$y_pred[as.numeric(a$predictions$target_datetime) %in% common] -
                    b$predictions$y_pred[as.numeric(b$predictions$target_datetime) %in% common])), 1)
})

## =====================================================================================================
## AC3 -- alinhamento da entrega na grade prevcarga_delivery_grid
## =====================================================================================================
test_that("AC3: deck com available_until 09:30 => os 5 tipos entregam a partir de 10:00 na grade, rotulo interval_end", {
  d <- ic_deck("d0930")
  for (m in c("toy-30", "toy-30w", "toy-60", "toy-60dl", "toy-60rf")) {
    s <- ic_setup(m, d)
    ic_train(s)
    r <- ic_infer(s)
    days <- if (grepl("^toy-60", m)) 9L else 2L
    cuts <- list(available_until = at("09:30"), tz = TZ, origin_day = O)
    expect_identical(r$predictions$target_datetime, env$prevcarga_delivery_grid(cuts, days))
    expect_identical(r$predictions$target_datetime[[1L]], at("10:00"))
    expect_identical(attr(r$predictions$target_datetime, "tzone"), TZ)
    expect_identical(r$predictions$target_datetime[[nrow(r$predictions)]], at("00:00", format(O + days)))
  }
})

test_that("AC3 [CONTROL]: plugin-fixture que entrega a partir de 09:30 sai exit 2 (despachante ou fachada)", {
  s <- ic_setup("toy-60mis", ic_deck("d0930"))
  ic_train(s)
  cond <- tryCatch(ic_infer(s), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$exit_code, 2L)
})

test_that("AC3 [CONTROL]: prevcarga_infer_check_alignment => contract.output_misaligned (1o/ultimo esperado e obtido)", {
  cuts <- list(available_until = at("09:30"), tz = TZ, origin_day = O)
  desc <- list(model_id = "toy", delivery_days = 2L)
  grid <- env$prevcarga_delivery_grid(cuts, 2L)
  expect_true(env$prevcarga_infer_check_alignment(data.frame(target_datetime = grid), cuts, desc))
  for (bad in list(c(at("09:30"), grid), grid[-1L], grid[-5L], c(grid[1:3], grid[3:length(grid)]),
                   lubridate::with_tz(grid, "UTC"), c(grid[-1L], NA))) {
    cond <- expect_pc(env$prevcarga_infer_check_alignment(data.frame(target_datetime = bad), cuts, desc),
                      "contract.output_misaligned")
    expect_true(all(c("esperado", "obtido", "n_esperado", "n_obtido") %in% names(cond$details)))
  }
})

test_that("AC3: a fachada confere o alinhamento DEPOIS do plugin (despachante substituido devolvendo a grade sem o 1o alvo)", {
  s <- ic_setup("toy-60", ic_deck("d0930"))
  ic_train(s)
  orig <- env$prevcarga_plugin_infer
  assign("prevcarga_plugin_infer", function(...) { e <- orig(...); e$delivery <- e$delivery[-1L]; e }, envir = env)
  withr::defer(assign("prevcarga_plugin_infer", orig, envir = env))
  expect_pc(ic_infer(s), "contract.output_misaligned")
})

## =====================================================================================================
## AC4 -- regra de clima por emissao (00Z/06Z/18Z) nas duas regras
## =====================================================================================================
ic_wx_bump <- function(dir, hm0, hm1, day0 = "2026-10-01") ic_bump_wx(dir, issue_after = utc(hm0, day0), issue_at_or_before = utc(hm1))

test_that("AC4: regra available_until (12Z): perturbar a 18Z => Delta=0; CONTROL perturbar a 06Z => Delta != 0", {
  s <- ic_setup("toy-30w")
  ic_train(s)
  base <- ic_infer(s)
  expect_identical(base$weather_issue_cutoff, "available_until")
  expect_identical(ic_delta(ic_infer(ic_on(s, ic_wx_bump(s$deck, "17:00", "18:00"))), base), 0)
  expect_gt(ic_delta(ic_infer(ic_on(s, ic_wx_bump(s$deck, "05:00", "06:00"))), base), 1)
})

test_that("AC4: --weather-cutoff origin_midnight_utc sobre artefato available_until => contract.weather_cutoff_mismatch", {
  s <- ic_setup("toy-30w")
  ic_train(s)
  cond <- expect_pc(ic_infer(ic_on(s, s$deck, weather_issue_cutoff = "origin_midnight_utc")), "contract.weather_cutoff_mismatch")
  expect_identical(cond$details$contract.weather_cutoff_mismatch$artefato, "available_until")
  expect_identical(cond$details$contract.weather_cutoff_mismatch$efetivo, "origin_midnight_utc")
})

test_that("AC4: artefato origin_midnight_utc roda nessa regra: 06Z => Delta=0; CONTROL 00Z => Delta != 0", {
  s <- ic_setup("toy-30w", weather_issue_cutoff = "origin_midnight_utc")
  ic_train(s)
  base <- ic_infer(s)
  expect_identical(base$weather_issue_cutoff, "origin_midnight_utc")
  expect_identical(ic_delta(ic_infer(ic_on(s, ic_wx_bump(s$deck, "05:00", "06:00"))), base), 0)
  expect_gt(ic_delta(ic_infer(ic_on(s, ic_wx_bump(s$deck, "23:00", "00:00", day0 = "2026-09-30"))), base), 1)
})

test_that("AC4 [CONTROL]: modelo sem clima previsto no treino NAO tem o corte de clima conferido pela fachada", {
  cuts <- list(origin = O, tz = TZ, load_label = "end", weather_issue_cutoff = "origin_midnight_utc")
  art <- list(spec_efetivo = list(split = "origin_agnostic"), load_label = "end", timezone = TZ,
              weather_issue_cutoff = "available_until")
  expect_true(env$prevcarga_infer_check_conventions(art, cuts, list(load_label = "end"), list(model_id = "x", weather_forecast_in_train = FALSE)))
  expect_pc(env$prevcarga_infer_check_conventions(art, cuts, list(load_label = "end"), list(model_id = "x", weather_forecast_in_train = TRUE)),
            "contract.weather_cutoff_mismatch")
})

## =====================================================================================================
## AC5 -- rotulo e fuso da carga
## =====================================================================================================
test_that("AC5: artefato com load_label start sobre deck end => contract.load_label_mismatch; mesma convencao roda", {
  d_end <- ic_deck("end", load_label = "end")
  s <- ic_setup("toy-30")
  ic_train(s)
  expect_pc(ic_infer(ic_on(s, d_end)), "contract.load_label_mismatch")
  expect_s3_class(ic_infer(s)$predictions, "data.frame")                   # CONTROL
})

test_that("AC5: artefato com fuso Etc/GMT+2 sobre deck Etc/GMT+3 => contract.timezone_mismatch; mesma convencao roda", {
  d2 <- ic_deck("tz2", at("09:00", tz = "Etc/GMT+2"), tz = "Etc/GMT+2")
  s2 <- ic_setup("toy-30", d2)
  ic_train(s2)
  expect_pc(ic_infer(ic_on(s2, ic_deck())), "contract.timezone_mismatch")
  expect_s3_class(ic_infer(s2)$predictions, "data.frame")                  # CONTROL
})

test_that("AC5: divergencias acumulam e abortam uma vez (rotulo + fuso)", {
  d2 <- ic_deck("tz2", at("09:00", tz = "Etc/GMT+2"), tz = "Etc/GMT+2")
  s2 <- ic_setup("toy-30", d2)
  ic_train(s2)
  cond <- expect_pc(ic_infer(ic_on(s2, ic_deck("end", load_label = "end"))), "contract.load_label_mismatch")
  expect_true(all(c("contract.load_label_mismatch", "contract.timezone_mismatch") %in% names(cond$details)))
})

## =====================================================================================================
## AC6 -- origin_scoped: chave = origem; available_until nao e conferido
## =====================================================================================================
test_that("AC6: artefato treinado em O-1 sobre --origin O => contract.origin_mismatch; o de O roda nos dois decks", {
  s <- ic_setup("toy-60dl", ic_deck("d09"))
  ic_train(s, O - 1L)
  expect_pc(ic_infer(s), "contract.origin_mismatch")
  s <- ic_setup("toy-60dl", ic_deck("d09"))
  ic_train(s)
  a <- ic_infer(s)
  b <- ic_infer(ic_on(s, ic_deck("d12", at("12:00"))))
  expect_identical(a$available_until, "2026-10-01T09:00:00-03:00")
  expect_identical(b$available_until, "2026-10-01T12:00:00-03:00")
  common <- intersect(as.numeric(a$predictions$target_datetime), as.numeric(b$predictions$target_datetime))
  expect_gt(max(abs(a$predictions$y_pred[as.numeric(a$predictions$target_datetime) %in% common] -
                    b$predictions$y_pred[as.numeric(b$predictions$target_datetime) %in% common])), 1e-5)
})

test_that("AC6: origin_agnostic nao confere a origem do artefato (CONTROL do braco origin_scoped)", {
  cuts <- list(origin = O, tz = TZ, load_label = "end", weather_issue_cutoff = "available_until")
  art <- list(spec_efetivo = list(split = "origin_agnostic"), load_label = "end", timezone = TZ,
              weather_issue_cutoff = "available_until", origin = O - 5L)
  expect_true(env$prevcarga_infer_check_conventions(art, cuts, list(load_label = "end"), list(model_id = "x")))
  art$spec_efetivo$split <- "origin_scoped"
  expect_pc(env$prevcarga_infer_check_conventions(art, cuts, list(load_label = "end"), list(model_id = "x")),
            "contract.origin_mismatch")
})

## =====================================================================================================
## Painel e catalogo
## =====================================================================================================
test_that("prevcarga_infer_panel: painel alem de available_until => contract.panel_beyond_cut; 60 min aplica to_hourly", {
  s <- ic_setup("toy-30")
  rc <- env$prevcarga_resolve_cuts(s$ctx, O, model = "toy-30")
  panel <- env$prevcarga_read_panel(s$ctx, O, "A", 1L, 3L)
  data.table::setnames(panel$weather_observed, "timestamp", "valid_time")
  cuts <- list(available_until = rc$available_until, tz = rc$tz)
  p30 <- env$prevcarga_infer_panel(panel, cuts, list(granularity_min = 30L))
  expect_identical(p30$load, panel$load)
  p60 <- env$prevcarga_infer_panel(panel, cuts, list(granularity_min = 60L))
  expect_lt(nrow(p60$load), nrow(panel$load))
  cuts$available_until <- cuts$available_until - lubridate::minutes(30L)
  expect_pc(env$prevcarga_infer_panel(panel, cuts, list(granularity_min = 30L)), "contract.panel_beyond_cut")
})

test_that("errors/infer_cut.tsv: ASCII, carrega pelo loader junto com os demais sem id duplicado; 6 ids contract.* classe 2", {
  path <- file.path(pc_root(), "scripts", "lib", "errors", "infer_cut.tsv")
  raw <- readBin(path, "raw", file.size(path))
  expect_true(all(as.integer(raw) < 128L))
  expect_false(as.raw(13L) %in% raw)
  tsv <- utils::read.delim(path, colClasses = "character")
  expect_identical(names(tsv), c("id", "class", "message", "hint"))
  ids <- c("contract.load_label_mismatch", "contract.timezone_mismatch", "contract.weather_cutoff_mismatch",
           "contract.origin_mismatch", "contract.output_misaligned", "contract.panel_beyond_cut")
  expect_setequal(tsv$id, ids)
  cat <- env$prevcarga_errors_load()
  expect_false(anyDuplicated(cat$id) > 0L)
  for (id in ids) {
    expect_true(id %in% cat$id)
    expect_identical(env$prevcarga_exit_codes[[env$prevcarga_error_lookup(id)$class]], 2L)
  }
})
