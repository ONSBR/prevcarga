## test-backtest-weather.R -- M7.8: weather_issue_cutoff ponta a ponta no backtest (ADR-0024; ADR-0023, ADR-0040).
## Deck sintetico com emissoes 00Z/06Z/18Z por dia (fixture_store.R) e dois brinquedos: `toy-w` (previsto no treino; y_pred
## sensivel a emissao mais fresca vista pelo painel) e `toy-h` (sem previsto no treino). Cada FORBIDDEN (delta 0) tem o seu
## CONTROL (delta != 0). O M7 nunca filtra clima: o limite vem do leitor (deck_reader.R) e so e repassado/registrado.
## Tudo em tempdir; sem variaveis de ambiente. Rodar com TZ=UTC LC_ALL=C.UTF-8.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
lib <- file.path(pc_root(), "scripts", "lib")

fx <- new.env()
sys.source(file.path(lib, "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

BW_TZ <- "Etc/GMT+3"
WO <- lubridate::ymd("2026-10-06")
bw_utc <- function(day, hm) lubridate::ymd_hm(paste(format(lubridate::as_date(day)), hm), tz = "UTC")
RULES <- c("available_until", "origin_midnight_utc")

## y_pred = nivel do artefato + ultima carga vista + media da temperatura prevista da emissao mais fresca do painel
SENS_W <- c(
  "toy_ingestion <- function(panel, cuts) list(load = panel$load, area = panel$load$area_code[1L], wf = panel$weather_forecast)",
  "toy_inference_features <- function(frame, payload) {",
  "  wf <- frame$wf",
  "  fresh <- if (NROW(wf)) mean(wf$temperature[wf$issue_time == max(wf$issue_time)]) else 0",
  "  list(area = frame$area, last = utils::tail(frame$load$load_mwh, 1L), fresh = fresh)",
  "}",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 9L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = rep(payload$model$level, length(ie)) + inf_features$last + inf_features$fresh))",
  "}")
SENS_H <- c(
  "toy_inference_features <- function(frame, payload) list(area = frame$area, last = utils::tail(frame$load$load_mwh, 1L))",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 9L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = rep(payload$model$level, length(ie)) + inf_features$last))",
  "}")

## o descriptor vai por dput (sem closure): uma funcao literal por brinquedo
DESC_BASE <- function(d) { d$modes_implemented <- c("train", "infer"); d$stages$forecast <- NULL; d$model_time_offset_min <- 0L; d }
DESC_W <- function(d) { d$modes_implemented <- c("train", "infer"); d$stages$forecast <- NULL; d$model_time_offset_min <- 0L
                        d$weather_forecast_in_train <- TRUE; d }

toy_dir <- function(name, sens, descriptor) {
  pc_toy_variant("toy-60", name = name,
                 run_yaml = list(split = "origin_agnostic", modes = c("train", "infer"), horizon_days = 9L,
                                 backtest = list(protocol_default = "daily")),
                 descriptor = descriptor,
                 files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines("toy-60", 0L, 0L, character(), "ok"),
                              "R/zz_sens.R" = sens))
}

ROOT <- pc_install_copy(mutate = function(r) {
  tvs <- list(toy_dir("toy-w", SENS_W, DESC_W), toy_dir("toy-h", SENS_H, DESC_BASE))
  for (tv in tvs) for (n in setdiff(list.files(tv), "_toy-engine"))
    file.copy(file.path(tv, n), file.path(r, "models"), recursive = TRUE)
  file.copy(file.path(tvs[[1L]], "_toy-engine"), file.path(r, "models"), recursive = TRUE)
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [A, B], loss: PX}",
               "order: [X]", "members:",
               "  - {sigla: MW, mdir: toy-w, exogena: null}",
               "  - {sigla: MH, mdir: toy-h, exogena: null}"),
             file.path(r, "config", "topology.yaml"))
})

make_deck <- function(lag = 0L) {
  d <- tempfile("bw-deck-")
  fx$prevcarga_fixture_deck(d, seed = 5, areas = c("A", "B"), days = seq(WO - 30L, WO + 1L, by = "day"), weather_lag_hours = lag)
  d
}
DECK <- make_deck()

mod <- new.env()
for (m in c("errors", "runtime", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
            "features", "registry", "training", "inference", "build_long_output", "evaluation", "combination", "reconcile", "cache", "honesty_gate", "orchestrate", "backtest"))
  sys.source(file.path(lib, paste0(m, ".R")), envir = mod)
suppressMessages(mod$prevcarga_runtime_enforce())

wctx <- function(rule = NULL, data = DECK, config = NULL, .frame = parent.frame()) {
  state <- withr::local_tempdir("bw-state-", .local_envir = .frame)
  a <- list("--data" = data, "--state" = state)
  if (!is.null(rule)) a[["--weather-cutoff"]] <- rule
  if (!is.null(config)) a[["--config"]] <- config
  ctx <- mod$prevcarga_ctx(a, ROOT, "backtest")
  ctx$code_commit <- "c0ffee"
  ctx$data_hash <- "deck-fixed"                          # chaves iguais entre decks perturbados (so o dado muda)
  ctx$progress <- function(done, total) NULL
  ctx
}

run_w <- function(ctx, models = "toy-w", ...)
  mod$prevcarga_backtest(ctx, origins = WO, models = models, areas = "A", cortes = "09:00", ...)

member_ypred <- function(res, membro) {
  p <- res$paths$members
  f <- p$long_parquet[p$membro == membro & p$folha == "A"]
  data.table::as.data.table(arrow::read_parquet(f))$y_pred
}

## perturba emissoes de clima previsto da area A: issue_time em (after, at_or_before] (UTC)
perturb_issue <- function(after = NULL, at_or_before = NULL)
  fx$prevcarga_fixture_perturb(DECK, channel = "weather_forecast", areas = "A", issue_after = after, issue_at_or_before = at_or_before)

## limite e emissoes por regra (corte 09:00 no fuso -03 => 12:00Z; lag 0 do deck sintetico)
LIMIT <- list(available_until = bw_utc(WO, "12:00"), origin_midnight_utc = bw_utc(WO, "00:00"))
LAST_ALLOWED <- list(available_until = list(after = bw_utc(WO, "00:00"), at_or_before = bw_utc(WO, "06:00")),
                     origin_midnight_utc = list(after = bw_utc(WO - 1L, "18:00"), at_or_before = bw_utc(WO, "00:00")))
ABOVE <- list(available_until = list(after = bw_utc(WO, "12:00")), origin_midnight_utc = list(after = bw_utc(WO, "00:00")))

expect_pc <- function(expr, id, exit = 2L) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (!is.null(cond)) { expect_identical(cond$id, id); expect_identical(cond$exit_code, as.integer(exit)) }
  invisible(cond)
}

## --- AC1: FORBIDDEN/CONTROL no canal de clima, nas duas regras, serial e --jobs 2 ------------------------------------
for (rule in RULES) {
  test_that(paste("[AC1] clima previsto honesto na regra", rule, "(serial e --jobs 2)"), {
    base <- member_ypred(run_w(wctx(rule)), "MW")
    expect_false(anyNA(base))
    ## o limite da regra e o do leitor
    plan <- mod$prevcarga_plan(wctx(rule), WO, "09:00", filters = list(model = "toy-w", area = "A"))
    expect_equal(as.numeric(plan$cells$weather_issue_limit[[1L]]), as.numeric(LIMIT[[rule]]))
    ## FORBIDDEN: emissao com issue_time acima do limite nao altera nada; CONTROL: a ultima emissao permitida altera
    above <- do.call(perturb_issue, ABOVE[[rule]])
    expect_equal(member_ypred(run_w(wctx(rule, data = above)), "MW"), base)
    last <- do.call(perturb_issue, LAST_ALLOWED[[rule]])
    ctrl <- member_ypred(run_w(wctx(rule, data = last)), "MW")
    expect_gt(max(abs(ctrl - base)), 0)
    ## serial == --jobs 2 (o filtro de clima e o mesmo do leitor nos workers PSOCK)
    expect_equal(member_ypred(run_w(wctx(rule, data = last), jobs = 2L), "MW"), ctrl)
    expect_equal(member_ypred(run_w(wctx(rule, data = above), jobs = 2L), "MW"), base)
  })
}

test_that("[AC1] a 06Z de O e legal em available_until e vazamento em origin_midnight_utc (CONTROL entre regras)", {
  d06 <- perturb_issue(after = bw_utc(WO, "00:00"), at_or_before = bw_utc(WO, "06:00"))
  b_au <- member_ypred(run_w(wctx("available_until")), "MW"); b_mid <- member_ypred(run_w(wctx("origin_midnight_utc")), "MW")
  expect_gt(max(abs(member_ypred(run_w(wctx("available_until", data = d06)), "MW") - b_au)), 0)
  expect_equal(member_ypred(run_w(wctx("origin_midnight_utc", data = d06)), "MW"), b_mid)
})

test_that("[AC1] lag de publicacao do deck: emissao dentro da janela de lag nao tem efeito; a ultima permitida tem", {
  ## lag 12 h, corte 09:00 (12Z): available_until => limite 00Z de O; a 06Z de O esta dentro da janela do lag (06 + 12 > 12Z)
  d12 <- make_deck(lag = 12L)
  pert <- function(...) fx$prevcarga_fixture_perturb(d12, channel = "weather_forecast", areas = "A", ...)
  base <- member_ypred(run_w(wctx("available_until", data = d12)), "MW")
  in_lag <- pert(issue_after = bw_utc(WO, "00:00"), issue_at_or_before = bw_utc(WO, "06:00"))
  expect_equal(member_ypred(run_w(wctx("available_until", data = in_lag)), "MW"), base)                   # FORBIDDEN: dentro do lag
  last <- pert(issue_after = bw_utc(WO - 1L, "18:00"), issue_at_or_before = bw_utc(WO, "00:00"))
  expect_gt(max(abs(member_ypred(run_w(wctx("available_until", data = last)), "MW") - base)), 0)           # CONTROL: ultima permitida
  after_cut <- pert(issue_after = bw_utc(WO, "12:00"))
  expect_equal(member_ypred(run_w(wctx("available_until", data = after_cut)), "MW"), base)                 # FORBIDDEN: apos o corte
  ## CONTROL: sem lag (deck 0) a mesma 06Z de O e a ultima permitida; origin_midnight_utc ignora o lag (a 00Z continua a ultima)
  d0 <- perturb_issue(after = bw_utc(WO, "00:00"), at_or_before = bw_utc(WO, "06:00"))
  expect_gt(max(abs(member_ypred(run_w(wctx("available_until", data = d0)), "MW") -
                    member_ypred(run_w(wctx("available_until")), "MW"))), 0)
  bm <- member_ypred(run_w(wctx("origin_midnight_utc", data = d12)), "MW")
  expect_gt(max(abs(member_ypred(run_w(wctx("origin_midnight_utc", data = last)), "MW") - bm)), 0)
  ## o lag move o limite (12 h) e muda o run_id
  p12 <- mod$prevcarga_plan(wctx("available_until", data = d12), WO, "09:00", filters = list(model = "toy-w", area = "A"))
  p0 <- mod$prevcarga_plan(wctx("available_until"), WO, "09:00", filters = list(model = "toy-w", area = "A"))
  expect_equal(as.numeric(difftime(p0$cells$weather_issue_limit[[1L]], p12$cells$weather_issue_limit[[1L]], units = "hours")), 12)
  expect_false(identical(p12$run_id, p0$run_id))
})

test_that("[AC1] molde prevcarga_honesty_gate_weather sobre o limite do plano: HONESTO nas duas regras; emissao sobre o limite VAZA", {
  ctx <- wctx("available_until")
  fc <- as.data.frame(mod$prevcarga_read_weather(ctx, "forecast", areas = "A", from = WO - 2L, to = WO + 2L))
  panel <- data.frame(interval_start = seq(bw_utc(WO, "00:00"), by = "30 min", length.out = 4L), y_true = 1)
  for (rule in RULES) {
    lim <- mod$prevcarga_plan(wctx(rule), WO, "09:00", filters = list(model = "toy-w", area = "A"))$cells$weather_issue_limit[[1L]]
    honest <- function(p, w) { f <- mod$prevcarga_freshest_issue(data.table::as.data.table(w), lim)
                               data.frame(k = f$valid_time, y_pred = f$temperature) }
    g <- mod$prevcarga_honesty_gate_weather(honest, panel, fc, lim, cols = "temperature")
    expect_identical(g$verdict, "HONESTO", info = rule)
    expect_gt(g$control_delta, 1e-5)
    leaky <- function(p, w) { f <- data.table::as.data.table(w); f <- f[issue_time == max(issue_time)]
                              data.frame(k = f$valid_time, y_pred = f$temperature) }          # ignora o limite
    expect_identical(mod$prevcarga_honesty_gate_weather(leaky, panel, fc, lim, cols = "temperature")$verdict, "VAZA", info = rule)
  }
})

## --- AC2: chaves de features/member_long/artefato com e sem previsto no treino -------------------------------------------
test_that("[AC2] chaves por regra: member_long e artefato com previsto no treino mudam; artefato sem previsto, nao", {
  plans <- lapply(stats::setNames(RULES, RULES), function(r) mod$prevcarga_plan(wctx(r), WO, "09:00", filters = list(area = "A")))
  ml <- function(p, sigla) p$cells$cache_key[p$cells$membro == sigla]
  for (m in c("MW", "MH")) expect_false(identical(ml(plans[[1L]], m), ml(plans[[2L]], m)), info = m)   # member_long
  art_key <- function(rule, sigla) {
    ctx <- wctx(rule); p <- mod$prevcarga_plan(ctx, WO, "09:00", filters = list(area = "A"))
    cell <- p$cells[which(p$cells$membro == sigla)[[1L]], ]
    do.call(mod$prevcarga_cache_key, mod$.bt_key_args(ctx, p, cell, mod$.bt_spec(ctx, cell)))
  }
  expect_false(identical(art_key(RULES[[1L]], "MW"), art_key(RULES[[2L]], "MW")))   # previsto no treino: regra entra
  expect_identical(art_key(RULES[[1L]], "MH"), art_key(RULES[[2L]], "MH"))          # sem previsto: sem over-key
  ## o limite do treino vem do dates.R (plano) e diferencia as regras
  expect_false(identical(plans[[1L]]$cells$train_weather_issue_limit[[1L]], plans[[2L]]$cells$train_weather_issue_limit[[1L]]))
  ## meta.json do artefato registra regra e limite efetivos
  ctx <- wctx("origin_midnight_utc"); res <- run_w(ctx)
  ms <- Sys.glob(file.path(ctx$state_dir, "cache", "artifact", "*", "*", "meta.json"))
  expect_length(ms, 1L)
  w <- jsonlite::read_json(ms)$identity$feature_config$weather
  expect_identical(w$weather_issue_cutoff, "origin_midnight_utc")
  expect_match(w$weather_issue_limit, "^\\d{4}-\\d{2}-\\d{2}T")
})

## --- AC3: run_id, result e .key divergentes entre regras (flag > config.yaml > default) -----------------------------------
test_that("[AC3] run_id e result.weather_issue_cutoff por regra; flag > config.yaml > default", {
  cfg <- function(rule) { f <- withr::local_tempfile(fileext = ".yaml", .local_envir = parent.frame())
                          yaml::write_yaml(list(schema_version = 1L, weather_issue_cutoff = rule), f); f }
  req <- function(ctx) mod$prevcarga_plan_result(mod$prevcarga_plan_request(ctx, list("--corte" = "09:00", "--model" = "toy-w", "--area" = "A")))
  ctxw <- function(...) { cx <- wctx(...); cx$origin <- WO; cx }
  r_au <- req(ctxw("available_until")); r_mid <- req(ctxw("origin_midnight_utc"))
  expect_false(identical(r_au$run_id, r_mid$run_id))
  expect_identical(r_au$weather_issue_cutoff, "available_until")
  expect_identical(r_mid$weather_issue_cutoff, "origin_midnight_utc")
  expect_identical(req(ctxw())$weather_issue_cutoff, "available_until")                                   # default
  c_mid <- req(ctxw(config = cfg("origin_midnight_utc")))
  expect_identical(c_mid$weather_issue_cutoff, "origin_midnight_utc"); expect_identical(c_mid$run_id, r_mid$run_id)   # config.yaml
  f <- req(ctxw("available_until", config = cfg("origin_midnight_utc")))                                  # flag vence o config
  expect_identical(f$weather_issue_cutoff, "available_until"); expect_identical(f$run_id, r_au$run_id)
  ## backtest: summary registra a regra efetiva
  res <- run_w(wctx("origin_midnight_utc"))
  expect_identical(res$summary$weather_issue_cutoff, "origin_midnight_utc")
  expect_identical(res$summary$verdict, "ok")
})

test_that("[AC3] celula done na 1a regra NAO conta done na 2a: .key diverge e a celula recomputa no --resume", {
  c1 <- wctx("available_until"); r1 <- run_w(c1)
  p1 <- mod$prevcarga_plan(c1, WO, "09:00", filters = list(model = "toy-w", area = "A"))
  c2 <- wctx("origin_midnight_utc")
  p2 <- mod$prevcarga_plan(c2, WO, "09:00", filters = list(model = "toy-w", area = "A"))
  expect_false(identical(p1$cells$cache_key, p2$cells$cache_key))
  ## copia a celula concluida da regra 1 para o diretorio da celula da regra 2 (mesmo membro/folha/origem/corte)
  from <- mod$.m7_cell_dir(c1, p1$run_id, p1$cells[1L]); to <- mod$.m7_cell_dir(c2, p2$run_id, p2$cells[1L])
  dir.create(dirname(to), recursive = TRUE); file.copy(from, dirname(to), recursive = TRUE)
  expect_identical(mod$.m7_cell_state(c2, p2$run_id, p2$cells[1L]), "pending")                          # .key da regra 1 diverge
  expect_identical(mod$.m7_cell_state(c1, p1$run_id, p1$cells[1L]), "done")                             # CONTROL: mesma regra
  res <- run_w(c2, resume = TRUE)
  expect_identical(res$summary$ok, 1L)
  expect_identical(readLines(file.path(to, ".key")), p2$cells$cache_key)                                # recomputada com a chave da regra 2
})

## --- AC4: pureza do canal de clima em backtest.R / cache.R ----------------------------------------------------------------
test_that("[AC4] backtest.R e cache.R sem filtro proprio de emissao (issue_time <=, issue_time<, freshest)", {
  strip <- function(f) { l <- readLines(file.path(lib, f)); paste(sub("^\\s*##.*$", "", l), collapse = "\n") }
  for (f in c("backtest.R", "cache.R")) {
    code <- strip(f)
    for (p in c("issue_time\\s*<=", "issue_time\\s*<", "freshest")) expect_false(grepl(p, code), info = paste(f, p))
  }
  ## CONTROL: o grep enxerga o que procura, e o leitor (unico dono) o contem
  expect_true(grepl("issue_time\\s*<=", "ok <- fc[issue_time <= limit]"))
  expect_true(grepl("freshest", strip("deck_reader.R")))
})
