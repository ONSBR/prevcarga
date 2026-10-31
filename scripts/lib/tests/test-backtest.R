## test-backtest.R -- M7.3 (parte MAP): backtest.R = truncagem pelo leitor, corte de treino por classe, protocolos, LONG da celula
## (ADR-0023, ADR-0032, ADR-0040). In-process, plugins-brinquedo das duas classes (sensiveis a ultima carga vista) + deck
## sintetico (fixture_store.R). Cada FORBIDDEN (delta 0) tem o seu CONTROL (delta != 0). Tudo em tempdir; sem env vars.
## O REDUCE (combina/reconcile/scores) e testado em test-backtest-reduce.R (M7.7).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
lib <- file.path(pc_root(), "scripts", "lib")

fx <- new.env()
sys.source(file.path(lib, "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

BT_TZ <- "Etc/GMT+3"
BO1 <- lubridate::ymd("2026-10-05")
BO2 <- lubridate::ymd("2026-10-06")
BO3 <- lubridate::ymd("2026-10-07")
bt_at <- function(day, hm) lubridate::ymd_hm(paste(format(lubridate::as_date(day)), hm), tz = BT_TZ)

## --- brinquedos sensiveis: y_pred = nivel do artefato + ultima carga VISTA (hora cheia para 60 min) -------------------
SENS_30 <- c(
  "toy_inference_features <- function(frame, payload) list(area = frame$load$area_code[1L], last = utils::tail(frame$load$load_mwh, 1L))",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 2L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = payload$model$level[toy_slots(ie)] + inf_features$last))",
  "}")
SENS_60 <- c(
  "toy_inference_features <- function(frame, payload) list(area = frame$area, last = utils::tail(frame$load$load_mwh, 1L))",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 9L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = rep(payload$model$level, length(ie)) + inf_features$last))",
  "}")

toy_dir <- function(base, name, sens, run_yaml = list(), descriptor = list()) {
  pc_toy_variant(base, run_yaml = run_yaml, descriptor = descriptor, name = name,
                 files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines(base, 0L, 0L, character(), "ok"),
                              "R/zz_sens.R" = sens))
}

## toy-h: horario, origin_agnostic H=9 (RF); toy-s: semi-horario, origin_agnostic H=1, data_lag_days 2 (LGBM);
## toy-sc: horario, origin_scoped (HW). `proto`: backtest.protocol_default de cada um.
mk_root <- function(proto = c(h = "daily", s = "daily", sc = "daily")) {
  pc_install_copy(mutate = function(r) {
    tvs <- list(
      toy_dir("toy-60", "toy-h", SENS_60,
              run_yaml = list(split = "origin_agnostic", modes = c("train", "infer"), horizon_days = 9L,
                              backtest = list(protocol_default = proto[["h"]])),
              descriptor = function(d) { d$modes_implemented <- c("train", "infer"); d$stages$forecast <- NULL
                                         d$model_time_offset_min <- 0L; d }),
      toy_dir("toy-30", "toy-s", SENS_30, run_yaml = list(horizon_days = 1L, backtest = list(protocol_default = proto[["s"]])),
              descriptor = list(data_lag_days = 2L, uses_rng = TRUE)),
      toy_dir("toy-60", "toy-sc", SENS_60, run_yaml = list(backtest = list(protocol_default = proto[["sc"]])),
              descriptor = list(model_time_offset_min = 0L)))
    for (tv in tvs) for (n in setdiff(list.files(tv), "_toy-engine"))
      file.copy(file.path(tv, n), file.path(r, "models"), recursive = TRUE)
    file.copy(file.path(tvs[[1L]], "_toy-engine"), file.path(r, "models"), recursive = TRUE)
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [A, B], loss: PX}",
                 "order: [X]", "members:",
                 "  - {sigla: MH, mdir: toy-h, exogena: null}",
                 "  - {sigla: MS, mdir: toy-s, exogena: null}",
                 "  - {sigla: MC, mdir: toy-sc, exogena: null}"),
               file.path(r, "config", "topology.yaml"))
  })
}
ROOT <- mk_root()

DECK <- local({
  d <- tempfile("bt-deck-")
  fx$prevcarga_fixture_deck(d, seed = 5, areas = c("A", "B"), days = seq(BO1 - 30L, BO3, by = "day"))
  d
})

mod <- new.env()
for (m in c("errors", "runtime", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
            "features", "registry", "training", "inference", "build_long_output", "evaluation", "combination", "reconcile", "cache", "orchestrate", "backtest"))
  sys.source(file.path(lib, paste0(m, ".R")), envir = mod)
suppressMessages(mod$prevcarga_runtime_enforce())

bctx <- function(root = ROOT, data = DECK, .frame = parent.frame()) {
  state <- withr::local_tempdir("bt-state-", .local_envir = .frame)
  ctx <- mod$prevcarga_ctx(list("--data" = data, "--state" = state), root, "backtest")
  ctx$code_commit <- "c0ffee"
  ctx$data_hash <- "deck-fixed"                          # chaves iguais entre decks perturbados (so o dado muda)
  ctx$progress <- function(done, total) NULL
  ctx
}

## troca binding do env do modulo e restaura ao fim do teste
mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- mod[[nm]]; withr::defer(assign(nm, old, envir = mod), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = mod)
}
count_trains <- function(.frame = parent.frame()) {
  cnt <- new.env(); cnt$n <- 0L
  orig <- mod$prevcarga_plugin_train
  mock(prevcarga_plugin_train = function(...) { cnt$n <- cnt$n + 1L; orig(...) }, .frame = .frame)
  cnt
}

run_bt <- function(ctx, origins = BO1, models = NULL, areas = "A", cortes = "08:00", ...)
  mod$prevcarga_backtest(ctx, origins = origins, models = models, areas = areas, cortes = cortes, ...)

cell_long <- function(res, membro, origem, corte, folha = "A") {
  p <- res$paths$members
  f <- p$long_parquet[p$membro == membro & p$folha == folha & p$origem == origem & p$corte == corte]
  data.table::as.data.table(arrow::read_parquet(f))
}
artifact_shas <- function(ctx) {
  ms <- Sys.glob(file.path(ctx$state_dir, "cache", "artifact", "*", "*", "meta.json"))
  stats::setNames(vapply(ms, function(f) jsonlite::read_json(f)$sha256, ""), basename(dirname(ms)))
}
perturb <- function(...) fx$prevcarga_fixture_perturb(DECK, channel = "load", areas = "A", ...)

expect_pc <- function(expr, id, exit = 2L) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (!is.null(cond)) { expect_identical(cond$id, id); expect_identical(cond$exit_code, as.integer(exit)) }
  invisible(cond)
}

## ---------------------------------------------------------------------------------------------------------------------
test_that("[AC1] --corte 09:00 trunca a carga ANTES do leitor: available_until 09:00, 1a entrega 09:30", {
  ctx <- bctx()
  seen <- new.env(); seen$cut <- list()
  orig <- mod$prevcarga_read_panel
  mock(prevcarga_read_panel = function(ctx, origin, areas, horizon_days, history_days, truncate_at = NULL) {
    seen$cut[[length(seen$cut) + 1L]] <- truncate_at
    orig(ctx, origin, areas, horizon_days, history_days, truncate_at = truncate_at)
  })
  res <- run_bt(ctx, models = "toy-s", cortes = "09:00")
  expect_identical(res$summary$verdict, "ok")
  trunc <- Filter(Negate(is.null), seen$cut)
  expect_gte(length(trunc), 1L)
  expect_true(all(vapply(trunc, function(t) identical(t, bt_at(BO1, "09:00")), NA)))     # O + corte, fuso da carga
  long <- cell_long(res, "MS", BO1, "09:00")
  expect_identical(min(long$target_datetime), bt_at(BO1, "09:30"))
  expect_identical(lubridate::tz(long$target_datetime), BT_TZ)
  base <- long$y_pred
  ## FORBIDDEN: carga com interval_end > O 09:00 nao muda y_pred
  f <- run_bt(bctx(data = perturb(after = bt_at(BO1, "08:30"))), models = "toy-s", cortes = "09:00")
  expect_equal(cell_long(f, "MS", BO1, "09:00")$y_pred, base)
  ## CONTROL: o intervalo 08:30-09:00 de O muda o membro semi-horario
  c1 <- run_bt(bctx(data = perturb(after = bt_at(BO1, "08:00"), at_or_before = bt_at(BO1, "08:30"))),
               models = "toy-s", cortes = "09:00")
  expect_gt(max(abs(cell_long(c1, "MS", BO1, "09:00")$y_pred - base)), 0)
})

test_that("[AC2] RF horario: delta 0 entre 09:00 e 09:30 (artefato HIT); LGBM semi-horario: delta != 0 (CONTROL)", {
  ctx <- bctx()
  cnt <- count_trains()
  res <- run_bt(ctx, models = "toy-h,toy-s", cortes = "09:00,09:30")
  expect_identical(res$summary$verdict, "ok")
  h0 <- cell_long(res, "MH", BO1, "09:00"); h1 <- cell_long(res, "MH", BO1, "09:30")
  s0 <- cell_long(res, "MS", BO1, "09:00"); s1 <- cell_long(res, "MS", BO1, "09:30")
  expect_identical(min(h0$target_datetime), bt_at(BO1, "09:30"))
  expect_identical(min(h1$target_datetime), bt_at(BO1, "10:00"))
  expect_identical(min(s0$target_datetime), bt_at(BO1, "09:30"))
  expect_identical(min(s1$target_datetime), bt_at(BO1, "10:00"))
  common <- merge(h0, h1, by = "target_datetime")
  expect_gt(nrow(common), 0L)
  expect_identical(common$y_pred.x, common$y_pred.y)                                    # horario ignora a meia hora
  cs <- merge(s0, s1, by = "target_datetime")
  expect_gt(max(abs(cs$y_pred.x - cs$y_pred.y)), 0)                                     # CONTROL semi-horario
  expect_identical(cnt$n, 2L)                    # 1 treino por membro (toy-h e toy-s); o 2o corte e HIT do artefato
  expect_gte(mod$prevcarga_cache_stats(ctx)$hit, 2L)
})

test_that("[AC3] corte de treino por classe (plano, dates.R): RF O-10, LGBM O-2 (last_data_day O-3), HW O; artefatos honestos", {
  ctx <- bctx()
  plan <- mod$prevcarga_plan(ctx, BO1, "08:00", filters = list(area = "A"))
  g <- function(m) plan$cells[plan$cells$membro == m]
  expect_identical(g("MH")$train_origin, BO1 - 10L); expect_identical(g("MH")$last_data_day, BO1 - 1L)
  expect_identical(g("MS")$train_origin, BO1 - 2L);  expect_identical(g("MS")$last_data_day, BO1 - 3L)
  expect_identical(g("MC")$train_origin, BO1);       expect_identical(g("MC")$last_data_day, BO1 - 1L)
  expect_gt(length(unique(plan$cells$train_origin)), 1L)           # FORBIDDEN: corte uniforme
  res <- run_bt(ctx)
  expect_identical(res$summary$verdict, "ok")
  base <- artifact_shas(ctx)
  expect_length(base, 3L)
  ## FORBIDDEN: carga do dia O em diante nao muda nenhum artefato
  f <- bctx(data = perturb(after = bt_at(BO1 - 1L, "23:30")))
  run_bt(f)
  expect_identical(artifact_shas(f), base)
  ## CONTROL: a carga de last_data_day muda os tres artefatos
  cc <- bctx(data = perturb(after = bt_at(BO1 - 4L, "23:30"), at_or_before = bt_at(BO1 - 3L, "23:30")))
  run_bt(cc)
  ch <- artifact_shas(cc)
  expect_identical(names(ch), names(base))
  expect_true(all(ch != base))
})

test_that("[AC4] --protocol fixed treina 1x (corte da menor origem), daily 1x por origem; default do run.yaml; so cache/", {
  origins <- c(BO1, BO2, BO3)
  ctx <- bctx(); cnt <- count_trains()
  res <- run_bt(ctx, origins = origins, models = "toy-s", protocol = "fixed")
  expect_identical(res$summary$verdict, "ok")
  expect_identical(cnt$n, 1L)
  plan <- mod$prevcarga_plan(ctx, origins, "08:00", protocol = "fixed", filters = list(model = "toy-s", area = "A"))
  expect_true(all(plan$cells$train_origin == BO1 - 2L))             # min(origens) - 1 - H, H = 1
  expect_true(all(plan$cells$last_data_day == BO1 - 3L))            # min(origens) - 1 - data_lag_days (2)
  expect_false(dir.exists(file.path(ctx$state_dir, "artifacts")))
  expect_true(dir.exists(file.path(ctx$state_dir, "cache", "artifact")))
  d <- bctx(); cnt2 <- count_trains()
  run_bt(d, origins = origins, models = "toy-s", protocol = "daily")
  expect_identical(cnt2$n, 3L)
  ## sem --protocol: cada membro usa backtest.protocol_default do seu run.yaml
  r2 <- mk_root(c(h = "daily", s = "fixed", sc = "daily"))
  e <- bctx(root = r2); cnt3 <- count_trains()
  run_bt(e, origins = origins, models = "toy-h,toy-s")
  expect_identical(cnt3$n, 1L + 3L)
})

test_that("[AC5] HW + fixed aborta ANTES do MAP (exit 2, nenhum arquivo); CONTROL: retrain_harness roda o forecast acoplado", {
  ctx <- bctx()
  cnt <- count_trains()
  expect_pc(run_bt(ctx, models = "toy-sc", protocol = "fixed"), "contract.protocol_incompatible", 2L)
  expect_false(dir.exists(file.path(ctx$state_dir, "backtest")))
  expect_identical(cnt$n, 0L)
  ## CONTROL
  rh <- run_bt(ctx, models = "toy-sc", protocol = "retrain_harness")
  expect_identical(rh$summary$verdict, "ok")
  expect_identical(cnt$n, 1L)                                          # 1 treino por origem, dentro do forecast do plugin
  d <- bctx()
  dl <- run_bt(d, models = "toy-sc", protocol = "daily")
  expect_identical(cell_long(rh, "MC", BO1, "08:00")$y_pred, cell_long(dl, "MC", BO1, "08:00")$y_pred)   # infer == forecast
  expect_false(dir.exists(file.path(ctx$state_dir, "cache", "artifact")))   # o harness nao usa artefato de cache
})

test_that("[AC6] LONG da celula: 8 colunas, forecast_origin texto, nada <= available_until, .key = cache_key do plano", {
  ctx <- bctx()
  res <- run_bt(ctx, models = "toy-s", cortes = "09:30")
  long <- cell_long(res, "MS", BO1, "09:30")
  expect_identical(names(long), names(mod$prevcarga_schema_long))
  expect_silent(mod$prevcarga_schema_long_check(long))
  expect_true(is.character(long$forecast_origin))
  expect_identical(unique(long$forecast_origin), format(BO1))
  expect_true(all(long$target_datetime > bt_at(BO1, "09:30")))
  expect_false(anyNA(long$y_pred))
  expect_true(all(is.na(long$y_true)))
  plan <- mod$prevcarga_plan(ctx, BO1, "09:30", filters = list(model = "toy-s", area = "A"))
  kf <- file.path(dirname(res$paths$members$long_parquet[[1L]]), ".key")
  expect_identical(readLines(kf), plan$cells$cache_key)
  ## CONTROL: y_pred NA no meio da grade aborta com contract (nunca preenche com 0)
  cell <- plan$cells[1L]
  au <- bt_at(BO1, "09:30")
  ie <- seq(au + lubridate::minutes(30L), bt_at(BO1 + 2L, "00:00"), by = "30 min")
  yp <- as.double(seq_along(ie)); yp[10L] <- NA_real_
  inf <- list(delivery = data.table::data.table(interval_end = ie, y_pred = yp), available_until = au,
              descriptor = list(delivery_days = 2L))
  e <- expect_pc(mod$.bt_member_long(plan, cell, inf), "contract.backtest_long_na", 2L)
  expect_match(conditionMessage(e), "^\\[prevcarga/contract\\]")
  ## FORBIDDEN: forecast_origin como Date reprova a conferencia de tipos do LONG
  inf$delivery$y_pred <- as.double(seq_along(ie))
  ok <- mod$.bt_member_long(plan, cell, inf)
  expect_true(is.character(ok$forecast_origin))
  bad <- data.table::copy(ok); bad[, forecast_origin := lubridate::as_date(forecast_origin)]
  expect_pc(mod$.bt_check_types(bad), "contract.long_schema", 2L)
})

test_that("[AC7] pureza do MAP: sem lista de modelos, derivador de corte, solver, ambiente, relogio, rede, registry", {
  src <- paste(readLines(file.path(lib, "backtest.R")), collapse = "\n")
  code <- paste(sub("^\\s*##.*$", "", strsplit(src, "\n", fixed = TRUE)[[1L]]), collapse = "\n")
  bad <- c('c\\("holt-winters"', "refit_per_origin", "predict\\(", "\\blm\\(", "ranger\\(", "lgb\\.", "prevcarga_available_until\\(",
           "Sys\\.getenv", "Sys\\.time", "mclapply", "system\\(", "s3://", "registry_promote", "registry_resolve_latest",
           "latest\\.txt", "resolve_version", "registry_publish", "\\bquit\\(")
  for (p in bad) expect_false(grepl(p, code), info = p)
  ## CONTROL: o grep enxerga o que procura
  expect_true(grepl("predict\\(", "x <- predict(m)"))
  expect_true(grepl("\\.m7_class\\(", code))                       # a classe vem so de .m7_class()
  expect_false(grepl("[$]train_origin\\s*<-|[$]last_data_day\\s*<-|\\blast_data_day\\s*<-", code))   # nenhum corte de treino calculado aqui
})
