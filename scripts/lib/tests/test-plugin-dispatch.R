## test-plugin-dispatch.R -- despachantes prevcarga_plugin_train/_infer/_forecast (P.0c): conferencia de paineis e
## cortes, RNG/globalenv isolados, deslocamento de modelagem centralizado e envelope na grade da entrega.
## Cortes literais (pc_toy_cuts); a fiacao com o leitor real e o honesty_gate sao do P.0i.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("runtime", "errors", "provenance", "topology", "deck_reader", "validation", "dates", "registry",
                "features"), env = environment())
prevcarga_registry_reset(); prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
au_at <- function(hm) lubridate::ymd_hm(paste("2026-10-01", hm), tz = TZ)
cuts_for <- function(split = "origin_agnostic", hm = "09:00") pc_toy_cuts(O, au_at(hm), split = split)
FULL <- local({ p <- pc_sentinel_panel(O - 12L, 13L, "SE"); p$attrs$origin <- O; p })
rs <- function() get(".Random.seed", envir = globalenv())
fmt <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = TZ)

expect_cond <- function(expr, id, class = "contract", text = NULL) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (is.null(cond)) return(invisible(NULL))
  expect_identical(cond$id, id); expect_identical(cond$class_name, class)
  expect_identical(cond$exit_code, unname(prevcarga_exit_codes[[class]]))
  expect_match(conditionMessage(cond), sprintf("[prevcarga/%s]", class), fixed = TRUE)
  for (t in text) expect_true(grepl(t, conditionMessage(cond), fixed = TRUE), info = t)
  invisible(cond)
}
p30 <- function(...) pc_toy_disp("toy-30", descriptor = list(uses_rng = TRUE), ...)
train <- function(p, panel, cuts, spec = p$spec) prevcarga_plugin_train(p, panel, cuts, spec, list())

test_that("ac-0 treino alem do corte e data_lag_days com CONTROL; o painel nunca e cortado", {
  cuts <- cuts_for()
  p <- p30()
  tp <- pc_train_slice(FULL, cuts)
  bad <- pc_panel_with(tp, "day_O")
  before <- lapply(bad[c("load", "weather_observed", "weather_forecast")], data.table::copy)
  expect_cond(train(p, bad, cuts), "plugin.train_beyond_cut", text = c("2026-10-01", "2026-09-30"))
  expect_equal(lapply(bad[names(before)], identity), before)
  ## teto por plugin: data_lag_days = 2 => last_data_day = O - 3 e o intervalo de 09-29 e recusado
  p2 <- pc_toy_lag(2L)
  c2 <- cuts; c2$last_data_day <- O - 3L
  expect_cond(train(p2, pc_train_slice(FULL, cuts), c2), "plugin.train_beyond_cut", text = c("2026-09-29", "2026-09-28"))
  expect_s3_class(train(p2, pc_train_slice(FULL, c2), c2), "prevcarga_plugin_artifact")
  ## cuts incoerentes com o descriptor: o corte uniforme O-1 num plugin de lag 2 e recusado
  expect_cond(train(p2, tp, cuts), "plugin.cuts_mismatch", text = "esperado=2026-09-28")
  c3 <- cuts; c3$last_data_day <- O - 2L
  expect_cond(train(p, tp, c3), "plugin.cuts_mismatch")
  ## CONTROL: o painel correto treina e devolve o artefato carimbado
  art <- train(p, tp, cuts)
  expect_s3_class(art, "prevcarga_plugin_artifact")
  expect_identical(art$stamp$model_id, "toy-30")
  expect_identical(art$stamp$train_origin, O - 2L)
})

test_that("ac-1 quatro recusas do infer sem chamar estagio; CONTROL y_pred finito", {
  cuts <- cuts_for()
  p <- p30()
  art <- train(p, pc_train_slice(FULL, cuts), cuts)
  ip <- pc_infer_slice(FULL, cuts)
  sp <- pc_spy(p, "ingestion")
  inf <- function(panel) prevcarga_plugin_infer(sp, art, panel, cuts, list())
  expect_cond(inf(pc_panel_with(ip, "end_0930")), "plugin.infer_beyond_cut", text = "09:30:00")
  expect_cond(inf(pc_panel_with(ip, "late_issue")), "plugin.weather_beyond_limit", text = "13:00:00")
  expect_cond(inf(pc_panel_with(ip, "no_load_mwh")), "plugin.missing_column", text = "load_mwh")
  expect_cond(inf(pc_panel_with(ip, "timestamp")), "plugin.raw_timestamp", text = "timestamp")
  lbl <- ip; lbl$load <- data.table::copy(ip$load)[, load_label := "end"]
  expect_cond(inf(lbl), "plugin.raw_timestamp", text = "load_label")
  expect_identical(sp$spy$calls, 0L)
  env <- inf(ip)
  expect_true(all(is.finite(env$delivery$y_pred)))
  expect_identical(sp$spy$calls, 1L)
})

test_that("ac-2 RNG do chamador e globalenv intactos; seed do run.yaml tem dente; toy-leaky e apanhado", {
  cuts <- cuts_for()
  p <- p30()
  tp <- pc_train_slice(FULL, cuts); ip <- pc_infer_slice(FULL, cuts)
  set.seed(7); before <- rs(); snap <- pc_globalenv_snapshot()
  a1 <- train(p, tp, cuts)
  expect_identical(rs(), before)
  prevcarga_plugin_infer(p, a1, ip, cuts, list())
  expect_identical(rs(), before)
  expect_identical(pc_globalenv_snapshot(), snap)
  ## o estagio aborta depois de consumir o gerador: o estado do chamador volta igual
  boom <- p30(train_extra = c("set.seed(seed)", "stop('boom')"))
  expect_error(train(boom, tp, cuts), "boom")
  expect_identical(rs(), before)
  ## CONTROL: seed 1 vs 2 no run.yaml muda a saida
  a2 <- train(p, tp, cuts, modifyList(p$spec, list(seed = 2L)))
  expect_gt(max(abs(unlist(a1$payload) - unlist(a2$payload))), 1e-5)
  expect_identical(rs(), before)
  ## CONTROL: toy-leaky planta `x` no globalenv; o snapshot acusa e remove
  expect_cond(train(pc_toy_leaky(), tp, cuts), "plugin.global_side_effect", text = "x")
  expect_false(exists("x", envir = globalenv(), inherits = FALSE))
  expect_identical(pc_globalenv_snapshot(), snap)
  expect_identical(rs(), before)
  ## uses_rng = FALSE e consumo do gerador => plugin.rng
  nor <- pc_toy_disp("toy-30", descriptor = list(uses_rng = FALSE))
  expect_cond(train(nor, tp, cuts), "plugin.rng", class = "repro")
})

test_that("ac-3 envelope na grade de entrega (30 min x 2/9 dias) e cinco variantes desalinhadas", {
  fields <- c("model_id", "variant", "area", "origin_day", "available_until", "weather_issue_cutoff", "layout",
              "granularity_min", "values", "delivery", "meta")
  expect_identical(prevcarga_envelope_fields, fields)
  for (hm in c("09:00", "09:30")) {
    cuts <- cuts_for(hm = hm); p <- p30()
    art <- train(p, pc_train_slice(FULL, cuts), cuts)
    env <- prevcarga_plugin_infer(p, art, pc_infer_slice(FULL, cuts), cuts, list())
    expect_identical(names(env), fields)
    expect_identical(env$delivery$interval_end, prevcarga_delivery_grid(cuts, 2L))
    expect_identical(fmt(env$delivery$interval_end[[1L]]), if (hm == "09:00") "2026-10-01 09:30:00" else "2026-10-01 10:00:00")
    expect_identical(fmt(env$delivery$interval_end[[length(env$delivery$interval_end)]]), "2026-10-03 00:00:00")
    expect_identical(lubridate::tz(env$delivery$interval_end), TZ)
    expect_identical(env$layout, "lgbm_day_48"); expect_identical(env$granularity_min, 30L)
    expect_identical(env$available_until, cuts$available_until)
    expect_false(any(c("forecast_origin", "lead", "d_plus", "y_true") %in% names(env)))
    c6 <- cuts_for("origin_scoped", hm); p6 <- pc_toy_offset(60L)
    a6 <- train(p6, pc_train_slice(FULL, c6), c6)
    e6 <- prevcarga_plugin_infer(p6, a6, pc_infer_slice(FULL, c6), c6, list())
    expect_identical(e6$delivery$interval_end, prevcarga_delivery_grid(c6, 9L))
    expect_identical(fmt(e6$delivery$interval_end[[length(e6$delivery$interval_end)]]), "2026-10-10 00:00:00")
  }
  cuts <- cuts_for(); art <- train(p30(), pc_train_slice(FULL, cuts), cuts); ip <- pc_infer_slice(FULL, cuts)
  want <- list(from_0900 = "09:00:00", short_day = "ausente", na = "y_pred", dup = "10:00:00", lead_field = "lead")
  for (k in names(want))
    expect_cond(prevcarga_plugin_infer(pc_toy_misaligned(k), art, ip, cuts, list()), "plugin.output_misaligned",
                text = want[[k]])
})

test_that("ac-4 infer sem params: ambiente sujo identical ao limpo, zero chamadas de training, re-treino k=5 tem dente", {
  cuts <- cuts_for()
  p <- p30()
  tp <- pc_train_slice(FULL, cuts); ip <- pc_infer_slice(FULL, cuts)
  art3 <- train(p, tp, cuts)
  p5 <- p; p5$spec <- modifyList(p$spec, list(params = list(k = 5L)))
  sp <- pc_spy(pc_spy(p5, "inference_features"), "training")
  clean <- prevcarga_plugin_infer(p5, art3, ip, cuts, list())
  dirty <- pc_with_env(c(EXOGENA = "MAX", TOY_PARAM = "99"), prevcarga_plugin_infer(p5, art3, ip, cuts, list()))
  expect_identical(dirty, clean)
  expect_identical(clean, prevcarga_plugin_infer(p, art3, ip, cuts, list()))
  expect_identical(art3$stamp$params$k, 3L)
  sp_tr <- pc_spy(p5, "training")
  prevcarga_plugin_infer(sp_tr, art3, ip, cuts, list())
  expect_identical(sp_tr$spy$calls, 0L)
  sp_if <- pc_spy(p5, "inference_features")
  prevcarga_plugin_infer(sp_if, art3, ip, cuts, list())
  expect_length(sp_if$spy$args[[1L]], 2L)
  ## CONTROL: re-treinar com k = 5 muda y_pred
  art5 <- train(p5, tp, cuts)
  e5 <- prevcarga_plugin_infer(p5, art5, ip, cuts, list())
  expect_gt(max(abs(e5$delivery$y_pred - clean$delivery$y_pred)), 1e-5)
})

test_that("ac-5 forecast == train + infer (origin_scoped), rng_deviation em meta, mode_unavailable, outra origem", {
  cuts <- cuts_for("origin_scoped")
  tp <- pc_train_slice(FULL, cuts)
  p <- pc_toy_offset(60L)
  e1 <- prevcarga_plugin_infer(p, train(p, tp, cuts), tp, cuts, list())
  e2 <- prevcarga_plugin_forecast(p, tp, cuts, p$spec, list())
  expect_identical(e2, e1)
  expect_null(e2$meta$rng_deviation)
  pd <- pc_toy_disp("toy-60", descriptor = list(model_time_offset_min = 60L, uses_rng = TRUE,
                                                rng_deviation = "plano B de teste"), native_min = 60L)
  ed <- prevcarga_plugin_forecast(pd, tp, cuts, pd$spec, list())
  expect_identical(ed$meta$rng_deviation, "plano B de teste")
  expect_identical(ed, prevcarga_plugin_infer(pd, train(pd, tp, cuts), tp, cuts, list()))
  c30 <- cuts_for()
  expect_cond(prevcarga_plugin_forecast(p30(), pc_train_slice(FULL, c30), c30, p30()$spec, list()),
              "plugin.mode_unavailable", text = "forecast")
  ## CONTROL: artefato de outra origem e recusado no infer
  other <- pc_toy_cuts(O + 1L, au_at("09:00") + lubridate::days(1L), split = "origin_scoped")
  expect_cond(prevcarga_plugin_infer(p, train(p, tp, cuts), tp, other, list()), "plugin.artifact_mismatch",
              text = "origin_day")
})

test_that("ac-6 deslocamento de modelagem centralizado: spy com offset 60 vs 0, entrega de volta, CONTROL e auto-deslocamento", {
  cuts <- cuts_for("origin_scoped")
  tp <- pc_train_slice(FULL, cuts); ip <- pc_infer_slice(FULL, cuts)
  keep <- data.table::copy(tp$load); keep_i <- data.table::copy(ip$load)
  m <- lubridate::minutes(60L)
  p60 <- pc_spy(pc_toy_offset(60L), "ingestion"); p0 <- pc_spy(pc_toy_offset(0L), "ingestion")
  run <- function(p, tpan = tp) {
    a <- train(p, tpan, cuts)
    prevcarga_plugin_infer(p, a, ip, cuts, list())
  }
  e60 <- run(p60); e0 <- run(p0)
  for (k in 1:2) {   # 1 = treino, 2 = infer
    src <- if (k == 1L) tp else ip
    s <- p60$spy$args[[k]]
    expect_identical(s[[1L]]$load$interval_start, src$load$interval_start - m)
    expect_identical(s[[1L]]$load$interval_end, src$load$interval_end - m)
    expect_identical(s[[2L]]$available_until, cuts$available_until - m)
    expect_identical(s[[1L]]$weather_observed, src$weather_observed)
    expect_identical(s[[1L]]$weather_forecast, src$weather_forecast)
    expect_identical(s[[1L]]$holidays, src$holidays)
    expect_identical(p0$spy$args[[k]][[1L]], src)
    expect_identical(p0$spy$args[[k]][[2L]], cuts)
  }
  expect_equal(tp$load, keep); expect_equal(ip$load, keep_i)   # sem mutacao por referencia
  grid <- prevcarga_delivery_grid(cuts, 9L)
  for (e in list(e60, e0)) {
    expect_identical(e$delivery$interval_end, grid)
    expect_identical(fmt(e$delivery$interval_end[[1L]]), "2026-10-01 09:30:00")
    expect_identical(fmt(e$delivery$interval_end[[length(grid)]]), "2026-10-10 00:00:00")
  }
  ## CONTROL: offset 0 vs 60 no mesmo brinquedo muda y_pred
  expect_gt(max(abs(e60$delivery$y_pred - e0$delivery$y_pred)), 1e-5)
  ## CONTROL: plugin que desloca por conta propria alem do despachante sai da grade
  ps <- pc_toy_offset(60L, self_shift = 60L)
  expect_cond(prevcarga_plugin_infer(ps, train(ps, tp, cuts), ip, cuts, list()), "plugin.output_misaligned")
})
