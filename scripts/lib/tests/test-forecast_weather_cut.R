## test-forecast_weather_cut.R -- clima previsto incompleto corta o horizonte, sem persistir dia entregue (F3.7).
## Leitor (weather_forecast_until/next pelo predicado unico de validation.R) -> despachante (.prevcarga_inf_effective_days /
## .prevcarga_inf_apply_weather_cut) -> forecast (aviso unico por folha, membro ausente, delivery_days efetivo na combina).
## Nenhum membro real treina ou infere: os membros sao STUBS de brinquedo (valores fixos na grade de cuts$delivery_days) com os
## descriptors de dependencia copiados dos reais. Fixture m6f (SS1 = A1,A2; SS2 = B1); tudo em tempdir; sem variaveis de ambiente.
## Cada FORBIDDEN (clima completo ate O+9) tem o seu CONTROL (clima so ate O+3).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("fwc-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)

at <- function(x) lubridate::ymd_hm(x, tz = M6_TZ)
day_start <- function(d) lubridate::force_tz(lubridate::as_datetime(d), M6_TZ)

## deck com o previsto truncado em `until` (digitos civis do store, como o leitor os le): a copia do deck do fixture
deck_truncated <- function(until, .frame = parent.frame()) {
  d <- withr::local_tempdir("fwc-deck-", .local_envir = .frame)
  file.copy(list.files(DECK, full.names = TRUE), d, recursive = TRUE)
  keep <- lubridate::force_tz(until, "UTC")
  for (f in list.files(file.path(d, "weather", "forecast"), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)) {
    tb <- arrow::read_parquet(f, as_data_frame = FALSE)
    arrow::write_parquet(tb[tb$valid_time$as_vector() <= keep, ], f)
  }
  d
}
fx_ctx <- function(deck, .frame = parent.frame()) {
  st <- withr::local_tempdir("fwc-state-", .local_envir = .frame)
  file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
  m6f_ctx(deck, st, TOPO)
}
mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- env[[nm]]; withr::defer(assign(nm, old, envir = env), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
}

## --- membros-stub: descriptors de dependencia dos reais (clima/offset/weather_cut/delivery_days) -------------------------
stub_plugin <- function(delivery_days, offset = 0L, weather = TRUE, weather_cut = "truncate")
  list(descriptor = list(delivery_days = as.integer(delivery_days), model_time_offset_min = as.integer(offset),
                         weather_cut = weather_cut,
                         stages = list(inference = list(fn = identity,
                                                        requires = if (weather) "data.weather_forecast.temperature" else "data.load.load_mwh",
                                                        provides = "member_envelope"))))
## O fixture m6f tem 3 membros treinados (pesos da combina com 9/2/9 dias): as siglas viram os stubs de dependencia que
## precisam de 9 dias -- RF = stub-dessem (offset 60, weather_cut absent: DESSEM so com horizonte inteiro), LGBM = membro que corta (truncate), HW = stub-hw (sem clima).
## O stub-lgbm (2 dias, truncate) so entra nas unidades do despachante (STUB_LGBM): nao e membro do fixture.
STUB <- list(RF = stub_plugin(9L, offset = 60L, weather_cut = "absent"), LGBM = stub_plugin(9L), HW = stub_plugin(9L, weather = FALSE))
STUB_DESSEM <- STUB$RF
STUB_RF <- stub_plugin(9L, weather_cut = "absent")
STUB_TRUNC <- STUB$LGBM
STUB_LGBM <- stub_plugin(2L)

## Substitui prevcarga_infer_cell: leitor REAL + corte REAL do despachante; o "estagio" do stub so devolve valores fixos na
## grade de cuts$delivery_days. log$stage conta as chamadas ao estagio (o RF ausente nunca chega a ele).
stub_infer <- function(log) {
  function(member, area, origin, ctx, variant = NULL, version = NULL, pin = FALSE) {
    sg <- as.character(member$sigla)
    cuts <- env$prevcarga_resolve_cuts(ctx, origin = origin)
    panel <- env$prevcarga_read_panel(ctx, origin, area, 9L, 5L)
    cuts <- env$.prevcarga_inf_apply_weather_cut(STUB[[sg]], member$mdir, area, origin, panel$attrs, cuts)
    log$stage[[length(log$stage) + 1L]] <- paste0(sg, "/", area)
    d <- m6_fixture_delivery(m6_member(sg, cuts$delivery_days), origin, cuts$available_until, cuts$tz)
    list(predictions = data.table::data.table(target_datetime = d$interval_end, y_pred = d$y_pred + 10 * match(sg, M6F_MEMBERS)),
         version = paste0("v_", substr(digest::digest(paste(sg, area), algo = "sha256"), 1L, 16L)),
         available_until_posix = cuts$available_until, delivery_days_efetivo = cuts$delivery_days,
         weather_forecast_until = env$.prevcarga_inf_covered_until(panel$attrs, area))
  }
}
run_fc <- function(ctx, .frame = parent.frame(), ...) {
  log <- new.env(); log$stage <- list(); log$train <- list()
  mock(prevcarga_infer_cell = stub_infer(log), prevcarga_train = m6f_train_spy(env, log), .frame = .frame)
  r <- env$prevcarga_forecast(ctx, skip_validate = TRUE, ...)
  r$log <- log
  r
}
yp <- function(r, f) arrow::read_parquet(file.path(r$result$saida, "folhas", paste0(f, ".parquet")))
ym <- function(r, sg, f) arrow::read_parquet(file.path(r$result$saida, "membros", sg, paste0(f, ".parquet")))
run_json <- function(ctx) jsonlite::read_json(list.files(file.path(ctx$state_dir, "scratch"), pattern = "^run\\.json$", recursive = TRUE, full.names = TRUE)[[1L]])
cut_warns <- function(r) Filter(function(x) identical(x$id, "warning.forecast_weather_horizon_cut"), r$warnings)

## --- FORBIDDEN: clima completo ate O+9 -----------------------------------------------------------------------------
test_that("[FORBIDDEN] clima completo ate O+9 23:30: sem aviso, delivery_days = descriptor, clima_previsto_ate = O+9", {
  ctx <- fx_ctx(DECK)
  r <- run_fc(ctx, escopo = "area", areas = "A1", incluir_membros = TRUE)
  expect_length(cut_warns(r), 0L)
  expect_length(r$result$membros_ausentes, 0L)
  expect_identical(r$result$clima_previsto_ate, format(O + 9L))
  expect_identical(r$result$delivery_days_efetivo$A1[c("RF", "LGBM", "HW")], list(RF = 9L, LGBM = 9L, HW = 9L))
  cuts <- env$prevcarga_resolve_cuts(ctx, origin = O)
  for (sg in names(STUB))
    expect_identical(ym(r, sg, "A1")$target_datetime,
                     env$prevcarga_delivery_grid(cuts, STUB[[sg]]$descriptor$delivery_days), info = sg)
  expect_identical(run_json(ctx)$clima_previsto_ate, format(O + 9L))
  ## o corte efetivo (leitor + despachante) = o do descriptor para o stub tipo DESSEM, com o instante O+10 00:00 presente
  pa <- env$prevcarga_read_panel(ctx, O, "A1", 9L, 5L)$attrs
  expect_identical(env$.prevcarga_inf_effective_days(STUB_DESSEM, pa, "A1", O), 9L)
})

## --- CONTROL: clima so ate O+3 (mais os dois 1os instantes de O+4, que o offset de 60 min exige) ----------------------------
test_that("[CONTROL] clima ate O+3: aviso com a data, DESSEM ausente (recusa catalogada), membro que corta entrega ate O+3, hw inteiro", {
  deck <- deck_truncated(at("2026-01-19 00:30"))                 # O+4 00:00 e 00:30 presentes: dia O+4 NAO coberto
  ctx <- fx_ctx(deck)
  pa <- env$prevcarga_read_panel(ctx, O, "A1", 9L, 5L)$attrs
  expect_identical(pa$weather_forecast_until[area_code == "A1"]$until, O + 3L)
  expect_identical(pa$weather_forecast_next[area_code == "A1"]$n_next, 2L)
  cuts <- env$prevcarga_resolve_cuts(ctx, origin = O)
  ## DESSEM (offset 60, weather_cut absent) recusa antes de qualquer estagio
  e <- tryCatch(env$.prevcarga_inf_apply_weather_cut(STUB_DESSEM, "dessem-linear", "A1", O, pa, cuts), prevcarga_error = function(e) e)
  expect_identical(e$id, "completude.weather_forecast_short")
  ## membro que corta (truncate, sem offset): entrega ate o fim de O+3 e o aviso traz a data coberta
  got <- withCallingHandlers(env$.prevcarga_inf_apply_weather_cut(STUB_TRUNC, "lgbm", "A1", O, pa, cuts),
                             prevcarga_warning = function(w) invokeRestart("muffleWarning"))
  expect_identical(got$delivery_days, 4L)
  expect_identical(max(env$prevcarga_delivery_grid(got, got$delivery_days)), at("2026-01-19 00:00"))
  w <- tryCatch(env$.prevcarga_inf_apply_weather_cut(STUB_TRUNC, "lgbm", "A1", O, pa, cuts), prevcarga_warning = function(w) w)
  expect_identical(w$id, "warning.forecast_weather_horizon_cut")
  expect_identical(w$details$ate, format(O + 3L))

  r <- run_fc(ctx, escopo = "area", areas = "A1", incluir_membros = TRUE)
  cw <- cut_warns(r)
  expect_length(cw, 1L)                                           # um aviso por folha, com os membros cortados
  expect_identical(cw[[1L]]$details$ate, format(O + 3L))
  expect_identical(cw[[1L]]$details$membros, list("lgbm"))
  expect_match(cw[[1L]]$message, paste0("clima previsto ate ", format(O + 3L), "; a inferencia sera feita ate essa data"), fixed = TRUE)
  expect_identical(r$result$clima_previsto_ate, format(O + 3L))
  expect_identical(run_json(ctx)$clima_previsto_ate, format(O + 3L))
  ## DESSEM-like (sigla RF) ausente com a recusa catalogada; o estagio nunca e chamado; nenhuma linha dele
  expect_false("RF/A1" %in% unlist(r$log$stage))
  ab <- Filter(function(x) identical(x$id, "warning.forecast_member_absent"), r$warnings)
  expect_length(ab, 1L)
  expect_identical(ab[[1L]]$details, list(folha = "A1", membro = "RF", error_id = "completude.weather_forecast_short"))
  expect_false("RF" %in% list.files(file.path(r$result$saida, "membros")))
  ## o membro que corta entrega ate O+4 00:00 e o hw o horizonte inteiro
  cuts0 <- env$prevcarga_resolve_cuts(ctx, origin = O)
  lg <- ym(r, "LGBM", "A1")
  expect_identical(max(lg$target_datetime), at("2026-01-19 00:00"))
  expect_false(any(lg$target_datetime > day_start(O + 4L)))
  expect_identical(ym(r, "HW", "A1")$target_datetime, env$prevcarga_delivery_grid(cuts0, 9L))
  expect_identical(r$result$delivery_days_efetivo$A1[c("LGBM", "HW")], list(LGBM = 4L, HW = 9L))
  ## de O+4 em diante a combinacao usa so o hw
  y <- yp(r, "A1")
  late <- y[y$target_datetime > day_start(O + 4L), ]
  hw <- ym(r, "HW", "A1")
  expect_gt(nrow(late), 0L)
  m <- merge(data.frame(t = late$target_datetime, y = late$y_pred), data.frame(t = hw$target_datetime, h = hw$y_pred), by = "t")
  expect_equal(m$y, m$h, tolerance = 1e-9)
})

## --- AC5/AC6: horizonte efetivo (unidade, plugin-lista) ---------------------------------------------------------------
test_that("[UNIT] .prevcarga_inf_effective_days: offset 60 sem O+4 00:00 => 3; com => 4; sem requires de clima nunca corta", {
  eff <- env$.prevcarga_inf_effective_days
  at3 <- function(n_next) list(weather_forecast_until = data.table::data.table(area_code = "A1", until = O + 3L),
                               weather_forecast_next = data.table::data.table(area_code = "A1", n_next = as.integer(n_next)))
  expect_identical(eff(STUB_DESSEM, at3(0L), "A1", O), 3L)
  expect_identical(eff(STUB_DESSEM, at3(1L), "A1", O), 3L)       # offset 60 = 2 instantes de 30 min
  expect_identical(eff(STUB_DESSEM, at3(2L), "A1", O), 4L)
  expect_identical(eff(stub_plugin(9L), at3(0L), "A1", O), 4L)    # sem offset: nao precisa do dia seguinte
  ## cobertura alem do declarado: sem desconto do offset, teto = delivery_days
  full <- list(weather_forecast_until = data.table::data.table(area_code = "A1", until = O + 9L),
               weather_forecast_next = data.table::data.table(area_code = "A1", n_next = 0L))
  expect_identical(eff(STUB_DESSEM, full, "A1", O), 9L)
  ## sem requires de clima: nunca cortado, qualquer cobertura (inclusive nenhuma)
  none <- list(weather_forecast_until = data.table::data.table(area_code = "A1", until = lubridate::as_date(NA)),
               weather_forecast_next = data.table::data.table(area_code = "A1", n_next = 0L))
  expect_identical(eff(stub_plugin(9L, weather = FALSE), none, "A1", O), 9L)
  expect_identical(eff(stub_plugin(9L, weather = FALSE), at3(0L), "A1", O), 9L)
  ## CONTROL: com requires de clima e sem dia O coberto => 0
  expect_identical(eff(stub_plugin(9L), none, "A1", O), 0L)
  ## stub-lgbm (2 dias) com clima ate O+3: entrega inteiro, sem aviso
  expect_identical(eff(STUB_LGBM, at3(0L), "A1", O), 2L)
  ## membro sem requires de clima: nenhum aviso de corte e cuts$delivery_days = descriptor
  cuts <- env$prevcarga_resolve_cuts(fx_ctx(DECK), origin = O)
  got <- env$.prevcarga_inf_apply_weather_cut(stub_plugin(9L, weather = FALSE), "stub-hw", "A1", O, at3(0L), cuts)
  expect_identical(got$delivery_days, 9L)
})

test_that("[UNIT] membro weather_cut absent aborta completude.weather_forecast_short; sem dia O coberto, tambem o truncate", {
  cuts <- env$prevcarga_resolve_cuts(fx_ctx(DECK), origin = O)
  at3 <- list(weather_forecast_until = data.table::data.table(area_code = "A1", until = O + 3L),
              weather_forecast_next = data.table::data.table(area_code = "A1", n_next = 2L))
  e <- tryCatch(env$.prevcarga_inf_apply_weather_cut(STUB_RF, "random-forest", "A1", O, at3, cuts), prevcarga_error = function(e) e)
  expect_identical(e$id, "completude.weather_forecast_short")
  none <- list(weather_forecast_until = data.table::data.table(area_code = "A1", until = lubridate::as_date(NA)),
               weather_forecast_next = data.table::data.table(area_code = "A1", n_next = 0L))
  e2 <- tryCatch(env$.prevcarga_inf_apply_weather_cut(STUB$LGBM, "lgbm", "A1", O, none, cuts), prevcarga_error = function(e) e)
  expect_identical(e2$id, "completude.weather_forecast_short")
  ## CONTROL: o mesmo RF com clima ate O+9 nao aborta
  full <- list(weather_forecast_until = data.table::data.table(area_code = "A1", until = O + 9L),
               weather_forecast_next = data.table::data.table(area_code = "A1", n_next = 0L))
  expect_identical(env$.prevcarga_inf_apply_weather_cut(STUB_RF, "random-forest", "A1", O, full, cuts)$delivery_days, 9L)
})

test_that("[UNIT] RF e DESSEM reais declaram weather_cut = absent (lido sem treinar); os demais, truncate", {
  desc <- function(m) env$prevcarga_plugin_descriptor(m, env$prevcarga_models_root(pc_root()))
  expect_identical(desc("random-forest")$weather_cut, "absent")
  expect_identical(desc("dessem-linear")$weather_cut, "absent")      # DESSEM so com horizonte inteiro (decisao do Gabriel)
  expect_identical(desc("dessem-regdin")$weather_cut, "absent")
  expect_identical(desc("lgbm")$weather_cut, "truncate")
  expect_identical(desc("holt-winters")$weather_cut, "truncate")
  ## HW univariado nao le clima: nenhum estagio com requires data.weather_forecast.
  expect_false(env$.prevcarga_inf_weather_dependent(list(descriptor = desc("holt-winters"))))
  expect_true(env$.prevcarga_inf_weather_dependent(list(descriptor = desc("random-forest"))))
  expect_true(env$.prevcarga_inf_weather_dependent(list(descriptor = desc("lgbm"))))
})

test_that("[UNIT] leitor: cobertura exige os 48 instantes (sem tolerancia ao 23:30) e n_next conta de 00:00 do dia seguinte", {
  fcov <- env$.prevcarga_weather_forecast_cover
  vt <- function(from, n) from + 1800 * (seq_len(n) - 1L)
  mk <- function(vts) data.table::data.table(area_code = "A1", valid_time = do.call(c, vts))
  d0 <- day_start(O)
  full3 <- vt(d0, 48L * 4L)                                       # O..O+3 completos
  r <- fcov(mk(list(full3)), O, M6_TZ)
  expect_identical(r$weather_forecast_until$until, O + 3L); expect_identical(r$weather_forecast_next$n_next, 0L)
  r2 <- fcov(mk(list(c(full3, vt(day_start(O + 4L), 3L)))), O, M6_TZ)
  expect_identical(r2$weather_forecast_next$n_next, 3L)
  ## falta o 23:30 do O+3 => O+2 (a regra nao abre excecao para o 23:30)
  r3 <- fcov(mk(list(full3[-length(full3)])), O, M6_TZ)
  expect_identical(r3$weather_forecast_until$until, O + 2L)
  ## dia O incompleto => NA
  r4 <- fcov(mk(list(full3[-5L])), O, M6_TZ)
  expect_true(is.na(r4$weather_forecast_until$until)); expect_identical(r4$weather_forecast_next$n_next, 0L)
})
