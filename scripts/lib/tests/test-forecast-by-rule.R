## test-forecast-by-rule.R -- prevcarga_read_forecast_by_rule (E2-T2): as tres regras do previsto (freshest, per_origin,
## min_lead) contra a escolha do proprio plugin sobre TODAS as emissoes, honestidade (nada > limit), segmento vazio com o
## schema do store, espioes por membro no treino e na inferencia, declaracoes reais normalizadas. Deck sintetico pequeno
## (helper-deck.R, 4 emissoes por dia) e fixture_store.R; tudo em tempdir; sem variaveis de ambiente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "holiday_islands", "deck_reader", "validation", "dates",
                       "registry", "features", "inference_needs", "training", "inference"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
MODELS <- file.path(pc_root(), "models")
## limite = O 00:00 (fuso da carga) menos 6h: o atraso de publicacao de 6h do store real
LIM <- lubridate::force_tz(lubridate::as_datetime(O), TZ) - lubridate::hours(6)
seg <- function(kind, from, to, lead = NA_integer_) list(kind = kind, from = as.integer(from), to = as.integer(to),
                                                         lead = as.integer(lead))
plain <- function(x) data.frame(as.list(x), check.names = FALSE)     # so colunas: sem key nem atributos da tabela
by_rule <- function(ctx, area, segs, lim = LIM) env$prevcarga_read_forecast_by_rule(ctx, area, O, segs, lim)
radix <- function(x) x[order(area_code, issue_time, valid_time, method = "radix")]

## deck: emissoes 00Z/06Z/12Z/18Z de O-6 a O+2; as primeiras emissoes <= LIM nao cobrem origens mais antigas que O-5
DECK <- pc_deck_sentinel(days = seq(O - 6L, O + 2L, by = "day"), areas = c("SP", "RJ"), issues = c(0L, 6L, 12L, 18L),
                         dir = tempfile("fbr-deck-"))
CTX <- pc_ctx_deck(DECK)
ALL <- env$prevcarga_read_weather(CTX, "forecast", "SP", O - 12L, NULL)
plugin_env <- function(m) env$prevcarga_plugin_env(m, MODELS)

test_that("freshest por regra == prevcarga_freshest_issue sobre todas as emissoes; nada > limit", {
  got <- by_rule(CTX, "SP", list(seg("freshest", 0, 2)))
  all <- env$prevcarga_read_weather(CTX, "forecast", "SP", O, O + 2L)
  want <- radix(env$prevcarga_freshest_issue(all, LIM))
  expect_identical(plain(got), plain(want))
  expect_false(anyDuplicated(got[, .(area_code, valid_time)]) > 0L)
  expect_true(all(got$issue_time <= LIM))
  expect_identical(unique(as.character(lubridate::as_date(got$valid_time, tz = TZ))),
                   as.character(O + 0:1))                     # com limit O-6h o previsto cobre O e O+1 (a 18Z de O-1 + 48 h)
  ## honestidade: nenhum limite (de O-5 00:00 a O 00:00) deixa passar emissao posterior, em nenhuma regra
  for (k in 0:5) {
    lim <- LIM - lubridate::days(k)
    for (s in list(seg("freshest", 0, 2), seg("per_origin", -9, 0), seg("min_lead", -5, 0, 0)))
      expect_true(all(by_rule(CTX, "SP", list(s), lim)$issue_time <= lim), info = paste(s$kind, k))
  }
  ## CONTROL: um limite mais adiantado (O 00:00) traz emissao que o limite O-6h deixa de fora
  expect_true(max(by_rule(CTX, "SP", list(seg("freshest", 0, 2)), LIM + lubridate::hours(6))$issue_time) > LIM)
})

test_that("per_origin == .rf_weather_vintage: no maximo uma emissao inteira por origem; sem candidata, nada", {
  rf <- plugin_env("random-forest")
  got <- by_rule(CTX, "SP", list(seg("per_origin", -9, 0)))
  origins <- seq(O - 9L, O, by = "day")
  cuts <- list(origin_day = O, weather_issue_limit = LIM, tz = TZ)
  v_all <- rf$.rf_weather_vintage(ALL, origins, cuts, 9L)
  v_rule <- rf$.rf_weather_vintage(got, origins, cuts, 9L)
  expect_gt(nrow(v_all), 0L)
  expect_identical(v_rule, v_all)
  ## todas as linhas da emissao escolhida chegam (o RF faz o recorte por day_of_horizon)
  iss <- sort(unique(got$issue_time))
  expect_identical(iss, sort(unique(v_all$issue_time)) |> lubridate::with_tz(TZ))
  for (i in seq_along(iss))
    expect_identical(nrow(got[issue_time == iss[[i]]]), nrow(ALL[issue_time == iss[[i]]]))
  ## origem sem candidata em (limit(T) - 1 dia, limit(T)] nao entrega nada: o deck so tem emissoes de O-6 em diante
  expect_lt(length(iss), length(origins))
  expect_identical(length(iss), 6L)
  expect_true(all(got$issue_time <= LIM))
  none <- by_rule(CTX, "SP", list(seg("per_origin", -9, -7)))
  expect_identical(nrow(none), 0L)
  expect_identical(names(none), names(ALL))
})

test_that("min_lead == lgbm_weather_vintage: so a mais fresca com valid_day - issue_day(UTC) >= lead por valid_time", {
  lg <- plugin_env("lgbm")
  k <- lg$plugin_constants()
  heat <- unname(k$heat_area["SECO"])
  expect_identical(heat, "SP")
  segs <- list(seg("min_lead", -5, 0, 0), seg("min_lead", 1, 1, 1))
  got <- by_rule(CTX, heat, segs)
  expect_false(anyDuplicated(got[, .(area_code, valid_time)]) > 0L)
  expect_true(all(got$issue_time <= LIM))
  vd <- lubridate::as_date(got$valid_time, tz = TZ); id <- lubridate::as_date(got$issue_time, tz = "UTC")
  expect_true(all(as.integer(vd - id) >= ifelse(vd == O + 1L, 1L, 0L)))
  for (spec in list(list(days = seq(O - 5L, O, by = "day"), lead = 0L), list(days = O + 1L, lead = 1L)))
    expect_identical(lg$lgbm_weather_vintage(got, "SECO", spec$days, spec$lead, tz = TZ),
                     lg$lgbm_weather_vintage(ALL[issue_time <= LIM], "SECO", spec$days, spec$lead, tz = TZ))
  ## CONTROL: lead 1 no dia O exclui as emissoes do proprio dia (a regra nao e freshest)
  g1 <- by_rule(CTX, heat, list(seg("min_lead", 0, 0, 1)))
  expect_true(all(lubridate::as_date(g1$issue_time, tz = "UTC") < O))
})

test_that("uniao: ordem radix (area, issue, valid), deduplicada; segmento vazio => 0 linhas com o schema do store", {
  got <- by_rule(CTX, "SP", list(seg("freshest", 0, 2), seg("freshest", 0, 1), seg("per_origin", -2, 0)))
  expect_identical(plain(got), plain(radix(unique(got))))
  expect_false(anyDuplicated(plain(got)) > 0L)
  e <- by_rule(CTX, "SP", list())
  expect_identical(nrow(e), 0L)
  expect_identical(vapply(e, function(x) class(x)[[1L]], ""), vapply(ALL, function(x) class(x)[[1L]], ""))
  expect_identical(names(e), c("area_code", "issue_time", "valid_time", "temperature", "heat_index"))
  expect_identical(lubridate::tz(e$issue_time), lubridate::tz(ALL$issue_time))
  expect_identical(lubridate::tz(e$valid_time), lubridate::tz(ALL$valid_time))
  expect_error(by_rule(CTX, "SP", list(seg("latest", 0, 1))), class = "prevcarga_error")
})

test_that("declaracoes reais: lgbm (SECO -> SP), RF, dessem-linear e dessem-regdin normalizam sem erro", {
  models <- file.path(pc_root(), "models")
  spec <- list(history_days = list(infer = 9L, train = 60L), horizon_days = 9L)
  pl <- function(m) env$prevcarga_plugin_load(m, models)
  nz <- function(m, area) env$prevcarga_inference_needs_of(pl(m), spec, area)
  l <- nz("lgbm", "SECO")
  expect_identical(l$weather_forecast$area, "SP")
  expect_identical(l$weather_forecast$segments, list(seg("min_lead", -9, 0, 0), seg("min_lead", 1, 1, 1)))
  r <- nz("random-forest", "SECO")
  expect_identical(r$weather_forecast, list(area = "SECO", segments = list(seg("per_origin", -9, 0))))
  expect_identical(r$holidays, list(from = -366L, to = 9L))
  for (m in c("dessem-linear", "dessem-regdin"))
    expect_identical(nz(m, "SECO")$weather_forecast, list(area = "SECO", segments = list(seg("freshest", 0, 9))), info = m)
  ## HW univariado: sem data_needs, o default generico nao declara previsto
  expect_identical(nz("holt-winters", "SECO")$weather_forecast$segments, list())
  ## regra do treino: from = NA vira -span; sem data_needs => NULL (o chamador le todas as emissoes)
  tr <- env$prevcarga_inference_needs_train_rule(pl("lgbm"), spec, "SECO", 70L)
  expect_identical(tr, list(area = "SP", segments = list(seg("min_lead", -70, 0, 0), seg("min_lead", 1, 1, 1))))
  expect_identical(env$prevcarga_inference_needs_train_rule(pl("random-forest"), spec, "SECO", 70L)$segments,
                   list(seg("per_origin", -70, 0)))
  expect_identical(env$prevcarga_inference_needs_train_rule(pl("dessem-linear"), spec, "SECO", 70L)$segments,
                   list(seg("freshest", 0, 9)))
  expect_null(env$prevcarga_inference_needs_train_rule(pl("holt-winters"), spec, "SECO", 70L))
})

## --- espioes: membros reais, treino e inferencia, pelas duas montagens da fachada ------------------------------------
HIST <- 30L
AREA <- "SP"
at <- function(hm, day = O) lubridate::ymd_hm(paste(day, hm), tz = TZ)
FDECK <- tempfile("fbr-fix-")
env$prevcarga_fixture_deck(FDECK, seed = 11, areas = AREA, days = seq(O - (HIST + 25L), O, by = "day"),
                           load_until = at("09:30"))
MEM <- list("random-forest" = list(sigla = "RF", mdir = "random-forest", exogena = NA_character_),
            "lgbm" = list(sigla = "LGBM", mdir = "lgbm", exogena = NA_character_),
            "holt-winters" = list(sigla = "HW", mdir = "holt-winters", exogena = NA_character_),
            "dessem-linear" = list(sigla = "LIN_MED", mdir = "dessem-linear", exogena = "MED"))
FCTX <- list(root = pc_root(), data_dir = FDECK, weather_issue_cutoff = "origin_midnight_utc", origin = O,
             state_dir = tempdir(), spec_overrides = list(history_days.train = HIST, history_days.infer = HIST))

## espiao: conta as linhas que prevcarga_read_forecast_by_rule entrega e as chamadas (a leitura real continua valendo; no lugar de testthat::local_mocked_bindings, que exige pacote carregado)
spy_run <- function(f) {
  orig <- env$prevcarga_read_forecast_by_rule
  calls <- list()
  ## substitui o binding no env sourced (as funcoes do fachada o resolvem la) e restaura ao sair
  assign("prevcarga_read_forecast_by_rule", function(...) {
    r <- orig(...); calls[[length(calls) + 1L]] <<- r; r
  }, envir = env)
  on.exit(assign("prevcarga_read_forecast_by_rule", orig, envir = env), add = TRUE)
  res <- f()
  list(res = res, calls = calls)
}
infer_panel <- function(m) {
  plugin <- env$prevcarga_plugin_load(m, MODELS)
  spec <- env$prevcarga_model_spec(m, pc_root(), list(history_days.train = HIST, history_days.infer = HIST))
  cuts <- env$.prevcarga_inf_cuts(plugin, spec, O, FCTX, m)
  list(plugin = plugin, spec = spec, cuts = cuts,
       rp = suppressWarnings(env$prevcarga_infer_read_panel(plugin, spec, cuts, AREA, O, FCTX)))
}
fm_days <- function(x) lubridate::as_date(x, tz = TZ)

test_that("inferencia: HW recebe zero emissoes (nem chama a leitura por regra)", {
  s <- spy_run(function() infer_panel("holt-winters"))
  expect_length(s$calls, 0L)
})

test_that("inferencia: RF no maximo uma emissao por origem; LGBM so a regra min_lead; dessem-linear so a mais fresca", {
  ## RF
  s <- spy_run(function() infer_panel("random-forest"))
  fc <- s$res$rp$panel$weather_forecast
  H <- s$res$spec$history_days$infer
  expect_length(s$calls, 1L)
  expect_lte(length(unique(fc$issue_time)), H + 1L)
  expect_true(all(fc$issue_time <= s$res$rp$cuts$weather_issue_limit))
  ## LGBM: valid_day - issue_day(UTC) >= lead (0 nos dias <= O, 1 em O+1), uma emissao por valid_time
  s <- spy_run(function() infer_panel("lgbm"))
  fc <- s$res$rp$panel$weather_forecast
  vd <- fm_days(fc$valid_time); id <- lubridate::as_date(fc$issue_time, tz = "UTC")
  expect_gt(nrow(fc), 0L)
  expect_false(anyDuplicated(fc[, .(area_code, valid_time)]) > 0L)
  expect_true(all(vd <= O + 1L))
  expect_true(all(as.integer(vd - id) >= ifelse(vd == O + 1L, 1L, 0L)))
  ## dessem-linear: a mais fresca por valid_time em O..O+H
  s <- spy_run(function() infer_panel("dessem-linear"))
  fc <- s$res$rp$panel$weather_forecast
  Hd <- s$res$spec$horizon_days
  expect_false(anyDuplicated(fc[, .(area_code, valid_time)]) > 0L)
  expect_true(all(fm_days(fc$valid_time) >= O & fm_days(fc$valid_time) <= O + Hd))
  full <- env$prevcarga_read_weather(FCTX, "forecast", AREA, O, O + Hd)
  want <- env$prevcarga_freshest_issue(full, s$res$rp$cuts$weather_issue_limit)
  expect_identical(plain(fc[order(valid_time)]), plain(want[order(valid_time)]))
})

test_that("delta 0 da escolha do plugin: leitura por regra == leitura de todas as emissoes (RF e LGBM, infer)", {
  rf <- infer_panel("random-forest"); lg <- infer_panel("lgbm")
  limit <- rf$rp$cuts$weather_issue_limit
  all_rf <- env$prevcarga_read_forecast_vintages(FCTX, AREA, O - rf$spec$history_days$infer, O + rf$spec$horizon_days, limit)
  origins <- seq(O - rf$spec$history_days$infer, O, by = "day")
  cuts <- list(origin_day = O, weather_issue_limit = limit, tz = TZ)
  rfe <- plugin_env("random-forest")
  expect_identical(rfe$.rf_weather_vintage(rf$rp$panel$weather_forecast, origins, cuts, rf$spec$horizon_days),
                   rfe$.rf_weather_vintage(all_rf, origins, cuts, rf$spec$horizon_days))
  lge <- plugin_env("lgbm")
  n <- lg$spec$history_days$infer
  all_lg <- env$prevcarga_read_forecast_vintages(FCTX, AREA, O - n, O + lg$spec$horizon_days, limit)
  for (spec in list(list(days = seq(O - n, O, by = "day"), lead = 0L), list(days = O + 1L, lead = 1L)))
    expect_identical(lge$lgbm_weather_vintage(lg$rp$panel$weather_forecast, AREA, spec$days, spec$lead, tz = TZ),
                     lge$lgbm_weather_vintage(all_lg, AREA, spec$days, spec$lead, tz = TZ))
})

test_that("treino: HW zero emissoes; RF uma por origem; LGBM min_lead; dessem-linear freshest [0, H]", {
  tin <- function(m) suppressWarnings(env$prevcarga_train_inputs(MEM[[m]], AREA, O, FCTX))
  s <- spy_run(function() tin("holt-winters"))
  expect_length(s$calls, 0L)
  s <- spy_run(function() tin("random-forest"))
  expect_length(s$calls, 1L)
  fc <- s$res$panel$weather_forecast
  expect_true(all(fc$issue_time <= s$res$cuts$weather_issue_limit))
  r <- s$res
  first <- if (identical(r$spec$split, "origin_agnostic")) r$cuts$train_origin - r$spec$history_days$train else
    r$cuts$last_data_day - r$spec$history_days$train + 1L
  expect_lte(length(unique(fc$issue_time)), as.integer(O - first) + 2L)   # uma emissao por origem T em [O - span, O]
  s <- spy_run(function() tin("lgbm"))
  fc <- s$res$panel$weather_forecast
  vd <- fm_days(fc$valid_time); id <- lubridate::as_date(fc$issue_time, tz = "UTC")
  expect_false(anyDuplicated(fc[, .(area_code, valid_time)]) > 0L)
  expect_true(all(as.integer(vd - id) >= 0L))
  s <- spy_run(function() tin("dessem-linear"))
  fc <- s$res$panel$weather_forecast
  expect_false(anyDuplicated(fc[, .(area_code, valid_time)]) > 0L)
  expect_true(all(fm_days(fc$valid_time) >= O & fm_days(fc$valid_time) <= O + s$res$spec$horizon_days))
})

test_that("plugin-brinquedo com weather_forecast_in_train e sem data_needs segue lendo todas as emissoes <= limit no treino", {
  rt <- tempfile("fbr-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
  vr <- pc_toy_variant("toy-30", run_yaml = list(backtest = list(protocol_default = "fixed"),
                                                 history_days = list(train = 5L, infer = 3L)),
                       descriptor = list(weather_forecast_in_train = TRUE), name = "toy-30")
  file.copy(file.path(vr, "toy-30"), file.path(rt, "models"), recursive = TRUE)
  file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
  dir <- tempfile("fbr-toydeck-")
  env$prevcarga_fixture_deck(dir, seed = 11, areas = "A", days = seq(O - 12L, O, by = "day"),
                             load_until = at("09:00"))
  pc_deck_topology(file.path(dir, "topology.yaml"), "A")
  ctx <- list(root = rt, data_dir = dir, topology_path = file.path(dir, "topology.yaml"),
              weather_issue_cutoff = "available_until", origin = O, state_dir = tempdir())
  mem <- list(sigla = "T", mdir = "toy-30", exogena = NA_character_)
  s <- spy_run(function() suppressWarnings(env$prevcarga_train_inputs(mem, "A", O, ctx)))
  expect_length(s$calls, 0L)                                      # sem regra: a leitura por regra nao entra
  inp <- s$res
  expect_true(isTRUE(inp$plugin$descriptor$weather_forecast_in_train))
  span <- as.integer(O - (inp$cuts$train_origin - inp$spec$history_days$train)) + 1L
  want <- env$prevcarga_read_forecast_vintages(ctx, "A", O - span, O + inp$spec$horizon_days, inp$cuts$weather_issue_limit)
  expect_identical(plain(inp$panel$weather_forecast), plain(want))
  expect_gt(length(unique(want$issue_time)), 1L)                   # varias emissoes: todas <= limit
})
