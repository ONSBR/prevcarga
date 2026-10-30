## test-weather.R -- LGBM.6: canal de clima do honesty_gate (M3.5, ADR-0024) sobre o plugin lgbm, nas DUAS regras de
## weather_issue_cutoff (`available_until` e `origin_midnight_utc`), no deck sintetico (P.0e: emissoes 00Z/06Z/18Z, available_until
## O 09:00 Etc/GMT+3 = 12:00Z). O `weather_issue_limit` vem PRONTO de prevcarga_resolve_cuts (via lgbm_fx_cuts); o teste nunca o recalcula.
## `run_fn(panel, weather)` = ingestao -> features de infer -> infer pelo despachante com artefato FIXO. O recorte `issue_time <=
## limite` no run_fn e o do LEITOR (como lgbm_fx_panel): o plugin so CONFERE (caso `clima_limite`), nunca filtra.
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
if (!is.function(lgbm_lib$prevcarga_honesty_gate_weather))
  sys.source(file.path(lgbm_root(), "scripts", "lib", "honesty_gate.R"), envir = lgbm_lib)
LGW_RULES <- c("available_until", "origin_midnight_utc")
LGW_TOL <- 1e-5
LGW_O <- LGBM_O
LGW_DECK <- lgbm_fx_deck(days = 120L, available_until = lubridate::ymd_hm("2026-10-01 09:00", tz = LGBM_TZ))

## artefato FIXO por regra (o carimbo leva weather_issue_cutoff): treino ate last_data_day, uma vez por regra e processo
.lgw_origins <- new.env(parent = emptyenv())
lgw_origin <- function(rule) {
  if (is.null(.lgw_origins[[rule]])) {
    ctx <- lgbm_fx_ctx(LGW_DECK, weather_issue_cutoff = rule)
    cuts <- lgbm_fx_cuts(ctx)
    plugin <- lgbm_fx_plugin()
    tp <- lgbm_fx_dpanel(LGW_DECK, ctx, "train", cuts, 115L)
    stamp_ctx <- list(load_label = cuts$load_label, timezone = cuts$tz, weather_issue_cutoff = cuts$weather_issue_cutoff)
    art <- suppressWarnings(lgbm_with_cli(lgbm_lib$prevcarga_plugin_train(plugin, tp, cuts, plugin$spec, stamp_ctx)))
    assign(rule, list(plugin = plugin, ctx = ctx, cuts = cuts, art = art), envir = .lgw_origins)
  }
  .lgw_origins[[rule]]
}

## contexto por regra: ctx/cuts da regra (limite pronto), painel do despachante e o clima COMPLETO do leitor (inclui > limite)
lgw_ctx <- function(rule) {
  ctx <- lgbm_fx_ctx(LGW_DECK, weather_issue_cutoff = rule)
  cuts <- lgbm_fx_cuts(ctx)
  panel <- lgbm_fx_dpanel(LGW_DECK, ctx, "infer", cuts)
  weather <- lgbm_lib$prevcarga_read_weather(ctx, "forecast", c(LGBM_AREA, LGBM_HEAT), LGW_O - 45L, LGW_O + 2L)
  list(ctx = ctx, cuts = cuts, panel = panel, weather = weather)
}

## o gate de clima recebe `weather` e `panel` como data.frame (`x[cols]` num data.table viraria uma juncao)
lgw_gate <- function(run_fn, cx)
  lgbm_lib$prevcarga_honesty_gate_weather(run_fn, panel = as.data.frame(cx$panel$load), weather = as.data.frame(cx$weather),
                                          weather_issue_limit = cx$cuts$weather_issue_limit, tol = LGW_TOL)

lgw_run_fn <- function(cx) {
  function(load, weather) {
    panel <- cx$panel
    panel$load <- data.table::as.data.table(load)
    w <- data.table::as.data.table(weather)
    panel$weather_forecast <- w[issue_time <= cx$cuts$weather_issue_limit & area_code == LGBM_HEAT]
    as.data.frame(lgbm_fx_infer(lgw_origin(cx$cuts$weather_issue_cutoff), panel, cx$cuts)$delivery)
  }
}

## perturbacao de VALOR das emissoes marcadas por `mask` (a do gate: y * 1.30 + 1) em todas as colunas numericas
lgw_perturb <- function(weather, mask) {
  w <- data.table::copy(data.table::as.data.table(weather))
  for (cl in setdiff(names(w)[vapply(w, is.numeric, TRUE)], c("area_code", "issue_time", "valid_time")))
    data.table::set(w, which(mask), cl, w[[cl]][mask] * 1.30 + 1)
  w
}

lgw_utc_at <- function(day, h) lubridate::make_datetime(lubridate::year(day), lubridate::month(day), lubridate::mday(day), h, tz = "UTC")

test_that("[PLUGINS] clima_honesto: honesto nas duas regras (FORBIDDEN <= 1e-5, CONTROL na ultima emissao permitida > 1e-5) e CONTROL de harness cego ao clima", {
  for (rule in LGW_RULES) {
    cx <- lgw_ctx(rule)
    lim <- cx$cuts$weather_issue_limit
    expect_identical(cx$cuts$weather_issue_cutoff, rule)
    expect_gt(sum(cx$weather$issue_time > lim), 0L)                     # ha emissao alem do limite para o braco FORBIDDEN
    run_fn <- lgw_run_fn(cx)
    v <- lgw_gate(run_fn, cx)
    expect_identical(v$verdict, "HONESTO", info = rule)
    expect_true(v$reach_ok, info = rule)
    expect_lte(v$clean2_delta, LGW_TOL); expect_lte(v$forbidden_delta, LGW_TOL)
    expect_gt(v$control_delta, LGW_TOL)
    ## a ultima emissao permitida: 06Z de O (available_until 12:00Z) ou 00Z de O (origin_midnight_utc)
    fresh <- max(cx$weather$issue_time[cx$weather$issue_time <= lim])
    expect_identical(as.numeric(fresh), as.numeric(if (rule == "available_until") lgw_utc_at(LGW_O, 6L) else lgw_utc_at(LGW_O, 0L)))
    base <- run_fn(cx$panel$load, cx$weather)
    ctrl <- run_fn(cx$panel$load, lgw_perturb(cx$weather, cx$weather$issue_time == fresh))
    expect_gt(max(abs(ctrl$y_pred - base$y_pred)), LGW_TOL)
    ## FORBIDDEN explicito: nada alem do limite muda a entrega (identica)
    forb <- run_fn(cx$panel$load, lgw_perturb(cx$weather, cx$weather$issue_time > lim))
    expect_identical(forb$y_pred, base$y_pred)
    ## CONTROL de harness: um run_fn cego ao clima recebido => HARNESS QUEBRADO
    blind <- function(load, weather) run_fn(load, cx$weather)
    expect_identical(lgw_gate(blind, cx)$verdict, "HARNESS QUEBRADO", info = rule)
  }
})

test_that("[PLUGINS] clima_honesto: a 06Z de O so e permitida (e muda a entrega) na regra available_until; na origin_midnight_utc e FORBIDDEN", {
  dd <- vapply(LGW_RULES, function(rule) {
    cx <- lgw_ctx(rule); run_fn <- lgw_run_fn(cx)
    base <- run_fn(cx$panel$load, cx$weather)
    z06 <- run_fn(cx$panel$load, lgw_perturb(cx$weather, cx$weather$issue_time == lgw_utc_at(LGW_O, 6L)))
    z00 <- run_fn(cx$panel$load, lgw_perturb(cx$weather, cx$weather$issue_time == lgw_utc_at(LGW_O, 0L)))
    c(z06 = max(abs(z06$y_pred - base$y_pred)), z00 = max(abs(z00$y_pred - base$y_pred)))
  }, c(z06 = 0, z00 = 0))
  expect_gt(dd[["z06", "available_until"]], LGW_TOL)          # 06Z <= available_until (12:00Z): a mais fresca => CONTROL
  expect_gt(dd[["z00", "origin_midnight_utc"]], LGW_TOL)      # 00Z de O e a mais fresca da regra da meia-noite UTC
  expect_lte(dd[["z06", "origin_midnight_utc"]], LGW_TOL)     # 06Z > limite 00Z: FORBIDDEN (o leitor a corta)
  ## (a 00Z de O ainda alimenta os valid_time anteriores a 06Z que a emissao 06Z nao cobre: nao e vazamento, so cobertura)
})

test_that("[PLUGINS] clima_limite: emissao com issue_time > weather_issue_limit entregue ao plugin => [prevcarga/contract] exit 2; CONTROL sem ela entrega", {
  P <- lgbm_source_plugin()
  for (rule in LGW_RULES) {
    cx <- lgw_ctx(rule)
    lim <- cx$cuts$weather_issue_limit
    over <- sort(unique(cx$weather$issue_time[cx$weather$issue_time > lim]))
    expect_gt(length(over), 0L)
    bad <- cx$panel
    bad$weather_forecast <- data.table::as.data.table(cx$weather)[area_code == LGBM_HEAT]   # TODAS as emissoes: o plugin confere, nao filtra
    e <- fail(P$plugin_ingest(bad, cx$cuts))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
    expect_true(grepl("emissao alem de weather_issue_limit", conditionMessage(e), fixed = TRUE))
    ## CONTROL: o mesmo painel pelo recorte do leitor roda ate a entrega
    ok <- lgw_run_fn(cx)(cx$panel$load, cx$weather)
    expect_gt(nrow(ok), 0L); expect_true(all(is.finite(ok$y_pred)))
  }
})

test_that("[PLUGINS] clima_safra: CONTROL issue_day_tz no fuso da carga: a 00Z de X+1 entra como lead 0 de X e muda as features (> 1e-5)", {
  Pl <- lgbm_source_plugin()
  K <- lgbm_consts(Pl)
  cx <- lgw_ctx("available_until")
  wf <- data.table::as.data.table(cx$weather)[issue_time <= cx$cuts$weather_issue_limit]
  Kc <- K; Kc$issue_day_tz <- cx$cuts$tz
  d <- LGW_O - 3L
  a <- Pl$lgbm_weather_vintage(wf, LGBM_HEAT, d, 0L, K, cx$cuts$tz)
  b <- Pl$lgbm_weather_vintage(wf, LGBM_HEAT, d, 0L, Kc, cx$cuts$tz)
  expect_identical(a$DataHora, b$DataHora)
  expect_gt(max(abs(a$Temperatura - b$Temperatura)), LGW_TOL)
  ## FORBIDDEN: a regra do plugin (UTC) e a do oraculo: a mesma entrada da o mesmo resultado
  expect_identical(Pl$lgbm_weather_vintage(wf, LGBM_HEAT, d, 0L, K, cx$cuts$tz), a)
})
