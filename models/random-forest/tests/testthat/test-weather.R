## test-weather.R -- RF.5: canal de clima do honesty_gate (M3.5, ADR-0024) sobre o plugin random-forest, nas DUAS regras de
## weather_issue_cutoff (`available_until` e `origin_midnight_utc`), no deck sintetico (P.0e: emissoes 00Z/06Z/18Z, available_until
## O 09:00 Etc/GMT+3). Porta de gate09/gate10 do oraculo para dado sintetico.
## O `weather_issue_limit` vem PRONTO de `prevcarga_weather_issue_limit` (via rf_fx_cuts); o teste nunca o recalcula.
## `run_fn(panel, weather)` = ingestao -> features de infer -> infer com artefato FIXO. O `panel` do gate e a carga (data.frame);
## o previsto do gate traz TODAS as emissoes do leitor (inclusive > limite). O recorte `issue_time <= limite` no `run_fn` e o do
## LEITOR do despachante (como rf_fx_panel): o plugin so CONFERE (caso `weather_limit` abaixo), nunca filtra.
suppressWarnings(suppressMessages(library(testthat)))

RFW_RULES <- c("available_until", "origin_midnight_utc")
RFW_TOL <- 1e-5

## clima completo do leitor (inclui emissoes > limite): a mesma janela de rf_fx_panel
rfw_weather_all <- function(dir, cuts)
  rf_lib$prevcarga_read_weather(rf_fx_ctx(dir, cuts$weather_issue_cutoff), "forecast", RF_AREA,
                                cuts$train_origin - 370L, cuts$forecast_origin + 10L)

## contexto por regra: deck de 09:00, cuts de infer (limite pronto), painel do leitor e artefato fixo
rfw_ctx <- function(rule) {
  dir <- rf_inf_deck("09:00")
  cuts <- rf_fx_cuts(O = RF_O, rule = rule, phase = "infer", dir = dir)
  spec <- rf_tr_spec(); cuts$spec <- spec
  list(dir = dir, cuts = cuts, panel = rf_fx_panel(dir, cuts), weather = rfw_weather_all(dir, cuts), spec = spec)
}

## o gate de clima recebe `weather` como data.frame: `weather[cols]` num data.table viraria uma juncao (ver discovery do RF.5)
rfw_gate <- function(run_fn, cx) {
  rf_lib$prevcarga_honesty_gate_weather(run_fn, panel = cx$panel$load, weather = as.data.frame(cx$weather),
                                        weather_issue_limit = cx$cuts$weather_issue_limit, tol = RFW_TOL)
}

## run_fn do gate: devolve o `delivery` (data.frame com y_pred)
rfw_run_fn <- function(cx, art = rf_inf_artifact()) {
  p <- rf_plug()
  function(load, weather) {
    panel <- cx$panel
    panel$load <- load
    panel$weather_forecast <- data.table::as.data.table(weather)[issue_time <= cx$cuts$weather_issue_limit]
    frame <- p$plugin_ingest(panel, cx$cuts, cx$spec)
    feat <- p$plugin_inference_features(frame, art, cx$cuts)
    as.data.frame(p$plugin_infer(art, feat, cx$cuts)$delivery)
  }
}

## perturbacao de VALOR das emissoes marcadas por `mask` (a do gate: y * 1.30 + 1) sobre todas as colunas numericas
rfw_perturb <- function(weather, mask) {
  w <- data.table::copy(weather)
  for (cl in setdiff(names(w)[vapply(w, is.numeric, TRUE)], c("area_code", "issue_time", "valid_time")))
    data.table::set(w, which(mask), cl, w[[cl]][mask] * 1.30 + 1)
  w
}

## max |d y_pred| por dia do calendario local (interval_end - 1 s => o ultimo slot 00:00 pertence ao dia anterior)
rfw_by_day <- function(a, b, tz) {
  day <- lubridate::as_date(a$interval_end - 1, tz = tz)
  tapply(abs(a$y_pred - b$y_pred), day, max)
}

utc_at <- function(day, h) lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::hours(h)

test_that("[PLUGINS] weather: honesto nas duas regras (FORBIDDEN d=0, CONTROL d>tol em O+1..O+8) e CONTROL de harness cego ao clima", {
  for (rule in RFW_RULES) {
    cx <- rfw_ctx(rule)
    lim <- cx$cuts$weather_issue_limit
    expect_identical(cx$cuts$weather_issue_cutoff, rule)
    expect_gt(sum(cx$weather$issue_time > lim), 0L)                     # ha emissao alem do limite para o braco FORBIDDEN
    run_fn <- rfw_run_fn(cx)
    v <- rfw_gate(run_fn, cx)
    expect_identical(v$verdict, "HONESTO", info = rule)
    expect_true(v$reach_ok, info = rule)
    expect_lte(v$clean2_delta, RFW_TOL); expect_lte(v$forbidden_delta, RFW_TOL)
    expect_gt(v$control_delta, RFW_TOL)
    ## o CONTROL (ultima emissao permitida de O) muda a entrega em cada dia O+1..O+8
    allowed <- cx$weather$issue_time <= lim
    fresh <- max(cx$weather$issue_time[allowed])
    expect_identical(as.numeric(fresh), as.numeric(if (rule == "available_until") utc_at(RF_O, 6L) else utc_at(RF_O, 0L)))
    base <- run_fn(cx$panel$load, cx$weather)
    ctrl <- run_fn(cx$panel$load, rfw_perturb(cx$weather, cx$weather$issue_time == fresh))
    d <- rfw_by_day(base, ctrl, cx$cuts$tz)
    expect_true(all(d[as.character(RF_O + 1:8)] > RFW_TOL), info = paste(rule, paste(signif(d, 3), collapse = " ")))
    expect_identical(nrow(base), nrow(ctrl))
    ## FORBIDDEN (nada permitido muda): Delta exatamente 0 na entrega inteira
    forb <- run_fn(cx$panel$load, rfw_perturb(cx$weather, cx$weather$issue_time > lim))
    expect_identical(forb$y_pred, base$y_pred)
    ## CONTROL de harness: um run_fn cego ao clima (ignora o previsto recebido) => HARNESS QUEBRADO
    blind <- function(load, weather) run_fn(load, cx$weather)
    vb <- rfw_gate(blind, cx)
    expect_identical(vb$verdict, "HARNESS QUEBRADO", info = rule)
  }
})

test_that("[PLUGINS] weather: a 06Z de O muda a entrega so na regra available_until (00Z de O so em origin_midnight_utc)", {
  dd <- vapply(RFW_RULES, function(rule) {
    cx <- rfw_ctx(rule); run_fn <- rfw_run_fn(cx)
    base <- run_fn(cx$panel$load, cx$weather)
    z06 <- run_fn(cx$panel$load, rfw_perturb(cx$weather, cx$weather$issue_time == utc_at(RF_O, 6L)))
    z00 <- run_fn(cx$panel$load, rfw_perturb(cx$weather, cx$weather$issue_time == utc_at(RF_O, 0L)))
    c(z06 = max(abs(z06$y_pred - base$y_pred)), z00 = max(abs(z00$y_pred - base$y_pred)))
  }, c(z06 = 0, z00 = 0))
  expect_gt(dd[["z06", "available_until"]], RFW_TOL)          # 06Z (03:00 local) <= available_until 09:00: a mais fresca => CONTROL
  expect_gt(dd[["z00", "origin_midnight_utc"]], RFW_TOL)      # 00Z de O e a mais fresca da regra da meia-noite UTC
  expect_lte(dd[["z06", "origin_midnight_utc"]], RFW_TOL)     # 06Z > limite 00Z: FORBIDDEN (o leitor a corta)
  expect_lte(dd[["z00", "available_until"]], RFW_TOL)         # 00Z de O ja foi superada pela 06Z de O
})

test_that("[PLUGINS] weather_limit: emissao com issue_time > weather_issue_limit no painel => [prevcarga/contract] exit 2 nomeando a emissao; CONTROL sem ela entrega", {
  p <- rf_plug()
  for (rule in RFW_RULES) {
    cx <- rfw_ctx(rule)
    lim <- cx$cuts$weather_issue_limit
    over <- sort(unique(cx$weather$issue_time[cx$weather$issue_time > lim]))
    expect_gt(length(over), 0L)
    bad <- cx$panel
    bad$weather_forecast <- cx$weather                                  # painel com TODAS as emissoes: o plugin confere, nao filtra
    e <- fail(p$plugin_ingest(bad, cx$cuts, cx$spec))
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, "plugin.weather_beyond_limit")
    expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
    expect_identical(e$details$issue_time, format(over[[1L]], tz = "UTC"))      # nomeia a emissao (a mais antiga acima do limite)
    ## CONTROL: o mesmo painel sem a emissao alem do limite roda ate a entrega
    ok <- cx$panel
    ok$weather_forecast <- cx$weather[issue_time <= lim]
    frame <- p$plugin_ingest(ok, cx$cuts, cx$spec)
    feat <- p$plugin_inference_features(frame, rf_inf_artifact(), cx$cuts)
    env <- p$plugin_infer(rf_inf_artifact(), feat, cx$cuts)
    expect_gt(nrow(env$delivery), 0L)
    ## so uma emissao a mais (a primeira acima do limite) ja basta para abortar
    one <- cx$panel
    one$weather_forecast <- rbind(cx$weather[issue_time <= lim], cx$weather[issue_time == over[[1L]]])
    expect_identical(fail(p$plugin_ingest(one, cx$cuts, cx$spec))$id, "plugin.weather_beyond_limit")
  }
})
