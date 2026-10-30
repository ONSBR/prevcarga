## test-facade-members.R -- os 5 membros REAIS (random-forest, lgbm, holt-winters, dessem-linear, dessem-regdin) treinam por
## prevcarga_train e inferem por prevcarga_infer_cell sobre um deck de scripts/tools/fixture_store.R (fix/facade-channels).
## Cobre os tres canais que a fachada nao fiava: cuts$spec/cuts$phase do despachante (RF), clima previsto no treino e no
## infer para quem monta a propria safra (`weather_forecast_in_train`: RF/LGBM/DESSEM) e a safra por (area, valid_time) do
## leitor (DESSEM/HW: a emissao 06Z comeca as 03:00 e nao apaga o inicio do dia O). Historico curto por
## ctx$spec_overrides (history_days.train; DESSEM tambem history_days.infer). Cada FORBIDDEN (delta 0) tem o seu CONTROL.
## Tudo em tempdir; sem variaveis de ambiente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                       "registry", "features", "training", "inference"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
AREA <- "SP"                    # folha da topologia real (config/topology.yaml da raiz)
HIST <- 60L                     # history_days.train do teste (o run.yaml pede 365..1825)
at <- function(hm, day = O) lubridate::ymd_hm(paste(day, hm), tz = TZ)
utc <- function(day, h) lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::hours(h)
brt <- function(day, h) lubridate::with_tz(utc(day, h), TZ)       # emissao como o leitor a entrega (fuso da carga)

## Deck: HIST + 25 dias ate O, carga ate O 09:30. O motor DESSEM pede amostra historica de cada tipo de dia do horizonte
## (o feriado futuro do fixture em O+3 tem prevcarga_code 3: dois dias historicos do mesmo codigo) e uma serie diaria com
## ruido proprio (funcao so do valor, sem RNG; a carga do fixture e semanal quase pura e o ARIMA do regdin degenera).
FM_DECK <- local({
  dir <- tempfile("fm-deck-")
  env$prevcarga_fixture_deck(dir, seed = 11, areas = AREA, days = seq(O - (HIST + 25L), O, by = "day"),
                             load_until = at("09:30"))
  for (k in 1:2) dir <- env$prevcarga_fixture_set_holiday(dir, AREA, O - 14L * k,
                                                          c(holiday_type_id = 1, prevcarga_code = 3, simplified_code = 1))
  env$prevcarga_fixture_perturb(dir, "load", perturb = function(v) v + 40 * sin(v * 0.37))
})

FM_MEMBERS <- list(
  "random-forest" = list(sigla = "RF", mdir = "random-forest", exogena = NA_character_),
  "lgbm"          = list(sigla = "LGBM", mdir = "lgbm", exogena = NA_character_),
  "holt-winters"  = list(sigla = "HW", mdir = "holt-winters", exogena = NA_character_),
  "dessem-linear" = list(sigla = "LIN_MED", mdir = "dessem-linear", exogena = "MED"),
  "dessem-regdin" = list(sigla = "RGD_MED", mdir = "dessem-regdin", exogena = "MED"))

## A CLI poe a API da fundacao no globalenv (interino: os plugins a buscam la); aqui so durante o teste chamador.
FM_CLI <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_warn", "prevcarga_delivery_grid",
            "prevcarga_plugin_check_artifact", "prevcarga_plugin_scan_pure", "prevcarga_plugin_stamp",
            "prevcarga_preserve_rng", "to_semihourly")
fm_with_cli <- function(frame = parent.frame()) {
  nm <- FM_CLI[!vapply(FM_CLI, exists, NA, envir = globalenv(), inherits = FALSE)]
  for (n in nm) assign(n, env[[n]], envir = globalenv())
  withr::defer(rm(list = nm, envir = globalenv()), envir = frame)
}

fm_ctx <- function(mdir, state, deck = FM_DECK) {
  ov <- list(history_days.train = HIST)
  if (startsWith(mdir, "dessem-")) ov$history_days.infer <- HIST     # regdin: infer_history_equals_train
  list(root = pc_root(), data_dir = deck, weather_issue_cutoff = "available_until", origin = O, state_dir = state,
       code_commit = "c0ffee", renv_lock_hash = "ab12", spec_overrides = ov)
}
fm_train <- function(mdir, ctx) suppressMessages(suppressWarnings(env$prevcarga_train(FM_MEMBERS[[mdir]], AREA, O, ctx)))
fm_infer <- function(mdir, ctx) {
  m <- FM_MEMBERS[[mdir]]
  v <- if (is.na(m$exogena)) NULL else m$exogena
  suppressMessages(suppressWarnings(env$prevcarga_infer_cell(m, AREA, O, ctx, variant = v)))
}

## =====================================================================================================
## Os 5 membros pela fachada: treino publicado e infer alinhado a grade de entrega, y_pred finito
## =====================================================================================================
## state_dir por membro no nivel do arquivo (os bracos de clima abaixo reusam o artefato; limpo no fim do arquivo)
FM_STATE <- local({
  st <- vapply(names(FM_MEMBERS), function(m) {
    d <- tempfile("fm-state-"); dir.create(file.path(d, "artifacts"), recursive = TRUE); d
  }, "")
  withr::defer(unlink(st, recursive = TRUE), envir = testthat::teardown_env())
  st
})

for (mdir in names(FM_MEMBERS)) local({
  mdir <- mdir
  test_that(sprintf("%s treina por prevcarga_train e infere por prevcarga_infer_cell no deck do fixture", mdir), {
    fm_with_cli()
    ctx <- fm_ctx(mdir, FM_STATE[[mdir]])
    row <- fm_train(mdir, ctx)
    expect_identical(row$status, "trained")
    expect_true(dir.exists(row$path))
    res <- fm_infer(mdir, ctx)
    d <- env$prevcarga_plugin_load(mdir, file.path(pc_root(), "models"))$descriptor
    grid <- env$prevcarga_delivery_grid(list(available_until = res$available_until_posix, origin_day = O, tz = TZ),
                                        d$delivery_days)
    expect_identical(res$predictions$target_datetime, grid)
    expect_identical(res$first_target, "2026-10-01T10:00:00-03:00")
    expect_true(all(is.finite(res$predictions$y_pred)))
    expect_identical(res$version, row$version)
  })
})

## =====================================================================================================
## Clima previsto do infer: emissao depois do limite nunca entra (FORBIDDEN), a que o membro usa muda a entrega (CONTROL)
## =====================================================================================================
## limite = available_until (O 09:30 BRT = 12:30Z): 00Z/06Z de O entram, 18Z de O nao.
fm_fc_bump <- function(deck, issue_after = NULL, issue_at_or_before = NULL)
  env$prevcarga_fixture_perturb(deck, "weather_forecast", issue_after = issue_after,
                                issue_at_or_before = issue_at_or_before, perturb = function(v) v + 7)
fm_arms <- function(mdir) {
  fm_with_cli(parent.frame())
  st <- FM_STATE[[mdir]]
  if (!length(list.files(file.path(st, "artifacts"), recursive = TRUE))) fm_train(mdir, fm_ctx(mdir, st))
  y <- function(deck) fm_infer(mdir, fm_ctx(mdir, st, deck))$predictions$y_pred
  list(base = y(FM_DECK),
       late = y(fm_fc_bump(FM_DECK, issue_after = utc(O, 12))),                                      # 18Z de O
       z00 = y(fm_fc_bump(FM_DECK, issue_after = utc(O - 1L, 18), issue_at_or_before = utc(O, 0))),  # 00Z de O
       z06 = y(fm_fc_bump(FM_DECK, issue_after = utc(O, 0), issue_at_or_before = utc(O, 6))))        # 06Z de O
}

test_that("lgbm (safra propria, weather_forecast_in_train): 18Z de O FORBIDDEN; 06Z de O CONTROL", {
  a <- fm_arms("lgbm")
  expect_identical(a$late, a$base)
  expect_false(identical(a$z06, a$base))
})

## O bug do M5.8 (series_gap EXOGENAPREV 21/24): as horas 00:00-03:00 de O so existem na 00Z (a 06Z comeca as 03:00 BRT).
test_that("dessem-linear: 18Z de O FORBIDDEN; 00Z de O (inicio do dia O) e 06Z de O CONTROL", {
  a <- fm_arms("dessem-linear")
  expect_identical(a$late, a$base)
  expect_false(identical(a$z00, a$base))
  expect_false(identical(a$z06, a$base))
})

## =====================================================================================================
## Leitor: safra por (area, valid_time) e o fixture cobre o inicio do dia O pela 00Z
## =====================================================================================================
test_that("leitor: a 00Z de O cobre desde 21:00 BRT de O-1; o painel do dia O sai completo e sem emissao alem do limite", {
  ctx <- list(root = pc_root(), data_dir = FM_DECK, weather_issue_cutoff = "available_until")
  all <- env$prevcarga_read_weather(ctx, "forecast", AREA, O - 1L, O + 1L)
  expect_identical(min(all[issue_time == brt(O, 0)]$valid_time), at("21:00", O - 1L))
  p <- suppressWarnings(env$prevcarga_read_panel(ctx, O, AREA, 9L, 5L))
  fc <- p$weather_forecast
  lim <- p$attrs$weather_issue_limit
  expect_identical(lim, at("09:30"))
  expect_true(all(fc$issue_time <= lim))
  expect_false(anyDuplicated(fc[, .(area_code, valid_time)]) > 0L)
  day_o <- fc[lubridate::as_date(valid_time, tz = TZ) == O]
  expect_identical(nrow(day_o), 48L)
  ## por valid_time, a mais fresca <= limite: 06Z a partir de 03:00, 00Z antes
  expect_identical(unique(day_o[valid_time < at("03:00")]$issue_time), brt(O, 0))
  expect_identical(unique(day_o[valid_time >= at("03:00")]$issue_time), brt(O, 6))
  ## CONTROL: com a regra origin_midnight_utc o limite e a 00Z e o dia O inteiro vem dela
  p2 <- suppressWarnings(env$prevcarga_read_panel(modifyList(ctx, list(weather_issue_cutoff = "origin_midnight_utc")),
                                                  O, AREA, 9L, 5L))
  d2 <- p2$weather_forecast[lubridate::as_date(valid_time, tz = TZ) == O]
  expect_identical(unique(d2$issue_time), brt(O, 0))
  expect_identical(nrow(d2), 48L)
})
