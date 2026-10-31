## test-harness-origin-cut.R -- M7.V: o corte intradiario e o available_until derivado chegam ao modelo (reescrita testthat portavel de
## jobs/tests/test_e2e_origin_cut.sh, test_prod_honesty_cut.sh e test_available_until.sh do oraculo; ADR-0023, ADR-0032, sem bash/Docker/S3).
## `prevcarga_backtest(--corte 09:00)` nos tres protocolos (fixed, daily, retrain_harness) com spy no leitor (truncate_at) e os tres bracos
## de perturbacao da carga, o gate pelo prevcarga_honesty_gate (FORBIDDEN > corte delta 0; CONTROL (corte-1d, corte] delta != 0) e as duas
## zonas isoladas: MANHA DE O (<= corte, dia O) e O-1. Uma origem ponta a ponta atravessa MAP -> combina -> reconcile com SIN = soma.
library(testthat)

OC_TZ <- "Etc/GMT+3"
OC_O <- lubridate::ymd("2026-10-05")
OC_ROOT <- pc_m7h_root()
OC_DECK <- pc_m7h_deck(lubridate::ymd("2026-08-25"), lubridate::ymd("2026-10-07"))
oc_mod <- pc_m7h_mod()
oc_at <- function(day, hm) lubridate::ymd_hm(paste(format(day), hm), tz = OC_TZ)
OC_CUT <- oc_at(OC_O, "09:00")
fx <- new.env()
sys.source(file.path(pc_root(), "scripts", "lib", "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

## membro e protocolo: o fixed e o daily rodam o semi-horario origin_agnostic; o retrain_harness, o origin_scoped
OC_CASES <- list(fixed = "toy-s", daily = "toy-s", retrain_harness = "toy-sc")

oc_backtest <- function(ctx, model, protocol, ...)
  suppressWarnings(oc_mod$prevcarga_backtest(ctx, origins = OC_O, models = model, areas = "A", cortes = "09:00", protocol = protocol,
                                             baseline = "none", ...))

## LONG da celula (membro, A, O, 09:00), ordenado por chave
oc_cell <- function(res) {
  f <- res$paths$members$long_parquet[[1L]]
  d <- data.table::as.data.table(arrow::read_parquet(f))
  d[order(d$target_datetime), ]
}

## carga realizada da area A em [O-3, O+2] como painel do gate (interval_start, interval_end, y_true)
oc_panel <- function() {
  ctx <- pc_m7h_ctx(OC_ROOT, OC_DECK)
  p <- oc_mod$prevcarga_read_actual(ctx, "A", from = oc_at(OC_O - 3L, "00:00"), to = oc_at(OC_O + 2L, "00:00"))
  p$interval_start <- p$interval_end - lubridate::minutes(30L)
  p
}

## run_fn(panel) -> LONG do membro: o deck e reescrito (copia) so onde o painel difere do original, e o backtest roda sobre ele.
## Os bracos do gate multiplicam y_true por 1,3 e somam 1, o mesmo default do perturbador do fixture_store.
oc_run_fn <- function(model, protocol, base_panel) function(panel) {
  chg <- which(abs(panel$y_true - base_panel$y_true) > 0)
  deck <- OC_DECK
  if (length(chg)) {
    deck <- fx$prevcarga_fixture_perturb(OC_DECK, channel = "load", areas = "A",
                                         after = min(panel$interval_start[chg]) - lubridate::seconds(1),
                                         at_or_before = max(panel$interval_start[chg]))
    withr::defer(unlink(deck, recursive = TRUE))
  }
  oc_cell(oc_backtest(pc_m7h_ctx(OC_ROOT, deck), model, protocol))
}

oc_delta <- function(a, b) oc_mod$.prevcarga_hg_delta(a, b)

for (proto in names(OC_CASES)) local({
  proto <- proto; model <- OC_CASES[[proto]]

  test_that(sprintf("[AC4] %s: o leitor recebe truncate_at = O 09:00 (spy) e o available_until derivado e 09:00", proto), {
    ctx <- pc_m7h_ctx(OC_ROOT, OC_DECK)
    seen <- new.env(); seen$cut <- list()
    orig <- oc_mod$prevcarga_read_panel
    pc_m7h_mock(prevcarga_read_panel = function(ctx, origin, areas, horizon_days, history_days, truncate_at = NULL) {
      seen$cut[[length(seen$cut) + 1L]] <- truncate_at
      orig(ctx, origin, areas, horizon_days, history_days, truncate_at = truncate_at)
    })
    res <- oc_backtest(ctx, model, proto)
    expect_identical(res$summary$verdict, "ok")
    trunc <- Filter(Negate(is.null), seen$cut)
    expect_gte(length(trunc), 1L)
    expect_true(all(vapply(trunc, function(t) identical(t, OC_CUT), NA)))              # O + corte, fuso da carga
    plan <- oc_mod$prevcarga_plan(ctx, OC_O, "09:00", protocol = proto, filters = list(model = model, area = "A"))
    expect_identical(plan$cells$available_until, OC_CUT)
    long <- oc_cell(res)
    expect_identical(min(long$target_datetime), oc_at(OC_O, "09:30"))                  # 1a entrega = available_until + 30
    ## CONTROL do spy: sem truncagem o leitor devolveria NULL (o spy distingue)
    expect_false(any(vapply(seen$cut, function(t) !is.null(t) && !identical(t, OC_CUT), NA)))
  })

  test_that(sprintf("[AC4] %s: FORBIDDEN carga com interval_end > 09:00 de O delta 0; CONTROL (corte-1d, corte] delta != 0 (prevcarga_honesty_gate)", proto), {
    panel <- oc_panel()
    g <- oc_mod$prevcarga_honesty_gate(oc_run_fn(model, proto, panel), panel, OC_CUT, horizon = 2L)
    expect_identical(g$verdict, "HONESTO")
    expect_true(g$reach_ok)
    expect_lte(g$forbidden_delta, 1e-5)
    expect_gt(g$control_delta, 1e-5)
    expect_lte(g$clean2_delta, 1e-5)
  })

  test_that(sprintf("[AC4] %s: MANHA DE O (<= corte) delta != 0; CONTROL do ultimo dia de treino (O-1 - lag) delta != 0; tarde de O e O+1 delta 0", proto), {
    panel <- oc_panel()
    run_fn <- oc_run_fn(model, proto, panel)
    base <- run_fn(panel)
    arm <- function(sel) { p <- data.table::copy(panel); p$y_true[sel] <- p$y_true[sel] * 1.3 + 1; oc_delta(base, run_fn(p)) }
    ie <- panel$interval_end
    expect_gt(arm(ie > oc_at(OC_O, "00:00") & ie <= OC_CUT), 1e-5)                      # MANHA DE O
    ## CONTROL do treino: o ultimo dia de dado do plano (O-1 menos o atraso do descritor; O-1 no origin_scoped) entra no artefato
    last_day <- oc_mod$prevcarga_plan(pc_m7h_ctx(OC_ROOT, OC_DECK), OC_O, "09:00", protocol = proto,
                                      filters = list(model = model, area = "A"))$cells$last_data_day
    expect_gt(arm(ie > oc_at(last_day, "00:00") & ie <= oc_at(last_day + 1L, "00:00")), 1e-5)
    expect_lte(arm(ie > OC_CUT & ie <= oc_at(OC_O + 1L, "00:00")), 1e-5)                # FORBIDDEN: tarde de O
    expect_lte(arm(ie > oc_at(OC_O + 1L, "00:00")), 1e-5)                               # FORBIDDEN: O+1
  })
})

test_that("[AC4] uma origem ponta a ponta atravessa MAP -> combina -> reconcile com SIN = soma dos subsistemas (tol 1e-5)", {
  ctx <- pc_m7h_ctx(OC_ROOT, OC_DECK)
  res <- suppressWarnings(oc_mod$prevcarga_backtest(ctx, origins = OC_O, models = NULL, areas = NULL, cortes = "09:00", baseline = "none"))
  expect_identical(res$summary$verdict, "ok")
  expect_true(file.exists(file.path(res$paths$combined, "A", "0900", "combined.parquet")))
  rec <- file.path(res$paths$reconciled, format(OC_O), "0900")
  sin <- data.table::as.data.table(arrow::read_parquet(file.path(rec, "SIN.parquet")))
  ss <- data.table::rbindlist(lapply(Sys.glob(file.path(rec, "subsistemas", "*.parquet")),
                                     function(f) data.table::as.data.table(arrow::read_parquet(f))))
  expect_gt(nrow(sin), 0L)
  soma <- ss[, list(s = sum(y_pred)), by = "target_datetime"]
  m <- merge(sin, soma, by = "target_datetime")
  expect_identical(nrow(m), nrow(sin))
  expect_lte(max(abs(m$y_pred - m$s)), 1e-5)
  ## CONTROL: a soma errada (um subsistema a menos) reprova a conferencia
  expect_gt(max(abs(m$y_pred - (m$s - 1))), 1e-5)
})
