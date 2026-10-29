## test-plugin-dispatch-honesty.R -- P.0i [verif]: honestidade model-agnostic PONTA A PONTA.
## deck sintetico (fixture_store.R, P.0e) -> leitor (deck_reader.R, M2) -> dates.R prevcarga_resolve_cuts ->
## loader P.0b -> despachantes P.0c (prevcarga_plugin_train/_infer) -> brinquedos toy-30 / toy-60, com o molde
## honesty_gate.R (M3.2: canal y_true/treino; M3.5: canal de clima). Nenhum corte e feito aqui: tudo vem do leitor e
## de dates.R. Cada braco FORBIDDEN (delta 0) tem o par CONTROL (delta != 0). Nenhum modelo real.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                       "registry", "features", "honesty_gate"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
AREAS <- c("A", "B")
TOL <- 1e-5
at <- function(hm, day = "2026-10-01") lubridate::ymd_hm(paste(day, hm), tz = TZ)
z <- function(h) lubridate::ymd_h(paste("2026-10-01", h), tz = "UTC")

## --- brinquedos com sensibilidade ao infer ------------------------------------------------------------------
## Os brinquedos do P.0b/P.0c ignoram o painel no infer. Aqui cada um recebe um R/zz_dispatch.R que soma a saida a
## 0.001 * (media da ultima hora/meia hora de carga do sistema) + 0.01 * (media da temperatura prevista). So definicoes;
## a carga do sistema e a soma das areas por intervalo. O despachante e quem desloca (toy-60: model_time_offset_min = 60).
.hz_overrides <- function(base) c(
  "toy_ingestion <- function(panel, cuts) {",
  "  ld <- data.table::as.data.table(panel$load)[, list(load_mwh = sum(load_mwh)), by = list(interval_start, interval_end)]",
  "  data.table::setorder(ld, interval_start)",
  "  list(load = ld, area = panel$load$area_code[1L], fc = panel$weather_forecast)",
  "}",
  sprintf("toy_inference_features <- function(frame, payload) list(area = frame$area, temp = mean(frame$fc$temperature),"),
  sprintf("  tail = mean(utils::tail(frame$load$load_mwh, %dL)))", if (base == "toy-30") 2L else 1L),
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  sprintf("  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, %dL), by = \"30 min\")",
          if (base == "toy-30") 2L else 9L),
  "  adj <- 0.001 * inf_features$tail + 0.01 * inf_features$temp",
  if (base == "toy-30") "  y <- payload$model$level[toy_slots(ie)] + adj" else "  y <- rep(payload$model$level, length(ie)) + adj",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie, y_pred = y))",
  "}")

## raiz de plugins para o leitor/dates e o loader: <root>/models/{toy-30,toy-60,_toy-engine}; run.yaml com `backtest`
## (exigido por prevcarga_model_spec; os brinquedos do P.0b nao o declaram).
.hz_models <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    rt <- tempfile("hz-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
    desc <- list("toy-30" = list(uses_rng = TRUE), "toy-60" = list(model_time_offset_min = 60L))
    nat <- c("toy-30" = 0L, "toy-60" = 60L)
    for (m in names(desc)) {
      vr <- pc_toy_variant(m, descriptor = desc[[m]],
        files = list("R/zz_dispatch.R" = c(.pc_toy_dispatch_lines(m, nat[[m]]), .hz_overrides(m))))
      file.copy(file.path(vr, m), file.path(rt, "models"), recursive = TRUE)
      f <- file.path(rt, "models", m, "run.yaml")
      y <- yaml::read_yaml(f); y$backtest <- list(protocol_default = "fixed"); yaml::write_yaml(y, f)
    }
    file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
    cache <<- list(root = rt, dir = file.path(rt, "models"))
    cache
  }
})

## --- decks ----------------------------------------------------------------------------------------------------
.hz_decks <- new.env()
.hz_deck <- function(load_until = "09:00", ragged = FALSE) {
  key <- paste(load_until, ragged)
  if (!is.null(.hz_decks[[key]])) return(.hz_decks[[key]])
  dir <- tempfile("hz-deck-")
  env$prevcarga_fixture_deck(dir, seed = 11, areas = AREAS, days = seq(O - 12L, O, by = "day"),
                             load_until = at(load_until))
  ## ragged: area B perde o intervalo 09:00-09:30 => available_until global = 09:00 e A tem carga alem dele
  if (ragged) dir <- env$prevcarga_fixture_drop_slots(dir, "B", O, "09:00", 1L)
  pc_deck_topology(file.path(dir, "topology.yaml"), AREAS)
  .hz_decks[[key]] <- dir
  dir
}

## --- montagem ponta a ponta --------------------------------------------------------------------------------------
## `rule` = weather_issue_cutoff. Devolve as funcoes do fluxo leitor -> cortes -> despachante -> plugin.
.hz_setup <- function(rule, deck = .hz_deck("09:00")) {
  mod <- .hz_models()
  topo <- file.path(deck, "topology.yaml")
  members <- c("toy-30", "toy-60")
  plugins <- lapply(stats::setNames(members, members), function(m) env$prevcarga_plugin_load(m, mod$dir))
  ctx_for <- function(dir) list(data_dir = dir, root = mod$root, topology_path = topo, weather_issue_cutoff = rule)
  cuts_for <- function(m, dir) {
    lag <- stats::setNames(plugins[[m]]$descriptor$data_lag_days, m)
    env$prevcarga_resolve_cuts(ctx_for(dir), origin = O, model = m, data_lag_days = lag)
  }
  dcuts <- function(rc, row) list(forecast_origin = rc$forecast_origin, origin_day = rc$forecast_origin,
    split = row$split, tz = rc$tz, load_label = rc$load_label, weather_issue_cutoff = rc$weather_issue_cutoff,
    available_until = rc$available_until, weather_issue_limit = rc$weather_issue_limit,
    train_origin = row$train_origin, last_data_day = row$last_data_day)
  read <- function(m, dir, origin, hist, horizon) {
    p <- env$prevcarga_read_panel(ctx_for(dir), origin, AREAS, horizon, hist)
    if (plugins[[m]]$spec$granularity_min == 60L) env$to_hourly(p) else p
  }
  prep <- function(m, dir) {
    rc <- suppressWarnings(cuts_for(m, dir))
    list(rc = rc, row = rc$cuts[rc$cuts$model == m, ], plugin = plugins[[m]])
  }
  train <- function(m, dir) {
    x <- prep(m, dir); sp <- x$plugin$spec
    panel <- suppressWarnings(read(m, dir, x$row$last_data_day, sp$history_days$train, sp$horizon_days))
    env$prevcarga_plugin_train(x$plugin, panel, dcuts(x$rc, x$row), sp, list())
  }
  infer <- function(m, art, dir) {
    x <- prep(m, dir); sp <- x$plugin$spec
    panel <- suppressWarnings(read(m, dir, O, sp$history_days$infer, sp$horizon_days))
    env$prevcarga_plugin_infer(x$plugin, art, panel, dcuts(x$rc, x$row), list())
  }
  artifact_sha <- function(m, dir) {
    f <- tempfile(fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))))
    env$prevcarga_plugin_freeze_bytes(train(m, dir), f)
  }
  list(rule = rule, deck = deck, members = members, plugins = plugins, ctx_for = ctx_for, prep = prep,
       train = train, infer = infer, artifact_sha = artifact_sha, dcuts = dcuts,
       ## carga do leitor (O, 10 dias) como painel y_true do molde; mesmo recorte que o infer
       gate_panel = function(dir) {
         ld <- suppressWarnings(env$prevcarga_read_load(ctx_for(dir), AREAS, from = O - 10L, to = O, origin = O))
         data.frame(area_code = ld$area_code, interval_start = ld$interval_start, interval_end = ld$interval_end,
                    y_true = ld$load_mwh, stringsAsFactors = FALSE)
       },
       ## todas as emissoes do clima previsto (sem `origin`: o leitor nao filtra) como `weather` do molde M3.5
       weather_all = function(dir) {
         w <- env$prevcarga_read_weather(ctx_for(dir), "forecast", AREAS, from = O, to = O + 9L)
         data.frame(area_code = w$area_code, issue_time = w$issue_time, valid_time = w$valid_time,
                    temperature = w$temperature, stringsAsFactors = FALSE)
       })
}

delta <- function(a, b) env$.prevcarga_hg_delta(a$delivery, b$delivery)
perturb_load <- function(deck, after, at_or_before = NULL)
  env$prevcarga_fixture_perturb(deck, "load", after = after, at_or_before = at_or_before)

## ponte molde -> store: o molde perturba o painel EM MEMORIA; run_fn(painel) refaz o mesmo recorte no store (copia)
## com a mesma funcao (y * 1.3 + 1) e confere que o leitor devolve exatamente o painel do molde.
.hz_store_for_gate <- function(s, base, p) {
  ch <- which(abs(p$y_true - base$y_true) > 0)
  if (!length(ch)) return(s$deck)
  st <- p$interval_start[ch]
  out <- env$prevcarga_fixture_perturb(s$deck, "load", after = min(st) - lubridate::minutes(30L), at_or_before = max(st))
  got <- s$gate_panel(out)
  stopifnot(isTRUE(all.equal(got$y_true, p$y_true, tolerance = 1e-9)))
  out
}

test_that("ac-0 treino: molde y_true HONESTO; FORBIDDEN sha256 igual e delta 0, CONTROL sha256 diferente e delta > tol", {
  s <- .hz_setup("available_until")
  for (m in s$members) {
    x <- s$prep(m, s$deck)
    expect_identical(x$row$last_data_day, lubridate::ymd("2026-09-30"))
    expect_identical(x$rc$available_until, at("09:00"))
    ldd_start <- lubridate::force_tz(lubridate::as_datetime(x$row$last_data_day), TZ)
    ## sha256 do artefato congelado: perturbar o dia O em diante nao muda; perturbar O-1 muda
    clean <- s$artifact_sha(m, s$deck)
    expect_identical(s$artifact_sha(m, s$deck), clean)
    last_slot <- at("00:00") - lubridate::minutes(30L)        # ultimo intervalo de last_data_day (inicio 23:30)
    forb <- s$artifact_sha(m, perturb_load(s$deck, after = last_slot))
    ctrl <- s$artifact_sha(m, perturb_load(s$deck, after = ldd_start - lubridate::minutes(30L), at_or_before = last_slot))
    expect_identical(forb, clean)
    expect_false(identical(ctrl, clean))
    ## molde M3.2: cut = fim de last_data_day (valor derivado por dates.R), horizonte do run.yaml
    base <- s$gate_panel(s$deck)
    art_for <- function(store) s$train(m, store)
    clean_inf <- function(art) s$infer(m, art, s$deck)$delivery
    run_fn <- function(p) clean_inf(art_for(.hz_store_for_gate(s, base, p)))
    cut <- lubridate::force_tz(lubridate::as_datetime(x$row$last_data_day + 1L), TZ)
    g <- env$prevcarga_honesty_gate(run_fn, base, cut, x$row$horizon_days)
    expect_identical(g$verdict, "HONESTO")
    expect_true(g$reach_ok)
    expect_lte(g$clean2_delta, TOL)
    expect_lte(g$forbidden_delta, TOL)
    expect_gt(g$control_delta, TOL)
    expect_gt(g$arms$forbidden$n_perturbed, 0L)
  }
})

test_that("ac-0 treino: o molde acusa VAZA quando o corte de treino vaza (CONTROL do proprio molde)", {
  s <- .hz_setup("available_until")
  m <- "toy-30"
  x <- s$prep(m, s$deck)
  base <- s$gate_panel(s$deck)
  ## despachante contornado de proposito: treina com o painel lido em O (inclui o dia O) => vazamento real
  leaky <- function(p) {
    store <- .hz_store_for_gate(s, base, p)
    ctx <- s$ctx_for(store); sp <- x$plugin$spec
    panel <- suppressWarnings(env$prevcarga_read_panel(ctx, O, AREAS, sp$horizon_days, sp$history_days$train))
    ld <- panel$load
    ## nivel por slot = media do ultimo dia lido (inclui o dia O parcial no fim da serie)
    v <- tapply(ld$load_mwh, ld$area_code, function(a) mean(utils::tail(a, 2L)))
    data.frame(y_pred = rep(mean(v), 4L))
  }
  cut <- lubridate::force_tz(lubridate::as_datetime(x$row$last_data_day + 1L), TZ)
  g <- env$prevcarga_honesty_gate(leaky, base, cut, x$row$horizon_days)
  expect_identical(g$verdict, "VAZA")
  expect_gt(g$forbidden_delta, TOL)
})

test_that("ac-1 intradiario: carga alem de available_until nao chega (FORBIDDEN), 08:30-09:00 muda (CONTROL)", {
  s <- .hz_setup("available_until", deck = .hz_deck("09:30", ragged = TRUE))
  for (m in s$members) {
    x <- s$prep(m, s$deck)
    expect_identical(x$rc$available_until, at("09:00"))        # B termina 09:00: minimo das areas
    art <- s$train(m, s$deck)
    base <- s$infer(m, art, s$deck)
    ## FORBIDDEN: A tem 09:00-09:30 no store (interval_end > available_until); o leitor nao entrega
    forb <- perturb_load(s$deck, after = at("08:30"))
    expect_lte(delta(base, s$infer(m, art, forb)), TOL)
    ## CONTROL: ultima hora/meia hora entregue
    ctrl <- perturb_load(s$deck, after = if (m == "toy-30") at("08:00") else at("07:30"), at_or_before = at("08:30"))
    expect_gt(delta(base, s$infer(m, art, ctrl)), TOL)
  }
})

test_that("ac-1 toy-60 horario: a meia hora isolada 09:00-09:30 e descartada; a hora completa 08:00-09:00 muda a saida", {
  s <- .hz_setup("available_until", deck = .hz_deck("09:30"))
  x <- s$prep("toy-60", s$deck)
  expect_identical(x$rc$available_until, at("09:30"))
  expect_identical(x$row$input_cut, at("09:00"))               # dates.R: 60 min => hora cheia
  art <- s$train("toy-60", s$deck)
  base <- s$infer("toy-60", art, s$deck)
  half <- perturb_load(s$deck, after = at("08:30"), at_or_before = at("09:00"))     # intervalo 09:00-09:30
  expect_lte(delta(base, s$infer("toy-60", art, half)), TOL)
  full <- perturb_load(s$deck, after = at("07:30"), at_or_before = at("08:30"))     # hora 08:00-09:00
  expect_gt(delta(base, s$infer("toy-60", art, full)), TOL)
  ## o toy-30 (semi-horario) usa o intervalo 09:00-09:30 de verdade: o mesmo CONTROL tem dente
  art30 <- s$train("toy-30", s$deck)
  b30 <- s$infer("toy-30", art30, s$deck)
  expect_gt(delta(b30, s$infer("toy-30", art30, half)), TOL)
})

test_that("ac-5 forecast (origin_scoped): treino corta em last_data_day, infer ve o dia O ate available_until", {
  s <- .hz_setup("available_until", deck = .hz_deck("09:30", ragged = TRUE))
  m <- "toy-60"
  ## painel de INFER do leitor (dia O ate available_until); o despachante corta o treino em last_data_day
  fc <- function(dir) {
    x <- s$prep(m, dir); sp <- x$plugin$spec
    panel <- suppressWarnings(env$prevcarga_read_panel(s$ctx_for(dir), O, AREAS, sp$horizon_days, sp$history_days$train))
    if (sp$granularity_min == 60L) panel <- env$to_hourly(panel)
    expect_true(any(lubridate::as_date(panel$load$interval_start, tz = TZ) == O))   # o dia O chega ao forecast
    env$prevcarga_plugin_forecast(x$plugin, panel, s$dcuts(x$rc, x$row), sp, list())
  }
  x <- s$prep(m, s$deck)
  expect_identical(x$row$last_data_day, O - 1L)
  expect_identical(x$rc$available_until, at("09:00"))
  base <- fc(s$deck)
  expect_identical(base$available_until, at("09:00"))
  ## FORBIDDEN: carga do dia O depois de available_until (A 09:00-09:30) nao muda nada
  forb <- fc(perturb_load(s$deck, after = at("08:30")))
  expect_lte(delta(base, forb), TOL)
  expect_identical(forb$values, base$values)
  ## CONTROL infer: a hora 08:00-09:00 do dia O (antes de available_until) muda a entrega...
  day_o <- fc(perturb_load(s$deck, after = at("07:30"), at_or_before = at("08:30")))
  expect_gt(delta(base, day_o), TOL)
  ## ...e FORBIDDEN treino: o mesmo dia O nao entra no artefato (nivel treinado identico)
  expect_identical(day_o$values, base$values)
  ## CONTROL treino: a manha de last_data_day muda o nivel treinado
  ldd <- fc(perturb_load(s$deck, after = at("00:00", "2026-09-30") - lubridate::minutes(30L),
                         at_or_before = at("10:30", "2026-09-30")))
  expect_false(identical(ldd$values, base$values))
})

## molde M3.5: run_fn(painel, clima) refaz o recorte de emissoes no store (copia) e confere contra o leitor
.hz_store_for_weather <- function(s, base, w) {
  ch <- which(abs(w$temperature - base$temperature) > 0)
  if (!length(ch)) return(s$deck)
  it <- w$issue_time[ch]
  out <- env$prevcarga_fixture_perturb(s$deck, "weather_forecast", issue_after = min(it) - lubridate::seconds(1),
                                       issue_at_or_before = max(it))
  stopifnot(isTRUE(all.equal(s$weather_all(out)$temperature, w$temperature, tolerance = 1e-9)))
  out
}

test_that("ac-2 clima nas duas regras: FORBIDDEN delta 0, CONTROL > tol, molde M3.5 HONESTO; 06Z cruza", {
  seen <- list()
  for (rule in c("available_until", "origin_midnight_utc")) {
    s <- .hz_setup(rule)
    limit <- s$prep("toy-30", s$deck)$rc$weather_issue_limit
    expect_identical(limit, if (rule == "available_until") at("09:00") else lubridate::with_tz(z("00"), TZ))
    for (m in s$members) {
      art <- s$train(m, s$deck)
      base_env <- s$infer(m, art, s$deck)
      wbase <- s$weather_all(s$deck)
      run_fn <- function(panel, w) s$infer(m, art, .hz_store_for_weather(s, wbase, w))$delivery
      g <- env$prevcarga_honesty_gate_weather(run_fn, s$gate_panel(s$deck), wbase, limit, cols = "temperature")
      expect_identical(g$verdict, "HONESTO")
      expect_true(g$reach_ok)
      expect_lte(g$forbidden_delta, TOL)
      expect_gt(g$control_delta, TOL)
      expect_identical(g$arms$weather_issue_limit, if (rule == "available_until") "2026-10-01T12:00:00Z" else "2026-10-01T00:00:00Z")
      ## a mesma perturbacao da emissao 06Z, direto no store: a regra efetiva chega ao plugin so pelo painel
      pert06 <- env$prevcarga_fixture_perturb(s$deck, "weather_forecast", issue_after = z("00"), issue_at_or_before = z("06"))
      seen[[paste(rule, m)]] <- delta(base_env, s$infer(m, art, pert06))
      ## caminho limpo: y_pred finito exatamente na grade de entrega
      x <- s$prep(m, s$deck)
      grid <- env$prevcarga_delivery_grid(s$dcuts(x$rc, x$row), x$plugin$descriptor$delivery_days)
      expect_identical(base_env$delivery$interval_end, grid)
      expect_true(all(is.finite(base_env$delivery$y_pred)))
      expect_identical(base_env$weather_issue_cutoff, rule)
    }
  }
  for (m in c("toy-30", "toy-60")) {
    expect_gt(seen[[paste("available_until", m)]], TOL)            # 06Z permitida: CONTROL
    expect_lte(seen[[paste("origin_midnight_utc", m)]], TOL)       # 06Z > limite: FORBIDDEN
  }
})

test_that("ac-2 clima: a ultima emissao permitida muda com a regra (06Z sob available_until, 00Z sob origin_midnight_utc)", {
  for (rule in c("available_until", "origin_midnight_utc")) {
    s <- .hz_setup(rule)
    x <- s$prep("toy-30", s$deck)
    art <- s$train("toy-30", s$deck)
    base <- s$infer("toy-30", art, s$deck)
    last_ok <- if (rule == "available_until") z("06") else z("00")
    pert <- env$prevcarga_fixture_perturb(s$deck, "weather_forecast", issue_after = last_ok - lubridate::seconds(1),
                                          issue_at_or_before = last_ok)
    expect_gt(delta(base, s$infer("toy-30", art, pert)), TOL)
    late <- env$prevcarga_fixture_perturb(s$deck, "weather_forecast", issue_after = z("06"))
    expect_lte(delta(base, s$infer("toy-30", art, late)), TOL)     # 18Z e posterior: nunca chega
  }
})
