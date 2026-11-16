## test-dessem-short-deck.R -- E4-T4: verificacao de ponta do epico E4 (ADR-045, ADR-046). Teste CRUZADO dos dois DESSEM:
## com o MESMO artefato de cada plugin (treino unico por cenario, fixture sintetica longa), a inferencia com o deck de 15 dias
## (recorte por inference_needs gravado no artefato) e indistinguivel da inferencia com o deck longo (leitor atual): values 9x48,
## delivery e meta identical() nos modos nowcast (O 09:00) e no_day_o (O 00:00).
##   - dessem-linear: store em disco (fixture_store.R); o deck curto sai do verbo de E2 (prevcarga_deck_inference_build) e a leitura
##     passa pela fachada (prevcarga_infer_read_panel com o inference_needs do artefato; ampliacao do escopo: resolucao
##     01M7GFR748KR43TF644B5Y7CNY). Os casos de origem recortam o painel em memoria pela MESMA regra de janelas.
##   - dessem-regdin: nao ha store para a fixture de 14 meses; o deck curto e o painel recortado em memoria por inference_needs
##     (carga/observado >= O - days_before; calendario em [O + from, O + to]), a regra do verbo (conferida no linear).
## Os helpers de fixture dos plugins (so leitura) sao carregados em envs proprios: nada vaza para o globalenv.
## Tempo: ~10 min (1 treino do linear e 4 do regdin, em cache de sessao). Feriados no horizonte (unico, dois seguidos, trio, ultimo
## dia entregue): so no regdin; a fixture pequena do linear nao sustenta o SVM de dia especial do motor. Sem variaveis de ambiente; datas por lubridate com tz explicito.

sd_helpers <- function(dir, files) {
  e <- new.env(parent = globalenv())
  for (f in files) sys.source(file.path(pc_root(), dir, "tests", "testthat", f), envir = e)
  e
}
DL <- sd_helpers("models/dessem-linear", "helper-dl.R")
RD <- sd_helpers("models/dessem-regdin", c("helper-fixtures.R", "helper-rd.R", "helper-rd2.R", "helper-rd3.R"))
for (m in c("runtime", "inference_needs", "training", "inference", "deck_inference"))
  sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = DL$dl_lib)
DL$dl_fx_prep()

sd_modes <- c("09:00", "00:00")
sd_cases <- list(single = c(`2` = 3L), dbl = c(`2` = 3L, `3` = 3L), trio = c(`2` = 3L, `3` = 4L, `4` = 5L), last = c(`8` = 3L))
sd_hist_codes <- c(3L, 4L, 5L)                 # amostras de feriado fora dos 15 dias do deck curto (O-36, O-26, O-20), codigos consecutivos

## calendario do cenario: tres semanas passadas com 3 dias consecutivos (codigos 3, 4, 5) + o horizonte (nomes = dia relativo a O)
sd_holidays <- function(area, O, horizon, weeks = c(36L, 26L, 20L)) {
  hist <- data.table::rbindlist(lapply(weeks, function(w)
    data.table::data.table(off = -w + 0:2, code = sd_hist_codes)))
  h <- rbind(hist, data.table::data.table(off = as.integer(names(horizon)), code = as.integer(horizon)))
  data.table::data.table(area_code = area, date = O + h$off, holiday_type_id = 1L, prevcarga_code = h$code, simplified_code = 3L)
}

## deck de 15 dias em memoria: a regra do verbo de E2 (janelas por inference_needs, relativas a O)
sd_cut <- function(pan, needs, O, tz) {
  day <- function(x) lubridate::as_date(x, tz = tz)
  pan$load <- pan$load[day(interval_start) >= O - needs$load$days_before]
  pan$weather_observed <- pan$weather_observed[day(valid_time) >= O - needs$weather_observed$days_before]
  pan$holidays <- pan$holidays[date >= O + needs$holidays$from & date <= O + needs$holidays$to]
  pan
}

sd_unlock_assign <- function(name, fn, env) {
  lk <- bindingIsLocked(name, env)
  if (lk) unlockBinding(name, env)
  assign(name, fn, envir = env)
  if (lk) lockBinding(name, env)
}

## captura o .Random.seed do globalenv logo apos o ajuste da DM(O) (`fn_name` do env do plugin), em `box$seed`
sd_seed_spy <- function(env, fn_name, box) {
  real <- get(fn_name, envir = env, inherits = FALSE)
  sd_unlock_assign(fn_name, function(...) {
    r <- real(...)
    box$seed <- get(".Random.seed", envir = globalenv()); box$n <- box$n + 1L
    r
  }, env)
  function() sd_unlock_assign(fn_name, real, env)
}

sd_count_spy <- function(eng, fns) {
  n <- new.env(); n$k <- 0L
  orig <- lapply(stats::setNames(fns, fns), function(f) get(f, envir = eng, inherits = FALSE))
  for (f in fns) local({ ff <- f; o <- orig[[ff]]; sd_unlock_assign(ff, function(...) { n$k <- n$k + 1L; o(...) }, eng) })
  list(count = function() n$k, restore = function() for (f in fns) sd_unlock_assign(f, orig[[f]], eng))
}

## O caller tem o .Random.seed conhecido; devolve list(value, seed_fit, caller_ok) de `run()` com o espiao do ajuste da DM(O)
sd_with_rng <- function(env, fn_name, run) {
  box <- new.env(); box$seed <- NULL; box$n <- 0L
  restore <- sd_seed_spy(env, fn_name, box); on.exit(restore(), add = TRUE)
  set.seed(7); before <- get(".Random.seed", envir = globalenv())
  val <- run()
  list(value = val, seed_fit = box$seed, n_fit = box$n, caller_ok = identical(get(".Random.seed", envir = globalenv()), before))
}

## --- dessem-linear -----------------------------------------------------------------------------------------------------------
sd_dl_O <- function() DL$DL8_O
## O linear usa o artefato base da fixture (feriado de flag 12 em O-5, dentro dos 15 dias; o feriado futuro do store sai do painel,
## como em dl_fx_panel): o SVM do perfil de dia especial do motor nao treina com as poucas amostras de uma fixture sintetica
## pequena (engine.dessem_tune_small_sample), entao os cenarios de feriado no horizonte sao do regdin (perfil de feriado por lm).
sd_dl_art <- function(nm) DL$dl_base_art()
sd_dl_panel <- function(nm, hhmm) {
  st <- DL$dl_fx_store(hhmm); cuts <- DL$dl_fx_cuts(st)
  list(st = st, cuts = cuts, long = DL$dl_fx_panel(st, cuts, "infer"))   # deck longo: history_days.train + 1 dias
}

## short_long(plugin, art, au): envelopes do deck de 15 dias e do deck longo (mesmo artefato, mesmo corte). `art` = list(art, nm).
short_long <- function(plugin, art, au, nm = "single", after_fit = NULL) {
  stopifnot(plugin %in% c("dessem-linear", "dessem-regdin"))
  hhmm <- format(au, "%H:%M")
  needs <- art$payload$inference_needs
  if (identical(plugin, "dessem-linear")) {
    d <- sd_dl_panel(nm, hhmm); e <- DL$dl_plugin()$env
    run <- function(panel) DL$dl_infer(art, d$st, d$cuts, panel = panel)
    short_panel <- sd_cut(d$long, needs, d$st$O, DL$DL_TZ)
    fit <- "dl_dm_o_fit"
  } else {
    O <- RD$RD2_O; e <- RD$rd2_plugin()$env
    long <- sd_rd_panel(nm, au); short_panel <- sd_cut(long, needs, O, RD$RD_TZ)
    d <- list(long = long)
    run <- function(panel) RD$rd3_infer(RD$rd2_plugin(), art, au, pan = panel)       # sem .bypass_window_guard
    fit <- "rd_dm_nowcast"
  }
  eng <- parent.env(e)
  go <- function(panel) {
    spy <- sd_count_spy(eng, c("getNormalizaCarga", "getNormalizaExogena")); on.exit(spy$restore(), add = TRUE)
    r <- sd_with_rng(e, fit, function() run(panel))
    r$n_norm <- spy$count()
    r
  }
  s <- go(short_panel)
  l <- go(d$long)
  list(short = s$value, long = l$value, seed = list(short = s$seed_fit, long = l$seed_fit),
       n_fit = c(short = s$n_fit, long = l$n_fit), n_norm = c(short = s$n_norm, long = l$n_norm),
       caller_ok = c(short = s$caller_ok, long = l$caller_ok), short_panel = short_panel)
}

## --- dessem-regdin -----------------------------------------------------------------------------------------------------------
sd_rd_panel <- function(nm, au, days = RD$RD2_D) {
  pan <- RD$rd3_panel(until = au, days = days)
  pan$holidays <- sd_holidays("SE", RD$RD2_O, sd_cases[[nm]])
  pan
}
sd_rd_art <- function(nm) RD$rd2_cached(paste("sd-rd-art", nm), {
  p <- RD$rd2_plugin()
  RD$rd_with_cli(suppressMessages(RD$rd_lib$prevcarga_plugin_train(p, sd_rd_panel(nm, NULL), RD$rd3_cuts(RD$rd3_au(9L)),
                                                                   RD$rd2_spec("MED"), ctx = RD$rd2_ctx())))
})

sd_art <- function(plugin, nm) if (identical(plugin, "dessem-linear")) sd_dl_art(nm) else sd_rd_art(nm)
sd_au <- function(plugin, hhmm) {
  if (identical(plugin, "dessem-linear")) DL$dl_au(hhmm) else RD$rd3_au(as.integer(substr(hhmm, 1L, 2L)))
}

sd_expect_same <- function(r, plugin, hhmm) {
  expect_identical(r$short$values, r$long$values)
  expect_identical(r$short$delivery, r$long$delivery)
  expect_identical(r$short$meta, r$long$meta)
  expect_identical(dim(r$short$values), c(9L, 48L))
  expect_false(anyNA(r$short$values)); expect_true(all(is.finite(r$short$values)))
  if (identical(plugin, "dessem-linear"))
    expect_identical(r$short$meta$dm_o_mode, if (hhmm == "09:00") "nowcast" else "no_day_o")
  else expect_identical(r$short$meta$head_used, hhmm == "09:00")
}

## --- AC1: deck de 15 dias == deck longo, nos dois plugins e nos dois modos --------------------------------------------------
for (pl in c("dessem-linear", "dessem-regdin")) for (hhmm in sd_modes) {
  test_that(sprintf("[PLUGINS] short-deck ac-0: %s deck de 15 dias == deck longo em %s (feriado unico, values/delivery/meta identical)", pl, hhmm), {
    art <- sd_art(pl, "single")
    r <- short_long(pl, art, sd_au(pl, hhmm), "single")
    sd_expect_same(r, pl, hhmm)
    ## o deck curto e de fato curto: nenhuma carga antes de O - 15 e o calendario so em [O-15, O+9]
    O <- if (pl == "dessem-linear") sd_dl_O() else RD$RD2_O
    tz <- if (pl == "dessem-linear") DL$DL_TZ else RD$RD_TZ
    expect_gte(min(lubridate::as_date(r$short_panel$load$interval_start, tz = tz)), O - 15L)
    expect_true(all(r$short_panel$holidays$date >= O - 15L))
  })
}

## --- AC1/AC2: cenarios de feriado: trio formato Carnaval, dois dias seguidos do mesmo codigo, feriado no ultimo dia entregue ----
for (pl in "dessem-regdin") for (nm in c("dbl", "trio", "last")) {
  test_that(sprintf("[PLUGINS] short-deck ac-1: %s feriados '%s' no horizonte: deck de 15 dias == deck longo (sem bypass da guarda; 09:00 e 00:00)", pl, nm), {
    art <- sd_art(pl, nm); hz <- sd_cases[[nm]]
    O <- RD$RD2_O
    for (hhmm in sd_modes) {
      r <- short_long(pl, art, sd_au(pl, hhmm), nm)
      sd_expect_same(r, pl, hhmm)
      ## as 48 posicoes de cada dia de feriado entregue sao identical() as do deck longo
      for (k in as.integer(names(hz))) {
        expect_identical(r$short$values[k + 1L, ], r$long$values[k + 1L, ])
        expect_true(all(is.finite(r$short$values[k + 1L, ])))
      }
      expect_true(all((O + as.integer(names(hz))) %in% r$short_panel$holidays$date))
      expect_identical(sum(r$short_panel$holidays$prevcarga_code %in% sd_hist_codes & r$short_panel$holidays$date < O), 0L)
    }
  })
}

## --- AC2: CONTROL: sem o estado do artefato o deck curto muda a saida (o teste tem dentes) ---------------------------------------
test_that("[PLUGINS] short-deck ac-1: CONTROL: o deck curto so e igual ao longo porque o estado vem do artefato (alterar a amostra grava delta)", {
  art <- sd_art("dessem-linear", "single")
  alt <- art; alt$payload$dm_o_sample$Carga <- alt$payload$dm_o_sample$Carga * 1.4
  d <- sd_dl_panel("single", "09:00")
  short <- sd_cut(d$long, art$payload$inference_needs, d$st$O, DL$DL_TZ)
  ok <- DL$dl_infer(art, d$st, d$cuts, panel = short)
  ctl <- DL$dl_infer(alt, d$st, d$cuts, panel = short)
  expect_gt(max(abs(ctl$values[1L, ] - ok$values[1L, ])), 1e-5)
})

## --- AC3: espioes e RNG ---------------------------------------------------------------------------------------------------
for (pl in c("dessem-linear", "dessem-regdin")) {
  test_that(sprintf("[PLUGINS] short-deck ac-2: %s getNormaliza* com 0 chamadas no deck curto; .Random.seed apos o ajuste da DM(O) identico; RNG do chamador restaurado", pl), {
    art <- sd_art(pl, "single")
    eng <- if (pl == "dessem-linear") DL$dl_engine_env(DL$dl_plugin()) else parent.env(RD$rd2_plugin()$env)
    for (hhmm in sd_modes) {
      r <- short_long(pl, art, sd_au(pl, hhmm), "single")
      expect_identical(r$n_norm[["short"]], 0L)                           # a normalizacao e a do artefato
      expect_identical(r$n_fit[["short"]], 1L); expect_identical(r$n_fit[["long"]], 1L)   # um ajuste da DM(O) por inferencia
      expect_false(is.null(r$seed$short))
      expect_identical(r$seed$short, r$seed$long)
      expect_true(r$caller_ok[["short"]]); expect_true(r$caller_ok[["long"]])
    }
    ## CONTROL: o espiao enxerga uma chamada direta
    spy <- sd_count_spy(eng, "getNormalizaCarga"); on.exit(spy$restore(), add = TRUE)
    eng$getNormalizaCarga(list(flag = TRUE, oper = "max", const = 1), data.table::data.table(Carga = c(1, 2)))
    expect_identical(spy$count(), 1L)
  })
}

## --- AC4: pureza e artefato v0.1.5 ----------------------------------------------------------------------------------------
test_that("[PLUGINS] short-deck ac-3: scan_pure verde nos dois congelados; payload v0.1.5 recusado com *_artifact_mismatch (exit 2) nomeando o campo, sem previsao", {
  dl_art <- sd_art("dessem-linear", "single"); rd_art <- sd_art("dessem-regdin", "single")
  dl_p <- DL$dl_plugin(); rd_p <- RD$rd2_plugin()
  expect_error(DL$dl_with_cli(DL$dl_lib$prevcarga_plugin_scan_pure(dl_p$env$dl_freeze(dl_art$payload))), NA)
  expect_error(RD$rd_with_cli(RD$rd_lib$prevcarga_plugin_scan_pure(rd_p$env$rd_freeze(rd_art$payload))), NA)
  ## o payload v0.1.5 e o atual sem o estado derivado; a inferencia o recusa antes de qualquer previsao
  old <- function(art, drop) { a <- art; a$payload <- a$payload[setdiff(names(a$payload), drop)]; a }
  d <- sd_dl_panel("single", "09:00")
  short <- sd_cut(d$long, dl_art$payload$inference_needs, d$st$O, DL$DL_TZ)
  got <- NULL
  cond <- tryCatch({ got <- DL$dl_infer(old(dl_art, c("dm_o_sample", "dm_o_formula", "columns", "inference_needs")), d$st, d$cuts,
                                         panel = short); NULL }, prevcarga_error = function(e) e)
  expect_null(got); expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.dessem_linear_artifact_mismatch")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
  expect_identical(cond$details$field, "dm_o_sample")
  au <- RD$rd3_au(9L); rshort <- sd_cut(sd_rd_panel("single", au), rd_art$payload$inference_needs, RD$RD2_O, RD$RD_TZ)
  got <- NULL
  cond <- tryCatch({ got <- RD$rd3_infer(rd_p, old(rd_art, c("serie_dm", "hol_profile", "columns", "inference_needs")), au, pan = rshort); NULL },
                   prevcarga_error = function(e) e)
  expect_null(got); expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_artifact_mismatch")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
  expect_identical(cond$details$field, "serie_dm")
  ## CONTROL: o payload completo infere com o mesmo deck curto
  expect_true(is.list(RD$rd3_infer(rd_p, rd_art, au, pan = rshort)))
})

## --- borda: deck curto que termina antes de train_window$end (regdin recusa) -----------------------------------------------
test_that("[PLUGINS] short-deck borda: regdin com deck de 15 dias que termina em O-2 e recusado (window_mismatch, exit 5) e nao infere", {
  art <- sd_art("dessem-regdin", "single"); p <- RD$rd2_plugin(); au <- RD$rd3_au(9L); O <- RD$RD2_O
  pan <- sd_cut(sd_rd_panel("single", au), art$payload$inference_needs, O, RD$RD_TZ)
  cu <- RD$rd3_cuts(au); cu$last_data_day <- O - 2L
  pan$load <- pan$load[lubridate::as_date(interval_end - 1) <= O - 2L | lubridate::as_date(interval_start) >= O]
  pan$weather_observed <- pan$weather_observed[lubridate::as_date(valid_time) <= O - 2L | lubridate::as_date(valid_time) >= O]
  pan$weather_forecast <- pan$weather_forecast[issue_time <= au]
  m <- RD$rd_model_panel(pan, cu)
  frame <- RD$rd_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  inf <- RD$rd_with_cli(suppressMessages(p$descriptor$stages$inference_features$fn(frame, art$payload)))
  got <- NULL
  cond <- tryCatch({ got <- RD$rd_with_cli(p$env$plugin_infer(art$payload, inf, m$cuts)); NULL }, prevcarga_error = function(e) e)
  expect_null(got); expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_window_mismatch")
  expect_identical(cond$exit_code, 5L); expect_identical(cond$class_name, "window")
  expect_identical(cond$details$expected_end, format(art$payload$train_window$end)); expect_identical(cond$details$deck_end, format(O - 2L))
})

## --- fachada: o caminho que le do store usa o inference_needs do artefato (ampliacao do escopo do E4-T4) -------------------
test_that("[PLUGINS] short-deck fachada: dessem-linear le do store pelo inference_needs do artefato; deck do verbo de E2 e store dao o mesmo painel e a mesma previsao", {
  art <- sd_art("dessem-linear", "single"); needs <- art$payload$inference_needs
  pl <- DL$dl_plugin(); L <- DL$dl_lib
  for (hhmm in sd_modes) {
    st <- DL$dl_fx_store(hhmm); spec <- DL$dl_fx_spec(hist = st$hist)
    topo <- tempfile(fileext = ".yaml"); artdir <- tempfile("sd-art"); dir.create(artdir)
    out <- file.path(tempfile("sd-short"), "deck")
    on.exit(unlink(c(topo, artdir, dirname(out)), recursive = TRUE), add = TRUE)
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", sprintf("  X: {areas: [%s], loss: PX}", DL$DL8_AREA),
                 "order: [X]", "members:", "  - {sigla: DL, mdir: dessem-linear, exogena: MED}"), topo)
    ctx <- list(data_dir = st$dir, root = DL$dl_root(), topology_path = topo, weather_issue_cutoff = "available_until",
                origin = st$O, artifacts_dir = artdir)
    m <- as.list(L$prevcarga_members(path = topo, root = DL$dl_root())[1L, , drop = FALSE])
    key <- L$prevcarga_registry_key(m, DL$DL8_AREA, ctx)
    L$prevcarga_registry_publish(list(model = "dessem-linear", inference_needs = needs), list(origin = format(st$O)), key, "v_1")
    L$prevcarga_registry_promote_latest(key, "v_1")
    res <- DL$dl_with_cli(L$prevcarga_deck_inference_build(ctx, st$O, DL$DL8_AREA, out))
    expect_identical(res$result$inference_needs_source, "artifacts")
    expect_false(dir.exists(file.path(out, "holiday_annex")))                 # holiday_annex FALSE no artefato: sem anexo
    ctx_short <- ctx; ctx_short$data_dir <- out
    cuts_st <- L$.prevcarga_inf_cuts(pl, spec, st$O, ctx, "dessem-linear")
    cuts_dk <- L$.prevcarga_inf_cuts(pl, spec, st$O, ctx_short, "dessem-linear")
    rd <- function(cx, cu, ...) DL$dl_with_cli(suppressWarnings(L$prevcarga_infer_read_panel(pl, spec, cu, DL$DL8_AREA, st$O, cx, ...)))
    p_long <- rd(ctx, cuts_st)                                                 # leitor atual: history_window (anexo)
    p_store <- rd(ctx, cuts_st, needs = needs, artifact_needs = TRUE)          # fachada nova: o que o artefato pede
    p_deck <- rd(ctx_short, cuts_dk, needs = needs, artifact_needs = TRUE)     # deck do verbo de E2
    expect_gt(nrow(p_long$panel$load), nrow(p_store$panel$load))               # o store longo deixou de ser lido
    expect_identical(p_store$panel$load, p_deck$panel$load)
    expect_identical(p_store$panel$weather_observed, p_deck$panel$weather_observed)
    expect_identical(p_store$panel$weather_forecast, p_deck$panel$weather_forecast)
    expect_identical(p_store$cuts$available_until, p_deck$cuts$available_until)
    no_future <- function(p) { p$panel$holidays <- p$panel$holidays[date < st$O]; p }   # a fixture do linear treina sem feriado em O..O+9
    e <- lapply(list(long = p_long, store = p_store, deck = p_deck), function(p) {
      p <- no_future(p)
      DL$dl_with_cli(suppressWarnings(suppressMessages(L$prevcarga_plugin_infer(pl, art, p$panel, p$cuts, DL$dl_fx_ctx_run(p$cuts)))))
    })
    expect_identical(e$store$values, e$long$values); expect_identical(e$deck$values, e$long$values)
    expect_identical(e$store$delivery, e$long$delivery); expect_identical(e$deck$delivery, e$long$delivery)
    expect_identical(e$deck$meta, e$long$meta)
    expect_identical(dim(e$deck$values), c(9L, 48L))
    ## CONTROL: sem artifact_needs a fachada segue lendo o store longo (comportamento anterior preservado)
    expect_identical(rd(ctx, cuts_st, needs = needs)$panel$load, p_long$panel$load)
  }
})
