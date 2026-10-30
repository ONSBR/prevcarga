## helper-m6-fixtures.R -- base dos fixtures do M6 (M6.1; o M6.4 estende). Auto-carregado por test_dir.
## Nao e codigo de produto. Tudo por lubridate com tz explicito, sem env vars, sem I/O.
## Valor sentinela: y_pred/load_mwh codificam os minutos desde a origem do interval_start (pc_m6_minutes),
## de modo que qualquer deslocamento de rotulo muda o valor.

M6_TZ <- "Etc/GMT+3"

m6_fixture_ctx <- function(tz = M6_TZ) list(tz = tz)

m6_member <- function(name = "random-forest", delivery_days = 9L, granularity_min = 60L, split = "origin_agnostic")
  list(name = name, delivery_days = as.integer(delivery_days), granularity_min = as.integer(granularity_min), split = split)

m6_minutes <- function(t, origin, tz = M6_TZ)
  lubridate::time_length(t - lubridate::force_tz(lubridate::as_datetime(origin), tz), "minute")

## Entrega semi-horaria [available_until + 30 .. O + days 00:00] (ou `first`/`last` explicitos), valor sentinela
## = minutos do interval_start desde O. `shift_min`: desloca o carimbo (CONTROL de off-by-one).
m6_fixture_delivery <- function(member = m6_member(), origin = lubridate::ymd("2026-01-15"),
                                available_until = NULL, tz = M6_TZ, first = NULL, last = NULL, shift_min = 0L) {
  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), tz)
  if (is.null(available_until)) available_until <- t0 + lubridate::hours(9)
  if (is.null(first)) first <- available_until + lubridate::minutes(30)
  if (is.null(last)) last <- t0 + lubridate::days(member$delivery_days)
  ie <- seq(first, last, by = "30 min")
  data.table::data.table(interval_end = ie + lubridate::minutes(shift_min),
                         y_pred = m6_minutes(ie - lubridate::minutes(30), origin, tz))
}

## Truth do leitor unico (interval_end, load_mwh) = mesmo sentinela, rotulado por interval_end.
## `label`: "end" (correto) ou "start" (CONTROL: rotulado por interval_start, desloca 1 slot).
m6_fixture_truth <- function(origin = lubridate::ymd("2026-01-15"), days = 10L, tz = M6_TZ, label = "end", perturb = 0) {
  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), tz)
  start <- t0 + lubridate::minutes(30L * (seq_len(48L * days) - 1L))
  data.table::data.table(interval_end = if (identical(label, "end")) start + lubridate::minutes(30) else start,
                         load_mwh = m6_minutes(start, origin, tz) + perturb)
}

## =========================================================================================================
## M6.4 -- fixtures da rodada operacional (forecast.R). Deck sintetico pequeno (scripts/tools/fixture_store.R), topologia
## minima (SS1 = A1+A2, SS2 = B1), 3 membros REAIS (2 origin_agnostic + 1 origin_scoped) e espioes de infer/train. O perfil
## k-hat e os pesos da combina sao treinados no setup (nao por forecast.R). `env` = ambiente com a lib e o fixture_store.
## =========================================================================================================
M6F_ORIGIN <- lubridate::ymd("2026-01-15")
M6F_LEAVES <- c("A1", "A2", "B1")
M6F_SS <- c("SS1", "SS2")
M6F_MEMBERS <- c("RF", "LGBM", "HW")            # RF/LGBM origin_agnostic, HW origin_scoped (run.yaml dos plugins reais)

m6f_topology_file <- function(dir, move_a2 = FALSE) {
  ss1 <- if (move_a2) "[A1]" else "[A1, A2]"
  ss2 <- if (move_a2) "[B1, A2]" else "[B1]"
  f <- file.path(dir, "topology.yaml")
  writeLines(c("schema_version: 1", "khat_range: [0.1, 5.0]", "subsystems:",
               sprintf("  SS1: {areas: %s, loss: P1}", ss1), sprintf("  SS2: {areas: %s, loss: P2}", ss2),
               "order: [SS1, SS2]", "members:",
               "  - {sigla: RF, mdir: random-forest, exogena: null}",
               "  - {sigla: LGBM, mdir: lgbm, exogena: null}",
               "  - {sigla: HW, mdir: holt-winters, exogena: null}"), f)
  f
}

## Deck de 40 dias ate O, carga ate O `until` (fuso M6_TZ); as 5 areas (folhas + subsistemas) carregam carga.
m6f_deck <- function(env, dir, until = "09:00", origin = M6F_ORIGIN, days = 40L, seed = 11) {
  env$prevcarga_fixture_deck(dir, seed = seed, areas = c(M6F_LEAVES, M6F_SS), days = seq(origin - (days - 1L), origin, by = "day"),
                             load_until = lubridate::ymd_hm(paste(origin, until), tz = M6_TZ), tz = M6_TZ)
  dir
}

m6f_ctx <- function(deck, state, topology, extra = list())
  utils::modifyList(list(root = pc_root(), data_dir = deck, topology_path = topology, state_dir = state,
                         weather_issue_cutoff = "available_until", version = "0.0.1", code_commit = "c0ffee",
                         generated_at = "2026-01-15T12:00:00Z"), extra)

## Entrega sentinela do membro `sigla` na area (grade de entrega do descriptor real, corte do leitor): a soma de um
## deslocamento por membro e por area ao sentinela de minutos. `perturb` = list(sigla = delta) move so aquele membro.
m6f_delivery <- function(env, member, area, origin, ctx, perturb = list()) {
  cuts <- env$prevcarga_resolve_cuts(ctx, origin = origin)
  dd <- as.integer(env$prevcarga_plugin_descriptor(member$mdir, env$prevcarga_models_root(ctx$root))$delivery_days)
  sg <- as.character(member$sigla)
  d <- m6_fixture_delivery(m6_member(sg, dd), origin, cuts$available_until, cuts$tz)
  off <- 10 * match(sg, M6F_MEMBERS) + 100 * match(area, M6F_LEAVES) + if (sg %in% names(perturb)) perturb[[sg]] else 0
  list(cuts = cuts, delivery_days = dd, predictions = data.table::data.table(target_datetime = d$interval_end, y_pred = d$y_pred + off))
}

## Espiao de prevcarga_infer_cell: registra (membro, area, version) em log$infer; `fail` = siglas que abortam
## state.artifact_missing (artefato ausente de origin_agnostic).
m6f_infer_spy <- function(env, log, perturb = list(), fail = character()) {
  function(member, area, origin, ctx, variant = NULL, version = NULL, pin = FALSE) {
    sg <- as.character(member$sigla)
    log$infer[[length(log$infer) + 1L]] <- list(member = sg, area = area, version = version)
    if (sg %in% fail) env$prevcarga_abort("state.artifact_missing", where = paste0(sg, "/", area))
    d <- m6f_delivery(env, member, area, origin, ctx, perturb)
    list(predictions = d$predictions,
         version = if (is.null(version)) paste0("v_", substr(digest::digest(paste(sg, area), algo = "sha256"), 1L, 16L)) else version,
         available_until_posix = d$cuts$available_until)
  }
}

## Artefato minimo no layout do registry (so o run_manifest com a origem: o que o localizador de origin_scoped le).
m6f_fake_artifact <- function(env, member, area, origin, ctx, tag = "x") {
  v <- paste0("v_", substr(digest::digest(paste(member$sigla, area, format(origin), tag), algo = "sha256"), 1L, 16L))
  d <- file.path(env$prevcarga_registry_key(member, area, ctx), v)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(list(origin = format(origin)), auto_unbox = TRUE), file.path(d, "run_manifest.json"))
  v
}

## Espiao de prevcarga_train: registra (membro, area, origem) em log$train e deixa um artefato da origem no registry.
m6f_train_spy <- function(env, log) {
  function(member, area, origin, ctx, force = FALSE) {
    log$train[[length(log$train) + 1L]] <- list(member = as.character(member$sigla), area = area, origin = origin)
    v <- m6f_fake_artifact(env, member, area, origin, ctx)
    data.frame(model = member$mdir, area = area, version = v, status = "trained", stringsAsFactors = FALSE)
  }
}

## Perfil k-hat + pesos da combina para as origens `origins` (setup: forecast.R nunca treina). k-hat pelo treino do
## Reconcile (janela curta), uma vez por origem; pesos pelo treino da combina sobre o painel que o proprio forecast monta
## (cada origem + uma copia em mes anterior => celulas EQW por fallback), um artefato por folha cobrindo todas as origens.
m6f_prepare_origin <- function(env, ctx, origins = M6F_ORIGIN, window_days = 30L) {
  mem <- env$prevcarga_members(path = ctx$topology_path, root = ctx$root)
  art_dir <- file.path(env$prevcarga_registry_root(ctx), "combina")
  dir.create(art_dir, recursive = TRUE, showWarnings = FALSE)
  for (o in as.list(origins)) suppressWarnings(env$prevcarga_reconcile_train(ctx, o, "all", window_days = window_days))
  for (a in M6F_LEAVES) {
    panels <- lapply(as.list(origins), function(origin) {
      g <- list(origin = origin, cuts = env$prevcarga_resolve_cuts(ctx, origin = origin))
      rctx <- ctx; rctx$tz <- g$cuts$tz; rctx$load_label <- g$cuts$load_label
      lm <- stats::setNames(lapply(seq_len(nrow(mem)), function(i) {
        d <- m6f_delivery(env, mem[i, ], a, origin, ctx)
        list(delivery = data.table::data.table(interval_end = d$predictions$target_datetime, y_pred = d$predictions$y_pred),
             member = list(name = mem$sigla[[i]], delivery_days = d$delivery_days))
      }), mem$sigla)
      panel <- env$.forecast_leaf_panel(rctx, lm, a, g, mem)$panel
      prior <- data.table::copy(panel)
      prior[, forecast_origin := format(origin - 40L)]
      prior[, ym := substr(forecast_origin, 1L, 7L)]
      list(panel = panel, prior = prior)
    })
    first <- panels[[1L]]$panel
    both <- data.table::rbindlist(c(lapply(panels, function(p) p$prior), lapply(panels, function(p) p$panel)), use.names = TRUE)
    both <- unique(both, by = c("area", "forecast_origin", "lead", "cut"))
    prov <- data.table::rbindlist(lapply(panels, function(p) attr(p$panel, "provenance")))
    data.table::setattr(both, "members_declared", attr(first, "members_declared"))
    data.table::setattr(both, "members_present", attr(first, "members_present"))
    data.table::setattr(both, "provenance", prov)
    cov <- unique(data.table::rbindlist(lapply(panels, function(p) attr(p$panel, "coverage"))))
    data.table::setattr(both, "coverage", cov)
    env$prevcarga_combina_train(a, both, art_dir, root = ctx$root, members = mem, trained_at = "2026-01-15T00:00:00Z")
  }
  invisible(TRUE)
}

## Perfil k-hat sintetico congelado com `khat_origin` arbitrario (ratio constante), promovido como latest.
m6f_profile <- function(env, ctx, ss, khat_origin, ratio = 0.55) {
  topo <- env$prevcarga_topology(path = ctx$topology_path, root = ctx$root)
  days <- seq(khat_origin - 40L, khat_origin - 1L, by = "day")
  st <- lubridate::ymd_hm(paste(format(days), "12:00"), tz = M6_TZ)
  tr <- data.frame(interval_start = st, interval_end = st + lubridate::minutes(30L), ratio = ratio)
  fit <- env$prevcarga_khat_fit(tr, khat_origin, 30L)
  meta <- list(ss = ss, khat_origin = khat_origin, window_days = fit$window_days, window_from = fit$window_from,
               window_to = fit$window_to, profile_keys = env$prevcarga_khat_keys, week_start = 7L,
               areas = topo$subsystems[[ss]]$areas, loss = topo$subsystems[[ss]]$loss, n_ratio = fit$n_ratio,
               load_label = "start", timezone = M6_TZ, data_hash = env$prevcarga_khat_data_hash(tr),
               code_commit = "abc", renv_lock_hash = strrep("a", 64))
  fz <- env$prevcarga_khat_freeze(fit, meta, ctx)
  suppressWarnings(env$prevcarga_khat_advance_latest(ss, fz$version, khat_origin, ctx))
  fz$version
}

## =========================================================================================================
## M6.4 -- ponta a ponta com os 8 membros REAIS num `Rscript --vanilla` FILHO (a lib no globalenv, como a CLI; o interino
## dos plugins busca a fundacao la). Deck de fixture_store.R (SP + XX, 460 dias ate O 09:30), historico curto por
## ctx$spec_overrides. O filho treina (setup) os origin_agnostic, o k-hat e os pesos da combina; depois roda prevcarga_forecast.
## `phase` "full" monta tudo e roda 2x; "rerun" (outro processo, mesmo `work`) so roda de novo.
## =========================================================================================================
M6E_O <- lubridate::ymd("2024-10-15")

m6e_child_script <- function(root, work, out, phase) {
  c(sprintf("ROOT <- %s", deparse(normalizePath(root, winslash = "/", mustWork = TRUE))),
    sprintf("WORK <- %s", deparse(work)), sprintf("OUT <- %s", deparse(out)), sprintf("PHASE <- %s", deparse(phase)),
    "pc_root <- function() ROOT",
    "source(file.path(ROOT, 'scripts', 'lib', 'tests', 'helper-porte-closure.R'))",
    "source(file.path(ROOT, 'scripts', 'lib', 'tests', 'helper-m6-fixtures.R'))",
    "m6e_child_main(ROOT, WORK, OUT, PHASE)")
}

m6e_child_main <- function(root, work, out, phase) {
  options(prevcarga.lib = file.path(root, "scripts", "lib"))
  pcc_child_lib(root)
  g <- globalenv()
  for (m in c("schema_long", "evaluation", "combination", "build_long_output", "reconcile", "exports", "forecast"))
    sys.source(file.path(root, "scripts", "lib", paste0(m, ".R")), envir = g)
  tz <- PCC_TZ; O <- M6E_O
  deckdir <- file.path(work, "deck")
  topo <- file.path(work, "topology.yaml")
  if (identical(phase, "train")) {
    d <- tempfile("m6e-deck-")
    au <- lubridate::ymd_hm("2024-10-15 09:30", tz = tz)
    prevcarga_fixture_deck(d, seed = PCC_SEED, areas = c("SP", "XX"), days = seq(O - (PCC_DECK_DAYS - 1L), O, by = "day"),
                           load_until = au, tz = tz)
    d <- pcc_day_effects(d)
    for (a in c("SP", "XX"))
      d <- prevcarga_fixture_set_holiday(d, a, O + 3L, c(holiday_type_id = 12L, prevcarga_code = 12L, simplified_code = 1L))
    file.copy(d, work, recursive = TRUE); file.rename(file.path(work, basename(d)), deckdir)
    real <- readLines(file.path(root, "config", "topology.yaml"), warn = FALSE)
    mem <- real[seq(grep("^members:", real), length(real))]
    writeLines(c("schema_version: 1", "khat_range: [0.1, 5.0]", "subsystems:", "  XX: {areas: [SP], loss: PX}", "order: [XX]", mem), topo)
  }
  ctx <- list(root = root, data_dir = deckdir, topology_path = topo, weather_issue_cutoff = PCC_CUTOFF,
              state_dir = file.path(work, "state"), code_commit = "c0ffee", renv_lock_hash = "ab12", version = "0.0.1",
              generated_at = "2024-10-15T12:00:00Z",
              spec_overrides = list("history_days.train" = 400L, "history_days.infer" = 400L))
  dir.create(ctx$state_dir, recursive = TRUE, showWarnings = FALSE)
  counts <- new.env(); counts$train <- 0L
  orig_train <- prevcarga_train
  assign("prevcarga_train", function(...) { counts$train <- counts$train + 1L; orig_train(...) }, envir = g)
  mem <- prevcarga_members(path = topo, root = root)
  res <- list(phase = phase)

  if (identical(phase, "train")) {
    ## origin_agnostic: o forecast nunca treina, entao o setup publica o artefato; o k-hat e treino do Reconcile
    for (i in seq_len(nrow(mem)))
      if (identical(prevcarga_model_spec(mem$mdir[[i]], root)$split, "origin_agnostic"))
        suppressWarnings(suppressMessages(orig_train(mem[i, ], "SP", O, ctx)))
    suppressWarnings(prevcarga_reconcile_train(ctx, O, "all", window_days = 300L))
    ## 1a chamada de prevcarga_forecast: sem pesos da combina aborta (exit 13), mas as 6 celulas origin_scoped ja treinaram
    ## DENTRO do forecast (so porque nao havia artefato de O)
    res$pre_error <- tryCatch(suppressWarnings(suppressMessages(prevcarga_forecast(ctx, skip_validate = TRUE))),
                              prevcarga_error = function(e) list(id = e$id, exit = e$exit_code))
    res$trained_in_forecast <- counts$train
  }

  if (identical(phase, "weights")) {
    ## pesos da combina treinados no setup sobre as entregas reais (2 origens em meses distintos => celulas EQW por fallback)
    lags <- prevcarga_plugin_data_lags(unique(mem$mdir), root)
    gg <- list(origin = O, cuts = prevcarga_resolve_cuts(ctx, origin = O, data_lag_days = lags))
    plan <- .forecast_plan(ctx, NULL)
    members <- suppressWarnings(suppressMessages(.forecast_members(ctx, plan, gg, mem)))
    rctx <- ctx; rctx$tz <- gg$cuts$tz; rctx$load_label <- gg$cuts$load_label
    panel <- .forecast_leaf_panel(rctx, members[["SP"]], "SP", gg, mem)$panel
    prior <- data.table::copy(panel)
    prior[, forecast_origin := format(O - 40L)]
    prior[, ym := substr(forecast_origin, 1L, 7L)]
    both <- data.table::rbindlist(list(prior, panel), use.names = TRUE)
    for (n in c("members_declared", "members_present", "provenance", "coverage"))
      if (!is.null(attr(panel, n))) data.table::setattr(both, n, attr(panel, n))
    art_dir <- file.path(prevcarga_registry_root(ctx), "combina")
    dir.create(art_dir, recursive = TRUE, showWarnings = FALSE)
    prevcarga_combina_train("SP", both, art_dir, root = root, members = mem, trained_at = "2024-10-15T00:00:00Z")
    res$trained_in_weights <- counts$train
  }

  if (phase %in% c("run", "rerun")) {
    tree <- function(d) {
      f <- sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
      stats::setNames(as.list(vapply(f, function(x) digest::digest(file = file.path(d, x), algo = "sha256"), "")), f)
    }
    go <- function(skip) tryCatch(
      prevcarga_forecast(ctx, niveis = "tudo", incluir_membros = TRUE, skip_validate = skip),
      prevcarga_error = function(e) list(error = list(id = e$id, exit = e$exit_code, where = e$where)))
    ## o deck sintetico (460 dias) e mais curto que a janela de producao dos run.yaml: o gate-0 real RECUSA (janela de
    ## historico, exit 5) -- a validacao roda e reprova; a rodada segue com skip_validate (o corte continua vindo do leitor)
    gate0 <- go(FALSE)
    skipped <- !is.null(gate0$error)
    r1 <- if (skipped) go(TRUE) else gate0
    res <- c(res, list(gate0_error = if (skipped) gate0$error else NULL, skip_validate_used = skipped, train_calls_run1 = counts$train))
    if (is.null(r1$error)) {
      res$run1 <- list(result = r1$result, warnings = r1$warnings, tree = tree(r1$result$saida))
      if (identical(phase, "run")) {
        counts$train <- 0L
        r2 <- go(skipped)
        res$run2 <- list(ja_publicado = r2$result$ja_publicado, versao = r2$result$versao, tree = tree(r2$result$saida),
                         train_calls = counts$train)
      }
    } else res$error <- r1$error
  }
  jsonlite::write_json(res, out, auto_unbox = TRUE, null = "null", digits = NA)
  invisible(res)
}

## Roda a fase num filho; `out-<phase>.json` existente (work reaproveitado, ex.: options(prevcarga.m6e_work)) e reusado.
m6e_run_child <- function(work, phase, root = pc_root()) {
  script <- file.path(work, paste0("child-", phase, ".R"))
  out <- file.path(work, paste0("out-", phase, ".json"))
  err <- file.path(work, paste0("err-", phase, ".txt"))
  if (file.exists(out))
    return(list(status = 0L, log = character(), json = jsonlite::read_json(out, simplifyVector = FALSE)))
  writeLines(m6e_child_script(root, work, out, phase), script)
  st <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)), stdout = err, stderr = err))
  list(status = as.integer(st), log = readLines(err, warn = FALSE),
       json = if (file.exists(out)) jsonlite::read_json(out, simplifyVector = FALSE) else NULL)
}
