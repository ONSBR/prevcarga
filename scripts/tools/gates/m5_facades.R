## scripts/tools/gates/m5_facades.R -- pack de sub-gates [M5] pelas FACHADAS (M5.8), descoberto por prevcarga_gates_run (P.0h).
##
## Barrel por descoberta (ADR-0040 item 8): o runner de cada models/<m>/tests/run_all_gates.R chama so prevcarga_gates_run, que varre
## scripts/tools/gates/*.R e chama prevcarga_gate_pack_m5(model, ctx, tier). As linhas devolvidas (dono `M5`) SUBSTITUEM o slot
## `[DEFERRED M5] fachada` do runner. Nenhum runner e editado. `schema_long`/`ytrue_real` ([DEFERRED M6]) e `harness_*` ([DEFERRED M7])
## seguem SKIP com dono.
##
## Cada gate treina e infere SO pelas fachadas (prevcarga_train / prevcarga_infer_cell) sobre um deck de scripts/tools/fixture_store.R,
## no molde FORBIDDEN (delta exato 0 / sha256 igual) + CONTROL (delta medido != 0). Os bracos saem de campos do spec e do descriptor
## (split, granularity_min, modes, data_lag_days, reads_day_O, weather_forecast_in_train, uses_rng), nunca do nome do modelo.
## Os cortes (last_data_day, available_until, weather_issue_limit) vem das fachadas/dates.R: o gate nunca calcula corte de negocio.
##
## O cenario pesado de um modelo roda UMA vez num `Rscript --vanilla` filho (a API da fundacao fica no globalenv do filho, como na CLI;
## o processo do runner nao e sujado), com historico curto por ctx$spec_overrides. Sem variaveis de ambiente; linux e windows nativo.
##
## Linhas do pack: gate, owner, tier_min, applies_to, test (funcao que o runner executa: erro => FAIL).
## Ids: m5_determinism, m5_split_honesty, m5_intraday, m5_weather_reach, m5_day0_infer, m5_transparency.

PC5G_MODELS <- c("random-forest", "lgbm", "holt-winters", "dessem-linear", "dessem-regdin")
PC5G_GATES <- c("m5_determinism", "m5_split_honesty", "m5_intraday", "m5_weather_reach", "m5_day0_infer", "m5_transparency")
PC5G_TZ <- "Etc/GMT+3"
PC5G_AREA <- "SP"                      # folha da topologia real; unica com heat_area do LGBM
PC5G_HIST <- 60L                       # history_days.train do fixture (o run.yaml pede 365..1825)
PC5G_TOL <- 1e-5
PC5G_FOUNDATION <- c("runtime", "errors", "config", "provenance", "topology", "schema_long", "deck_reader", "validation", "dates",
                     "registry", "features", "training", "inference")

## --- tabela de bracos esperados (matriz de campos) e verificacao cruzada --------------------------------------------
## `arms`: lista nomeada por gate com os bracos que o modelo DEVE exibir (a partir de campos do spec/descriptor).
prevcarga_gate_pack_m5_expected <- function(spec, d, lag) {
  wx <- isTRUE(d$weather_forecast_in_train)
  list(
    m5_determinism = c("FORBIDDEN", if (isTRUE(d$uses_rng)) "CONTROL"),
    m5_split_honesty = c("FORBIDDEN", "CONTROL", if (lag > 0L) "FORBIDDEN_LAG",
                         if (identical(spec$split, "origin_scoped")) "DECKS"),
    m5_intraday = c("FORBIDDEN", "CONTROL"),
    m5_weather_reach = if (wx) c("FORBIDDEN_AU", "CONTROL_AU", "FORBIDDEN_MID", "CONTROL_MID") else "NOT_APPLICABLE",
    m5_day0_infer = if ("forecast" %in% spec$modes && isTRUE(d$reads_day_O)) c("MORNING", "FORBIDDEN") else "NOT_APPLICABLE",
    m5_transparency = c("TRAIN", "INFER"))
}

## rows: data.frame(gate, arm, ok, delta, detail); expected: saida de prevcarga_gate_pack_m5_expected.
## Falha (erro) se faltar braco FORBIDDEN/CONTROL esperado, se algum braco falhar ou se alguma linha vier SKIP.
prevcarga_gate_pack_m5_assert <- function(rows, model, expected, gate = NULL) {
  gates <- if (is.null(gate)) names(expected) else gate
  for (g in gates) {
    r <- rows[rows$gate == g, , drop = FALSE]
    miss <- setdiff(expected[[g]], r$arm)
    if (length(miss)) stop(sprintf("%s/%s: braco(s) ausente(s): %s", model, g, paste(miss, collapse = ",")), call. = FALSE)
    if (any(r$arm == "SKIP")) stop(sprintf("%s/%s: SKIP nao permitido em gate do M5", model, g), call. = FALSE)
    bad <- r[!r$ok, , drop = FALSE]
    if (nrow(bad))
      stop(sprintf("%s/%s: %s", model, g, paste(sprintf("[%s] %s", bad$arm, bad$detail), collapse = " ; ")), call. = FALSE)
  }
  invisible(TRUE)
}

## --- pack ----------------------------------------------------------------------------------------------------
prevcarga_gate_pack_m5 <- function(model, ctx, tier) {
  root <- normalizePath(file.path(ctx$gates_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
  pack <- normalizePath(file.path(ctx$gates_dir, "m5_facades.R"), winslash = "/", mustWork = TRUE)
  memo <- new.env(parent = emptyenv())
  scenario <- function() {
    if (is.null(memo$res)) memo$res <- pc5g_run_child(root, pack, model)
    memo$res
  }
  mk <- function(g) function() {
    s <- scenario()
    prevcarga_gate_pack_m5_assert(s$rows, model, s$expected, gate = g)
  }
  df <- data.frame(gate = PC5G_GATES, owner = "M5", tier_min = "fast", stringsAsFactors = FALSE)
  df$applies_to <- rep(list(PC5G_MODELS), nrow(df))
  df$test <- lapply(PC5G_GATES, mk)
  df
}

## O runner procura `prevcarga_gate_pack_<nome do arquivo>`; o nome do contrato do ticket e prevcarga_gate_pack_m5.
prevcarga_gate_pack_m5_facades <- function(model, ctx, tier) prevcarga_gate_pack_m5(model, ctx, tier)

## --- processo filho --------------------------------------------------------------------------------------------
pc5g_run_child <- function(root, pack, model) {
  work <- tempfile("pc5g-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  script <- file.path(work, "child.R")
  out <- file.path(work, "out.json")
  writeLines(c("a <- commandArgs(TRUE)", "sys.source(a[[1L]], envir = globalenv())", "pc5g_child_main(a[-1L])"), script)
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  c("--vanilla", shQuote(script), shQuote(pack), shQuote(root), shQuote(model), shQuote(out)),
                                  stdout = TRUE, stderr = TRUE))
  if (!file.exists(out))
    stop(sprintf("%s: cenario M5 nao produziu saida: %s", model, paste(utils::tail(log, 4L), collapse = " | ")), call. = FALSE)
  res <- jsonlite::read_json(out, simplifyVector = TRUE)
  rows <- res$rows
  rows$ok <- as.logical(rows$ok)
  rows$delta <- suppressWarnings(as.numeric(rows$delta))
  list(rows = rows, expected = lapply(res$expected, as.character))
}

pc5g_child_main <- function(args) {
  root <- args[[1L]]; model <- args[[2L]]; out <- args[[3L]]
  options(prevcarga.lib = file.path(root, "scripts", "lib"))
  g <- globalenv()
  for (m in PC5G_FOUNDATION) sys.source(file.path(root, "scripts", "lib", paste0(m, ".R")), envir = g)
  sys.source(file.path(root, "scripts", "tools", "fixture_store.R"), envir = g)
  prevcarga_runtime_enforce()
  work <- tempfile("pc5g-work-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  res <- pc5g_scenario(root, model, work)
  jsonlite::write_json(list(rows = res$rows, expected = res$expected), out, auto_unbox = FALSE, digits = NA, na = "string")
  invisible(NULL)
}

## --- fixture e execucao pelas fachadas ---------------------------------------------------------------------------
pc5g_O <- function() lubridate::ymd("2026-10-01")
pc5g_at <- function(hm, day = pc5g_O()) lubridate::ymd_hm(paste(format(day), hm), tz = PC5G_TZ)
pc5g_utc <- function(day, h) lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::hours(h)
pc5g_sec <- function(x, s) x + lubridate::seconds(s)

## Deck: PC5G_HIST + 25 dias ate O, areas SP e ZZ1. SP tem carga ate `until_sp`; ZZ1 ate `until_zz` (o available_until do leitor e
## o MINIMO entre as areas: SP "ragged" tem carga alem do corte, escondida pelo leitor). Dois feriados historicos de codigo 3 (o
## feriado futuro O+3 do fixture) e uma serie com ruido proprio (sem RNG) para o ARIMA do dessem-regdin nao degenerar.
pc5g_deck <- function(dir, until_sp, until_zz = until_sp) {
  O <- pc5g_O()
  days <- seq(O - (PC5G_HIST + 25L), O, by = "day")
  areas <- c("SP", "ZZ1")
  prevcarga_fixture_deck(dir, seed = 11, areas = areas, days = days, load_until = until_sp, tz = PC5G_TZ)
  if (!identical(until_sp, until_zz)) {
    short <- tempfile("pc5g-zz-")
    prevcarga_fixture_deck(short, seed = 11, areas = areas, days = days, load_until = until_zz, tz = PC5G_TZ)
    for (ds in c("load", "weather_observed")) {
      rel <- .PC_FX_DATASETS[[ds]]
      unlink(file.path(dir, rel, "area_code=ZZ1"), recursive = TRUE)
      file.copy(file.path(short, rel, "area_code=ZZ1"), file.path(dir, rel), recursive = TRUE)
    }
    unlink(short, recursive = TRUE)
  }
  for (k in 1:2) dir <- prevcarga_fixture_set_holiday(dir, "SP", O - 14L * k,
                                                      c(holiday_type_id = 1, prevcarga_code = 3, simplified_code = 1))
  for (a in c("ZZ1")) dir <- prevcarga_fixture_set_holiday(dir, a, O - 14L, c(holiday_type_id = 1, prevcarga_code = 3, simplified_code = 1))
  dir <- prevcarga_fixture_perturb(dir, "load", perturb = function(v) v + 40 * sin(v * 0.37))
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [SP, ZZ1], loss: PX}",
               "order: [X]", "members: []"), file.path(dir, "topology.yaml"))
  dir
}

pc5g_member <- function(root, model) {
  mem <- prevcarga_members(root = root)
  r <- mem[mem$mdir == model, , drop = FALSE][1L, ]
  list(sigla = r$sigla, mdir = r$mdir, exogena = r$exogena)
}

## ctx do leitor/fachadas: spec_overrides rapidos escolhidos por CAMPOS do spec/descriptor.
pc5g_ctx <- function(S, deck, state = NULL, rule = "available_until", seed_delta = 0L) {
  if (is.null(state)) {
    state <- tempfile("state-", tmpdir = S$work)
    dir.create(file.path(state, "artifacts"), recursive = TRUE)
  }
  ov <- list(history_days.train = PC5G_HIST)
  if (isTRUE(S$descriptor$history_infer_equals_train) || isTRUE(S$spec$constraints$infer_history_equals_train))
    ov$history_days.infer <- PC5G_HIST
  if (!is.null(S$spec$params$ntree)) ov$params.ntree <- 5L
  if (seed_delta != 0L) {                  # a semente do run.yaml e todo param de semente (ex.: kmeans_seed do HW)
    ov$seed <- as.integer(S$spec$seed + seed_delta)
    for (k in grep("seed", names(S$spec$params), value = TRUE))
      if (is.numeric(S$spec$params[[k]])) ov[[paste0("params.", k)]] <- as.integer(S$spec$params[[k]] + seed_delta)
  }
  list(root = S$root, data_dir = deck, topology_path = file.path(deck, "topology.yaml"), weather_issue_cutoff = rule,
       origin = pc5g_O(), state_dir = state, code_commit = "c0ffee", renv_lock_hash = "ab12", spec_overrides = ov)
}

pc5g_quiet <- function(expr) {
  v <- NULL
  utils::capture.output(v <- suppressWarnings(suppressMessages(expr)))
  v
}

pc5g_train <- function(S, deck, ...) {
  ctx <- pc5g_ctx(S, deck, ...)
  row <- pc5g_quiet(prevcarga_train(S$member, PC5G_AREA, pc5g_O(), ctx))
  sha <- digest::digest(file.path(row$path, "artifact.rds"), algo = "sha256", file = TRUE)
  list(ctx = ctx, row = row, sha = sha, version = row$version)
}

pc5g_infer <- function(S, ctx) {
  v <- if (is.na(S$member$exogena)) NULL else S$member$exogena
  pc5g_quiet(prevcarga_infer_cell(S$member, PC5G_AREA, pc5g_O(), ctx, variant = v))
}

## Delta maximo entre dois y_pred alinhados por target_datetime (estrutura diferente => Inf).
pc5g_delta <- function(a, b) {
  if (!identical(a$target_datetime, b$target_datetime)) return(Inf)
  max(0, abs(a$y_pred - b$y_pred))
}

pc5g_row <- function(gate, arm, ok, delta = NA_real_, detail = "")
  data.frame(gate = gate, arm = arm, ok = isTRUE(ok), delta = as.numeric(delta), detail = as.character(detail),
             stringsAsFactors = FALSE)

pc5g_fmt <- function(x) format(x, digits = 6)

## --- cenario ---------------------------------------------------------------------------------------------------
pc5g_scenario <- function(root, model, work) {
  O <- pc5g_O()
  plugin <- prevcarga_plugin_load(model, prevcarga_models_root(root))
  d <- plugin$descriptor
  spec <- prevcarga_model_spec(model, root)
  lag <- as.integer(if (is.null(d$data_lag_days)) 0L else d$data_lag_days)
  S <- list(root = root, work = work, descriptor = d, spec = spec, member = pc5g_member(root, model))
  deck <- pc5g_deck(file.path(work, "deck"), until_sp = pc5g_at("12:00"), until_zz = pc5g_at("09:00"))
  S$deck <- deck
  expected <- prevcarga_gate_pack_m5_expected(spec, d, lag)
  rows <- list()
  add <- function(x) rows[[length(rows) + 1L]] <<- x
  guard <- function(gate, fn) tryCatch(fn(), error = function(e)
    add(pc5g_row(gate, "ERROR", FALSE, detail = gsub("[\r\n]+", " ", conditionMessage(e)))))

  base <- tryCatch({ b <- pc5g_train(S, deck); b$inf <- pc5g_infer(S, b$ctx); b },
                   error = function(e) e)
  if (inherits(base, "error")) {
    for (g in names(expected)) add(pc5g_row(g, "ERROR", FALSE, detail = paste("treino/infer base:", gsub("[\r\n]+", " ", conditionMessage(base)))))
    return(list(rows = do.call(rbind, rows), expected = expected))
  }
  au <- base$inf$available_until_posix
  guard("m5_determinism", function() for (r in pc5g_determinism(S, base)) add(r))
  guard("m5_transparency", function() for (r in pc5g_transparency(S, base)) add(r))
  guard("m5_split_honesty", function() for (r in pc5g_split_honesty(S, base, lag)) add(r))
  guard("m5_intraday", function() for (r in pc5g_intraday(S, base, au)) add(r))
  guard("m5_weather_reach", function() for (r in pc5g_weather_reach(S, base, au)) add(r))
  guard("m5_day0_infer", function() for (r in pc5g_day0_infer(S, base, au)) add(r))
  list(rows = do.call(rbind, rows), expected = expected)
}

## --- gates ------------------------------------------------------------------------------------------------------
pc5g_determinism <- function(S, base) {
  g <- "m5_determinism"
  b <- pc5g_train(S, S$deck)
  b$inf <- pc5g_infer(S, b$ctx)
  dl <- pc5g_delta(base$inf$predictions, b$inf$predictions)
  out <- list(pc5g_row(g, "FORBIDDEN", dl <= PC5G_TOL && identical(base$sha, b$sha), dl,
                       sprintf("2 rodadas: delta=%s sha %s", pc5g_fmt(dl), if (identical(base$sha, b$sha)) "igual" else "DIFERENTE")))
  if (isTRUE(S$descriptor$uses_rng)) {
    c2 <- pc5g_train(S, S$deck, seed_delta = 1L)
    c2$inf <- pc5g_infer(S, c2$ctx)
    dc <- pc5g_delta(base$inf$predictions, c2$inf$predictions)
    ## o sorteio tem que ALCANCAR a saida (delta > tol); se o fixture nao a move (ex.: kmeans estavel do HW), a semente ao menos
    ## alcanca o artefato (sha256 diferente): o detalhe registra qual dos dois sustentou o braco.
    sha_moved <- !identical(base$sha, c2$sha)
    out <- c(out, list(pc5g_row(g, "CONTROL", dc > PC5G_TOL || sha_moved, dc,
                                sprintf("semente %d x %d: delta=%s, sha256 %s", S$spec$seed, S$spec$seed + 1L, pc5g_fmt(dc),
                                        if (sha_moved) "diferente" else "IGUAL"))))
  }
  out
}

## Fachada x chamada direta do plugin (identical) no treino e na entrega do infer.
pc5g_transparency <- function(S, base) {
  g <- "m5_transparency"
  inp <- pc5g_quiet(prevcarga_train_inputs(S$member, PC5G_AREA, pc5g_O(), base$ctx))
  pctx <- list(load_label = inp$cuts$load_label, timezone = inp$cuts$tz, weather_issue_cutoff = inp$cuts$weather_issue_cutoff)
  fit <- pc5g_quiet(prevcarga_plugin_train(inp$plugin, inp$panel, inp$cuts, inp$spec, pctx))
  direct <- prevcarga_artifact_build(S$member, PC5G_AREA, inp, fit)
  key <- prevcarga_registry_key(S$member, PC5G_AREA, base$ctx)
  fac <- prevcarga_load_artifact(key, base$version)
  t_ok <- identical(digest::digest(unclass(direct), algo = "sha256"), digest::digest(unclass(fac), algo = "sha256"))
  ## infer: a entrega da fachada x prevcarga_plugin_infer sobre o mesmo painel e artefato
  plugin <- prevcarga_plugin_load(S$member$mdir, prevcarga_models_root(S$root))
  spec <- fac$spec_efetivo
  cuts <- .prevcarga_inf_cuts(plugin, spec, pc5g_O(), base$ctx, S$member$mdir)
  rp <- pc5g_quiet(prevcarga_infer_read_panel(plugin, spec, cuts, PC5G_AREA, pc5g_O(), base$ctx))
  since <- pc5g_O() - as.integer(spec$history_days$infer)
  day_of <- function(x) lubridate::as_date(x, tz = rp$cuts$tz)
  win <- rp$panel
  win$load <- rp$panel$load[day_of(rp$panel$load$interval_start) >= since]
  if (NROW(rp$panel$weather_observed))
    win$weather_observed <- rp$panel$weather_observed[day_of(rp$panel$weather_observed$valid_time) >= since]
  plugin$spec <- spec
  env <- pc5g_quiet(prevcarga_plugin_infer(plugin, fac$fit, win, rp$cuts, list()))
  dir_pred <- data.table::data.table(target_datetime = env$delivery$interval_end, y_pred = env$delivery$y_pred)
  i_ok <- identical(as.data.frame(dir_pred), as.data.frame(base$inf$predictions))
  list(pc5g_row(g, "TRAIN", t_ok, detail = if (t_ok) "artefato da fachada identical ao do plugin direto" else "artefato da fachada DIVERGE do plugin direto"),
       pc5g_row(g, "INFER", i_ok, detail = if (i_ok) "entrega da fachada identical a do plugin direto" else "entrega da fachada DIVERGE do plugin direto"))
}

## Honestidade do treino por classe de split: o artefato so depende de carga ate cuts$last_data_day (do dates.R, via fachada).
pc5g_split_honesty <- function(S, base, lag) {
  g <- "m5_split_honesty"
  ldd <- base$row$last_data_day
  eod <- function(day) lubridate::ymd_hms(paste(format(day), "23:59:59"), tz = PC5G_TZ)
  bump <- function(after = NULL, at_or_before = NULL)
    prevcarga_fixture_perturb(S$deck, "load", after = after, at_or_before = at_or_before)
  run <- function(deck) pc5g_train(S, deck)
  fz <- run(bump(after = eod(ldd)))                                    # carga depois de last_data_day (inclui o dia O)
  ct <- run(bump(after = eod(ldd - 1L), at_or_before = eod(ldd)))      # o proprio last_data_day
  out <- list(
    pc5g_row(g, "FORBIDDEN", identical(fz$sha, base$sha), detail = sprintf("carga > last_data_day (%s): sha256 %s",
             format(ldd), if (identical(fz$sha, base$sha)) "igual" else "DIFERENTE")),
    pc5g_row(g, "CONTROL", !identical(ct$sha, base$sha), detail = sprintf("carga em last_data_day (%s): sha256 %s",
             format(ldd), if (!identical(ct$sha, base$sha)) "diferente" else "IGUAL (braco nao alcanca)")))
  if (lag > 0L) {
    lg <- run(bump(after = eod(ldd), at_or_before = eod(pc5g_O() - 1L)))   # so os dias entre last_data_day e O-1
    out <- c(out, list(pc5g_row(g, "FORBIDDEN_LAG", identical(lg$sha, base$sha),
             detail = sprintf("carga em (%s, O-1]: sha256 %s", format(ldd), if (identical(lg$sha, base$sha)) "igual" else "DIFERENTE"))))
  }
  if (identical(S$spec$split, "origin_scoped")) {
    d12 <- pc5g_deck(file.path(S$work, "deck12"), until_sp = pc5g_at("12:00"))
    t12 <- run(d12)
    same <- identical(t12$version, base$version) && identical(t12$sha, base$sha)
    out <- c(out, list(pc5g_row(g, "DECKS", same, detail = sprintf("decks de O com available_until 09:00 x 12:00: %s %s %s",
             base$version, if (same) "==" else "!=", t12$version))))
  }
  out
}

## Intradiario: carga do dia O depois do available_until nunca entra (FORBIDDEN); a que o membro le muda a entrega (CONTROL).
pc5g_intraday <- function(S, base, au) {
  g <- "m5_intraday"
  gran <- S$spec$granularity_min
  y0 <- base$inf$predictions
  infer_on <- function(deck) { ctx <- base$ctx; ctx$data_dir <- deck; ctx$topology_path <- file.path(deck, "topology.yaml")
                               pc5g_infer(S, ctx)$predictions }
  eod <- function(day) lubridate::ymd_hms(paste(format(day), "23:59:59"), tz = PC5G_TZ)
  fz <- prevcarga_fixture_perturb(S$deck, "load", after = pc5g_sec(au, -1), areas = PC5G_AREA)
  df <- pc5g_delta(y0, infer_on(fz))
  if (isTRUE(S$descriptor$reads_day_O)) {
    ct <- prevcarga_fixture_perturb(S$deck, "load", after = pc5g_sec(au, -60 * gran - 1), at_or_before = pc5g_sec(au, -1), areas = PC5G_AREA)
    where <- sprintf("%s-%s de O", format(au - lubridate::minutes(gran), "%H:%M"), format(au, "%H:%M"))
    dc <- pc5g_delta(y0, infer_on(ct))
  } else {
    ## o membro nao le o dia O no infer: o CONTROL e a carga de O-1, que entra pelo treino (retreino pela fachada)
    ct <- prevcarga_fixture_perturb(S$deck, "load", after = eod(pc5g_O() - 2L), at_or_before = eod(pc5g_O() - 1L), areas = PC5G_AREA)
    where <- "O-1 (o membro nao le o dia O; retreino)"
    tc <- pc5g_train(S, ct)
    dc <- pc5g_delta(y0, pc5g_infer(S, tc$ctx)$predictions)
  }
  list(pc5g_row(g, "FORBIDDEN", df == 0, df, sprintf("carga depois de available_until %s: delta=%s", format(au, "%H:%M"), pc5g_fmt(df))),
       pc5g_row(g, "CONTROL", dc > PC5G_TOL, dc, sprintf("carga em %s (granularidade %d): delta=%s", where, gran, pc5g_fmt(dc))))
}

## Clima previsto: emissao depois do limite nunca entra; a ultima permitida muda a entrega. Duas regras de weather_issue_cutoff.
pc5g_weather_reach <- function(S, base, au) {
  g <- "m5_weather_reach"
  if (!isTRUE(S$descriptor$weather_forecast_in_train))
    return(list(pc5g_row(g, "NOT_APPLICABLE", TRUE, detail = "o membro nao consome clima previsto (descriptor$weather_forecast_in_train)")))
  O <- pc5g_O()
  y_on <- function(ctx, deck) { ctx$data_dir <- deck; ctx$topology_path <- file.path(deck, "topology.yaml"); pc5g_infer(S, ctx)$predictions }
  bump <- function(after, upto = NULL) prevcarga_fixture_perturb(S$deck, "weather_forecast", issue_after = after,
                                                                  issue_at_or_before = upto)
  iss <- pc5g_utc(O, .PC_FX_ISSUE_HOURS)
  latest <- max(iss[iss <= au])
  out <- list()
  one <- function(tag, ctx, y0, forb, ctrl, rule) {
    df <- pc5g_delta(y0, y_on(ctx, forb)); dc <- pc5g_delta(y0, y_on(ctx, ctrl))
    list(pc5g_row(g, paste0("FORBIDDEN_", tag), df == 0, df, sprintf("regra %s: emissao depois do limite: delta=%s", rule, pc5g_fmt(df))),
         pc5g_row(g, paste0("CONTROL_", tag), dc > PC5G_TOL, dc, sprintf("regra %s: ultima emissao permitida: delta=%s", rule, pc5g_fmt(dc))))
  }
  out <- c(out, one("AU", base$ctx, base$inf$predictions, bump(au),
                    bump(pc5g_sec(latest, -1), latest), "available_until"))
  mid <- pc5g_train(S, S$deck, rule = "origin_midnight_utc")
  ym <- pc5g_infer(S, mid$ctx)$predictions
  z00 <- pc5g_utc(O, 0)
  out <- c(out, one("MID", mid$ctx, ym, bump(z00, pc5g_utc(O, 12)), bump(pc5g_sec(z00, -1), z00), "origin_midnight_utc"))
  out
}

## Manha de O no infer (dessem): a carga do dia O ate available_until entra SO pelo infer.
pc5g_day0_infer <- function(S, base, au) {
  g <- "m5_day0_infer"
  if (!("forecast" %in% S$spec$modes) || !isTRUE(S$descriptor$reads_day_O))
    return(list(pc5g_row(g, "NOT_APPLICABLE", TRUE, detail = "sem braco: o membro nao tem modo forecast com leitura do dia O (spec$modes, descriptor$reads_day_O)")))
  O <- pc5g_O()
  y0 <- base$inf$predictions
  morn <- prevcarga_fixture_perturb(S$deck, "load", after = pc5g_sec(pc5g_at("00:00"), -1), at_or_before = pc5g_sec(au, -1), areas = PC5G_AREA)
  after_cut <- prevcarga_fixture_perturb(S$deck, "load", after = pc5g_sec(au, -1), areas = PC5G_AREA)
  inf_on <- function(deck, state = NULL) { ctx <- pc5g_ctx(S, deck, state = base$ctx$state_dir); pc5g_infer(S, ctx)$predictions }
  dm <- pc5g_delta(y0, inf_on(morn))
  tm <- pc5g_train(S, morn)                                   # retreina com a manha perturbada: o artefato nao muda
  da <- pc5g_delta(y0, inf_on(after_cut))
  list(pc5g_row(g, "MORNING", dm > PC5G_TOL && identical(tm$sha, base$sha), dm,
                sprintf("manha de O <= available_until: infer delta=%s, artefato sha256 %s", pc5g_fmt(dm),
                        if (identical(tm$sha, base$sha)) "igual" else "DIFERENTE")),
       pc5g_row(g, "FORBIDDEN", da == 0, da, sprintf("depois de available_until: infer delta=%s", pc5g_fmt(da))))
}
