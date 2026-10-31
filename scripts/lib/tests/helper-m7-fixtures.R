## helper-m7-fixtures.R -- fixtures do M7 por formula (M7.2 cria; M7.x acrescentam). Auto-carregado por testthat.
## Nao e codigo de produto. Sem variaveis de ambiente; tudo em tempdir (nunca data/); datas por lubridate com tz.

PC_M7_TZ <- "Etc/GMT+3"

## ctx de fixture: state_dir/data_dir em tempdir + config.yaml de fixture; limpeza ao fim do frame do chamador.
pc_m7_ctx <- function(.local_envir = parent.frame()) {
  d <- withr::local_tempdir("pc-m7-", .local_envir = .local_envir)
  state <- file.path(d, "state"); data <- file.path(d, "deck-store")
  dir.create(state); dir.create(data)
  cfg <- file.path(d, "config.yaml")
  yaml::write_yaml(list(state_dir = state, data_dir = data), cfg)
  list(state_dir = state, data_dir = data, config_path = cfg, root = pc_root())
}

## spec de run.yaml em memoria (so os campos que o M7 consome)
pc_m7_spec <- function(split = "origin_agnostic", horizon_days = 8L, seed = 42L, granularity_min = 60L,
                       protocol_default = "daily") {
  list(name = "fixture", split = split, horizon_days = as.integer(horizon_days), seed = as.integer(seed),
       granularity_min = as.integer(granularity_min), protocol_default = protocol_default)
}

## siglas do M7 como (mdir, exogena); exogena NA quando o membro nao tem variante
pc_m7_members <- function() {
  data.frame(sigla = c("RF", "LGBM", "HW", "LIN_MED", "LIN_MAX", "LIN_MXMN", "RGD_MED", "RGD_MAX"),
             mdir = c("random-forest", "lgbm", "holt-winters", rep("dessem-linear", 3L), rep("dessem-regdin", 2L)),
             exogena = c(NA, NA, NA, "MED", "MAX", "MXMN", "MED", "MAX"), stringsAsFactors = FALSE)
}

## rotulo de membro (mdir, exogena) a partir da sigla
pc_m7_label <- function(sigla) {
  m <- pc_m7_members()
  m <- m[m$sigla == sigla, , drop = FALSE]
  list(mdir = m$mdir, exogena = m$exogena)
}

## topologia de fixture: 1 sistema e `n_leaves` folhas (nomes por formula)
pc_m7_topology <- function(n_leaves = 2L) {
  leaves <- sprintf("L%02d", seq_len(n_leaves))
  list(system = "SIS", leaves = leaves)
}

## corte intradiario de fixture: instante HH:MM do dia `day` no tz da carga
pc_m7_au <- function(day, hm) lubridate::ymd_hm(paste(day, hm), tz = PC_M7_TZ)

## deck sintetico com manifest.json (fixture_store.R): um dia por origem, ate a maior origem
pc_m7_deck <- function(origins, issues = c("00", "06", "18"), areas = c("A", "B"), seed = 7L,
                       .local_envir = parent.frame()) {
  env <- new.env(parent = globalenv())
  for (m in c("errors")) sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = env)
  sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
  origins <- sort(origins)
  days <- seq(min(origins) - 12L, max(origins), by = "day")
  dir <- file.path(withr::local_tempdir("pc-m7-deck-", .local_envir = .local_envir), "deck")
  env$prevcarga_fixture_deck(dir, seed = seed, areas = areas, days = days)
  dir
}

## --- scaffold dos gates de harness do M7.V (test-harness-*.R) ------------------------------------------------------------
## Raiz-brinquedo com plugins das duas classes, deck por formula e modulos da fundacao num env; tudo em tempdir e memoizado por
## sessao de teste. Os brinquedos sao sensiveis a ultima carga VISTA (y_pred = nivel do artefato + ultima carga); `noise` soma um
## sorteio SEM semente propria (a semente vem do run.yaml, pelo orquestrador) para o gate de determinismo.
PC_M7H_MODULES <- c("errors", "runtime", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
                    "features", "registry", "training", "inference", "build_long_output", "evaluation", "honesty_gate", "combination",
                    "reconcile", "cache", "reporting", "orchestrate", "backtest")
PC_M7H_AREAS <- c("A", "B", "X")
.pc_m7h <- new.env()

.pc_m7h_sens <- function(slot_min, noise) {
  nz <- if (noise) " + stats::runif(length(ie))" else ""
  if (slot_min == 30L)
    c("toy_inference_features <- function(frame, payload) list(area = frame$load$area_code[1L], last = utils::tail(frame$load$load_mwh, 1L))",
      "toy_inference_core <- function(payload, inf_features, cuts) {",
      "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 2L), by = \"30 min\")",
      "  list(area = inf_features$area, values = payload$model$level,",
      sprintf("       delivery = data.table::data.table(interval_end = ie, y_pred = payload$model$level[toy_slots(ie)] + inf_features$last%s))", nz),
      "}")
  else
    c("toy_inference_features <- function(frame, payload) list(area = frame$area, last = utils::tail(frame$load$load_mwh, 1L))",
      "toy_inference_core <- function(payload, inf_features, cuts) {",
      "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 9L), by = \"30 min\")",
      "  list(area = inf_features$area, values = payload$model$level,",
      sprintf("       delivery = data.table::data.table(interval_end = ie, y_pred = rep(payload$model$level, length(ie)) + inf_features$last%s))", nz),
      "}")
}

## toy-h: horario, origin_agnostic H=9; toy-s: semi-horario, origin_agnostic H=1, data_lag_days 2; toy-sc: horario, origin_scoped.
## Topologia: subsistema X = {A, B}. `proto`: backtest.protocol_default de cada um.
pc_m7h_root <- function(noise = FALSE, proto = c(h = "daily", s = "daily", sc = "daily")) {
  key <- paste(c(noise, proto), collapse = "|")
  if (!is.null(.pc_m7h$root[[key]])) return(.pc_m7h$root[[key]])
  td <- function(base, name, slot_min, run_yaml = list(), descriptor = list())
    pc_toy_variant(base, run_yaml = run_yaml, descriptor = descriptor, name = name,
                   files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines(base, 0L, 0L, character(), "ok"),
                                "R/zz_sens.R" = .pc_m7h_sens(slot_min, noise)))
  rng_d <- if (noise) list(uses_rng = TRUE) else list()
  root <- pc_install_copy(mutate = function(r) {
    tvs <- list(
      td("toy-60", "toy-h", 60L, run_yaml = list(split = "origin_agnostic", modes = c("train", "infer"), horizon_days = 9L, seed = 11L,
                                                 backtest = list(protocol_default = proto[["h"]])),
         descriptor = eval(substitute(function(d) { d$modes_implemented <- c("train", "infer"); d$stages$forecast <- NULL
                                                    d$model_time_offset_min <- 0L; d$uses_rng <- NZ; d }, list(NZ = noise)))),
      td("toy-30", "toy-s", 30L, run_yaml = list(horizon_days = 1L, seed = 23L, backtest = list(protocol_default = proto[["s"]])),
         descriptor = list(data_lag_days = 2L, uses_rng = TRUE)),
      td("toy-60", "toy-sc", 60L, run_yaml = list(seed = 37L, backtest = list(protocol_default = proto[["sc"]])),
         descriptor = c(list(model_time_offset_min = 0L), rng_d)))
    for (tv in tvs) for (n in setdiff(list.files(tv), "_toy-engine")) file.copy(file.path(tv, n), file.path(r, "models"), recursive = TRUE)
    file.copy(file.path(tvs[[1L]], "_toy-engine"), file.path(r, "models"), recursive = TRUE)
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [A, B], loss: PX}",
                 "order: [X]", "members:",
                 "  - {sigla: MH, mdir: toy-h, exogena: null}",
                 "  - {sigla: MS, mdir: toy-s, exogena: null}",
                 "  - {sigla: MC, mdir: toy-sc, exogena: null}"),
               file.path(r, "config", "topology.yaml"))
  })
  .pc_m7h$root[[key]] <- root
  root
}

## deck sintetico (areas A, B, X) de `first` a `last` (Date), memoizado
pc_m7h_deck <- function(first, last) {
  key <- paste(format(first), format(last))
  if (!is.null(.pc_m7h$deck[[key]])) return(.pc_m7h$deck[[key]])
  env <- new.env(parent = globalenv())
  sys.source(file.path(pc_root(), "scripts", "lib", "errors.R"), envir = env)
  sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
  d <- tempfile("m7h-deck-")
  env$prevcarga_fixture_deck(d, seed = 5, areas = PC_M7H_AREAS, days = seq(first, last, by = "day"))
  .pc_m7h$deck[[key]] <- d
  d
}

## modulos da fundacao num env (como o test-backtest.R); o runtime e fixado in-process
pc_m7h_mod <- function() {
  if (!is.null(.pc_m7h$mod)) return(.pc_m7h$mod)
  lib <- file.path(pc_root(), "scripts", "lib")
  options(prevcarga.lib = lib)
  mod <- new.env()
  for (m in PC_M7H_MODULES) sys.source(file.path(lib, paste0(m, ".R")), envir = mod)
  suppressMessages(mod$prevcarga_runtime_enforce())
  .pc_m7h$mod <- mod
  mod
}

## ctx de backtest com state_dir novo em tempdir (limpo ao fim do frame do chamador); chaves iguais entre decks perturbados
pc_m7h_ctx <- function(root, data, n_min = 3L, .local_envir = parent.frame()) {
  mod <- pc_m7h_mod()
  state <- withr::local_tempdir("m7h-state-", .local_envir = .local_envir)
  ctx <- mod$prevcarga_ctx(list("--data" = data, "--state" = state), root, "backtest")
  ctx$code_commit <- "c0ffee"
  ctx$data_hash <- "deck-fixed"
  ctx$progress <- function(done, total) NULL
  ctx$combina <- list(n_min = as.integer(n_min))
  ctx
}

## troca binding do env dos modulos e restaura ao fim do frame
pc_m7h_mock <- function(..., .local_envir = parent.frame()) {
  force(.local_envir)                      # parent.frame() preguicoso dentro de local() aponta para o frame errado
  mod <- pc_m7h_mod()
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- mod[[nm]]; withr::defer(assign(nm, old, envir = mod), envir = .local_envir) })
  for (n in names(fs)) assign(n, fs[[n]], envir = mod)
}

## roda `body` (linhas R) num R novo com a lib em globalenv (como a CLI); `ctx` pronto; devolve status/stderr e o `out` salvo em OUT
pc_m7h_child <- function(body, root, deck, state, env = NULL, hooks = "NULL", n_min = 3L) {
  lib <- file.path(pc_root(), "scripts", "lib")
  out <- tempfile("m7h-out-", fileext = ".rds")
  s <- tempfile("m7h-child-", fileext = ".R")
  on.exit(unlink(s), add = TRUE)
  writeLines(c(
    sprintf("lib <- %s; options(prevcarga.lib = lib)", deparse(lib)),
    sprintf("for (m in %s) source(file.path(lib, paste0(m, '.R')))", paste(deparse(PC_M7H_MODULES), collapse = "")),
    "prevcarga_runtime_enforce()",
    sprintf("ctx <- prevcarga_ctx(list(`--data` = %s, `--state` = %s), %s, 'backtest')", deparse(deck), deparse(state), deparse(root)),
    sprintf("ctx$lib_dir <- lib; ctx$code_commit <- 'c0ffee'; ctx$data_hash <- 'deck-fixed'; ctx$progress <- function(done, total) NULL; ctx$combina <- list(n_min = %dL)", as.integer(n_min)),
    sprintf("ctx$worker_hooks <- %s", hooks),
    sprintf("OUT <- %s", deparse(out)),
    body), s)
  r <- if (length(env)) pc_with_env(as.list(env), pc_rscript(s)) else pc_rscript(s)
  r$out <- if (file.exists(out)) readRDS(out) else NULL
  r
}

## --- coleta dos long.parquet de uma rodada (Step 1) ---------------------------------------------------------------------------
## data.table, uma linha por (estagio, membro|NA, area, forecast_origin, corte, target_datetime) ordenada por chave (radix), de modo
## que identical() compare rodadas. `root` = backtest/<run_id>/ (res$paths$root).
pc_m7_collect_long <- function(root) {
  rd <- function(f) data.table::as.data.table(arrow::read_parquet(f))
  tag <- function(d, estagio, membro, corte) {
    d <- data.table::copy(d)
    data.table::set(d, j = "estagio", value = estagio)
    data.table::set(d, j = "membro", value = membro)
    data.table::set(d, j = "corte", value = corte)
    d
  }
  parts <- list()
  for (f in sort(Sys.glob(file.path(root, "members", "*", "*", "*", "*", "long.parquet")), method = "radix")) {
    p <- strsplit(dirname(substring(f, nchar(root) + 10L)), "/", fixed = TRUE)[[1L]]
    parts[[length(parts) + 1L]] <- tag(rd(f), "member", p[[1L]], p[[4L]])
  }
  for (f in sort(Sys.glob(file.path(root, "combined", "*", "*", "combined.parquet")), method = "radix"))
    parts[[length(parts) + 1L]] <- tag(rd(f), "combined", NA_character_, basename(dirname(f)))
  for (f in sort(c(Sys.glob(file.path(root, "reconciled", "*", "*", "SIN.parquet")),
                   Sys.glob(file.path(root, "reconciled", "*", "*", "subsistemas", "*.parquet"))), method = "radix")) {
    corte <- basename(dirname(if (basename(dirname(f)) == "subsistemas") dirname(f) else f))
    parts[[length(parts) + 1L]] <- tag(rd(f), "reconciled", NA_character_, corte)
  }
  out <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  data.table::setorderv(out, c("estagio", "membro", "area", "forecast_origin", "corte", "target_datetime"), na.last = TRUE)
  out
}
