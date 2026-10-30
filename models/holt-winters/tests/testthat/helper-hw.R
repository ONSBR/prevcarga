## helper-hw.R -- fixtures dos testes do plugin Holt-Winters (HW.1; HW.2/HW.3 reaproveitam). Auto-carregado por test_dir.
## Nao e codigo de produto. Sem variaveis de ambiente; datas por lubridate com tz explicito; tudo em tempdir (nunca data/).
suppressWarnings(suppressMessages(library(testthat)))

hw_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("hw_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}

options(prevcarga.lib = file.path(hw_root(), "scripts", "lib"))

## fundacao (leitor, dates, loader, fixture_store) num env de teste, nunca no globalenv
hw_lib <- local({
  e <- new.env(parent = globalenv())
  for (m in c("errors", "runtime", "config", "topology", "deck_reader", "validation", "dates", "registry", "features"))
    sys.source(file.path(hw_root(), "scripts", "lib", paste0(m, ".R")), envir = e)
  sys.source(file.path(hw_root(), "scripts", "tools", "fixture_store.R"), envir = e)
  e
})

HW_TZ <- "Etc/GMT+3"
HW_AREA <- "ZZ1"
HW_FILES <- c("register.R", "ingestion.R", "feature_engineering.R", "helpers_aux.R", "training.R", "freeze.R",
              "inference_features.R", "inference.R")

## Plugin: os 8 arquivos de R/ por sys.source num env cujo pai e a fundacao (hw_abort/.hw_foundation acham
## prevcarga_condition e prevcarga_plugin_scan_pure, que na CLI vivem no global). O loader do P.0b (env sob baseenv) so e
## exercitado em test-register.R. Nunca o global.
hw_env <- function(root = hw_root()) {
  rdir <- file.path(root, "models", "holt-winters", "R")
  env <- new.env(parent = hw_lib)
  for (f in intersect(HW_FILES, list.files(rdir))) sys.source(file.path(rdir, f), envir = env, keep.source = FALSE,
                                                              toplevel.env = env)
  env
}

hw_spec <- function(root = hw_root()) hw_lib$prevcarga_model_spec("holt-winters", root)
hw_fx_spec <- hw_spec

## Deck sintetico (P.0e): area ZZ1, `days` dias terminando em O, emissoes 00Z/06Z/18Z, feriados, load_label start.
## `load_until` (POSIXct) corta a carga do dia O (infer); `sentinel`: valor = minutos do interval_start/valid_time.
.hw_store_cache <- new.env(parent = emptyenv())
hw_fx_store <- function(O = "2024-05-15", days = 60L, sentinel = FALSE, load_until = NULL, seed = 20241015L) {
  O <- lubridate::ymd(O)
  key <- paste(format(O), days, sentinel, if (is.null(load_until)) "" else format(load_until, tz = "UTC"), seed)
  hit <- .hw_store_cache[[key]]
  if (!is.null(hit)) return(hit)
  dir <- tempfile("hw-deck-")
  hw_lib$prevcarga_fixture_deck(dir, seed, HW_AREA, O - (days - 1L):0L, load_label = "start", sentinel = sentinel,
                                load_until = load_until, tz = HW_TZ, horizon_days = 10L)
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", sprintf("  X: {areas: [%s], loss: PX}", HW_AREA),
               "order: [X]", "members: []"), file.path(dir, "topology.yaml"))
  out <- list(dir = dir, O = O, days = days, load_until = load_until)
  assign(key, out, envir = .hw_store_cache)
  out
}

hw_fx_ctx <- function(store, rule = "available_until") {
  list(data_dir = store$dir, root = hw_root(), topology_path = file.path(store$dir, "topology.yaml"),
       weather_issue_cutoff = rule, origin = store$O)
}

## cuts de dates.R (nunca literais) no formato lista do despachante, com horizon_days do run.yaml.
hw_fx_cuts <- function(store, origin = store$O, rule = "available_until") {
  ctx <- hw_fx_ctx(store, rule)
  O <- lubridate::ymd(origin)
  r <- hw_lib$prevcarga_resolve_cuts(ctx, O, "holt-winters")
  row <- r$cuts[r$cuts$model == "holt-winters", ]
  list(forecast_origin = r$forecast_origin, origin_day = r$forecast_origin, tz = r$tz, load_label = r$load_label,
       available_until = r$available_until, weather_issue_cutoff = r$weather_issue_cutoff,
       weather_issue_limit = r$weather_issue_limit, split = row$split, train_origin = row$train_origin,
       last_data_day = row$last_data_day, data_lag_days = row$data_lag_days, horizon_days = row$horizon_days)
}

## Painel pelo leitor do M2. O previsto traz TODAS as emissoes <= weather_issue_limit (o leitor entrega so a mais fresca;
## o HW escolhe a mais fresca por (dia, slot)); o observado usa `valid_time` (adaptador do P.0i). mode "train": carga e
## observado cortados em last_data_day; "infer": carga ate available_until.
hw_fx_panel <- function(store, cuts, mode = c("train", "infer"), history_days = 45L) {
  mode <- match.arg(mode)
  ctx <- hw_fx_ctx(store)
  O <- lubridate::as_date(cuts$origin_day)
  H <- cuts$horizon_days
  p <- hw_lib$prevcarga_read_panel(ctx, O, HW_AREA, horizon_days = H, history_days = history_days)
  fc <- hw_lib$prevcarga_read_weather(ctx, "forecast", HW_AREA, O, O + H)
  p$weather_forecast <- fc[issue_time <= cuts$weather_issue_limit]
  if (mode == "train") {
    p$load <- p$load[lubridate::as_date(interval_start, tz = cuts$tz) <= cuts$last_data_day]
    p$weather_observed <- p$weather_observed[lubridate::as_date(valid_time, tz = cuts$tz) <= cuts$last_data_day]
  }
  p
}

## Decodifica o sentinela (minutos desde 2026-09-01 00:00 no fuso) em instantes.
hw_sentinel_decode <- function(v, tz = HW_TZ) {
  lubridate::ymd_hm("2026-09-01 00:00", tz = tz) + lubridate::minutes(as.integer(round(v)))
}

## Copia de models/holt-winters (R/ + run.yaml mutado por `mutate(yaml_list)`) num tempdir; devolve a raiz de modelos.
hw_fx_runyaml <- function(mutate = identity) {
  root <- tempfile("hw-root-"); dir.create(file.path(root, "holt-winters"), recursive = TRUE)
  file.copy(file.path(hw_root(), "models", "holt-winters", "R"), file.path(root, "holt-winters"), recursive = TRUE)
  y <- yaml::read_yaml(file.path(hw_root(), "models", "holt-winters", "run.yaml"))
  yaml::write_yaml(mutate(y), file.path(root, "holt-winters", "run.yaml"))
  root
}

## linhas `arquivo:n` de .R em `dir` cujo codigo (sem comentarios) casa `pattern` (perl)
hw_grep_code <- function(dir, pattern) {
  fs <- list.files(dir, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
  hits <- character()
  for (f in fs) {
    ln <- readLines(f, warn = FALSE)
    code <- vapply(ln, function(l) {
      ch <- strsplit(l, "", fixed = TRUE)[[1L]]; q <- ""
      for (i in seq_along(ch)) {
        c1 <- ch[[i]]
        if (nzchar(q)) { if (c1 == q && (i == 1L || ch[[i - 1L]] != "\\")) q <- "" }
        else if (c1 == "\"" || c1 == "'") q <- c1
        else if (c1 == "#") return(substr(l, 1L, i - 1L))
      }
      l
    }, "", USE.NAMES = FALSE)
    h <- which(grepl(pattern, code, perl = TRUE))
    if (length(h)) hits <- c(hits, paste0(basename(f), ":", h))
  }
  hits
}

## condicao de erro/aviso classificado de uma expressao (NULL se nao houve)
hw_catch <- function(expr) {
  tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
}
