## helper-fixtures.R -- fixtures dos testes do plugin random-forest (RF.1). Auto-carregado por test_dir. Nao e codigo de
## produto. Sem variaveis de ambiente; datas por lubridate com tz explicito; tudo em tempdir (nunca toca data/).
suppressWarnings(suppressMessages(library(testthat)))

rf_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("rf_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}

options(prevcarga.lib = file.path(rf_root(), "scripts", "lib"))

## fundacao (leitor, dates, loader, features, fixture_store) num env de teste, nunca no globalenv
rf_lib <- local({
  e <- new.env(parent = globalenv())
  for (m in c("errors", "runtime", "config", "topology", "deck_reader", "validation", "dates", "registry", "features"))
    sys.source(file.path(rf_root(), "scripts", "lib", paste0(m, ".R")), envir = e)
  sys.source(file.path(rf_root(), "scripts", "tools", "fixture_store.R"), envir = e)
  e
})

RF_TZ <- "Etc/GMT+3"
RF_O <- lubridate::ymd("2026-10-01")
RF_AREA <- "ZZ1"
RF_PLUGIN_DIR <- function() file.path(rf_root(), "models", "random-forest")

## Carrega models/random-forest/R num env cujo pai e a fundacao (o plugin acha prevcarga_abort/to_hourly). Nunca o global.
rf_source_plugin <- function(dir = file.path(RF_PLUGIN_DIR(), "R"), parent = rf_lib) {
  e <- new.env(parent = parent)
  f <- list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)
  for (x in f[order(basename(f), method = "radix")]) sys.source(x, envir = e, keep.source = FALSE, toplevel.env = e)
  e
}

## spec do run.yaml REAL (ints como o loader entrega); `ntree` pequeno nos testes rapidos
rf_fx_spec <- function(ntree = 5L) {
  y <- yaml::read_yaml(file.path(RF_PLUGIN_DIR(), "run.yaml"))
  list(name = y$name, split = y$split, granularity_min = as.integer(y$granularity_min),
       horizon_days = as.integer(y$horizon_days),
       history_days = list(train = as.integer(y$history_days$train), infer = as.integer(y$history_days$infer)),
       seed = as.integer(y$seed), modes = unlist(y$modes), backtest = y$backtest, params = list(ntree = as.integer(ntree)))
}

## Deck sintetico (P.0e): area ZZ1, carga de 30 min de 2025-01-01 a O 09:00, emissoes 00Z/06Z/18Z. Cache por semente.
.rf_deck_cache <- new.env(parent = emptyenv())
rf_fx_store <- function(seed = 7, O = RF_O) {
  key <- paste(seed, format(O))
  if (!is.null(.rf_deck_cache[[key]])) return(.rf_deck_cache[[key]])
  dir <- tempfile("rf-deck-")
  au <- lubridate::ymd_hm(paste(format(O), "09:00"), tz = RF_TZ)
  rf_lib$prevcarga_fixture_deck(dir, seed, RF_AREA, seq(lubridate::ymd("2025-01-01"), O, by = "day"),
                                load_label = "start", load_until = au, tz = RF_TZ)
  assign(key, dir, envir = .rf_deck_cache)
  dir
}

rf_fx_ctx <- function(dir, rule = "available_until", root = rf_root())
  list(data_dir = dir, root = root, weather_issue_cutoff = rule)

## cuts como dates.R os entrega (origin_agnostic: train_origin = O-1-H; data_lag_days = 0 => last_data_day = O-1).
## `phase` e `spec` viajam em cuts (interino, ver discovery do RF.1).
rf_fx_cuts <- function(O = RF_O, rule = "available_until", phase = "train", dir = rf_fx_store(O = O)) {
  spec <- rf_fx_spec()
  ld <- rf_lib$prevcarga_read_load(rf_fx_ctx(dir, rule), RF_AREA, from = O, to = O, origin = O)
  au <- attr(ld, "available_until"); tz <- attr(ld, "tz")
  list(forecast_origin = O, origin_day = O, tz = tz, load_label = attr(ld, "load_label"), available_until = au,
       weather_issue_cutoff = rule, weather_issue_limit = rf_lib$prevcarga_weather_issue_limit(rule, O, au, tz),
       split = "origin_agnostic", train_origin = O - 1L - spec$horizon_days, last_data_day = O - 1L,
       data_lag_days = 0L, phase = phase, spec = spec)
}

## Painel pelo leitor unico. O previsto traz TODAS as emissoes <= weather_issue_limit (o painel do leitor entrega so a mais
## fresca, que nao serve a safra historica). train: carga cortada em last_data_day (como o despachante exige).
## hourly = TRUE: a forma que o estagio recebe do despachante (carga por to_hourly, clima cru: weather_resample raw);
## FALSE: painel cru de 30 min, para quem passa pelo despachante.
rf_fx_panel <- function(dir = rf_fx_store(), cuts = rf_fx_cuts(dir = dir), phase = cuts$phase, hourly = TRUE) {
  O <- cuts$forecast_origin
  from <- cuts$train_origin - 370L
  ctx <- rf_fx_ctx(dir, cuts$weather_issue_cutoff)
  ld <- rf_lib$prevcarga_read_load(ctx, RF_AREA, from = from, to = O, origin = O)
  if (identical(phase, "train")) ld <- ld[lubridate::as_date(interval_start, tz = cuts$tz) <= cuts$last_data_day]
  fc <- rf_lib$prevcarga_read_weather(ctx, "forecast", RF_AREA, from, O + 10L)
  p <- list(load = ld, weather_forecast = fc[issue_time <= cuts$weather_issue_limit],
            holidays = rf_lib$prevcarga_read_holidays(ctx, RF_AREA, from, O + 10L),
            attrs = list(origin = O, tz = cuts$tz, available_until = cuts$available_until,
                         weather_issue_limit = cuts$weather_issue_limit))
  if (isTRUE(hourly)) rf_lib$to_hourly(p, weather = FALSE) else p
}

## Copia do models/random-forest (R/ + run.yaml mutado por `mutate(yaml_list)`) num tempdir; devolve a raiz de modelos.
## Com `stubs = TRUE`, acrescenta R/zz_stubs.R so para os estagios AINDA NAO definidos em R/ (RF.2..RF.4): cada stub tem a
## assinatura canonica e aborta `plugin.stage_pending` se executado.
rf_fx_plugin_copy <- function(stubs = TRUE, mutate = identity, drop_pkgs = NULL, add_pkg = NULL) {
  root <- tempfile("rf-root-"); dir.create(file.path(root, "random-forest"), recursive = TRUE)
  file.copy(file.path(RF_PLUGIN_DIR(), "R"), file.path(root, "random-forest"), recursive = TRUE)
  y <- yaml::read_yaml(file.path(RF_PLUGIN_DIR(), "run.yaml"))
  yaml::write_yaml(mutate(y), file.path(root, "random-forest", "run.yaml"))
  rdir <- file.path(root, "random-forest", "R")
  if (!is.null(add_pkg)) {
    f <- file.path(rdir, "register.R")
    writeLines(sub("packages = c(\"randomForest\",", sprintf("packages = c(\"%s\", \"randomForest\",", add_pkg),
                   readLines(f), fixed = TRUE), f)
  }
  if (stubs) {
    defined <- unlist(lapply(list.files(rdir, "[.][Rr]$", full.names = TRUE), function(f)
      sub("^([A-Za-z0-9_.]+)\\s*<-\\s*function.*$", "\\1", grep("^[A-Za-z0-9_.]+\\s*<-\\s*function", readLines(f), value = TRUE))))
    sig <- list(plugin_build_features = "frame, params", plugin_train = "features, params, seed",
                plugin_inference_features = "frame, payload", plugin_infer = "payload, inf_features, cuts",
                rf_freeze = "model", rf_thaw = "payload", rf_semihourly_rule = "x")
    todo <- setdiff(names(sig), defined)
    if (length(todo))
      writeLines(unlist(lapply(todo, function(n) c(sprintf("%s <- function(%s) {", n, sig[[n]]),
                                                    sprintf("  stop(\"plugin.stage_pending: %s\")", n), "}"))),
                 file.path(rdir, "zz_stubs.R"))
  }
  root
}

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

## linhas `arquivo:n` de .R em `dir` cujo codigo (sem comentarios) casa `pattern` (perl)
rf_grep_code <- function(dir, pattern) {
  fs <- list.files(dir, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
  hits <- character()
  for (f in fs) {
    ln <- sub("\\s*#[^\"']*$", "", readLines(f, warn = FALSE))
    h <- which(grepl(pattern, ln, perl = TRUE))
    if (length(h)) hits <- c(hits, paste0(basename(f), ":", h))
  }
  hits
}
