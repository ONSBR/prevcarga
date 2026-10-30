## helper-lgbm.R -- fixtures dos testes do plugin LGBM (LGBM.1). Auto-carregado por test_dir. Nao e codigo de produto.
## Sem variaveis de ambiente; datas por lubridate com tz explicito; tudo em tempdir (nunca toca data/).
suppressWarnings(suppressMessages(library(testthat)))

lgbm_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("lgbm_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}

options(prevcarga.lib = file.path(lgbm_root(), "scripts", "lib"))

## fundacao (leitor, dates, loader, fixture_store) num env de teste, nunca no globalenv
lgbm_lib <- local({
  e <- new.env(parent = globalenv())
  for (m in c("errors", "runtime", "config", "topology", "deck_reader", "validation", "dates", "registry", "features"))
    sys.source(file.path(lgbm_root(), "scripts", "lib", paste0(m, ".R")), envir = e)
  sys.source(file.path(lgbm_root(), "scripts", "tools", "fixture_store.R"), envir = e)
  e
})

LGBM_TZ <- "Etc/GMT+3"
LGBM_O <- lubridate::ymd("2026-10-01")
LGBM_AREA <- "ZZ1"      # sem heat_area em plugin_constants(): os testes passam consts com o mapeamento
LGBM_HEAT <- "SP"       # area real da tabela heat_area (usada onde o consts padrao precisa valer)

## Carrega models/lgbm/R num env isolado cujo pai e a fundacao (lgbm_abort acha prevcarga_condition). Nunca o global.
lgbm_source_plugin <- function(dir = file.path(lgbm_root(), "models", "lgbm", "R"), parent = lgbm_lib) {
  e <- new.env(parent = parent)
  f <- list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)
  for (x in f[order(basename(f), method = "radix")]) sys.source(x, envir = e, keep.source = FALSE, toplevel.env = e)
  e
}

lgbm_consts <- function(plugin) {
  k <- plugin$plugin_constants()
  k$heat_area <- c(k$heat_area, ZZ1 = "ZZ1")
  k
}

## Deck sintetico (P.0e) com as areas ZZ1 e SP, `days` dias terminando em O, emissoes 00Z/06Z/18Z, carga so ate
## `available_until` (O 09:30 => o ultimo intervalo de O tem interval_start 09:00). Cache por (days, seed, until).
.lgbm_deck_cache <- new.env(parent = emptyenv())
lgbm_fx_deck <- function(days = 800L, seed = 11, O = LGBM_O, available_until = NULL, areas = c(LGBM_AREA, LGBM_HEAT),
                         sentinel = FALSE) {
  au <- if (is.null(available_until)) lubridate::ymd_hm("2026-10-01 09:30", tz = LGBM_TZ) else available_until
  key <- paste(days, seed, format(O), format(au, tz = "UTC"), paste(areas, collapse = ","), sentinel)
  if (!is.null(.lgbm_deck_cache[[key]])) return(.lgbm_deck_cache[[key]])
  dir <- tempfile("lgbm-deck-")
  lgbm_lib$prevcarga_fixture_deck(dir, seed, areas, O - (days - 1L):0L, load_label = "start", load_until = au,
                                  sentinel = sentinel, tz = LGBM_TZ, horizon_days = 2L)
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
               sprintf("  X: {areas: [%s], loss: PX}", paste(areas, collapse = ", ")), "order: [X]", "members: []"),
             file.path(dir, "topology.yaml"))
  out <- list(dir = dir, O = O, available_until = au, days = days)
  assign(key, out, envir = .lgbm_deck_cache)
  out
}

## ctx do leitor/dates sobre o deck e o run.yaml REAL do worktree (root = raiz do repo).
lgbm_fx_ctx <- function(deck, O = deck$O, available_until = deck$available_until, weather_issue_cutoff = "available_until",
                        root = lgbm_root()) {
  list(data_dir = deck$dir, root = root, topology_path = file.path(deck$dir, "topology.yaml"),
       weather_issue_cutoff = weather_issue_cutoff, origin = O)
}

## cuts de dates.R para o lgbm (data_lag_days = o do descriptor real, por argumento).
lgbm_fx_cuts <- function(ctx, O = ctx$origin, data_lag_days = c(lgbm = 2L)) {
  r <- lgbm_lib$prevcarga_resolve_cuts(ctx, O, "lgbm", data_lag_days = data_lag_days)
  row <- r$cuts[r$cuts$model == "lgbm", ]
  list(forecast_origin = r$forecast_origin, origin_day = r$forecast_origin, tz = r$tz, load_label = r$load_label,
       available_until = r$available_until, weather_issue_cutoff = r$weather_issue_cutoff,
       weather_issue_limit = r$weather_issue_limit, split = row$split, train_origin = row$train_origin,
       last_data_day = row$last_data_day, data_lag_days = row$data_lag_days)
}

## Painel pelo leitor unico. O previsto traz TODAS as emissoes <= weather_issue_limit (o painel do leitor entrega so a mais
## fresca, que nao serve a safra historica; ver discovery do LGBM.1). train: carga cortada em last_data_day.
lgbm_fx_panel <- function(deck, ctx, mode = c("train", "infer"), cuts = lgbm_fx_cuts(ctx), area = LGBM_AREA,
                          history_days = if (mode == "infer") 45L else 760L) {
  mode <- match.arg(mode)
  O <- ctx$origin
  p <- lgbm_lib$prevcarga_read_panel(ctx, O, area, horizon_days = 2L, history_days = history_days)
  fc <- lgbm_lib$prevcarga_read_weather(ctx, "forecast", c(area, "SP"), O - history_days, O + 2L)
  p$weather_forecast <- fc[issue_time <= cuts$weather_issue_limit]
  if (mode == "train") {
    p$load <- p$load[lubridate::as_date(interval_start, tz = cuts$tz) <= cuts$last_data_day]
    p$weather_observed <- p$weather_observed[lubridate::as_date(valid_time, tz = cuts$tz) <= cuts$last_data_day]
  }
  p
}

## Copia do models/lgbm (R/ + run.yaml mutado por `mutate(yaml_list)`) num tempdir; devolve a raiz de modelos.
lgbm_fx_runyaml <- function(mutate = identity, raw = NULL) {
  root <- tempfile("lgbm-root-"); dir.create(file.path(root, "lgbm"), recursive = TRUE)
  file.copy(file.path(lgbm_root(), "models", "lgbm", "R"), file.path(root, "lgbm"), recursive = TRUE)
  y <- yaml::read_yaml(file.path(lgbm_root(), "models", "lgbm", "run.yaml"))
  yaml::write_yaml(mutate(y), file.path(root, "lgbm", "run.yaml"))
  root
}

## linhas `arquivo:n` de .R em `dir` cujo codigo (sem comentarios) casa `pattern` (perl)
pc_grep_code <- function(dir, pattern) {
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
