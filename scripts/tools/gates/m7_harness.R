## scripts/tools/gates/m7_harness.R -- pack de gates de HARNESS [M7] dos plugins (M7.5), descoberto por prevcarga_gates_run (P.0h).
##
## Barrel por descoberta (ADR-0040 item 8): cada models/<m>/tests/run_all_gates.R chama so prevcarga_gates_run, que varre
## scripts/tools/gates/*.R (sort radix) e chama prevcarga_gate_pack_m7_harness(model, ctx, tier). As linhas devolvidas (dono `M7`)
## SUBSTITUEM os slots `[DEFERRED M7]` das tabelas dos runners; nenhum runner e editado. Mapa slot -> gate (nada some em silencio):
##   random-forest  gate12 (harness LONG)        -> gate08_ytrue_real + gate12_harness_long
##   holt-winters   harness_parity (gate10)      -> gate10_harness_parity
##   dessem-linear  gate03_sem_crash             -> gate03_sem_crash        (mesmo id; harness multiorigem sem crash)
##   dessem-regdin  sem_crash_harness (gate03)   -> sem_crash_harness       (mesmo id)
## lgbm nao reservou slot [DEFERRED M7].
##
## O HP (harness product) e o que prevcarga_backtest() produz (`$paths$members`: um long.parquet por celula) sobre um deck de fixture
## do proprio pack; nenhum runner cita walk_forward nem caminho fixo. Os plugins buscam a API da fundacao no globalenv (interino),
## entao o harness inteiro roda num `Rscript --vanilla` FILHO (a lib no globalenv, como a CLI): o processo do runner nao e sujado.
## O pai so devolve linhas finas (`test` = le o veredito do filho, memoizado por modelo). Sem variaveis de ambiente; datas so por
## lubridate com tz explicito; ASCII.
##
## Funcoes do filho (lib no globalenv): prevcarga_harness_hp, prevcarga_gate_ytrue_real, prevcarga_gate_harness_long,
## prevcarga_gate_harness_parity, prevcarga_gate_sem_crash e prevcarga_m7h_run. Cada gate devolve list(id, pass, detail).

M7H_TZ <- "Etc/GMT+3"
M7H_LOAD_SCALE <- 8      # a carga do fixture (~1 GW por area) sobe para a ordem de grandeza real (mediana > 5000 MW do gate08)
M7H_FOUNDATION <- c("errors", "runtime", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
                    "features", "registry", "training", "inference", "build_long_output", "evaluation", "combination",
                    "reconcile", "cache", "orchestrate", "backtest")

## plano por modelo: ultima origem, no de origens, filtro de membro (sigla|mdir), protocolo, janela do deck, overrides rapidos
## de TESTE do spec efetivo (a menor janela em que o plugin treina sobre o deck sintetico), gates.
M7H_PLAN <- list(
  "random-forest" = list(last = "2025-03-05", n_origins = 3L, models = "random-forest", protocol = NULL, deck_days = 120L,
                         overrides = list("history_days.train" = 40L, "params.ntree" = 5L),
                         gates = c("gate08_ytrue_real", "gate12_harness_long")),
  "holt-winters" = list(last = "2025-03-05", n_origins = 3L, models = "holt-winters", protocol = "retrain_harness", deck_days = 120L,
                        overrides = list("history_days.train" = 40L), gates = "gate10_harness_parity"),
  "dessem-linear" = list(last = "2025-03-05", n_origins = 2L, models = "dessem-linear/MED", protocol = NULL, deck_days = 120L,
                         overrides = list("history_days.train" = 40L, "history_days.infer" = 40L), gates = "gate03_sem_crash"),
  "dessem-regdin" = list(last = "2025-03-05", n_origins = 2L, models = "dessem-regdin/MED", protocol = NULL, deck_days = 460L,
                         overrides = list("history_days.train" = 400L, "history_days.infer" = 400L), gates = "sem_crash_harness"))
M7H_AREA <- "SP"
M7H_CORTE <- "09:00"

## --- deck e ctx de fixture (filho) --------------------------------------------------------------------------------------------
## Efeito dia-a-dia deterministico sobre a copia do deck (sem ele a serie do fixture e quase periodica e o ARIMA do regdin nao
## ajusta) e escala da carga. Usa o reescritor de particoes do fixture_store (so em copia).
.m7h_day_effects <- function(dir, scale = M7H_LOAD_SCALE) {
  eff <- function(t) {
    d <- as.numeric(as.Date(t))
    list(temp = 2 * sin(d * d * 0.37) + 1.5 * sin(d * d * 0.113 + 1), load = 60 * sin(d * d * 0.173) + 40 * sin(d * d * 0.271 + 2))
  }
  out <- prevcarga_fixture_copy(dir)
  hit_all <- function(x) rep(TRUE, nrow(x))
  .pc_fx_rewrite_hits(out, "load", NULL, hit_all, function(x, i) {
    e <- eff(x$timestamp[i])
    data.table::set(x, i = i, j = "load_mwh", value = scale * (x$load_mwh[i] + e$load + 40 * e$temp))
  })
  .pc_fx_rewrite_hits(out, "weather_observed", NULL, hit_all, function(x, i)
    data.table::set(x, i = i, j = "temperature", value = x$temperature[i] + eff(x$timestamp[i])$temp))
  .pc_fx_rewrite_hits(out, "weather_forecast", NULL, hit_all, function(x, i)
    data.table::set(x, i = i, j = "temperature", value = x$temperature[i] + eff(x$valid_time[i])$temp))
  out
}

## Deck sintetico de ate `last` (dia inteiro), `days` dias, area M7H_AREA; `load_until` opcional corta a carga (CONTROL de y_true).
prevcarga_m7h_deck <- function(dir, last, days, load_until = NULL) {
  last <- lubridate::ymd(last)
  prevcarga_fixture_deck(dir, seed = 11L, areas = M7H_AREA, days = seq(last - (days - 1L), last, by = "day"), tz = M7H_TZ,
                         load_until = load_until)
  d <- .m7h_day_effects(dir)
  ## o feriado futuro do fixture (O+3, sem amostra historica) vira codigo 12 (nao-modelado => dia normal)
  prevcarga_fixture_set_holiday(d, M7H_AREA, last + 3L, c(holiday_type_id = 12L, prevcarga_code = 12L, simplified_code = 1L))
}

prevcarga_m7h_ctx <- function(root, deck, state, overrides) {
  ctx <- prevcarga_ctx(list("--data" = deck, "--state" = state, "--weather-cutoff" = "origin_midnight_utc"), root, "backtest")
  ctx$spec_overrides <- overrides
  ctx$code_commit <- "c0ffee"
  ctx$renv_lock_hash <- "ab12"
  ctx$progress <- function(done, total) NULL
  ctx
}

## --- HP ------------------------------------------------------------------------------------------------------------------------
## Roda prevcarga_backtest() so com o membro pedido e devolve o HP: as celulas (`$paths$members`, um long.parquet cada) mais o que os
## gates precisam (ctx de leitura com tz/rotulo do deck, available_until por celula, delivery_days do descriptor).
prevcarga_harness_hp <- function(ctx, mdir, origins, cortes, protocol = NULL, areas = NULL, models = mdir) {
  res <- suppressWarnings(prevcarga_backtest(ctx, origins = origins, models = models, areas = areas, cortes = cortes,
                                             protocol = protocol))
  man <- prevcarga_deck_manifest(ctx$data_dir)
  rctx <- ctx
  rctx$tz <- man$timezone$load
  rctx$load_label <- prevcarga_resolve_load_label(ctx, man)$value
  cells <- data.table::as.data.table(res$paths$members)
  cells[, available_until := lubridate::ymd_hm(paste(format(origem, "%Y-%m-%d"), corte_hm), tz = rctx$tz)]
  dd <- as.integer(prevcarga_plugin_load(mdir, prevcarga_models_root(ctx$root))$descriptor$delivery_days)
  list(ctx = ctx, rctx = rctx, mdir = mdir, protocol = protocol, summary = res$summary, cells = cells,
       spec = list(name = mdir, delivery_days = dd), tz = rctx$tz)
}

.m7h_read_long <- function(path) data.table::as.data.table(arrow::read_parquet(as.character(path)))

.m7h_cell_id <- function(cells, i) sprintf("%s/%s/%s/%s", cells$membro[[i]], cells$folha[[i]], format(cells$origem[[i]], "%Y-%m-%d"),
                                          cells$corte_hm[[i]])

.m7h_gate <- function(id, ok, detail = "") list(id = id, pass = isTRUE(ok), detail = detail)

.m7h_detail <- function(bad, n) if (!length(bad)) sprintf("%d celula(s) ok", n) else paste(utils::head(bad, 3L), collapse = " | ")

## --- gate08: y_true real (verdade fresca re-juntada pelo leitor) --------------------------------------------------------------
## O y_true embutido no LONG da celula e descartado: a verdade vem do leitor unico (prevcarga_read_actual) por interval_end exato.
prevcarga_gate_ytrue_real <- function(hp) {
  y <- unlist(lapply(seq_len(nrow(hp$cells)), function(i) {
    long <- .m7h_read_long(hp$cells$long_parquet[[i]])
    if (!nrow(long)) return(numeric())
    act <- tryCatch(prevcarga_read_actual(hp$rctx, hp$cells$folha[[i]], from = min(long$target_datetime) - lubridate::seconds(1),
                                          to = max(long$target_datetime)), prevcarga_error = function(e) NULL)
    if (is.null(act)) return(rep(NA_real_, nrow(long)))
    act$y_true[match(as.numeric(long$target_datetime), as.numeric(act$interval_end))]
  }))
  ok <- length(y) > 0L && sum(!is.na(y)) > 0L && !all(y == 0, na.rm = TRUE) && stats::median(y, na.rm = TRUE) > 5000
  med <- if (length(y) && any(!is.na(y))) stats::median(y, na.rm = TRUE) else NA_real_
  .m7h_gate("gate08_ytrue_real", ok, sprintf("n_true = %d de %d; mediana = %s", sum(!is.na(y)), length(y), format(med, digits = 5)))
}

## --- gate12: LONG do harness (8 colunas, forecast_origin texto, grade a partir de available_until + 30) -------------------------
## nrow e a grade esperada vem de prevcarga_delivery_grid (features.R), nunca literal; o deslocamento de 1 intervalo mantem nrow e
## por isso a grade e comparada instante a instante.
.m7h_long_problem <- function(long, cell, delivery_days, tz) {
  ok <- tryCatch({ prevcarga_schema_long_check(long); TRUE }, error = function(e) FALSE)
  if (!ok) return("schema_long (colunas/ordem)")
  for (n in names(prevcarga_schema_long)) {
    good <- switch(prevcarga_schema_long[[n]], character = is.character(long[[n]]), POSIXct = inherits(long[[n]], "POSIXct"),
                   integer = is.integer(long[[n]]), double = is.double(long[[n]]), FALSE)
    if (!good) return(sprintf("tipo da coluna %s (esperado %s)", n, prevcarga_schema_long[[n]]))
  }
  fo <- format(cell$origem, "%Y-%m-%d")
  if (!all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", long$forecast_origin)) || !all(long$forecast_origin == fo))
    return("forecast_origin != texto yyyy-MM-dd da origem")
  grid <- prevcarga_delivery_grid(list(origin_day = cell$origem, available_until = cell$available_until, tz = tz), delivery_days)
  if (nrow(long) != length(grid)) return(sprintf("nrow %d != grade %d", nrow(long), length(grid)))
  if (!identical(as.numeric(long$target_datetime), as.numeric(grid))) return("target_datetime != grade de entrega")
  NULL
}

prevcarga_gate_harness_long <- function(hp, ctx = hp$ctx, spec = hp$spec) {
  cells <- hp$cells
  bad <- character()
  for (i in seq_len(nrow(cells))) {
    long <- .m7h_read_long(cells$long_parquet[[i]])
    why <- .m7h_long_problem(long, cells[i], spec$delivery_days, hp$tz)
    if (!is.null(why)) bad <- c(bad, paste(.m7h_cell_id(cells, i), why))
  }
  .m7h_gate("gate12_harness_long", nrow(cells) > 0L && !length(bad), .m7h_detail(bad, nrow(cells)))
}

## --- gate10: paridade do harness com o `forecast` acoplado single-origin ------------------------------------------------------
## Por celula (origem), UMA chamada ao despachante prevcarga_plugin_forecast (treino so ate last_data_day + infer no mesmo painel):
## o mesmo caminho de producao do protocolo retrain_harness, montado aqui de forma independente do laco do backtest.R.
.m7h_member_of <- function(ctx, sigla) {
  mem <- if (!is.null(ctx$topology_path)) prevcarga_members(path = ctx$topology_path, root = ctx$root) else prevcarga_members(root = ctx$root)
  m <- mem[mem$sigla == sigla, , drop = FALSE]
  list(sigla = m$sigla[[1L]], mdir = m$mdir[[1L]], exogena = m$exogena[[1L]])
}

.m7h_coupled_forecast <- function(hp, i) {
  cell <- hp$cells[i]
  ctx <- hp$ctx
  member <- .m7h_member_of(ctx, cell$membro)
  plugin <- prevcarga_plugin_load(member$mdir, prevcarga_models_root(ctx$root))
  prevcarga_plugin_check_signature(plugin)
  spec <- prevcarga_model_spec(member$mdir, ctx$root, .prevcarga_tr_overrides(plugin, member, ctx))
  cuts <- .prevcarga_inf_cuts(plugin, spec, cell$origem, ctx, member$mdir)
  rspec <- spec
  rspec$history_days$infer <- max(spec$history_days$train, spec$history_days$infer)
  rp <- prevcarga_infer_read_panel(plugin, rspec, cuts, cell$folha, cell$origem, ctx, truncate_at = cell$available_until)
  pctx <- list(load_label = rp$cuts$load_label, timezone = rp$cuts$tz, weather_issue_cutoff = rp$cuts$weather_issue_cutoff)
  prevcarga_plugin_forecast(plugin, rp$panel, rp$cuts, spec, pctx)$delivery
}

prevcarga_gate_harness_parity <- function(hp, ctx = hp$ctx, spec = hp$spec) {
  cells <- hp$cells
  delta <- vapply(seq_len(nrow(cells)), function(i) {
    long <- .m7h_read_long(cells$long_parquet[[i]])
    fc <- .m7h_coupled_forecast(hp, i)
    d <- abs(long$y_pred - fc$y_pred[match(as.numeric(long$target_datetime), as.numeric(fc$interval_end))])
    if (!length(d) || anyNA(d)) Inf else max(d)
  }, numeric(1))
  ok <- length(delta) > 0L && all(delta < 1e-6)
  .m7h_gate("gate10_harness_parity", ok, sprintf("max|delta| = %.3g em %d origem(ns)", if (length(delta)) max(delta) else NA_real_, length(delta)))
}

## --- gate03/sem_crash: o laco multiorigem fecha limpo -------------------------------------------------------------------------
## Varias origens em sequencia pelo prevcarga_backtest: nenhuma celula falha, todas publicam long.parquet e cada LONG cumpre o contrato.
prevcarga_gate_sem_crash <- function(hp, id = "gate03_sem_crash") {
  cells <- hp$cells
  s <- hp$summary
  files <- vapply(seq_len(nrow(cells)), function(i) file.exists(cells$long_parquet[[i]]), NA)
  bad <- character()
  if (!identical(s$verdict, "ok")) bad <- c(bad, sprintf("veredito %s", s$verdict))
  if (!identical(as.integer(s$failed), 0L) || !identical(as.integer(s$ok), as.integer(s$total)))
    bad <- c(bad, sprintf("celulas ok %s de %s, falhas %s", s$ok, s$total, s$failed))
  if (length(unique(cells$origem)) < 2L) bad <- c(bad, "menos de 2 origens")
  if (!all(files)) bad <- c(bad, "long.parquet ausente")
  if (!length(bad)) {
    lg <- prevcarga_gate_harness_long(hp)
    if (!lg$pass) bad <- c(bad, lg$detail)
  }
  .m7h_gate(id, nrow(cells) > 0L && !length(bad), if (length(bad)) paste(bad, collapse = " | ") else
              sprintf("%d celula(s) em %d origens", nrow(cells), length(unique(cells$origem))))
}

## --- filho: roda o harness de UM modelo e escreve o veredito dos gates ------------------------------------------------------------
prevcarga_m7h_run <- function(root, model, out, work = tempfile("m7h-")) {
  pl <- M7H_PLAN[[model]]
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  last <- lubridate::ymd(pl$last)
  deck <- prevcarga_m7h_deck(file.path(work, "deck"), last, pl$deck_days)
  ctx <- prevcarga_m7h_ctx(root, deck, file.path(work, "state"), pl$overrides)
  origins <- last - (pl$n_origins - 1L):0     # aritmetica (Date double): seq(by = "day") devolve Date inteiro e o HW o recusa
  hp <- prevcarga_harness_hp(ctx, model, origins, M7H_CORTE, protocol = pl$protocol, areas = M7H_AREA, models = pl$models)
  rows <- lapply(pl$gates, function(g) switch(g,
    gate08_ytrue_real = prevcarga_gate_ytrue_real(hp),
    gate12_harness_long = prevcarga_gate_harness_long(hp),
    gate10_harness_parity = prevcarga_gate_harness_parity(hp),
    gate03_sem_crash = prevcarga_gate_sem_crash(hp, "gate03_sem_crash"),
    sem_crash_harness = prevcarga_gate_sem_crash(hp, "sem_crash_harness")))
  jsonlite::write_json(list(model = model, gates = rows), out, auto_unbox = TRUE)
  invisible(rows)
}

## --- pai: pack fino (nada da lib e carregado no processo do runner) ------------------------------------------------------------
.m7h_state <- new.env(parent = emptyenv())

.m7h_child_lines <- function() {
  c("a <- commandArgs(TRUE)",
    "root <- a[[1L]]; model <- a[[2L]]; out <- a[[3L]]; self <- a[[4L]]",
    "options(prevcarga.lib = file.path(root, 'scripts', 'lib'))",
    sprintf("for (m in %s) sys.source(file.path(root, 'scripts', 'lib', paste0(m, '.R')), envir = globalenv())",
            paste(deparse(M7H_FOUNDATION), collapse = "")),
    "sys.source(file.path(root, 'scripts', 'tools', 'fixture_store.R'), envir = globalenv())",
    "sys.source(self, envir = globalenv())",
    "prevcarga_runtime_enforce()",
    "prevcarga_m7h_run(root, model, out)")
}

## Veredito dos gates do modelo (memoizado): roda o harness no filho e le o JSON.
.m7h_verdicts <- function(model, root, self) {
  key <- paste(root, model)
  if (!is.null(.m7h_state$v[[key]])) return(.m7h_state$v[[key]])
  work <- tempfile("m7h-pack-"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  script <- file.path(work, "child.R")
  out <- file.path(work, "verdict.json")
  writeLines(.m7h_child_lines(), script)
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  c("--vanilla", shQuote(script), shQuote(root), shQuote(model), shQuote(out), shQuote(self)),
                                  stdout = TRUE, stderr = TRUE))
  if (!file.exists(out))
    stop(sprintf("%s: harness nao produziu veredito: %s", model, paste(utils::tail(log, 3L), collapse = " | ")), call. = FALSE)
  g <- jsonlite::fromJSON(out, simplifyVector = FALSE)$gates
  v <- stats::setNames(lapply(g, function(x) x), vapply(g, function(x) x$id, ""))
  if (is.null(.m7h_state$v)) .m7h_state$v <- list()
  .m7h_state$v[[key]] <- v
  v
}

prevcarga_gate_pack_m7_harness <- function(model, ctx, tier) {
  empty <- data.frame(gate = character(), owner = character(), tier_min = character(), stringsAsFactors = FALSE)
  empty$applies_to <- list()
  if (!identical(tier, "fast") || is.null(M7H_PLAN[[model]])) return(empty)
  root <- normalizePath(file.path(ctx$gates_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
  self <- normalizePath(file.path(ctx$gates_dir, "m7_harness.R"), winslash = "/", mustWork = TRUE)
  ids <- M7H_PLAN[[model]]$gates
  tests <- lapply(ids, function(g) { force(g); function() {
    v <- .m7h_verdicts(model, root, self)[[g]]
    if (is.null(v)) stop(sprintf("gate %s ausente do veredito do harness", g), call. = FALSE)
    if (!isTRUE(v$pass)) stop(sprintf("%s: %s", g, v$detail), call. = FALSE)
    invisible(TRUE)
  } })
  df <- data.frame(gate = ids, owner = "M7", tier_min = "fast", stringsAsFactors = FALSE)
  df$applies_to <- rep(list(model), length(ids))
  df$test <- tests
  df
}
