## helper-parity.R -- fixtures dos gates de paridade e de deslocamento de dia do Holt-Winters (HW.5). Auto-carregado por test_dir
## depois do helper-hw.R. Nao e codigo de produto. Sem variaveis de ambiente; datas por lubridate com tz explicito.
## O oraculo (somente leitura) so aparece como GOLDEN em `.state/golden` (gerado por scripts/tools/plugin_golden.R, nunca
## versionado); aqui so se le. O store real (`data/`) e lido por um overlay de symlinks em tempdir + manifest (nada gravado nele).

if (!exists("prevcarga_freeze", envir = hw_lib, inherits = FALSE))
  sys.source(file.path(hw_root(), "scripts", "lib", "provenance.R"), envir = hw_lib)

## ferramentas de desenvolvimento (P.0f/P.0g) + metrica unica, num env filho da fundacao (nunca o global)
hp_tools <- local({
  e <- new.env(parent = hw_lib)
  for (f in c("schema_long.R", "evaluation.R")) sys.source(file.path(hw_root(), "scripts", "lib", f), envir = e)
  for (f in c("plugin_golden.R", "plugin_parity.R")) sys.source(file.path(hw_root(), "scripts", "tools", f), envir = e)
  e
})

HP_MODEL <- "holt-winters"
HP_TOL <- 1e-5                                   # unico limiar (ADR-0017): MW absoluto, trilho mesmo-hardware
HP_SMOKE <- c("seco-2024-05-15", "seco-2024-05-16", "seco-2024-07-01", "seco-2024-11-13")
HP_BASE <- "seco-2024-05-15"
HP_CRASH <- "seco-2024-07-01"

hp_units_path <- function() file.path(hw_root(), "models", HP_MODEL, "tests", "parity", "units.yaml")
hp_lock_path <- function() file.path(hw_root(), "models", HP_MODEL, "tests", "parity", "golden.lock")
hp_report_path <- function() file.path(hw_root(), "models", HP_MODEL, "tests", "parity", "parity_report.json")
hp_golden_root <- function() file.path(hw_root(), ".state", "golden")
hp_data_dir <- function() file.path(hw_root(), "data")
hp_rscript <- function() file.path(R.home("bin"), "Rscript")

## --- tier e grupo de unidades ---------------------------------------------------------------------------------
hp_tier <- function() if (exists(".pc_gates_state", mode = "environment")) get(".pc_gates_state")$tier else "fast"

## `--units smoke|full` na linha de comando do runner; sem ele: parity => smoke, parity-full => full
hp_group <- function(argv = commandArgs(TRUE), tier = hp_tier()) {
  i <- which(argv == "--units")
  g <- if (length(i) && i[[1L]] < length(argv)) argv[[i[[1L]] + 1L]] else if (identical(tier, "parity-full")) "full" else "smoke"
  if (!(g %in% c("smoke", "full"))) stop("--units: smoke | full", call. = FALSE)
  g
}

## --- units.yaml: esquema da fundacao + invariantes do HW ------------------------------------------------------
## O leitor da fundacao (pc_golden_units) valida o esquema; aqui o que e do HW: sem oracle_cut (o oraculo do HW nao recebe
## --available-until), day0_source/corte de clima/janela fixos. Recusa nomeia `<id>.<campo>` ([prevcarga/contract] tools.bad_units).
hp_validate_units <- function(path = hp_units_path()) {
  spec <- hw_lib$prevcarga_with_errors(hp_tools$pc_golden_units(path))
  if (!is.null(spec$cond)) stop(spec$cond)
  raw <- yaml::read_yaml(path)$units
  bad <- function(id, field, why)
    hw_lib$prevcarga_abort("tools.bad_units", where = paste0(id, ".", field), details = list(unit = id, field = field, why = why))
  for (u in raw) {
    if (!is.null(u$oracle_cut)) bad(u$id, "oracle_cut", "o HW do oraculo nao recebe --available-until (sem oracle_cut)")
    if (!identical(u$day0_source, "oracle_discarded_step")) bad(u$id, "day0_source", "oracle_discarded_step")
    if (!identical(u$weather_issue_cutoff, "origin_midnight_utc")) bad(u$id, "weather_issue_cutoff", "origin_midnight_utc")
    if (!identical(as.integer(u$history_days), 730L)) bad(u$id, "history_days", "730 (run_job.sh retrain_harness)")
    if (!identical(u$available_until, paste0(u$origin, "T00:00"))) bad(u$id, "available_until", "00:00 de O")
  }
  spec$value
}

hp_units <- function(group = c("smoke", "full", "all"), path = hp_units_path()) {
  group <- match.arg(group)
  us <- hp_validate_units(path)$units
  switch(group, all = us,
         smoke = us[vapply(us, function(u) u$id %in% HP_SMOKE, NA)],
         full = us[vapply(us, function(u) format(u$origin, "%Y-%m") == "2024-05", NA)])
}

hp_unit <- function(id) {
  us <- hp_units("all")
  us[[which(vapply(us, `[[`, "", "id") == id)]]
}

## --- golden.lock: ausente/divergente => mensagem 'regere o golden' (nunca skip) -------------------------------
## Devolve NULL (ok) ou o texto ASCII da falha. A conferencia e a do P.0g (sha de arquivo, R/plataforma/BLAS, versao de
## pacote, particoes de dados).
hp_check_golden <- function(u, root = hp_golden_root(), lock = hp_lock_path(), data_dir = hp_data_dir()) {
  regen <- "regere o golden com scripts/tools/plugin_golden.R --model holt-winters"
  if (!file.exists(lock)) return(sprintf("regere o golden: golden.lock ausente (%s)", regen))
  d <- hp_tools$.pc_golden_unit_dir(root, HP_MODEL, u)
  lk <- jsonlite::read_json(lock, simplifyVector = FALSE)
  r <- hw_lib$prevcarga_with_errors(hp_tools$.pc_parity_check_lock(lk, u, data_dir, d, hp_rscript()))
  if (is.null(r$cond)) return(NULL)
  dt <- r$cond$details
  sprintf("regere o golden: unidade %s: %s %s (%s)", u$id, r$cond$id, paste(dt$item, collapse = " "), regen)
}

hp_golden <- function(u, root = hp_golden_root()) {
  hp_tools$.pc_parity_read_golden(u, list(u), root, HP_MODEL, "Etc/GMT+3")
}

## --- store real por overlay + painel dos cortes da unidade ----------------------------------------------------
.hp <- new.env(parent = emptyenv())
.hp$runs <- list()
.hp$report <- list(units = list(), controls = list())

hp_store <- function() {
  if (!is.null(.hp$store)) return(.hp$store)
  ov <- tempfile("hp-store-"); dir.create(ov)
  for (d in c("load", "weather", "auxiliary")) file.symlink(file.path(hp_data_dir(), d), file.path(ov, d))
  file.copy(file.path(hw_root(), "config", "deck-manifest.store.json"), file.path(ov, "manifest.json"))
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
               "  X: {areas: [SECO, S, NE, \"N\"], loss: PX}", "order: [X]", "members: []"), file.path(ov, "topology.yaml"))
  .hp$store <- ov
  ov
}

hp_ctx <- function() {
  list(data_dir = hp_store(), root = hw_root(), topology_path = file.path(hp_store(), "topology.yaml"),
       weather_issue_cutoff = "origin_midnight_utc")
}

## Cortes da unidade: `prevcarga_resolve_cuts` (cortes por modelo, regra de clima 00Z de O) com o available_until da
## UNIDADE (00:00 de O): o store real ja tem o dia O, entao o painel e truncado pelo `truncate_at` do leitor.
hp_cuts <- function(u) {
  rc <- suppressWarnings(hw_lib$prevcarga_resolve_cuts(hp_ctx(), origin = u$origin, model = HP_MODEL))
  row <- rc$cuts[rc$cuts$model == HP_MODEL, ]
  list(forecast_origin = rc$forecast_origin, origin_day = rc$forecast_origin, tz = rc$tz, load_label = rc$load_label,
       weather_issue_cutoff = rc$weather_issue_cutoff, available_until = u$available_until,
       weather_issue_limit = rc$weather_issue_limit, split = row$split, train_origin = row$train_origin,
       last_data_day = row$last_data_day, data_lag_days = row$data_lag_days, horizon_days = row$horizon_days)
}

## Painel pelo leitor do M2 (carga e clima observado ate available_until; previsto: todas as emissoes <= weather_issue_limit,
## o HW escolhe a mais fresca por dia/slot; observado como `valid_time`, adaptador do P.0i; sem o ponto de 00:00 de O: o
## treino so ve dias <= last_data_day).
hp_panel <- function(u, cuts, history_days = 730L, horizon_days = cuts$horizon_days) {
  ctx <- hp_ctx(); O <- u$origin; AU <- cuts$available_until
  load <- hw_lib$prevcarga_read_load(ctx, u$area, from = O - history_days, to = O, origin = O, truncate_at = AU)
  obs <- hw_lib$prevcarga_read_weather(ctx, "observed", u$area, O - history_days, O)
  obs <- obs[timestamp <= AU & lubridate::as_date(timestamp) <= cuts$last_data_day]
  data.table::setnames(obs, "timestamp", "valid_time")
  fc <- hw_lib$prevcarga_read_weather(ctx, "forecast", u$area, O, O + horizon_days)
  list(load = load, weather_observed = obs, weather_forecast = fc[issue_time <= cuts$weather_issue_limit],
       holidays = hw_lib$prevcarga_read_holidays(ctx, u$area, O - history_days, O + horizon_days),
       attrs = list(origin = O, available_until = AU, weather_issue_cutoff = cuts$weather_issue_cutoff,
                    weather_issue_limit = cuts$weather_issue_limit))
}

hp_plugin <- function() hw_lib$prevcarga_plugin_load(HP_MODEL, file.path(hw_root(), "models"))

## fundacao no global so durante `code` (o plugin carregado pelo loader a procura la; interino como engine_abort)
hp_with_cli <- function(code) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_plugin_scan_pure", "prevcarga_delivery_grid")
  g <- globalenv()
  had <- nm[vapply(nm, exists, NA, envir = g, inherits = FALSE)]
  old <- mget(had, envir = g)
  for (n in nm) assign(n, hw_lib[[n]], envir = g)
  ## restaura o que o runner (plugin_gates.R) ja tinha no global (ex.: prevcarga_abort): so remove o que foi plantado aqui
  on.exit({ rm(list = nm, envir = g); for (n in had) assign(n, old[[n]], envir = g) }, add = TRUE)
  force(code)
}

## Envelope do clean-room pelo despachante `prevcarga_plugin_forecast` (cache por unidade + overrides). Overrides dos
## CONTROLs: `history_days` (janela do treino: painel e spec), `suaviza` (params) e `seed`.
hp_run <- function(u, history_days = 730L, suaviza = NULL, seed = NULL) {
  key <- paste(u$id, history_days, if (is.null(suaviza)) "" else suaviza, if (is.null(seed)) "" else seed)
  if (!is.null(.hp$runs[[key]])) return(.hp$runs[[key]])
  plugin <- hp_plugin(); spec <- plugin$spec
  spec$history_days$train <- as.integer(history_days)
  if (!is.null(suaviza)) spec$params$suaviza <- suaviza
  if (!is.null(seed)) spec$seed <- seed
  cuts <- hp_cuts(u)
  env <- hp_with_cli(suppressWarnings(suppressMessages(
    hw_lib$prevcarga_plugin_forecast(plugin, hp_panel(u, cuts, history_days), cuts, spec, list()))))
  .hp$runs[[key]] <- env
  env
}

## --- comparacao: values/delivery pelo comparador P.0g (layout hw_matrix_nx48) + dia O cru ----------------------
## O native.rds do HW e Previsao15 = O+1..O+n (o oraculo descarta o dia O): o comparador da fundacao so sabe o dia O como UM
## passo escalar (`discarded_step`), e os 48 passos do dia O cru vao em `meta$day0_raw`. Entao `values` e `delivery` de
## O+1..O+n passam pelo comparador com a unidade vista a partir do FIM do dia O (available_until = O+1 00:00, day0_source
## native; delivery = O+1 00:30 .. O+9 00:00) e o dia O cru e comparado aqui, passo a passo (`$mean[1]` das 48 series).
hp_compare <- function(env, gold, u, join_shift_min = 30L, tol = HP_TOL) {
  tz <- lubridate::tz(u$available_until)
  au1 <- lubridate::force_tz(lubridate::as_datetime(u$origin + 1L), tz)
  u2 <- u; u2$available_until <- au1; u2$day0_source <- "native"
  e2 <- env; e2$available_until <- au1
  e2$delivery <- env$delivery[env$delivery$interval_end > au1, ]
  cmp <- hp_tools$prevcarga_parity_compare(e2, gold, u2, tol = tol, delivery_days = 9L, join_shift_min = join_shift_min)
  d0 <- as.numeric(env$meta$day0_raw); g0 <- as.numeric(gold$native$meta$day0_raw)
  d0_shape <- length(d0) == 48L && length(g0) == 48L
  d0_na <- d0_shape && identical(is.na(d0), is.na(g0))
  d0_max <- if (d0_shape && any(!is.na(d0 - g0))) max(abs(d0 - g0), na.rm = TRUE) else if (d0_shape) 0 else Inf
  cmp$unit <- u$id
  cmp$day0_source <- u$day0_source
  cmp$day0_raw_max_abs_delta <- d0_max
  cmp$day0_raw_ok <- d0_na && d0_max <= tol
  cmp$max_abs_delta <- max(cmp$max_abs_delta, d0_max)
  cmp$ok <- isTRUE(cmp$ok) && cmp$day0_raw_ok
  cmp
}

## --- relatorio (gerado, nao versionado) ---------------------------------------------------------------------
hp_report_add <- function(kind, name, value) {
  .hp$report[[kind]][[name]] <- value
  invisible(NULL)
}

hp_report_write <- function(path = hp_report_path()) {
  rep <- .hp$report
  doc <- list(model = HP_MODEL, tolerance = HP_TOL, units = unname(lapply(rep$units, hp_tools$.pc_golden_canon)),
              controls = hp_tools$.pc_golden_canon(rep$controls),
              day0_coverage = paste("o resto do dia O pos-SemiFer/LOESS (delivery de O) nao tem contrapartida direta no oraculo:",
                                    "coberto pelo passo cru (meta$day0_raw, comparado) + as mesmas funcoes provadas em O+1..O+8"))
  txt <- jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null", digits = NA, na = "null", pretty = TRUE)
  writeBin(charToRaw(paste0(paste(as.character(txt), collapse = "\n"), "\n")), path)
  digest::digest(path, algo = "sha256", file = TRUE)
}

## --- deslocamento de dia (gate11 do oraculo) ----------------------------------------------------------------
## delivery (interval_end, y_pred) x y_true do store (interval_end): mesmo dia rotulado vs dia anterior (y_true + 1 dia).
hp_dayshift_verdict <- function(delivery, y_true) {
  same <- merge(delivery, y_true, by = "interval_end")
  prev <- merge(delivery, transform(y_true, interval_end = interval_end + lubridate::days(1)), by = "interval_end")
  w <- hp_tools$prevcarga_wmape
  m <- c(same = w(same$y_true, same$y_pred), prev = w(prev$y_true, prev$y_pred))
  r <- c(same = stats::cor(same$y_true, same$y_pred), prev = stats::cor(prev$y_true, prev$y_pred))
  list(ok = isTRUE(m[["same"]] < m[["prev"]]) && isTRUE(r[["same"]] > r[["prev"]]), wmape = m, corr = r,
       n = c(same = nrow(same), prev = nrow(prev)))
}
