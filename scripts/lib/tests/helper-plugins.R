## helper-plugins.R -- fixtures sentinela da fundacao de plugins (P.0a; P.0b/P.0c/P.0d/P.0j acrescentam).
## Auto-carregado por testthat::test_dir. Nao e codigo de produto. Tudo por lubridate com tz explicito, sem env vars.
##
## Sentinela: `load_mwh` e `temperature` codificam os MINUTOS desde 2026-09-01 00:00 (no tz do painel) do
## `interval_start`/`valid_time`. Assim qualquer deslocamento de rotulo ou agrupamento errado muda o valor.

.PC_SENTINEL_BASE <- "2026-09-01 00:00:00"

pc_sentinel_minutes <- function(t, tz = lubridate::tz(t)) {
  base <- lubridate::ymd_hms(.PC_SENTINEL_BASE, tz = tz)
  as.numeric(difftime(t, base, units = "mins"))
}

## v (minutos) -> instante no tz dado
pc_decode_sentinel <- function(v, tz = "Etc/GMT+3") {
  lubridate::ymd_hms(.PC_SENTINEL_BASE, tz = tz) + lubridate::minutes(as.integer(round(v)))
}

## Painel no formato do M2.7 (lista com `attrs`). Dias O .. O+days-1 em grade de 30 min.
## `drop`: vetor POSIXct de interval_start a remover da carga (ex.: deixa a meia hora isolada).
## Previsto: duas emissoes por area (00Z e 06Z em UTC); temperature = sentinela + 1e6 * indice da emissao.
pc_sentinel_panel <- function(origin, days, areas, tz = "Etc/GMT+3", drop = NULL) {
  O <- lubridate::as_date(origin)
  t0 <- lubridate::force_tz(lubridate::as_datetime(O), tz)
  starts <- t0 + lubridate::minutes(30L * (seq_len(48L * days) - 1L))
  load <- data.table::rbindlist(lapply(areas, function(a) data.table::data.table(
    area_code = a, interval_start = starts, interval_end = starts + lubridate::minutes(30),
    load_mwh = pc_sentinel_minutes(starts, tz))))
  if (!is.null(drop)) load <- load[!(interval_start %in% drop)]
  obs <- data.table::rbindlist(lapply(areas, function(a) data.table::data.table(
    area_code = a, valid_time = starts, temperature = pc_sentinel_minutes(starts, tz))))
  issues <- lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(c(0L, 6L))
  fc <- data.table::rbindlist(lapply(areas, function(a) data.table::rbindlist(lapply(seq_along(issues), function(k)
    data.table::data.table(area_code = a, issue_time = issues[[k]], valid_time = starts,
                           temperature = pc_sentinel_minutes(starts, tz) + 1e6 * (k - 1L))))))
  hol <- data.table::data.table(area_code = areas, date = rep(O, length(areas)),
                                holiday_type_id = 1L, prevcarga_code = 2L, simplified_code = 3L)
  au <- max(load$interval_end)
  list(load = load, weather_observed = obs, weather_forecast = fc, holidays = hol,
       attrs = list(origin = O, available_until = au, load_label = "end", tz = tz,
                    weather_issue_cutoff = "available_until", weather_issue_limit = au))
}

## Regra-brinquedo: cada hora vira 2 intervalos de 30 min com o mesmo valor (vetor y_pred de 2n).
pc_rule_split <- function(x) rep(x$y_pred, each = 2L)

## CONTROL: desloca o carimbo em 30 min (interval_end fora da ancora).
pc_rule_shifted <- function(x) {
  stamp <- sort(c(x$interval_end - lubridate::minutes(30), x$interval_end))
  data.table::data.table(interval_end = stamp + lubridate::minutes(30), y_pred = rep(x$y_pred, each = 2L))
}

## CONTROL: agrupa a carga por interval_end (errado) em vez de interval_start.
.pc_to_hourly_by_end <- function(load) {
  dt <- data.table::copy(data.table::as.data.table(load))
  dt[, hour_end := lubridate::floor_date(interval_end, "hour")]
  dt[, n_slots := .N, by = .(area_code, hour_end)]
  out <- dt[n_slots == 2L, .(load_mwh = mean(load_mwh)), by = .(area_code, interval_start = hour_end)]
  data.table::setorder(out, area_code, interval_start)
  out[]
}

## --- Fabrica de variantes dos plugins-brinquedo (P.0b) --------------------------------------------------------
pc_toy_root <- function() file.path(pc_root(), "scripts", "lib", "tests", "fixtures", "plugins")

.pc_dput <- function(x) paste(deparse(x, control = c("keepNA", "keepInteger", "niceNames", "showAttributes")),
                             collapse = "\n")

## Copia o brinquedo `base` (e o motor) para um tempdir e devolve o `root` novo.
##  run_yaml  : modifyList no run.yaml (NULL remove a chave);  descriptor : lista (NULL remove o campo) ou
##  function(d) -> d, aplicada sobre o descriptor original por R/zz_override.R;  files : nome relativo -> linhas;
##  engine    : modifyList no engine_manifest();  engine_files : nome em _toy-engine/R -> linhas;
##  name      : nome do plugin na copia (ex.: toy-60b); ajusta run.yaml e model_id.
pc_toy_variant <- function(base, run_yaml = list(), descriptor = list(), files = list(), engine = list(),
                           engine_files = list(), name = base) {
  root <- tempfile("toyroot-"); dir.create(root)
  dir.create(file.path(root, name))
  file.copy(list.files(file.path(pc_toy_root(), base), full.names = TRUE), file.path(root, name), recursive = TRUE)
  file.copy(file.path(pc_toy_root(), "_toy-engine"), root, recursive = TRUE)
  ry <- file.path(root, name, "run.yaml")
  y <- yaml::read_yaml(ry); y$name <- name
  yaml::write_yaml(modifyList(y, run_yaml), ry)
  ov <- descriptor
  if (name != base || length(ov)) {
    pf <- file.path(root, name, "R", "plugin.R")
    writeLines(sub("^plugin_descriptor <- function", ".plugin_descriptor_base <- function", readLines(pf)), pf)
    body <- if (is.function(ov)) paste0("f <- ", .pc_dput(ov), "\n  d <- f(d)") else paste0(
      "ov <- ", .pc_dput(ov), "\n  for (n in names(ov)) d[[n]] <- ov[[n]]")
    writeLines(c("plugin_descriptor <- function() {", "  d <- .plugin_descriptor_base()",
                 if (name != base) sprintf("  d$model_id <- \"%s\"", name), paste0("  ", body), "  d", "}"),
               file.path(root, name, "R", "zz_override.R"))
  }
  for (f in names(files)) {
    dir.create(dirname(file.path(root, name, f)), showWarnings = FALSE, recursive = TRUE)
    writeLines(files[[f]], file.path(root, name, f))
  }
  if (length(engine)) {
    e <- new.env(); sys.source(file.path(root, "_toy-engine", "R", "manifest.R"), envir = e)
    m <- modifyList(e$engine_manifest(), engine)
    writeLines(c("engine_manifest <- function()", paste0("  ", .pc_dput(m))), file.path(root, "_toy-engine", "R", "manifest.R"))
  }
  for (f in names(engine_files)) writeLines(engine_files[[f]], file.path(root, "_toy-engine", "R", f))
  root
}

pc_globalenv_snapshot <- function() list(globalenv = ls(globalenv(), all.names = TRUE), search = search())

## --- Artefatos-brinquedo do contrato freeze/thaw (P.0d) ------------------------------------------------------------
## cuts literais (sem M2.3): origin_agnostic => train_origin = O - 1 - H; origin_scoped => train_origin = O e origin_day = O.
pc_toy_cuts <- function(origin, available_until = NULL, split = c("origin_agnostic", "origin_scoped"),
                        horizon_days = 1L, tz = "Etc/GMT+3") {
  split <- match.arg(split)
  O <- lubridate::as_date(origin)
  au <- if (is.null(available_until)) lubridate::force_tz(lubridate::as_datetime(O), tz) else available_until
  list(forecast_origin = O, origin_day = O, split = split, tz = tz, load_label = "end",
       weather_issue_cutoff = "available_until", available_until = au, weather_issue_limit = au,
       train_origin = if (split == "origin_scoped") O else O - 1L - as.integer(horizon_days),
       last_data_day = O - 1L)
}

## Treina o brinquedo pelos estagios do env carregado e devolve o artefato carimbado.
pc_toy_artifact <- function(model = c("toy-30", "toy-60"), cuts, spec_overrides = list()) {
  model <- match.arg(model)
  lib <- parent.frame()  # o env do teste, onde o pc_source_lib carregou registry.R
  p <- get("prevcarga_plugin_load", envir = lib)(model, pc_toy_root())
  spec <- modifyList(p$spec, spec_overrides)
  tz <- cuts$tz
  panel <- pc_sentinel_panel(cuts$forecast_origin - 10L, 10L, "SE", tz = tz)
  st <- p$descriptor$stages
  frame <- st$ingestion$fn(panel, cuts)
  feats <- st$feature_engineering$fn(frame, spec$params)
  fit <- st$training$fn(feats, spec$params, spec$seed)
  payload <- p$descriptor$freeze(fit)
  get("prevcarga_plugin_stamp", envir = lib)(payload, p, spec, cuts,
                         ctx = list(load_label = panel$attrs$load_label, timezone = tz,
                                    weather_issue_cutoff = panel$attrs$weather_issue_cutoff))
}

## Planta `value` num caminho do artefato (ex.: "$payload$fit$x" ou "$payload$model"); cria niveis de lista.
pc_inject <- function(artifact, path, value) {
  keys <- strsplit(sub("^\\$", "", path), "$", fixed = TRUE)[[1L]]
  keys <- keys[nzchar(keys)]
  set <- function(x, ks) {
    if (length(ks) == 1L) { x[[ks]] <- value; return(x) }
    x[[ks[1L]]] <- set(if (is.null(x[[ks[1L]]])) list() else x[[ks[1L]]], ks[-1L])
    x
  }
  set(artifact, keys)
}

## --- Variantes e espioes do despachante (P.0c) ---------------------------------------------------------------------
## Os brinquedos originais devolvem o payload em `inference`; o despachante exige a saida (area, values, delivery).
## Cada variante acrescenta R/zz_dispatch.R ao brinquedo (so definicoes), redefinindo os estagios; o resto do plugin
## (descriptor, run.yaml, motor) e o original. `native_min` e a convencao do PROPRIO brinquedo sobre o quadro de
## modelagem (a grade que ele devolve ja esta no quadro de modelagem); o despachante e quem desloca.

.pc_toy_dispatch_lines <- function(base, native_min = 0L, self_shift = 0L, train_extra = character(), kind = "ok") {
  days <- if (base == "toy-60") 9L else 2L
  head <- c(
    sprintf("toy_native_min <- function() %dL", as.integer(native_min)),
    sprintf("toy_self_shift <- function() %dL", as.integer(self_shift)),
    "toy_grid_end <- function(cuts, days) lubridate::force_tz(lubridate::as_datetime(cuts$origin_day + days), cuts$tz) - lubridate::minutes(toy_native_min())",
    "toy_slots <- function(ie) { s <- (lubridate::hour(ie) * 60L + lubridate::minute(ie)) %/% 30L; s[s == 0L] <- 48L; s }")
  stages <- if (base == "toy-30") c(
    "toy_training <- function(features, params, seed) {",
    paste0("  ", train_extra),
    "  m <- features$m",
    "  k <- min(params$k, nrow(m))",
    "  b <- colMeans(m[seq(nrow(m) - k + 1L, nrow(m)), , drop = FALSE])",
    "  set.seed(seed)",
    "  list(k = k, level = b + stats::rnorm(length(b)))",
    "}",
    "toy_inference_features <- function(frame, payload) list(area = frame$load$area_code[1L])",
    "toy_inference_core <- function(payload, inf_features, cuts) {",
    sprintf("  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, %dL), by = \"30 min\")", days),
    "  list(area = inf_features$area, values = payload$model$level,",
    "       delivery = data.table::data.table(interval_end = ie, y_pred = payload$model$level[toy_slots(ie)]))",
    "}") else c(
    "toy_ingestion <- function(panel, cuts) list(load = panel$load, area = panel$load$area_code[1L])",
    "toy_feature_engineering <- function(frame, params) list(x = frame$load$load_mwh[lubridate::hour(frame$load$interval_start) < 12L])",
    "toy_training <- function(features, params, seed) {",
    paste0("  ", train_extra),
    "  engine_fit(features$x)",
    "}",
    "toy_inference_features <- function(frame, payload) list(area = frame$area)",
    "toy_inference_core <- function(payload, inf_features, cuts) {",
    sprintf("  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, %dL), by = \"30 min\")", days),
    "  list(area = inf_features$area, values = payload$model$level,",
    "       delivery = data.table::data.table(interval_end = ie, y_pred = rep(payload$model$level, length(ie))))",
    "}",
    "toy_forecast <- function(payload, inf_features, cuts) toy_inference_core(payload, inf_features, cuts)")
  tail <- c(
    "toy_inference <- function(payload, inf_features, cuts) {",
    "  out <- toy_inference_core(payload, inf_features, cuts)",
    "  out$delivery$interval_end <- out$delivery$interval_end + lubridate::minutes(toy_self_shift())",
    switch(kind,
      ok = NULL,
      from_0900 = "  out$delivery$interval_end <- out$delivery$interval_end - lubridate::minutes(30L)",
      short_day = "  out$delivery <- out$delivery[-seq(nrow(out$delivery) - 47L, nrow(out$delivery))]",
      na = "  out$delivery$y_pred[5L] <- NA_real_",
      dup = "  out$delivery <- out$delivery[c(1L, seq_len(nrow(out$delivery)))]",
      lead_field = "  out$lead <- 1L"),
    "  out",
    "}")
  c(head, stages, tail)
}

## Acha a funcao da lib na pilha de chamadas (o env do teste onde pc_source_lib carregou o registry.R).
.pc_find_lib <- function(name) {
  for (f in rev(sys.frames())) if (exists(name, envir = f, inherits = TRUE)) return(get(name, envir = f, inherits = TRUE))
  stop(".pc_find_lib: ", name, " nao encontrado")
}

## Carrega o brinquedo como plugin do despachante. `descriptor`/`run_yaml`: como em pc_toy_variant.
pc_toy_disp <- function(base = c("toy-30", "toy-60"), descriptor = list(), run_yaml = list(), native_min = 0L,
                        self_shift = 0L, train_extra = character(), kind = "ok") {
  base <- match.arg(base)
  root <- pc_toy_variant(base, run_yaml = run_yaml, descriptor = descriptor,
    files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines(base, native_min, self_shift, train_extra, kind)))
  .pc_find_lib("prevcarga_plugin_load")(base, root)
}

## toy-30 com o atraso de dados declarado (teto de treino O - 1 - data_lag_days).
pc_toy_lag <- function(data_lag_days)
  pc_toy_disp("toy-30", descriptor = list(uses_rng = TRUE, data_lag_days = as.integer(data_lag_days)))

## toy-60 sensivel a hora do dia (treina so as horas < 12 do quadro de modelagem), com model_time_offset_min = o.
pc_toy_offset <- function(o, self_shift = 0L)
  pc_toy_disp("toy-60", descriptor = list(model_time_offset_min = as.integer(o)), native_min = o, self_shift = self_shift)

## toy-30 cujo treino planta um simbolo no globalenv.
pc_toy_leaky <- function()
  pc_toy_disp("toy-30", descriptor = list(uses_rng = TRUE), train_extra = "assign('x', 1, envir = globalenv())")

pc_toy_misaligned <- function(kind = c("from_0900", "short_day", "na", "dup", "lead_field")) {
  kind <- match.arg(kind)
  pc_toy_disp("toy-30", descriptor = list(uses_rng = TRUE), kind = kind)
}

## Espiao: troca a `fn` do estagio por um wrapper que conta chamadas e guarda os argumentos recebidos.
pc_spy <- function(plugin, stage) {
  spy <- new.env()
  spy$calls <- 0L
  spy$args <- list()
  orig <- plugin$descriptor$stages[[stage]]$fn
  plugin$descriptor$stages[[stage]]$fn <- function(...) {
    spy$calls <- spy$calls + 1L
    spy$args[[length(spy$args) + 1L]] <- list(...)
    orig(...)
  }
  plugin$spy <- spy
  plugin
}

## Fatias do painel sentinela: treino (ate last_data_day) e infer (carga ate available_until).
pc_train_slice <- function(panel, cuts) {
  keep <- function(dt, col) dt[lubridate::as_date(dt[[col]]) <= cuts$last_data_day]
  out <- panel
  out$load <- keep(panel$load, "interval_start")
  out$weather_observed <- keep(panel$weather_observed, "valid_time")
  out$weather_forecast <- keep(panel$weather_forecast, "valid_time")
  out$attrs$available_until <- max(out$load$interval_end)
  out
}

pc_infer_slice <- function(panel, cuts) {
  out <- panel
  out$load <- panel$load[interval_end <= cuts$available_until]
  out$weather_observed <- panel$weather_observed[valid_time <= cuts$available_until]
  out$weather_forecast <- panel$weather_forecast[issue_time <= cuts$weather_issue_limit]
  out$attrs$available_until <- cuts$available_until
  out$attrs$weather_issue_limit <- cuts$weather_issue_limit
  out
}

## Planta no painel uma violacao: day_O (intervalo do dia O), end_0930 (interval_end = available_until + 30 min),
## late_issue (emissao depois do limite), no_load_mwh (remove a coluna) ou timestamp (coluna crua na carga).
pc_panel_with <- function(panel, what = c("day_O", "end_0930", "late_issue", "no_load_mwh", "timestamp")) {
  what <- match.arg(what)
  out <- panel
  ld <- data.table::copy(panel$load)
  tz <- panel$attrs$tz
  add_load <- function(start) {
    row <- ld[1L]
    row[, `:=`(interval_start = start, interval_end = start + lubridate::minutes(30L))]
    data.table::rbindlist(list(ld, row))
  }
  switch(what,
    day_O = out$load <- add_load(lubridate::force_tz(lubridate::as_datetime(panel$attrs$origin), tz)),
    end_0930 = out$load <- add_load(panel$attrs$available_until),
    late_issue = {
      wf <- data.table::copy(panel$weather_forecast)
      row <- wf[1L]
      row[, issue_time := panel$attrs$weather_issue_limit + lubridate::hours(1L)]
      out$weather_forecast <- data.table::rbindlist(list(wf, row))
    },
    no_load_mwh = { ld[, load_mwh := NULL]; out$load <- ld },
    timestamp = { ld[, timestamp := interval_end]; out$load <- ld })
  out
}

## --- Oraculo-brinquedo do gerador de golden (P.0f) -------------------------------------------------------------------
## Repo git em tempdir com 1 commit (user.* local ao repo); devolve o caminho.
pc_fake_oracle <- function() {
  d <- tempfile("fake-oracle-"); dir.create(d)
  git <- function(...) system2("git", c("-C", shQuote(d), ...), stdout = FALSE, stderr = FALSE)
  git("init", "-q")
  git("config", "user.email", "t@example.org"); git("config", "user.name", "t")
  writeLines("toy oracle", file.path(d, "README.txt"))
  git("add", "README.txt"); git("commit", "-q", "-m", "init")
  normalizePath(d, winslash = "/")
}

## Escreve `<root>/<model>/tests/parity/oracle_driver.R` (contrato --oracle --unit --out [--available-until]).
## ok: ecoa os argumentos (sem --out/--unit) e as variaveis de ambiente do filho num native.rds deterministico;
## nondet: o valor depende de Sys.time(); writes_oracle: escreve um arquivo dentro do oraculo; fails: sai com status 1.
pc_toy_driver <- function(root, model, kind = c("ok", "nondet", "writes_oracle", "fails")) {
  kind <- match.arg(kind)
  d <- file.path(root, model, "tests", "parity"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "a <- commandArgs(trailingOnly = TRUE)",
    "k <- a[seq(1L, length(a), by = 2L)]; v <- a[seq(2L, length(a), by = 2L)]",
    "args <- as.list(stats::setNames(v, k)); out <- args[['--out']]",
    "args <- args[setdiff(names(args), c('--out', '--unit'))]",
    "nm <- c('TZ', 'LC_ALL', 'OMP_NUM_THREADS', 'LGBM_TRAIN_CV', 'EXOGENA')",
    "env <- as.list(Sys.getenv(nm, unset = NA))",
    switch(kind,
      ok = NULL,
      nondet = "args$stamp <- as.numeric(Sys.time())",
      writes_oracle = "writeLines('x', file.path(args[['--oracle']], 'touched.txt'))",
      fails = c("message('boom-tail-marker'); quit(status = 1L)")),
    "saveRDS(list(args = args, env = env), file.path(out, 'native.rds'))",
    "arrow::write_parquet(data.frame(a = 1:3, y = c(0.5, 1, 1.5)), file.path(out, 'long.parquet'))"),
    file.path(d, "oracle_driver.R"))
  d
}

## units.yaml de 2 unidades: a 2a com oracle_env e oracle_cut start_label (available_until 09:00).
pc_units_two <- function(root, model = "toy-30", extra = NULL) {
  f <- file.path(root, model, "tests", "parity", "units.yaml"); dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("units:",
    "  - {id: u1, area: SE, origin: 2026-10-01, available_until: 2026-10-01T09:00, weather_issue_cutoff: available_until, day0_source: native}",
    "  - id: u2", "    area: SECO", "    origin: 2026-10-01", "    available_until: 2026-10-01T09:00",
    "    weather_issue_cutoff: origin_midnight_utc", "    day0_source: oracle_origin_minus_1",
    "    oracle_env: {LGBM_TRAIN_CV: \"0\"}", "    oracle_cut: start_label", extra), f)
  f
}

## --- Goldens-brinquedo do comparador de paridade (P.0g) ------------------------------------------------------------
## Valor sintetico por celula e por interval_end; nao ha oraculo nem modelo. Usa as funcoes de plugin_golden.R/plugin_parity.R
## carregadas no env do teste (achadas pela pilha, como .pc_find_lib).
.pc_toy_fp <- new.env()
.pc_toy_fingerprint <- function(packages = c("lubridate", "data.table")) {
  key <- paste(packages, collapse = ",")
  if (is.null(.pc_toy_fp[[key]])) .pc_toy_fp[[key]] <- .pc_find_lib("pc_runtime_fingerprint")(packages, pc_rscript_bin())
  .pc_toy_fp[[key]]
}

.pc_toy_cell <- function(r, c) 100 + 10 * r + 0.25 * c
.pc_toy_y <- function(end) 200 + (as.numeric(end) %/% 1800) %% 97

## Grava native.rds/long.parquet/invocation.json em <out>/plugins/<model>/_/<area>/<origin>/ e o golden.lock da unidade
## em <lock_root>/<model>/tests/parity/. Devolve o diretorio da unidade.
.pc_toy_golden_write <- function(unit, native, long, model, out, lock_root, data_dir = NULL,
                                 packages = c("lubridate", "data.table")) {
  d <- .pc_find_lib(".pc_golden_unit_dir")(out, model, unit)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  saveRDS(native, file.path(d, "native.rds"))
  lg <- data.frame(timestamp = lubridate::force_tz(long$timestamp, "UTC"), y_pred = long$y_pred)
  arrow::write_parquet(lg, file.path(d, "long.parquet"))
  writeLines("{}", file.path(d, "invocation.json"))
  sha <- .pc_find_lib(".pc_golden_sha")
  files <- vapply(c("native.rds", "long.parquet", "invocation.json"), function(f) sha(file.path(d, f)), "")
  lockf <- file.path(lock_root, model, "tests", "parity", "golden.lock")
  dir.create(dirname(lockf), recursive = TRUE, showWarnings = FALSE)
  .pc_find_lib("pc_golden_lock_update")(lockf, model, unit$id, files, "toycommit",
    .pc_find_lib("pc_data_digest")(data_dir, unit), .pc_toy_fingerprint(packages))
  d
}

## layout: um dos 4 comparaveis. n_days: dias entregues (1 ou 2 no lgbm; n nos de matriz). extra_day: o oraculo calcula 1 dia
## alem da faixa (long e matriz). day0_source: default por layout (rf: oracle_origin_minus_1; hw: oracle_discarded_step; demais: native).
pc_toy_golden <- function(layout, n_days = 2L, origin = "2026-10-01", extra_day = FALSE, day0_source = NULL,
                          tz = "Etc/GMT+3", model = "toy-30", out = tempfile("golden-"), lock_root = tempfile("lockroot-")) {
  O <- lubridate::ymd(origin)
  au <- lubridate::ymd_hm(paste(origin, "09:00"), tz = tz)
  if (is.null(day0_source)) day0_source <- switch(layout, rf_wide_hourly_216 = "oracle_origin_minus_1",
                                                  hw_matrix_nx48 = "oracle_discarded_step", "native")
  nr <- if (layout == "rf_wide_hourly_216") 2L else n_days + as.integer(extra_day)
  nc <- if (layout == "rf_wide_hourly_216") 216L else 48L
  truth <- outer(seq_len(nr), seq_len(nc), .pc_toy_cell)
  t0 <- lubridate::force_tz(lubridate::as_datetime(O), tz)
  first_end <- au + lubridate::minutes(30L)
  ds <- 123.5
  col0 <- as.integer(round(as.numeric(difftime(first_end, t0, units = "mins")) / 30))
  native_values <- truth
  meta <- list()
  sister <- NULL
  if (day0_source == "oracle_discarded_step") { truth[1L, col0] <- ds; native_values[1L, col0] <- NA_real_; meta$discarded_step <- ds }
  if (day0_source == "oracle_origin_minus_1") {
    native_values[1L, ] <- truth[1L, ] + 7                      # o oraculo nao grava a linha O-1 de forma util
    sister <- list(layout = layout, values = rbind(truth[1L, ] - 1, truth[1L, ]), meta = list())
  }
  ends_n <- 48L * (n_days + as.integer(extra_day)) - as.integer(round(as.numeric(difftime(au, t0, units = "mins")) / 30))
  ends <- au + lubridate::minutes(30L * seq_len(ends_n))
  y <- .pc_toy_y(ends)
  long <- data.frame(timestamp = ends - lubridate::minutes(30L), y_pred = y)
  if (day0_source == "oracle_discarded_step") long <- long[-1L, ]
  unit <- list(id = "u1", area = "SE", origin = O, available_until = au, weather_issue_cutoff = "available_until",
               day0_source = day0_source, variant = NULL, history_days = NULL)
  native <- list(layout = layout, values = native_values, meta = meta)
  dir <- .pc_toy_golden_write(unit, native, long, model, out, lock_root)
  ## o long em memoria, ja no tz da unidade (como .pc_parity_read_dir o devolve)
  lg <- list(native = native, long = long, sister = sister)
  if (!is.null(sister)) {
    su <- modifyList(unit, list(id = "u0", origin = O - 1L))
    .pc_toy_golden_write(su, sister, long, model, out, lock_root)
  }
  list(layout = layout, unit = unit, truth = truth, golden = lg, dir = dir, out = out, lock_root = lock_root, model = model,
       n_days = n_days, tz = tz, yfun = .pc_toy_y, ds = ds, au = au)
}

## Envelope do clean-room igual ao golden (values = a verdade; delivery = a grade do membro); `delta` soma a uma celula de `where`.
pc_toy_envelope_from <- function(g, cuts = NULL, delivery_days = g$n_days, delta = 0, where = c("values", "delivery")) {
  where <- match.arg(where)
  if (is.null(cuts)) cuts <- list(available_until = g$au, tz = g$tz, origin_day = g$unit$origin)
  grid <- .pc_find_lib("prevcarga_delivery_grid")(cuts, delivery_days)
  y <- g$yfun(grid)
  if (g$unit$day0_source == "oracle_discarded_step") y[1L] <- g$ds
  v <- g$truth
  if (g$layout != "rf_wide_hourly_216") v <- v[seq_len(min(nrow(v), delivery_days)), , drop = FALSE]   # o membro so entrega a faixa
  if (where == "values") v[1L, 3L] <- v[1L, 3L] + delta else y[3L] <- y[3L] + delta
  list(model_id = g$model, variant = NULL, area = "SE", origin_day = g$unit$origin, available_until = g$au,
       weather_issue_cutoff = "available_until", layout = g$layout, granularity_min = 30L, values = v,
       delivery = data.table::data.table(interval_end = grid, y_pred = y), meta = list())
}

## --- P.0h: brinquedos do runner de gates (plugin_gates.R) -------------------------------------------------------
## Escreve <root>/<name>/tests/{run_all_gates.R, testthat/test-*.R}. `tests` escolhe os gates: ok = contrato + corte
## [PLUGINS] verdes e schema_long [DEFERRED M6]; parity = paridade [PLUGINS] (tier parity, sem golden => FAIL);
## fail = determinismo [PLUGINS] forcado a falhar (CONTROL); unowned = teste sem rotulo de dono (CONTROL).
## O runner tem 3 linhas e chama so prevcarga_gates_run (nunca e editado para incluir packs).
pc_toy_gates_model <- function(root, name, tests = c("ok", "fail", "unowned", "parity"), gates_dir = NULL) {
  tests <- match.arg(tests, c("ok", "fail", "unowned", "parity"), several.ok = TRUE)
  tdir <- file.path(root, name, "tests", "testthat")
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  T <- function(file, ...) writeLines(c(...), file.path(tdir, file))
  gates <- list(gate = character(), owner = character(), tier = character())
  add <- function(g, o, t) gates <<- list(gate = c(gates$gate, g), owner = c(gates$owner, o), tier = c(gates$tier, t))
  if ("ok" %in% tests) {
    T("test-ok.R",
      'test_that("[PLUGINS] contrato: o runtime foi fixado antes dos testes", {',
      '  expect_identical(Sys.getenv("TZ"), "UTC")',
      '  expect_identical(Sys.timezone(), "UTC")',
      '})',
      'test_that("[PLUGINS] corte: sem dado alem do corte", { expect_true(TRUE) })',
      'test_that("[DEFERRED M6] schema_long: saida LONG", { pc_gate_deferred("M6", "LONG nasce no M6") })')
    add("contrato", "PLUGINS", "fast"); add("corte", "PLUGINS", "fast"); add("schema_long", "DEFERRED M6", "fast")
  }
  if ("parity" %in% tests) {
    T("test-parity.R",
      'test_that("[PLUGINS] paridade: golden do oraculo presente", {',
      '  pc_gate_tier("parity")',
      sprintf('  pc_gate_require_golden("%s", "u1")', name),
      '})')
    add("paridade", "PLUGINS", "parity")
  }
  if ("fail" %in% tests) {
    T("test-fail.R", 'test_that("[PLUGINS] determinismo: duas rodadas iguais", { expect_true(FALSE) })')
    add("determinismo", "PLUGINS", "fast")
  }
  if ("unowned" %in% tests) T("test-unowned.R", 'test_that("teste sem rotulo de dono", { expect_true(TRUE) })')
  gt <- sprintf("gate_table <- data.frame(gate = %s, owner = %s, tier = %s, stringsAsFactors = FALSE)",
                .pc_dput(gates$gate), .pc_dput(gates$owner), .pc_dput(gates$tier))
  call <- sprintf("prevcarga_gates_run(gate_table, tier = \"fast\", model = \"%s\"%s)", name,
                  if (is.null(gates_dir)) "" else sprintf(", gates_dir = \"%s\"", gates_dir))
  writeLines(c(sprintf("source(\"%s\")", file.path(pc_root(), "scripts", "tools", "plugin_gates.R")), gt, call),
             file.path(root, name, "tests", "run_all_gates.R"))
  invisible(file.path(root, name, "tests", "run_all_gates.R"))
}

## Packs de fixture em `dir`. specs: lista nomeada (nome do arquivo sem .R) de listas de linhas
## list(gate, applies_to, pass, owner = "PLUGINS", tier_min = "fast"); spec NULL => arquivo SEM a funcao do pack.
pc_toy_gate_packs <- function(dir, specs) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(specs)) {
    rows <- specs[[nm]]
    f <- file.path(dir, paste0(nm, ".R"))
    if (is.null(rows)) { writeLines("pc_sem_funcao <- 1", f); next }
    g <- vapply(rows, `[[`, "", "gate")
    ap <- vapply(rows, function(r) paste(deparse(as.character(r$applies_to)), collapse = ""), "")
    ow <- vapply(rows, function(r) if (is.null(r$owner)) "PLUGINS" else r$owner, "")
    tm <- vapply(rows, function(r) if (is.null(r$tier_min)) "fast" else r$tier_min, "")
    ts <- vapply(rows, function(r) if (isTRUE(r$pass)) "function() invisible(TRUE)" else
                   "function() stop(\"falha de fixture\")", "")
    writeLines(c(
      sprintf("prevcarga_gate_pack_%s <- function(model, ctx, tier) {", nm),
      sprintf("  d <- data.frame(gate = %s, owner = %s, tier_min = %s, stringsAsFactors = FALSE)", .pc_dput(g), .pc_dput(ow), .pc_dput(tm)),
      sprintf("  d$applies_to <- list(%s)", paste(ap, collapse = ", ")),
      sprintf("  d$test <- list(%s)", paste(ts, collapse = ", ")),
      "  d", "}"), f)
  }
  invisible(dir)
}

## Topologia-fixture (config/topology.yaml + models/<m>/run.yaml) em <base>; devolve o diretorio de modelos.
pc_toy_gates_topology <- function(base, models = c("toy-30", "toy-60", "toy-none")) {
  dir.create(file.path(base, "config"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  S: {areas: [RS], loss: PES}",
               "order: [S]", "members:",
               sprintf("  - {sigla: M%d, mdir: %s, exogena: null}", seq_along(models), models)),
             file.path(base, "config", "topology.yaml"))
  for (m in models) {
    dir.create(file.path(base, "models", m), recursive = TRUE, showWarnings = FALSE)
    writeLines("split: origin_agnostic", file.path(base, "models", m, "run.yaml"))
  }
  file.path(base, "models")
}

## ---- P.0j: higiene (grep de codigo) ------------------------------------------------------------------
## `allow`: lista nomeada "<basename> <padrao>" -> justificativa. Linhas so-comentario nunca contam.
## `lines`: vetor de pares (ini, fim) opcional por arquivo nao usado; use copia do trecho se precisar.
pc_grep_code <- function(paths, patterns, allow = list()) {
  out <- lapply(paths, function(f) {
    txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
    keep <- !grepl("^\\s*#", txt, perl = TRUE)
    hits <- lapply(names(patterns), function(p) {
      i <- which(keep & grepl(patterns[[p]], txt, perl = TRUE, useBytes = TRUE))
      if (length(i)) data.table::data.table(file = f, line = i, pattern = p, text = txt[i])
    })
    data.table::rbindlist(hits)
  })
  res <- data.table::rbindlist(out)
  if (!nrow(res)) res <- data.table::data.table(file = character(), line = integer(), pattern = character(), text = character())
  if (length(allow)) res <- res[!paste(basename(file), pattern) %in% names(allow)]
  res[]
}

## Padroes proibidos no codigo da fundacao (ADR-0012/0019/0035, CLAUDE.md principios 2 e 7).
pc_foundation_forbidden <- list(
  env      = "Sys[.](getenv|setenv)",   clock = "Sys[.](time|Date)[(]",
  base_dt  = "as[.]POSIXct|strptime|difftime|as[.]Date[(]", setwd = "setwd[(]",
  superass = "<<-",                       system = "(^|[^2a-z_])system[(]",
  fork     = "mclapply|parallel::mc",     global = "(^|[^.])library[(]|(^|[^.])source[(]",
  byname   = "switch[(]model|grepl[(]\"dessem|%in% c[(]\"lgbm",
  upper    = "PREVCARGA_[A-Z_]+"
)

## Bytes > 127 dentro de literais de mensagem (prevcarga_abort/message/cat/stop/warning).
pc_bytes_over_127 <- function(paths) {
  out <- lapply(paths, function(f) {
    txt <- readLines(f, warn = FALSE)
    keep <- !grepl("^\\s*#", txt, perl = TRUE)
    msg <- grepl("(prevcarga_abort|message|cat|stop|warning)[(]", txt, perl = TRUE)
    hi <- vapply(txt, function(s) any(as.integer(charToRaw(s)) > 127L), logical(1L), USE.NAMES = FALSE)
    i <- which(keep & msg & hi)
    if (length(i)) data.table::data.table(file = f, line = i, pattern = "bytes_127", text = txt[i])
  })
  res <- data.table::rbindlist(out)
  if (!nrow(res)) res <- data.table::data.table(file = character(), line = integer(), pattern = character(), text = character())
  res[]
}

## Aritmetica de hora proibida em models/ (so o despachante desloca hora). to_hourly/to_semihourly sao permitidos.
pc_time_arith_patterns <- function() list(
  period_arith = "[%+-]\\s*(lubridate::)?(hours|minutes|seconds|period)[(]|%m[+-]%",
  hours_call   = "(^|[^_a-zA-Z.])(lubridate::)?hours[(]\\s*[0-9]*\\s*[)]",
  minutes_30   = "(lubridate::)?minutes[(]\\s*30L?\\s*[)]",
  secs_3600    = "[+-]\\s*3600L?|[+-]\\s*60L?\\s*\\*\\s*60L?",
  day_secs     = "86400",
  difftime     = "as[.]difftime|difftime[(]",
  frame_fns    = "to_model_frame[(]|to_display_frame[(]",
  tz_fns       = "force_tz[(]|with_tz[(]"
)

## Excecoes versionadas e FECHADAS (arquivo + padrao -> justificativa). Nada alem destas.
pc_time_arith_allow <- function() list(
  "holt_winters.R period_arith" = "flag de horario de verao do HW: `+ hours(6)` (rotulo, nao deslocamento de frame)",
  "holt_winters.R hours_call"   = "mesmo `+ hours(6)` do HW (o padrao hours() tambem casa a linha)",
  "dessem_engine.R hours_call"  = "lag de feature hours(lagh) de obtemLagHora/obtemLagSemiHora (busca do valor defasado)",
  "dessem_engine.R period_arith" = "lag de feature `- hours(lagh)` de obtemLagHora/obtemLagSemiHora (busca do valor defasado)"
)

## Varre <model_dir>/R/*.R (+ o motor de descriptor$engine, quando existir `engine_dir`).
pc_grep_time_arith <- function(model_dir, allow = pc_time_arith_allow(), engine_dir = NULL) {
  files <- list.files(file.path(model_dir, "R"), pattern = "[.]R$", full.names = TRUE)
  if (!is.null(engine_dir)) files <- c(files, list.files(file.path(engine_dir, "R"), pattern = "[.]R$", full.names = TRUE))
  pc_grep_code(sort(files, method = "radix"), pc_time_arith_patterns(), allow)
}
