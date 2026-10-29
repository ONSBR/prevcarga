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
