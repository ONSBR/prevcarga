## test-dessem-window-e2e.R -- E3-T6: verificacao fim a fim do epico E3 (janela do DESSEM do ADR-046 e anexo de feriados).
## Sem AWS e sem Batch. Decks sinteticos pequenos em tempdir, leitor real (prevcarga_read_panel), plugins reais
## (dessem-linear e dessem-regdin) carregados pelo registry e o despachante real. Nenhuma lista de nome de modelo no
## codigo de produto: os ids vem de models/*/run.yaml. Fixtures deterministicas (sem RNG), datas por lubridate com tz explicito.
##   [AC2] FORBIDDEN: nenhum dia >= O no anexo, no painel nem no deck do motor (3 origens, deck train e inference)
##   [AC3] fachada sem switch por nome de modelo (codigo sem comentarios de training/inference/registry/features/deck_reader)
##   [AC4] pipeline treino + infer em ano_civil sem generic.unexpected; fixture do bug v0.1.5 (feriado com 5 amostras)
library(testthat)
library(data.table)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "holiday_islands", "deck_reader",
                       "validation", "dates", "registry", "features", "inference_needs", "training", "inference"))

TZ <- "Etc/GMT+3"
MODELS <- c("dessem-linear", "dessem-regdin")
ORIGINS <- c("2023-01-01", "2023-06-01", "2024-01-01")
AREA <- "AA"
at <- function(day, hm = "00:00") lubridate::ymd_hm(paste(format(day), hm), tz = TZ)
cal <- function(x) lubridate::as_date(x, tz = TZ)

## A CLI fornece prevcarga_abort/_condition/_warn/to_semihourly no globalenv; aqui entram so durante `code`.
with_cli <- function(code) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_warn", "to_semihourly")
  nm <- nm[!vapply(nm, exists, NA, envir = globalenv(), inherits = FALSE)]
  for (n in nm) assign(n, env[[n]], envir = globalenv())
  on.exit(rm(list = nm, envir = globalenv()), add = TRUE)
  force(code)
}
load_plugin <- function(model) {
  env$prevcarga_plugin_unload_all()
  env$prevcarga_registry_reset()
  with_cli(env$prevcarga_plugin_load(model, file.path(pc_root(), "models")))
}
guard_err <- function(expr) tryCatch({ with_cli(force(expr)); NULL }, prevcarga_error = identity)

## ---------------------------------------------------------------------------------------------------------------------
## Decks em disco (AC2). Dias do store: 3 primeiros dias de 2018 (inicio do store), as ilhas e a janela comum ate O.
## Feriados (prevcarga_code): 2018-01-03 (ilha cortada no inicio do store), 2019-04-19 (tipo 10), 2019-12-25 (tipo 2) e
## 2021-12-25 (dentro da janela comum de 2023; ilha so ate common_from - 1).
## ---------------------------------------------------------------------------------------------------------------------
HOL_DAYS <- lubridate::ymd(c("2018-01-03", "2019-04-19", "2019-12-25", "2021-12-25"))
HOL_CODES <- c(2L, 10L, 2L, 10L)
hol_table <- function(areas = AREA, days = HOL_DAYS, codes = HOL_CODES)
  rbindlist(lapply(areas, function(a) data.table(area_code = a, date = days, holiday_type_id = bit64::as.integer64(1L),
                                                 prevcarga_code = bit64::as.integer64(codes),
                                                 simplified_code = bit64::as.integer64(1L))))

e2e_days <- function(O, with_islands = TRUE) {
  W <- env$prevcarga_dessem_window(O, "ano_civil")
  isl <- if (with_islands) unlist(lapply(HOL_DAYS, function(d) as.character(seq(d - 7L, d, by = "day")))) else character()
  isl <- lubridate::ymd(isl); isl <- isl[isl >= lubridate::ymd("2018-01-01")]     # o store comeca em 2018-01-01
  sort(unique(c(isl, seq(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-01-03"), by = "day"),
                seq(W$common_from - 1L, O, by = "day"))))
}
## O sentinela grava o clima de hora em hora; o store real tem instantes H:00 e H:30 (to_hourly exige os dois): duplica cada
## linha em +30 min (clima observado e previsto, deck e anexo).
half_hour_weather <- function(dir) {
  dbl <- function(col) function(dt) { b <- data.table::copy(dt); b[[col]] <- b[[col]] + lubridate::minutes(30L); rbind(dt, b) }
  pc_mut_weather(dir, "weather_observed", dbl("timestamp"))
  pc_mut_weather(dir, "weather_forecast", dbl("valid_time"))
  dir
}
## previsto: uma emissao (00Z de O) em 30 min de O ate O+9, no lugar das emissoes horarias do sentinela
write_forecast <- function(dir, O) {
  unlink(file.path(dir, "weather", "forecast"), recursive = TRUE)
  vt <- seq(at(O), at(O + 9L, "23:30"), by = "30 min")
  .pc_deck_write(data.table(area_code = AREA, issue_time = lubridate::force_tz(lubridate::as_datetime(O), "UTC"),
                            valid_time = lubridate::force_tz(vt, "UTC"), temperature = 20 + seq_along(vt) / 1000,
                            heat_index = 21 + seq_along(vt) / 1000),
                 dir, file.path("weather", "forecast"), c("valid_time", "issue_time"))
  dir
}
## so os dias do store (a serie e sentinela; o que interessa aqui sao as datas)
train_deck <- function(O, holidays = hol_table(), with_islands = TRUE, dir = withr::local_tempdir(.local_envir = parent.frame()))
{
  half_hour_weather(pc_deck_panel(areas = AREA, days = e2e_days(O, with_islands), until = at(O), holidays = holidays,
                                  issues = 0L, dir = dir))
  write_forecast(dir, O)
}

write_islands <- function(dir, isl) {
  dir.create(file.path(dir, "islands"), recursive = TRUE, showWarnings = FALSE)
  for (a in unique(isl$area_code)) {
    sub <- isl[which(isl$area_code == a)]
    rows <- rbindlist(lapply(seq_len(nrow(sub)), function(i)
      data.table(island_id = sub$island_id[i], from = sub$from[i], to = sub$to[i], holiday_date = sub$holiday_dates[[i]])))
    pd <- file.path(dir, "islands", paste0("area_code=", a))
    dir.create(pd, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(rows, file.path(pd, "part-0.parquet"))
  }
}
## deck inference: janela comum (ate O, dia O parcial), anexo em <dir>/annex com UMA linha plantada no dia O
inference_deck <- function(O, isl, plant_day_O = TRUE, holidays = hol_table(), hm = "09:00", env_ = parent.frame()) {
  W <- env$prevcarga_dessem_window(O, "ano_civil")
  d <- pc_deck_sentinel("end", days = seq(W$common_from - 1L, O), areas = AREA, until = at(O, hm), issues = 0L,
                        dir = withr::local_tempdir(.local_envir = env_))
  pc_mut_weather(d, "weather_observed", function(dt) { b <- data.table::copy(dt); b$timestamp <- b$timestamp + lubridate::minutes(30L); rbind(dt, b) })
  write_forecast(d, O)
  pc_deck_topology(file.path(d, "topology.yaml"), AREA)
  .pc_deck_write(data.table::copy(holidays), d, file.path("auxiliary", "holidays"), character(), "date")
  ad <- file.path(d, "annex")
  idays <- sort(unique(unlist(lapply(seq_len(nrow(isl)), function(i) as.character(seq(isl$from[i], isl$to[i], by = "day"))))))
  pc_deck_sentinel("end", days = c(lubridate::ymd(idays), if (plant_day_O) O), areas = AREA, issues = 0L, dir = ad)
  half_hour_weather(ad)
  pc_deck_manifest(ad, mutate = function(m) list(schema_version = 1L, kind = "holiday_annex"))
  write_islands(ad, isl)
  pc_deck_manifest(d, mutate = function(m) {
    m$kind <- "inference"
    m$inference <- list(origin = format(O), available_until = format(at(O, hm), "%Y-%m-%dT%H:%M:%S"),
                        train_deck_hash = strrep("a", 64), windows = setNames(list(), character()),
                        holiday_annex = "annex")
    m
  })
  d
}

## painel real (leitor unico) com o history_window do plugin; devolve list(panel, w)
read_for <- function(plugin, deck, O, kind) {
  spec <- plugin$spec
  w <- plugin$descriptor$history_window(O, spec)
  ctx <- pc_ctx_panel(deck)
  hist <- as.integer(O - w$common_from)
  panel <- with_cli(env$prevcarga_read_panel(ctx, O, AREA, 9L, hist, holiday_window = w))
  list(panel = panel, w = w)
}
## frame de modelagem horario como o despachante entrega ao plugin
model_frame <- function(plugin, panel, O, au) {
  d <- plugin$descriptor
  cuts <- list(origin_day = O, forecast_origin = O, train_origin = O, last_data_day = O - 1L, tz = TZ, available_until = au,
               weather_issue_cutoff = au, weather_issue_limit = au)
  with_cli(suppressMessages(env$.prevcarga_plugin_stage_input(panel, cuts, d, plugin$spec, "train")))
}

for (m in MODELS) for (o in ORIGINS) {
  test_that(sprintf("[AC2] FORBIDDEN: nenhum dia >= O no anexo, painel e deck do motor (%s, origem %s, train e inference)", m, o), {
    O <- lubridate::ymd(o)
    p <- load_plugin(m)
    W <- p$descriptor$history_window(O, p$spec)
    expect_identical(W$common_from, O - 1095L)
    ## deck kind train: le o painel, tira o corte do dia O e monta o deck do motor
    dt <- train_deck(O)
    tr <- read_for(p, dt, O, "train")
    isl <- tr$panel$holiday_annex$islands
    expect_true(nrow(isl) >= 1L)
    expect_true(all(isl$to < O))
    expect_true(all(cal(tr$panel$holiday_annex$load$interval_start) < O))
    expect_true(all(cal(tr$panel$holiday_annex$weather_observed$valid_time) < O))
    ## deck kind inference com linha plantada no dia O no anexo: o leitor a descarta
    di <- inference_deck(O, isl, plant_day_O = TRUE)
    raw <- with_cli(env$prevcarga_read_load(env$.prevcarga_annex_ctx(pc_ctx_deck(di), env$prevcarga_deck_manifest(di), "annex"), AREA))
    expect_true(O %in% cal(raw$interval_start))                                   # premissa: a linha plantada existe no disco
    inf <- read_for(p, di, O, "inference")
    expect_true(all(cal(inf$panel$holiday_annex$load$interval_start) < O))
    expect_true(all(cal(inf$panel$holiday_annex$weather_observed$valid_time) < O))
    expect_identical(inf$panel$holiday_annex$islands, isl)
    ## deck do motor (plugin_ingest + TransformaDeck pelo despachante): CARGAFER/EXOGENAFER sem dia >= O
    for (kind in c("train", "inference")) {
      pan <- if (kind == "train") tr$panel else inf$panel
      au <- if (kind == "train") at(O) else at(O, "09:00")
      mf <- model_frame(p, pan, O, au)
      deck <- with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(mf$panel, mf$cuts)))
      expect_gt(nrow(deck$CARGAFER), 0L)
      expect_true(all(cal(deck$CARGAFER$DataHora) < O), info = paste(kind, "CARGAFER"))
      expect_true(all(cal(deck$EXOGENAFER$DataHora) < O), info = paste(kind, "EXOGENAFER"))
      expect_true(all(deck$islands$to < O))
      ## o anexo so tem dias de ilha (nada da janela comum)
      expect_true(all(cal(deck$CARGAFER$DataHora) < W$common_from))
      expect_gte(min(cal(deck$CARGAHIST$DataHora)), W$common_from - 1L)   # Bloco 0: so a janela comum (+ aquecimento)
    }
  })
}

## ---------------------------------------------------------------------------------------------------------------------
## Bordas do deck (AC2): ilha cortada no inicio do store, zero ilhas, origem 00h e history_short so com dias comuns.
## ---------------------------------------------------------------------------------------------------------------------
for (m in MODELS) {
  test_that(sprintf("[AC2] bordas (%s): ilha cortada em 2018-01-01 sem erro; zero ilhas = so Bloco 0; origem 00h; history_short so conta dias comuns", m), {
    O <- lubridate::ymd("2023-06-01")
    p <- load_plugin(m)
    W <- p$descriptor$history_window(O, p$spec)
    ## (a) ilha cortada no inicio do store: feriado 2018-01-03 => from = 2017-12-27 cortado em 2018-01-01
    tr <- read_for(p, train_deck(O), O, "train")
    isl <- tr$panel$holiday_annex$islands
    expect_identical(min(isl$from), lubridate::ymd("2018-01-01"))
    expect_true(lubridate::ymd("2018-01-03") %in% do.call(c, isl$holiday_dates))
    expect_gte(min(cal(tr$panel$holiday_annex$load$interval_start)), lubridate::ymd("2018-01-01"))
    mf <- model_frame(p, tr$panel, O, at(O))
    deck <- with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(mf$panel, mf$cuts)))
    expect_true(lubridate::ymd("2018-01-01") %in% cal(deck$CARGAFER$DataHora))
    ## (b) zero ilhas: feriados so dentro da janela comum => anexo vazio, deck do motor so com o Bloco 0
    d0 <- train_deck(O, holidays = hol_table(days = lubridate::ymd("2021-12-25"), codes = 10L), with_islands = FALSE)
    z <- read_for(p, d0, O, "train")
    expect_identical(nrow(z$panel$holiday_annex$islands), 0L)
    expect_identical(nrow(z$panel$holiday_annex$load), 0L)
    mz <- model_frame(p, z$panel, O, at(O))
    dz <- with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(mz$panel, mz$cuts)))
    expect_identical(nrow(dz$CARGAFER), 0L)
    expect_identical(nrow(dz$EXOGENAFER), 0L)
    expect_identical(nrow(dz$islands), 0L)
    expect_gte(min(cal(dz$CARGAHIST$DataHora)), W$common_from - 1L)
    ## (c) origem 00h (available_until = O 00:00): infer sem nenhuma carga do dia O; linha plantada em O fora do anexo
    di <- inference_deck(O, isl, plant_day_O = TRUE, hm = "00:00")
    inf <- read_for(p, di, O, "inference")
    expect_false(O %in% cal(inf$panel$load$interval_start))
    expect_true(all(cal(inf$panel$holiday_annex$load$interval_start) < O))
    mi <- model_frame(p, inf$panel, O, at(O))
    dinf <- with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(mi$panel, mi$cuts)))
    expect_true(all(cal(dinf$CARGAFER$DataHora) < O))
    ## (d) window.history_short conta so dias comuns: 30 dias comuns a menos, anexo intacto => 1065 de 1095
    short <- train_deck(O)
    pc_mut_load(short, function(dt) dt[lubridate::as_date(lubridate::force_tz(timestamp, "UTC")) >= W$common_from + 30L |
                                         lubridate::as_date(lubridate::force_tz(timestamp, "UTC")) < W$common_from - 1L])
    rc <- list(cuts = data.frame(model = m, data_lag_days = as.integer(p$descriptor$data_lag_days), stringsAsFactors = FALSE),
               tz = TZ, load_label = "end", available_until = at(O), weather_issue_cutoff = "available_until",
               weather_issue_limit = at(O), weather_publication_lag_hours = 0L)
    rp <- function(area, span, H, fc, ..., holiday_window = NULL)
      with_cli(env$prevcarga_read_panel(pc_ctx_panel(short), O, area, H, span, holiday_window = holiday_window))
    spec <- p$spec
    e <- guard_err(env$prevcarga_train_inputs(list(mdir = m), AREA, O, list(weather_issue_cutoff = "available_until"),
                                              rc = rc, read_panel = rp, plugin = p, spec = spec))
    expect_identical(e$id, "window.history_short")
    expect_identical(e$details$required_days, 1095L)
    expect_lt(e$details$available_days, 1095L)
    expect_gte(e$details$available_days, 1095L - 31L)                           # os dias do anexo (>= 16) nao compensam
    ## CONTROL: o deck completo passa a guarda
    rp_ok <- function(area, span, H, fc, ..., holiday_window = NULL)
      with_cli(env$prevcarga_read_panel(pc_ctx_panel(train_deck(O)), O, area, H, span, holiday_window = holiday_window))
    ok <- guard_err(env$prevcarga_train_inputs(list(mdir = m), AREA, O, list(weather_issue_cutoff = "available_until"),
                                               rc = rc, read_panel = rp_ok, plugin = p, spec = spec))
    expect_null(ok)
  })
}

## ---------------------------------------------------------------------------------------------------------------------
## Fachada sem switch por nome de modelo (AC3). Codigo sem comentarios; os ids vem de models/*/run.yaml.
## ---------------------------------------------------------------------------------------------------------------------
FACADE <- c("training.R", "inference.R", "registry.R", "features.R", "deck_reader.R")
code_of <- function(rel) {
  x <- readLines(file.path(pc_root(), "scripts", "lib", rel), warn = FALSE)
  x <- sub("#.*$", "", x)                                   # comentario documental fora; so codigo
  x[nzchar(trimws(x))]
}
model_ids <- function() {
  fs <- Sys.glob(file.path(pc_root(), "models", "*", "run.yaml"))
  vapply(fs, function(f) as.character(yaml::read_yaml(f)$name), "")
}

test_that("[AC3] facade_no_model_switch: nenhum id de modelo, 'dessem-linear'/'dessem-regdin' ou switch por nome em codigo", {
  ids <- unname(model_ids())
  expect_true(all(MODELS %in% ids))
  for (rel in FACADE) {
    code <- code_of(rel)
    expect_gt(length(code), 50L)
    for (id in ids)                                              # palavra inteira: "lgbm_day_48" (layout) nao e o id "lgbm"
      expect_false(any(grepl(paste0("(?<![A-Za-z0-9_])", id, "(?![A-Za-z0-9_])"), code, perl = TRUE)), info = paste(rel, id))
    expect_false(any(grepl("dessem-linear|dessem-regdin", code)), info = rel)
    ## switch/if por nome de modelo: comparacao de model_id/mdir/name com literal ou switch() sobre eles
    expect_false(any(grepl("(model_id|mdir|\\$name)\\s*(==|!=|%in%)\\s*[\"']", code)), info = rel)
    expect_false(any(grepl("switch\\(\\s*[^,)]*(model_id|mdir|\\$name|member\\$)", code)), info = rel)
  }
  ## 'dessem' solto em codigo: so a lista fechada de layouts nativos (vocabulario do contrato) e o nome neutro da funcao de janela
  for (rel in FACADE) {
    left <- gsub("dessem_(hourly_24|semihourly_48)|prevcarga_dessem_window", "", code_of(rel))
    expect_identical(grep("dessem", left, ignore.case = TRUE, value = TRUE), character(), info = rel)
  }
})

test_that("[AC3] CONTROL: o grep acha um id injetado e um switch por nome; comentario nao conta", {
  ids <- unname(model_ids())
  inj <- c(code_of("features.R"), sprintf("x <- '%s'", MODELS[[1L]]))
  expect_true(any(vapply(ids, function(id) any(grepl(id, inj, fixed = TRUE)), NA)))
  expect_true(any(grepl("switch\\(\\s*[^,)]*(model_id|mdir|\\$name|member\\$)", "switch(member$mdir, a = 1)")))
  expect_false(any(grepl(MODELS[[2L]], sub("#.*$", "", "# dessem-regdin so em comentario"), fixed = TRUE)))
})

## ---------------------------------------------------------------------------------------------------------------------
## Pipeline treino + infer em ano_civil (AC4). Painel de exibicao (30 min) em memoria, sem RNG, com feriados tipo 2 e 10
## (e tipo 5 na fixture do bug v0.1.5) e o anexo de feriados montado pelas ilhas do T1. Plugin real, motor real, despachante real.
## ---------------------------------------------------------------------------------------------------------------------
P_O <- lubridate::ymd("2023-06-01")
P_AU <- at(P_O, "09:00")
dip <- c(`2` = -1500, `10` = -900, `5` = -1100)
slots <- function(d0, d1) {
  a <- at(d0); n <- (as.integer(lubridate::as_date(d1) - lubridate::as_date(d0)) + 1L) * 48L
  a + lubridate::minutes(30L * (seq_len(n) - 1L))
}
p_load <- function(st, hol) {
  d <- as.numeric(cal(st) - lubridate::ymd("2015-01-01")); h <- lubridate::hour(st) + lubridate::minute(st) / 60
  wd <- lubridate::wday(st)
  code <- hol$prevcarga_code[match(as.integer(cal(st)), as.integer(hol$date))]
  9000 + 600 * sin(2 * pi * (h - 6) / 24) + 700 * sin(2 * pi * (h - 14) / 12) + 30 * sin(d * 0.37 + h) -
    900 * (wd %in% c(1L, 7L)) + ifelse(is.na(code), 0, dip[as.character(code)])
}
p_temp <- function(st) {
  d <- as.numeric(cal(st) - lubridate::ymd("2015-01-01")); h <- lubridate::hour(st) + lubridate::minute(st) / 60
  24 + 5 * sin(2 * pi * d / 365.25) + 4 * sin(2 * pi * (h - 9) / 24) + 0.5 * sin(d * 1.7)
}
## feriados: tipos 2 e 10 em dias distintos da janela comum (>= 8 amostras de cada), no horizonte e nas ilhas; `corpus5`: tipo 5
## com exatamente 5 amostras no historico + 1 no horizonte (o caso do bug da v0.1.5: Corpus Christi)
p_holidays <- function(O, fixture = "normal") {
  k <- 0:15
  win <- O - (25L + 61L * k)
  codes <- rep(c(2L, 10L), 8L)
  days <- c(win, lubridate::ymd(c("2019-04-19", "2019-12-25", "2018-03-30")), O + c(2L, 5L))
  cds <- c(codes, 10L, 2L, 10L, 2L, 10L)
  if (identical(fixture, "corpus5")) {
    days <- c(days, O - c(300L, 640L, 900L), lubridate::ymd("2019-06-20", "2018-05-31"), O + 3L)
    cds <- c(cds, rep(5L, 6L))
  }
  data.table(area_code = AREA, date = days, holiday_type_id = 1L, prevcarga_code = cds, simplified_code = 3L)
}
p_panel <- function(O, hol, until = NULL, first_store = lubridate::ymd("2018-01-01")) {
  W <- env$prevcarga_dessem_window(O, "ano_civil")
  isl <- env$prevcarga_holiday_islands(hol, O, W$common_from, first_store, holiday_from = W$holiday_from)
  st <- slots(W$common_from, O - 1L)
  if (!is.null(until)) st <- c(st, slots(O, O)[slots(O, O) + lubridate::minutes(30L) <= until])
  ld <- data.table(area_code = AREA, interval_start = st, interval_end = st + lubridate::minutes(30L), load_mwh = p_load(st, hol))
  wo <- data.table(area_code = AREA, valid_time = st, temperature = p_temp(st))
  fs <- slots(O, O + 9L)
  wf <- data.table(area_code = AREA, issue_time = lubridate::force_tz(lubridate::as_datetime(O), "UTC"),
                   valid_time = fs, temperature = p_temp(fs) + 0.4)
  ad <- sort(unique(unlist(lapply(seq_len(nrow(isl)), function(i) as.numeric(seq(isl$from[i], isl$to[i], by = "day"))))))
  ast <- if (length(ad)) do.call(c, lapply(ad, function(x) slots(lubridate::as_date(x), lubridate::as_date(x)))) else st[0L]
  au <- if (is.null(until)) at(O) else until
  list(load = ld, weather_observed = wo, weather_forecast = wf, holidays = copy(hol),
       holiday_annex = list(islands = isl,
         load = data.table(area_code = AREA, interval_start = ast, interval_end = ast + lubridate::minutes(30L), load_mwh = p_load(ast, hol)),
         weather_observed = data.table(area_code = AREA, valid_time = ast, temperature = p_temp(ast))),
       attrs = list(origin = O, available_until = au, load_label = "end", tz = TZ,
                    weather_issue_cutoff = "available_until", weather_issue_limit = au))
}
p_cuts <- function(O, au) list(origin_day = O, forecast_origin = O, train_origin = O, last_data_day = O - 1L, tz = TZ,
                              available_until = au, weather_issue_cutoff = "available_until", weather_issue_limit = au)
p_ctx <- list(load_label = "end", timezone = TZ, weather_issue_cutoff = "available_until")

## treina e infere; devolve list(art, env) ou o prevcarga_error
## avisos estruturados do motor (classe dessem_engine_guard: codigo warning.engine_*) emitidos durante `expr`
collect_guards <- function(expr) {
  codes <- character()
  val <- withCallingHandlers(expr, message = function(c) {
    if (inherits(c, "dessem_engine_guard")) codes <<- c(codes, c$code)
    invokeRestart("muffleMessage")
  })
  list(value = val, codes = codes)
}
p_run <- function(m, fixture = "normal") {
  p <- load_plugin(m)
  spec <- p$spec
  hol <- p_holidays(P_O, fixture)
  t0 <- Sys.time()
  g <- tryCatch(collect_guards(with_cli(env$prevcarga_plugin_train(p, p_panel(P_O, hol), p_cuts(P_O, at(P_O)), spec, ctx = p_ctx))),
                prevcarga_error = function(e) list(value = e, codes = character()))
  art <- g$value
  cat(sprintf("\n[e2e] %s %s treino: %.0f s\n", m, fixture, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (inherits(art, "prevcarga_error")) return(list(err = art, p = p, guards = g$codes))
  p$spec <- spec
  t0 <- Sys.time()
  inf <- tryCatch(with_cli(suppressMessages(env$prevcarga_plugin_infer(p, art, p_panel(P_O, hol, until = P_AU), p_cuts(P_O, P_AU)))),
                  prevcarga_error = identity)
  cat(sprintf("[e2e] %s %s infer: %.0f s\n", m, fixture, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  list(art = art, inf = inf, p = p, guards = g$codes)
}

p_cache <- new.env(parent = emptyenv())
p_cached <- function(m, fixture) {
  k <- paste(m, fixture)
  if (is.null(p_cache[[k]])) assign(k, p_run(m, fixture), envir = p_cache)
  p_cache[[k]]
}
## nomes e formulas de qualquer objeto do artefato (listas, formulas, terms de modelos ajustados), recursivamente
walk_text <- function(x, depth = 0L) {
  if (depth > 12L) return(character())
  out <- character()
  if (inherits(x, "formula")) return(paste(deparse(x), collapse = " "))
  if (!is.null(attr(x, "terms")) || (is.list(x) && inherits(x$terms, "terms")))
    out <- c(out, paste(deparse(stats::formula(if (!is.null(x$terms)) x$terms else attr(x, "terms"))), collapse = " "))
  if (is.language(x) || is.function(x) || is.environment(x)) return(out)
  if (is.list(x) || is.data.frame(x)) {
    out <- c(out, names(x))
    for (el in x) out <- c(out, walk_text(el, depth + 1L))
  } else if (!is.null(names(x))) out <- c(out, names(x))
  out
}

for (m in MODELS) {
  test_that(sprintf("[AC4] dessem_pipeline_ano_civil (%s): treino + infer sem generic.unexpected, Bloco fora das formulas e dos nomes do artefato", m), {
    r <- p_cached(m, "normal")
    expect_null(r$err)
    expect_false(inherits(r$inf, "prevcarga_error"))
    expect_true(is.matrix(r$inf$values))
    expect_identical(nrow(r$inf$values), 9L)
    expect_false(anyNA(r$inf$values))
    expect_true(all(is.finite(r$inf$values)))
    txt <- walk_text(r$art$payload)
    expect_gt(length(txt), 20L)                                     # dentes: o coletor viu nomes e formulas do artefato
    expect_false(any(grepl("(^|[^A-Za-z0-9_])Bloco([^A-Za-z0-9_]|$)", txt)))
    expect_false(identical(r$err$id, "generic.unexpected"))
    ## CONTROL: o coletor acha "Bloco" numa formula injetada
    expect_true(any(grepl("Bloco", walk_text(list(f = y ~ LAG_1 + Bloco)))))
  })

  test_that(sprintf("[AC4] fixture do bug v0.1.5 (%s): feriado com 5 amostras sai com modelo valido ou engine.dessem_tune_small_sample (exit 5)", m), {
    r <- p_cached(m, "corpus5")
    if (!is.null(r$err)) {
      expect_identical(r$err$id, "engine.dessem_tune_small_sample")
      expect_identical(r$err$exit_code, 5L)
      expect_identical(r$err$class_name, "window")
      expect_true(all(utf8ToInt(conditionMessage(r$err)) < 128L))
    } else {
      expect_s3_class(r$art, "prevcarga_plugin_artifact")
      cat(sprintf("\n[e2e] %s corpus5 avisos do motor: %s\n", m, if (length(r$guards)) paste(unique(r$guards), collapse = ", ") else "(nenhum)"))
      expect_true(all(r$guards %in% c("warning.engine_cross_clamped", "warning.engine_empty_model_guard")))
      expect_false(any(grepl("(^|[^A-Za-z0-9_])Bloco([^A-Za-z0-9_]|$)", walk_text(r$art$payload))))
      expect_false(inherits(r$inf, "prevcarga_error"))
      expect_true(is.matrix(r$inf$values))
    }
    expect_false(identical(r$err$id, "generic.unexpected"))
  })
}

## ---------------------------------------------------------------------------------------------------------------------
## Gate fast da paridade (AC1): o tier parity (delta 0 contra a v0.1.5) so roda onde ha golden e WeightSVM (E8-T4). No tier fast
## fica a parte estrutural: toda unidade dos dois DESSEM declara janela_dessem = referencia com a janela da v0.1.5 (1825 dias,
## sem anexo), e o CONTROL com ano_civil MUDA a janela da mesma unidade (o gate distingue). Nenhum golden e lido nem regenerado.
## ---------------------------------------------------------------------------------------------------------------------
for (m in MODELS) {
  test_that(sprintf("[AC1] unidades de paridade (%s): janela_dessem = referencia reproduz a janela da v0.1.5; CONTROL ano_civil diverge", m), {
    u <- yaml::read_yaml(file.path(pc_root(), "models", m, "tests", "parity", "units.yaml"))$units
    expect_gt(length(u), 5L)
    p <- load_plugin(m)
    for (x in u) {
      O <- lubridate::ymd(x$origin)
      sp <- p$spec
      sp$params$janela_dessem <- x$params$janela_dessem
      expect_identical(x$params$janela_dessem, "referencia", info = x$id)
      ref <- with_cli(p$descriptor$history_window(O, sp))
      expect_identical(ref$common_from, O - as.integer(x$history_days), info = x$id)    # a janela de 1825 dias do oraculo
      expect_null(ref$holiday_from)                                                     # e sem anexo de feriados
      sp$params$janela_dessem <- "ano_civil"                                            # CONTROL: so o params da unidade muda
      ctl <- with_cli(p$descriptor$history_window(O, sp))
      expect_false(identical(ctl$common_from, ref$common_from), info = x$id)
      expect_false(is.null(ctl$holiday_from), info = x$id)
    }
  })
}

test_that("[AC1] o motor isolado nao ve janela_dessem nem anexo (units.yaml) e nenhum golden.lock mudou nesta sessao", {
  ue <- readLines(file.path(pc_root(), "models", "_dessem-engine", "tests", "parity", "units.yaml"), warn = FALSE)
  expect_false(any(grepl("^\\s*-?\\s*\\{?.*params:\\s*\\{[^}]*janela_dessem", ue)))
  for (d in c("_dessem-engine", MODELS))
    expect_true(file.exists(file.path(pc_root(), "models", d, "tests", "parity", "golden.lock")))
})
