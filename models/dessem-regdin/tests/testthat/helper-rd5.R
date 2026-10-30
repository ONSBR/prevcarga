## helper-rd5.R -- fixtures da verificacao RD.5 do dessem-regdin (nowcast, corte horario, split, clima, sentinela, portabilidade,
## higiene). Auto-carregado por test_dir. Nao e codigo de produto. Camada sobre o `helper-fixtures.R` (RD.4) e o
## `helper-rd.R`: aqui o mundo e o DECK SINTETICO de `scripts/tools/fixture_store.R` (nunca dado real) lido pelo leitor do M2
## (`prevcarga_read_panel`, `prevcarga_resolve_cuts`, `truncate_at` para os cortes do dia O), despachado pela fundacao e
## verificado com o molde do `honesty_gate.R` (M3.2/M3.5). Sem variaveis de ambiente; datas por lubridate com tz explicito.
## Um treino completo custa ~1,5 min: os artefatos ficam em cache de sessao (`rd2_cached`) e, so se o chamador der
## `options(rd5.cache = <dir>)`, tambem em disco (acelera os shards; nada e versionado).

RD5_O      <- lubridate::ymd("2024-10-07")   # 12/out cai em d_plus = 5 (feriado fixo do fixture, codigo 2)
RD5_AREA   <- "ZZ1"
RD5_DAYS   <- 460L                           # abaixo de ~400 dias o ARIMA do fixture nao ajusta (docs/estado/plugins_porte_fechamento.md)
RD5_HIST   <- 400L                           # history_days.train == infer do spec de teste (run.yaml: 1825)
RD5_SEED   <- 20241015L
RD5_TOL    <- 1e-5
RD5_RULES  <- c("available_until", "origin_midnight_utc")

rd5_prep <- local({
  done <- FALSE
  function() {
    if (done) return(invisible(TRUE))
    for (m in c("config", "dates", "provenance", "honesty_gate", "schema_long", "evaluation"))
      sys.source(file.path(rd_root(), "scripts", "lib", paste0(m, ".R")), envir = rd_lib)
    sys.source(file.path(rd_root(), "scripts", "tools", "fixture_store.R"), envir = rd_lib)
    done <<- TRUE
    invisible(TRUE)
  }
})

rd5_au <- function(hhmm, O = RD5_O) lubridate::ymd_hm(paste(format(O), hhmm), tz = RD_TZ)
rd5_day_start <- function(d) lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(d)), RD_TZ)

## Efeito dia-a-dia deterministico (sem RNG) do fechamento do porte (`pcc_day_effects`, M5.11): sem ele a serie do fixture e
## quase periodica e o ARIMA do regdin nao ajusta. Aplicado so em copia do deck (o `dir` de entrada nunca e escrito).
rd5_day_effects <- function(dir) {
  eff <- function(t) {
    d <- as.numeric(as.Date(t))
    list(temp = 2 * sin(d * d * 0.37) + 1.5 * sin(d * d * 0.113 + 1), load = 60 * sin(d * d * 0.173) + 40 * sin(d * d * 0.271 + 2))
  }
  out <- rd_lib$prevcarga_fixture_copy(dir)
  hit_all <- function(x) rep(TRUE, nrow(x))
  rd_lib$.pc_fx_rewrite_hits(out, "load", NULL, hit_all, function(x, i) {
    e <- eff(x$timestamp[i])
    data.table::set(x, i = i, j = "load_mwh", value = x$load_mwh[i] + e$load + 40 * e$temp)
  })
  rd_lib$.pc_fx_rewrite_hits(out, "weather_observed", NULL, hit_all, function(x, i)
    data.table::set(x, i = i, j = "temperature", value = x$temperature[i] + eff(x$timestamp[i])$temp))
  rd_lib$.pc_fx_rewrite_hits(out, "weather_forecast", NULL, hit_all, function(x, i)
    data.table::set(x, i = i, j = "temperature", value = x$temperature[i] + eff(x$valid_time[i])$temp))
  out
}

## Deck sintetico de 460 dias terminando em O com o dia O INTEIRO no store (os cortes do dia O vem de `truncate_at`).
## O feriado futuro do fixture em O+3 (codigo 3, sem amostra historica) vira o codigo 12 (dia normal): o motor o recusaria.
## `sentinel`: valor = minutos do interval_start/valid_time (sem efeito de dia). Cache por sessao.
rd5_store <- function(sentinel = FALSE) {
  rd5_prep()
  rd2_cached(paste("rd5-store", sentinel), {
    dir <- tempfile("rd5-deck-")
    rd_lib$prevcarga_fixture_deck(dir, RD5_SEED, RD5_AREA, RD5_O - (RD5_DAYS - 1L):0L, load_label = "start", sentinel = sentinel,
                                  tz = RD_TZ, horizon_days = 10L)
    if (!sentinel) dir <- rd5_day_effects(dir)
    dir <- rd_lib$prevcarga_fixture_set_holiday(dir, RD5_AREA, RD5_O + 3L,
                                                c(holiday_type_id = 12L, prevcarga_code = 12L, simplified_code = 1L))
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
                 sprintf("  X: {areas: [%s], loss: PX}", RD5_AREA), "order: [X]", "members: []"),
               file.path(dir, "topology.yaml"))
    list(dir = dir, O = RD5_O, areas = RD5_AREA)
  })
}

## store apontando para uma copia do deck (mutadores em copia do P.0e)
rd5_with_dir <- function(st, dir) {
  file.copy(file.path(st$dir, "topology.yaml"), file.path(dir, "topology.yaml"), overwrite = TRUE)
  utils::modifyList(st, list(dir = dir))
}

rd5_perturb <- function(st, channel, ...) {
  rd5_prep()
  rd5_with_dir(st, rd_lib$prevcarga_fixture_perturb(st$dir, channel, ..., areas = RD5_AREA))
}

rd5_ctx <- function(st, rule = "available_until")
  list(data_dir = st$dir, root = rd_root(), topology_path = file.path(st$dir, "topology.yaml"),
       weather_issue_cutoff = rule, origin = st$O)

## cuts de dates.R (nunca literais): `hhmm` = corte intradiario do dia O (truncate_at) no fuso do deck; NULL = fim do store
rd5_cuts <- function(st, hhmm = "09:00", rule = "available_until") {
  rd5_prep()
  tr <- if (is.null(hhmm)) NULL else rd5_au(hhmm, st$O)
  r <- suppressWarnings(rd_lib$prevcarga_resolve_cuts(rd5_ctx(st, rule), st$O, "dessem-regdin", truncate_at = tr))
  row <- r$cuts[r$cuts$model == "dessem-regdin", ]
  list(forecast_origin = r$forecast_origin, origin_day = r$forecast_origin, tz = r$tz, load_label = r$load_label,
       available_until = r$available_until, weather_issue_cutoff = r$weather_issue_cutoff,
       weather_issue_limit = r$weather_issue_limit, split = row$split, train_origin = row$train_origin,
       last_data_day = row$last_data_day, data_lag_days = row$data_lag_days, horizon_days = row$horizon_days)
}

## Painel do leitor (M2) no corte `cuts`: carga e clima observado ate available_until (truncate_at); previsto = TODAS as
## emissoes <= weather_issue_limit (o corte e do chamador, como o despachante exige; a regra do regdin pega a mais
## recente por valid_time). mode "train": ate last_data_day (corte da fundacao, nunca recalculado).
rd5_panel <- function(st, cuts, mode = c("infer", "train"), history_days = RD5_HIST + 1L) {
  rd5_prep()
  mode <- match.arg(mode)
  ctx <- rd5_ctx(st, cuts$weather_issue_cutoff)
  O <- lubridate::as_date(cuts$origin_day); H <- cuts$horizon_days
  p <- suppressWarnings(rd_lib$prevcarga_read_panel(ctx, O, st$areas, horizon_days = H, history_days = history_days,
                                                     truncate_at = cuts$available_until))
  fc <- rd_lib$prevcarga_read_weather(ctx, "forecast", st$areas, O, O + H)
  p$weather_forecast <- fc[issue_time <= cuts$weather_issue_limit]
  if (mode == "train") p <- rd_lib$.prevcarga_plugin_train_slice(p, cuts)
  p
}

## spec do despachante: o run.yaml real do plugin com a janela de teste (400 dias) e a variante
rd5_spec <- function(exogena = "MED", plugin = rd_fx_plugin()) {
  s <- plugin$spec
  s$history_days$train <- RD5_HIST; s$history_days$infer <- RD5_HIST
  s$params$exogena <- exogena
  s
}
rd5_run_ctx <- function(cuts) list(load_label = cuts$load_label, timezone = cuts$tz, weather_issue_cutoff = cuts$weather_issue_cutoff)

rd5_train <- function(st, cuts = rd5_cuts(st), spec = rd5_spec(), panel = NULL, plugin = rd_fx_plugin()) {
  pan <- if (is.null(panel)) rd5_panel(st, cuts, "train") else panel
  rd_with_cli(suppressWarnings(suppressMessages(rd_lib$prevcarga_plugin_train(plugin, pan, cuts, spec, rd5_run_ctx(cuts)))))
}

rd5_infer <- function(art, st, cuts, panel = NULL, plugin = rd_fx_plugin()) {
  pan <- if (is.null(panel)) rd5_panel(st, cuts, "infer") else panel
  rd_with_cli(suppressWarnings(suppressMessages(rd_lib$prevcarga_plugin_infer(plugin, art, pan, cuts))))
}

## forecast ACOPLADO pelo despachante: `panel` e o painel de INFER; o despachante treina so ate last_data_day e infere
rd5_forecast <- function(st, cuts, spec = rd5_spec(), panel = NULL, plugin = rd_fx_plugin()) {
  pan <- if (is.null(panel)) rd5_panel(st, cuts, "infer") else panel
  rd_with_cli(suppressWarnings(suppressMessages(rd_lib$prevcarga_plugin_forecast(plugin, pan, cuts, spec, rd5_run_ctx(cuts)))))
}

## artefato de O da variante/regra (o carimbo leva a regra de clima): cache de sessao e, com options(rd5.cache), de disco
rd5_art <- function(exogena = "MED", rule = "available_until", hhmm = "09:00") {
  key <- paste("rd5-art", exogena, rule, hhmm)
  rd2_cached(key, {
    dir <- getOption("rd5.cache")
    f <- if (is.null(dir)) NULL else file.path(dir, paste0(gsub("[^A-Za-z0-9]+", "-", key), ".rds"))
    if (!is.null(f) && file.exists(f)) readRDS(f) else {
      st <- rd5_store()
      a <- rd5_train(st, rd5_cuts(st, hhmm, rule), rd5_spec(exogena))
      if (!is.null(f)) saveRDS(a, f)
      a
    }
  })
}

## max|Delta y_pred| casado por interval_end entre duas entregas (o molde: diferenca estrutural = Inf)
rd5_delta <- function(a, b) { rd5_prep(); rd_lib$.prevcarga_hg_delta(a, b) }

## entregas comparadas nos interval_end comuns (cortes diferentes entregam grades diferentes)
rd5_common <- function(a, b) {
  k <- intersect(as.numeric(a$interval_end), as.numeric(b$interval_end))
  list(a = a$y_pred[match(k, as.numeric(a$interval_end))], b = b$y_pred[match(k, as.numeric(b$interval_end))], n = length(k))
}

## espiao de contagem so das chamadas da cabeca de nowcast (rd_dm_nowcast com `mm` nao vazio) no ENV DO PLUGIN; os ajustes
## de SVM da cabeca (tune_wsvm no env do motor) sao contados por `rd2_spy`. Restaurado por $restore() ou no defer do chamador.
rd5_head_spy <- function(env = rd_fx_plugin()$env, frame = parent.frame()) {
  real <- get("rd_dm_nowcast", envir = env, inherits = FALSE)
  log <- new.env(); log$calls <- 0L; log$head <- 0L
  assign("rd_dm_nowcast", function(serieDM, config_mdl, mm, ...) {
    log$calls <- log$calls + 1L
    if (NROW(mm) > 0L) log$head <- log$head + 1L
    real(serieDM, config_mdl, mm, ...)
  }, envir = env)
  restore <- function() assign("rd_dm_nowcast", real, envir = env)
  withr::defer(restore(), envir = frame)
  list(head = function() log$head, calls = function() log$calls, reset = function() { log$calls <- 0L; log$head <- 0L },
       restore = restore)
}

## caminho de um arquivo temporario do artefato (e do manifesto), limpo no fim do chamador
rd5_freeze_path <- function(frame = parent.frame()) {
  f <- tempfile("rd5-art-", fileext = ".rds")
  withr::defer(unlink(c(f, paste0(f, ".manifest.json"))), envir = frame)
  f
}

rd5_sha <- function(art) {
  f <- rd5_freeze_path()
  rd_lib$prevcarga_freeze(art, f)
}

## erro do despachante/plugin como condicao (NULL se nao houve)
rd5_err <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)

expect_rd5_class <- function(e, class_name, exit_code, id = NULL) {
  testthat::expect_s3_class(e, "prevcarga_error")
  testthat::expect_identical(e$class_name, class_name)
  testthat::expect_identical(e$exit_code, as.integer(exit_code))
  if (!is.null(id)) testthat::expect_identical(e$id, id)
  invisible(e)
}

## variantes sob teste: shardavel por `options(rd5.variants = "MED")` (nunca por variavel de ambiente)
rd5_variants <- function() getOption("rd5.variants", c("MED", "MAX"))
