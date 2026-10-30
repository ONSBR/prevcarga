## helper-dl.R -- fixtures dos testes do plugin dessem-linear (DL.5). Auto-carregado por test_dir. Nao e codigo de
## produto. Sem variaveis de ambiente; datas por lubridate com tz explicito; sem RNG nas fixtures.
suppressWarnings(suppressMessages(library(testthat)))

dl_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("dl_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}
dl_models <- function() file.path(dl_root(), "models")

options(prevcarga.lib = file.path(dl_root(), "scripts", "lib"))
## fundacao carregada num env de teste, nunca no globalenv
dl_lib <- local({
  e <- new.env(parent = globalenv())
  for (m in c("errors", "topology", "deck_reader", "validation", "registry", "features"))
    sys.source(file.path(dl_root(), "scripts", "lib", paste0(m, ".R")), envir = e)
  e
})

dl_load <- function(root = dl_models()) {
  dl_lib$prevcarga_plugin_unload_all()
  dl_lib$prevcarga_registry_reset()
  dl_lib$prevcarga_plugin_load("dessem-linear", root)
}

## a CLI fornece prevcarga_abort/_condition/_warn no globalenv; aqui entram so durante `code`
dl_with_cli <- function(code) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_warn", "to_semihourly")
  nm <- nm[!vapply(nm, exists, NA, envir = globalenv(), inherits = FALSE)]   # aninhado: so o mais externo instala/remove
  for (n in nm) assign(n, dl_lib[[n]], envir = globalenv())
  on.exit(rm(list = nm, envir = globalenv()), add = TRUE)
  force(code)
}

expect_dl_error <- function(expr, id, fragment = NULL) {
  cond <- tryCatch({ dl_with_cli(force(expr)); NULL }, prevcarga_error = function(e) e)
  testthat::expect_false(is.null(cond), info = "esperava prevcarga_error")
  if (!is.null(cond)) {
    testthat::expect_identical(cond$id, id)
    if (!is.null(fragment)) testthat::expect_match(conditionMessage(cond), fragment, fixed = TRUE)
  }
  invisible(cond)
}

DL_TZ <- "Etc/GMT+3"

## Painel de exibicao (meia hora, rotulo ja convertido pelo leitor: interval_start/interval_end) que o leitor entrega,
## determinístico. `days` dias terminando em O (incl.), dia O parcial ate `until` (POSIXct) quando dado.
## `drop`: interval_start (display) a remover da carga. area unica.
dl_panel <- function(origin = "2024-12-23", days = 420L, until = NULL, drop = NULL, area = "SE", tz = DL_TZ,
                     fc_days = 9L, fc_short = integer()) {
  O <- lubridate::ymd(origin)
  t0 <- lubridate::force_tz(lubridate::as_datetime(O - days), tz)
  n_hist <- as.integer(days) * 48L
  s_hist <- t0 + lubridate::minutes(30L * (seq_len(n_hist) - 1L))
  tO <- lubridate::force_tz(lubridate::as_datetime(O), tz)
  s_o <- if (is.null(until)) s_hist[0] else tO + lubridate::minutes(30L * (seq_len(48L) - 1L))
  if (!is.null(until)) s_o <- s_o[s_o + lubridate::minutes(30) <= until]
  starts <- c(s_hist, s_o)
  hr <- lubridate::hour(starts) + lubridate::minute(starts) / 60
  dd <- as.numeric(lubridate::as_date(starts) - (O - days))
  temp <- function(d, h) 24 + 5 * sin(2 * pi * d / 365.25) + 4 * sin(2 * pi * (h - 9) / 24) + 0.5 * sin(d * 1.7)
  wd <- lubridate::wday(starts)
  load_mwh <- 9000 + 600 * sin(2 * pi * (hr - 6) / 24) + 700 * sin(2 * pi * (hr - 14) / 12) +
    40 * (temp(dd, hr) - 24) - 900 * (wd %in% c(1, 7)) + 25 * sin(dd * 0.37 + hr)
  ld <- data.table::data.table(area_code = area, interval_start = starts,
                               interval_end = starts + lubridate::minutes(30), load_mwh = load_mwh)
  if (!is.null(drop)) ld <- ld[!(interval_start %in% drop)]
  obs <- data.table::data.table(area_code = area, valid_time = starts, temperature = temp(dd, hr))
  fs <- tO + lubridate::minutes(30L * (seq_len(48L * fc_days) - 1L))
  keep <- !(lubridate::as_date(fs) %in% (O + fc_short))
  fs <- fs[keep]
  fd <- as.numeric(lubridate::as_date(fs) - (O - days)); fh <- lubridate::hour(fs) + lubridate::minute(fs) / 60
  iss <- lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(c(0L, 6L))
  fc <- data.table::rbindlist(lapply(seq_along(iss), function(k) data.table::data.table(
    area_code = area, issue_time = iss[[k]], valid_time = fs, temperature = temp(fd, fh) + 0.4 + 100 * (k - 1L))))
  fer_days <- O - c(10L, 40L, 100L, 200L, 300L, 399L)
  hol <- data.table::data.table(area_code = area, date = c(fer_days, O + 1L),
                                holiday_type_id = 1L, prevcarga_code = c(10L, 3L, 4L, 12L, 6L, 9L, 5L),
                                simplified_code = 3L)
  au <- if (is.null(until)) lubridate::force_tz(lubridate::as_datetime(O), tz) else until
  list(load = ld, weather_observed = obs, weather_forecast = fc, holidays = hol,
       attrs = list(origin = O, available_until = au, load_label = "end", tz = tz,
                    weather_issue_cutoff = "available_until", weather_issue_limit = au))
}

dl_cuts <- function(origin = "2024-12-23", available_until = NULL, tz = DL_TZ) {
  O <- lubridate::ymd(origin)
  au <- if (is.null(available_until)) lubridate::force_tz(lubridate::as_datetime(O), tz) +
    lubridate::hours(9L) else available_until
  list(origin_day = O, train_origin = O, last_data_day = O - 1L, tz = tz, available_until = au,
       weather_issue_cutoff = au, weather_issue_limit = au)
}

## painel horario no frame de modelagem, como o despachante entrega ao plugin (to_hourly + to_model_frame)
dl_model_panel <- function(panel, cuts, offset = 60L) {
  h <- dl_lib$to_hourly(panel)
  dl_lib$.prevcarga_plugin_to_model(h, cuts, offset)
}

dl_spec <- function(exogena = "MED", train_days = 400L) {
  list(name = "dessem-linear", split = "origin_scoped", granularity_min = 60L, horizon_days = 9L,
       history_days = list(train = train_days, infer = train_days), seed = 42L,
       modes = c("train", "infer", "forecast"), params = list(exogena = exogena))
}

## =====================================================================================================================
## DL.8 -- suite de verificacao [verif] do plugin (contrato, honestidade, corte, sentinela, clima, split, portabilidade).
## Tudo pelo leitor do M2, pelo despachante da fundacao e pelo molde honesty_gate.R (M3) sobre o deck sintetico de
## scripts/tools/fixture_store.R (SECO-fixture: emissoes 00Z/06Z/18Z, feriado codigo 12 em O-5). Nunca dado real do store.
## Treinar custa ~30 s (225 ajustes de SVM): os treinos compartilhados ficam em cache de sessao (`dl_cached`).
## =====================================================================================================================
DL8_O <- lubridate::ymd("2024-10-15")
DL8_AREA <- "ZZ1"
DL8_DAYS <- 60L                     # dias do deck sintetico (terminam em O)
DL8_HIST <- 40L                     # history_days.train == infer do spec de teste (janela menor que o run.yaml: 1825)
DL8_TOL <- 1e-5

dl_fx_prep <- local({
  done <- FALSE
  function() {
    if (done) return(invisible(TRUE))
    for (m in c("config", "dates", "provenance", "honesty_gate", "schema_long", "evaluation"))
      sys.source(file.path(dl_root(), "scripts", "lib", paste0(m, ".R")), envir = dl_lib)
    sys.source(file.path(dl_root(), "scripts", "tools", "fixture_store.R"), envir = dl_lib)
    done <<- TRUE
    invisible(TRUE)
  }
})

dl_cached <- local({
  cache <- new.env(parent = emptyenv())
  function(key, expr) {
    if (!is.null(cache[[key]])) return(cache[[key]])
    val <- force(expr)
    assign(key, val, envir = cache)
    val
  }
})

dl_au <- function(hhmm, O = DL8_O) lubridate::ymd_hm(paste(format(O), hhmm), tz = DL_TZ)
dl_day_start <- function(d) lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(d)), DL_TZ)
dl_ascii <- function(x) all(utf8ToInt(paste(x, collapse = "")) < 128L)

## Deck sintetico: `load_until` (hhmm do dia O) corta carga e observado (= available_until que o leitor deriva);
## `sentinel`: valor = minutos do interval_start/valid_time; `ragged` = "HH:MM": 2 areas, ZZ2 perde HH:MM-24:00 de O (o corte
## global vira HH:MM e ZZ1 guarda carga alem dele, so no store).
## `noise` (funcao so do proprio valor, sem RNG) e `days`/`hist` maiores formam o deck "sensivel ao CV" (dl_fx_cv_store).
dl_fx_store <- function(load_until = "09:00", sentinel = FALSE, ragged = FALSE, seed = 20241015L, days = DL8_DAYS,
                        hist = DL8_HIST, noise = NULL) {
  dl_fx_prep()
  key <- paste("store", load_until, sentinel, ragged, seed, days, hist, paste(deparse(noise), collapse = ""))
  dl_cached(key, {
    dir <- tempfile("dl8-deck-")
    areas <- if (!isFALSE(ragged)) c(DL8_AREA, "ZZ2") else DL8_AREA
    until <- if (is.null(load_until) || !isFALSE(ragged)) NULL else dl_au(load_until)
    dl_lib$prevcarga_fixture_deck(dir, seed, areas, DL8_O - (days - 1L):0L, load_label = "start", sentinel = sentinel,
                                  load_until = until, tz = DL_TZ, horizon_days = 10L)
    if (is.function(noise)) dir <- dl_lib$prevcarga_fixture_perturb(dir, "load", perturb = noise)
    if (!isFALSE(ragged)) {
      hm <- as.integer(strsplit(ragged, ":", fixed = TRUE)[[1L]])
      dir <- dl_lib$prevcarga_fixture_drop_slots(dir, "ZZ2", DL8_O, ragged, as.integer((1440L - 60L * hm[[1L]] - hm[[2L]]) / 30L))
    }
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
                 sprintf("  X: {areas: [%s], loss: PX}", paste(areas, collapse = ", ")), "order: [X]", "members: []"),
               file.path(dir, "topology.yaml"))
    list(dir = dir, O = DL8_O, days = days, hist = hist, areas = areas)
  })
}

## store apontando para uma copia do deck (mutadores em copia do P.0e)
dl_fx_with_dir <- function(st, dir) {
  file.copy(file.path(st$dir, "topology.yaml"), file.path(dir, "topology.yaml"), overwrite = TRUE)
  modifyList(st, list(dir = dir))
}

dl_fx_ctx <- function(st, rule = "available_until")
  list(data_dir = st$dir, root = dl_root(), topology_path = file.path(st$dir, "topology.yaml"),
       weather_issue_cutoff = rule, origin = st$O)

## cuts de dates.R (nunca literais), no formato lista do despachante
dl_fx_cuts <- function(st, rule = "available_until", origin = st$O) {
  dl_fx_prep()
  O <- lubridate::ymd(origin)
  r <- suppressWarnings(dl_lib$prevcarga_resolve_cuts(dl_fx_ctx(st, rule), O, "dessem-linear"))
  row <- r$cuts[r$cuts$model == "dessem-linear", ]
  list(forecast_origin = r$forecast_origin, origin_day = r$forecast_origin, tz = r$tz, load_label = r$load_label,
       available_until = r$available_until, weather_issue_cutoff = r$weather_issue_cutoff,
       weather_issue_limit = r$weather_issue_limit, split = row$split, train_origin = row$train_origin,
       last_data_day = row$last_data_day, data_lag_days = row$data_lag_days, horizon_days = row$horizon_days)
}

## Painel do leitor (M2). O previsto traz as emissoes <= weather_issue_limit (o leitor entrega todas; o corte e do
## chamador, como o despachante exige); o observado usa `valid_time`. mode "train": ate last_data_day; "infer": ate
## available_until. Feriado futuro do fixture (O+3, codigo 3) sai: sem amostra historica do mesmo tipo o motor recusaria
## (o mesmo recorte de test-dl-train.R). Deck de 2 areas: restrito a ZZ1.
dl_fx_panel <- function(st, cuts, mode = c("train", "infer"), history_days = st$hist + 1L) {
  dl_fx_prep()
  mode <- match.arg(mode)
  ctx <- dl_fx_ctx(st, cuts$weather_issue_cutoff)
  O <- lubridate::as_date(cuts$origin_day); H <- cuts$horizon_days
  p <- suppressWarnings(dl_lib$prevcarga_read_panel(ctx, O, st$areas, horizon_days = H, history_days = history_days))
  fc <- dl_lib$prevcarga_read_weather(ctx, "forecast", st$areas, O, O + H)
  one <- function(x) x[x$area_code == DL8_AREA]
  p$load <- one(p$load); p$weather_observed <- one(p$weather_observed); p$holidays <- one(p$holidays)
  p$weather_forecast <- one(fc[issue_time <= cuts$weather_issue_limit])
  p$holidays <- p$holidays[date < O]
  data.table::setnames(p$weather_observed, "timestamp", "valid_time")
  if (mode == "train") {
    p$load <- p$load[lubridate::as_date(interval_start, tz = cuts$tz) <= cuts$last_data_day]
    p$weather_observed <- p$weather_observed[lubridate::as_date(valid_time, tz = cuts$tz) <= cuts$last_data_day]
  }
  p
}

## --- motor: DL.3 emulado (so ate o DL.3 entrar) e espiao de tune_wsvm ------------------------------------------------
## O motor ainda faz `%m-% hours(1)` em TransformaDeck (o DL.3 troca por lags de indice sobre grade completa verificada).
## Enquanto a linha existir, este teste devolve +1 h aos rotulos de CARGAHIST (o estado pos-DL.3, como o shim do
## test-dl-infer.R). Com o DL.3 integrado a linha some e a emulacao vira no-op. DEPENDENCIA: DL.3.
dl_engine_has_hour_shift <- function(eng) any(grepl("hours\\(1", deparse(body(get("TransformaDeck", envir = eng)))))

dl_emulate_dl3 <- function(eng) {
  if (!dl_engine_has_hour_shift(eng)) return(invisible(FALSE))
  orig <- get("TransformaDeck", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("TransformaDeck", eng)
  if (unlock) unlockBinding("TransformaDeck", eng)
  assign("TransformaDeck", function(deck, ...) {
    deck$CARGAHIST <- data.table::copy(deck$CARGAHIST)[, DataHora := DataHora + lubridate::hours(1L)]
    orig(deck = deck, ...)
  }, envir = eng)
  if (unlock) lockBinding("TransformaDeck", eng)
  invisible(TRUE)
}

dl_engine_env <- function(plugin = dl_plugin()) parent.env(plugin$env)

## Plugin do loader (motor como env-pai), carregado uma vez por sessao; emulacao do DL.3 aplicada.
dl_plugin <- function() dl_cached("plugin", {
  p <- dl_load()
  dl_emulate_dl3(dl_engine_env(p))
  p
})

## espiao em `tune_wsvm` do ENV DO MOTOR: $n (contagem), $fml (formulas), $seed0 (.Random.seed do 1o ajuste desde o reset),
## $state (hash do .Random.seed na entrada de CADA ajuste: a posicao do gerador que cada SVM enxergou)
dl_spy_tune <- function(eng = dl_engine_env()) {
  log <- new.env(); log$n <- 0L; log$fml <- character(); log$seed0 <- NULL; log$state <- character()
  real <- get("tune_wsvm", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("tune_wsvm", eng)
  if (unlock) unlockBinding("tune_wsvm", eng)
  assign("tune_wsvm", function(formula, ..., data) {
    log$n <- log$n + 1L
    log$fml[[log$n]] <- paste(deparse(formula, width.cutoff = 500L), collapse = "")
    seed <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    if (log$n == 1L) log$seed0 <- seed
    log$state[[log$n]] <- rlang::hash(seed)
    real(formula, ..., data = data)
  }, envir = eng)
  list(log = log, reset = function() { log$n <- 0L; log$fml <- character(); log$seed0 <- NULL; log$state <- character() },
       restore = function() { assign("tune_wsvm", real, envir = eng); if (unlock) lockBinding("tune_wsvm", eng) })
}

## --- despachantes da fundacao ---------------------------------------------------------------------------------------
dl_fx_spec <- function(exogena = "MED", plugin = dl_plugin(), hist = DL8_HIST) {
  s <- plugin$spec
  s$history_days$train <- hist; s$history_days$infer <- hist
  s$params$exogena <- exogena
  s
}
dl_fx_ctx_run <- function(cuts) list(load_label = cuts$load_label, timezone = cuts$tz,
                                     weather_issue_cutoff = cuts$weather_issue_cutoff)

dl_train <- function(st, cuts = dl_fx_cuts(st), spec = dl_fx_spec(hist = st$hist), plugin = dl_plugin(), panel = NULL) {
  pan <- if (is.null(panel)) dl_fx_panel(st, cuts, "train") else panel
  dl_with_cli(suppressWarnings(suppressMessages(
    dl_lib$prevcarga_plugin_train(plugin, dl_lib$to_hourly(pan), cuts, spec, dl_fx_ctx_run(cuts)))))
}

dl_infer <- function(art, st, cuts = dl_fx_cuts(st), plugin = dl_plugin(), panel = NULL) {
  pan <- if (is.null(panel)) dl_fx_panel(st, cuts, "infer") else panel
  dl_with_cli(suppressWarnings(suppressMessages(
    dl_lib$prevcarga_plugin_infer(plugin, art, dl_lib$to_hourly(pan), cuts, dl_fx_ctx_run(cuts)))))
}

## forecast acoplado do plugin (mesmo codigo do train + infer, sem freeze no meio): painel de infer, frame de modelagem
dl_forecast <- function(st, cuts = dl_fx_cuts(st), spec = dl_fx_spec(hist = st$hist), plugin = dl_plugin(), panel = NULL) {
  pan <- if (is.null(panel)) dl_fx_panel(st, cuts, "infer") else panel
  m <- dl_model_panel(pan, cuts)
  out <- dl_with_cli(suppressWarnings(suppressMessages(plugin$env$plugin_forecast(m$panel, m$cuts, spec))))
  ## o acoplado do plugin sai no frame de modelagem; a volta ao horario de exibicao e do despachante (to_display_frame)
  out$delivery <- dl_lib$to_display_frame(out$delivery, plugin$descriptor$model_time_offset_min, "interval_end")
  out
}

## erro classificado de uma expressao (NULL se nao houve); avisos do plugin ficam mudos
dl_err <- function(expr) tryCatch({ dl_with_cli(suppressWarnings(suppressMessages(force(expr)))); NULL },
                                  prevcarga_error = function(e) e)

expect_dl_class <- function(e, class, exit, id = NULL) {
  testthat::expect_s3_class(e, "prevcarga_error")
  testthat::expect_identical(e$class_name, class)
  testthat::expect_identical(e$exit_code, exit)
  testthat::expect_match(conditionMessage(e), sprintf("^\\[prevcarga/%s\\]", class))
  testthat::expect_true(dl_ascii(conditionMessage(e)))
  if (!is.null(id)) testthat::expect_identical(e$id, id)
  invisible(e)
}

## sha256 do artefato congelado (manifest do freeze da fundacao)
dl_sha <- function(art) {
  dl_fx_prep()
  f <- tempfile("dl8-sha-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))), add = TRUE)
  dl_with_cli(dl_lib$prevcarga_plugin_freeze_bytes(art, f))
  jsonlite::read_json(paste0(f, ".manifest.json"), simplifyVector = TRUE)$sha256
}

## Delta do molde (max|dy_pred| casado por chave; diferenca estrutural = Inf)
dl_delta <- function(a, b) { dl_fx_prep(); dl_lib$.prevcarga_hg_delta(a, b) }

## perturbacao do store (copia): carga/clima, eixos do fixture_store
dl_fx_perturb <- function(st, channel, ...) {
  dl_fx_prep()
  dl_fx_with_dir(st, dl_lib$prevcarga_fixture_perturb(st$dir, channel, ...))
}

## linhas `arquivo:n` de .R em `dir` cujo codigo (sem comentarios nem texto entre aspas) casa `pattern` (perl)
pc_grep_code <- function(dir, pattern) {
  fs <- if (dir.exists(dir)) list.files(dir, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE) else dir
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

## --- memoizacao por conteudo do painel de treino -------------------------------------------------------------------
## Treinar e deterministico (dois treinos do mesmo painel dao o mesmo sha256, provado em test-portability.R): um painel
## de treino de conteudo identico reaproveita o artefato ja treinado em vez de pagar outros ~30 s. O que se compara nos
## testes e SEMPRE o conteudo do painel entregue ao plugin (a chave), nunca a identidade do store.
dl_panel_key <- function(pan, extra = "") {
  parts <- lapply(pan[c("load", "weather_observed", "weather_forecast", "holidays")], function(t) lapply(t, identity))
  f <- tempfile("dl8-key-"); on.exit(unlink(f), add = TRUE)
  saveRDS(list(parts, extra), f, compress = FALSE, version = 3L)
  unname(tools::md5sum(f))
}

dl_train_memo <- function(st, cuts = dl_fx_cuts(st), spec = dl_fx_spec(hist = st$hist), panel = NULL) {
  pan <- if (is.null(panel)) dl_fx_panel(st, cuts, "train") else panel
  key <- paste("train", dl_panel_key(pan, c(spec$params$exogena, spec$seed, spec$history_days$train)))
  dl_cached(key, dl_train(st, cuts, spec, panel = pan))
}

## artefato base (store de 09:00, MED): treinado uma vez por sessao
dl_base_art <- function() dl_train_memo(dl_fx_store("09:00"))

## Deck "sensivel ao CV": 421 dias (janela de 400) e carga com ruido deterministico moderado. No deck regular o CV do
## tune_wsvm escolhe SEMPRE os mesmos hiperparametros (qualquer posicao do RNG), o que cegaria os CONTROL de RNG/semente
## (a saida nao mudaria nem com o gerador deslocado). Aqui as escolhas por hora de perfil variam com a semente (medido).
## Custa ~55 s por treino: so os CONTROL de RNG (test-mm-head.R, test-split.R) o usam.
DL8_CV_DAYS <- 421L
DL8_CV_HIST <- 400L
dl_fx_cv_store <- function() dl_fx_store("09:00", days = DL8_CV_DAYS, hist = DL8_CV_HIST,
                                         noise = function(v) v + 120 * sin(v * 7919.13))

## artefato treinado de verdade sob available_until 12:00 (painel de treino de conteudo identico ao de 09:00, mas o
## treino roda de fato: a memoizacao por painel nao vale aqui, e a prova de que o corte nao entra no artefato)
dl_art12 <- function() dl_cached("art-12-real", { st <- dl_fx_store("12:00"); dl_train(st, dl_fx_cuts(st)) })
