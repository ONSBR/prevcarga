## test-forecast.R -- verificacao ponta a ponta do `forecast` (M6.11, fecha o epico M6): dois modos de load_label, sentinela de
## alinhamento (1a entrega O 09:30, 48 rotulos por dia completo, intervalo preservado por combina/reconcile/export), clima nas duas
## regras de weather_issue_cutoff (reach-control do Reconcile) e a rodada das 00h. Deck sentinela do fixture_store.R; k-hat `latest`
## treinado pelo reconcile REAL (`prevcarga_reconcile_train`) e pesos da combina treinados no setup (forecast.R nunca treina).
## Os membros sao os 3 reais da topologia-fixture (RF, LGBM, HW) com ESPIAO em prevcarga_infer_cell: entregam o sentinela (a verdade do
## deck, minutos desde 2026-09-01 do interval_start); o espiao de clima le o clima previsto pelo LEITOR REAL (regra + lag do manifest).
## Os 8 membros reais ponta a ponta vivem em test-forecast_e2e.R (processo filho, lento); o shell sujo em test-forecast_shell.R.
## Cada FORBIDDEN (delta 0) tem o seu CONTROL. Tudo em tempdir; sem variaveis de ambiente; datas por lubridate com tz explicito.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast", "honesty_gate"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- lubridate::ymd("2026-10-15")      # sentinela positivo: o deck codifica minutos desde 2026-09-01
SENTINEL_BASE <- lubridate::ymd_hm("2026-09-01 00:00", tz = M6_TZ)
base <- withr::local_tempdir("m6e2e-")
TOPO <- m6f_topology_file(base)

## --- sentinela --------------------------------------------------------------------------------------------------------------
## y_pred do membro = minutos desde 2026-09-01 do interval_start (a mesma codificacao da carga do deck sentinela).
m6_sentinel_value <- function(interval_end, tz = M6_TZ)
  lubridate::time_length((interval_end - lubridate::minutes(30L)) - lubridate::force_tz(SENTINEL_BASE, tz), "minute")
## decodifica o valor de volta para o interval_end que ele codifica
m6_sentinel_decode <- function(y, tz = M6_TZ) lubridate::force_tz(SENTINEL_BASE, tz) + lubridate::minutes(as.integer(round(y)) + 30L)

m6_to_local <- function(x, tz = M6_TZ) lubridate::with_tz(x, tz)
m6_hhmm <- function(x, tz = M6_TZ) format(x, "%H:%M", tz = tz)

## Espiao de prevcarga_infer_cell: o membro entrega o sentinela na grade real do descriptor, no corte que o LEITOR deriva do deck
## (nunca calculado aqui). `weather` = pondera o clima previsto lido pelo leitor real (regra de corte + lag do manifest) no y_pred.
m6_infer_spy <- function(weather = FALSE) function(member, area, origin, ctx, variant = NULL, version = NULL, pin = FALSE, ...) {
  cuts <- env$prevcarga_resolve_cuts(ctx, origin = origin)
  dd <- as.integer(env$prevcarga_plugin_descriptor(member$mdir, env$prevcarga_models_root(ctx$root))$delivery_days)
  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), cuts$tz)
  ie <- seq(cuts$available_until + lubridate::minutes(30L), t0 + lubridate::days(dd), by = "30 min")
  y <- m6_sentinel_value(ie, cuts$tz)
  if (!isFALSE(weather)) {
    wf <- if (identical(weather, "leak"))                        # CONTROL de VAZA: TODAS as emissoes, a mais fresca por valid_time (ignora o limite)
      env$prevcarga_freshest_issue(env$prevcarga_read_weather(ctx, "forecast", area, origin, origin + dd),
                                   lubridate::ymd_h("2100-01-01 00", tz = "UTC"))
    else env$prevcarga_read_weather(ctx, "forecast", area, origin, origin + dd, origin = origin)
    key <- as.numeric(lubridate::with_tz(wf$valid_time, "UTC"))
    t_ <- stats::setNames(wf$temperature, key)[as.character(as.numeric(lubridate::with_tz(ie - lubridate::minutes(30L), "UTC")))]
    y <- y + 1e-3 * ifelse(is.na(t_), 0, t_)
  }
  list(predictions = data.table::data.table(target_datetime = ie, y_pred = y),
       version = if (is.null(version)) paste0("v_", substr(digest::digest(paste(member$sigla, area), algo = "sha256"), 1L, 16L)) else version,
       available_until_posix = cuts$available_until)
}

m6_mock <- function(..., .frame = parent.frame()) {
  fs <- list(...)
  for (n in names(fs)) local({ nm <- n; old <- env[[nm]]; withr::defer(assign(nm, old, envir = env), envir = .frame) })
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
}

## Entrega com o espiao ativo; envolve o forecast com skip_validate (o deck sintetico e curto: a validacao reprova a janela).
m6_forecast <- function(ctx, origin, weather = FALSE, .frame = parent.frame(), ...) {
  m6_mock(prevcarga_infer_cell = m6_infer_spy(weather), prevcarga_train = m6f_train_spy(env, new.env()), .frame = .frame)
  env$prevcarga_forecast(ctx, origin = origin, skip_validate = TRUE, ...)
}

m6_tree <- function(d) {
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
  f <- f[!f %in% c("manifest.json", "run_manifest.json")]            # identidade da rodada: carregam o rotulo DECLARADO (abaixo)
  stats::setNames(vapply(f, function(x) digest::digest(file = file.path(d, x), algo = "sha256"), ""), f)
}
## manifest sem o que e funcao do rotulo declarado (M6.7: load_label entra na versao e no perfil k-hat) e do relogio (gerado_em):
## o resto (origem, available_until, fuso, arquivos[] com sha256, regra de clima, niveis) e identico entre os dois modos.
m6_manifest <- function(d) {
  m <- jsonlite::read_json(file.path(d, "manifest.json"))
  m$gerado_em <- NULL; m$load_label <- NULL; m$versao <- NULL
  m$khat <- lapply(m$khat, function(k) { k$versao <- NULL; k })
  m
}
m6_sha_table <- function(r) list(files = m6_tree(r$saida), manifest = m6_manifest(r$saida))

## --- setup ponta a ponta ----------------------------------------------------------------------------------------------------
## `load_label`: rotulo DECLARADO no manifest; `shift_min` = 0 grava a carga rotulada pelo fim (interval_end), -30 grava o MESMO
## dado rotulado pelo inicio (timestamp = interval_end - 30 min). Declarar o rotulo errado e o CONTROL (desloca todo intervalo).
## `weather_rule` = weather_issue_cutoff do ctx; `weather_lag_hours` do manifest (deck sintetico: 0 por padrao).
m6_e2e_setup <- function(load_label = "end", shift_min = 0L, weather_rule = "available_until", until = "09:00",
                         weather_lag_hours = 0L, prepare = TRUE, .frame = parent.frame()) {
  dir <- withr::local_tempdir("m6e2e-case-", .local_envir = .frame)
  deck <- file.path(dir, "deck")
  env$prevcarga_fixture_deck(deck, seed = 11, areas = c(M6F_LEAVES, M6F_SS), days = seq(O - 39L, O, by = "day"), sentinel = TRUE,
                             load_label = if (identical(as.integer(shift_min), 0L)) "end" else "start", manifest_label = load_label,
                             load_until = if (identical(until, "24:00")) lubridate::ymd_hm(paste(O + 1L, "00:00"), tz = M6_TZ)
                                          else lubridate::ymd_hm(paste(O, until), tz = M6_TZ),
                             tz = M6_TZ, weather_lag_hours = weather_lag_hours)
  state <- file.path(dir, "state"); dir.create(state, recursive = TRUE)
  ctx <- m6f_ctx(deck, state, TOPO, list(weather_issue_cutoff = weather_rule))
  m6_mock(prevcarga_infer_cell = m6_infer_spy(FALSE), .frame = .frame)
  if (prepare) m6f_prepare_origin(env, ctx, O)                       # k-hat `latest` (reconcile_train real) + pesos da combina
  list(ctx = ctx, origin = O, deck = deck, state = state, dir = dir)
}

## copia do estado preparado para outra rodada (o export e imutavel: uma rodada nova pede um state novo)
m6_clone <- function(s, deck = s$deck, .frame = parent.frame(), ...) {
  st <- withr::local_tempdir("m6e2e-st-", .local_envir = .frame)
  file.copy(list.files(s$state, full.names = TRUE), st, recursive = TRUE)
  utils::modifyList(s$ctx, c(list(data_dir = deck, state_dir = st), list(...)))
}

## =========================================================================================================================
test_that("[AC1] load_label end x start deslocado => sha256 identical; CONTROL: deck deslocado lido como end e acusado", {
  a <- m6_e2e_setup("end", 0L); b <- m6_e2e_setup("start", -30L)
  ra <- m6_forecast(a$ctx, a$origin, niveis = "tudo")$result
  rb <- m6_forecast(b$ctx, b$origin, niveis = "tudo")$result
  expect_identical(ra$available_until, "2026-10-15T09:00")
  expect_identical(rb$available_until, ra$available_until)
  expect_identical(m6_sha_table(rb), m6_sha_table(ra))              # subsistemas/SIN e manifest (menos gerado_em e o rotulo declarado)
  expect_identical(jsonlite::read_json(file.path(ra$saida, "manifest.json"))$load_label, "end")
  expect_identical(jsonlite::read_json(file.path(rb$saida, "manifest.json"))$load_label, "start")
  ## CONTROL: o MESMO deck deslocado, declarado como end (rotulo trocado): o available_until e o alinhamento mudam 1 slot
  ctl <- m6_e2e_setup("end", -30L)
  rc <- m6_forecast(ctl$ctx, ctl$origin, niveis = "tudo")$result
  expect_identical(rc$available_until, "2026-10-15T08:30")                           # o leitor le o dado de inicio como fim: 1 slot a menos
  expect_false(identical(m6_sha_table(rc), m6_sha_table(ra)))
})

## =========================================================================================================================
## AC2 -- sentinela de alinhamento
## =========================================================================================================================
m6_with_mock <- function(fs, code) {
  old <- lapply(stats::setNames(nm = names(fs)), function(n) env[[n]])
  on.exit(for (n in names(old)) assign(n, old[[n]], envir = env), add = TRUE)
  for (n in names(fs)) assign(n, fs[[n]], envir = env)
  force(code)
}

m6_read <- function(r, rel) data.table::as.data.table(env$prevcarga_read_long(file.path(r$saida, rel), list(tz = M6_TZ)))

## Devolve as violacoes do sentinela em `rel` (character(0) = ok). `div`: divisor do valor (SIN = soma de 2 subsistemas = 2 x).
m6_sentinel_violations <- function(r, rel, div = 1) {
  dt <- m6_read(r, rel)
  v <- character()
  d0 <- dt[d_plus == 0L]
  if (!identical(m6_hhmm(min(d0$target_datetime)), "09:30") || !identical(as.Date(min(d0$target_datetime), tz = M6_TZ), as.Date(O)))
    v <- c(v, "1o rotulo do dia O != 09:30")
  full <- dt[d_plus > 0L, .N, by = d_plus]
  if (!all(full$N == 48L)) v <- c(v, "dia completo sem 48 rotulos")
  want <- c(sprintf("%02d:%02d", rep(0:23, each = 2L), rep(c(0L, 30L), 24L))[-1L], "00:00")        # 00:30 .. 23:30, 00:00
  day1 <- dt[d_plus == 1L]
  if (!identical(m6_hhmm(day1$target_datetime), want)) v <- c(v, "rotulos do dia completo != 00:30..00:00")
  if (!identical(m6_sentinel_decode(dt$y_pred / div), dt$target_datetime)) v <- c(v, "valor nao decodifica o proprio intervalo")
  if (!identical(dt$interval_start, dt$target_datetime - lubridate::minutes(30L))) v <- c(v, "interval_start != target - 30 min")
  v
}

m6_check_delivery <- function(r) {
  paths <- vapply(r$arquivos, function(a) a$path, "")
  bad <- list()
  for (p in paths) {
    div <- if (identical(p, "SIN.parquet")) 2 else 1
    v <- m6_sentinel_violations(r, p, div)
    if (length(v)) bad[[p]] <- v
    ## ADR-0032: forecast_origin e TEXTO yyyy-MM-dd em todo parquet entregue (leitura crua, sem o leitor da lib)
    raw <- arrow::read_parquet(file.path(r$saida, p))
    if (!is.character(raw$forecast_origin) || !all(grepl("^\\d{4}-\\d{2}-\\d{2}$", raw$forecast_origin))) bad[[p]] <- c(bad[[p]], "forecast_origin nao e texto yyyy-MM-dd")
  }
  bad
}

## a saida do modulo deslocada 1 slot: cada valor passa a carregar o rotulo do slot vizinho (passa nos asserts estruturais do LONG)
shift_slot <- function(dt) {
  dt <- data.table::copy(dt)
  dt[, y_pred := c(y_pred[-1L], y_pred[.N]), by = area]
  dt[]
}

test_that("[AC2] 1a entrega O 09:30 em folha/subsistema/SIN/membro; 48 rotulos por dia completo; valor decodifica o proprio intervalo", {
  s <- m6_e2e_setup("end", 0L)
  r <- m6_forecast(s$ctx, s$origin, niveis = "tudo", incluir_membros = TRUE)$result
  paths <- vapply(r$arquivos, function(a) a$path, "")
  expect_true(all(c("SIN.parquet", "subsistemas/SS1.parquet", "folhas/A1.parquet", "membros/RF/A1.parquet", "membros/HW/B1.parquet") %in% paths))
  expect_length(paths, 15L)
  expect_identical(m6_check_delivery(r), list())
  ## FORBIDDEN: um writer-fixture que grava forecast_origin como Date => a leitura da entrega falha contract.long_schema
  raw <- arrow::read_parquet(file.path(r$saida, "folhas", "A1.parquet"))
  raw$forecast_origin <- as.Date(raw$forecast_origin)
  bad <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(raw, bad)
  cond <- tryCatch(env$prevcarga_read_long(bad, list(tz = M6_TZ)), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "contract.long_schema")
})

test_that("[AC2] CONTROL: a saida da combina (depois a do reconcile) deslocada 1 slot reprova o sentinela", {
  s <- m6_e2e_setup("end", 0L)
  real_c <- env$prevcarga_combina_apply
  shifted_c <- function(...) { ap <- real_c(...); ap$long <- shift_slot(ap$long); ap }
  r1 <- m6_with_mock(list(prevcarga_combina_apply = shifted_c), m6_forecast(s$ctx, s$origin, niveis = "tudo")$result)
  b1 <- m6_check_delivery(r1)
  expect_true(length(b1) > 0L)                                                      # folhas e (derivados) subsistemas/SIN acusados
  expect_true("folhas/A1.parquet" %in% names(b1))
  s2 <- m6_e2e_setup("end", 0L)
  real_r <- env$prevcarga_reconcile_apply
  shifted_r <- function(...) { rec <- real_r(...); rec$long <- lapply(rec$long, shift_slot); rec }
  r2 <- m6_with_mock(list(prevcarga_reconcile_apply = shifted_r), m6_forecast(s2$ctx, s2$origin, niveis = "tudo")$result)
  b2 <- m6_check_delivery(r2)
  expect_false("folhas/A1.parquet" %in% names(b2))                                  # a folha (combina) segue alinhada
  expect_true(all(c("SIN.parquet", "subsistemas/SS1.parquet", "subsistemas/SS2.parquet") %in% names(b2)))
})

test_that("[AC2] dia O completo ao meio-dia: o 1o rotulo acompanha o available_until (12:30), o sentinela continua decodificando", {
  s <- m6_e2e_setup("end", 0L, until = "12:00")
  r <- m6_forecast(s$ctx, s$origin, niveis = "tudo")$result
  expect_identical(r$available_until, "2026-10-15T12:00")
  dt <- m6_read(r, "folhas/A2.parquet")
  expect_identical(m6_hhmm(min(dt$target_datetime[dt$d_plus == 0L])), "12:30")
  expect_identical(m6_sentinel_decode(dt$y_pred), dt$target_datetime)
})

## =========================================================================================================================
## AC3 -- clima nas duas regras de weather_issue_cutoff (reach-control do Reconcile, ADR-0024/ADR-0029)
## =========================================================================================================================
M6_LAST_ISSUE <- lubridate::ymd_h("2026-10-15 00", tz = "UTC")      # ultima emissao permitida nas duas regras (deck 00Z/06Z/18Z)

## modulo = forecast inteiro sobre o deck de entrada; saida = SIN (target_datetime, y_pred). `shas` guarda o sha256 da entrega por deck.
m6_weather_module <- function(s, weather = TRUE, shas = new.env()) {
  function(input) {
    ctx <- m6_clone(s, deck = input$deck[[1L]])
    r <- m6_forecast(ctx, s$origin, weather = weather)$result
    shas[[input$deck[[1L]]]] <- m6_sha_table(r)
    out <- m6_read(r, "SIN.parquet")
    data.frame(target_datetime = out$target_datetime, y_pred = out$y_pred)
  }
}

m6_reach <- function(s, limit, module, forbidden_extra = list(), control = NULL) {
  perturb <- function(...) function(d) data.table::data.table(deck = env$prevcarga_fixture_perturb(d$deck[[1L]], "weather_forecast", ...))
  env$prevcarga_reach_control(
    module, data.table::data.table(deck = s$deck),
    arms = list(forbidden = c(list(emissao_apos_limite = perturb(issue_after = limit)), forbidden_extra),
                control = if (is.null(control)) list(ultima_emissao_permitida = perturb(issue_after = M6_LAST_ISSUE - lubridate::minutes(1L),
                                                                                    issue_at_or_before = M6_LAST_ISSUE)) else control))
}

test_that("[AC3] clima nas duas regras: FORBIDDEN (apos o limite) byte-identico, CONTROL (ultima permitida) muda => HONESTO", {
  for (rule in c("available_until", "origin_midnight_utc")) {
    ## 08:30 + lag de 6 h: a emissao 06Z so chega as 09:00 (> available_until) e NAO pode entrar na regra available_until
    s <- m6_e2e_setup("end", 0L, weather_rule = rule, until = "08:30", weather_lag_hours = 6L)
    cuts <- env$prevcarga_resolve_cuts(s$ctx, origin = s$origin)
    limit <- cuts$weather_issue_limit
    expect_identical(format(lubridate::with_tz(limit, "UTC"), "%Y-%m-%d %H:%M"),
                     if (identical(rule, "available_until")) "2026-10-15 05:30" else "2026-10-15 00:00", info = rule)
    shas <- new.env()
    v <- m6_reach(s, limit, m6_weather_module(s, TRUE, shas))
    expect_identical(v$veredito, "HONESTO", info = rule)
    d <- v$deltas
    expect_true(all(d$changed_input), info = rule)
    expect_identical(d$delta[d$kind == "forbidden"], 0, info = rule)
    expect_gt(d$delta[d$kind == "control"], 0)
    ## byte-identico: a entrega do braco FORBIDDEN tem os mesmos sha256 (e o mesmo manifest); a do CONTROL muda
    perturbed <- setdiff(ls(shas), s$deck)
    expect_length(perturbed, 2L)
    ref <- shas[[s$deck]]
    by_arm <- vapply(perturbed, function(k) identical(shas[[k]], ref), NA)
    expect_identical(sum(by_arm), 1L, info = rule)                            # so o braco FORBIDDEN e identical a referencia
  }
})

test_that("[AC3] modulo que vaza o clima apos o limite da VAZA; CONTROL vacuo da INDETERMINADO; ambos reprovam", {
  s <- m6_e2e_setup("end", 0L, weather_rule = "available_until", until = "08:30", weather_lag_hours = 6L)
  limit <- env$prevcarga_resolve_cuts(s$ctx, origin = s$origin)$weather_issue_limit
  vaza <- m6_reach(s, limit, m6_weather_module(s, weather = "leak"))
  expect_identical(vaza$veredito, "VAZA")
  expect_gt(max(vaza$deltas$delta[vaza$deltas$kind == "forbidden"]), 0)
  ## CONTROL vacuo: perturba uma emissao antiga, sempre superada pela mais fresca permitida => delta 0 no CONTROL
  stale <- function(d) data.table::data.table(deck = env$prevcarga_fixture_perturb(d$deck[[1L]], "weather_forecast",
                                                                                     issue_at_or_before = M6_LAST_ISSUE - lubridate::days(2L)))
  vazio <- m6_reach(s, limit, m6_weather_module(s, TRUE), control = list(emissao_superada = stale))
  expect_identical(vazio$veredito, "INDETERMINADO")
  expect_identical(vazio$deltas$delta[vazio$deltas$kind == "control"], 0)
  expect_false(identical(vaza$veredito, "HONESTO")); expect_false(identical(vazio$veredito, "HONESTO"))
})

test_that("[AC3] as duas regras dao versao e chaves distintas, ambas preservadas; o manifest traz a regra efetiva", {
  s <- m6_e2e_setup("end", 0L, weather_rule = "available_until", until = "09:00", weather_lag_hours = 6L)
  ## deck com a emissao 06Z do dia O perturbada: so a regra available_until (06Z + 6 h <= 09:00) a enxerga
  s$ctx$data_dir <- env$prevcarga_fixture_perturb(s$deck, "weather_forecast", issue_after = M6_LAST_ISSUE, issue_at_or_before = M6_LAST_ISSUE + lubridate::hours(6L))
  withr::defer(unlink(s$ctx$data_dir, recursive = TRUE))
  ctx_m <- utils::modifyList(s$ctx, list(weather_issue_cutoff = "origin_midnight_utc"))     # mesmo deck, mesmo state
  ra <- m6_forecast(s$ctx, s$origin, weather = TRUE)$result
  rm_ <- m6_forecast(ctx_m, s$origin, weather = TRUE)$result
  expect_false(identical(ra$versao, rm_$versao))
  expect_false(identical(ra$saida, rm_$saida))
  expect_true(dir.exists(ra$saida) && dir.exists(rm_$saida))                                 # imutavel: a segunda nao apagou a primeira
  expect_identical(jsonlite::read_json(file.path(ra$saida, "manifest.json"))$weather_issue_cutoff, "available_until")
  expect_identical(jsonlite::read_json(file.path(rm_$saida, "manifest.json"))$weather_issue_cutoff, "origin_midnight_utc")
  ## releitura da primeira regra: no-op na mesma chave (as duas chaves seguem la)
  again <- m6_forecast(s$ctx, s$origin, weather = TRUE)$result
  expect_true(again$ja_publicado); expect_identical(again$saida, ra$saida)
  ## o clima entra no y_pred: as duas regras entregam valores diferentes (a regra do limite foi aplicada pelo leitor)
  expect_false(identical(m6_read(ra, "SIN.parquet")$y_pred, m6_read(rm_, "SIN.parquet")$y_pred))
})

## =========================================================================================================================
## Rodada das 00h (available_until = O 00:00: nenhuma informacao do dia O) e recusa do dia O completo
## =========================================================================================================================
test_that("[00h] available_until = O 00:00: todos os membros e niveis entregam a partir de O 00:30, sem carga do dia O; dia O completo e recusado", {
  s <- m6_e2e_setup("end", 0L, until = "00:00")
  r <- m6_forecast(s$ctx, s$origin, niveis = "tudo", incluir_membros = TRUE)$result
  expect_identical(r$available_until, "2026-10-15T00:00")
  expect_match(r$saida, "/forecasts/2026-10-15/20261015T0000/[0-9a-f]{12}$")
  paths <- vapply(r$arquivos, function(a) a$path, "")
  expect_length(grep("^membros/", paths), 9L)                                    # 3 membros x 3 folhas
  for (p in paths) {
    dt <- m6_read(r, p)
    expect_identical(format(min(dt$target_datetime), "%Y-%m-%d %H:%M", tz = M6_TZ), "2026-10-15 00:30", info = p)
    expect_true(all(dt$d_plus[dt$target_datetime <= lubridate::force_tz(lubridate::ymd_hm("2026-10-16 00:00"), M6_TZ)] == 0L), info = p)
    expect_identical(m6_sentinel_decode(dt$y_pred / if (identical(p, "SIN.parquet")) 2 else 1), dt$target_datetime, info = p)
  }
  ld <- env$prevcarga_read_load(s$ctx, areas = "A1", origin = O)
  expect_true(all(lubridate::as_date(ld$interval_start, tz = M6_TZ) < O))         # o deck nao tem nenhuma carga do dia O
  ## CONTROL: o dia O completo (available_until = O+1 00:00) e recusado antes de qualquer entrega
  c0 <- m6_e2e_setup("end", 0L, until = "24:00", prepare = FALSE)
  cond <- tryCatch(m6_forecast(c0$ctx, c0$origin), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "completude.forecast_origin_day_complete")
  expect_identical(cond$exit_code, 3L)
  expect_false(dir.exists(file.path(c0$state, "forecasts")))
})
