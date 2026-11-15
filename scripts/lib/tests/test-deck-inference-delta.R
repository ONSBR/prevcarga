## test-deck-inference-delta.R -- verificacao do E2 (deck de inferencia minimo): delta 0 do LONG contra o deck de treino, determinismo
## byte a byte, carga de 9 dias por area, tamanho e FORBIDDEN/CONTROL das regras de emissao e de available_until. Ticket de
## verificacao: nao muda codigo de produto; falha aponta para o dono (E2-T1..T5). HW, LGBM e RF REAIS treinam por prevcarga_train
## (history_days.train reduzido por ctx$spec_overrides; o infer fica no run.yaml) sobre um deck sintetico de helper-deck.R.
## Tudo em tempdir: nenhum acesso a data/, rede, AWS ou variavel de ambiente. O delta 0 grande sobre o store real e o E8.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "holiday_islands", "deck_reader", "validation",
                       "dates", "registry", "features", "inference_needs", "training", "inference", "deck_inference"))
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2023-06-01")
O_09h <- lubridate::ymd_hm("2023-06-01 09:00", tz = TZ)
HIST <- 50L                                   # history_days.train do teste (o run.yaml pede 365..730)
AREAS <- c("SP", "RJ")                        # folhas da topologia real
E2_MEMBERS <- data.table::data.table(mdir = c("holt-winters", "lgbm", "random-forest"),
                                     sigla = c("HW", "LGBM", "RF"))
E2_TOPO <- c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  S: {areas: [SP, RJ], loss: PS}", "order: [S]",
             "members:", "  - {sigla: HW, mdir: holt-winters, exogena: null}", "  - {sigla: LGBM, mdir: lgbm, exogena: null}",
             "  - {sigla: RF, mdir: random-forest, exogena: null}")

## A CLI poe a API da fundacao no globalenv (interino: os plugins a buscam la); aqui so durante o teste chamador.
E2_CLI <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_warn", "prevcarga_delivery_grid", "prevcarga_plugin_check_artifact",
            "prevcarga_plugin_scan_pure", "prevcarga_plugin_stamp", "prevcarga_preserve_rng", "to_semihourly")
e2_with_cli <- function(frame = parent.frame()) {
  nm <- E2_CLI[!vapply(E2_CLI, exists, NA, envir = globalenv(), inherits = FALSE)]
  for (n in nm) assign(n, env[[n]], envir = globalenv())
  withr::defer(rm(list = nm, envir = globalenv()), envir = frame)
}

e2_tmp <- function(frame = parent.frame()) { d <- tempfile("e2"); dir.create(d); withr::defer(unlink(d, recursive = TRUE), envir = frame); d }
e2_files <- function(d) sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
e2_sha <- function(d, fs = e2_files(d)) vapply(file.path(d, fs), digest::digest, "", algo = "sha256", file = TRUE, USE.NAMES = FALSE)

## --- fixture compartilhada ---------------------------------------------------------------------------------------------------
## Deck de treino: HIST + 15 dias ate O (carga ate O 09:00), 4 emissoes de previsto por dia, feriados nas duas areas, topologia so
## com os 3 membros (o deck gerado nao pede janela de DESSEM). Treina HW/LGBM/RF por area na origem O e gera o deck inference.
pc_e2_fixture <- function(frame = parent.frame()) {
  e2_with_cli(frame)
  root <- e2_tmp(frame)
  topo <- file.path(root, "topology.yaml"); writeLines(E2_TOPO, topo)
  deck <- e2_tmp(frame)
  pc_deck_sentinel("end", days = seq(O - (HIST + 14L), O, by = "day"), areas = AREAS, until = O_09h, dir = deck)
  ## a carga do sentinela e periodica pura (o kmeans do HW degenera): ruido deterministico por linha, sem RNG
  ld0 <- .pc_deck_read(deck, "load")
  ld0[, load_mwh := load_mwh + 40 * sin(seq_len(.N) * 0.37) + 25 * cos(seq_len(.N) * 0.011)]
  .pc_deck_rewrite(ld0, deck, "load")
  ## previsto do sentinela e horario e curto (48 h): LGBM/RF montam safra propria em grade de 30 min e RF pede 9 dias de horizonte.
  ## Regrava com 4 emissoes/dia (00/06/12/18Z), valid_time de 30 min ate +9 dias; digitos como o store (issue UTC, valid no fuso da carga)
  em <- data.table::CJ(area_code = AREAS, day = seq(O - (HIST + 14L), O, by = "day"), hh = c(0L, 6L, 12L, 18L), sorted = FALSE)
  em[, issue_utc := lubridate::force_tz(lubridate::as_datetime(day), "UTC") + lubridate::hours(hh)]
  fc <- em[rep(seq_len(nrow(em)), each = 432L)]
  fc[, k := rep(seq_len(432L), times = nrow(em))]
  fc[, `:=`(issue_time = issue_utc, valid_time = lubridate::force_tz(lubridate::with_tz(issue_utc, TZ) + lubridate::minutes(30L * k), "UTC"))]
  fc[, temperature := 20 + match(area_code, AREAS) + 6 * sin(2 * pi * lubridate::hour(valid_time) / 24) +
       3 * sin(as.numeric(lubridate::as_date(valid_time)) * 0.5) + hh / 1000]
  fc[, heat_index := temperature + 1]
  .pc_deck_rewrite(fc[, .(area_code, issue_time, valid_time, temperature, heat_index)], deck, "weather_forecast")
  hol <- data.table::data.table(area_code = rep(AREAS, each = 2L), date = rep(O + c(-20L, 3L), 2L),
                                holiday_type_id = bit64::as.integer64(1L), prevcarga_code = bit64::as.integer64(5L),
                                simplified_code = bit64::as.integer64(1L))
  .pc_deck_write(hol, deck, file.path("auxiliary", "holidays"), character(), "date")
  state <- e2_tmp(frame); dir.create(file.path(state, "artifacts"))
  base <- list(root = pc_root(), topology_path = topo, data_dir = deck, weather_issue_cutoff = "available_until", origin = O,
               code_commit = "c0ffee", renv_lock_hash = "ab12", spec_overrides = list(history_days.train = HIST))
  ctx_fit <- c(base, list(state_dir = state))
  for (i in seq_len(nrow(E2_MEMBERS))) for (a in AREAS) {
    m <- list(sigla = E2_MEMBERS$sigla[[i]], mdir = E2_MEMBERS$mdir[[i]], exogena = NA_character_)
    r <- suppressMessages(suppressWarnings(env$prevcarga_train(m, a, O, ctx_fit)))
    if (!identical(r$status, "trained")) stop("fixture: treino nao concluiu para ", m$mdir, "/", a)
  }
  ctx_train <- c(base, list(artifacts_dir = file.path(state, "artifacts")))
  out_a <- file.path(e2_tmp(frame), "A"); out_b <- file.path(e2_tmp(frame), "B")
  gen <- function(out, data_dir = deck) {
    ctx <- ctx_train; ctx$data_dir <- data_dir
    env$prevcarga_deck_inference_build(ctx, O, AREAS, out)
  }
  ra <- gen(out_a); rb <- gen(out_b)
  list(ctx_train = ctx_train,
       ctx_inference = c(ctx_train[setdiff(names(ctx_train), "data_dir")], list(data_dir = out_a, data_role = "inference")),
       origin = O, areas = AREAS, members = E2_MEMBERS[, .(mdir)], deck = deck, out_a = out_a, out_b = out_b,
       res_a = ra, res_b = rb, gen = gen, topo = topo)
}

FX <- NULL
fx <- function() { if (is.null(FX)) FX <<- pc_e2_fixture(testthat::teardown_env()); FX }

## --- delta 0 ---------------------------------------------------------------------------------------------------------------------
test_that("[AC1] deck inference da delta 0 contra o deck de treino (HW, LGBM, RF)", {
  f <- fx(); e2_with_cli()
  for (m in f$members$mdir) for (a in f$areas) {
    mem <- list(mdir = m, exogena = NA_character_)
    tr <- suppressMessages(suppressWarnings(env$prevcarga_infer_cell(mem, a, f$origin, f$ctx_train)))
    inf <- suppressMessages(suppressWarnings(env$prevcarga_infer_cell(mem, a, f$origin, f$ctx_inference)))
    info <- paste(m, a)
    expect_gt(nrow(tr$predictions), 0L)
    expect_identical(inf$predictions$target_datetime, tr$predictions$target_datetime, info = info)
    expect_identical(nrow(inf$predictions), nrow(tr$predictions), info = info)
    expect_identical(max(abs(inf$predictions$y_pred - tr$predictions$y_pred)), 0, info = info)
    expect_identical(inf$predictions$y_pred, tr$predictions$y_pred, info = info)
    expect_identical(inf$available_until, tr$available_until, info = info)
  }
})

## --- determinismo, 9 dias e tamanho -------------------------------------------------------------------------------------------
test_that("[AC2] duas geracoes: mesma lista de arquivos e sha256 identico (manifest incluso); carga cobre O-9..O-1 e O ate available_until", {
  f <- fx()
  expect_identical(e2_files(f$out_a), e2_files(f$out_b))
  expect_identical(e2_sha(f$out_a), e2_sha(f$out_b))
  expect_true("manifest.json" %in% e2_files(f$out_a))
  ## mesmos bytes no envelope: arquivos e sha256 que o verbo devolve
  expect_identical(f$res_a$result$files, f$res_b$result$files)
  ld <- env$prevcarga_read_load(list(data_dir = f$out_a, weather_issue_cutoff = "available_until"), f$areas)
  for (a in f$areas) {
    s <- ld$interval_start[ld$area_code == a]
    days <- lubridate::as_date(s, tz = TZ)
    expect_identical(as.character(sort(unique(days))), as.character(seq(O - 9L, O, by = "day")), info = a)
    for (d in as.character(O - 9L:1L))                            # O-9..O-1: 48 intervalos por dia
      expect_identical(sum(as.character(days) == d), 48L, info = paste(a, d))
    expect_identical(sum(days == O), 18L, info = a)               # dia O ate available_until (00:00 a 09:00)
    expect_identical(max(ld$interval_end[ld$area_code == a]), O_09h, info = a)
    expect_false(anyDuplicated(s) > 0L)
  }
  ## NFR de tamanho: no maximo 15 dias de carga por area
  raw <- .pc_deck_read(f$out_a, "load")
  expect_lte(max(table(raw$area_code)) / 48, 15)
  expect_lt(nrow(raw), nrow(.pc_deck_read(f$deck, "load")))
  ## CONTROL: um deck gerado de outra origem nao tem os mesmos bytes (o teste de igualdade enxerga diferenca)
  other <- file.path(e2_tmp(), "O2")
  env$prevcarga_deck_inference_build(c(f$ctx_train, list(data_dir = f$deck)), O, f$areas[1L], other)
  expect_false(identical(e2_files(other), e2_files(f$out_a)) && identical(e2_sha(other), e2_sha(f$out_a)))
})

## --- FORBIDDEN / CONTROL ---------------------------------------------------------------------------------------------------------
## FORBIDDEN: copia do deck de treino com (i) uma emissao com issue_time = weather_issue_limit + 1h e (ii) uma linha de carga depois
## de available_until. CONTROL: a copia sem injecao. O deck gerado e o LONG nao podem ver as linhas injetadas.
test_that("[AC4] FORBIDDEN: emissao > limite e carga > available_until nao entram no deck nem no painel; CONTROL: saida identica", {
  f <- fx(); e2_with_cli()
  ctl <- e2_tmp()
  file.copy(list.files(f$deck, full.names = TRUE), ctl, recursive = TRUE)
  forb <- e2_tmp()
  file.copy(list.files(f$deck, full.names = TRUE), forb, recursive = TRUE)
  lim <- env$prevcarga_resolve_cuts(c(f$ctx_train, list(data_dir = forb)), origin = O)$weather_issue_limit
  ## (i) emissao do dia O deslocada para limite + 1h, com valores marcados
  fc <- .pc_deck_read(forb, "weather_forecast")
  lim_utc <- lubridate::force_tz(lubridate::with_tz(lim, "UTC"), "UTC")           # digitos UTC do limite
  base <- fc[issue_time == lim_utc]
  expect_gt(nrow(base), 0L)
  inj <- data.table::copy(base)
  inj[, `:=`(issue_time = issue_time + 3600, valid_time = valid_time + 3600, temperature = temperature + 70)]
  expect_gt(max(inj$issue_time), lim_utc)
  .pc_deck_rewrite(rbind(fc, inj), forb, "weather_forecast")
  ## (ii) carga depois de available_until (intervalo 09:00-09:30 do dia O, rotulo end = 09:30)
  ld <- .pc_deck_read(forb, "load")
  ## so em SP: o available_until e derivado do dado (corte global = menor ultimo intervalo entre as areas), e RJ o mantem em 09:00
  extra <- ld[area_code == "SP", .SD[which.max(timestamp)], by = area_code]
  extra[, `:=`(timestamp = timestamp + 1800, load_mwh = 99999)]
  .pc_deck_rewrite(rbind(ld, extra[, names(ld), with = FALSE]), forb, "load")
  expect_gt(nrow(.pc_deck_read(forb, "load")), nrow(.pc_deck_read(ctl, "load")))

  expect_warning(gf <- f$gen(file.path(e2_tmp(), "F"), forb), "available_until diferentes")   # a linha de SP alem do corte global existe
  gc <- f$gen(file.path(e2_tmp(), "C"), ctl)
  of <- gf$result$out; oc <- gc$result$out
  ## nenhuma linha injetada no deck gerado
  gfc <- .pc_deck_read(of, "weather_forecast")
  expect_false(any(gfc$issue_time == max(inj$issue_time)))
  expect_false(any(gfc$temperature >= 70 + min(base$temperature)))
  expect_true(all(gfc$issue_time <= lim_utc))
  expect_false(any(.pc_deck_read(of, "load")$load_mwh == 99999))
  ## nenhuma linha injetada no painel de inferencia (leitor sobre o deck gerado)
  p <- suppressWarnings(env$prevcarga_read_panel(list(data_dir = of, weather_issue_cutoff = "available_until", root = pc_root(), topology_path = f$topo),
                                                 O, AREAS, 9L, 9L))
  expect_true(all(p$weather_forecast$issue_time <= p$attrs$weather_issue_limit))
  expect_false(any(p$weather_forecast$temperature >= 70 + min(base$temperature)))
  expect_false(any(p$load$load_mwh == 99999))
  expect_identical(max(p$load$interval_end), O_09h)
  ## CONTROL: arquivos de dados do deck gerado byte a byte iguais; o manifest so difere no hash do deck de treino (que e a injecao)
  expect_identical(e2_files(of), e2_files(oc))
  fs <- setdiff(e2_files(of), "manifest.json")
  expect_identical(e2_sha(of, fs), e2_sha(oc, fs))
  expect_identical(gf$result$data_hash, gc$result$data_hash)
  expect_false(identical(gf$result$train_deck_hash, gc$result$train_deck_hash))
  mf <- jsonlite::read_json(file.path(of, "manifest.json")); mc <- jsonlite::read_json(file.path(oc, "manifest.json"))
  mf$inference$train_deck_hash <- NULL; mc$inference$train_deck_hash <- NULL
  expect_identical(mf, mc)
  ## LONG identico entre FORBIDDEN e CONTROL (inferencia sobre cada deck gerado)
  for (m in f$members$mdir) {
    mem <- list(mdir = m, exogena = NA_character_)
    yf <- suppressMessages(suppressWarnings(env$prevcarga_infer_cell(mem, "SP", O, c(f$ctx_inference[setdiff(names(f$ctx_inference), "data_dir")], list(data_dir = of)))))
    yc <- suppressMessages(suppressWarnings(env$prevcarga_infer_cell(mem, "SP", O, c(f$ctx_inference[setdiff(names(f$ctx_inference), "data_dir")], list(data_dir = oc)))))
    expect_identical(yf$predictions, yc$predictions, info = m)
  }
  ## CONTROL positivo: o deck de treino COM a injecao e visto pelo leitor sem corte (a injecao existe)
  expect_true(any(.pc_deck_read(forb, "load")$load_mwh == 99999))
  expect_true(any(.pc_deck_read(forb, "weather_forecast")$issue_time == max(inj$issue_time)))
})
