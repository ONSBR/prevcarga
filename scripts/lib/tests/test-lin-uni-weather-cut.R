## test-lin-uni-weather-cut.R -- E5-T5: fecha o AC de epico do corte de clima com os MEMBROS REAIS (LIN_UNI e LIN_MED, dessem-linear).
## Store sintetico em tempdir (fixture_store.R) + config/ e models/ copiados do projeto; topologia com os membros reais (derivados de
## prevcarga_members, nenhuma contagem literal) e uma unica area sintetica. Os dois membros treinam pela fachada (prevcarga_train)
## na MESMA origem e area; a inferencia (prevcarga_infer_cell) roda com o previsto cobrindo so O..O+3 e com o previsto vazio.
##   FORBIDDEN: LIN_UNI nao le clima => entrega 9 dias, sem warning.forecast_weather_horizon_cut, igual com previsto curto e vazio.
##   CONTROL:   LIN_MED le clima => aborta completude.weather_forecast_short nos mesmos dois decks (o teste nao e vacuo).
## Sem golden do oraculo para a UNI (decisao L2: aceite por invariancia). Tempo: 2 treinos reais do SVM (~1-3 min).
## Sem variaveis de ambiente; datas por lubridate com tz explicito; saida ASCII.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                       "registry", "features", "holiday_islands", "inference_needs", "training", "inference"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2024-10-15")
AREA <- "ZZ1"
DAYS <- 60L
HIST <- 40L

## --- raiz de projeto de teste: config/ e models/ copiados; run.yaml do dessem-linear com janela curta (deck sintetico) --------------
lw_root <- function() {
  rt <- withr::local_tempdir("lw-root-", .local_envir = globalenv())
  dir.create(file.path(rt, "config"))
  dir.create(file.path(rt, "models"))
  src <- pc_root()
  file.copy(list.files(file.path(src, "config"), full.names = TRUE), file.path(rt, "config"), recursive = TRUE)
  file.copy(list.files(file.path(src, "models"), full.names = TRUE), file.path(rt, "models"), recursive = TRUE)
  yml <- file.path(rt, "models", "dessem-linear", "run.yaml")
  ln <- readLines(yml, warn = FALSE)
  ln <- sub("^history_days:.*$", sprintf("history_days: {train: %d, infer: %d}", HIST, HIST), ln)
  ln <- sub("janela_dessem: ano_civil", "janela_dessem: referencia", ln, fixed = TRUE)
  writeLines(ln, yml)
  ## topologia: os membros reais (config/topology.yaml) sobre uma unica area sintetica
  tl <- readLines(file.path(src, "config", "topology.yaml"), warn = FALSE)
  i0 <- which(tl == "subsystems:"); i1 <- which(grepl("^order:", tl))
  tl <- c(tl[seq_len(i0)], sprintf("  X: {areas: [%s], loss: PX}", AREA), "order: [X]", tl[(i1 + 1L):length(tl)])
  writeLines(tl, file.path(rt, "config", "topology.yaml"))
  rt
}
RT <- lw_root()
TOPO <- file.path(RT, "config", "topology.yaml")
MEMBERS <- env$prevcarga_members(path = TOPO, root = RT)
mem <- function(sigla) {
  r <- MEMBERS[MEMBERS$sigla == sigla, , drop = FALSE]
  list(sigla = r$sigla, mdir = r$mdir, exogena = r$exogena)
}

## --- decks: base (previsto completo), previsto so ate O+3 e previsto vazio ---------------------------------------------------------
deck_base <- function() {
  d <- withr::local_tempdir("lw-deck-", .local_envir = globalenv())
  until <- lubridate::ymd_hm(paste(format(O), "09:00"), tz = TZ)
  env$prevcarga_fixture_deck(d, 20241015L, AREA, O - (DAYS - 1L):0L, load_label = "start", load_until = until, tz = TZ,
                             horizon_days = 10L)
  ## o feriado sintetico de O+3 (codigo sem amostra historica) e do gerador, nao do caso: sai para o motor nao recusar
  f <- list.files(d, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  f <- f[grepl("holidays", f, fixed = TRUE) & grepl("year=2024", f, fixed = TRUE)]
  stopifnot(length(f) > 0L)
  for (p in f) {
    x <- env$.pc_fx_read_file(p, "holidays")
    env$.pc_fx_write_file(x[!(x$date %in% (O + 3L))], p, character())
  }
  writeLines(readLines(TOPO), file.path(d, "topology.yaml"))
  d
}
## deck com o previsto truncado em `keep` (digitos civis do store, como o leitor os le)
deck_forecast <- function(base, keep) {
  d <- withr::local_tempdir("lw-deck-fc-", .local_envir = globalenv())
  file.copy(list.files(base, full.names = TRUE), d, recursive = TRUE)
  for (f in list.files(file.path(d, "weather", "forecast"), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)) {
    tb <- arrow::read_parquet(f, as_data_frame = FALSE)
    arrow::write_parquet(tb[tb$valid_time$as_vector() <= keep, ], f)
  }
  d
}
DECK <- deck_base()
UNTIL3 <- lubridate::force_tz(lubridate::ymd_hm(paste(format(O + 3L), "23:30"), tz = "UTC"), "UTC")
DECK_SHORT <- deck_forecast(DECK, UNTIL3)
DECK_EMPTY <- deck_forecast(DECK, lubridate::ymd_hm("2000-01-01 00:00", tz = "UTC"))

lw_ctx <- function(deck, state)
  list(root = RT, data_dir = deck, topology_path = TOPO, weather_issue_cutoff = "available_until", origin = O,
       state_dir = state, code_commit = "c0ffee", renv_lock_hash = "ab12")
STATE <- withr::local_tempdir("lw-state-")
dir.create(STATE, showWarnings = FALSE)

train <- function(sigla)
  suppressWarnings(suppressMessages(env$prevcarga_train(mem(sigla), AREA, O, lw_ctx(DECK, STATE))))
## a CLI poe as funcoes da fundacao no globalenv (o plugin as le de la); aqui entram so durante a inferencia
infer <- function(sigla, deck) {
  nm <- c("prevcarga_abort", "prevcarga_condition", "prevcarga_warn", "to_semihourly")
  nm <- nm[!vapply(nm, exists, NA, envir = globalenv(), inherits = FALSE)]
  for (n in nm) assign(n, env[[n]], envir = globalenv())
  on.exit(rm(list = nm, envir = globalenv()), add = TRUE)
  warns <- character()
  val <- withCallingHandlers(
    suppressMessages(env$prevcarga_infer_cell(mem(sigla), AREA, O, lw_ctx(deck, STATE))),
    prevcarga_warning = function(w) { warns <<- c(warns, w$id); invokeRestart("muffleWarning") },
    warning = function(w) invokeRestart("muffleWarning"))
  list(res = val, warns = warns)
}
infer_err <- function(sigla, deck)
  tryCatch({ infer(sigla, deck); NULL }, prevcarga_error = function(e) e)

test_that("[e5-ac-0] topologia real: LIN_UNI e LIN_MED sao membros do dessem-linear, nas variantes UNI e MED", {
  expect_true(all(c("LIN_UNI", "LIN_MED") %in% MEMBERS$sigla))
  expect_identical(mem("LIN_UNI")$mdir, "dessem-linear"); expect_identical(mem("LIN_UNI")$exogena, "UNI")
  expect_identical(mem("LIN_MED")$mdir, "dessem-linear"); expect_identical(mem("LIN_MED")$exogena, "MED")
  expect_identical(nrow(MEMBERS), length(unique(MEMBERS$sigla)))
})

test_that("[e5-ac-1] treino dos dois membros na mesma origem e area; artefatos distintos por variante (chave por key_params)", {
  u <- train("LIN_UNI"); m <- train("LIN_MED")
  expect_true(u$status %in% c("trained", "reused")); expect_true(m$status %in% c("trained", "reused"))
  expect_false(identical(u$path, m$path))
  expect_true(grepl("UNI", u$path, fixed = TRUE)); expect_true(grepl("MED", m$path, fixed = TRUE))
})

test_that("[e5-ac-2] FORBIDDEN: LIN_UNI entrega 9 dias sem warning de corte, com previsto curto e com previsto vazio", {
  s <- infer("LIN_UNI", DECK_SHORT); e <- infer("LIN_UNI", DECK_EMPTY)
  for (r in list(s, e)) {
    expect_identical(r$res$delivery_days_efetivo, 9L)
    expect_false("warning.forecast_weather_horizon_cut" %in% r$warns)
    expect_identical(r$res$variant, "UNI")
  }
  ## o previsto nao entra na UNI: mesmo resultado com o previsto curto e com o previsto vazio
  expect_identical(s$res$predictions, e$res$predictions)
  expect_identical(s$res$delivery_days_efetivo, e$res$delivery_days_efetivo)
})

test_that("[e5-ac-3] CONTROL: LIN_MED aborta completude.weather_forecast_short com previsto curto e com previsto vazio", {
  for (deck in list(DECK_SHORT, DECK_EMPTY)) {
    e <- infer_err("LIN_MED", deck)
    expect_false(is.null(e))
    expect_identical(e$id, "completude.weather_forecast_short")
    expect_identical(e$exit_code, 3L)
  }
  ## com o previsto completo a MED infere (o aborto acima e do clima curto, nao de outra causa)
  ok <- infer("LIN_MED", DECK)
  expect_identical(ok$res$delivery_days_efetivo, 9L)
})

test_that("[e5-ac-4] saida ASCII dos erros e identificadores", {
  e <- infer_err("LIN_MED", DECK_EMPTY)
  expect_true(all(utf8ToInt(paste(conditionMessage(e), e$id)) < 128L))
})
