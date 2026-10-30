## helper-engine.R -- fixtures dos testes do motor _dessem-engine (DL.1). Auto-carregado por test_dir.
## Nao e codigo de produto. Sem variaveis de ambiente; datas por lubridate com tz explicito.
suppressWarnings(suppressMessages(library(testthat)))

eng_root <- function() {
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(d, "scripts", "lib", "runtime.R"))) return(d)
    up <- dirname(d)
    if (identical(up, d)) stop("eng_root: scripts/lib/runtime.R nao encontrado")
    d <- up
  }
}
eng_dir <- function() file.path(eng_root(), "models", "_dessem-engine")

options(prevcarga.lib = file.path(eng_root(), "scripts", "lib"))
## fundacao (loader de plugins) carregada num env de teste, nunca no globalenv
eng_lib <- local({
  e <- new.env(parent = globalenv())
  for (m in c("errors", "topology", "deck_reader", "validation", "registry", "features"))
    sys.source(file.path(eng_root(), "scripts", "lib", paste0(m, ".R")), envir = e)
  e
})
eng_globalenv_snapshot <- function() list(globalenv = ls(globalenv(), all.names = TRUE), search = search())

.ENG_ATTACH <- c("data.table", "lubridate", "jsonlite", "rlist", "WeightSVM", "e1071", "stats", "utils")

## Escreve o plugin-descriptor `name` (copiado do toy-60 da fundacao, com engine = "_dessem-engine") em `root`.
eng_write_plugin <- function(root, name) {
  toy <- file.path(eng_root(), "scripts", "lib", "tests", "fixtures", "plugins", "toy-60")
  dir.create(file.path(root, name, "R"), recursive = TRUE)
  writeLines(gsub("toy-60", name, readLines(file.path(toy, "run.yaml")), fixed = TRUE), file.path(root, name, "run.yaml"))
  pl <- gsub("toy-60", name, readLines(file.path(toy, "R", "plugin.R")), fixed = TRUE)
  pl <- gsub("_toy-engine", "_dessem-engine", pl, fixed = TRUE)
  pl <- sub("history_infer_equals_train = TRUE,", paste0("history_infer_equals_train = TRUE, attach = ",
            paste(deparse(.ENG_ATTACH), collapse = ""), ","), pl, fixed = TRUE)
  writeLines(pl, file.path(root, name, "R", "plugin.R"))
  invisible(root)
}

## Raiz temporaria com a copia do motor + plugin(s)-descriptor. `manifest`: modifyList no engine_manifest
## (R/manifest.R reescrito); `engine_files`: nome -> linhas gravadas em R/ da copia; `drop_files`: remove de R/.
eng_plugin_root <- function(name = "dessem-fix", manifest = list(), engine_files = list(), drop_files = character()) {
  root <- tempfile("engroot-"); dir.create(root)
  file.copy(eng_dir(), root, recursive = TRUE)
  for (n in name) eng_write_plugin(root, n)
  rd <- file.path(root, "_dessem-engine", "R")
  if (length(manifest)) {
    e <- new.env(); sys.source(file.path(rd, "manifest.R"), envir = e)
    m <- modifyList(e$engine_manifest(), manifest)
    writeLines(c("engine_manifest <- function()", paste0("  ", paste(deparse(m), collapse = "\n  "))),
               file.path(rd, "manifest.R"))
  }
  for (f in names(engine_files)) writeLines(engine_files[[f]], file.path(rd, f))
  for (f in drop_files) unlink(file.path(rd, f))
  root
}

## env do motor (pai do env do plugin) de um plugin carregado
eng_env <- function(p) parent.env(p$env)

eng_load <- function(root, name = "dessem-fix") {
  eng_lib$prevcarga_plugin_unload_all()
  eng_lib$prevcarga_registry_reset()
  eng_lib$prevcarga_plugin_load(name, root)
}

## Deck sintetico deterministico (sem RNG): horario, 2016-03-01 .. 2019-02-27 (cobre 3 horarios de verao
## e dias especiais). Origem O = 2019-02-28; exogena prevista so ate O+2 => horizonte 10DIAS ajustado a 3 dias.
eng_deck <- function(origin = "2019-02-28", from = "2016-03-01", prev_days = 3L, tz = "Etc/GMT+3") {
  O <- lubridate::ymd(origin); F0 <- lubridate::ymd(from)
  hrs <- function(d0, d1) lubridate::force_tz(lubridate::as_datetime(d0), tz) +
    lubridate::hours(seq.int(0L, as.integer(d1 - d0) * 24L - 1L))
  hist_start <- hrs(F0, O)                       # inicio de cada hora historica
  fer <- data.table::data.table(
    Data = lubridate::ymd(c(outer(2016:2019, c("-01-01", "-09-07", "-10-12", "-11-02", "-11-15", "-12-25"), paste0))),
    Tipo = rep(c(10L, 3L, 4L, 5L, 6L, 9L), each = 4L))[Data < O]
  hh <- lubridate::hour(hist_start); dd <- as.numeric(lubridate::as_date(hist_start) - F0)
  temp <- function(d, h) 24 + 5 * sin(2 * pi * d / 365.25) + 4 * sin(2 * pi * (h - 9) / 24) + 0.5 * sin(d * 1.7)
  carga <- function(d, h, t, wd, isfer)
    9000 + 600 * sin(2 * pi * (h - 6) / 24) + 700 * sin(2 * pi * (h - 14) / 12) + 40 * (t - 24) * (1 + 0.3 * cos(2 * pi * h / 24)) -
      900 * (wd %in% c(1, 7)) - 1100 * isfer + 25 * sin(d * 0.37 + h)
  isfer <- lubridate::as_date(hist_start) %in% fer$Data
  wd <- lubridate::wday(hist_start)
  th <- temp(dd, hh)
  ch <- data.table::data.table(DataHora = hist_start + lubridate::hours(1), Carga = carga(dd, hh, th, wd, isfer))
  eh <- data.table::data.table(DataHora = hist_start, Exo_Temperatura = th)
  pstart <- lubridate::force_tz(lubridate::as_datetime(O), tz) + lubridate::hours(seq.int(0L, prev_days * 24L - 1L))
  dp <- as.numeric(lubridate::as_date(pstart) - F0)
  ep <- data.table::data.table(DataHora = pstart, Exo_Temperatura = temp(dp, lubridate::hour(pstart)) + 0.4)
  list(CARGAHIST = ch, EXOGENAHIST = eh, EXOGENAPREV = ep, FERIADOS = fer)
}

## Deck no frame de modelagem: o que o despachante entrega ao motor (to_model_frame da carga, 60 min; o clima
## nao e deslocado). `offset_min = 0L` e o CONTROL: motor alimentado sem o deslocamento do despachante.
eng_model_deck <- function(deck, offset_min = 60L) {
  d <- deck
  d$CARGAHIST <- eng_lib$to_model_frame(deck$CARGAHIST, offset_min, "DataHora")
  d
}

## Semi-horario como o despachante entrega: to_display_frame do DataHora (ancora) + carimbo de 30 min do
## to_semihourly (meia hora anterior a ancora nos Passos impares). `semi` = saida de desagregaModeloSH.
eng_semi_stamp <- function(semi, offset_min = 60L) {
  anchor <- eng_lib$to_display_frame(data.table::data.table(DataHora = semi$DataHora), offset_min, "DataHora")$DataHora
  anchor - lubridate::minutes(ifelse(semi$Passo %% 2L == 1L, 30L, 0L))
}
