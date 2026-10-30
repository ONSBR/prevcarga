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

## ---- DL.4: golden do motor isolado (plugin_golden.R --model _dessem-engine) ---------------------------------------
## O oraculo (somente leitura) so aparece como GOLDEN em `.state/golden` (gerado por scripts/tools/plugin_golden.R, nunca
## versionado; so o `golden.lock` e versionado). Sem variaveis de ambiente; datas por lubridate com tz explicito.
ENG_TOL <- 1e-5                                  # MW absoluto (ADR-0017)
ENG_TZ <- "Etc/GMT+3"
ENG_ORACLE_RSCRIPT <- "/opt/R/4.4.3/bin/Rscript" # runtime do oraculo (WeightSVM/e1071 na biblioteca do oracle_env)
eng_parity_dir <- function() file.path(eng_dir(), "tests", "parity")
eng_golden_root <- function() file.path(eng_root(), ".state", "golden")

## tier do gate: o do runner de gates quando ele esta carregado; senao `options(prevcarga.tier = "parity")`; senao fast
eng_tier <- function() {
  if (exists(".pc_gates_state", mode = "environment")) return(get(".pc_gates_state")$tier)
  t <- getOption("prevcarga.tier", "fast")
  if (t %in% c("parity", "parity-full")) "parity" else "fast"
}
eng_require_parity <- function() {
  if (!identical(eng_tier(), "parity")) testthat::skip("[tier] gate so roda no tier parity (atual: fast)")
  invisible(TRUE)
}

## unidades de units.yaml do grupo (engine | robustez | all); cada uma com `model_key` derivado da variante
eng_variant_key <- function(v) c(MED = "Linear.TmpMedia", MAX = "Linear.TmpMaxima", MXMN = "Linear.TmpMaxMin")[[v]]
eng_units <- function(group = "all", path = file.path(eng_parity_dir(), "units.yaml")) {
  doc <- yaml::read_yaml(path)
  ids <- if (identical(group, "all")) vapply(doc$units, `[[`, "", "id") else unlist(doc$groups[[group]])
  us <- Filter(function(u) u$id %in% ids, doc$units)
  lapply(us[match(ids, vapply(us, `[[`, "", "id"))], function(u) {
    u$model_key <- eng_variant_key(u$variant); u$group <- if (u$id %in% unlist(doc$groups$engine)) "engine" else "robustez"; u
  })
}
eng_unit_dir <- function(u, root = eng_golden_root())
  file.path(root, "plugins", "_dessem-engine", u$variant, u$area, u$origin)

## NULL (ok) ou o texto ASCII da falha pedindo regerar: lock ausente/sem a unidade, arquivo do golden ausente, sha256 divergente
## do lock ou fingerprint do runtime do oraculo (R, plataforma, BLAS, pacotes) diferente do gravado no lock.
eng_check_golden <- function(u, root = eng_golden_root(), lock = file.path(eng_parity_dir(), "golden.lock"),
                             rscript = ENG_ORACLE_RSCRIPT) {
  regen <- "regere o golden: Rscript --vanilla scripts/tools/plugin_golden.R --model _dessem-engine --units engine|robustez --oracle <clone limpo do oraculo> --data data --rscript <R do oraculo>"
  if (!file.exists(lock)) return(sprintf("golden.lock ausente (%s)", regen))
  lk <- jsonlite::read_json(lock, simplifyVector = FALSE)
  e <- lk$units[[u$id]]
  if (is.null(e)) return(sprintf("unidade %s ausente do golden.lock (%s)", u$id, regen))
  d <- eng_unit_dir(u, root)
  for (f in names(e$files)) {
    p <- file.path(d, f)
    if (!file.exists(p)) return(sprintf("unidade %s: %s ausente (%s)", u$id, f, regen))
    if (!identical(digest::digest(p, algo = "sha256", file = TRUE), e$files[[f]]))
      return(sprintf("unidade %s: sha256 de %s difere do golden.lock (%s)", u$id, f, regen))
  }
  if (!file.exists(rscript)) return(sprintf("unidade %s: R do oraculo ausente (%s); fingerprint nao conferivel", u$id, rscript))
  inv <- jsonlite::read_json(file.path(d, "invocation.json"), simplifyVector = TRUE)
  tools_e <- new.env(parent = globalenv())
  sys.source(file.path(eng_root(), "scripts", "tools", "plugin_golden.R"), envir = tools_e)
  fp <- tools_e$pc_runtime_fingerprint(names(e$fingerprint$packages), rscript, as.character(inv$env))
  for (k in c("R", "platform", "blas"))
    if (!identical(fp[[k]], e$fingerprint[[k]])) return(sprintf("unidade %s: fingerprint %s difere do lock (%s)", u$id, k, regen))
  for (p in names(e$fingerprint$packages))
    if (!identical(fp$packages[[p]], e$fingerprint$packages[[p]]))
      return(sprintf("unidade %s: fingerprint do pacote %s difere do lock (%s)", u$id, p, regen))
  NULL
}

eng_golden <- function(u) {
  msg <- eng_check_golden(u)
  if (!is.null(msg)) { testthat::fail(msg); return(NULL) }
  readRDS(file.path(eng_unit_dir(u), "native.rds"))
}

## O deck CRU do oraculo (rotulos naive-BRT em UTC) no frame que o despachante entrega ao motor: relabel do fuso por
## force_tz (instante de parede preservado) e `to_model_frame` da carga com `offset_min` (60 no DESSEM; 0 e o CONTROL).
eng_golden_deck <- function(g, offset_min = 60L, tz = ENG_TZ) {
  d <- g$deck[c("CARGAHIST", "EXOGENAHIST", "EXOGENAPREV", "FERIADOS")]
  for (n in c("CARGAHIST", "EXOGENAHIST", "EXOGENAPREV")) {
    d[[n]] <- data.table::copy(d[[n]])
    data.table::set(d[[n]], j = "DataHora", value = lubridate::force_tz(d[[n]]$DataHora, tz))
  }
  eng_model_deck(d, offset_min)
}

## Roda o motor clean-room (env `eng`, ja carregado) sobre o deck do golden como o despachante o faz: `to_model_frame` na
## entrada, TransformaDeck, `set.seed(seed)` (NULL = sem semente: CONTROL), processaModelosDMPerfil, desagregaModeloSH, e na
## saida semi-horaria `to_display_frame` + carimbo de 30 min (o motor nao desloca hora). `hourly` fica no frame de modelagem (e o
## que `processaModelosDMPerfil` entrega e o oraculo grava); `semihourly` no de exibicao (end-of-interval).
eng_run <- function(eng, g, model_key = g$model_key, seed = 42L, offset_min = 60L, tz = ENG_TZ) {
  dk <- suppressMessages(eng$TransformaDeck(eng_golden_deck(g, offset_min, tz), format(g$dataPrev), "10DIAS", tz = tz))
  cfg <- eng$getConfigModel(model_key)
  if (is.null(seed)) set.seed(NULL) else set.seed(as.integer(seed))   # NULL: gerador reinicializado do relogio (CONTROL sem semente)
  guards <- character()
  hor <- withCallingHandlers(eng$processaModelosDMPerfil(dk, cfg), message = function(m) {
    if (inherits(m, "dessem_engine_guard")) guards <<- c(guards, m$code)
    invokeRestart("muffleMessage")
  })
  sh <- suppressMessages(eng$desagregaModeloSH(model_key, data.table::copy(hor)[, Modelo := model_key]))
  hourly <- data.table::copy(hor)          # o horario do oraculo e o do frame de modelagem (rotulo = inicio da hora)
  semi <- data.table::data.table(DataHora = eng_semi_stamp(sh, offset_min), Passo = sh$Passo, Carga = sh$Carga)
  list(hourly = hourly, semihourly = semi, seriecomp = dk$SERIECOMP, diasprev = dk$DIASPREV, guards = guards)
}

## rotulo de parede (texto) para comparar o frame do oraculo (UTC naive) com o do clean-room (Etc/GMT+3)
eng_wall <- function(x) format(x, "%Y-%m-%d %H:%M:%S")
eng_maxdiff <- function(a, b) max(abs(a$Carga - b$Carga))

## cache de rodadas do clean-room na sessao do test_dir (cada rodada e ~minutos): chave = unidade + offset + semente
.eng_runs <- new.env(parent = emptyenv())
eng_run_cached <- function(eng, g, u, seed = 42L, offset_min = 60L) {
  key <- paste(u$id, if (is.null(seed)) "noseed" else seed, offset_min)
  if (is.null(.eng_runs[[key]])) .eng_runs[[key]] <- eng_run(eng, g, u$model_key, seed, offset_min)
  .eng_runs[[key]]
}

## Serie de carga de TransformaDeck (Prev == FALSE) do clean-room contra a do oraculo (`proc$SERIECOMP`), por rotulo de parede:
## max|delta Carga| e n de linhas casadas. Com o deslocamento do despachante (offset 60) e 0; sem ele a carga fica 1 h defasada.
eng_series_diff <- function(eng, g, offset_min = 60L, tz = ENG_TZ) {
  dk <- suppressMessages(eng$TransformaDeck(eng_golden_deck(g, offset_min, tz), format(g$dataPrev), "10DIAS", tz = tz))
  a <- dk$SERIECOMP[Prev == FALSE]; b <- g$proc$SERIECOMP[Prev == FALSE]
  m <- merge(data.table::data.table(k = eng_wall(a$DataHora), a = a$Carga), data.table::data.table(k = eng_wall(b$DataHora), b = b$Carga), by = "k")
  list(n = nrow(m), max = max(abs(m$a - m$b), na.rm = TRUE))
}
