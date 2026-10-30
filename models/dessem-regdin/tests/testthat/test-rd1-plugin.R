## test-rd1-plugin.R -- RD.1: contrato/registro Tier A, run.yaml, ingestao, conferencia do painel, helpers de serie,
## equivalencia de rotulos com o oraculo e cauda de exogena. Loader real, motor real, painel sintetico por to_hourly +
## .prevcarga_plugin_to_model (nunca dado real do store). Os testes de contrato completos do plugin ficam no RD.4.

rd_oracle_fit <- "/opt/ons-source/prevcargadessem3/models/dessem-regdin/R/training/dessem_regdin_fit.R"

## ---- AC1: contrato e registro Tier A ------------------------------------------------------------------------------
test_that("[PLUGINS] descritor: carrega Tier A com o descriptor do contrato, motor distinto do dessem-linear e sem simbolo no globalenv", {
  before <- ls(globalenv(), all.names = TRUE)
  lin <- rd_load("dessem-linear")
  rd <- rd_lib$prevcarga_plugin_load("dessem-regdin", rd_models())
  d <- rd$descriptor
  expect_identical(d$delivery_days, 9L)
  expect_identical(d$model_time_offset_min, 60L)
  expect_true(d$history_infer_equals_train)
  expect_identical(d$engine, "_dessem-engine")
  expect_null(d$rng_deviation)
  expect_identical(d$params_schema$exogena$allowed, c("MED", "MAX"))
  expect_null(d$params_schema$exogena$default)
  expect_identical(d$variant_param, "exogena")
  expect_setequal(names(d$stages), c("ingestion", "feature_engineering", "training", "inference_features",
                                      "inference", "forecast"))
  reg <- names(rd_lib$prevcarga_registry())
  expect_true(all(paste0("dessem-regdin/", names(d$stages)) %in% reg))
  expect_false(identical(parent.env(rd$env), parent.env(lin$env)))
  expect_setequal(setdiff(ls(globalenv(), all.names = TRUE), ".Random.seed"), setdiff(before, ".Random.seed"))
})

test_that("[PLUGINS] descritor: CONTROL descriptor sem o estagio inference aborta contract nomeando o estagio", {
  root <- rd_fixture_root()
  f <- file.path(root, "dessem-regdin", "R", "register.R")
  txt <- readLines(f)
  i <- grep("^      inference = list\\(fn = rd_pick\\(\"rd_inference\"", txt)
  j <- i + which(grepl("provides = \"forecast.y\"", txt[i:length(txt)]))[[1L]] - 1L
  expect_length(i, 1L)
  writeLines(txt[-(i:j)], f)
  cond <- tryCatch(rd_load("dessem-regdin", root), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.bad_descriptor")
  expect_match(conditionMessage(cond), "stages$inference", fixed = TRUE)
})

## ---- AC2: run.yaml invalido aborta no loader (nada de estagio roda) ---------------------------------------------------
test_that("[PLUGINS] descritor: run.yaml de fixture invalido aborta contract nomeando o campo; MED e MAX carregam", {
  cases <- list(
    list(sub = function(x) sub("infer: 1825", "infer: 45", x), id = "plugin.history_window", field = "history_days.infer"),
    list(sub = function(x) sub("granularity_min: 60", "granularity_min: 30", x), id = "plugin.bad_run_yaml", field = "granularity_min"),
    list(sub = function(x) x[!grepl("^seed:", x)], id = "plugin.bad_run_yaml", field = "seed"),
    list(sub = function(x) sub("exogena: MED", "exogena: MXMN", x), id = "plugin.bad_run_yaml", field = "params$exogena"),
    ## origin_agnostic com modo forecast: o loader recusa a combinacao (nomeia `modes`)
    list(sub = function(x) sub("split: origin_scoped", "split: origin_agnostic", x), id = "plugin.bad_run_yaml", field = "modes"))
  for (cs in cases) {
    root <- rd_fixture_root(cs$sub)
    cond <- tryCatch(rd_load("dessem-regdin", root), prevcarga_error = function(e) e)
    expect_s3_class(cond, "prevcarga_error")
    expect_identical(cond$id, cs$id)
    expect_match(conditionMessage(cond), cs$field, fixed = TRUE)
    expect_identical(cond$exit_code, 2L)
  }
  for (e in c("MED", "MAX")) {
    p <- rd_load("dessem-regdin", rd_fixture_root(function(x) sub("exogena: MED", paste0("exogena: ", e), x)))
    expect_identical(p$env$plugin_constants()$model_key[[e]],
                     c(MED = "RegDin.Radial.TmpMed", MAX = "RegDin.Radial.TmpMax")[[e]])
    expect_identical(p$env$plugin_variant_meta(list(exogena = e))$member, paste0("RGD_", e))
  }
  p <- rd_load()
  expect_rd_error(p$env$.rd_variant_key(list(exogena = "MXMN")), "plugin.regdin_bad_exogena", "MXMN")
  expect_rd_error(p$env$.rd_variant_key(list()), "plugin.regdin_bad_exogena", "ausente")
})

test_that("[PLUGINS] descritor: plugin_required_functions existem todas no env do motor do regdin", {
  p <- rd_load()
  miss <- setdiff(p$env$plugin_required_functions(), ls(parent.env(p$env), all.names = TRUE))
  expect_identical(miss, character())
})

## ---- AC3: ingestao e feriados ---------------------------------------------------------------------------------------
test_that("[PLUGINS] ingestao: plugin_ingest monta o deck; feriado flag-12 vira Tipo 1 e simplified_code 0 vira Tipo 0 antes do motor", {
  p <- rd_load()
  cu <- rd_cuts(); m <- rd_model_panel(rd_panel(), cu)
  deck <- rd_with_cli(p$env$plugin_ingest(m$panel, m$cuts, rd_spec()))
  expect_true(all(c("CARGAHIST", "EXOGENAHIST", "EXOGENAPREV", "FERIADOS") %in% names(deck)))
  expect_true("Exo_Temperatura" %in% names(deck$EXOGENAPREV))
  fer <- deck$FERIADOS
  expect_identical(fer[Data == lubridate::ymd("2024-12-24")]$Tipo, 1L)
  expect_identical(fer[Data == lubridate::ymd("2024-12-18")]$Tipo, 0L)
  expect_identical(deck$dataPrev, cu$origin_day)
  f <- rd_with_cli(p$env$plugin_build_features(deck, rd_spec()))
  expect_length(f$dias, 10L)                      # o motor roda horizon_days + 1 dias (O..O+9), como o oraculo (RD.6)
  expect_identical(f$dias, format(cu$origin_day + 0:9))
  expect_identical(unname(f$day_types[["2024-12-24"]]), 1L)
  ## CONTROL: sem o pre-mapeamento o normalizador do motor leva o flag-12 a 0 (o dia deixa de ser especial)
  raw <- data.table::data.table(Data = lubridate::ymd("2024-12-24"), Tipo = 12L)
  off <- suppressMessages(parent.env(p$env)$normalize_special_day_codes(raw, area = "SE"))
  expect_identical(off$Tipo, 0L)
  expect_false(identical(off$Tipo, fer[Data == lubridate::ymd("2024-12-24")]$Tipo))
})

## ---- AC4: conferencia do painel (confere, nunca corta) -------------------------------------------------------------
test_that("[PLUGINS] ingestao: painel invalido aborta com a classe certa; painel valido passa sem aviso", {
  p <- rd_load()
  ingest <- function(pn, cu, spec = rd_spec()) {
    m <- rd_model_panel(pn, cu)
    rd_with_cli(p$env$plugin_ingest(m$panel, m$cuts, spec))
  }
  ## (a) carga de O alem do available_until
  O <- lubridate::ymd("2024-12-23")
  full_o <- rd_panel(until = lubridate::force_tz(lubridate::as_datetime(O + 1L), RD_TZ))
  c1 <- expect_rd_error(ingest(full_o, rd_cuts()), "plugin.regdin_train_panel_has_origin")
  expect_identical(c1$exit_code, 2L)
  ## (b) dia historico com 23 horas (hora 5 de O-3 sem os dois meios-intervalos)
  pn <- rd_panel()
  dia <- lubridate::force_tz(lubridate::as_datetime(O - 3L), RD_TZ) + lubridate::hours(5L)
  pn$load <- pn$load[!(interval_start %in% (dia + lubridate::minutes(c(0L, 30L))))]
  c2 <- expect_rd_error(ingest(pn, rd_cuts()), "plugin.regdin_series_gap", "2024-12-20")
  expect_identical(c2$exit_code, 3L)
  ## (c) emissao de clima depois do weather_issue_limit
  cu <- rd_cuts(); cu$weather_issue_limit <- lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(3L)
  c3 <- expect_rd_error(ingest(rd_panel(), cu), "plugin.regdin_panel_contract", "issue_time")
  expect_identical(c3$exit_code, 2L)
  ## (d) historia menor que history_days.train
  c4 <- expect_rd_error(ingest(rd_panel(), rd_cuts(), rd_spec(train_days = 1800L)), "plugin.regdin_history_short", "dias faltantes")
  expect_identical(c4$exit_code, 5L)
  ## (e) coluna obrigatoria ausente
  pn <- rd_panel(); pn$weather_forecast$temperature <- NULL
  expect_rd_error(ingest(pn, rd_cuts()), "plugin.regdin_panel_contract", "temperature")
  ## CONTROL: painel valido passa sem aviso
  expect_no_warning(ingest(rd_panel(), rd_cuts()))
})

test_that("[PLUGINS] descritor: ids plugin.regdin_* constam em errors/dessem_regdin.tsv e carregam pelo loader do catalogo", {
  f <- file.path(rd_root(), "scripts", "lib", "errors", "dessem_regdin.tsv")
  tsv <- utils::read.delim(f, sep = "\t", colClasses = "character", quote = "", comment.char = "")
  expect_setequal(c("plugin.regdin_train_panel_has_origin", "plugin.regdin_history_short", "plugin.regdin_window_mismatch",
                    "plugin.regdin_arima_drift", "plugin.regdin_bad_exogena", "plugin.regdin_panel_contract",
                    "plugin.regdin_series_gap", "warning.regdin_desagrega_fallback", "plugin.regdin_artifact_tampered",
                    "warning.regdin_rng_deviation", "plugin.regdin_artifact_mismatch"), tsv$id)
  cat <- rd_lib$prevcarga_errors_load(file.path(rd_root(), "scripts", "lib", "errors"))
  expect_true(all(tsv$id %in% cat$id))
  expect_false(anyDuplicated(cat$id) > 0L)
  src <- unlist(lapply(list.files(file.path(rd_models(), "dessem-regdin", "R"), full.names = TRUE), readLines))
  used <- unique(unlist(regmatches(src, gregexpr("(plugin|warning)\\.regdin_[a-z_]+", src))))
  expect_true(all(used %in% tsv$id))
})

## ---- AC5: helpers de serie identicos aos do oraculo ------------------------------------------------------------------
test_that("[PLUGINS] ingestao: .rd_build_series/.rd_day_types/.rd_enforce_real_history identicos aos do oraculo; CONTROL de chave", {
  skip_if_not(file.exists(rd_oracle_fit), "oraculo ausente")
  p <- rd_load()
  cu <- rd_cuts(); m <- rd_model_panel(rd_panel(), cu)
  deck <- rd_with_cli(p$env$plugin_ingest(m$panel, m$cuts, rd_spec()))
  f <- rd_with_cli(p$env$plugin_build_features(deck, rd_spec()))
  proc <- f$processado
  ## oraculo: so as 3 definicoes, num env separado cujo pai e o env do motor (mesmos simbolos de serie)
  oe <- new.env(parent = parent.env(p$env))
  for (e in parse(rd_oracle_fit, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1L]]) == "<-" && as.character(e[[2L]]) %in%
        c(".rd_build_series", ".rd_enforce_real_history", ".rd_day_types")) eval(e, oe)
  cfg <- f$config_mdl
  mine <- p$env$.rd_build_series(data.table::copy(proc$SERIECOMP), cfg)
  ora <- oe$.rd_build_series(data.table::copy(proc$SERIECOMP), cfg)
  expect_identical(mine$serieDM, ora$serieDM)
  expect_identical(mine$seriePF, ora$seriePF)
  expect_identical(p$env$.rd_day_types(mine$seriePF, f$dias), oe$.rd_day_types(ora$seriePF, f$dias))
  expect_identical(p$env$.rd_enforce_real_history(data.table::copy(proc$SERIECOMP)),
                   oe$.rd_enforce_real_history(data.table::copy(proc$SERIECOMP)))
  expect_identical(p$env$.rd_enforce_real_history(data.table::copy(proc$SERIECOMP)), 0L)
  ## com vazamento (Prev == TRUE com Carga) as duas zeram e contam igual
  leak <- data.table::copy(proc$SERIECOMP); leak[Prev == TRUE, Carga := 1]
  expect_identical(p$env$.rd_enforce_real_history(data.table::copy(leak)), oe$.rd_enforce_real_history(data.table::copy(leak)))
  expect_gt(p$env$.rd_enforce_real_history(data.table::copy(leak)), 0L)
  ## CONTROL: trocar a chave de normalizacao do perfil faz seriePF divergir
  cfg2 <- cfg; cfg2$normalizacao$perfil$carga$oper <- cfg$normalizacao$dm$carga$oper
  expect_false(identical(p$env$.rd_build_series(data.table::copy(proc$SERIECOMP), cfg2)$seriePF, mine$seriePF))
})

## ---- AC6: rotulos de hora iguais aos do oraculo, sem aritmetica de hora no plugin --------------------------------------
test_that("[PLUGINS] ingestao: CARGAHIST/EXOGENAHIST/EXOGENAPREV iguais ao adapter do oraculo no periodo comum (sentinela)", {
  p <- rd_load()
  pn <- rd_panel(); cu <- rd_cuts()
  ## sentinela: cada intervalo codifica o proprio interval_start (segundos desde a epoca / 1800)
  pn$load[, load_mwh := as.numeric(interval_start) / 1800]
  pn$weather_observed[, temperature := as.numeric(valid_time) / 1800]
  m <- rd_model_panel(pn, cu)
  deck <- rd_with_cli(p$env$plugin_ingest(m$panel, m$cuts, rd_spec()))
  ## oraculo (data_adapter.R:60-105 + -1h do TransformaDeck), emulado sobre o mesmo store: hora de interval_start,
  ## media da hora, rotulo +3600 no adapter e -1 h no motor => rotulo = inicio da hora
  oracle <- function(dt, tcol, vcol) {
    d <- data.table::copy(dt)
    d[, h := lubridate::floor_date(get(tcol), "hour")]
    d <- d[, .(v = mean(get(vcol)), n = .N), by = h][n == 2L]
    d[, .(DataHora = h + 3600 - 3600, v)][order(DataHora)]
  }
  o_load <- oracle(pn$load[lubridate::as_date(interval_start) < cu$origin_day], "interval_start", "load_mwh")
  expect_identical(deck$CARGAHIST$DataHora, o_load$DataHora)
  expect_equal(deck$CARGAHIST$Carga, o_load$v)
  o_exo <- oracle(pn$weather_observed[lubridate::as_date(valid_time) < cu$origin_day], "valid_time", "temperature")
  expect_identical(deck$EXOGENAHIST$DataHora, o_exo$DataHora)
  expect_equal(deck$EXOGENAHIST$Exo_Temperatura, o_exo$v)
  ## CONTROL 1: agrupar por interval_end desloca 1 hora e reprova a igualdade
  pe <- data.table::copy(pn$load); pe[, interval_start := interval_end]
  e_end <- oracle(pe[lubridate::as_date(interval_start) < cu$origin_day], "interval_start", "load_mwh")
  expect_false(isTRUE(all.equal(e_end$v, o_load$v)))
  ## CONTROL 2: o mesmo painel com offset 0 (sem o despachante) diverge 1 hora
  m0 <- rd_model_panel(pn, cu, offset = 0L)
  deck0 <- tryCatch(rd_with_cli(p$env$plugin_ingest(m0$panel, m0$cuts, rd_spec())), prevcarga_error = function(e) NULL)
  ## sem o deslocamento o rotulo e o fim da hora: ou a conferencia recusa (hora de borda) ou o rotulo diverge
  expect_true(is.null(deck0) || !identical(deck0$CARGAHIST$DataHora, o_load$DataHora))
})

## ---- AC7: cauda curta de exogena -------------------------------------------------------------------------------------
test_that("[PLUGINS] ingestao: cauda curta de ate 2 dias e completada pela persistencia do motor; lacuna maior aborta", {
  p <- rd_load()
  O <- lubridate::ymd("2024-12-23")
  run <- function(short) {
    cu <- rd_cuts(); m <- rd_model_panel(rd_panel(fc_short = short), cu)
    rd_with_cli(p$env$plugin_ingest(m$panel, m$cuts, rd_spec()))
  }
  d2 <- suppressMessages(run(8:9))
  dd <- lubridate::as_date(d2$EXOGENAPREV$DataHora)
  expect_setequal(unique(dd), O + 0:9)
  expect_false(anyNA(d2$EXOGENAPREV$Exo_Temperatura))
  expect_true(all(table(dd) == 24L))
  cond <- expect_rd_error(suppressMessages(run(5:9)), "plugin.regdin_panel_contract", "2024-12-28")
  expect_identical(cond$exit_code, 2L)
  ## CONTROL: cobertura completa => persistencia e no-op (a saida e o proprio painel horario)
  cu <- rd_cuts(); m <- rd_model_panel(rd_panel(), cu)
  out <- p$env$.rd_exo(m$panel$weather_forecast, "prev", m$cuts, 10L, "SE")
  wf <- data.table::as.data.table(m$panel$weather_forecast)
  data.table::setorder(wf, valid_time, -issue_time); wf <- wf[, .SD[1L], by = valid_time]
  ref <- data.table::data.table(DataHora = wf$valid_time, Exo_Temperatura = wf$temperature)[order(DataHora)]
  expect_identical(out, ref)
})

## ---- higiene do codigo do plugin -------------------------------------------------------------------------------------
test_that("[PLUGINS] grep: RD.1 zero hits dos padroes proibidos em models/dessem-regdin/R (divida de globalenv() pinada)", {
  rdir <- file.path(rd_models(), "dessem-regdin", "R")
  files <- list.files(rdir, pattern = "[.]R$", full.names = TRUE)
  pats <- c(as_date = "(?<![A-Za-z0-9_.])as\\.Date\\(", as_posixct = "as\\.POSIXct|as\\.POSIXlt", strptime = "strptime",
            difftime = "difftime", sys_time = "Sys\\.time", sys_date = "Sys\\.Date", sys_getenv = "Sys\\.getenv|Sys\\.setenv",
            system_call = "(?<![A-Za-z0-9_.])system2?\\(", mclapply = "mclapply|mcparallel", super_assign = "<<-",
            hours_1 = "(?<![A-Za-z0-9_.])d?hours\\(\\s*1\\s*\\)", hours_empty = "(?<![A-Za-z0-9_.])d?hours\\(\\s*\\)",
            minutes_30 = "(?<![A-Za-z0-9_.])d?minutes\\(\\s*-?30\\s*\\)", secs_3600 = "(?<![0-9.])3600L?(?![0-9])",
            to_model = "to_model_frame\\(|to_display_frame\\(", library = "(^|[^.])library\\(|(^|[^.])source\\(")
  for (nm in names(pats)) {
    hits <- unlist(lapply(files, function(f) {
      ln <- readLines(f, warn = FALSE); ln <- sub("#.*$", "", ln)
      i <- which(grepl(pats[[nm]], ln, perl = TRUE))
      if (length(i)) paste0(basename(f), ":", i) else character()
    }))
    expect_identical(if (is.null(hits)) character() else hits, character(), info = nm)
  }
  ## so ASCII
  for (f in files) expect_false(any(grepl("[\\x80-\\xFF]", readLines(f, warn = FALSE, encoding = "bytes"), perl = TRUE, useBytes = TRUE)), info = f)
  ## divida interina do globalenv (mesmo padrao do dessem-linear, DL8-GAP-2): conjunto exato = rd_abort, rd_warn e rd_foundation (RD.3)
  g <- unlist(lapply(files, function(f) {
    ln <- sub("#.*$", "", readLines(f, warn = FALSE)); i <- which(grepl("globalenv\\(\\)", ln))
    if (length(i)) paste0(basename(f), ":", trimws(ln[i])) else character()
  }))
  expect_length(g, 3L)
  expect_true(all(grepl("^register.R:.*get0\\(\"prevcarga_(condition|warn)\"", g) |
                  grepl("^inference.R:.*get0\\(name, envir = globalenv\\(\\)", g)))   # RD.3: rd_foundation (to_semihourly)
})
