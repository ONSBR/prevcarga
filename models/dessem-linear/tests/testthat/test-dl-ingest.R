## test-dl-ingest.R -- DL.5 AC2 (contrato de exogena) e AC3 (deck do motor sem aritmetica de hora).

test_that("ac-2 exogena ausente, 'XYZ' ou 'med' aborta [prevcarga/contract] antes de treinar; nenhum SVM e ajustado", {
  p <- dl_load()
  eng <- parent.env(p$env)
  calls <- new.env(); calls$n <- 0L
  real <- eng$tune_wsvm
  assign("tune_wsvm", function(...) { calls$n <- calls$n + 1L; real(...) }, envir = eng)
  panel <- dl_panel(days = 60L); cuts <- dl_cuts(); m <- dl_model_panel(panel, cuts)
  for (bad in list(NULL, "XYZ", "med", c("MED", "MAX"), NA_character_)) {
    sp <- dl_spec(); sp$params$exogena <- bad
    cond <- expect_dl_error(p$env$plugin_ingest(m$panel, m$cuts, sp), "plugin.dessem_linear_bad_exogena", "params.exogena")
    expect_identical(cond$exit_code, 2L)
    expect_match(conditionMessage(cond), "MED, MAX, MXMN", fixed = TRUE)
    expect_match(conditionMessage(cond), "[prevcarga/contract]", fixed = TRUE)
  }
  ## o estagio feature_engineering (so params) tambem recusa antes de chamar o motor
  expect_dl_error(p$env$plugin_build_features(list(), list(params = list(exogena = "XYZ"))), "plugin.dessem_linear_bad_exogena")
  expect_identical(calls$n, 0L)
  ## CONTROL: MED segue
  deck <- dl_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts, dl_spec("MED", 40L))))
  expect_identical(deck$variant, "MED"); expect_identical(deck$model_key, "Linear.TmpMedia")
  for (ex in c("MAX", "MXMN"))
    expect_identical(dl_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts, dl_spec(ex, 40L))))$model_key, unname(p$env$plugin_constants()$model_key[[ex]]))
})

test_that("ac-3 deck: DataHora como recebido, 24 linhas/dia, O..O+8, flag-12 -> 0, origin_load, tzone de cuts", {
  p <- dl_load()
  cuts <- dl_cuts(); O <- cuts$origin_day
  until <- lubridate::force_tz(lubridate::as_datetime(O), DL_TZ) + lubridate::hours(9L)
  panel <- dl_panel(days = 420L, until = until)
  m <- dl_model_panel(panel, cuts)
  deck <- dl_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts, dl_spec("MED", 400L))))
  ## CARGAHIST: o instante da hora exatamente como recebido (interval_end do frame de modelagem), 24/dia
  hist <- m$panel$load[lubridate::as_date(interval_end) <= O - 1L & lubridate::as_date(interval_end) >= O - 401L]
  expect_identical(deck$CARGAHIST$DataHora, hist$interval_end)
  expect_identical(deck$CARGAHIST$Carga, hist$load_mwh)
  expect_identical(nrow(deck$CARGAHIST), 401L * 24L)               # 400 dias + 1 de aquecimento
  expect_identical(min(lubridate::as_date(deck$CARGAHIST$DataHora)), O - 401L)
  expect_identical(max(lubridate::as_date(deck$CARGAHIST$DataHora)), O - 1L)
  expect_true(all(table(lubridate::as_date(deck$CARGAHIST$DataHora)) == 24L))
  ## o dia O nunca entra em CARGAHIST; origin_load = horas completas de O ate available_until (09:00 de exibicao)
  expect_false(any(lubridate::as_date(deck$CARGAHIST$DataHora) >= O))
  expect_identical(nrow(deck$origin_load), 9L)
  expect_true(all(deck$origin_load$interval_end <= lubridate::floor_date(m$cuts$available_until, "hour")))
  ## EXOGENAHIST: valid_time da hora do to_hourly
  expect_identical(deck$EXOGENAHIST$DataHora, dl_lib$to_hourly(panel)$weather_observed[
    lubridate::as_date(valid_time) <= O - 1L & lubridate::as_date(valid_time) >= O - 401L]$valid_time)
  expect_identical(names(deck$EXOGENAHIST), c("DataHora", "Exo_Temperatura"))
  ## EXOGENAPREV: 216 linhas O..O+8, emissao mais recente por valid_time (a de 06Z soma +100)
  expect_identical(nrow(deck$EXOGENAPREV), 216L)
  expect_identical(range(lubridate::as_date(deck$EXOGENAPREV$DataHora)), c(O, O + 8L))
  expect_gt(min(deck$EXOGENAPREV$Exo_Temperatura), 100)
  ## FERIADOS: 12 -> 0; dataPrev = O; horizon = 9L
  expect_identical(sort(unique(deck$FERIADOS$Tipo)), c(0L, 3L, 4L, 5L, 6L, 9L, 10L))
  expect_identical(sum(deck$FERIADOS$Tipo == 0L), 1L)
  expect_s3_class(deck$FERIADOS$Data, "Date")
  expect_identical(deck$dataPrev, O); expect_identical(deck$horizon, 9L); expect_identical(deck$area, "SE")
  ## tzone de cuts em todo POSIXct
  for (x in list(deck$CARGAHIST$DataHora, deck$EXOGENAHIST$DataHora, deck$EXOGENAPREV$DataHora,
                 deck$origin_load$interval_end, deck$hourly_load$interval_end))
    expect_identical(lubridate::tz(x), DL_TZ)
  ## hourly_load = historico + horas completas de O (base do mm)
  expect_identical(nrow(deck$hourly_load), 401L * 24L + 9L)
})

test_that("ac-3 treino: painel sem o dia O => origin_load vazio; mesmas series", {
  p <- dl_load()
  panel <- dl_panel(days = 60L); cuts <- dl_cuts(); m <- dl_model_panel(panel, cuts)
  deck <- dl_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts, dl_spec("MED", 40L))))
  expect_identical(nrow(deck$origin_load), 0L)
  expect_identical(nrow(deck$CARGAHIST), 41L * 24L)
  ## estagio de 2 formais (despachante): sem spec => janela do proprio painel, variante NA
  d2 <- dl_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  expect_true(is.na(d2$variant))
  expect_identical(nrow(d2$CARGAHIST), 60L * 24L)
})

test_that("ac-3 CONTROL: hora faltante em O-10 => [prevcarga/completude] CARGAHIST <dia> com 23/24 horas", {
  p <- dl_load()
  cuts <- dl_cuts(); O <- cuts$origin_day
  hole <- lubridate::force_tz(lubridate::as_datetime(O - 10L), DL_TZ) + lubridate::hours(10L) + lubridate::minutes(30)
  m <- dl_model_panel(dl_panel(days = 60L, drop = hole), cuts)
  cond <- expect_dl_error(p$env$plugin_ingest(m$panel, m$cuts, dl_spec("MED", 40L)),
                          "plugin.dessem_linear_series_gap", paste("CARGAHIST", format(O - 10L), "com 23/24 horas"))
  expect_identical(cond$exit_code, 3L)
  expect_match(conditionMessage(cond), "[prevcarga/completude]", fixed = TRUE)
})

test_that("ac-3 CONTROL: EXOGENAPREV com 3 dias curtos => completude; 2 dias curtos sao persistidos (cauda)", {
  p <- dl_load()
  cuts <- dl_cuts()
  m3 <- dl_model_panel(dl_panel(days = 60L, fc_short = c(4L, 6L, 8L)), cuts)
  cond <- expect_dl_error(p$env$plugin_ingest(m3$panel, m3$cuts, dl_spec("MED", 40L)),
                          "plugin.dessem_linear_series_gap", "EXOGENAPREV")
  expect_identical(cond$exit_code, 3L)
  m2 <- dl_model_panel(dl_panel(days = 60L, fc_short = c(7L, 8L)), cuts)
  deck <- dl_with_cli(suppressMessages(p$env$plugin_ingest(m2$panel, m2$cuts, dl_spec("MED", 40L))))
  expect_identical(nrow(deck$EXOGENAPREV), 216L)
})

test_that("ac-3 janela menor que history_days.train => [prevcarga/window] (exit 5); FERIADOS vazio => completude", {
  p <- dl_load()
  cuts <- dl_cuts()
  m <- dl_model_panel(dl_panel(days = 60L), cuts)
  cond <- expect_dl_error(p$env$plugin_ingest(m$panel, m$cuts, dl_spec("MED", 100L)), "plugin.dessem_linear_window_short", "history_days.train=100")
  expect_identical(cond$exit_code, 5L)
  m$panel$holidays <- m$panel$holidays[0L]
  cond2 <- expect_dl_error(p$env$plugin_ingest(m$panel, m$cuts, dl_spec("MED", 40L)), "plugin.dessem_linear_series_gap", "FERIADOS")
  expect_identical(cond2$exit_code, 3L)
})

test_that("ac-3 sem aritmetica de hora na ingestao e demais fontes do plugin (grep de fonte)", {
  files <- list.files(file.path(dl_models(), "dessem-linear", "R"), pattern = "[.]R$", full.names = TRUE)
  strip <- function(l) sub("#.*$", "", l)
  pat <- paste0("[+-]\\s*3600|(?<![A-Za-z0-9_.])(hours|minutes|seconds|days|dhours|dminutes)\\(|difftime|with_tz|",
                "as\\.POSIX|force_tz|Sys\\.time|Sys\\.getenv|%m[+-]%")
  hits <- Filter(function(f) any(grepl(pat, strip(readLines(f, warn = FALSE)), perl = TRUE)), files)
  expect_identical(basename(hits), character())
  ## CONTROL: a isca casa
  bait <- tempfile(fileext = ".R"); writeLines("x <- t + 3600", bait)
  expect_true(any(grepl(pat, readLines(bait), perl = TRUE)))
  ## so-definicoes: o loader aceitou (nenhum codigo de topo) e nenhum arquivo fora do conjunto esperado
  expect_true(all(basename(files) %in% c("feature_engineering.R", "freeze.R", "helpers_nowcast.R", "ingestion.R", "register.R", "training.R")))
})

test_that("despachante de treino alcanca ingestao, feature_engineering e o estagio de treino de DL.6 (erro de variante antes do motor)", {
  p <- dl_load()
  panel <- dl_lib$to_hourly(dl_panel(days = 60L)); cuts <- dl_cuts()
  spec <- dl_spec("XYZ", 40L)
  expect_dl_error(dl_lib$prevcarga_plugin_train(p, panel, cuts, spec), "plugin.dessem_linear_bad_exogena")
})
