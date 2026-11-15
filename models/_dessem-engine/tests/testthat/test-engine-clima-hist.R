## test-engine-clima-hist.R -- E5-T3: exo_hist_lead0 (EXOGENAHIST em clima_hist = previsto_lead0). Fixture sintetica pequena.
## Sem variaveis de ambiente; datas por lubridate com tz explicito.

HIST_TZ <- "America/Sao_Paulo"

## env do plugin lgbm (so para a igualdade com lgbm_weather_vintage); pai = fundacao, com data.table anexado como no plugin
hist_lgbm_env <- function() {
  e <- new.env(parent = eng_lib)
  d <- file.path(eng_root(), "models", "lgbm", "R")
  f <- list.files(d, pattern = "[.][Rr]$", full.names = TRUE)
  for (x in f[order(basename(f), method = "radix")]) sys.source(x, envir = e, keep.source = FALSE, toplevel.env = e)
  e
}

## emissoes 00Z/06Z/12Z/18Z de cada dia UTC, cada uma com 24 horas validas (valid = dia civil BRT); inclui as emissoes
## 21h-00h BRT (= 00Z do dia UTC seguinte). Temperatura codifica a emissao para distinguir qual venceu.
hist_wf <- function(days, area = "SP", from = days[1L] - 1L) {
  rows <- list()
  for (iu in as.list(seq(from, max(days) + 1L, by = "day"))) for (h in c(0L, 6L, 12L, 18L)) {
    iss <- lubridate::ymd_h(paste(format(iu), h), tz = "UTC")
    for (d in as.list(days)) {
      v <- lubridate::force_tz(lubridate::as_datetime(d), HIST_TZ) + lubridate::hours(0:23)
      rows[[length(rows) + 1L]] <- data.table::data.table(area_code = area, issue_time = iss, valid_time = v,
        temperature = 20 + as.numeric(iss - lubridate::as_datetime(days[1L])) / 3600 / 100 + (seq_along(v) - 1L) / 10)
    }
  }
  data.table::rbindlist(rows)
}
hist_wo <- function(days, area = "SP") {
  data.table::rbindlist(lapply(as.list(days), function(d) {
    v <- lubridate::force_tz(lubridate::as_datetime(d), HIST_TZ) + lubridate::hours(0:23)
    data.table::data.table(area_code = area, valid_time = v, temperature = 100 + as.numeric(d - days[1L]) + (0:23) / 100)
  }))
}
hist_eng <- function() eng_env(eng_load(eng_plugin_root()))

test_that("ac-0 lead 0 igual ao lgbm_weather_vintage na virada do dia UTC, dias_completados 0", {
  days <- lubridate::ymd("2026-10-01") + 0:6
  ## o lead 0 so aceita valid_day >= issue_day (UTC): emissoes de 00Z do dia seguinte (21h BRT) sao descartadas para as horas 21-23
  wf <- hist_wf(days); wo <- hist_wo(days)
  eng <- hist_eng()
  got <- eng$exo_hist_lead0(wf, wo, "SP", days, tz = HIST_TZ)
  ref <- hist_lgbm_env()$lgbm_weather_vintage(wf, "SP", days, lead = 0L, consts = list(heat_area = c(SP = "SP"), issue_day_tz = "UTC"), tz = HIST_TZ)
  expect_identical(got$dt$DataHora, ref$DataHora)
  expect_identical(got$dt$Exo_Temperatura, ref$Temperatura)
  expect_identical(got$dias_completados, 0L)
  expect_identical(got$primeiro_dia_lead0, min(days))
  expect_equal(nrow(got$dt), 24L * length(days))
})

test_that("ac-1 buraco antes de 2018-12-23 e dia com 23 horas vem inteiros do observado (contagem 4)", {
  days <- lubridate::ymd("2018-12-20") + 0:6
  wf <- hist_wf(days[days >= lubridate::ymd("2018-12-23")], from = lubridate::ymd("2018-12-22"))
  d24 <- lubridate::ymd("2018-12-24")
  wf <- wf[!(lubridate::as_date(valid_time, tz = HIST_TZ) == d24 & lubridate::hour(valid_time) == 5L)]
  wo <- hist_wo(days)
  got <- hist_eng()$exo_hist_lead0(wf, wo, "SP", days, tz = HIST_TZ)
  expect_identical(got$dias_completados, 4L)
  expect_identical(got$primeiro_dia_lead0, lubridate::ymd("2018-12-23"))
  expect_equal(nrow(got$dt), 24L * length(days))
  dd <- lubridate::as_date(got$dt$DataHora, tz = HIST_TZ)
  for (d in as.list(c(days[1:3], d24))) {
    sub <- got$dt[dd == d]
    ref <- wo[lubridate::as_date(valid_time, tz = HIST_TZ) == d]
    expect_equal(nrow(sub), 24L)
    expect_identical(sub$Exo_Temperatura, ref$temperature)
  }
  expect_true(all(got$dt[dd %in% days[c(4, 6, 7)]]$Exo_Temperatura < 100))
})

test_that("ac-2 dia sem fonte aborta engine.dessem_exo_gap; previsto vazio usa so o observado", {
  days <- lubridate::ymd("2026-10-01") + 0:2
  wf <- hist_wf(days); wo <- hist_wo(days)
  miss <- days[2L]
  wo2 <- wo[lubridate::as_date(valid_time, tz = HIST_TZ) != miss | lubridate::hour(valid_time) != 3L]
  wf2 <- wf[lubridate::as_date(valid_time, tz = HIST_TZ) != miss | lubridate::hour(valid_time) != 3L]
  eng <- hist_eng()
  err <- tryCatch(eng$exo_hist_lead0(wf2, wo2, "SP", days, tz = HIST_TZ), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "engine.dessem_exo_gap", fixed = TRUE)
  ## com a fundacao no globalenv do despachante o abort e classificado (completude) e traz details$dia
  captured <- NULL
  old <- globalenv()$prevcarga_abort
  assign("prevcarga_abort", function(id, where = NULL, details = NULL) { captured <<- list(id = id, where = where, details = details); stop("x") }, envir = globalenv())
  on.exit(if (is.null(old)) rm("prevcarga_abort", envir = globalenv()) else assign("prevcarga_abort", old, envir = globalenv()), add = TRUE)
  try(eng$exo_hist_lead0(wf2, wo2, "SP", days, tz = HIST_TZ), silent = TRUE)
  expect_identical(captured$id, "engine.dessem_exo_gap")
  expect_identical(captured$details$dia, format(miss))
  expect_equal(captured$details$horas_observado, 23L)
  ## previsto vazio
  got <- eng$exo_hist_lead0(wf[0L], wo, "SP", days, tz = HIST_TZ)
  expect_identical(got$dias_completados, 3L)
  expect_true(is.na(got$primeiro_dia_lead0))
  expect_equal(nrow(got$dt), 72L)
  expect_identical(got$dt$Exo_Temperatura, wo$temperature)
})

test_that("manifest do motor exporta exo_hist_lead0 e o catalogo tem engine.dessem_exo_gap", {
  expect_true("exo_hist_lead0" %in% eng_env(eng_load(eng_plugin_root()))$engine_manifest()$required_functions)
  tsv <- utils::read.delim(file.path(eng_root(), "scripts", "lib", "errors", "dessem_engine.tsv"), stringsAsFactors = FALSE)
  expect_identical(tsv$class[tsv$id == "engine.dessem_exo_gap"], "completude")
})
