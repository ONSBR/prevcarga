## test-engine-feriados-exogena.R -- DL.2: normalize_special_day_codes (flag-12) e fill_forecast_tail (AC2, AC3 do ticket).

fx_engine <- function() eng_env(eng_load(eng_plugin_root()))

test_that("ac-3 codigo 12 vira 0 com mensagem ASCII; as demais linhas ficam identicas", {
  eng <- fx_engine()
  fer <- data.table::data.table(Data = lubridate::ymd(c("2024-12-22", "2024-12-25", "2024-11-15")), Tipo = c(12L, 6L, 5L))
  orig <- data.table::copy(fer)
  msgs <- character()
  out <- withCallingHandlers(eng$normalize_special_day_codes(fer, "Tipo", 0:11, "SE"),
                             message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_identical(out$Tipo, c(0L, 6L, 5L))
  expect_identical(out$Data, fer$Data)
  expect_true(any(grepl("nao-modelado (12)", msgs, fixed = TRUE)))
  expect_false(any(grepl("[^\\x01-\\x7F]", msgs, perl = TRUE)))
  expect_identical(fer, orig)                      # a entrada nao e mutada por referencia
})

test_that("ac-3 codigos so em 0..11 => NO-OP identico; NULL/vazio passa; nada le ENV", {
  eng <- fx_engine()
  fer <- data.table::data.table(Data = lubridate::ymd(c("2024-12-25", "2024-11-15")), Tipo = c(6L, 0L))
  expect_identical(eng$normalize_special_day_codes(fer, "Tipo", 0:11, "SE"), fer)
  expect_null(eng$normalize_special_day_codes(NULL))
  empty <- fer[0L]
  expect_identical(eng$normalize_special_day_codes(empty), empty)
  bad <- data.table::data.table(Data = lubridate::ymd("2024-12-22"), Tipo = 12L)
  a <- suppressMessages(eng$normalize_special_day_codes(bad))
  old <- Sys.getenv("DESSEM_NORMALIZE_CODES", NA_character_)
  Sys.setenv(DESSEM_NORMALIZE_CODES = "0")
  on.exit(if (is.na(old)) Sys.unsetenv("DESSEM_NORMALIZE_CODES") else Sys.setenv(DESSEM_NORMALIZE_CODES = old), add = TRUE)
  b <- suppressMessages(eng$normalize_special_day_codes(bad))
  expect_identical(a, b)
  expect_identical(b$Tipo, 0L)
})

fx_prev <- function(short = integer(), days = 9L, keep = 5L, tz = "Etc/GMT+3") {
  O <- lubridate::force_tz(lubridate::as_datetime(lubridate::ymd("2025-06-20")), tz)
  hrs <- lapply(seq_len(days) - 1L, function(i) {
    n <- if (i %in% short) keep else 24L
    O + lubridate::days(i) + lubridate::hours(seq_len(n) - 1L)
  })
  t <- do.call(c, hrs)
  data.table::data.table(DataHora = t, Exo_Temperatura = as.numeric(seq_along(t)) %% 17 + 20)
}

test_that("ac-3 cauda curta de 2 dias: cada dia recebe o ultimo completo anterior (216 linhas) com mensagem", {
  eng <- fx_engine()
  O <- lubridate::ymd("2025-06-20")
  dt <- fx_prev(short = 7:8)
  expect_lt(nrow(dt), 216L)
  orig <- data.table::copy(dt)
  msgs <- character()
  out <- withCallingHandlers(eng$fill_forecast_tail(dt, "DataHora", O, O + 8, 24L, "SE", O, max_fill_days = 2L),
                             message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_identical(nrow(out), 216L)
  expect_true(all(is.finite(out$Exo_Temperatura)))
  d6 <- out[lubridate::date(DataHora) == O + 6]
  for (k in 7:8) {
    dk <- out[lubridate::date(DataHora) == O + k]
    expect_identical(nrow(dk), 24L)
    expect_identical(dk$Exo_Temperatura, d6$Exo_Temperatura)
    expect_identical(dk$DataHora, d6$DataHora + lubridate::days(k - 6L))
  }
  expect_length(msgs, 2L)
  expect_true(all(grepl("persistido do ultimo dia completo", msgs, fixed = TRUE)))
  expect_false(any(grepl("[^\\x01-\\x7F]", msgs, perl = TRUE)))
  expect_identical(dt, orig)                       # a entrada nao e mutada
})

test_that("ac-3 completa => NO-OP identico; 3 dias curtos ou sem doador => inalterada; ENV nao desliga", {
  eng <- fx_engine()
  O <- lubridate::ymd("2025-06-20")
  full <- fx_prev()
  expect_identical(eng$fill_forecast_tail(full, "DataHora", O, O + 8, 24L, "SE", O), full)
  three <- fx_prev(short = 6:8)
  expect_identical(eng$fill_forecast_tail(three, "DataHora", O, O + 8, 24L, "SE", O, max_fill_days = 2L), three)
  nodonor <- fx_prev(short = 0:8)
  expect_identical(eng$fill_forecast_tail(nodonor, "DataHora", O, O + 8, 24L, "SE", O, max_fill_days = 99L), nodonor)
  edge <- fx_prev(short = 0L)                     # dia curto de borda inicial: sem completo antes
  expect_identical(eng$fill_forecast_tail(edge, "DataHora", O, O + 8, 24L, "SE", O), edge)
  old <- Sys.getenv("DESSEM_EXOG_TAIL_FILL", NA_character_)
  Sys.setenv(DESSEM_EXOG_TAIL_FILL = "0")
  on.exit(if (is.na(old)) Sys.unsetenv("DESSEM_EXOG_TAIL_FILL") else Sys.setenv(DESSEM_EXOG_TAIL_FILL = old), add = TRUE)
  out <- suppressMessages(eng$fill_forecast_tail(fx_prev(short = 7:8), "DataHora", O, O + 8, 24L, "SE", O))
  expect_identical(nrow(out), 216L)
})
