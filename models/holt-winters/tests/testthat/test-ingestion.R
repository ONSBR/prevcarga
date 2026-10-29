## test-ingestion.R -- gates [PLUGINS] ingest_* e hv_lubridate do plugin Holt-Winters (HW.1; AC 2-5 e 7).

hw_ascii <- function(x) all(as.integer(charToRaw(x)) <= 127L)

test_that("[PLUGINS] ingest_shape: H01 = 00:00-00:30 por interval_start; CONTROL por interval_end reprova", {
  env <- hw_env()
  st <- hw_fx_store(sentinel = TRUE)
  cuts <- hw_fx_cuts(st)
  panel <- hw_fx_panel(st, cuts)
  deck <- env$plugin_ingest(panel, cuts)
  ctv <- deck$CargaTempVer
  expect_identical(names(ctv), env$hw_ctv_cols())
  expect_identical(ncol(ctv), 105L)
  expect_identical(max(ctv$Data), cuts$last_data_day)
  expect_identical(ctv$Data[[1L]], min(lubridate::as_date(panel$load$interval_start, tz = cuts$tz)))
  expect_identical(names(deck$CargaTempMed), c("data", "feriado", "diasemana", "Cmed", "Tmed", "TIPO_FER"))
  ## cada valor cai na coluna Hkk e no dia que codifica
  dec <- hw_sentinel_decode(as.matrix(ctv[, env$hw_slot_cols()]), cuts$tz)
  want <- lubridate::force_tz(lubridate::as_datetime(rep(ctv$Data, 48L)), cuts$tz) +
    lubridate::minutes(30L * rep(0:47, each = nrow(ctv)))
  expect_true(all(dec == want))
  expect_true(all(lubridate::hour(dec[seq_len(nrow(ctv))]) == 0L & lubridate::minute(dec[seq_len(nrow(ctv))]) == 0L))
  ## o clima observado cai por valid_time no mesmo mapeamento
  dect <- hw_sentinel_decode(as.matrix(ctv[, env$hw_temp_cols()]), cuts$tz)
  expect_true(all(dect == want))
  ## CONTROL: agrupar por interval_end desloca a 1a coluna e reprova o sentinela
  hist <- panel$load[lubridate::as_date(interval_start, tz = cuts$tz) <= cuts$last_data_day]
  bad <- env$.hw_wide_by_slot(hist, "interval_end", "load_mwh", "H", cuts$tz)
  decb <- hw_sentinel_decode(as.matrix(bad[, env$hw_slot_cols(), with = FALSE]), cuts$tz)
  wantb <- lubridate::force_tz(lubridate::as_datetime(rep(bad$Data, 48L)), cuts$tz) +
    lubridate::minutes(30L * rep(0:47, each = nrow(bad)))
  expect_false(isTRUE(all(decb == wantb, na.rm = TRUE)))
  expect_false(all(lubridate::hour(decb[seq_len(nrow(bad))]) == 0L & lubridate::minute(decb[seq_len(nrow(bad))]) == 0L,
                   na.rm = TRUE))
})

test_that("[PLUGINS] ingest_shape: tipos, feriados do calendario e dia O fora da CargaTempVer", {
  env <- hw_env()
  st <- hw_fx_store()
  cuts <- hw_fx_cuts(st)
  deck <- env$plugin_ingest(hw_fx_panel(st, cuts), cuts)
  ctv <- deck$CargaTempVer
  expect_s3_class(ctv$Data, "Date")
  expect_type(ctv$Feriado, "integer"); expect_type(ctv$DiaSemana, "integer")
  expect_type(ctv$HV, "integer"); expect_type(ctv$TIPO_FER, "integer")
  expect_true(all(is.na(ctv$FHV15)) && all(is.na(ctv$DHV15)))
  expect_identical(ctv$DiaSemana, as.integer(lubridate::wday(ctv$Data)))
  expect_equal(ctv$CargaMedia, rowMeans(as.matrix(ctv[, env$hw_slot_cols()])))
  ## O-5 e feriado do fixture (flag 12, simplified 1): entra na CargaTempVer com o codigo do calendario
  o5 <- ctv[ctv$Data == cuts$origin_day - 5L, ]
  expect_identical(o5$Feriado, 1L); expect_identical(o5$TIPO_FER, 12L)
  expect_identical(ctv$Feriado[ctv$Data == cuts$origin_day - 4L], 0L)
  expect_false(cuts$origin_day %in% ctv$Data)
  expect_identical(nrow(deck$load_day_o), 0L)
  expect_identical(deck$meta$area, HW_AREA)
  expect_identical(deck$meta$last_data_day, cuts$last_data_day)
  expect_identical(deck$n_steps, 8L)
  expect_false(deck$weather_absent)
  expect_identical(names(deck$TempPre), c("DIA", env$hw_slot_cols(), "FER"))
  expect_identical(names(deck$CDDPre), c("DIA", "TEMP", "TOT", "DS", "TIPO_FER"))
  expect_identical(deck$TempPre$DIA, cuts$origin_day + 0:8)
})

test_that("[PLUGINS] ingest_shape: infer traz a carga do dia O (ate available_until) so em load_day_o", {
  env <- hw_env()
  au <- lubridate::ymd_hm("2024-05-15 09:30", tz = HW_TZ)
  st <- hw_fx_store(load_until = au)
  cuts <- hw_fx_cuts(st)
  expect_identical(cuts$available_until, au)
  deck <- env$plugin_ingest(hw_fx_panel(st, cuts, "infer"), cuts)
  expect_identical(nrow(deck$load_day_o), 19L)
  expect_true(all(deck$load_day_o$interval_end <= au))
  expect_identical(max(deck$CargaTempVer$Data), cuts$last_data_day)
})

test_that("[PLUGINS] ingest_contract: load_mwh ausente, timestamp cru, dia O no treino => contract; O-1 incompleto => completude", {
  env <- hw_env()
  st <- hw_fx_store()
  cuts <- hw_fx_cuts(st)
  panel <- hw_fx_panel(st, cuts)
  msgs <- character()
  chk <- function(e, id, class, exit) {
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, id)
    expect_identical(e$class_name, class)
    expect_identical(e$exit_code, exit)
    expect_match(conditionMessage(e), sprintf("^\\[prevcarga/%s\\]", class))
    msgs <<- c(msgs, conditionMessage(e))
  }
  ## load_mwh ausente
  p1 <- panel; p1$load <- data.table::copy(panel$load)[, load_mwh := NULL]
  e1 <- hw_catch(env$plugin_ingest(p1, cuts))
  chk(e1, "hw.input_column", "contract", 2L); expect_match(conditionMessage(e1), "load_mwh")
  ## timestamp cru
  p2 <- panel; p2$load <- data.table::copy(panel$load)[, timestamp := interval_start]
  chk(hw_catch(env$plugin_ingest(p2, cuts)), "hw.raw_timestamp", "contract", 2L)
  ## dia O na carga do treino (cuts sem available_until)
  cuts_tr <- cuts; cuts_tr$available_until <- NULL
  p3 <- hw_fx_panel(st, cuts, "infer")
  e3 <- hw_catch(env$plugin_ingest(p3, cuts_tr))
  chk(e3, "hw.train_day_o", "contract", 2L); expect_match(conditionMessage(e3), "2024-05-15")
  ## dia O alem de available_until
  cuts_au <- cuts; cuts_au$available_until <- lubridate::ymd_hm("2024-05-15 09:30", tz = HW_TZ)
  chk(hw_catch(env$plugin_ingest(p3, cuts_au)), "hw.train_day_o", "contract", 2L)
  ## O-1 com 47 intervalos (mutador do P.0e)
  d47 <- hw_lib$prevcarga_fixture_drop_slots(st$dir, HW_AREA, st$O - 1L, "12:00", 1L)
  st47 <- modifyList(st, list(dir = d47))
  cuts47 <- hw_fx_cuts(st47)
  e4 <- hw_catch(env$plugin_ingest(hw_fx_panel(st47, cuts47), cuts47))
  chk(e4, "hw.last_day_incomplete", "completude", 3L); expect_match(conditionMessage(e4), "47")
  expect_true(all(vapply(msgs, hw_ascii, NA)))
  ## CONTROL: o painel completo devolve o deck sem erro
  expect_null(hw_catch(env$plugin_ingest(panel, cuts)))
})

test_that("[PLUGINS] ingest_weather: emissao 06Z escolhida com limite 06Z; 18Z => contract; sem emissao => fallback + aviso", {
  env <- hw_env()
  st <- hw_fx_store()
  cuts <- hw_fx_cuts(st)
  O <- cuts$origin_day
  iss <- function(h) lubridate::force_tz(lubridate::as_datetime(O), "UTC") + lubridate::hours(h)
  ctx <- hw_fx_ctx(st)
  fc_all <- hw_lib$prevcarga_read_weather(ctx, "forecast", HW_AREA, O, O + cuts$horizon_days)
  expect_true(any(fc_all$issue_time == iss(18L)) || any(fc_all$issue_time > iss(6L)))
  c06 <- cuts; c06$weather_issue_limit <- iss(6L)
  p06 <- hw_fx_panel(st, c06)
  expect_true(max(p06$weather_forecast$issue_time) == iss(6L))
  deck <- env$plugin_ingest(p06, c06)
  day <- O + 2L
  want06 <- fc_all[issue_time == iss(6L) & lubridate::as_date(valid_time, tz = cuts$tz) == day][order(valid_time)]
  want18 <- fc_all[issue_time == iss(18L) & lubridate::as_date(valid_time, tz = cuts$tz) == day][order(valid_time)]
  expect_identical(nrow(want06), 48L)
  got <- unlist(deck$TempPre[deck$TempPre$DIA == day, env$hw_slot_cols()], use.names = FALSE)
  expect_equal(got, want06$temperature)
  expect_false(isTRUE(all.equal(got, want18$temperature)))
  ## 18Z no painel com limite 06Z => confere e aborta (nunca corta)
  p18 <- p06; p18$weather_forecast <- fc_all[issue_time <= iss(18L)]
  e <- hw_catch(env$plugin_ingest(p18, c06))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$id, "hw.weather_beyond_cut"); expect_identical(e$exit_code, 2L)
  expect_true(hw_ascii(conditionMessage(e)))
  ## sem nenhuma emissao: fallback do oraculo
  p0 <- p06; p0$weather_forecast <- p06$weather_forecast[0L]
  w <- NULL
  deck0 <- withCallingHandlers(env$plugin_ingest(p0, c06),
    prevcarga_warning = function(cnd) { w <<- cnd; invokeRestart("muffleWarning") })
  expect_s3_class(w, "prevcarga_warning")
  expect_identical(w$id, "hw.weather_absent")
  expect_true(hw_ascii(conditionMessage(w)))
  expect_true(deck0$weather_absent)
  expect_identical(deck0$n_steps, cuts$horizon_days)
  expect_true(all(is.na(as.matrix(deck0$TempPre[, env$hw_slot_cols()]))))
  expect_true(all(is.na(deck0$CDDPre$TEMP)) && all(is.na(deck0$CDDPre$TOT)))
  expect_identical(deck0$TempPre$FER[deck0$TempPre$DIA == O + 3L], 1L)    # feriado futuro do calendario
  expect_identical(deck0$TempPre$FER[deck0$TempPre$DIA == O + 4L], 0L)
  expect_identical(deck0$CDDPre$DS, as.integer(lubridate::wday(O + 0:8)))
  ## CONTROL: com a emissao presente o deck nao e fallback
  expect_false(deck$weather_absent)
})

test_that("[PLUGINS] ingest_puro: duas rodadas identical, insensiveis a env/relogio/RNG; 730 dias de historia", {
  env <- hw_env()
  st <- hw_fx_store(days = 760L)
  cuts <- hw_fx_cuts(st)
  panel <- hw_fx_panel(st, cuts, history_days = 730L)
  g0 <- ls(globalenv(), all.names = TRUE)
  seed0 <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
  clean <- env$plugin_ingest(panel, cuts)
  dirty1 <- withr::with_envvar(c(EXOGENA = "MAX", PREVCARGA_DATA_DIR = "/nada"), env$plugin_ingest(panel, cuts))
  dirty2 <- withr::with_envvar(c(EXOGENA = "MAX", PREVCARGA_DATA_DIR = "/nada"), env$plugin_ingest(panel, cuts))
  expect_identical(dirty1, dirty2)
  expect_identical(dirty1, clean)
  expect_identical(ls(globalenv(), all.names = TRUE), g0)
  seed1 <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
  expect_identical(seed1, seed0)
  ctv <- clean$CargaTempVer
  expect_identical(ctv$Data[[1L]], cuts$origin_day - 730L)
  expect_identical(ctv$Data[[nrow(ctv)]], cuts$last_data_day)
  expect_identical(nrow(ctv), 730L)      # o ingest nao completa a janela com dia sintetico
  ## a ingestao nao toca relogio/ambiente/RNG (o set.seed(spec$seed) antes do MO1 e do feature_engineering.R, HW.2)
  ing <- tempfile("hw-ing-"); dir.create(ing)
  file.copy(file.path(hw_root(), "models", "holt-winters", "R", "ingestion.R"), ing)
  expect_length(hw_grep_code(ing, "Sys\\.|set\\.seed|Sys.getenv"), 0L)
})

test_that("[PLUGINS] hv_lubridate: .hw_compute_hv identical a formula do oraculo (2010..2025); sem POSIXct/as.Date/3600", {
  env <- hw_env()
  d <- seq(lubridate::ymd("2010-01-01"), lubridate::ymd("2025-12-31"), by = "day")
  oracle <- function(dias) {
    base <- as.POSIXct(as.character(dias), tz = "UTC") + 6 * 3600
    as.integer(format(lubridate::with_tz(base, "America/Sao_Paulo"), "%Z") == "-02")
  }
  got <- env$.hw_compute_hv(d)
  expect_identical(got, oracle(d))
  expect_true(any(got == 1L))
  expect_true(all(got[d >= lubridate::ymd("2019-02-17")] == 0L))
  ## CONTROL: um deslocamento errado (sem as 6 horas) diverge do oraculo em alguma data
  ctl <- as.integer(lubridate::dst(lubridate::with_tz(lubridate::force_tz(lubridate::as_datetime(d), "UTC"),
                                                      "America/Sao_Paulo")))
  expect_false(identical(ctl, oracle(d)))
  expect_length(hw_grep_code(file.path(hw_root(), "models", "holt-winters", "R"),
                             "as\\.POSIXct|as\\.Date\\(|strptime|3600|Sys\\."), 0L)
})
