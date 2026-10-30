## test-rd2-nowcast.R -- RD.2: cabeca de nowcast (helpers_nowcast.R). AC1 rd_morning_means, AC2 rd_dm_nowcast contra o
## wrapper do oraculo (_load_engine.R:300-370, so leitura, em env separado). Loader real, motor real, painel sintetico.

## ---- AC1: mm = media das horas completas <= floor_hour(available_until) --------------------------------------------------
test_that("[RD.2] rd_morning_means: 09:00 = media das 9 horas completas; 09:30 identico; 00:00 vazio; CONTROL de perturbacao", {
  p <- rd2_plugin(); f <- p$env$rd_morning_means
  O <- RD2_O
  pan <- rd_panel(days = 10L, until = rd2_au(12L))
  h <- rd_lib$to_hourly(pan)$load
  m09 <- f(h, rd2_au(9L), RD_TZ)
  expect_identical(names(m09), c("Data", "mm"))
  expect_true(O %in% m09$Data); expect_identical(nrow(m09), 11L)           # 10 dias historicos + O
  ## mm[O] = media das 9 horas cujo rotulo interval_end <= 09:00 (18 intervalos semi-horarios)
  d <- pan$load[lubridate::as_date(interval_start) == O & interval_end <= rd2_au(9L)]
  expect_identical(nrow(d), 18L)
  hh <- data.table::data.table(h)[lubridate::as_date(interval_start) == O & interval_end <= rd2_au(9L)]
  expect_identical(nrow(hh), 9L)
  expect_equal(m09[Data == O]$mm, mean(hh$load_mwh))
  hist_d <- O - 3L
  hh2 <- data.table::data.table(h)[lubridate::as_date(interval_start) == hist_d &
                                                 lubridate::as_date(interval_end) == hist_d & lubridate::hour(interval_end) <= 9L]
  expect_equal(m09[Data == hist_d]$mm, mean(hh2$load_mwh))
  ## 09:30 isolada nao forma hora completa: identico ao de 09:00 (painel cortado em 09:30 tambem)
  h30 <- rd_lib$to_hourly(rd_panel(days = 10L, until = rd2_au(9L) + lubridate::minutes(30L)))$load
  expect_identical(f(h30, rd2_au(9L) + lubridate::minutes(30L), RD_TZ), m09)
  expect_identical(f(h, rd2_au(9L) + lubridate::minutes(30L), RD_TZ), m09)
  ## 00:00 => vazio com o schema
  m00 <- f(h, rd2_au(0L), RD_TZ)
  expect_identical(m00, data.table::data.table(Data = lubridate::as_date(character()), mm = numeric()))
  ## CONTROL: 08:30-09:00 (hora de rotulo 09:00) muda mm[O]; 09:00-09:30 (hora de rotulo 10:00) nao
  pa <- data.table::copy(pan); pa$load[interval_start == rd2_au(8L) + lubridate::minutes(30L), load_mwh := load_mwh + 500]
  expect_gt(abs(f(rd_lib$to_hourly(pa)$load, rd2_au(9L), RD_TZ)[Data == O]$mm - m09[Data == O]$mm), 1)
  pb <- data.table::copy(pan); pb$load[interval_start == rd2_au(9L), load_mwh := load_mwh + 500]
  expect_identical(f(rd_lib$to_hourly(pb)$load, rd2_au(9L), RD_TZ), m09)
})

## ---- AC2: rd_dm_nowcast identico ao wrapper com estado do oraculo -----------------------------------------------------
test_that("[RD.2] rd_dm_nowcast: infer e head_O identicos ao wrapper do oraculo, sem estado global; CONTROL sem mm = motor puro", {
  skip_if_not(file.exists(RD2_ORACLE_ENG), "oraculo ausente")
  p <- rd2_plugin(); eng <- parent.env(p$env); O <- RD2_O
  pan <- rd2_panel(until = rd2_au(12L))
  fe <- rd2_feats(p, pan, 12L)
  h <- rd_lib$to_hourly(pan)$load
  mm <- p$env$rd_morning_means(h, rd2_au(12L), RD_TZ)
  expect_true(O %in% mm$Data)
  cfg <- fe$config_mdl
  before <- list(g = ls(globalenv(), all.names = TRUE), p = ls(p$env, all.names = TRUE), e = ls(eng, all.names = TRUE))
  mine <- suppressWarnings(p$env$rd_dm_nowcast(data.table::copy(fe$series$serieDM), cfg, mm, O, 42L))
  expect_identical(list(g = ls(globalenv(), all.names = TRUE), p = ls(p$env, all.names = TRUE), e = ls(eng, all.names = TRUE)), before)
  expect_setequal(names(mine), c("mdl", "infer", "head_O"))
  expect_true(is.finite(mine$head_O))
  oe <- rd2_oracle_wrapper(eng)
  oe$.rd_nowcast_set(O, mm)
  ora <- suppressWarnings(suppressMessages(oe$execDemandaMediaTS(data.table::copy(fe$series$serieDM), cfg)))
  expect_identical(mine$infer, ora$infer)
  expect_identical(mine$head_O, ora$infer[Data == O]$Carga.infer)
  expect_true(all((O + 0:8) %in% mine$infer$Data))
  ## CONTROL: sem mm delega ao motor, identico ao ARIMA puro
  pure <- suppressWarnings(eng$execDemandaMediaTS(data.table::copy(fe$series$serieDM), cfg))
  nomm <- suppressWarnings(p$env$rd_dm_nowcast(data.table::copy(fe$series$serieDM), cfg, mm[0L], O, 42L))
  expect_identical(nomm$infer, pure$infer)
  expect_null(nomm$head_O)
  ## e a cabeca mexe: a infer com mm difere do ARIMA puro em O
  expect_false(identical(mine$infer[Data == O]$Carga.infer, pure$infer[Data == O]$Carga.infer))
})
