## test-ingestion.R -- LGBM.1: ingestao semi-horaria, conferencia de cortes, safra de clima e feriados.
## Cada gate traz FORBIDDEN (aborta/delta=0) e CONTROL (delta != 0 / passa). Deck sintetico P.0e, area ZZ1 (800 dias).
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
P <- lgbm_source_plugin()
K <- lgbm_consts(P)
SPEC <- lgbm_lib$prevcarga_model_spec("lgbm", lgbm_root())
deck <- lgbm_fx_deck()
ctx <- lgbm_fx_ctx(deck)
cuts <- lgbm_fx_cuts(ctx)
O <- LGBM_O
panel_tr <- lgbm_fx_panel(deck, ctx, "train", cuts)
panel_in <- lgbm_fx_panel(deck, ctx, "infer", cuts)
ingest <- function(panel, cuts_ = cuts, mode, spec = SPEC, consts = K) P$lgbm_ingest(panel, cuts_, spec, mode, consts)
clone <- function(p) { q <- p; for (n in c("load", "weather_forecast", "holidays")) q[[n]] <- data.table::copy(p[[n]]); q }

test_that("[PLUGINS] ingest_cortes: [AC3] cuts: last_data_day = O-3 (data_lag_days = 2 do descriptor)", {
  expect_identical(cuts$last_data_day, lubridate::ymd("2026-09-28"))
  expect_identical(cuts$train_origin, lubridate::ymd("2026-09-29"))
  expect_identical(cuts$tz, LGBM_TZ)
})

test_that("[PLUGINS] ingest_cortes: [AC3] treino: 48 slots/dia, DataHora = interval_start, max(Data) = last_data_day; painel com 09-29 aborta", {
  f <- ingest(panel_tr, mode = "train")
  expect_equal(max(f$Data), cuts$last_data_day)
  expect_identical(lubridate::tz(f$DataHora), cuts$tz)
  expect_true(all(f[, .N, by = Data]$N == 48L))
  expect_identical(f$Hora, lubridate::hour(f$DataHora) * 2L + lubridate::minute(f$DataHora) %/% 30L)
  expect_identical(range(f$Hora), c(0L, 47L))
  expect_false(any(c("timestamp", "interval_end", "interval_start") %in% names(f)))
  expect_false(anyNA(f$CargaGlobal)); expect_false(any(f$is_synthetic))
  ld <- panel_tr$load
  m <- merge(f, ld, by.x = "DataHora", by.y = "interval_start")
  expect_identical(nrow(m), nrow(ld))
  expect_equal(m$CargaGlobal, m$load_mwh)
  expect_equal(f$Minuto, (f$Hora %% 2L) * 30L)
  ## FORBIDDEN: painel com intervalos de 09-29
  e <- fail(ingest(panel_in, mode = "train"))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(c(e$class_name, e$exit_code), c("contract", 2L))
  expect_true(grepl("[prevcarga/contract] lgbm: painel de treino alem de last_data_day=2026-09-28", conditionMessage(e), fixed = TRUE))
  expect_true(grepl("2026-09-29", conditionMessage(e), fixed = TRUE))
  ## CONTROL: data_lag_days = 0 => o MESMO codigo devolve ate 09-30 (o plugin segue cuts, nao um literal)
  cuts0 <- lgbm_fx_cuts(ctx, data_lag_days = c(lgbm = 0L))
  expect_identical(cuts0$last_data_day, lubridate::ymd("2026-09-30"))
  f0 <- ingest(lgbm_fx_panel(deck, ctx, "train", cuts0), cuts0, mode = "train")
  expect_equal(max(f0$Data), lubridate::ymd("2026-09-30"))
})

test_that("[PLUGINS] ingest_cortes: [AC4] infer: O 09:30, Hora 18 real, depois NA sintetico; O+1 sintetico; cortes e completude abortam", {
  f <- ingest(panel_in, mode = "infer")
  expect_identical(f[Data == O, .N], 48L); expect_identical(f[Data == O + 1L, .N], 48L)
  expect_equal(max(f$Data), O + 1L)
  d0 <- f[Data == O]
  expect_false(anyNA(d0[Hora <= 18L, CargaGlobal])); expect_false(any(d0[Hora <= 18L, is_synthetic]))
  expect_true(all(is.na(d0[Hora >= 19L, CargaGlobal]))); expect_true(all(d0[Hora >= 19L, is_synthetic]))
  expect_true(all(is.na(f[Data == O + 1L, CargaGlobal]))); expect_true(all(f[Data == O + 1L, is_synthetic]))
  expect_false(any(f[Data < O, is_synthetic])); expect_false(anyNA(f[Data < O, CargaGlobal]))
  ld9 <- panel_in$load[interval_start == lubridate::ymd_hm("2026-10-01 09:00", tz = LGBM_TZ)]
  expect_equal(d0[Hora == 18L, CargaGlobal], ld9$load_mwh)
  ## FORBIDDEN 1: intervalo alem do corte
  p1 <- clone(panel_in); last <- p1$load[.N]
  last[, `:=`(interval_start = interval_start + lubridate::minutes(30L), interval_end = interval_end + lubridate::minutes(30L))]
  p1$load <- rbind(p1$load, last)
  e1 <- fail(ingest(p1, mode = "infer"))
  expect_identical(c(e1$class_name, e1$exit_code), c("contract", 2L))
  expect_true(grepl("intervalo alem de available_until", conditionMessage(e1), fixed = TRUE))
  expect_true(grepl("10:00:00", conditionMessage(e1), fixed = TRUE))
  ## FORBIDDEN 2: available_until NULL (sem auto-deteccao)
  cn <- cuts; cn["available_until"] <- list(NULL)
  e2 <- fail(ingest(panel_in, cn, mode = "infer"))
  expect_identical(e2$class_name, "contract")
  expect_true(grepl("[prevcarga/contract] lgbm: cuts$available_until ausente", conditionMessage(e2), fixed = TRUE))
  ## FORBIDDEN 3: dia de historia com 47 intervalos => completude (exit 3), nunca preenche
  p3 <- clone(panel_in)
  day <- O - 10L
  drop <- p3$load[lubridate::as_date(interval_start, tz = LGBM_TZ) == day][20L, interval_start]
  p3$load <- p3$load[interval_start != drop]
  e3 <- fail(ingest(p3, mode = "infer"))
  expect_identical(c(e3$class_name, e3$exit_code), c("completude", 3L))
  expect_true(grepl(format(day), conditionMessage(e3), fixed = TRUE))
  ## CONTROL: o painel completo passa e o painel original nao foi mutado
  expect_null(fail(ingest(panel_in, mode = "infer")))
})

## independente da implementacao: candidatos por slot em base R
expected_temp <- function(weather, heat, t, lead, tz_issue = "UTC") {
  w <- weather[weather$area_code == heat & weather$valid_time == t, ]
  vd <- lubridate::as_date(t, tz = LGBM_TZ)
  id <- lubridate::as_date(lubridate::with_tz(w$issue_time, tz_issue))
  w <- w[as.integer(vd - id) >= lead, ]
  if (!nrow(w)) return(NA_real_)
  w$temperature[which.max(as.numeric(w$issue_time))]
}

test_that("[PLUGINS] ingest_safra: [AC5] safra: emissao mais recente por slot, lead 0 no historico e em O, lead 1 em O+1; CONTROL issue_day_tz", {
  wf <- panel_in$weather_forecast
  expect_true(all(wf$issue_time <= cuts$weather_issue_limit))
  area <- LGBM_AREA
  days0 <- seq(O - 4L, O, by = "day")
  t0 <- P$lgbm_weather_vintage(wf, area, days0, 0L, K, cuts$tz)
  t1 <- P$lgbm_weather_vintage(wf, area, O + 1L, 1L, K, cuts$tz)
  expect_identical(nrow(t0), 48L * length(days0)); expect_identical(nrow(t1), 48L)
  expect_identical(lubridate::tz(t0$DataHora), cuts$tz)
  slots <- function(day, idx) lubridate::force_tz(lubridate::as_datetime(day), cuts$tz) + lubridate::minutes(30L * idx)
  for (cs in list(list(day = O - 1L, lead = 0L, t = t0), list(day = O, lead = 0L, t = t0), list(day = O + 1L, lead = 1L, t = t1))) {
    for (k in c(0L, 5L, 18L, 40L, 47L)) {
      tt <- slots(cs$day, k)
      expect_equal(cs$t$Temperatura[cs$t$DataHora == tt], expected_temp(wf, "ZZ1", tt, cs$lead), info = format(tt))
    }
  }
  ## FORBIDDEN: o quadro de ingestao usa lead 0 ate O e lead 1 em O+1 (a safra sai no mesmo DataHora, sem deslocamento)
  f <- ingest(panel_in, mode = "infer")
  expect_equal(f$Temperatura[f$DataHora == slots(O + 1L, 10L)], expected_temp(wf, "ZZ1", slots(O + 1L, 10L), 1L))
  expect_equal(f$Temperatura[f$DataHora == slots(O, 10L)], expected_temp(wf, "ZZ1", slots(O, 10L), 0L))
  ## CONTROL: issue_day_tz = fuso da carga deixa a emissao 00Z de X+1 (21:00 de X) entrar como lead 0 de X
  Kc <- K; Kc$issue_day_tz <- cuts$tz
  d <- O - 3L
  a <- P$lgbm_weather_vintage(wf, area, d, 0L, K, cuts$tz)
  b <- P$lgbm_weather_vintage(wf, area, d, 0L, Kc, cuts$tz)
  expect_identical(a$DataHora, b$DataHora)
  expect_gt(max(abs(a$Temperatura - b$Temperatura)), 0)
  ## FORBIDDEN: emissao alem de weather_issue_limit no painel
  late <- lgbm_lib$prevcarga_read_weather(ctx, "forecast", c(LGBM_AREA, LGBM_HEAT), O - 45L, O + 2L)
  expect_true(any(late$issue_time > cuts$weather_issue_limit))
  p5 <- clone(panel_in); p5$weather_forecast <- late
  e5 <- fail(ingest(p5, mode = "infer"))
  expect_identical(e5$class_name, "contract")
  expect_true(grepl("emissao alem de weather_issue_limit", conditionMessage(e5), fixed = TRUE))
  ## area sem mapeamento de clima => contract (o oraculo so avisava)
  e6 <- fail(P$lgbm_weather_vintage(wf, "ZZ1", days0, 0L, P$plugin_constants(), cuts$tz))
  expect_true(grepl("area ZZ1 sem heat_area", conditionMessage(e6), fixed = TRUE))
})

test_that("[PLUGINS] ingest_contrato: [AC6] requires ausente, timestamp cru e historia curta abortam; estagio despachado (panel, cuts)", {
  p1 <- clone(panel_in); p1$load[, load_mwh := NULL]
  e1 <- fail(ingest(p1, mode = "infer"))
  expect_identical(c(e1$class_name, e1$exit_code), c("contract", 2L))
  expect_true(grepl("[prevcarga/contract] lgbm: requires ausente: load_mwh", conditionMessage(e1), fixed = TRUE))
  p2 <- clone(panel_in); p2$load[, timestamp := interval_start]
  e2 <- fail(ingest(p2, mode = "infer"))
  expect_true(grepl("[prevcarga/contract] lgbm: coluna timestamp fora do leitor", conditionMessage(e2), fixed = TRUE))
  p3 <- lgbm_fx_panel(deck, ctx, "infer", cuts, history_days = 2L)
  e3 <- fail(ingest(p3, mode = "infer"))
  expect_identical(c(e3$class_name, e3$exit_code), c("window", 5L))
  ## treino com historia menor que history_days$train
  p4 <- lgbm_fx_panel(deck, ctx, "train", cuts, history_days = 100L)
  e4 <- fail(ingest(p4, mode = "train"))
  expect_identical(e4$class_name, "window")
  ## fuso errado
  p5 <- clone(panel_in); p5$load[, interval_end := lubridate::with_tz(interval_end, "UTC")]
  expect_identical(fail(ingest(p5, mode = "infer"))$class_name, "contract")
  ## CONTROL: o painel completo do leitor passa e nao e mutado
  keep <- clone(panel_in)
  expect_null(fail(ingest(panel_in, mode = "infer")))
  expect_identical(panel_in$load, keep$load)
  expect_identical(panel_in$weather_forecast, keep$weather_forecast)
  ## estagio despachado: assinatura canonica, modo derivado do painel, area com heat_area no consts padrao
  expect_identical(names(formals(P$plugin_ingest)), c("panel", "cuts"))
  sp_tr <- lgbm_fx_panel(deck, ctx, "train", cuts, area = LGBM_HEAT)
  ft <- P$plugin_ingest(sp_tr, cuts)
  expect_equal(max(ft$Data), cuts$last_data_day)
  fi <- P$plugin_ingest(lgbm_fx_panel(deck, ctx, "infer", cuts, area = LGBM_HEAT), cuts)
  expect_equal(max(fi$Data), O + 1L); expect_true(any(fi$is_synthetic))
  ## area sem heat_area no consts padrao
  expect_true(grepl("sem heat_area", conditionMessage(fail(P$plugin_ingest(panel_tr, cuts))), fixed = TRUE))
  ## codigo do plugin sem env/relogio/conversoes da base
  expect_identical(pc_grep_code(file.path(lgbm_root(), "models", "lgbm", "R"),
                                "Sys\\.(getenv|setenv|time|Date)|as\\.Date\\(|as\\.POSIXct\\(|strptime"), character())
  expect_identical(pc_grep_code(file.path(lgbm_root(), "models", "lgbm", "R"),
                                "to_model_frame|to_display_frame|PREVCARGA_JOB|setwd\\(|<<-|saveRDS\\(|system2?\\("), character())
  expect_identical(pc_grep_code(file.path(lgbm_root(), "models", "lgbm", "R"), "minutes\\(\\s*-?30\\s*\\)|1800"), character())
  ## CONTROL do grep: a isca casa
  tf <- file.path(tempfile("grepctl-")); dir.create(tf); writeLines("x <- Sys.getenv('A')", file.path(tf, "a.R"))
  expect_length(pc_grep_code(tf, "Sys\\.(getenv|setenv|time|Date)"), 1L)
})

test_that("[PLUGINS] ingest_feriados: [AC7] feriados: prevcarga_code >= 1 + lista fixa; holidays ausente aborta; CONTROL remove um feriado", {
  p <- clone(panel_in)
  extra <- data.table::data.table(area_code = LGBM_AREA, date = lubridate::ymd(c("2026-06-10", "2026-10-03", "2026-03-04")),
                                  holiday_type_id = 1L, prevcarga_code = c(1L, 1L, 0L), simplified_code = c(1L, 1L, -1L))
  p$holidays <- rbind(p$holidays[, names(extra), with = FALSE], extra)
  f <- ingest(p, mode = "infer")
  fer <- attr(f, "feriados")
  expect_s3_class(fer, "Date")
  want <- sort(unique(c(p$holidays$date[p$holidays$prevcarga_code >= 1L], K$holidays_fixed)))
  expect_identical(fer, want)
  expect_true(all(lubridate::ymd(c("2026-06-10", "2026-10-03")) %in% fer))     # inclui O+2 (PreFeriado de O+1)
  expect_false(lubridate::ymd("2026-03-04") %in% fer)                          # prevcarga_code 0 / simplified -1
  expect_true(all(K$holidays_fixed %in% fer))
  expect_false(is.unsorted(fer)); expect_false(anyDuplicated(fer) > 0L)
  ## FORBIDDEN: dataset ausente => contract (o oraculo caia em silencio na lista fixa)
  pn <- clone(panel_in); pn["holidays"] <- list(NULL)
  e <- fail(ingest(pn, mode = "infer"))
  expect_identical(c(e$class_name, e$exit_code), c("contract", 2L))
  expect_true(grepl("[prevcarga/contract] lgbm: requires ausente: holidays", conditionMessage(e), fixed = TRUE))
  ## CONTROL: remover um feriado com prevcarga_code = 1 muda o resultado
  pc <- clone(p); pc$holidays <- pc$holidays[date != lubridate::ymd("2026-06-10")]
  expect_false(identical(attr(ingest(pc, mode = "infer"), "feriados"), fer))
})

test_that("[PLUGINS] ingest_efeitos: efeitos: .Random.seed e ambiente global intactos; sem Sys.getenv no plugin", {
  set.seed(7); seed0 <- .Random.seed; g0 <- ls(globalenv(), all.names = TRUE); s0 <- search()
  ingest(panel_in, mode = "infer"); ingest(panel_tr, mode = "train")
  expect_identical(.Random.seed, seed0)
  expect_identical(ls(globalenv(), all.names = TRUE), g0)
  expect_identical(search(), s0)
})
