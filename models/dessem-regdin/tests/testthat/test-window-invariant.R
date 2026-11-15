## test-window-invariant.R -- RD.4 (AC2, AC3): invariante de janela do infer e deriva do ARIMA. E4-T3 (ADR-045 fase C): a janela
## deixou de ser deck == train_window; o deck (curto ou longo) so precisa TERMINAR em train_window$end e o historico vem do
## artefato (serie_dm). FORBIDDEN/CONTROL de cada um; os dois bracos rodam, sem skip. Loader real, motor real, despachante real,
## painel sintetico cru. Os artefatos MED/MAX sao os do cache de sessao (rd3_state; corte de treino 09:00).

test_that("[PLUGINS] janela_infer_igual_treino: FORBIDDEN painel exato (fingerprint bate, infer == forecast no dia D)", {
  st <- rd3_state(); p <- st$p; cuts <- rd_fx_cuts()
  ok <- rd_infer(st$med, cuts = cuts)
  expect_identical(dim(ok$values), c(9L, 48L)); expect_false(anyNA(ok$values))
  ## o fingerprint bate: o refit do ARIMA sobre a janela exata do infer reproduz ordem, coeficientes e digest do treino
  s <- rd3_stage_inf(p, st$med, rd_fx_panel(until = cuts$available_until), cuts$available_until)
  expect_identical(s$inf$panel_window$start, st$med$payload$train_window$start)
  expect_identical(s$inf$panel_window$end, st$med$payload$train_window$end)
  expect_true(rd_with_cli(suppressMessages(is.list(p$env$.rd_check_fingerprint(
    s$inf$features$series$serieDM, st$med$payload$config_mdl, st$med$payload$arima_fingerprint)))))
  ## infer == forecast acoplado no dia D
  fc <- rd_forecast(cuts = cuts)
  dD <- max(abs(ok$values[1L, ] - fc$values[1L, ]), na.rm = TRUE)
  cat(sprintf("\n[RD.4 medida] infer x forecast no dia D: max|Delta| = %.3g MW\n", dD))
  expect_lte(dD, 1e-5)
  expect_lte(rd_fx_ydelta(ok, fc), 1e-5)
})

test_that("[PLUGINS] janela_infer_igual_treino: painel 30 dias mais curto no inicio passa com delta 0 (E4-T3); deck que termina em O-2 => window exit 5 antes de ajustar; CONTROL bypass entrega", {
  st <- rd3_state(); p <- st$p; cuts <- rd_fx_cuts(); au <- cuts$available_until; O <- RD2_O
  ok <- rd_infer(st$med, cuts = cuts)
  ## o historico vem do artefato: 30 dias a menos no INICIO do deck nao mudam nada (antes: window_mismatch)
  short <- rd_infer(st$med, rd_fx_shorten(until = au, days = 30L), cuts)
  expect_identical(short$values, ok$values)
  expect_identical(short$delivery$y_pred, ok$delivery$y_pred)
  ## deck valido que termina em O-2: aborta nomeando os dois dias, sem ajustar nada
  pan <- rd_fx_shorten(until = au, days = 30L)
  cu <- rd3_cuts(au); cu$last_data_day <- O - 2L
  pan$load <- pan$load[lubridate::as_date(interval_end - 1) <= O - 2L | lubridate::as_date(interval_start) >= O]
  pan$weather_observed <- pan$weather_observed[lubridate::as_date(valid_time) <= O - 2L | lubridate::as_date(valid_time) >= O]
  m <- rd_model_panel(pan, cu)
  frame <- rd_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  inf <- rd_with_cli(suppressMessages(p$descriptor$stages$inference_features$fn(frame, st$med$payload)))
  spy_pass <- rd_spy(".rd_predict_pass"); spy_fp <- rd_spy(".rd_check_fingerprint")
  cond <- tryCatch(rd_with_cli(p$env$plugin_infer(st$med$payload, inf, m$cuts)), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_window_mismatch")
  expect_identical(cond$exit_code, 5L); expect_identical(cond$class_name, "window")
  expect_true(rd_ascii(conditionMessage(cond)))
  expect_identical(cond$details$expected_end, format(O - 1L)); expect_identical(cond$details$deck_end, format(O - 2L))
  expect_identical(spy_pass$count(), 0L); expect_identical(spy_fp$count(), 0L)    # nenhum ajuste, nem o refit do ARIMA
  spy_pass$restore(); spy_fp$restore()
  ## CONTROL com dente: com o bypass (so teste; ausente do descriptor) o mesmo deck passa da guarda
  ctl <- tryCatch(rd_with_cli(suppressMessages(p$env$plugin_infer(st$med$payload, inf, m$cuts, .bypass_window_guard = TRUE))),
                  prevcarga_error = function(e) e)
  expect_false(inherits(ctl, "prevcarga_error") && identical(ctl$id, "plugin.regdin_window_mismatch"))
  expect_false(".bypass_window_guard" %in% names(formals(p$descriptor$stages$inference$fn)))
  expect_false(".bypass_window_guard" %in% names(p$descriptor))
})

test_that("[PLUGINS] arima_drift: coeficiente +1e-3, ordem e digest adulterados abortam nomeando o campo; CONTROL original infere", {
  st <- rd3_state(); cuts <- rd_fx_cuts()
  expect_true(is.list(rd_infer(st$med, cuts = cuts)))                       # CONTROL: o artefato original infere
  fp <- st$med$payload$arima_fingerprint
  nm <- names(fp$coef)[2L]
  bad <- st$med; bad$payload$arima_fingerprint$coef[[nm]] <- bad$payload$arima_fingerprint$coef[[nm]] + 1e-3
  cond <- tryCatch(rd_infer(bad, cuts = cuts), prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_arima_drift"); expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L)
  expect_identical(cond$details$field, "coef"); expect_identical(cond$details$coef, nm)
  expect_match(conditionMessage(cond), nm, fixed = TRUE)                       # o nome do coeficiente na mensagem
  expect_true(rd_ascii(conditionMessage(cond)))
  swap <- st$med; swap$payload$arima_fingerprint$order[["p"]] <- swap$payload$arima_fingerprint$order[["p"]] + 1L
  c2 <- tryCatch(rd_infer(swap, cuts = cuts), prevcarga_error = function(e) e)
  expect_identical(c2$id, "plugin.regdin_arima_drift"); expect_identical(c2$details$field, "order")
  ## dm_digest adulterado: a guarda de janela (E4-T3) confere serie_dm contra o digest ANTES do ARIMA => artifact_tampered (exit 9)
  dig <- st$med; dig$payload$arima_fingerprint$dm_digest <- "0"
  c3 <- tryCatch(rd_infer(dig, cuts = cuts), prevcarga_error = function(e) e)
  expect_identical(c3$id, "plugin.regdin_artifact_tampered"); expect_identical(c3$exit_code, 9L)
  ## o campo dm_digest do refit continua nomeado quando a serie composta difere do digest (conferencia direta)
  p <- st$p; au <- cuts$available_until
  s <- rd3_stage_inf(p, st$med, rd_fx_panel(until = au), au)
  c4 <- tryCatch(rd_with_cli(p$env$.rd_check_fingerprint(s$inf$features$series$serieDM, st$med$payload$config_mdl, dig$payload$arima_fingerprint)),
                 prevcarga_error = function(e) e)
  expect_identical(c4$id, "plugin.regdin_arima_drift"); expect_identical(c4$details$field, "dm_digest")
})

test_that("[PLUGINS] arima_drift: mable NULL do fable (serie sem os 12 meses / regressores colineares) aborta arima_drift nomeando a causa", {
  st <- rd3_state(); p <- st$p; au <- rd_fx_cuts()$available_until
  ## E4-T3: o historico vem do artefato, entao o NULL model so surge de uma serie composta curta (artefato adulterado/outra fonte):
  ## recorta a serieDM composta para os ultimos 100 dias historicos (menos de 12 meses)
  full <- rd3_stage_inf(p, st$med, rd_fx_panel(until = au), au)
  sdm <- data.table::copy(full$inf$features$series$serieDM)
  ult <- max(sdm[Prev == FALSE]$Data)
  short <- sdm[Prev == TRUE | Data > ult - 100L]
  eng <- parent.env(p$env)
  raw <- suppressWarnings(eng$execDemandaMediaTS(data.table::copy(short), st$med$payload$config_mdl))
  expect_true(inherits(raw$mdl$modeloTreinado[[1L]][[1L]]$fit, "null_mdl"))   # premissa: o motor nao avisa nem erra
  cond <- tryCatch(rd_with_cli(p$env$.rd_check_fingerprint(short, st$med$payload$config_mdl, st$med$payload$arima_fingerprint)),
                   prevcarga_error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_arima_drift"); expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$details$field, "null_model")
  expect_match(conditionMessage(cond), "NULL", fixed = TRUE)
  expect_match(conditionMessage(cond), "12 meses", fixed = TRUE); expect_match(conditionMessage(cond), "colineares", fixed = TRUE)
  expect_true(rd_ascii(conditionMessage(cond)))
  ## CONTROL: a serie composta inteira (14 meses) nao produz NULL model
  ok <- suppressWarnings(eng$execDemandaMediaTS(data.table::copy(sdm), st$med$payload$config_mdl))
  expect_false(inherits(ok$mdl$modeloTreinado[[1L]][[1L]]$fit, "null_mdl"))
})

## =====================================================================================================================
## E3-T4 (ADR-046): janela 1095 dias, anexo de feriados por ilha e guarda de janela com o conjunto de ilhas.
## Fixtures sinteticas em memoria: painel JA horario e no frame de modelagem (como o despachante entrega ao plugin).
## =====================================================================================================================
rdw_O <- lubridate::ymd("2023-06-01")
rdw_area <- "AA"
rdw_hours <- function(d0, d1) {
  a <- lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(d0)), RD_TZ)
  seq(a, by = "hour", length.out = (as.integer(lubridate::as_date(d1) - lubridate::as_date(d0)) + 1L) * 24L)
}
rdw_load <- function(dh, plus = 0) {
  d <- as.numeric(lubridate::as_date(dh) - lubridate::ymd("2015-01-01")); h <- lubridate::hour(dh)
  8000 + plus + 500 * sin(2 * pi * h / 24) + 30 * (d %% 7) + 3 * sin(d * 1.3 + h)
}
rdw_temp <- function(dh, plus = 0) 22 + plus + 4 * sin(2 * pi * lubridate::hour(dh) / 24) + 0.2 * (as.numeric(lubridate::as_date(dh)) %% 5)
rdw_islands <- data.table::data.table(
  area_code = rdw_area, island_id = 1:2,
  from = lubridate::ymd(c("2019-04-12", "2019-12-18")), to = lubridate::ymd(c("2019-04-19", "2019-12-25")),
  holiday_dates = list(lubridate::ymd("2019-04-19"), lubridate::ymd("2019-12-25")))

rdw_panel <- function(O = rdw_O, from_day = O - 1095L, annex = "islands", drop_hour = NULL, na_hour = NULL) {
  hc <- rdw_hours(from_day, O - 1L)
  ld <- data.table::data.table(area_code = rdw_area, interval_start = hc, interval_end = hc, load_mwh = rdw_load(hc))
  wo <- data.table::data.table(area_code = rdw_area, valid_time = hc, temperature = rdw_temp(hc))
  fh <- rdw_hours(O, O + 10L)
  wf <- data.table::data.table(area_code = rdw_area, issue_time = lubridate::force_tz(lubridate::as_datetime(O), "UTC"),
                               valid_time = fh, temperature = rdw_temp(fh))
  hol <- data.table::data.table(area_code = rdw_area, date = O - c(10L, 200L), holiday_type_id = c(10L, 3L), simplified_code = 3L)
  panel <- list(load = ld, weather_observed = wo, weather_forecast = wf, holidays = hol,
                attrs = list(origin = O, load_label = "end", tz = RD_TZ))
  if (!is.null(annex)) {
    isl <- if (identical(annex, "islands")) rdw_islands else rdw_islands[0L]
    hf <- if (nrow(isl)) do.call(c, lapply(seq_len(nrow(isl)), function(i) rdw_hours(isl$from[[i]], isl$to[[i]]))) else hc[0L]
    lf <- data.table::data.table(area_code = rdw_area, interval_start = hf, interval_end = hf, load_mwh = rdw_load(hf, 5000))
    if (!is.null(na_hour)) lf[interval_end == na_hour, load_mwh := NA_real_]
    if (!is.null(drop_hour)) hf2 <- hf[!(hf %in% drop_hour)] else hf2 <- hf
    if (!is.null(drop_hour)) lf <- lf[interval_end %in% hf2]
    panel$holiday_annex <- list(islands = isl, load = lf,
      weather_observed = data.table::data.table(area_code = rdw_area, valid_time = hf2, temperature = rdw_temp(hf2, 10)))
  }
  panel
}
rdw_spec <- function(janela = "ano_civil") { s <- rd_spec("MED", 1095L); s$params$janela_dessem <- janela; s }
rdw_ingest <- function(panel, janela = "ano_civil") {
  p <- rd_load()
  rd_with_cli(suppressMessages(p$env$plugin_ingest(panel, rd_cuts(format(rdw_O)), rdw_spec(janela))))
}

test_that("[PLUGINS] janela_infer_igual_treino: descriptor, run.yaml e history_window do regdin nos dois modos (E3-T4)", {
  p <- rd_load()
  expect_identical(p$spec$history_days, list(train = 1095L, infer = 15L))   # E4-T3: o deck de inferencia tem so 15 dias
  expect_identical(p$spec$params$janela_dessem, "ano_civil")
  expect_identical(p$descriptor$params_schema$janela_dessem$allowed, c("ano_civil", "referencia"))
  expect_identical(p$env$plugin_constants()$reference_history_days, 1825L)
  w <- rd_with_cli(p$descriptor$history_window("2023-06-01", p$spec))
  expect_identical(w$common_from, lubridate::ymd("2020-06-01"))
  sp <- p$spec; sp$params$janela_dessem <- "referencia"
  r <- rd_with_cli(p$descriptor$history_window("2023-06-01", sp))
  expect_identical(r$common_from, lubridate::ymd("2018-06-02")); expect_null(r$holiday_from)
  root <- tempfile("rdroot-"); dir.create(root)
  file.copy(file.path(rd_models(), c("_dessem-engine", "dessem-regdin")), root, recursive = TRUE)
  ry <- file.path(root, "dessem-regdin", "run.yaml")
  writeLines(sub("janela_dessem: ano_civil", "janela_dessem: xpto", readLines(ry), fixed = TRUE), ry)
  cond <- expect_rd_error(rd_load(root = root), "plugin.bad_run_yaml", "janela_dessem")
  expect_identical(cond$class_name, "contract")
})

test_that("[PLUGINS] janela_infer_igual_treino: ingestao com anexo monta CARGAFER/EXOGENAFER so com dias de ilha; sem anexo o deck e o de antes (E3-T4)", {
  O <- rdw_O
  deck <- rdw_ingest(rdw_panel())
  expect_identical(nrow(deck$CARGAFER), 16L * 24L); expect_identical(nrow(deck$EXOGENAFER), 16L * 24L)
  expect_identical(names(deck$CARGAFER), c("DataHora", "Carga")); expect_identical(names(deck$EXOGENAFER), c("DataHora", "Exo_Temperatura"))
  expect_true(all(lubridate::as_date(deck$CARGAFER$DataHora) < O))
  expect_identical(min(lubridate::as_date(deck$CARGAHIST$DataHora)), O - 1095L)   # common_from
  expect_identical(deck$history_start, O - 1095L)
  expect_identical(deck$islands$island_id, 1:2)
  nua <- rdw_ingest(rdw_panel(annex = NULL))
  expect_false(any(c("CARGAFER", "EXOGENAFER", "islands") %in% names(nua)))
  expect_identical(deck[setdiff(names(deck), c("CARGAFER", "EXOGENAFER", "islands"))], nua)
  vz <- rdw_ingest(rdw_panel(annex = "empty"))                                       # zero ilhas: series vazias, so Bloco 0
  expect_identical(nrow(vz$CARGAFER), 0L); expect_identical(nrow(vz$islands), 0L)
  ## ilhas gravadas no artefato: numerico, data.frame puro, vazio sem anexo
  wi <- deck$islands; wi <- rd_load()$env$.rd_window_islands(wi)
  expect_identical(wi$from, as.numeric(rdw_islands$from)); expect_identical(wi$to, as.numeric(rdw_islands$to))
  expect_identical(nrow(rd_load()$env$.rd_window_islands(NULL)), 0L)
})

test_that("[PLUGINS] janela_infer_igual_treino: dia de ilha com 23 horas ou NA de carga => plugin.regdin_series_gap (exit 3) nomeando dia e hora (E3-T4)", {
  miss <- rdw_hours("2019-12-20", "2019-12-20")[[7L]]
  cond <- expect_rd_error(rdw_ingest(rdw_panel(drop_hour = miss)), "plugin.regdin_series_gap", "2019-12-20")
  expect_identical(cond$exit_code, 3L); expect_true(rd_ascii(conditionMessage(cond)))
  nah <- rdw_hours("2019-04-15", "2019-04-15")[[10L]]
  c2 <- expect_rd_error(rdw_ingest(rdw_panel(na_hour = nah)), "plugin.regdin_series_gap", "2019-04-15")
  expect_identical(c2$exit_code, 3L); expect_match(conditionMessage(c2), "09:00", fixed = TRUE)
  expect_true(is.list(rdw_ingest(rdw_panel())))                                       # CONTROL: completo ingere
  ## history_short continua so para a janela comum
  c3 <- expect_rd_error(rdw_ingest(rdw_panel(from_day = rdw_O - 1000L)), "plugin.regdin_history_short")
  expect_identical(c3$class_name, "window")
})

test_that("[PLUGINS] janela_infer_igual_treino: serieDM, normalizacao e perfil de dia comum so usam o Bloco 0 (E3-T4)", {
  p <- rd_load(); eng <- parent.env(p$env)
  seen_dm <- NULL; seen_lag <- NULL
  o_agg <- eng$aggregaSerie; o_lag <- eng$obtemLagsHora
  assign("aggregaSerie", function(serieHist, ...) { seen_dm <<- range(serieHist$Data[!serieHist$Prev]); o_agg(serieHist, ...) }, envir = eng)
  assign("obtemLagsHora", function(seriehist, ...) { seen_lag <<- range(seriehist$Data[!seriehist$Prev]); o_lag(seriehist, ...) }, envir = eng)
  on.exit({ assign("aggregaSerie", o_agg, envir = eng); assign("obtemLagsHora", o_lag, envir = eng) }, add = TRUE)
  build <- function(panel) rd_with_cli(suppressMessages(p$env$plugin_build_features(rdw_ingest(panel), rdw_spec())))
  f <- build(rdw_panel())
  cf <- lubridate::ymd("2020-06-01")
  expect_true(any(f$processado$SERIECOMP$Bloco > 0L))                                  # dentes: ha dias de ilha na serie do motor
  expect_gte(seen_dm[[1L]], cf)
  eng$entradaPerfilDefault(f$series$seriePF, lagsmodelo = c(1L, 2L))
  expect_gte(seen_lag[[1L]], cf)
  expect_true(any(f$series$seriePF$Data < cf))                                         # o seriePF mantem os blocos (perfil de feriado)
  nua <- build(rdw_panel(annex = NULL))
  expect_identical(p$env$.rd_norm_constants(f$processado$SERIECOMP, f$config_mdl),
                   p$env$.rd_norm_constants(nua$processado$SERIECOMP, nua$config_mdl))
  expect_equal(f$series$serieDM$Carga, nua$series$serieDM$Carga)
  ## CONTROL: sem o filtro de Bloco a base historica traria os dias de ilha (as constantes do regdin so dependem dela se normalizadas)
  expect_gt(nrow(f$processado$SERIECOMP[Prev == FALSE]), nrow(p$env$.rd_common_base(f$processado$SERIECOMP)))
})

test_that("[PLUGINS] janela_infer_igual_treino: a guarda confere o artefato e o fim do deck, nao o conjunto de ilhas (E4-T3; antes E3-T4)", {
  st <- rd3_state(); p <- st$p; pl <- st$med$payload; O <- RD2_O
  mk <- function(from, to) data.frame(island_id = seq_along(from), from = as.numeric(lubridate::ymd(from)), to = as.numeric(lubridate::ymd(to)))
  isl <- mk(c("2019-04-12", "2019-12-18"), c("2019-04-19", "2019-12-25"))
  win <- function(end, islands = NULL) list(start = O - 15L, end = end, islands = islands)
  art <- p$env$rd_thaw(pl)
  ## o deck de inferencia nao carrega anexo: com ou sem ilhas no painel a guarda passa quando o deck termina em train_window$end
  expect_true(rd_with_cli(p$env$.rd_check_window(list(panel_window = win(O - 1L)), art)))
  expect_true(rd_with_cli(p$env$.rd_check_window(list(panel_window = win(O - 1L, isl)), art)))
  ## fim do deck diferente: aborta nomeando os dois dias (esperado e obtido)
  cond <- expect_rd_error(p$env$.rd_check_window(list(panel_window = win(O - 3L)), art), "plugin.regdin_window_mismatch")
  expect_identical(cond$exit_code, 5L); expect_identical(cond$class_name, "window")
  expect_match(conditionMessage(cond), format(O - 1L), fixed = TRUE); expect_match(conditionMessage(cond), format(O - 3L), fixed = TRUE)
  expect_true(rd_ascii(conditionMessage(cond)))
  ## serie_dm adulterada ou fora de train_window => tampered (exit 9), antes de olhar o deck
  raw <- pl; raw$serie_dm$Carga[10L] <- raw$serie_dm$Carga[10L] + 1; bad <- p$env$rd_thaw(raw)
  expect_identical(expect_rd_error(p$env$.rd_check_window(list(panel_window = win(O - 1L)), bad), "plugin.regdin_artifact_tampered")$exit_code, 9L)
  bad2 <- art; bad2$train_window$start <- bad2$train_window$start + 1L
  expect_rd_error(p$env$.rd_check_window(list(panel_window = win(O - 1L)), bad2), "plugin.regdin_artifact_tampered", "train_window")
  ## a guarda e a primeira coisa do infer: plugin_infer aborta antes de refazer o ARIMA
  spy <- rd_spy(".rd_check_fingerprint")
  cnd <- expect_rd_error(p$env$plugin_infer(pl, list(panel_window = win(O - 3L)), list()), "plugin.regdin_window_mismatch")
  expect_identical(cnd$id, "plugin.regdin_window_mismatch"); expect_identical(spy$count(), 0L)
})
