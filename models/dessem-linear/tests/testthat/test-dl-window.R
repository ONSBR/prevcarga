## test-dl-window.R -- E3-T4 (ADR-046): janela 1095 dias, anexo de feriados e normalizacao so no Bloco 0 do dessem-linear.
## Fixtures sinteticas em memoria (painel JA horario e no frame de modelagem, como o despachante entrega ao plugin); sem store.
## [PLUGINS] ingestao: janela_dessem, o rotulo mapeia para o gate `descritor` do run_all_gates.R.

dlw_O <- lubridate::ymd("2023-06-01")
dlw_area <- "AA"
dlw_hours <- function(d0, d1) {                       # horas inteiras de [d0, d1] (dias inclusive), rotulo = inicio da hora
  a <- lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(d0)), DL_TZ)
  seq(a, by = "hour", length.out = (as.integer(lubridate::as_date(d1) - lubridate::as_date(d0)) + 1L) * 24L)
}
dlw_load <- function(dh, plus = 0) {
  d <- as.numeric(lubridate::as_date(dh) - lubridate::ymd("2015-01-01")); h <- lubridate::hour(dh)
  8000 + plus + 500 * sin(2 * pi * h / 24) + 30 * (d %% 7) + 3 * sin(d * 1.3 + h)
}
dlw_temp <- function(dh, plus = 0) 22 + plus + 4 * sin(2 * pi * lubridate::hour(dh) / 24) + 0.2 * (as.numeric(lubridate::as_date(dh)) %% 5)

## ilhas do anexo (antes de common_from = O - 1095 = 2020-06-01)
dlw_islands <- data.table::data.table(
  area_code = dlw_area, island_id = 1:2,
  from = lubridate::ymd(c("2019-04-12", "2019-12-18")), to = lubridate::ymd(c("2019-04-19", "2019-12-25")),
  holiday_dates = list(lubridate::ymd("2019-04-19"), lubridate::ymd("2019-12-25")))

## `from_day`: primeiro dia da carga/observado; `annex`: NULL (sem anexo), "islands" ou "empty" (anexo sem ilha);
## `drop_hour`: interval_end/valid_time do ANEXO a remover (dia de ilha incompleto).
dlw_panel <- function(O = dlw_O, from_day = O - 1096L, annex = "islands", drop_hour = NULL, plus = 5000) {
  hc <- dlw_hours(from_day, O - 1L)
  ld <- data.table::data.table(area_code = dlw_area, interval_start = hc, interval_end = hc, load_mwh = dlw_load(hc))
  wo <- data.table::data.table(area_code = dlw_area, valid_time = hc, temperature = dlw_temp(hc))
  fh <- dlw_hours(O, O + 10L)
  wf <- data.table::data.table(area_code = dlw_area, issue_time = lubridate::force_tz(lubridate::as_datetime(O), "UTC"),
                               valid_time = fh, temperature = dlw_temp(fh))
  hol <- data.table::data.table(area_code = dlw_area, date = O - c(10L, 200L), holiday_type_id = 1L,
                                prevcarga_code = c(10L, 3L), simplified_code = 3L)
  panel <- list(load = ld, weather_observed = wo, weather_forecast = wf, holidays = hol,
                attrs = list(origin = O, load_label = "end", tz = DL_TZ))
  if (!is.null(annex)) {
    isl <- if (identical(annex, "islands")) dlw_islands else dlw_islands[0L]
    hf <- if (nrow(isl)) do.call(c, lapply(seq_len(nrow(isl)), function(i) dlw_hours(isl$from[[i]], isl$to[[i]]))) else hc[0L]
    if (!is.null(drop_hour)) hf <- hf[!(hf %in% drop_hour)]
    ## valores deslocados (plus): quem misturar o anexo na normalizacao muda as constantes
    panel$holiday_annex <- list(
      islands = isl,
      load = data.table::data.table(area_code = dlw_area, interval_start = hf, interval_end = hf, load_mwh = dlw_load(hf, plus)),
      weather_observed = data.table::data.table(area_code = dlw_area, valid_time = hf, temperature = dlw_temp(hf, plus / 500)))
  }
  panel
}
dlw_cuts <- function(O = dlw_O) dl_cuts(format(O))
dlw_spec <- function(janela = "ano_civil") {
  s <- dl_spec("MED", 1095L); s$params$janela_dessem <- janela; s
}
dlw_ingest <- function(panel, janela = "ano_civil", O = dlw_O) {
  p <- dl_load()
  dl_with_cli(suppressMessages(p$env$plugin_ingest(panel, dlw_cuts(O), dlw_spec(janela))))
}

test_that("[PLUGINS] descritor: janela_dessem, run.yaml, params_schema e history_window nos dois modos", {
  p <- dl_load()
  expect_identical(p$spec$history_days, list(train = 1095L, infer = 15L))   # E4-T2: infer = minimo (lagsPerfil 336 h + 1 dia)
  expect_identical(p$spec$params$janela_dessem, "ano_civil")
  expect_identical(p$descriptor$params_schema$janela_dessem$allowed, c("ano_civil", "referencia"))
  expect_identical(p$descriptor$variant_param, "exogena")
  expect_identical(p$env$plugin_constants()$reference_history_days, 1825L)
  sp <- p$spec
  w <- dl_with_cli(p$descriptor$history_window("2023-06-01", sp))
  expect_identical(w$common_from, lubridate::ymd("2020-06-01"))
  expect_identical(w$holiday_from, lubridate::ymd("2018-01-01"))
  sp$params$janela_dessem <- "referencia"
  r <- dl_with_cli(p$descriptor$history_window("2023-06-01", sp))
  expect_identical(r$common_from, lubridate::ymd("2018-06-02"))
  expect_null(r$holiday_from)
  expect_identical(r$common_from, lubridate::ymd("2023-06-01") - p$env$plugin_constants()$reference_history_days)
  ## data_needs: o anexo so em ano_civil
  expect_true(p$descriptor$data_needs(p$spec, "SE")$infer$holiday_annex)
  expect_false(p$descriptor$data_needs(sp, "SE")$infer$holiday_annex)
  ## CONTROL: janela_dessem fora do conjunto e recusada pelo params_schema
  root <- tempfile("dlroot-"); dir.create(root)
  file.copy(file.path(dl_models(), c("_dessem-engine", "dessem-linear")), root, recursive = TRUE)
  ry <- file.path(root, "dessem-linear", "run.yaml")
  writeLines(sub("janela_dessem: ano_civil", "janela_dessem: xpto", readLines(ry), fixed = TRUE), ry)
  cond <- expect_dl_error(dl_load(root), "plugin.bad_run_yaml", "janela_dessem")
  expect_identical(cond$class_name, "contract")
})

test_that("[PLUGINS] ingestao: janela_dessem, ingestao com anexo monta CARGAFER/EXOGENAFER so com dias de ilha, CARGAHIST desde common_from - 1", {
  O <- dlw_O
  deck <- dlw_ingest(dlw_panel())
  expect_identical(nrow(deck$CARGAFER), 16L * 24L)
  expect_identical(names(deck$CARGAFER), c("DataHora", "Carga"))
  expect_identical(names(deck$EXOGENAFER), c("DataHora", "Exo_Temperatura"))
  expect_identical(nrow(deck$EXOGENAFER), 16L * 24L)
  dias <- sort(unique(lubridate::as_date(deck$CARGAFER$DataHora)))
  expect_equal(dias, sort(c(seq(lubridate::ymd("2019-04-12"), lubridate::ymd("2019-04-19"), by = "day"),
                                seq(lubridate::ymd("2019-12-18"), lubridate::ymd("2019-12-25"), by = "day"))))
  expect_true(all(lubridate::as_date(deck$CARGAFER$DataHora) < O))
  expect_identical(min(lubridate::as_date(deck$CARGAHIST$DataHora)), O - 1096L)   # common_from - 1 (aquecimento)
  expect_identical(deck$history_start, O - 1095L)
  expect_identical(deck$islands$island_id, 1:2)
  ## hourly_load nao inclui o anexo
  expect_true(all(lubridate::as_date(deck$hourly_load$DataHora) >= O - 1096L))
  ## o resto do deck e identico ao do painel sem anexo (anexo so acrescenta CARGAFER/EXOGENAFER/islands)
  nua <- dlw_ingest(dlw_panel(annex = NULL))
  expect_identical(deck[setdiff(names(deck), c("CARGAFER", "EXOGENAFER", "islands"))], nua)
  expect_false(any(c("CARGAFER", "EXOGENAFER", "islands") %in% names(nua)))
  ## zero ilhas em ano_civil: series vazias, validacao passa
  vz <- dlw_ingest(dlw_panel(annex = "empty"))
  expect_identical(nrow(vz$CARGAFER), 0L); expect_identical(nrow(vz$EXOGENAFER), 0L)
})

test_that("[PLUGINS] ingestao: janela_dessem, dia de ilha com 23 horas => plugin.dessem_linear_series_gap (exit 3) nomeando o dia", {
  miss <- dlw_hours("2019-12-20", "2019-12-20")[[7L]]
  cond <- expect_dl_error(dlw_ingest(dlw_panel(drop_hour = miss)), "plugin.dessem_linear_series_gap", "2019-12-20")
  expect_identical(cond$exit_code, 3L)
  expect_identical(cond$details$dia, "2019-12-20"); expect_identical(cond$details$horas, 23L)
  expect_true(dl_ascii(conditionMessage(cond)))
  ## CONTROL: o mesmo painel completo ingere
  expect_true(is.list(dlw_ingest(dlw_panel())))
})

test_that("[PLUGINS] ingestao: janela_dessem, serieDM, normalizacao e perfil de dia comum so usam o Bloco 0 (>= common_from)", {
  p <- dl_load(); eng <- parent.env(p$env)
  ## FORBIDDEN: espioes em aggregaSerie e obtemLagsHora (esta ultima dentro de entradaPerfilDefault)
  seen_dm <- NULL; seen_lag <- NULL
  o_agg <- eng$aggregaSerie; o_lag <- eng$obtemLagsHora
  assign("aggregaSerie", function(serieHist, ...) { seen_dm <<- range(serieHist$Data[!serieHist$Prev]); o_agg(serieHist, ...) }, envir = eng)
  assign("obtemLagsHora", function(seriehist, ...) { seen_lag <<- range(seriehist$Data[!seriehist$Prev]); o_lag(seriehist, ...) }, envir = eng)
  on.exit({ assign("aggregaSerie", o_agg, envir = eng); assign("obtemLagsHora", o_lag, envir = eng) }, add = TRUE)
  deck <- dlw_ingest(dlw_panel())
  f <- dl_with_cli(suppressMessages(p$env$plugin_build_features(deck, dlw_spec())))
  cf <- lubridate::ymd("2020-06-01") - 1L                                   # common_from - 1: o dia de aquecimento tambem e Bloco 0
  ## dentes: a serie do motor TEM dias de ilha (Bloco > 0) e o espiao viu so o Bloco 0
  expect_true(any(f$processado$SERIECOMP$Bloco > 0L))
  expect_gte(seen_dm[[1L]], cf)
  eng$entradaPerfilDefault(f$seriePF, lagsmodelo = c(1L, 2L))
  expect_gte(seen_lag[[1L]], cf)
  expect_true(any(f$seriePF$Data < cf))                                      # o seriePF mantem os blocos para o perfil de feriado
  ## constantes de normalizacao: as do deck SEM anexo (o anexo tem valores deslocados em +5000 MW)
  nua <- dl_with_cli(suppressMessages(p$env$plugin_build_features(dlw_ingest(dlw_panel(annex = NULL)), dlw_spec())))
  expect_identical(f$norm, nua$norm)
  expect_equal(f$serieDM$Carga, nua$serieDM$Carga)
  ## CONTROL: constantes sobre TODAS as linhas historicas (sem o filtro de Bloco) diferem
  all_rows <- f$processado$SERIECOMP[Prev == FALSE]
  expect_false(isTRUE(all.equal(eng$getNormalizaCarga(f$config_mdl$normalizacao$dm$carga, all_rows), f$norm$dm_carga)) &&
               isTRUE(all.equal(eng$getNormalizaExogena(f$config_mdl$normalizacao$dm$exogena, all_rows), f$norm$dm_exo)))
})

test_that("[PLUGINS] ingestao: janela_dessem, modo referencia nao tem anexo e o deck e o de antes", {
  panel <- dlw_panel(from_day = dlw_O - 1826L, annex = NULL)
  deck <- dlw_ingest(panel, "referencia")
  expect_false(any(c("CARGAFER", "EXOGENAFER", "islands") %in% names(deck)))
  expect_identical(deck$history_start, dlw_O - 1825L)
  f <- dl_with_cli(suppressMessages(dl_load()$env$plugin_build_features(deck, dlw_spec("referencia"))))
  expect_true(all(f$processado$SERIECOMP$Bloco == 0L))
})
