## test-sentinel.R -- DL.8 (AC8 do epico): sentinela de rotulo de hora do dessem-linear. A grade de entrega e a do
## despachante (`prevcarga_delivery_grid(cuts, 9)`); cada valor entregue cai no intervalo da SUA hora (por interval_start);
## CONTROL: agrupar por interval_end reprova. Deck sentinela: valor = minutos do interval_start/valid_time, o que prova
## que carga e clima chegam ao motor com o MESMO rotulo de hora (o deslocamento de 60 min e so do despachante).

test_that("[PLUGINS] sentinela: carga e clima chegam ao motor alinhados na mesma hora (deck sentinela, rotulo start)", {
  st <- dl_fx_store("09:00", sentinel = TRUE); cuts <- dl_fx_cuts(st)
  base_min <- function(t) as.numeric(difftime(t, lubridate::ymd_hm("2026-09-01 00:00", tz = DL_TZ), units = "mins"))
  pan <- dl_fx_panel(st, cuts, "infer")
  m <- dl_model_panel(pan, cuts)
  deck <- dl_with_cli(suppressMessages(dl_plugin()$env$plugin_ingest(m$panel, m$cuts, NULL)))
  ## sentinela: hora [H, H+1h) = media de 2 meias-horas => minutos(H) + 15. Rotulo do motor = inicio da hora (DataHora)
  ch <- deck$CARGAHIST
  expect_equal(ch$Carga, base_min(ch$DataHora) + 15)
  eh <- deck$EXOGENAHIST
  expect_equal(eh$Exo_Temperatura, base_min(eh$DataHora) + 15)
  ep <- deck$EXOGENAPREV
  expect_equal(ep$Exo_Temperatura, base_min(ep$DataHora) + 15)
  ## carga e clima na MESMA hora carregam o mesmo rotulo (nenhum +-1 h entre os dois lados do motor)
  j <- merge(ch, eh, by = "DataHora")
  expect_identical(nrow(j), nrow(ch))
  expect_equal(j$Carga, j$Exo_Temperatura)
  ## o rotulo da carga no motor e o inicio real da hora: o interval_end de exibicao menos 60 min (so o despachante o aplica)
  hp <- dl_lib$to_hourly(pan)
  expect_equal(as.numeric(m$panel$load$interval_end), as.numeric(hp$load$interval_end) - 3600)
  expect_identical(m$panel$weather_observed$valid_time, hp$weather_observed$valid_time)   # clima nao e deslocado
  ## CONTROL: manifesto com rotulo errado desloca todo intervalo em 30 min e o sentinela reprova
  bad <- dl_lib$prevcarga_fixture_deck(tempfile("dl8-lbl-"), 20241015L, DL8_AREA, DL8_O - 59:0, load_label = "start",
                                       sentinel = TRUE, manifest_label = "end", tz = DL_TZ, horizon_days = 10L)
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", sprintf("  X: {areas: [%s], loss: PX}", DL8_AREA),
               "order: [X]", "members: []"), file.path(bad, "topology.yaml"))
  sb <- list(dir = bad, O = DL8_O, days = 60L, hist = DL8_HIST, areas = DL8_AREA)
  pb <- suppressWarnings(dl_fx_panel(sb, dl_fx_cuts(sb), "infer"))
  shifted <- pb$load[interval_start >= min(pan$load$interval_start) & interval_start <= max(pan$load$interval_start)]
  expect_false(isTRUE(all.equal(base_min(shifted$interval_start), shifted$load_mwh)))
})

test_that("[PLUGINS] sentinela: grade == prevcarga_delivery_grid(cuts, 9); valor de cada intervalo = sua hora (interval_start); CONTROL interval_end reprova", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art()
  p <- dl_plugin(); env <- p$env; eng <- dl_engine_env(p)
  ## so o rotulo e testado: a predicao vira `dia*100 + hora do DataHora (frame de modelagem)` e a regra semi-horaria do
  ## motor falha de proposito => duplicacao :00/:30 do oraculo + aviso (a sentinela independe do modelo)
  fb <- get("desagregaModeloSH", envir = eng, inherits = FALSE)
  pp <- get(".dl_predict_pass", envir = env, inherits = FALSE)
  unlock <- bindingIsLocked("desagregaModeloSH", eng)
  if (unlock) unlockBinding("desagregaModeloSH", eng)
  assign("desagregaModeloSH", function(modelo, dadosMdl) stop("forcado"), envir = eng)
  assign(".dl_predict_pass", function(a, f, dm_head, ...) {
    sp <- f$seriePF[Prev == TRUE, .(DataHora)]
    sp[, Carga := as.numeric(as.integer(lubridate::as_date(DataHora) - f$cuts$origin_day)) * 100 + lubridate::hour(DataHora)]
    sp[]
  }, envir = env)
  on.exit({
    assign(".dl_predict_pass", pp, envir = env)
    assign("desagregaModeloSH", fb, envir = eng); if (unlock) lockBinding("desagregaModeloSH", eng)
  }, add = TRUE)
  e <- dl_infer(art, st, cuts)
  expect_true("warning.dessem_linear_desagrega_fallback" %in% e$meta$warnings)
  d <- data.table::copy(e$delivery)
  grid <- dl_lib$prevcarga_delivery_grid(cuts, 9)
  expect_identical(d$interval_end, grid)
  expect_identical(d$interval_end[[1L]], dl_au("09:30")); expect_identical(d$interval_end[[nrow(d)]], dl_au("00:00", DL8_O + 9L))
  expect_true(all(diff(as.numeric(d$interval_end)) == 1800))
  by_start <- function(x) { s <- x - lubridate::minutes(30L); as.numeric(as.integer(lubridate::as_date(s) - DL8_O)) * 100 + lubridate::hour(s) }
  by_end <- function(x) as.numeric(as.integer(lubridate::as_date(x) - DL8_O)) * 100 + lubridate::hour(x)
  expect_equal(d$y_pred, by_start(d$interval_end))
  ## CONTROL: agrupar por interval_end (o erro classico de +-30 min) reprova a sentinela
  expect_false(isTRUE(all.equal(d$y_pred, by_end(d$interval_end))))
  expect_gt(sum(abs(d$y_pred - by_end(d$interval_end)) > 0), 0L)
  expect_identical(dim(e$values), c(9L, 48L))
})
