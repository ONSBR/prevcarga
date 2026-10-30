## test-sentinel.R -- RD.5 (AC11 do epico, `sentinela`): sentinela de rotulo de hora do dessem-regdin. Deck sentinela do
## fixture_store (`prevcarga_fixture_deck(sentinel = TRUE)`: cada intervalo codifica o proprio interval_start/valid_time em
## minutos), lido pelo leitor do M2 e despachado pela fundacao. A grade de entrega e a do despachante
## (`prevcarga_delivery_grid(cuts, 9)`: semi-horaria, sem buraco, de 09:30 ate O+9 00:00 no fuso da carga, rotulada por
## interval_end); cada valor horario entregue cai na hora que codifica (hora H = inicios H:00 e H:30). O MODELO sai de cena:
## `rd_with_identity_profile` troca o passe de predicao por um transform identidade (a previsao da hora = codigo da propria
## hora no frame de modelagem) e a regra semi-horaria do motor falha de proposito (duplicacao :00/:30 do oraculo + aviso),
## de modo que o valor entregue e o codificado. CONTROL: agrupar por interval_end (o erro classico de +-30 min) reprova.

## min do sentinela no fuso do deck
rd5_sent_min <- function(t) rd_lib$.pc_fx_minutes(t, RD_TZ)

## identidade no lugar do modelo: (a) `.rd_predict_pass` entrega `codigo = dia_de_O * 100 + hora` por DataHora do frame de
## modelagem; (b) a conferencia do fingerprint do ARIMA e dispensada (o sentinela nao e a serie do treino); (c) a regra
## semi-horaria do motor falha. Tudo restaurado ao sair.
rd_with_identity_profile <- function(code, plugin = rd_fx_plugin()) {
  env <- plugin$env; eng <- parent.env(env)
  pred <- get(".rd_predict_pass", envir = env, inherits = FALSE)
  fp <- get(".rd_check_fingerprint", envir = env, inherits = FALSE)
  fb <- get("desagregaModeloSH", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("desagregaModeloSH", eng)
  if (unlock) unlockBinding("desagregaModeloSH", eng)
  assign(".rd_predict_pass", function(inf_feat, artifact, acc = new.env()) {
    sp <- data.table::copy(inf_feat$features$series$seriePF[Prev == TRUE, .(DataHora)])
    sp[, Carga := as.numeric(as.integer(lubridate::as_date(DataHora) - inf_feat$cuts$origin_day)) * 100 +
         lubridate::hour(DataHora)]
    sp[]
  }, envir = env)
  assign(".rd_check_fingerprint", function(...) invisible(NULL), envir = env)
  assign("desagregaModeloSH", function(modelo, dadosMdl) stop("forcado"), envir = eng)
  withr::defer({
    assign(".rd_predict_pass", pred, envir = env); assign(".rd_check_fingerprint", fp, envir = env)
    assign("desagregaModeloSH", fb, envir = eng); if (unlock) lockBinding("desagregaModeloSH", eng)
  }, envir = parent.frame())
  invisible(NULL)
}

test_that("[PLUGINS] sentinela: carga e clima chegam ao motor alinhados na mesma hora (deck sentinela, rotulo start)", {
  sst <- rd5_store(sentinel = TRUE); cu <- rd5_cuts(sst, "09:00")
  expect_identical(cu$available_until, rd5_au("09:00"))
  pan <- rd5_panel(sst, cu, "infer")
  m <- rd_model_panel(pan, cu)
  deck <- rd_with_cli(suppressMessages(rd_fx_plugin()$env$plugin_ingest(m$panel, m$cuts, NULL)))
  ## sentinela: hora [H, H+1h) = media de 2 meias-horas => minutos(H) + 15. Rotulo do motor = inicio da hora (DataHora)
  ch <- deck$CARGAHIST
  expect_equal(ch$Carga, rd5_sent_min(ch$DataHora) + 15)
  eh <- deck$EXOGENAHIST
  expect_equal(eh$Exo_Temperatura, rd5_sent_min(eh$DataHora) + 15)
  ep <- deck$EXOGENAPREV
  expect_equal(ep$Exo_Temperatura, rd5_sent_min(ep$DataHora) + 15)
  ## carga e clima na MESMA hora carregam o mesmo rotulo (nenhum +-1 h entre os dois lados do motor)
  j <- merge(ch, eh, by = "DataHora")
  expect_identical(nrow(j), nrow(ch))
  expect_equal(j$Carga, j$Exo_Temperatura)
  ## o rotulo da carga no motor e o inicio real da hora: o interval_end de exibicao menos 60 min (so o despachante o aplica)
  hp <- rd_lib$to_hourly(pan)
  expect_equal(as.numeric(m$panel$load$interval_end), as.numeric(hp$load$interval_end) - 3600)
  expect_identical(m$panel$weather_observed$valid_time, hp$weather_observed$valid_time)   # clima nao e deslocado
  ## a carga de O so traz horas completas: a ultima hora 08:00-09:00 (DataHora = inicio real da hora, 08:00)
  expect_identical(as.numeric(max(deck$origin_load$DataHora)), as.numeric(rd5_au("08:00")))
  expect_equal(deck$origin_load$Carga, rd5_sent_min(deck$origin_load$DataHora) + 15)
})

test_that("[PLUGINS] sentinela: grade == prevcarga_delivery_grid(cuts, 9); valor de cada intervalo = sua hora (interval_start); CONTROL interval_end reprova", {
  sst <- rd5_store(sentinel = TRUE); cu <- rd5_cuts(sst, "09:00"); art <- rd5_art("MED"); O <- sst$O
  rd_with_identity_profile()
  e <- rd5_infer(art, sst, cu)
  expect_true("warning.regdin_desagrega_fallback" %in% e$meta$warnings)
  d <- data.table::copy(e$delivery)
  grid <- rd_lib$prevcarga_delivery_grid(cu, 9L)
  expect_identical(d$interval_end, grid)
  expect_identical(d$interval_end[[1L]], rd5_au("09:30")); expect_identical(d$interval_end[[nrow(d)]], rd5_au("00:00", O + 9L))
  expect_true(all(diff(as.numeric(d$interval_end)) == 1800))           # sem buraco
  expect_identical(lubridate::tz(d$interval_end), cu$tz)                # fuso da carga
  by_start <- function(x) { s <- x - lubridate::minutes(30L); as.numeric(as.integer(lubridate::as_date(s) - O)) * 100 + lubridate::hour(s) }
  by_end <- function(x) as.numeric(as.integer(lubridate::as_date(x) - O)) * 100 + lubridate::hour(x)
  ## cada valor cai na hora que codifica: hora H = inicios H:00 (fim H:30) e H:30 (fim H+1:00)
  expect_equal(d$y_pred, by_start(d$interval_end))
  h <- split(d$y_pred, by_start(d$interval_end))
  expect_identical(length(h), 9L * 24L - 9L)                           # horas 09:00 de O ate 23:00 de O+8
  expect_true(all(lengths(h) == 2L))                                   # cada hora = os dois inicios H:00 e H:30
  expect_true(all(vapply(h, function(v) length(unique(v)) == 1L, NA)))
  ## CONTROL: agrupar por interval_end (o erro classico de +-30 min) reprova a sentinela
  expect_false(isTRUE(all.equal(d$y_pred, by_end(d$interval_end))))
  expect_gt(sum(abs(d$y_pred - by_end(d$interval_end)) > 0), 0L)
  expect_identical(dim(e$values), c(9L, 48L))
})
