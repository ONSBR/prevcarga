## helper-infer.R -- fixtures da inferencia do plugin random-forest (RF.3). Auto-carregado apos helper-train.R. Nao e codigo
## de produto. Sem variaveis de ambiente; tudo em tempdir; nada toca data/.

## Deck sintetico com carga ate `load_until` ("HH:MM" do dia O); mesma semente => mesmos valores comuns entre decks.
.rf_inf_decks <- new.env(parent = emptyenv())
rf_inf_deck <- function(load_until = "09:30", seed = 7, O = RF_O, sentinel = FALSE) {
  key <- paste(load_until, seed, format(O), sentinel)
  if (!is.null(.rf_inf_decks[[key]])) return(.rf_inf_decks[[key]])
  dir <- tempfile("rf-inf-deck-")
  rf_lib$prevcarga_fixture_deck(dir, seed, RF_AREA, seq(lubridate::ymd("2025-01-01"), O, by = "day"),
    load_label = "start", sentinel = sentinel, tz = RF_TZ,
    load_until = lubridate::ymd_hm(paste(format(O), load_until), tz = RF_TZ))
  assign(key, dir, envir = .rf_inf_decks)
  dir
}

## Artefato fixo (ntree = 5, historico de treino curto) treinado uma vez sobre o deck de 09:00 de O = RF_O.
rf_inf_artifact <- local({ a <- NULL; function() { if (is.null(a)) a <<- rf_tr_run(rf_inf_deck("09:00"))$art; a } })

## Infer sobre um deck: ingestao (fase infer) -> features de infer -> inferencia. Devolve list(env, feat, frame, cuts).
rf_inf_run <- function(dir, art = rf_inf_artifact(), O = RF_O, panel_edit = identity) {
  p <- rf_plug()
  cuts <- rf_fx_cuts(O = O, dir = dir, phase = "infer")
  spec <- rf_tr_spec(); cuts$spec <- spec
  panel <- rf_lib$to_hourly(panel_edit(rf_fx_panel(dir, cuts, hourly = FALSE)), weather = FALSE)   # edita o cru; reamostra como o despachante
  frame <- p$plugin_ingest(panel, cuts, spec)
  feat <- p$plugin_inference_features(frame, art, cuts)
  list(env = p$plugin_infer(art, feat, cuts), feat = feat, frame = frame, cuts = cuts)
}

## Perturba a carga de [first_day, last_day] (dias inteiros, pelo interval_start) num deck e devolve o deck novo.
rf_inf_perturb <- function(dir, first_day, last_day) {
  rf_lib$prevcarga_fixture_perturb(dir, "load",
    after = lubridate::ymd_hm(paste(format(first_day - 1L), "23:30"), tz = RF_TZ),
    at_or_before = lubridate::ymd_hm(paste(format(last_day), "23:30"), tz = RF_TZ))
}

## max |d y_pred| nos interval_end comuns
rf_inf_maxdiff <- function(a, b, keep = rep(TRUE, nrow(a))) {
  m <- merge(a[keep], b, by = "interval_end", suffixes = c("_a", "_b"))
  max(abs(m$y_pred_a - m$y_pred_b))
}
