## test-dayshift.R -- gate [PLUGINS] sem_deslocamento do Holt-Winters (HW.5; gate11 do oraculo; AC 7). Tier parity.
## O y_pred de O+1..O+8 casa melhor com a carga REAL do proprio dia rotulado do que com a do dia anterior (WMAPE same < prev
## e corr same > prev; metrica da fonte unica, evaluation.R). CONTROL: artefato-fixture com `mean_interval` deslocado 1 passo
## (o erro historico de ler `$mean[1]` como D: o dia D recebe o previsto de D-1) inverte a desigualdade e a MESMA funcao de
## veredito reprova. O deslocamento do ticket (`[c(2:9, 9), ]`) nao inverte neste dia: fica so medido (discovery).

## base: SECO 2024-05-15 pela cadeia do plugin sobre o painel real (store por overlay), sem o despachante, para reter o artefato
hp_dayshift_base <- function() {
  if (!is.null(.hp$dayshift)) return(.hp$dayshift)
  u <- hp_unit(HP_BASE); cuts <- hp_cuts(u)
  env <- hw_env(); spec <- hw_spec()
  deck <- suppressWarnings(env$plugin_ingest(hp_panel(u, cuts), cuts))
  tdeck <- deck; tdeck$load_day_o <- deck$load_day_o[0L]
  art <- env$plugin_freeze(env$plugin_train(suppressMessages(env$plugin_build_features(tdeck, spec)), spec))
  y <- hw_lib$prevcarga_read_load(hp_ctx(), u$area, from = u$origin, to = u$origin + 9L)
  y_true <- data.frame(interval_end = y$interval_end, y_true = y$load_mwh)
  .hp$dayshift <- list(u = u, cuts = cuts, env = env, deck = deck, art = art, y_true = y_true)
  .hp$dayshift
}

## delivery de O+1..O+8 (o dia O nao tem y_true "do mesmo dia" no oraculo) para um artefato
hp_dayshift_delivery <- function(b, art) {
  inf <- suppressWarnings(b$env$plugin_inference_features(b$deck, art, b$cuts))
  out <- suppressWarnings(b$env$plugin_infer(art, inf, b$cuts))
  au1 <- lubridate::force_tz(lubridate::as_datetime(b$u$origin + 1L), b$cuts$tz)
  as.data.frame(out$delivery[out$delivery$interval_end > au1, ])
}

test_that("[PLUGINS] sem_deslocamento: y_pred casa com o proprio dia (gate11); CONTROL deslocado 1 passo reprova", {
  pc_gate_tier("parity")
  b <- hp_dayshift_base()
  d0 <- hp_dayshift_delivery(b, b$art)
  expect_identical(nrow(d0), 384L)
  v <- hp_dayshift_verdict(d0, b$y_true)
  expect_identical(unname(v$n), c(384L, 384L))
  expect_true(v$ok, info = sprintf("WMAPE same %.3f prev %.3f; corr same %.4f prev %.4f", v$wmape[["same"]], v$wmape[["prev"]],
                                   v$corr[["same"]], v$corr[["prev"]]))
  ## CONTROL do ticket (`[c(2:9, 9), ]`: o dia D recebe o previsto de D+1): MEDIDO e registrado, sem asserir a inversao. No
  ## SECO 2024-05-15 o previsto de D+1 ainda casa melhor com a carga de D do que com a de D-1 (WMAPE same < prev), entao este
  ## sentido NAO inverte a desigualdade (discovery HW.5).
  sf <- b$art; sf$mean_interval <- b$art$mean_interval[c(2:9, 9), ]
  vf <- hp_dayshift_verdict(hp_dayshift_delivery(b, sf), b$y_true)
  ## CONTROL com dente (o erro historico: `$mean[1]` lido como D => o dia D recebe o previsto de D-1): inverte e reprova
  sb <- b$art; sb$mean_interval <- b$art$mean_interval[c(1, 1:8), ]
  vb <- hp_dayshift_verdict(hp_dayshift_delivery(b, sb), b$y_true)
  expect_false(vb$ok, info = sprintf("deslocado p/ tras: WMAPE same %.3f prev %.3f", vb$wmape[["same"]], vb$wmape[["prev"]]))
  expect_gt(vb$wmape[["same"]], vb$wmape[["prev"]])
  expect_lt(vb$corr[["same"]], vb$corr[["prev"]])
  hp_report_add("controls", "dayshift", list(wmape_same = v$wmape[["same"]], wmape_prev = v$wmape[["prev"]],
                                             corr_same = v$corr[["same"]], corr_prev = v$corr[["prev"]],
                                             forward_shift_ok = vf$ok, forward_shift_wmape_same = vf$wmape[["same"]], forward_shift_wmape_prev = vf$wmape[["prev"]], backward_shift_ok = vb$ok))
})
