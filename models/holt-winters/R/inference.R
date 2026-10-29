## inference.R -- estagio de inferencia e forecast acoplado do plugin Holt-Winters (HW.4). SO definicoes. Reescreve hw_infer
## do oraculo (prevcargadessem3/models/holt-winters/R/inference/hw_inference.R, somente leitura) sobre o artefato de dado
## puro: o infer SO LE numeros do artefato (matriz 9x48 de `$mean`), nenhum refit, nenhum forecast::, sem nowcast na v1 (a
## carga do dia O so delimita a entrega). Passo cru -> SemiFer (FER 1/2 fora de domingo) -> LOESS 0,13 (se params.suaviza do
## ARTEFATO) sobre as 9 linhas (O..O+8). A entrega vem da grade da fundacao (prevcarga_delivery_grid): do resto do dia O a
## partir de available_until + 30 ate O+9 00:00; o valor de cada intervalo nao depende de available_until.
## Sem hora aritmetica sobre data-hora (grade de calendario por seq), sem relogio, sem ambiente. Saida: ASCII puro.

## Passo cru: P9 = artefato$mean_interval (linha 1 = dia O, 2..9 = O+1..O+8; o oraculo le $mean[align_first_idx-1+k]).
.assemble_previsao15 <- function(artifact) {
  P9 <- artifact$mean_interval
  if (!is.matrix(P9) || !identical(dim(P9), c(9L, 48L)))
    hw_abort("hw.bad_artifact", "mean_interval: esperado matriz 9x48", list(field = "mean_interval"),
             where = "holt-winters/inference")
  P9
}

## Substitui as linhas FER 1 (F) / 2 (S) fora de domingo por SemiFer (membro HOLTW puro: efeitoFer = efeitoSFer = 0). So
## com clima no dia; falha do SemiFer mantem a linha crua com aviso ASCII (oraculo).
.apply_semifer_override <- function(P9, inf_feat, artifact, efeitoFer = 0, efeitoSFer = 0) {
  hm <- inf_feat$horizon_meta
  if (isTRUE(inf_feat$weather_absent) || !NROW(artifact$carga_temp_ver_fer)) return(P9)
  for (gg in seq_len(nrow(hm))) {
    fer <- hm$FER[gg]
    if (fer == 0L || hm$mDom[gg] == 0L || is.na(hm$TEMP[gg])) next
    tipo <- if (fer == 1L) "F" else "S"
    p_cm <- inf_feat$PrevMedDia[gg] + (if (fer == 1L) efeitoFer else efeitoSFer)
    res <- tryCatch(SemiFer(artifact$carga_temp_ver_fer, tipo, hm$efeitoTemp[gg], p_cm, hm$m_mes[gg], hm$TIPO_FER[gg]),
                    error = function(e) {
                      hw_warn("hw.semifer_failed", sprintf("SemiFer(%s) falhou no dia %s", tipo, format(hm$DIA[gg])),
                              list(day = format(hm$DIA[gg])), where = "holt-winters/inference")
                      NULL
                    })
    if (!is.null(res) && !is.null(res[[1L]])) P9[gg, ] <- as.numeric(res[[1L]])
  }
  P9
}

## LOESS 0,13 por linha (as 9; a linha do dia O e suavizada inteira antes de truncar a entrega); sem arredondar alem do
## que Suaviza.LOESS1 ja faz (funcoes_aux.R). So se params.suaviza do artefato.
.apply_loess_smoothing <- function(P9, artifact) {
  if (!isTRUE(artifact$params$suaviza)) return(P9)
  span <- plugin_constants()$loess_span_out
  for (z in seq_len(nrow(P9))) P9[z, ] <- Suaviza.LOESS1(P9[z, ], span)[[1L]]
  P9
}

## Entrega: 9 linhas achatadas (432 valores) casadas por POSICAO com a grade de calendario de 30 min de interval_end dos dias
## O..O+8 (H01 termina 00:30; o ultimo, O+9 00:00) e recortadas na grade da fundacao (available_until + 30 .. O+9 00:00).
.hw_delivery <- function(P9, cuts) {
  O <- cuts$origin_day
  ie <- seq(lubridate::make_datetime(lubridate::year(O), lubridate::month(O), lubridate::mday(O), tz = cuts$tz),
            by = "30 min", length.out = 433L)[-1L]
  grid <- .hw_foundation("prevcarga_delivery_grid")(cuts, plugin_descriptor()$delivery_days)
  idx <- match(grid, ie)
  y <- as.vector(t(P9))[idx]
  if (anyNA(idx) || anyNA(y))
    hw_abort("plugin.output_misaligned", "intervalo da grade sem valor", list(field = "delivery"), where = "holt-winters/inference")
  data.table::data.table(interval_end = grid, y_pred = y)
}

## artifact (payload), inf_feat, cuts -> envelope parcial (o despachante completa model_id, variant, layout...).
plugin_infer <- function(artifact, inf_feat, cuts) {
  artifact <- plugin_thaw(artifact)
  if (!identical(artifact$origin_day, cuts$origin_day))
    hw_abort("hw.artifact_origin", sprintf("artefato de %s, origem %s", format(artifact$origin_day), format(cuts$origin_day)),
             list(artifact = format(artifact$origin_day), origin = format(cuts$origin_day)), where = "holt-winters/inference")
  n <- inf_feat$n_steps
  P0 <- .assemble_previsao15(artifact)
  P9 <- .apply_loess_smoothing(.apply_semifer_override(P0, inf_feat, artifact), artifact)
  list(area = inf_feat$area, values = P9[2:(n + 1L), , drop = FALSE],
       meta = list(day0_raw = as.numeric(P0[1L, ]), day0 = as.numeric(P9[1L, ]), n_steps = n,
                   weather_absent = inf_feat$weather_absent, no_weather_days = inf_feat$no_weather_days),
       delivery = .hw_delivery(P9, cuts))
}

## Forecast acoplado (origin_scoped): ingestao 1x, treino sem o dia O, FE 1x, freeze em memoria, infer. Mesma cadeia
## numerica do infer sobre o artefato congelado => infer(thaw(freeze(train))) e forecast sao identicos.
plugin_forecast <- function(panel, cuts, spec) {
  deck  <- plugin_ingest(panel, cuts)
  tdeck <- deck
  tdeck$load_day_o <- deck$load_day_o[0L]
  feat  <- plugin_build_features(tdeck, spec)
  art   <- plugin_freeze(plugin_train(feat, spec))
  plugin_infer(art, plugin_inference_features(deck, art, cuts), cuts)
}
