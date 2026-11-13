## inference_features.R -- features de inferencia do plugin Holt-Winters (HW.4). SO definicoes. Reescreve
## hw_compute_inference_features do oraculo (prevcargadessem3/models/holt-winters/R/inference_features/
## hw_inference_features.R, somente leitura) sobre o artefato congelado: tudo vem do artefato (m_mes, PrevMedDia) e do
## deck de plugin_ingest (calendario do horizonte); nada e re-treinado. HW univariado: nenhum clima entra aqui. O horizonte
## e estendido ao dia O (linha extra, 1 + n linhas) e n_steps = horizon_days do deck.
## Sem hora, sem relogio, sem ambiente, sem RNG. Datas so por lubridate. Saida: ASCII puro.

## cuts do infer: o despachante P.0c entrega ao estagio so (frame, payload); nesse caso origem e fuso vem do `meta` do
## deck (carimbado pela ingestao a partir dos cuts).
.hw_inf_cuts <- function(frame, cuts) {
  if (!is.null(cuts)) return(cuts)
  list(origin_day = frame$meta$origin_day, tz = frame$meta$tz, available_until = NULL)
}

## frame (deck de plugin_ingest), artifact (payload congelado), cuts -> features do infer. Sem `params`: tudo do artefato.
plugin_inference_features <- function(frame, artifact, cuts = NULL) {
  at <- "holt-winters/inference_features"
  cuts <- .hw_inf_cuts(frame, cuts)
  O <- cuts$origin_day
  if (!identical(artifact$origin_day, O))
    hw_abort("hw.artifact_origin", sprintf("artefato de %s, origem %s", format(artifact$origin_day), format(O)),
             list(artifact = format(artifact$origin_day), origin = format(O)), where = at)
  au <- cuts$available_until
  ld <- frame$load_day_o
  if (!is.null(au) && NROW(ld) && any(ld$interval_end > au))
    hw_abort("hw.train_day_o", sprintf("intervalo %s alem de available_until %s",
                                       format(min(ld$interval_end[ld$interval_end > au])), format(au)),
             list(available_until = format(au)), where = at)
  n <- as.integer(frame$n_steps)
  last <- plugin_descriptor()$delivery_days - 1L
  if (is.na(n) || n < 1L || n > last)
    hw_abort("hw.input_column", sprintf("n_steps %s fora de 1..%d", format(n), last), list(column = "n_steps"), where = at)
  dias <- O + 0:n
  hc <- frame$horizon_calendar
  ih <- match(dias, hc$DIA)
  if (anyNA(ih))
    hw_abort("hw.input_column", "horizon_calendar nao cobre O..O+n", list(column = "horizon_calendar"), where = at)
  meta <- data.frame(DIA = dias, FER = as.integer(hc$FER[ih]), mDom = as.integer(lubridate::wday(dias) - 1L),
                     DS = as.integer(hc$DS[ih]), TIPO_FER = as.integer(hc$TIPO_FER[ih]),
                     m_mes = rep(as.integer(artifact$m_mes), length(dias)), stringsAsFactors = FALSE)
  list(area = frame$meta$area, n_steps = n, horizon_meta = meta,
       PrevMedDia = as.double(artifact$mean_daily[seq_len(n + 1L)]))
}
