## inference_features.R -- features de inferencia do plugin Holt-Winters (HW.4). SO definicoes. Reescreve
## hw_compute_inference_features do oraculo (prevcargadessem3/models/holt-winters/R/inference_features/
## hw_inference_features.R e PrevisaoDiaria.R:1688-1708, somente leitura) sobre o artefato congelado: tudo vem do
## artefato (CargaTempVer limpa, m_mes, PrevMedDia) e do deck de plugin_ingest; nada e re-treinado. O horizonte e
## estendido ao dia O (linha extra, 1 + n linhas). O clima previsto ja chega escolhido pela regra weather_issue_cutoff
## (ADR-0024) no leitor/ingestao; sem emissao valida o deck traz o fallback do oraculo (temperaturas NA, n = horizon_days).
## Sem hora, sem relogio, sem ambiente, sem RNG. Datas so por lubridate. Saida: ASCII puro.

## cuts do infer: o despachante P.0c entrega ao estagio so (frame, payload); nesse caso origem e fuso vem do `meta` do
## deck (carimbado pela ingestao a partir dos cuts).
.hw_inf_cuts <- function(frame, cuts) {
  if (!is.null(cuts)) return(cuts)
  list(origin_day = frame$meta$origin_day, tz = frame$meta$tz, available_until = NULL)
}

## CargaTempVer3 (PrevisaoDiaria.R:194): linhas sem feriado do mes da origem ou do mes de origem - 31 dias.
.build_CargaTempVer3 <- function(carga_temp_ver, origin_day) {
  m_mes    <- lubridate::month(origin_day)
  m_mesant <- lubridate::month(origin_day - lubridate::days(31))
  mm <- lubridate::month(carga_temp_ver$Data)
  carga_temp_ver[(mm == m_mesant | mm == m_mes) & (carga_temp_ver$Feriado == 0), , drop = FALSE]
}

## efeitoTemp legado (PrevisaoDiaria.R:1688-1705), com o quirk do oraculo: `qdata` criado 1x FORA do laco, `conttemp`
## reiniciado a cada dia, escrita sobrescreve sem truncar e a ecdf cobre TODAS as linhas de `qdata`.
.compute_efeitoTemp_legacy <- function(ctv3, DS_vec, TEMP_vec) {
  n  <- length(DS_vec)
  ef <- numeric(n)
  qdata <- data.frame()
  for (gg in seq_len(n)) {
    conttemp <- 1L
    for (ff in seq_len(nrow(ctv3))) {
      if (ctv3$DiaSemana[ff] == DS_vec[gg]) {
        qdata[conttemp, 1] <- ctv3$CargaMedia[ff]
        qdata[conttemp, 2] <- ctv3$TempMedia[ff]
        conttemp <- conttemp + 1L
      }
    }
    if (nrow(qdata) == 0L || all(is.na(qdata[, 2]))) {
      ef[gg] <- 0.5
    } else {
      fn <- stats::ecdf(qdata[, 2])
      ef[gg] <- fn(TEMP_vec[gg])
    }
  }
  ef
}

## efeitoTemp por dia do horizonte (dia O + O+1..O+n). O+1..O+n percorrem a MESMA sequencia de estados do oraculo
## (1 chamada legada so com eles); o dia O vai numa chamada separada com `qdata` proprio, para a linha extra nao
## contaminar os dias seguintes.
.hw_efeito_temp <- function(ctv3, cdd, origin_day) {
  is_o <- cdd$DIA == origin_day
  hz   <- cdd[!is_o, , drop = FALSE]
  ef   <- numeric(nrow(cdd))
  if (nrow(hz)) ef[!is_o] <- .compute_efeitoTemp_legacy(ctv3, as.integer(hz$DS), as.numeric(hz$TEMP))
  if (any(is_o))
    ef[is_o] <- .compute_efeitoTemp_legacy(ctv3, as.integer(cdd$DS[is_o]), as.numeric(cdd$TEMP[is_o]))
  ef
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
  dias <- O + lubridate::days(0:n)
  cd <- frame$CDDPre; tp <- frame$TempPre
  ic <- match(dias, cd$DIA); it <- match(dias, tp$DIA)
  cdd <- data.frame(DIA = dias, TEMP = as.numeric(cd$TEMP[ic]),
                    DS = as.integer(ifelse(is.na(ic), lubridate::wday(dias), cd$DS[ic])),
                    TIPO_FER = as.integer(ifelse(is.na(ic), 0L, cd$TIPO_FER[ic])), stringsAsFactors = FALSE)
  ctv3 <- .build_CargaTempVer3(artifact$carga_temp_ver, O)
  ef <- .hw_efeito_temp(ctv3, cdd, O)
  ef[is.na(cdd$TEMP)] <- 0.5                       # sem clima no dia: efeito neutro (SemiFer e pulado no infer)
  meta <- data.frame(DIA = dias, FER = as.integer(ifelse(is.na(it), 0L, tp$FER[it])),
                     mDom = as.integer(lubridate::wday(dias) - 1L), DS = cdd$DS, TIPO_FER = cdd$TIPO_FER,
                     m_mes = rep(as.integer(artifact$m_mes), length(dias)), efeitoTemp = ef, TEMP = cdd$TEMP,
                     stringsAsFactors = FALSE)
  covered <- cd$DIA[!is.na(cd$TEMP)]
  list(area = frame$meta$area, n_steps = n, horizon_meta = meta, PrevMedDia = as.double(artifact$mean_daily[seq_len(n + 1L)]),
       weather_absent = isTRUE(frame$weather_absent),
       no_weather_days = (O + lubridate::days(1:last))[!(O + lubridate::days(1:last)) %in% covered])
}
