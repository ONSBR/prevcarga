## feature_engineering.R -- estagio de feature engineering do plugin LGBM (LGBM.2). SO definicoes.
## Porte de build_features_train e auxiliares de prevcargadessem3/models/lgbm/R/feature_engineering/feature_engineering.R
## (somente leitura). Tudo governado por `params` (spec no treino; o LGBM.4 passa os do artefato no infer): nenhuma env var.
## Sem aritmetica de hora (principio 7): o shift de 48 h do CarWav e uma chave de calendario (Data + carwav_shift_days, Hora)
## na grade flat-48 -- equivalente ao DataHora + 172800 s do oraculo. Os cortes (`cuts`) chegam prontos de dates.R; o
## plugin nao recalcula last_data_day. Os rotulos de calendario sao de helpers_calendar.R; o LOESS, de helpers_loess.R.
## Saida ASCII.

## --- Ondaletas Haar -----------------------------------------------------------------------------------------------
## Coeficientes W (detalhe) e V (escala) de UMA janela (potencia de 2); nomes <nivel>_<i> (ex.: W1_1..W1_16, W5_1, V1_1..).
lgbm_wavelet_window <- function(v) {
  wt <- wavelets::dwt(v, filter = "haar")
  mat <- function(lst) {
    m <- do.call(cbind, lapply(lst, t))
    colnames(m) <- unlist(lapply(names(lst), function(s) paste0(s, "_", seq_len(nrow(lst[[s]])))))
    m
  }
  cbind(mat(wt@W), mat(wt@V))
}

## Numero de coeficientes (W + V) de Haar para janela `width` (potencia de 2): 2 * (width - 1).
lgbm_wavelet_ncoef <- function(width) 2L * (as.integer(width) - 1L)

## Ondaletas moveis alinhadas a direita (as width-1 primeiras linhas NA). Janela com NA => linha NA (nunca erro).
lgbm_build_wavelets <- function(x, width) {
  width <- as.integer(width)
  if (!is.numeric(x) || length(x) < width)
    lgbm_abort("lgbm.window", sprintf("serie com %d pontos para janela de ondaleta %d", length(x), width))
  nc <- lgbm_wavelet_ncoef(width)
  f <- function(v) if (anyNA(v)) rep(NA_real_, nc) else as.numeric(lgbm_wavelet_window(v))
  out <- zoo::rollapply(x, width = width, FUN = f, fill = NA, align = "right")
  ref <- lgbm_wavelet_window(seq_len(width) * 1.0)
  out <- matrix(out, ncol = nc)
  colnames(out) <- colnames(ref)
  data.table::as.data.table(out)
}

## Junta CarWav_* (de CargaGlobal_anterior_BLF, chave (Data + carwav_shift_days, Hora)) e TempWav_* (de
## Temperatura_prevista_BLF, mesma (Data, Hora)) por chave de calendario. dt ordenado por (Data, Hora).
lgbm_add_wavelets <- function(dt, consts = plugin_constants()) {
  w <- consts$wavelet_width
  car <- lgbm_build_wavelets(dt$CargaGlobal_anterior_BLF, w)
  n <- ncol(car)
  data.table::setnames(car, paste0("CarWav_", seq_len(n)))
  car[, `:=`(Data = dt$Data + consts$carwav_shift_days, Hora = dt$Hora)]
  tmp <- lgbm_build_wavelets(dt$Temperatura_prevista_BLF, w)
  data.table::setnames(tmp, paste0("TempWav_", seq_len(ncol(tmp))))
  tmp[, `:=`(Data = dt$Data, Hora = dt$Hora)]
  dt <- merge(dt, car, by = c("Data", "Hora"), all.x = TRUE)
  merge(dt, tmp, by = c("Data", "Hora"), all.x = TRUE)
}

## --- Sazonalidade (so linhas de treino: Data <= train_cutoff com carga) ---------------------------------------------
.lgbm_saz_rows <- function(dt, train_cutoff) dt[Data <= train_cutoff & !is.na(CargaGlobal)]

.lgbm_check_cutoff <- function(train_cutoff) {
  if (!inherits(train_cutoff, "Date") || length(train_cutoff) != 1L || is.na(train_cutoff))
    lgbm_abort("lgbm.contract", "train_cutoff ausente ou nao Date")
  invisible(TRUE)
}

## Tabela (TipoDia, Hora) -> media da carga; com tipodia_legacy a chave rica (TipoDia, Hora, Feriado, DiaEspecialFimAno)
## com LOESS span 0.2 grau 2 em Hora/2 nos grupos com >= 8 linhas (ANN_PVDS.R:336-346; roda mesmo com disable_loess:
## e a curva de consulta congelada no artefato, nao a suavizacao por dia).
lgbm_compute_sazonalidade <- function(dt, train_cutoff, params) {
  .lgbm_check_cutoff(train_cutoff)
  rows <- .lgbm_saz_rows(dt, train_cutoff)
  if (!isTRUE(params$tipodia_legacy)) {
    sazon <- rows[, list(Sazonalidade = mean(CargaGlobal)), by = list(TipoDia, Hora)]
    data.table::setorder(sazon, TipoDia, Hora)
    return(sazon[])
  }
  if (!all(c("Feriado", "DiaEspecialFimAno") %in% names(dt)))
    lgbm_abort("lgbm.params", "tipodia_legacy exige calendar_yearend (DiaEspecialFimAno)")
  sazon <- rows[, list(CargaGlobal_media = mean(CargaGlobal)), by = list(TipoDia, Hora, Feriado, DiaEspecialFimAno)]
  sazon[, Sazonalidade := {
    y <- CargaGlobal_media
    hx <- Hora / 2.0
    if (.N >= 8L) {
      tryCatch(as.numeric(suppressWarnings(stats::predict(stats::loess(y ~ hx, span = 0.2, degree = 2L)))),
               error = function(e) y)
    } else y
  }, by = list(TipoDia, Feriado, DiaEspecialFimAno)]
  sazon[, CargaGlobal_media := NULL]
  data.table::setorder(sazon, TipoDia, Feriado, DiaEspecialFimAno, Hora)
  sazon[]
}

## Fallbacks do merge rico: fb2 (TipoDia, Hora, Feriado), fb3 (TipoDia, Hora), fb4 (Hora); medias cruas.
lgbm_compute_sazonalidade_fallbacks <- function(dt, train_cutoff) {
  .lgbm_check_cutoff(train_cutoff)
  rows <- .lgbm_saz_rows(dt, train_cutoff)
  list(fb2 = rows[, list(Saz_fb2 = mean(CargaGlobal)), by = list(TipoDia, Hora, Feriado)],
       fb3 = rows[, list(Saz_fb3 = mean(CargaGlobal)), by = list(TipoDia, Hora)],
       fb4 = rows[, list(Saz_fb4 = mean(CargaGlobal)), by = list(Hora)])
}

## Aplica a Sazonalidade congelada (tambem usada no infer com a tabela do artefato). Sem legacy: merge (TipoDia, Hora).
## Com legacy: chave rica + fallback hierarquico fb2/fb3/fb4 (fcoalesce). O chamador reordena por DataHora.
lgbm_merge_sazonalidade <- function(dt, sazon, fallbacks, params) {
  if (!isTRUE(params$tipodia_legacy)) return(merge(dt, sazon, by = c("TipoDia", "Hora"), all.x = TRUE))
  if (is.null(fallbacks)) lgbm_abort("lgbm.contract", "sazonalidade legacy sem fallbacks")
  dt <- merge(dt, sazon, by = c("TipoDia", "Hora", "Feriado", "DiaEspecialFimAno"), all.x = TRUE)
  if (anyNA(dt$Sazonalidade)) {
    dt <- merge(dt, fallbacks$fb2, by = c("TipoDia", "Hora", "Feriado"), all.x = TRUE)
    dt <- merge(dt, fallbacks$fb3, by = c("TipoDia", "Hora"), all.x = TRUE)
    dt <- merge(dt, fallbacks$fb4, by = "Hora", all.x = TRUE)
    dt[, Sazonalidade := data.table::fcoalesce(Sazonalidade, Saz_fb2, Saz_fb3, Saz_fb4)]
    dt[, c("Saz_fb2", "Saz_fb3", "Saz_fb4") := NULL]
  }
  dt
}

## --- Cortes internos de treino/validacao --------------------------------------------------------------------------
## Divisao interna do MODELO (nao e corte de honestidade: esse chega pronto em `cuts`). train_cv => ambos = last_data_day.
## Senao porta default_val_cutoff/default_train_cutoff (orchestrator.R:383-416) com origin_day = cuts$train_origin.
lgbm_split_cutoffs <- function(cuts, params, consts = plugin_constants()) {
  ldd <- cuts$last_data_day
  if (!inherits(ldd, "Date") || length(ldd) != 1L || is.na(ldd)) lgbm_abort("lgbm.contract", "cuts$last_data_day ausente")
  if (isTRUE(params$train_cv)) return(list(train_cutoff = ldd, val_cutoff = ldd))
  to <- cuts$train_origin
  if (!inherits(to, "Date") || length(to) != 1L || is.na(to)) lgbm_abort("lgbm.contract", "cuts$train_origin ausente")
  month_back <- function(d) lubridate::floor_date(lubridate::add_with_rollback(d, lubridate::period(-1, "month"),
                                                                              roll_to_first = FALSE), "month") - 1L
  scheme <- params$cutoff_scheme
  val <- switch(scheme,
    rolling = ldd,
    legacy_month = lubridate::floor_date(to, "month") - 1L,
    v2_old = month_back(to),
    lgbm_abort("lgbm.params", sprintf("cutoff_scheme=%s fora do schema", scheme)))
  trn <- switch(scheme,
    rolling = val - consts$rolling_val_days,
    legacy_month = lubridate::floor_date(val, "month") - 1L,
    v2_old = month_back(val))
  if (max(val, trn) > ldd)
    lgbm_abort("lgbm.contract", sprintf("corte interno %s alem de last_data_day=%s", format(max(val, trn)), format(ldd)))
  list(train_cutoff = trn, val_cutoff = val)
}

## --- Esquema de colunas -------------------------------------------------------------------------------------------
## Ordem LOAD-BEARING (indices categoricos do LightGBM): escalares(4) + CarWav + TempWav + DiaSemana_1..6 +
## DiaEspecial_FDS/FE/FEFDS (salvo legacy) + Mes_2..12 + extras de adequacao. `n_wav` = coeficientes por serie (62).
lgbm_input_cols <- function(params, n_wav) {
  c("CargaGlobal_anterior_BLF", "Temperatura_prevista_BLF", "Sazonalidade", "Hora_seno",
    paste0("CarWav_", seq_len(n_wav)), paste0("TempWav_", seq_len(n_wav)),
    paste0("DiaSemana_", 1L:6L),
    if (isTRUE(params$tipodia_legacy)) character(0L) else c("DiaEspecial_FDS", "DiaEspecial_FE", "DiaEspecial_FEFDS"),
    paste0("Mes_", 2L:12L),
    lgbm_adequacao_extra_features(params))
}

## Colunas nao normalizadas: dummies + extras (tudo que nao e escalar/ondaleta).
lgbm_nonorm_cols <- function(params) {
  c(paste0("DiaSemana_", 1L:6L),
    if (isTRUE(params$tipodia_legacy)) character(0L) else c("DiaEspecial_FDS", "DiaEspecial_FE", "DiaEspecial_FEFDS"),
    paste0("Mes_", 2L:12L), lgbm_adequacao_extra_features(params))
}

## --- Estagio ------------------------------------------------------------------------------------------------------
## Forma completa (a do contrato do ticket): frame de ingestao, spec (params + seed) e cuts. Devolve a lista do oraculo.
lgbm_build_features <- function(frame, spec, cuts, consts = plugin_constants()) {
  params <- lgbm_check_params(spec$params)
  keep_rng <- .lgbm_foundation("prevcarga_preserve_rng")
  if (is.null(keep_rng)) lgbm_abort("lgbm.contract", "prevcarga_preserve_rng indisponivel (fundacao)")
  keep_rng({
    if (!is.null(spec$seed)) set.seed(spec$seed)
    res <- .lgbm_build_features(frame, params, cuts, consts)
    ## INTERIM (F1 adiado, decisao do Gabriel): o treino recebe (features, params, seed); os cortes seguem em attr
    attr(res, "cuts") <- cuts[c("last_data_day", "train_origin")]
    res
  })
}

.lgbm_build_features <- function(frame, params, cuts, consts) {
  feriados <- attr(frame, "feriados")
  if (!inherits(feriados, "Date")) lgbm_abort("lgbm.contract", "frame sem attr feriados (ingestao)")
  cut <- lgbm_split_cutoffs(cuts, params, consts)
  dt <- data.table::copy(data.table::as.data.table(frame))
  data.table::setorder(dt, DataHora)
  lgbm_add_calendar_cols(dt, feriados, params)
  ## LOESS por dia (grau 2, span_prep) sobre Hora/2; identidade com disable_loess
  dt[, HoraDecimal := Hora / 2.0]
  dt[, CargaGlobal_suav := lgbm_loess_daily(CargaGlobal, HoraDecimal, consts$span_prep, params$disable_loess), by = Data]
  dt[, Temperatura_suav := lgbm_loess_daily(Temperatura, HoraDecimal, consts$span_prep, params$disable_loess), by = Data]
  ## lag D-1 suavizado por (Data + 1, Hora); Temperatura_prevista_BLF = suavizada do proprio dia
  lag_tbl <- dt[, list(Data_next = Data + 1L, Hora = Hora, CargaGlobal_anterior_BLF = CargaGlobal_suav)]
  dt <- merge(dt, lag_tbl, by.x = c("Data", "Hora"), by.y = c("Data_next", "Hora"), all.x = TRUE)
  dt[, Temperatura_prevista_BLF := Temperatura_suav]
  dt <- dt[!is.na(CargaGlobal_anterior_BLF)]
  data.table::setorder(dt, Data, Hora)
  dt[, Hora_seno := sin(2.0 * pi * Hora / 48.0)]
  dt <- lgbm_add_wavelets(dt, consts)
  n_wav <- sum(grepl("^CarWav_", names(dt)))
  ## Sazonalidade congelada so nas linhas Data <= train_cutoff
  sazon <- lgbm_compute_sazonalidade(dt, cut$train_cutoff, params)
  fb <- if (isTRUE(params$tipodia_legacy)) lgbm_compute_sazonalidade_fallbacks(dt, cut$train_cutoff) else NULL
  dt <- lgbm_merge_sazonalidade(dt, sazon, fb, params)
  for (lvl in 1L:6L) dt[, (paste0("DiaSemana_", lvl)) := as.integer(DiaSemanaNum == lvl)]
  if (!isTRUE(params$tipodia_legacy)) {
    dt[, DiaEspecial_FDS := as.integer(TipoDia == "FDS")]
    dt[, DiaEspecial_FE := as.integer(TipoDia == "FE")]
    dt[, DiaEspecial_FEFDS := as.integer(TipoDia == "FE+FDS")]
  } else dt[, Feriado := as.integer(Feriado)]
  lgbm_add_fimano_dummies(dt, params)
  for (m in 2L:12L) dt[, (paste0("Mes_", m)) := as.integer(lubridate::month(Data) == m)]
  cols <- lgbm_input_cols(params, n_wav)
  if (!all(cols %in% names(dt))) lgbm_abort("lgbm.contract", sprintf("coluna de schema ausente: %s", setdiff(cols, names(dt))[[1L]]))
  dt <- stats::na.omit(dt)
  data.table::setorder(dt, DataHora)
  list(feature_table = dt, sazonalidade = sazon, sazonalidade_fallbacks = fb, colunas_entrada_BLF = cols,
       colunas_nao_normalizar = lgbm_nonorm_cols(params), train_cutoff = cut$train_cutoff, val_cutoff = cut$val_cutoff)
}

## Estagio despachado pela fundacao: assinatura canonica (frame, params). Os cortes vem de attr(frame, "cuts") (anexado pela
## ingestao); o seed nao chega ao estagio (o treino recebe spec$seed): a FE e deterministica, sem consumo de RNG.
plugin_build_features <- function(frame, params) {
  cuts <- attr(frame, "cuts")
  if (is.null(cuts)) lgbm_abort("lgbm.contract", "frame sem attr cuts (ingestao)")
  lgbm_build_features(frame, list(params = params, seed = NULL), cuts)
}
