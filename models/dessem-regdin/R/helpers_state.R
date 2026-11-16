## helpers_state.R -- dessem-regdin (E4-T3, ADR-045 fase C). So definicoes, sem estado. Estado DERIVADO que o treino grava no
## artefato para a inferencia nao recalcular nada sobre o historico longo do deck: a serie diaria historica (serie_dm) com o mm
## cru por hora de corte, o perfil de cada dia especial do horizonte (hol_profile: sd, amostra e lm congelados), as colunas do
## perfil normal e a necessidade de dados da inferencia. Nenhuma aritmetica de hora; nenhuma leitura de ambiente.
## Fonte unica do inference_needs (decisao do Gabriel, ADR-045): o estado do artefato. A inferencia NAO le o anexo de feriados
## (holiday_annex = FALSE): a serie do ARIMA e o perfil de feriado vem do artefato; o data_needs do descriptor segue com o anexo
## porque monta o deck de TREINO.

## data.frame puro (sem .internal.selfref nem chave de data.table; Data em Date): o que o scan_pure da fundacao aceita.
.rd_plain_df <- function(x) {
  d <- as.data.frame(x)
  attr(d, ".internal.selfref") <- NULL
  attr(d, "sorted") <- NULL
  if ("Data" %in% names(d)) d$Data <- lubridate::as_date(d$Data)
  d
}

## Digest da serie DM historica (Prev == FALSE; Data, Carga, Exo_*): a MESMA expressao do fingerprint do treino, para que o
## digest do artefato (data.frame congelado) e o do refit sobre a serie composta sejam comparaveis byte a byte.
.rd_dm_hist_digest <- function(serieDM) {
  hist <- data.table::as.data.table(serieDM)[Prev == FALSE, c("Data", "Carga", grep("^Exo", names(serieDM), value = TRUE)),
                                             with = FALSE]
  digest::digest(hist, algo = "sha256")
}

## --- serie_dm ---------------------------------------------------------------------------------------------------------------
## serieDM[Prev == FALSE] do treino (normalizada, a mesma que o ARIMA ve) + mm CRU por hora de corte 0..23 (.rd_mm_core: o que
## rd_morning_means calcula para o corte h). Dia sem hora completa ate o corte fica NA. data.frame puro, Data em Date.
.rd_serie_dm_payload <- function(serieDM, panel, tz) {
  s <- data.table::copy(data.table::as.data.table(serieDM))[Prev == FALSE]
  for (h in 0:23) {
    m <- .rd_mm_core(panel, h, tz)
    data.table::setnames(m, "mm", sprintf("mm_h%02d", h))
    s <- merge(s, m, by = "Data", all.x = TRUE, sort = TRUE)
  }
  .rd_plain_df(s)
}

## serieDM da inferencia: o historico gravado (sem mm_h*) + as linhas Prev == TRUE (O..O+9) do deck curto, ja normalizadas com
## a$norm. Mesmas colunas e mesma ordem do treino; nomes divergentes => artifact_mismatch nomeando o campo.
.rd_compose_serieDM <- function(serie_dm, serieDM_deck) {
  h <- data.table::as.data.table(serie_dm)
  h <- h[, grep("^mm_h[0-9]{2}$", names(h), invert = TRUE, value = TRUE), with = FALSE]
  p <- data.table::as.data.table(serieDM_deck)[Prev == TRUE]
  if (!setequal(names(h), names(p))) rd_artifact_mismatch("serie_dm", sort(names(h)), sort(names(p)))
  out <- rbind(h, p[, names(h), with = FALSE], use.names = TRUE)
  data.table::setorder(out, Data)
  out[]
}

## mm da inferencia: o historico vem de serie_dm$mm_hHH na hora de corte (floor da hora de available_until) e a linha de O, do
## deck curto. Sem linha de O (corte 00:00 de exibicao) devolve vazio: o ARIMA base, sem cabeca, como hoje.
.rd_compose_mm <- function(serie_dm, mm_deck, available_until, O) {
  if (!(O %in% mm_deck$Data)) return(mm_deck[0L])
  hh <- sprintf("mm_h%02d", lubridate::hour(lubridate::floor_date(available_until, "hour")))
  if (!(hh %in% names(serie_dm))) rd_artifact_mismatch("serie_dm", names(serie_dm), hh)
  hist <- data.table::data.table(Data = lubridate::as_date(serie_dm$Data), mm = serie_dm[[hh]])
  out <- rbind(hist[!is.na(mm)], mm_deck[Data == O], use.names = TRUE)
  data.table::setkey(out, Data)
  out[]
}

## --- perfil de feriado -------------------------------------------------------------------------------------------------------
## Config da janela do dia especial (rlist::list.filter do motor sem NSE): o primeiro grupo cujo tipoDiaEsp contem o codigo.
.rd_info_fer <- function(cfg, diaEsp) Filter(function(x) diaEsp %in% x$tipoDiaEsp, cfg$janelaDadosFer)[[1L]]

## Serie denormalizada e padronizada por dia: as 4 primeiras linhas de RL_perfilFeriado (modelos_perfil.R:26-50). Devolve a
## serie, cmed (CargaMed denormalizada do dia) e o que o motor deriva do dia (diaEsp, infoFer, diaAno, HV).
.rd_fer_input <- function(dp, seriePF, cfg, constNorm) {
  serie <- data.table::copy(seriePF)
  serie[, CargaMed := CargaMed * constNorm]
  cmed <- serie[Data == dp]$CargaMed[1L]
  serie[, Carga := Carga * CargaMed]
  serie[, CargaMed := NULL]
  diaEsp <- serie[Data == dp]$DiaEspecial[1L]
  infoFer <- .rd_info_fer(cfg, diaEsp)
  list(serie = serie, cmed = cmed, diaEsp = diaEsp, infoFer = infoFer,
       diaAno = obtemJanelaDiaria(dp, infoFer$janelaDados), HV = serie[Data == dp]$HV[1L])
}

## DataHora e relogio: sai da amostra congelada (o scan_pure recusa POSIXct; a coluna esta em colunaRemoverPerfil e nao entra no lm).
.rd_drop_clock <- function(x) {
  x <- data.table::as.data.table(x)
  if ("DataHora" %in% names(x)) x <- x[, setdiff(names(x), "DataHora"), with = FALSE]
  x
}

## Espelha RL_perfilFeriado ate o lm, sobre a seriePF do TREINO, e grava o que a inferencia precisa para nao tocar no historico:
## sd_hist (obtemSD sobre o historico), a amostra (linhas historicas de entradaPerfilFeriadoReg com Carga nao-NA), coef e xlevels
## do lm, a formula em texto e o que o motor deriva do dia. Sem RNG. A amostra e cronologica (a mesma ordem do deck longo).
.rd_hol_profile <- function(dp, seriePF, cfg, constNorm) {
  inp <- .rd_fer_input(dp, seriePF, cfg, constNorm)
  serie <- inp$serie
  sdHist <- obtemSD(serie[Prev == FALSE], diaAno = inp$diaAno, hv = inp$HV, tpFer = inp$diaEsp)
  padronizaCargaHorariaRef(serie)
  entradamdl <- entradaPerfilFeriadoReg(serie, dslag = inp$infoFer$lagsdiasemana, diaAno = inp$diaAno, hv = inp$HV,
                                        tpFer = inp$diaEsp)
  frm <- getFormula(names(entradamdl[, -cfg$colunaRemoverPerfil, with = FALSE]))
  O <- min(serie[Prev == TRUE]$Data)
  smp <- entradamdl[Data < O & !is.na(Carga)]
  mdl <- stats::lm(frm, smp)
  list(sd_hist = sdHist, sample = .rd_plain_df(.rd_drop_clock(smp)), coef = stats::coef(mdl), xlevels = mdl$xlevels,
       contrasts = mdl$contrasts, formula = paste(deparse(frm, width.cutoff = 500L), collapse = ""),
       diaAno = inp$diaAno, HV = inp$HV, dslag = inp$infoFer$lagsdiasemana, diaEsp = inp$diaEsp)
}

## Substitui RL_perfilFeriado na inferencia (mesma saida: reescreve seriePF[Data == dp] com Carga e CargaMed). Nunca chama obtemSD:
## o sd vem congelado. Previsao:
##  - nenhuma linha do horizonte entra na amostra do lm: coef congelado, a mesma conta do predict.lm;
##  - um feriado anterior do mesmo codigo foi escrito de volta na serie (feriados consecutivos): o lm e re-ajustado sobre
##    rbind(amostra congelada, linhas elegiveis do horizonte) - exatamente as linhas que o deck longo usaria.
rd_profile_holiday <- function(dp, seriePF, cfg, constNorm, hp, O) {
  d <- lubridate::as_date(dp)
  if (is.null(hp) || !all(c("sd_hist", "sample", "coef", "xlevels", "formula", "diaAno", "HV", "dslag", "diaEsp") %in% names(hp)))
    rd_artifact_mismatch("hol_profile", paste(names(hp), collapse = ","), dp)
  serie <- data.table::copy(seriePF)
  serie[, CargaMed := CargaMed * constNorm]
  cmed <- serie[Data == dp]$CargaMed[1L]
  serie[, Carga := Carga * CargaMed]
  serie[, CargaMed := NULL]
  if (!identical(as.integer(serie[Data == dp]$DiaEspecial[1L]), as.integer(hp$diaEsp)))
    rd_artifact_mismatch("hol_profile", sprintf("%s: codigo do deck=%s", dp, format(serie[Data == dp]$DiaEspecial[1L])),
                         sprintf("codigo do artefato=%s", format(hp$diaEsp)))
  padronizaCargaHorariaRef(serie)
  entradamdl <- .rd_drop_clock(entradaPerfilFeriadoReg(serie, dslag = hp$dslag, diaAno = hp$diaAno, hv = hp$HV, tpFer = hp$diaEsp))
  smp <- data.table::as.data.table(hp$sample)
  miss <- setdiff(names(smp), names(entradamdl))
  if (length(miss)) rd_artifact_mismatch("hol_profile", paste(miss, collapse = ","), paste(names(entradamdl), collapse = ","))
  frm <- stats::as.formula(hp$formula); environment(frm) <- baseenv()
  row <- entradamdl[Data == d]
  elig <- entradamdl[Data >= O & Data != d & !is.na(Carga)][, names(smp), with = FALSE]
  infer <- if (!nrow(elig)) {
    Terms <- stats::delete.response(stats::terms(frm))
    mf <- stats::model.frame(Terms, row, na.action = stats::na.pass, xlev = hp$xlevels)
    X <- stats::model.matrix(Terms, mf, contrasts.arg = hp$contrasts)
    if (!identical(colnames(X), names(hp$coef)))
      rd_artifact_mismatch("hol_profile", paste(colnames(X), collapse = ","), paste(names(hp$coef), collapse = ","))
    piv <- !is.na(hp$coef)
    drop(X[, piv, drop = FALSE] %*% hp$coef[piv])               # mesma conta de predict.lm
  } else {
    mdl <- stats::lm(frm, rbind(smp, elig, use.names = TRUE))
    stats::predict(mdl, row)
  }
  infer <- infer * hp$sd_hist + cmed
  cmedn <- mean(infer)
  infer <- infer / cmedn
  seriePF[Data == dp, `:=`(Carga = infer, CargaMed = cmedn / constNorm)]
}

## --- colunas do perfil normal -----------------------------------------------------------------------------------------------
## Linha de entrada do perfil normal do dia dp e hora h: a MESMA chamada do SVMhorario (treino) e do passe de predict.
.rd_profile_row <- function(seriePF, cfg, dp, h) {
  cp <- obterDiaEspPerfilConfig(dp, seriePF, cfg)
  entradaPerfil(serie = seriePF, tipoEntrada = cp$tpEntrada, diaSemana = cp$ds, hora = h, diaAno = cp$diasAno,
                lagsmodelo = cp$hrlag, hv = seriePF[Data == dp]$HV[1L], lagsdsFer = cp$lagds, tpFer = cp$diaEsp,
                addHourFac = FALSE)[Data == dp]
}

## Descreve uma linha: nomes na ordem e niveis das colunas fator.
.rd_row_cols <- function(row) {
  fac <- Filter(is.factor, as.list(row))
  list(vars = names(row), levels = lapply(fac, levels))
}

## Colunas (preditores e niveis de fator) da linha do perfil normal por (dia, hora) que o treino enxergou. Dia especial nao tem
## SVM (perfil recomputado): fica fora. Forma: list(<dp> = list(<h> = list(vars, levels))). Nao consome RNG.
rd_columns_of <- function(features) {
  cfg <- features$config_mdl; sp <- features$series$seriePF
  out <- lapply(features$dias, function(dp) {
    dt <- features$day_types[[dp]]
    if (!is.na(dt) && dt != 0L) return(NULL)
    stats::setNames(lapply(0:23, function(h) .rd_row_cols(.rd_profile_row(sp, cfg, dp, h))), as.character(0:23))
  })
  stats::setNames(out, features$dias)
}

## Alinha a linha de predicao do deck curto as colunas gravadas no treino. Recomposicoes (as do E4-T2, so quando a coluna
## gravada falta na linha): LAG_FER_k = indicador as.numeric(DiaEspecial > 0) do dia d-k (o que obtemLag calcula; NAO o
## codigo) e DiaEspecial = fator com os niveis gravados. Qualquer outra coluna ausente ou sobrando => artifact_mismatch
## `columns` (exit 2). Devolve a linha na ordem de cols$vars.
rd_align_row <- function(row, cols, serie, dp, what) {
  bad <- function(miss, extra)
    rd_abort("plugin.regdin_artifact_mismatch", where = "dessem-regdin/align_row",
             reason = sprintf("columns %s: faltando=%s sobrando=%s", what, paste(miss, collapse = ","), paste(extra, collapse = ",")),
             details = list(field = "columns", linha = what, faltando = miss, sobrando = extra))
  if (is.null(cols) || is.null(cols$vars)) bad("(sem colunas gravadas)", character())
  row <- data.table::copy(data.table::as.data.table(row)); d <- lubridate::as_date(dp)
  for (v in setdiff(grep("^LAG_FER_[0-9]+$", cols$vars, value = TRUE), names(row))) {
    k <- as.integer(sub("^LAG_FER_", "", v))
    row[, (v) := as.numeric(serie[Data == d - k]$DiaEspecial[1L] > 0)]
  }
  lv <- cols$levels$DiaEspecial
  if ("DiaEspecial" %in% cols$vars && !is.null(lv) && !("DiaEspecial" %in% names(row)))
    row[, DiaEspecial := factor(paste0("F", sprintf("%02d", as.integer(serie[Data == d]$DiaEspecial[1L]))), levels = lv)]
  miss <- setdiff(cols$vars, names(row)); extra <- setdiff(names(row), cols$vars)
  if (length(miss) || length(extra)) bad(miss, extra)
  row[, cols$vars, with = FALSE]
}

## --- inference_needs --------------------------------------------------------------------------------------------------------
## Necessidade de dados da inferencia no formato de scripts/lib/inference_needs.R (schema_version 1), derivada do config_mdl:
## carga e observado = max(lagsPerfil em horas) / 24 dias de lag + 1 de aquecimento antes de O (o dia O ate available_until e
## do deck); previsto O..O+H (freshest, H = dias entregues); calendario O-n..O+H. O estado (serie, perfil de feriado, colunas)
## vem do artefato, entao o anexo de feriados NAO entra (holiday_annex = FALSE).
## clima_hist = previsto_lead0 (E5-T4): o previsto dos dias -n..-1 entra como segmento min_lead (lead 0), a regra de exo_hist_lead0.
rd_inference_needs <- function(config_mdl, area, horizon_days = plugin_constants()$horizon_days, clima_hist = "observado") {
  n <- as.integer(ceiling(max(config_mdl$lagsPerfil) / plugin_constants()$hours_per_day)) + 1L
  H <- as.integer(horizon_days)
  segs <- list(list(kind = "freshest", from = 0L, to = H, lead = NA_integer_))
  if (identical(clima_hist, "previsto_lead0"))
    segs <- c(segs, list(list(kind = "min_lead", from = -n, to = -1L, lead = 0L)))
  list(schema_version = 1L, load = list(days_before = n), weather_observed = list(days_before = n),
       weather_forecast = list(area = area, segments = segs),
       holidays = list(from = -n, to = H), holiday_annex = FALSE)
}
