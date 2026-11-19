## helpers_state.R -- dessem-linear (E4-T2, ADR-045 fase B). So definicoes, sem estado. Estado DERIVADO que o treino grava no
## artefato para a inferencia nao recalcular nada sobre o historico longo do deck: amostra de treino da cabeca DM(O) com o mm
## cru por hora de corte, formula, colunas (preditores e niveis de fator) por dia e por (dia, hora) e a necessidade de dados
## da inferencia. Nenhuma aritmetica de hora alem de make_datetime com tz explicito (a hora de corte vem do dado).

## Instante h:00 do dia O no frame de modelagem (tz explicito).
dl_cut_at <- function(O, tz, h)
  lubridate::make_datetime(lubridate::year(O), lubridate::month(O), lubridate::mday(O), hour = as.integer(h), tz = tz)

## mm CRU (sem normalizar) de cada dia historico (< O) para cada hora de corte 0..23: tabela larga Data + mm_h00..mm_h23.
## Dia sem hora completa ate o corte fica NA. A regra de cada coluna e a do dl_morning_means (mesma da inferencia).
dl_mm_by_cut <- function(hourly_load, O, tz) {
  out <- data.table::data.table(Data = sort(unique(lubridate::as_date(hourly_load$interval_end))))
  for (h in 0:23) {
    m <- dl_morning_means(hourly_load, dl_cut_at(O, tz, h), O)
    data.table::setnames(m, "mm", sprintf("mm_h%02d", h))
    out <- merge(out, m, by = "Data", all.x = TRUE)
  }
  out[Data < O]
}

## Amostra de treino da cabeca DM(O) (as linhas que tune_wsvm ve hoje) + mm cru por hora de corte; data.frame puro, Data em
## Date. `formula` = texto da formula do motor SEM mm. Nao consome RNG; roda antes de .dl_fit_pass.
dl_dm_o_sample <- function(f) {
  eng <- dl_engine(); O <- f$deck$dataPrev
  inp <- eng$obterEntradaFormulaDM(format(O), f$serieDM, f$config_mdl)
  tr <- eng$filtraDadosTreinamento(inp$dadosEntrada)
  s <- merge(tr, dl_mm_by_cut(f$deck$hourly_load, O, f$deck$tz), by = "Data", all.x = TRUE)
  s <- as.data.frame(s)
  s$Data <- lubridate::as_date(s$Data)
  attr(s, "sorted") <- NULL
  list(sample = s, formula = paste(deparse(inp$formulaMDL, width.cutoff = 500L), collapse = ""))
}

## Descreve uma linha: nomes na ordem e niveis das colunas fator.
.dl_row_cols <- function(row) {
  fac <- Filter(is.factor, as.list(row))
  list(vars = names(row), levels = lapply(fac, levels))
}

## Colunas (preditores e niveis de fator) da linha de DM por dia dp de O..O+8 e da linha de perfil por (dp, h): o que o
## treino enxergou. Forma: list(dm = list(<dp> = list(vars, levels)), perfil = list(<dp> = list(<h> = list(vars, levels)))).
dl_columns_of <- function(f) {
  eng <- dl_engine(); cfg <- f$config_mdl
  dias <- format(f$processado$DIASPREV)
  dm <- lapply(stats::setNames(dias, dias), function(dp)
    .dl_row_cols(eng$obterEntradaFormulaDM(dp, f$serieDM, cfg)$dadosEntrada$serieDiariaModelo[Data == lubridate::as_date(dp)]))
  perfil <- lapply(stats::setNames(dias, dias), function(dp)
    stats::setNames(lapply(0:23, function(h) .dl_row_cols(.dl_profile_row(eng, f$seriePF, cfg, dp, h))), as.character(0:23)))
  list(dm = dm, perfil = perfil)
}

## Alinha a linha de predicao do deck curto as colunas gravadas no treino. Tres recomposicoes (as duas do plano mais as dummies, achadas pelo gate m5): LAG_FER_k ausente =
## as.numeric(DiaEspecial > 0) do dia d-k na serie (indicador 0/1 de obtemLag, funcoes_apoio.R:11; antes do corte de amostra pequena) e DiaEspecial
## ausente (o deck curto o derruba quando so resta um codigo) = fator com os niveis gravados; mais as dummies one-hot F/D/M da
## entrada ML_DEFAULT (dia especial na DM), exatas por construcao (ver abaixo). Qualquer outra coluna ausente
## ou sobrando => plugin.dessem_linear_artifact_mismatch field columns. Devolve a linha na ordem de cols$vars.
dl_align_row <- function(row, cols, serie, dp, what) {
  bad <- function(miss, extra)
    dl_abort("plugin.dessem_linear_artifact_mismatch", where = "dessem-linear/align_row",
             reason = sprintf("columns %s: faltando=%s sobrando=%s", what, paste(miss, collapse = ","),
                              paste(extra, collapse = ",")),
             details = list(field = "columns", linha = what, faltando = miss, sobrando = extra))
  if (is.null(cols) || is.null(cols$vars)) bad("(sem colunas gravadas)", character())
  row <- data.table::copy(data.table::as.data.table(row)); d <- lubridate::as_date(dp)
  for (v in setdiff(grep("^LAG_FER_[0-9]+$", cols$vars, value = TRUE), names(row))) {
    k <- as.integer(sub("^LAG_FER_", "", v))
    row[, (v) := as.numeric(serie[Data == d - k]$DiaEspecial[1L] > 0)]   # indicador 0/1, igual a obtemLag (funcoes_apoio.R:11)
  }
  lv <- cols$levels$DiaEspecial
  if ("DiaEspecial" %in% cols$vars && !is.null(lv) && !("DiaEspecial" %in% names(row))) {
    code <- serie[Data == d]$DiaEspecial[1L]
    row[, DiaEspecial := factor(paste0("F", sprintf("%02d", as.integer(code))), levels = lv)]
  }
  ## dummies one-hot da entrada ML_DEFAULT (DM de dia especial): F<tipo>, D<dia da semana>, M<mes>. O dcast do motor so cria a coluna
  ## dos valores presentes na serie, entao o deck curto nao as tem todas; o valor numa linha unica e 1 se o proprio dia casa, 0 senao.
  code <- serie[Data == d]$DiaEspecial[1L]
  own <- c(if (!is.na(code)) paste0("F", sprintf("%02d", as.integer(code))), paste0("D", lubridate::wday(d)),
           paste0("M", sprintf("%02d", lubridate::month(d))))
  dummy <- function(v) grepl("^F[0-9]{2}$|^D[1-7]$|^M[0-9]{2}$", v)
  for (v in setdiff(cols$vars[dummy(cols$vars)], names(row))) row[, (v) := as.integer(v %in% own)]
  for (v in setdiff(names(row)[dummy(names(row))], cols$vars))                       # dummy desconhecida: so some se for 0
    if (isTRUE(row[[v]] == 0L)) row[, (v) := NULL]
  miss <- setdiff(cols$vars, names(row)); extra <- setdiff(names(row), cols$vars)
  if (length(miss) || length(extra)) bad(miss, extra)
  row[, cols$vars, with = FALSE]
}

## Necessidade de dados da inferencia no formato de scripts/lib/inference_needs.R (schema_version 1), derivada do config_mdl:
## carga e observado = max(lagsPerfil em horas) / 24 dias de lag + 1 de aquecimento antes de O (o dia O ate available_until
## e do deck); previsto O..O+H (freshest, H = dias entregues); calendario O-n..O+H. O estado (amostra, formulas e colunas)
## vem do artefato, entao o anexo de feriados nao entra.
## clima_hist = previsto_lead0 (E5-T4): o historico do clima (lags) vem do previsto de lead 0; o previsto dos dias -n..-1 entra
## como segmento min_lead (lead 0), a mesma regra de exo_hist_lead0. Estado do artefato = fonte unica (sem o modo, nada muda).
dl_inference_needs <- function(config_mdl, area, horizon_days = plugin_constants()$horizon_days, clima_hist = "observado") {
  n <- as.integer(ceiling(max(config_mdl$lagsPerfil) / plugin_constants()$hours_per_day)) + 1L
  H <- as.integer(horizon_days)
  segs <- list(list(kind = "freshest", from = 0L, to = H, lead = NA_integer_))
  if (identical(clima_hist, "previsto_lead0"))
    segs <- c(segs, list(list(kind = "min_lead", from = -n, to = -1L, lead = 0L)))
  list(schema_version = 1L, load = list(days_before = n), weather_observed = list(days_before = n),
       weather_forecast = list(area = area, segments = segs),
       holidays = list(from = -n, to = H), holiday_annex = FALSE)
}
