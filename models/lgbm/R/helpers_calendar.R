## helpers_calendar.R -- rotulos de calendario do plugin LGBM (LGBM.2). SO definicoes (o loader recusa codigo de topo).
## Porte de .add_calendar_cols / .add_fimano_dummies / .adequacao_extra_features do oraculo
## (prevcargadessem3/models/lgbm/R/ingestion/data_adapter.R:386-443 e feature_engineering.R:~300-370, somente leitura).
## Governado so por `params` (as env flags LGBM_* do oraculo viram argumento; o pacote nao le variavel de ambiente).
## Os rotulos nascem AQUI (e nao na ingestao, como no oraculo) para o infer nunca depender do spec do treino.
## Datas so Date civil (sem fuso, sem aritmetica de hora). Saida ASCII.

## Mapa locale-invariante lubridate::wday(week_start = 7) (Dom=1..Sab=7) -> legado (Dom=1, Sab=2, Seg=3..Sex=7).
.lgbm_wday_legacy <- function() c(1L, 3L, 4L, 5L, 6L, 7L, 2L)

## Niveis FIXOS de DiaEspecialFimAno (declarados sempre: o schema do artefato e estavel).
lgbm_fimano_levels <- function() c("Natal_AnoNovo", "Vespera", "PreVespera", "PosNatalAnoNovo", "SemanaNatalAnoNovo",
                                   "PrimeiraSemanaAno")

## TipoDia: 4 niveis (ND/FDS/FE/FE+FDS) ou 5 niveis legados por dia da semana (ANN_PVDS.R:319-329).
lgbm_tipodia <- function(dow_num, fim_de_semana, feriado, legacy) {
  if (isTRUE(legacy)) {
    return(data.table::fcase(
      dow_num == 3L, "segunda-feira",
      dow_num == 7L, "sexta-feira",
      dow_num == 2L, "sabado",
      dow_num == 1L, "domingo",
      default = "Dia util intermediario"))
  }
  data.table::fcase(
    feriado & fim_de_semana, "FE+FDS",
    feriado & !fim_de_semana, "FE",
    fim_de_semana, "FDS",
    default = "ND")
}

## Rotulo de fim de ano a partir de "%m-%d". A ORDEM das atribuicoes e load-bearing: 12-30 entra como PreVespera e e
## SOBRESCRITO por SemanaNatalAnoNovo (efetivo do legado); `prevespera_1230` mantem 12-30 como PreVespera.
lgbm_fimano_label <- function(md, prevespera_1230) {
  out <- rep(NA_character_, length(md))
  out[md %in% c("12-25", "01-01")] <- "Natal_AnoNovo"
  out[md %in% c("12-24", "12-31")] <- "Vespera"
  out[md %in% c("12-23", "12-30")] <- "PreVespera"
  out[md %in% c("12-26", "01-02")] <- "PosNatalAnoNovo"
  semana <- if (isTRUE(prevespera_1230)) sprintf("12-%02d", 27:29) else sprintf("12-%02d", 27:30)
  out[md %in% semana] <- "SemanaNatalAnoNovo"
  out[md %in% sprintf("01-%02d", 3:7)] <- "PrimeiraSemanaAno"
  out[is.na(out)] <- "0"
  out
}

## Colunas de calendario por referencia em `dt` (data.table com Data: Date). `feriados`: vetor Date do calendario completo.
## `params` completo (lgbm_check_params) -- o chamador valida.
lgbm_add_calendar_cols <- function(dt, feriados, params) {
  if (!inherits(dt$Data, "Date")) lgbm_abort("lgbm.contract", "calendario: coluna Data nao e Date")
  if (!inherits(feriados, "Date")) lgbm_abort("lgbm.contract", "calendario: feriados nao e Date")
  dt[, DiaSemanaNum := .lgbm_wday_legacy()[lubridate::wday(Data, week_start = 7L)]]
  dt[, FimDeSemana := DiaSemanaNum %in% c(1L, 2L)]
  dt[, Feriado := Data %in% feriados]
  dt[, DiaEspecial := FimDeSemana | Feriado]
  dt[, TipoDia := lgbm_tipodia(DiaSemanaNum, FimDeSemana, Feriado, params$tipodia_legacy)]
  if (isTRUE(params$calendar_yearend)) {
    md <- format(dt$Data, "%m-%d")
    dt[, DiaEspecialFimAno := lgbm_fimano_label(md, params$prevespera_1230)]
    dt[, PreFeriado := as.integer(Data %in% (feriados - 1L))]
    dt[, PosFeriado := as.integer(Data %in% (feriados + 1L))]
  }
  dt[]
}

## 6 dummies fixas de fim de ano (0 quando ausentes); devolve os nomes criados (vazio com calendar_yearend FALSE).
lgbm_add_fimano_dummies <- function(dt, params) {
  if (!isTRUE(params$calendar_yearend)) return(character(0L))
  if (!("DiaEspecialFimAno" %in% names(dt))) lgbm_abort("lgbm.contract", "dummies de fim de ano sem DiaEspecialFimAno")
  lv <- lgbm_fimano_levels()
  nms <- paste0("DiaEspecial_", lv)
  for (i in seq_along(lv)) dt[, (nms[i]) := as.integer(DiaEspecialFimAno == lv[i])]
  nms
}

## Extras de adequacao na ordem canonica do schema: Feriado (tipodia_legacy), 6 DiaEspecial_<nivel> + PreFeriado +
## PosFeriado (calendar_yearend).
lgbm_adequacao_extra_features <- function(params) {
  c(if (isTRUE(params$tipodia_legacy)) "Feriado" else character(0L),
    if (isTRUE(params$calendar_yearend)) c(paste0("DiaEspecial_", lgbm_fimano_levels()), "PreFeriado", "PosFeriado")
    else character(0L))
}
