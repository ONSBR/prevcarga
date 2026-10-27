## dates.R — camada de datas honesta (M2)
##
## Responsabilidade: derivar os TRÊS cortes a partir do ÚNICO date-flag humano (--as-of). É o ponto
## #1 de honestidade do sistema.
##
## Os três cortes:
##   1. forecast-origin  = --as-of (o "hoje" da rodada). Único flag humano.
##   2. corte-de-treino  = origem − 1 − horizonte, DERIVADO POR MODELO (lê horizon_days via topology.R;
##                         RF C−10, LGBM C−2, HW/dessem --retrain). NUNCA um valor único p/ todos.
##   3. corte-intradiário = available_until, propriedade do SNAPSHOT de dado (manifest.json 'corte').
##                         NUNCA flag humana em produção; backtest pode VARRÊ-lo com --corte.
##
## Gate falsificável (M2): --as-of uniforme NÃO produz corte-de-treino igual entre RF(C−10) e LGBM(C−2).

prevcarga_train_origin <- function(as_of, model) {
  # origem − 1 − horizonte, lendo horizon_days do modelo (topology.R). NUNCA literal uniforme.
  stop("dates.R: prevcarga_train_origin() não implementado — TODO M2")
}

prevcarga_resolve_cuts <- function(as_of, data_dir, model = NULL) {
  # Deriva os 3 cortes: forecast_origin (=as_of, ou último dia COMPLETO do store se NULL —
  # varre data_dir), train_cut (por-modelo, origem−1−horizonte), intraday_cut (lido de
  # <data_dir>/manifest.json 'corte', ADR-0007; NUNCA flag humana).
  stop("dates.R: prevcarga_resolve_cuts() não implementado — TODO M2")
}
