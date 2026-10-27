## evaluation.R — fonte única de métrica (M3)
##
## Responsabilidade: ser a ÚNICA definição de métrica do repo. Na referência, WMAPE está definido em
## ≥4 lugares divergentes (metrics.R, combina_baseline.py, reconcilia_baseline.R, backtest inline) —
## aqui colapsa em um só, consumido por score, backtest e forecast --report (senão os scorecards discordam).
##
## Expõe: wmape/mape/mae/rmse/bias, peak/ramp/timing, resto-de-hoje (d_plus=0 pós-corte), e
## SKILL-SCORE vs baseline (persistência / sazonal-ingênuo D−7) — a referência não tem régua hoje.
## Sempre horizon-aware (por d_plus) e por área/subsistema/SIN.
##
## Gate (M3): uma única definição de WMAPE (grep prova); score e backtest batem no mesmo input.

prevcarga_wmape <- function(y_true, y_pred, weights = NULL) {
  stop("evaluation.R: prevcarga_wmape() não implementado — TODO M3")
}

prevcarga_scorecard <- function(long, by = c("d_plus", "area"), baseline = NULL) {
  stop("evaluation.R: prevcarga_scorecard() não implementado — TODO M3")
}
