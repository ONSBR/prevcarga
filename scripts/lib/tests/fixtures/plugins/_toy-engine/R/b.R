## Motor-brinquedo, parte B: usa engine_base() definido em a.R (a ordem de carga importa).
engine_predict <- function(model, x) rep(model$level + engine_base() * 0, length(x))
