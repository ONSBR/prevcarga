## helpers_loess.R -- suavizacao LOESS do plugin LGBM (LGBM.2). SO definicoes.
## O flag `disable_loess` chega por ARGUMENTO (spec no treino, artefato no infer); substitui o .loess_off() /
## Sys.getenv("LGBM_DISABLE_LOESS") do oraculo. Porte de feature_engineering.R:445-480 e inference.R:118-130.

## LOESS grau 2 em `horas` (Hora/2) por dia: so com >= 5 nao-NA; erro => devolve o cru; NA fica NA (na.exclude).
lgbm_loess_daily <- function(vals, horas, span, disable_loess) {
  if (isTRUE(disable_loess) || sum(!is.na(vals)) < 5L) return(vals)
  tryCatch(
    as.numeric(suppressWarnings(stats::predict(stats::loess(vals ~ horas, span = span, degree = 2L,
                                                            na.action = stats::na.exclude)))),
    error = function(e) vals)
}

## LOESS grau 1 sobre seq_along(x), `$fitted`; identidade com `disable_loess`, n < 5 ou < 5 nao-NA.
lgbm_loess_smooth <- function(x, span, disable_loess) {
  n <- length(x)
  if (isTRUE(disable_loess) || n < 5L || sum(!is.na(x)) < 5L) return(x)
  ok <- !is.na(x)
  idx <- seq_len(n)
  tryCatch({
    fit <- suppressWarnings(stats::loess(x[ok] ~ idx[ok], span = span, degree = 1L))
    out <- x
    out[ok] <- as.numeric(fit$fitted)
    out
  }, error = function(e) x)
}
