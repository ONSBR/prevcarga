## helpers_norm.R -- normalizacao min-max do plugin LGBM (LGBM.3). SO definicoes.
## Porte de .compute_min_max / .normalise_dt (train_lgbm.R:93-140) e .norm / .denorm (inference.R:90-116) do oraculo
## (somente leitura). O escopo e fixo no treino (plugin_constants()$normalization_scope = "train"): min/max so das
## linhas Data <= train_cutoff, congelados no artefato (min_max_BLF). Sem aritmetica de hora; saida ASCII.

## min/max por coluna so das linhas de treino. Coluna toda NA => c(min = 0, max = 1) com aviso.
lgbm_compute_min_max <- function(dt, cols, train_cutoff) {
  if (!inherits(train_cutoff, "Date") || length(train_cutoff) != 1L || is.na(train_cutoff))
    lgbm_abort("lgbm.contract", "train_cutoff ausente ou nao Date na normalizacao")
  fit <- dt[dt$Data <= train_cutoff, ]
  lapply(stats::setNames(cols, cols), function(col) {
    v <- fit[[col]]
    v <- v[!is.na(v)]
    if (!length(v)) {
      warning(sprintf("lgbm: coluna %s toda NA no treino", col), call. = FALSE)
      return(c(min = 0, max = 1))
    }
    c(min = min(v), max = max(v))
  })
}

## (x - min) / (max - min) por referencia; amplitude 0 => 0. So as colunas de min_max presentes em dt.
lgbm_normalise_dt <- function(dt, min_max) {
  for (col in intersect(names(min_max), names(dt))) {
    mn <- min_max[[col]][["min"]]
    rng <- min_max[[col]][["max"]] - mn
    if (rng == 0) data.table::set(dt, j = col, value = 0)
    else data.table::set(dt, j = col, value = (dt[[col]] - mn) / rng)
  }
  invisible(dt)
}

## Escalares/vetores: sempre com o min_max_BLF do artefato (nunca recomputado no infer).
lgbm_norm <- function(x, col, min_max) {
  mm <- min_max[[col]]
  rng <- mm[["max"]] - mm[["min"]]
  if (rng == 0) return(rep(0, length(x)))
  (x - mm[["min"]]) / rng
}

lgbm_denorm <- function(x, col, min_max) {
  mm <- min_max[[col]]
  x * (mm[["max"]] - mm[["min"]]) + mm[["min"]]
}
