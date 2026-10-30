## inference.R -- estagio de inferencia BLF de duas passadas do plugin LGBM (LGBM.4). SO definicoes.
## Porte de infer_single_origin (prevcargadessem3/models/lgbm/R/inference/inference.R:200-475, somente leitura), 100% a partir
## do artefato (G-ART): nenhum refit, nenhum `params`/`spec`, nada do ambiente. Passada 1 (Stage A preve a forma de 48
## intervalos de O; Stage B completa O com a carga real ate available_until + razoes do Stage A) e Passada 2 (O+1: so o lag
## CargaGlobal_anterior_BLF e sobrescrito com o dia O completado; CarWav/TempWav/Sazonalidade ficam como
## plugin_inference_features entregou -- CONSERTO A do oraculo, que NAO re-extrai as ondaletas).
## Sem aritmetica de hora: o corte real e o is_synthetic da ingestao (interval_end > available_until); a entrega usa a
## grade da fundacao (prevcarga_delivery_grid) casada POR POSICAO do slot. Saida ASCII.

.lgbm_check_features <- function(cols, dt, where) {
  miss <- setdiff(cols, names(dt))
  if (length(miss))
    lgbm_abort("lgbm.contract", sprintf("%s: %d feature(s) do modelo ausente(s): %s", where, length(miss),
                                        paste(miss, collapse = ", ")))
  invisible(TRUE)
}

## Passada 1 / Stage A: 48 previsoes normalizadas da forma do dia O (gate07: sem reciclagem).
lgbm_stage_a <- function(booster, X_D, best_iter) {
  if (!is.matrix(X_D) || nrow(X_D) != 48L)
    lgbm_abort("lgbm.contract", sprintf("gate07: matriz do Stage A com %d linhas (esperado 48)", NROW(X_D)))
  p <- booster$predict(X_D, num_iteration = best_iter)
  if (length(p) != 48L)
    lgbm_abort("lgbm.contract", sprintf("gate07: Stage A devolveu %d previsoes (esperado 48)", length(p)))
  p
}

## Passada 1 / Stage B: completa O. Intervalo real = carga presente e fora do trecho sintetico (interval_end <=
## available_until, decidido pela ingestao); o resto = anterior * razao do Stage A. Devolve o perfil MW (lag suavizado
## conforme disable_loess do artefato, span_lag/grau 2 em Hora/2).
lgbm_stage_b <- function(pred_A, dt_D, artifact, consts = plugin_constants()) {
  key <- artifact$target_col
  mm <- artifact$min_max_BLF
  real <- !is.na(dt_D$CargaGlobal) & !dt_D$is_synthetic
  if (!any(real))
    warning("lgbm: sem carga real no dia O; Stage B usa so as razoes do Stage A", call. = FALSE)
  rn <- lgbm_norm(dt_D$CargaGlobal, key, mm)
  ratio <- round(c(1, pred_A[-1L] / pmax(abs(pred_A[-48L]), 1e-9)), 7L)
  out <- numeric(48L)
  for (i in seq_len(48L))
    out[i] <- if (real[i]) rn[i] else (if (i > 1L) out[i - 1L] else pred_A[i]) * ratio[i]
  mw <- lgbm_denorm(out, key, mm)
  lgbm_loess_daily(mw, dt_D$Hora / 2.0, consts$span_lag, artifact$params$disable_loess)
}

## Passada 2: sobrescreve so o lag de O+1 com o dia O completado; normaliza com o min_max_BLF do artefato; LOESS inline
## (span_inline, grau 1) sobre o normalizado e por dia (span_perday) sobre o MW.
lgbm_predict_d1 <- function(booster, dt_D1, completed_D, artifact, consts = plugin_constants()) {
  off <- artifact$params$disable_loess
  d1 <- data.table::copy(dt_D1)
  data.table::setorder(d1, Hora)
  if (nrow(d1) != 48L)
    lgbm_abort("lgbm.contract", sprintf("gate07: dt_D1 com %d linhas (esperado 48)", nrow(d1)))
  d1[, CargaGlobal_anterior_BLF := completed_D]
  lgbm_normalise_dt(d1, artifact$min_max_BLF)
  .lgbm_check_features(artifact$colunas_entrada_BLF, d1, "Passada 2")
  X <- as.matrix(d1[, artifact$colunas_entrada_BLF, with = FALSE])
  p <- booster$predict(X, num_iteration = artifact$best_iter)
  if (length(p) != 48L)
    lgbm_abort("lgbm.contract", sprintf("gate07: Passada 2 devolveu %d previsoes (esperado 48)", length(p)))
  p <- lgbm_loess_smooth(p, consts$span_inline, off)
  lgbm_loess_smooth(lgbm_denorm(p, artifact$target_col, artifact$min_max_BLF), consts$span_perday, off)
}

## Entrega: a grade da fundacao (available_until + 30 min ate O+delivery_days 00:00) casada por POSICAO. O interval_end do
## slot k (1..96 sobre O e O+1) e o interval_start do slot k+1; o ultimo e a meia-noite civil de O+2. Sem somar minutos.
lgbm_delivery <- function(cuts, dt_D, dt_D1, completed_D, d1) {
  grid <- .lgbm_delivery_grid(cuts)
  tz <- cuts$tz
  d2 <- dt_D1$Data[[1L]] + 1L
  O2 <- lubridate::make_datetime(lubridate::year(d2), lubridate::month(d2), lubridate::mday(d2), tz = tz)
  ends <- c(dt_D$DataHora[-1L], dt_D1$DataHora, O2)
  k <- match(grid, ends)
  if (anyNA(k) || length(ends) != 96L)
    lgbm_abort("lgbm.contract", "grade de entrega fora dos slots de O e O+1")
  y <- c(completed_D, d1)[k]
  data.table::data.table(interval_end = grid, y_pred = as.numeric(y))
}

## A grade e da fundacao (features.R); o plugin so a consome (acesso interino como errors.R: .lgbm_foundation).
.lgbm_delivery_grid <- function(cuts) {
  f <- .lgbm_foundation("prevcarga_delivery_grid")
  if (is.null(f)) lgbm_abort("lgbm.contract", "prevcarga_delivery_grid indisponivel (fundacao)")
  f(cuts, plugin_descriptor()$delivery_days)
}

## Estagio despachado: (payload, inf_feat, cuts). `cuts` ja no frame do modelo (offset 0 no LGBM).
plugin_infer <- function(artifact, inf_feat, cuts) {
  art <- plugin_thaw(artifact, cuts)
  lgbm_check_artifact_params(art)
  consts <- plugin_constants()
  dt_D <- data.table::copy(inf_feat$dt_D)
  dt_D1 <- inf_feat$dt_D1
  data.table::setorder(dt_D, Hora)
  if (nrow(dt_D) != 48L || nrow(dt_D1) != 48L)
    lgbm_abort("lgbm.contract", sprintf("gate07: dt_D com %d e dt_D1 com %d linhas (esperado 48)", nrow(dt_D), nrow(dt_D1)))
  data.table::setorder(dt_D1, Hora)
  dn <- data.table::copy(dt_D)
  lgbm_normalise_dt(dn, art$min_max_BLF)
  .lgbm_check_features(art$colunas_entrada_BLF, dn, "Stage A")
  X_D <- as.matrix(dn[, art$colunas_entrada_BLF, with = FALSE])
  pred_A <- lgbm_stage_a(art$booster, X_D, art$best_iter)
  completed_D <- lgbm_stage_b(pred_A, dt_D, art, consts)
  d1 <- lgbm_predict_d1(art$booster, dt_D1, completed_D, art, consts)
  delivery <- lgbm_delivery(cuts, dt_D, dt_D1, completed_D, d1)
  if (!all(is.finite(delivery$y_pred)))
    lgbm_abort("lgbm.contract", "y_pred nao finito na entrega (feature ausente ou NA na matriz)")
  day_full <- all(!is.na(dt_D$CargaGlobal) & !dt_D$is_synthetic)
  values <- if (day_full) matrix(d1, nrow = 1L) else rbind(completed_D, d1)
  dimnames(values) <- NULL
  list(area = inf_feat$area, values = values, delivery = delivery,
       meta = list(best_iter = as.integer(art$best_iter), params_source = "artifact", n_real_D = as.integer(inf_feat$n_real_D)))
}
