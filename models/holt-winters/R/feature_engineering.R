## feature_engineering.R -- estagio de feature engineering do plugin Holt-Winters (HW.2). SO definicoes.
## Porte de prevcargadessem3/models/holt-winters/R/feature_engineering/feature_engineering.R (somente leitura): os 7
## passos do oraculo, na ordem do oraculo, sobre o deck interno de plugin_ingest():
##   (1) Corrige.Dias.Faltantes  (2) .preconsist_ctm  (3) Corrige.Dados  (4) MO1 (set.seed(seed) imediatamente antes)
##   -> snapshot CargaTempVerFer/CargaTempMed1  (5) Substitui.Feriados  (6) Suaviza.LOESS em T01..T48
##   (7) recomputo de CargaTempMed[,4:5].
## Os 7 params (ic, perfil_mo, classes_mo, similar, feriado, kmeans_seed) vem SO do run.yaml, sem default no codigo.
## O FE roda 1x por treino. Sem relogio, sem ambiente, sem hora; datas so por lubridate. Saida: ASCII puro.

## Aborto de contrato por passo cujo flag de erro (rv[[2]]) veio TRUE; devolve o resultado (rv[[1]]) caso contrario.
.hw_ok <- function(rv, passo) {
  if (isTRUE(rv[[2L]]))
    hw_abort("hw.fe_flag", sprintf("passo %s devolveu flag de erro", passo), list(step = passo),
             where = "holt-winters/feature_engineering")
  rv[[1L]]
}

## Acessor estrito de params.<name>: sem default; tipo/enum/faixa vem do params_schema do descriptor (allowed, min, max).
## Faixa numerica: `min` e exclusivo para tipo numeric (similar em (0, 1]) e inclusivo para integer (classes_mo >= 1).
.hw_param <- function(params, name) {
  v  <- params[[name]]
  sc <- plugin_descriptor()$params_schema[[name]]
  ok <- !is.null(sc) && !is.null(v) && length(v) == 1L && !is.na(v) &&
    switch(sc$type, integer = is.numeric(v) && v == round(v), numeric = is.numeric(v) && is.finite(v),
           character = is.character(v), logical = is.logical(v), FALSE)
  if (ok && !is.null(sc$allowed)) ok <- v %in% sc$allowed
  if (ok && !is.null(sc$min)) ok <- if (identical(sc$type, "numeric")) v > sc$min else v >= sc$min
  if (ok && !is.null(sc$max)) ok <- v <= sc$max
  if (!ok) .hw_bad_param(paste0("params.", name), "holt-winters/feature_engineering")
  v
}

## `spec` do ticket: lista com `params` e `seed` (modelo spec do run.yaml). O despachante P.0c entrega so spec$params
## (a seed vai ao estagio de treino); nesse caso nao ha seed a fixar aqui e o MO1 depende so de params.kmeans_seed
## (classifica_kmeans re-semeia o gerador).
.hw_spec_parts <- function(spec) {
  if (!is.list(spec)) .hw_bad_param("params", "holt-winters/feature_engineering")
  if (is.list(spec[["params"]])) {
    s0 <- spec[["seed"]]
    if (length(s0) != 1L || is.na(s0) || !is.numeric(s0)) .hw_bad_param("seed", "holt-winters/feature_engineering")
    return(list(params = spec[["params"]], seed = as.integer(s0)))
  }
  list(params = spec, seed = NULL)
}

## Pre-consistencia da CargaTempMed (PrevisaoDiaria.R:60-92 do oraculo; feature_engineering.R:256). Limites de
## Cmed/Tmed em plugin_constants().
.preconsist_ctm <- function(ctm) {
  k <- plugin_constants()
  nLIcarga <- k$cmed_bounds[[1L]]
  nLScarga <- k$cmed_bounds[[2L]]
  nLItemp  <- k$tmed_bounds[[1L]]
  nLStemp  <- k$tmed_bounds[[2L]]

  for (fh in 1:dim(ctm)[1]) {
    if (is.na(ctm[fh, 2])) { ctm[fh, 2] <- 0 }
    if (is.na(ctm[fh, 3])) {
      if (fh > 1) {
        if (ctm[(fh - 1), 3] == 7) {
          ctm[fh, 3] <- 1
        } else {
          ctm[fh, 3] <- as.numeric(ctm[(fh - 1), 3]) + 1
        }
      } else {
        ctm[fh, 3] <- 1
      }
    }
    if (is.na(ctm[fh, 6])) { ctm[fh, 6] <- 0 }

    if ((is.na(ctm[fh, 4])) | (ctm[fh, 4] > nLScarga) | (ctm[fh, 4] < nLIcarga)) {
      if (fh <= 7) {
        look_fwd <- fh + 7
        if (look_fwd <= dim(ctm)[1] && !is.na(ctm[look_fwd, 4])) {
          ctm[fh, 4] <- ctm[look_fwd, 4]
        } else {
          ctm[fh, 4] <- mean(ctm[, 4], na.rm = TRUE)
        }
      } else {
        ctm[fh, 4] <- ctm[(fh - 7), 4]
      }
    }

    if ((is.na(ctm[fh, 5])) | (ctm[fh, 5] > nLStemp) | (ctm[fh, 5] < nLItemp)) {
      if (fh <= 7) {
        look_fwd <- fh + 7
        if (look_fwd <= dim(ctm)[1] && !is.na(ctm[look_fwd, 5])) {
          ctm[fh, 5] <- ctm[look_fwd, 5]
        } else {
          ctm[fh, 5] <- mean(ctm[, 5], na.rm = TRUE)
        }
      } else {
        ctm[fh, 5] <- ctm[(fh - 7), 5]
      }
    }
  }
  ctm
}

## Passo 7 (feature_engineering.R:562-586 do oraculo): alinha as duas tabelas pela intersecao das datas quando as
## linhas divergem e recalcula Cmed/Tmed da CargaTempVer ja limpa. Devolve list(ctv, ctm).
.hw_recompute_ctm <- function(ctv, ctm) {
  if (nrow(ctm) != nrow(ctv)) {
    dates_ctv <- ctv[, 1]
    dates_ctm <- ctm[, 1]
    common    <- intersect(dates_ctv, dates_ctm)
    ctv <- ctv[dates_ctv %in% common, ]
    ctm <- ctm[dates_ctm %in% common, ]
    message(sprintf("[feature_engineering/HW] Aligned CargaTempMed to %d rows.", nrow(ctv)))
  }
  for (x in 1:nrow(ctv)) {
    ctm[x, 4] <- mean(as.numeric(ctv[x, 4:51]), na.rm = TRUE)
    ctm[x, 5] <- mean(as.numeric(ctv[x, 53:100]), na.rm = TRUE)
  }
  list(ctv = ctv, ctm = ctm)
}

## deck (plugin_ingest), spec (params + seed do run.yaml) -> features do treino. Nao muda o deck (copia explicita).
plugin_build_features <- function(deck, spec) {
  sp <- .hw_spec_parts(spec)
  p  <- lapply(stats::setNames(nm = c("ic", "perfil_mo", "classes_mo", "similar", "feriado", "kmeans_seed")),
               function(n) .hw_param(sp$params, n))
  k  <- plugin_constants()

  ctv <- as.data.frame(deck$CargaTempVer)
  ctm <- as.data.frame(deck$CargaTempMed)
  ncol_legacy <- 52L

  ## (1) dias faltantes/repetidos
  ctv <- Corrige.Dias.Faltantes(ctv)
  ctm <- Corrige.Dias.Faltantes(ctm)
  ## (2) pre-consistencia da CargaTempMed
  ctm <- .preconsist_ctm(ctm)
  ## (3) faixas e feriados
  ctv <- .hw_ok(Corrige.Dados(p_tabela = ctv, p_nlin = nrow(ctv), p_ncol = ncol_legacy, p_Negativos = TRUE,
                              p_TrocaDias = FALSE, p_TabelaCargaMedia = FALSE), "Corrige.Dados")
  ## (4) MO1: a semente do run.yaml imediatamente antes (posicao do oraculo)
  if (!is.null(sp$seed)) set.seed(sp$seed)
  ctv <- MO1(p_datatrain = ctv, p_ic = p$ic, p_perfil = p$perfil_mo, p_nclusters = p$classes_mo,
             p_similar = p$similar, p_kmeans_seed = p$kmeans_seed)[[1L]]
  ## snapshots pos-MO1 e pre-feriado (CRITICO: consumidos pelo SemiFer e pelo EfeitoFer)
  ctv_fer <- ctv
  ctm1    <- ctm
  ## (5) feriados
  if (isTRUE(p$feriado))
    ctv <- .hw_ok(Substitui.Feriados(p_tabela = ctv, p_nlin = nrow(ctv), p_ncol = ncol_legacy, p_csv = FALSE),
                  "Substitui.Feriados")
  ## (6) LOESS nas temperaturas
  ctv <- .hw_ok(Suaviza.LOESS(p_tabela = ctv, p_nlin = nrow(ctv), p_span = k$loess_span_temp), "Suaviza.LOESS")
  ## (7) recomputo de CargaTempMed[,4:5]
  ali <- .hw_recompute_ctm(ctv, ctm)

  c(list(CargaTempVer = as.data.frame(ali$ctv), CargaTempMed = ali$ctm, CargaTempVerFer = ctv_fer,
         CargaTempMed1 = ctm1, m_mes = as.integer(lubridate::month(deck$meta$origin_day))),
    deck[c("TempPre", "CDDPre", "load_day_o", "n_steps", "weather_absent", "meta")])
}
