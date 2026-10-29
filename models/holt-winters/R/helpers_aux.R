## helpers_aux.R -- funcoes auxiliares do feature engineering do Holt-Winters (HW.2). SO definicoes.
## Porte de prevcargadessem3/models/holt-winters/R/feature_engineering/funcoes_aux.R (copia canonica; as copias de
## R/helpers/ e R/preprocessing/ sao shims e nao entram). Nomes e ordem de operacoes do oraculo preservados
## (rastreabilidade). Mudancas, e so elas: (a) a semente do kmeans vem do run.yaml (`p_seed`), nao do literal do
## oraculo; (b) datas por lubridate sobre colunas que ja sao `Date`; (c) tudo qualificado (`stats::`, `forecast::`,
## `lubridate::`), sem `source()`. O plugin nao le ambiente nem relogio. Saida e mensagens: ASCII puro.

## Aborto de contrato do porte (id hw.bad_param, scripts/lib/errors/holt_winters.tsv).
.hw_bad_param <- function(param, where) {
  hw_abort("hw.bad_param", sprintf("parametro %s ausente ou invalido", param), list(param = param), where = where)
}

## Porte de funcoes_aux.R:53. Unica mudanca: a semente vem de params.kmeans_seed (o oraculo fixava 123 literal).
classifica_kmeans <- function(p_dados, p_nclusters, p_seed) {
  if (missing(p_seed) || length(p_seed) != 1L || is.na(p_seed))
    .hw_bad_param("params.kmeans_seed", "holt-winters/helpers_aux")
  set.seed(p_seed)
  gpo <- stats::kmeans(p_dados, p_nclusters, iter.max = 30)
  gpo
}

## Passo 1: dias repetidos saem; dia faltante entra como copia do mesmo dia da semana (nunca 0). funcoes_aux.R:79.
Corrige.Dias.Faltantes <- function(p_tabela) {
  r_tabela <- data.frame()
  tam <- dim(p_tabela)[2]
  r_tabela[1, 1:tam] <- p_tabela[1, 1:tam]
  lcont <- 2

  for (i in 2:dim(p_tabela)[1]) {
    diaAnt <- p_tabela[(i - 1), 1]

    if (p_tabela[i, 1] == diaAnt + lubridate::days(1)) {
      r_tabela[lcont, 1:tam] <- p_tabela[i, 1:tam]
      lcont <- lcont + 1

    } else if (p_tabela[i, 1] == diaAnt) {
      message("Dia Repetido Retirado da Base!")

    } else {
      message("Dia Faltante Incluido na Base!")
      if (i <= 7) {
        r_tabela[lcont, 1:tam] <- p_tabela[(i + 7), 1:tam]
      } else {
        r_tabela[lcont, 1:tam] <- p_tabela[(i - 7), 1:tam]
      }
      r_tabela[lcont, 1] <- diaAnt + lubridate::days(1)
      lcont <- lcont + 1

      r_tabela[lcont, 1:tam] <- p_tabela[i, 1:tam]
      lcont <- lcont + 1
    }
  }

  return(r_tabela)
}

## Passo 3: faixa de carga/temperatura, feriado/semi-feriado, medias. funcoes_aux.R:159.
Corrige.Dados <- function(p_tabela, p_nlin, p_ncol, p_Negativos, p_TrocaDias, p_TabelaCargaMedia) {

  nLIcarga <- 1
  nLScarga <- 9999999
  nLItemp  <- -20
  nLStemp  <- 50

  if (p_Negativos) {
    for (x in 1:p_nlin) {
      if (is.na(p_tabela[x, 2]))   { p_tabela[x, 2] <- 0 }
      if (is.na(p_tabela[x, 102])) {
        if (x > 1) p_tabela[x, 102] <- p_tabela[(x - 1), 102]
        else       p_tabela[x, 102] <- 0
      }
      if (is.na(p_tabela[x, 105])) { p_tabela[x, 105] <- 0 }
      if (is.na(p_tabela[x, 3])) {
        if (x > 1) {
          if (p_tabela[(x - 1), 3] == 7) {
            p_tabela[x, 3] <- 1
          } else {
            p_tabela[x, 3] <- p_tabela[(x - 1), 3] + 1
          }
        } else {
          p_tabela[x, 3] <- 1
        }
      }
      for (y in 4:51) {
        if ((p_tabela[x, y] <= nLIcarga) |
            (p_tabela[x, y] >= nLScarga) |
            is.na(p_tabela[x, y])) {
          p_tabela[x, y] <- stats::median(p_tabela[, y], na.rm = TRUE)
        }
      }
      p_tabela[x, 101] <- mean(as.numeric(p_tabela[x, 4:51]), na.rm = TRUE)
    }

    if (p_nlin > 1) {
      x <- p_nlin
      if ((p_tabela[(x - 1), 2] == 1) &
          (p_tabela[x, 3]       == 6) &
          (p_tabela[x, 2]       == 0)) {
        p_tabela[x, 2] <- 2
      }
      if ((p_tabela[x, 2]       == 1) &
          (p_tabela[x, 3]       == 3) &
          (p_tabela[(x - 1), 2] == 0)) {
        p_tabela[(x - 1), 2] <- 2
      }
    }

    for (x in 1:p_nlin) {
      for (y in 53:100) {
        if ((p_tabela[x, y] <= nLItemp) |
            (p_tabela[x, y] >= nLStemp) |
            is.na(p_tabela[x, y])) {
          p_tabela[x, y] <- stats::median(p_tabela[, y], na.rm = TRUE)
        }
      }
      p_tabela[x, 52] <- mean(as.numeric(p_tabela[x, 53:100]), na.rm = TRUE)
    }
  }

  if (p_TrocaDias) {
    mloop <- seq(from = p_nlin, to = (p_nlin - 14))
    for (x in mloop) {
      aux_medcarga <- mean(as.numeric(p_tabela[x, 4:51]))
      aux_diasem   <- p_tabela[x, 3]
      aux_carga    <- as.numeric(p_tabela[x, 4:51]) / aux_medcarga
      mloop1       <- seq(from = (x - 1), to = (x - 28))
      aux_tot      <- 1
      for (xx in mloop1) {
        if (p_tabela[xx, 3] == aux_diasem) {
          aux_medcarga1 <- mean(as.numeric(p_tabela[xx, 4:51]))
          aux_carga     <- aux_carga + (as.numeric(p_tabela[xx, 4:51]) / aux_medcarga1)
          aux_tot       <- aux_tot + 1
        }
      }
      aux_carga         <- (aux_carga / aux_tot) * aux_medcarga
      p_tabela[x, 4:51] <- aux_carga
    }
  }

  if (p_TabelaCargaMedia) {
    for (x in 8:p_nlin) {
      if ((p_tabela[x, 4] <= nLIcarga) |
          (p_tabela[x, 4] >= nLScarga) |
          is.na(p_tabela[x, 4])) {
        p_tabela[x, 4] <- p_tabela[(x - 7), 4]
      }
      if ((p_tabela[x, 6] <= nLItemp) |
          (p_tabela[x, 6] >= nLStemp) |
          is.na(p_tabela[x, 6])) {
        p_tabela[x, 6] <- p_tabela[(x - 7), 6]
      }
    }
  }

  list(p_tabela, FALSE)
}

## Passo 4: imputacao de dias atipicos. perfil C = perfis por cluster (kmeans semeado por `p_kmeans_seed`);
## perfil ST = residuos ARIMA (portado como esta, sem gate de paridade proprio). funcoes_aux.R:295.
MO1 <- function(p_datatrain, p_ic, p_perfil, p_nclusters, p_similar, p_kmeans_seed) {
  if (length(p_perfil) != 1L || is.na(p_perfil) || !(p_perfil %in% c("C", "ST")))
    .hw_bad_param("params.perfil_mo", "holt-winters/MO1")

  p_cont <- 0

  if (p_perfil == "ST") {
    tam          <- dim(p_datatrain)[1]
    p_datainiver <- p_datatrain$Data[1]
    p_datainijan <- lubridate::floor_date(p_datainiver, "year")
    p_contdias   <- as.numeric(p_datainiver - p_datainijan) + 1

    for (j in 1:48) {
      fator <- p_ic

      for (ll in 1:7) {
        if (((p_datatrain[ll, (3 + j)] / p_datatrain[(ll + 7), (3 + j)]) >= 2) |
            ((p_datatrain[ll, (3 + j)] / p_datatrain[(ll + 7), (3 + j)]) <= 0.3)) {
          p_datatrain[ll, (3 + j)] <- p_datatrain[(ll + 7), (3 + j)]
        }
      }
      for (ll in 8:tam) {
        if (((p_datatrain[ll, (3 + j)] / p_datatrain[(ll - 7), (3 + j)]) >= 2) |
            ((p_datatrain[ll, (3 + j)] / p_datatrain[(ll - 7), (3 + j)]) <= 0.3)) {
          p_datatrain[ll, (3 + j)] <- p_datatrain[(ll - 7), (3 + j)]
        }
      }

      tsCargaVer <- stats::ts(
        as.integer(p_datatrain[, (3 + j)]),
        start     = c(lubridate::year(p_datainiver), lubridate::yday(p_datainiver)),
        frequency = plugin_constants()$ts_frequency
      )
      fit <- forecast::Arima(
        tsCargaVer,
        order            = c(1, 0, 0),
        seasonal         = c(1, 1, 0),
        include.mean     = FALSE,
        include.constant = FALSE,
        include.drift    = FALSE
      )
      IC <- fator * stats::sd(fit$x - fit$fitted)
      p_datatrain[, (3 + j)] <- vapply(
        1:tam,
        function(i) {
          if (abs(fit$residuals[i]) > IC) fit$fitted[i]
          else p_datatrain[i, (3 + j)]
        },
        0
      )
      p_datatrain[, (3 + j)] <- as.integer(p_datatrain[, (3 + j)])

      for (i in 30:tam) {
        prevARIMA <- (fit$coef[1] * p_datatrain[(i - 1),  (3 + j)]) +
          ((1 + fit$coef[2]) * p_datatrain[(i - 7),  (3 + j)]) -
          (fit$coef[1] * (1 + fit$coef[2]) * p_datatrain[(i - 8),  (3 + j)]) -
          (fit$coef[2] * p_datatrain[(i - 14), (3 + j)]) +
          (fit$coef[1] * fit$coef[2] * p_datatrain[(i - 15), (3 + j)])
        upper <- prevARIMA + IC
        lower <- prevARIMA - IC
        if (!((p_datatrain[i, (3 + j)] >= lower) |
              (p_datatrain[i, (3 + j)] <= upper))) {
          p_datatrain[i, (3 + j)] <- prevARIMA
          p_cont                  <- p_cont + 1
        }
      }
      p_cont <- round((p_cont / (tam * 48) * 100), digits = 2)
    }

  } else {
    p_datatrainaux <- as.matrix(p_datatrain[, 4:51])
    p_datatrainaux <- t(apply(p_datatrainaux, 1, function(x) x / mean(x)))
    tam            <- dim(p_datatrainaux)[1]

    ClasseCarga <- classifica_kmeans(p_datatrainaux, p_nclusters, p_seed = p_kmeans_seed)

    for (j in 1:dim(p_datatrain)[1]) {
      distancia  <- 0
      centroWin  <- 99999
      centroPos  <- 0

      for (i in 1:p_nclusters) {
        if (ClasseCarga$size[i] > 2) {
          p_dados   <- rbind(
            as.numeric(p_datatrainaux[j, ]),
            as.numeric(ClasseCarga$centers[i, ])
          )
          distancia <- stats::dist(p_dados, method = "euclidean")
          if (distancia <= centroWin) {
            centroWin <- distancia
            centroPos <- i
          }
        }
      }

      if ((stats::sd(as.numeric(ClasseCarga$centers[centroPos, ]), na.rm = TRUE) == 0) |
          (stats::sd(as.numeric(p_datatrainaux[j, ]), na.rm = TRUE) == 0)) {
        p_r2adjusted <- 1
      } else {
        m_dd <- cbind(
          ClasseCarga$centers[centroPos, ],
          as.numeric(p_datatrainaux[j, ])
        )
        colnames(m_dd) <- c("centro", "dado")
        m_ddres        <- stats::lm(centro ~ ., data = as.data.frame(m_dd))
        m_ddres1       <- summary(m_ddres)
        p_r2adjusted   <- m_ddres1$adj.r.squared
      }

      if ((abs(p_r2adjusted) <= p_similar) | is.na(p_r2adjusted)) {
        p_datatrain[j, 4:51] <-
          as.numeric(ClasseCarga$centers[centroPos, ]) *
          mean(as.numeric(p_datatrain[j, 4:51]), na.rm = FALSE)
        p_cont <- p_cont + 1
      }
    }
    p_cont <- round((p_cont / tam * 100), digits = 2)
  }

  for (j in 1:dim(p_datatrain)[1]) {
    p_datatrain[j, 101] <- as.integer(mean(as.numeric(p_datatrain[j, 4:51]), na.rm = FALSE))
  }

  list(p_datatrain, p_cont)
}

## Passo 5: perfis de feriado/semi-feriado trocados pelo do mesmo dia da semana mais proximo. funcoes_aux.R:451.
Substitui.Feriados <- function(p_tabela, p_nlin, p_ncol, p_csv) {

  pseq <- seq(p_nlin, 1, -1)

  for (x in pseq) {
    if ((((p_tabela[x, 2] == 1) | (p_tabela[x, 2] == 2)) &
         !is.na(p_tabela[x, 2])) & (x > 1)) {

      xx     <- x - 1
      m_Flag <- TRUE

      while (m_Flag) {
        if (((p_tabela[xx, 3] == p_tabela[x, 3]) &
             (p_tabela[xx, 2] == 0) &
             !is.na(p_tabela[xx, 3]))) {
          break
        }
        xx <- xx - 1
        if (xx == 0) {
          xx <- x + 1
          while (m_Flag) {
            if (((p_tabela[xx, 3] == p_tabela[x, 3]) &
                 (p_tabela[xx, 2] == 0))) {
              break
            }
            xx <- xx + 1
            if (xx == p_nlin) {
              m_Flag <- FALSE
              break
            }
          }
        }
      }

      if (m_Flag) {
        p_tabela[x, 4:(p_ncol - 1)] <- p_tabela[xx, 4:(p_ncol - 1)]
      }
      p_tabela[x, 2]   <- 0
      p_tabela[x, 101] <- mean(as.numeric(p_tabela[x, 4:51]), na.rm = TRUE)
    }
  }

  list(p_tabela, FALSE)
}

## Passo 6: LOESS linha a linha em T01..T48 (colunas 53:100); ajustados arredondados. funcoes_aux.R:519.
Suaviza.LOESS <- function(p_tabela, p_nlin, p_span) {
  x <- c(1:48)
  for (z in 1:p_nlin) {
    y     <- as.numeric(p_tabela[z, (53:100)])
    media <- mean(y, na.rm = TRUE)
    for (zz in 1:48) {
      if (is.na(y[zz])) y[zz] <- media
    }
    l_data  <- data.frame(x, y)
    l_loess <- stats::loess(y ~ x, data = l_data, span = p_span)
    p_tabela[z, (53:100)] <- round(l_loess$fitted)
  }
  list(p_tabela, FALSE)
}

## Pos-infer: uma linha de 48 valores. funcoes_aux.R:552.
Suaviza.LOESS1 <- function(p_tabela, p_span) {
  x       <- c(1:48)
  y       <- p_tabela
  l_data  <- data.frame(x, y)
  l_loess <- stats::loess(y ~ x, data = l_data, span = p_span)
  p_tabela <- round(l_loess$fitted)
  list(p_tabela, FALSE)
}

## Perfil de feriado (F) ou semi-feriado (S) do dia previsto. funcoes_aux.R:596. `p_table` e a CargaTempVerFer.
SemiFer <- function(p_table, p_F_S, p_alfa, p_cm, p_mes, p_tipoFer) {

  m_Flag  <- "sabdom"
  m_table <- as.data.frame(p_table)
  m_month <- lubridate::month(m_table$Data)

  if (p_mes == 1) {
    m_mesI <- 1; m_mesF <- 2
  } else if (p_mes == 12) {
    m_mesI <- 11; m_mesF <- 12
  } else {
    m_mesI <- p_mes - 1; m_mesF <- p_mes + 1
  }

  m_s <- matrix(0, nrow = 1, ncol = 48)
  m_d <- matrix(0, nrow = 1, ncol = 48)
  m_f <- matrix(0, nrow = 1, ncol = 48)

  if (p_F_S == "F") {
    if (m_Flag == "sabdom") {
      m_sab <- m_table[which((m_month == p_mes) & (m_table$DiaSemana == 7) & (m_table$Feriado == 0)), ]
      for (j in 1:48) {
        m_s[1, j] <- mean(m_sab[, (j + 3)], na.rm = TRUE) / mean(as.matrix(m_sab[, 4:51]), na.rm = TRUE)
      }
      m_dom <- m_table[which((m_month == p_mes) & (m_table$DiaSemana == 1) & (m_table$Feriado == 0)), ]
      for (j in 1:48) {
        m_d[1, j] <- mean(m_dom[, (j + 3)], na.rm = TRUE) / mean(as.matrix(m_dom[, 4:51]), na.rm = TRUE)
      }
      m_fer <- m_table[which((m_month >= m_mesI) & (m_month <= m_mesF) & (m_table$TIPO_FER == p_tipoFer)), ]
      tam_fer <- dim(m_fer)[1]
      m_fer   <- m_fer[tam_fer, ]
      for (j in 1:48) {
        m_f[1, j] <- mean(m_fer[, (j + 3)], na.rm = TRUE) / mean(as.matrix(m_fer[, 4:51]), na.rm = TRUE)
      }
      DataFer           <- cbind(as.numeric(m_f), as.numeric(m_s), as.numeric(m_d))
      colnames(DataFer) <- c("fer", "sab", "dom")
      DataFer           <- as.data.frame(DataFer)
      ModFer            <- stats::lm(fer ~ sab + dom + 0, data = DataFer)
      m_res             <- ModFer$fitted * p_cm
    } else {
      m_sab <- m_table[which((m_month >= m_mesI) & (m_month <= m_mesF) & (m_table$TIPO_FER == p_tipoFer)), ]
      for (j in 1:48) {
        m_s[1, j] <- mean(m_sab[, (j + 3)], na.rm = TRUE) / mean(as.matrix(m_sab[, 4:51]), na.rm = TRUE)
      }
      m_res <- ((p_alfa * m_s) + ((1 - p_alfa) * m_s)) * p_cm
    }

  } else {
    if (m_Flag == "sabdom") {
      m_sab <- m_table[which((m_month == p_mes) & (m_table$DiaSemana == 7) & (m_table$Feriado == 0)), ]
      for (j in 1:48) {
        m_s[1, j] <- mean(m_sab[, (j + 3)], na.rm = TRUE) / mean(as.matrix(m_sab[, 4:51]), na.rm = TRUE)
      }
      m_du <- m_table[which((m_month == p_mes) &
                              ((m_table$DiaSemana == 2) | (m_table$DiaSemana == 6)) &
                              (m_table$Feriado == 0)), ]
      for (j in 1:48) {
        m_d[1, j] <- mean(m_du[, (j + 3)], na.rm = TRUE) / mean(as.matrix(m_du[, 4:51]), na.rm = TRUE)
      }
      if ((p_mes == 1) | (p_mes == 12)) {
        m_fer <- m_table[which((m_month >= m_mesI) & (m_month <= m_mesF) & (m_table$TIPO_FER == p_tipoFer)), ]
      } else {
        m_fer <- m_table[which(m_table$TIPO_FER == p_tipoFer), ]
      }
      for (j in 1:48) {
        m_f[1, j] <- mean(m_fer[, (j + 3)], na.rm = TRUE) / mean(as.matrix(m_fer[, 4:51]), na.rm = TRUE)
      }
      DataFer           <- cbind(as.numeric(m_f), as.numeric(m_s), as.numeric(m_d))
      colnames(DataFer) <- c("fer", "sab", "dom")
      DataFer           <- as.data.frame(DataFer)
      ModFer            <- stats::lm(fer ~ sab + dom + 0, data = DataFer)
      m_res             <- ModFer$fitted * p_cm
    } else {
      m_sab <- m_table[which((m_month >= m_mesI) & (m_month <= m_mesF) & (m_table$TIPO_FER == p_tipoFer)), ]
      for (j in 1:48) {
        m_s[1, j] <- mean(m_sab[, (j + 3)], na.rm = TRUE) / mean(as.matrix(m_sab[, 4:51]), na.rm = TRUE)
      }
      m_res <- ((p_alfa * m_s) + ((1 - p_alfa) * m_s)) * p_cm
    }
  }

  list(m_res)
}
