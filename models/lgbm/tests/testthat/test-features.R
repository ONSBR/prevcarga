## test-features.R -- LGBM.2: calendario, ondaletas Haar, Sazonalidade + fallbacks, LOESS por disable_loess, cortes internos
## e o estagio plugin_build_features. Cada gate traz FORBIDDEN (identico/Delta=0) e CONTROL (Delta != 0).
## Deck sintetico P.0e, area ZZ1. A comparacao contra o golden do oraculo (dez/24) e das unidades de paridade do LGBM.5.
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
P <- lgbm_source_plugin()
K <- lgbm_consts(P)
SPEC <- lgbm_lib$prevcarga_model_spec("lgbm", lgbm_root())
PRM <- SPEC$params
with_p <- function(...) { p <- PRM; a <- list(...); for (n in names(a)) p[[n]] <- a[[n]]; p }
deck <- lgbm_fx_deck(days = 120L)   # 120 dias: o rollapply de dwt custa ~40 s por 36k linhas (como no oraculo)
ctx <- lgbm_fx_ctx(deck)
cuts <- lgbm_fx_cuts(ctx)
panel_tr <- lgbm_fx_panel(deck, ctx, "train", cuts, history_days = 115L)
frame <- P$lgbm_ingest(panel_tr, cuts, NULL, "train", K)   # sem spec: history_days$train (730) e do validate-deck
D <- lubridate::ymd
build <- function(params = PRM, fr = frame, cuts_ = cuts) P$lgbm_build_features(fr, list(params = params, seed = SPEC$seed), cuts_, K)

## ---- G-CAL ------------------------------------------------------------------------------------------------
cal_dt <- function() data.table::data.table(Data = seq(D("2024-12-15"), D("2025-01-10"), by = "day"))
FER <- D(c("2024-12-25", "2025-01-01"))
cal <- function(params) P$lgbm_add_calendar_cols(cal_dt(), FER, params)
lab <- function(x, md) x[format(Data, "%m-%d") == md, DiaEspecialFimAno]

test_that("[PLUGINS] gate_cal: [AC1] calendario com params do run.yaml: DiaSemanaNum sem locale, TipoDia 4 niveis, fim de ano na ordem do oraculo", {
  x <- cal(PRM)
  expect_identical(x[Data == D("2024-12-15"), DiaSemanaNum], 1L)   # domingo
  expect_identical(x[Data == D("2024-12-21"), DiaSemanaNum], 2L)   # sabado
  expect_identical(x[Data == D("2024-12-16"), DiaSemanaNum], 3L)   # segunda
  expect_identical(x[Data == D("2024-12-20"), DiaSemanaNum], 7L)   # sexta
  expect_setequal(unique(x$TipoDia), c("ND", "FDS", "FE"))
  expect_identical(x[Data == D("2024-12-25"), TipoDia], "FE")
  expect_identical(x[Data == D("2025-01-04"), TipoDia], "FDS")
  expect_identical(lab(x, "12-25"), "Natal_AnoNovo"); expect_identical(lab(x, "01-01"), "Natal_AnoNovo")
  expect_identical(lab(x, "12-24"), "Vespera");       expect_identical(lab(x, "12-31"), "Vespera")
  expect_identical(lab(x, "12-23"), "PreVespera")
  expect_identical(lab(x, "12-26"), "PosNatalAnoNovo"); expect_identical(lab(x, "01-02"), "PosNatalAnoNovo")
  expect_identical(unique(x[format(Data, "%m-%d") %in% sprintf("12-%02d", 27:30), DiaEspecialFimAno]), "SemanaNatalAnoNovo")
  expect_identical(unique(x[format(Data, "%m-%d") %in% sprintf("01-%02d", 3:7), DiaEspecialFimAno]), "PrimeiraSemanaAno")
  expect_identical(unique(x[format(Data, "%m-%d") %in% c("12-15", "01-08", "01-10"), DiaEspecialFimAno]), "0")
  expect_identical(x[Data == D("2024-12-24"), PreFeriado], 1L); expect_identical(x[Data == D("2024-12-26"), PosFeriado], 1L)
  expect_identical(x[Data == D("2024-12-31"), PreFeriado], 1L); expect_identical(x[Data == D("2025-01-02"), PosFeriado], 1L)
  expect_identical(sum(x$PreFeriado), 2L); expect_identical(sum(x$PosFeriado), 2L)
})

test_that("[PLUGINS] gate_cal: [AC1] DiaSemanaNum e locale-invariante (weekdays() nao e usado) e o mapa vale em qualquer LC_TIME", {
  expect_identical(pc_grep_code(file.path(lgbm_root(), "models", "lgbm", "R"), "weekdays\\s*\\("), character())
  old <- Sys.getlocale("LC_TIME")
  on.exit(suppressWarnings(Sys.setlocale("LC_TIME", old)), add = TRUE)
  a <- cal(PRM)$DiaSemanaNum
  suppressWarnings(Sys.setlocale("LC_TIME", "C"))
  expect_identical(cal(PRM)$DiaSemanaNum, a)
})

test_that("[PLUGINS] gate_cal: [AC1] prevespera_1230: 12-30 vira PreVespera; CONTROL padrao deixa SemanaNatalAnoNovo", {
  expect_identical(lab(cal(with_p(prevespera_1230 = TRUE)), "12-30"), "PreVespera")
  expect_identical(lab(cal(with_p(prevespera_1230 = TRUE)), "12-29"), "SemanaNatalAnoNovo")
  expect_identical(lab(cal(PRM), "12-30"), "SemanaNatalAnoNovo")
})

test_that("[PLUGINS] gate_cal: [AC1] CONTROL calendar_yearend FALSE: sem DiaEspecialFimAno/PreFeriado/PosFeriado; rotulos de 24-31/12 mudam", {
  off <- cal(with_p(calendar_yearend = FALSE))
  expect_false(any(c("DiaEspecialFimAno", "PreFeriado", "PosFeriado") %in% names(off)))
  on <- cal(PRM)
  expect_true(all(c("DiaEspecialFimAno", "PreFeriado", "PosFeriado") %in% names(on)))
  expect_identical(off$DiaSemanaNum, on$DiaSemanaNum)
  expect_identical(P$lgbm_adequacao_extra_features(with_p(calendar_yearend = FALSE)), character())
  expect_length(P$lgbm_adequacao_extra_features(PRM), 8L)
})

test_that("[PLUGINS] gate_cal: [AC1] tipodia_legacy: 5 niveis (segunda, sexta, sabado, domingo, Dia util intermediario)", {
  x <- cal(with_p(tipodia_legacy = TRUE))
  expect_setequal(unique(x$TipoDia), c("segunda-feira", "sexta-feira", "sabado", "domingo", "Dia util intermediario"))
  expect_identical(x[Data == D("2024-12-16"), TipoDia], "segunda-feira")
  expect_identical(x[Data == D("2024-12-20"), TipoDia], "sexta-feira")
  expect_identical(x[Data == D("2024-12-21"), TipoDia], "sabado")
  expect_identical(x[Data == D("2024-12-15"), TipoDia], "domingo")
  expect_identical(x[Data == D("2024-12-18"), TipoDia], "Dia util intermediario")
  expect_identical(P$lgbm_adequacao_extra_features(with_p(tipodia_legacy = TRUE))[[1L]], "Feriado")
  ## feriado em fim de semana (FE+FDS): 2024-12-25 e quarta; usa feriado no sabado
  y <- P$lgbm_add_calendar_cols(cal_dt(), D("2024-12-21"), PRM)
  expect_identical(y[Data == D("2024-12-21"), TipoDia], "FE+FDS")
})

## ---- G-SAZ / G-NA -----------------------------------------------------------------------------------------
feat_dt <- function(params) {
  dt <- data.table::copy(frame)
  P$lgbm_add_calendar_cols(dt, attr(frame, "feriados"), params)
  dt
}
test_that("[PLUGINS] gate_saz: [AC2] Sazonalidade: FORBIDDEN (carga > train_cutoff) identica; CONTROL (carga no train_cutoff) muda", {
  tc <- cuts$last_data_day - 20L
  dt <- feat_dt(PRM)
  base <- P$lgbm_compute_sazonalidade(dt, tc, PRM)
  expect_identical(nrow(base), 48L * length(unique(dt[Data <= tc, TipoDia])))
  fb <- data.table::copy(dt); fb[Data > tc, CargaGlobal := CargaGlobal + 500]
  expect_identical(P$lgbm_compute_sazonalidade(fb, tc, PRM), base)
  cn <- data.table::copy(dt); cn[Data == tc, CargaGlobal := CargaGlobal + 500]
  expect_gt(max(abs(P$lgbm_compute_sazonalidade(cn, tc, PRM)$Sazonalidade - base$Sazonalidade)), 1e-5)
  ## o mesmo no modo legacy (chave rica + fallbacks)
  pl <- with_p(tipodia_legacy = TRUE); dl <- feat_dt(pl)
  bl <- P$lgbm_compute_sazonalidade(dl, tc, pl)
  fbl <- data.table::copy(dl); fbl[Data > tc, CargaGlobal := CargaGlobal + 500]
  expect_identical(P$lgbm_compute_sazonalidade(fbl, tc, pl), bl)
  expect_identical(P$lgbm_compute_sazonalidade_fallbacks(fbl, tc), P$lgbm_compute_sazonalidade_fallbacks(dl, tc))
  cnl <- data.table::copy(dl); cnl[Data == tc, CargaGlobal := CargaGlobal + 500]
  expect_gt(max(abs(P$lgbm_compute_sazonalidade(cnl, tc, pl)$Sazonalidade - bl$Sazonalidade)), 1e-5)
})

test_that("[PLUGINS] gate_saz: [AC2] Sazonalidade nao legacy = media por (TipoDia, Hora) das linhas Data <= train_cutoff", {
  tc <- cuts$last_data_day - 20L
  dt <- feat_dt(PRM)
  s <- P$lgbm_compute_sazonalidade(dt, tc, PRM)
  r <- dt[Data <= tc & TipoDia == "ND" & Hora == 7L, mean(CargaGlobal)]
  expect_equal(s[TipoDia == "ND" & Hora == 7L, Sazonalidade], r, tolerance = 1e-12)
  expect_identical(names(s), c("TipoDia", "Hora", "Sazonalidade"))
})

test_that("[PLUGINS] gate_saz: [AC2] legacy: chave rica com LOESS span 0.2 grau 2 em grupos >= 8 linhas; G-NA zero NA apos o merge com fallbacks", {
  pl <- with_p(tipodia_legacy = TRUE)
  dl <- feat_dt(pl)
  tc <- D("2026-08-01")      # feriados posteriores (ex.: 07/09) nao tem celula rica no treino
  s <- P$lgbm_compute_sazonalidade(dl, tc, pl)
  expect_identical(names(s), c("TipoDia", "Hora", "Feriado", "DiaEspecialFimAno", "Sazonalidade"))
  g <- dl[Data <= tc, list(m = mean(CargaGlobal)), by = list(TipoDia, Hora, Feriado, DiaEspecialFimAno)]
  g1 <- g[TipoDia == "segunda-feira" & !Feriado]
  data.table::setorder(g1, Hora)
  hx <- g1$Hora / 2
  ref <- as.numeric(predict(loess(g1$m ~ hx, span = 0.2, degree = 2L)))
  got <- s[TipoDia == "segunda-feira" & Feriado == FALSE][order(Hora), Sazonalidade]
  expect_equal(got, ref, tolerance = 1e-12)
  expect_gt(max(abs(got - g1$m)), 1e-6)
  fb <- P$lgbm_compute_sazonalidade_fallbacks(dl, tc)
  expect_identical(names(fb), c("fb2", "fb3", "fb4"))
  m <- P$lgbm_merge_sazonalidade(data.table::copy(dl), s, fb, pl)
  expect_identical(nrow(m), nrow(dl))
  expect_false(anyNA(m$Sazonalidade))
  ## CONTROL: sem fallbacks o mesmo merge deixa NA (as celulas do fim de ano nao existem na tabela rica)
  m0 <- merge(data.table::copy(dl), s, by = c("TipoDia", "Hora", "Feriado", "DiaEspecialFimAno"), all.x = TRUE)
  expect_true(anyNA(m0$Sazonalidade))
  expect_s3_class(fail(P$lgbm_merge_sazonalidade(data.table::copy(dl), s, NULL, pl)), "prevcarga_error")
})

## ---- ondaletas --------------------------------------------------------------------------------------------
ref_wav <- function(v) { wt <- wavelets::dwt(v, filter = "haar"); c(unlist(lapply(wt@W, as.numeric)), unlist(lapply(wt@V, as.numeric))) }

test_that("[PLUGINS] gate12_ondaletas_d1: [AC3] lgbm_build_wavelets: janela de 32 alinhada a direita = dwt Haar direto (<= 1e-12); 31 primeiras NA", {
  set.seed(5)
  x <- cumsum(rnorm(500)) + 1000
  w <- P$lgbm_build_wavelets(x, 32L)
  expect_identical(dim(w), c(500L, 62L))
  expect_true(all(is.na(as.matrix(w[1:31]))))
  for (i in c(32L, 33L, 100L, 257L, 500L)) {
    expect_lt(max(abs(unlist(w[i]) - ref_wav(x[(i - 31L):i]))), 1e-12)
  }
  expect_identical(names(w)[c(1L, 16L, 17L, 31L, 32L, 62L)], c("W1_1", "W1_16", "W2_1", "W5_1", "V1_1", "V5_1"))
  expect_s3_class(fail(P$lgbm_build_wavelets(x[1:20], 32L)), "prevcarga_error")
})

test_that("[PLUGINS] gate12_ondaletas_d1: [AC3] CarWav de CargaGlobal_anterior_BLF com shift de 48 h (2 dias), TempWav sem shift; 62 + 62 colunas", {
  o <- build()
  ft <- o$feature_table
  expect_identical(sum(grepl("^CarWav_", names(ft))), 62L); expect_identical(sum(grepl("^TempWav_", names(ft))), 62L)
  expect_true(all(paste0("CarWav_", 1:62) %in% names(ft)))
  ## serie contigua no fim do treino: janela de 32 linhas terminando na linha alvo
  d <- cuts$last_data_day - 30L
  row <- ft[Data == d & Hora == 20L]
  ser <- ft[Data <= d]
  i <- which(ser$Data == d & ser$Hora == 20L)
  expect_lt(max(abs(unlist(row[, paste0("TempWav_", 1:62), with = FALSE]) - ref_wav(ser$Temperatura_prevista_BLF[(i - 31L):i]))), 1e-12)
  j <- which(ft$Data == d - 2L & ft$Hora == 20L)
  expect_lt(max(abs(unlist(row[, paste0("CarWav_", 1:62), with = FALSE]) -
                      ref_wav(ft$CargaGlobal_anterior_BLF[(j - 31L):j]))), 1e-12)
  ## CONTROL: sem o shift o resultado seria outro
  j0 <- which(ft$Data == d & ft$Hora == 20L)
  expect_gt(max(abs(unlist(row[, paste0("CarWav_", 1:62), with = FALSE]) -
                      ref_wav(ft$CargaGlobal_anterior_BLF[(j0 - 31L):j0]))), 1e-6)
  fe <- file.path(lgbm_root(), "models", "lgbm", "R")
  expect_identical(Filter(function(h) !grepl("^ingestion", h), pc_grep_code(fe,
                                "(hours|seconds|minutes)\\s*\\(|%m\\+%|\\+\\s*172800|\\+/-\\s*1800")), character())
})

## ---- LOESS ------------------------------------------------------------------------------------------------
test_that("[PLUGINS] fe_loess: [AC4] lgbm_loess_daily/lgbm_loess_smooth: identidade por disable_loess ou < 5 nao-NA; senao = loess", {
  set.seed(2)
  h <- (0:47) / 2; y <- sin(h / 3) * 100 + rnorm(48, 0, 10) + 5000
  expect_identical(P$lgbm_loess_daily(y, h, 0.3, TRUE), y)
  r <- P$lgbm_loess_daily(y, h, 0.3, FALSE)
  expect_lt(max(abs(r - as.numeric(predict(loess(y ~ h, span = 0.3, degree = 2L))))), 1e-12)
  expect_gt(max(abs(r - y)), 1e-5)
  y3 <- rep(NA_real_, 48); y3[c(3, 9, 20)] <- c(1, 2, 3)
  expect_identical(P$lgbm_loess_daily(y3, h, 0.3, FALSE), y3)
  expect_identical(P$lgbm_loess_smooth(y, 0.1, TRUE), y)
  s <- P$lgbm_loess_smooth(y, 0.1, FALSE)
  i <- seq_along(y)
  expect_lt(max(abs(s - loess(y ~ i, span = 0.1, degree = 1L)$fitted)), 1e-12)
  expect_identical(P$lgbm_loess_smooth(y[1:4], 0.1, FALSE), y[1:4])
  yn <- y; yn[10] <- NA
  sn <- P$lgbm_loess_smooth(yn, 0.1, FALSE)
  expect_true(is.na(sn[10])); expect_false(anyNA(sn[-10]))
})

test_that("[PLUGINS] fe_loess: [AC4] plugin: disable_loess TRUE => suavizada == crua; FALSE => difere e bate com loess por dia; <5 nao-NA fica cru", {
  on <- build(with_p(disable_loess = TRUE))$feature_table
  off <- build(with_p(disable_loess = FALSE))$feature_table
  expect_identical(on$CargaGlobal_suav, on$CargaGlobal); expect_identical(on$Temperatura_suav, on$Temperatura)
  expect_gt(max(abs(off$CargaGlobal_suav - off$CargaGlobal)), 1e-5)
  expect_gt(max(abs(off$Temperatura_suav - off$Temperatura)), 1e-5)
  d <- cuts$last_data_day - 40L
  x <- off[Data == d][order(Hora)]
  hh <- x$Hora / 2
  expect_lt(max(abs(x$CargaGlobal_suav - as.numeric(predict(loess(x$CargaGlobal ~ hh, span = 0.3, degree = 2L))))), 1e-12)
  expect_lt(max(abs(x$Temperatura_suav - as.numeric(predict(loess(x$Temperatura ~ hh, span = 0.3, degree = 2L))))), 1e-12)
  ## dia com < 5 nao-NA de carga: cru
  fr <- data.table::copy(frame); data.table::setattr(fr, "feriados", attr(frame, "feriados")); data.table::setattr(fr, "cuts", attr(frame, "cuts"))
  dd <- cuts$last_data_day - 60L
  keep <- fr[Data == dd, DataHora][1:3]
  fr[Data == dd & !(DataHora %in% keep), CargaGlobal := NA_real_]
  ## o stage e na.omit: usa a funcao diretamente sobre o dia
  v <- fr[Data == dd][order(Hora), CargaGlobal]
  expect_identical(P$lgbm_loess_daily(v, (0:47) / 2, 0.3, FALSE), v)
})

test_that("[PLUGINS] fe_loess: [AC4] CONTROL de vazamento: o disable_loess e o de spec$params, nunca de env (LGBM_DISABLE_LOESS=0 => identico)", {
  a <- build(with_p(disable_loess = TRUE))$feature_table
  Sys.setenv(LGBM_DISABLE_LOESS = "0"); on.exit(Sys.unsetenv("LGBM_DISABLE_LOESS"), add = TRUE)
  b <- build(with_p(disable_loess = TRUE))$feature_table
  expect_identical(a, b)
  Sys.setenv(LGBM_DISABLE_LOESS = "1")
  expect_identical(a, build(with_p(disable_loess = TRUE))$feature_table)
  c0 <- build(with_p(disable_loess = FALSE))$feature_table
  expect_identical(c0, build(with_p(disable_loess = FALSE))$feature_table)
  expect_gt(max(abs(c0$CargaGlobal_suav - c0$CargaGlobal)), 1e-5)
  expect_identical(pc_grep_code(file.path(lgbm_root(), "models", "lgbm", "R"), "Sys\\.getenv|Sys\\.setenv"), character())
})

## ---- G-CUT ------------------------------------------------------------------------------------------------
orc_val <- function(o, s) if (s == "rolling") o - 1L else if (s == "legacy_month") as.Date(format(o, "%Y-%m-01")) - 1L else
  as.Date(format(lubridate::`%m-%`(o, months(1L)), "%Y-%m-01")) - 1L
orc_trn <- function(v, s) if (s == "rolling") v - 28L else if (s == "legacy_month") as.Date(format(v, "%Y-%m-01")) - 1L else
  as.Date(format(lubridate::`%m-%`(v, months(1L)), "%Y-%m-01")) - 1L

test_that("[PLUGINS] gate_cut: [AC5] lgbm_split_cutoffs: train_cv => ambos = last_data_day; 3 esquemas reproduzem o oraculo dia a dia (G-CUT)", {
  for (to in D(c("2024-10-31", "2024-03-01", "2024-07-15"))) {
    to <- lubridate::as_date(to)
    cu <- list(last_data_day = to - 1L, train_origin = to)
    cv <- P$lgbm_split_cutoffs(cu, with_p(train_cv = TRUE))
    expect_identical(cv, list(train_cutoff = to - 1L, val_cutoff = to - 1L))
    for (s in c("v2_old", "legacy_month", "rolling")) {
      r <- P$lgbm_split_cutoffs(cu, with_p(train_cv = FALSE, cutoff_scheme = s))
      v <- orc_val(to, s)
      expect_identical(r$val_cutoff, v, info = paste(to, s))
      expect_identical(r$train_cutoff, orc_trn(v, s), info = paste(to, s))
      expect_lte(max(r$val_cutoff, r$train_cutoff), cu$last_data_day)
    }
  }
  ## todos os dias de 2024 (dia a dia)
  for (to in seq(D("2024-01-05"), D("2024-12-28"), by = "day")) {
    to <- lubridate::as_date(to)
    for (s in c("v2_old", "legacy_month")) {
      r <- P$lgbm_split_cutoffs(list(last_data_day = to - 1L, train_origin = to), with_p(train_cv = FALSE, cutoff_scheme = s))
      expect_identical(c(r$val_cutoff, r$train_cutoff), c(orc_val(to, s), orc_trn(orc_val(to, s), s)))
    }
  }
})

test_that("[PLUGINS] gate_cut: [AC5] corte interno alem de last_data_day aborta contract; CONTROL last_data_day maior passa", {
  cu <- list(last_data_day = D("2024-10-15"), train_origin = D("2024-10-31"))
  e <- fail(P$lgbm_split_cutoffs(cu, with_p(train_cv = FALSE, cutoff_scheme = "rolling", sazonalidade_scope = "train")))
  expect_null(e)   # rolling: val = last_data_day, nunca alem
  cu2 <- list(last_data_day = D("2024-10-15"), train_origin = D("2024-12-31"))
  e <- fail(P$lgbm_split_cutoffs(cu2, with_p(train_cv = FALSE, cutoff_scheme = "legacy_month")))
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$class_name, "contract")
  expect_null(fail(P$lgbm_split_cutoffs(list(last_data_day = D("2024-11-30"), train_origin = D("2024-12-31")),
                                        with_p(train_cv = FALSE, cutoff_scheme = "legacy_month"))))
})

## ---- estagio ----------------------------------------------------------------------------------------------
test_that("[PLUGINS] fe_schema: [AC5] schema: colunas_entrada_BLF na ordem do contrato, nao_normalizar = dummies + extras, sem NA", {
  for (cfg in list(PRM, with_p(calendar_yearend = FALSE), with_p(tipodia_legacy = TRUE, disable_loess = TRUE),
                   with_p(train_cv = FALSE, cutoff_scheme = "legacy_month"))) {
    o <- build(cfg)
    ft <- o$feature_table
    cols <- o$colunas_entrada_BLF
    exp <- c("CargaGlobal_anterior_BLF", "Temperatura_prevista_BLF", "Sazonalidade", "Hora_seno",
             paste0("CarWav_", 1:62), paste0("TempWav_", 1:62), paste0("DiaSemana_", 1:6),
             if (!isTRUE(cfg$tipodia_legacy)) c("DiaEspecial_FDS", "DiaEspecial_FE", "DiaEspecial_FEFDS"),
             paste0("Mes_", 2:12), if (isTRUE(cfg$tipodia_legacy)) "Feriado",
             if (isTRUE(cfg$calendar_yearend)) c(paste0("DiaEspecial_", c("Natal_AnoNovo", "Vespera", "PreVespera",
               "PosNatalAnoNovo", "SemanaNatalAnoNovo", "PrimeiraSemanaAno")), "PreFeriado", "PosFeriado"))
    expect_identical(cols, exp)
    expect_identical(o$colunas_nao_normalizar, c(paste0("DiaSemana_", 1:6),
      if (!isTRUE(cfg$tipodia_legacy)) c("DiaEspecial_FDS", "DiaEspecial_FE", "DiaEspecial_FEFDS"), paste0("Mes_", 2:12),
      P$lgbm_adequacao_extra_features(cfg)))
    expect_true(all(cols %in% names(ft)))
    expect_false(anyNA(ft)); expect_gt(nrow(ft), 0L)
    expect_identical(P$lgbm_input_cols(cfg, 62L), cols)
    expect_identical(is.null(o$sazonalidade_fallbacks), !isTRUE(cfg$tipodia_legacy))
  }
})

test_that("[PLUGINS] fe_schema: [AC5] train_cv e esquemas no estagio: cortes de cuts, todos <= last_data_day; Sazonalidade so de Data <= train_cutoff", {
  o <- build(PRM)
  expect_identical(o$train_cutoff, cuts$last_data_day); expect_identical(o$val_cutoff, cuts$last_data_day)
  for (s in c("v2_old", "legacy_month", "rolling")) {
    p <- with_p(train_cv = FALSE, cutoff_scheme = s)
    o <- build(p)
    expect_lte(max(o$train_cutoff, o$val_cutoff), cuts$last_data_day)
    expect_identical(o$train_cutoff, P$lgbm_split_cutoffs(cuts, p)$train_cutoff)
    d <- data.table::copy(frame)[Data > min(Data)]; P$lgbm_add_calendar_cols(d, attr(frame, "feriados"), p)
    expect_identical(o$sazonalidade, P$lgbm_compute_sazonalidade(d, o$train_cutoff, p))
  }
  ## FORBIDDEN no estagio: carga depois de train_cutoff nao altera a Sazonalidade congelada
  p <- with_p(train_cv = FALSE, cutoff_scheme = "legacy_month")
  base <- build(p)
  fr <- data.table::copy(frame); data.table::setattr(fr, "feriados", attr(frame, "feriados")); data.table::setattr(fr, "cuts", attr(frame, "cuts"))
  fr[Data > base$train_cutoff, CargaGlobal := CargaGlobal + 500]
  expect_identical(build(p, fr)$sazonalidade, base$sazonalidade)
  fr2 <- data.table::copy(frame); data.table::setattr(fr2, "feriados", attr(frame, "feriados")); data.table::setattr(fr2, "cuts", attr(frame, "cuts"))
  fr2[Data == base$train_cutoff, CargaGlobal := CargaGlobal + 500]
  expect_gt(max(abs(build(p, fr2)$sazonalidade$Sazonalidade - base$sazonalidade$Sazonalidade)), 1e-5)
})

test_that("[PLUGINS] fe_determinismo: [AC5] determinismo, .Random.seed intacto, dispatch por nome e sem literal de seed", {
  set.seed(99); s0 <- .Random.seed
  a <- build(); b <- build()
  expect_identical(a, b)
  expect_identical(.Random.seed, s0)
  rm(.Random.seed, envir = globalenv())
  build()
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  assign(".Random.seed", s0, envir = globalenv())
  ## a assinatura canonica do loader: (frame, params), cuts do attr da ingestao
  c1 <- P$plugin_build_features(frame, PRM)
  expect_identical(c1$feature_table, a$feature_table)
  expect_identical(names(formals(P$plugin_build_features)), c("frame", "params"))
  expect_identical(P$plugin_descriptor()$stages$feature_engineering$fn(frame, PRM)$feature_table, a$feature_table)
  expect_identical(pc_grep_code(file.path(lgbm_root(), "models", "lgbm", "R"), "123L"), character())
  nc <- data.table::copy(frame); data.table::setattr(nc, "cuts", NULL)
  expect_s3_class(fail(P$plugin_build_features(nc, PRM)), "prevcarga_error")
})
