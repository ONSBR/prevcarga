## test-spline-rule.R -- RF.3, AC 4: rf_semihourly_rule x transcricao literal de run_pipeline.R:503-515 (oraculo).
library(testthat)

## Transcricao literal do oraculo (n_h = 216); `clamp` e `method` so existem aqui para os CONTROLs.
oracle_rule <- function(y_hourly, clamp = TRUE, method = "monoH.FC") {
  n_h <- length(y_hourly)
  x_anchor <- seq.int(0L, n_h - 1L)
  grid <- seq(0, n_h - 0.5, by = 0.5)
  f_spline <- stats::splinefun(x_anchor, y_hourly, method = method)
  y_half <- f_spline(grid)
  .even <- which(seq_along(grid) %% 2L == 0L & grid <= max(x_anchor))
  if (clamp) for (.i in .even) {
    .lo <- min(y_half[.i - 1L], y_half[.i + 1L]); .hi <- max(y_half[.i - 1L], y_half[.i + 1L])
    y_half[.i] <- min(max(y_half[.i], .lo), .hi)
  }
  beyond <- grid > max(x_anchor)
  if (any(beyond)) {
    slope <- y_hourly[n_h] - y_hourly[n_h - 1L]
    y_half[beyond] <- y_hourly[n_h] + (grid[beyond] - max(x_anchor)) * slope
  }
  y_half
}

k <- 1:216
vecs <- list(
  diurno = 900 + 300 * sin(2 * pi * (k - 6) / 24) + 0.5 * k,
  degrau = c(rep(100, 100), rep(1000, 116)),
  overshoot = 500 + 300 * sin(k^2 * 0.37),     # monoH.FC passa das ancoras vizinhas nesta serie
  zigzag = 500 + 400 * sin(k * 2.1) * (k %% 3))

test_that("[PLUGINS] spline_rule: 432 valores, <= 1e-12 contra a transcricao literal do oraculo", {
  p <- rf_plug()
  for (nm in names(vecs)) {
    got <- p$rf_semihourly_rule(vecs[[nm]])
    expect_length(got, 432L)
    expect_lte(max(abs(got - oracle_rule(vecs[[nm]]))), 1e-12)
  }
})

test_that("[PLUGINS] spline_rule: ancoras exatas, :30 interior entre vizinhas, ultimo slot por extrapolacao linear", {
  p <- rf_plug()
  for (nm in names(vecs)) {
    y <- vecs[[nm]]; got <- p$rf_semihourly_rule(y)
    expect_identical(got[seq(1L, 431L, by = 2L)], y)
    mid <- got[seq(2L, 430L, by = 2L)]
    nb_lo <- pmin(y[1:215], y[2:216])[1:215]; nb_hi <- pmax(y[1:215], y[2:216])[1:215]
    expect_true(all(mid >= nb_lo - 1e-9 & mid <= nb_hi + 1e-9))
    expect_identical(got[[432L]], y[[216L]] + 0.5 * (y[[216L]] - y[[215L]]))
  }
})

test_that("[PLUGINS] spline_rule: cortar a faixa depois do spline nao muda nenhum valor mantido", {
  p <- rf_plug(); y <- vecs$overshoot
  full <- p$rf_semihourly_rule(y)
  ## o spline e do bloco inteiro: cortar a serie ANTES muda os valores proximos do corte (CONTROL), depois nao
  short <- p$rf_semihourly_rule(y[1:120])
  expect_gt(max(abs(short[1:238] - full[1:238])), 1e-9)
  keep <- full[1:96]
  expect_identical(keep, p$rf_semihourly_rule(y)[1:96])
})

test_that("[PLUGINS] spline_rule: CONTROL fmm ou sem clamp divergem > 1e-5 no vetor com overshoot", {
  p <- rf_plug(); y <- vecs$overshoot
  got <- p$rf_semihourly_rule(y)
  expect_gt(max(abs(got - oracle_rule(y, method = "fmm"))), 1e-5)
  expect_gt(max(abs(got - oracle_rule(y, clamp = FALSE))), 1e-5)
  ## o argumento data.frame (forma que to_semihourly entrega) e equivalente ao vetor
  df <- data.table::data.table(interval_end = seq_along(y), y = y)
  expect_identical(p$rf_semihourly_rule(df), got)
})
