## helper-combination.R -- fixtures por FORMULA FECHADA da combina (M4.1). Sem RNG; escala O(1) para o
## grad_tol absoluto ser atingivel. Auto-carregado por test_dir; nao e codigo de produto.

pc_cmb_tz <- "Etc/GMT+3"   # fuso da carga do store

## verdade fechada em funcao do instante (slot de 30 min do dia + dia do ano)
pc_cmb_truth <- function(ts) {
  slot <- lubridate::hour(ts) * 2 + lubridate::minute(ts) / 30
  1 + 0.1 * sin(2 * pi * slot / 48) + 0.03 * cos(2 * pi * lubridate::yday(ts) / 7)
}

.pc_cmb_stamps <- function(n) {
  lubridate::ymd_hms("2024-01-01 00:00:00", tz = pc_cmb_tz) + lubridate::minutes(30 * (seq_len(n) - 1L))
}

## bem-condicionado: colunas = verdade + perturbacoes senoidais distintas; otimo interior exato
pc_cmb_wellcond <- function(n = 60L) {
  i <- seq_len(n)
  t <- pc_cmb_truth(.pc_cmb_stamps(n))
  e <- cbind(0.05 * sin(2 * pi * i / 7), 0.08 * cos(2 * pi * i / 5), 0.06 * sin(2 * pi * i / 11 + 1))
  w_star <- c(0.4, 0.35, 0.25)
  X <- t + e
  colnames(X) <- c("f_A", "f_B", "f_C")
  list(X = X, y = drop(t + e %*% w_star), w_star = w_star)
}

## mal-condicionado: colunas t +/- d*u quase colineares; otimo (0.65, 0.35, 0) na direcao lenta
pc_cmb_illcond <- function(d = 3e-4, n = 60L) {
  i <- seq_len(n)
  t <- pc_cmb_truth(.pc_cmb_stamps(n))
  u <- sin(2 * pi * i / 5) + 0.5 * cos(2 * pi * i / 3)
  X <- cbind(t + d * u, t - d * u, t + 0.05 * sin(2 * pi * i / 13))
  colnames(X) <- c("f_A", "f_B", "f_C")
  list(X = X, y = t + 0.3 * d * u, w_star = c(0.65, 0.35, 0))
}

## painel largo de teste: uma linha por (origem, d_plus, slot, cut); target_datetime = interval_end
## no fuso da carga; ym = mes-de-ORIGEM. `bias` nomeado por membro; `drop` = lista nomeada por membro de
## funcoes(painel) -> logical (onde a coluna vira NA); `truth_until` = "YYYY-MM-DD" (y_true NA depois).
pc_cmb_panel <- function(origins, d_plus = 0:9, slots = c(0L, 18L, 36L, 47L),
                         members = c("A", "B", "C"), cuts = "",
                         bias = stats::setNames(c(0.02, -0.01, 0.03)[seq_along(members)], members),
                         drop = list(), truth_until = NULL) {
  g <- expand.grid(oi = seq_along(origins), d = as.integer(d_plus), slot = as.integer(slots),
                   ci = seq_along(cuts), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  o0 <- lubridate::ymd(origins[g$oi], tz = pc_cmb_tz)
  istart <- o0 + lubridate::days(g$d) + lubridate::minutes(30L * g$slot)
  tdt <- istart + lubridate::minutes(30)
  k <- g$oi * 7 + g$d * 3 + g$slot
  p <- data.frame(area = "X", forecast_origin = origins[g$oi], target_datetime = tdt,
                  lead = g$d * 48L + g$slot, d_plus = g$d, cut = cuts[g$ci],
                  ym = substr(origins[g$oi], 1L, 7L), stringsAsFactors = FALSE)
  truth <- pc_cmb_truth(istart)
  p$y_true <- truth + 0.01 * sin(2 * pi * k / 11)
  if (!is.null(truth_until))
    p$y_true[tdt > lubridate::ymd(truth_until, tz = pc_cmb_tz)] <- NA_real_
  for (j in seq_along(members)) {
    m <- members[j]
    p[[paste0("f_", m)]] <- truth + bias[[m]] + 0.02 * j * sin(2 * pi * (k + 3 * g$ci) / (5 + j) + j)
  }
  for (m in names(drop)) p[[paste0("f_", m)]][drop[[m]](p)] <- NA_real_
  p
}

## linhas-isca: ym < m (origem em mes anterior) cujo intervalo de alvo comeca em/apos inicio(m)
pc_cmb_bait_row <- function(panel, m) {
  ms <- lubridate::ymd(paste0(m, "-01"), tz = pc_cmb_tz)
  panel$ym < m & (panel$target_datetime - lubridate::minutes(30)) >= ms
}

## gradient mapping do objetivo bruto recalculado SEM chamar o modulo (projecao por bissecao)
pc_cmb_grad_indep <- function(X, y, w) {
  a <- mean(y) - sum(colMeans(X) * w)
  r <- y - a - drop(X %*% w)
  gw <- -2 * drop(t(X) %*% r)
  Xc <- t(t(X) - colMeans(X))
  L <- 2 * max(eigen(t(Xc) %*% Xc, symmetric = TRUE)$values) + 1e-9
  v <- w - gw / L
  lo <- min(v) - 1; hi <- max(v)
  for (i in 1:200) {
    th <- (lo + hi) / 2
    if (sum(pmax(v - th, 0)) > 1) lo <- th else hi <- th
  }
  p <- pmax(v - (lo + hi) / 2, 0)
  sqrt(sum((L * (w - p))^2) + (2 * sum(r))^2)
}
