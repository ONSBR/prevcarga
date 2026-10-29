## helper-combination-gates.R -- fixtures dos gates da combina (M4.V). Auto-carregado por test_dir; nao e codigo de
## produto. Nao edita os helpers de M4.1..M4.4: reusa pc_cmb_panel8_lead/pc_cmb_lead_origins (helper-combination-lead.R),
## pc_cmb_truth/pc_cmb_tz (helper-combination.R). Formula fechada, sem RNG, escala O(1). `env` = modulos carregados pelo teste.

## painel de backtest em que o leak morde: 8 membros de erros distintos (o 1o curto ate O+1), corte 0900, origens diarias,
## y_true realizado ate `truth_until` (NA depois), linhas-isca = origens de fevereiro com alvo >= 2024-03-01.
pc_cmb_g2_panel <- function(env, members, origins = pc_cmb_lead_origins("2024-01-01", "2024-04-03"),
                            truth_until = "2024-04-12") {
  p <- pc_cmb_panel8_lead(env, members, origins = origins)
  lim <- lubridate::ymd(truth_until, tz = pc_cmb_tz)
  p[target_datetime > lim, y_true := NA_real_]
  p[]
}

## copia no formato que o honesty_gate (M3.2) perturba: interval_end = target_datetime (rotulo de fim do LONG) e
## interval_start = fim - 30 min. As colunas livres (f_*) nunca sao perturbadas; os atributos (coverage...) sobrevivem.
pc_cmb_gate_view <- function(panel) {
  v <- data.table::copy(panel)
  v[, `:=`(interval_end = target_datetime,
           interval_start = target_datetime - lubridate::minutes(30))]
  for (n in c("members_declared", "members_present", "provenance", "coverage"))
    if (!is.null(attr(panel, n))) data.table::setattr(v, n, attr(panel, n))
  v[]
}

## run_fn do G2: combina_full (fit + produce do painel recebido) restrita a UMA origem. O escopo
## `forecast_origin == origin` e necessario porque os pesos de meses posteriores veem legitimamente a verdade
## perturbada [TBD: ADR-0029].
pc_cmb_wrap_full <- function(env, ctx, origin) {
  force(origin)
  function(p) {
    out <- suppressWarnings(env$prevcarga_combina_full(p, ctx))
    out[out$forecast_origin == origin, ]
  }
}

## troca prevcarga_combina_train_mask no env do modulo pelo criterio LEAKY por mes-de-origem e restaura em on.exit
pc_cmb_with_leaky_mask <- function(env, code) {
  orig <- get("prevcarga_combina_train_mask", envir = env)
  on.exit(assign("prevcarga_combina_train_mask", orig, envir = env), add = TRUE)
  assign("prevcarga_combina_train_mask",
         function(panel, m) panel$ym < m & !is.na(panel$y_true), envir = env)
  force(code)
}

## painel com a celula de um lead mal-condicionada: f_<s1>/f_<s2> quase colineares (truth +/- d*u) e o resto = truth +
## ruido pequeno, de modo que o FISTA nao alcanca o gradiente em 500 iteracoes (celula nao-estacionaria, warning)
pc_cmb_illcond_lead <- function(panel, lead, d = 3e-4) {
  out <- data.table::copy(panel)
  fc <- grep("^f_", names(out), value = TRUE)
  rows <- out$lead == lead & !is.na(out[[fc[1L]]])
  k <- seq_len(nrow(out))
  tt <- pc_cmb_truth(out$interval_start)
  u <- sin(2 * pi * k / 5) + 0.5 * cos(2 * pi * k / 3)
  idx <- which(rows)
  data.table::set(out, i = idx, j = fc[2L], value = (tt + d * u)[idx])
  data.table::set(out, i = idx, j = fc[3L], value = (tt - d * u)[idx])
  data.table::set(out, i = idx, j = "y_true", value = (tt + 0.3 * d * u)[idx])
  for (n in c("members_declared", "members_present", "provenance", "coverage"))
    if (!is.null(attr(panel, n))) data.table::setattr(out, n, attr(panel, n))
  out[]
}

## X/y de uma celula (lead, cut, ym) reconstruidos pelo painel: mascara de treino + complete-case sobre names(w);
## nunca chama o fit. `cell` = 1 linha da tabela de pesos (lead, cut, ym, w).
pc_cmb_cell_xy <- function(env, panel, cell) {
  w <- cell$w[[1L]]
  grp <- panel[panel$lead == cell$lead & as.character(panel$cut) == cell$cut]
  X <- as.matrix(as.data.frame(grp)[, names(w), drop = FALSE])
  ok <- env$prevcarga_combina_train_mask(grp, cell$ym) & stats::complete.cases(X)
  list(X = X[ok, , drop = FALSE], y = grp$y_true[ok])
}

## gradient mapping do objetivo BRUTO recalculado SEM o modulo (projecao no simplex por bissecao), com intercepto `a`
## dado (NULL = otimo de minimos quadrados para o w dado). Caminho independente do .prevcarga_cmb_gn.
pc_cmb_gate_grad <- function(X, y, w, a = NULL) {
  w <- unname(w)
  if (is.null(a)) a <- mean(y) - sum(colMeans(X) * w)
  r <- y - a - drop(X %*% w)
  gw <- -2 * drop(crossprod(X, r))
  Xc <- sweep(X, 2L, colMeans(X), "-")
  L <- 2 * max(eigen(crossprod(Xc), symmetric = TRUE, only.values = TRUE)$values) + 1e-9
  v <- w - gw / L
  lo <- min(v) - 1; hi <- max(v)
  for (i in 1:200) {
    th <- (lo + hi) / 2
    if (sum(pmax(v - th, 0)) > 1) lo <- th else hi <- th
  }
  p <- pmax(v - (lo + hi) / 2, 0)
  sqrt(sum((L * (w - p))^2) + (2 * sum(r))^2)
}
