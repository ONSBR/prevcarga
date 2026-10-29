## helper-coherence.R -- fixtures do check-coherence (M3.4; reusadas pelo M3.V). Nao e codigo de produto.
## Auto-carregado por testthat::test_dir. Nomes de area lidos de `topo` (so o literal de teste `RR` e fixo);
## tudo em tempfile(): nunca toca data/ nem config/.

## LONG reconciliado coerente: folhas = curvas positivas distintas; subsistema = khat * soma das folhas (khat escalar ou
## nomeado por subsistema); SIN = soma dos subsistemas. 8 colunas, SEM nos de perda. target_datetime = interval_end.
pc_coherent_long <- function(topo, origin = "2026-10-15", d_plus = 0:1, khat = 1.05, ctx) {
  o0 <- lubridate::force_tz(lubridate::as_datetime(origin), ctx$tz)
  g <- expand.grid(slot = 1:48, k = as.integer(d_plus), KEEP.OUT.ATTRS = FALSE)
  curve <- function(i) 100 * i + g$slot + 10 * g$k + (i %% 7) / 10
  kh <- function(s) if (!is.null(names(khat))) khat[[s]] else khat
  y <- list(); i <- 0L
  for (s in topo$order) {
    for (a in topo$subsystems[[s]]$areas) { i <- i + 1L; y[[a]] <- curve(i) }
    y[[s]] <- kh(s) * Reduce(`+`, y[topo$subsystems[[s]]$areas])
  }
  y[["SIN"]] <- Reduce(`+`, y[topo$order])
  order_ <- c("SIN", topo$order, topo$leaves)
  do.call(rbind, lapply(order_, function(a) data.frame(
    area = a, forecast_origin = origin,
    target_datetime = o0 + lubridate::days(g$k) + lubridate::minutes(30L * g$slot),
    lead = as.integer(g$k * 48L + g$slot), y_pred = y[[a]], y_true = NA_real_,
    d_plus = g$k, granularity_min = 30L, stringsAsFactors = FALSE)))
}

## `at` = indice (1-based) do instante dentro dos instantes distintos ordenados do LONG (default o 1o).
.pc_coh_rows <- function(long, area, at) {
  t <- sort(unique(as.numeric(long$target_datetime)))[at]
  which(long$area %in% area & as.numeric(long$target_datetime) %in% t)
}

pc_coh_bump <- function(long, area, delta, at = 1L) {
  i <- .pc_coh_rows(long, area, at); long$y_pred[i] <- long$y_pred[i] + delta; long
}

## Reescala UMA folha de `s` para que k = subsistema/soma das folhas valha `k`, mantendo subsistema e SIN.
pc_coh_set_khat <- function(long, topo, s, k, at = 1L) {
  lv <- topo$subsystems[[s]]$areas
  v <- function(a) long$y_pred[.pc_coh_rows(long, a, at)]
  new <- v(s) / k - Reduce(`+`, lapply(lv[-1L], v))
  long$y_pred[.pc_coh_rows(long, lv[[1L]], at)] <- new
  long
}

pc_coh_drop <- function(long, areas, at = 1L) {
  long[-.pc_coh_rows(long, areas, at), , drop = FALSE]
}

pc_coh_add_area <- function(long, area = "RR") {
  x <- long[long$area == long$area[[1L]], , drop = FALSE]
  x$area <- area
  rbind(long, x)
}

## Desloca a grade de `area` na origem em `minutes`; recalcula lead/d_plus pelo dia do interval_start (contrato do LONG).
pc_coh_shift_grid <- function(long, area, origin, minutes = 30) {
  i <- which(long$area == area & long$forecast_origin == origin)
  long$target_datetime[i] <- long$target_datetime[i] + lubridate::minutes(minutes)
  o <- lubridate::ymd(origin, tz = lubridate::tz(long$target_datetime))
  istart <- long$target_datetime[i] - lubridate::minutes(30L)
  long$d_plus[i] <- as.integer(lubridate::as_date(istart, tz = lubridate::tz(istart)) - lubridate::as_date(o))
  long
}

.pc_coh_topology_lines <- function() readLines(file.path(pc_root(), "config", "topology.yaml"), warn = FALSE)

## topology.yaml de fixture em `path` a partir do seed: faixa de k-hat de `s` sobrescrita ...
pc_coh_topology_override <- function(path, s, range) {
  ln <- .pc_coh_topology_lines()
  hit <- grepl(sprintf("^\\s+\"?%s\"?: \\{", s), ln)
  ln[hit] <- sub("\\}\\s*$", sprintf(", khat_range: [%.2f, %.2f]}", range[[1L]], range[[2L]]), ln[hit])
  writeLines(ln, path); invisible(path)
}

## ... ou uma folha extra `leaf` em `s`.
pc_coh_topology_extra_leaf <- function(path, leaf, s) {
  ln <- .pc_coh_topology_lines()
  hit <- grepl(sprintf("^\\s+\"?%s\"?: \\{", s), ln)
  ln[hit] <- sub("\\]", sprintf(", %s]", leaf), ln[hit])
  writeLines(ln, path); invisible(path)
}
