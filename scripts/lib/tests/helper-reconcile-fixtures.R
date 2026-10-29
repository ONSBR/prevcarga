## helper-reconcile-fixtures.R -- fixtures do epico Reconcile (R.1). Auto-carregado por test_dir; nao e codigo de produto.
## Nada de literal da topologia real: subsistemas SS1..SSn e folhas Ls<i>_<j> sao sinteticos, em tempdir().

pc_rc_tz <- "Etc/GMT+3"
pc_rc_ctx <- function() list(tz = pc_rc_tz)

## topology.yaml de fixture com `n_ss` subsistemas (3 ou 5); subsistema i tem i+1 folhas. `move` = list(leaf, to)
## move uma folha de subsistema (CONTROL do AC1). Devolve o caminho do YAML (gravado em tempdir()).
pc_topology_fixture <- function(n_ss = 3L, move = NULL, dir = tempfile("topo-")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ss <- stats::setNames(lapply(seq_len(n_ss), function(i) paste0("Ls", i, "_", seq_len(i + 1L))),
                        paste0("SS", seq_len(n_ss)))
  if (!is.null(move)) {
    for (s in names(ss)) ss[[s]] <- setdiff(ss[[s]], move$leaf)
    ss[[move$to]] <- c(ss[[move$to]], move$leaf)
  }
  lines <- c("schema_version: 1", "khat_range: [1.0, 1.2]", "subsystems:",
             unlist(lapply(seq_along(ss), function(i)
               sprintf("  %s: {areas: [%s], loss: P%d}", names(ss)[[i]], paste(ss[[i]], collapse = ", "), i))),
             sprintf("order: [%s]", paste(names(ss), collapse = ", ")),
             "members:", "  - {sigla: M1, mdir: random-forest, exogena: null}")
  f <- file.path(dir, "topology.yaml")
  writeLines(lines, f)
  f
}

## codigo estavel da folha (nao depende da posicao no YAML): Ls<i>_<j> -> 10*i + j
pc_leaf_code <- function(leaf) {
  p <- as.integer(strsplit(sub("^Ls", "", leaf), "_", fixed = TRUE)[[1L]])
  10L * p[[1L]] + p[[2L]]
}

## LONG de folhas (8 col, semi-horario): y_pred = 1000 * codigo_da_folha + indice do intervalo (1..48).
## target_datetime = interval_end no tz; o ultimo intervalo (D+1 00:00) tem d_plus 0 (dia do interval_start).
## drop = list(leaf =, idx =) remove esses intervalos da folha (corte desalinhado).
pc_leaves_long <- function(topology, origin, ctx, drop = NULL, slots = 1:48) {
  o <- lubridate::ymd(origin, tz = ctx$tz)
  do.call(rbind, lapply(topology$leaves, function(leaf) {
    code <- pc_leaf_code(leaf)
    idx <- setdiff(slots, if (!is.null(drop) && identical(drop$leaf, leaf)) drop$idx else integer())
    data.frame(area = leaf, forecast_origin = origin,
               target_datetime = o + lubridate::minutes(30L * idx),
               lead = as.integer(idx - 1L), y_pred = 1000 * code + idx, y_true = NA_real_,
               d_plus = 0L, granularity_min = 30L, stringsAsFactors = FALSE)
  }))
}

## y_pred esperado de um subsistema no intervalo idx, recalculado SEM o modulo (soma fechada dos codigos)
pc_leaves_expected <- function(topology, ss, idx) {
  codes <- vapply(topology$subsystems[[ss]]$areas, pc_leaf_code, 0L)
  vapply(idx, function(i) sum(1000 * codes + i), 0)
}

## ratio (saida do ajuste de verdade) com valor conhecido por dia: uma linha por dia as 12:00 (interval_start)
pc_truth_ratio <- function(days, f, tz = pc_rc_tz) {
  st <- lubridate::ymd_hm(paste(format(days), "12:00"), tz = tz)
  data.frame(interval_start = st, interval_end = st + lubridate::minutes(30L), ratio = vapply(days, f, 0),
             stringsAsFactors = FALSE)
}

## verdade do leitor (area_code, interval_start, interval_end, load_mwh): folha = 100*codigo + idx, SS = factor * soma
pc_truth_load <- function(topology, ss, day, factor = 1.1, miss = NULL, tz = pc_rc_tz) {
  st <- lubridate::ymd(day, tz = tz) + lubridate::minutes(30L * (0:47))
  codes <- stats::setNames(vapply(topology$leaves, pc_leaf_code, 0L), topology$leaves)
  leaves <- topology$subsystems[[ss]]$areas
  rows <- lapply(leaves, function(l) data.frame(area_code = l, interval_start = st,
                 interval_end = st + lubridate::minutes(30L), load_mwh = 100 * codes[[l]] + 0:47))
  tot <- Reduce(`+`, lapply(rows, function(r) r$load_mwh))
  rows[[length(rows) + 1L]] <- data.frame(area_code = ss, interval_start = st,
                 interval_end = st + lubridate::minutes(30L), load_mwh = factor * tot)
  out <- do.call(rbind, rows)
  if (!is.null(miss)) out <- out[!(out$area_code == miss$leaf & out$interval_start %in% st[miss$idx]), ]
  out
}

## perfil sentinela: k = 1 + hour/100 + month/1e4 + dow/1e6 em todos os 24 x 12 x 7 buckets (dow: dom = 1)
pc_khat_profile_sentinel <- function(glob = 9) {
  g <- expand.grid(hour = 0:23, month = 1:12, dow = 1:7, KEEP.OUT.ATTRS = FALSE)
  g$k <- 1 + g$hour / 100 + g$month / 1e4 + g$dow / 1e6
  g$n <- 1L
  list(profile = data.table::as.data.table(g), glob = glob, n_ratio = nrow(g),
       window_from = lubridate::ymd("2020-01-01"), window_to = lubridate::ymd("2020-01-02"))
}

## perfil unitario (k = 1 em todos os buckets) para o AC1
pc_khat_profile_unit <- function() {
  p <- pc_khat_profile_sentinel(glob = 1)
  p$profile$k <- 1
  p
}

## diretorio de estado do epico (para os tickets seguintes) e escrita das folhas em <dir>/<area>/long.parquet
pc_reconcile_state_dir <- function() {
  d <- tempfile("reconc-state-")
  dir.create(d, recursive = TRUE)
  d
}
pc_write_leaves <- function(dir, long) {
  for (a in unique(long$area)) {
    p <- file.path(dir, a)
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(long[long$area == a, , drop = FALSE], file.path(p, "long.parquet"))
  }
  invisible(dir)
}
