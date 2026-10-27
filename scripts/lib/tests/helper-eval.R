## helper-eval.R -- fixtures de avaliacao (M3.1; reusados por M3.3/M3.4/M3.V). Nao e codigo de produto.
## Auto-carregado por testthat::test_dir. Tudo em tempfile(): nunca toca data/.
## Constroi sobre o deck sentinela do helper-deck.R (carga = slot do dia + 1000 * indice da area).

.pc_eval_origin <- "2026-10-15"

## ctx de teste sobre o deck sentinela: 21 dias antes de O e 10 dias a partir de O, fuso Etc/GMT+3.
pc_eval_ctx <- function(dir = tempfile("eval-deck"), origin = .pc_eval_origin, areas = c("A1", "A2"),
                        label = "end") {
  o <- lubridate::ymd(origin)
  pc_deck_sentinel(label, days = o + (-21:9), areas = areas, dir = dir)
  c(pc_ctx_deck(dir), list(tz = .pc_deck_tz, areas = areas, origin = origin))
}

## LONG de 8 colunas: 48 intervalos/dia x d_plus; target_datetime = interval_end; lead sequencial;
## y_true = carga do deck (formula do sentinela) e y_pred = y_true + bias.
pc_eval_long <- function(areas, origin = .pc_eval_origin, d_plus = 0:9, bias = 0, ctx) {
  o0 <- lubridate::force_tz(lubridate::as_datetime(origin), ctx$tz)
  parts <- lapply(areas, function(a) {
    g <- expand.grid(slot = 1:48, k = as.integer(d_plus), KEEP.OUT.ATTRS = FALSE)
    y <- as.double(g$slot + 1000L * match(a, ctx$areas))
    data.frame(area = a, forecast_origin = origin,
               target_datetime = o0 + lubridate::days(g$k) + lubridate::minutes(30L * g$slot),
               lead = as.integer(g$k * 48L + g$slot), y_pred = y + bias, y_true = y,
               d_plus = g$k, granularity_min = 30L, stringsAsFactors = FALSE)
  })
  do.call(rbind, parts)
}

## Reescreve a carga do deck aplicando `fn` a load_mwh nos intervalos com interval_start em [from, to).
pc_eval_perturb <- function(dir, area, from, to, fn) {
  dt <- .pc_deck_read(dir, "load"); v <- .pc_deck_view(dt, "load", dir)
  idx <- which(v$area_code == area & v$interval_start >= from & v$interval_start < to)
  data.table::set(dt, i = idx, j = "load_mwh", value = fn(dt$load_mwh[idx]))
  .pc_deck_rewrite(dt, dir, "load")
  invisible(length(idx))
}

## Deck sem a coluna `col` na carga (default load_mwh).
pc_eval_drop_col <- function(dir, col = "load_mwh") {
  dt <- .pc_deck_read(dir, "load")
  dt[, (col) := NULL]
  .pc_deck_rewrite(dt, dir, "load")
  invisible(dir)
}

pc_eval_write_long <- function(long) {
  p <- tempfile("long", fileext = ".parquet")
  arrow::write_parquet(long, p)
  p
}
