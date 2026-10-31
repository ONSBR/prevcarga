## test-harness-reduce-batch.R -- M7.V: REDUCE em lote (reescrita testthat portavel de jobs/tests/test_reduce_batch_parity.sh do oraculo,
## sem bash/Docker/S3; ADR-0023, ADR-0032). O unico acoplamento entre origens e o FIT da combina (pesos do mes m = origens de meses < m,
## ESTRITO); o reconcile e causal por origem (k-hat na janela [O-W, O-1]). Deck de 3 meses: A aquece (>= n_min origens), B e C.
##   P1 invariancia causal: fatiar O de [A..O] == fatiar O de [A..O + mes futuro] (delta 0 exato em y_pred/y_true reconciliados e no conjunto de chaves);
##   P2 lote == por-origem: o lote A..C == N backtests de uma origem (aquecimento A + a origem) para as origens do mes B.
## CONTROL: sabotagem que libera `target >= inicio(m)` no fit (mascara de treino aberta) faz P1 reprovar. Tudo em tempdir; sem env vars.
library(testthat)

RB_ROOT <- pc_m7h_root()
RB_DECK <- pc_m7h_deck(lubridate::ymd("2026-07-20"), lubridate::ymd("2026-10-02"))
rb_ymd <- function(x) lubridate::ymd(x)
RB_A <- rb_ymd("2026-08-29") + 0:2                    # mes A: aquecimento (n_min = 3)
RB_B <- rb_ymd("2026-09-01") + 0:2                    # mes B: origens reportadas, contiguas
RB_C <- rb_ymd("2026-10-01") + 0:1                    # mes C: so o futuro causal
RB_O <- rb_ymd("2026-09-02")                          # origem de B do teste de invariancia
rb_mod <- pc_m7h_mod()

rb_run <- function(origins, .frame = parent.frame()) {
  ctx <- pc_m7h_ctx(RB_ROOT, RB_DECK, .local_envir = .frame)
  res <- suppressWarnings(rb_mod$prevcarga_backtest(ctx, origins = origins, models = NULL, areas = NULL, cortes = "08:00",
                                                    baseline = "none"))
  expect_identical(res$summary$verdict, "ok")
  res
}

## fatia da origem O: reconciliado (subsistemas + SIN) com y_pred/y_true e as chaves, ordenado por chave (radix)
rb_slice <- function(res, O) {
  d <- data.table::rbindlist(lapply(c(Sys.glob(file.path(res$paths$reconciled, format(O), "*", "SIN.parquet")),
                                      Sys.glob(file.path(res$paths$reconciled, format(O), "*", "subsistemas", "*.parquet"))),
                                    function(f) data.table::as.data.table(arrow::read_parquet(f))))
  data.table::setorderv(d, c("area", "forecast_origin", "target_datetime", "lead"))
  data.table::setindex(d, NULL)
  d
}
rb_keys <- function(d) paste(d$area, d$forecast_origin, as.numeric(d$target_datetime), d$lead)

test_that("[AC3] P1 invariancia causal: fatiar O de [A..O] == fatiar O de [A..O + mes futuro] (delta 0 exato, mesmo conjunto de chaves)", {
  short <- rb_run(c(RB_A, RB_B[RB_B <= RB_O]))
  long <- rb_run(c(RB_A, RB_B, RB_C))
  s <- rb_slice(short, RB_O); l <- rb_slice(long, RB_O)
  expect_gt(nrow(s), 0L)
  expect_true("SIN" %in% s$area)
  expect_identical(rb_keys(s), rb_keys(l))              # conjunto de chaves
  expect_identical(s$y_pred, l$y_pred)                  # identical(): delta 0 exato
  expect_identical(s$y_true, l$y_true)
  ## a combina de O no mes B usa so o mes A (fit estrito): tambem o combinado de O e identico
  cb <- function(res) { d <- data.table::as.data.table(arrow::read_parquet(file.path(res$paths$combined, "A", "0800", "combined.parquet")))
                        d <- d[d$forecast_origin == format(RB_O), ]; d[order(d$target_datetime), ] }
  expect_identical(cb(short)$y_pred, cb(long)$y_pred)
})

test_that("[AC3] P1 CONTROL: sabotagem que libera target >= inicio(m) no fit faz a invariancia causal reprovar", {
  pc_m7h_mock(prevcarga_combina_train_mask = function(panel, m) is.finite(panel$y_true))
  short <- rb_run(c(RB_A, RB_B[RB_B <= RB_O]))
  long <- rb_run(c(RB_A, RB_B, RB_C))
  s <- rb_slice(short, RB_O); l <- rb_slice(long, RB_O)
  expect_identical(rb_keys(s), rb_keys(l))              # as chaves nao mudam: so o fit enxerga o futuro
  expect_gt(max(abs(s$y_pred - l$y_pred)), 0)
})

test_that("[AC3] P2 lote == por-origem: o lote [A..C] == N backtests de uma origem (aquecimento A + a origem), delta 0 por origem de B", {
  batch <- rb_run(c(RB_A, RB_B, RB_C))
  for (k in seq_along(RB_B)) {
    o <- RB_B[[k]]
    one <- rb_run(c(RB_A, o))
    b <- rb_slice(batch, o); p <- rb_slice(one, o)
    expect_gt(nrow(b), 0L)
    expect_identical(rb_keys(b), rb_keys(p))
    expect_identical(b$y_pred, p$y_pred)
    expect_identical(b$y_true, p$y_true)
  }
  ## CONTROL: origens diferentes diferem (o teste tem dentes) -- a fatia de B[1] nao e a de B[2]
  expect_gt(max(abs(rb_slice(batch, RB_B[[1L]])$y_pred - rb_slice(batch, RB_B[[2L]])$y_pred)), 0)
})
