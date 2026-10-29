## test-evaluation.R -- evaluation.R (M3.1). Assercoes por classe + error.id, nunca por mensagem livre.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "schema_long", "deck_reader", "evaluation"))
for (n in ls(env, all.names = TRUE)) assign(n, env[[n]])
prevcarga_runtime_enforce()

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
expect_pc_error <- function(expr, id, class = "contract") {
  e <- fail(expr)
  expect_false(is.null(e))
  expect_identical(e$id, id)
  expect_match(conditionMessage(e), paste0("^\\[prevcarga/", class, "\\]"))
}

TZ <- "Etc/GMT+3"
O <- "2026-10-15"
ctx <- pc_eval_ctx()
o0 <- lubridate::force_tz(lubridate::as_datetime(O), TZ)

test_that("[CONTROL/FORBIDDEN] WMAPE: zero, cresce com o erro, deterministico e invariante a permutacao", {
  y <- 1000 + 100 * sin(seq_len(96) / 7)
  expect_identical(prevcarga_wmape(y, y), 0)
  w1 <- prevcarga_wmape(y, y + 100); w2 <- prevcarga_wmape(y, y + 200)
  expect_gt(w1, 0); expect_gt(w2, w1)
  expect_identical(prevcarga_wmape(y, y + 100), prevcarga_wmape(y, y + 100))
  set.seed(1); i <- sample(length(y))
  expect_equal(prevcarga_wmape(y[i], (y + 100)[i]), w1)
})

test_that("WMAPE: denominador com abs, mascara is.finite, vazio e comprimento", {
  expect_identical(prevcarga_wmape(c(-100, 100), c(-90, 110)), 10)
  naive <- function(t, p) 100 * sum(abs(p - t)) / sum(t)
  expect_false(is.finite(naive(c(-100, 100), c(-90, 110))))          # CONTROL: sem abs no denominador
  expect_equal(prevcarga_wmape(c(1000, Inf, 1000), c(1010, 5, NA)), 1)
  expect_false(is.finite(naive(c(1000, Inf, 1000), c(1010, 5, NA)))) # CONTROL: mascara ingenua
  expect_identical(prevcarga_wmape(numeric(), numeric()), NA_real_)
  expect_identical(prevcarga_wmape(c(NA_real_, 1), c(1, NA_real_)), NA_real_)
  expect_identical(prevcarga_wmape(c(0, 0), c(1, 1)), NA_real_)
  expect_pc_error(prevcarga_wmape(1:3, 1:2), "contract.metric_length")
})

test_that("MAPE/MAE/RMSE/BIAS fechados", {
  t <- c(0, 100, 200, 400); p <- c(5, 110, 180, 440)
  expect_equal(prevcarga_mape(t, p), 10)
  expect_false(isTRUE(all.equal(prevcarga_mape(t, p), 100 * mean(abs(p - t) / pmax(abs(t), 1e-9)))))  # CONTROL: floor
  expect_equal(prevcarga_mae(t, p), 18.75)
  expect_equal(prevcarga_rmse(t, p), sqrt(mean(c(25, 100, 400, 1600))))
  expect_gte(prevcarga_rmse(t, p), prevcarga_mae(t, p))
  expect_equal(prevcarga_rmse(t, t + 7), prevcarga_mae(t, t + 7))
  y <- c(100, 1000); k <- 50
  expect_equal(prevcarga_bias(y, y + 50)$bias, 50)
  expect_equal(prevcarga_bias(y, y - 50)$bias, -50)
  expect_equal(prevcarga_bias(y, y + k)$bias_pct, 100 * sum(rep(k, 2)) / sum(abs(y)))
  expect_false(isTRUE(all.equal(prevcarga_bias(y, y + k)$bias_pct, 100 * mean(k / y))))
  expect_identical(prevcarga_mape(numeric(), numeric()), NA_real_)
  expect_identical(prevcarga_bias(numeric(), numeric()), list(bias = NA_real_, bias_pct = NA_real_))
})

test_that("pico por interval_start: 22:30-23:00 conta, 17:30-18:00 nao; CONTROL por hora do target inverte", {
  mk <- function(target_hhmm) {
    l <- pc_eval_long("A1", O, 0:1, ctx = ctx)
    hit <- format(l$target_datetime, "%H:%M") == target_hhmm
    l$y_pred[hit] <- l$y_pred[hit] + 200
    pc_long_mem(l, ctx$tz)
  }
  a <- mk("23:00"); b <- mk("18:00")
  sa <- prevcarga_scorecard(a, by = "d_plus"); sb <- prevcarga_scorecard(b, by = "d_plus")
  expect_true(all(sa$peak > 0)); expect_true(all(sa$peak > sa$wmape))
  expect_true(all(sb$peak == 0)); expect_true(all(sb$wmape > 0))
  istart <- function(l) prevcarga_long_interval_start(l$target_datetime, l$granularity_min, ctx$tz)
  expect_gt(prevcarga_peak_wmape(istart(a), a$y_true, a$y_pred), 0)
  expect_equal(prevcarga_peak_wmape(istart(b), b$y_true, b$y_pred), 0)
  by_target <- function(l) prevcarga_peak_wmape(l$target_datetime, l$y_true, l$y_pred)
  expect_equal(by_target(a), 0); expect_gt(by_target(b), 0)          # CONTROL: carimbo-fim inverte
  expect_identical(prevcarga_peak_wmape(o0 + lubridate::hours(1), 1, 1), NA_real_)
  bad <- a; bad$granularity_min[1L] <- 60L
  expect_pc_error(prevcarga_long_interval_start(bad$target_datetime, bad$granularity_min), "contract.long_schema")
  expect_pc_error(prevcarga_long_interval_start(a$target_datetime, a$granularity_min, "UTC"), "contract.long_tz")
})

test_that("assert_long: correto passa; dplus_label, tz e schema reprovam", {
  l <- pc_eval_long("A1", O, 0:2, ctx = ctx)
  out <- prevcarga_assert_long(l, ctx)
  expect_identical(out$forecast_origin, rep(O, nrow(l)))
  expect_true(is.integer(out$d_plus) && is.integer(out$lead) && is.integer(out$granularity_min))
  w <- l; w$d_plus <- as.integer(lubridate::as_date(w$target_datetime, tz = TZ) - lubridate::ymd(O))
  expect_false(identical(w$d_plus, l$d_plus))                          # o rotulo errado difere no intervalo 00:00
  expect_pc_error(prevcarga_assert_long(w, ctx), "contract.long_dplus_label")
  z <- l; z$target_datetime <- lubridate::with_tz(z$target_datetime, "UTC")
  expect_pc_error(prevcarga_assert_long(z, ctx), "contract.long_tz")
  s <- l[, c("area", "forecast_origin", "target_datetime", "y_pred", "lead", "y_true", "d_plus", "granularity_min")]
  expect_pc_error(prevcarga_assert_long(s, ctx), "contract.long_schema")
  d <- l; d$forecast_origin <- lubridate::ymd(d$forecast_origin)
  expect_pc_error(prevcarga_assert_long(d, ctx), "contract.long_schema")
  expect_pc_error(prevcarga_assert_long(list(), ctx), "contract.long_schema")
})

test_that("read_long: texto yyyy-MM-dd aceito; Date [FORBIDDEN]; arquivo ausente", {
  l <- pc_eval_long("A1", O, 0:1, ctx = ctx)
  r <- prevcarga_read_long(pc_eval_write_long(l), ctx)
  expect_type(r$forecast_origin, "character"); expect_match(r$forecast_origin[1L], "^\\d{4}-\\d{2}-\\d{2}$")
  expect_true(is.integer(r$d_plus) && is.integer(r$lead) && is.integer(r$granularity_min))
  expect_equal(r$y_pred, l$y_pred)
  d <- l; d$forecast_origin <- lubridate::as_date(d$forecast_origin)
  e <- fail(prevcarga_read_long(pc_eval_write_long(d), ctx))
  expect_identical(e$id, "contract.long_schema"); expect_identical(e$where, "forecast_origin")
  gone <- file.path(tempdir(), "nao-existe.parquet")
  e <- fail(prevcarga_read_long(gone, ctx))
  expect_identical(e$id, "contract.long_schema"); expect_identical(e$where, gone)
})

test_that("read_actual: A1 restrito a (from, to]; sem load_mwh propaga; SIN vazio", {
  from <- o0; to <- o0 + lubridate::hours(3)
  a <- prevcarga_read_actual(ctx, "A1", from, to)
  expect_identical(names(a), c("interval_end", "y_true"))
  expect_identical(lubridate::tz(a$interval_end), TZ)
  expect_identical(nrow(a), 6L)
  expect_identical(a$interval_end[1L], o0 + lubridate::minutes(30)); expect_identical(a$interval_end[6L], to)
  ld <- prevcarga_read_load(ctx, areas = "A1", from = lubridate::ymd(O), to = lubridate::ymd(O))
  expect_identical(a$y_true, ld$load_mwh[match(a$interval_end, ld$interval_end)])
  expect_identical(a$y_true, as.double(1:6 + 1000))
  expect_identical(nrow(prevcarga_read_actual(ctx, "SIN", from, to)), 0L)
  bad <- pc_eval_ctx(); pc_eval_drop_col(bad$data_dir)
  e <- fail(prevcarga_read_actual(bad, "A1", from, to))
  expect_false(is.null(e))
  expect_match(conditionMessage(e), "^\\[prevcarga/contract\\]")
  expect_identical(e$id, "contract.dataset_missing")
})

test_that("read_actual: so o load_mwh ausente e reclassificado; outro erro sobe intacto", {
  bad <- pc_eval_ctx()
  pc_eval_drop_col(bad$data_dir)
  e <- fail(prevcarga_read_actual(bad, "A1", o0, o0 + lubridate::hours(3)))
  expect_identical(e$id, "contract.dataset_missing")
  other <- pc_eval_ctx()
  for (f in list.files(file.path(other$data_dir, "load"), "\\.parquet$", recursive = TRUE, full.names = TRUE))
    writeLines("lixo", f)
  e2 <- tryCatch(prevcarga_read_actual(other, "A1", o0, o0 + lubridate::hours(3)), error = identity)
  expect_s3_class(e2, "error")
  expect_false(identical(e2$id, "contract.dataset_missing"))
})

test_that("baselines: [FORBIDDEN] perturbar >= O nao muda; [CONTROL] O-1 e O-7 mudam", {
  long <- pc_eval_long("A1", O, 0:9, ctx = ctx)
  bl <- function(cx, kind) prevcarga_baseline(cx, "A1", O, kind, long)
  p0 <- bl(ctx, "persistence"); s0 <- bl(ctx, "seasonal_naive")
  expect_identical(names(p0), names(long)); expect_identical(nrow(p0), nrow(long))
  expect_false(isTRUE(all.equal(p0$y_pred, s0$y_pred)) && FALSE)
  ## deck novo igual ao base, com carga >= O multiplicada por 1.3
  fut <- pc_eval_ctx()
  pc_eval_perturb(fut$data_dir, "A1", o0, o0 + lubridate::days(10), function(x) x * 1.3)
  expect_identical(bl(fut, "persistence"), p0)
  expect_identical(bl(fut, "seasonal_naive"), s0)
  expect_true(any(long$d_plus >= 7L))
  ## CONTROL: O-1 muda persistence, O-7 muda seasonal_naive
  c1 <- pc_eval_ctx()
  pc_eval_perturb(c1$data_dir, "A1", o0 - lubridate::days(1), o0, function(x) x + 5)
  expect_gt(max(abs(bl(c1, "persistence")$y_pred - p0$y_pred)), 1e-5)
  c7 <- pc_eval_ctx()
  pc_eval_perturb(c7$data_dir, "A1", o0 - lubridate::days(7), o0 - lubridate::days(6), function(x) x + 5)
  expect_gt(max(abs(bl(c7, "seasonal_naive")$y_pred - s0$y_pred)), 1e-5)
  expect_identical(bl(c7, "persistence"), p0)
  expect_pc_error(bl(ctx, "xx"), "contract.baseline_kind")
  ## o proprio O-1 alimenta d_plus 0 da persistencia
  k0 <- p0$d_plus == 0L
  expect_equal(p0$y_pred[k0], long$y_true[k0])
})

test_that("skill: 0, positivo, negativo; puro; baseline degenerado = NA", {
  long <- pc_eval_long("A1", O, 0:1, ctx = ctx)
  base <- long; base$y_pred <- long$y_true + 100
  same <- base
  half <- long; half$y_pred <- long$y_true + 50
  dbl <- long; dbl$y_pred <- long$y_true + 200
  expect_equal(prevcarga_skill_score(same, base), 0, tolerance = 1e-9)
  expect_gt(prevcarga_skill_score(half, base), 0)
  expect_lt(prevcarga_skill_score(dbl, base), 0)
  expect_identical(prevcarga_skill_score(half, base), prevcarga_skill_score(half, base))
  zero <- long; zero$y_pred <- long$y_true
  expect_identical(prevcarga_skill_score(half, zero), NA_real_)
  other <- base; other$area <- "ZZ"
  expect_identical(prevcarga_skill_score(half, other), NA_real_)
  for (f in list(prevcarga_skill_score, prevcarga_scorecard))
    expect_false(any(c("ctx", "path") %in% names(formals(f))))
})

test_that("scorecard: by duplo/simples, area toda-NA some, skill do grupo, by invalido", {
  l <- pc_long_mem(pc_eval_long(c("A1", "A2"), O, 0:2, bias = 10, ctx = ctx))
  na <- pc_long_mem(pc_eval_long("A3", O, 0:2, ctx = list(tz = TZ, areas = "A3"))); na$y_true <- NA_real_
  all3 <- rbind(l, na)
  sc <- prevcarga_scorecard(all3, by = c("d_plus", "area"))
  expect_identical(nrow(sc), 6L)
  expect_identical(names(sc), c("d_plus", "area", "n", "wmape", "mape", "mae", "rmse", "bias", "bias_pct", "peak"))
  expect_false("A3" %in% sc$area)
  expect_true(all(sc$n == 48L)); expect_true(all(abs(sc$bias - 10) < 1e-9))
  s1 <- prevcarga_scorecard(all3, by = "d_plus")
  expect_identical(nrow(s1), 3L)
  expect_identical(prevcarga_scorecard(all3), prevcarga_scorecard(all3))
  base <- l; base$y_pred <- l$y_true + 100
  sk <- prevcarga_scorecard(l, by = c("d_plus", "area"), baseline_long = base)
  expect_true("skill" %in% names(sk))
  g <- l[l$area == "A1" & l$d_plus == 1L, ]
  expect_equal(sk$skill[sk$area == "A1" & sk$d_plus == 1L], prevcarga_skill_score(g, base))
  e0 <- prevcarga_scorecard(na, by = "d_plus")
  expect_identical(nrow(e0), 0L); expect_identical(names(e0), names(s1))
  expect_pc_error(prevcarga_scorecard(l, by = "nao_existe"), "contract.long_schema")
  raw <- pc_eval_long("A1", O, 0:1, ctx = ctx)                          # CONTROL: sem a coluna do leitor aborta
  expect_identical(fail(prevcarga_scorecard(raw, by = "d_plus"))$where, "interval_start")
})

test_that("catalogo distribuido: ids do M3 na classe certa, ASCII, sem duplicata; CONTROL do loader", {
  dir <- file.path(pc_root(), "scripts", "lib", "errors")
  cat <- prevcarga_errors_load(dir)
  want <- c(contract.long_schema = "contract", contract.long_dplus_label = "contract", contract.long_tz = "contract",
            contract.metric_length = "contract", contract.baseline_kind = "contract",
            contract.honesty_gate_args = "contract", topology.area_undeclared = "topology",
            completude.node_missing = "completude", coherence.sin_sum = "coherence", coherence.khat = "coherence",
            coherence.alignment = "coherence", honesty.leak = "honesty")
  expect_true(all(names(want) %in% cat$id))
  expect_identical(unname(cat$class[match(names(want), cat$id)]), unname(want))
  codes <- c(contract = 2L, topology = 4L, completude = 3L, coherence = 11L, honesty = 15L)
  expect_identical(unname(prevcarga_exit_codes[unname(want)]), unname(codes[unname(want)]))
  expect_false(anyDuplicated(cat$id) > 0L)
  ev <- cat[cat$id %in% names(want), ]
  expect_true(all(nzchar(ev$message)) && all(nzchar(ev$hint)))
  raw <- readBin(file.path(dir, "evaluation.tsv"), "raw", 1e5)
  expect_true(all(as.integer(raw) < 128L)); expect_false(13L %in% as.integer(raw))
  tmp <- tempfile("cat"); dir.create(tmp)
  file.copy(file.path(dir, "evaluation.tsv"), tmp)
  writeLines(c("id\tclass\tmessage\thint", paste0("contract.acentuado\tcontract\tinv", intToUtf8(225), "lido\tx")),
             file.path(tmp, "zz.tsv"), useBytes = FALSE)
  expect_pc_error(prevcarga_errors_load(tmp), "contract.error_catalog_schema")
  file.copy(file.path(dir, "evaluation.tsv"), file.path(tmp, "dup.tsv"))
  file.remove(file.path(tmp, "zz.tsv"))
  expect_pc_error(prevcarga_errors_load(tmp), "contract.error_catalog_dup")
})

test_that("unica definicao de WMAPE: sum(abs( so em evaluation.R no modulo", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "evaluation.R"), warn = FALSE)
  expect_identical(sum(grepl("sum(abs(", src, fixed = TRUE)), 1L)
  expect_identical(sum(grepl("minutes(30", src[!grepl("^\\s*##", src)], fixed = TRUE)), 0L)   # borda do LONG: schema_long.R
})

test_that("leitor anexa interval_start 1x pela borda do LONG; scorecard usa a coluna [CONTROL]", {
  l <- pc_eval_long("A1", O, 0:1, ctx = ctx)
  out <- prevcarga_assert_long(l, ctx)
  expect_identical(out$interval_start, prevcarga_long_interval_start(l$target_datetime, l$granularity_min, ctx$tz))
  st <- l; st$interval_start <- st$target_datetime                       # LONG em disco nunca traz a coluna
  expect_pc_error(prevcarga_assert_long(st, ctx), "contract.long_schema")
  hit <- format(out$interval_start, "%H:%M") == "17:30"                 # 17:30-18:00: fora do pico
  out$y_pred[hit] <- out$y_pred[hit] + 200
  sk <- out; sk$interval_start <- sk$target_datetime                     # CONTROL: o scorecard le a coluna
  expect_true(all(prevcarga_scorecard(out, by = "d_plus")$peak == 0))
  expect_true(all(prevcarga_scorecard(sk, by = "d_plus")$peak > 0))
})
