## test-training.R -- gates [PLUGINS] train_* do treino do Holt-Winters (HW.3; AC 1-4). Cadeia ingest -> FE -> train sobre o
## store sintetico (P.0e). Espioes: o plugin e clonado num env filho e .hw_hw (unico ponto que chama forecast::hw) e
## trocado por um envoltorio que registra a chamada; o plugin real nunca e alterado.

## spec do run.yaml com a janela de treino do fixture (60 dias => 59 linhas na CargaTempVer)
tr_spec <- function() { s <- hw_spec(); s$history_days$train <- 59L; s }

tr_chain <- function(store, spec = tr_spec(), env = hw_env()) {
  cuts <- hw_fx_cuts(store)
  deck <- env$plugin_ingest(hw_fx_panel(store, cuts, history_days = 60L), cuts)
  list(feats = suppressMessages(env$plugin_build_features(deck, spec)), cuts = cuts)
}

tr_base <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      env <- hw_env(); st <- hw_fx_store(); ch <- tr_chain(st)
      cache <<- c(ch, list(store = st, model = env$plugin_train(ch$feats, tr_spec())))
    }
    cache
  }
})

## provenance.R (prevcarga_freeze/thaw, M1.3) no env de teste da fundacao; o helper-hw.R nao o carrega
if (!exists("prevcarga_freeze", envir = hw_lib, inherits = FALSE))
  sys.source(file.path(hw_root(), "scripts", "lib", "provenance.R"), envir = hw_lib)

tr_spy_env <- function(env, log) {
  e2 <- new.env(parent = parent.env(env))
  for (n in ls(env, all.names = TRUE)) assign(n, get(n, envir = env), envir = e2)
  for (n in ls(e2, all.names = TRUE)) {
    f <- get(n, envir = e2)
    if (is.function(f)) environment(f) <- e2
    assign(n, f, envir = e2)
  }
  orig <- get(".hw_hw", envir = e2)
  assign(".hw_hw", function(y, h) { log$n <- log$n + 1L; orig(y, h) }, envir = e2)
  e2
}

test_that("[PLUGINS] train_shape: 9x48 e 9, h_fit 9, align 2, datafimver O-1; hw(h=9)$mean[1:8] == hw(h=8)$mean", {
  b <- tr_base(); m <- b$model
  expect_identical(dim(m$mean_interval), c(9L, 48L))
  expect_true(is.double(m$mean_interval) && all(is.finite(m$mean_interval)))
  expect_true(is.double(m$mean_daily) && length(m$mean_daily) == 9L && all(is.finite(m$mean_daily)))
  expect_identical(m$h_fit, 9L); expect_identical(m$n_steps_fit, 8L); expect_identical(m$align_first_idx, 2L)
  expect_identical(m$datainiver, min(b$feats$CargaTempVer$Data))
  expect_identical(m$datafimver, b$cuts$last_data_day)
  expect_identical(m$datafimver, b$cuts$origin_day - 1L)
  expect_identical(m$origin_day, b$cuts$origin_day); expect_identical(m$train_origin, b$cuts$origin_day)
  expect_identical(colnames(m$mean_interval), hw_env()$hw_slot_cols())
  expect_false(any(c("trained_at", "config", "call", "available_until") %in% names(m)))
  ## hipotese do oraculo (hw_training.R:369-389): o ajuste nao depende de h
  env <- hw_env(); ctv <- b$feats$CargaTempVer
  for (z in c(4L, 30L, 51L)) {
    x <- env$.hw_ts(ctv[, z], m$datainiver)
    expect_identical(as.numeric(forecast::hw(x, h = 9L, seasonal = "multiplicative")$mean)[1:8],
                     as.numeric(forecast::hw(x, h = 8L, seasonal = "multiplicative")$mean))
  }
  ## a linha k e o passo k de forecast::hw(h = 9) da coluna j
  x1 <- env$.hw_ts(ctv[, 4L], m$datainiver)
  expect_identical(unname(m$mean_interval[, 1L]), as.numeric(forecast::hw(x1, h = 9L, seasonal = "multiplicative")$mean))
})

test_that("[PLUGINS] ts_start: identical a expressao do oraculo em 2019..2024 (29/02 e 31/12 inclusos) e mesmo tsp", {
  env <- hw_env()
  ds <- seq(lubridate::ymd("2019-01-01"), lubridate::ymd("2024-12-31"), by = "day")
  ora <- function(d) c(as.integer(format(d, "%Y")), as.integer(format(d, "%j")))   # so no teste
  expect_true(all(vapply(ds, function(d) identical(env$.hw_ts_start(d), ora(d)), NA)))
  expect_true(lubridate::ymd("2024-02-29") %in% ds && lubridate::ymd("2024-12-31") %in% ds)
  for (d in as.character(ds[c(1, 60, 366 * 2, 1500, length(ds))])) {
    d <- lubridate::ymd(d); x <- seq_len(30)
    expect_identical(stats::tsp(env$.hw_ts(x, d)), stats::tsp(stats::ts(x, start = ora(d), frequency = 7L)))
  }
  expect_length(hw_grep_code(file.path(hw_root(), "models", "holt-winters", "R"), "format\\(\\w+, *[\"']%[Yj]"), 0L)
})

test_that("[PLUGINS] train_contract: cortes como Date de typeof integer (seq(by = 'day') do backtest) passam; comparacao por valor", {
  b <- tr_base(); env <- hw_env(); spec <- tr_spec()
  as_int_date <- function(d) structure(as.integer(unclass(d)), class = "Date")
  f <- b$feats
  for (k in c("origin_day", "train_origin", "last_data_day")) f$meta[[k]] <- as_int_date(f$meta[[k]])
  expect_identical(typeof(f$meta$last_data_day), "integer")
  expect_false(identical(f$meta$last_data_day, b$feats$meta$last_data_day))   # CONTROL: identical() reprovaria
  m <- env$.hw_check_series(f, spec)
  expect_identical(as.double(m$datafimver), as.double(b$feats$meta$last_data_day))
})

test_that("[PLUGINS] train_contract: O-1 ausente => completude, dia O => contract, train_origin != O => contract, historico curto => window; sem hw()", {
  b <- tr_base(); env <- hw_env(); spec <- tr_spec()
  run <- function(feats, sp = spec) {
    log <- new.env(); log$n <- 0L
    cnd <- hw_catch(tr_spy_env(env, log)$plugin_train(feats, sp))
    list(cnd = cnd, n = log$n)
  }
  ## (1) O-1 ausente: serie termina em O-2
  f1 <- b$feats; keep <- f1$CargaTempVer$Data < b$cuts$last_data_day
  f1$CargaTempVer <- f1$CargaTempVer[keep, ]; f1$CargaTempMed <- f1$CargaTempMed[keep, ]
  r <- run(f1)
  expect_identical(r$cnd$id, "hw.last_day_incomplete")
  expect_identical(r$cnd$class_name, "completude"); expect_identical(r$cnd$exit_code, 3L)
  expect_match(conditionMessage(r$cnd), "datafimver"); expect_identical(r$n, 0L)
  ## (2) 1 intervalo do dia O
  f2 <- b$feats
  f2$load_day_o <- data.table::data.table(interval_start = lubridate::ymd_hm("2024-05-15 00:00", tz = HW_TZ),
                                          interval_end = lubridate::ymd_hm("2024-05-15 00:30", tz = HW_TZ), load_mwh = 1)
  r <- run(f2); expect_identical(r$cnd$id, "hw.train_day_o"); expect_identical(r$cnd$class_name, "contract"); expect_identical(r$n, 0L)
  ## (3) train_origin != O
  f3 <- b$feats; f3$meta$train_origin <- f3$meta$origin_day - 9L
  r <- run(f3); expect_identical(r$cnd$id, "hw.bad_cut"); expect_match(conditionMessage(r$cnd), "train_origin"); expect_identical(r$n, 0L)
  ## (4) 700 dias com history_days.train = 730 (aqui: 58 < 59), sem completar a janela
  sp4 <- spec; sp4$history_days$train <- 730L
  r <- run(b$feats, sp4); expect_identical(r$cnd$id, "hw.history_short")
  expect_match(conditionMessage(r$cnd), "59 dias de historico < history_days.train 730"); expect_identical(r$n, 0L)
  expect_identical(r$cnd$class_name, "window"); expect_identical(r$cnd$exit_code, 5L)
})

test_that("[PLUGINS] train_cut: FORBIDDEN dia O perturbado => sha256 igual; CONTROL O-1 perturbado => muda e max|d| > 1e-5", {
  b <- tr_base(); env <- hw_env(); O <- b$cuts$origin_day
  start <- function(d) lubridate::as_datetime(d, tz = HW_TZ)
  sha_of <- function(model) {
    f <- tempfile("hw-sha-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))))
    hw_lib$prevcarga_freeze(env$plugin_freeze(model), f)
  }
  base_sha <- sha_of(b$model)
  run_pert <- function(after, at_or_before = NULL) {
    d <- hw_lib$prevcarga_fixture_perturb(b$store$dir, "load", after = after, at_or_before = at_or_before,
                                          perturb = function(v) v * 1.2)
    on.exit(unlink(d, recursive = TRUE))
    st <- b$store; st$dir <- d
    file.copy(file.path(b$store$dir, "topology.yaml"), file.path(d, "topology.yaml"), overwrite = TRUE)
    ch <- tr_chain(st); env$plugin_train(ch$feats, tr_spec())
  }
  mo <- run_pert(after = start(O) - 1)                                       # dia O inteiro +20%
  expect_identical(sha_of(mo), base_sha)
  expect_identical(mo$mean_interval, b$model$mean_interval)
  mc <- run_pert(after = start(O - 1L) - 1, at_or_before = start(O) - 1)     # O-1 inteiro +20%
  expect_false(identical(sha_of(mc), base_sha))
  expect_gt(max(abs(mc$mean_interval - b$model$mean_interval)), 1e-5)
})
