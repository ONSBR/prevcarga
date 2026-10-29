## test-honesty-gate.R -- honesty_gate.R (M3.2). Assercoes por classe + error.id, nunca por mensagem livre.
## Cada braco que afirma delta <= 1e-5 vem pareado com um que exige > 1e-5.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "honesty_gate"))
for (n in ls(env, all.names = TRUE)) assign(n, env[[n]])
prevcarga_runtime_enforce()

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
expect_args_error <- function(expr, where) {
  e <- fail(expr)
  expect_false(is.null(e))
  expect_identical(e$id, "contract.honesty_gate_args")
  expect_match(conditionMessage(e), "^\\[prevcarga/contract\\]")
  expect_match(paste(e$where, collapse = " "), where, fixed = TRUE)
}

TZ <- "Etc/GMT+3"
tt <- function(s) lubridate::force_tz(lubridate::ymd_hms(s), TZ)
O <- "2026-10-01"
panel <- pc_hg_panel(O, back = 8L, horizon = 9L)
nm <- c("verdict", "clean2_delta", "forbidden_delta", "control_delta", "reach_ok", "arms")

test_that("AC1 inferencia: available_until 09:00 honesto vs leitor do intervalo 09:00-09:30", {
  cut <- tt("2026-10-01 09:00:00")
  h <- prevcarga_honesty_gate(pc_rf_infer_honest(cut), panel, cut, horizon = 9)
  expect_named(h, nm)
  expect_identical(h$verdict, "HONESTO")
  expect_lte(h$clean2_delta, 1e-5); expect_lte(h$forbidden_delta, 1e-5)
  expect_true(h$reach_ok); expect_gt(h$control_delta, 1e-5)
  v <- prevcarga_honesty_gate(pc_rf_infer_peek(cut), panel, cut, horizon = 9)
  expect_identical(v$verdict, "VAZA")
  expect_gt(v$forbidden_delta, 1e-5)
  expect_identical(h$arms$channel, "y_true"); expect_identical(h$arms$horizon, 9L)
  expect_identical(h$arms$tol, 1e-5)
})

test_that("AC2 regioes exatas pelo espiao; membros e relogio intocados; 4 chamadas", {
  cut <- tt("2026-10-01 09:00:00")
  e <- new.env()
  g <- prevcarga_honesty_gate(pc_rf_spy(cut, e), panel, cut, horizon = 9)
  expect_length(e$calls, 4L)
  clean <- data.table::as.data.table(panel)
  ie <- panel$interval_end
  fz <- ie > cut & ie <= cut + lubridate::days(9)
  ct <- ie > cut - lubridate::days(1) & ie <= cut
  expect_identical(e$calls[[1]], clean); expect_identical(e$calls[[2]], clean)
  expect_identical(e$calls[[3]]$y_true != clean$y_true, fz)
  expect_identical(e$calls[[4]]$y_true != clean$y_true, ct)
  for (i in 1:4) for (col in c("m1", "m2", "interval_start", "interval_end", "area"))
    expect_identical(e$calls[[i]][[col]], clean[[col]])
  expect_identical(g$arms$forbidden$n_perturbed, sum(fz))
  expect_identical(g$arms$control$n_perturbed, sum(ct))
  expect_false(any(fz & ct))   # CONTROL: regioes disjuntas
})

test_that("AC3 copia profunda: run_fn que muta por referencia nao toca o painel do chamador", {
  cut <- tt("2026-10-01 09:00:00")
  dt <- data.table::as.data.table(panel)
  snap <- data.table::copy(dt)
  g <- prevcarga_honesty_gate(pc_rf_mutating(cut), dt, cut, horizon = 9)
  expect_identical(dt, snap)
  expect_identical(g$verdict, "HONESTO")
  expect_identical(g$verdict,
    prevcarga_honesty_gate(pc_rf_infer_honest(cut), dt, cut, horizon = 9)$verdict)
  # copia de data.frame nao compartilha referencia
  cp <- .prevcarga_hg_copy(panel); cp$y_true[1] <- -1
  expect_false(identical(cp$y_true[1], panel$y_true[1]))
})

test_that("AC4 delta estrutural e Inf; chaves presentes; sem y_pred e contract", {
  cut <- tt("2026-10-01 09:00:00")
  honest <- pc_rf_infer_honest(cut)
  fut <- function(p) any(p$y_true[p$interval_end > cut & p$interval_end <= cut + lubridate::days(9)] !=
                         panel$y_true[panel$interval_end > cut & panel$interval_end <= cut + lubridate::days(9)])
  fewer <- function(p) { r <- honest(p); if (fut(p)) r[-1, ] else r }
  nas   <- function(p) { r <- honest(p); if (fut(p)) r$y_pred[3] <- NA_real_; r }
  slim  <- function(p) { r <- honest(p); r[, c("target_datetime", "y_pred")] }
  for (f in list(fewer, nas)) {
    g <- prevcarga_honesty_gate(f, panel, cut, horizon = 9)
    expect_identical(g$forbidden_delta, Inf); expect_identical(g$verdict, "VAZA")
  }
  g <- prevcarga_honesty_gate(slim, panel, cut, horizon = 9)
  expect_identical(g$verdict, "HONESTO")
  expect_args_error(prevcarga_honesty_gate(function(p) data.frame(x = 1), panel, cut, 9), "run_fn output")
  # chave diferente -> Inf (CONTROL do controle estrutural)
  a <- honest(panel); b <- a; b$lead <- b$lead + 1L
  expect_identical(.prevcarga_hg_delta(a, b), Inf)
  expect_identical(.prevcarga_hg_delta(a, a), 0)
})

test_that("AC5 alcance vazio ou perturbacao identidade e INDETERMINADO, nunca HONESTO", {
  cut <- tt("2026-10-01 09:00:00")
  short <- panel[panel$interval_end <= cut, ]
  g <- prevcarga_honesty_gate(pc_rf_infer_honest(cut), short, cut, horizon = 9)
  expect_false(g$reach_ok); expect_identical(g$verdict, "INDETERMINADO")
  expect_identical(g$arms$forbidden$n_perturbed, 0L)
  g2 <- prevcarga_honesty_gate(pc_rf_infer_honest(cut), panel, cut, horizon = 9, perturb = function(y) y)
  expect_false(g2$reach_ok); expect_identical(g2$verdict, "INDETERMINADO")
  g3 <- prevcarga_honesty_gate(pc_rf_infer_honest(cut), panel, cut, horizon = 9)  # pareado
  expect_true(g3$reach_ok); expect_identical(g3$verdict, "HONESTO")
  # nao deterministico -> INDETERMINADO; cego -> HARNESS QUEBRADO
  expect_identical(prevcarga_honesty_gate(pc_rf_nondet(cut), panel, cut, 9)$verdict, "INDETERMINADO")
  expect_identical(prevcarga_honesty_gate(pc_rf_blind(), panel, cut, 9)$verdict, "HARNESS QUEBRADO")
})

test_that("AC6 limiar 1e-5 e tol efetivo", {
  cut <- tt("2026-10-01 09:00:00")
  honest <- pc_rf_infer_honest(cut)
  mk <- function(k) function(p) {
    r <- honest(p)
    r$y_pred <- r$y_pred + k * mean(p$y_true[p$interval_end > cut]); r
  }
  g1 <- prevcarga_honesty_gate(mk(1e-10), panel, cut, 9)
  expect_gt(g1$forbidden_delta, 0); expect_lte(g1$forbidden_delta, 1e-5)
  expect_identical(g1$verdict, "HONESTO")
  g2 <- prevcarga_honesty_gate(mk(1e-5), panel, cut, 9)
  expect_gt(g2$forbidden_delta, 1e-5); expect_identical(g2$verdict, "VAZA")
  g3 <- prevcarga_honesty_gate(mk(1e-10), panel, cut, 9, tol = 1e-9)
  expect_identical(g3$verdict, "VAZA"); expect_identical(g3$arms$tol, 1e-9)
})

test_that("AC7 contrato de entrada antes de chamar run_fn; erro de run_fn propaga", {
  cut <- tt("2026-10-01 09:00:00"); honest <- pc_rf_infer_honest(cut)
  called <- FALSE
  probe <- function(p) { called <<- TRUE; honest(p) }
  expect_args_error(prevcarga_honesty_gate(probe, panel, "2026-10-01", 9), "cut")
  expect_args_error(prevcarga_honesty_gate(probe, panel, lubridate::with_tz(cut, "UTC"), 9), "cut (tzone)")
  expect_args_error(prevcarga_honesty_gate(probe, panel, cut, 0), "horizon")
  expect_args_error(prevcarga_honesty_gate(probe, panel, cut, 2.5), "horizon")
  expect_args_error(prevcarga_honesty_gate(probe, panel[, setdiff(names(panel), "y_true")], cut, 9), "panel")
  expect_args_error(prevcarga_honesty_gate(probe, panel[, setdiff(names(panel), "interval_end")], cut, 9), "panel")
  expect_args_error(prevcarga_honesty_gate("nao", panel, cut, 9), "run_fn")
  expect_args_error(prevcarga_honesty_gate(probe, panel, cut, 9, tol = 0), "tol")
  expect_false(called)
  e <- tryCatch(prevcarga_honesty_gate(function(p) stop("boom"), panel, cut, 9), error = identity)
  expect_match(conditionMessage(e), "boom")
  expect_false(inherits(e, "prevcarga_error"))
})

test_that("AC8 treino por classe: cut = fim de last_data_day (O-1); train_origin faz VAZA", {
  cut <- tt("2026-10-01 00:00:00")
  for (rf in list(pc_rf_train_agnostic(O, 9L), pc_rf_train_scoped(O))) {
    g <- prevcarga_honesty_gate(rf, panel, cut, horizon = 9)
    expect_identical(g$verdict, "HONESTO")
    expect_lte(g$forbidden_delta, 1e-5); expect_gt(g$control_delta, 1e-5)
  }
  v <- prevcarga_honesty_gate(pc_rf_train_leaky(O), panel, cut, horizon = 9)
  expect_identical(v$verdict, "VAZA"); expect_gt(v$forbidden_delta, 1e-5)
  g <- prevcarga_honesty_gate(pc_rf_train_agnostic(O, 9L), panel, tt("2026-09-22 00:00:00"), horizon = 9)
  expect_identical(g$verdict, "VAZA")
})

test_that("AC9 treino com atraso: cut = fim de O-1-lag; cut = O-1 da HARNESS QUEBRADO", {
  rf <- pc_rf_train_lagged(O, 2L)
  g <- prevcarga_honesty_gate(rf, panel, tt("2026-09-29 00:00:00"), horizon = 9)
  expect_identical(g$verdict, "HONESTO")
  expect_lte(g$forbidden_delta, 1e-5); expect_gt(g$control_delta, 1e-5)
  v <- prevcarga_honesty_gate(pc_rf_train_lagged(O, 1L), panel, tt("2026-09-29 00:00:00"), horizon = 9)
  expect_identical(v$verdict, "VAZA")
  w <- prevcarga_honesty_gate(rf, panel, tt("2026-10-01 00:00:00"), horizon = 9)
  expect_identical(w$verdict, "HARNESS QUEBRADO")
  expect_lte(w$control_delta, 1e-5)
})

test_that("veredito em ordem fixa e regioes via lubridate", {
  vd <- .prevcarga_hg_verdict
  expect_identical(vd(1, 1, 1, TRUE, 1e-5), "INDETERMINADO")
  expect_identical(vd(0, 1, 1, FALSE, 1e-5), "VAZA")
  expect_identical(vd(0, 0, 1, FALSE, 1e-5), "INDETERMINADO")
  expect_identical(vd(0, 0, 0, TRUE, 1e-5), "HARNESS QUEBRADO")
  expect_identical(vd(0, 0, 1, TRUE, 1e-5), "HONESTO")
  src <- readLines(file.path(pc_root(), "scripts", "lib", "honesty_gate.R"))
  code <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("as\\.POSIXct|Sys\\.|system2|Sys.getenv", code)))
})
