## Comparador puro do spike R 4.5.3 (M1.7): identico => delta 0; perturbado => CONTROL delta != 0;
## chave faltando => erro; --out sob o oraculo recusado. Nao executa o oraculo nem dois R.
skip_if_not_installed("digest")

spk <- local({
  e <- new.env(parent = globalenv())
  sys.source(file.path(pc_root(), "scripts", "tools", "spike_r453.R"), envir = e)
  e
})

mk_long <- function(n = 6L) {
  data.frame(area = "SECO", forecast_origin = "2024-06-01",
             target_datetime = format(as.POSIXct("2024-06-02 00:00:00", tz = "UTC") + 1800 * (seq_len(n) - 1L),
                                      "%Y-%m-%d %H:%M:%S", tz = "UTC"),
             y_pred = 40000 + 100 * seq_len(n), stringsAsFactors = FALSE)
}

test_that("identicos => max_abs 0, max_rel 0, sha_equal TRUE", {
  a <- mk_long(); r <- spk$spike_compare(a, a[rev(seq_len(nrow(a))), ])
  expect_equal(r$n, 6L); expect_identical(r$max_abs, 0); expect_identical(r$max_rel, 0); expect_true(r$sha_equal)
})

test_that("CONTROL: perturbacao de 1e-9 e acusada", {
  a <- mk_long(); b <- a; b$y_pred[3] <- b$y_pred[3] + 1e-9
  r <- spk$spike_compare(a, b)
  expect_gt(r$max_abs, 0); expect_false(r$sha_equal)
})

test_that("chave faltando => erro 'spike: chaves divergentes'", {
  a <- mk_long(); expect_error(spk$spike_compare(a, a[-2, ]), "spike: chaves divergentes", fixed = TRUE)
})

test_that("spike_table e spike_verdict: confirmada, e rever nomeando o engine", {
  ok <- list(model = "lgbm", r_a = "4.2.3", r_b = "4.5.3", status = "ok", detail = "", n = 6L, max_abs = 0, max_rel = 0, sha_equal = TRUE)
  bad <- list(model = "dessem-linear", r_a = "4.2.3", r_b = "4.5.3", status = "compile_failed", detail = "WeightSVM")
  t1 <- spk$spike_table(list(ok)); expect_match(spk$spike_verdict(t1), "ADR-0036 confirmada", fixed = TRUE)
  t2 <- spk$spike_table(list(ok, bad))
  expect_equal(names(t2), c("model", "r_a", "r_b", "n", "max_abs", "max_rel", "sha_equal", "status", "detail"))
  v <- spk$spike_verdict(t2)
  expect_match(v, "rever ADR-0036", fixed = TRUE); expect_match(v, "WeightSVM", fixed = TRUE)
})

test_that("spike_guard_out recusa caminho sob o oraculo (CONTROL) e aceita fora dele", {
  o <- spk$spike_oracle_default
  skip_if_not(dir.exists(o))
  expect_error(spk$spike_guard_out(file.path(o, "x")), "spike: --out dentro do oraculo e proibido", fixed = TRUE)
  expect_error(spk$spike_guard_out(o), "dentro do oraculo", fixed = TRUE)
  ok <- spk$spike_guard_out(file.path(tempdir(), "spike-out")); expect_true(dir.exists(ok))
})
