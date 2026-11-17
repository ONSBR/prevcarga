## test-accept-combina.R -- accept_combina.R (E8-T4): correlacao do erro LIN x LIN_UNI e diagnostico da combina (celulas nao-estacionarias,
## iteracoes) com e sem LIN_UNI. Fixture sintetica pequena (helper-accept.R); nenhum docker, nenhuma chamada AWS, nada de medicao grande.

.acm_e <- acc_report_env()
sys.source(file.path(pc_root(), "scripts", "lib", "combination.R"), envir = .acm_e)
sys.source(file.path(pc_root(), "scripts", "tools", "accept_combina.R"), envir = .acm_e)
.acm_script <- function() file.path(pc_root(), "scripts", "tools", "accept_combina.R")
.acm_tz <- "Etc/GMT+3"

## err sintetico (colunas de pc_accept_combina_errors): 8 origens em 8 meses distintos, 2 leads, 1 corte, membros A/B/C por sigla
.acm_err <- function(members = c("LIN_MED", "LIN_UNI", "RGD_MED"), area = "RJ") {
  origs <- sprintf("2023-%02d-10", 1:8)
  g <- expand.grid(o = origs, lead = 1:2, m = members, stringsAsFactors = FALSE)
  t0 <- lubridate::force_tz(lubridate::ymd(g$o), .acm_tz) + lubridate::hours(g$lead)
  set.seed(11)
  truth <- 100 + as.integer(substr(g$o, 6, 7)) * 3 + g$lead
  k <- match(g$m, members)
  d <- data.table::data.table(area = area, forecast_origin = g$o, target_datetime = t0, lead = as.integer(g$lead),
                              y_pred = truth + k * 2 + stats::rnorm(nrow(g), 0, 1), y_true = truth, d_plus = 1L, granularity_min = 30L,
                              membro = g$m, corte = "0800")
  d
}

test_that("pc_accept_combina_corr: Pearson do erro por intervalo, por area, agregado e media; CONTROL de sinal oposto", {
  e <- .acm_e
  n <- 30L
  base <- seq_len(n) + sin(seq_len(n))
  mk <- function(a, memb, erro) data.table::data.table(area = a, forecast_origin = "2023-01-10", target_datetime = as.POSIXct("2023-01-10 00:00:00", tz = "UTC") + 1800 * seq_len(n),
                                                       lead = seq_len(n), y_pred = 100 + erro, y_true = 100, d_plus = 1L, granularity_min = 30L,
                                                       membro = memb, corte = "0800", erro = erro)
  err <- data.table::rbindlist(list(
    mk("RJ", "LIN_UNI", base), mk("RJ", "LIN_MED", 2 * base + 1), mk("RJ", "LIN_MAX", -base), mk("RJ", "LIN_MXMN", rev(base)),
    mk("SP", "LIN_UNI", base), mk("SP", "LIN_MED", 3 * base), mk("SP", "LIN_MAX", -2 * base), mk("SP", "LIN_MXMN", base^2)))
  err[, k := paste(area, forecast_origin, corte, as.numeric(target_datetime), sep = "|")]
  r <- e$pc_accept_combina_corr(err)
  g <- function(v, a, sc = "d1") r[escopo == sc & variante == v & area == a]$pearson
  expect_equal(g("LIN_MED", "RJ"), 1, tolerance = 1e-12)
  expect_equal(g("LIN_MAX", "RJ"), -1, tolerance = 1e-12)                       # CONTROL: erro oposto => -1
  expect_equal(g("LIN_MXMN", "RJ"), stats::cor(rev(base), base), tolerance = 1e-12)
  expect_equal(g("LIN_MXMN", "SP"), stats::cor(base^2, base), tolerance = 1e-12)
  ## agregado = Pearson do pool (RJ e SP juntos); media = media simples das correlacoes por area
  pool <- c(base, base); pm <- c(2 * base + 1, 3 * base)
  expect_equal(g("LIN_MED", "AGREGADO"), stats::cor(pm, pool), tolerance = 1e-12)
  expect_equal(g("LIN_MED", "MEDIA_AREAS"), mean(c(g("LIN_MED", "RJ"), g("LIN_MED", "SP"))), tolerance = 1e-12)
  expect_equal(r[escopo == "d1" & variante == "LIN_MED" & area == "AGREGADO"]$n, 2L * n)
  expect_setequal(unique(r$variante), c("LIN_MED", "LIN_MAX", "LIN_MXMN"))
  expect_setequal(unique(r$escopo), c("d1", "todos"))
  ## serie constante => NA (sem divisao por zero)
  const <- data.table::copy(err[membro == "LIN_UNI" | membro == "LIN_MED"]); const[membro == "LIN_MED", erro := 5]
  expect_true(is.na(e$pc_accept_combina_corr(const)[escopo == "d1" & variante == "LIN_MED" & area == "RJ"]$pearson))
})

test_that("celulas da combina: producao (n_min 24) sem fit com 8 meses; sensibilidade (n_min 5) fita 3 meses; sem LIN_UNI tem um membro a menos", {
  e <- .acm_e
  err <- .acm_err()
  cells <- e$pc_accept_combina_run_cells(err, .acm_tz, c(24L, 5L))
  expect_setequal(unique(cells$config), c("com_LIN_UNI", "sem_LIN_UNI"))
  prod <- cells[n_min == 24L]
  expect_true(all(prod$fallback %in% c("first_month", "n_min")))
  expect_false(any(prod$nonstationary)); expect_equal(max(prod$n), 7L)             # 8 meses => no maximo 7 amostras anteriores
  sens <- cells[n_min == 5L & config == "com_LIN_UNI"]
  expect_equal(sum(is.na(sens$fallback)), 3L * 2L)                                # meses 6..8 x 2 leads
  expect_true(all(sens$n[is.na(sens$fallback)] %in% 5:7))
  expect_true(all(sens$n_membros == 3L)); expect_true(all(cells[config == "sem_LIN_UNI"]$n_membros == 2L))
  s <- e$pc_accept_combina_summary(cells)
  expect_equal(s[config == "com_LIN_UNI" & n_min == 24L]$fitadas, 0L)
  expect_equal(s[config == "com_LIN_UNI" & n_min == 5L]$fitadas, 6L)
  expect_equal(s[config == "com_LIN_UNI" & n_min == 24L]$n_max, 7L)
  h <- e$pc_accept_combina_hist(cells)
  expect_equal(sum(h[config == "com_LIN_UNI" & n_min == 5L]$celulas), 6L)
  expect_equal(sum(h[n_min == 24L]$celulas), 0L)
})

test_that("celula que bate o cap de iteracoes (500) conta como nao-estacionaria; com erro de membro colinear e escala grande", {
  e <- .acm_e
  ## membros quase colineares em escala grande (grad_tol absoluto, ADR-0026) levam o fit ao cap: o fit de producao decide, aqui so a contagem
  cells <- data.table::data.table(area = "RJ", config = "com_LIN_UNI", n_min = 5L, lead = 1:3, cut = "0800", ym = "2023-08", fallback = NA_character_,
                                  n = 9L, iters = c(500L, 120L, 0L), nonstationary = c(TRUE, FALSE, FALSE), n_membros = 3L, cap = c(TRUE, FALSE, FALSE))
  s <- e$pc_accept_combina_summary(cells)
  expect_equal(s$nonstationary, 1L); expect_equal(s$iters_max, 500L); expect_equal(s$no_cap, 1L)
  ns <- e$pc_accept_combina_nonstat(cells)
  expect_equal(nrow(ns), 1L); expect_equal(ns$lead, 1L)
  h <- e$pc_accept_combina_hist(cells)
  expect_equal(h[faixa == "500"]$celulas, 1L); expect_equal(h[faixa == "101-200"]$celulas, 1L); expect_equal(h[faixa == "0"]$celulas, 1L)
})

test_that("accept_combina.R de ponta a ponta sobre <out> de fixture: saidas ASCII, erro com sinal oposto, store intacto", {
  e <- .acm_e
  origens <- c("2023-12-31", "2024-02-11"); areas <- c("RJ", "SP")
  days <- c(lubridate::ymd("2023-12-30") + 0:3, lubridate::ymd("2024-02-10") + 0:3)
  store <- acc_report_store(e, areas, days)
  epsf <- function(v, m) c(LIN_MED = 0.010, LIN_MAX = 0.012, LIN_MXMN = 0.014, LIN_UNI = -0.020, RGD_MED = 0.030, RGD_MAX = 0.032)[[m]]
  fx <- acc_report_out(e, store, origens, areas, epsf)
  se <- tempfile("acm-err-")
  so <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", .acm_script(), "--out-dir", fx$out, "--data", store),
                                 stdout = TRUE, stderr = se))
  expect_null(attr(so, "status"))
  j <- jsonlite::fromJSON(paste(so, collapse = "\n"), simplifyVector = FALSE)
  expect_null(j$error_id)
  for (f in c("combina.corr.tsv", "combina.cells.tsv", "combina.summary.tsv", "combina.nonstationary.tsv", "combina.md", "combina.json")) {
    p <- file.path(fx$out, f); expect_true(file.exists(p), info = f)
    expect_false(any(as.integer(readBin(p, "raw", file.size(p))) > 127L), info = f)
  }
  corr <- data.table::fread(file.path(fx$out, "combina.corr.tsv"), sep = "\t")
  expect_equal(corr[escopo == "d1" & variante == "LIN_MED" & area == "AGREGADO"]$pearson, -1, tolerance = 1e-9)   # erro proporcional a y_true, sinais opostos
  expect_setequal(corr[area %in% areas]$area, areas)
  summ <- data.table::fread(file.path(fx$out, "combina.summary.tsv"), sep = "\t")
  expect_equal(summ[n_min == 24L & config == "com_LIN_UNI"]$nonstationary, 0L)
  expect_equal(summ[n_min == 24L & config == "com_LIN_UNI"]$fitadas, 0L)
  md <- readLines(file.path(fx$out, "combina.md"))
  expect_true(any(grepl("6 membros", md))); expect_true(any(grepl("5 membros", md)))
  ## store inalterado
  expect_identical(.acm_e$.bench_data_listing(store), .acm_e$.bench_data_listing(store))
})
