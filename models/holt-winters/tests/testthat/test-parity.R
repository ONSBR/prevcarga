## test-parity.R -- gates [PLUGINS] paridade / paridade_controle / sem_crash_1o_mes do Holt-Winters (HW.5; AC 1-6). Tiers parity
## (grupo smoke: 4 unidades SECO) e parity-full (grupo full: SECO, S, NE, N x 2024-05). O clean-room roda cada unidade pelo
## despachante `prevcarga_plugin_forecast` (corte 00Z de O) e e comparado ao GOLDEN do oraculo (.state/golden, so lido; o
## golden.lock versionado confere sha256, R/pacotes/BLAS e particoes de dados). Golden ausente/divergente => FAIL 'regere o
## golden' (nunca skip). Limiar unico: 1e-5 MW absoluto.

test_that("[PLUGINS] paridade: values, day0_raw e delivery O+1..O+8 <= 1e-5 MW contra o oraculo; golden.lock confere", {
  pc_gate_tier("parity")
  units <- hp_units(hp_group())
  expect_gt(length(units), 0L)
  for (u in units) {
    msg <- hp_check_golden(u)
    if (!is.null(msg)) { testthat::fail(msg); next }          # nunca skip: regerar o golden
    r <- hp_compare(hp_run(u), hp_golden(u), u)
    hp_report_add("units", u$id, r)
    expect_identical(r$layout, "hw_matrix_nx48")
    expect_true(r$shape_ok, info = u$id); expect_true(r$na_ok, info = u$id)
    expect_true(r$delivery_ok, info = u$id); expect_true(r$day0_raw_ok, info = u$id)
    expect_identical(r$fora_da_faixa, 0L)
    expect_identical(r$day0_source, "oracle_discarded_step")
    expect_lte(r$max_abs_delta, HP_TOL)
    expect_true(r$ok, info = sprintf("%s max|d|=%.3e", u$id, r$max_abs_delta))
  }
  hp_report_write()
})

test_that("[PLUGINS] sem_crash_1o_mes: 1o do mes entrega 8 x 48 sem erro (gate03)", {
  pc_gate_tier("parity")
  u <- hp_unit(HP_CRASH)
  expect_identical(format(u$origin, "%d"), "01")
  env <- NULL
  expect_no_error(env <- hp_run(u))
  expect_identical(dim(env$values), c(8L, 48L))
  expect_true(all(is.finite(env$values)))
  expect_identical(nrow(env$delivery), 432L)
  expect_identical(length(env$meta$day0_raw), 48L)
})

## golden sintetico com SENTINELA de dia (valor = 1000 * k + slot, dia O+k) para o adaptador hp_compare: um deslocamento de
## 1 dia (valores ou juncao do long) tem de reprovar
hp_sentinel <- function(u) {
  tz <- lubridate::tz(u$available_until)
  t0 <- lubridate::force_tz(lubridate::as_datetime(u$origin), tz)
  vals <- outer(1:8, 1:48, function(k, j) 1000 * k + j)
  ie <- unlist(lapply(1:8, function(k) as.numeric(t0 + lubridate::days(k) + lubridate::minutes(30L * (1:48)))))
  ie <- lubridate::as_datetime(ie, tz = tz)
  d0 <- 100 + 1:48
  env <- list(layout = "hw_matrix_nx48", values = vals, meta = list(day0_raw = d0), available_until = u$available_until,
              delivery = data.table::data.table(interval_end = c(t0 + lubridate::minutes(30L * (1:48)), ie),
                                                y_pred = c(d0, as.vector(t(vals)))))
  gold <- list(native = list(layout = "hw_matrix_nx48", values = vals, meta = list(day0_raw = d0)),
               long = data.frame(timestamp = ie - lubridate::minutes(30L), y_pred = as.vector(t(vals))))
  list(env = env, gold = gold)
}

test_that("[PLUGINS] paridade_controle: regua com dente (janela, LOESS, sentinela de dia, golden, units, oraculo intocado)", {
  pc_gate_tier("parity")
  u <- hp_unit(HP_BASE)
  ## CONTROL com dente: history_days.train 730 -> 723 e params.suaviza true -> false mudam o resultado alem de 1e-5 MW
  if (!is.null(msg <- hp_check_golden(u))) { testthat::fail(msg) } else {
    gold <- hp_golden(u)
    base <- hp_compare(hp_run(u), gold, u)
    expect_true(base$ok, info = "base")
    c723 <- hp_compare(hp_run(u, history_days = 723L), gold, u)
    expect_gt(c723$max_abs_delta, HP_TOL); expect_false(c723$ok)
    csu <- hp_compare(hp_run(u, suaviza = FALSE), gold, u)
    expect_gt(csu$max_abs_delta, HP_TOL); expect_false(csu$ok)
    ## seed 1234 -> 1235: MEDIDO e registrado, sem asserir (o hw() e MLE; o kmeans re-semeia por kmeans_seed)
    cs <- hp_compare(hp_run(u, seed = 1235), gold, u)
    hp_report_add("controls", "history_723", list(max_abs_delta = c723$max_abs_delta, differs = c723$max_abs_delta > HP_TOL))
    hp_report_add("controls", "suaviza_false", list(max_abs_delta = csu$max_abs_delta, differs = csu$max_abs_delta > HP_TOL))
    hp_report_add("controls", "seed_1235", list(max_abs_delta = cs$max_abs_delta, measured_only = TRUE))
  }
  ## sentinela de dia no adaptador: certo passa; valores deslocados 1 dia, delivery deslocada 1 dia e juncao do long com
  ## +1 dia (join_shift_min) reprovam
  s <- hp_sentinel(u)
  ok <- hp_compare(s$env, s$gold, u)
  expect_true(ok$ok); expect_identical(ok$fora_da_faixa, 0L); expect_identical(ok$shape_ok, TRUE)
  e1 <- s$env; e1$values <- s$env$values[c(2:8, 8), ]
  r1 <- hp_compare(e1, s$gold, u); expect_false(r1$ok); expect_gt(r1$max_abs_delta, HP_TOL)
  e2 <- s$env; e2$delivery$y_pred <- c(s$env$meta$day0_raw, as.vector(t(s$env$values[c(2:8, 8), ])))
  r2 <- hp_compare(e2, s$gold, u); expect_false(r2$ok); expect_gt(r2$max_abs_delta, HP_TOL)
  r3 <- hp_compare(s$env, s$gold, u, join_shift_min = 30L + 1440L); expect_false(r3$ok)
  e4 <- s$env; e4$meta$day0_raw <- s$env$meta$day0_raw + 1e-3
  r4 <- hp_compare(e4, s$gold, u); expect_false(r4$ok); expect_false(r4$day0_raw_ok)
  ## golden ausente / versao do `forecast` divergente => 'regere o golden' nomeando unidade e pacote
  tmp <- tempfile("hp-lock-"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  m0 <- hp_check_golden(u, root = file.path(tmp, "vazio"))
  expect_match(m0, "regere o golden"); expect_match(m0, u$id, fixed = TRUE)
  if (file.exists(hp_lock_path())) {
    lk <- jsonlite::read_json(hp_lock_path(), simplifyVector = FALSE)
    if (!is.null(lk$units[[u$id]])) {
      lk$units[[u$id]]$fingerprint$packages$forecast <- "0.0.0"
      lock2 <- file.path(tmp, "golden.lock")
      writeLines(as.character(jsonlite::toJSON(lk, auto_unbox = TRUE, null = "null", digits = NA)), lock2)
      m1 <- hp_check_golden(u, lock = lock2)
      expect_match(m1, "regere o golden"); expect_match(m1, "forecast", fixed = TRUE)
    }
  }
  expect_match(hp_check_golden(u, lock = file.path(tmp, "nao-existe.lock")), "regere o golden")
  ## units.yaml: o real passa; fixture com oracle_cut e recusada [prevcarga/contract] tools.bad_units nomeando o campo
  expect_identical(length(hp_units("smoke")), 4L)
  expect_identical(length(hp_units("full")), 124L)
  expect_identical(length(hp_units("all")), 126L)
  fx <- file.path(tmp, "units.yaml")
  y <- yaml::read_yaml(hp_units_path()); y$units <- y$units[1:2]; y$units[[1L]]$oracle_cut <- "start_label"
  yaml::write_yaml(y, fx)
  e <- hw_catch(hp_validate_units(fx))
  expect_identical(e$id, "tools.bad_units"); expect_identical(e$class_name, "contract")
  expect_match(e$where, "oracle_cut", fixed = TRUE)
  ## --out dentro do oraculo => exit 2 antes de invocar o driver (oraculo-fixture git limpo em tempdir; o real nao e tocado)
  orc <- file.path(tmp, "oraculo"); dir.create(orc)
  writeLines("x", file.path(orc, "README"))
  git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(orc), "-c", "user.name=t", "-c", "user.email=t@t", ...),
                                                stdout = TRUE, stderr = TRUE))
  git("init", "-q"); git("add", "-A"); git("commit", "-q", "-m", "x")
  out <- suppressWarnings(system2(hp_rscript(), c("--vanilla", shQuote(file.path(hw_root(), "scripts", "tools", "plugin_golden.R")),
                                                  "--model", HP_MODEL, "--units", shQuote(hp_units_path()), "--oracle", shQuote(orc),
                                                  "--out", shQuote(file.path(orc, "saida"))), stdout = TRUE, stderr = TRUE))
  expect_identical(attr(out, "status"), 2L)
  expect_false(dir.exists(file.path(orc, "saida")))
  ## relatorio com sha256 (gerado, nao versionado)
  sha <- hp_report_write()
  expect_match(sha, "^[0-9a-f]{64}$")
})
