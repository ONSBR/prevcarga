## test-plugin-tools-parity.R -- scripts/tools/plugin_parity.R (P.0g): comparador por LAYOUT na sobreposicao da faixa.
## Goldens-brinquedo nos 4 layouts (pc_toy_golden); o CLI roda em processo real (pc_rscript) contra o toy-30 e um deck
## sintetico. Nenhum oraculo, nenhum modelo real. Limiar fixo 1e-5 MW absoluto.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                       "registry", "features"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
sys.source(file.path(pc_root(), "scripts", "tools", "plugin_golden.R"), envir = env)
sys.source(file.path(pc_root(), "scripts", "tools", "plugin_parity.R"), envir = env)
list2env(as.list(env, all.names = TRUE), envir = environment())   # .pc_find_lib acha as funcoes pela pilha
PARITY <- file.path(pc_root(), "scripts", "tools", "plugin_parity.R")
err_id <- function(expr) tryCatch({ force(expr); NA_character_ }, prevcarga_error = function(e) e$id)
cmp <- function(...) env$prevcarga_parity_compare(...)
G <- function(...) pc_toy_golden(...)
E <- function(g, ...) pc_toy_envelope_from(g, ...)

## 4 layouts: (layout, n_days do golden = delivery_days do membro)
CASES <- list(list("rf_wide_hourly_216", 9L), list("lgbm_day_48", 2L), list("lgbm_day_48", 1L),
              list("hw_matrix_nx48", 9L), list("dessem_semihourly_48", 9L))

test_that("[ac-0] os 4 layouts: igual e +9e-6 ok; +2e-5 nomeia a celula; forma, NA e x1.001 reprovam", {
  for (cs in CASES) {
    g <- G(cs[[1L]], cs[[2L]])
    lab <- paste(cs[[1L]], cs[[2L]])
    r0 <- cmp(E(g), g$golden, g$unit)
    expect_true(r0$ok, info = lab); expect_identical(r0$max_abs_delta, 0, info = lab)
    expect_identical(r0$layout, cs[[1L]]); expect_true(r0$shape_ok && r0$na_ok && r0$delivery_ok)
    r1 <- cmp(E(g, delta = 9e-6), g$golden, g$unit)
    expect_true(r1$ok, info = lab); expect_gt(r1$max_abs_delta, 8e-6); expect_lte(r1$max_abs_delta, 1e-5)
    r2 <- cmp(E(g, delta = 2e-5), g$golden, g$unit)
    expect_false(r2$ok, info = lab); expect_identical(r2$worst, c(1L, 3L), info = lab)
    r2d <- cmp(E(g, delta = 2e-5, where = "delivery"), g$golden, g$unit)
    expect_false(r2d$ok, info = lab); expect_false(r2d$delivery_ok)
    ev <- E(g); ev$values <- ev$values * 1.001
    expect_false(cmp(ev, g$golden, g$unit)$ok, info = lab)
    ev <- E(g); ev$values[nrow(ev$values), 2L] <- NA_real_
    rn <- cmp(ev, g$golden, g$unit)
    expect_false(rn$ok, info = lab); expect_false(rn$na_ok)
    if (nrow(g$truth) > 1L) {
      ev <- E(g); ev$values <- ev$values[-nrow(ev$values), , drop = FALSE]
      rs <- cmp(ev, g$golden, g$unit)
      expect_false(rs$ok, info = lab); expect_false(rs$shape_ok)
    }
  }
  g <- G("lgbm_day_48", 2L)
  ev <- E(g); ev$layout <- "toy_layout_9"
  expect_identical(err_id(cmp(ev, g$golden, g$unit)), "tools.bad_layout")
  expect_identical(err_id(env$.pc_parity_as_matrix(matrix(1, 2, 5), "rf_wide_hourly_216")), "tools.bad_layout")
})

test_that("[ac-0] dispatch por layout: nenhum ramo por nome de modelo no comparador", {
  src <- readLines(PARITY)
  code <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("model_id|toy-30|lgbm\\b|dessem-|holt|random-forest", sub("##.*$", "", code))))
})

test_that("[ac-1] excesso do oraculo vira fora_da_faixa sem falhar; um dia a menos que a faixa falha por forma", {
  for (cs in list(list("dessem_semihourly_48", 9L), list("rf_wide_hourly_216", 9L), list("lgbm_day_48", 2L))) {
    g <- G(cs[[1L]], cs[[2L]], extra_day = TRUE)
    r <- cmp(E(g), g$golden, g$unit)
    expect_true(r$ok, info = cs[[1L]])
    expect_identical(r$fora_da_faixa_long, 48L, info = cs[[1L]])
    expect_identical(r$fora_da_faixa, if (cs[[1L]] == "rf_wide_hourly_216") 0L else 48L, info = cs[[1L]])
  }
  g <- G("dessem_semihourly_48", 9L, extra_day = TRUE)
  expect_identical(cmp(E(g), g$golden, g$unit)$fora_da_faixa, 48L)
  ## CONTROL: o envelope entrega 1 dia a menos que a faixa do membro (9 dias)
  for (cs in list(list("dessem_semihourly_48", 9L), list("rf_wide_hourly_216", 9L), list("lgbm_day_48", 2L))) {
    g <- G(cs[[1L]], cs[[2L]], extra_day = TRUE)
    ev <- E(g, delivery_days = cs[[2L]] - 1L)
    if (cs[[1L]] != "rf_wide_hourly_216") ev$values <- ev$values[seq_len(cs[[2L]] - 1L), , drop = FALSE]
    r <- cmp(ev, g$golden, g$unit, delivery_days = cs[[2L]])
    expect_false(r$ok, info = cs[[1L]])
    expect_gt(r$delivery_missing, 0L)
    if (cs[[1L]] != "rf_wide_hourly_216") expect_false(r$shape_ok)
  }
})

test_that("[ac-2] join por interval_end = timestamp + 30 min; a meia hora ja observada nao entra; sem o +30 min acusa deslocamento", {
  for (cs in CASES) {
    g <- G(cs[[1L]], cs[[2L]])
    ev <- E(g)
    obs <- data.table::data.table(interval_end = g$au, y_pred = 9e6)       # interval_end <= available_until: ignorado
    ev$delivery <- data.table::rbindlist(list(obs, ev$delivery))
    expect_true(cmp(ev, g$golden, g$unit)$ok, info = cs[[1L]])
    ctl <- cmp(E(g), g$golden, g$unit, join_shift_min = 0L)               # comparador-fixture sem o deslocamento
    expect_false(ctl$ok, info = cs[[1L]]); expect_gt(ctl$max_abs_delta, 1e-5)
  }
})

test_that("[ac-3] tres day0_source leem o dia O da sua fonte; fonte errada reprova e o relatorio a nomeia", {
  for (cs in list(list("rf_wide_hourly_216", 9L, "oracle_origin_minus_1"), list("hw_matrix_nx48", 9L, "oracle_discarded_step"),
                  list("lgbm_day_48", 2L, "native"), list("dessem_semihourly_48", 9L, "native"))) {
    g <- G(cs[[1L]], cs[[2L]])
    r <- cmp(E(g), g$golden, g$unit)
    expect_true(r$ok, info = cs[[3L]]); expect_identical(r$day0_source, cs[[3L]])
  }
  g <- G("rf_wide_hourly_216", 9L)
  u <- g$unit; u$day0_source <- "native"                                  # CONTROL: fonte errada numa unidade RF
  r <- cmp(E(g), g$golden, u)
  expect_false(r$ok); expect_gt(r$max_abs_delta, 1e-5); expect_identical(r$day0_source, "native")
  g <- G("hw_matrix_nx48", 9L)
  u <- g$unit; u$day0_source <- "native"
  r <- cmp(E(g), g$golden, u)
  expect_false(r$ok); expect_identical(r$day0_source, "native")
  g <- G("dessem_semihourly_48", 9L)
  u <- g$unit; u$day0_source <- "oracle_origin_minus_1"
  expect_identical(err_id(cmp(E(g), g$golden, u)), "tools.bad_units")
  g <- G("rf_wide_hourly_216", 9L); g$golden$sister <- NULL
  expect_identical(err_id(cmp(E(g), g$golden, g$unit)), "tools.golden_missing")
})

## --- CLI (processo real) -------------------------------------------------------------------------------------------
TZ <- "Etc/GMT+3"
cli_setup <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    rt <- tempfile("pp-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
    vr <- pc_toy_variant("toy-30", descriptor = list(uses_rng = TRUE),
                         files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines("toy-30", 0L)))
    file.copy(file.path(vr, "toy-30"), file.path(rt, "models"), recursive = TRUE)
    file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
    f <- file.path(rt, "models", "toy-30", "run.yaml")
    y <- yaml::read_yaml(f); y$backtest <- list(protocol_default = "fixed"); yaml::write_yaml(y, f)
    O <- lubridate::ymd("2026-10-01")
    deck <- tempfile("pp-deck-")
    env$prevcarga_fixture_deck(deck, seed = 11, areas = c("A", "B"), days = seq(O - 12L, O, by = "day"),
                               load_until = lubridate::ymd_hm("2026-10-01 09:00", tz = TZ))
    pc_deck_topology(file.path(deck, "topology.yaml"), c("A", "B"))
    uf <- tempfile("units-", fileext = ".yaml")
    writeLines(c("packages: [lubridate, data.table]", "units:",
      "  - {id: u1, area: A, origin: 2026-10-01, available_until: 2026-10-01T09:00, weather_issue_cutoff: available_until, day0_source: native}"), uf)
    unit <- env$pc_golden_units(uf)$units[[1L]]
    models <- file.path(rt, "models")
    envl <- env$.pc_parity_envelope("toy-30", unit, models, deck, file.path(deck, "topology.yaml"))
    out <- tempfile("pp-golden-")
    native <- list(layout = envl$layout, values = envl$values, meta = list())
    long <- data.frame(timestamp = envl$delivery$interval_end - lubridate::minutes(30L), y_pred = envl$delivery$y_pred)
    .pc_toy_golden_write(unit, native, long, "toy-30", out, models, data_dir = deck)
    cache <<- list(models = models, deck = deck, units = uf, out = out, topo = file.path(deck, "topology.yaml"), envl = envl)
  }
})
cli <- function(s, extra = character(), golden = s$out)
  pc_rscript(c(PARITY, "--model", "toy-30", "--units", s$units, "--golden", golden, "--root", s$models,
               "--data", s$deck, "--topology", s$topo, extra))

test_that("[ac-4] rodada verde: exit 0, relatorio e sha256 (texto e JSON unico)", {
  s <- cli_setup()
  expect_identical(s$envl$layout, "lgbm_day_48")
  r <- cli(s)
  expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  sha <- digest::digest(file.path(s$out, "parity_report.json"), algo = "sha256", file = TRUE)
  expect_true(any(grepl(paste0("sha256: ", sha), r$stdout, fixed = TRUE)))
  expect_true(any(grepl("u1 layout=lgbm_day_48 .*day0_source=native ok=TRUE", r$stdout)))
  rep <- jsonlite::read_json(file.path(s$out, "parity_report.json"))
  expect_identical(rep$units[[1L]]$ok, TRUE)
  expect_true(all(c("unit", "layout", "max_abs_delta", "n_compared", "fora_da_faixa", "day0_source", "ok") %in% names(rep$units[[1L]])))
  rj <- cli(s, c("--format", "json"))
  expect_identical(rj$status, 0L)
  j <- jsonlite::fromJSON(paste(rj$stdout, collapse = "\n"), simplifyVector = FALSE)
  expect_true(j$ok); expect_identical(j$result$sha256, sha)
})

test_that("[ac-4] golden ausente, fingerprint e digest de dados divergentes: exit 1, ASCII, nunca skip", {
  s <- cli_setup()
  ascii <- function(x) expect_false(any(grepl("[^ -~]", x)))
  r <- cli(s, golden = file.path(tempdir(), "nao-existe"))
  expect_identical(r$status, 1L); expect_true(any(grepl("tools.golden_missing", r$stderr))); ascii(r$stderr)
  lockf <- file.path(s$models, "toy-30", "tests", "parity", "golden.lock")
  orig <- readLines(lockf)
  on.exit(writeLines(orig, lockf), add = TRUE)
  lk <- jsonlite::read_json(lockf, simplifyVector = FALSE)
  lk$units$u1$fingerprint$packages$lubridate <- "0.0.1"
  writeLines(jsonlite::toJSON(lk, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA), lockf)
  r <- cli(s)
  expect_identical(r$status, 1L); expect_true(any(grepl("tools.golden_stale", r$stderr)))
  expect_true(any(grepl("lubridate", r$stderr))); expect_true(any(grepl("plugin_golden.R", r$stderr))); ascii(r$stderr)
  writeLines(orig, lockf)
  part <- file.path(s$deck, "load", "area_code=A", "year=2026")
  extra <- file.path(part, "part-zz.parquet")
  file.copy(list.files(part, full.names = TRUE)[[1L]], extra)
  on.exit(unlink(extra), add = TRUE)
  r <- cli(s)
  expect_identical(r$status, 1L); expect_true(any(grepl("tools.golden_stale", r$stderr)))
  expect_true(any(grepl("particao", r$stderr))); ascii(r$stderr)
  unlink(extra)
})

test_that("[ac-4] paridade reprovada (+2e-5 numa celula do golden): exit 1 tools.parity_failed, sem trocar o limiar", {
  s <- cli_setup()
  unit <- env$pc_golden_units(s$units)$units[[1L]]
  e <- s$envl
  mk <- function(delta, out) {
    v <- e$values; v[[3L]] <- v[[3L]] + delta
    long <- data.frame(timestamp = e$delivery$interval_end - lubridate::minutes(30L), y_pred = e$delivery$y_pred)
    .pc_toy_golden_write(unit, list(layout = e$layout, values = v, meta = list()), long, "toy-30", out, s$models, data_dir = s$deck)
  }
  on.exit(mk(0, s$out), add = TRUE)       # restaura o lock do golden verde
  bad <- tempfile("pp-bad-")
  mk(2e-5, bad)
  r <- cli(s, golden = bad)
  expect_identical(r$status, 1L); expect_true(any(grepl("tools.parity_failed", r$stderr)))
  expect_true(any(grepl("sha256: ", r$stderr))); expect_false(any(grepl("[^ -~]", r$stderr)))
  mk(9e-6, bad)
  expect_identical(cli(s, golden = bad)$status, 0L)
})

test_that("[ac-4] grep: plugin_parity.R sem 1e-4, regua relativa nem prevcarga_wmape; limiar unico 1e-5", {
  src <- paste(readLines(PARITY), collapse = "\n")
  expect_false(grepl("1e-4", src, fixed = TRUE))
  expect_false(grepl("prevcarga_wmape|max_rel|rel_tol|relativ", src))
  expect_identical(length(unique(regmatches(src, gregexpr("[0-9]e-[0-9]+", src))[[1L]])), 1L)
  expect_false(any(grepl("[^\x01-\x7f]", readLines(PARITY, warn = FALSE))))
})

test_that("[ac-4] .pc_parity_check_lock em memoria: golden velho (arquivo adulterado) e unidade fora do lock", {
  g <- G("lgbm_day_48", 2L)
  lockf <- file.path(g$lock_root, "toy-30", "tests", "parity", "golden.lock")
  lk <- jsonlite::read_json(lockf, simplifyVector = FALSE)
  d <- env$.pc_golden_unit_dir(g$out, "toy-30", g$unit)
  chk <- function(lock, unit = g$unit) env$.pc_parity_check_lock(lock, unit, NULL, d, pc_rscript_bin())
  expect_true(chk(lk))
  expect_identical(err_id(chk(lk, modifyList(g$unit, list(id = "zz")))), "tools.golden_missing")
  saveRDS(list(x = 1), file.path(d, "native.rds"))
  expect_identical(err_id(chk(lk)), "tools.golden_stale")
})
