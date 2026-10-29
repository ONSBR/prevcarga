## test-score.R -- verbo `score` (M3.3): modulo prevcarga_score (evaluation.R) e verbo cli/score.R pela fachada in-process.
## Verdade sempre re-juntada do deck por igualdade exata com interval_end; y_true embutido nunca e pontuado.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
for (m in c("errors", "runtime", "config", "banner", "cli", "topology", "schema_long", "provenance", "deck_reader",
            "dates", "validation", "evaluation"))
  sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())
prevcarga_runtime_enforce()

TZ <- "Etc/GMT+3"
O <- "2026-10-15"
o0 <- lubridate::force_tz(lubridate::as_datetime(O), TZ)

## raiz-copia com a topologia-fixture {A1, A2} (config + scripts/lib sem tests); --skip-validate nos casos de modulo
sc_root <- function(areas = c("A1", "A2"), env = parent.frame()) {
  d <- tempfile("sc-root"); dir.create(d)
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  file.copy(file.path(pc_root(), c("config", "models")), d, recursive = TRUE)
  dir.create(file.path(d, "scripts"))
  file.copy(file.path(pc_root(), "scripts", "lib"), file.path(d, "scripts"), recursive = TRUE)
  unlink(file.path(d, "scripts", "lib", "tests"), recursive = TRUE)
  pc_deck_topology(file.path(d, "config", "topology.yaml"), areas)
  d
}
run_s <- function(root, argv) {
  err <- character(); code <- NA_integer_
  out <- utils::capture.output({
    err <- utils::capture.output({
      code <- prevcarga_main(argv, root = root, runtime = function() invisible(NULL))
    }, type = "message")
  }, type = "output")
  list(code = code, out = out, err = err)
}
jd <- function(run) jsonlite::fromJSON(paste(run$out, collapse = "\n"), simplifyVector = FALSE)
non_ascii <- function(x) any(unlist(lapply(x, function(l) any(as.integer(charToRaw(l)) > 127L))))
## deck sentinela de A1/A2 de O-21 ate O+last; ctx do modulo com raiz da topologia-fixture
sc_deck <- function(last = 9L, areas = c("A1", "A2")) {
  d <- tempfile("sc-deck")
  pc_deck_sentinel("end", days = lubridate::ymd(O) + (-21:last), areas = areas, dir = d)
  d
}
sc_ctx <- function(d, root, areas = c("A1", "A2")) {
  c(pc_ctx_deck(d), list(tz = TZ, areas = areas, origin = O, root = root, topology_path = NULL,
                                weather_issue_cutoff = "available_until"))
}
sc_long <- function(ctx, areas = "A1", bias = 0, d_plus = 0:9, embedded = 0) {
  l <- pc_eval_long(areas, O, d_plus = d_plus, bias = bias, ctx = ctx)
  l$y_true <- l$y_true + embedded
  l
}
cli_score <- function(root, deck, long, extra = character(), fmt = "json") {
  run_s(root, c("score", "--long", pc_eval_write_long(long), "--data", deck, "--origin", O, "--skip-validate",
                "--format", fmt, extra))
}
hl_wmape <- function(run) vapply(jd(run)$result$headline$rows, function(r) r$wmape, 0)

root1 <- sc_root(env = globalenv())

test_that("WMAPE contra o deck: perfeito, enviesado, y_true embutido errado e perturbacao do deck (ac-1)", {
  d <- sc_deck(); ctx <- sc_ctx(d, root1)
  r0 <- cli_score(root1, d, sc_long(ctx), c("--area", "A1"))
  expect_identical(r0$code, 0L)
  j <- jd(r0)
  expect_true(j$ok); expect_identical(j$verb, "score"); expect_identical(r0$err, character())
  expect_identical(hl_wmape(r0), 0)
  deck_y <- sc_long(ctx)$y_true[sc_long(ctx)$d_plus == 1L]
  r1 <- cli_score(root1, d, sc_long(ctx, bias = 100), c("--area", "A1"))
  expect_gt(hl_wmape(r1), 0)
  expect_equal(hl_wmape(r1), prevcarga_wmape(deck_y, deck_y + 100))
  ## embutido +500 nunca e pontuado: o WMAPE segue o deck, nao o embutido
  rw <- cli_score(root1, d, sc_long(ctx, bias = 100, embedded = 500), c("--area", "A1"))
  expect_equal(hl_wmape(rw), prevcarga_wmape(deck_y, deck_y + 100))
  expect_false(isTRUE(all.equal(hl_wmape(rw), prevcarga_wmape(deck_y + 500, deck_y + 100))))
  ## perturbar o deck no dia d_plus = 1 muda a manchete
  n <- pc_eval_perturb(d, "A1", o0 + lubridate::days(1), o0 + lubridate::days(2), function(x) x * 1.3)
  expect_gt(n, 0)
  rp <- cli_score(root1, d, sc_long(ctx), c("--area", "A1"))
  expect_gt(hl_wmape(rp), 0)
  expect_equal(hl_wmape(rp), prevcarga_wmape(deck_y * 1.3, deck_y))
  ## so d_plus com verdade realizada: deck ate O+1 => d_plus 0 e 1
  d2 <- sc_deck(last = 1L)
  r2 <- cli_score(root1, d2, sc_long(sc_ctx(d2, root1)), c("--area", "A1"))
  expect_setequal(vapply(jd(r2)$result$scorecard, function(r) r$d_plus, 0L), 0:1)
})

test_that("escopo de areas: NULL == SIN, area sem particao some, area unica (ac-2)", {
  topo <- prevcarga_topology(root = pc_root())
  ss <- topo$order[[1L]]; lv <- topo$subsystems[[ss]]$areas
  g <- lv[[3L]]; present <- c(lv[1:2], ss)
  d <- tempfile("sc-deck"); ctx0 <- c(pc_ctx_deck(d), list(tz = TZ, areas = present, origin = O, root = pc_root()))
  pc_deck_sentinel("end", days = lubridate::ymd(O) + (-21:9), areas = present, dir = d)
  ctx <- ctx0; ctx$areas <- c(present, g, "SIN")
  long <- pc_eval_long(c(present, g, "SIN"), O, ctx = ctx)
  long$y_true <- long$y_true + 1e6
  r_null <- prevcarga_score(long, ctx0)
  r_sin <- prevcarga_score(long, ctx0, area = "SIN")
  expect_identical(r_null$scorecard, r_sin$scorecard)
  expect_identical(r_null$headline, r_sin$headline)
  expect_setequal(unique(r_null$scorecard$area), present)
  expect_false(g %in% r_null$scorecard$area); expect_false("SIN" %in% r_null$scorecard$area)
  expect_false(any(r_null$long$y_true >= 1e6, na.rm = TRUE))
  r_ss <- prevcarga_score(long, ctx0, area = ss)
  expect_identical(unique(r_ss$scorecard$area), ss)
  expect_false(any(r_ss$long$y_true >= 1e6, na.rm = TRUE))
  expect_identical(fail <- tryCatch(prevcarga_score(long, ctx0, area = "XX"), prevcarga_error = function(e) e$id),
                   "topology.area_undeclared")
})

test_that("matriz de exits JSON in-process: 2 / 2 / 2 / 4 / 0 com envelope unico e stderr vazio (ac-3)", {
  d <- sc_deck(); ctx <- sc_ctx(d, root1); lg <- sc_long(ctx)
  chk <- function(r, code, id = NULL, class = NULL) {
    j <- jd(r)
    expect_identical(r$code, code); expect_identical(j$exit_code, code)
    expect_length(Filter(nzchar, r$out), 1L); expect_identical(r$err, character())
    if (!is.null(id)) { expect_identical(j$error$id, id); expect_identical(j$error$class, class); expect_false(j$ok) }
    j
  }
  dm <- sc_deck(); pc_eval_drop_col(dm)
  chk(cli_score(root1, dm, lg, c("--area", "A1")), 2L, "contract.dataset_missing", "contract")
  chk(cli_score(root1, d, lg[, setdiff(names(lg), "y_pred")]), 2L, "contract.long_schema", "contract")
  chk(cli_score(root1, d, lg, c("--baseline", "xx")), 2L, "contract.baseline_kind", "contract")
  chk(run_s(root1, c("score", "--long", pc_eval_write_long(lg), "--data", file.path(tempdir(), "sem-deck"), "--area", "XX",
                     "--skip-validate", "--format", "json")), 4L, "topology.area_undeclared", "topology")
  dfut <- sc_deck(last = -1L)
  fut <- cli_score(root1, dfut, lg, c("--area", "A1"))
  j <- chk(fut, 0L)
  expect_true(j$ok)
  expect_identical(j$warnings[[1L]]$type, "nothing_to_score")
  expect_identical(j$warnings[[1L]]$id, "warning.nothing_to_score")
  expect_length(j$result$scorecard, 0L)
  ## erro em texto: mesmo exit, stdout vazio
  rt <- run_s(root1, c("score", "--long", pc_eval_write_long(lg), "--data", dm, "--skip-validate", "--origin", O))
  expect_identical(rt$code, 2L); expect_identical(rt$out, character())
})

test_that("manchete d_plus = 1, data_hash, available_until e skill contra o baseline (ac-4)", {
  d <- sc_deck(last = 2L); ctx <- sc_ctx(d, root1); lg <- sc_long(ctx, bias = 50)
  ## O-1 diferente do resto: a persistencia (d_plus = 1 le O-1) deixa de ser perfeita e o skill fica definido
  pc_eval_perturb(d, "A1", o0 - lubridate::days(1), o0, function(x) x * 1.1)
  rb <- cli_score(root1, d, lg, c("--area", "A1", "--baseline", "persistence"))
  rn <- cli_score(root1, d, lg, c("--area", "A1"))
  jb <- jd(rb)$result; jn <- jd(rn)$result
  expect_identical(jb$headline$d_plus, 1L)
  expect_true(all(vapply(jb$headline$rows, function(r) r$d_plus, 0L) == 1L))
  expect_true(0L %in% vapply(jb$scorecard, function(r) r$d_plus, 0L))
  expect_match(jb$data_hash, "^[0-9a-f]{64}$")
  man <- prevcarga_deck_manifest(d)
  expect_identical(jb$data_hash, prevcarga_data_hash(d, man, "end"))
  expect_identical(jb$data_hash, jn$data_hash)
  au <- prevcarga_resolve_cuts(c(ctx, list(topology_path = NULL)), lubridate::ymd(O), model = "lgbm")$available_until
  expect_identical(jb$available_until, sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", format(au, "%Y-%m-%dT%H:%M:%S%z", tz = TZ)))
  expect_identical(jb$baseline$kind, "persistence")
  r <- prevcarga_score(lg[lg$area == "A1", ], ctx, area = "A1", baseline_kind = "persistence")
  one <- r$long$d_plus == 1L
  sk <- prevcarga_skill_score(r$long[one, ], r$baseline_long)
  expect_equal(jb$headline$rows[[1L]]$skill, sk)
  expect_false(is.na(sk))
  expect_null(jn$baseline); expect_false("skill" %in% names(jn$headline$rows[[1L]]))
})

test_that("join por interval_end; CONTROL por interval_start reprova (ac-5)", {
  d <- sc_deck(); ctx <- sc_ctx(d, root1); lg <- sc_long(ctx, areas = c("A1", "A2"), embedded = 999)
  r <- prevcarga_score(lg, ctx)
  expect_true(all(r$scorecard$wmape == 0))
  act <- prevcarga_read_actual(ctx, "A1")
  l1 <- lg[lg$area == "A1", ]
  y_start <- act$y_true[match(l1$target_datetime, act$interval_end - lubridate::minutes(30))]   # join errado (CONTROL)
  expect_gt(prevcarga_wmape(y_start, l1$y_pred), 0)
  y_end <- act$y_true[match(l1$target_datetime, act$interval_end)]
  expect_identical(prevcarga_wmape(y_end, l1$y_pred), 0)
})

## --- gate-0 e config ---------------------------------------------------------------------------------------------
gate_root <- function(env = parent.frame()) {
  d <- tempfile("sc-groot"); dir.create(d)
  withr::defer(unlink(d, recursive = TRUE), envir = env)
  file.copy(file.path(pc_root(), c("config", "models")), d, recursive = TRUE)
  dir.create(file.path(d, "scripts"))
  file.copy(file.path(pc_root(), "scripts", "lib"), file.path(d, "scripts"), recursive = TRUE)
  unlink(file.path(d, "scripts", "lib", "tests"), recursive = TRUE)
  for (m in c("holt-winters", "random-forest", "lgbm", "dessem-regdin", "dessem-linear")) {
    f <- file.path(d, "models", m, "run.yaml"); y <- yaml::read_yaml(f)
    y$history_days <- list(train = 10, infer = 10); yaml::write_yaml(y, f)
  }
  d
}
gate_deck <- function() {
  areas <- c("SECO", "S"); days <- lubridate::ymd("2026-10-01") + (-39:0)
  d <- tempfile("sc-gdeck")
  pc_deck_sentinel("end", days = days, areas = areas, dir = d)
  h <- pc_deck_holidays_default(areas)
  h[which(as.numeric(simplified_code) < 0), simplified_code := bit64::as.integer64(1L)]
  .pc_deck_write(h, d, file.path("auxiliary", "holidays"), character(), "date")
  d
}

test_that("gate-0 com e sem --skip-validate, --model XX, --origin invalida; exit igual em texto (ac-6)", {
  groot <- gate_root(); d <- gate_deck()
  ctxg <- c(pc_ctx_deck(d), list(tz = TZ, areas = c("SECO", "S"), origin = "2026-10-01", root = groot))
  o1 <- "2026-10-01"
  lg <- pc_eval_long("SECO", o1, d_plus = 0:1, ctx = ctxg)
  lp <- pc_eval_write_long(lg)
  base <- c("score", "--long", lp, "--data", d, "--origin", o1)
  for (fmt in c("json", "text")) {
    ok <- run_s(groot, c(base, "--format", fmt))
    expect_identical(ok$code, 0L, info = fmt)
    sk <- run_s(groot, c(base, "--skip-validate", "--format", fmt))
    expect_identical(sk$code, 0L, info = fmt)
    mx <- run_s(groot, c(base, "--model", "XX", "--format", fmt))
    expect_identical(mx$code, 2L, info = fmt)
    bo <- run_s(groot, c("score", "--long", lp, "--data", d, "--origin=2026-13-01", "--format", fmt))
    expect_identical(bo$code, 8L, info = fmt)
  }
  expect_identical(jd(run_s(groot, c("score", "--long", lp, "--data", d, "--origin=2026-13-01", "--format", "json")))$error$id,
                   "config.bad_origin")
  ## manifest invalido (chave legada `corte`): gate-0 reprova com a classe do validate-deck
  dbad <- gate_deck()
  pc_deck_manifest(dbad, mutate = function(m) { m$corte <- "2026-10-01"; m })
  rb <- run_s(groot, c("score", "--long", lp, "--data", dbad, "--origin", o1, "--format", "json"))
  expect_identical(rb$code, 2L)
  expect_identical(jd(rb)$error$class, "contract")
  rt <- run_s(groot, c("score", "--long", lp, "--data", dbad, "--origin", o1))
  expect_identical(rt$code, 2L)
})

test_that("registro do verbo: kind impl, dono M3, nao e stub, parse docopt, fora da isencao (ac-7)", {
  tab <- prevcarga_verbs_discover()
  r <- tab[tab$verb == "score", ]
  expect_identical(nrow(r), 1L)
  expect_identical(r$owner, "M3"); expect_false(r$exempt)
  h <- r$handler[[1L]]
  expect_true(is.function(h))                                    # nao e stub (todo.not_implemented)
  expect_true(file.exists(file.path(pc_root(), "scripts", "lib", "cli", "score.R")))
  skip_if_not_installed("docopt")
  a <- .prevcarga_parse(c("score", "--long=a.parquet", "--data=d", "--baseline=persistence", "--skip-validate"), tab)
  expect_identical(a[["--long"]], "a.parquet"); expect_identical(a[["--data"]], "d")
  expect_identical(a[["--baseline"]], "persistence"); expect_true(isTRUE(a[["--skip-validate"]]))
  expect_null(a[["--area"]])
  expect_false("score" %in% c("-h", "--help", "doctor", "status"))
  expect_true(all(tab$verb[tab$exempt] %in% c("doctor", "status")))
  expect_true(grepl("--origin=<data>", prevcarga_usage(tab), fixed = TRUE))
})

test_that("ASCII puro e contrato de erro nos dois formatos, com e sem --debug (ac-8)", {
  d <- sc_deck(); ctx <- sc_ctx(d, root1); lg <- sc_long(ctx)
  lp <- pc_eval_write_long(lg)
  for (dbg in list(character(), "--debug")) {
    debug <- length(dbg) > 0L
    bad <- run_s(root1, c("score", "--long", lp, "--data", d, "--area", "XX", "--skip-validate", dbg))
    expect_identical(bad$code, 4L); expect_identical(bad$out, character())
    i <- which(bad$err == "prevcarga: erro [topology] (exit 4)")
    expect_length(i, 1L)
    expect_true(startsWith(bad$err[[i + 1L]], "o que:")); expect_true(startsWith(bad$err[[i + 2L]], "onde:"))
    expect_true(startsWith(bad$err[[i + 3L]], "como corrigir:"))
    expect_identical(i == 1L, !debug)                                   # traceback so com --debug
    expect_false(non_ascii(c(bad$out, bad$err)))
    badj <- run_s(root1, c("score", "--long", lp, "--data", d, "--area", "XX", "--skip-validate", "--format", "json", dbg))
    if (!debug) expect_identical(badj$err, character())
    expect_false(non_ascii(c(badj$out, badj$err)))
    expect_identical(jd(badj)$exit_code, 4L)
    for (fmt in c("text", "json")) {
      ok <- run_s(root1, c("score", "--long", lp, "--data", d, "--area", "A1", "--origin", O, "--skip-validate",
                           "--baseline", "persistence", "--format", fmt, dbg))
      expect_identical(ok$code, 0L); expect_false(non_ascii(c(ok$out, ok$err)))
      expect_identical(ok$err, character())
    }
  }
  ok <- run_s(root1, c("score", "--long", lp, "--data", d, "--area", "A1", "--origin", O, "--skip-validate",
                       "--baseline", "persistence"))
  expect_true(any(grepl("^score origins=2026-10-15 available_until=", ok$out)))
  expect_true(any(grepl("^MANCHETE D\\+1$", ok$out))); expect_true(any(grepl("resto-do-dia", ok$out)))
  expect_true(any(grepl("SKILL", ok$out)))
})
