## test-reconcile-parity.R -- paridade Delta=0 do ajuste por origem contra o golden do oraculo (R.6).
## Linux e Windows: nunca chama o oraculo nem Rscript. A entrada e sintetica e regerada em memoria por
## `rg_build_inputs()` (scripts/tools/reconcile_golden.R); nada dela e versionado. As saidas do oraculo moram em
## `.state/golden/reconcile/` (gitignored) com lock hash-only versionado em fixtures/reconcile/golden.lock:
## golden ausente = SKIP com a receita; sha256 divergente do lock = FALHA. Os bracos FORBIDDEN/CONTROL sobre a
## entrada sintetica rodam sempre. Receita e excecoes conscientes do A/B: fixtures/reconcile/README.md.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "schema_long", "deck_reader", "evaluation", "reconcile"))
rg <- new.env(parent = env)
sys.source(file.path(pc_root(), "scripts", "tools", "reconcile_golden.R"), envir = rg)
topo <- env$prevcarga_topology(root = pc_root())
ctx <- list(tz = "Etc/GMT+3")
FX <- file.path(pc_root(), "scripts", "lib", "tests", "fixtures", "reconcile")
LOCK <- jsonlite::read_json(file.path(FX, "golden.lock"), simplifyVector = FALSE)
GDIR <- file.path(pc_root(), ".state", "golden", "reconcile")
GOLDEN <- c("golden_profile.parquet", "golden_long.parquet")
TOL <- 1e-9
WINDOW <- 730L

INP <- rg$rg_build_inputs(LOCK$origin, LOCK$days, LOCK$seed)
INP$leaves <- env$prevcarga_assert_long(as.data.frame(INP$leaves), ctx)

O <- function() lubridate::ymd(LOCK$origin)

fit_apply <- function(truth, leaves, fit_fun = env$prevcarga_khat_fit, window = WINDOW) {
  ratio <- env$prevcarga_khat_ratio(truth, "S", topo)
  fit <- fit_fun(ratio, O(), window)
  bu <- suppressWarnings(env$prevcarga_reconcile_bottomup(leaves, "S", topo, ctx))
  ap <- suppressWarnings(env$prevcarga_khat_apply(bu, fit, ctx))
  list(fit = fit, bu = bu, ap = ap, ratio = ratio)
}

## troca o binding `name` em `e` (ambiente do modulo sourced) ate o fim do local() chamador; restaura ao sair
with_binding <- function(e, name, value, frame = parent.frame()) {
  old <- get(name, envir = e)
  assign(name, value, envir = e)
  do.call(on.exit, list(substitute(assign(name, old, envir = e), list(name = name, old = old, e = e)), add = TRUE),
          envir = frame)
  invisible(old)
}

## perfil de referencia (hour, month, dow, k): do oraculo (golden) ou do proprio ajuste base (bracos sinteticos)
as_profile <- function(fit) data.table::as.data.table(fit$profile)[, c("hour", "month", "dow", "k"), with = FALSE]
as_ylong <- function(ap) data.table::as.data.table(ap)[, c("forecast_origin", "target_datetime", "BU_pred", "y_pred"), with = FALSE]

max_dk <- function(fit, gp) {
  m <- merge(as_profile(fit), gp, by = c("hour", "month", "dow"), suffixes = c("", ".ref"))
  if (nrow(m) != nrow(gp)) return(Inf)   # bucket da referencia ausente no ajuste tambem quebra a paridade
  max(abs(m$k - m$k.ref))
}

max_dy <- function(ap, gl) {
  m <- merge(data.table::as.data.table(ap)[, c("forecast_origin", "target_datetime", "y_pred"), with = FALSE], gl,
             by = c("forecast_origin", "target_datetime"), suffixes = c("", ".ref"), all = TRUE)
  if (anyNA(m$y_pred) || anyNA(m$y_pred.ref)) return(Inf)
  max(abs(m$y_pred - m$y_pred.ref))
}

## os 3 CONTROLs (dow com week_start = 1, janela de 729 dias, bucket por interval_end) contra uma referencia
controls <- function(base, gp, gl) {
  dow1 <- local({
    with_binding(env, "prevcarga_khat_week_start", 1L)
    fit_apply(INP$truth, INP$leaves)
  })
  w729 <- fit_apply(INP$truth, INP$leaves, window = 729L)
  end_ratio <- data.table::copy(base$ratio)
  end_ratio[, interval_start := interval_end]
  fit_end <- env$prevcarga_khat_fit(end_ratio, O(), WINDOW)
  ap_end <- local({
    with_binding(env, "prevcarga_long_interval_start", function(target_datetime, granularity_min, tz = NULL) target_datetime)
    suppressWarnings(env$prevcarga_khat_apply(base$bu, base$fit, ctx))
  })
  c(dow1_k = max_dk(dow1$fit, gp),
    ## perfil ajustado com week_start = 1 e aplicado com a convencao do oraculo (domingo = 1): y_pred diverge
    dow1_y = max_dy(suppressWarnings(env$prevcarga_khat_apply(base$bu, dow1$fit, ctx)), gl),
    w729_k = max_dk(w729$fit, gp), w729_y = max_dy(w729$ap, gl),
    end_k = max_dk(fit_end, gp), end_y = max_dy(ap_end, gl))
}

test_that("R.6 AC4 - lock hash-only: entrada regerada bate com o digest do lock; nada de dado versionado", {
  expect_true(file.exists(file.path(FX, "README.md")))
  expect_match(LOCK$oracle_commit, "^[0-9a-f]{40}$")
  expect_identical(LOCK$window_days, WINDOW)
  expect_identical(LOCK$golden_dir, ".state/golden/reconcile")
  expect_setequal(names(LOCK$files), GOLDEN)
  for (f in GOLDEN) expect_match(LOCK$files[[f]]$sha256, "^[0-9a-f]{64}$")
  ## a entrada sintetica regerada e exatamente a que o oraculo leu quando o golden foi gerado
  expect_identical(nrow(INP$truth), LOCK$inputs$truth_rows)
  expect_identical(nrow(INP$leaves), LOCK$inputs$leaves_rows)
  expect_identical(rg$rg_inputs_digest(INP), LOCK$inputs$sha256)
  ## CONTROL do digest: semente diferente muda a entrada (o digest tem dente)
  expect_false(identical(rg$rg_inputs_digest(rg$rg_build_inputs(LOCK$origin, LOCK$days, LOCK$seed + 1L)),
                         LOCK$inputs$sha256))
  ## so o lock e o README sao versionados aqui: nenhum parquet (extrato) na pasta de fixtures
  expect_identical(sort(list.files(FX), method = "radix"), c("README.md", "golden.lock"))
  ## o teste nao chama oraculo nem Rscript (varredura do proprio fonte)
  src <- readLines(file.path(pc_root(), "scripts", "lib", "tests", "test-reconcile-parity.R"), warn = FALSE)
  src <- src[!grepl("^\\s*##", src)]
  banned <- paste0(c("system", "reconcilia_", "rg_run_"), c("2(", "baseline", "oracle("))
  expect_false(any(vapply(banned, function(b) any(grepl(b, src, fixed = TRUE)), NA)))
})

BASE <- fit_apply(INP$truth, INP$leaves)

test_that("R.6 AC2/AC3 sintetico - FORBIDDEN: o futuro (>= O) nao mexe no ajuste; CONTROLs quebram contra a base", {
  expect_identical(attr(BASE$ap, "n_fallback"), 0L)
  expect_identical(BASE$fit$window_from, O() - 730L)
  expect_identical(BASE$fit$window_to, O() - 1L)
  o_ts <- lubridate::force_tz(lubridate::as_datetime(O()), ctx$tz)
  expect_true(any(BASE$ratio$interval_end > o_ts))   # ha futuro na verdade
  expect_gt(max(BASE$fit$profile$k) - min(BASE$fit$profile$k), 0.02)
  gp <- as_profile(BASE$fit)
  gl <- as_ylong(BASE$ap)
  ## FORBIDDEN (Delta = 0): inflar a verdade de S a partir de O nao muda k nem y_pred
  fut <- data.table::copy(INP$truth)
  fut[area_code == "S" & interval_start >= o_ts, load_mwh := load_mwh * 1.5]
  ff <- fit_apply(fut, INP$leaves)
  expect_identical(max_dk(ff$fit, gp), 0)
  expect_identical(max_dy(ff$ap, gl), 0)
  ## CONTROL (Delta != 0): inflar o dia O-730 (o primeiro da janela) muda o ajuste
  d0 <- lubridate::force_tz(lubridate::as_datetime(O() - 730L), ctx$tz)
  past <- data.table::copy(INP$truth)
  past[area_code == "S" & interval_start >= d0 & interval_start < d0 + lubridate::days(1L), load_mwh := load_mwh * 1.5]
  expect_gt(max_dk(fit_apply(past, INP$leaves)$fit, gp), 1e-6)
  ## os 3 CONTROLs tem dente na propria entrada sintetica, sem oraculo
  ctl <- controls(BASE, gp, gl)
  for (n in names(ctl)) expect_gt(ctl[[n]], 1e-6, label = n)
})

golden_or_skip <- function() {
  if (!dir.exists(GDIR))
    skip(paste0("golden do oraculo ausente em .state/golden/reconcile (gitignored): regenere no Linux com ",
                "`Rscript --vanilla scripts/tools/reconcile_golden.R --origin ", LOCK$origin,
                " --rscript <Rscript do oraculo>` (receita em scripts/lib/tests/fixtures/reconcile/README.md)"))
  for (f in GOLDEN) {
    p <- file.path(GDIR, f)
    if (!file.exists(p)) fail(paste("golden incompleto em .state/golden/reconcile: falta", f))
    expect_identical(digest::digest(p, algo = "sha256", file = TRUE), LOCK$files[[f]]$sha256,
                     info = paste("sha256 divergente do golden.lock:", f))
  }
  gp <- data.table::as.data.table(arrow::read_parquet(file.path(GDIR, "golden_profile.parquet")))
  gl <- data.table::as.data.table(arrow::read_parquet(file.path(GDIR, "golden_long.parquet")))
  expect_identical(nrow(gp), LOCK$files$golden_profile.parquet$rows)
  expect_identical(nrow(gl), LOCK$files$golden_long.parquet$rows)
  list(gp = gp, gl = gl)
}

test_that("R.6 AC2 oraculo - ratio + fit(O, 730) + bottom-up + apply: k e y_pred com Delta=0 (tol 1e-9) contra o golden", {
  g <- golden_or_skip()
  ## k por (hour, month, dow): todo bucket do golden existe no ajuste, mesmos valores
  m <- merge(BASE$fit$profile, g$gp, by = c("hour", "month", "dow"), suffixes = c("", ".oracle"))
  expect_equal(nrow(m), nrow(g$gp))
  expect_lt(max(abs(m$k - m$k.oracle)), TOL)
  ## y_pred por intervalo: target_datetime do golden = interval_end (oraculo: rotulo de inicio + 30 min)
  expect_equal(nrow(BASE$ap), nrow(g$gl))
  mm <- merge(data.table::as.data.table(BASE$ap), g$gl, by = c("forecast_origin", "target_datetime"),
              suffixes = c("", ".oracle"))
  expect_equal(nrow(mm), nrow(g$gl))
  expect_lt(max(abs(mm$y_pred - mm$y_pred.oracle)), TOL)
  expect_lt(max(abs(mm$BU_pred - mm$BU_pred.oracle)), TOL)
  expect_identical(mm$lead, mm$lead.oracle)
  expect_identical(mm$d_plus, mm$d_plus.oracle)
  ## o golden nao e trivial: k varia entre buckets
  expect_gt(max(g$gp$k) - min(g$gp$k), 0.02)
})

test_that("R.6 AC3 oraculo - CONTROL: week_start = 1, janela de 729 dias e bucket por interval_end quebram a paridade", {
  g <- golden_or_skip()
  expect_lt(max_dk(BASE$fit, g$gp), TOL)   # o ramo FORBIDDEN (paridade) vale antes dos CONTROLs
  ctl <- controls(BASE, g$gp, g$gl)
  for (n in names(ctl)) expect_gt(ctl[[n]], 1e-6, label = n)
})
