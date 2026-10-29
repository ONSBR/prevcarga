## test-score-cli.R -- gates de PROCESSO do verbo `score` (M3.V; M3.3): `Rscript --vanilla <copia>/scripts/prevcarga score ...`
## real (pc_m3_cli: R.home('bin'), sem bash), matriz de exits x {text, json}, score == scorecard in-process, contrato
## de saida (ASCII, erro estruturado, traceback so com --debug) e shell sujo (TZ/LC_ALL/OPENBLAS) vs limpo (delta = 0).
## Tudo em tempfile(); nunca toca data/. Cada braco FORBIDDEN vem pareado com um CONTROL.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

TZ <- "Etc/GMT+3"
O <- "2026-10-15"
AREAS <- c("A1", "A2")

## instalacao-copia com a topologia-fixture {A1, A2}
root_fx <- pc_install_copy(mutate = function(r) pc_deck_topology(file.path(r, "config", "topology.yaml"), AREAS))
sdeck <- function(last = 9L, areas = AREAS) {
  d <- tempfile("m3-deck")
  pc_deck_sentinel("end", days = lubridate::ymd(O) + (-21:last), areas = areas, dir = d)
  d
}
sctx <- function(d, areas = AREAS) c(pc_ctx_deck(d), list(tz = TZ, areas = areas, origin = O))
slong <- function(ctx, areas = "A1", bias = 0, d_plus = 0:9, embedded = 0) {
  l <- pc_eval_long(areas, O, d_plus = d_plus, bias = bias, ctx = ctx)
  l$y_true <- l$y_true + embedded
  l
}
sc <- function(args, fmt = "json", envvars = character(), root = root_fx)
  pc_m3_cli(c(args, "--format", fmt), envvars = envvars, root = root)
mk <- function(long, deck, extra = character(), skip = TRUE, origin = TRUE)
  c("score", "--long", long, "--data", deck, if (origin) c("--origin", O), if (skip) "--skip-validate", extra)

d_ok <- sdeck(); ctx_ok <- sctx(d_ok)
d_nomwh <- sdeck(); pc_eval_drop_col(d_nomwh)
d_fut <- sdeck(last = -1L)
lg <- slong(ctx_ok)
f_perfect <- pc_eval_write_long(lg)
f_bias    <- pc_eval_write_long(slong(ctx_ok, bias = 100))
f_nopred  <- pc_eval_write_long(lg[, setdiff(names(lg), "y_pred")])

## ---- matriz de exits (AC 4) ---------------------------------------------------------------------------------------
score_cli_cases <- data.frame(
  name = c("perfeito", "enviesado", "sem_load_mwh", "sem_y_pred", "model_xx", "baseline_xx", "area_xx", "futuro",
           "origem_invalida"),
  exit = c(0L, 0L, 2L, 2L, 2L, 2L, 4L, 0L, 8L),
  id   = c(NA, NA, "contract.dataset_missing", "contract.long_schema", NA, "contract.baseline_kind",
           "topology.area_undeclared", NA, "config.bad_origin"),
  stringsAsFactors = FALSE)
score_cli_cases$argv <- list(
  mk(f_perfect, d_ok, c("--area", "A1")),
  mk(f_bias, d_ok, c("--area", "A1")),
  mk(f_perfect, d_nomwh, c("--area", "A1")),
  mk(f_nopred, d_ok),
  mk(f_perfect, d_ok, c("--model", "XX")),
  mk(f_perfect, d_ok, c("--baseline", "xx")),
  mk(f_perfect, d_ok, c("--area", "XX")),
  mk(f_perfect, d_fut, c("--area", "A1")),
  c("score", "--long", f_perfect, "--data", d_ok, "--origin=2024-13-01", "--skip-validate"))

headline_wmape <- function(j) vapply(j$result$headline$rows, function(r) r$wmape, 0)

for (i in seq_len(nrow(score_cli_cases))) local({
  cs <- score_cli_cases[i, ]
  argv <- cs$argv[[1L]]
  test_that(paste0("score [", cs$name, "] -> exit ", cs$exit, " em text e json"), {
    rj <- sc(argv, "json"); rt <- sc(argv, "text")
    expect_identical(rj$status, cs$exit, info = cs$name)
    expect_identical(rt$status, cs$exit, info = cs$name)               # status identico entre formatos
    j <- pc_m3_json(rj)                                                # um unico documento JSON em stdout
    expect_identical(j$exit_code, cs$exit)                             # exit_code do JSON == status real
    expect_identical(length(rj$stderr), 0L)                            # stderr vazio em --format json
    expect_identical(isTRUE(j$ok), cs$exit == 0L)
    if (!is.na(cs$id)) expect_identical(j$error$id, cs$id)
    expect_true(pc_m3_ascii(rj)); expect_true(pc_m3_ascii(rt))
    if (cs$exit != 0L) expect_identical(length(rt$stdout), 0L)         # erro em texto: stdout vazio
  })
})

test_that("score: acuracia, perfeito (wmape 0) vs enviesado (wmape > 0) e futuro com nothing_to_score", {
  jp <- pc_m3_json(sc(score_cli_cases$argv[[1L]]))
  jb <- pc_m3_json(sc(score_cli_cases$argv[[2L]]))
  expect_identical(headline_wmape(jp), 0)
  expect_gt(headline_wmape(jb), 0)                                      # CONTROL do braco perfeito
  jf <- pc_m3_json(sc(score_cli_cases$argv[[8L]]))
  expect_identical(jf$warnings[[1L]]$type, "nothing_to_score")
  expect_length(jf$result$scorecard, 0L)
  expect_length(jp$warnings, 0L)                                        # CONTROL: com carga realizada nao ha aviso
})

## ---- gate-0 (validate-deck) com e sem --skip-validate -----------------------------------------------------------------
root_gate <- pc_install_copy(mutate = function(r) {
  for (f in Sys.glob(file.path(r, "models", "*", "run.yaml"))) {
    y <- yaml::read_yaml(f)
    if (!is.null(y$history_days)) { y$history_days <- list(train = 10, infer = 10); yaml::write_yaml(y, f) }
  }
})
gate_deck <- function(bad = FALSE) {
  areas <- c("SECO", "S"); d <- tempfile("m3-gdeck")
  pc_deck_sentinel("end", days = lubridate::ymd("2026-10-01") + (-39:0), areas = areas, dir = d)
  h <- pc_deck_holidays_default(areas)
  h[which(as.numeric(simplified_code) < 0), simplified_code := bit64::as.integer64(1L)]
  .pc_deck_write(h, d, file.path("auxiliary", "holidays"), character(), "date")
  if (bad) pc_deck_manifest(d, mutate = function(m) { m$corte <- "2026-10-01"; m })   # chave legada: gate-0 reprova
  d
}
g_ctx <- function(d) c(pc_ctx_deck(d), list(tz = TZ, areas = c("SECO", "S"), origin = "2026-10-01"))
d_g_ok <- gate_deck(); d_g_bad <- gate_deck(bad = TRUE)
f_g <- pc_eval_write_long(pc_eval_long("SECO", "2026-10-01", d_plus = 0:1, ctx = g_ctx(d_g_ok)))
gargv <- function(d, skip = FALSE) c("score", "--long", f_g, "--data", d, "--origin", "2026-10-01",
                                      if (skip) "--skip-validate")

test_that("gate-0: manifest invalido sem --skip-validate -> 2 contract; CONTROL deck valido -> 0", {
  for (fmt in c("json", "text")) {
    bad <- sc(gargv(d_g_bad), fmt, root = root_gate)
    expect_identical(bad$status, 2L, info = fmt)
    ok <- sc(gargv(d_g_ok), fmt, root = root_gate)
    expect_identical(ok$status, 0L, info = fmt)
  }
  jb <- pc_m3_json(sc(gargv(d_g_bad), "json", root = root_gate))
  expect_identical(jb$error$class, "contract")
  expect_identical(jb$error$details$gate, "validate-deck")              # reprovado pelo gate-0
  sk <- pc_m3_json(sc(gargv(d_g_bad, skip = TRUE), "json", root = root_gate))
  expect_null(sk$error$details)                                         # CONTROL: --skip-validate pula o gate-0 (o leitor ainda recusa)
  expect_identical(sk$error$id, jb$error$id)
})

## ---- score == scorecard (AC 5) ---------------------------------------------------------------------------------------
test_that("score por processo == prevcarga_score + prevcarga_scorecard in-process sobre o LONG re-juntado", {
  for (m in c("errors", "runtime", "config", "banner", "cli", "topology", "schema_long", "provenance", "deck_reader",
              "dates", "validation", "evaluation"))
    sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())
  root_real <- pc_install_copy()
  topo <- prevcarga_topology(root = pc_root())
  ss <- topo$order[[1L]]; lv <- topo$subsystems[[ss]]$areas
  g <- lv[[3L]]; present <- c(lv[1:2], ss)
  d <- tempfile("m3-mdeck")
  pc_deck_sentinel("end", days = lubridate::ymd(O) + (-21:9), areas = present, dir = d)   # sem SIN e sem a folha g
  ctx0 <- c(pc_ctx_deck(d), list(tz = TZ, areas = c(present, g, "SIN"), origin = O, root = pc_root(),
                                 topology_path = NULL, weather_issue_cutoff = "available_until"))
  long <- pc_eval_long(c(present, g, "SIN"), O, d_plus = 0:9, bias = 37, ctx = ctx0)
  long$y_true <- long$y_true + 1e6                                            # y_true embutido errado: nunca e pontuado
  fp <- pc_eval_write_long(long)
  o0 <- lubridate::force_tz(lubridate::as_datetime(O), TZ)
  for (a in present) pc_eval_perturb(d, a, o0 - lubridate::days(1), o0, function(x) x * 1.1)   # persistencia nao perfeita
  args <- c("score", "--long", fp, "--data", d, "--origin", O, "--skip-validate", "--baseline", "persistence")
  rj <- pc_m3_cli(c(args, "--format", "json"), root = root_real)
  expect_identical(rj$status, 0L)
  j <- pc_m3_json(rj)
  r <- prevcarga_score(prevcarga_read_long(fp, ctx0), ctx0, baseline_kind = "persistence", origin = O)
  ref <- prevcarga_scorecard(r$long, c("area", "d_plus"), r$baseline_long)
  expect_gt(nrow(ref), 0L)
  expect_length(j$result$scorecard, nrow(ref))
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  for (k in seq_len(nrow(ref))) {
    row <- Filter(function(x) identical(x$area, ref$area[[k]]) && identical(as.integer(x$d_plus), ref$d_plus[[k]]),
                  j$result$scorecard)
    expect_length(row, 1L)
    for (col in c("wmape", "mape", "bias", "peak", "skill"))
      expect_equal(num(row[[1L]][[col]]), ref[[col]][[k]], tolerance = 1e-9, info = paste(ref$area[[k]], ref$d_plus[[k]], col))
  }
  ## skill == prevcarga_skill_score do grupo; o skill esta definido (CONTROL: nao e NA em todos os grupos)
  k1 <- which(ref$area == present[[1L]] & ref$d_plus == 1L)
  grp <- r$long[r$long$area == present[[1L]] & r$long$d_plus == 1L, , drop = FALSE]
  expect_equal(ref$skill[[k1]], prevcarga_skill_score(grp, r$baseline_long), tolerance = 1e-9)
  expect_true(any(!is.na(ref$skill)))
  ## y_true embutido (+1e6) nunca entra: o wmape com o deck e o do bias, nao o do embutido
  expect_lt(max(ref$wmape), 10)                                  # wmape em %: o embutido +1e6 daria > 1e3
  ## escopo: SIN e a folha sem particao fora do scorecard
  areas_json <- unique(vapply(j$result$scorecard, function(x) x$area, ""))
  expect_setequal(areas_json, present)
  expect_false(any(c("SIN", g) %in% areas_json))
  expect_match(j$result$data_hash, "^[0-9a-f]{64}$")
  ## --area omitido == --area SIN (stdout identico sem run_id)
  r_null <- pc_m3_cli(c(args, "--format", "json"), root = root_real)
  r_sin <- pc_m3_cli(c(args, "--area", "SIN", "--format", "json"), root = root_real)
  expect_identical(r_sin$status, 0L)
  expect_identical(pc_m3_strip_run_id(pc_m3_json(r_sin)), pc_m3_strip_run_id(pc_m3_json(r_null)))
  ## CONTROL: uma area explicita muda o escopo
  r_a <- pc_m3_cli(c(args, "--area", present[[1L]], "--format", "json"), root = root_real)
  expect_false(identical(pc_m3_strip_run_id(pc_m3_json(r_a)), pc_m3_strip_run_id(pc_m3_json(r_null))))
})

## ---- contrato de saida (AC 7) ---------------------------------------------------------------------------------------
test_that("ASCII <= 127 e erro estruturado em stderr; traceback so com --debug", {
  bad <- mk(f_perfect, d_ok, c("--area", "XX"))
  ok <- mk(f_perfect, d_ok, c("--area", "A1", "--baseline", "persistence"))
  for (dbg in list(character(), "--debug")) {
    debug <- length(dbg) > 0L
    rb <- pc_m3_cli(c(bad, dbg), root = root_fx)
    expect_identical(rb$status, 4L); expect_identical(length(rb$stdout), 0L)
    expect_true(pc_m3_ascii(rb))
    err <- pc_m3_lines(rb$stderr)
    i <- which(err == "prevcarga: erro [topology] (exit 4)")
    expect_length(i, 1L)
    expect_true(startsWith(err[[i + 1L]], "o que:")); expect_true(startsWith(err[[i + 2L]], "onde:"))
    expect_true(startsWith(err[[i + 3L]], "como corrigir:"))
    expect_false(any(grepl("Error in", err, fixed = TRUE)))
    expect_identical(i == 1L, !debug)                                   # traceback so com --debug
    rj <- pc_m3_cli(c(bad, "--format", "json", dbg), root = root_fx)
    expect_true(pc_m3_ascii(rj)); expect_identical(pc_m3_json(rj)$exit_code, 4L)
    if (!debug) expect_identical(length(rj$stderr), 0L)
    for (fmt in c("text", "json")) {
      r <- pc_m3_cli(c(ok, "--format", fmt, dbg), root = root_fx)
      expect_identical(r$status, 0L); expect_true(pc_m3_ascii(r)); expect_identical(length(r$stderr), 0L)
    }
  }
})

## ---- shell sujo vs limpo (AC 8) ---------------------------------------------------------------------------------------
dirty_ok <- !is.na(pc_dirty_lc_all())
clean <- c(TZ = NA, LC_ALL = NA, OMP_NUM_THREADS = NA, OPENBLAS_NUM_THREADS = NA)
dirty <- c(TZ = "America/Sao_Paulo", LC_ALL = pc_dirty_lc_all(), OPENBLAS_NUM_THREADS = "4")

test_that("shell sujo nao muda o numero (score): stdout byte-identico, exit 0 nos dois", {
  skip_if(!dirty_ok, "host sem locale UTF-8 alternativo para sujar LC_ALL")
  args <- c(mk(f_bias, d_ok, c("--area", "A1", "--baseline", "persistence")), "--format", "text")
  limpo <- pc_m3_cli(args, clean, root = root_fx)
  sujo  <- pc_m3_cli(args, dirty, root = root_fx)
  expect_identical(limpo$status, 0L); expect_identical(sujo$status, 0L)
  expect_identical(sujo$stdout, limpo$stdout)
  expect_gt(length(limpo$stdout), 0L)
  expect_true(all(as.integer(c(sujo$stdout, sujo$stderr)) <= 127L))
  jl <- pc_m3_cli(c(args[-length(args) + 0:1], "--format", "json"), clean, root = root_fx)
  js <- pc_m3_cli(c(args[-length(args) + 0:1], "--format", "json"), dirty, root = root_fx)
  expect_identical(pc_m3_strip_run_id(pc_m3_json(js)), pc_m3_strip_run_id(pc_m3_json(jl)))
  ## CONTROL: uma fixture diferente muda o stdout
  outro <- pc_m3_cli(c(mk(f_perfect, d_ok, c("--area", "A1", "--baseline", "persistence")), "--format", "text"),
                     clean, root = root_fx)
  expect_false(identical(outro$stdout, limpo$stdout))
})
