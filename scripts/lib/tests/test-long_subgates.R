## test-long_subgates.R -- sub-gates [M6] do LONG single-origin (M6.6): scripts/tools/gates/m6_long.R, descoberto por
## prevcarga_gates_run (P.0h). Paineis integros passam os 6 sub-gates; cada CONTROL (defeito injetado) reprova o gate correspondente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
.m6l_pack <- file.path(pc_root(), "scripts", "tools", "gates", "m6_long.R")
.m6l <- local({ e <- new.env(parent = globalenv()); sys.source(.m6l_pack, envir = e); e })
.m6l$.m6l_lib(file.path(pc_root(), "scripts", "lib"))

.fx <- function(dd = 9L, hm = "09:00") .m6l$m6l_fixture(dd, available_hm = hm)
.run <- function(fx, panel = fx$panel) .m6l$prevcarga_long_subgates(panel, fx$member, fx$cuts)
.passed <- function(r) stats::setNames(r$passed, r$gate)
.t0 <- function(fx) lubridate::force_tz(lubridate::as_datetime(fx$cuts$origin_day), fx$cuts$tz)
.k <- function(fx, p) as.integer(round(lubridate::time_length(p$target_datetime - .t0(fx), "minute") / 30))

test_that("[M6] paineis integros passam os 6 sub-gates (faixa LGBM O+2, faixa O+9; cortes 00:00, 09:00, 23:30)", {
  for (dd in c(2L, 9L)) for (hm in c("00:00", "09:00", "23:30")) {
    fx <- .fx(dd, hm)
    r <- .run(fx)
    expect_identical(r$gate, .m6l$M6L_SUBGATES)
    expect_true(all(r$passed), info = sprintf("dd=%d hm=%s: %s", dd, hm, paste(r$gate[!r$passed], collapse = ",")))
    expect_identical(max(fx$panel$target_datetime), lubridate::force_tz(lubridate::as_datetime(fx$cuts$origin_day + dd), fx$cuts$tz))
  }
})

test_that("[CONTROL] lead off-by-one reprova schema_long (e so ele)", {
  fx <- .fx(); p <- data.table::copy(fx$panel)
  p[, lead := lead + 1L]
  ok <- .passed(.run(fx, p))
  expect_false(ok[["schema_long"]])
  expect_true(all(ok[setdiff(names(ok), "schema_long")]))
  p2 <- data.table::copy(fx$panel); p2[, d_plus := d_plus + 1L]
  expect_false(.passed(.run(fx, p2))[["schema_long"]])
  p3 <- data.table::copy(fx$panel); data.table::setnames(p3, "y_true", "y_truth")
  expect_false(.passed(.run(fx, p3))[["schema_long"]])
})

test_that("[CONTROL] painel deslocado 1 dia reprova no_dayshift", {
  fx <- .fx(); p <- data.table::copy(fx$panel)
  p[, y_true := .m6l$.m6l_signal(.k(fx, p) + 48L)]
  ok <- .passed(.run(fx, p))
  expect_false(ok[["no_dayshift"]])
  expect_true(ok[["ytrue_real"]] && ok[["anti_leak"]] && ok[["schema_long"]])
  q <- data.table::copy(fx$panel); q[, y_true := .m6l$.m6l_signal(.k(fx, q) - 48L)]       # sentido contrario (D-1)
  expect_false(.passed(.run(fx, q))[["no_dayshift"]])
})

test_that("[CONTROL] y_true copia de y_pred reprova o anti-leak (MAPE > 0 exigido)", {
  fx <- .fx(); p <- data.table::copy(fx$panel)
  p[, y_true := y_pred]
  ok <- .passed(.run(fx, p))
  expect_false(ok[["anti_leak"]])
  expect_true(ok[["ytrue_real"]] && ok[["schema_long"]])
})

test_that("[CONTROL] ytrue_real forte: tudo NA, tudo zero e mediana baixa reprovam", {
  fx <- .fx()
  for (v in list(rep(NA_real_, nrow(fx$panel)), rep(0, nrow(fx$panel)), fx$panel$y_true / 10000)) {
    p <- data.table::copy(fx$panel); p[, y_true := v]
    expect_false(.passed(.run(fx, p))[["ytrue_real"]])
  }
})

test_that("[CONTROL] fim 1 slot antes reprova fim_da_faixa; inicio em available_until reprova primeira_entrega", {
  fx <- .fx()
  short <- fx$panel[-nrow(fx$panel)]
  ok <- .passed(.run(fx, short))
  expect_false(ok[["fim_da_faixa"]])
  expect_true(ok[["primeira_entrega"]])
  early <- data.table::copy(fx$panel)
  early <- rbind(early[1L], early)
  early[1L, target_datetime := fx$cuts$available_until]
  ok2 <- .passed(.run(fx, early))
  expect_false(ok2[["primeira_entrega"]])
  expect_false(ok2[["fim_da_faixa"]])                                    # a faixa tem 1 linha a mais
  late <- fx$panel[-1L]
  expect_false(.passed(.run(fx, late))[["primeira_entrega"]])            # comeca 1 slot depois
  long <- .m6l$m6l_fixture(9L); long$member$delivery_days <- 2L            # faixa maior que a do membro
  expect_false(.passed(.m6l$prevcarga_long_subgates(long$panel, long$member, long$cuts))[["fim_da_faixa"]])
})

test_that("[M6] nowcast D+0: sufixo do dia O nos cortes 12:00 (1o rotulo 12:30, lead 25..48 por interval_start), 13:30 e 00h; [CONTROL] dia O completo/sem sufixo reprova", {
  for (hm in c("00:00", "12:00", "13:30", "23:30")) {
    fx <- .fx(2L, hm)
    expect_true(.m6l$prevcarga_nowcast_check(fx$panel, fx$cuts)$passed, info = hm)
  }
  fx <- .fx(2L, "12:00")
  d0 <- fx$panel[d_plus == 0L]
  expect_identical(min(d0$lead), 25L); expect_identical(nrow(d0), 24L); expect_identical(min(d0$target_datetime), fx$cuts$available_until + 1800)
  expect_identical(min(.fx(2L, "13:30")$panel[d_plus == 0L]$lead), 28L)
  expect_identical(min(.fx(2L, "00:00")$panel[d_plus == 0L]$lead), 1L)
  extra <- data.table::copy(fx$panel); extra <- rbind(extra[1L], extra); extra[1L, lead := 25L]
  expect_false(.m6l$prevcarga_nowcast_check(extra, fx$cuts)$passed)           # slot dentro do que ja e conhecido
  expect_false(.m6l$prevcarga_nowcast_check(fx$panel[d_plus != 0L], fx$cuts)$passed)   # sem sufixo
  other <- .fx(2L, "00:00")                                                   # painel de 00h avaliado com corte 12:00
  expect_false(.m6l$prevcarga_nowcast_check(other$panel, fx$cuts)$passed)
})

test_that("pack: 7 gates, dono M6, tier fast, applies_to = os 5 modelos, sem harness_*/gate08, ids no charset do runner", {
  d <- .m6l$prevcarga_gate_pack_m6_long("lgbm", list(gates_dir = dirname(.m6l_pack)), "fast")
  expect_identical(d$gate, c("m6_schema_long", "m6_ytrue_real", "m6_anti_leak", "m6_no_dayshift", "m6_primeira_entrega", "m6_fim_da_faixa", "m6_nowcast_dplus0"))
  expect_true(all(d$owner == "M6") && all(d$tier_min == "fast") && all(vapply(d$test, is.function, NA)))
  expect_true(all(grepl("^[a-z0-9_]+$", d$gate)))
  for (a in d$applies_to) expect_setequal(a, c("random-forest", "lgbm", "holt-winters", "dessem-linear", "dessem-regdin"))
  expect_false(any(grepl("harness_|gate08", d$gate)))
  expect_false(any(grepl("quit\\(", readLines(.m6l_pack))))              # nao e standalone: o runner o carrega
})

## plugin_gates.R resolve a raiz pelo `ofile` do source(): roda num filho (script de topo), imprime uma linha por (modelo, gate).
.m6l_child <- function(body) {
  f <- tempfile("m6l-child", fileext = ".R"); on.exit(unlink(f), add = TRUE)
  writeLines(c(sprintf("source(%s)", deparse(file.path(pc_root(), "scripts", "tools", "plugin_gates.R"))),
               sprintf("gd <- %s", deparse(file.path(pc_root(), "scripts", "tools", "gates"))), body), f)
  pc_rscript(f)
}

test_that("descoberta pelo runner: os 6 sub-gates entram para cada um dos 5 modelos (e passam); nenhum para modelo fora de applies_to", {
  r <- .m6l_child(c(
    "for (m in c('random-forest', 'lgbm', 'holt-winters', 'dessem-linear', 'dessem-regdin', 'modelo-fora')) {",
    "  rows <- .pc_gates_packs(m, list(model = m, tier = 'fast', gates_dir = gd), 'fast', gd)",
    "  mine <- Filter(function(r) identical(r$source, 'm6_long.R'), rows)",
    "  for (x in mine) cat(m, x$gate, x$owner, x$tier_min, sep = ' | ', fill = TRUE)",
    "  cat(m, 'N', length(mine), 'ALL', paste(vapply(rows, `[[`, '', 'gate'), collapse = ','), fill = TRUE)",
    "  if (m %in% c('lgbm', 'dessem-regdin')) for (x in mine) { x$test(); cat(m, x$gate, 'PASS', sep = ' | ', fill = TRUE) }",
    "}"))
  expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  for (m in .m6l$M6L_MODELS) {
    gl <- sub(paste0("^", m, " \\| (m6_[a-z0-9_]+) \\| M6 \\| fast$"), "\\1", grep(paste0("^", m, " \\| m6_.* \\| M6 \\| fast$"), r$stdout, value = TRUE))
    expect_setequal(gl, .m6l$M6L_GATES)
    expect_true("m6_nowcast_dplus0" %in% gl)
  }
  ## os gates de harness/gate08 sao do pack M7 (m7_harness.R, M7.5): podem existir na lista ALL, mas nunca como linha do pack M6
  expect_false(any(grepl("harness_|gate08", grep(" \\| M6 \\| ", r$stdout, value = TRUE))))
  expect_match(grep("^modelo-fora N", r$stdout, value = TRUE), "^modelo-fora N 0 ")
  for (m in c("lgbm", "dessem-regdin")) expect_equal(sum(grepl(paste0("^", m, " \\| m6_.* \\| PASS$"), r$stdout)), 7L)
})

test_that("o pack substitui os slots [DEFERRED M6]; M7 segue como slot com dono", {
  r <- .m6l_child(c(
    "mk <- function(g, o) list(gate = g, owner = o, tier_min = 'fast', applies_to = 'all', test = NULL, source = 'gate_table')",
    "tab <- list(mk('contrato', 'PLUGINS'), mk('schema_long', 'DEFERRED M6'), mk('harness_parity', 'DEFERRED M7'))",
    "pack <- list(list(gate = 'm6_schema_long', owner = 'M6', tier_min = 'fast', applies_to = 'all', test = function() TRUE, source = 'm6_long.R'))",
    "tests <- data.frame(test = c('[PLUGINS] contrato: a', '[DEFERRED M6] schema_long: b', '[DEFERRED M7] harness_parity: c'), stringsAsFactors = FALSE)",
    "s <- .pc_gates_substitute(c(tab, pack), tests)",
    "cat(vapply(s$rows, `[[`, '', 'gate'), fill = TRUE)"))
  expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  g <- strsplit(r$stdout[[1L]], " ")[[1L]]
  expect_false("schema_long" %in% g)
  expect_true(all(c("contrato", "harness_parity", "m6_schema_long") %in% g))
})
