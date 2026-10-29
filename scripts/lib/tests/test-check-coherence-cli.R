## test-check-coherence-cli.R -- gates de PROCESSO do verbo `check-coherence` (M3.V; M3.4): processo real por
## pc_m3_cli (R.home('bin'), sem bash), matriz de exits x {text, json}, contrato de saida (ASCII, erro estruturado,
## traceback so com --debug) e shell sujo vs limpo (delta = 0). LONGs em tempfile(); nunca toca data/ nem config/.
## Cada braco FORBIDDEN (reprova) vem pareado com o CONTROL (LONG coerente passa).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
for (m in c("errors", "topology"))
  sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())

O <- "2026-10-15"
topo <- prevcarga_topology(root = pc_root())
S1 <- topo$order[[1L]]
ctx0 <- pc_eval_ctx(dir = tempfile("m3-coh-deck"))
base <- pc_coherent_long(topo, O, ctx = ctx0)
root_real <- pc_install_copy()
w <- function(l) pc_eval_write_long(l)
cc <- function(f, fmt = "json", envvars = character(), extra = character())
  pc_m3_cli(c("check-coherence", "--long", f, "--data", ctx0$data_dir, "--origin", O, "--format", fmt, extra),
            envvars = envvars, root = root_real)

## ---- matriz de exits (AC 6) -------------------------------------------------------------------------------------------
coh_cases <- data.frame(
  name = c("coerente", "sin_mais_500", "khat_1.20", "grade_deslocada", "sin_ausente", "sin_ausente_khat_fora", "area_RR",
           "sem_y_pred"),
  exit = c(0L, 11L, 11L, 11L, 3L, 3L, 4L, 2L),
  id   = c(NA, "coherence.sin_sum", "coherence.khat", "coherence.alignment", "completude.node_missing",
           "completude.node_missing", "topology.area_undeclared", "contract.long_schema"),
  stringsAsFactors = FALSE)
coh_cases$file <- c(
  w(base),
  w(pc_coh_bump(base, "SIN", 500)),
  w(pc_coh_set_khat(base, topo, S1, 1.20)),
  w(pc_coh_shift_grid(base, S1, O)),
  w(pc_coh_drop(base, "SIN")),
  w(pc_coh_set_khat(pc_coh_drop(base, "SIN", at = 1L), topo, S1, 1.20, at = 2L)),
  w(pc_coh_add_area(base, "RR")),
  w(base[, setdiff(names(base), "y_pred")]))
finding_cols <- c("check", "area", "forecast_origin", "target_datetime", "value", "expected", "class", "id", "detail")

for (i in seq_len(nrow(coh_cases))) local({
  cs <- coh_cases[i, ]
  test_that(paste0("check-coherence [", cs$name, "] -> exit ", cs$exit, " em text e json"), {
    rj <- cc(cs$file, "json"); rt <- cc(cs$file, "text")
    expect_identical(rj$status, cs$exit, info = cs$name)
    expect_identical(rt$status, cs$exit, info = cs$name)               # status identico entre formatos
    j <- pc_m3_json(rj)
    expect_identical(j$exit_code, cs$exit)                             # exit_code do JSON == status real
    expect_identical(length(rj$stderr), 0L)                            # stderr vazio em --format json
    expect_identical(isTRUE(j$ok), cs$exit == 0L)
    expect_true(pc_m3_ascii(rj)); expect_true(pc_m3_ascii(rt))
    if (!is.na(cs$id) && cs$exit != 0L) expect_identical(j$error$id, cs$id)
    if (cs$exit == 0L) {
      expect_identical(pc_m3_lines(rt$stdout)[[length(pc_m3_lines(rt$stdout))]], "OK")
      expect_length(j$result$findings, 0L)
    }
    if (cs$exit %in% c(3L, 4L, 11L)) {
      fl <- j$error$result$findings %||% j$result$findings
      expect_gt(length(fl), 0L)
      for (f in fl) expect_true(all(finding_cols %in% names(f)), info = cs$name)
      expect_true(cs$id %in% vapply(fl, function(f) f$id, ""))
    }
  })
})

test_that("SIN ausente + k fora: exit 3 (completude vence) com os dois findings", {
  j <- pc_m3_json(cc(coh_cases$file[coh_cases$name == "sin_ausente_khat_fora"]))
  fl <- j$error$result$findings %||% j$result$findings
  ids <- vapply(fl, function(f) f$id, "")
  expect_true(all(c("completude.node_missing", "coherence.khat") %in% ids))
  expect_identical(j$exit_code, 3L)
  ## CONTROL: so o k fora (sem remover o SIN) e exit 11, nao 3
  expect_identical(cc(coh_cases$file[coh_cases$name == "khat_1.20"])$status, 11L)
})

test_that("--area e recusado (config.usage, exit 8); CONTROL sem --area passa", {
  r <- cc(coh_cases$file[[1L]], "json", extra = c("--area", "A1"))
  expect_identical(r$status, 8L)
  expect_identical(pc_m3_json(r)$error$id, "config.usage")
  expect_identical(cc(coh_cases$file[[1L]])$status, 0L)
})

## ---- contrato de saida (AC 7) -------------------------------------------------------------------------------------------
test_that("ASCII <= 127 e erro estruturado em stderr; traceback so com --debug", {
  bad <- coh_cases$file[coh_cases$name == "sin_ausente"]
  for (dbg in list(character(), "--debug")) {
    debug <- length(dbg) > 0L
    rb <- cc(bad, "text", extra = dbg)
    expect_identical(rb$status, 3L)
    expect_true(pc_m3_ascii(rb))
    err <- pc_m3_lines(rb$stderr)
    i <- which(err == "prevcarga: erro [completude] (exit 3)")
    expect_length(i, 1L)
    expect_true(startsWith(err[[i + 1L]], "o que:")); expect_true(startsWith(err[[i + 2L]], "onde:"))
    expect_true(startsWith(err[[i + 3L]], "como corrigir:"))
    expect_false(any(grepl("Error in", err, fixed = TRUE)))
    expect_identical(i == 1L, !debug)                                   # traceback so com --debug
    rj <- cc(bad, "json", extra = dbg)
    expect_true(pc_m3_ascii(rj)); expect_identical(pc_m3_json(rj)$exit_code, 3L)
    if (!debug) expect_identical(length(rj$stderr), 0L)
    for (fmt in c("text", "json")) {
      r <- cc(coh_cases$file[[1L]], fmt, extra = dbg)
      expect_identical(r$status, 0L); expect_true(pc_m3_ascii(r)); expect_identical(length(r$stderr), 0L)
    }
  }
})

## ---- shell sujo vs limpo (AC 8) -----------------------------------------------------------------------------------------
dirty_ok <- !is.na(pc_dirty_lc_all())
clean <- c(TZ = NA, LC_ALL = NA, OMP_NUM_THREADS = NA, OPENBLAS_NUM_THREADS = NA)
dirty <- c(TZ = "America/Sao_Paulo", LC_ALL = pc_dirty_lc_all(), OPENBLAS_NUM_THREADS = "4")

test_that("shell sujo nao muda o numero (check-coherence): stdout byte-identico, exit 0 nos dois", {
  skip_if(!dirty_ok, "host sem locale UTF-8 alternativo para sujar LC_ALL")
  f <- coh_cases$file[[1L]]
  for (fmt in c("text", "json")) {
    limpo <- cc(f, fmt, clean); sujo <- cc(f, fmt, dirty)
    expect_identical(limpo$status, 0L); expect_identical(sujo$status, 0L)
    expect_gt(length(limpo$stdout), 0L)
    if (fmt == "text") expect_identical(sujo$stdout, limpo$stdout)
    else expect_identical(pc_m3_strip_run_id(pc_m3_json(sujo)), pc_m3_strip_run_id(pc_m3_json(limpo)))
    expect_true(all(as.integer(c(sujo$stdout, sujo$stderr)) <= 127L))
  }
  ## idem no ramo de falha: o numero dos findings nao depende do ambiente
  fb <- coh_cases$file[coh_cases$name == "sin_mais_500"]
  expect_identical(cc(fb, "text", dirty)$stdout, cc(fb, "text", clean)$stdout)
  ## CONTROL: uma fixture diferente muda o stdout
  expect_false(identical(cc(fb, "text", clean)$stdout, cc(f, "text", clean)$stdout))
})
