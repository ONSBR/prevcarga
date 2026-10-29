## test-coherence.R -- verbo `check-coherence` (M3.4): modulo prevcarga_check_coherence (evaluation.R) e verbo
## cli/check-coherence.R pela fachada in-process. Cada identidade com braco FORBIDDEN (passa) e CONTROL (reprova).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
for (m in c("errors", "runtime", "config", "banner", "cli", "topology", "schema_long", "provenance", "deck_reader",
            "dates", "validation", "evaluation"))
  sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())
prevcarga_runtime_enforce()

O <- "2026-10-15"
topo <- prevcarga_topology(root = pc_root())
subs <- topo$order
S1 <- subs[[1L]]; S2 <- subs[[2L]]
ctx0 <- pc_eval_ctx(dir = tempfile("coh-deck"))
base <- pc_coherent_long(topo, O, ctx = ctx0)
chk <- function(long, tp = topo, ...) prevcarga_check_coherence(long, tp, ...)
ids <- function(r) r$findings$id
run_c <- function(argv) {
  err <- character(); code <- NA_integer_
  out <- utils::capture.output({
    err <- utils::capture.output({
      code <- prevcarga_main(argv, root = pc_root(), runtime = function() invisible(NULL))
    }, type = "message")
  }, type = "output")
  list(code = code, out = out, err = err)
}
non_ascii <- function(x) any(unlist(lapply(x, function(l) any(as.integer(charToRaw(l)) > 127L))))

test_that("LONG coerente: ok, findings vazio com as 9 colunas, deterministico (ac-0)", {
  a <- chk(base); b <- chk(base)
  expect_true(a$ok); expect_identical(a$class, "ok"); expect_equal(nrow(a$findings), 0L)
  expect_identical(names(a$findings), c("check", "area", "forecast_origin", "target_datetime", "value", "expected",
                                        "class", "id", "detail"))
  expect_identical(a, b)
  expect_false(any(topo$subsystems[[S1]]$loss %in% base$area))  # sem nos de perda
})

test_that("(a) SIN: +500 e +1e-3 reprovam (CONTROL), +1e-6 passa (FORBIDDEN), tol e usado (ac-1)", {
  for (d in c(500, 1e-3)) {
    r <- chk(pc_coh_bump(base, "SIN", d))
    expect_false(r$ok); expect_identical(r$class, "coherence"); expect_identical(r$id, "coherence.sin_sum")
    expect_identical(r$findings$check, "sin_sum"); expect_identical(r$findings$area, "SIN")
    expect_false("khat" %in% r$findings$check)
    expect_equal(r$findings$value - r$findings$expected, d, tolerance = 1e-6)
  }
  small <- pc_coh_bump(base, "SIN", 1e-6)
  expect_true(chk(small)$ok)
  expect_false(chk(small, tol = 1e-7)$ok)
  expect_identical(prevcarga_error_lookup("coherence.sin_sum")$class, "coherence")
  expect_identical(unname(prevcarga_exit_codes[["coherence"]]), 11L)
})

test_that("(b) k-hat por subsistema: 1.20 e 0.95 reprovam, 1.05 passa, soma zero = indefinido, override com CONTROL (ac-2)", {
  hi <- chk(pc_coh_set_khat(base, topo, S1, 1.20))
  lo <- chk(pc_coh_set_khat(base, topo, S1, 0.95))
  for (r in list(hi, lo)) {
    expect_identical(r$class, "coherence"); expect_identical(r$findings$id, "coherence.khat")
    expect_identical(r$findings$area, S1); expect_false("sin_sum" %in% r$findings$check)
  }
  expect_true(chk(pc_coh_set_khat(base, topo, S1, 1.05))$ok)
  zero <- base
  zero$y_pred[zero$area %in% topo$subsystems[[S1]]$areas & zero$target_datetime == min(zero$target_datetime)] <- 0
  expect_no_warning(rz <- chk(zero))
  expect_true(any(rz$findings$id == "coherence.khat" & rz$findings$detail == "indefinido"))
  f <- tempfile(fileext = ".yaml")
  tp <- prevcarga_topology(path = pc_coh_topology_override(f, S1, c(1.0, 1.25)), root = pc_root())
  l120 <- pc_coh_set_khat(base, topo, S1, 1.20)
  expect_true(chk(l120, tp)$ok)                                     # FORBIDDEN: faixa do override
  expect_false(chk(pc_coh_set_khat(base, topo, S2, 1.20), tp)$ok)   # CONTROL: outro subsistema segue reprovando
})

test_that("(c) completude: SIN ausente, cobertura parcial legitima, folha ausente, folha extra na topologia (ac-3)", {
  r1 <- chk(pc_coh_drop(base, "SIN"))
  expect_identical(r1$class, "completude")
  expect_identical(r1$findings$id, "completude.node_missing"); expect_identical(r1$findings$area, "SIN")
  lv <- topo$subsystems[[S1]]$areas
  expect_true(chk(pc_coh_drop(base, c(lv[[1L]], S1, "SIN")))$ok)    # parcial legitima (ADR-0015)
  r3 <- chk(pc_coh_drop(base, lv[[1L]]))
  expect_true(lv[[1L]] %in% r3$findings$area[r3$findings$id == "completude.node_missing"])
  expect_identical(r3$class, "completude")
  f <- tempfile(fileext = ".yaml")
  tp <- prevcarga_topology(path = pc_coh_topology_extra_leaf(f, "ZZFICT", S1), root = pc_root())
  r4 <- chk(base, tp)
  expect_true("ZZFICT" %in% r4$findings$area); expect_identical(r4$class, "completude")
  expect_true(chk(base)$ok)                                          # CONTROL: seed
})

test_that("(d) area fora da topologia e precedencia completude > coherence (ac-4)", {
  r <- chk(pc_coh_add_area(base, "RR"))
  expect_identical(r$findings$check, "topology"); expect_identical(r$findings$id, "topology.area_undeclared")
  expect_equal(nrow(r$findings), 1L); expect_identical(r$class, "topology")
  both <- pc_coh_set_khat(pc_coh_drop(base, "SIN", at = 1L), topo, S1, 1.20, at = 2L)
  rb <- chk(both)
  expect_true(all(c("completude.node_missing", "coherence.khat") %in% rb$findings$id))
  expect_identical(rb$class, "completude")
  expect_identical(rb$class, prevcarga_exit_precedence(rb$findings$class))
})

test_that("(e) alinhamento: grade deslocada +30 min reprova antes das identidades; sem deslocamento passa (ac-5)", {
  sh <- pc_coh_shift_grid(base, S1, O)
  r <- chk(sh)
  expect_false(r$ok); expect_identical(r$class, "coherence")
  expect_identical(unique(r$findings$id), "coherence.alignment")
  expect_true(S1 %in% r$findings$area)
  expect_false(any(r$findings$check %in% c("sin_sum", "khat", "completude")))
  expect_true(chk(pc_coh_shift_grid(base, S1, O, minutes = 0))$ok)   # CONTROL
})

test_that("acumula sem abortar e ordena deterministicamente (ac-6)", {
  l <- pc_coh_bump(pc_coh_bump(base, "SIN", 500, at = 1L), "SIN", 500, at = 5L)
  l <- pc_coh_set_khat(l, topo, S1, 1.20, at = 9L)
  r <- chk(l)
  expect_equal(sum(r$findings$check == "sin_sum"), 2L); expect_equal(sum(r$findings$check == "khat"), 1L)
  expect_identical(r$findings, r$findings[order(r$findings$check, r$findings$forecast_origin,
                                                r$findings$target_datetime, r$findings$area, method = "radix"), ,
                                          drop = FALSE] |> `rownames<-`(NULL))
  expect_identical(chk(l[sample(nrow(l)), ]), r)
})

test_that("linha duplicada por (instante, area) e erro de contrato", {
  e <- tryCatch(chk(rbind(base, base[1L, ])), prevcarga_error = identity)
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$id, "contract.long_schema")
})

test_that("matriz de exits pela fachada nos dois formatos (ac-7)", {
  w <- function(l) pc_eval_write_long(l)
  cases <- list(
    list(w(base), 0L), list(w(pc_coh_bump(base, "SIN", 500)), 11L), list(w(pc_coh_drop(base, "SIN")), 3L),
    list(w(pc_coh_add_area(base, "RR")), 4L), list(w(pc_coh_shift_grid(base, S1, O)), 11L),
    list(w(base[, setdiff(names(base), "y_pred")]), 2L))
  for (fmt in c("json", "text")) for (cs in cases) {
    r <- run_c(c("check-coherence", "--long", cs[[1L]], "--data", ctx0$data_dir, "--origin", O, "--format", fmt))
    expect_identical(r$code, cs[[2L]])
    expect_false(non_ascii(c(r$out, r$err)))
    if (fmt == "json") {
      d <- jsonlite::fromJSON(paste(r$out, collapse = "\n"), simplifyVector = FALSE)
      expect_identical(as.integer(d$exit_code), r$code); expect_length(r$err, 0L)
      if (cs[[2L]] != 2L) expect_true(is.list(d$result$findings))
      if (cs[[2L]] == 11L) expect_gt(length(d$result$findings), 0L)
    } else if (cs[[2L]] != 0L) {
      expect_true(any(grepl(sprintf("^prevcarga: erro \\[[a-z]+\\] \\(exit %d\\)", cs[[2L]]), r$err)))
    } else expect_equal(r$out[[length(r$out)]], "OK")
  }
})

test_that("registro do verbo: tabela, parse docopt e recusa de --area (ac-8)", {
  v <- prevcarga_verbs_discover()
  row <- v[v$verb == "check-coherence", ]
  expect_identical(row$owner, "M3"); expect_false(row$exempt); expect_false(is.null(row$handler[[1L]]))
  r <- run_c(c("check-coherence", "--long=a", "--area=SECO"))
  expect_identical(r$code, 8L)
  a <- .prevcarga_parse(c("check-coherence", "--long=a.parquet", "--format=json"), v)
  expect_identical(a[["--long"]], "a.parquet"); expect_identical(a[["--format"]], "json")
  nt <- run_c(c("check-coherence", "--long", "nao-existe.parquet", "--data", ctx0$data_dir, "--format", "json"))
  expect_false(grepl("todo.not_implemented", paste(nt$out, collapse = "")))
})
