## test-combination-next-month.R -- E6-T1: celula do mes seguinte do combine train (next_month). Fixture sintetica pequena
## (helper-combination-freeze.R), sem RNG, tudo em tempdir.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                       "evaluation", "features", "registry", "combination"))
env$prevcarga_runtime_enforce()

members <- env$prevcarga_members(root = pc_root())
ctx <- pc_cmb_ctx_freeze()
days <- function(from, to) format(seq(as.Date(from), as.Date(to), by = 1))
p1 <- pc_cmb_panel_joined(env, members, origins = days("2023-01-01", "2023-03-31"))
p2 <- pc_cmb_panel_joined(env, members, origins = days("2023-01-01", "2023-04-30"))

## sha256 da tabela de pesos do default (sem next_month) em p1, gravado ANTES da mudanca em combination.R (E6-T1)
## (pinado nos 8 membros originais; o 9o, LIN_UNI do E5-T2, entra no 2o caso do AC4 com sha proprio)
E6_FIT_P1_SHA256 <- "938f5bcf90709ab74af9436e1bf5cefcd374789090bc1ac21fe32f9a7fb56750"
E6_FIT_P1_SHA256_9 <- "345d983637266113817d1f11f90f8417a9756cc2d38d1bb5da761b67ceb49dd2"
plain <- function(x) env$.prevcarga_cmb_plain_table(x)

edit_panel <- function(p, f) {
  out <- data.table::copy(p)
  for (n in c("members_declared", "members_present", "provenance", "coverage"))
    data.table::setattr(out, n, attr(p, n))
  f(out)
  out
}

test_that("[AC1] train congela a celula 2023-04 por (lead, cut), com meta e cutoff do fingerprint no mes seguinte", {
  tr <- pc_cmb_trained(env, p1, members)
  cells <- tr$res$cells
  nx <- cells[cells$ym == "2023-04", ]
  keys <- unique(cells[, c("lead", "cut")])
  expect_equal(nrow(nx), nrow(keys))
  expect_identical(names(cells), names(env$.prevcarga_cmb_empty_weights()))
  obj <- env$prevcarga_thaw(file.path(tr$res$path, "weights.rds"))
  expect_identical(obj$meta$panel_last_ym, "2023-03")
  expect_identical(obj$meta$next_ym, "2023-04")
  expect_true("2023-04" %in% obj$weights$ym)
  fp <- env$.prevcarga_cmb_fingerprint("SECO", p1, cells, list(n_min = 24L, iters = 500L, grad_tol = 1e-6),
                                       members, pc_root(), next_ym = "2023-04")
  expect_identical(fp$cutoff, "2023-04-01")
})

test_that("[AC1] n da celula 2023-04 e o numero de linhas com interval_start < 2023-04-01 e y_true presente (membros completos)", {
  wt <- env$prevcarga_combina_fit(p1, next_month = TRUE)
  d <- data.table::as.data.table(p1)
  tz <- attr(d$target_datetime, "tzone")[1L]
  lim <- lubridate::ymd("2023-04-01", tz = tz)
  for (i in which(wt$ym == "2023-04" & is.na(wt$fallback))) {
    g <- d[d$lead == wt$lead[i] & d$cut == wt$cut[i]]
    Fm <- as.matrix(g[, names(wt$w[[i]]), with = FALSE])
    expect_identical(wt$n[i], as.integer(sum(g$interval_start < lim & !is.na(g$y_true) & stats::complete.cases(Fm))))
  }
})

test_that("[AC2] apply em 2023-04-01 com pesos de ate 2023-03: sem erro e sem aviso; y_pred == produce_combined da celula 2023-04", {
  tr <- pc_cmb_trained(env, p1, members)
  rows <- edit_panel(p2, function(x) NULL)[p2$ym == "2023-04"]
  expect_no_warning(ap <- env$prevcarga_combina_apply("SECO", tr$dir, rows, ctx, members = members, fallback_eqw = FALSE))
  expect_gt(nrow(ap), 0L)
  wt <- data.table::as.data.table(env$prevcarga_thaw(file.path(tr$res$path, "weights.rds"))$weights)
  want <- env$prevcarga_produce_combined(rows, wt)
  expect_identical(sort(ap$y_pred), sort(want[!is.na(want)]))
})

test_that("[AC3] w, a, fallback e n da celula 2023-04 do next_month == os do fit com as origens de abril", {
  ta <- env$prevcarga_combina_fit(p1, next_month = TRUE)
  tb <- env$prevcarga_combina_fit(p2)
  ta <- ta[ta$ym == "2023-04", ]; tb <- tb[tb$ym == "2023-04", ]
  expect_gt(nrow(ta), 0L)
  expect_identical(ta$lead, tb$lead); expect_identical(ta$cut, tb$cut)
  expect_identical(ta$w, tb$w); expect_identical(ta$a, tb$a)
  expect_identical(ta$fallback, tb$fallback); expect_identical(ta$n, tb$n)
})

test_that("[AC4] default == snapshot sha256 (8 membros originais, sem LIN_UNI); next_month sem as linhas 2023-04 == default; delta 0 do full", {
  m8 <- members[members$sigla != "LIN_UNI", , drop = FALSE]
  expect_identical(nrow(m8), nrow(members) - 1L)
  q1 <- pc_cmb_panel_joined(env, m8, origins = days("2023-01-01", "2023-03-31"))
  def <- env$prevcarga_combina_fit(q1)
  expect_identical(digest::digest(plain(def), algo = "sha256"), E6_FIT_P1_SHA256)
  nx <- env$prevcarga_combina_fit(q1, next_month = TRUE)
  expect_false("2023-04" %in% def$ym)
  expect_identical(plain(nx[nx$ym != "2023-04", ]), plain(def))
})

test_that("[AC4] default == snapshot sha256 com os membros de prevcarga_members() (9, com LIN_UNI); LIN_UNI pesa na celula", {
  def <- env$prevcarga_combina_fit(p1)
  expect_identical(digest::digest(plain(def), algo = "sha256"), E6_FIT_P1_SHA256_9)
  expect_true("LIN_UNI" %in% members$sigla)
  expect_true(all(vapply(def$w, function(x) "f_LIN_UNI" %in% names(x), NA)))
  expect_true(any(vapply(def$w, function(x) isTRUE(x[["f_LIN_UNI"]] != 0), NA)))
  nx <- env$prevcarga_combina_fit(p1, next_month = TRUE)
  expect_false("2023-04" %in% def$ym)
  expect_identical(plain(nx[nx$ym != "2023-04", ]), plain(def))
})

test_that("[AC5] ultimo mes sem membro vira no_member em 2024-01 (virada de ano); painel vazio devolve a tabela vazia sem m_next", {
  pd <- pc_cmb_panel_joined(env, members, origins = days("2023-11-01", "2023-12-31"))
  fc <- grep("^f_", names(pd), value = TRUE)
  l1 <- min(pd$lead)
  pn <- edit_panel(pd, function(x) {
    for (f in fc) data.table::set(x, i = which(x$lead == l1 & x$ym == "2023-12"), j = f, value = NA_real_)
  })
  wt <- env$prevcarga_combina_fit(pn, next_month = TRUE)
  r <- wt[wt$lead == l1 & wt$ym == "2024-01", ]
  expect_equal(nrow(r), 1L)
  expect_identical(r$fallback, "no_member")
  expect_true("2024-01" %in% wt$ym[wt$lead != l1])
  e <- env$prevcarga_combina_fit(pn[0L], next_month = TRUE)
  expect_identical(plain(e), plain(env$.prevcarga_cmb_empty_weights()))
})

test_that("[AC1] helper next_ym: virada de ano e validacao", {
  expect_identical(env$.prevcarga_cmb_next_ym("2023-12", "Etc/GMT+3"), "2024-01")
  expect_identical(env$.prevcarga_cmb_next_ym("2023-03", "Etc/GMT+3"), "2023-04")
  e <- tryCatch(env$.prevcarga_cmb_next_ym("2023-3", "Etc/GMT+3"), prevcarga_error = identity)
  expect_identical(e$id, "contract.combina_input")
})
