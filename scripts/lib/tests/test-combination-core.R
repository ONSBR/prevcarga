## test-combination-core.R -- nucleo numerico da combina (M4.1): simplex, FISTA, mascara, fit, produce, catalogo
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "combination"))
env$prevcarga_runtime_enforce()

## o modulo le o catalogo de erros do env onde foi carregado
fn <- function(name) get(name, envir = env)
expect_contract <- function(expr, where) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, "contract.combina_input")
  expect_identical(cond$exit_code, 2L)
  expect_match(conditionMessage(cond), "^\\[prevcarga/contract\\]")
  expect_match(cond$where, where, fixed = TRUE)
  expect_false(any(as.integer(charToRaw(conditionMessage(cond))) > 127L))
}
origins <- function(from = "2024-01-01", to = "2024-03-10")
  format(seq(as.Date(from), as.Date(to), by = 1))

test_that("M4.1 - AC1 simplex fechado, length 1, CONTROL which.max", {
  v <- c(0.6, 0.5, 0.1)
  expect_equal(unname(env$prevcarga_proj_simplex(v)), c(8, 6.5, 0.5) / 15, tolerance = 1e-12)
  for (v in list(c(0.6, 0.5, 0.1), c(-3, 2, 2), c(-1, -1, -1, -1), c(1e6, -1e6))) {
    w <- env$prevcarga_proj_simplex(v)
    expect_true(all(w >= -1e-9))
    expect_lt(abs(sum(w) - 1), 1e-9)
  }
  expect_identical(env$prevcarga_proj_simplex(5), 1)
  expect_identical(names(env$prevcarga_proj_simplex(c(a = 0.6, b = 0.5, c = 0.1))), c("a", "b", "c"))
  ## CONTROL: rho = which.max(cond) (primeiro TRUE) nao soma 1
  v <- c(0.6, 0.5, 0.1)
  u <- sort(v, decreasing = TRUE); css <- cumsum(u) - 1; ind <- seq_along(u)
  rho <- which.max(u - css / ind > 0)
  wbad <- pmax(v - css[rho] / rho, 0)
  expect_equal(sum(wbad), 2.4)
  expect_gt(abs(sum(wbad) - 1), 1)
  expect_contract(env$prevcarga_proj_simplex(numeric(0)), "proj_simplex")
  expect_contract(env$prevcarga_proj_simplex(c(1, NA)), "proj_simplex")
})

test_that("M4.1 - AC2 otimalidade bem-condicionada, recomputacao independente, CONTROL w deslocado", {
  f <- pc_cmb_wellcond()
  fit <- env$prevcarga_fit_convex(f$X, f$y)
  expect_false(fit$nonstationary)
  expect_lt(fit$grad_norm, 1e-6)
  g_ind <- pc_cmb_grad_indep(f$X, f$y, fit$w)
  expect_lt(g_ind, 1e-6)
  expect_equal(g_ind, env$prevcarga_proj_grad_norm(f$X, f$y, fit$w, fit$a), tolerance = 1e-9)
  expect_true(all(fit$w >= 0)); expect_lt(abs(sum(fit$w) - 1), 1e-9)
  expect_identical(names(fit$w), colnames(f$X))
  ## CONTROL: todo o peso no membro de maior erro
  err <- colMeans((f$X - f$y)^2)
  w_bad <- as.numeric(seq_along(err) == which.max(err))
  expect_gt(env$prevcarga_proj_grad_norm(f$X, f$y, w_bad), 1e-6)
  expect_gt(pc_cmb_grad_indep(f$X, f$y, w_bad), 1e-6)
})

test_that("M4.1 - AC3 cap-hit avisa, nonstationary e invariante da flag", {
  f <- pc_cmb_illcond(d = 3e-4, n = 60L)
  expect_warning(fit <- env$prevcarga_fit_convex(f$X, f$y, iters = 500L, grad_tol = 1e-6), "nao-estacion")
  expect_true(fit$nonstationary)
  expect_identical(fit$iters, 500L)
  expect_gte(fit$grad_norm, 1e-6)
  expect_gte(fit$grad_norm, 1.5e-6)   # margem do fixture sobre o cap
  w <- pc_cmb_wellcond()
  fits <- list(fit, env$prevcarga_fit_convex(w$X, w$y),
               env$prevcarga_fit_convex(w$X[, 1L, drop = FALSE], w$y),
               suppressWarnings(env$prevcarga_fit_convex(f$X, f$y, iters = 3L)))
  for (ft in fits) expect_identical(ft$nonstationary, ft$grad_norm >= 1e-6)
})

test_that("M4.1 - AC4 produce_combined e produce_eqw: valores fechados e CONTROLs", {
  p <- data.frame(lead = c(0L, 0L, 1L, 1L, 2L), cut = "", ym = "2024-02",
                  f_A = c(1, 2, 5, NA, 2), f_B = c(2, NA, 7, NA, 4), f_C = c(3, 4, NA, NA, 6))
  W <- data.table::data.table(
    lead = c(0L, 1L, 2L), cut = "", ym = "2024-02", a = c(2, 2, 9), fallback = c(NA, NA, "n_min"),
    w = list(c(f_A = 0.5, f_B = 0.3, f_C = 0.2), c(f_A = 0, f_B = 1, f_C = 0), c(f_A = 1/3, f_B = 1/3, f_C = 1/3)))
  y <- env$prevcarga_produce_combined(p, W)
  expect_equal(y[1], 2 + 0.5 * 1 + 0.3 * 2 + 0.2 * 3, tolerance = 1e-12)
  expect_equal(y[2], 2 + (0.5 * 2 + 0.2 * 4) / 0.7, tolerance = 1e-12)
  ## grupo lead=1: so f_A (peso 0) e f_B (peso 1) presentes na linha 3: sum(w_pres)=1; linha 4 sem membro -> NA
  expect_equal(y[3], 2 + 7, tolerance = 1e-12)
  expect_true(is.na(y[4]))
  ## fallback EQW: base 0
  expect_equal(y[5], env$prevcarga_produce_eqw(p)[5], tolerance = 1e-12)
  expect_equal(y[5], 4, tolerance = 1e-12)
  ## sum(w_pres) == 0: so o membro de peso 0 presente -> media dos presentes
  W0 <- W; W0$w[[1L]] <- c(f_A = 0, f_B = 0.6, f_C = 0.4)
  p0 <- data.frame(lead = 0L, cut = "", ym = "2024-02", f_A = 4, f_B = NA, f_C = NA)
  expect_equal(env$prevcarga_produce_combined(p0, W0), 4, tolerance = 1e-12)
  eq <- env$prevcarga_produce_eqw(p)
  expect_equal(eq, c(2, 3, 6, NA, 4), tolerance = 1e-12)
  ## CONTROLs: variantes erradas divergem do valor fechado
  closed <- 2 + (0.5 * 2 + 0.2 * 4) / 0.7
  expect_false(isTRUE(all.equal((2 + 0.5 * 2 + 0.2 * 4) / 0.7, closed)))
  expect_false(isTRUE(all.equal(2 + 0.5 * 2 + 0.2 * 4, closed)))
  ## linha sem celula aborta
  pm <- p[1, ]; pm$lead <- 9L
  expect_contract(env$prevcarga_produce_combined(pm, W), "produce_combined:lead=9 cut= ym=2024-02")
})

test_that("M4.1 - AC5 mascara de treino: isca fora, 23:30-00:00 dentro, CONTROLs", {
  tz <- pc_cmb_tz
  p <- pc_cmb_panel(origins())
  expect_identical(attr(p$target_datetime, "tzone"), tz)
  mask <- env$prevcarga_combina_train_mask(p, "2024-03")
  expect_gt(sum(mask), 0L)
  bait <- pc_cmb_bait_row(p, "2024-03")
  expect_true(any(bait))
  expect_true(any(bait & p$forecast_origin == "2024-02-25" & p$d_plus == 7L))
  expect_false(any(mask & bait))
  at <- function(s) lubridate::ymd_hms(s, tz = tz)
  last <- p$target_datetime == at("2024-03-01 00:00:00")     # 23:30-00:00 de 2024-02-29
  first <- p$target_datetime == at("2024-03-01 00:30:00")    # 00:00-00:30 de 2024-03-01
  expect_true(any(last)); expect_true(any(first))
  expect_true(all(mask[last]))
  expect_false(any(mask[first]))
  expect_true(any(p$ym[mask] == "2024-02"))
  ## CONTROL: criterio leaky ym < m inclui a isca
  expect_true(any((p$ym < "2024-03") & bait))
  ## CONTROL: target < inicio(m) exclui o 23:30-00:00
  ms <- env$prevcarga_combina_month_start("2024-03", tz)
  expect_false(any((p$target_datetime < ms)[last]))
  expect_identical(ms, lubridate::ymd("2024-03-01", tz = tz))
  ## tzone ausente/vazio
  p2 <- p; attr(p2$target_datetime, "tzone") <- NULL
  expect_contract(env$prevcarga_combina_train_mask(p2, "2024-03"), "train_mask")
  attr(p2$target_datetime, "tzone") <- ""
  expect_contract(env$prevcarga_combina_train_mask(p2, "2024-03"), "train_mask")
})

test_that("M4.1 - AC6 grade de celulas, fallbacks, cortes e coluna toda-NA fora do X", {
  p <- pc_cmb_panel(origins(), d_plus = 0:2, slots = c(0L, 47L), cuts = c("0900", "1700"),
    drop = list(C = function(p) p$lead == 0L & p$cut == "0900" & p$ym == "2024-03",
                A = function(p) p$cut == "1700" & p$ym < "2024-03"))
  W <- env$prevcarga_combina_fit(p, n_min = 24L)
  expect_identical(anyDuplicated(W[, c("lead", "cut", "ym")]), 0L)
  expect_identical(nrow(W), nrow(unique(p[, c("lead", "cut", "ym")])))
  expect_identical(names(W), c("lead", "cut", "ym", "a", "w", "fallback", "n", "iters", "grad_norm", "nonstationary"))
  jan <- W[W$ym == "2024-01" & W$cut == "0900", ]
  expect_true(all(jan$fallback == "first_month"))
  expect_true(all(jan$a == 0))
  expect_true(all(vapply(seq_len(nrow(jan)), function(i) isTRUE(all.equal(unname(jan$w[[i]]), rep(1 / 3, 3))), NA)))
  ## corte 1700 em marco: f_A quase ausente no treino -> n < n_min
  c17 <- W[W$cut == "1700" & W$ym == "2024-03", ]
  expect_true(all(c17$fallback == "n_min"))
  expect_true(all(c17$n < 24L))
  ## corte 0900 fita normalmente em marco
  expect_true(any(is.na(W$fallback[W$cut == "0900" & W$ym == "2024-03"])))
  ## cortes tem ajustes proprios; nenhuma coluna de interpolacao/replicacao
  expect_false(any(grepl("ponder|replic", names(W))))
  fa <- W[W$cut == "0900" & W$ym == "2024-02" & W$lead == 47L, ]
  fb <- W[W$cut == "1700" & W$ym == "2024-02" & W$lead == 47L, ]
  expect_false(identical(fa$fallback, fb$fallback) && isTRUE(all.equal(fa$w[[1L]], fb$w[[1L]])))
  ## f_C toda NA na celula (lead 0, 0900, 2024-03)
  cc <- W[W$lead == 0L & W$cut == "0900" & W$ym == "2024-03", ]
  expect_false("f_C" %in% names(cc$w[[1L]]))
  expect_lt(abs(sum(cc$w[[1L]]) - 1), 1e-9)
  ## n = complete-case dentro da mascara
  g <- p[p$lead == 47L & p$cut == "0900", ]
  mk <- env$prevcarga_combina_train_mask(g, "2024-02") & stats::complete.cases(g[, c("f_A", "f_B", "f_C")])
  expect_identical(fa$n, sum(mk))
  expect_true(all(W$n[W$ym == "2024-01"] == 0L))
})

test_that("M4.1 - AC7 determinismo em y_pred, dente do comparador, sem RNG", {
  p <- pc_cmb_panel(origins(), d_plus = 0:1, slots = c(0L, 36L))
  run <- function(pp) env$prevcarga_produce_combined(pp, env$prevcarga_combina_fit(pp))
  seed_before <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
  y1 <- run(p); y2 <- run(p)
  seed_after <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
  expect_identical(seed_before, seed_after)
  expect_lt(max(abs(y1 - y2), na.rm = TRUE), 1e-12)
  pp <- p; pp$f_A <- pp$f_A + 0.05
  expect_gt(max(abs(run(pp) - y1), na.rm = TRUE), 1e-5)
})

test_that("M4.1 - AC8 contrato de entradas invalidas", {
  f <- pc_cmb_wellcond()
  Xna <- f$X; Xna[2, 2] <- NA
  expect_contract(env$prevcarga_fit_convex(Xna, f$y), "fit_convex:X")
  expect_contract(env$prevcarga_fit_convex(f$X, f$y[-1L]), "fit_convex:length(y)")
  p <- pc_cmb_panel(origins("2024-01-01", "2024-01-05"), d_plus = 0L, slots = 0L)
  for (cl in c("target_datetime", "y_true", "ym", "lead", "cut"))
    expect_contract(env$prevcarga_combina_fit(p[, setdiff(names(p), cl)]), paste0("combina_fit:", cl))
  nof <- p[, !grepl("^f_", names(p))]
  expect_contract(env$prevcarga_combina_fit(nof), "combina_fit:f_*")
  expect_contract(env$prevcarga_produce_combined(nof, data.table::data.table()), "produce_combined:f_*")
  expect_contract(env$prevcarga_produce_eqw(nof), "produce_eqw:f_*")
  for (bad in c("2024-1", "202401", "2024-01-01", "ab"))
    expect_contract(env$prevcarga_combina_month_start(bad, pc_cmb_tz), "month_start:ym")
  expect_contract(env$prevcarga_combina_month_start("2024-03", ""), "month_start:tz")
})

test_that("M4.1 - AC9 catalogo distribuido combination.tsv", {
  dir <- file.path(pc_root(), "scripts", "lib", "errors")
  cat <- env$prevcarga_errors_load(dir = dir)
  ids <- c(contract.combina_input = "contract", contract.combina_run_mode_dup = "contract",
           contract.member_misaligned = "contract", contract.combina_artifact_mismatch = "contract",
           state.combina_not_trained = "state", config.bad_mode = "config")
  expect_true(all(names(ids) %in% cat$id))
  expect_identical(cat$class[match(names(ids), cat$id)], unname(ids))
  rows <- cat[cat$id %in% names(ids), ]
  expect_true(all(nzchar(rows$message)) && all(nzchar(rows$hint)))
  for (col in c("id", "message", "hint"))
    expect_false(any(vapply(rows[[col]], function(s) any(as.integer(charToRaw(s)) > 127L), NA)))
  expect_identical(anyDuplicated(cat$id), 0L)
  ## todo id referido em combination.R consta no catalogo
  src <- readLines(file.path(pc_root(), "scripts", "lib", "combination.R"), warn = FALSE)
  refs <- unique(sub("^prevcarga_abort\\(\"([a-z_.]+)\"$", "\\1",
                     unlist(regmatches(src, gregexpr("prevcarga_abort\\(\"([a-z_.]+)\"", src)))))
  expect_true(length(refs) >= 1L)
  expect_true(all(refs %in% cat$id))
  ## CONTROL: sem a linha do id, o codigo referiria id ausente
  tsv <- readLines(file.path(dir, "combination.tsv"), warn = FALSE)
  d2 <- tempfile("errs"); dir.create(d2)
  file.copy(list.files(dir, full.names = TRUE), d2)
  writeLines(tsv[!grepl("^contract.combina_input\t", tsv)], file.path(d2, "combination.tsv"))
  expect_false(all(refs %in% env$prevcarga_errors_load(dir = d2)$id))
  ## CONTROL: id repetido em outro TSV aborta contract.error_catalog_dup
  writeLines(c(tsv[1L], tsv[grepl("^contract.combina_input\t", tsv)]), file.path(d2, "zz-dup.tsv"))
  writeLines(tsv, file.path(d2, "combination.tsv"))
  cond <- tryCatch(env$prevcarga_errors_load(dir = d2), prevcarga_error = identity)
  expect_identical(cond$id, "contract.error_catalog_dup")
})
