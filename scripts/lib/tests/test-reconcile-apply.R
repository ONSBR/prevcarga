## test-reconcile-apply.R -- orquestracao train/apply do Reconcile (R.3). Leitor espionado (nenhum deck real);
## perfis congelados pelo registry (R.2) em tempdir. Bracos FORBIDDEN (delta=0) pareados a CONTROL (delta!=0).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long",
                       "deck_reader", "dates", "evaluation", "reconcile"))
O <- "2026-01-15"
Od <- lubridate::ymd(O)

apply_q <- function(...) suppressWarnings(env$prevcarga_reconcile_apply(...))

## troca binding do env de teste (as funcoes da lib moram em `env`) e restaura ao fim do teste
ra_mock <- function(..., .frame = parent.frame()) {
  for (n in names(list(...))) {
    old <- env[[n]]
    withr::defer(assign(n, old, envir = env), envir = .frame)
    assign(n, list(...)[[n]], envir = env)
  }
}

expect_pc <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$exit_code, as.integer(exit))
  invisible(cond)
}

ra_setup <- function(.env = parent.frame(), load_label = "start") {
  d <- tempfile("ra-state-"); dir.create(file.path(d, "artifacts"), recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE), envir = .env)
  topo_path <- pc_topology_fixture(3L)
  ctx <- list(state_dir = d, tz = pc_rc_tz, load_label = load_label, topology_path = topo_path, root = tempdir())
  list(ctx = ctx, topo = env$prevcarga_topology(path = topo_path, root = tempdir()))
}

## congela + promove um perfil para `ss` com khat_origin = origin (ratio constante `r`)
ra_profile <- function(s, ss, origin = O, r = 1.1, load_label = "start", tz = pc_rc_tz) {
  o <- lubridate::ymd(origin)
  ratio <- pc_truth_ratio(seq(o - 40L, o - 1L, by = "day"), function(d) r, tz = tz)
  fit <- env$prevcarga_khat_fit(ratio, o, 30L)
  meta <- list(ss = ss, khat_origin = o, window_days = fit$window_days, window_from = fit$window_from,
               window_to = fit$window_to, profile_keys = env$prevcarga_khat_keys, week_start = 7L,
               areas = s$topo$subsystems[[ss]]$areas, loss = s$topo$subsystems[[ss]]$loss, n_ratio = fit$n_ratio,
               load_label = load_label, timezone = tz, data_hash = env$prevcarga_khat_data_hash(ratio),
               code_commit = "abc", renv_lock_hash = strrep("a", 64))
  fz <- env$prevcarga_khat_freeze(fit, meta, s$ctx)
  suppressWarnings(env$prevcarga_khat_advance_latest(ss, fz$version, o, s$ctx))
  fz$version
}

## verdade espionada: SS_true = f * soma das folhas; folha = 100*codigo + idx do intervalo
ra_actual_stub <- function(s, calls, factor = 1.3) {
  function(ctx, area, from = NULL, to = NULL) {
    calls$n <- calls$n + 1L
    st <- lubridate::ymd(O, tz = pc_rc_tz) + lubridate::minutes(30L * (1:48))
    leaf <- function(a) 100 * pc_leaf_code(a) + 1:48
    y <- if (area %in% s$topo$order) {
      factor * Reduce(`+`, lapply(s$topo$subsystems[[area]]$areas, leaf))
    } else leaf(area)
    data.frame(interval_end = st, y_true = y)
  }
}

test_that("AC1 need_truth = eval: sem eval o leitor nao e chamado; com eval y_true casa e y_pred e identical", {
  s <- ra_setup(); ra_profile(s, "SS1"); ra_profile(s, "SS2"); ra_profile(s, "SS3")
  leaves <- pc_leaves_long(s$topo, O, s$ctx)
  calls <- new.env(); calls$n <- 0L
  loads <- 0L
  ra_mock(prevcarga_read_load = function(...) { loads <<- loads + 1L; stop("nao") },
                                  prevcarga_read_actual = ra_actual_stub(s, calls))
  r0 <- apply_q(leaves, s$ctx, "SS2")
  expect_identical(calls$n, 0L); expect_identical(loads, 0L)
  expect_true(all(is.na(r0$long$SS2$y_true)))
  expect_null(r0$wmape); expect_null(r0$long$SIN)
  r1 <- apply_q(leaves, s$ctx, "SS2", eval = TRUE)
  expect_gt(calls$n, 0L)
  expect_identical(r1$long$SS2$y_pred, r0$long$SS2$y_pred)
  codes <- vapply(s$topo$subsystems$SS2$areas, pc_leaf_code, 0L)
  expect_equal(r1$long$SS2$y_true, 1.3 * (100 * sum(codes) + length(codes) * 1:48), tolerance = 1e-9)
  expect_named(r1$wmape$SS2, c("bottomup", "reconciliado", "teto"))
  expect_true(all(vapply(r1$wmape$SS2, is.finite, NA)))
  expect_s3_class(r1$scorecard, "data.frame")
  ## FORBIDDEN: SS_true x 1,3 nao mexe em y_pred; CONTROL: o wmape reconciliado muda
  calls$n <- 0L
  ra_mock(prevcarga_read_actual = ra_actual_stub(s, calls, factor = 1.3 * 1.3))
  r2 <- apply_q(leaves, s$ctx, "SS2", eval = TRUE)
  expect_identical(r2$long$SS2$y_pred, r1$long$SS2$y_pred)
  expect_gt(abs(r2$wmape$SS2$reconciliado - r1$wmape$SS2$reconciliado), 1e-5)
})

test_that("AC2 state.khat_missing: sem latest => exit 13, hint e nada devolvido", {
  s <- ra_setup()
  leaves <- pc_leaves_long(s$topo, O, s$ctx)
  res <- NULL
  cond <- expect_pc(res <- apply_q(leaves, s$ctx, "SS1"), "state.khat_missing", 13L)
  expect_null(res)
  expect_match(cond$hint, "reconcile --mode train antes", fixed = TRUE)
  expect_false(grepl("[^ -~]", conditionMessage(cond)))
})

test_that("AC3 politica por origem: O+1 aborta; O aplica (idade 0, com e sem pino); O-1 aborta mesmo com pino", {
  s <- ra_setup(); leaves <- pc_leaves_long(s$topo, O, s$ctx)
  ra_profile(s, "SS1", origin = format(Od + 1L))
  expect_pc(apply_q(leaves, s$ctx, "SS1"), "honesty.khat_after_origin", 15L)
  s2 <- ra_setup(); v1 <- ra_profile(s2, "SS1", origin = format(Od - 1L))
  ## FORBIDDEN: nunca reusa o latest de outra origem
  expect_pc(apply_q(leaves, s2$ctx, "SS1"), "state.khat_missing", 13L)
  expect_pc(apply_q(leaves, s2$ctx, "SS1", khat_version = v1), "state.khat_missing", 13L)   # pino O-1 tambem aborta
  ## CONTROL: perfil ajustado para a propria origem aplica com idade 0
  v0 <- ra_profile(s2, "SS1", origin = O, r = 1.2)
  r0 <- apply_q(leaves, s2$ctx, "SS1")
  expect_identical(r0$khat$SS1$versao, v0)
  expect_identical(r0$khat$SS1$khat_age_days, 0L)
  rp <- apply_q(leaves, s2$ctx, "SS1", khat_version = v0)         # pino em O aplica
  expect_identical(rp$khat$SS1$versao, v0); expect_identical(rp$khat$SS1$khat_age_days, 0L)
  expect_match(r0$khat$SS1$versao, "^v_[0-9a-f]{16}$")
  expect_identical(env$prevcarga_khat_check_policy(env$prevcarga_khat_thaw(s2$ctx, "SS1", v0), s2$ctx, Od), 0L)
})

test_that("refit por origem: origens diferentes pegam perfis diferentes, mesmo com latest em outra origem (backtest para tras)", {
  s <- ra_setup()
  vA <- ra_profile(s, "SS1", origin = "2026-01-20", r = 1.05)
  vB <- ra_profile(s, "SS1", origin = "2026-01-10", r = 1.15)      # treinada depois, origem anterior: latest segue em 01-20
  expect_false(identical(vA, vB))
  expect_identical(env$prevcarga_khat_resolve(s$ctx, "SS1"), vA)
  lA <- pc_leaves_long(s$topo, "2026-01-20", s$ctx); lB <- pc_leaves_long(s$topo, "2026-01-10", s$ctx)
  rA <- apply_q(lA, s$ctx, "SS1"); rB <- apply_q(lB, s$ctx, "SS1")
  expect_identical(rA$khat$SS1$versao, vA); expect_identical(rB$khat$SS1$versao, vB)
  expect_identical(rA$khat$SS1$khat_age_days, 0L); expect_identical(rB$khat$SS1$khat_age_days, 0L)
  ## CONTROL: o y_pred reflete o k de cada origem (1,05 x 1,15 sobre o mesmo BU)
  expect_equal(rB$long$SS1$y_pred[[1L]] / rA$long$SS1$y_pred[[1L]],
               1.15 / 1.05 * (lB$y_pred[1L] / lA$y_pred[1L]), tolerance = 1e-9)
})

test_that("AC4 contract.khat_convention: load_label ou fuso divergente aborta; CONTROL mesma convencao aplica", {
  s <- ra_setup(load_label = "end"); leaves <- pc_leaves_long(s$topo, O, s$ctx)
  ra_profile(s, "SS1", load_label = "start")
  c1 <- expect_pc(apply_q(leaves, s$ctx, "SS1"), "contract.khat_convention", 2L)
  expect_identical(c1$where, "load_label")
  s2 <- ra_setup(load_label = "start"); ra_profile(s2, "SS1", load_label = "start", tz = "America/Sao_Paulo")
  lv2 <- pc_leaves_long(s2$topo, O, s2$ctx)
  c2 <- expect_pc(apply_q(lv2, s2$ctx, "SS1"), "contract.khat_convention", 2L)
  expect_identical(c2$where, "timezone")
  s3 <- ra_setup(load_label = "start"); ra_profile(s3, "SS1", load_label = "start")
  expect_no_error(apply_q(pc_leaves_long(s3$topo, O, s3$ctx), s3$ctx, "SS1"))
})

test_that("AC5 completude.leaf_missing: subsistema unico e 'todos' (SIN nao composto); CONTROL fecha SIN", {
  s <- ra_setup(); for (x in c("SS1", "SS2", "SS3")) ra_profile(s, x)
  full <- pc_leaves_long(s$topo, O, s$ctx)
  miss <- full[full$area != "Ls2_2", ]
  res <- NULL
  c1 <- expect_pc(res <- apply_q(miss, s$ctx, "SS2"), "completude.leaf_missing", 3L)
  expect_identical(c1$where, "Ls2_2")
  c2 <- expect_pc(res <- apply_q(miss, s$ctx, "all"), "completude.leaf_missing", 3L)
  expect_identical(c2$where, "Ls2_2"); expect_null(res)
  ## subsistema que fecha pode ser pedido sozinho mesmo com folha de outro ausente; SIN nao sai
  ok <- apply_q(miss, s$ctx, "SS1")
  expect_null(ok$long$SIN)
  all3 <- apply_q(full, s$ctx, "all")
  expect_identical(all3$long$SIN$y_pred, all3$long$SS1$y_pred + all3$long$SS2$y_pred + all3$long$SS3$y_pred)
  expect_true(all(all3$long$SIN$area == "SIN"))
})

test_that("entrada: tz errado => contract.long_tz; lead divergente => contract.reconcile_input", {
  s <- ra_setup(); ra_profile(s, "SS1")
  full <- pc_leaves_long(s$topo, O, s$ctx)
  bad <- full; bad$target_datetime <- lubridate::with_tz(bad$target_datetime, "UTC")
  expect_pc(apply_q(bad, s$ctx, "SS1"), "contract.long_tz", 2L)
  bad2 <- full; i <- which(bad2$area == "Ls1_1")[[3L]]; bad2$lead[[i]] <- bad2$lead[[i]] + 1L
  expect_pc(apply_q(bad2, s$ctx, "SS1"), "contract.reconcile_input", 2L)
})

test_that("AC6 train: leitor com to = O-1; janela [O-W, O-1] inclusiva; congela e promove; sem verdade => khat_no_truth", {
  s <- ra_setup()
  W <- 60L
  days <- seq(Od - 800L, Od + 5L, by = "day")
  mk_truth <- function(change = NULL, factor = 1.1) {
    function(ctx, areas = NULL, from = NULL, to = NULL, ...) {
      seen$to <<- to; seen$from <<- from
      d <- days[days <= to & days >= from]
      out <- do.call(rbind, lapply(d, function(day) {
        t <- pc_truth_load(s$topo, "SS1", day, factor = if (!is.null(change) && day %in% change) 2 else factor)
        t[t$area_code %in% areas, ]
      }))
      data.table::setattr(out, "load_label", "start"); data.table::setattr(out, "tz", pc_rc_tz)
      out
    }
  }
  seen <- new.env()
  ra_mock(
    prevcarga_resolve_cuts = function(ctx, origin, ...) list(cuts = data.frame(last_data_day = origin - 1L)),
    prevcarga_read_load = mk_truth())
  r <- env$prevcarga_reconcile_train(s$ctx, Od, "SS1", W)
  expect_identical(seen$to, Od - 1L)
  expect_identical(r$SS1$khat_origin, Od)
  expect_true(r$SS1$promovido)
  expect_identical(r$SS1$n_ratio, W * 48L)
  expect_named(r$SS1$k_hat_resumo, c("min", "max", "fora_faixa"))
  expect_equal(r$SS1$k_hat_resumo$min, 1.1, tolerance = 1e-9)
  expect_identical(r$SS1$k_hat_resumo$fora_faixa, 0L)   # 1,1 dentro de [1,0; 1,2]
  expect_identical(env$prevcarga_khat_resolve(s$ctx, "SS1"), r$SS1$versao)
  v0 <- r$SS1$versao
  ## FORBIDDEN: verdade em O-61 e em O nao altera o perfil; CONTROL: O-60 e O-1 alteram
  for (d in list(Od - 61L, Od)) {
    ra_mock(prevcarga_read_load = mk_truth(change = d))
    expect_identical(env$prevcarga_reconcile_train(s$ctx, Od, "SS1", W)$SS1$versao, v0)
  }
  for (d in list(Od - 60L, Od - 1L)) {
    ra_mock(prevcarga_read_load = mk_truth(change = d))
    expect_false(identical(env$prevcarga_reconcile_train(s$ctx, Od, "SS1", W)$SS1$versao, v0))
  }
  ## fora_faixa contado contra khat_range
  ra_mock(prevcarga_read_load = mk_truth(factor = 1.5))
  expect_identical(env$prevcarga_reconcile_train(s$ctx, Od, "SS1", W)$SS1$k_hat_resumo$fora_faixa > 0L, TRUE)
  ## verdade vazia na janela
  ra_mock(
    prevcarga_read_load = function(ctx, areas = NULL, from = NULL, to = NULL, ...)
      data.table::data.table(area_code = character(), interval_start = as.POSIXct(character(), tz = pc_rc_tz),
                             interval_end = as.POSIXct(character(), tz = pc_rc_tz), load_mwh = double()))
  expect_pc(env$prevcarga_reconcile_train(s$ctx, Od, "SS1", W), "completude.khat_no_truth", 3L)
})
