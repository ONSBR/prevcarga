## test-combination-gates.R -- M4.V: gates da combina (G2 honesty-gate, otimalidade bruta, determinismo, sentinela, qualidade).
## Todo delta e sobre y_pred, nunca sobre w. A/B da combina e por propriedade/qualidade (ADR-0014), sem paridade com o legado
## leaky. Fixtures em tempdir; nenhum acesso a env vars (o ambiente sujo dos filhos fica em test-combine-cli.R).
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                       "evaluation", "features", "registry", "honesty_gate", "combination"))
env$prevcarga_runtime_enforce()

members <- env$prevcarga_members(root = pc_root())
short_m <- members$sigla[[1L]]
ctx_g2  <- pc_cmb_ctx_freeze()
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

## painel do G2 (origens 2024-01-01..2024-04-03, 8 membros, curto ate O+1) com as convencoes de carga para o apply
p_g2 <- pc_cmb_prov_conv(pc_cmb_g2_panel(env, members))
view <- pc_cmb_gate_view(p_g2)
cut_m <- env$prevcarga_combina_month_start("2024-03", pc_cmb_tz)
gate <- function(run_fn, v = view, cut = cut_m) env$prevcarga_honesty_gate(run_fn, v, cut = cut, horizon = 9)
wrap <- function(origin) pc_cmb_wrap_full(env, ctx_g2, origin)

## ---- G2 braco honesto -------------------------------------------------------------------------------------------------
g_ok <- gate(wrap("2024-03-01"))
test_that("M4.V - G2 honesto na 1a origem de um mes fitado (FORBIDDEN delta=0, CONTROL delta!=0)", {
  expect_identical(g_ok$verdict, "HONESTO")
  expect_lte(g_ok$forbidden_delta, 1e-5)
  expect_gt(g_ok$control_delta, 1e-5)
  expect_true(g_ok$reach_ok)
  expect_lte(g_ok$clean2_delta, 1e-5)
  ## guardas de nao-vacuo: O == inicio(m); mes FITADO; isca existe; o intervalo 23:30-00:00 de 29/fev e CONTROL e treino
  expect_identical(cut_m, lubridate::ymd("2024-03-01", tz = pc_cmb_tz))
  expect_identical(lubridate::ymd("2024-03-01", tz = pc_cmb_tz), env$prevcarga_combina_month_start("2024-03", pc_cmb_tz))
  det <- suppressWarnings(env$prevcarga_combina_full(p_g2, ctx_g2, detail = TRUE))
  used <- unique(p_g2$lead[p_g2$forecast_origin == "2024-03-01"])
  march <- det$weights[det$weights$ym == "2024-03" & det$weights$lead %in% used]
  expect_gt(nrow(march), 0L)
  expect_true(all(is.na(march$fallback)))
  expect_true(all(march$n >= env$prevcarga_combina_defaults$n_min))
  n_isca <- sum(p_g2$ym < "2024-03" & view$interval_end > cut_m & is.finite(p_g2$y_true))
  expect_gt(n_isca, 0L)
  edge <- which(view$interval_end == cut_m)
  expect_gt(length(edge), 0L)
  reg <- env$.prevcarga_hg_regions(view$interval_end, cut_m, 9L)
  expect_true(all(reg$control[edge]))
  expect_false(any(reg$forbidden[edge]))
  expect_true(all(env$prevcarga_combina_train_mask(p_g2, "2024-03")[edge]))
  expect_gt(g_ok$arms$forbidden$n_perturbed, 0L)
  expect_gt(g_ok$arms$control$n_perturbed, 0L)
})

## ---- G2 dentes ---------------------------------------------------------------------------------------------------------
test_that("M4.V - G2 dente (a): mascara leaky por mes-de-origem => VAZA", {
  orig <- get("prevcarga_combina_train_mask", envir = env)
  g <- pc_cmb_with_leaky_mask(env, gate(wrap("2024-03-01")))
  expect_identical(g$verdict, "VAZA")
  expect_gt(g$forbidden_delta, 1e-5)
  expect_identical(get("prevcarga_combina_train_mask", envir = env), orig)   # restaurada (on.exit)
  g2 <- gate(wrap("2024-03-01"))                                            # CONTROL: restaurada => volta a HONESTO
  expect_identical(g2$verdict, "HONESTO")
})

test_that("M4.V - G2 dente (b): combina_apply como run_fn => HARNESS QUEBRADO (vacuo rejeitado)", {
  dir <- file.path(withr::local_tempdir(), "state", "artifacts", "combina")
  dir.create(dir, recursive = TRUE)
  env$prevcarga_combina_train("SECO", p_g2, dir, root = pc_root(), members = members,
                              trained_at = "2024-01-01T00:00:00Z")
  run_apply <- function(p) suppressWarnings(env$prevcarga_combina_apply("SECO", dir, p, ctx_g2, members = members))
  g <- gate(run_apply)
  expect_identical(g$verdict, "HARNESS QUEBRADO")
  expect_lte(g$control_delta, 1e-5)
  expect_lte(g$forbidden_delta, 1e-5)
})

test_that("M4.V - G2 dente (c): full SEM o filtro por origem => VAZA; envolvido => HONESTO", {
  g <- gate(function(p) suppressWarnings(env$prevcarga_combina_full(p, ctx_g2)))
  expect_identical(g$verdict, "VAZA")
  expect_gt(g$forbidden_delta, 1e-5)
  expect_identical(g_ok$verdict, "HONESTO")   # o escopo forecast_origin == origin e necessario [TBD: ADR-0029]
})

test_that("M4.V - G2 dente (d): origem em mes EQW => HARNESS QUEBRADO", {
  g <- gate(wrap("2024-01-01"))
  expect_identical(g$verdict, "HARNESS QUEBRADO")
  expect_lte(g$control_delta, 1e-5)
  expect_true(g$reach_ok)
})

## ---- otimalidade bruta ---------------------------------------------------------------------------------------------------
test_that("M4.V - otimalidade bruta nas celulas fitadas e convergidas, caminho independente", {
  p_opt <- pc_cmb_illcond_lead(p_g2, lead = 18L)
  expect_warning(d <- env$prevcarga_combina_full(p_opt, ctx_g2, detail = TRUE), "nao-estacion")
  wt <- d$weights
  conv <- wt[is.na(fallback) & nonstationary == FALSE]
  expect_gt(nrow(conv), 0L)
  expect_true(any(wt$lead == 18L & wt$nonstationary))                 # a celula mal-condicionada nao convergiu
  expect_false(any(conv$lead == 18L & conv$ym %in% wt$ym[wt$lead == 18L & wt$nonstationary]))
  expect_true(all(is.na(conv$fallback)))                              # as de fallback ficam fora do conjunto testado
  expect_true(any(wt$nonstationary == FALSE & is.na(wt$fallback) & lengths(wt$w) == 7L))   # celulas com k = 7 (curto ausente)
  grads <- vapply(seq_len(nrow(conv)), function(i) {
    xy <- pc_cmb_cell_xy(env, p_opt, conv[i])
    pc_cmb_gate_grad(xy$X, xy$y, conv$w[[i]], conv$a[i])
  }, numeric(1))
  expect_gt(length(grads), 0L)
  expect_true(all(grads < 1e-6))
  ## CONTROL: todo o peso no membro de maior erro, intercepto otimo => gradient mapping > 1e-6 (fora de null(G))
  xy1 <- pc_cmb_cell_xy(env, p_opt, conv[1L])
  w_bad <- conv$w[[1L]]; w_bad[] <- 0
  w_bad[which.max(colMeans(abs(xy1$X - xy1$y)))] <- 1
  expect_gt(pc_cmb_gate_grad(xy1$X, xy1$y, w_bad, NULL), 1e-6)
  expect_lt(pc_cmb_gate_grad(xy1$X, xy1$y, conv$w[[1L]], conv$a[1L]), 1e-6)
})

## ---- determinismo in-process + dente ---------------------------------------------------------------------------------------
fx <- pc_cmb_cli_fixture(dir = withr::local_tempdir())
ctx_fx <- c(pc_ctx_deck(fx$data), list(tz = pc_cmb_tz, root = pc_root(), load_label = "start",
                                       weather_issue_cutoff = "available_until"))
run_fx <- function() suppressWarnings(env$prevcarga_combina_full(env$prevcarga_combina_load(fx$root, "SECO", ctx_fx, fx$members), ctx_fx))
test_that("M4.V - determinismo: max|delta y_pred| < 1e-12 in-process; dente com parquet alterado > 1e-5", {
  a <- run_fx(); b <- run_fx()
  expect_gt(nrow(a), 0L)
  expect_identical(nrow(a), nrow(b))
  expect_lt(max(abs(a$y_pred - b$y_pred)), 1e-12)
  ## membro de maior peso nas celulas de marco: o dente morde por construcao
  det <- suppressWarnings(env$prevcarga_combina_full(env$prevcarga_combina_load(fx$root, "SECO", ctx_fx, fx$members),
                                                    ctx_fx, detail = TRUE))
  wm <- det$weights[det$weights$ym == "2024-03" & is.na(det$weights$fallback)]
  mean_w <- rowMeans(sapply(wm$w, function(w) w[paste0("f_", fx$members$sigla[-1L])]))
  star <- sub("^f_", "", names(which.max(mean_w)))
  run <- pc_cmb_run_dir(fx$root, star, fx$members)
  run <- file.path(dirname(run), grep("2024-03-09", list.files(dirname(run)), value = TRUE))
  lp <- file.path(run, "long.parquet")
  l <- as.data.frame(arrow::read_parquet(lp))
  l$y_pred <- l$y_pred + 0.05
  arrow::write_parquet(l, lp)
  d <- run_fx()
  expect_identical(nrow(d), nrow(a))
  expect_gt(max(abs(d$y_pred - a$y_pred)), 1e-5)
})

## ---- sentinela de alinhamento ------------------------------------------------------------------------------------------------
test_that("M4.V - sentinela: y_pred casa o sentinela do interval_end, faixa por lead, deslocado recusado", {
  dir <- withr::local_tempdir()
  root <- pc_cmb_root_aligned8(members, dir = file.path(dir, "root"), value = "sentinel")
  ctx <- pc_cmb_ctx(file.path(dir, "deck"))
  panel <- env$prevcarga_combina_load(root, "SECO", ctx, members)
  d <- suppressWarnings(env$prevcarga_combina_full(panel, ctx, detail = TRUE))
  out <- d$long
  expect_gt(nrow(out), 0L)
  expect_lt(max(abs(out$y_pred - pc_cmb_sentinel(out$target_datetime))), 1e-9)
  expect_lt(max(abs(out$y_true - pc_cmb_sentinel(out$target_datetime))), 1e-9)       # a verdade fresca e o sentinela do deck
  o <- lubridate::ymd("2024-03-01", tz = pc_cmb_tz)
  expect_identical(min(out$target_datetime), o + lubridate::hours(9) + lubridate::minutes(30))   # available_until + 30
  expect_identical(max(out$target_datetime), o + lubridate::days(9))                                # fim da maior faixa
  mbl <- d$members_by_lead
  expect_true(all(mbl$n_members[pc_cmb_lead_day(mbl$lead) <= 1L] == 8L))
  expect_true(all(mbl$n_members[pc_cmb_lead_day(mbl$lead) >= 2L] == 7L))
  expect_false(any(vapply(mbl$members[pc_cmb_lead_day(mbl$lead) >= 2L], function(m) short_m %in% m, NA)))
  ## CONTROL: membro deslocado +30 min e recusado com o campo `deslocamento` (sem a recusa o sentinela divergiria)
  pc_cmb_misalign(root, members$sigla[[3L]], "deslocamento", members)
  e <- fail(env$prevcarga_combina_load(root, "SECO", ctx, members))
  expect_false(is.null(e))
  expect_identical(e$id, "contract.member_misaligned")
  expect_match(e$where, "deslocamento", fixed = TRUE)
})

## ---- qualidade (so nos fixtures desenhados; ADR-0025: sem epsilon) ------------------------------------------------------------
test_that("M4.V - qualidade: combina <= melhor membro e <= EQW no fixture complementar; degenerada reprova por propriedade", {
  expect_false("eps" %in% names(formals(env$prevcarga_combina_gate)))
  qp <- pc_cmb_quality_panel()
  g <- env$prevcarga_combina_gate(qp, NULL)
  q <- g$quality
  expect_gt(nrow(q), 0L)
  for (i in seq_len(nrow(q))) {                                   # comparacao estrita, sem folga
    expect_lte(q$wmape_combina[i], q$wmape_best[i])
    expect_lte(q$wmape_combina[i], q$wmape_eqw[i])
  }
  ## recalcula os tres WMAPE por prevcarga_scorecard direto, nas MESMAS linhas (holdout fitado, finito)
  w <- suppressWarnings(env$prevcarga_combina_fit(qp))
  fk <- paste(w$lead, w$cut, w$ym, sep = "\r")[is.na(w$fallback)]
  hold <- qp[paste(qp$lead, qp$cut, qp$ym, sep = "\r") %in% fk & is.finite(qp$y_true)]
  base <- data.frame(area = hold$area, d_plus = hold$d_plus, interval_start = hold$interval_start, y_true = hold$y_true)
  sc <- function(pred) env$prevcarga_scorecard(cbind(base, y_pred = pred), by = c("area", "d_plus"))
  s_c <- sc(env$prevcarga_produce_combined(hold, w)); s_e <- sc(env$prevcarga_produce_eqw(hold))
  expect_equal(q$wmape_combina, s_c$wmape[match(q$d_plus, s_c$d_plus)], tolerance = 1e-12)
  expect_equal(q$wmape_eqw, s_e$wmape[match(q$d_plus, s_e$d_plus)], tolerance = 1e-12)
  s_b <- vapply(seq_len(nrow(q)), function(i) {
    keep <- hold$d_plus == q$d_plus[i]
    s <- env$prevcarga_scorecard(cbind(base[keep, ], y_pred = hold[[paste0("f_", q$best_member[i])]][keep]), by = c("area", "d_plus"))
    s$wmape
  }, numeric(1))
  expect_equal(q$wmape_best, s_b, tolerance = 1e-12)
  expect_true(g$pass)
  ## degenerada: w = 1 no pior membro => viola <= EQW na tabela reportada e reprova pela PROPRIEDADE
  gd <- env$prevcarga_combina_gate(qp, weights = pc_cmb_degenerate_weights(w, "D"))
  expect_false(all(gd$quality$le_eqw))
  expect_gte(gd$property$max_grad_norm, 1e-6)
  expect_false(gd$pass)
  ## o veredito nao depende da tabela: qualidade ruim com pesos otimos continua passando
  expect_true(env$prevcarga_combina_gate(pc_cmb_loses_panel())$pass)
})
