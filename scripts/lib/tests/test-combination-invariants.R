## test-combination-invariants.R -- M4.V: os 7 invariantes da combina por formula fechada (um test_that por invariante).
## Reusa os fixtures de M4.1..M4.4 sem editar seus helpers. Todo delta e sobre y_pred, nunca sobre w.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                       "evaluation", "features", "registry", "combination"))
env$prevcarga_runtime_enforce()

members <- env$prevcarga_members(root = pc_root())
short_m <- members$sigla[[1L]]
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)

## ---- freeze_parity -------------------------------------------------------------------------------------------------------
test_that("M4.V - invariante freeze_parity: apply(train()) == full, bit a bit; CONTROL com outro painel", {
  ctx <- pc_cmb_ctx_freeze()
  panel <- pc_cmb_panel_joined(env, members, origins = pc_cmb_lead_origins("2024-01-01", "2024-02-25"))
  tr <- pc_cmb_trained(env, panel, members)
  ap <- env$prevcarga_combina_apply("SECO", tr$dir, panel, ctx, members = members)
  fu <- env$prevcarga_combina_full(panel, ctx)
  expect_gt(nrow(fu), 0L)
  expect_identical(ap, fu)
  expect_identical(ap$y_pred, fu$y_pred)
  ## CONTROL: pesos treinados com y_true alterado nao reproduzem o full original
  alt <- data.table::copy(panel)
  for (n in c("members_declared", "members_present", "provenance", "coverage")) data.table::setattr(alt, n, attr(panel, n))
  alt[, y_true := y_true * 1.1 + 0.2]
  tr2 <- pc_cmb_trained(env, alt, members)
  ap2 <- env$prevcarga_combina_apply("SECO", tr2$dir, panel, ctx, members = members)
  expect_gt(max(abs(ap2$y_pred - fu$y_pred)), 1e-6)
})

## ---- member_discovery ----------------------------------------------------------------------------------------------------
test_that("M4.V - invariante member_discovery: 8/8 runs por identidade, sem run.yaml; CONTROL 7/8", {
  ctx <- list(tz = pc_cmb_tz, root = pc_root())
  root <- pc_cmb_root_infer8(members, dir = file.path(withr::local_tempdir(), "root"))
  expect_identical(list.files(root, pattern = "run\\.yaml", recursive = TRUE), character(0))   # nada de run.yaml no disco
  long <- env$prevcarga_combina_discover(root, "SECO", ctx, members)
  expect_identical(attr(long, "members_present"), members$sigla)
  expect_identical(length(unique(paste(long$member, long$run_id))), 8L)
  expect_setequal(unique(long$member), members$sigla)
  src <- sub("\\s*##.*$", "", deparse(body(env$prevcarga_combina_discover)))
  expect_false(any(grepl("run\\.yaml|\\$split|\\$protocol", src)))
  r7 <- pc_cmb_root_7of8(members, dir = file.path(withr::local_tempdir(), "root"))
  l7 <- env$prevcarga_combina_discover(r7, "SECO", ctx, members)
  expect_identical(length(attr(l7, "members_present")), 7L)
  expect_false("HW" %in% attr(l7, "members_present"))
})

## ---- ytrue_join ----------------------------------------------------------------------------------------------------------
test_that("M4.V - invariante ytrue_join: y_true == deck por interval_end; deck alterado reflete na 2a execucao", {
  dir <- withr::local_tempdir()
  root <- pc_cmb_root_aligned8(members, dir = file.path(dir, "root"), value = "sentinel")
  ctx <- pc_cmb_ctx(file.path(dir, "deck"))
  p1 <- env$prevcarga_combina_load(root, "SECO", ctx, members)
  expect_gt(nrow(p1), 0L)
  expect_identical(p1$y_true, pc_cmb_sentinel(p1$target_datetime))          # igualdade exata por interval_end == target_datetime
  expect_false(any(abs(p1$y_true - (p1$f_LGBM + 500)) < 1, na.rm = TRUE))                    # o y_true do LONG do membro (+500) e descartado
  pc_mut_load(file.path(dir, "deck"), function(dt) { dt[, load_mwh := load_mwh + 7]; dt })
  p2 <- env$prevcarga_combina_load(root, "SECO", ctx, members)
  expect_identical(p2$y_true, p1$y_true + 7)                                 # leitura fresca a cada execucao
})

## ---- panel_append --------------------------------------------------------------------------------------------------------
test_that("M4.V - invariante panel_append: dois roots empilhados == root unico; CONTROL sem uma origem de fevereiro", {
  ctx <- c(pc_ctx_deck(NULL), list(tz = pc_cmb_tz, root = pc_root(), load_label = "start", weather_issue_cutoff = "available_until"))
  orig <- pc_cmb_cli_origins()
  one <- pc_cmb_cli_fixture(orig, dir = withr::local_tempdir())
  ctx$data_dir <- one$data
  jan <- orig[substr(orig, 1L, 7L) == "2024-01"]
  a <- pc_cmb_cli_fixture(jan, dir = withr::local_tempdir())
  b <- pc_cmb_cli_fixture(setdiff(orig, jan), dir = withr::local_tempdir())
  stacked <- file.path(withr::local_tempdir(), "root")
  dir.create(stacked)
  for (r in c(a$root, b$root)) file.copy(list.files(r, full.names = TRUE), stacked, recursive = TRUE)
  run <- function(root) suppressWarnings(env$prevcarga_combina_full(env$prevcarga_combina_load(root, "SECO", ctx, one$members), ctx))
  f1 <- run(one$root); f2 <- run(stacked)
  expect_gt(nrow(f1), 0L)
  expect_identical(f2$y_pred, f1$y_pred)
  ## CONTROL: tirar a origem 2024-02-27 do empilhado muda o y_pred das origens que sobram (os pesos de marco)
  unlink(Sys.glob(file.path(stacked, "*", "SECO", "2024-02-27_*")), recursive = TRUE)
  f3 <- run(stacked)
  key <- function(d) paste(d$forecast_origin, d$lead)
  k <- intersect(key(f1), key(f3))
  expect_gt(length(k), 0L)
  expect_gt(max(abs(f1$y_pred[match(k, key(f1))] - f3$y_pred[match(k, key(f3))])), 0)
})

## ---- refit_per_cut -------------------------------------------------------------------------------------------------------
test_that("M4.V - invariante refit_per_cut: corte 1700 com n < n_min => EQW daquele corte, != pesos do 0900", {
  panel <- pc_cmb_panel_two_cuts(env, members, origins = pc_cmb_lead_origins("2024-01-01", "2024-02-20"))
  panel <- panel[!(panel$cut == "1700" & panel$forecast_origin > "2024-01-10" & panel$ym == "2024-01")]   # 1700: n = 10 < n_min em fev
  ## o corte 1700 so tem 10 origens de janeiro e as de fevereiro; fev pesa em n_min
  wt <- suppressWarnings(env$prevcarga_combina_fit(panel))
  y <- env$prevcarga_produce_combined(panel, wt)
  w17 <- wt[wt$cut == "1700" & wt$ym == "2024-02", ]
  w09 <- wt[wt$cut == "0900" & wt$ym == "2024-02" & is.na(wt$fallback), ]       # celulas de 0900 realmente fitadas
  expect_gt(nrow(w17), 0L)
  expect_gt(nrow(w09), 0L)
  expect_true(all(w17$fallback == "n_min"))
  rows <- which(panel$cut == "1700" & panel$ym == "2024-02")
  eqw <- env$prevcarga_produce_eqw(panel[rows])
  expect_equal(y[rows], eqw, tolerance = 1e-12)                                # EQW daquele corte
  ## CONTROL: aplicar os pesos do corte 0900 (mesmo lead) a essas linhas da resultado DIFERENTE (nao ha interp entre cortes)
  w_alt <- data.table::copy(w09)
  w_alt[, cut := "1700"]
  sel <- rows[panel$lead[rows] %in% w_alt$lead]
  expect_gt(length(sel), 0L)
  y_alt <- env$prevcarga_produce_combined(panel[sel], w_alt)
  expect_gt(max(abs(y_alt - y[sel])), 1e-4)
})

## ---- member_alignment ----------------------------------------------------------------------------------------------------
test_that("M4.V - invariante member_alignment: membro deslocado 30 min recusado contract.member_misaligned", {
  dir <- withr::local_tempdir()
  root <- pc_cmb_root_aligned8(members, dir = file.path(dir, "root"))
  ctx <- pc_cmb_ctx(file.path(dir, "deck"))
  expect_false(is.null(env$prevcarga_combina_load(root, "SECO", ctx, members)))      # alinhado passa
  pc_cmb_misalign(root, members$sigla[[2L]], "deslocamento", members)
  e <- fail(env$prevcarga_combina_load(root, "SECO", ctx, members))
  expect_identical(e$id, "contract.member_misaligned")
  expect_identical(e$exit_code, 2L)
  expect_match(e$where, paste0(members$sigla[[2L]], ":deslocamento"), fixed = TRUE)
})

## ---- lead_coverage -------------------------------------------------------------------------------------------------------
test_that("M4.V - invariante lead_coverage: curto fora de O+2..O+8 e soma 1 dos 7", {
  panel <- pc_cmb_panel_joined(env, members, origins = pc_cmb_lead_origins("2024-01-01", "2024-02-25"))
  w <- suppressWarnings(env$prevcarga_combina_fit(panel))
  day <- pc_cmb_lead_day(w$lead)
  nm <- lapply(w$w, function(x) sub("^f_", "", names(x)))
  tail_ <- day >= 2L
  expect_true(any(tail_))
  expect_true(all(vapply(nm[tail_], function(x) length(x) == 7L && !short_m %in% x, NA)))
  expect_true(all(vapply(nm[!tail_], function(x) length(x) == 8L, NA)))
  expect_true(all(abs(vapply(w$w[tail_], sum, 0) - 1) < 1e-9))
  ## FORBIDDEN: cauda do curto PRESENTE no LONG mas fora da faixa declarada (dd = 2) fica mascarada (ausente, nunca 0)
  o5 <- pc_cmb_lead_origins("2024-01-01", "2024-01-05")
  long_t <- pc_cmb_fill_tail(pc_cmb_long8_lead(members, origins = o5), short_m, members, origins = o5)
  pt <- env$prevcarga_build_panel(long_t)
  expect_true(all(is.na(pt[[paste0("f_", short_m)]][pc_cmb_lead_day(pt$lead) >= 2L])))
  ## CONTROL: com delivery_days = 9 a mesma cauda entra (sem a mascara por faixa o teste nao morderia)
  p9 <- env$prevcarga_build_panel(pc_cmb_with_prov(long_t, pc_cmb_set_days(attr(long_t, "provenance"), short_m, 9L)))
  expect_true(any(!is.na(p9[[paste0("f_", short_m)]][pc_cmb_lead_day(p9$lead) >= 2L])))
})
