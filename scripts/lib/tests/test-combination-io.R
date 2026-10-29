## test-combination-io.R -- camada de IO da combina (M4.2): descoberta, proveniencia, painel por corte, join fresco
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader", "evaluation", "combination"))
env$prevcarga_runtime_enforce()

fn <- function(name) get(name, envir = env)
members <- env$prevcarga_members(root = pc_root())
base_ctx <- list(tz = pc_cmb_tz, root = pc_root())

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
expect_pcerr <- function(expr, id, class = "contract", where = NULL) {
  e <- fail(expr)
  expect_false(is.null(e))
  expect_identical(e$id, id)
  expect_match(conditionMessage(e), paste0("^\\[prevcarga/", class, "\\]"))
  if (!is.null(where)) expect_match(e$where, where)
  expect_false(any(as.integer(charToRaw(conditionMessage(e))) > 127L))
  invisible(e)
}
with_attrs <- function(x, from) {
  for (n in c("members_declared", "members_present", "provenance"))
    data.table::setattr(x, n, attr(from, n))
  x
}

test_that("M4.2 - ac-0 descoberta 8/8 sem acionar split/protocol, proveniencia e CONTROL do io.py", {
  root <- pc_cmb_root_infer8(members)
  spy <- function(...) stop("spy")
  old <- env$prevcarga_model_spec
  assign("prevcarga_model_spec", spy, envir = env)
  on.exit(assign("prevcarga_model_spec", old, envir = env), add = TRUE)
  out <- fn("prevcarga_combina_discover")(root, "SECO", base_ctx)
  expect_setequal(unique(out$member), members$sigla)
  expect_identical(attr(out, "members_declared"), members$sigla)
  expect_identical(attr(out, "members_present"), members$sigla)
  expect_identical(nrow(unique(out[, c("member", "run_id")])), 8L)   # 1 run_id por sigla
  expect_identical(names(out)[1:8], c("area", "forecast_origin", "target_datetime", "lead", "y_pred",
                                      "y_true", "d_plus", "granularity_min"))
  pv <- attr(out, "provenance")
  expect_identical(nrow(pv), 8L)
  expect_true(all(c("member", "run_id", "run_mode", "cut", "available_until", "load_label", "load_tz",
                    "weather_issue_cutoff", "delivery_days") %in% names(pv)))
  expect_true(all(pv$cut == "0900"))
  expect_true(is.integer(pv$delivery_days))
  expect_true(all(pv$run_mode == "infer"))
  expect_identical(pv$load_tz[[1L]], pc_cmb_tz)
  expect_identical(lubridate::hour(pv$available_until[[1L]]), 9L)
  expect_identical(pv$origin[[1L]], "2026-09-30")
  ## CONTROL: o matcher de sufixo fixo do io.py nao casa nenhum run_id (so daily|retrain_harness)
  expect_identical(sum(grepl("_(daily|retrain_harness)(_[A-Z]+)?$", pv$run_id)), 0L)
  ## agnostico ao run-mode
  for (m in c("fixed", "daily", "retrain_harness")) {
    r <- pc_cmb_root_infer8(members, mode = m)
    o <- fn("prevcarga_combina_discover")(r, "SECO", base_ctx)
    expect_setequal(unique(attr(o, "provenance")$run_mode), m)
    expect_identical(length(unique(o$member)), 8L)
  }
  r <- pc_cmb_root_infer8(members, mode = "daily")
  pv2 <- attr(fn("prevcarga_combina_discover")(r, "SECO", base_ctx), "provenance")
  expect_identical(sum(grepl("_(daily|retrain_harness)(_[A-Z]+)?$", pv2$run_id)), 0L)  # exogena antes do corte: nao casa
})

test_that("M4.2 - ac-1 run-mode duplo aborta e variantes LIN disjuntas", {
  root <- pc_cmb_root_dup_mode(members)
  e <- expect_pcerr(fn("prevcarga_combina_discover")(root, "SECO", base_ctx), "contract.combina_run_mode_dup",
                    where = "LIN_MED.*dessem-linear.*MED.*(daily|infer)")
  root2 <- pc_cmb_root_lin_variants(members)
  out <- fn("prevcarga_combina_discover")(root2, "SECO", base_ctx)
  n <- table(out$member)
  expect_identical(as.integer(n[["LIN_MED"]]), 96L)
  expect_identical(as.integer(n[["LIN_MAX"]]), 48L)
  expect_identical(as.integer(n[["LIN_MXMN"]]), 24L)
  pv <- attr(out, "provenance")
  expect_identical(pv$run_id[pv$member == "LIN_MED"], "2026-09-30_2026-10-01_infer_MED_c0900")
  expect_identical(nrow(pv), 3L)
  expect_identical(attr(out, "members_present"), c("LIN_MED", "LIN_MAX", "LIN_MXMN"))
})

test_that("M4.2 - ac-2 7 de 8 e 0 de 8", {
  root <- pc_cmb_root_7of8(members)
  out <- NULL
  expect_no_error(out <- fn("prevcarga_combina_discover")(root, "SECO", base_ctx))
  expect_identical(length(attr(out, "members_present")), 7L)
  panel <- fn("prevcarga_build_panel")(out, members)
  expect_identical(grep("^f_", names(panel), value = TRUE), paste0("f_", members$sigla))
  expect_true(all(is.na(panel$f_HW)))
  expect_false(anyNA(panel$f_RF))
  empty <- withr::local_tempdir()
  expect_pcerr(fn("prevcarga_combina_discover")(empty, "SECO", base_ctx), "contract.combina_input",
               where = "nenhum membro com run")
})

test_that("M4.2 - ac-3 painel canonico, duplicata e target divergente", {
  m3 <- members[1:3, ]
  root <- withr::local_tempdir()
  pc_cmb_write_root(root, "SECO", lapply(m3$sigla, function(s) list(sigla = s, origins = c("2026-09-30", "2026-10-01"),
                                                                    d_plus = 0:1, slots = c(0L, 20L))), m3)
  long <- fn("prevcarga_combina_discover")(root, "SECO", base_ctx, members = m3)
  expect_true(all(is.finite(long$y_true)))            # y_true embutido nao-NA no LONG
  panel <- fn("prevcarga_build_panel")(long, m3)
  expect_identical(nrow(panel), 2L * 2L * 2L)
  expect_identical(names(panel), c("area", "forecast_origin", "target_datetime", "interval_start", "lead", "d_plus",
                                   "granularity_min", "cut", "ym", "y_true", paste0("f_", m3$sigla)))
  expect_true(all(panel$cut == "0900"))
  expect_true(all(panel$ym == "2026-09" | panel$ym == "2026-10"))
  expect_identical(panel$ym, substr(panel$forecast_origin, 1, 7))
  expect_identical(attr(panel$target_datetime, "tzone"), pc_cmb_tz)
  expect_true(all(is.na(panel$y_true)))
  expect_false(anyDuplicated(panel, by = c("area", "forecast_origin", "target_datetime", "lead", "cut")) > 0)
  ## y_pred preservado por membro (formula fechada)
  ts <- panel$target_datetime - lubridate::minutes(30)
  expect_equal(panel$f_LGBM, pc_cmb_truth(ts) * 1.02, tolerance = 1e-12)
  ## duplicata
  dup <- with_attrs(data.table::rbindlist(list(long, long[1L])), long)
  expect_pcerr(fn("prevcarga_build_panel")(dup, m3), "contract.combina_input", where = "duplicada.*RF")
  ## target divergente entre membros
  dv <- data.table::copy(long)
  i <- which(dv$member == "LGBM")[1L]
  dv[i, target_datetime := target_datetime + lubridate::minutes(30)]
  dv <- with_attrs(dv, long)
  expect_pcerr(fn("prevcarga_build_panel")(dv, m3), "contract.combina_input", where = "target_datetime divergente")
  ## CONTROL: sem o interval_start do leitor unico o painel aborta (nunca o deriva)
  ni <- with_attrs(data.table::copy(long)[, interval_start := NULL], long)
  expect_pcerr(fn("prevcarga_build_panel")(ni, m3), "contract.combina_input", where = "build_panel:interval_start")
})

test_that("M4.2 - ac-4 join exato por interval_end sobre o deck sentinela", {
  dir <- pc_deck_sentinel("end", areas = "SECO")
  ctx <- c(pc_ctx_deck(dir), list(tz = pc_cmb_tz, root = pc_root()))
  m2 <- members[1:2, ]
  root <- withr::local_tempdir()
  pc_cmb_write_root(root, "SECO", lapply(m2$sigla, function(s)
    list(sigla = s, origins = "2026-10-01", delivery_days = 3L,       # alinhado (M4.2b): 1o alvo = corte + 30 min
         long = pc_cmb_aligned_long(s, "SECO", "2026-10-01", "0900", 3L, m2))), m2)
  panel <- fn("prevcarga_combina_load")(root, "SECO", ctx, members = m2)
  istart <- panel$target_datetime - lubridate::minutes(30)
  slot <- lubridate::hour(istart) * 2 + lubridate::minute(istart) / 30 + 1
  in_deck <- panel$target_datetime <= lubridate::ymd_hm("2026-10-03 00:00", tz = pc_cmb_tz)
  expect_identical(panel$y_true, ifelse(in_deck, slot + 1000, NA_real_))
  expect_true(any(!in_deck)); expect_true(any(in_deck))
  ## alvo fora da grade (+14 min) -> NA
  off <- data.table::copy(panel)
  off[, target_datetime := target_datetime + lubridate::minutes(14)]
  expect_true(all(is.na(fn("prevcarga_join_fresh_truth")(off, ctx, "SECO")$y_true)))
  ## y_true embutido nunca sobrevive ao join
  pre <- data.table::copy(panel); pre[, y_true := 9e9]
  expect_identical(fn("prevcarga_join_fresh_truth")(pre, ctx, "SECO")$y_true, panel$y_true)
  ## alteracao no deck anterior a origem e refletida so naquele intervalo
  pc_eval_perturb(dir, "SECO", from = lubridate::ymd_hm("2026-10-01 09:00", tz = pc_cmb_tz),
                  to = lubridate::ymd_hm("2026-10-01 09:30", tz = pc_cmb_tz), fn = function(x) x + 7)
  again <- fn("prevcarga_join_fresh_truth")(panel, ctx, "SECO")
  d <- which(!is.na(again$y_true) & again$y_true != panel$y_true)
  expect_length(d, 1L)                                 # o intervalo perturbado esta no painel e no deck
  expect_identical(d, which(panel$target_datetime == lubridate::ymd_hm("2026-10-01 09:30", tz = pc_cmb_tz)))
  expect_identical(again$y_true[d], panel$y_true[d] + 7)
  ## CONTROL: rotulo trocado (interval_start == target) casa o vizinho e diverge do sentinela
  act <- fn("prevcarga_read_actual")(ctx, "SECO", from = min(panel$target_datetime) - lubridate::seconds(1), to = max(panel$target_datetime))
  wrong <- act$y_true[match(as.numeric(panel$target_datetime), as.numeric(act$interval_end - 1800))]
  expect_false(isTRUE(all.equal(wrong, again$y_true)))
  ## higiene de fonte: nada de with_tz, round(epoch), leitura direta de parquet de carga
  src <- paste(readLines(file.path(pc_root(), "scripts", "lib", "combination.R")), collapse = "\n")
  expect_false(grepl("with_tz|open_dataset|read_parquet|cut_invariant|broadcast", src))
  expect_false(grepl("round\\(", src))
  expect_false(grepl("\\b(RF|LGBM|HW|RGD_M[A-Z]+|LIN_M[A-Z]+)\\b", gsub("#.*", "", src)))
})

test_that("M4.2 - ac-5 dois cortes sem replicacao e EQW abaixo de n_min", {
  m2 <- members[1:2, ]
  orgs <- format(seq(as.Date("2026-09-01"), as.Date("2026-10-05"), by = 1))
  root <- withr::local_tempdir()
  s <- m2$sigla
  mk <- function(sg, cut) list(sigla = sg, cut = cut, origins = orgs, d_plus = 0L, slots = c(0L, 10L))
  pc_cmb_write_root(root, "SECO", list(mk(s[[1]], "0900"), mk(s[[1]], "1700"), mk(s[[2]], "0900")), m2)
  long <- fn("prevcarga_combina_discover")(root, "SECO", base_ctx, members = m2)
  panel <- fn("prevcarga_build_panel")(long, m2)
  expect_setequal(unique(panel$cut), c("0900", "1700"))
  p17 <- panel[panel$cut == "1700"]
  expect_true(all(is.na(p17[[paste0("f_", s[[2]])]])))
  expect_false(anyNA(p17[[paste0("f_", s[[1]])]]))
  expect_false(anyNA(panel[panel$cut == "0900"][[paste0("f_", s[[2]])]]))
  panel[, y_true := pc_cmb_truth(target_datetime - lubridate::minutes(30))]
  wt <- fn("prevcarga_combina_fit")(panel, n_min = 24L)
  c17 <- wt[wt$cut == "1700" & wt$ym == "2026-10", ]
  expect_true(nrow(c17) > 0L)
  for (k in seq_len(nrow(c17))) {
    expect_identical(names(c17$w[[k]]), paste0("f_", s[[1]]))
    expect_equal(sum(c17$w[[k]]), 1)
    expect_true(is.na(c17$fallback[[k]]))
  }
  w09 <- wt[wt$cut == "0900" & wt$ym == "2026-10", ]
  expect_equal(sum(w09$w[[1]]), 1)
  expect_identical(length(w09$w[[1]]), 2L)
  big <- fn("prevcarga_combina_fit")(panel, n_min = 1000L)
  expect_true(all(big$fallback %in% c("first_month", "n_min")))
  expect_identical(fn("prevcarga_produce_combined")(panel, big), fn("prevcarga_produce_eqw")(panel))
})

test_that("M4.2 - ac-6 erros de topologia, root e parquet por classe", {
  root <- pc_cmb_root_infer8(members)
  d <- fn("prevcarga_combina_discover")
  expect_pcerr(d(root, "XX", base_ctx), "topology.area_undeclared", class = "topology")
  expect_pcerr(d(root, "PESE", base_ctx), "topology.area_undeclared", class = "topology")
  miss <- file.path(withr::local_tempdir(), "nao-existe")
  expect_pcerr(d(miss, "SECO", base_ctx), "contract.combina_input", where = "nao-existe")
  ## parquet de run sem y_pred -> contract do read_long, com o caminho do run
  bad <- withr::local_tempdir()
  lg <- pc_cmb_member_long("RF", "SECO", "2026-09-30", 0L, 0:3, members)
  lg$y_pred <- NULL
  pc_cmb_write_root(bad, "SECO", list(list(sigla = "RF", origins = "2026-09-30", long = lg)), members)
  e <- expect_pcerr(d(bad, "SECO", base_ctx), "contract.long_schema")
  expect_match(e$where, "random-forest")
  ## run sem long.parquet
  nolong <- pc_cmb_root_infer8(members)
  unlink(list.files(file.path(nolong, "lgbm", "SECO"), pattern = "long.parquet", recursive = TRUE, full.names = TRUE))
  expect_pcerr(d(nolong, "SECO", base_ctx), "contract.combina_input", where = "long.parquet ausente")
})

test_that("M4.2 - panel_hash estavel e sensivel ao conteudo", {
  root <- pc_cmb_root_7of8(members)
  p <- fn("prevcarga_build_panel")(fn("prevcarga_combina_discover")(root, "SECO", base_ctx), members)
  h <- fn("prevcarga_combina_panel_hash")(p)
  expect_match(h, "^v_[0-9a-f]{16}$")
  expect_identical(h, fn("prevcarga_combina_panel_hash")(data.table::copy(p)))
  q <- data.table::copy(p); q$f_RF[1] <- q$f_RF[1] + 1
  expect_false(identical(h, fn("prevcarga_combina_panel_hash")(with_attrs(q, p))))
})
