## test-combination-align.R -- bloco member_alignment: recusa de membro desalinhado da combina (M4.2b)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader", "evaluation", "combination"))
env$prevcarga_runtime_enforce()

fn <- function(name) get(name, envir = env)
members <- env$prevcarga_members(root = pc_root())
base_ctx <- list(tz = pc_cmb_tz, root = pc_root(), load_label = "start", weather_issue_cutoff = "available_until")
short_m <- members$sigla[[1L]]

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
expect_misaligned <- function(expr, member, field) {
  e <- fail(expr)
  expect_false(is.null(e))
  expect_identical(e$id, "contract.member_misaligned")
  expect_match(conditionMessage(e), "^\\[prevcarga/contract\\]")
  expect_match(e$where, member, fixed = TRUE)
  expect_match(e$where, field, fixed = TRUE)
  expect_match(e$where, "esperado=.*obtido=")
  expect_false(any(as.integer(charToRaw(conditionMessage(e))) > 127L))
  invisible(e)
}
discover <- function(root, ctx = base_ctx) fn("prevcarga_combina_discover")(root, "SECO", ctx)
assert_al <- function(long, ctx = base_ctx) fn("prevcarga_assert_members_aligned")(long, ctx)

test_that("M4.2b - ac-0 alinhado com fins diferentes passa; delivery_days sem cauda aborta", {
  root <- pc_cmb_root_aligned8(members)
  long <- discover(root)
  ends <- long[, list(e = max(target_datetime)), by = member]
  expect_identical(format(ends$e[ends$member == short_m], "%Y-%m-%d %H:%M:%S"), "2024-03-03 00:00:00")
  expect_identical(sum(format(ends$e, "%Y-%m-%d %H:%M:%S") == "2024-03-10 00:00:00"), 7L)
  expect_identical(format(min(long$target_datetime), "%Y-%m-%d %H:%M:%S"), "2024-03-01 09:30:00")
  expect_warning(r <- assert_al(long), NA)
  expect_true(r)
  ## CONTROL: sem o interval_start do leitor unico o alinhamento aborta (nunca o deriva)
  ni <- data.table::copy(long)[, interval_start := NULL]
  for (n in c("members_declared", "provenance")) data.table::setattr(ni, n, attr(long, n))
  e <- fail(assert_al(ni))
  expect_identical(e$id, "contract.combina_input"); expect_identical(e$where, "members_aligned:interval_start")
  ## a faixa vem do run-manifest, nao do nome do modelo: dd = 9 sem a cauda aborta em delivery_days
  pc_cmb_edit_manifest(pc_cmb_run_dir(root, short_m, members), function(m) { m$delivery_days <- 9L; m })
  expect_misaligned(assert_al(discover(root)), short_m, "delivery_days")
})

test_that("M4.2b - ac-1 10 desalinhamentos abortam com membro e campo, exit 2, ASCII", {
  field <- c(load_label = "load_label", load_tz = "load_tz", weather_issue_cutoff = "weather_issue_cutoff",
             grade = "grade", deslocamento = "deslocamento", primeira_entrega = "primeira_entrega",
             delivery_days_curto = "delivery_days", delivery_days_longo = "delivery_days",
             available_until = "available_until", sem_manifest = "proveniencia")
  for (kind in names(field)) {
    for (m in c(short_m, members$sigla[[4L]])) {
      root <- pc_cmb_root_aligned8(members)
      pc_cmb_misalign(root, m, kind, members)
      e <- expect_misaligned(assert_al(discover(root)), m, field[[kind]])
      expect_identical(e$class_name, "contract")
      expect_identical(e$exit_code, 2L)
    }
  }
  ## tzone: o leitor unico recusa o LONG em outro fuso no discover; a combina recusa em memoria
  root <- pc_cmb_root_aligned8(members)
  pc_cmb_misalign(root, short_m, "tzone", members)
  e <- fail(discover(root))
  expect_identical(e$id, "contract.long_tz")
  long <- discover(pc_cmb_root_aligned8(members))
  long[, target_datetime := lubridate::with_tz(target_datetime, "UTC")]
  expect_misaligned(assert_al(long), short_m, "tzone")
  ## provenia ausente por completo
  expect_misaligned(assert_al(data.table::data.table(member = "x")), "-", "proveniencia")
  ## determinismo: duas falhas, a primeira na ordem fixa de membros e a reportada
  root <- pc_cmb_root_aligned8(members)
  pc_cmb_misalign(root, members$sigla[[6L]], "grade", members)
  pc_cmb_misalign(root, members$sigla[[2L]], "load_label", members)
  for (i in 1:2) expect_misaligned(assert_al(discover(root)), members$sigla[[2L]], "load_label")
})

test_that("M4.2b - ac-2 ctx divergente aborta antes do build_panel; ctx coerente monta o painel", {
  root <- pc_cmb_root_aligned8(members)
  called <- 0L
  old <- env$prevcarga_build_panel
  assign("prevcarga_build_panel", function(...) { called <<- called + 1L; old(...) }, envir = env)
  on.exit(assign("prevcarga_build_panel", old, envir = env), add = TRUE)
  ctx_ok <- pc_cmb_ctx(withr::local_tempdir())
  for (bad in list(list(load_label = "end"), list(weather_issue_cutoff = "origin_midnight_utc"))) {
    ctx <- utils::modifyList(ctx_ok, bad)
    e <- expect_misaligned(fn("prevcarga_combina_load")(root, "SECO", ctx), "vs ctx", names(bad))
    expect_identical(called, 0L)
  }
  panel <- fn("prevcarga_combina_load")(root, "SECO", ctx_ok)
  expect_identical(called, 1L)
  expect_identical(nrow(panel), nrow(unique(panel[, c("forecast_origin", "target_datetime")])))
  expect_identical(sort(grep("^f_", names(panel), value = TRUE)), sort(paste0("f_", members$sigla)))
})

test_that("M4.2b - ac-3 CONTROL: sem alinhamento o membro deslocado entra; pelo caminho normal e recusado", {
  ctx <- pc_cmb_ctx(withr::local_tempdir())
  victim <- members$sigla[[3L]]
  root <- pc_cmb_root_aligned8(members, value = "sentinel")
  ## membro deslocado +30 min mas com y_pred = sentinela do alvo ORIGINAL
  run <- pc_cmb_run_dir(root, victim, members)
  l <- pc_cmb_aligned_long(victim, "SECO", "2024-03-01", "0900", 9L, members, "sentinel")
  l$target_datetime <- l$target_datetime + lubridate::minutes(30)
  l$d_plus <- as.integer(lubridate::as_date(l$target_datetime - lubridate::minutes(30), tz = pc_cmb_tz) - lubridate::ymd("2024-03-01"))
  arrow::write_parquet(l, file.path(run, "long.parquet"))
  long <- discover(root, ctx)
  ## CONTROL: direto no build_panel (sem alinhamento) o defeito entra em silencio e diverge do sentinela
  # build_panel exige o mesmo target entre membros: o deslocado e unico, entao so ele tem linhas no seu proprio alvo
  panel_ctrl <- tryCatch(fn("prevcarga_build_panel")(long[long$member == victim], members), error = identity)
  expect_false(inherits(panel_ctrl, "error"))
  col <- paste0("f_", victim)
  expect_gt(max(abs(panel_ctrl[[col]] - pc_cmb_sentinel(panel_ctrl$target_datetime)), na.rm = TRUE), 0)
  ## caminho normal recusa
  expect_misaligned(fn("prevcarga_combina_load")(root, "SECO", ctx), victim, "deslocamento")
  ## sem o membro deslocado passa e o painel e o sentinela
  ok <- members[members$sigla != victim, ]
  root2 <- pc_cmb_root_aligned8(members, value = "sentinel", only = ok$sigla)
  panel <- fn("prevcarga_combina_load")(root2, "SECO", ctx, members = ok)
  for (s in ok$sigla)
    expect_lt(max(abs(panel[[paste0("f_", s)]] - pc_cmb_sentinel(panel$target_datetime)), na.rm = TRUE), 1e-9)
})

test_that("M4.2b - higiene de fonte: regra de faixa sem sigla, O+1/O+8 nem conversao de tempo proibida", {
  src <- gsub("#[^\n]*", "", paste(readLines(file.path(pc_root(), "scripts", "lib", "combination.R")), collapse = "\n"))
  expect_false(grepl("\\b(RF|LGBM|HW|RGD_M[A-Z]+|LIN_M[A-Z]+)\\b", src))
  expect_false(grepl("O\\+1|O\\+8|as\\.POSIXct|difftime|with_tz", src))
})
