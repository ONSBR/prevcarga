## test-combination-freeze.R -- M4.3: freeze/apply content-addressed da combina + split-parity + saida LONG
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                       "evaluation", "features", "registry", "combination"))
env$prevcarga_runtime_enforce()

members <- env$prevcarga_members(root = pc_root())
ctx <- pc_cmb_ctx_freeze()
panel <- pc_cmb_panel_joined(env, members)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
expect_err <- function(expr, id, exit) {
  e <- fail(expr)
  expect_false(is.null(e))
  expect_identical(e$id, id)
  expect_identical(e$exit_code, as.integer(exit))
  expect_false(any(as.integer(charToRaw(conditionMessage(e))) > 127L))
  invisible(e)
}
edit_panel <- function(p, f) {
  out <- data.table::copy(p)
  for (n in c("members_declared", "members_present", "provenance", "coverage"))
    data.table::setattr(out, n, attr(p, n))
  f(out)
  out
}
tr <- pc_cmb_trained(env, panel, members)
full <- env$prevcarga_combina_full(panel, ctx)
sha <- function(p) digest::digest(file = p, algo = "sha256")
wpath <- function(t) file.path(t$res$path, "weights.rds")

test_that("M4.3 - ac-0 round-trip saveRDS bit a bit; CONTROL JSON digits=4 nao e identico", {
  det <- env$prevcarga_combina_full(panel, ctx, detail = TRUE)
  w <- as.data.frame(det$weights)
  obj <- list(meta = list(k = 1L), weights = w)
  d <- withr::local_tempdir()
  env$prevcarga_freeze(obj, file.path(d, "o.rds"))
  expect_identical(env$prevcarga_thaw(file.path(d, "o.rds")), obj)
  flat <- unlist(det$weights$w)
  expect_true(any(abs(flat - round(flat, 4)) > 0))
  viaj <- jsonlite::fromJSON(jsonlite::toJSON(flat, digits = 4))
  expect_false(identical(viaj, flat))
  ## o artefato do train e o mesmo contrato
  th <- env$prevcarga_thaw(wpath(tr))
  expect_identical(names(th), c("meta", "weights"))
  expect_identical(env$prevcarga_thaw(wpath(tr)), th)
})

test_that("M4.3 - ac-1 split-parity apply == full bit a bit; CONTROL sem uma origem de fevereiro", {
  ap <- env$prevcarga_combina_apply("SECO", tr$dir, panel, ctx, members = members)
  expect_identical(ap$y_pred, full$y_pred)
  expect_identical(ap, full)
  expect_gt(nrow(full), 0L)
  drop_o <- edit_panel(panel, function(p) NULL)
  drop_o <- drop_o[drop_o$forecast_origin != "2024-02-15"]
  for (n in c("members_declared", "members_present", "provenance", "coverage"))
    data.table::setattr(drop_o, n, attr(panel, n))
  trc <- pc_cmb_trained(env, drop_o, members)
  apc <- env$prevcarga_combina_apply("SECO", trc$dir, panel, ctx, members = members)
  expect_identical(nrow(apc), nrow(full))
  expect_gt(max(abs(apc$y_pred - full$y_pred), na.rm = TRUE), 0)
})

test_that("M4.3 - ac-2 version_hash estavel sob trained_at, sensivel a painel e load_label", {
  a <- pc_cmb_trained(env, panel, members, trained_at = "2024-01-01T00:00:00Z")
  d <- a$dir
  s0 <- sha(wpath(a))
  b <- pc_cmb_trained(env, panel, members, trained_at = "2025-06-01T12:00:00Z", dir = d)
  expect_match(a$res$version_hash, "^v_[0-9a-f]{16}$")
  expect_identical(b$res$version_hash, a$res$version_hash)
  expect_false(a$res$reused)
  expect_true(b$res$reused)
  expect_identical(sha(wpath(a)), s0)
  expect_length(list.dirs(file.path(d, "SECO"), recursive = FALSE), 1L)
  p3 <- edit_panel(panel, function(p) data.table::set(p, i = 5L, j = "y_true", value = p$y_true[5L] + 1e-9))
  c3 <- pc_cmb_trained(env, p3, members, dir = d)
  expect_false(identical(c3$res$version_hash, a$res$version_hash))
  expect_identical(readLines(file.path(d, "SECO", "latest.txt")), c3$res$version_hash)
  p4 <- pc_cmb_prov_conv(edit_panel(panel, function(p) NULL), label = "end")
  c4 <- pc_cmb_trained(env, p4, members, dir = d)
  expect_false(identical(c4$res$version_hash, a$res$version_hash))
  expect_false(identical(c4$res$version_hash, c3$res$version_hash))
  expect_length(list.dirs(file.path(d, "SECO"), recursive = FALSE), 3L)
  expect_identical(readLines(file.path(d, "SECO", "latest.txt")), c4$res$version_hash)
})

test_that("M4.3 - ac-3 meta do M1.3: member_set_hash, convencao, members_by_lead, sem cutoff", {
  meta <- env$prevcarga_thaw(wpath(tr))$meta
  expect_identical(meta$member_set_hash, env$prevcarga_member_set_hash(members))
  expect_match(meta$member_set_hash, "^[0-9a-f]{64}$")
  expect_match(meta$data_hash, "^[0-9a-f]{64}$")
  expect_identical(meta$data_hash, env$prevcarga_combina_panel_hash(panel))
  for (k in c("load_label", "load_tz", "weather_issue_cutoff", "members_by_lead", "model_versions", "area",
              "version_hash", "trained_at", "code_commit", "platform"))
    expect_true(k %in% names(meta), info = k)
  expect_identical(meta$model_versions, structure(list(), names = character()))
  expect_false("cutoff" %in% names(meta))
  expect_identical(meta$version_hash, tr$res$version_hash)
  red <- pc_cmb_members_without_last(members)
  tr2 <- pc_cmb_trained(env, panel, red)
  m2 <- env$prevcarga_thaw(wpath(tr2))$meta
  expect_identical(m2$member_set_hash, env$prevcarga_member_set_hash(red))
  expect_false(identical(m2$member_set_hash, meta$member_set_hash))
  pn <- edit_panel(panel, function(p) data.table::set(p, j = paste0("f_", members$sigla[[nrow(members)]]),
                                                      value = NA_real_))
  trn <- pc_cmb_trained(env, pn, members)
  mn <- env$prevcarga_thaw(wpath(trn))$meta
  expect_identical(mn$member_set_hash, meta$member_set_hash)
  expect_false(identical(mn$data_hash, meta$data_hash))
})

test_that("M4.3 - ac-4 LONG: nomes, ordem, classes, fuso; CONTROLs", {
  ap <- env$prevcarga_combina_apply("SECO", tr$dir, panel, ctx, members = members)
  for (out in list(ap, full)) {
    expect_identical(names(out), names(env$prevcarga_schema_long))
    expect_identical(unname(vapply(out, function(x) class(x)[[1L]], "")),
                     c("character", "character", "POSIXct", "integer", "numeric", "numeric", "integer", "integer"))
    expect_identical(attr(out$target_datetime, "tzone"), ctx$tz)
    expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", out$forecast_origin)))
    expect_no_error(env$prevcarga_assert_long(out, ctx))
    expect_false(is.unsorted(order(out$forecast_origin, out$lead)))
  }
  ## target_datetime = interval_end do painel
  k <- match(paste(full$forecast_origin, full$lead), paste(panel$forecast_origin, panel$lead))
  expect_identical(as.numeric(full$target_datetime), as.numeric(panel$target_datetime[k]))
  ## CONTROL: lead double / target_datetime em UTC falham a mesma asserção
  bad1 <- data.table::copy(full); bad1[, lead := as.numeric(lead)]
  expect_false(identical(vapply(bad1, function(x) class(x)[[1L]], "")[["lead"]], "integer"))   # assert_long coage inteiro-valido (M3.1)
  bad2 <- data.table::copy(full); bad2[, target_datetime := lubridate::with_tz(target_datetime, "UTC")]
  expect_error(env$prevcarga_assert_long(bad2, ctx))
})

test_that("M4.3 - ac-5 apply: state 13, integrity 9, contract 2; nunca LONG vazio", {
  empty <- withr::local_tempdir()
  e <- expect_err(env$prevcarga_combina_apply("SECO", empty, panel, ctx, members = members),
                  "state.combina_not_trained", 13)
  expect_match(e$where, "SECO", fixed = TRUE); expect_match(e$where, empty, fixed = TRUE)
  ## latest.txt aponta para versao inexistente
  d1 <- withr::local_tempdir(); dir.create(file.path(d1, "SECO"))
  writeLines("v_0123456789abcdef", file.path(d1, "SECO", "latest.txt"))
  expect_err(env$prevcarga_combina_apply("SECO", d1, panel, ctx, members = members),
             "integrity.freeze_file_missing", 9)
  ## byte corrompido (sha conferido antes do readRDS)
  t2 <- pc_cmb_trained(env, panel, members)
  pc_cmb_corrupt_byte(wpath(t2))
  expect_err(env$prevcarga_combina_apply("SECO", t2$dir, panel, ctx, members = members),
             "integrity.freeze_sha_mismatch", 9)
  ## outro conjunto de membros e outra convencao
  t3 <- pc_cmb_trained(env, panel, pc_cmb_members_without_last(members))
  expect_err(env$prevcarga_combina_apply("SECO", t3$dir, panel, ctx, members = members),
             "contract.combina_artifact_mismatch", 2)
  p_end <- pc_cmb_prov_conv(edit_panel(panel, function(p) NULL), label = "end")
  t4 <- pc_cmb_trained(env, p_end, members)
  expect_err(env$prevcarga_combina_apply("SECO", t4$dir, panel, ctx, members = members),
             "contract.combina_artifact_mismatch", 2)
  ## latest.txt invalido
  writeLines("lixo", file.path(d1, "SECO", "latest.txt"))
  expect_err(env$prevcarga_combina_apply("SECO", d1, panel, ctx, members = members),
             "contract.combina_input", 2)
})

test_that("M4.3 - ac-6 selecao de corte", {
  p2 <- pc_cmb_panel_two_cuts(env, members)
  expect_err(env$prevcarga_combina_full(p2, ctx), "contract.combina_input", 2) |>
    (\(e) expect_match(e$where, "corte ambiguo", fixed = TRUE))()
  t2 <- pc_cmb_trained(env, p2, members)
  e <- expect_err(env$prevcarga_combina_apply("SECO", t2$dir, p2, ctx, members = members),
                  "contract.combina_input", 2)
  expect_match(e$where, "corte ambiguo", fixed = TRUE)
  f9 <- env$prevcarga_combina_full(p2, ctx, t_cut = "0900")
  fc <- env$prevcarga_combina_full(p2, ctx, t_cut = "c0900")
  a9 <- env$prevcarga_combina_apply("SECO", t2$dir, p2, ctx, t_cut = "0900", members = members)
  ac <- env$prevcarga_combina_apply("SECO", t2$dir, p2, ctx, t_cut = "c0900", members = members)
  expect_identical(f9, fc)
  expect_identical(a9, ac)
  expect_identical(a9$y_pred, f9$y_pred)
  expect_identical(a9, f9)
  rows9 <- p2[p2$cut == "0900"]
  expect_lte(nrow(f9), nrow(rows9))
  expect_gt(nrow(f9), 0L)
  e2 <- expect_err(env$prevcarga_combina_full(p2, ctx, t_cut = "1200"), "contract.combina_input", 2)
  expect_match(e2$where, "0900", fixed = TRUE); expect_match(e2$where, "1700", fixed = TRUE)
})

test_that("M4.3 - ac-7 layout do registry/bucket: latest.txt, auto-contido, copia identica, CONTROL .tmp orfao", {
  d <- tr$dir
  vh <- tr$res$version_hash
  expect_identical(readLines(file.path(d, "SECO", "latest.txt")), vh)
  expect_length(list.files(file.path(d, "SECO"), pattern = "^[.]tmp-", all.files = TRUE), 0L)
  expect_length(list.files(file.path(d, "SECO"), pattern = "staging", all.files = TRUE), 0L)
  expect_setequal(list.files(file.path(d, "SECO", vh), all.files = TRUE, no.. = TRUE),
                  c("weights.rds", "weights.rds.manifest.json"))
  mf <- jsonlite::read_json(file.path(d, "SECO", vh, "weights.rds.manifest.json"))
  expect_identical(mf$sha256, sha(file.path(d, "SECO", vh, "weights.rds")))
  bucket <- pc_cmb_copy_as_bucket(d, "SECO")
  expect_true(file.exists(file.path(bucket, "SECO", "latest.txt")))
  a1 <- env$prevcarga_combina_apply("SECO", d, panel, ctx, members = members)
  a2 <- env$prevcarga_combina_apply("SECO", bucket, panel, ctx, members = members)
  expect_identical(a1, a2)
  pc_cmb_corrupt_byte(file.path(bucket, "SECO", vh, "weights.rds"))
  expect_err(env$prevcarga_combina_apply("SECO", bucket, panel, ctx, members = members),
             "integrity.freeze_sha_mismatch", 9)
  ## CONTROL: treino interrompido antes do rename deixa um .tmp-*; apply e latest.txt nao o veem
  d2 <- pc_cmb_trained(env, panel, members)$dir
  before <- env$prevcarga_combina_apply("SECO", d2, panel, ctx, members = members)
  fenv <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                          "evaluation", "features", "registry", "combination"))
  fenv$prevcarga_runtime_enforce()
  real <- fenv$prevcarga_freeze
  fenv$prevcarga_freeze <- function(obj, path) { real(obj, path); stop("falha injetada") }
  p5 <- edit_panel(panel, function(p) data.table::set(p, i = 7L, j = "y_true", value = p$y_true[7L] + 1e-6))
  expect_error(fenv$prevcarga_combina_train("SECO", p5, d2, root = pc_root(), members = members), "falha injetada")
  expect_gt(length(list.files(file.path(d2, "SECO"), pattern = "^[.]tmp-", all.files = TRUE)), 0L)
  expect_identical(readLines(file.path(d2, "SECO", "latest.txt")), before_vh <- readLines(file.path(d2, "SECO", "latest.txt")))
  expect_length(list.files(file.path(d2, "SECO"), pattern = "^v_"), 1L)
  expect_identical(env$prevcarga_combina_apply("SECO", d2, panel, ctx, members = members), before)
})
