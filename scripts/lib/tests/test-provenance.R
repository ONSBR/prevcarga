## test-provenance.R -- provenance.R (M1.3): freeze/thaw, version_hash (8 chaves), run-manifest, code_commit.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
local({
  e <- pc_source_lib(c("errors", "runtime", "topology", "provenance"))
  for (n in ls(e, all.names = TRUE)) assign(n, get(n, e), envir = globalenv())
})
suppressMessages(prevcarga_runtime_enforce())

expect_pc_error <- function(expr, id, class = "contract", where = NULL) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$class_name, class)
  if (!is.null(where)) expect_match(cond$where, where, fixed = TRUE)
}
new_dir <- function() { d <- tempfile("prov"); dir.create(d); d }

long_panel <- function() {
  t0 <- as.POSIXct("2024-06-01 00:30:00", tz = "UTC")
  tt <- t0 + 1800 * (0:95)
  data.table::data.table(
    area = rep("SECO", 96L), forecast_origin = rep("2024-06-01", 96L), target_datetime = tt,
    lead = rep(1L, 96L), y_pred = seq(0.1, 9.6, length.out = 96L) / 3,
    y_true = c(rep(NA_real_, 10L), sqrt(2) * (11:96)), d_plus = rep(0L, 96L),
    granularity_min = rep(30L, 96L))
}
## data.table guarda um ponteiro externo (.internal.selfref) que muda a cada readRDS; fora isso, identical.
no_selfref <- function(x) { if (is.data.frame(x)) attr(x, ".internal.selfref") <- NULL; x }
fingerprint <- function() list(
  model_id = "dessem-linear", data_fingerprint = strrep("a", 64), cutoff = as.Date("2024-05-22"),
  load_label = "start", load_tz = "Etc/GMT+3",
  feature_config = list(exogena = "MED", params = list(alpha = 0.5, lags = c(1L, 2L), nested = list(b = 1, a = 2))),
  code_commit = strrep("b", 40), renv_lock_hash = NULL)

test_that("[FORBIDDEN] freeze/thaw round-trip identical + manifest + sem temporarios", {
  d <- new_dir(); on.exit(unlink(d, recursive = TRUE))
  for (x in list(long_panel(), list(a = 1/3, b = list(c = "x", d = c(pi, exp(1))), e = NULL))) {
    p <- file.path(d, "obj.rds")
    sha <- prevcarga_freeze(x, p)
    expect_identical(no_selfref(prevcarga_thaw(p)), no_selfref(x))
    mf <- jsonlite::read_json(paste0(p, ".manifest.json"), simplifyVector = TRUE)
    expect_match(mf$sha256, "^[0-9a-f]{64}$")
    expect_identical(mf$sha256, digest::digest(file = p, algo = "sha256"))
    expect_identical(mf$sha256, sha)
    expect_identical(mf$format_version, 1L)
    expect_true(!is.null(mf$class))
    expect_true(!is.null(mf$nrows) || !is.null(mf$length))
    expect_length(list.files(d, all.files = TRUE, pattern = "^\\.freeze-"), 0L)
  }
  expect_identical(attr(prevcarga_thaw(file.path(d, "obj.rds"))$a, "tzone"), NULL)
  px <- file.path(d, "panel.rds"); prevcarga_freeze(long_panel(), px)
  expect_identical(attr(prevcarga_thaw(px)$target_datetime, "tzone"), "UTC")
  expect_pc_error(prevcarga_freeze(1, file.path(d, "nao", "x.rds")), "contract.freeze_dir_missing")
  expect_false(dir.exists(file.path(d, "nao")))
})

test_that("[CONTROL] integrity: byte alterado e manifest ausente; readRDS nao alcancado", {
  d <- new_dir(); on.exit(unlink(d, recursive = TRUE))
  p1 <- file.path(d, "a.rds"); p2 <- file.path(d, "b.rds")
  prevcarga_freeze(long_panel(), p1); prevcarga_freeze(long_panel(), p2)
  b <- readBin(p1, "raw", file.size(p1)); i <- length(b) %/% 2L
  b[[i]] <- as.raw(bitwXor(as.integer(b[[i]]), 0xFFL)); writeBin(b, p1)
  unlink(paste0(p2, ".manifest.json"))
  calls <- 0L
  suppressMessages(trace(readRDS, quote(calls <<- calls + 1L), print = FALSE, where = baseenv()))
  on.exit(suppressMessages(untrace(readRDS, where = baseenv())), add = TRUE)
  expect_pc_error(prevcarga_thaw(p1), "integrity.freeze_sha_mismatch", class = "integrity", where = p1)
  expect_pc_error(prevcarga_thaw(p2), "integrity.freeze_manifest_missing", class = "integrity")
  e <- tryCatch(prevcarga_thaw(p1), prevcarga_error = identity)
  expect_identical(e$exit_code, 9L)
  expect_identical(calls, 0L)
})

test_that("version_hash: 7 bracos (identidade, CONTROL cutoff/label/tz, permutacao, lateral, ausente)", {
  f <- fingerprint(); h <- prevcarga_version_hash(f)
  expect_match(h, "^v_[0-9a-f]{16}$")
  expect_identical(prevcarga_version_hash(fingerprint()), h)
  f2 <- f; f2$cutoff <- f$cutoff + 1; expect_false(identical(prevcarga_version_hash(f2), h))
  f3 <- f; f3$load_label <- "end"; expect_false(identical(prevcarga_version_hash(f3), h))
  f4 <- f; f4$load_tz <- "UTC"; expect_false(identical(prevcarga_version_hash(f4), h))
  f5 <- f[rev(names(f))]
  f5$feature_config$params <- f$feature_config$params[c("nested", "lags", "alpha")]
  f5$feature_config$params$nested <- f$feature_config$params$nested[c("b", "a")]
  f5$feature_config <- f5$feature_config[c("params", "exogena")]
  expect_identical(prevcarga_version_hash(f5), h)
  f6 <- f; f6$trained_at <- Sys.time()
  expect_pc_error(prevcarga_version_hash(f6), "contract.fingerprint_lateral", where = "trained_at")
  f7 <- f; f7$load_tz <- NULL
  expect_pc_error(prevcarga_version_hash(f7), "contract.fingerprint_missing_key", where = "load_tz")
  f8 <- f; f8$zzz <- 1
  expect_pc_error(prevcarga_version_hash(f8), "contract.fingerprint_extra_key", where = "zzz")
  f9 <- f; f9$feature_config <- list(1, a = 2)
  expect_pc_error(prevcarga_version_hash(f9), "contract.fingerprint_partially_named")
})

test_that("membros DESSEM: exogenas distintas e RGD_MED != LIN_MED", {
  h <- function(model_id, exo) { f <- fingerprint(); f$model_id <- model_id
    f$feature_config <- list(exogena = exo, params = list(alpha = 0.5)); prevcarga_version_hash(f) }
  ex <- c(h("dessem-linear", "MED"), h("dessem-linear", "MAX"), h("dessem-linear", "MXMN"))
  expect_false(anyDuplicated(ex) > 0)
  expect_false(identical(h("dessem-regdin", "MED"), h("dessem-linear", "MED")))
})

valid_args <- function(...) {
  a <- list(data_hash = strrep("c", 64), origin = as.Date("2024-06-01"), available_until = NULL,
    load_label = "start", load_tz = "Etc/GMT+3", weather_issue_cutoff = "available_until",
    split = "origin_agnostic", delivery_days = 2L, model_versions = list(LGBM = "v_0123456789abcdef"),
    seeds = list(LGBM = 123L), bundle_id = strrep("d", 64), root = pc_root(),
    trained_at = as.POSIXct("2024-06-01 12:00:00", tz = "UTC"), run_id = "run-1")
  mod <- list(...)
  for (n in names(mod)) a[n] <- list(mod[[n]])
  a
}
manifest <- function(...) do.call(prevcarga_run_manifest, valid_args(...))

test_that("run-manifest: chaves, sem cutoff, escrita atomica e releitura", {
  m <- manifest()
  want <- c("data_hash", "origin", "available_until", "load_label", "load_tz", "weather_issue_cutoff",
            "split", "delivery_days", "member_set_hash", "model_versions", "seeds", "code_commit",
            "renv_lock_hash", "platform", "blas_threads", "tz", "lc", "bundle_id",
            "trained_at", "package_version", "run_id")
  expect_true(all(want %in% names(m)))
  expect_false("cutoff" %in% names(m))
  expect_identical(m$origin, "2024-06-01")
  expect_null(m$available_until)
  expect_identical(m$delivery_days, 2L)
  expect_identical(m$blas_threads, 1L); expect_identical(m$tz, "UTC")
  expect_identical(m$lc, prevcarga_runtime_stamp()$lc)
  expect_identical(m$member_set_hash, prevcarga_member_set_hash(prevcarga_members(root = pc_root())))
  expect_match(m$member_set_hash, "^[0-9a-f]{64}$")
  d <- new_dir(); on.exit(unlink(d, recursive = TRUE))
  dest <- prevcarga_write_run_manifest(m, d)
  expect_identical(basename(dest), "run_manifest.json")
  expect_identical(list.files(d, all.files = TRUE, no.. = TRUE), "run_manifest.json")
  r <- prevcarga_read_run_manifest(d)
  expect_true(all(want %in% names(r)))
  expect_identical(r$delivery_days, 2L); expect_identical(r$load_tz, "Etc/GMT+3")
  expect_identical(r$seeds$LGBM, 123L)
  expect_pc_error(prevcarga_write_run_manifest(m, file.path(d, "nao")), "contract.freeze_dir_missing")
  m2 <- manifest(available_until = as.POSIXct("2024-06-01 10:00:00", tz = "UTC"), delivery_days = list(RF = 9L))
  expect_identical(m2$available_until, "2024-06-01T10:00:00+0000")
})

test_that("run-manifest: invalidos nomeiam a chave; member_set reduzido difere", {
  bad <- function(key, ...) expect_pc_error(manifest(...), "contract.run_manifest_bad_value", where = key)
  bad("bundle_id", bundle_id = "/opt/bundle/x")
  bad("model_versions", model_versions = list(RF = "v_20240601_120000"))
  bad("origin", origin = "2024-13-01")
  bad("load_label", load_label = "meio")
  bad("split", split = "daily")
  bad("delivery_days", delivery_days = 0L)
  bad("delivery_days", delivery_days = 2.5)
  bad("load_tz", load_tz = "Mars/Olympus")
  expect_pc_error(manifest(cutoff = "2024-05-31"), "contract.run_manifest_forbidden_key", where = "cutoff")
  all8 <- prevcarga_members(root = pc_root())
  expect_false(identical(prevcarga_member_set_hash(all8[-1L, ]), prevcarga_member_set_hash(all8)))
})

test_that("code_commit e renv_lock_hash por argumento", {
  d <- new_dir(); on.exit(unlink(d, recursive = TRUE))
  expect_identical(prevcarga_code_commit(d), "unknown")
  expect_null(prevcarga_renv_lock_hash(d))
  writeLines("{\"R\": {}}", file.path(d, "renv.lock"))
  expect_identical(prevcarga_renv_lock_hash(d), digest::digest(file = file.path(d, "renv.lock"), algo = "sha256"))
  expect_match(prevcarga_code_commit(pc_root()), "^([0-9a-f]{40}(-dirty)?|unknown)$")
})
