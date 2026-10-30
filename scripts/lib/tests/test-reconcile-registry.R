## test-reconcile-registry.R -- perfil k-hat como artefato oficial do registry (R.2). Tudo em tempdir.
## O ciclo de vida (publicar/resolver/carregar/ponteiro) e do nucleo do registry; aqui so o k-hat.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long",
                       "deck_reader", "evaluation", "reconcile"))
reg_env <- env   # onde vivem as funcoes do registry: o mock de file.rename entra aqui
ss <- "SS2"

expect_pc <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$exit_code, as.integer(exit))
  invisible(cond)
}

kh_ctx <- function(.env = parent.frame()) {
  d <- tempfile("kh-state-")
  dir.create(file.path(d, "artifacts"), recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE), envir = .env)
  list(state_dir = d, tz = pc_rc_tz)
}

## ratio sintetico deterministico + fit + meta (khat_origin = O; janela termina em O-1)
kh_ratio <- function(from = "2025-10-01", to = "2026-01-14") {
  days <- seq(lubridate::ymd(from), lubridate::ymd(to), by = "day")
  pc_truth_ratio(days, function(d) 1 + (as.integer(format(d, "%d")) %% 7) / 100)
}
kh_fit <- function(origin = "2026-01-15", window_days = 60L, ratio = kh_ratio()) {
  list(fit = env$prevcarga_khat_fit(ratio, lubridate::ymd(origin), window_days),
       ratio = ratio, origin = lubridate::ymd(origin))
}
kh_meta <- function(f, trained_at = "2026-01-15T10:00:00Z") {
  list(ss = ss, khat_origin = f$origin, window_days = f$fit$window_days, window_from = f$fit$window_from,
       window_to = f$fit$window_to, profile_keys = env$prevcarga_khat_keys, week_start = 7L,
       areas = c("Ls2_2", "Ls2_1", "Ls2_3"), loss = "P2", n_ratio = f$fit$n_ratio, load_label = "start",
       timezone = pc_rc_tz, data_hash = env$prevcarga_khat_data_hash(f$ratio),
       code_commit = "abc123", renv_lock_hash = strrep("a", 64), trained_at = trained_at)
}
kh_freeze <- function(f, ctx, ...) env$prevcarga_khat_freeze(f$fit, kh_meta(f, ...), ctx)
kh_cell <- function(ctx) env$prevcarga_khat_key(ss, ctx)

test_that("AC1: mesmo perfil => mesmo hash e no-op; outra janela => outro hash; latest regular; manifest com sha", {
  ctx <- kh_ctx()
  a <- kh_fit(); b <- kh_fit()
  r1 <- kh_freeze(a, ctx, trained_at = "2026-01-15T10:00:00Z")
  expect_false(r1$reused)
  expect_match(r1$version, "^v_[0-9a-f]{16}$")
  f_rds <- file.path(r1$path, "artifact.rds"); f_mf <- file.path(r1$path, "run_manifest.json")
  Sys.setFileTime(c(f_rds, f_mf), as.POSIXct("2020-01-01 00:00:00", tz = "UTC"))   # marca de mtime (so teste)
  m0 <- file.mtime(c(f_rds, f_mf)); b0 <- c(readBin(f_rds, "raw", 1e7), readBin(f_mf, "raw", 1e7))
  r2 <- kh_freeze(b, ctx, trained_at = "2026-01-16T11:00:00Z")           # trained_at muda, hash nao
  expect_identical(r2$version, r1$version)
  expect_true(r2$reused)
  expect_identical(file.mtime(c(f_rds, f_mf)), m0)
  expect_identical(c(readBin(f_rds, "raw", 1e7), readBin(f_mf, "raw", 1e7)), b0)
  ## outra janela => outro hash
  c60 <- kh_fit(window_days = 30L)
  r3 <- kh_freeze(c60, ctx)
  expect_false(identical(r3$version, r1$version))
  expect_identical(sort(list.files(dirname(r1$path), pattern = "^v_")), sort(c(r1$version, r3$version)))
  ## ponteiro pelo nucleo; latest regular, 1 linha, LF
  expect_true(env$prevcarga_khat_advance_latest(ss, r1$version, a$origin, ctx))
  lp <- file.path(kh_cell(ctx), "latest.txt")
  expect_true(file.exists(lp)); expect_true(is.na(Sys.readlink(lp)) || !nzchar(Sys.readlink(lp)))
  expect_identical(readBin(lp, "raw", 100L), charToRaw(paste0(r1$version, "\n")))
  ## manifest: sha do arquivo e meta
  mf <- env$prevcarga_read_run_manifest(r1$path)
  expect_identical(mf$arquivos$path, "artifact.rds")
  expect_identical(mf$arquivos$sha256, digest::digest(file = f_rds, algo = "sha256"))
  expect_identical(mf$khat_origin, "2026-01-15"); expect_identical(mf$window_days, 60L)
  expect_identical(mf$load_label, "start"); expect_identical(mf$timezone, pc_rc_tz)
  expect_identical(mf$trained_at, "2026-01-15T10:00:00Z")
  expect_false(any(grepl("^\\.staging-", list.files(dirname(r1$path), all.files = TRUE))))
})

test_that("AC2: 1 byte alterado => freeze khat_conflict sem sobrescrever; thaw exit 9; CONTROL integro", {
  ctx <- kh_ctx(); a <- kh_fit()
  r <- kh_freeze(a, ctx)
  want <- a$fit$profile
  th <- env$prevcarga_khat_thaw(ctx, ss, r$version)                     # CONTROL: identical ao congelado
  expect_equal(th$profile, data.table::as.data.table(want), ignore_attr = TRUE)
  expect_identical(th$glob, a$fit$glob)
  expect_identical(kh_freeze(a, ctx)$reused, TRUE)                      # CONTROL: freeze integro e no-op
  f <- file.path(r$path, "artifact.rds")
  raw <- readBin(f, "raw", 1e7); raw[length(raw) %/% 2L] <- as.raw(bitwXor(as.integer(raw[length(raw) %/% 2L]), 1L))
  writeBin(raw, f)
  expect_pc(kh_freeze(a, ctx), "integrity.khat_conflict", 9L)
  expect_identical(readBin(f, "raw", 1e7), raw)                         # bytes adulterados preservados
  ## tamper no nucleo => integrity.artifact_hash (exit 9): o thaw passa pelo SHA256SUMS do registry
  expect_pc(env$prevcarga_khat_thaw(ctx, ss, r$version), "integrity.artifact_hash", 9L)
})

test_that("AC3a: falha injetada no rename final => sem v_ final, sem .tmp-*, latest intacto; CONTROL anda", {
  ctx <- kh_ctx(); old <- kh_fit(origin = "2026-01-10")
  ro <- kh_freeze(old, ctx); env$prevcarga_khat_advance_latest(ss, ro$version, old$origin, ctx)
  nw <- kh_fit(origin = "2026-01-20")
  ## o mock cobre so o rename que publica v_<hash>/ (o do staging e do freeze interno seguem reais)
  assign("file.rename", function(from, to) if (grepl("/v_[0-9a-f]+$", to)) FALSE else base::file.rename(from, to),
         envir = reg_env)
  withr::defer(if (exists("file.rename", envir = reg_env, inherits = FALSE)) rm("file.rename", envir = reg_env))
  expect_pc(kh_freeze(nw, ctx), "state.promote_failed", 13L)
  cell <- kh_cell(ctx)
  expect_identical(list.files(cell, pattern = "^v_"), ro$version)
  expect_identical(list.files(cell, pattern = "^\\.tmp-", all.files = TRUE), character())
  expect_identical(env$prevcarga_khat_resolve(ctx, ss), ro$version)
  expect_identical(env$prevcarga_khat_thaw(ctx, ss, env$prevcarga_khat_resolve(ctx, ss))$meta$khat_origin, old$origin)
  rm("file.rename", envir = reg_env)
  rn <- kh_freeze(nw, ctx)                                              # CONTROL: sem falha o v_ aparece de uma vez
  expect_true(dir.exists(rn$path))
  expect_true(env$prevcarga_khat_advance_latest(ss, rn$version, nw$origin, ctx))
  expect_identical(env$prevcarga_khat_resolve(ctx, ss), rn$version)
})

test_that("AC3b: processo filho encerrado antes do rename final => so .tmp-* orfao, ignorado por resolve/thaw", {
  ctx <- kh_ctx(); old <- kh_fit(origin = "2026-01-10")
  ro <- kh_freeze(old, ctx); env$prevcarga_khat_advance_latest(ss, ro$version, old$origin, ctx)
  nw <- kh_fit(origin = "2026-01-20")
  wd <- tempfile("kh-child-"); dir.create(wd); withr::defer(unlink(wd, recursive = TRUE))
  saveRDS(list(f = nw, meta = kh_meta(nw), ctx = ctx), file.path(wd, "in.rds"))
  libs <- file.path(pc_root(), "scripts", "lib")
  writeLines(c(
    "suppressMessages({library(data.table); library(lubridate)})",
    sprintf("options(prevcarga.lib = %s)", deparse(libs)),
    sprintf("for (m in c('errors','runtime','config','topology','provenance','registry','schema_long','deck_reader','evaluation','reconcile')) sys.source(file.path(%s, paste0(m, '.R')), envir = globalenv())", deparse(libs)),
    ## morte do processo no instante do rename final (nenhum on.exit roda)
    "file.rename <- function(from, to) if (grepl('/v_[0-9a-f]+$', to)) quit(save = 'no', status = 3L, runLast = FALSE) else base::file.rename(from, to)",
    sprintf("x <- readRDS(%s)", deparse(file.path(wd, "in.rds"))),
    "prevcarga_khat_freeze(x$f$fit, x$meta, x$ctx)"), file.path(wd, "child.R"))
  st <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), file.path(wd, "child.R"), stdout = FALSE, stderr = FALSE))
  expect_identical(as.integer(st), 3L)
  cell <- kh_cell(ctx)
  expect_identical(list.files(cell, pattern = "^v_"), ro$version)
  orf <- list.files(cell, pattern = "^\\.tmp-", all.files = TRUE)
  expect_length(orf, 1L)
  expect_false(any(grepl("^\\.staging-", list.files(cell, all.files = TRUE))))
  expect_identical(env$prevcarga_khat_resolve(ctx, ss), ro$version)
  expect_pc(env$prevcarga_khat_resolve(ctx, ss, orf), "contract.version_malformed", 2L)   # orfao nunca e versao
  expect_identical(env$prevcarga_khat_thaw(ctx, ss, ro$version)$meta$khat_origin, old$origin)
  expect_identical(readLines(file.path(cell, "latest.txt")), ro$version)
})

test_that("AC4: ponteiro so anda para frente; origem menor grava a versao e avisa", {
  ctx <- kh_ctx()
  mid <- kh_fit(origin = "2026-01-15"); lo <- kh_fit(origin = "2026-01-10"); hi <- kh_fit(origin = "2026-01-20")
  rm <- kh_freeze(mid, ctx); expect_true(env$prevcarga_khat_advance_latest(ss, rm$version, mid$origin, ctx))
  rl <- kh_freeze(lo, ctx)
  warned <- NULL
  w <- withCallingHandlers(env$prevcarga_khat_advance_latest(ss, rl$version, lo$origin, ctx),
                           prevcarga_warning = function(c) { warned <<- c; invokeRestart("muffleWarning") })
  expect_false(w)
  expect_identical(warned$id, "reconcile.khat_not_promoted")
  expect_true(dir.exists(rl$path))
  expect_identical(env$prevcarga_khat_resolve(ctx, ss), rm$version)
  rh <- kh_freeze(hi, ctx)
  expect_true(env$prevcarga_khat_advance_latest(ss, rh$version, hi$origin, ctx))
  expect_identical(env$prevcarga_khat_resolve(ctx, ss), rh$version)
})

test_that("AC5: copia byte a byte no layout do bucket aplica sem conversao; --khat-version sem latest; ausente => 13", {
  ctx <- kh_ctx(); a <- kh_fit()
  r <- kh_freeze(a, ctx); env$prevcarga_khat_advance_latest(ss, r$version, a$origin, ctx)
  ctx2 <- kh_ctx()
  dst <- kh_cell(ctx2); dir.create(dst, recursive = TRUE)
  expect_true(all(file.copy(list.files(kh_cell(ctx), full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)))
  v2 <- env$prevcarga_khat_resolve(ctx2, ss)
  expect_identical(v2, r$version)
  expect_identical(env$prevcarga_khat_thaw(ctx2, ss, v2), env$prevcarga_khat_thaw(ctx, ss, r$version))
  unlink(file.path(dst, "latest.txt"))
  expect_identical(env$prevcarga_khat_resolve(ctx2, ss, r$version), r$version)
  expect_equal(env$prevcarga_khat_thaw(ctx2, ss, r$version)$profile, data.table::as.data.table(a$fit$profile), ignore_attr = TRUE)
  c1 <- expect_pc(env$prevcarga_khat_resolve(ctx2, ss), "state.khat_missing", 13L)
  expect_match(c1$hint, "rode reconcile --mode train antes", fixed = TRUE)
  expect_false(grepl("[^\x01-\x7F]", c1$hint))
  c2 <- expect_pc(env$prevcarga_khat_resolve(ctx2, ss, "v_0123456789abcdef"), "state.khat_missing", 13L)
  expect_identical(c2$where, "v_0123456789abcdef")
})

test_that("AC6: schema_version != 1 ou sem coluna dow => contract.khat_profile; CONTROL canonico passa", {
  ctx <- kh_ctx(); a <- kh_fit()
  r <- kh_freeze(a, ctx)
  expect_identical(env$prevcarga_khat_thaw(ctx, ss, r$version)$schema_version, 1L)   # CONTROL
  ## artefato com sha valido (publicado pelo nucleo) mas invalido para o k-hat
  art <- env$.prevcarga_khat_artifact(a$fit, kh_meta(a))
  put <- function(x, v) env$prevcarga_registry_publish(x, list(n = v), kh_cell(ctx), v, ctx = ctx)
  bad1 <- art; bad1$schema_version <- 2L; put(bad1, "v_00000000000000a1")
  bad2 <- art; bad2$profile <- bad2$profile[, c("hour", "month", "k", "n")]; put(bad2, "v_00000000000000a2")
  expect_identical(expect_pc(env$prevcarga_khat_thaw(ctx, ss, "v_00000000000000a1"), "contract.khat_profile", 2L)$where, "schema_version")
  expect_identical(expect_pc(env$prevcarga_khat_thaw(ctx, ss, "v_00000000000000a2"), "contract.khat_profile", 2L)$where, "dow")
})
