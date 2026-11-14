## test-doctor-tz.R -- doctor confere fuso e tzdata (E1-T6): linhas 'fuso <tz>', linhas por papel de dados e raiz de artefatos.

lib_dir <- file.path(pc_root(), "scripts", "lib")
withr::local_options(prevcarga.lib = lib_dir)

new_env <- function()
  pc_source_lib(c("errors", "runtime", "config", "banner", "cli", "topology", "provenance", "deck_reader",
                  "dates", "introspect"))
mk_ctx <- function(E, ...) {
  ctx <- E$prevcarga_ctx(list(), pc_root(), NULL)
  extra <- list(...)
  for (n in names(extra)) ctx[[n]] <- extra[[n]]
  ctx
}
fixed_check <- function() {
  f <- list(tz = NA_character_, lc = "en_US.UTF-8", omp = NA_character_, openblas = NA_character_,
            mkl = NA_character_, veclib = NA_character_, blas_threads = 16L)
  e <- list(tz = "UTC", lc = "C.UTF-8", omp = "1", openblas = "1", mkl = "1", veclib = "1", blas_threads = 1L)
  function() list(ok = TRUE, found = f, effective = e, offenders = character())
}
all_pkgs <- function(p) TRUE
row_of <- function(d, ck) d$checks[d$checks$check == ck, ]

test_that("[AC0] sem tzdata: FAIL runtime.tzdata_missing, exit 7 e mesmo numero de linhas", {
  E <- new_env()
  ok <- E$prevcarga_build_doctor(mk_ctx(E), check = fixed_check(), has_pkg = all_pkgs)
  ko <- E$prevcarga_build_doctor(mk_ctx(E), check = fixed_check(), has_pkg = all_pkgs, olson = function() character(0))
  r <- row_of(ko, "fuso America/Sao_Paulo")
  expect_identical(r$status, "FAIL"); expect_identical(r$found, "runtime.tzdata_missing")
  expect_match(r$effective, "tzdata", fixed = TRUE)
  expect_identical(attr(ko, "exit_code"), 7L)
  expect_identical(nrow(ko$checks), nrow(ok$checks))
})

test_that("[AC1] deck de fixture: fusos Etc/GMT+3 e UTC PASS, linhas por papel no lugar de data_dir, exit 0", {
  E <- new_env()
  d <- pc_deck_clean("end", dir = withr::local_tempdir())
  ctx <- mk_ctx(E, data_train_dir = d, data_dir = d)
  res <- E$prevcarga_build_doctor(ctx, check = fixed_check(), has_pkg = all_pkgs)
  for (tz in c("America/Sao_Paulo", "Etc/GMT+3", "UTC"))
    expect_identical(row_of(res, paste("fuso", tz))$status, "PASS", info = tz)
  expect_identical(row_of(res, "data_train_dir")$status, "PASS")
  expect_identical(row_of(res, "data_inference_dir")$status, "WARN")
  expect_identical(row_of(res, "data_inference_dir")$found, "ausente")
  expect_true("artifacts_dir" %in% res$checks$check)
  expect_false("data_dir" %in% res$checks$check)
  expect_identical(attr(res, "exit_code"), 0L)
})

test_that("[AC2] sentinela forcado a falhar: FAIL mesmo com o nome presente em OlsonNames", {
  E <- new_env()
  rows <- E$.prevcarga_doctor_tz_rows(mk_ctx(E), olson = OlsonNames, offset = function() "-0300")
  expect_identical(rows$status, "FAIL"); expect_identical(rows$found, "runtime.tzdata_missing")
  ok <- E$.prevcarga_doctor_tz_rows(mk_ctx(E), olson = OlsonNames, offset = function() "-0200")
  expect_identical(ok$status, "PASS")
})

test_that("[AC3] artefatos: doctor_extend le a raiz apontada; CONTROL: sem artifacts_dir varre <state_dir>/artifacts", {
  E <- new_env()
  root <- pc_install_copy()
  f <- "config/VERSION"
  writeLines("1.2.3", file.path(root, f))
  x <- list(version = "1.2.3", platform = E$.prevcarga_host_platform(), r_version = as.character(getRversion()),
            files = list(list(path = f, sha256 = digest::digest(file = file.path(root, f), algo = "sha256"))))
  writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE)), file.path(root, "RUNTIME.json"))
  put_latest <- function(base) {   # latest.txt de um membro SEM a pasta v_<hash>
    dir.create(file.path(base, "m1"), recursive = TRUE)
    writeLines("v_0123456789abcdef", file.path(base, "m1", "latest.txt"))
    base
  }
  presenca <- function(ctx) {
    d <- E$prevcarga_doctor_extend(E$prevcarga_build_doctor(ctx, check = fixed_check(), has_pkg = all_pkgs), ctx)
    d$checks$status[d$checks$check == "artefatos.presenca"]
  }
  st <- file.path(tempfile("state-"), "s"); dir.create(st, recursive = TRUE)
  pointed <- put_latest(file.path(tempfile("pt-"), "arts")); 
  ctx <- function(ad) { c0 <- E$prevcarga_ctx(list(), root, NULL); c0$state_dir <- st; c0$artifacts_dir <- ad; c0 }
  expect_identical(presenca(ctx(pointed)), "FAIL")
  expect_identical(presenca(ctx(NULL)), "PASS")            # CONTROL: <state_dir>/artifacts vazio
  put_latest(file.path(st, "artifacts"))
  expect_identical(presenca(ctx(NULL)), "FAIL")            # CONTROL: sem artifacts_dir varre <state_dir>/artifacts
  empty <- file.path(tempfile("empty-"), "arts"); dir.create(empty, recursive = TRUE)
  expect_identical(presenca(ctx(empty)), "PASS")           # a raiz apontada manda, nao o state_dir
})
