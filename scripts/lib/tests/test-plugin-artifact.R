## test-plugin-artifact.R -- contrato freeze/thaw dos plugins (P.0d): carimbo, pureza, conferencia e integridade.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("runtime", "errors", "provenance", "topology", "deck_reader", "validation", "registry", "features"),
              env = environment())
prevcarga_registry_reset(); prevcarga_plugin_unload_all()

O <- lubridate::ymd("2026-10-01")
cuts_for <- function(m) pc_toy_cuts(O, split = if (m == "toy-60") "origin_scoped" else "origin_agnostic")
plug <- function(m) prevcarga_plugin_load(m, pc_toy_root())
frz <- function(a) { f <- tempfile(fileext = ".rds"); list(path = f, sha = prevcarga_plugin_freeze_bytes(a, f)) }

expect_cond <- function(expr, id, class = "contract", exit = 2L, text = NULL) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (is.null(cond)) return(invisible(NULL))
  expect_identical(cond$id, id); expect_identical(cond$class_name, class); expect_identical(cond$exit_code, exit)
  expect_match(conditionMessage(cond), sprintf("[prevcarga/%s]", class), fixed = TRUE)
  for (t in text) expect_true(grepl(t, conditionMessage(cond), fixed = TRUE), info = t)
  invisible(cond)
}

test_that("ac-0 carimbos, sha256 estavel e thaw/infer identicos nas duas classes", {
  want <- c("model_id", "variant", "split", "granularity_min", "load_label", "timezone", "weather_issue_cutoff",
            "params", "seed", "contract_version", "train_origin")
  for (m in c("toy-30", "toy-60")) {
    cuts <- cuts_for(m); p <- plug(m)
    a1 <- pc_toy_artifact(m, cuts); a2 <- pc_toy_artifact(m, cuts)
    expect_identical(names(a1$stamp), if (m == "toy-60") c(want, "origin_day") else want)
    expect_null(a1$stamp$available_until)
    expect_identical(a1$stamp$train_origin, if (m == "toy-60") lubridate::ymd("2026-10-01") else lubridate::ymd("2026-09-29"))
    if (m == "toy-60") expect_identical(a1$stamp$origin_day, O)
    f1 <- frz(a1); f2 <- frz(a2)
    expect_identical(f1$sha, f2$sha)
    th <- prevcarga_plugin_thaw(f1$path, p)
    expect_identical(th, a1)
    inf <- function(a) p$descriptor$stages$inference$fn(p$descriptor$thaw(a$payload), list(x = c(1, 2, 3)), cuts)
    expect_identical(inf(th), inf(a1))
    expect_silent(prevcarga_plugin_check_artifact(th, p, p$spec, cuts))
  }
})

test_that("ac-1 seis injecoes acusadas com caminho; call puro passa; CONTROL do sha256 com relogio", {
  a <- pc_toy_artifact("toy-30", cuts_for("toy-30"))
  tm <- terms(y ~ x)
  inj <- list(
    list("$payload$fit$trained_at", Sys.time(), "$payload$fit$trained_at"),
    list("$payload$fit$e", new.env(), "$payload$fit$e"),
    list("$payload$fit$f", function(x) x, "$payload$fit$f"),
    list("$payload$fit$p", normalizePath(tempdir()), "$payload$fit$p"),
    list("$payload$fit$x", methods::new("externalptr"), "$payload$fit$x"),
    list("$payload$fit$t", tm, ".Environment"))
  for (i in inj) {
    bad <- pc_inject(a, i[[1L]], i[[2L]])
    expect_cond(prevcarga_plugin_scan_pure(bad), "plugin.impure_artifact", text = i[[3L]])
  }
  expect_true(prevcarga_plugin_scan_pure(pc_inject(a, "$payload$call", quote(f(x)))))
  expect_true(prevcarga_plugin_scan_pure(a))
  ## CONTROL: o relogio injetado faz o sha256 de dois freezes identicos divergir (e o scan o acusa)
  b1 <- pc_inject(a, "$payload$trained_at", Sys.time()); Sys.sleep(1.1)
  b2 <- pc_inject(a, "$payload$trained_at", Sys.time())
  s1 <- prevcarga_freeze(b1, tempfile(fileext = ".rds")); s2 <- prevcarga_freeze(b2, tempfile(fileext = ".rds"))
  expect_false(identical(s1, s2))
  expect_cond(prevcarga_plugin_freeze_bytes(b1, tempfile(fileext = ".rds")), "plugin.impure_artifact")
})

test_that("ac-2 matriz de check_artifact e integridade do arquivo", {
  p60 <- plug("toy-60"); p30 <- plug("toy-30")
  c60 <- cuts_for("toy-60"); c30 <- cuts_for("toy-30")
  a60 <- pc_toy_artifact("toy-60", c60); a30 <- pc_toy_artifact("toy-30", c30)
  expect_silent(prevcarga_plugin_check_artifact(a60, p60, p60$spec, c60))
  expect_silent(prevcarga_plugin_check_artifact(a30, p30, p30$spec, c30))
  mm <- function(a, p, cuts, field, text) expect_cond(
    prevcarga_plugin_check_artifact(a, p, p$spec, cuts), "plugin.artifact_mismatch", text = c(field, text))
  mm({ x <- a60; x$stamp$model_id <- "toy-30"; x }, p60, c60, "model_id", c("toy-30", "toy-60"))
  mm({ x <- a60; x$stamp$contract_version <- 2L; x }, p60, c60, "contract_version", c("2", "1"))
  mm({ x <- a30; x$stamp$granularity_min <- 60L; x }, p30, c30, "granularity_min", c("60", "30"))
  mm({ x <- a60; x$stamp$origin_day <- lubridate::ymd("2026-09-30"); x }, p60, c60, "origin_day", c("2026-09-30", "2026-10-01"))
  mm({ x <- a30; x$stamp$train_origin <- lubridate::ymd("2026-09-30"); x }, p30, c30, "train_origin", c("2026-09-30", "2026-09-29"))
  ## CONTROL: treino mais antigo que o corte e aceito
  old <- a30; old$stamp$train_origin <- lubridate::ymd("2026-09-20")
  expect_silent(prevcarga_plugin_check_artifact(old, p30, p30$spec, c30))
  ## arquivo truncado => integrity exit 9
  f <- frz(a30)$path
  raw <- readBin(f, "raw", file.size(f)); writeBin(raw[seq_len(length(raw) %/% 2L)], f)
  expect_cond(prevcarga_plugin_thaw(f, p30), "plugin.artifact_corrupt", class = "integrity", exit = 9L)
  expect_cond(prevcarga_plugin_thaw(tempfile(fileext = ".rds"), p30), "plugin.artifact_corrupt", "integrity", 9L)
})
