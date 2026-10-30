## test-portability.R -- gates [PLUGINS] artefato_puro e [DEFERRED M8] portabilidade_cross_os do Holt-Winters (HW.7; AC 12).
## O artefato do despachante (P.0c/P.0d) e dado puro: carimbo + matrizes/data.frames; nada de forecast/ets/call/env/closure,
## caminho ou relogio. O artefato e congelado com prevcarga_plugin_freeze_bytes e lido por prevcarga_plugin_thaw.
if (!exists("pc_gate_deferred", mode = "function"))
  pc_gate_deferred <- function(owner, reason) testthat::skip(sprintf("[DEFERRED %s] %s", owner, reason))

HP_STAMP <- c("load_label", "timezone", "weather_issue_cutoff", "split", "granularity_min", "seed", "params",
              "train_origin", "origin_day")

hp_base <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      st <- hv_store("09:00"); cuts <- hv_cuts(st); tr <- hv_panel(st, cuts, "train"); inf <- hv_panel(st, cuts, "infer")
      cache <<- list(cuts = cuts, infer = inf, art = hv_train(tr, cuts))
    }
    cache
  }
})

## classes e tipos proibidos em qualquer profundidade (listas, atributos, data.frames)
hp_walk <- function(x, bad = c("forecast", "ets", "ts", "call", "lm", "loess", "kmeans")) {
  hit <- character()
  add <- function(w) hit <<- c(hit, w)
  rec <- function(v, path) {
    if (inherits(v, bad)) add(paste0(path, ":class"))
    if (is.environment(v)) add(paste0(path, ":environment"))
    if (is.function(v)) add(paste0(path, ":closure"))
    if (typeof(v) %in% c("externalptr", "language", "symbol")) add(paste0(path, ":", typeof(v)))
    if (is.character(v) && any(grepl("^(/|[A-Za-z]:[\\\\/])", v))) add(paste0(path, ":path"))
    for (a in setdiff(names(attributes(v)), c("names", "dim", "dimnames", "class", "row.names", "levels", "tzone")))
      rec(attr(v, a), paste0(path, "@", a))
    if (is.list(v)) for (i in seq_along(v)) rec(v[[i]], paste0(path, "$", if (is.null(names(v))) i else names(v)[[i]]))
  }
  rec(x, "$")
  hit
}

test_that("[PLUGINS] artefato_puro: scan_pure ok, sem forecast/ets/call/env/closure/caminho/relogio, com os carimbos; thaw+infer identical; CONTROL trained_at; byte adulterado => exit 9", {
  b <- hp_base(); art <- b$art
  expect_s3_class(art, "prevcarga_plugin_artifact")
  expect_silent(hw_lib$prevcarga_plugin_scan_pure(art))
  expect_identical(hp_walk(art), character())
  expect_true(all(HP_STAMP %in% names(art$stamp)))
  expect_identical(art$stamp$split, "origin_scoped"); expect_identical(art$stamp$seed, 1234L)
  expect_identical(art$stamp$granularity_min, 30L)
  expect_identical(art$stamp$origin_day, b$cuts$origin_day); expect_identical(art$stamp$train_origin, b$cuts$train_origin)
  expect_false(any(grepl("^(trained_at|created_at|timestamp|now|hostname)$", c(names(art$stamp), names(art$payload)))))
  ## congelado e lido de volta: infer do artefato descongelado == infer do objeto em memoria
  f <- tempfile("hp-art-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))), add = TRUE)
  sha <- hw_lib$prevcarga_plugin_freeze_bytes(art, f)
  back <- hw_lib$prevcarga_plugin_thaw(f)
  expect_identical(back, art)
  expect_identical(hw_lib$prevcarga_plugin_freeze_bytes(back, tempfile("hp-back-", fileext = ".rds")), sha)
  a <- hv_infer(art, b$infer, b$cuts); z <- hv_infer(back, b$infer, b$cuts)
  expect_identical(z, a)
  expect_identical(hv_infer(hw_lib$prevcarga_plugin_thaw(f), b$infer, b$cuts)$values, a$values)
  ## CONTROL: campo de relogio injetado => scan_pure acusa e o sha256 muda
  dirty <- art; dirty$payload$trained_at <- Sys.time()
  cnd <- hw_catch(hw_lib$prevcarga_plugin_scan_pure(dirty))
  expect_identical(cnd$id, "plugin.impure_artifact"); expect_match(conditionMessage(cnd), "clock")
  sha_of <- function(o) { g <- tempfile("hp-sha-", fileext = ".rds"); on.exit(unlink(c(g, paste0(g, ".manifest.json")))); hw_lib$prevcarga_freeze(o, g) }
  expect_false(identical(sha_of(dirty), sha_of(art)))
  expect_identical(hw_catch(hw_lib$prevcarga_plugin_freeze_bytes(dirty, tempfile(fileext = ".rds")))$id, "plugin.impure_artifact")
  ## CONTROL do andador: forecast/closure/caminho/environment plantados no payload sao acusados
  for (x in list(structure(list(), class = "forecast"), function() 1, "/tmp/x", new.env(), quote(f(x))))
    expect_gt(length(hp_walk(list(p = x))), 0L)
  ## 1 byte adulterado => integrity, exit 9
  raw <- readBin(f, "raw", file.info(f)$size)
  raw[length(raw) %/% 2L] <- as.raw(bitwXor(as.integer(raw[length(raw) %/% 2L]), 1L)); writeBin(raw, f)
  e <- hw_catch(hw_lib$prevcarga_plugin_thaw(f))
  expect_hv_error(e, "integrity", 9L, id = "plugin.artifact_corrupt")
})

test_that("[DEFERRED M8] portabilidade_cross_os: artefato Linux -> Windows <= 0,1%, VM limpa", {
  pc_gate_deferred("M8", "artefato Linux -> Windows <= 0,1%, VM limpa (so o ajuste MLE pode diferir; o artefato e dado puro)")
})
