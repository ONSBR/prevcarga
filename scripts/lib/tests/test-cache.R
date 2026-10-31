## test-cache.R -- cache.R (M7.2; ADR-0023): chaves por classe de split e os dois cortes. Cada braco FORBIDDEN (delta 0)
## tem o seu CONTROL (delta != 0). Tudo em tempdir; sem variaveis de ambiente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "provenance", "cache"))
suppressMessages(env$prevcarga_runtime_enforce())

expect_pc <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$exit_code, as.integer(exit))
  invisible(cond)
}

SHA <- paste(rep("a", 64L), collapse = "")
O <- lubridate::ymd("2025-03-01")
WL <- lubridate::ymd_hm("2025-03-01 06:00", tz = PC_M7_TZ)   # weather_issue_limit devolvido pelo leitor

## chave com defaults de fixture; `...` sobrescreve qualquer argumento de prevcarga_cache_key
pc_m7_key <- function(kind, label, spec, ...) {
  a <- list(kind = kind, member_label = label, model_spec = spec, data_fingerprint = SHA, load_label = "start",
            timezone = PC_M7_TZ, weather_issue_cutoff = "available_until", weather_issue_limit = WL,
            feature_config = list(a = 1L),
            model_versions = list(x = "v1"), code_version = "abc123", lock_hash = "lock1")
  d <- list(...)
  if (kind %in% c("features", "member_long")) {
    a$origin <- d$origin; a$available_until <- d$available_until; d$origin <- NULL; d$available_until <- NULL
  }
  if (kind == "artifact") {
    a$train_origin <- d$train_origin; a$last_data_day <- d$last_data_day
    d$train_origin <- NULL; d$last_data_day <- NULL
    d$origin <- NULL; d$available_until <- NULL   # o chamador pode passar o corte do sweep: o cache o ignora
  }
  a[names(d)] <- d
  do.call(env$prevcarga_cache_key, a)
}

## --- AC1: available_until sempre em features e member_long ------------------------------------------------------
test_that("AC1 FORBIDDEN: features e member_long diferem por available_until; CONTROL: mesmo corte => mesma chave", {
  spec <- pc_m7_spec("origin_agnostic")
  lbl <- pc_m7_label("RF")
  for (kind in c("features", "member_long")) {
    k0900 <- pc_m7_key(kind, lbl, spec, origin = O, available_until = pc_m7_au(O, "09:00"))
    k0930 <- pc_m7_key(kind, lbl, spec, origin = O, available_until = pc_m7_au(O, "09:30"))
    expect_false(identical(k0900, k0930), info = kind)
    expect_identical(k0900, pc_m7_key(kind, lbl, spec, origin = O, available_until = pc_m7_au(O, "09:00")))
  }
  ctx <- pc_m7_ctx()
  k0930 <- pc_m7_key("features", lbl, spec, origin = O, available_until = pc_m7_au(O, "09:30"))
  k0900 <- pc_m7_key("features", lbl, spec, origin = O, available_until = pc_m7_au(O, "09:00"))
  env$prevcarga_cache_put(ctx, k0930, "features", list(x = 1), list(), list())
  expect_null(env$prevcarga_cache_get(ctx, k0900, "features"))      # MISS cruzado
  expect_false(is.null(env$prevcarga_cache_get(ctx, k0930, "features")))
  expect_pc(pc_m7_key("features", lbl, spec, origin = O), "contract.cache_identity", 2L)
})

## --- AC2: artefato ignora available_until e segue train_origin/last_data_day -------------------------------------------
test_that("AC2 FORBIDDEN: artefato (scoped e agnostic) e estavel entre 09:00 e 09:30; CONTROL: sensivel aos cortes", {
  k <- function(sigla, split, to, ld, au) pc_m7_key("artifact", pc_m7_label(sigla), pc_m7_spec(split),
                                                    train_origin = to, last_data_day = ld, available_until = au,
                                                    origin = O)
  for (s in list(list("LIN_MED", "origin_scoped"), list("RF", "origin_agnostic"), list("LGBM", "origin_agnostic"))) {
    to <- if (s[[2]] == "origin_scoped") O else O - 9L
    ld <- O - 1L
    expect_identical(k(s[[1]], s[[2]], to, ld, pc_m7_au(O, "09:00")), k(s[[1]], s[[2]], to, ld, pc_m7_au(O, "09:30")),
                     info = s[[1]])
    expect_false(identical(k(s[[1]], s[[2]], to, ld, pc_m7_au(O, "09:00")),
                           k(s[[1]], s[[2]], to + 1L, ld, pc_m7_au(O, "09:00"))), info = s[[1]])
  }
  ## origin_agnostic: last_data_day (data_lag_days do descriptor: LGBM O-3 contra O-1) muda a chave
  expect_false(identical(k("LGBM", "origin_agnostic", O - 9L, O - 3L, NULL), k("LGBM", "origin_agnostic", O - 9L, O - 1L, NULL)))
  expect_pc(k("LGBM", "origin_agnostic", O - 9L, NULL, NULL), "contract.cache_identity", 2L)
  ## origin_scoped: o corte e train_origin = O (nova origem => novo artefato)
  expect_false(identical(k("LIN_MED", "origin_scoped", O, O - 1L, NULL), k("LIN_MED", "origin_scoped", O + 1L, O, NULL)))
})

## --- AC3: determinismo, laterais fora, CONTROL seed/load_label/timezone -----------------------------------------------
test_that("AC3: chave deterministica v_<hash>, laterais fora do hash e do identity no meta.json; CONTROL seed/load_label/timezone", {
  ctx <- pc_m7_ctx()
  spec <- pc_m7_spec("origin_scoped"); lbl <- pc_m7_label("LIN_MED")
  args <- list(kind = "artifact", member_label = lbl, model_spec = spec, data_fingerprint = SHA, load_label = "start",
               timezone = PC_M7_TZ, weather_issue_cutoff = "available_until", train_origin = O,
               code_version = "c1", lock_hash = "l1")
  key <- do.call(env$prevcarga_cache_key, args)
  expect_match(key, "^v_[0-9a-f]{16}$")
  expect_identical(key, do.call(env$prevcarga_cache_key, args))
  id <- do.call(env$prevcarga_cache_identity, args)
  lat1 <- list(platform = "x86_64-pc-linux-gnu", bundle_id = "b1", trained_at = "2025-03-01T09:12:00Z", host = "h1")
  lat2 <- list(platform = "x86_64-w64-mingw32", bundle_id = "b2", trained_at = "2025-03-02T10:00:00Z", host = "h2")
  expect_identical(key, do.call(env$prevcarga_cache_key, args))   # laterais nem sao parametro da chave
  obj <- list(a = 1:3, b = "x")
  p <- env$prevcarga_cache_put(ctx, key, "artifact", obj, id, lat1)
  expect_identical(env$prevcarga_cache_put(ctx, key, "artifact", obj, id, lat2), p)   # lateral diferente: idempotente
  got <- env$prevcarga_cache_get(ctx, key, "artifact")
  expect_identical(got, obj)
  meta <- jsonlite::read_json(file.path(p, "meta.json"))
  expect_identical(meta$sha256, digest::digest(file = file.path(p, meta$format), algo = "sha256"))
  expect_identical(meta$lateral$bundle_id, "b1")
  expect_null(meta$identity$bundle_id)
  expect_false(grepl("b1|trained_at|host", jsonlite::toJSON(meta$identity, auto_unbox = TRUE)))
  for (ch in list(list(seed = 43L), list(load_label = "end"), list(timezone = "UTC"))) {
    k2 <- do.call(env$prevcarga_cache_key, utils::modifyList(args, ch))
    expect_false(identical(k2, key), info = names(ch))
    expect_null(env$prevcarga_cache_get(ctx, k2, "artifact"), info = names(ch))
  }
  ## artefato de membro SEM previsto no treino: a regra de clima nao entra (sem over-key; M7.8)
  expect_identical(key, do.call(env$prevcarga_cache_key, utils::modifyList(args, list(weather_issue_cutoff = "origin_midnight_utc"))))
  ## parquet para data.table
  dt <- data.table::data.table(a = 1:3, b = c(0.5, 1.5, 2.5))
  kd <- do.call(env$prevcarga_cache_key, utils::modifyList(args, list(seed = 7L)))
  pd <- env$prevcarga_cache_put(ctx, kd, "artifact", dt, id, lat1)
  expect_true(file.exists(file.path(pd, "object.parquet")))
  expect_equal(env$prevcarga_cache_get(ctx, kd, "artifact"), dt)
})

## --- AC4: LIN_MED x LIN_MAX --------------------------------------------------------------------------------------------
test_that("AC4 FORBIDDEN: LIN_MED e LIN_MAX diferem em todo kind; member_label sem exogena aborta", {
  spec <- pc_m7_spec("origin_scoped")
  au <- pc_m7_au(O, "09:00")
  for (kind in c("features", "artifact", "member_long", "khat")) {
    f <- function(sigla) pc_m7_key(kind, pc_m7_label(sigla), spec, origin = O, available_until = au,
                                   train_origin = O, last_data_day = O - 1L, subsistema = "S", window_days = 7L)
    expect_false(identical(f("LIN_MED"), f("LIN_MAX")), info = kind)
    expect_identical(f("LIN_MED"), f("LIN_MED"), info = kind)
  }
  expect_pc(pc_m7_key("features", list(mdir = "dessem-linear"), spec, origin = O, available_until = au),
            "contract.cache_member_label", 2L)
})

## --- AC5: quarentena ---------------------------------------------------------------------------------------------------
test_that("AC5: object alterado => quarentena, NULL, aviso ASCII e contador corrupt; CONTROL: entrada integra da HIT", {
  ctx <- pc_m7_ctx()
  spec <- pc_m7_spec("origin_agnostic"); lbl <- pc_m7_label("RF")
  key <- pc_m7_key("artifact", lbl, spec, train_origin = O - 9L, last_data_day = O - 1L)
  id <- list(model_id = "x")
  p <- env$prevcarga_cache_put(ctx, key, "artifact", list(v = 1), id, list())
  expect_silent(got <- env$prevcarga_cache_get(ctx, key, "artifact"))
  expect_identical(got, list(v = 1))
  expect_identical(env$prevcarga_cache_stats(ctx)$hit, 1L)
  expect_identical(env$prevcarga_cache_stats(ctx)$corrupt, 0L)
  f <- file.path(p, "object.rds")
  writeBin(c(readBin(f, "raw", 1000L), as.raw(1L)), f)
  w <- NULL
  got <- withCallingHandlers(env$prevcarga_cache_get(ctx, key, "artifact"),
                             prevcarga_warning = function(c) { w <<- c; invokeRestart("muffleWarning") })
  expect_null(got)
  expect_false(is.null(w))
  expect_identical(w$id, "warning.cache_corrupt_quarantined")
  expect_false(any(as.integer(charToRaw(conditionMessage(w))) > 127L))
  expect_false(dir.exists(p))
  expect_true(dir.exists(file.path(ctx$state_dir, "cache", "_quarantine", "artifact", key)))
  expect_identical(env$prevcarga_cache_stats(ctx)$corrupt, 1L)
  expect_null(env$prevcarga_cache_get(ctx, key, "artifact"))        # agora MISS comum
  expect_identical(env$prevcarga_cache_stats(ctx)$corrupt, 1L)
})

## --- AC6: put conflict e idempotencia ------------------------------------------------------------------------------------
test_that("AC6: put com sha diferente aborta integrity exit 9 sem tocar a entrada; CONTROL: mesmo objeto e idempotente", {
  ctx <- pc_m7_ctx()
  spec <- pc_m7_spec("origin_scoped"); lbl <- pc_m7_label("LIN_MAX")
  key <- pc_m7_key("artifact", lbl, spec, train_origin = O)
  p <- env$prevcarga_cache_put(ctx, key, "artifact", list(v = 1), list(), list())
  before <- pc5_tree_hash(p)
  expect_identical(env$prevcarga_cache_put(ctx, key, "artifact", list(v = 1), list(), list()), p)
  cond <- expect_pc(env$prevcarga_cache_put(ctx, key, "artifact", list(v = 2), list(), list()),
                    "integrity.cache_put_conflict", 9L)
  expect_false(any(as.integer(charToRaw(conditionMessage(cond))) > 127L))
  expect_identical(pc5_tree_hash(p), before)
  expect_length(list.files(dirname(p), pattern = "[.]tmp-"), 0L)
})

## --- AC7: contrato do split, layout e grep ---------------------------------------------------------------------------------
test_that("AC7: split ausente ou invalido aborta contract exit 2 antes de qualquer IO; kind/key invalidos", {
  ctx <- pc_m7_ctx()
  lbl <- pc_m7_label("RF")
  for (sp in list(NULL, "uniform", NA_character_)) {
    spec <- pc_m7_spec(); spec$split <- sp
    expect_pc(pc_m7_key("artifact", lbl, spec, train_origin = O, last_data_day = O), "contract.cache_split", 2L)
  }
  expect_false(dir.exists(file.path(ctx$state_dir, "cache")))
  expect_pc(env$prevcarga_cache_get(ctx, "v_0123456789abcdef", "outro"), "contract.cache_kind", 2L)
  expect_pc(env$prevcarga_cache_get(ctx, "v_xyz", "features"), "contract.cache_key", 2L)
  key <- "v_0123456789abcdef"
  expect_identical(env$.pc_cache_dir(ctx, "khat", key), file.path(ctx$state_dir, "cache", "khat", "01", key))
})

test_that("AC7: cache.R sem cut_invariant/train_cutoff/last_label_day/Sys.time/Sys.getenv/s3/paws; catalogo carrega sem id duplicado", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "cache.R"), warn = FALSE)
  src <- sub("^\\s*##.*$", "", src)
  expect_false(any(grepl("cut_invariant|train_cutoff|last_label_day|Sys\\.time|Sys\\.getenv|s3://|paws", src)))
  cat <- env$prevcarga_errors_load()
  ids <- c("contract.cache_kind", "contract.cache_key", "contract.cache_member_label", "contract.cache_split",
           "contract.cache_identity", "integrity.cache_put_conflict", "warning.cache_corrupt_quarantined")
  expect_true(all(ids %in% cat$id))
  expect_false(anyDuplicated(cat$id) > 0L)
  expect_identical(cat$class[cat$id == "integrity.cache_put_conflict"], "integrity")
})

test_that("fixtures do M7: deck sintetico e ctx em tempdir", {
  ctx <- pc_m7_ctx()
  expect_true(startsWith(normalizePath(ctx$state_dir), normalizePath(tempdir())))
  d <- pc_m7_deck(O)
  expect_true(file.exists(file.path(d, "manifest.json")))
  expect_identical(nrow(pc_m7_members()), 8L)
})

## --- M7.8 AC2: regra e limite de clima na identidade ------------------------------------------------------------------
test_that("M7.8 AC2: features, member_long e artefato com previsto no treino diferem por regra e limite; sem previsto, nao", {
  lbl <- pc_m7_label("RF"); spec <- pc_m7_spec("origin_agnostic")
  au <- lubridate::ymd_hm("2025-03-01 09:00", tz = PC_M7_TZ)
  rules <- c("available_until", "origin_midnight_utc")
  for (kind in c("features", "member_long")) {
    k <- vapply(rules, function(r) pc_m7_key(kind, lbl, spec, origin = O, available_until = au, weather_issue_cutoff = r), "")
    expect_false(identical(k[[1L]], k[[2L]]), info = kind)                                             # CONTROL: regra
    expect_false(identical(k[[1L]], pc_m7_key(kind, lbl, spec, origin = O, available_until = au, weather_issue_limit = WL + 3600)),
                 info = kind)                                                                           # CONTROL: limite
    expect_identical(k[[1L]], pc_m7_key(kind, lbl, spec, origin = O, available_until = au), info = kind)  # FORBIDDEN: mesmo pedido
  }
  a <- function(...) pc_m7_key("artifact", lbl, spec, train_origin = O, last_data_day = O, ...)
  expect_false(identical(a(weather_forecast_in_train = TRUE),
                         a(weather_forecast_in_train = TRUE, weather_issue_cutoff = "origin_midnight_utc")))
  expect_false(identical(a(weather_forecast_in_train = TRUE), a(weather_forecast_in_train = TRUE, weather_issue_limit = WL + 3600)))
  expect_identical(a(), a(weather_issue_cutoff = "origin_midnight_utc", weather_issue_limit = WL + 3600))   # sem previsto: igual
  expect_false(identical(a(weather_forecast_in_train = TRUE), a()))                                        # CONTROL: o flag entra
  ## meta.json registra os dois campos efetivos
  ctx <- pc_m7_ctx()
  args <- list(kind = "member_long", member_label = lbl, model_spec = spec, data_fingerprint = SHA, load_label = "start",
               timezone = PC_M7_TZ, weather_issue_cutoff = "origin_midnight_utc", weather_issue_limit = WL, origin = O,
               available_until = au, code_version = "c1", lock_hash = "l1")
  key <- do.call(env$prevcarga_cache_key, args)
  d <- env$prevcarga_cache_put(ctx, key, "member_long", list(1L), do.call(env$prevcarga_cache_identity, args))
  w <- jsonlite::read_json(file.path(d, "meta.json"))$identity$feature_config$weather
  expect_identical(w$weather_issue_cutoff, "origin_midnight_utc")
  expect_identical(w$weather_issue_limit, "2025-03-01T09:00:00+0000")
  ## regra ausente, invalida ou limite ausente => contract (exit 2), antes de qualquer IO
  expect_pc(pc_m7_key("member_long", lbl, spec, origin = O, available_until = au, weather_issue_cutoff = NULL),
            "contract.cache_identity", 2L)
  expect_pc(pc_m7_key("member_long", lbl, spec, origin = O, available_until = au, weather_issue_cutoff = "x"),
            "contract.cache_identity", 2L)
  expect_pc(pc_m7_key("features", lbl, spec, origin = O, available_until = au, weather_issue_limit = NULL),
            "contract.cache_identity", 2L)
  expect_pc(a(weather_forecast_in_train = TRUE, weather_issue_limit = NULL), "contract.cache_identity", 2L)
})
