## test-inference-needs.R -- inference_needs (E2-T1): normalize, default generico, registry (data_needs), gravacao no
## artefato fora do fingerprint, aggregate deterministico e resolve artifacts|topology. Fixtures sinteticas; tudo em tempdir.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                       "registry", "features", "inference_needs", "training", "inference"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")

expect_pc <- function(expr, id, exit = 2L) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (is.null(cond)) return(invisible(NULL))
  expect_identical(cond$id, id)
  expect_identical(cond$exit_code, as.integer(exit))
  invisible(cond)
}

## forma v1 minima valida (campos sobrescrevem por nome)
nd <- function(load = 3L, annex = FALSE, segs = list(), wf_area = "SP", hol = c(-3L, 2L)) list(
  schema_version = 1L, load = list(days_before = load), weather_observed = list(days_before = load),
  weather_forecast = list(area = wf_area, segments = segs),
  holidays = list(from = hol[[1L]], to = hol[[2L]]), holiday_annex = annex)
seg <- function(kind, from, to, lead = NA_integer_) list(kind = kind, from = from, to = to, lead = lead)

test_that("normalize: forma valida vira inteiros, segmentos radix deduplicados, sempre a mesma forma", {
  x <- nd(load = 9, segs = list(seg("freshest", 0, 8), seg("per_origin", -9, 0), seg("freshest", 0, 8),
                                seg("min_lead", 1, 1, 1)))
  y <- env$prevcarga_inference_needs_normalize(x, "m")
  expect_identical(y$schema_version, 1L)
  expect_identical(y$load$days_before, 9L)
  expect_identical(vapply(y$weather_forecast$segments, `[[`, "", "kind"), c("freshest", "min_lead", "per_origin"))
  expect_identical(y$weather_forecast$segments[[1L]], seg("freshest", 0L, 8L))
  expect_identical(y$weather_forecast$segments[[2L]]$lead, 1L)
  expect_identical(env$prevcarga_inference_needs_normalize(y, "m"), y)          # idempotente
  z <- env$prevcarga_inference_needs_normalize(nd(), "m")                      # sem previsto
  expect_identical(z$weather_forecast, list(area = "SP", segments = list()))
})

test_that("normalize: cada campo invalido aborta contract.inference_needs_invalid com details.field e where", {
  chk <- function(x, field) {
    c <- expect_pc(env$prevcarga_inference_needs_normalize(x, "mdl"), "contract.inference_needs_invalid", 2L)
    expect_identical(c$where, "mdl")
    expect_identical(c$details$field, field)
  }
  chk(c(nd(), list(extra = 1)), "extra")
  chk(nd(segs = list(seg("latest", 0, 1))), "weather_forecast.segments.kind")
  chk(nd(segs = list(seg("freshest", 1.5, 2))), "weather_forecast.segments.from")
  chk(nd(segs = list(seg("freshest", 3, 2))), "weather_forecast.segments.from")
  chk(nd(segs = list(seg("min_lead", 0, 1))), "weather_forecast.segments.lead")
  chk(nd(wf_area = ""), "weather_forecast.area")
  chk(modifyList(nd(), list(load = list(days_before = 2.5))), "load.days_before")
  chk(nd(hol = c(3L, 1L)), "holidays.from")
  chk(modifyList(nd(), list(holiday_annex = NA)), "holiday_annex")
})

fake_plugin <- function(requires, id = "fake") list(descriptor = list(model_id = id,
  stages = list(inference = list(requires = requires))))
test_that("default generico por campos: com e sem requires de previsto", {
  spec <- list(history_days = list(infer = 7L), horizon_days = 2L)
  a <- env$prevcarga_inference_needs_of(fake_plugin(c("data.load.y", "data.weather_forecast.temperature")), spec, "SP")
  expect_identical(a$load$days_before, 7L); expect_identical(a$weather_observed$days_before, 7L)
  expect_identical(a$weather_forecast, list(area = "SP", segments = list(seg("freshest", 0L, 2L))))
  expect_identical(a$holidays, list(from = -7L, to = 2L)); expect_false(a$holiday_annex)
  b <- env$prevcarga_inference_needs_of(fake_plugin("data.load.y"), spec, "SP")
  expect_identical(b$weather_forecast$segments, list())
})

test_that("of: data_needs do descriptor tem precedencia e passa pelo normalize", {
  p <- fake_plugin("data.load.y"); spec <- list(history_days = list(infer = 7L), horizon_days = 2L)
  p$descriptor$data_needs <- function(spec, area) list(infer = nd(load = 5, segs = list(seg("min_lead", -5, 0, 0)),
                                                                  hol = c(-5L, 2L), wf_area = area))
  r <- env$prevcarga_inference_needs_of(p, spec, "SP")
  expect_identical(r$load$days_before, 5L)
  p$descriptor$data_needs <- function(spec, area) list(infer = nd(segs = list(seg("latest", 0, 1))))
  expect_pc(env$prevcarga_inference_needs_of(p, spec, "SP"), "contract.inference_needs_invalid")
})

test_that("registry: data_needs opcional, assinatura (spec, area); ausente => NULL", {
  load1 <- function(dn) {
    root <- pc_toy_variant("toy-30", descriptor = if (is.null(dn)) list() else list(data_needs = dn))
    env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()
    env$prevcarga_plugin_load("toy-30", root)
  }
  expect_null(load1(NULL)$descriptor$data_needs)
  ok <- load1(function(spec, area) list(infer = list()))
  expect_identical(names(formals(ok$descriptor$data_needs)), c("spec", "area"))
  expect_pc(load1(function(area) NULL), "plugin.bad_signature")
  expect_pc(load1("nao-funcao"), "plugin.bad_descriptor")
})

## --- treino real: artefato grava inference_needs, versao inalterada, invalido aborta sem gravar ----------------------
tr_run <- function(descriptor, name = "toy-30") {
  rt <- tempfile("in-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
  vr <- pc_toy_variant("toy-30", run_yaml = list(backtest = list(protocol_default = "fixed"),
                                                 history_days = list(train = 5L, infer = 3L)),
                       descriptor = descriptor, name = name)
  file.copy(file.path(vr, name), file.path(rt, "models"), recursive = TRUE)
  file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
  dir <- tempfile("in-deck-")
  env$prevcarga_fixture_deck(dir, seed = 11, areas = "A", days = seq(O - 12L, O, by = "day"),
                             load_until = lubridate::ymd_hm("2026-10-01 09:00", tz = TZ))
  pc_deck_topology(file.path(dir, "topology.yaml"), "A")
  st <- tempfile("in-state-"); dir.create(file.path(st, "artifacts"), recursive = TRUE)
  withr::defer(unlink(c(rt, dir, st), recursive = TRUE), envir = parent.frame())
  ctx <- list(root = rt, data_dir = dir, topology_path = file.path(dir, "topology.yaml"),
              weather_issue_cutoff = "available_until", origin = O, state_dir = st,
              code_commit = "c0ffee", renv_lock_hash = "ab12")
  mem <- list(sigla = "T", mdir = name, exogena = NA_character_)
  list(ctx = ctx, mem = mem, st = st,
       train = function() suppressWarnings(env$prevcarga_train(mem, "A", O, ctx)))
}

DN <- function(spec, area) list(infer = list(schema_version = 1L, load = list(days_before = 5L),
  weather_observed = list(days_before = 5L),
  weather_forecast = list(area = area, segments = list(list(kind = "min_lead", from = -5L, to = 0L, lead = 0L))),
  holidays = list(from = -5L, to = spec$horizon_days), holiday_annex = FALSE))

test_that("artifact_build: grava inference_needs normalizado; versao igual a de um treino sem data_needs", {
  a <- tr_run(list(data_needs = DN)); b <- tr_run(list())
  ra <- a$train(); rb <- b$train()
  art <- env$prevcarga_load_artifact(dirname(ra$path), ra$version)
  H <- art$spec_efetivo$horizon_days
  expect_identical(art$inference_needs, list(schema_version = 1L, load = list(days_before = 5L),
    weather_observed = list(days_before = 5L),
    weather_forecast = list(area = "A", segments = list(seg("min_lead", -5L, 0L, 0L))),
    holidays = list(from = -5L, to = as.integer(H)), holiday_annex = FALSE))
  expect_identical(ra$version, rb$version)
  expect_true("inference_needs" %in% names(env$prevcarga_load_artifact(dirname(rb$path), rb$version)))
  expect_identical(env$prevcarga_load_artifact(dirname(rb$path), rb$version)$inference_needs$weather_forecast$segments,
                   if (any(startsWith(unlist(lapply(art$contract, `[[`, "requires")), "data.weather_forecast.")))
                     list(seg("freshest", 0L, as.integer(H))) else list())
})

test_that("artifact_build: data_needs invalido aborta antes de gravar (where = mdir, details.field)", {
  ## funcoes autocontidas (o descriptor e serializado para o plugin: sem closure)
  base_nd <- "list(schema_version = 1L, load = list(days_before = 5L), weather_observed = list(days_before = 5L),
    weather_forecast = list(area = area, segments = SEGS), holidays = list(from = HF, to = HT), holiday_annex = FALSE)"
  mk <- function(segs = "list()", hf = "-5L", ht = "2L", extra = "")
    eval(parse(text = sprintf("function(spec, area) list(infer = c(%s, %s))",
      sub("HT", ht, sub("HF", hf, sub("SEGS", segs, base_nd, fixed = TRUE), fixed = TRUE), fixed = TRUE),
      if (nzchar(extra)) extra else "list()")))
  bad <- list(mk(segs = "list(list(kind = 'latest', from = 0L, to = 1L))"), mk(hf = "3L", ht = "1L"), mk(extra = "list(zzz = 1)"))
  fields <- c("weather_forecast.segments.kind", "holidays.from", "zzz")
  for (i in seq_along(bad)) {
    s <- tr_run(list(data_needs = bad[[i]]))
    c <- expect_pc(s$train(), "contract.inference_needs_invalid", 2L)
    expect_identical(c$where, "toy-30"); expect_identical(c$details$field, fields[[i]])
    expect_false(any(grepl("artifact.rds", list.files(s$st, recursive = TRUE))))
  }
})

test_that("aggregate: uniao por area, segmentos radix deduplicados, anexo OR, independe da ordem", {
  rows <- data.table::data.table(member = c("a", "b", "c"), mdir = c("a", "b", "c"), area = "SP",
    needs = list(nd(load = 2L, segs = list(seg("freshest", 0L, 8L)), hol = c(-2L, 8L)),
                 nd(load = 5L, segs = list(seg("per_origin", -9L, 0L), seg("freshest", 0L, 8L)), hol = c(-5L, 3L)),
                 nd(load = 9L, annex = TRUE, hol = c(-9L, 1L))), source = "artifacts")
  ag <- env$prevcarga_inference_needs_aggregate(rows)
  expect_identical(ag$load$days_before, 9L)
  expect_identical(ag$weather_observed$days_before, 9L)
  expect_identical(ag$weather_forecast$kind, c("freshest", "per_origin"))
  expect_identical(ag$weather_forecast$area_code, c("SP", "SP"))
  expect_identical(ag$holidays$from, -9L); expect_identical(ag$holidays$to, 8L)
  expect_true(ag$holiday_annex)
  for (perm in list(c(3, 1, 2), c(2, 3, 1), c(3, 2, 1))) expect_identical(env$prevcarga_inference_needs_aggregate(rows[perm]), ag)
  rows2 <- data.table::copy(rows); rows2$area <- c("RJ", "SP", "RJ")
  ag2 <- env$prevcarga_inference_needs_aggregate(rows2)
  expect_identical(ag2$load$area_code, c("RJ", "SP")); expect_identical(ag2$load$days_before, c(9L, 5L))
})

test_that("resolve: artifacts exige o campo (missing nomeia membro/area/origem); topology nao le artefato", {
  rt <- tempfile("rs-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
  for (nm in c("m-new", "m-old")) {
    vr <- pc_toy_variant("toy-30", run_yaml = list(backtest = list(protocol_default = "fixed")), name = nm)
    file.copy(file.path(vr, nm), file.path(rt, "models"), recursive = TRUE)
  }
  file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
  withr::defer(unlink(rt, recursive = TRUE))
  tp <- file.path(rt, "topology.yaml")
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  X: {areas: [A], loss: PX}", "order: [X]",
               "members:", "  - {sigla: SNEW, mdir: m-new}", "  - {sigla: SOLD, mdir: m-old}"), tp)
  ctx <- list(root = rt, topology_path = tp)
  orig <- get("prevcarga_infer_resolve", envir = env)
  withr::defer(assign("prevcarga_infer_resolve", orig, envir = env))
  assign("prevcarga_infer_resolve", function(member, area, ctx, ...)
    list(artifact = if (identical(member$mdir, "m-new")) list(inference_needs = nd(wf_area = area)) else list())
    , envir = env)
  c <- expect_pc(env$prevcarga_inference_needs_resolve(ctx, O, "A", "artifacts"), "contract.inference_needs_missing", 2L)
  expect_identical(c$details$member, "m-old"); expect_identical(c$details$area, "A")
  expect_identical(c$details$origin, "2026-10-01")
  assign("prevcarga_infer_resolve", function(...) stop("nao deve ler artefato"), envir = env)
  r <- env$prevcarga_inference_needs_resolve(ctx, O, "A", "topology")
  expect_identical(names(r), c("member", "mdir", "area", "needs", "source"))
  expect_identical(r$member, c("m-new", "m-old")); expect_true(all(r$source == "topology"))
  expect_identical(r$needs[[1L]]$schema_version, 1L)
})

test_that("inference_needs.R nao cita nome de modelo nem env var", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "inference_needs.R"))
  src <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("lgbm|dessem|holt|random|Sys\\.getenv|Sys\\.time", src, ignore.case = TRUE)))
})
