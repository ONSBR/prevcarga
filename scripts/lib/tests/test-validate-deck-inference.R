## test-validate-deck-inference.R -- validate-deck e forecast com deck kind inference (E2-T5). Fixture sintetica pequena em tempdir:
## deck inference (manifest kind inference gravado a mao) + artefatos-brinquedo com inference_needs no registry apontado.
## O check de necessidade AGREGADA (completude.need_gap) substitui o check de janela por history_days; deck train fica igual.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
for (m in c("errors", "runtime", "config", "banner", "cli"))
  sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())
lib <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "inference_needs", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2023-06-01")
AREAS <- c("RJ", "RS")                       # folhas de config/topology.yaml
HASH <- strrep("a", 64)

tmp <- function(env) { d <- tempfile("vdi"); dir.create(d); withr::defer(unlink(d, recursive = TRUE), envir = env); d }

## raiz copia (config + models + scripts/lib sem tests); `topology` opcional substitui config/topology.yaml
root_copy <- function(env, topology = NULL) {
  d <- tmp(env)
  file.copy(file.path(pc_root(), c("config", "models")), d, recursive = TRUE)
  dir.create(file.path(d, "scripts"))
  file.copy(file.path(pc_root(), "scripts", "lib"), file.path(d, "scripts"), recursive = TRUE)
  unlink(file.path(d, "scripts", "lib", "tests"), recursive = TRUE)
  if (!is.null(topology)) writeLines(topology, file.path(d, "config", "topology.yaml"))
  hd <- list(`holt-winters` = c(30, 10), `random-forest` = c(30, 10), lgbm = c(20, 5), `dessem-regdin` = c(20, 20),
             `dessem-linear` = c(20, 20))                 # historicos pequenos (deck train de 40 dias)
  for (m in names(hd)) {
    f <- file.path(d, "models", m, "run.yaml"); y <- yaml::read_yaml(f)
    y$history_days <- list(train = hd[[m]][1L], infer = hd[[m]][2L]); yaml::write_yaml(y, f)
  }
  d
}

## necessidade-brinquedo (schema v1): carga/observado 3 dias, previsto min_lead lead 1 em O+1 e freshest [0,1], feriados [-3, +2]
needs_toy <- function(area, annex = FALSE) list(
  schema_version = 1L, load = list(days_before = 3L), weather_observed = list(days_before = 3L),
  weather_forecast = list(area = area, segments = list(
    list(kind = "min_lead", from = 1L, to = 1L, lead = 1L), list(kind = "freshest", from = 0L, to = 1L))),
  holidays = list(from = -3L, to = 2L), holiday_annex = annex)

## deck inference: carga 4 dias ate O-1 (O 00:00 = available_until), clima observado e previsto, feriados, manifest kind inference
## `drop_load_day` tira o dia inteiro da carga da area; `drop_fc_day` tira toda a linha de previsto cujo dia valido e esse.
deck_fixture <- function(env, drop_load_day = NULL, drop_load_area = "RS", drop_fc_day = NULL, hol = c("2023-05-25", "2023-06-10")) {
  d <- tmp(env)
  days <- seq(O - 4L, O - 1L, by = "day")
  pc_deck_sentinel("end", days = days, areas = AREAS, dir = d)
  h <- pc_deck_holidays_default(AREAS)
  h[which(as.numeric(simplified_code) < 0), simplified_code := bit64::as.integer64(1L)]
  .pc_deck_write(h, d, file.path("auxiliary", "holidays"), character(), "date")
  pc_deck_forecast_extend(d, O, O + 1L, issue_hour_utc = 0L)
  if (!is.null(drop_load_day)) {
    dd <- lubridate::ymd(drop_load_day)
    st <- lubridate::force_tz(lubridate::as_datetime(dd), TZ) + lubridate::minutes(30L * (0:47))
    pc_deck_drop(d, drop_load_area, st)
  }
  if (!is.null(drop_fc_day)) {
    fc <- .pc_deck_read(d, "weather_forecast")
    v <- .pc_deck_view(fc, "weather_forecast", d)
    .pc_deck_rewrite(fc[lubridate::as_date(v$valid_time, tz = TZ) != lubridate::ymd(drop_fc_day)], d, "weather_forecast")
  }
  pc_deck_manifest(d, mutate = function(m) {
    m$kind <- "inference"
    m$inference <- list(origin = format(O), available_until = paste0(format(O), "T00:00:00"), train_deck_hash = HASH,
                        windows = list(holidays = list(from = hol[[1L]], to = hol[[2L]])))
    m
  })
  d
}

## publica um artefato-brinquedo por membro x area no registry apontado `art` (+ latest); `needs` = function(area) -> lista | NULL
publish <- function(art, members, areas, needs = needs_toy, skip = function(m, a) FALSE) {
  ctx <- list(artifacts_dir = art)
  for (i in seq_len(nrow(members))) {
    m <- as.list(members[i, , drop = FALSE])
    for (a in areas) {
      key <- lib$prevcarga_registry_key(m, a, ctx)
      v <- paste0("v_", substr(digest::digest(paste(m$sigla, a), algo = "sha256"), 1L, 16L))
      nd <- if (skip(m, a)) NULL else needs(a)
      art_obj <- list(model = m$mdir)
      if (!is.null(nd)) art_obj$inference_needs <- nd
      lib$prevcarga_registry_publish(art_obj, list(origin = format(O)), key, v)
      lib$prevcarga_registry_promote_latest(key, v)
    }
  }
  invisible(art)
}

run_v <- function(root, argv) {
  err <- character(); code <- NA_integer_
  out <- utils::capture.output({
    err <- utils::capture.output({
      code <- prevcarga_main(argv, root = root, runtime = function() invisible(NULL))
    }, type = "message")
  }, type = "output")
  list(code = code, out = out, err = err)
}
jdoc <- function(run) jsonlite::fromJSON(paste(run$out, collapse = "\n"), simplifyVector = FALSE)

setup_run <- function(env, deck) {
  root <- root_copy(env)
  art <- tmp(env)
  publish(art, lib$prevcarga_members(root = root), AREAS)
  list(root = root, art = art, deck = deck)
}
vd_json <- function(s, extra = character())
  run_v(s$root, c("validate-deck", "--format=json", paste0("--data-inference=", s$deck), paste0("--data-artifacts=", s$art),
                  paste0("--origin=", format(O)), extra))

test_that("[AC0] falta de O-3 na carga: exit 3 need_gap com dataset, area, dias e origin", {
  s <- setup_run(environment(), deck_fixture(environment(), drop_load_day = "2023-05-29", drop_load_area = "RS"))
  r <- vd_json(s)
  expect_identical(r$code, 3L)
  j <- jdoc(r)
  expect_identical(j$error$id, "completude.need_gap")
  expect_identical(j$error$details$dataset, "load")
  expect_identical(j$error$details$area, "RS")
  expect_identical(j$error$details$dias, "2023-05-29")
  expect_identical(j$error$details$origin, "2023-06-01")
  expect_false(any(vapply(j$error$details$items, function(x) identical(x$what, "window_infer_deficit"), NA)))
})

test_that("[AC0] deck completo: exit 0 e inference_windows com from/to por dataset e area; texto traz a tabela", {
  s <- setup_run(environment(), deck_fixture(environment()))
  r <- vd_json(s)
  expect_identical(r$code, 0L)
  iw <- jdoc(r)$result$inference_windows
  key <- vapply(iw, function(x) paste(x$dataset, x$area), "")
  expect_identical(key, sort(key, method = "radix"))
  ld <- Filter(function(x) x$dataset == "load" && x$area == "RS", iw)[[1L]]
  expect_identical(ld$from, "2023-05-29"); expect_identical(ld$to, "2023-05-31"); expect_identical(ld$source, "artifacts")
  wf <- Filter(function(x) x$dataset == "weather_forecast" && x$area == "RJ", iw)[[1L]]
  expect_identical(wf$from, "2023-06-01"); expect_identical(wf$to, "2023-06-02")
  expect_setequal(unique(vapply(iw, function(x) x$dataset, "")), c("load", "weather_observed", "weather_forecast", "holidays"))
  rt <- run_v(s$root, c("validate-deck", paste0("--data-inference=", s$deck), paste0("--data-artifacts=", s$art),
                        paste0("--origin=", format(O))))
  expect_identical(rt$code, 0L)
  expect_true(any(grepl("^DATASET +AREA +FROM +TO$", rt$out)))
  expect_false(any(as.integer(unlist(lapply(rt$out, charToRaw))) > 127L))
})

test_that("[AC1] falta da emissao min_lead lead 1 para O+1: need_gap weather_forecast com o dia O+1; sem window_infer_deficit", {
  s <- setup_run(environment(), deck_fixture(environment(), drop_fc_day = "2023-06-02"))
  r <- vd_json(s)
  expect_identical(r$code, 3L)
  j <- jdoc(r)
  expect_identical(j$error$id, "completude.need_gap")
  expect_identical(j$error$details$dataset, "weather_forecast")
  expect_identical(j$error$details$area, "RJ")
  expect_identical(j$error$details$dias, "2023-06-02")
  expect_false(any(vapply(j$error$details$items, function(x) identical(x$what, "window_infer_deficit"), NA)))
})

test_that("check de needs: janela de feriados do manifest curta e anexo pedido e ausente viram need_gap (puro)", {
  d <- deck_fixture(environment(), hol = c("2023-05-30", "2023-06-10"))
  ctx <- list(data_dir = d, root = pc_root())
  man <- lib$prevcarga_deck_manifest(d)
  cuts <- list(weather_issue_limit = lubridate::force_tz(lubridate::as_datetime(O), TZ))
  rows <- data.table::data.table(member = "m", mdir = "m", area = "RS", needs = list(needs_toy("RS", annex = TRUE)), source = "artifacts")
  chk <- lib$.prevcarga_check_inference_needs(ctx, O, cuts, lib$prevcarga_inference_needs_aggregate(rows), TZ, man)
  f <- chk$findings
  expect_identical(f$detail[f$dataset == "holidays"], "2023-05-29")
  expect_identical(f$what[f$dataset == "holiday_annex"], "need_gap")
  expect_true(all(f$class == "completude" & f$level == "error"))
  expect_identical(names(chk$windows), c("dataset", "area", "from", "to", "source"))
})

test_that("[AC2] forecast: artefato sem inference_needs => exit 2 nomeando membro, area e origem, zero chamadas de plugin (com e sem skip-validate)", {
  env <- environment()
  topo <- c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  S: {areas: [RS, RJ], loss: PS}", "order: [S]",
            "members:", "  - {sigla: RF, mdir: random-forest, exogena: null}", "  - {sigla: HW, mdir: holt-winters, exogena: null}")
  root <- root_copy(env, topo)
  tp <- file.path(root, "config", "topology.yaml")
  deck <- deck_fixture(env)
  mem <- lib$prevcarga_members(path = tp, root = root)
  art <- tmp(env)
  publish(art, mem, AREAS, skip = function(m, a) identical(m$sigla, "RF"))
  calls <- new.env(); calls$n <- 0L
  for (nm in c("prevcarga_plugin_infer", "prevcarga_infer_cell", "prevcarga_train")) local({
    f <- lib[[nm]]; withr::defer(assign(nm, f, envir = lib), envir = env)
    assign(nm, function(...) { calls$n <- calls$n + 1L; stop("nao deve chamar ", nm) }, envir = lib)
  })
  for (skip in c(TRUE, FALSE)) {
    ctx <- list(root = root, topology_path = tp, data_dir = deck, data_role = "inference", artifacts_dir = art,
                state_dir = tmp(env), weather_issue_cutoff = "available_until", origin = O, version = "0.0.1",
                code_commit = "c0ffee", generated_at = "2023-06-01T00:00:00Z")
    e <- tryCatch(lib$prevcarga_forecast(ctx, origin = O, skip_validate = skip, escopo = "sin"), prevcarga_error = identity)
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, "contract.inference_needs_missing")
    expect_identical(e$exit_code, 2L)
    expect_identical(e$details$member, "random-forest")
    expect_identical(e$details$area, "RJ")
    expect_identical(e$details$origin, "2023-06-01")
  }
  expect_identical(calls$n, 0L)
})

test_that("[AC2] forecast com falta no deck: completude.need_gap (exit 3) mesmo com skip-validate, antes de qualquer plugin", {
  env <- environment()
  topo <- c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  S: {areas: [RS, RJ], loss: PS}", "order: [S]",
            "members:", "  - {sigla: HW, mdir: holt-winters, exogena: null}")
  root <- root_copy(env, topo)
  tp <- file.path(root, "config", "topology.yaml")
  deck <- deck_fixture(env, drop_load_day = "2023-05-29", drop_load_area = "RS")
  art <- tmp(env)
  publish(art, lib$prevcarga_members(path = tp, root = root), AREAS)
  calls <- new.env(); calls$n <- 0L
  f0 <- lib$prevcarga_infer_cell; withr::defer(assign("prevcarga_infer_cell", f0, envir = lib))
  assign("prevcarga_infer_cell", function(...) { calls$n <- calls$n + 1L; stop("nao deve inferir") }, envir = lib)
  ctx <- list(root = root, topology_path = tp, data_dir = deck, data_role = "inference", artifacts_dir = art,
              state_dir = tmp(env), weather_issue_cutoff = "available_until", origin = O, version = "0.0.1",
              code_commit = "c0ffee", generated_at = "2023-06-01T00:00:00Z")
  e <- tryCatch(lib$prevcarga_forecast(ctx, origin = O, skip_validate = TRUE, escopo = "sin"), prevcarga_error = identity)
  expect_identical(e$id, "completude.need_gap")
  expect_identical(e$exit_code, 3L)
  expect_identical(e$details$dataset, "load"); expect_identical(e$details$area, "RS")
  expect_identical(e$details$dias, "2023-05-29"); expect_identical(e$details$origin, "2023-06-01")
  expect_identical(calls$n, 0L)
})

test_that("[AC3] deck train: JSON sem inference_windows e check de janela por history_days intacto", {
  d <- tmp(environment())
  days <- seq(lubridate::ymd("2026-10-01") - 39L, lubridate::ymd("2026-10-01"), by = "day")
  pc_deck_sentinel("end", days = days, areas = AREAS, dir = d)
  h <- pc_deck_holidays_default(AREAS)
  h[which(as.numeric(simplified_code) < 0), simplified_code := bit64::as.integer64(1L)]
  .pc_deck_write(h, d, file.path("auxiliary", "holidays"), character(), "date")
  pc_deck_forecast_extend(d, lubridate::ymd("2026-10-01"), lubridate::ymd("2026-10-10"))
  root <- root_copy(environment())
  r <- run_v(root, c("validate-deck", "--format=json", paste0("--data-train=", d)))
  j <- jdoc(r)
  expect_false("inference_windows" %in% names(j$result))
  expect_false(is.null(j$result$cuts))
  v <- lib$prevcarga_validate_deck(list(root = root, data_dir = d, data_role = "train", weather_issue_cutoff = "available_until"), origin = lubridate::ymd("2026-10-01"))
  expect_null(v$inference_windows)
})
