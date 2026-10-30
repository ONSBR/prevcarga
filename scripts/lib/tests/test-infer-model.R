## test-infer-model.R -- fachada de inferencia prevcarga_infer / prevcarga_infer_cell e verbo `infer-model` (M5.5; ADR-0023,
## ADR-0039, ADR-0040). Deck sintetico (fixture_store.R) -> treino M5.3 -> registry (M5.2) -> inferencia. Plugins-brinquedo
## com estagio de inferencia real (zz_dispatch.R do P.0c + zz_read.R que le o dia O): os modelos reais ainda nao tem todas as
## etapas (so o RF tem inferencia). Cada braco FORBIDDEN tem o seu CONTROL. Tudo em tempdir; sem variaveis de ambiente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                       "registry", "features", "training", "inference"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
cli <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli"))
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
AREAS <- c("A", "B")
at <- function(hm, day) lubridate::ymd_hm(paste(day, hm), tz = TZ)

expect_pc <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$exit_code, as.integer(exit))
  invisible(cond)
}

## --- raizes de modelos: brinquedos com inferencia que LE o dia O (so o toy-30; o toy-60 usa a grade) ---------------------
IM_READ <- c(
  "toy_inference_features <- function(frame, payload) {",
  "  ld <- frame$load",
  "  list(area = ld$area_code[1L], today = mean(utils::tail(ld$load_mwh, 4L)))",
  "}",
  "toy_inference_core <- function(payload, inf_features, cuts) {",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), toy_grid_end(cuts, 2L), by = \"30 min\")",
  "  list(area = inf_features$area, values = payload$model$level,",
  "       delivery = data.table::data.table(interval_end = ie,",
  "                                         y_pred = payload$model$level[toy_slots(ie)] + inf_features$today))",
  "}")
VAR_SCHEMA <- list(k = list(type = "integer", min = 1L), tag = list(type = "character"))

im_def <- function(base, run_yaml = list(), descriptor = list(), read = FALSE) {
  list(base = base, run_yaml = run_yaml, descriptor = descriptor, read = read,
       native = if (identical(base, "toy-60")) 60L else 0L)        # o toy-60 declara model_time_offset_min = 60
}
IM_DEFS <- list(
  "toy-30" = im_def("toy-30", descriptor = list(uses_rng = TRUE), read = TRUE),
  "toy-lg" = im_def("toy-30", list(horizon_days = 1L), list(uses_rng = TRUE, data_lag_days = 2L), read = TRUE),
  "toy-w5" = im_def("toy-30", list(history_days = list(train = 10L, infer = 5L)), list(uses_rng = TRUE), read = TRUE),
  "toy-var" = im_def("toy-30", list(params = list(k = 3L, tag = "MED")),
                     list(uses_rng = TRUE, variant_param = "tag", params_schema = VAR_SCHEMA), read = TRUE),
  "toy-60" = im_def("toy-60"),
  "toy-60s30" = im_def("toy-60", list(granularity_min = 30L), list(granularity_supported = c(30L, 60L))),
  "toy-60a" = im_def("toy-60", list(split = "origin_agnostic", modes = c("train", "infer"))))

.im_roots <- new.env()
im_models <- function(defs = IM_DEFS) {
  key <- paste(names(defs), collapse = ",")
  if (!is.null(.im_roots[[key]])) return(.im_roots[[key]])
  rt <- tempfile("im-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
  for (nm in names(defs)) {
    d <- defs[[nm]]
    files <- list("R/zz_dispatch.R" = .pc_toy_dispatch_lines(d$base, native_min = d$native))
    if (d$read) files[["R/zz_read.R"]] <- IM_READ
    vr <- pc_toy_variant(d$base, run_yaml = modifyList(list(backtest = list(protocol_default = "fixed")), d$run_yaml),
                         descriptor = d$descriptor, files = files, name = nm)
    file.copy(file.path(vr, nm), file.path(rt, "models"), recursive = TRUE)
  }
  file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
  .im_roots[[key]] <- rt
  rt
}

.im_decks <- new.env()
im_deck <- function() {
  if (!is.null(.im_decks$d)) return(.im_decks$d)
  dir <- tempfile("im-deck-")
  env$prevcarga_fixture_deck(dir, seed = 11, areas = AREAS, days = seq(O - 13L, O, by = "day"),
                             load_until = at("09:00", "2026-10-01"))
  pc_deck_topology(file.path(dir, "topology.yaml"), AREAS)
  .im_decks$d <- dir
  dir
}
im_bump <- function(dir, after, at_or_before = NULL)
  env$prevcarga_fixture_perturb(dir, "load", after = after, at_or_before = at_or_before, perturb = function(v) v + 1000)
im_day <- function(dir, day) im_bump(dir, at("00:00", day) - lubridate::minutes(1L), at("23:30", day))

im_ctx <- function(dir, root, state, ...)
  c(list(root = root, data_dir = dir, topology_path = file.path(dir, "topology.yaml"),
         weather_issue_cutoff = "available_until", origin = O, state_dir = state,
         code_commit = "c0ffee", renv_lock_hash = "ab12"), list(...))
im_member <- function(mdir, exogena = NA_character_) list(sigla = toupper(mdir), mdir = mdir, exogena = exogena)

im_setup <- function(mdir, deck = im_deck(), exogena = NA_character_, defs = IM_DEFS) {
  rt <- im_models(defs)
  st <- pc5_state_tmp(.local_envir = parent.frame())
  list(mem = im_member(mdir, exogena), ctx = im_ctx(deck, rt, st), deck = deck, root = rt, state = st)
}
im_train <- function(s, area = "A", mem = s$mem) suppressWarnings(env$prevcarga_train(mem, area, O, s$ctx))
im_infer <- function(s, area = "A", mem = s$mem, ...) suppressWarnings(env$prevcarga_infer_cell(mem, area, O, s$ctx, ...))
im_with_deck <- function(s, deck) { s$ctx$data_dir <- deck; s$ctx$topology_path <- file.path(deck, "topology.yaml"); s }

## espioes: substituem a funcao no env e restauram ao fim do frame do chamador
im_spy <- function(name, frame = parent.frame()) {
  spy <- new.env(); spy$n <- 0L; spy$args <- list()
  orig <- get(name, envir = env)
  assign(name, function(...) {
    spy$n <- spy$n + 1L; spy$args[[length(spy$args) + 1L]] <- list(...)
    orig(...)
  }, envir = env)
  withr::defer(assign(name, orig, envir = env), envir = frame)
  spy
}

verbs <- cli$prevcarga_verbs_discover()
vrow <- verbs[verbs$verb == "infer-model", ]

## =====================================================================================================
## AC1 -- Usage sem --horizon/--exogena; prevcarga_verbs lista infer-model; docopt recusa => exit 8
## =====================================================================================================
test_that("AC1: o registro do verbo e a tabela prevcarga_verbs; usage/options sem --horizon/--exogena", {
  expect_identical(nrow(vrow), 1L)
  expect_true(is.function(vrow$handler[[1L]]))
  expect_identical(vrow$owner, "M5")
  txt <- c(vrow$usage, unlist(vrow$options))
  expect_false(any(grepl("--horizon|--exogena", txt)))
  expect_true(any(grepl("--variant", txt)) && any(grepl("--artifact-version", txt)) && any(grepl("--pin", txt)))
  help <- cli$prevcarga_usage(verbs)
  expect_true(grepl("infer-model", help, fixed = TRUE))
  ## so as linhas do proprio infer-model: train-models (M5.4) declara --horizon/--exogena legitimamente
  im_lines <- grep("infer-model", strsplit(help, "\n", fixed = TRUE)[[1L]], value = TRUE, fixed = TRUE)
  expect_true(length(im_lines) > 0L)
  expect_false(any(grepl("--horizon|--exogena", im_lines)))
})

root_cli <- pc_install_copy()
im_cli <- function(args, fmt = NULL) pc_cli(root_cli, c("infer-model", args, if (!is.null(fmt)) c("--format", fmt)))

## data valido + state vazio: o unico motivo possivel de exit 8 config.usage e a flag (nao a falta de --data)
im_cli_data <- function(args, fmt = "json")
  im_cli(c(args, paste0("--data=", im_deck()), paste0("--state=", pc5_state_tmp(parent.frame()))), fmt)
im_err_id <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)$error$id

test_that("AC1: infer-model --model lgbm --area SECO --horizon 3 => exit 8 config.usage (flag escopada); stdout vazio (texto)", {
  r <- im_cli_data(c("--model", "lgbm", "--area", "SECO", "--horizon", "3"), NULL)
  expect_identical(r$status, 8L)
  expect_length(r$stdout, 0L)
  expect_true(any(grepl("[config]", r$stderr, fixed = TRUE)))
  pc_assert_ascii(r$raw, "infer-model --horizon")
})

test_that("AC1: o mesmo com --format json => um unico JSON com error.id = config.usage; --exogena idem", {
  for (flag in c("--horizon", "--exogena")) {
    r <- im_cli_data(c("--model", "lgbm", "--area", "SECO", flag, "3"))
    expect_identical(r$status, 8L)
    expect_length(r$stdout, 1L)
    j <- jsonlite::fromJSON(r$stdout, simplifyVector = FALSE)
    expect_identical(j$error$class, "config")
    expect_identical(j$error$id, "config.usage")
    expect_identical(j$error$where, flag)
    expect_false(j$ok)
  }
})

test_that("AC1 [CONTROL]: o mesmo comando SEM a flag passa do guard de uso (nao e config.usage nem data_dir_missing)", {
  r <- im_cli_data(c("--model", "lgbm", "--area", "SECO"))
  expect_false(im_err_id(r) %in% c("config.usage", "config.data_dir_missing"))
  r2 <- im_cli_data(c("--model", "lgbm", "--area", "SECO", "--variant", "MED"))
  expect_false(identical(im_err_id(r2), "config.usage"))
})

test_that("AC1 [CONTROL]: train-models continua aceitando --horizon/--exogena (so o infer-model recusa)", {
  r <- pc_cli(root_cli, c("train-models", "--model=lgbm", "--area=SECO", "--horizon", "3", "--dry-run", "--format=json",
                          paste0("--data=", im_deck()), paste0("--state=", pc5_state_tmp())))
  id <- if (length(r$stdout)) im_err_id(r)
  expect_false(identical(id, "config.usage"))
})

## =====================================================================================================
## AC2 -- so le o artefato (FORBIDDEN dados de treino); read-only; CONTROL dia O; nunca plugin_train
## =====================================================================================================
test_that("AC2: perturbar o treino (dias < last_data_day) nao muda y_pred; nunca chama plugin_train; registry read-only", {
  s <- im_setup("toy-lg")
  tr <- im_train(s)
  base <- im_infer(s)
  h0 <- pc5_tree_hash(file.path(s$state, "artifacts"))
  sp_t <- im_spy("prevcarga_plugin_train")
  sp_p <- im_spy("prevcarga_train")
  s2 <- im_with_deck(s, im_day(s$deck, "2026-09-20"))
  pert <- im_infer(s2)
  expect_lte(max(abs(pert$predictions$y_pred - base$predictions$y_pred)), 1e-5)
  expect_identical(pert$predictions$target_datetime, base$predictions$target_datetime)
  expect_identical(sp_t$n, 0L); expect_identical(sp_p$n, 0L)
  expect_identical(pc5_tree_hash(file.path(s$state, "artifacts")), h0)
  expect_identical(base$version, tr$version)
})

test_that("AC2 [CONTROL]: perturbar a carga <= available_until do dia O muda y_pred", {
  s <- im_setup("toy-lg")
  im_train(s)
  base <- im_infer(s)
  s3 <- im_with_deck(s, im_bump(s$deck, at("00:00", "2026-10-01") - lubridate::minutes(1L)))
  ctl <- im_infer(s3)
  expect_gt(max(abs(ctl$predictions$y_pred - base$predictions$y_pred)), 1)
})

## =====================================================================================================
## AC3 -- resolucao latest | version | pin | inexistente | malformada
## =====================================================================================================
test_that("AC3: default = latest.txt; --artifact-version = v_<x>; --pin = pinada; v_00 => exit 13; ../x => exit 2", {
  s <- im_setup("toy-30")
  v <- character()
  for (day in c("2026-09-22", "2026-09-23", "2026-09-24")) {                 # 3 versoes da celula (decks distintos)
    sd <- im_with_deck(s, im_day(s$deck, day))
    v <- c(v, im_train(sd)$version)
  }
  expect_identical(length(unique(v)), 3L)
  key <- file.path(s$state, "artifacts", "toy-30", "A")
  writeLines(v[[2L]], file.path(key, ".pin"))
  d <- im_infer(s)
  expect_identical(d$version, v[[3L]]); expect_identical(d$resolved_by, "latest")
  e <- im_infer(s, version = v[[1L]])
  expect_identical(e$version, v[[1L]]); expect_identical(e$resolved_by, "version")
  p <- im_infer(s, pin = TRUE)
  expect_identical(p$version, v[[2L]]); expect_identical(p$resolved_by, "pin")
  expect_pc(im_infer(s, version = "v_00"), "state.artifact_missing", 13L)
  expect_pc(im_infer(s, version = "../x"), "contract.version_malformed", 2L)
  s0 <- im_setup("toy-30")
  expect_pc(im_infer(s0), "state.artifact_missing", 13L)                    # celula sem artefato: nunca saida vazia
})

## =====================================================================================================
## AC4 -- variante obrigatoria so com EXOGENA
## =====================================================================================================
test_that("AC4: dessem-linear sem --variant => exit 2 contract.variant_required; com MED resolve .../dessem-linear/MED/SECO/latest.txt", {
  st <- pc5_state_tmp()
  ctx <- list(root = pc_root(), state_dir = st)
  m <- im_member("dessem-linear")
  expect_pc(env$prevcarga_infer_resolve(m, "SECO", ctx), "contract.variant_required", 2L)
  cond <- expect_pc(env$prevcarga_infer_resolve(m, "SECO", ctx, variant = "MED"), "state.artifact_missing", 13L)
  expect_true(endsWith(cond$where, "artifacts/dessem-linear/MED/SECO/latest.txt"))
  expect_pc(env$prevcarga_infer_resolve(m, "SECO", ctx, variant = "ZZZ"), "config.usage", 8L)
  ## CONTROL: o RF (sem exogena) nao pede variante; a falta e so do artefato
  rf <- expect_pc(env$prevcarga_infer_resolve(im_member("random-forest"), "SECO", ctx), "state.artifact_missing", 13L)
  expect_true(endsWith(rf$where, "artifacts/random-forest/SECO/latest.txt"))
  expect_pc(env$prevcarga_infer_resolve(im_member("random-forest"), "SECO", ctx, variant = "MED"), "config.usage", 8L)
})

test_that("AC4: celula com variante (toy-var) infere com --variant e recusa sem ela; variante do artefato divergente => exit 2", {
  s <- im_setup("toy-var", exogena = "MED")
  im_train(s)
  expect_pc(im_infer(s, mem = im_member("toy-var")), "contract.variant_required", 2L)
  ok <- im_infer(s, mem = im_member("toy-var"), variant = "MED")
  expect_identical(ok$variant, "MED")
  ## artefato MED copiado para a celula MAX => contract.variant_mismatch (DL.7 lacuna 1); CONTROL: MED roda
  src <- file.path(s$state, "artifacts", "toy-var", "MED", "A")
  dst <- file.path(s$state, "artifacts", "toy-var", "MAX", "A")
  dir.create(dirname(dst), recursive = TRUE)
  file.copy(src, dirname(dst), recursive = TRUE); file.rename(file.path(dirname(dst), "A"), dst)
  expect_pc(im_infer(s, mem = im_member("toy-var"), variant = "MAX"), "contract.variant_mismatch", 2L)
})

## =====================================================================================================
## AC5 -- janela pelo spec DO ARTEFATO; sem literais nem switch por modelo
## =====================================================================================================
im_days_seen <- function(s, mdir = "toy-30") {
  sp <- im_spy("prevcarga_plugin_infer")
  im_infer(s, mem = im_member(mdir))
  p <- sp$args[[1L]][[3L]]$load
  length(unique(lubridate::as_date(p$interval_start, tz = TZ)))
}
test_that("AC5: a janela e history_days$infer do artefato (3 dias antes de O); editar o run.yaml atual nao muda", {
  s <- im_setup("toy-30")
  im_train(s)
  expect_identical(im_days_seen(s), 4L)                                       # O-3 .. O
  ry <- file.path(s$root, "models", "toy-30", "run.yaml")
  y0 <- yaml::read_yaml(ry)
  y <- y0; y$history_days$infer <- 6L
  yaml::write_yaml(y, ry)
  withr::defer(yaml::write_yaml(y0, ry))
  expect_identical(im_days_seen(s), 4L)                                       # artefato manda, nao o run.yaml atual
})

test_that("AC5 [CONTROL]: retreinar com infer=5 no run.yaml-fixture muda a janela", {
  s <- im_setup("toy-w5")
  im_train(s)
  expect_identical(im_days_seen(s, "toy-w5"), 6L)
})

test_that("AC5: inference.R sem literal 1825/730/45 e sem switch por nome de modelo", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "inference.R"), encoding = "UTF-8")
  code <- sub("#.*$", "", src)
  expect_false(any(grepl("\\b(1825|730|45)\\b", code)))
  expect_false(any(grepl("switch\\(", code)))
  expect_false(any(grepl("(holt|random-forest|lgbm|dessem)", code)))
})

test_that("AC5: spec passado diferente do efetivo do artefato => contract.infer_spec_divergent (exit 2); igual passa", {
  s <- im_setup("toy-30")
  im_train(s)
  sp <- im_spy("prevcarga_plugin_infer")
  im_infer(s)
  a <- sp$args[[1L]]
  plugin <- env$prevcarga_plugin_load("toy-30", file.path(s$root, "models"))
  key <- env$prevcarga_registry_key(s$mem, "A", s$ctx)
  art <- env$prevcarga_load_artifact(key, env$prevcarga_resolve_version(key, "latest"))
  panel <- a[[3L]]
  expect_s3_class(env$prevcarga_infer(plugin, art, panel, a[[4L]], list(), spec = art$spec_efetivo), "data.table")
  bad <- art$spec_efetivo; bad$params$k <- bad$params$k + 1L
  expect_pc(env$prevcarga_infer(plugin, art, panel, a[[4L]], list(), spec = bad), "contract.infer_spec_divergent", 2L)
})

test_that("artefato sem spec_efetivo ou fit => contract.artifact_spec_missing (exit 2), nao integrity; completo passa (CONTROL)", {
  s <- im_setup("toy-30")
  im_train(s)
  sp <- im_spy("prevcarga_plugin_infer")
  im_infer(s)
  a <- sp$args[[1L]]
  plugin <- env$prevcarga_plugin_load("toy-30", file.path(s$root, "models"))
  key <- env$prevcarga_registry_key(s$mem, "A", s$ctx)
  art <- env$prevcarga_load_artifact(key, env$prevcarga_resolve_version(key, "latest"))
  a1 <- art; a1$spec_efetivo <- NULL
  cond <- expect_pc(env$prevcarga_infer(plugin, a1, a[[3L]], a[[4L]], list()), "contract.artifact_spec_missing", 2L)
  expect_identical(cond$details$missing, "spec_efetivo")
  a2 <- art; a2$fit <- NULL
  expect_pc(env$prevcarga_infer(plugin, a2, a[[3L]], a[[4L]], list()), "contract.artifact_spec_missing", 2L)
  expect_s3_class(env$prevcarga_infer(plugin, art, a[[3L]], a[[4L]], list()), "data.table")
})

## =====================================================================================================
## AC6 -- integridade
## =====================================================================================================
test_that("AC6: 1 byte de artifact.rds alterado => exit 9 integrity.artifact_hash sem resultado; o original roda (CONTROL)", {
  s <- im_setup("toy-30")
  row <- im_train(s)
  f <- file.path(row$path, "artifact.rds")
  orig <- readBin(f, "raw", file.info(f)$size)
  bad <- orig; bad[[length(bad) %/% 2L]] <- as.raw(bitwXor(as.integer(bad[[length(bad) %/% 2L]]), 1L))
  writeBin(bad, f)
  res <- NULL
  cond <- expect_pc(res <- im_infer(s), "integrity.artifact_hash", 9L)
  expect_null(res)
  writeBin(orig, f)
  expect_identical(im_infer(s)$version, row$version)
})

## =====================================================================================================
## AC7 -- artefato oficial copiado no layout do bucket => delta 0
## =====================================================================================================
test_that("AC7: v_<hash>/ + latest.txt copiados para outro state/artifacts => predictions identical()", {
  s <- im_setup("toy-lg")
  row <- im_train(s)
  base <- im_infer(s)
  st2 <- pc5_state_tmp()
  dst <- file.path(st2, "artifacts", "toy-lg", "A")
  dir.create(dst, recursive = TRUE)
  file.copy(file.path(dirname(row$path), c(row$version, "latest.txt")), dst, recursive = TRUE)
  s2 <- s; s2$state <- st2; s2$ctx$state_dir <- st2
  copy <- im_infer(s2)
  expect_identical(copy$predictions, base$predictions)
  expect_identical(copy$version, base$version)
  file.remove(file.path(dst, "latest.txt"))                                   # CONTROL: sem latest.txt a celula nao resolve
  expect_pc(im_infer(s2), "state.artifact_missing", 13L)
})

## =====================================================================================================
## AC8 -- transparencia fachada == despachante direto nas 5 familias; CONTROL do dia O
## =====================================================================================================
for (m in c("toy-30", "toy-lg", "toy-60", "toy-60a", "toy-60s30")) {
  local({
    m <- m
    test_that(sprintf("AC8 [%s]: prevcarga_infer == prevcarga_plugin_infer direto (identical); dia O so muda quem le o dia O", m), {
      s <- im_setup(m)
      im_train(s)
      sp <- im_spy("prevcarga_plugin_infer")
      via <- im_infer(s)
      a <- sp$args[[1L]]
      plugin <- env$prevcarga_plugin_load(m, file.path(s$root, "models"))
      direct <- env$prevcarga_plugin_infer(plugin, a[[2L]], a[[3L]], a[[4L]], list())
      expect_identical(via$predictions$y_pred, direct$delivery$y_pred)
      expect_identical(via$predictions$target_datetime, direct$delivery$interval_end)
      s2 <- im_with_deck(s, im_bump(s$deck, at("00:00", "2026-10-01") - lubridate::minutes(1L)))
      pert <- im_infer(s2)
      reads_o <- m %in% c("toy-30", "toy-lg")
      expect_identical(isTRUE(all.equal(pert$predictions$y_pred, via$predictions$y_pred, tolerance = 1e-9)), !reads_o)
    })
  })
}

## =====================================================================================================
## AC9 -- data_lag_days do descriptor pela linha de corte do dates.R
## =====================================================================================================
test_that("AC9: linha de corte repassada ao despachante: LGBM-like lag 2 / last_data_day 2026-09-28; RF-like lag 0 / 2026-09-30", {
  for (cell in list(list(m = "toy-lg", lag = 2L, last = "2026-09-28"), list(m = "toy-30", lag = 0L, last = "2026-09-30"))) {
    s <- im_setup(cell$m)
    im_train(s)
    sp <- im_spy("prevcarga_plugin_infer")
    im_infer(s)
    cu <- sp$args[[1L]][[4L]]
    expect_identical(cu$data_lag_days, cell$lag)
    expect_identical(cu$last_data_day, lubridate::ymd(cell$last))
  }
  src <- paste(readLines(file.path(pc_root(), "scripts", "lib", "inference.R"), encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("data_lag_days\\s*(<-|=)\\s*[0-9]", sub("#[^\n]*", "", src)))
})

test_that("AC9: linha de corte injetada com data_lag_days divergente do descriptor => contract.cut_mismatch (exit 2) antes de plugin_infer", {
  s <- im_setup("toy-lg")
  im_train(s)
  orig <- get("prevcarga_resolve_cuts", envir = env)
  assign("prevcarga_resolve_cuts", function(...) { r <- orig(...); r$cuts$data_lag_days <- 0L; r }, envir = env)
  withr::defer(assign("prevcarga_resolve_cuts", orig, envir = env))
  sp <- im_spy("prevcarga_plugin_infer")
  expect_pc(im_infer(s), "contract.cut_mismatch", 2L)
  expect_identical(sp$n, 0L)
  assign("prevcarga_resolve_cuts", orig, envir = env)                         # CONTROL: sem injecao roda
  expect_identical(nrow(im_infer(s)$predictions) > 0L, TRUE)
})

## =====================================================================================================
## --out e verbo in-process (handler fino)
## =====================================================================================================
im_handler <- function() {
  h <- vrow$handler[[1L]]
  parent.env(environment(h)) <- env
  h
}
im_args <- function(...) modifyList(list(`--model` = "toy-lg", `--area` = "A"), list(...))

test_that("handler: JSON devolve o result (predictions ISO), texto devolve linhas ASCII; --out grava 7 colunas fora do registry", {
  s <- im_setup("toy-lg")
  im_train(s)
  h <- im_handler()
  ctx <- s$ctx; ctx$format <- "json"
  r <- h(ctx, im_args())
  expect_identical(r$model, "toy-lg"); expect_identical(r$origin, "2026-10-01"); expect_identical(r$resolved_by, "latest")
  expect_identical(r$first_target, "2026-10-01T09:30:00-03:00")
  expect_identical(r$available_until, "2026-10-01T09:00:00-03:00")
  expect_identical(r$predictions$target_datetime[[1L]], r$first_target)
  expect_false("available_until_posix" %in% names(r))
  ctx$format <- "text"
  lines <- h(ctx, im_args())
  expect_true(all(!grepl("[^\x01-\x7f]", lines)))
  expect_true(grepl("^infer-model model=toy-lg area=A origin=2026-10-01", lines[[1L]]))
  out <- file.path(tempfile("im-out-"), "p.parquet"); dir.create(dirname(out))
  h0 <- pc5_tree_hash(file.path(s$state, "artifacts"))
  h(ctx, im_args(`--out` = out))
  df <- as.data.frame(arrow::read_parquet(out))
  expect_identical(names(df), c("model", "variant", "area", "origin", "available_until", "target_datetime", "y_pred"))
  expect_identical(nrow(df), nrow(im_infer(s)$predictions))
  expect_identical(pc5_tree_hash(file.path(s$state, "artifacts")), h0)
  expect_identical(length(list.files(dirname(out), all.files = TRUE, no.. = TRUE)), 1L)
  ## --out inutilizavel: diretorio ausente e dentro do registry => io.out_unwritable (exit 6)
  expect_pc(h(ctx, im_args(`--out` = file.path(tempfile("nao-existe-"), "x.parquet"))), "io.out_unwritable", 6L)
  expect_pc(env$prevcarga_infer_write_out(im_infer(s), file.path(s$state, "artifacts", "x.parquet"), ctx),
            "io.out_unwritable", 6L)
  expect_pc(h(ctx, list(`--area` = "A")), "config.usage", 8L)
  expect_pc(h(ctx, im_args(`--model` = "nao-existe")), "contract.run_yaml_missing", 2L)
})

## =====================================================================================================
## Catalogo de erros do modulo (ADR-0040 item 8)
## =====================================================================================================
test_that("errors/inference.tsv carrega pelo loader sem id duplicado; contract.variant_required nao e redeclarado", {
  cat <- env$prevcarga_errors_load()
  expect_false(anyDuplicated(cat$id) > 0L)
  tsv <- utils::read.delim(file.path(pc_root(), "scripts", "lib", "errors", "inference.tsv"), colClasses = "character")
  expect_setequal(tsv$id, c("contract.infer_spec_divergent", "contract.variant_mismatch", "contract.artifact_spec_missing", "io.out_unwritable"))
  expect_false("contract.variant_required" %in% tsv$id)
  expect_identical(env$prevcarga_error_lookup("contract.infer_spec_divergent")$class, "contract")
  expect_identical(env$prevcarga_exit_codes[[env$prevcarga_error_lookup("io.out_unwritable")$class]], 6L)
  expect_true("contract.variant_required" %in% cat$id)
})
