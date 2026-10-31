## test-introspection.R -- comandos de introspeccao doctor/status/models/areas/describe (M1.5).
## Construtores puros in-process + fachada (prevcarga_main) sobre copias da instalacao em tempdir().

lib_dir <- file.path(pc_root(), "scripts", "lib")
withr::local_options(prevcarga.lib = lib_dir)

new_env <- function()
  pc_source_lib(c("errors", "runtime", "config", "banner", "cli", "topology", "provenance", "deck_reader",
                  "dates", "introspect"))
noop <- function() invisible(NULL)
verbs5 <- c("doctor", "status", "models", "areas", "describe")

mk_ctx <- function(E, root = pc_root(), args = list(), ...) {
  ctx <- E$prevcarga_ctx(args, root, NULL)
  extra <- list(...)
  for (n in names(extra)) ctx[[n]] <- extra[[n]]
  ctx
}

fixed_check <- function(found = NULL, eff = NULL, offenders = character()) {
  f <- list(tz = NA_character_, lc = "en_US.UTF-8", omp = NA_character_, openblas = NA_character_,
            mkl = NA_character_, veclib = NA_character_, blas_threads = 16L)
  e <- list(tz = "UTC", lc = "C.UTF-8", omp = "1", openblas = "1", mkl = "1", veclib = "1", blas_threads = 1L)
  for (n in names(found)) f[[n]] <- found[[n]]
  for (n in names(eff)) e[[n]] <- eff[[n]]
  function() list(ok = !length(offenders), found = f, effective = e, offenders = offenders)
}
all_pkgs <- function(p) TRUE

copy_install <- function() {
  d <- tempfile("install-"); dir.create(d)
  for (x in c("config", "models", "scripts")) file.copy(file.path(pc_root(), x), d, recursive = TRUE)
  d
}

## A fachada carrega `modules` em globalenv (como o shim real); por isso o main roda com a pilha da CLI em
## globalenv tambem (equivale ao source do shim). Os construtores in-process usam o env isolado E.
cli_global <- function() {
  if (!exists(".pc_introspection_cli", envir = globalenv()))
    for (m in c("errors", "runtime", "config", "banner", "cli"))
      sys.source(file.path(lib_dir, paste0(m, ".R")), envir = globalenv())
  assign(".pc_introspection_cli", TRUE, envir = globalenv())
  globalenv()
}

run_main <- function(E, argv, root = pc_root()) {
  E <- cli_global()
  err <- character(); code <- NA_integer_
  out <- utils::capture.output({
    err <- utils::capture.output({
      code <- E$prevcarga_main(argv, root = root, runtime = noop)
    }, type = "message")
  }, type = "output")
  list(code = code, out = out, err = err)
}

non_ascii <- function(x) any(unlist(lapply(x, function(l) any(as.integer(charToRaw(l)) > 127L))))

test_that("doctor conforme: schema, linhas, vocabulario e exit 0 (ac-7)", {
  E <- new_env()
  d <- E$prevcarga_build_doctor(mk_ctx(E), check = fixed_check(), has_pkg = all_pkgs)
  expect_identical(d$schema_version, 1L)
  expect_identical(names(d$checks), c("check", "status", "found", "effective"))
  expect_identical(d$checks$check, c("TZ", "locale", "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "pool BLAS", "versao do R", "docopt", "yaml", "jsonlite", "digest", "lubridate",
    "RhpcBLASctl", "arrow", "withr", "config.yaml", "data_dir"))
  expect_true(all(d$checks$status %in% c("PASS", "WARN", "FAIL", "N/A")))
  expect_identical(d$checks$status[d$checks$check == "data_dir"], "WARN")
  expect_identical(d$checks$found[d$checks$check == "data_dir"], "ausente")
  expect_identical(attr(d, "exit_code"), 0L)
  rv <- E$prevcarga_build_doctor(mk_ctx(E), check = fixed_check(), has_pkg = all_pkgs, r_version = "4.2.3")
  expect_identical(rv$checks$status[rv$checks$check == "versao do R"], "WARN")
  expect_identical(attr(rv, "exit_code"), 0L)
  noarrow <- E$prevcarga_build_doctor(mk_ctx(E), check = fixed_check(), has_pkg = function(p) p != "arrow")
  expect_identical(noarrow$checks$status[noarrow$checks$check == "arrow"], "WARN")
  expect_identical(attr(noarrow, "exit_code"), 0L)
})

test_that("doctor [CONTROL]: sujo nao e erro; falhas mantem o numero de linhas e o exit certo (ac-8)", {
  E <- new_env()
  base <- E$prevcarga_build_doctor(mk_ctx(E), check = fixed_check(), has_pkg = all_pkgs)
  n <- nrow(base$checks)
  bad_cfg <- structure(list(id = "config.bad_type"), class = "list")
  dirty <- E$prevcarga_build_doctor(mk_ctx(E), has_pkg = all_pkgs,
    check = fixed_check(found = list(tz = "America/Sao_Paulo", omp = "4")))
  expect_identical(attr(dirty, "exit_code"), 0L)
  expect_identical(dirty$checks$status[1:2], c("PASS", "PASS"))
  expect_identical(dirty$checks$found[1L], "America/Sao_Paulo")
  expect_identical(dirty$checks$effective[1L], "UTC")
  expect_identical(dirty$checks$found[3L], "4")
  expect_identical(dirty$checks$effective[3L], "1")
  blas <- E$prevcarga_build_doctor(mk_ctx(E), has_pkg = all_pkgs,
    check = fixed_check(eff = list(blas_threads = 4L), offenders = "BLAS"))
  expect_identical(blas$checks$status[blas$checks$check == "pool BLAS"], "FAIL")
  expect_identical(attr(blas, "exit_code"), 7L)
  nodoc <- E$prevcarga_build_doctor(mk_ctx(E), check = fixed_check(), has_pkg = function(p) p != "docopt")
  expect_identical(nodoc$checks$status[nodoc$checks$check == "docopt"], "FAIL")
  expect_identical(attr(nodoc, "exit_code"), 6L)
  cfg <- E$prevcarga_build_doctor(mk_ctx(E, config_error = bad_cfg), check = fixed_check(), has_pkg = all_pkgs)
  expect_identical(cfg$checks$status[cfg$checks$check == "config.yaml"], "FAIL")
  expect_identical(attr(cfg, "exit_code"), 8L)
  both <- E$prevcarga_build_doctor(mk_ctx(E, config_error = bad_cfg), has_pkg = all_pkgs,
    check = fixed_check(eff = list(blas_threads = 4L), offenders = "BLAS"))
  expect_identical(attr(both, "exit_code"), 7L)
  for (x in list(dirty, blas, nodoc, cfg, both)) {
    expect_identical(nrow(x$checks), n)
    txt <- E$prevcarga_render_doctor(x)
    expect_true(all(vapply(x$checks$check, function(ch) any(grepl(ch, txt, fixed = TRUE)), NA)))
  }
  ## fachada: o texto sai por completo ANTES do exit 7 (config quebrado: ctx tolerante)
  r <- run_main(E, c("doctor", "--format=json"))
  expect_true(r$code %in% c(0L, 6L, 7L))
  j <- jsonlite::fromJSON(paste(r$out, collapse = ""), simplifyVector = FALSE)
  expect_identical(length(j$result$checks), n + 7L)   # + 7 itens de bundle/artefatos do M8.7 (prevcarga_platform_check; N/A na dev)
})

test_that("status: sem deck, com manifest, manifest ilegivel, BLAS FAIL; exit 0 sempre (ac-9)", {
  E <- new_env()
  chk <- fixed_check()
  s0 <- E$prevcarga_build_status(mk_ctx(E), check = chk, has_pkg = all_pkgs)
  expect_identical(s0$deck$note, "sem deck")
  expect_true("deck: sem deck" %in% E$prevcarga_render_status(s0))
  expect_identical(s0$bundle, "N/A")
  expect_identical(attr(s0, "exit_code"), 0L)

  d <- tempfile("deck-"); dir.create(d)
  writeLines(jsonlite::toJSON(list(schema_version = 1, load_label = "start",
                                   timezone = list(load = "Etc/GMT+3")), auto_unbox = TRUE),
             file.path(d, "manifest.json"))
  s1 <- E$prevcarga_build_status(mk_ctx(E, data_dir = d), check = chk, has_pkg = all_pkgs)
  expect_identical(s1$deck$load_label, "start")
  expect_identical(s1$deck$timezone, "Etc/GMT+3")
  expect_identical(attr(s1, "exit_code"), 0L)
  expect_true(any(grepl("Etc/GMT+3", E$prevcarga_render_status(s1), fixed = TRUE)))

  d2 <- tempfile("deck-"); dir.create(d2)
  writeLines("{ nao e json", file.path(d2, "manifest.json"))
  s2 <- E$prevcarga_build_status(mk_ctx(E, data_dir = d2), check = chk, has_pkg = all_pkgs)
  expect_match(s2$deck$note, "ilegivel")
  expect_identical(attr(s2, "exit_code"), 0L)

  s3 <- E$prevcarga_build_status(mk_ctx(E), has_pkg = all_pkgs,
    check = fixed_check(eff = list(blas_threads = 4L), offenders = "BLAS"))
  expect_identical(s3$offenders, "pool BLAS")
  expect_true(any(grepl("pool BLAS", E$prevcarga_render_status(s3), fixed = TRUE)))
  expect_identical(s3$bundle, "N/A")
  expect_identical(attr(s3, "exit_code"), 0L)
})

test_that("models: um por membro identico ao spec; copia com horizon 3 aparece [CONTROL config-driven] (ac-10)", {
  E <- new_env()
  root <- pc_root()
  m <- E$prevcarga_build_models(mk_ctx(E))
  mem <- E$prevcarga_members(root = root)
  expect_identical(vapply(m$members, function(r) r$sigla, ""), mem$sigla)
  expect_identical(vapply(m$members, function(r) r$mdir, ""), mem$mdir)
  for (r in m$members) {
    s <- E$prevcarga_model_spec(r$mdir, root)
    expect_identical(r$split, s$split)
    expect_identical(r$granularity_min, s$granularity_min)
    expect_identical(r$horizon_days, s$horizon_days)
    expect_identical(r$history_days, s$history_days)
    expect_identical(r$seed, s$seed)
    expect_identical(unlist(r$modes), s$modes)
    expect_identical(r$protocol_default, s$backtest$protocol_default)
  }
  cp <- copy_install()
  f <- file.path(cp, "models", "lgbm", "run.yaml")
  writeLines(sub("horizon_days: 1", "horizon_days: 3", readLines(f)), f)
  m2 <- E$prevcarga_build_models(mk_ctx(E, root = cp))
  lg <- m2$members[[which(vapply(m2$members, function(r) r$sigla, "") == "LGBM")]]
  expect_identical(lg$horizon_days, 3L)
  expect_true(any(grepl("horizonte=3d", E$prevcarga_render_models(m2), fixed = TRUE)))
  expect_false(identical(m$members, m2$members))
})

test_that("areas: ordem, perda e khat_range da topologia; override de S na copia [CONTROL] (ac-11)", {
  E <- new_env()
  t <- E$prevcarga_topology(root = pc_root())
  a <- E$prevcarga_build_areas(mk_ctx(E))
  expect_identical(a$sin, "SIN")
  expect_identical(unlist(a$order), t$order)
  expect_identical(vapply(a$subsystems, function(s) s$name, ""), t$order)
  for (s in a$subsystems) {
    expect_identical(s$loss, t$subsystems[[s$name]]$loss)
    expect_identical(unlist(s$khat_range), t$subsystems[[s$name]]$khat_range)
    expect_identical(unlist(s$areas), t$subsystems[[s$name]]$areas)
  }
  txt <- E$prevcarga_render_areas(a)
  expect_true(any(grepl("S  perda=PES  khat_range=1.00-1.15", txt, fixed = TRUE)))
  cp <- copy_install()
  file.copy(file.path(pc_root(), "scripts", "lib", "tests", "fixtures", "topology_khat_override_s.yaml"),
            file.path(cp, "config", "topology.yaml"), overwrite = TRUE)
  a2 <- E$prevcarga_build_areas(mk_ctx(E, root = cp))
  kr <- stats::setNames(lapply(a2$subsystems, function(s) unlist(s$khat_range)), vapply(a2$subsystems, function(s) s$name, ""))
  expect_identical(kr$S, c(0.98, 1.20))
  expect_identical(kr$SECO, c(1.00, 1.15))
  expect_identical(kr$NE, c(1.00, 1.15))
  expect_true(any(grepl("S  perda=PES  khat_range=0.98-1.20", E$prevcarga_render_areas(a2), fixed = TRUE)))
})

test_that("describe: corte de treino por membro derivado em dates.R, so a origem e data de entrada; --model filtra; desconhecido = exit 8 (ac-12)", {
  E <- new_env()
  ctx <- mk_ctx(E, args = list(`--origin` = "2024-06-01"))
  d <- E$prevcarga_build_describe(ctx)
  txt <- E$prevcarga_render_describe(d)
  ## o placeholder do M1.5 saiu; entra a assercao positiva do corte por membro (sem deck: available_until indisponivel)
  expect_false(any(grepl("derivado no M2", txt, fixed = TRUE)))
  expect_true("cortes por membro:" %in% txt)
  expect_length(d$cuts$members, 5L)
  lg <- Filter(function(r) r$model == "lgbm", d$cuts$members)[[1L]]
  expect_identical(lg$train_origin, "2024-05-30"); expect_identical(lg$last_data_day, "2024-05-31")
  expect_true(any(grepl("available_until: derivado do deck (via validate-deck) [indisponivel", txt, fixed = TRUE)))
  js <- as.character(jsonlite::toJSON(d, auto_unbox = TRUE, null = "null"))
  j <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  expect_null(j$cuts$available_until)
  expect_identical(length(j$cuts$members), 5L)
  expect_identical(d$origin, "2024-06-01")
  expect_identical(d$member_set_hash, E$prevcarga_member_set_hash(E$prevcarga_members(root = pc_root())))
  l <- E$prevcarga_build_describe(mk_ctx(E), model = "lgbm")
  expect_identical(names(l$models), "lgbm")
  sp <- E$prevcarga_model_spec("lgbm", pc_root()); sp$modes <- as.list(sp$modes)      # modes sai como array (ADR-0040)
  expect_identical(l$models$lgbm, sp)
  expect_gt(length(E$prevcarga_build_describe(mk_ctx(E))$models), 1L)
  e <- tryCatch(E$prevcarga_build_describe(mk_ctx(E), model = "inexistente"), prevcarga_error = function(e) e)
  expect_identical(e$id, "config.unknown_model")
  expect_identical(e$exit_code, 8L)
  r <- run_main(E, c("describe", "--model=inexistente"))
  expect_identical(r$code, 8L)
})

test_that("instalacao-copia quebrada: areas/models saem exit 2 contract em texto e JSON pela fachada (ac-13)", {
  E <- new_env()
  c1 <- copy_install()
  f <- file.path(c1, "config", "topology.yaml")
  txt <- readLines(f); writeLines(sub("S: {areas: [RS, PR, SC]", "S: {areas: [RS, PR, SC, RJ]", txt, fixed = TRUE), f)
  c2 <- copy_install()
  unlink(file.path(c2, "models", "holt-winters", "run.yaml"))
  for (case in list(list("areas", c1), list("models", c2))) {
    rt <- run_main(E, case[[1L]], root = case[[2L]])
    expect_identical(rt$code, 2L)
    rj <- run_main(E, c(case[[1L]], "--format=json"), root = case[[2L]])
    expect_identical(rj$code, 2L)
    j <- jsonlite::fromJSON(paste(rj$out, collapse = ""), simplifyVector = FALSE)
    expect_identical(j$error$class, "contract")
    expect_identical(j$exit_code, 2L)
  }
})

test_that("paridade texto x JSON, ASCII e exit igual nos dois formatos (ac-14)", {
  E <- new_env()
  for (v in verbs5) {
    rt <- run_main(E, v); rj <- run_main(E, c(v, "--format=json"))
    expect_identical(rt$code, rj$code, info = v)
    expect_false(non_ascii(c(rt$out, rt$err, rj$out)), info = v)
    j <- jsonlite::fromJSON(paste(rj$out, collapse = ""), simplifyVector = FALSE)
    expect_identical(j$result$schema_version, 1L, info = v)
    expect_identical(j$verb, v)
  }
  doc <- run_main(E, "doctor", root = pc_root())
  for (ch in c("TZ", "locale", "pool BLAS", "config.yaml", "data_dir")) expect_true(any(grepl(ch, doc$out, fixed = TRUE)))
  mod <- run_main(E, "models")
  for (s in E$prevcarga_members(root = pc_root())$sigla) expect_true(any(grepl(s, mod$out, fixed = TRUE)))
  ar <- run_main(E, "areas")
  t <- E$prevcarga_topology(root = pc_root())
  for (s in t$order) {
    expect_true(any(grepl(t$subsystems[[s]]$loss, ar$out, fixed = TRUE)))
    for (a in t$subsystems[[s]]$areas) expect_true(any(grepl(a, ar$out, fixed = TRUE)))
  }
  expect_true(any(grepl("khat_range=1.00-1.15", ar$out, fixed = TRUE)))
  de <- run_main(E, "describe")
  expect_false(any(grepl("derivado no M2", de$out, fixed = TRUE)))
  expect_true(any(grepl("available_until: derivado do deck (via validate-deck)", de$out, fixed = TRUE)))
})

test_that("auto-registro: os 5 verbos descobertos, owner M1, exempt so doctor/status, sem stubs (ac-15)", {
  E <- new_env()
  tab <- E$prevcarga_verbs_discover()
  for (v in verbs5) {
    r <- tab[tab$verb == v, ]
    expect_identical(nrow(r), 1L, info = v)
    expect_identical(r$owner, "M1")
    expect_identical(r$exempt, v %in% c("doctor", "status"))
    expect_true("introspect.R" %in% r$modules[[1L]])
    expect_false(is.null(r$handler[[1L]]))
    if (!(v %in% c("doctor", "status"))) expect_true(all(c("topology.R", "provenance.R") %in% r$modules[[1L]]))
    expect_true(file.exists(file.path(lib_dir, "cli", paste0(v, ".R"))))
  }
  expect_identical(tab$usage[tab$verb == "describe"], "prevcarga describe [--origin=<data>] [--model=<m>]")
  usage <- E$prevcarga_usage(tab)
  for (u in tab$usage[tab$verb %in% verbs5]) expect_true(grepl(u, usage, fixed = TRUE))
  stubs <- tab$verb[vapply(tab$handler, is.null, NA)]
  expect_false(any(verbs5 %in% stubs))
  ## cada registro define so o objeto do verbo
  for (v in verbs5) {
    env <- new.env(parent = globalenv())
    sys.source(file.path(lib_dir, "cli", paste0(v, ".R")), envir = env)
    expect_identical(ls(env), paste0("prevcarga_verb_", v))
  }
  for (v in verbs5) {
    r <- run_main(E, v)
    expect_false(any(grepl("todo.not_implemented|ainda nao implementada", c(r$out, r$err))), info = v)
  }
})

test_that("[M8.9] ADR-0040: campos de lista saem como array no JSON mesmo com 1 elemento (1 subsistema, 1 area, 1 modo); CONTROL: o vetor cru viraria escalar", {
  E <- new_env()
  cp <- copy_install()
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  SECO: {areas: [SP], loss: PESE}", "order: [SECO]",
               "members:", "  - {sigla: RF, mdir: random-forest, exogena: null}"), file.path(cp, "config", "topology.yaml"))
  js <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  a <- E$prevcarga_build_areas(mk_ctx(E, root = cp))
  expect_match(js(a), "\"order\":\\[\"SECO\"\\]")
  expect_match(js(a), "\"areas\":\\[\"SP\"\\]")
  expect_match(js(a), "\"khat_range\":\\[1,1.15\\]")
  m <- E$prevcarga_build_models(mk_ctx(E, root = cp))
  expect_match(js(m), "\"modes\":\\[[^]]+\\]")
  d <- E$prevcarga_build_describe(mk_ctx(E, root = cp))
  expect_match(js(d), "\"order\":\\[\"SECO\"\\]")
  expect_match(js(d$models[[1L]]), "\"modes\":\\[")
  ## CONTROL: sem o as.list o mesmo conteudo vira escalar (o teste tem dentes)
  expect_match(js(list(order = "SECO", areas = "SP")), "\"order\":\"SECO\"")
})

