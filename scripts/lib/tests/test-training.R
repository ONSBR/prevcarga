## test-training.R -- fachada de treino prevcarga_train (M5.3; ADR-0023): spec efetivo, cortes por classe de split e
## identidade v_<hash>. Deck sintetico (fixture_store.R) -> leitor -> dates.R -> despachante -> registry (M5.2), com os
## plugins-brinquedo toy-30 / toy-60 (os modelos reais ainda nao tem todas as etapas). Cada braco FORBIDDEN (delta 0)
## tem o seu CONTROL (delta != 0). Tudo em tempdir; sem variaveis de ambiente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                       "registry", "features", "training"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
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

## --- raizes de modelos: <root>/models/{toy-*, _toy-engine}; run.yaml com `backtest` e historico curto -----------------
## `defs`: nome -> list(base, run_yaml, descriptor). Cache por assinatura (a copia e barata, a montagem nao).
.tr_roots <- new.env()
tr_models <- function(defs, key = paste(names(defs), collapse = ",")) {
  hit <- .tr_roots[[key]]
  if (!is.null(hit)) return(hit)
  rt <- tempfile("tr-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
  for (nm in names(defs)) {
    d <- defs[[nm]]
    vr <- pc_toy_variant(d$base, run_yaml = modifyList(list(backtest = list(protocol_default = "fixed")),
                                                        if (is.null(d$run_yaml)) list() else d$run_yaml),
                         descriptor = if (is.null(d$descriptor)) list() else d$descriptor, name = nm)
    file.copy(file.path(vr, nm), file.path(rt, "models"), recursive = TRUE)
  }
  file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
  .tr_roots[[key]] <- rt
  rt
}

TR_DEFS <- list(
  "toy-30" = list(base = "toy-30"),
  "toy-60" = list(base = "toy-60"),
  ## H=9, lag 0 (RF) e H=1, lag 2 (LGBM) do ticket, com o historico curto dos brinquedos
  "toy-rf" = list(base = "toy-30", run_yaml = list(horizon_days = 9L)),
  "toy-lg" = list(base = "toy-30", run_yaml = list(horizon_days = 1L), descriptor = list(data_lag_days = 2L)),
  "toy-lg0" = list(base = "toy-30", run_yaml = list(horizon_days = 1L), descriptor = list(data_lag_days = 0L)),
  "toy-lg9" = list(base = "toy-30", run_yaml = list(horizon_days = 9L), descriptor = list(data_lag_days = 2L)),
  ## origin_scoped em 30 min (o toy-60 declara 30 e 60 min; deslocamento de modelagem de 60 min do descriptor)
  "toy-60s30" = list(base = "toy-60", run_yaml = list(granularity_min = 30L),
                     descriptor = list(granularity_supported = c(30L, 60L))),
  "toy-60a" = list(base = "toy-60", run_yaml = list(split = "origin_agnostic", modes = c("train", "infer"))),
  "toy-long" = list(base = "toy-30", run_yaml = list(history_days = list(train = 30L, infer = 3L))))

## --- decks (14 dias, areas A/B) ----------------------------------------------------------------------------------
.tr_decks <- new.env()
tr_deck <- function(until = "09:00") {
  hit <- .tr_decks[[until]]
  if (!is.null(hit)) return(hit)
  dir <- tempfile("tr-deck-")
  env$prevcarga_fixture_deck(dir, seed = 11, areas = AREAS, days = seq(O - 13L, O, by = "day"),
                             load_until = at(until, "2026-10-01"))
  pc_deck_topology(file.path(dir, "topology.yaml"), AREAS)
  .tr_decks[[until]] <- dir
  dir
}

## perturba a carga de [from, to] (interval_start, fuso do deck); +1000 MW
tr_bump <- function(dir, after, at_or_before = NULL)
  env$prevcarga_fixture_perturb(dir, "load", after = after, at_or_before = at_or_before, perturb = function(v) v + 1000)
tr_day <- function(dir, day)     # o dia civil `day` inteiro
  tr_bump(dir, after = at("00:00", day) - lubridate::minutes(1L), at_or_before = at("23:30", day))

tr_ctx <- function(dir, root, state, ...) {
  c(list(root = root, data_dir = dir, topology_path = file.path(dir, "topology.yaml"),
         weather_issue_cutoff = "available_until", origin = O, state_dir = state,
         code_commit = "c0ffee", renv_lock_hash = "ab12"), list(...))
}
tr_member <- function(mdir, exogena = NA_character_) list(sigla = toupper(mdir), mdir = mdir, exogena = exogena)

## espiao do despachante de treino (conta chamadas a prevcarga_plugin_train); restaura ao fim do frame do chamador
tr_spy_train <- function(frame = parent.frame()) {
  spy <- new.env(); spy$n <- 0L
  orig <- get("prevcarga_plugin_train", envir = env)
  assign("prevcarga_plugin_train", function(...) { spy$n <- spy$n + 1L; orig(...) }, envir = env)
  withr::defer(assign("prevcarga_plugin_train", orig, envir = env), envir = frame)
  spy
}

tr_art_sha <- function(path) digest::digest(file = file.path(path, "artifact.rds"), algo = "sha256")
tr_art <- function(row) env$prevcarga_load_artifact(dirname(row$path), row$version)

tr_setup <- function(mdir, until = "09:00", deck = tr_deck(until), defs = TR_DEFS, exogena = NA_character_) {
  rt <- tr_models(defs)
  st <- pc5_state_tmp(.local_envir = parent.frame())
  list(mem = tr_member(mdir, exogena), ctx = tr_ctx(deck, rt, st), deck = deck, root = rt, state = st)
}
tr_train <- function(s, area = "A", ...) suppressWarnings(env$prevcarga_train(s$mem, area, O, s$ctx, ...))
tr_inputs <- function(s, area = "A") suppressWarnings(env$prevcarga_train_inputs(s$mem, area, O, s$ctx))

## =====================================================================================================
## AC1 -- identidade content-addressed: 10 bracos de perturbacao + FORBIDDEN-CONTROL de trained_at
## =====================================================================================================
fp_base <- local({
  s <- tr_setup("toy-30", defs = TR_DEFS)
  inp <- tr_inputs(s)
  list(s = s, inp = inp)
})
fp_id <- function(member = fp_base$s$mem, spec = fp_base$inp$spec, cuts = fp_base$inp$cuts,
                  panel = fp_base$inp$panel, ctx = fp_base$s$ctx)
  env$prevcarga_train_fingerprint(member, spec, cuts, panel, ctx)$version

test_that("AC1: ids de duas rodadas com insumos identicos sao identicos; formato v_<16 hex>", {
  expect_identical(fp_id(), fp_id())
  expect_match(fp_id(), "^v_[0-9a-f]{16}$")
  s2 <- tr_setup("toy-30")                      # outra rodada completa dos insumos (mesmo deck, mesmo spec)
  i2 <- tr_inputs(s2)
  expect_identical(fp_id(panel = i2$panel, cuts = i2$cuts, spec = i2$spec, ctx = s2$ctx), fp_id())
})

test_that("AC1 [FORBIDDEN-CONTROL]: trained_at fica fora do hash; dentro do fingerprint o contrato recusa", {
  ctx2 <- c(fp_base$s$ctx, list(trained_at = "2026-10-01T12:00:00Z", run_id = "r1", version = "9.9"))
  expect_identical(fp_id(ctx = ctx2), fp_id())
  fp <- env$prevcarga_train_fingerprint(fp_base$s$mem, fp_base$inp$spec, fp_base$inp$cuts, fp_base$inp$panel,
                                        fp_base$s$ctx)$fingerprint
  expect_pc(env$prevcarga_version_hash(c(fp, list(trained_at = "2026-10-01T12:00:00Z"))),
            "contract.fingerprint_lateral", 2L)
})

test_that("AC1 braco 1/10: model_id", {
  expect_false(identical(fp_id(member = tr_member("toy-60")), fp_id()))
})
test_that("AC1 braco 2/10: data_fingerprint (1 valor do painel ja cortado)", {
  p <- fp_base$inp$panel
  p$load <- data.table::copy(p$load); p$load[1L, load_mwh := load_mwh + 1]
  expect_false(identical(fp_id(panel = p), fp_id()))
})
test_that("AC1 braco 3/10: cuts$train_origin", {
  cu <- fp_base$inp$cuts; cu$train_origin <- cu$train_origin + 1L
  expect_false(identical(fp_id(cuts = cu), fp_id()))
})
test_that("AC1 braco 4/10: horizon_days do spec efetivo", {
  sp <- fp_base$inp$spec; sp$horizon_days <- sp$horizon_days + 1L
  expect_false(identical(fp_id(spec = sp), fp_id()))
})
test_that("AC1 braco 5/10: 1 item de params", {
  sp <- fp_base$inp$spec; sp$params$k <- sp$params$k + 1L
  expect_false(identical(fp_id(spec = sp), fp_id()))
})
test_that("AC1 braco 6/10: exogena do membro", {
  expect_false(identical(fp_id(member = tr_member("toy-30", "MED")), fp_id()))
  expect_false(identical(fp_id(member = tr_member("toy-30", "MED")), fp_id(member = tr_member("toy-30", "MAX"))))
})
test_that("AC1 braco 7/10: load_label", {
  cu <- fp_base$inp$cuts; cu$load_label <- if (identical(cu$load_label, "end")) "start" else "end"
  expect_false(identical(fp_id(cuts = cu), fp_id()))
})
test_that("AC1 braco 8/10: fuso da carga", {
  cu <- fp_base$inp$cuts; cu$tz <- "UTC"
  expect_false(identical(fp_id(cuts = cu), fp_id()))
})
test_that("AC1 braco 9/10: code_commit", {
  expect_false(identical(fp_id(ctx = modifyList(fp_base$s$ctx, list(code_commit = "deadbeef"))), fp_id()))
})
test_that("AC1 braco 10/10: renv_lock_hash", {
  expect_false(identical(fp_id(ctx = modifyList(fp_base$s$ctx, list(renv_lock_hash = "cd34"))), fp_id()))
})

test_that("AC1: mesmo deck, membros de modelos distintos (LIN_MED x RGD_MED) => ids distintos", {
  a <- fp_id(member = list(sigla = "LIN_MED", mdir = "dessem-linear", exogena = "MED"))
  b <- fp_id(member = list(sigla = "RGD_MED", mdir = "dessem-regdin", exogena = "MED"))
  expect_false(identical(a, b))
})

## =====================================================================================================
## AC2 -- spec efetivo e cortes pela classe de split (ADR-0023)
## =====================================================================================================
test_that("AC2: origin_agnostic H=9/lag 0 (RF) e H=1/lag 2 (LGBM) com --origin 2026-10-01", {
  rf <- tr_inputs(tr_setup("toy-rf"))$cuts
  expect_identical(rf$train_origin, lubridate::ymd("2026-09-21"))
  expect_identical(rf$last_data_day, lubridate::ymd("2026-09-30"))
  lg <- tr_inputs(tr_setup("toy-lg"))$cuts
  expect_identical(lg$train_origin, lubridate::ymd("2026-09-29"))
  expect_identical(lg$last_data_day, lubridate::ymd("2026-09-28"))
  expect_identical(lg$data_lag_days, 2L)                       # a linha de corte repassada ao despachante
})

test_that("AC2 [CONTROL]: o atraso vem do descriptor (lag 0 leva last_data_day a O-1), nao de literal", {
  c0 <- tr_inputs(tr_setup("toy-lg0"))$cuts
  expect_identical(c0$last_data_day, lubridate::ymd("2026-09-30"))
  expect_identical(c0$data_lag_days, 0L)
})

test_that("AC2: linha de corte com data_lag_days diferente do descriptor => contract.cut_mismatch (exit 2)", {
  s <- tr_setup("toy-lg")
  orig <- get("prevcarga_resolve_cuts", envir = env)
  assign("prevcarga_resolve_cuts", function(...) {
    r <- orig(...); r$cuts$data_lag_days <- 0L; r
  }, envir = env)
  on.exit(assign("prevcarga_resolve_cuts", orig, envir = env), add = TRUE)
  expect_pc(suppressWarnings(env$prevcarga_train_inputs(s$mem, "A", O, s$ctx)), "contract.cut_mismatch", 2L)
})

test_that("AC2: override de H (--horizon) move train_origin e o v_<hash>; editar o run.yaml-fixture faz o mesmo (CONTROL)", {
  s <- tr_setup("toy-lg"); base <- tr_inputs(s)
  s9 <- s; s9$ctx$spec_overrides <- list(horizon_days = 9)
  i9 <- tr_inputs(s9)
  expect_identical(i9$cuts$train_origin, lubridate::ymd("2026-09-21"))
  expect_identical(i9$spec$horizon_days, 9L)
  v <- function(i, s) env$prevcarga_train_fingerprint(s$mem, i$spec, i$cuts, i$panel, s$ctx)$version
  expect_false(identical(v(i9, s9), v(base, s)))
  y9 <- tr_inputs(tr_setup("toy-lg9"))                          # run.yaml-fixture com H=9
  expect_identical(y9$cuts$train_origin, i9$cuts$train_origin)
})

test_that("AC2: override de chave nao declarada => config.override_unknown (exit 8); valor invalido => config.override_bad_value", {
  s <- tr_setup("toy-lg"); s$ctx$spec_overrides <- list(foo = 1)
  expect_pc(suppressWarnings(env$prevcarga_train_inputs(s$mem, "A", O, s$ctx)), "config.override_unknown", 8L)
  s$ctx$spec_overrides <- list(params.foo = 1)
  expect_pc(suppressWarnings(env$prevcarga_train_inputs(s$mem, "A", O, s$ctx)), "config.override_unknown", 8L)
  for (bad in list(0, 1.5, "9", NA, c(1, 2))) {
    s$ctx$spec_overrides <- list(horizon_days = bad)
    expect_pc(suppressWarnings(env$prevcarga_train_inputs(s$mem, "A", O, s$ctx)), "config.override_bad_value", 8L)
  }
})

test_that("AC2: prevcarga_model_spec ordena por nome, aplica params.<k> declarado e confere tipo", {
  root <- pc_root()
  sp <- env$prevcarga_model_spec("dessem-linear", root, list(horizon_days = 5L, params.exogena = "MAX"))
  expect_identical(names(sp), sort(names(sp), method = "radix"))
  expect_identical(sp$horizon_days, 5L)
  expect_identical(sp$params$exogena, "MAX")
  expect_identical(env$prevcarga_model_spec("dessem-linear", root), env$prevcarga_model_spec("dessem-linear", root, list()))
  expect_pc(env$prevcarga_model_spec("dessem-linear", root, list(params.exogena = 3)), "config.override_bad_value", 8L)
  expect_pc(env$prevcarga_model_spec("dessem-linear", root, list(params.nope = "MAX")), "config.override_unknown", 8L)
  expect_false(env$prevcarga_model_spec("lgbm", root, list(params.disable_loess = FALSE))$params$disable_loess)
  expect_pc(env$prevcarga_model_spec("lgbm", root, list(params.train_cv = "nao")), "config.override_bad_value", 8L)
})

test_that("AC2: o corpo de training.R nao faz conta de corte (sem days(, - 1, - 3)", {
  fns <- ls(env, pattern = "^\\.?prevcarga_(tr_|train|cell_row|artifact_build)")
  expect_true(all(c("prevcarga_train", "prevcarga_train_inputs", "prevcarga_train_fingerprint") %in% fns))
  body_txt <- vapply(fns, function(f) paste(deparse(body(get(f, envir = env))), collapse = "\n"), "")
  expect_false(any(grepl("days\\(|- 1|- 3", body_txt)))
})

## =====================================================================================================
## AC3 -- FORBIDDEN dia O em diante; CONTROL last_data_day (RF O-1; LGBM O-3), por sha256 do artifact.rds
## =====================================================================================================
for (cell in list(list(m = "toy-30", last = "2026-09-30"), list(m = "toy-lg", last = "2026-09-28"))) {
  local({
    cell <- cell
    test_that(sprintf("AC3 [%s]: carga de O em diante nao entra no treino (FORBIDDEN); last_data_day entra (CONTROL)", cell$m), {
      s <- tr_setup(cell$m)
      base <- tr_train(s)
      expect_identical(base$status, "trained")
      expect_identical(base$last_data_day, lubridate::ymd(cell$last))
      sha0 <- tr_art_sha(base$path)
      ## FORBIDDEN: +1000 MW de O em diante (re-treino em outro state_dir: nada de reuso mascarando o delta)
      sf <- tr_setup(cell$m, deck = tr_bump(s$deck, after = at("00:00", "2026-10-01") - lubridate::minutes(1L)))
      rf <- tr_train(sf)
      expect_identical(rf$status, "trained")
      expect_identical(tr_art_sha(rf$path), sha0)
      expect_identical(rf$version, base$version)
      ## CONTROL: perturbar o ultimo dia permitido muda o artefato
      sc <- tr_setup(cell$m, deck = tr_day(s$deck, cell$last))
      rc <- tr_train(sc)
      expect_false(identical(tr_art_sha(rc$path), sha0))
      expect_false(identical(rc$version, base$version))
    })
  })
}

## =====================================================================================================
## AC4 -- origin_scoped: chaveado so pela origem; 2 decks do mesmo dia (09:00 / 12:00) => mesmo v_<hash>, 2o reused
## =====================================================================================================
for (m in c("toy-60", "toy-60s30")) {
  local({
    m <- m
    test_that(sprintf("AC4 [%s]: dois decks da mesma origem dao o mesmo v_<hash>; 2o = reused (plugin_train 1x)", m), {
      s9 <- tr_setup(m, "09:00"); s12 <- tr_setup(m, "12:00")
      s12$state <- s9$state; s12$ctx$state_dir <- s9$state
      spy <- tr_spy_train()
      r9 <- tr_train(s9); r12 <- tr_train(s12)
      expect_identical(r9$status, "trained"); expect_identical(r12$status, "reused")
      expect_identical(r12$version, r9$version)
      expect_identical(spy$n, 1L)
      art <- tr_art(r9)
      expect_identical(art$origin, O)
      expect_null(art$available_until)
      mf <- env$prevcarga_read_run_manifest(r9$path)
      expect_false("available_until" %in% names(mf))
      expect_identical(mf$weather_issue_cutoff, "available_until")
      expect_match(mf$fingerprint$data_fingerprint, "^[0-9a-f]{64}$")
    })
    test_that(sprintf("AC4 [%s]: dia O perturbado nao muda o artefato (FORBIDDEN); O-1 muda (CONTROL)", m), {
      s <- tr_setup(m, "09:00")
      base <- tr_train(s)
      sO <- tr_setup(m, deck = tr_day(s$deck, "2026-10-01"))
      rO <- tr_train(sO)
      expect_identical(rO$version, base$version)
      expect_identical(tr_art_sha(rO$path), tr_art_sha(base$path))
      sP <- tr_setup(m, deck = tr_day(s$deck, "2026-09-30"))
      rP <- tr_train(sP)
      expect_false(identical(rP$version, base$version))
      expect_false(identical(tr_art_sha(rP$path), tr_art_sha(base$path)))
    })
  })
}

## =====================================================================================================
## AC5 -- reused sem force (spy, mtime) e re-materializacao byte-identica com force; rename falho preserva a versao
## =====================================================================================================
test_that("AC5: reused sem force nao chama o plugin nem toca o v_<hash>/; force re-materializa byte-identico", {
  s <- tr_setup("toy-30")
  r1 <- tr_train(s)
  expect_identical(r1$status, "trained")
  key <- dirname(r1$path)
  expect_identical(env$prevcarga_resolve_version(key, "latest"), r1$version)
  mt <- file.mtime(r1$path); sha <- tr_art_sha(r1$path)
  spy <- tr_spy_train()
  r2 <- tr_train(s)
  expect_identical(r2$status, "reused"); expect_identical(r2$version, r1$version); expect_identical(r2$path, r1$path)
  expect_identical(spy$n, 0L)
  expect_identical(file.mtime(r1$path), mt)
  r3 <- tr_train(s, force = TRUE)
  expect_identical(r3$status, "trained"); expect_identical(spy$n, 1L)
  expect_identical(tr_art_sha(r3$path), sha)
  expect_identical(length(list.files(key, pattern = "^[.]tmp-", all.files = TRUE)), 0L)
})

test_that("AC5: com file.rename falho o v_<hash>/ anterior fica intacto (CONTROL: sem falha, force publica)", {
  s <- tr_setup("toy-30")
  r1 <- tr_train(s)
  h0 <- pc5_tree_hash(r1$path)
  assign("file.rename", function(from, to) FALSE, envir = env)
  err <- tryCatch(tr_train(s, force = TRUE), prevcarga_error = function(e) e)
  rm("file.rename", envir = env)
  expect_s3_class(err, "prevcarga_error")
  expect_identical(pc5_tree_hash(r1$path), h0)
  expect_identical(length(list.files(dirname(r1$path), pattern = "^[.]tmp-", all.files = TRUE)), 0L)
  expect_identical(tr_train(s, force = TRUE)$status, "trained")
})

## =====================================================================================================
## AC6 -- historico curto => exit 5 window.history_short; nada gravado; historico completo treina (CONTROL)
## =====================================================================================================
test_that("AC6: menos dias que history_days.train antes de last_data_day => exit 5, sem .tmp-* nem v_*", {
  s <- tr_setup("toy-long")                                    # exige 30 dias; o deck tem 14
  cond <- expect_pc(tr_train(s), "window.history_short", 5L)
  expect_identical(cond$details$required_days, 30L)
  expect_lt(cond$details$available_days, 30L)
  expect_identical(cond$details$member, "toy-long")
  expect_false(dir.exists(file.path(s$state, "artifacts", "toy-long")) &&
                 length(list.files(file.path(s$state, "artifacts", "toy-long"), recursive = TRUE, all.files = TRUE)) > 0L)
  ok <- tr_setup("toy-30")                                     # 10 dias exigidos: treina
  expect_identical(tr_train(ok)$status, "trained")
})

## =====================================================================================================
## AC7 -- transparencia: fachada == prevcarga_plugin_train direto (bytes de freeze); CONTROL: 1 valor do painel
## =====================================================================================================
payload_bytes <- function(art) serialize(art$payload, NULL, version = 3)
for (m in c("toy-30", "toy-lg", "toy-60s30", "toy-60", "toy-60a")) {
  local({
    m <- m
    test_that(sprintf("AC7 [%s]: bytes de freeze identicos entre fachada e despachante direto; perturbar o painel muda", m), {
      s <- tr_setup(m)
      row <- tr_train(s)
      via <- tr_art(row)$fit
      inp <- tr_inputs(s)
      pctx <- list(load_label = inp$cuts$load_label, timezone = inp$cuts$tz,
                   weather_issue_cutoff = inp$cuts$weather_issue_cutoff)
      direct <- env$prevcarga_plugin_train(inp$plugin, inp$panel, inp$cuts, inp$spec, pctx)
      expect_identical(payload_bytes(via), payload_bytes(direct))
      expect_identical(via$stamp, direct$stamp)
      p2 <- inp$panel
      p2$load <- data.table::copy(p2$load)
      k <- nrow(p2$load)
      p2$load[k, load_mwh := load_mwh + 100]
      pert <- env$prevcarga_plugin_train(inp$plugin, p2, inp$cuts, inp$spec, pctx)
      expect_false(identical(payload_bytes(pert), payload_bytes(direct)))
    })
  })
}

## =====================================================================================================
## Catalogo de erros do modulo (ADR-0040 item 8)
## =====================================================================================================
test_that("errors/training.tsv carrega pelo loader sem id duplicado, ASCII e classes certas", {
  cat <- env$prevcarga_errors_load()
  expect_false(anyDuplicated(cat$id) > 0L)
  tr <- utils::read.delim(file.path(pc_root(), "scripts", "lib", "errors", "training.tsv"), colClasses = "character")
  expect_setequal(tr$id, c("window.history_short", "config.override_unknown", "config.override_bad_value",
                           "contract.cut_mismatch"))
  expect_identical(env$prevcarga_error_lookup("window.history_short")$class, "window")
  expect_identical(env$prevcarga_exit_codes[["window"]], 5L)
  expect_identical(env$prevcarga_error_lookup("config.override_unknown")$class, "config")
  expect_identical(env$prevcarga_exit_codes[["config"]], 8L)
  expect_identical(env$prevcarga_error_lookup("contract.cut_mismatch")$class, "contract")
  raw <- readBin(file.path(pc_root(), "scripts", "lib", "errors", "training.tsv"), "raw", 1e5)
  expect_true(all(as.integer(raw) < 128L)); expect_false(13L %in% as.integer(raw))
})
