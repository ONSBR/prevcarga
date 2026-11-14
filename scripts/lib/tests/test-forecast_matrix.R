## test-forecast_matrix.R -- verificacao do epico 3 (F3.8): matriz pairwise escopo x membro ausente x pesos x clima x --jobs
## sobre o fixture m6f (SS1 = A1,A2; SS2 = B1), rodando prevcarga_forecast de verdade (leitor, despachante do corte de clima,
## combina com EQW, reconcilia, export). Nenhum membro real treina ou infere: o espiao de infer (anexado a uma COPIA do lib, que
## os workers PSOCK tambem carregam) usa o leitor REAL e o corte REAL do despachante, com descriptors-stub de dependencia
## (RF = como DESSEM: offset 60, weather_cut absent; LGBM = truncate; HW = sem clima). Tudo em tempdir; sem variaveis de ambiente.
## Invariantes: nenhum subsistema/SIN com folha ausente ou fora do plano; folha vazia em subsistema/sin => aborta
## completude.forecast_leaf_no_member sem scratch; escopo area com folha vazia => exit 14 com as demais entregues; nenhuma linha
## de membro alem de O + delivery_days_efetivo 00:00 (nenhum dia de clima persistido); jobs 1 x 2 identicos.
## Comportamento verificado = decisao F3.7: DESSEM so roda com horizonte inteiro (recusa vira membro ausente).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "inference_needs", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
base <- withr::local_tempdir("fmx-base-")
DECK_O9 <- m6f_deck(env, file.path(base, "deck-o9"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK_O9, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)
MEM <- env$prevcarga_members(path = TOPO, root = pc_root())
for (a in M6F_LEAVES) m6f_fake_artifact(env, MEM[MEM$sigla == "HW", ], a, O, FX)   # HW origin_scoped: artefato da origem

day_start <- function(d) lubridate::force_tz(lubridate::as_datetime(d), M6_TZ)

## deck com o previsto truncado em O+4 00:30 (O+3 completo + 2 instantes de O+4): digitos civis do store, como o leitor os le
DECK_O3 <- local({
  d <- file.path(base, "deck-o3")
  dir.create(d)
  file.copy(list.files(DECK_O9, full.names = TRUE, all.files = TRUE, no.. = TRUE), d, recursive = TRUE)
  keep <- lubridate::force_tz(lubridate::ymd_hm("2026-01-19 00:30", tz = M6_TZ), "UTC")
  for (f in list.files(file.path(d, "weather", "forecast"), pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)) {
    tb <- arrow::read_parquet(f, as_data_frame = FALSE)
    arrow::write_parquet(tb[tb$valid_time$as_vector() <= keep, ], f)
  }
  d
})
DECKS <- list(o9 = DECK_O9, o3 = DECK_O3)

## --- espiao de infer (texto): leitor REAL + corte REAL do despachante com descriptors-stub; falha por (sigla/folha) ------------
.stub <- function(dd, offset = 0L, weather = TRUE, cut = "truncate")
  sprintf("list(descriptor = list(delivery_days = %dL, model_time_offset_min = %dL, weather_cut = '%s', stages = list(inference = list(fn = identity, requires = '%s', provides = 'member_envelope'))))",
          dd, offset, cut, if (weather) "data.weather_forecast.temperature" else "data.load.load_mwh")
shim <- function(fails) c(
  "", "## ESPIAO DE TESTE (so nesta copia do lib, em tempdir): estagio de inferencia de brinquedo; o resto da rodada e REAL.",
  sprintf("MX_FAILS <- c(%s)", paste0("'", fails, "'", collapse = ", ")),
  sprintf("MX_STUB <- list(RF = %s, LGBM = %s, HW = %s)", .stub(9L, 60L, cut = "absent"), .stub(9L), .stub(9L, weather = FALSE)),
  "prevcarga_infer_cell <- function(member, area, origin, ctx, variant = NULL, version = NULL, pin = FALSE, ...) {",
  "  sg <- as.character(member$sigla)",
  "  if (paste0(sg, '/', area) %in% MX_FAILS) prevcarga_abort('state.artifact_missing', where = paste0(sg, '/', area))",
  "  cuts <- prevcarga_resolve_cuts(ctx, origin = origin)",
  "  panel <- prevcarga_read_panel(ctx, origin, area, 9L, 5L)",
  "  cuts <- .prevcarga_inf_apply_weather_cut(MX_STUB[[sg]], member$mdir, area, origin, panel$attrs, cuts)",
  "  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), cuts$tz)",
  "  ie <- prevcarga_delivery_grid(cuts, cuts$delivery_days)",
  "  y <- lubridate::time_length((ie - lubridate::minutes(30L)) - t0, 'minute') + 10 * match(sg, c('RF', 'LGBM', 'HW')) +",
  "       100 * match(area, c('A1', 'A2', 'B1'))",
  "  list(predictions = data.table::data.table(target_datetime = ie, y_pred = y),",
  "       version = paste0('v_', substr(digest::digest(paste(sg, area), algo = 'sha256'), 1L, 16L)),",
  "       available_until_posix = cuts$available_until, delivery_days_efetivo = cuts$delivery_days,",
  "       weather_forecast_until = .prevcarga_inf_covered_until(panel$attrs, area))",
  "}")
## uma copia do lib por conjunto de falhas (cache): os workers PSOCK sourceiam ESTE forecast.R via ctx$lib_dir
LIBS <- new.env()
lib_for <- function(fails) {
  k <- paste(c("lib", fails), collapse = ",")
  if (is.null(LIBS[[k]])) {
    d <- file.path(base, paste0("lib-", length(ls(LIBS))))
    dir.create(d)
    file.copy(list.files(file.path(pc_root(), "scripts", "lib"), full.names = TRUE), d, recursive = TRUE)
    unlink(file.path(d, "tests"), recursive = TRUE)
    f <- file.path(d, "forecast.R")
    writeLines(c(readLines(f, warn = FALSE), shim(fails)), f)
    LIBS[[k]] <- list(dir = d, fn = {
      e <- new.env(parent = env)
      eval(parse(text = shim(fails)), envir = e)
      e$prevcarga_infer_cell
    })
  }
  LIBS[[k]]
}

## --- mx_case(row): monta o estado, aplica as mutacoes e roda prevcarga_forecast -------------------------------------------
## result/error/arquivos/members/scratch_parent como na especificacao do ticket; `ctx` e `state` para as asserts.
mx_case <- function(row, .frame = parent.frame()) {
  fails <- switch(row$ausente, nenhum = character(), um = "LGBM/A1", folha = paste0(M6F_MEMBERS, "/A2"))
  lib <- lib_for(fails)
  st <- withr::local_tempdir("fmx-state-", .local_envir = .frame)
  file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
  ctx <- m6f_ctx(DECKS[[row$clima]], st, TOPO, extra = list(lib_dir = lib$dir))
  if (identical(row$pesos, "eqw"))
    for (a in M6F_LEAVES) unlink(file.path(env$prevcarga_registry_root(ctx), "combina", a, "latest.txt"))
  old <- env$prevcarga_infer_cell
  assign("prevcarga_infer_cell", lib$fn, envir = env)
  withr::defer(assign("prevcarga_infer_cell", old, envir = env), envir = .frame)
  args <- switch(row$escopo, area = list(escopo = "area", areas = c("A1", "A2")),
                 subsistema = list(escopo = "subsistema", subsistema = "SS1"), sin = list(escopo = "sin"))
  out <- list(result = NULL, error = NULL, ctx = ctx, scratch_parent = file.path(st, "scratch"), warnings = list())
  tryCatch({
    r <- do.call(env$prevcarga_forecast, c(list(ctx, skip_validate = TRUE, incluir_membros = TRUE, jobs = as.integer(row$jobs)), args))
    out$result <- r$result; out$warnings <- r$warnings
  }, prevcarga_error = function(e) {
    out$error <<- e
    if (!is.null(e$result)) { out$result <<- e$result; out$warnings <<- e$result$warnings }
  })
  ## arquivos = o que o escopo entrega (folhas, subsistemas, SIN); os membros/ (incluir_membros) ficam em `members`
  out$arquivos <- if (is.null(out$result)) character() else
    sort(Filter(function(p) !startsWith(p, "membros/"), vapply(out$result$arquivos, function(a) a$path, "")), method = "radix")
  out$members <- list()
  if (!is.null(out$result))
    for (sg in M6F_MEMBERS) for (a in M6F_LEAVES) {
      f <- file.path(out$result$saida, "membros", sg, paste0(a, ".parquet"))
      if (file.exists(f)) out$members[[paste0(sg, "/", a)]] <- arrow::read_parquet(f)
    }
  ## impressao digital da entrega (calculada aqui: o state_dir some ao fim do teste): versao, run_id, sha256 de arquivos[] e do manifest
  out$digest <- if (is.null(out$result)) list(error = out$error$id) else {
    mf <- file.path(out$result$saida, "run_manifest.json")
    list(versao = out$result$versao, version = jsonlite::read_json(mf)$version, run_id = jsonlite::read_json(mf)$run_id,
         arquivos = vapply(out$result$arquivos, function(x) paste(x$path, x$sha256), ""),
         manifest = digest::digest(file = mf, algo = "sha256"))
  }
  out
}
no_scratch <- function(r) !dir.exists(r$scratch_parent) || !length(list.files(r$scratch_parent))

## --- invariantes ---------------------------------------------------------------------------------------------------------
LEAVES_OF <- list(SS1 = c("A1", "A2"), SS2 = "B1")
mx_assert_no_partial_aggregate <- function(r, row) {
  paths <- r$arquivos
  have <- function(p) p %in% paths
  ## o conjunto pedido e inteiro: nenhum agregado sem todas as folhas filhas, nenhuma folha/agregado fora do plano
  for (ss in names(LEAVES_OF)) if (have(paste0("subsistemas/", ss, ".parquet")))
    expect_true(all(paste0("folhas/", LEAVES_OF[[ss]], ".parquet") %in% paths), info = paste("subsistema", ss))
  if (have("SIN.parquet")) expect_true(all(paste0("subsistemas/", names(LEAVES_OF), ".parquet") %in% paths))
  if (identical(row$escopo, "area")) expect_false(any(grepl("^subsistemas/|^SIN", paths)))
  if (identical(row$escopo, "subsistema")) {
    expect_identical(paths, sort(c("folhas/A1.parquet", "folhas/A2.parquet", "subsistemas/SS1.parquet"), method = "radix"))
  }
  if (identical(row$escopo, "sin"))
    expect_identical(paths, sort(c(paste0("folhas/", M6F_LEAVES, ".parquet"), paste0("subsistemas/", M6F_SS, ".parquet"), "SIN.parquet"), method = "radix"))
  ## em todo agregado entregue, a folha do membro ausente nao entra no plano de ausentes de outra folha
  expect_identical(length(r$result$membros_ausentes) > 0L, !identical(row$ausente, "nenhum") || identical(row$clima, "o3"))
}
mx_assert_no_persisted_weather_day <- function(r, row) {
  if (is.null(r$result)) return(invisible())
  eff <- r$result$delivery_days_efetivo
  for (key in names(r$members)) {
    sg <- sub("/.*", "", key); a <- sub(".*/", "", key)
    bound <- day_start(O + as.integer(eff[[a]][[sg]]))
    expect_true(all(r$members[[key]]$target_datetime <= bound), info = paste(row$clima, key))
  }
  if (identical(row$clima, "o3")) {
    ## CONTROL do clima curto: o DESSEM-like (RF) nao entrega nada; o que corta vai ate O+4 00:00; hw inteiro
    expect_false(any(startsWith(names(r$members), "RF/")))
    expect_identical(r$result$clima_previsto_ate, format(O + 3L))
    for (key in names(r$members)) if (startsWith(key, "LGBM/")) expect_identical(max(r$members[[key]]$target_datetime), day_start(O + 4L))
  } else {
    expect_identical(r$result$clima_previsto_ate, format(O + 9L))
    expect_true(any(startsWith(names(r$members), "RF/")))
  }
}

MX <- data.frame(
  escopo  = c("area","area","area","subsistema","subsistema","subsistema","sin","sin","sin","area","subsistema","sin"),
  ausente = c("nenhum","um","folha","nenhum","um","folha","nenhum","um","folha","um","nenhum","um"),
  pesos   = c("treinados","eqw","treinados","eqw","treinados","eqw","treinados","eqw","treinados","eqw","treinados","eqw"),
  clima   = c("o9","o3","o9","o3","o9","o3","o3","o9","o3","o9","o3","o9"),
  jobs    = c(1L,2L,2L,1L,2L,1L,2L,1L,1L,2L,1L,2L), stringsAsFactors = FALSE)

test_that("a matriz cobre todo par de fatores (pairwise)", {
  f <- c("escopo", "ausente", "pesos", "clima", "jobs")
  for (i in seq_along(f)) for (j in seq_along(f)) if (i < j) {
    got <- unique(paste(MX[[f[[i]]]], MX[[f[[j]]]])); want <- nrow(unique(expand.grid(unique(MX[[f[[i]]]]), unique(MX[[f[[j]]]]))))
    expect_length(got, want)
  }
})

CASES <- list()
for (i in seq_len(nrow(MX))) test_that(sprintf("matriz %02d %s", i, paste(MX[i, ], collapse = "/")), {
  row <- MX[i, ]
  r <- mx_case(row)
  CASES[[i]] <<- r[c("digest", "error")]
  empty_leaf <- identical(row$ausente, "folha")
  if (empty_leaf && row$escopo != "area") {
    expect_identical(r$error$id, "completude.forecast_leaf_no_member")
    expect_identical(r$error$exit_code, 3L)
    expect_true(no_scratch(r))
    expect_length(r$arquivos, 0L)
  } else if (empty_leaf) {
    expect_identical(r$error$id, "partial.forecast_leaf_absent")
    expect_identical(r$error$exit_code, 14L)
    expect_identical(r$arquivos, "folhas/A1.parquet")
    expect_identical(r$result$dropped, list("A2"))
    mx_assert_no_persisted_weather_day(r, row)
  } else {
    expect_null(r$error)
    mx_assert_no_partial_aggregate(r, row)
    mx_assert_no_persisted_weather_day(r, row)
    if (identical(row$pesos, "eqw"))
      expect_true(any(vapply(r$warnings, function(w) identical(w$id, "warning.combina_eqw"), NA)))
  }
})

## --- jobs 1 x 2: a mesma linha com jobs = 1 (state_dir separado) entrega a mesma versao, run_id e sha256 da linha com jobs = 2 ------
for (i in which(MX$jobs == 2L)) test_that(sprintf("jobs 1 x 2 identicos na linha %02d %s", i, paste(MX[i, ], collapse = "/")), {
  a <- mx_case(modifyList(MX[i, ], list(jobs = 1L)))
  expect_identical(a$digest, CASES[[i]]$digest)
  if (is.null(a$result)) expect_identical(a$error$id, CASES[[i]]$error$id) else expect_true(nzchar(a$digest$run_id))
})
