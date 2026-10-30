## test-cli_m6.R -- gramatica docopt do M6 (M6.9): verbos `forecast`, `export` e `features` por descoberta de scripts/lib/cli/*.R e o
## contrato de saida do ADR-0040 (JSON unico, stderr vazio, exit igual ao do texto, ASCII). Processo real: `Rscript --vanilla
## <copia>/scripts/prevcarga` (a fachada e executada, nunca editada). Exits 2/3/4/5/7/8 saem do pipeline REAL (gate-0, topologia, runtime);
## 0/9/13/14/15 precisam de uma rodada completa (8 membros treinados) e vem de uma copia de instalacao cujo forecast.R e um STUB
## (so na copia, em tempdir) que devolve o result/aborta por area-sentinela: o que se testa aqui e a fachada docopt + envelope, nao a rodada
## (a rodada real e test-forecast_e2e.R). `features` roda in-process com plugin-brinquedo e espioes. Tudo em tempdir; sem env vars.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-01-15")
ENVELOPE_KEYS <- c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version")
ERROR_KEYS <- c("class", "id", "message", "where", "hint", "details")
parse_json <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)

## --- instalacoes-copia ----------------------------------------------------------------------------------------------------
topo_file <- m6f_topology_file(tempfile("topo-") |> (\(d) { dir.create(d); d })())
with_topology <- function(r) file.copy(topo_file, file.path(r, "config", "topology.yaml"), overwrite = TRUE)

STUB <- c(
  "## STUB de teste (so nesta copia): prevcarga_forecast decide pela area-sentinela.",
  "prevcarga_forecast <- function(ctx, origin = NULL, areas = NULL, niveis = 'subsistemas', report = FALSE,",
  "                               report_format = 'html', skip_validate = FALSE, incluir_membros = FALSE) {",
  "  sc <- if (length(areas)) areas[[1L]] else 'OK'",
  "  switch(sc,",
  "    KC = prevcarga_abort('export.key_conflict', where = 'stub'),",
  "    ST = prevcarga_abort('state.combina_not_trained', where = 'stub'),",
  "    HN = prevcarga_abort('honesty.khat_after_origin', where = 'stub'),",
  "    BOOM = stop('falha inesperada do stub'),",
  "    NULL)",
  "  rep <- if (identical(sc, 'RP')) list(status = 'failed', path = NULL, error = 'contract.report_input')",
  "         else if (isTRUE(report)) list(status = 'ok', path = '/tmp/report.html') else list(status = 'skipped', path = NULL)",
  "  list(result = list(origem = '2026-01-15', available_until = '2026-01-15T09:00', versao = 'abc123def456',",
  "                     saida = '/tmp/x/forecasts', niveis = niveis, covered = list('A1'), stray = list(), khat = list(),",
  "                     arquivos = list(list(path = 'SIN.parquet', area = 'SIN', linhas = 7L, sha256 = strrep('a', 64))),",
  "                     ja_publicado = FALSE, report = rep, areas = as.list(areas), origin_flag = if (is.null(origin)) NULL else format(origin),",
  "                     skip_validate = skip_validate, incluir_membros = incluir_membros, report_format = report_format),",
  "       warnings = if (identical(sc, 'WARN')) list(list(id = 'completude.export_file_missing', message = 'x')) else list())",
  "}")
stub_forecast <- function(r) writeLines(STUB, file.path(r, "scripts", "lib", "forecast.R"))

root_stub <- pc_install_copy(mutate = function(r) { with_topology(r); stub_forecast(r) })
root_real <- pc_install_copy(plugins = TRUE, mutate = with_topology)

env <- pc_source_lib(c("errors"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
deck <- tempfile("m6cli-deck-")
env$prevcarga_fixture_deck(deck, seed = 11, areas = c(M6F_LEAVES, M6F_SS), days = seq(O - 39L, O, by = "day"),
                           load_until = lubridate::ymd_hm("2026-01-15 09:00", tz = TZ), tz = TZ)
state <- tempfile("m6cli-state-"); dir.create(state)

cli <- function(root, args, fmt = "json", deck_dir = deck)
  pc_cli(root, c(args, paste0("--data=", deck_dir), paste0("--state=", state), if (!is.null(fmt)) paste0("--format=", fmt)))

expect_envelope <- function(r, exit, id = NULL) {
  expect_identical(r$status, as.integer(exit))
  expect_length(r$stderr, 0L)
  expect_length(r$stdout, 1L)                                  # um unico documento, uma linha
  j <- parse_json(r)
  expect_identical(sort(names(j)), sort(ENVELOPE_KEYS))
  expect_identical(j$exit_code, as.integer(exit))
  expect_identical(j$ok, identical(as.integer(exit), 0L))
  if (!is.null(id)) {
    expect_identical(sort(names(j$error)), sort(ERROR_KEYS))
    expect_identical(j$error$id, id)
  }
  expect_identical(as.integer(r$status), as.integer(exit))
  pc_assert_ascii(r$raw, paste("envelope exit", exit))
  invisible(j)
}

## =====================================================================================================
## AC3 -- gramatica por descoberta (tabela prevcarga_verbs + Usage), sem --corte, reconcile nao e do M6
## =====================================================================================================
cl <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli"))
verbs <- cl$prevcarga_verbs_discover()

test_that("AC3: forecast/export/features existem com dono M6, registrados em cli/<verbo>.R como prevcarga_verb_<verbo>", {
  for (v in c("forecast", "export", "features")) {
    row <- verbs[verbs$verb == v, ]
    expect_identical(nrow(row), 1L)
    expect_identical(row$owner, "M6")
    expect_true(is.function(row$handler[[1L]]))
    f <- file.path(pc_root(), "scripts", "lib", "cli", paste0(v, ".R"))
    expect_true(file.exists(f))
    e <- new.env(); sys.source(f, envir = e)
    expect_identical(grep("^prevcarga_verb_", ls(e), value = TRUE), paste0("prevcarga_verb_", v))
    expect_true(is.function(e[[paste0("cmd_", v)]]))                   # handler no mesmo arquivo do registro
  }
  expect_false(identical(verbs$owner[verbs$verb == "reconcile"], "M6"))   # reconcile e do epico Reconcile (R.5)
  expect_false(file.exists(file.path(pc_root(), "scripts", "lib", "cli", "reconcile.R")) &&
                 any(grepl("owner *= *\"M6\"", readLines(file.path(pc_root(), "scripts", "lib", "cli", "reconcile.R")))))
})

test_that("AC3: nenhum verbo do M6 declara --corte; --origin e o unico flag humano de data; Usage lista os 3 verbos", {
  for (v in c("forecast", "export", "features")) {
    row <- verbs[verbs$verb == v, ]
    txt <- c(row$usage, unlist(row$options), unlist(row$scoped))
    expect_false(any(grepl("--corte|--as-of|--cut|morning|prever", txt)))
    expect_false(any(grepl("--(data-?|dia|date|from-date|to-date)", sub("--data=<dir>", "", txt))) && v != "export")
  }
  help <- cl$prevcarga_usage(verbs)
  for (v in c("forecast", "export", "features")) expect_true(any(grepl(paste0("^  prevcarga ", v, " "), strsplit(help, "\n")[[1L]])))
})

test_that("AC3: forecast --corte 09:00 => exit 8 config.usage; --origin=2026-13-01 => exit 8 config.bad_origin; sem aviso de deprecacao", {
  r <- cli(root_real, c("forecast", "--corte", "09:00"))
  j <- expect_envelope(r, 8L, "config.usage")
  expect_identical(j$error$where, "--corte")
  r2 <- cli(root_real, c("forecast", "--origin=2026-13-01"))
  expect_envelope(r2, 8L, "config.bad_origin")
  t1 <- cli(root_real, c("forecast", "--corte", "09:00"), fmt = NULL)
  expect_identical(t1$status, 8L); expect_length(t1$stdout, 0L)
  expect_false(any(grepl("deprec|obsolet", c(t1$stderr, r$stdout), ignore.case = TRUE)))
})

test_that("AC3: --corte (do backtest/plan) e flags escopadas de outros verbos nao vazam para o M6", {
  for (v in list(c("export", "--from=/x"), c("features", "--model=m", "--area=a"))) {
    r <- cli(root_stub, c(v, "--corte=09:00"))
    expect_envelope(r, 8L, "config.usage")
  }
  r <- cli(root_stub, c("forecast", "--from=/x"))                         # --from e do export
  expect_envelope(r, 8L, "config.usage")
  r <- cli(root_stub, c("forecast", "--horizon=3"))                       # --horizon e do train-models
  expect_envelope(r, 8L, "config.usage")
})

test_that("AC3 [CONTROL]: sem scripts/lib/cli/features.R o verbo some da descoberta (vira stub planejado) e features reprova", {
  root <- pc_install_copy(mutate = function(r) { with_topology(r); unlink(file.path(r, "scripts", "lib", "cli", "features.R")) })
  v2 <- cl$prevcarga_verbs_discover(dir = file.path(root, "scripts", "lib", "cli"))
  expect_true(is.null(v2$handler[[which(v2$verb == "features")]]))        # sem arquivo: so o stub planejado
  expect_true(is.function(verbs$handler[[which(verbs$verb == "features")]]))
  r <- cli(root, c("features", "--model=random-forest", "--area=A1"))
  expect_false(identical(parse_json(r)$error$id, "topology.model_undeclared"))
  expect_identical(r$status, 1L)
  expect_identical(parse_json(r)$error$id, "todo.not_implemented")
})

## =====================================================================================================
## AC1/AC2 -- envelope JSON, ASCII, exit igual ao do texto e a matriz de exits do M6
## =====================================================================================================
test_that("AC1/AC2: exit 0 -- sucesso em JSON: um documento, stderr vazio, result completo; --origin e as flags chegam ao pipeline", {
  r <- cli(root_stub, c("forecast", "--origin=2026-01-15", "--niveis=tudo", "--incluir-membros", "--report", "--report-format=pdf",
                        "--skip-validate", "--area=A1", "--area=B1"))
  j <- expect_envelope(r, 0L)
  expect_null(j$error)
  expect_identical(j$result$origin_flag, "2026-01-15")
  expect_identical(j$result$niveis, "tudo")
  expect_identical(unlist(j$result$areas), c("A1", "B1"))
  expect_true(j$result$skip_validate); expect_true(j$result$incluir_membros)
  expect_identical(j$result$report_format, "pdf")
  expect_identical(j$result$report$status, "ok")
  expect_match(j$run_id, "^[0-9]{8}T[0-9]{6}Z-")
})

test_that("AC1: --subsistema expande em folhas pela topologia; all => todas; exclusivo com --area; fora => exit 4", {
  r <- cli(root_stub, c("forecast", "--subsistema=SS1"))
  expect_identical(unlist(expect_envelope(r, 0L)$result$areas), c("A1", "A2"))
  r <- cli(root_stub, c("forecast", "--subsistema=all"))
  expect_length(expect_envelope(r, 0L)$result$areas, 0L)
  expect_envelope(cli(root_stub, c("forecast", "--subsistema=SS9")), 4L, "topology.area_undeclared")
  expect_envelope(cli(root_stub, c("forecast", "--subsistema=SS1", "--area=A1")), 8L, "config.usage")
})

test_that("AC1: texto == JSON no exit, stdout ASCII, avisos do catalogo viram warnings do envelope (e stderr no texto)", {
  t <- cli(root_stub, c("forecast", "--area=WARN"), fmt = NULL)
  expect_identical(t$status, 0L)
  expect_true(any(grepl("^forecast ", t$stdout)) && identical(t$stdout[[length(t$stdout)]], "OK"))
  expect_true(any(grepl("aviso [completude.export_file_missing]", t$stderr, fixed = TRUE)))
  pc_assert_ascii(t$raw, "forecast texto")
  j <- expect_envelope(cli(root_stub, c("forecast", "--area=WARN")), 0L)
  expect_identical(j$warnings[[1L]]$id, "completude.export_file_missing")
})

test_that("AC2: exits 7 (stdout vazio) da matriz com o runtime quebrado; classe runtime", {
  bad <- pc_install_copy(broken_tz = TRUE, mutate = function(r) { with_topology(r); stub_forecast(r) })
  ## o setter quebrado so devolve o fuso observado: com o shell ja em UTC ele "acerta" sem querer; o shell sujo (como em
  ## test-cli-process.R) e o que torna a quebra observavel
  r <- pc_with_env(list(TZ = "America/Sao_Paulo"), cli(bad, c("forecast"), fmt = NULL))
  expect_identical(r$status, 7L)
  expect_length(r$stdout, 0L)
  expect_true("prevcarga: erro [runtime] (exit 7)" %in% r$stderr)
  pc_assert_ascii(r$raw, "exit 7")
})

test_that("AC1/AC2: erros do pipeline REAL (fixtures em tempdir): exit 4 area fora, 3 dia O completo, 5 historico curto", {
  j4 <- expect_envelope(cli(root_real, c("forecast", "--origin=2026-01-15", "--skip-validate", "--area=ZZ")), 4L, "topology.area_undeclared")
  expect_identical(j4$error$class, "topology"); expect_identical(j4$error$where, "ZZ")
  j3 <- expect_envelope(cli(root_real, c("forecast", "--origin=2026-01-14", "--skip-validate")), 3L,
                        "completude.forecast_origin_day_complete")
  expect_identical(j3$error$class, "completude")
  j5 <- expect_envelope(cli(root_real, c("forecast", "--origin=2026-01-15")), 5L, "window.window_train_deficit")
  expect_identical(j5$error$class, "window")
  for (a in list(c("--origin=2026-01-15", "--skip-validate", "--area=ZZ"), c("--origin=2026-01-14", "--skip-validate"), "--origin=2026-01-15")) {
    t <- cli(root_real, c("forecast", a), fmt = NULL)
    expect_length(t$stdout, 0L)                                         # texto: erro so em stderr
    pc_assert_ascii(t$raw, "erro texto")
    expect_true(any(grepl("^prevcarga: erro \\[", t$stderr)))
  }
  expect_identical(cli(root_real, c("forecast", "--origin=2026-01-14", "--skip-validate"), fmt = NULL)$status, 3L)
})

test_that("AC1/AC2: exit 2 -- manifest sem timezone (contrato do deck); exit 2 igual em texto", {
  bad <- tempfile("m6cli-nomf-"); dir.create(bad)
  file.copy(list.files(deck, full.names = TRUE, all.files = TRUE, no.. = TRUE), bad, recursive = TRUE)
  mf <- jsonlite::read_json(file.path(bad, "manifest.json"))
  mf$timezone <- NULL
  jsonlite::write_json(mf, file.path(bad, "manifest.json"), auto_unbox = TRUE)
  r <- cli(root_real, c("forecast", "--origin=2026-01-15", "--skip-validate"), deck_dir = bad)
  j <- expect_envelope(r, 2L)
  expect_identical(j$error$class, "contract")
  t <- cli(root_real, c("forecast", "--origin=2026-01-15", "--skip-validate"), fmt = NULL, deck_dir = bad)
  expect_identical(t$status, 2L)
})

test_that("AC2: exits 13 state, 15 honesty (khat_after_origin propagado), 9 integrity, 14 partial (entrega publicada, relatorio falho)", {
  j13 <- expect_envelope(cli(root_stub, c("forecast", "--area=ST")), 13L, "state.combina_not_trained")
  expect_identical(j13$error$class, "state")
  j15 <- expect_envelope(cli(root_stub, c("forecast", "--area=HN")), 15L, "honesty.khat_after_origin")
  expect_identical(j15$error$class, "honesty")
  j9 <- expect_envelope(cli(root_stub, c("forecast", "--area=KC")), 9L, "export.key_conflict")
  expect_identical(j9$error$class, "integrity")
  j14 <- expect_envelope(cli(root_stub, c("forecast", "--area=RP", "--report")), 14L, "partial.report_failed")
  expect_identical(j14$error$class, "partial")
  expect_identical(j14$error$where, "contract.report_input")
  expect_identical(j14$result$report$status, "failed")             # a entrega fica e vai no envelope
  expect_identical(j14$result$saida, "/tmp/x/forecasts")
  ## texto: exit igual, stdout vazio, erro em stderr (ASCII)
  for (cs in list(c("ST", 13L), c("HN", 15L), c("KC", 9L), c("RP", 14L))) {
    t <- cli(root_stub, c("forecast", paste0("--area=", cs[[1L]])), fmt = NULL)
    expect_identical(t$status, as.integer(cs[[2L]]))
    expect_length(t$stdout, 0L)
    pc_assert_ascii(t$raw, paste("texto", cs[[1L]]))
  }
})

test_that("AC1: exits da matriz so existem na tabela do ADR-0040; --niveis/--report-format invalidos => 8 config.usage", {
  tabela <- unname(pc_source_lib("errors")$prevcarga_exit_codes)
  for (a in c("ST", "HN", "KC", "RP", "OK")) {
    r <- cli(root_stub, c("forecast", paste0("--area=", a)))
    expect_true(r$status %in% tabela)
  }
  expect_envelope(cli(root_stub, c("forecast", "--niveis=x")), 8L, "config.usage")
  expect_envelope(cli(root_stub, c("forecast", "--report-format=docx")), 8L, "config.usage")
  expect_envelope(cli(root_stub, c("export", "--from=/x", "--niveis=x")), 8L, "config.usage")
})

test_that("AC1: traceback so com --debug: inesperado => exit 1 generic.unexpected, stderr vazio em JSON sem --debug", {
  r <- cli(root_stub, c("forecast", "--area=BOOM"))
  j <- expect_envelope(r, 1L, "generic.unexpected")
  expect_identical(j$error$class, "generic")
  d <- cli(root_stub, c("forecast", "--area=BOOM", "--debug"))
  expect_identical(d$status, 1L)
  expect_length(d$stdout, 1L)
  expect_gt(length(d$stderr), 0L)                                        # [CONTROL] com --debug o traceback aparece
  t <- cli(root_stub, c("forecast", "--area=BOOM"), fmt = NULL)
  expect_false(any(grepl("prevcarga_forecast|withCallingHandlers", t$stderr)))
  pc_assert_ascii(d$raw, "debug")
})

test_that("AC1: nenhum byte > 127 em stdout/stderr (texto), mesmo com acento na entrada", {
  r <- cli(root_stub, c("forecast", "--area=ção"), fmt = NULL)
  expect_true(pc_assert_ascii(r$raw, "entrada com acento"))
})

## =====================================================================================================
## AC5 -- export standalone: chave reconstruida do run.json, mesmo manifest do forecast
## =====================================================================================================
SSS <- c("SS1", "SS2"); COVERED <- c("A1", "A2", "B1")
make_scratch <- function(origem = "2026-01-15", au = "2026-01-15T09:00", versao = "abc123def456", tz = TZ) {
  s <- tempfile("m6cli-scratch-"); rows <- list()
  put <- function(rel, area, n) {
    p <- file.path(s, rel); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(data.frame(area = area, y_pred = seq_len(n) + 0.5), p, compression = "snappy")
    rows[[length(rows) + 1L]] <<- list(path = rel, area = area, linhas = n, sha256 = digest::digest(file = p, algo = "sha256"))
  }
  put("SIN.parquet", "SIN", 7L)
  for (ss in SSS) put(paste0("subsistemas/", ss, ".parquet"), ss, 6L)
  for (a in COVERED) put(paste0("folhas/", a, ".parquet"), a, 4L)
  run <- list(origem = origem, available_until = au, available_until_por_area = list(A1 = au), timezone = tz, load_label = "end",
              weather_issue_cutoff = "available_until",
              khat = lapply(SSS, function(ss) list(ss = ss, versao = "v_0123456789abcdef", khat_origin = origem)),
              covered = as.list(COVERED), stray = list(), closed = as.list(SSS), sin_closed = TRUE, members = structure(list(), names = character()),
              arquivos = rows, versao = versao, version = "0.1.0", runtime_id = "rt-1")
  writeLines(as.character(jsonlite::toJSON(run, auto_unbox = TRUE, pretty = TRUE)), file.path(s, "run.json"))
  s
}
exp_env <- pc_source_lib(c("errors", "schema_long", "exports"))

test_that("AC5: export --from <scratch> --niveis tudo entrega em forecasts/<O>/<au>/<versao>/ com o manifest que o forecast produziria", {
  sc <- make_scratch()
  st <- tempfile("m6cli-exp-state-"); dir.create(st)
  r <- pc_cli(root_stub, c("export", paste0("--from=", sc), "--niveis=tudo", paste0("--state=", st), paste0("--data=", deck), "--format=json"))
  j <- expect_envelope(r, 0L)
  expect_match(j$result$saida, "/forecasts/2026-01-15/20260115T0900/abc123def456$")
  expect_identical(normalizePath(j$result$saida, winslash = "/"), normalizePath(file.path(st, "forecasts/2026-01-15/20260115T0900/abc123def456"), winslash = "/"))
  paths <- vapply(j$result$arquivos, function(a) a$path, "")
  expect_true(all(c("SIN.parquet", "subsistemas/SS1.parquet", "folhas/A1.parquet", "folhas/B1.parquet") %in% paths))
  expect_false(j$result$ja_publicado)
  ## mesmo manifest que a entrega direta (a mesma chave) produziria, exceto gerado_em
  ref_state <- tempfile("m6cli-ref-"); dir.create(ref_state)
  key <- exp_env$prevcarga_export_key(O, lubridate::ymd_hm("2026-01-15 09:00", tz = TZ), "abc123def456", TZ)
  ref <- exp_env$prevcarga_export(sc, key, niveis = "tudo", incluir_membros = FALSE, ctx = list(state_dir = ref_state, generated_at = "x"))
  m1 <- jsonlite::read_json(file.path(j$result$saida, "manifest.json")); m2 <- jsonlite::read_json(file.path(ref$saida, "manifest.json"))
  m1$gerado_em <- NULL; m2$gerado_em <- NULL
  expect_identical(m1, m2)
  expect_match(jsonlite::read_json(file.path(j$result$saida, "manifest.json"))$gerado_em, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
  ## segunda entrega do mesmo scratch: no-op (ja_publicado), mesma chave
  r2 <- pc_cli(root_stub, c("export", paste0("--from=", sc), "--niveis=tudo", paste0("--state=", st), paste0("--data=", deck), "--format=json"))
  expect_true(expect_envelope(r2, 0L)$result$ja_publicado)
  ## texto ASCII
  t <- pc_cli(root_stub, c("export", paste0("--from=", sc), "--niveis=tudo", paste0("--state=", st), paste0("--data=", deck)))
  expect_identical(t$status, 0L); expect_length(t$stderr, 0L)
  pc_assert_ascii(t$raw, "export texto")
})

test_that("AC5: --niveis invalido => exit 8 config.usage; scratch sem run.json/ausente => exit 13; a chave nao vem de flag (--origin e ignorado)", {
  sc <- make_scratch()
  st <- tempfile("m6cli-exp-state-"); dir.create(st)
  base <- c(paste0("--state=", st), paste0("--data=", deck), "--format=json")
  expect_envelope(pc_cli(root_stub, c("export", paste0("--from=", sc), "--niveis=zzz", base)), 8L, "config.usage")
  expect_envelope(pc_cli(root_stub, c("export", paste0("--from=", tempfile("nada-")), base)), 13L, "state.nothing_to_deliver")
  empty <- tempfile("m6cli-empty-"); dir.create(empty)
  expect_envelope(pc_cli(root_stub, c("export", paste0("--from=", empty), base)), 13L, "state.nothing_to_deliver")
  j <- expect_envelope(pc_cli(root_stub, c("export", paste0("--from=", sc), "--origin=2030-01-01", base)), 0L)
  expect_match(j$result$saida, "/forecasts/2026-01-15/")
  expect_envelope(pc_cli(root_stub, c("export", base)), 8L, "config.usage")          # --from obrigatorio (docopt)
})

## =====================================================================================================
## AC4 -- features: passthrough da FE-fachada, sem train/infer; modelo/area fora => exit 4
## =====================================================================================================
fe <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "deck_reader", "validation", "dates",
                      "registry", "features", "training", "inference"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fe)
sys.source(file.path(pc_root(), "scripts", "lib", "cli", "features.R"), envir = fe)
suppressMessages(fe$prevcarga_runtime_enforce())
fe$prevcarga_registry_reset(); fe$prevcarga_plugin_unload_all()

FE_AREAS <- c("A", "B")
FE_O <- lubridate::ymd("2026-10-01")
FE_READ <- c(
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
fe_models <- local({
  rt <- tempfile("fe-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
  vr <- pc_toy_variant("toy-30", run_yaml = list(horizon_days = 1L, backtest = list(protocol_default = "fixed")),
                       descriptor = list(uses_rng = TRUE, data_lag_days = 2L),
                       files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines("toy-30", native_min = 0L), "R/zz_read.R" = FE_READ),
                       name = "toy-lg")
  file.copy(file.path(vr, "toy-lg"), file.path(rt, "models"), recursive = TRUE)
  file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
  rt
})
fe_deck <- local({
  d <- tempfile("fe-deck-")
  fe$prevcarga_fixture_deck(d, seed = 11, areas = FE_AREAS, days = seq(FE_O - 20L, FE_O, by = "day"),
                            load_until = lubridate::ymd_hm("2026-10-01 09:00", tz = TZ))
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", sprintf("  X: {areas: [%s], loss: PX}", paste(FE_AREAS, collapse = ", ")),
               "order: [X]", "members:", "  - {sigla: TOYLG, mdir: toy-lg, exogena: null}"), file.path(d, "topology.yaml"))
  d
})
fe_state <- function(.local_envir = parent.frame()) pc5_state_tmp(.local_envir = .local_envir)
fe_ctx <- function(state, format = "json")
  list(root = fe_models, data_dir = fe_deck, topology_path = file.path(fe_deck, "topology.yaml"), weather_issue_cutoff = "available_until",
       origin = FE_O, state_dir = state, code_commit = "c0ffee", renv_lock_hash = "ab12", format = format)
fe_args <- function(model = "toy-lg", area = "A") list(`--model` = model, `--area` = area)
fe_spy <- function(name, frame = parent.frame()) {
  spy <- new.env(); spy$n <- 0L
  orig <- get(name, envir = fe)
  assign(name, function(...) { spy$n <- spy$n + 1L; orig(...) }, envir = fe)
  withr::defer(assign(name, orig, envir = fe), envir = frame)
  spy
}
expect_pc <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id); expect_identical(cond$exit_code, as.integer(exit))
}

test_that("AC4: features materializa a feature_table em <state>/scratch/features/<m>/<a>/ via prevcarga_build_features (1 chamada), sem train/infer", {
  st <- fe_state(); ctx <- fe_ctx(st)
  member <- list(sigla = "TOYLG", mdir = "toy-lg", exogena = NA_character_)
  suppressWarnings(fe$prevcarga_train(member, "A", FE_O, ctx))
  sp_b <- fe_spy("prevcarga_build_features")
  sp_t <- fe_spy("prevcarga_train"); sp_i <- fe_spy("prevcarga_infer"); sp_c <- fe_spy("prevcarga_infer_cell")
  sp_pt <- fe_spy("prevcarga_plugin_train"); sp_pi <- fe_spy("prevcarga_plugin_infer")
  res <- fe$cmd_features(ctx, fe_args())
  expect_identical(sp_b$n, 1L)
  for (s in list(sp_t, sp_i, sp_c, sp_pt, sp_pi)) expect_identical(s$n, 0L)
  expect_identical(res$model, "toy-lg"); expect_identical(res$area, "A"); expect_identical(res$origin, "2026-10-01")
  expect_identical(normalizePath(dirname(res$saida), winslash = "/"), normalizePath(file.path(st, "scratch", "features", "toy-lg", "A"), winslash = "/"))
  expect_true(file.exists(res$saida))
  tab <- arrow::read_parquet(res$saida)
  expect_identical(nrow(tab), res$linhas); expect_identical(ncol(tab), res$colunas)
  expect_identical(unlist(res$nomes), names(tab))
  expect_false(any(grepl("^\\.features-", list.files(dirname(res$saida), all.files = TRUE))))   # escrita atomica sem sobra
  ## texto: linhas ASCII
  txt <- fe$cmd_features(fe_ctx(st, "text"), fe_args())
  expect_true(is.character(txt)); expect_true(all(utf8ToInt(paste(txt, collapse = "")) < 128L))
})

test_that("AC4 [CONTROL]: a origem muda a feature_table (carga do dia O entra); registry intacto (so le o artefato)", {
  st <- fe_state(); ctx <- fe_ctx(st)
  member <- list(sigla = "TOYLG", mdir = "toy-lg", exogena = NA_character_)
  suppressWarnings(fe$prevcarga_train(member, "A", FE_O, ctx))
  h0 <- pc5_tree_hash(file.path(st, "artifacts"))
  base <- arrow::read_parquet(fe$cmd_features(ctx, fe_args())$saida)
  pert <- fe$prevcarga_fixture_perturb(fe_deck, "load", after = lubridate::ymd_hm("2026-10-01 00:00", tz = TZ) - lubridate::minutes(1L),
                                       at_or_before = lubridate::ymd_hm("2026-10-01 09:00", tz = TZ), perturb = function(v) v + 1000)
  ctx2 <- ctx; ctx2$data_dir <- pert
  after <- arrow::read_parquet(fe$cmd_features(ctx2, fe_args())$saida)
  expect_false(identical(base$today, after$today))
  expect_identical(pc5_tree_hash(file.path(st, "artifacts")), h0)
})

test_that("AC4: modelo ou area fora da topologia => exit 4; artefato ausente => exit 13; sem --model/--area => config.usage", {
  st <- fe_state(); ctx <- fe_ctx(st)
  expect_pc(fe$cmd_features(ctx, fe_args(model = "nao-existe")), "topology.model_undeclared", 4L)
  expect_pc(fe$cmd_features(ctx, fe_args(area = "ZZ")), "topology.area_undeclared", 4L)
  expect_pc(fe$cmd_features(ctx, list(`--area` = "A")), "config.usage", 8L)
  expect_pc(fe$cmd_features(ctx, list(`--model` = "toy-lg")), "config.usage", 8L)
  cond <- tryCatch(fe$cmd_features(ctx, fe_args()), prevcarga_error = function(e) e)       # nenhum artefato treinado
  expect_identical(cond$exit_code, 13L)
  expect_false(dir.exists(file.path(st, "scratch")))                                       # nada materializado em erro
})

test_that("AC4: processo real -- features com modelo/area fora => exit 4 em JSON, stderr vazio", {
  j <- expect_envelope(cli(root_stub, c("features", "--model=nao-existe", "--area=A1", "--origin=2026-01-15")), 4L, "topology.model_undeclared")
  expect_identical(j$error$class, "topology")
  j2 <- expect_envelope(cli(root_stub, c("features", "--model=random-forest", "--area=ZZ", "--origin=2026-01-15")), 4L, "topology.area_undeclared")
  expect_identical(j2$error$where, "ZZ")
  expect_envelope(cli(root_stub, c("features", "--model=random-forest")), 8L, "config.usage")   # --area obrigatorio (docopt)
})
