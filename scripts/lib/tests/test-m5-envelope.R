## test-m5-envelope.R -- contrato de saida dos 3 verbos do M5 (M5.9; ADR-0040) e ASCII do catalogo (ADR-0040 itens 7-8).
## Gates de PROCESSO (`Rscript --vanilla <copia>/scripts/prevcarga <verbo> ... --format json`) sobre instalacao-copia em tempdir +
## deck sintetico (fixture_store.R) + plugin-brinquedo com inferencia: um unico JSON em stdout com as 8 chaves do envelope, exit_code
## do JSON = exit do processo = exit do mesmo caso em modo texto, stderr vazio em json e stdout vazio no erro em texto.
## Tudo em tempdir; sem variaveis de ambiente; datas por lubridate com tz explicito. Cada FORBIDDEN tem o seu CONTROL.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
lib <- file.path(pc_root(), "scripts", "lib")

fx <- new.env()
sys.source(file.path(lib, "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
AREAS <- c("A", "B")
MD <- "toy-env"
ENV_KEYS <- c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version")

## plugin-brinquedo com treino + inferencia que LE o dia O (mesmo desenho do test-infer-model.R)
READ <- c(
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
root <- pc_install_copy(mutate = function(r) {
  files <- list("R/zz_dispatch.R" = .pc_toy_dispatch_lines("toy-30", native_min = 0L), "R/zz_read.R" = READ)
  tv <- pc_toy_variant("toy-30", run_yaml = list(backtest = list(protocol_default = "fixed")),
                       descriptor = list(uses_rng = TRUE), files = files, name = MD)
  file.copy(c(file.path(tv, MD), file.path(tv, "_toy-engine")), file.path(r, "models"), recursive = TRUE)
  writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
               sprintf("  X: {areas: [%s], loss: PX}", paste(AREAS, collapse = ", ")),
               "order: [X]", "members:", sprintf("  - {sigla: M1, mdir: %s, exogena: null}", MD)),
             file.path(r, "config", "topology.yaml"))
})
deck <- local({
  d <- tempfile("env-deck-")
  fx$prevcarga_fixture_deck(d, seed = 11, areas = AREAS, days = seq(O - 13L, O, by = "day"),
                            load_until = lubridate::ymd_hm("2026-10-01 09:00", tz = TZ))
  d
})
cfg <- tempfile("env-config-", fileext = ".yaml")
writeLines("schema_version: 1", cfg)           # config minimo: flags > config.yaml > default

## roda o verbo num `state` e devolve processo + JSON; `--config` e `--state` explicitos (nunca env)
run <- function(verb, args, state, fmt = "json", data = TRUE) {
  a <- c(verb, args, paste0("--state=", state), paste0("--config=", cfg), paste0("--origin=", format(O)), if (data) paste0("--data=", deck),
         paste0("--format=", fmt))
  r <- pc_cli(root, a)
  r$json <- if (identical(fmt, "json") && length(r$stdout)) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
  r
}
train_state <- local({
  st <- tempfile("env-state-"); dir.create(file.path(st, "artifacts"), recursive = TRUE)
  r <- run("train-models", c(paste0("--model=", MD), "--area=A"), st)
  if (!identical(r$status, 0L)) stop(paste(c(r$stdout, r$stderr), collapse = "\n"))
  st
})
fresh_state <- function(.local_envir = parent.frame()) pc5_state_tmp(.local_envir)

## ---- casos: verbo, args, exit esperado (sucesso = 0) --------------------------------------------------------------------------
CASES <- list(
  list(verb = "train-models", args = c(paste0("--model=", MD), "--area=A", "--dry-run"), exit = 0L, st = "fresh"),
  list(verb = "train-models", args = c("--area=A", "--subsistema=X"), exit = 2L, st = "fresh"),
  list(verb = "train-models", args = c("--area=INEXISTENTE", "--dry-run"), exit = 4L, st = "fresh"),
  list(verb = "train-models", args = c("--jobs=0"), exit = 8L, st = "fresh"),
  list(verb = "infer-model", args = c(paste0("--model=", MD), "--area=A"), exit = 0L, st = "trained"),
  list(verb = "infer-model", args = c(paste0("--model=", MD), "--area=A"), exit = 8L, st = "trained", data = FALSE),
  list(verb = "infer-model", args = c(paste0("--model=", MD), "--area=A"), exit = 9L, st = "tampered"),
  list(verb = "infer-model", args = c(paste0("--model=", MD), "--area=B"), exit = 13L, st = "trained"),
  list(verb = "registry", args = c("ls"), exit = 0L, st = "trained"),
  list(verb = "registry", args = c("pin", MD, "A", "v_00"), exit = 13L, st = "trained"))

## `tampered`: copia do estado treinado com 1 byte do artefato alterado (a soma sha256 do registry deixa de bater => exit 9)
tampered_state <- local({
  st <- tempfile("env-tampered-"); dir.create(st)
  file.copy(file.path(train_state, "artifacts"), st, recursive = TRUE)
  f <- list.files(file.path(st, "artifacts"), pattern = "^artifact[.]rds$", recursive = TRUE, full.names = TRUE)[[1L]]
  b <- readBin(f, "raw", file.size(f)); b[[length(b)]] <- as.raw(bitwXor(as.integer(b[[length(b)]]), 1L))
  writeBin(b, f)
  st
})
state_of <- function(k) switch(k, trained = train_state, tampered = tampered_state, fresh_state(parent.frame(2L)))

for (cs in CASES) local({
  cs <- cs
  lab <- sprintf("%s %s => exit %d", cs$verb, paste(cs$args, collapse = " "), cs$exit)
  test_that(sprintf("[FORBIDDEN] json: %s", lab), {
    st <- state_of(cs$st)
    dt <- !isFALSE(cs$data)
    j <- run(cs$verb, cs$args, st, data = dt)
    expect_identical(j$status, cs$exit, info = paste(lab, paste(j$stdout, j$stderr)))
    expect_length(j$stdout, 1L)                                  # um unico documento (uma linha), nada mais
    expect_length(j$stderr, 0L)                                  # avisos so em warnings[]
    expect_setequal(names(j$json), ENV_KEYS)
    expect_identical(j$json$exit_code, cs$exit)
    expect_identical(j$json$ok, cs$exit == 0L)
    expect_identical(j$json$verb, cs$verb)
    expect_true(is.list(j$json$warnings))
    expect_identical(is.null(j$json$error), cs$exit == 0L)
    if (cs$exit != 0L) expect_true(is.character(j$json$error$id) && nzchar(j$json$error$id))
    expect_true(is.character(j$json$version) && nzchar(j$json$version))
    pc_assert_ascii(j$raw, lab)
    ## o mesmo caso em texto: mesmo exit; erro => stderr, stdout vazio
    tx <- run(cs$verb, cs$args, st, fmt = "text", data = dt)
    expect_identical(tx$status, cs$exit)
    if (cs$exit != 0L) {
      expect_length(tx$stdout, 0L)
      expect_gt(length(tx$stderr), 0L)
    } else {
      expect_gt(length(tx$stdout), 0L)
    }
    pc_assert_ascii(tx$raw, paste(lab, "(texto)"))
  })
})

test_that("[CONTROL] o contrato tem dente: stdout de erro em texto vazio mas json do erro nao e vazio; exit 0 nao tem error", {
  e <- run("train-models", "--jobs=0", fresh_state())
  expect_false(is.null(e$json$error))
  s <- run("registry", "ls", train_state)
  expect_null(s$json$error)
  expect_false(identical(s$json$exit_code, e$json$exit_code))
})

test_that("[CONTROL] saida nao-ASCII e acusada por pc_assert_ascii", {
  expect_error(pc_assert_ascii(charToRaw("a\xc3\xa7"), "isca"), "byte")
})

## ---- ASCII do catalogo do M5 + ids orfaos ------------------------------------------------------------------------------------
M5_TSV <- c("features_facade", "registry_core", "training", "train_models", "inference", "infer_cut", "registry_lifecycle")
M5_CODE <- c("features.R", "training.R", "inference.R", "registry.R")

test_that("[FORBIDDEN] os 7 TSV do M5 sao ASCII puro (message, hint, id) e nao repetem id entre si nem com os demais", {
  e <- new.env(); sys.source(file.path(lib, "errors.R"), envir = e)
  all_cat <- e$prevcarga_errors_load(dir = file.path(lib, "errors"), refresh = TRUE)   # aborta em id duplicado entre TSV
  expect_false(anyDuplicated(all_cat$id) > 0L)
  ids <- character()
  for (t in M5_TSV) {
    p <- file.path(lib, "errors", paste0(t, ".tsv"))
    expect_true(file.exists(p), info = t)
    d <- utils::read.delim(p, stringsAsFactors = FALSE, quote = "", comment.char = "")
    expect_gt(nrow(d), 0L)
    for (col in c("id", "message", "hint"))
      expect_true(all(vapply(d[[col]], function(s) all(utf8ToInt(s) <= 127L), NA)), info = paste(t, col))
    expect_true(all(as.integer(readBin(p, "raw", file.size(p))) <= 127L), info = t)
    ids <- c(ids, d$id)
  }
  expect_false(anyDuplicated(ids) > 0L)
  expect_true(all(ids %in% all_cat$id))
  ## nada declarado dentro de errors.R alem da tabela interna minima
  expect_false(any(ids %in% fx$.prevcarga_builtin_errors$id))
})

test_that("[CONTROL] TSV-fixture com `c-cedilha` e acusado pelo loader e pela checagem de bytes", {
  d <- withr::local_tempdir()
  writeLines(c("id\tclass\tmessage\thint", "contract.fixture_acento\tcontract\tnao e ação\tcorrija"), file.path(d, "fx.tsv"),
             useBytes = FALSE)
  e <- new.env(); sys.source(file.path(lib, "errors.R"), envir = e)
  cond <- tryCatch(e$prevcarga_errors_load(dir = d, refresh = TRUE), prevcarga_error = function(c) c)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "contract.error_catalog_schema")
  m <- utils::read.delim(file.path(d, "fx.tsv"), stringsAsFactors = FALSE, quote = "", encoding = "UTF-8")$message
  expect_false(all(utf8ToInt(m) <= 127L))
  e$prevcarga_errors_load(dir = file.path(lib, "errors"), refresh = TRUE)            # restaura o cache do catalogo real
})

test_that("[FORBIDDEN] todo id citado em prevcarga_abort()/prevcarga_condition() do codigo do M5 existe no catalogo (sem orfao)", {
  e <- new.env(); sys.source(file.path(lib, "errors.R"), envir = e)
  cat <- e$prevcarga_errors_load(dir = file.path(lib, "errors"), refresh = TRUE)
  files <- c(file.path(lib, M5_CODE), file.path(lib, "cli", c("train-models.R", "infer-model.R", "registry.R")))
  cited <- character()
  for (f in files) {
    x <- readLines(f, encoding = "UTF-8", warn = FALSE)
    x <- x[!grepl("^\\s*#", x)]
    m <- regmatches(x, gregexpr("prevcarga_(abort|condition)\\(\\s*\"[a-z_]+[.][a-z0-9_]+\"", x))
    cited <- c(cited, sub(".*\"([^\"]+)\"$", "\\1", unlist(m)))
  }
  cited <- sort(unique(cited), method = "radix")
  expect_gt(length(cited), 5L)
  expect_identical(setdiff(cited, cat$id), character())
})

test_that("[CONTROL] id citado e ausente do catalogo e acusado como orfao", {
  e <- new.env(); sys.source(file.path(lib, "errors.R"), envir = e)
  cat <- e$prevcarga_errors_load(dir = file.path(lib, "errors"), refresh = TRUE)
  expect_identical(setdiff(c("contract.id_que_nao_existe", "config.usage"), cat$id), "contract.id_que_nao_existe")
})
