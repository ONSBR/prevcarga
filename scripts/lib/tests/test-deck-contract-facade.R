## test-deck-contract-facade.R -- verificacao do epico E1 (contrato de decks e opcoes) pela fachada prevcarga_main: exit code, classe do erro
## e stderr. Nao repete as suites de E1-T1..T6; cada criterio do epico tem um caso e seu CONTROL (o mesmo cenario sem a condicao passa).
## Fixtures sinteticas em tempdir (deck do helper-deck.R com kind via pc_deck_manifest(mutate=), fixture m6f para o forecast, config.yaml
## sintetico); runtime injetado; sem AWS, sem Docker, sem env vars.
library(testthat)
skip_if_not_installed("docopt")
LIB <- file.path(pc_root(), "scripts", "lib")
options(prevcarga.lib = LIB)
## a fachada real roda com errors/runtime/config/banner/cli no ambiente global (o launcher os carrega la; os modulos do verbo tambem)
E <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli"), env = globalenv())
VERBS <- E$prevcarga_verbs_discover()
real_handler <- function(v) VERBS$handler[[match(v, VERBS$verb)]]

## --- harness: exit, stdout e stderr da fachada -----------------------------------------------------------------------------
fc_run <- function(argv, handlers = NULL, root = pc_root()) {
  err <- character(); code <- NULL
  out <- utils::capture.output(
    err <- utils::capture.output(code <- E$prevcarga_main(argv, root = root, runtime = function() NULL, handlers = handlers),
                                 type = "message"),
    type = "output")
  list(code = as.integer(code), stdout = out, stderr = err)
}
## envelope de erro em --format=json (um unico JSON em stdout, ADR-0040)
fc_err <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)$error

## deck sintetico do helper-deck.R: kind train (ausente no manifest) ou inference com o bloco inference completo
fc_deck <- function(kind = "train", origin = "2023-06-01", .frame = parent.frame()) {
  dir <- withr::local_tempdir("fc-deck-", .local_envir = .frame)
  pc_deck_manifest(dir, mutate = function(m) {
    if (identical(kind, "inference")) {
      m$kind <- "inference"
      m$inference <- list(origin = origin, available_until = paste0(origin, "T08:00:00"), train_deck_hash = strrep("a", 64L),
                          windows = structure(list(), names = character()))
    }
    m
  })
  dir
}
ascii <- function(x) !any(grepl("[^\x01-\x7f]", x, useBytes = TRUE))

## --- [E1-AC2] kind x opcao nos dois sentidos; train-models recusa --data-inference antes de treinar -------------------------
test_that("[E1-AC2] kind x opcao nos dois sentidos (exit 2, deck_kind_mismatch) e train-models com --data-inference (deck_role_refused, zero treino)", {
  trn <- fc_deck("train"); inf <- fc_deck("inference")
  ## o handler real do forecast nunca roda: o contrato do deck falha antes de qualquer cobertura
  a <- fc_run(c("forecast", paste0("--data-inference=", trn), "--format=json"))
  expect_identical(a$code, 2L)
  ea <- fc_err(a)
  expect_identical(ea$id, "contract.deck_kind_mismatch")
  expect_identical(ea$where, "--data-inference")
  expect_identical(ea$details$kind, "train")
  b <- fc_run(c("forecast", paste0("--data-train=", inf), "--format=json"))
  expect_identical(b$code, 2L)
  eb <- fc_err(b)
  expect_identical(eb$id, "contract.deck_kind_mismatch")
  expect_identical(eb$where, "--data-train")
  expect_identical(eb$details$kind, "inference")
  ## train-models + --data-inference: recusa de papel antes do handler; espiao de prevcarga_train fica em zero
  n <- 0L
  assign("prevcarga_train", function(...) { n <<- n + 1L; NULL }, envir = globalenv())
  withr::defer(rm("prevcarga_train", envir = globalenv()))
  c1 <- fc_run(c("train-models", paste0("--data-inference=", inf), "--origin", "2023-06-01", "--format=json"))
  expect_identical(c1$code, 2L)
  expect_identical(fc_err(c1)$id, "contract.deck_role_refused")
  expect_identical(fc_err(c1)$where, "--data-inference")
  expect_identical(n, 0L)
  ## origem divergente (deck inference de 06-01 pedido em 06-02): exit 2
  d <- fc_run(c("forecast", paste0("--data-inference=", inf), "--origin", "2023-06-02", "--format=json"))
  expect_identical(d$code, 2L)
  expect_identical(fc_err(d)$id, "contract.deck_origin_mismatch")
  ## CONTROL: o papel certo passa o contrato (handler substituido por um stub que devolve ok)
  stub <- function(ctx, args) list(ok = TRUE)
  expect_identical(fc_run(c("forecast", paste0("--data-train=", trn), "--format=json"), handlers = list(forecast = stub))$code, 0L)
  expect_identical(fc_run(c("forecast", paste0("--data-inference=", inf), "--origin", "2023-06-01", "--format=json"),
                          handlers = list(forecast = stub))$code, 0L)
  expect_identical(fc_run(c("train-models", paste0("--data-train=", trn), "--origin", "2023-06-01", "--format=json"),
                          handlers = list(`train-models` = stub))$code, 0L)
  expect_true(ascii(c(a$stdout, b$stdout, c1$stdout, a$stderr, b$stderr, c1$stderr)))
})

## --- fixture m6f (forecast de ponta a ponta com espioes de infer/train, instalados depois que a fachada carrega os modulos) ---
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- M6F_ORIGIN
OS <- format(O, "%Y-%m-%d")
BASE <- withr::local_tempdir("fc-base-")
DECK <- m6f_deck(env, file.path(BASE, "deck"))
TOPO <- m6f_topology_file(BASE)
FX <- m6f_ctx(DECK, file.path(BASE, "state"), TOPO, list(data_role = "train"))
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)                       # combina + k-hat de O no registry do FX
MEM <- env$prevcarga_members(path = TOPO, root = pc_root())
HW <- MEM[MEM$sigla == "HW", ][1L, ]

lst <- function(d) sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
## raiz de artefatos: combina/k-hat do FX + HW com versao de O em todas as folhas menos `sem_hw`; RF/LGBM com latest
make_root <- function(sem_hw = character(), .frame = parent.frame()) {
  art <- withr::local_tempdir("fc-art-", .local_envir = .frame)
  file.copy(list.files(file.path(FX$state_dir, "artifacts"), full.names = TRUE), art, recursive = TRUE)
  actx <- m6f_ctx(DECK, file.path(BASE, "unused"), TOPO, list(artifacts_dir = art))
  for (a in setdiff(M6F_LEAVES, sem_hw)) m6f_fake_artifact(env, HW, a, O, actx)
  for (sg in c("RF", "LGBM")) for (a in M6F_LEAVES) {
    m <- MEM[MEM$sigla == sg, ][1L, ]
    writeLines(m6f_fake_artifact(env, m, a, O, actx), file.path(env$prevcarga_registry_key(m, a, actx), "latest.txt"))
  }
  art
}
## deck m6f marcado como inference (o manifest do fixture e preservado; so kind e bloco inference entram)
fc_inference_deck <- function(.frame = parent.frame()) {
  d <- withr::local_tempdir("fc-infdeck-", .local_envir = .frame)
  file.copy(list.files(DECK, full.names = TRUE), d, recursive = TRUE)
  f <- file.path(d, "manifest.json")
  m <- jsonlite::read_json(f)
  m$kind <- "inference"
  m$inference <- list(origin = OS, available_until = paste0(OS, "T09:00:00"), train_deck_hash = strrep("a", 64L),
                      windows = structure(list(), names = character()))
  jsonlite::write_json(m, f, auto_unbox = TRUE, pretty = TRUE)
  d
}
## handler do forecast: o real, com topologia sintetica e espioes de infer/train no ambiente global (onde a fachada sourceia os modulos)
fc_forecast_handler <- function(log) function(ctx, args) {
  assign("prevcarga_infer_cell", m6f_infer_spy(globalenv(), log), envir = globalenv())
  assign("prevcarga_train", m6f_train_spy(globalenv(), log), envir = globalenv())
  suppressMessages(get("prevcarga_runtime_enforce", envir = globalenv())())     # o runtime injetado da fachada e NULL; o carimbo do forecast pede o enforce
  ctx$topology_path <- TOPO
  ctx$weather_issue_cutoff <- "available_until"
  if (is.null(ctx$code_commit)) ctx$code_commit <- "c0ffee"
  real_handler("forecast")(ctx, args)
}
fc_forecast <- function(opts, extra = character(), .frame = parent.frame()) {
  log <- new.env(); log$infer <- list(); log$train <- list()
  st <- withr::local_tempdir("fc-state-", .local_envir = .frame)
  r <- fc_run(c("forecast", opts, "--origin", OS, "--state", st, "--skip-validate", extra), handlers = list(forecast = fc_forecast_handler(log)))
  withr::defer(suppressWarnings(rm(list = c("prevcarga_infer_cell", "prevcarga_train"), envir = globalenv())), envir = .frame)
  r$log <- log; r$state <- st
  r
}
long_of <- function(r) {
  f <- file.path(sub("^saida: ", "", r$stdout[grepl("^saida: ", r$stdout)][1L]), "SIN.parquet")   # o LONG entregue do escopo sin
  stopifnot(file.exists(f))
  f
}

## --- [E1-AC3] deck de inferencia sem a versao de O: exit 13, zero treino, raiz intacta -----------------------------------
test_that("[E1-AC3] forecast --data-inference com raiz sem a versao de O: exit 13 state.inference_artifact_missing, zero treino, raiz intacta", {
  inf <- fc_inference_deck()
  art <- make_root(sem_hw = "A1")
  antes <- lst(art)
  r <- fc_forecast(c(paste0("--data-inference=", inf), paste0("--data-artifacts=", art), "--format=json"))
  expect_identical(r$code, 13L)
  e <- fc_err(r)
  expect_identical(e$id, "state.inference_artifact_missing")
  expect_identical(e$details$membro, "HW")
  expect_identical(e$details$area, "A1")
  expect_identical(e$details$origem, OS)
  expect_length(r$log$train, 0L)
  expect_identical(lst(art), antes)
  expect_true(ascii(c(r$stdout, r$stderr)))
  ## CONTROL: raiz completa => o mesmo deck de inferencia entrega, sem treinar
  ok <- make_root()
  s <- fc_forecast(c(paste0("--data-inference=", inf), paste0("--data-artifacts=", ok), "--format=json"))
  expect_identical(s$code, 0L)
  expect_length(s$log$train, 0L)
  expect_gt(length(s$log$infer), 0L)
})

## --- [E1-AC4] sem apelidos (ADR-044, clean break): a opcao antiga e opcao desconhecida --------------------------------------
test_that("[E1-AC4] --data e --artifacts-dir sao opcao desconhecida (erro de uso padrao, zero entrega); os nomes novos entregam o LONG", {
  art <- make_root()
  a <- fc_forecast(c(paste0("--data=", DECK), paste0("--artifacts-dir=", art), "--format=json"))
  expect_identical(a$code, 8L)                                   # config.usage: o erro de uso padrao do docopt (catalogo: classe config)
  expect_identical(fc_err(a)$id, "config.usage")
  expect_length(a$log$infer, 0L); expect_length(a$log$train, 0L)
  expect_false(dir.exists(file.path(a$state, "forecasts")))
  expect_false(any(grepl("warning.option_alias", c(a$stdout, a$stderr), fixed = TRUE)))
  ## cada apelido sozinho tambem e recusado
  expect_identical(fc_forecast(c(paste0("--data=", DECK), "--format=json"))$code, 8L)
  expect_identical(fc_forecast(c(paste0("--data-train=", DECK), paste0("--artifacts-dir=", art), "--format=json"))$code, 8L)
  ## CONTROL: os nomes novos entregam o LONG (SIN.parquet), sem aviso
  b <- fc_forecast(c(paste0("--data-train=", DECK), paste0("--data-artifacts=", art)))
  expect_identical(b$code, 0L)
  expect_true(file.exists(long_of(b)))
  expect_false(any(grepl("aviso", b$stderr, fixed = TRUE)))
  expect_true(ascii(c(a$stdout, a$stderr, b$stderr)))
})

## --- [E1-AC5] set-data-dir interativo pela fachada -----------------------------------------------------------------------
test_that("[E1-AC5] set-data-dir interativo: 's' grava as tres chaves e cria os diretorios; 'N' deixa o arquivo igual; sem terminal exit 2", {
  inst <- normalizePath(tempfile("R"), winslash = "/", mustWork = FALSE)
  code <- file.path(inst, "versions", "9.9.9", "code")
  dir.create(file.path(code, "config"), recursive = TRUE)
  writeLines("9.9.9", file.path(code, "config", "VERSION"))
  cfg <- file.path(inst, "config.yaml")
  writeLines(c("# meu comentario", "schema_version: 1", "state_dir: '/s'", "data_train_dir: '/velho'"), cfg)
  base <- normalizePath(tempfile("d"), winslash = "/", mustWork = FALSE)
  dir.create(file.path(base, "train"), recursive = TRUE)
  run <- function(lines, is_tty = TRUE) {
    h <- function(ctx, args) {
      inp <- textConnection(lines); on.exit(close(inp))
      sink <- textConnection("sdd_out", "w", local = TRUE); on.exit(close(sink), add = TRUE)
      res <- E$prevcarga_set_data_dir_interactive(ctx, base = base, input = inp, is_tty = is_tty, output = sink)
      list(gravado = isTRUE(res$gravado), chaves = length(res$changes))
    }
    fc_run(c("set-data-dir", "--format=json"), handlers = list(`set-data-dir` = h), root = code)
  }
  antes <- unname(tools::md5sum(cfg))
  ## N: nada muda
  n <- run(c("", "", "", "N"))
  expect_identical(n$code, 0L)
  expect_identical(unname(tools::md5sum(cfg)), antes)
  expect_false(dir.exists(file.path(base, "artifacts")))
  ## sem terminal: exit 2, entrada nao lida, arquivo igual
  t <- run(c("", "", "", "s"), is_tty = FALSE)
  expect_identical(t$code, 2L)
  expect_identical(fc_err(t)$id, "contract.set_data_dir_noninteractive")
  expect_identical(unname(tools::md5sum(cfg)), antes)
  ## CONTROL: 's' grava as tres chaves, mantem o comentario e cria os diretorios
  g <- run(c("", "", "", "s"))
  expect_identical(g$code, 0L)
  y <- readLines(cfg)
  expect_true(all(c("# meu comentario", "state_dir: '/s'") %in% y))
  expect_false(any(startsWith(y, "data_dir:")))
  kv <- E$prevcarga_config_load(cfg)
  expect_identical(kv$data_train_dir, file.path(base, "train"))
  expect_identical(kv$artifacts_dir, file.path(base, "artifacts"))
  expect_identical(kv$data_inference_dir, file.path(base, "inference"))
  expect_true(dir.exists(file.path(base, "artifacts")) && dir.exists(file.path(base, "inference")))
  expect_true(ascii(c(n$stdout, t$stdout, g$stdout)))
})

## --- [E1-AC6] doctor sem tzdata -------------------------------------------------------------------------------------------
test_that("[E1-AC6] doctor com olson vazio: FAIL runtime.tzdata_missing em 'fuso America/Sao_Paulo', exit 7, mesmo numero de linhas", {
  D <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli", "topology", "provenance", "deck_reader", "dates", "introspect"))
  f <- list(tz = NA_character_, lc = "en_US.UTF-8", omp = NA_character_, openblas = NA_character_, mkl = NA_character_,
            veclib = NA_character_, blas_threads = 16L)
  e <- list(tz = "UTC", lc = "C.UTF-8", omp = "1", openblas = "1", mkl = "1", veclib = "1", blas_threads = 1L)
  check <- function() list(ok = TRUE, found = f, effective = e, offenders = character())
  ctx <- D$prevcarga_ctx(list(), pc_root(), NULL)
  ok <- D$prevcarga_build_doctor(ctx, check = check, has_pkg = function(p) TRUE)
  ko <- D$prevcarga_build_doctor(ctx, check = check, has_pkg = function(p) TRUE, olson = function() character(0))
  r <- ko$checks[ko$checks$check == "fuso America/Sao_Paulo", ]
  expect_identical(r$status, "FAIL")
  expect_identical(r$found, "runtime.tzdata_missing")
  expect_identical(attr(ko, "exit_code"), 7L)
  expect_identical(nrow(ko$checks), nrow(ok$checks))
  ## CONTROL: com tzdata a mesma linha passa e o exit nao e 7
  expect_identical(ok$checks$status[ok$checks$check == "fuso America/Sao_Paulo"], "PASS")
  expect_false(identical(attr(ok, "exit_code"), 7L))
  expect_true(ascii(unlist(ko$checks)))
})
