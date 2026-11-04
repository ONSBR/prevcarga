## test-facade.R -- fachada da CLI (M1.4): descoberta de verbos, usage gerado, dispatch runtime-first,
## stubs, envelope, --debug, banner. Tudo in-process; docopt e pre-requisito (sem skip).
## A lista de verbos e DERIVADA da descoberta; este arquivo nao e editado por outros epicos.

lib_dir <- file.path(pc_root(), "scripts", "lib")
withr::local_options(prevcarga.lib = lib_dir)

new_env <- function() pc_source_lib(c("errors", "runtime", "config", "banner", "cli"))
noop <- function() invisible(NULL)

## verbos-fixture em diretorio temporario (arquivos cli/<verbo>.R reais, lidos pelo descobridor real)
write_verb <- function(dir, verb, exempt = FALSE, modules = character(), body = "NULL") {
  fn <- paste0("cmd_", gsub("-", "_", verb))
  writeLines(c(
    sprintf("%s <- function(ctx, args) %s", fn, body),
    sprintf("prevcarga_verb_%s <- list(", gsub("-", "_", verb)),
    sprintf("  name = '%s', usage = 'prevcarga %s [--x=<n>]', options = c('--x=<n>  Opcao do fixture'),", verb, verb),
    sprintf("  handler = %s, owner = 'fixture', exempt = %s, modules = %s)", fn, if (exempt) "TRUE" else "FALSE",
            if (length(modules)) sprintf("c(%s)", paste0("'", modules, "'", collapse = ",")) else "character()")
  ), file.path(dir, paste0(verb, ".R")))
}

fixture_dir <- function() {
  d <- tempfile("cli-fixture-"); dir.create(d)
  for (v in c("models", "fixture-ok", "fixture-fail", "fixture-boom")) write_verb(d, v)
  for (v in c("doctor", "status")) write_verb(d, v, exempt = TRUE)
  d
}

run_main <- function(E, argv, verbs, handlers = NULL, runtime = noop) {
  err <- character(); code <- NA_integer_
  out <- utils::capture.output({
    err <- utils::capture.output({
      code <- E$prevcarga_main(argv, verbs = verbs, root = pc_root(), runtime = runtime, handlers = handlers)
    }, type = "message")
  }, type = "output")
  list(code = code, out = out, err = err)
}


test_that("descoberta em ordem radix sobre diretorio-fixture, nome com hifen e arquivo quebrado (ac-15)", {
  E <- new_env()
  d <- tempfile("cli-disc-"); dir.create(d)
  writeLines(c("cmd_zeta <- function(ctx, args) 'zeta'",
               "prevcarga_verb_zeta <- list(name = 'zeta', usage = 'prevcarga zeta', options = character(),",
               "  handler = cmd_zeta, owner = 'T', exempt = FALSE, modules = character())"),
             file.path(d, "zeta.R"))
  writeLines(c("cmd_alfa_beta <- function(ctx, args) 'alfa'",
               "prevcarga_verb_alfa_beta <- list(name = 'alfa-beta', usage = 'prevcarga alfa-beta [--x=<n>]',",
               "  options = c('--x=<n>  um numero'), handler = cmd_alfa_beta, owner = 'T', exempt = FALSE,",
               "  modules = c('topology.R'))"),
             file.path(d, "alfa-beta.R"))
  tab <- E$prevcarga_verbs_discover(d, planned = character())
  expect_identical(tab$verb, c("alfa-beta", "zeta"))
  expect_true(all(c("verb", "owner", "exempt", "modules", "usage", "handler") %in% names(tab)))
  expect_identical(tab$handler[[1L]](NULL, NULL), "alfa")
  expect_identical(tab$modules[[1L]], "topology.R")
  expect_identical(tab$usage[[1L]], "prevcarga alfa-beta [--x=<n>]")
  usage <- E$prevcarga_usage(tab)
  expect_true(all(vapply(tab$verb, function(v) grepl(paste0("prevcarga ", v), usage, fixed = TRUE), NA)))

  writeLines("x <- 1", file.path(d, "quebrado.R"))
  expect_error(E$prevcarga_verbs_discover(d, planned = character()), "quebrado.R")

  d2 <- tempfile("cli-name-"); dir.create(d2)
  writeLines(c("cmd_a <- function(ctx, args) 1",
               "prevcarga_verb_a <- list(name = 'b', usage = 'prevcarga b', options = character(),",
               "  handler = cmd_a, owner = 'T', exempt = FALSE, modules = character())"), file.path(d2, "a.R"))
  expect_error(E$prevcarga_verbs_discover(d2, planned = character()), "a.R")
  ## erro interno na fachada vira generic.unexpected exit 1
  bad <- function() E$prevcarga_verbs_discover(d, planned = character())
  r <- run_main(E, "zeta", verbs = tab[0, ], handlers = NULL)  # tabela vazia: gramatica sem verbo
  expect_identical(r$code, 8L)
  out <- E$prevcarga_with_errors(bad())
  expect_identical(out$exit_code, 1L)
  expect_identical(out$cond$id, "generic.unexpected")

  ## diretorio real: carrega sem erro e todo verbo descoberto consta no usage
  real <- E$prevcarga_verbs_discover()
  usage_real <- E$prevcarga_usage(real)
  expect_true(all(vapply(real$verb, function(v) grepl(paste0("prevcarga ", v), usage_real, fixed = TRUE), NA)))
  expect_true(all(names(E$prevcarga_verbs_planned) %in% real$verb))
})

test_that("tabela descoberta x gramatica + CONTROL (ac-7)", {
  E <- new_env()
  tab <- E$prevcarga_verbs_discover(fixture_dir())
  ## stubs planejados entram na mesma tabela
  expect_true(all(names(E$prevcarga_verbs_planned) %in% tab$verb))
  usage <- E$prevcarga_usage(tab)
  parsed <- vapply(tab$verb, function(v) {
    r <- NULL
    utils::capture.output(r <- docopt::docopt(usage, args = v, help = FALSE, strip_names = FALSE))
    isTRUE(r[[v]])
  }, NA)
  expect_true(all(parsed))
  ## verbos do usage == verbos da tabela (gramatica gerada so da tabela)
  ulines <- grep("^  prevcarga [a-z]", strsplit(usage, "\n")[[1L]], value = TRUE)
  uverbs <- sub("^  prevcarga ([a-z-]+).*$", "\\1", ulines)
  expect_setequal(uverbs, tab$verb)
  ## CONTROL: sem train-models na tabela, o verbo sai exit 8 config.usage
  ctl <- tab[tab$verb != "train-models", , drop = FALSE]
  r <- run_main(E, "train-models", verbs = ctl, runtime = function() stop("nao deve rodar"))
  expect_identical(r$code, 8L)
  expect_length(r$out, 0L)
  expect_true(any(grepl("train-models", r$err, fixed = TRUE)))
  rj <- run_main(E, c("train-models", "--format=json"), verbs = ctl)
  expect_identical(jsonlite::fromJSON(paste(rj$out, collapse = "\n"))$error$id, "config.usage")
})

test_that("enforce antes do corpo; isentos nao chamam o runtime; exit 7 com stdout vazio (ac-8)", {
  E <- new_env()
  log <- character()
  add <- function(x) log <<- c(log, x)
  orig_ctx <- E$prevcarga_ctx
  E$prevcarga_ctx <- function(...) { add("ctx"); orig_ctx(...) }
  tab <- E$prevcarga_verbs_discover(fixture_dir())
  spy <- function() { add("enforce"); invisible(NULL) }
  h <- list(models = (function(ctx, args) { add("cmd"); list(schema_version = 1L) }),
            `fixture-ok` = (function(ctx, args) { add("cmd"); list(schema_version = 1L) }),
            doctor = (function(ctx, args) { add("cmd"); list(schema_version = 1L) }),
            status = (function(ctx, args) { add("cmd"); list(schema_version = 1L) }))
  for (v in c("models", "fixture-ok")) {
    log <- character()
    r <- run_main(E, v, tab, handlers = h, runtime = spy)
    expect_identical(r$code, 0L)
    expect_identical(log, c("enforce", "ctx", "cmd"))
  }
  for (a in list("doctor", "status", "--help", "--version", character())) {
    log <- character()
    r <- run_main(E, a, tab, handlers = h, runtime = spy)
    expect_identical(r$code, 0L)
    expect_false("enforce" %in% log)
  }
  ## runtime falho: exit 7, stdout vazio, handler nunca chamado
  log <- character()
  boom <- (function() E$prevcarga_abort("runtime.tz_unfixable", where = "TZ"))
  r <- run_main(E, "models", tab, handlers = h, runtime = boom)
  expect_identical(r$code, 7L)
  expect_length(r$out, 0L)
  expect_true(any(grepl("prevcarga: erro [runtime] (exit 7)", r$err, fixed = TRUE)))
  expect_identical(log, character())
  ## isento com config.yaml quebrado: nao aborta, ctx$config_error preenchido; exit_code do resultado e respeitado
  bad <- tempfile(fileext = ".yaml"); writeLines("schema_version: 1\nchave_ruim: 2", bad)
  seen <- NULL
  h2 <- list(doctor = (function(ctx, args) {
    seen <<- ctx$config_error
    structure(list(schema_version = 1L), exit_code = 6L)
  }))
  r <- run_main(E, c("doctor", paste0("--config=", bad)), tab, handlers = h2, runtime = spy)
  expect_identical(r$code, 6L)
  expect_s3_class(seen, "prevcarga_error")
  ## guardado com o mesmo config quebrado aborta exit 8
  r <- run_main(E, c("models", paste0("--config=", bad)), tab, handlers = h, runtime = noop)
  expect_identical(r$code, 8L)
})

test_that("stubs derivados da descoberta, com dono (ac-9)", {
  E <- new_env()
  tab <- E$prevcarga_verbs_discover()
  stubs <- tab$verb[vapply(tab$handler, is.null, NA)]
  expect_setequal(stubs, setdiff(names(E$prevcarga_verbs_planned), sub("\\.R$", "", list.files(E$prevcarga_cli_dir()))))
  ## M8.5: release-pack tem arquivo em cli/ e deixou de ser stub; com todos os verbos planejados implementados nao sobra stub (o laco cobre o que sobrar)
  expect_false("release-pack" %in% stubs)
  expect_false(is.null(tab$handler[[which(tab$verb == "release-pack")]]))
  ## M2.5: validate-deck tem arquivo em cli/ e deixou de ser stub (a lista de stubs vem so da descoberta)
  expect_false("validate-deck" %in% stubs)
  expect_true("validate-deck" %in% tab$verb)
  expect_false(is.null(tab$handler[[which(tab$verb == "validate-deck")]]))
  expect_true(file.exists(file.path(E$prevcarga_cli_dir(), "validate-deck.R")))
  ## R.5: reconcile tem arquivo em cli/ e deixou de ser stub
  expect_false("reconcile" %in% stubs)
  expect_false(is.null(tab$handler[[which(tab$verb == "reconcile")]]))
  for (v in stubs) {
    owner <- unname(E$prevcarga_verbs_planned[[v]])
    r <- run_main(E, v, tab)
    expect_identical(r$code, 1L)
    expect_length(r$out, 0L)
    expect_true(any(grepl(paste0(v, " -> ", owner), r$err, fixed = TRUE)), info = v)
    expect_false(any(grepl("Usage:|Traceback|deprecat", c(r$out, r$err))), info = v)
    rj <- run_main(E, c(v, "--format=json"), tab)
    j <- jsonlite::fromJSON(paste(rj$out, collapse = "\n"))
    expect_identical(rj$code, 1L)
    expect_identical(j$error$id, "todo.not_implemented")
    expect_identical(j$error$where, paste0(v, " -> ", owner))
  }
  ## quando o epico cria o arquivo, o verbo sai do conjunto de stubs
  d <- tempfile("cli-own-"); dir.create(d); write_verb(d, "score")
  t2 <- E$prevcarga_verbs_discover(d)
  expect_false(is.null(t2$handler[[which(t2$verb == "score")]]))
  expect_true(is.null(t2$handler[[which(t2$verb == "combine")]]))
})

test_that("verbo ou flag fora da gramatica sai exit 8 config.usage (ac-10)", {
  E <- new_env()
  tab <- E$prevcarga_verbs_discover(fixture_dir())
  h <- list(models = (function(ctx, args) list(schema_version = 1L)))
  cases <- list(list(c("nao-existe"), "nao-existe"), list(c("models", "--nao-existe"), "--nao-existe"),
                list(c("models", "extra"), "extra"))
  for (cs in cases) {
    r <- run_main(E, cs[[1L]], tab, handlers = h)
    expect_identical(r$code, 8L, info = cs[[2L]])
    expect_length(r$out, 0L)
    expect_true(any(grepl(paste0("onde: ", cs[[2L]]), r$err, fixed = TRUE)), info = cs[[2L]])
    expect_false(any(grepl("deprecated|deprecado", c(r$out, r$err))))
    rj <- run_main(E, c(cs[[1L]], "--format=json"), tab, handlers = h)
    j <- jsonlite::fromJSON(paste(rj$out, collapse = "\n"))
    expect_identical(j$error$id, "config.usage")
    expect_identical(j$error$where, cs[[2L]])
  }
  expect_identical(run_main(E, "models", tab, handlers = h)$code, 0L)
})

test_that("flags globais invalidas saem exit 8 com o id certo (ac-11)", {
  E <- new_env()
  tab <- E$prevcarga_verbs_discover(fixture_dir())
  h <- list(models = (function(ctx, args) list(schema_version = 1L)))
  cases <- c("--origin=2024-13-01" = "config.bad_origin", "--load-label=meio" = "config.bad_load_label",
             "--weather-cutoff=agora" = "config.bad_weather_cutoff", "--color=rosa" = "config.bad_color")
  for (f in names(cases)) {
    r <- run_main(E, c("models", f), tab, handlers = h)
    expect_identical(r$code, 8L, info = f)
    expect_length(r$out, 0L)
    expect_true(any(grepl("^o que:", r$err)) && any(grepl("^onde:", r$err)) && any(grepl("^como corrigir:", r$err)))
    rj <- run_main(E, c("models", f, "--format=json"), tab, handlers = h)
    expect_identical(jsonlite::fromJSON(paste(rj$out, collapse = "\n"))$error$id, cases[[f]])
  }
})

test_that("envelope JSON unico, stderr vazio (ac-12)", {
  E <- new_env()
  tab <- E$prevcarga_verbs_discover(fixture_dir())
  h <- list(`fixture-ok` = (function(ctx, args) list(schema_version = 1L, x = 1)),
            `fixture-fail` = (function(ctx, args) E$prevcarga_abort("contract.khat_range_invalid", where = "k")))
  keys <- c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version")
  r <- run_main(E, c("fixture-ok", "--format=json"), tab, handlers = h)
  expect_length(r$err, 0L)
  j <- jsonlite::fromJSON(paste(r$out, collapse = "\n"))
  expect_true(all(keys %in% names(j)))
  expect_true(j$ok); expect_identical(j$exit_code, 0L); expect_identical(j$result$schema_version, 1L)
  expect_identical(r$code, 0L)
  r <- run_main(E, c("fixture-fail", "--format=json"), tab, handlers = h)
  expect_length(r$err, 0L)
  j <- jsonlite::fromJSON(paste(r$out, collapse = "\n"))
  expect_true(all(keys %in% names(j)))
  expect_false(j$ok); expect_identical(j$exit_code, 2L); expect_identical(j$error$class, "contract")
  expect_identical(r$code, 2L)
  ## aviso classificado do handler entra no envelope
  h3 <- list(`fixture-ok` = (function(ctx, args) { E$prevcarga_warn("config.unknown_key", where = "k"); list(schema_version = 1L) }))
  r <- run_main(E, c("fixture-ok", "--format=json"), tab, handlers = h3)
  j <- jsonlite::fromJSON(paste(r$out, collapse = "\n"), simplifyVector = FALSE)
  expect_identical(j$warnings[[1L]]$id, "config.unknown_key")
})

test_that("erro inesperado: sem traceback por padrao, com --debug em stderr (ac-13)", {
  E <- new_env()
  tab <- E$prevcarga_verbs_discover(fixture_dir())
  h <- list(`fixture-boom` = (function(ctx, args) stop("boom")))
  r <- run_main(E, "fixture-boom", tab, handlers = h)
  expect_identical(r$code, 1L)
  expect_length(r$out, 0L)
  expect_true(any(grepl("(exit 1)", r$err, fixed = TRUE)))
  expect_false(any(grepl("stop(\"boom\")", r$err, fixed = TRUE)))
  rj <- run_main(E, c("fixture-boom", "--format=json"), tab, handlers = h)
  expect_identical(jsonlite::fromJSON(paste(rj$out, collapse = "\n"))$error$id, "generic.unexpected")
  r <- run_main(E, c("fixture-boom", "--debug"), tab, handlers = h)
  expect_identical(r$code, 1L)
  expect_true(any(grepl("stop(\"boom\")", r$err, fixed = TRUE)))
})

test_that("banner so nos pontos permitidos, so com stdout em terminal, e ESC so com --color=always (ac-14)", {
  E <- new_env()
  E$.prevcarga_stdout_tty <- function() TRUE                      # simula stdout em terminal (capture.output nao e TTY)
  tab <- E$prevcarga_verbs_discover(fixture_dir())
  h <- list(models = (function(ctx, args) list(schema_version = 1L)))
  spy <- function() stop("runtime nao deve ser chamado")
  esc <- intToUtf8(27L)
  ident <- "Previsao de carga do SIN - ONS"
  for (a in list("--help", character(), "--version")) {
    never <- run_main(E, c(a, "--color=never"), tab, handlers = h, runtime = spy)
    expect_identical(never$code, 0L)
    expect_true(any(grepl(ident, never$out, fixed = TRUE)))
    expect_identical(never$out[1:15], E$prevcarga_banner(list(color = "never", version = E$prevcarga_ctx(list(), pc_root())$version,
                                                              root = pc_root())))
    expect_false(any(grepl(esc, never$out, fixed = TRUE)))
    if (identical(a, "--version")) expect_true(any(grepl("^prevcarga ", never$out)))
    else expect_true(any(grepl("Usage:", never$out, fixed = TRUE)))
    always <- run_main(E, c(a, "--color=always"), tab, handlers = h, runtime = spy)
    expect_true(any(grepl(esc, always$out, fixed = TRUE)))
    json <- run_main(E, c(a, "--format=json"), tab, handlers = h, runtime = spy)
    expect_false(any(grepl(ident, json$out, fixed = TRUE)))
  }
  for (c in c("never", "always")) {
    r <- run_main(E, c("models", paste0("--color=", c)), tab, handlers = h)
    expect_identical(r$code, 0L)
    expect_false(any(grepl(ident, r$out, fixed = TRUE)))
    expect_false(any(grepl(esc, r$out, fixed = TRUE)))
  }
  ## CONTROL: stdout redirecionado (nao-TTY) => sem banner nem com --color=always; o uso/versao continua
  E$.prevcarga_stdout_tty <- function() FALSE
  for (a in list("--help", character(), "--version")) {
    r <- run_main(E, c(a, "--color=always"), tab, handlers = h, runtime = spy)
    expect_identical(r$code, 0L)
    expect_false(any(grepl(ident, r$out, fixed = TRUE)))
    expect_false(any(grepl(esc, r$out, fixed = TRUE)))
    if (identical(a, "--version")) expect_true(any(grepl("^prevcarga ", r$out))) else expect_true(any(grepl("Usage:", r$out, fixed = TRUE)))
  }
})

test_that("shim scripts/prevcarga e fino e carregavel sem executar", {
  shim <- file.path(pc_root(), "scripts", "prevcarga")
  txt <- readLines(shim, warn = FALSE)
  expect_true(any(grepl("prevcarga_main(commandArgs(TRUE)", txt, fixed = TRUE)))
  expect_true(any(grepl("identical(environment(), globalenv())", txt, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  expect_silent(sys.source(shim, envir = env))
  expect_length(ls(env, all.names = TRUE), 0L)
  ## ponta a ponta: processo real, sem verbo => banner + usage, exit 0
  r <- pc_run_cli("--help")
  expect_identical(r$status, 0L)
  expect_true(any(grepl("Usage:", r$stdout, fixed = TRUE)))
})
