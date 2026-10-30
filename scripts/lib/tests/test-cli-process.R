## test-cli-process.R -- gates de PROCESSO do M1 (M1.V): `Rscript --vanilla <copia>/scripts/prevcarga ...` real,
## stdout/stderr separados, sobre instalacao-copia em tempdir(). Verbos-fixture entram por arquivos
## scripts/lib/cli/fixture-<x>.R descobertos pela fachada (nenhuma tabela editada). Ambiente sujo e aplicado
## ao processo de teste por pc_with_env e herdado pelo filho (portavel Linux/Windows).

exit_codes <- pc_source_lib("errors")$prevcarga_exit_codes
classes <- setdiff(names(exit_codes), "ok")
fx_names <- c("probe", classes, "multi", "boom", "accent")
root <- pc_install_copy(fixtures = fx_names)
parse_json <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
clean_env <- c(TZ = NA, LC_ALL = NA, OMP_NUM_THREADS = NA, OPENBLAS_NUM_THREADS = NA)

## ---- bloco 1: runtime no processo (ac-9, ac-10) ----------------------------------------------------------
test_that("[FORBIDDEN] fixture-probe: shell sujo == limpo, sem stderr (delta=0)", {
  dirty <- pc_dirty_env()
  skip_if(is.na(dirty[["LC_ALL"]]), "host sem nenhum locale UTF-8 alternativo para sujar LC_ALL")
  suj <- pc_cli(root, "fixture-probe", env = dirty)
  lim <- pc_cli(root, "fixture-probe", env = clean_env)
  expect_identical(suj$status, 0L); expect_identical(lim$status, 0L)
  expect_length(suj$stderr, 0L); expect_length(lim$stderr, 0L)
  expect_identical(suj$stdout, lim$stdout)
  expect_identical(suj$stdout[[1L]], "0")
})

test_that("[CONTROL] a mesma sonda SEM a CLI no shell sujo diverge (hora 21)", {
  dirty <- pc_dirty_env()
  probe <- tempfile("probe-", fileext = ".R")
  writeLines(c("writeLines(paste(lubridate::hour(.POSIXct(0))))",
               "writeLines(paste(sort(c('b','A','a','B','_1')), collapse = ' '))"), probe)
  ctl <- pc_with_env(as.list(dirty), pc_rscript(probe))
  cli <- pc_cli(root, "fixture-probe", env = dirty)
  expect_identical(ctl$stdout[[1L]], "21")
  expect_false(identical(ctl$stdout[[1L]], cli$stdout[[1L]]))
})

test_that("doctor --format=json no shell sujo mostra found sujo e effective UTC/1", {
  r <- pc_cli(root, c("doctor", "--format=json"), env = pc_dirty_env())
  expect_identical(r$status, 0L); expect_length(r$stderr, 0L)
  ck <- parse_json(r)$result$checks
  row <- function(nm) Filter(function(x) identical(x$check, nm), ck)[[1L]]
  expect_identical(row("TZ")$found, "America/Sao_Paulo"); expect_identical(row("TZ")$effective, "UTC")
  expect_identical(row("OMP_NUM_THREADS")$found, "4"); expect_identical(row("OMP_NUM_THREADS")$effective, "1")
  expect_identical(row("OPENBLAS_NUM_THREADS")$effective, "1")
})

test_that("setter de TZ quebrado: models exit 7 e stdout vazio; doctor imprime tudo e sai 7; status sai 0", {
  bad <- pc_install_copy(broken_tz = TRUE)
  env <- c(TZ = "America/Sao_Paulo")
  ok_doc <- pc_cli(root, "doctor", env = env)
  expect_identical(ok_doc$status, 0L)
  m <- pc_cli(bad, "models", env = env)
  expect_identical(m$status, 7L)
  expect_length(m$stdout, 0L)
  expect_true("prevcarga: erro [runtime] (exit 7)" %in% m$stderr)
  d <- pc_cli(bad, "doctor", env = env)
  expect_identical(d$status, 7L)
  expect_length(d$stderr, 0L)
  expect_true(any(grepl("^\\[FAIL\\] TZ ", d$stdout)))
  nchk <- function(x) sum(grepl("^\\[(PASS|WARN|FAIL|N/A) *\\]", x))
  expect_identical(nchk(d$stdout), nchk(ok_doc$stdout))                    # todas as linhas, nunca abortou no meio
  expect_true(any(grepl("^resumo: ", d$stdout)))
  s <- pc_cli(bad, "status", env = env)
  expect_identical(s$status, 0L)
  expect_true(any(grepl("TZ", s$stdout)))
  expect_identical(pc_cli(root, "models", env = env)$status, 0L)           # CONTROL: copia intacta, mesmo TZ sujo, sai 0
})

## ---- bloco 2: config, flags, gramatica (ac-11) ------------------------------------------------------------
cfg_file <- function(lines) { f <- tempfile("cfg-", fileext = ".yaml"); writeLines(c("schema_version: 1", lines), f); f }

test_that("config.yaml invalido e flags invalidas saem exit 8 com o error.id esperado", {
  cases <- list(
    list(c(paste0("--config=", cfg_file("dados_dir: x"))), "config.unknown_key"),
    list(c(paste0("--config=", cfg_file("color: 3"))), "config.bad_type"),
    list(c(paste0("--config=", cfg_file("weather_issue_cutoff: meia_noite"))), "config.bad_weather_cutoff"),
    list("--origin=2024-13-01", "config.bad_origin"),
    list("--load-label=meio", "config.bad_load_label"),
    list("--weather-cutoff=agora", "config.bad_weather_cutoff"),
    list("--color=rosa", "config.bad_color"))
  for (cs in cases) for (fmt in c("text", "json")) {
    r <- pc_cli(root, c("models", cs[[1L]], paste0("--format=", fmt)))
    expect_identical(r$status, 8L, info = paste(cs[[2L]], fmt))
    if (fmt == "json") expect_identical(parse_json(r)$error$id, cs[[2L]])
    else expect_true("prevcarga: erro [config] (exit 8)" %in% r$stderr)
    expect_false(any(grepl("deprecated|deprecado", c(r$stdout, r$stderr), ignore.case = TRUE)))
  }
})

test_that("verbo ou flag fora da gramatica: exit 8 config.usage", {
  for (a in list("nao-existe", c("models", "--nao-existe"))) {
    j <- pc_cli(root, c(a, "--format=json"))
    expect_identical(j$status, 8L); expect_identical(parse_json(j)$error$id, "config.usage")
    expect_length(j$stderr, 0L)
    t <- pc_cli(root, a)
    expect_identical(t$status, 8L); expect_length(t$stdout, 0L)
    expect_true("prevcarga: erro [config] (exit 8)" %in% t$stderr)
  }
})

test_that("stubs de outro dono (derivados da descoberta): exit 1 todo.not_implemented nomeando o dono", {
  E <- pc_source_lib("errors")
  sys.source(file.path(root, "scripts", "lib", "cli.R"), envir = E)
  planned <- E$prevcarga_verbs_planned
  have <- sub("\\.R$", "", list.files(file.path(root, "scripts", "lib", "cli"), pattern = "\\.R$"))
  stubs <- setdiff(names(planned), have)
  expect_false("reconcile" %in% stubs)   # R.5: reconcile tem cli/reconcile.R
  expect_true("release-pack" %in% stubs)
  expect_true(length(stubs) >= 1L)
  for (v in stubs) {
    t <- pc_cli(root, v)
    expect_identical(t$status, 1L, info = v)
    expect_length(t$stdout, 0L)
    expect_true(any(grepl(paste0("^onde: ", v, " -> ", planned[[v]], "$"), t$stderr)), info = v)
    expect_false(any(grepl("Usage:|Error in|[Tt]raceback", c(t$stdout, t$stderr))), info = v)
    j <- pc_cli(root, c(v, "--format=json"))
    expect_identical(j$status, 1L)
    expect_identical(parse_json(j)$error$id, "todo.not_implemented")
    expect_true(grepl(planned[[v]], parse_json(j)$error$where, fixed = TRUE))
  }
  expect_true(any(grepl("M8", pc_cli(root, "release-pack")$stderr)))
})

## ---- bloco 3: matriz de classes 0-15, precedencia, --debug, catalogo (ac-12) -------------------------------
check_class <- function(args, exp_status, exp_class, root = get("root", envir = parent.frame())) {
  tx <- pc_cli(root, c(args, "--format=text"))
  js <- pc_cli(root, c(args, "--format=json"))
  expect_identical(tx$status, as.integer(exp_status), info = paste(args, collapse = " "))
  expect_identical(js$status, as.integer(exp_status), info = paste(args, collapse = " "))
  env <- parse_json(js)
  expect_length(js$stdout, 1L)
  expect_setequal(names(env), c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version"))
  expect_length(js$stderr, 0L)
  expect_identical(env$exit_code, as.integer(exp_status))
  if (identical(exp_class, "ok")) {
    expect_true(env$ok); expect_null(env$error)
  } else {
    expect_false(env$ok)
    expect_identical(env$error$class, exp_class)
    for (p in c("^o que: ", "^onde: ", "^como corrigir: ")) expect_true(any(grepl(p, tx$stderr)), info = p)
    expect_true(grepl(paste0("^prevcarga: erro \\[", exp_class, "\\] \\(exit ", exp_status, "\\)$"), tx$stderr[[1L]]))
  }
  invisible(list(text = tx, json = js, env = env))
}

for (cls in classes) test_that(paste("[CLASSE]", cls, "(verbo-fixture) texto e json"), {
  check_class(paste0("fixture-", cls), exit_codes[[cls]], cls)
})

test_that("[CLASSE] 0, 1, 2, 7, 8 pela CLI real", {
  check_class("models", 0L, "ok")
  check_class("fixture-generic", 1L, "generic")                       # stop() nao classificado -> generic.unexpected
  expect_identical(parse_json(pc_cli(root, c("fixture-generic", "--format=json")))$error$id, "generic.unexpected")
  dup <- pc_install_copy(mutate = function(r) file.copy(file.path(pc_root(), "scripts/lib/tests/fixtures/topology_dup_area.yaml"),
                                                       file.path(r, "config", "topology.yaml"), overwrite = TRUE))
  check_class("areas", 2L, "contract", root = dup)
  bad <- pc_install_copy(broken_tz = TRUE)
  check_class_env <- pc_with_env(list(TZ = "America/Sao_Paulo"), {
    list(tx = pc_cli(bad, c("models", "--format=text")), js = pc_cli(bad, c("models", "--format=json")))
  })
  expect_identical(check_class_env$tx$status, 7L); expect_identical(check_class_env$js$status, 7L)
  expect_length(check_class_env$tx$stdout, 0L)
  expect_identical(parse_json(check_class_env$js)$error$class, "runtime")
  check_class("models --color=rosa", 8L, "config")
})

test_that("precedencia 7, 8, 2, 3, 5, 9 independente da ordem de emissao (6 conjuntos, normal e invertida)", {
  expected <- c(`1` = "runtime", `2` = "config", `3` = "contract", `4` = "completude", `5` = "window", `6` = "integrity")
  for (n in names(expected)) for (suffix in c("", "r")) {
    res <- check_class(c("fixture-multi", paste0("--model=", n, suffix)), exit_codes[[expected[[n]]]], expected[[n]])
    expect_gt(length(res$env$warnings), 0L)                           # os demais erros viajam como avisos
  }
  expect_identical(unname(exit_codes[expected]), c(7L, 8L, 2L, 3L, 5L, 9L))
})

test_that("traceback so com --debug; fixture-boom e generic.unexpected em text e json", {
  nd <- pc_cli(root, "fixture-boom")
  expect_identical(nd$status, 1L)
  expect_false(any(grepl("fx_inner_boom", nd$stderr)))
  expect_true("prevcarga: erro [generic] (exit 1)" %in% nd$stderr)
  dbg <- pc_cli(root, c("fixture-boom", "--debug"))
  expect_identical(dbg$status, 1L)
  expect_true(any(grepl("fx_inner_boom", dbg$stderr)))
  expect_true("prevcarga: erro [generic] (exit 1)" %in% dbg$stderr)
  jn <- pc_cli(root, c("fixture-boom", "--format=json"))
  expect_identical(jn$status, 1L); expect_length(jn$stderr, 0L)
  expect_identical(parse_json(jn)$error$class, "generic")
  expect_length(pc_cli(root, c("fixture-boom", "--format=json", "--debug"))$stdout, 1L)
})

test_that("catalogo gerado por export_error_catalog.R == prevcarga_error_catalog() (uniao dos errors/*.tsv)", {
  out <- tempfile("catalog-", fileext = ".tsv")
  st <- suppressWarnings(system2(pc_rscript_bin(), c("--vanilla",
          shQuote(file.path(root, "scripts", "tools", "export_error_catalog.R")), "--out", shQuote(out)),
          stdout = TRUE, stderr = TRUE))
  expect_true(file.exists(out), info = paste(st, collapse = "\n"))
  gen <- utils::read.delim(out, quote = "", comment.char = "", colClasses = "character")
  tsvs <- Sys.glob(file.path(root, "scripts", "lib", "errors", "*.tsv"))
  uni <- do.call(rbind, lapply(tsvs, utils::read.delim, quote = "", comment.char = "", colClasses = "character"))
  uni <- uni[order(uni$id, method = "radix"), ]
  E <- pc_source_lib("errors")
  cat <- E$prevcarga_errors_load(dir = file.path(root, "scripts", "lib", "errors"))
  expect_identical(names(gen), c("id", "class", "exit_code", "message", "hint"))
  expect_identical(gen$id, uni$id); expect_identical(gen$id, cat$id)
  expect_identical(gen$message, uni$message); expect_identical(gen$hint, uni$hint)
  expect_identical(as.integer(gen$exit_code), unname(exit_codes[gen$class]))
  expect_false(anyDuplicated(gen$id) > 0L)
  ## CONTROL: um catalogo adulterado e detectavel pela mesma comparacao
  adulterado <- uni; adulterado$message[[1L]] <- paste(adulterado$message[[1L]], "x")
  expect_false(identical(gen$message, adulterado$message))
})

test_that("install/error-catalog.tsv nao e rastreado no git (gerado, nunca versionado)", {
  skip_if(!nzchar(Sys.which("git")), "git indisponivel no host")
  tracked <- suppressWarnings(system2("git", c("-C", shQuote(pc_root()), "ls-files", "install/error-catalog.tsv"),
                                      stdout = TRUE, stderr = FALSE))
  expect_length(tracked, 0L)
})

## ---- bloco 4: instalacao-copia quebrada => exit 2 (ac-14) -----------------------------------------------------
test_that("topology com area duplicada e run.yaml ausente: exit 2 contract; copia intacta sai 0", {
  dup <- pc_install_copy(mutate = function(r) file.copy(file.path(pc_root(), "scripts/lib/tests/fixtures/topology_dup_area.yaml"),
                                                       file.path(r, "config", "topology.yaml"), overwrite = TRUE))
  norun <- pc_install_copy(mutate = function(r) unlink(file.path(r, "models", "holt-winters", "run.yaml")))
  for (case in list(list(dup, "areas"), list(norun, "models"))) for (fmt in c("text", "json")) {
    r <- pc_cli(case[[1L]], c(case[[2L]], paste0("--format=", fmt)))
    expect_identical(r$status, 2L, info = paste(case[[2L]], fmt))
    if (fmt == "text") { expect_length(r$stdout, 0L); expect_true("prevcarga: erro [contract] (exit 2)" %in% r$stderr) }
    else expect_identical(parse_json(r)$error$class, "contract")
  }
  for (v in c("areas", "models")) for (fmt in c("text", "json"))
    expect_identical(pc_cli(root, c(v, paste0("--format=", fmt)))$status, 0L)
})
