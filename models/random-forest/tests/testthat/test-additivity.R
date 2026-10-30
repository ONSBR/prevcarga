## test-additivity.R -- fechamento do plugin random-forest (RF.5): aditividade, higiene, README e CONTROL do runner.
## Gates [PLUGINS] aditividade, higiene, readme e runner_controle. O grep usa pc_grep_code / pc_grep_time_arith / pc_bytes_over_127 da
## fundacao (P.0j, scripts/lib/tests/helper-plugins.R), sem grep local. Sem variaveis de ambiente; clones e fixtures em tempdir.
suppressWarnings(suppressMessages(library(testthat)))

if (!exists("pc_grep_code", envir = rf_lib, inherits = FALSE))
  sys.source(file.path(rf_root(), "scripts", "lib", "tests", "helper-plugins.R"), envir = rf_lib)

RFA_ORACLE <- "/opt/ons-source/prevcargadessem3"
RFA_MODEL_DIR <- function() file.path(rf_root(), "models", "random-forest")
rfa_tier <- function() if (exists(".pc_gates_state", envir = globalenv())) get(".pc_gates_state", envir = globalenv())$tier else "fast"
rfa_parity_tier <- function() rfa_tier() %in% c("parity", "parity-full")

rfa_git <- function(args, dir = rf_root()) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(dir), args), stdout = TRUE, stderr = FALSE))
  st <- attr(out, "status")
  list(out = out, status = if (is.null(st)) 0L else as.integer(st))
}

## ---- aditividade: o epico so escreve em models/random-forest/ -----------------------------------------------------------
## Caminhos sob models/ tocados pelos commits do epico (assunto "RF.n:", sem merges) e pelo working tree, fora de
## models/random-forest/. Nao usa o merge-base com origin/main: a branch de integracao ja traz os outros plugins.
rfa_foreign_changes <- function(repo) {
  commits <- rfa_git(c("log", "--no-merges", "--format=%H", "--grep=^RF[.][0-9]"), repo)$out
  files <- unlist(lapply(commits, function(cm)
    rfa_git(c("diff-tree", "--no-commit-id", "--name-only", "-r", cm), repo)$out))
  wt <- rfa_git(c("status", "--porcelain", "--untracked-files=all", "--", "models"), repo)$out
  files <- c(files, sub("^.{3}", "", wt))
  files <- unique(files[startsWith(files, "models/")])
  sort(files[!startsWith(files, "models/random-forest/")], method = "radix")
}

## clone temporario com um arquivo plantado fora de models/random-forest/ em um commit "RF.9: ..."
rfa_clone_with_plant <- function(path, commit = TRUE) {
  dir <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "clone")
  system2("git", c("clone", "-q", shQuote(rf_root()), shQuote(dir)), stdout = FALSE, stderr = FALSE)
  full <- file.path(dir, path)
  dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
  writeLines("# plantado", full)
  if (commit) {
    rfa_git(c("add", "--", shQuote(path)), dir)
    system2("git", c("-C", shQuote(dir), "-c", "user.name=ctl", "-c", "user.email=ctl@example.invalid", "commit", "-q", "-m",
                     shQuote("RF.9: arquivo plantado (CONTROL)")), stdout = FALSE, stderr = FALSE)
  }
  dir
}

## `invocation.json` do golden: nenhum caminho de saida (--out) dentro do oraculo e todo caminho de escrita sob o scratch (cwd)
rfa_invocation_problems <- function(inv, oracle = RFA_ORACLE) {
  unq <- function(x) gsub("^'|'$", "", x)
  a <- unq(unlist(inv$args))
  val <- function(flag) { i <- match(flag, a); if (is.na(i) || i == length(a)) NA_character_ else a[[i + 1L]] }
  out <- val("--out"); unit <- val("--unit"); cwd <- inv$cwd
  bad <- character()
  if (is.na(out)) return("sem --out")
  under <- function(p, root) !is.na(p) && !is.na(root) && startsWith(p, paste0(sub("/+$", "", root), "/"))
  if (under(out, oracle) || identical(out, oracle)) bad <- c(bad, paste("--out dentro do oraculo:", out))
  if (!under(out, cwd)) bad <- c(bad, paste("--out fora do scratch:", out))
  if (!is.na(unit) && !under(unit, cwd)) bad <- c(bad, paste("--unit fora do scratch:", unit))
  bad
}

test_that("[PLUGINS] aditividade: so models/random-forest/ mudou; oraculo limpo; saidas do golden sob o --out; CONTROL com arquivo plantado", {
  skip_if(!nzchar(Sys.which("git")) || rfa_git(c("rev-parse", "--git-dir"))$status != 0L, "[tier] git indisponivel")
  expect_identical(rfa_foreign_changes(rf_root()), character())
  ## CONTROL: arquivo plantado em models/holt-winters/ (commit RF.9 e arquivo solto) reprova nomeando-o
  c1 <- rfa_clone_with_plant("models/holt-winters/plantado.R")
  expect_identical(rfa_foreign_changes(c1), "models/holt-winters/plantado.R")
  c2 <- rfa_clone_with_plant("models/holt-winters/solto.R", commit = FALSE)
  expect_identical(rfa_foreign_changes(c2), "models/holt-winters/solto.R")
  ## um arquivo plantado DENTRO de models/random-forest/ nao e estranho ao epico
  c3 <- rfa_clone_with_plant("models/random-forest/novo.R")
  expect_identical(rfa_foreign_changes(c3), character())

  ## golden e artefatos nunca versionados
  expect_identical(rfa_git(c("check-ignore", "-q", ".state/golden/x"))$status, 0L)
  expect_length(rfa_git(c("ls-files", ".state", "models/random-forest/models"))$out, 0L)

  ## oraculo somente leitura: so as 3 notas + verify-log/ de docs/roadmap/ (ja existiam la, nao sao deste epico) sao toleraveis
  if (!dir.exists(file.path(RFA_ORACLE, ".git"))) {
    if (rfa_parity_tier()) fail("oraculo ausente nesta maquina no tier parity")
    skip("[tier] oraculo ausente nesta maquina")
  }
  st <- rfa_git(c("status", "--porcelain"), RFA_ORACLE)$out
  tolerado <- paste("??", c("docs/roadmap/CLI-MODULAR-OBJETIVO.md", "docs/roadmap/CLI-MODULAR-ONDE-ESTAMOS.md",
                            "docs/roadmap/MIGRACAO-COMBINA-R.md", "docs/roadmap/verify-log/"))
  expect_length(setdiff(st, tolerado), 0L)

  ## todo invocation.json do golden: saida sob o --out (CONTROL: --out dentro do oraculo e acusado)
  ## (a) forma do registro: invocacao valida passa; --out dentro do oraculo ou fora do scratch e acusado
  good <- list(args = list("--vanilla", "'/x/oracle_driver.R'", "--oracle", "'/tmp/o'", "--unit", "'<scratch>/u/unit.json'",
                           "--out", "'<scratch>/u/run_1'"), cwd = "<scratch>")
  expect_identical(rfa_invocation_problems(good), character())
  inv_out <- function(x, v) { x$args[[match("--out", gsub("^'|'$", "", unlist(x$args))) + 1L]] <- shQuote(v); x }
  expect_match(rfa_invocation_problems(inv_out(good, file.path(RFA_ORACLE, "saida"))), "dentro do oraculo", all = FALSE)
  expect_match(rfa_invocation_problems(inv_out(good, "/tmp/fora/run_1")), "fora do scratch", all = FALSE)
  ## (b) golden real (gitignored; so existe depois de plugin_golden.R): todo invocation.json registrado
  inv_files <- list.files(file.path(rf_root(), ".state", "golden"), pattern = "^invocation[.]json$", recursive = TRUE, full.names = TRUE)
  if (!length(inv_files) && rfa_parity_tier())
    fail("regere o golden (scripts/tools/plugin_golden.R --model random-forest --units smoke): sem invocation.json")
  for (f in inv_files)
    expect_identical(rfa_invocation_problems(jsonlite::read_json(f, simplifyVector = FALSE)), character(), info = f)
})

## ---- higiene: models/random-forest/R -------------------------------------------------------------------------------------
RFA_FORBIDDEN <- list(
  getenv = "Sys[.](getenv|setenv)", systime = "Sys[.]time", sysdate = "Sys[.]Date", asdate = "as[.]Date[(]",
  posixct = "as[.]POSIXct", strptime = "strptime", difftime = "difftime", saverds = "saveRDS", s3600 = "3600",
  hours1 = "hours[(]\\s*1\\s*[)]", system = "(^|[^A-Za-z_.])system2?[(]", mclapply = "mclapply",
  prevcarga_env = "PREVCARGA_[A-Z_]+",
  ## o plugin le os cortes de `cuts`; atribuir um corte (train_origin, last_data_day, ...) e conta de corte propria
  corte_proprio = "(^|[^._A-Za-z$])(train_origin|last_data_day|available_until|weather_issue_limit|forecast_origin)\\s*<-")

rfa_r_files <- function(dir) sort(list.files(dir, pattern = "[.]R$", full.names = TRUE), method = "radix")
rfa_fmt <- function(h) paste0(basename(h$file), ":", h$line, " [", h$pattern, "]", collapse = "; ")

test_that("[PLUGINS] higiene: models/random-forest/R sem chamadas proibidas, sem aritmetica de hora (allow vazio), sem conta de corte; CONTROL plantado", {
  rdir <- file.path(RFA_MODEL_DIR(), "R")
  fs <- rfa_r_files(rdir)
  expect_gt(length(fs), 0L)
  h <- rf_lib$pc_grep_code(fs, RFA_FORBIDDEN)
  expect_identical(nrow(h), 0L, info = rfa_fmt(h))
  b <- rf_lib$pc_bytes_over_127(fs)
  expect_identical(nrow(b), 0L, info = paste(basename(b$file), b$line))
  ## aritmetica de hora da fundacao (P.0j): ZERO excecoes; to_model_frame/to_display_frame nunca no plugin
  r <- rf_lib$pc_grep_time_arith(RFA_MODEL_DIR(), allow = list())
  expect_identical(nrow(r), 0L, info = rfa_fmt(r))
  ## o plugin le os cortes dos `cuts`
  expect_true(any(grepl("cuts$train_origin", readLines(file.path(rdir, "ingestion.R")), fixed = TRUE)))
  expect_true(any(grepl("cuts$last_data_day", readLines(file.path(rdir, "ingestion.R")), fixed = TRUE)))

  ## CONTROL: copia de R/ com Sys.time() e uma mensagem com acento plantados => 2 hits nomeando arquivo:linha
  cp <- file.path(withr::local_tempdir(), "random-forest"); dir.create(cp)
  file.copy(rdir, cp, recursive = TRUE)
  expect_identical(nrow(rf_lib$pc_grep_code(rfa_r_files(file.path(cp, "R")), RFA_FORBIDDEN)), 0L)
  plant <- file.path(cp, "R", "zz_plant.R")
  writeLines(enc2utf8(c("x <- Sys.time()", "# Sys.getenv('X') so comentario", "stop(\"mensagem com ação\")")), plant, useBytes = TRUE)
  fs2 <- rfa_r_files(file.path(cp, "R"))
  hp <- rf_lib$pc_grep_code(fs2, RFA_FORBIDDEN)
  bp <- rf_lib$pc_bytes_over_127(fs2)
  expect_identical(nrow(hp), 1L); expect_identical(nrow(bp), 1L)
  expect_identical(c(basename(hp$file), hp$line, hp$pattern), c("zz_plant.R", "1", "systime"))
  expect_identical(c(basename(bp$file), bp$line), c("zz_plant.R", "3"))
  ## CONTROL de cada padrao e da aritmetica de hora
  for (bad in c(".z <- function(x) x + lubridate::hours(1)", ".z <- function(x) lubridate::with_tz(x, 'UTC')",
                ".z <- function(x) lubridate::force_tz(x, 'UTC')", ".z <- function(x) to_model_frame(x, 60L)",
                ".z <- function(x) to_display_frame(x, 60L)", ".z <- function(x) x + lubridate::minutes(30)")) {
    writeLines(bad, plant)
    expect_gt(nrow(rf_lib$pc_grep_time_arith(cp, allow = list())), 0L, label = bad)
  }
  for (bad in c("train_origin <- O - 1L - 9L", "last_data_day <- O - 1L", "Sys.Date()", "x <- as.Date('2026-01-01')", "system('ls')",
                "system2('ls')", "parallel::mclapply(1, f)", "saveRDS(x, f)", "d <- difftime(a, b)", "n <- 3600")) {
    writeLines(bad, plant)
    expect_gt(nrow(rf_lib$pc_grep_code(rfa_r_files(file.path(cp, "R")), RFA_FORBIDDEN)), 0L, label = bad)
  }
})

## ---- README: secoes obrigatorias e coerencia com o runner e o oraculo ------------------------------------------------------
RFA_README_SECTIONS <- c(
  "Mapa funcao -> arquivo:linha do oraculo", "Contrato", "Layout `rf_wide_hourly_216`", "Emenda do dia O", "Faixa O..O+8",
  "Alvos ate train_origin + H", "Desvios conscientes do oraculo", "Gates x dono")

rfa_runner_gate_names <- function() {
  ex <- parse(file.path(RFA_MODEL_DIR(), "tests", "run_all_gates.R"), keep.source = FALSE)
  env <- new.env(parent = baseenv())
  for (e in ex)
    if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]])[[1L]] %in% c(".rf_gates", "gate_table"))
      eval(e, env)
  env$gate_table
}

test_that("[PLUGINS] readme: secoes obrigatorias, desvios 1-5, toda linha do runner, mapa com linhas validas no oraculo e ponteiro do estado", {
  rd <- readLines(file.path(RFA_MODEL_DIR(), "README.md"), encoding = "UTF-8")
  heads <- sub("^##+ +", "", grep("^##+ ", rd, value = TRUE))
  for (s in RFA_README_SECTIONS) expect_true(s %in% heads, info = s)
  expect_true(all(utf8ToInt(paste(rd, collapse = "\n")) < 128L))
  ## desvios conscientes 1-5 (lista numerada dentro da secao)
  i0 <- grep("^## Desvios conscientes", rd); i1 <- grep("^## ", rd); i1 <- i1[i1 > i0][1L]
  sec <- rd[(i0 + 1L):(i1 - 1L)]
  expect_identical(sub("^([0-9]+)[.] .*$", "\\1", grep("^[0-9]+[.] ", sec, value = TRUE)), as.character(1:5))
  ## o estado duravel mora em docs/estado/random-forest.md e o README aponta para ele
  expect_true(any(grepl("docs/estado/random-forest.md", rd, fixed = TRUE)))
  expect_true(file.exists(file.path(rf_root(), "docs", "estado", "random-forest.md")))
  ## todo gate do runner aparece na secao Gates x dono, com o dono certo na mesma linha
  g0 <- grep("^## Gates x dono", rd); gsec <- rd[g0:length(rd)]
  gt <- rfa_runner_gate_names()
  for (i in seq_len(nrow(gt))) {
    ln <- grep(sprintf("`%s`", gt$gate[[i]]), gsec, fixed = TRUE, value = TRUE)
    expect_gt(length(ln), 0L, label = gt$gate[[i]])
    expect_true(any(grepl(sprintf("[%s]", gt$owner[[i]]), ln, fixed = TRUE)), info = gt$gate[[i]])
  }
  ## mapa: cada `arquivo.R:a-b` do oraculo existe e cabe no arquivo (so com o oraculo na maquina)
  if (dir.exists(file.path(RFA_ORACLE, "models", "random-forest"))) {
    refs <- unique(unlist(regmatches(paste(rd, collapse = "\n"),
                                     gregexpr("[A-Za-z_]+\\.R:[0-9]+(-[0-9]+)?", paste(rd, collapse = "\n")))))
    expect_gt(length(refs), 4L)
    base <- file.path(RFA_ORACLE, "models", "random-forest")
    for (r in refs) {
      f <- sub(":.*$", "", r); rg <- as.integer(strsplit(sub("^.*:", "", r), "-", fixed = TRUE)[[1L]])
      p <- list.files(base, pattern = paste0("^", sub("[.]", "[.]", f), "$"), recursive = TRUE, full.names = TRUE)
      if (!length(p)) next                                   # arquivo do plugin (ex.: inference.R:53), nao do oraculo
      expect_lte(max(rg), length(readLines(p[[1L]], warn = FALSE)), label = r)
    }
  }
  ## CONTROL: secao removida => o gate acusa
  expect_false("Alvos ate train_origin + H" %in% sub("^##+ +", "", grep("^##+ ", rd[-grep("^## Alvos ate", rd)], value = TRUE)))
})

## ---- CONTROL do runner: repo-fixture com o MESMO run_all_gates.R, um teste por linha da tabela -------------------------------
rfa_runner_src <- function() file.path(RFA_MODEL_DIR(), "tests", "run_all_gates.R")

rfa_fixture_repo <- function(failing = NULL) {
  base <- withr::local_tempdir(.local_envir = parent.frame())
  lib <- file.path(base, "scripts", "lib"); dir.create(lib, recursive = TRUE)
  dir.create(file.path(base, "scripts", "tools", "gates"), recursive = TRUE)
  file.copy(list.files(file.path(rf_root(), "scripts", "lib"), pattern = "[.]R$", full.names = TRUE), lib)
  file.copy(file.path(rf_root(), "scripts", "lib", "errors"), lib, recursive = TRUE)
  file.copy(file.path(rf_root(), "scripts", "tools", "plugin_gates.R"), file.path(base, "scripts", "tools"))
  tdir <- file.path(base, "models", "random-forest", "tests"); dir.create(file.path(tdir, "testthat"), recursive = TRUE)
  file.copy(rfa_runner_src(), tdir)
  gt <- rfa_runner_gate_names()
  tests <- vapply(seq_len(nrow(gt)), function(i) {
    lab <- sprintf("[%s] %s: fixture", gt$owner[[i]], gt$gate[[i]])
    body <- if (grepl("^DEFERRED", gt$owner[[i]])) sprintf("testthat::skip(\"[%s] fixture\")", gt$owner[[i]])
            else if (identical(gt$gate[[i]], failing)) "testthat::expect_true(FALSE)" else "testthat::expect_true(TRUE)"
    sprintf("test_that(%s, { %s })", encodeString(lab, quote = "\""), body)
  }, "")
  writeLines(tests, file.path(tdir, "testthat", "test-fixture.R"))
  file.path(tdir, "run_all_gates.R")
}

rfa_run <- function(runner, ...) {
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner), ...), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  list(status = if (is.null(st)) 0L else as.integer(st), out = out)
}

test_that("[PLUGINS] runner_controle: tabela completa verde => exit 0, ASCII, DEFERRED como SKIP com dono; gate [PLUGINS] falhando => exit 1", {
  gt <- rfa_runner_gate_names()
  expect_true(all(c("contrato", "ingest_frame", "weather_vintage", "carry_forward", "features_contract", "corte_treino", "determinismo", "freeze",
                    "no_env", "corte_dia_o", "corte_horario", "sentinela", "spline_rule", "estagios", "completude_infer", "weather",
                    "aditividade", "higiene", "readme") %in% gt$gate[gt$owner == "PLUGINS"]))
  expect_identical(gt$owner[match(c("gate04", "gate08", "gate12", "fachada", "cross_os"), gt$gate)],
                   c("DEFERRED M6", "DEFERRED M6", "DEFERRED M7", "DEFERRED M5", "DEFERRED M8"))
  expect_identical(gt$tier[gt$gate == "paridade"], "parity")
  ok <- rfa_run(rfa_fixture_repo(), "--tier", "fast")
  expect_identical(ok$status, 0L)
  expect_true(all(utf8ToInt(paste(ok$out, collapse = "")) < 128L))
  expect_true(any(grepl("^gate04 +\\| \\[DEFERRED M6\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^gate12 +\\| \\[DEFERRED M7\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^fachada +\\| \\[DEFERRED M5\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^cross_os +\\| \\[DEFERRED M8\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^paridade +\\| \\[PLUGINS\\] +\\| N/A\\(tier\\)", ok$out)))
  expect_true(any(grepl("^weather +\\| \\[PLUGINS\\] +\\| PASS", ok$out)))
  ## CONTROL: um gate [PLUGINS] falhando derruba o runner (exit 1); DEFERRED nunca derruba
  bad <- rfa_run(rfa_fixture_repo(failing = "weather"), "--tier", "fast")
  expect_identical(bad$status, 1L)
  expect_true(any(grepl("^weather +\\| \\[PLUGINS\\] +\\| FAIL", bad$out)))
})
