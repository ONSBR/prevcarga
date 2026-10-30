## test-additivity.R -- fechamento do plugin lgbm (LGBM.6): aditividade, higiene, README/estado duravel e CONTROL do runner.
## Gates [PLUGINS] aditividade_epico, higiene, readme e runner_controle. O grep usa pc_grep_code / pc_grep_time_arith /
## pc_bytes_over_127 da fundacao (P.0j, scripts/lib/tests/helper-plugins.R). Sem variaveis de ambiente; clones e fixtures em tempdir.
## Aditividade = o epico so escreve em models/lgbm/ (e nos itens sancionados abaixo): medida pelo git (commits "LGBM.n:" + working
## tree), como RF.5, porque o repo nao carrega o carimbo `.state/manifests` (gitignored, nunca existiu neste clone).
suppressWarnings(suppressMessages(library(testthat)))

lga_env <- new.env(parent = lgbm_lib)
sys.source(file.path(lgbm_root(), "scripts", "lib", "tests", "helper-plugins.R"), envir = lga_env)

LGA_ORACLE <- "/opt/ons-source/prevcargadessem3"
LGA_MODEL_DIR <- function() file.path(lgbm_root(), "models", "lgbm")
lga_tier <- function() if (exists(".pc_gates_state", envir = globalenv())) get(".pc_gates_state", envir = globalenv())$tier else "fast"
lga_parity_tier <- function() lga_tier() %in% c("parity", "parity-full")

## itens fora de models/lgbm/ que o epico sancionadamente tocou: catalogo de erros (barrel, ADR-0040), estado duravel e os
## fixtures de fundacao que o LGBM.1 estendeu (topologia/heat_area, helper de higiene)
LGA_SANCTIONED <- c("docs/estado/lgbm.md", "scripts/lib/errors/lgbm.tsv", "scripts/lib/tests/test-topology.R",
                    "scripts/lib/tests/helper-plugins.R")

lga_git <- function(args, dir = lgbm_root()) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(dir), args), stdout = TRUE, stderr = FALSE))
  st <- attr(out, "status")
  list(out = out, status = if (is.null(st)) 0L else as.integer(st))
}

## caminhos sob models/ e scripts/lib/ tocados pelos commits do epico (assunto "LGBM.n:", sem merges) e pelo working tree,
## fora de models/lgbm/ e dos itens sancionados
lga_foreign_changes <- function(repo) {
  commits <- lga_git(c("log", "--no-merges", "--format=%H", "--grep=^LGBM[.][0-9]"), repo)$out
  files <- unlist(lapply(commits, function(cm) lga_git(c("diff-tree", "--no-commit-id", "--name-only", "-r", cm), repo)$out))
  wt <- lga_git(c("status", "--porcelain", "--untracked-files=all", "--", "models", "scripts/lib"), repo)$out
  files <- unique(c(files, sub("^.{3}", "", wt)))
  files <- files[startsWith(files, "models/") | startsWith(files, "scripts/lib/")]
  files <- files[!startsWith(files, "models/lgbm/") & !(files %in% LGA_SANCTIONED)]
  sort(files, method = "radix")
}

## clone temporario com um arquivo plantado em `path` em um commit "LGBM.9: ..." (ou solto)
lga_clone_with_plant <- function(path, commit = TRUE) {
  dir <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "clone")
  system2("git", c("clone", "-q", shQuote(lgbm_root()), shQuote(dir)), stdout = FALSE, stderr = FALSE)
  full <- file.path(dir, path)
  dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
  writeLines("# plantado", full)
  if (commit) {
    lga_git(c("add", "--", shQuote(path)), dir)
    system2("git", c("-C", shQuote(dir), "-c", "user.name=ctl", "-c", "user.email=ctl@example.invalid", "commit", "-q", "-m",
                     shQuote("LGBM.9: arquivo plantado (CONTROL)")), stdout = FALSE, stderr = FALSE)
  }
  dir
}

## `invocation.json` do golden: nenhum caminho de saida (--out) dentro do oraculo e todo caminho de escrita sob o scratch (cwd)
lga_invocation_problems <- function(inv, oracle = LGA_ORACLE) {
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

test_that("[PLUGINS] aditividade_epico: so models/lgbm/ mudou (nenhum outro plugin, nada em scripts/lib alem do sancionado); oraculo limpo; golden fora do git; CONTROL plantado", {
  skip_if(!nzchar(Sys.which("git")) || lga_git(c("rev-parse", "--git-dir"))$status != 0L, "[tier] git indisponivel")
  expect_identical(lga_foreign_changes(lgbm_root()), character())
  ## CONTROL: arquivo plantado em outro plugin (commit LGBM.9 e arquivo solto) e em scripts/lib reprova nomeando-o
  c1 <- lga_clone_with_plant("models/holt-winters/plantado.R")
  expect_identical(lga_foreign_changes(c1), "models/holt-winters/plantado.R")
  c2 <- lga_clone_with_plant("models/_dessem-engine/solto.R", commit = FALSE)
  expect_identical(lga_foreign_changes(c2), "models/_dessem-engine/solto.R")
  c3 <- lga_clone_with_plant("scripts/lib/plantado.R")
  expect_identical(lga_foreign_changes(c3), "scripts/lib/plantado.R")
  ## um arquivo plantado DENTRO de models/lgbm/ nao e estranho ao epico
  c4 <- lga_clone_with_plant("models/lgbm/novo.R")
  expect_identical(lga_foreign_changes(c4), character())

  ## golden e artefatos nunca versionados; o golden e um diretorio real (check-ignore nao atravessa symlink)
  expect_identical(lga_git(c("check-ignore", "-q", ".state/golden/x"))$status, 0L)
  expect_length(lga_git(c("ls-files", ".state", "models/lgbm/models"))$out, 0L)

  ## oraculo somente leitura: so as 3 notas + verify-log/ de docs/roadmap/ (ja existiam la, nao sao deste epico) sao toleraveis
  if (!dir.exists(file.path(LGA_ORACLE, ".git"))) {
    if (lga_parity_tier()) fail("oraculo ausente nesta maquina no tier parity")
    skip("[tier] oraculo ausente nesta maquina")
  }
  st <- lga_git(c("status", "--porcelain"), LGA_ORACLE)$out
  tolerado <- paste("??", c("docs/roadmap/CLI-MODULAR-OBJETIVO.md", "docs/roadmap/CLI-MODULAR-ONDE-ESTAMOS.md",
                            "docs/roadmap/MIGRACAO-COMBINA-R.md", "docs/roadmap/verify-log/"))
  expect_length(setdiff(st, tolerado), 0L)
  ## CONTROL do criterio: um arquivo a mais no oraculo seria acusado
  expect_gt(length(setdiff(c(st, "?? plantado.R"), tolerado)), 0L)

  ## todo invocation.json do golden: saida sob o --out (CONTROL: --out dentro do oraculo e acusado)
  good <- list(args = list("--vanilla", "'/x/oracle_driver.R'", "--oracle", "'/tmp/o'", "--unit", "'<scratch>/u/unit.json'",
                           "--out", "'<scratch>/u/run_1'"), cwd = "<scratch>")
  expect_identical(lga_invocation_problems(good), character())
  inv_out <- function(x, v) { x$args[[match("--out", gsub("^'|'$", "", unlist(x$args))) + 1L]] <- shQuote(v); x }
  expect_match(lga_invocation_problems(inv_out(good, file.path(LGA_ORACLE, "saida"))), "dentro do oraculo", all = FALSE)
  expect_match(lga_invocation_problems(inv_out(good, "/tmp/fora/run_1")), "fora do scratch", all = FALSE)
  inv_files <- list.files(file.path(lgbm_root(), ".state", "golden"), pattern = "^invocation[.]json$", recursive = TRUE, full.names = TRUE)
  if (!length(inv_files) && lga_parity_tier())
    fail("regere o golden (scripts/tools/plugin_golden.R --model lgbm): sem invocation.json")
  for (f in inv_files)
    expect_identical(lga_invocation_problems(jsonlite::read_json(f, simplifyVector = FALSE)), character(), info = f)
})

## ---- higiene: models/lgbm/R ----------------------------------------------------------------------------------------------
LGA_FORBIDDEN <- list(
  getenv = "Sys[.](getenv|setenv)", systime = "Sys[.]time", sysdate = "Sys[.]Date", asdate = "as[.]Date[(]",
  posixct = "as[.]POSIXct", saverds = "saveRDS", lgbsave = "lgb[.]save", to_hourly = "to_hourly",
  to_model_frame = "to_model_frame", to_display_frame = "to_display_frame", prevcarga_job = "PREVCARGA_JOB",
  ## o plugin le os cortes de `cuts`; refazer o corte (`O - 3`, `origin - 3`) ou atribuir um corte e conta propria
  corte_O_menos = "(^|[^._A-Za-z$])(O|origin)\\s*-\\s*[0-9]+L?",
  corte_proprio = "(^|[^._A-Za-z$])(train_origin|last_data_day|available_until|weather_issue_limit|forecast_origin)\\s*<-")

lga_r_files <- function(dir) sort(list.files(dir, pattern = "[.]R$", full.names = TRUE), method = "radix")
lga_fmt <- function(h) paste0(basename(h$file), ":", h$line, " [", h$pattern, "]", collapse = "; ")

test_that("[PLUGINS] higiene: models/lgbm/R sem env/relogio/saveRDS/lgb.save/datas base/transforms, sem aritmetica de hora (allow vazio), sem conta de corte, ASCII; CONTROL de cada padrao", {
  rdir <- file.path(LGA_MODEL_DIR(), "R")
  fs <- lga_r_files(rdir)
  expect_gt(length(fs), 0L)
  h <- lga_env$pc_grep_code(fs, LGA_FORBIDDEN)
  expect_identical(nrow(h), 0L, info = lga_fmt(h))
  b <- lga_env$pc_bytes_over_127(fs)
  expect_identical(nrow(b), 0L, info = paste(basename(b$file), b$line))
  ## aritmetica de hora da fundacao (P.0j): ZERO excecoes (with_tz/force_tz, hours()/minutes(), %m+%, 3600, difftime...)
  r <- lga_env$pc_grep_time_arith(LGA_MODEL_DIR(), allow = list())
  expect_identical(nrow(r), 0L, info = lga_fmt(r))
  ## o plugin le os cortes dos `cuts`
  src <- unlist(lapply(fs, readLines, warn = FALSE))
  expect_true(any(grepl("cuts$last_data_day", src, fixed = TRUE)))
  expect_true(any(grepl("cuts$train_origin", src, fixed = TRUE)))

  ## CONTROL: copia de R/ com Sys.time() e uma mensagem com acento plantados => 1 hit de codigo + 1 byte > 127 em zz_plant.R
  cp <- file.path(withr::local_tempdir(), "lgbm"); dir.create(cp)
  file.copy(rdir, cp, recursive = TRUE)
  expect_identical(nrow(lga_env$pc_grep_code(lga_r_files(file.path(cp, "R")), LGA_FORBIDDEN)), 0L)
  plant <- file.path(cp, "R", "zz_plant.R")
  writeLines(enc2utf8(c("x <- Sys.time()", "# Sys.getenv('X') so comentario", "stop(\"mensagem com ação\")")), plant, useBytes = TRUE)
  fs2 <- lga_r_files(file.path(cp, "R"))
  hp <- lga_env$pc_grep_code(fs2, LGA_FORBIDDEN)
  bp <- lga_env$pc_bytes_over_127(fs2)
  expect_identical(nrow(hp), 1L); expect_identical(nrow(bp), 1L)
  expect_identical(c(basename(hp$file), hp$line, hp$pattern), c("zz_plant.R", "1", "systime"))
  expect_identical(c(basename(bp$file), bp$line), c("zz_plant.R", "3"))
  ## CONTROL de cada padrao de aritmetica de hora
  for (bad in c(".z <- function(x) x + lubridate::hours(1)", ".z <- function(x) lubridate::with_tz(x, 'UTC')",
                ".z <- function(x) lubridate::force_tz(x, 'UTC')", ".z <- function(x) to_model_frame(x, 60L)",
                ".z <- function(x) to_display_frame(x, 60L)", ".z <- function(x) x + lubridate::minutes(30)",
                ".z <- function(x) x %m+% lubridate::period(1)", "n <- 3600 + 1")) {
    writeLines(bad, plant)
    expect_gt(nrow(lga_env$pc_grep_time_arith(cp, allow = list())), 0L, label = bad)
  }
  ## CONTROL de cada padrao proibido do AC4 e do corte
  for (bad in c("Sys.getenv('X')", "Sys.setenv(X = '1')", "Sys.time()", "Sys.Date()", "x <- as.Date('2026-01-01')",
                "x <- as.POSIXct('2026-01-01')", "saveRDS(x, f)", "lgb.save(m, f)", "to_hourly(x)", "to_model_frame(x, 60L)",
                "to_display_frame(x, 60L)", "j <- 'PREVCARGA_JOB'", "ld <- O - 3L", "ld <- origin - 3", "train_origin <- O - 1L",
                "last_data_day <- O - 3L")) {
    writeLines(bad, plant)
    expect_gt(nrow(lga_env$pc_grep_code(lga_r_files(file.path(cp, "R")), LGA_FORBIDDEN)), 0L, label = bad)
  }
})

## ---- README e estado duravel: secoes obrigatorias e coerencia com o runner e o oraculo ----------------------------------------
LGA_README_SECTIONS <- c(
  "Mapa funcao -> arquivo:linha do oraculo", "Contrato do descriptor", "Layout `lgbm_day_48`", "Tabela env flag -> param",
  "Cortes por fase", "Entrega O..O+1", "Paridade (golden.lock)", "Desvios conscientes do oraculo", "Gates x dono")
LGA_ESTADO_SECTIONS <- c("LGBM.6 -- verificacao de fechamento")

lga_runner_gate_names <- function() {
  ex <- parse(file.path(LGA_MODEL_DIR(), "tests", "run_all_gates.R"), keep.source = FALSE)
  env <- new.env(parent = baseenv())
  for (e in ex)
    if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]])[[1L]] %in% c(".lgbm_gates", "gate_table"))
      eval(e, env)
  env$gate_table
}

test_that("[PLUGINS] readme: secoes obrigatorias, contrato (data_lag_days 2, delivery_days 2), env flag -> param, cortes, entrega, toda linha do runner, mapa com linhas validas no oraculo e estado duravel", {
  rd <- readLines(file.path(LGA_MODEL_DIR(), "README.md"), encoding = "UTF-8")
  txt <- paste(rd, collapse = "\n")
  heads <- sub("^##+ +", "", grep("^##+ ", rd, value = TRUE))
  for (s in LGA_README_SECTIONS) expect_true(s %in% heads, info = s)
  expect_true(all(utf8ToInt(txt) < 128L))
  for (needle in c("data_lag_days: 2", "delivery_days: 2", "lgbm_day_48", "train_origin = O-2", "last_data_day = O-3",
                   "available_until", "O..O+1", "golden.lock"))
    expect_true(grepl(needle, txt, fixed = TRUE), info = needle)
  ## tabela env flag -> param: as 7 flags do oraculo e os 7 params do run.yaml
  y <- yaml::read_yaml(file.path(LGA_MODEL_DIR(), "run.yaml"))
  for (p in names(y$params)) expect_true(grepl(sprintf("`%s`", p), txt, fixed = TRUE), info = p)
  for (f in c("LGBM_DISABLE_LOESS", "LGBM_CALENDAR_YEAREND", "LGBM_TRAIN_CV", "LGBM_TIPODIA_LEGACY", "LGBM_1230_PREVESPERA",
              "LGBM_CUTOFF_SCHEME", "LGBM_SAZONALIDADE_SCOPE"))
    expect_true(grepl(f, txt, fixed = TRUE), info = f)
  ## desvios conscientes 1-5 (lista numerada dentro da secao)
  i0 <- grep("^## Desvios conscientes", rd); i1 <- grep("^## ", rd); i1 <- i1[i1 > i0][1L]
  sec <- rd[(i0 + 1L):(i1 - 1L)]
  expect_identical(sub("^([0-9]+)[.] .*$", "\\1", grep("^[0-9]+[.] ", sec, value = TRUE)), as.character(1:5))
  ## o hash do golden.lock citado no README e o do arquivo versionado
  lock <- file.path(LGA_MODEL_DIR(), "tests", "parity", "golden.lock")
  expect_true(grepl(digest::digest(lock, algo = "sha256", file = TRUE), txt, fixed = TRUE))
  ## o estado duravel mora em docs/estado/lgbm.md e o README aponta para ele
  expect_true(any(grepl("docs/estado/lgbm.md", rd, fixed = TRUE)))
  est_path <- file.path(lgbm_root(), "docs", "estado", "lgbm.md")
  expect_true(file.exists(est_path))
  est <- readLines(est_path, encoding = "UTF-8")
  eheads <- sub("^##+ +", "", grep("^##+ ", est, value = TRUE))
  for (s in LGA_ESTADO_SECTIONS) expect_true(any(startsWith(eheads, s)), info = s)
  etxt <- paste(est, collapse = "\n")
  for (needle in c("Membro LGBM vivo", "origin_agnostic", "golden.lock", "Desvios conscientes", "sem auto-deteccao de corte",
                   "sem fallback silencioso de feriados", "sem heranca de flags por env", "rotulos de calendario"))
    expect_true(grepl(needle, etxt, fixed = TRUE), info = needle)
  ## todo gate do runner aparece na secao Gates x dono, com o dono certo na mesma linha
  g0 <- grep("^## Gates x dono", rd); gsec <- rd[g0:length(rd)]
  gt <- lga_runner_gate_names()
  for (i in seq_len(nrow(gt))) {
    ln <- grep(sprintf("`%s`", gt$gate[[i]]), gsec, fixed = TRUE, value = TRUE)
    expect_gt(length(ln), 0L, label = gt$gate[[i]])
    expect_true(any(grepl(sprintf("[%s]", gt$owner[[i]]), ln, fixed = TRUE)), info = gt$gate[[i]])
  }
  ## mapa: cada `arquivo.R:a-b` do oraculo existe e cabe no arquivo (so com o oraculo na maquina)
  if (dir.exists(file.path(LGA_ORACLE, "models", "lgbm", "R"))) {
    refs <- unique(unlist(regmatches(txt, gregexpr("[A-Za-z_]+\\.R:[0-9]+(-[0-9]+)?", txt))))
    expect_gt(length(refs), 8L)
    base <- file.path(LGA_ORACLE, "models", "lgbm", "R")
    for (r in refs) {
      f <- sub(":.*$", "", r); rg <- as.integer(strsplit(sub("^.*:", "", r), "-", fixed = TRUE)[[1L]])
      p <- list.files(base, pattern = paste0("^", sub("[.]", "[.]", f), "$"), recursive = TRUE, full.names = TRUE)
      if (!length(p)) next                                   # arquivo do plugin, nao do oraculo
      expect_lte(max(rg), length(readLines(p[[1L]], warn = FALSE)), label = r)
    }
  }
  ## CONTROL: secao removida => o criterio acusa
  expect_false("Cortes por fase" %in% sub("^##+ +", "", grep("^##+ ", rd[-grep("^## Cortes por fase", rd)], value = TRUE)))
})

## ---- CONTROL do runner: repo-fixture com o MESMO run_all_gates.R, um teste por linha da tabela ----------------------------------
lga_runner_src <- function() file.path(LGA_MODEL_DIR(), "tests", "run_all_gates.R")

lga_fixture_repo <- function(failing = NULL) {
  base <- withr::local_tempdir(.local_envir = parent.frame())
  lib <- file.path(base, "scripts", "lib"); dir.create(lib, recursive = TRUE)
  dir.create(file.path(base, "scripts", "tools", "gates"), recursive = TRUE)
  file.copy(list.files(file.path(lgbm_root(), "scripts", "lib"), pattern = "[.]R$", full.names = TRUE), lib)
  file.copy(file.path(lgbm_root(), "scripts", "lib", "errors"), lib, recursive = TRUE)
  file.copy(file.path(lgbm_root(), "scripts", "tools", "plugin_gates.R"), file.path(base, "scripts", "tools"))
  tdir <- file.path(base, "models", "lgbm", "tests"); dir.create(file.path(tdir, "testthat"), recursive = TRUE)
  file.copy(lga_runner_src(), tdir)
  gt <- lga_runner_gate_names()
  tests <- vapply(seq_len(nrow(gt)), function(i) {
    lab <- sprintf("[%s] %s: fixture", gt$owner[[i]], gt$gate[[i]])
    body <- if (grepl("^DEFERRED", gt$owner[[i]])) sprintf("testthat::skip(\"[%s] fixture\")", gt$owner[[i]])
            else if (identical(gt$gate[[i]], failing)) "testthat::expect_true(FALSE)" else "testthat::expect_true(TRUE)"
    sprintf("test_that(%s, { %s })", encodeString(lab, quote = "\""), body)
  }, "")
  writeLines(tests, file.path(tdir, "testthat", "test-fixture.R"))
  file.path(tdir, "run_all_gates.R")
}

lga_run <- function(runner, ...) {
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner), ...), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  list(status = if (is.null(st)) 0L else as.integer(st), out = out)
}

test_that("[PLUGINS] runner_controle: tabela completa verde => exit 0, ASCII, DEFERRED como SKIP com dono, JSON unico; gate [PLUGINS] falhando => exit 1; parity sem golden => exit 1", {
  gt <- lga_runner_gate_names()
  need <- c("gate01_perturbacao", "gate02_escala_mw", "gate03_norm_train_only", "gate04_decoupling", "gate05_sem_crash",
            "gate07_stage_a_48", "gate09_fuso", "gate10_determinismo", "gate12_ondaletas_d1", "gate_cal", "gate_saz",
            "gate_inf_adequacao", "gate_cv_refit", "gate_cut", "gate_adequacao_add", "gate_nowcast_dplus0", "parity_construcao_d1",
            "clima_honesto", "clima_limite", "clima_safra", "aditividade_epico", "higiene", "readme", "runner_controle")
  expect_true(all(need %in% gt$gate[gt$owner == "PLUGINS"]))
  expect_identical(gt$owner[match(c("gate11_ytrue_real", "nowcast_dplus0_long", "fachada", "cross_os"), gt$gate)],
                   c("DEFERRED M6", "DEFERRED M6", "DEFERRED M5", "DEFERRED M8"))
  expect_identical(gt$tier[match(c("paridade", "paridade_control", "gate_adequacao_add"), gt$gate)], rep("parity", 3L))
  ok <- lga_run(lga_fixture_repo(), "--tier", "fast")
  expect_identical(ok$status, 0L)
  expect_true(all(utf8ToInt(paste(ok$out, collapse = "")) < 128L))
  expect_true(any(grepl("^gate11_ytrue_real +\\| \\[DEFERRED M6\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^nowcast_dplus0_long +\\| \\[DEFERRED M6\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^fachada +\\| \\[DEFERRED M5\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^cross_os +\\| \\[DEFERRED M8\\] +\\| SKIP", ok$out)))
  expect_true(any(grepl("^paridade +\\| \\[PLUGINS\\] +\\| N/A\\(tier\\)", ok$out)))
  expect_true(any(grepl("^clima_honesto +\\| \\[PLUGINS\\] +\\| PASS", ok$out)))
  ## --format json: um unico documento JSON
  js <- lga_run(lga_fixture_repo(), "--tier", "fast", "--format", "json")
  expect_identical(js$status, 0L)
  parsed <- jsonlite::fromJSON(paste(js$out, collapse = "\n"), simplifyVector = FALSE)
  expect_true(is.list(parsed))
  ## CONTROL: um gate [PLUGINS] falhando derruba o runner (exit 1); DEFERRED nunca derruba
  bad <- lga_run(lga_fixture_repo(failing = "clima_honesto"), "--tier", "fast")
  expect_identical(bad$status, 1L)
  expect_true(any(grepl("^clima_honesto +\\| \\[PLUGINS\\] +\\| FAIL", bad$out)))
  ## CONTROL: --tier parity com o fixture (sem golden no repo-fixture) mantem exit 1 so se algum teste de paridade falhar; aqui o
  ## teste de paridade do fixture falha de proposito (gate de tier parity falhando => exit 1)
  badp <- lga_run(lga_fixture_repo(failing = "paridade"), "--tier", "parity")
  expect_identical(badp$status, 1L)
})
