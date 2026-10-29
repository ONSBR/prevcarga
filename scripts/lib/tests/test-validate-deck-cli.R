## test-validate-deck-cli.R -- gates de PROCESSO do M2 (M2.V): `Rscript --vanilla <copia>/scripts/prevcarga validate-deck|describe`
## real, stdout/stderr separados (sem shell). Os testes in-process por modulo vivem em M2.1-M2.7 e nao sao duplicados.
## Fixtures so em tempfile()/tempdir() (nunca data/); caminhos por file.path; instalacao-copia com historicos pequenos
## para o deck curto de 40 dias. Shell sujo e simulado por wrapper R portavel (Sys.setenv/Sys.setlocale antes da fachada),
## sem `env VAR=` (nao existe no Windows). Modelo de referencia: M1.V (test-cli-process.R, helper-install-copy.R).
library(testthat)

M2_TZ <- "Etc/GMT+3"

## ---- instalacao-copia com historicos pequenos (editaveis) ------------------------------------------------------------
m2_root <- function(hw = c(30, 10)) {
  pc_install_copy(mutate = function(r) {
    set <- function(m, tr, inf) {
      f <- file.path(r, "models", m, "run.yaml"); y <- yaml::read_yaml(f)
      y$history_days <- list(train = tr, infer = inf); yaml::write_yaml(y, f)
    }
    set("holt-winters", hw[1L], hw[2L]); set("random-forest", 30, 10); set("lgbm", 20, 5)
    set("dessem-regdin", 20, 20); set("dessem-linear", 20, 20)
  })
}

## ---- decks (tempdir): limpo de 40 dias terminando em O = 2026-10-01 ------------------------------------------------------
m2_tmp <- function() { d <- tempfile("m2deck-"); dir.create(d); d }
m2_holidays <- function(areas) {
  h <- pc_deck_holidays_default(areas)
  h[which(as.numeric(simplified_code) < 0), simplified_code := bit64::as.integer64(1L)]   # deck limpo: sem codigo negativo
  h
}
m2_build <- function(n = 40, areas = c("SECO", "S")) {
  d <- m2_tmp()
  days <- seq(lubridate::ymd("2026-10-01") - (n - 1L), lubridate::ymd("2026-10-01"), by = "day")
  pc_deck_sentinel("end", days = days, areas = areas, dir = d)
  .pc_deck_write(m2_holidays(areas), d, file.path("auxiliary", "holidays"), character(), "date")
  d
}
.m2_tpl <- new.env()
m2_clean <- function() {
  if (is.null(.m2_tpl$d)) .m2_tpl$d <- m2_build()
  dst <- m2_tmp()
  file.copy(list.files(.m2_tpl$d, full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)
  dst
}

## ---- execucao de processo ---------------------------------------------------------------------------------------------------
m2_lit <- function(x) paste(deparse(x), collapse = " ")

## Wrapper R em tempfile(): aplica o ambiente sujo no processo filho ANTES da fachada (simula o shell herdado, portavel).
## `preset` = lista nomeada (TZ, LC_ALL, ...); `skip_runtime` = CONTROL que pula prevcarga_runtime_enforce().
m2_preset_lines <- function(preset) {
  env <- preset[setdiff(names(preset), "LC_ALL")]
  c(if (length(env)) sprintf("do.call(Sys.setenv, %s)", m2_lit(env)),
    if (!is.null(preset$LC_ALL)) sprintf("invisible(Sys.setlocale('LC_ALL', %s))", m2_lit(preset$LC_ALL)))
}

m2_wrapper <- function(root, args, preset, skip_runtime) {
  fac <- normalizePath(file.path(root, "scripts", "prevcarga"), winslash = "/")
  lib <- normalizePath(file.path(root, "scripts", "lib"), winslash = "/")
  w <- c(m2_preset_lines(preset),
    sprintf(".pc_args <- %s", m2_lit(as.character(args))),
    sprintf("commandArgs <- function(trailingOnly = FALSE) if (trailingOnly) .pc_args else c('Rscript', '--file=%s')", fac))
  if (!isTRUE(skip_runtime)) {
    w <- c(w, sprintf("source('%s', local = FALSE)", fac))                       # a fachada real
  } else {                                                                        # fachada sem a 1a acao do corpo
    w <- c(w, sprintf("options(prevcarga.lib = '%s')", lib),
           sprintf("for (m in c('errors.R', 'runtime.R')) source(file.path('%s', m), local = FALSE)", lib),
           sprintf("for (m in c('config.R', 'banner.R', 'cli.R')) source(file.path('%s', m), local = FALSE)", lib),
           sprintf("quit(status = prevcarga_main(.pc_args, root = '%s', runtime = function() invisible(NULL)), save = 'no')",
                   normalizePath(root, winslash = "/")))
  }
  f <- tempfile("m2-wrap-", fileext = ".R")
  writeLines(w, f)
  f
}

## Sonda: o MESMO shell sujo, com ou sem a guarda do runtime, imprime a hora da epoca (instante) no processo filho.
m2_probe <- function(root, preset, enforce) {
  lib <- normalizePath(file.path(root, "scripts", "lib"), winslash = "/")
  w <- c(m2_preset_lines(preset), sprintf("options(prevcarga.lib = '%s')", lib),
         sprintf("for (m in c('errors.R', 'runtime.R')) source(file.path('%s', m), local = FALSE)", lib),
         if (enforce) "invisible(prevcarga_runtime_enforce())",
         "writeLines(paste(lubridate::hour(.POSIXct(0))))")
  f <- tempfile("m2-probe-", fileext = ".R"); writeLines(w, f)
  pc_rscript(f)
}

m2_run <- function(root, args = character(), preset = NULL, skip_runtime = FALSE) {
  tf1 <- tempfile("pc-out"); tf2 <- tempfile("pc-err")
  on.exit(unlink(c(tf1, tf2)), add = TRUE)
  script <- if (length(preset) || skip_runtime) m2_wrapper(root, args, preset, skip_runtime)
            else file.path(root, "scripts", "prevcarga")
  st <- suppressWarnings(system2(pc_rscript_bin(), c("--vanilla", shQuote(script), if (!length(preset) && !skip_runtime) args),
                                 stdout = tf1, stderr = tf2))
  bytes <- function(f) { n <- file.info(f)$size; if (is.na(n) || n == 0) raw() else readBin(f, "raw", n) }
  list(status = as.integer(st), stdout = readLines(tf1, warn = FALSE), stderr = readLines(tf2, warn = FALSE),
       raw = c(bytes(tf1), bytes(tf2)))
}
m2_vd <- function(root, deck, extra = character(), fmt = "json", ...)
  m2_run(root, c("validate-deck", "--data", deck, extra, "--format", fmt), ...)
m2_doc <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
m2_strip <- function(r) { d <- m2_doc(r); d$run_id <- NULL; d }

root_ok <- m2_root()
root_hw <- m2_root(hw = c(60, 10))        # HW exige mais historico que o deck de 40 dias

## ---- bloco 1: matriz de exit por classe (AC 9) ------------------------------------------------------------------------------
fx <- local({
  miss <- m2_clean(); pc_mut_drop_col(miss, "load", "load_mwh")
  d47 <- m2_clean(); pc_mut_load(d47, function(dt) dt[-(.N - 240L)])
  xx <- m2_clean(); pc_mut_add_area(xx, "XX")
  ok <- m2_clean()
  list(limpo = ok, sem_load_mwh = miss, dia_47 = d47, area_xx = xx)
})
CLI_CASES <- data.frame(
  fixture = c("limpo", "sem_load_mwh", "dia_47", "area_xx", "limpo", "limpo"),
  exit = c(0L, 2L, 3L, 4L, 5L, 8L),
  class = c("ok", "contract", "completude", "topology", "window", "config"),
  id = c(NA, "contract.schema_missing", "completude.day_incomplete", "topology.area_undeclared",
         "window.window_train_deficit", "config.bad_load_label"),
  stringsAsFactors = FALSE)
CLI_ARGS <- list(character(), character(), character(), character(), c("--model", "holt-winters"), c("--load-label", "xyz"))
CLI_ROOT <- list(root_ok, root_ok, root_ok, root_ok, root_hw, root_ok)

test_that("[CLASSE] exit 0/2/3/4/5/8 por processo real: um por classe, texto e json (AC 9)", {
  for (i in seq_len(nrow(CLI_CASES))) for (fmt in c("text", "json")) {
    cs <- CLI_CASES[i, ]; nm <- paste(cs$fixture, cs$class, fmt)
    r <- m2_vd(CLI_ROOT[[i]], fx[[cs$fixture]], CLI_ARGS[[i]], fmt)
    expect_identical(r$status, cs$exit, info = nm)
    expect_false(any(r$raw > as.raw(127)), info = nm)                                 # nenhum byte > 127
    expect_false(any(grepl("Traceback|Error in", c(r$stdout, r$stderr))), info = nm)  # sem --debug nunca traceback
    if (fmt == "json") {
      doc <- m2_doc(r)
      expect_length(Filter(nzchar, r$stdout), 1L)                                     # um unico documento
      expect_setequal(names(doc), c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version"))
      expect_identical(doc$verb, "validate-deck")
      expect_identical(doc$exit_code, r$status, info = nm)
      expect_length(r$stderr, 0L)
      if (cs$exit == 0L) { expect_true(doc$ok); expect_null(doc$error) }
      else { expect_false(doc$ok); expect_identical(doc$error$class, cs$class, info = nm); expect_identical(doc$error$id, cs$id, info = nm) }
    } else if (cs$exit > 0L) {
      expect_match(r$stderr[1L], sprintf("^prevcarga: erro \\[%s\\] \\(exit %d\\)$", cs$class, cs$exit), info = nm)
    } else {
      expect_length(r$stderr, 0L); expect_true("OK" %in% r$stdout)
    }
  }
})

test_that("[CONTROL] a tabela tem dente: o mesmo deck limpo com exit esperado 2 reprova a assercao de status", {
  r <- m2_vd(root_ok, fx$limpo)
  expect_false(identical(r$status, 2L))
  expect_false(identical(m2_vd(root_ok, fx$sem_load_mwh)$status, 0L))
})

## ---- bloco 2: shell sujo (AC 10) ----------------------------------------------------------------------------------------------
test_that("[FORBIDDEN] shell sujo (TZ/LC_ALL/BLAS) == sessao limpa, salvo run_id (delta=0) (AC 10)", {
  lc <- pc_dirty_lc_all()
  skip_if(is.na(lc), "host sem nenhum locale UTF-8 alternativo para sujar LC_ALL")
  preset <- list(TZ = "America/Sao_Paulo", LC_ALL = lc, OPENBLAS_NUM_THREADS = "4")
  D <- fx$limpo
  clean <- m2_vd(root_ok, D)
  dirty <- m2_vd(root_ok, D, preset = preset)
  expect_identical(clean$status, 0L); expect_identical(dirty$status, 0L)
  expect_length(dirty$stderr, 0L); expect_length(clean$stderr, 0L)
  expect_identical(m2_strip(dirty), m2_strip(clean))
  expect_false(identical(m2_doc(dirty)$run_id, m2_doc(clean)$run_id))               # run_id e o unico campo que muda
  dtxt <- m2_vd(root_ok, D, fmt = "text", preset = preset); ctxt <- m2_vd(root_ok, D, fmt = "text")
  expect_identical(dtxt$stdout, ctxt$stdout)
})

test_that("[CONTROL] o shell sujo e real: sem a guarda o instante desloca; com a guarda (fachada) o doctor mostra found != effective (AC 10)", {
  lc <- pc_dirty_lc_all()
  preset <- list(TZ = "America/Sao_Paulo", OPENBLAS_NUM_THREADS = "4")
  sem <- m2_probe(root_ok, preset, enforce = FALSE)
  com <- m2_probe(root_ok, preset, enforce = TRUE)
  expect_identical(sem$stdout[[1L]], "21")                       # CONTROL: sem runtime_enforce o instante da epoca sai deslocado
  expect_identical(com$stdout[[1L]], "0")                        # FORBIDDEN: com a guarda o instante e UTC
  expect_false(identical(sem$stdout[[1L]], com$stdout[[1L]]))
  ## a fachada recebe o preset (nao e um no-op) e o fixa in-process: found sujo, effective UTC/1
  r <- m2_run(root_ok, c("doctor", "--format", "json"), preset = c(preset, if (!is.na(lc)) list(LC_ALL = lc)))
  expect_identical(r$status, 0L); expect_length(r$stderr, 0L)
  ck <- m2_doc(r)$result$checks
  row <- function(nm) Filter(function(x) identical(x$check, nm), ck)[[1L]]
  expect_identical(row("TZ")$found, "America/Sao_Paulo"); expect_identical(row("TZ")$effective, "UTC")
  expect_identical(row("OPENBLAS_NUM_THREADS")$effective, "1")
  ## a CLI com skip_runtime e o shell limpo ainda roda o verbo (o CONTROL nao quebra o resto da cadeia)
  expect_identical(m2_vd(root_ok, fx$limpo, preset = list(TZ = "UTC"), skip_runtime = TRUE)$status, 0L)
})

## ---- bloco 3: parquet corrompido por processo (AC 11) ----------------------------------------------------------------------
test_that("parquet corrompido: exit 2, parquet_unreadable com where relativo ao data_dir, nunca exit 1 nem traceback (AC 11)", {
  d <- m2_clean()
  rel <- "load/area_code=SECO/year=2026/part-0.parquet"
  expect_true(file.exists(file.path(d, rel)))
  pc_mut_corrupt(d, rel)
  for (fmt in c("json", "text")) {
    r <- m2_vd(root_ok, d, fmt = fmt)
    expect_identical(r$status, 2L, info = fmt)
    expect_false(any(grepl("Traceback|Error in", c(r$stdout, r$stderr))))
    expect_false(any(r$raw > as.raw(127)))
  }
  r <- m2_vd(root_ok, d)
  doc <- m2_doc(r)
  expect_identical(doc$exit_code, 2L); expect_identical(doc$error$class, "contract")
  expect_identical(doc$error$id, "contract.parquet_unreadable")
  expect_length(r$stderr, 0L)
  f <- Filter(function(x) identical(x$what, "parquet_unreadable"), doc$error$details$items)
  expect_gte(length(f), 1L)
  expect_identical(f[[1L]]$where, rel)                                                # relativo: sem o data_dir
  expect_false(grepl(d, f[[1L]]$where, fixed = TRUE))
  expect_identical(m2_vd(root_ok, m2_clean())$status, 0L)                             # CONTROL: sem corromper, limpo
})

## ---- bloco 4: identidade e cortes entre processos (AC 13) -------------------------------------------------------------------
## 1 byte trocado num parquet SEM tornar o arquivo ilegivel: o `created_by` do rodape (texto fixo) e alterado.
m2_flip_one_byte <- function(f) {
  b <- readBin(f, "raw", file.size(f))
  tag <- charToRaw("parquet-cpp-arrow")
  i <- which(b == tag[1L])
  i <- i[vapply(i, function(k) k + length(tag) - 1L <= length(b) && identical(b[k:(k + length(tag) - 1L)], tag), NA)]
  stopifnot(length(i) >= 1L)
  b[i[[length(i)]]] <- as.raw(bitwXor(as.integer(b[i[[length(i)]]]), 1L))
  writeBin(b, f)
}

test_that("data_hash: identico entre processos e modelos no limpo; difere na copia com 1 byte trocado (AC 13)", {
  D <- fx$limpo
  hash <- function(deck, ...) m2_doc(m2_vd(root_ok, deck, ...))$result$data_hash
  h1 <- hash(D, c("--model", "lgbm")); h2 <- hash(D, c("--model", "random-forest")); h3 <- hash(D, c("--model", "lgbm"))
  expect_match(h1, "^[0-9a-f]{64}$")
  expect_identical(h2, h1); expect_identical(h3, h1)                                  # FORBIDDEN delta=0
  expect_identical(hash(D), h1)                                                       # sem --model tambem
  cp <- m2_clean()
  f <- list.files(file.path(cp, "load"), pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)[[1L]]
  m2_flip_one_byte(f)
  r <- m2_vd(root_ok, cp, c("--model", "lgbm"))
  expect_identical(r$status, 0L)                                                      # a copia segue legivel
  hc <- m2_doc(r)$result$data_hash
  expect_match(hc, "^[0-9a-f]{64}$"); expect_false(identical(hc, h1))                 # CONTROL delta != 0
})

test_that("describe --origin 2026-10-01 por processo: uma linha por membro com train_origin por classe de split (AC 13)", {
  r <- m2_run(root_ok, c("describe", "--origin", "2026-10-01", "--format", "json"))
  expect_identical(r$status, 0L); expect_length(r$stderr, 0L)
  expect_false(any(r$raw > as.raw(127)))
  mem <- m2_doc(r)$result$cuts$members
  row <- function(m) Filter(function(x) identical(x$model, m), mem)[[1L]]
  expect_length(mem, 5L)
  expect_setequal(vapply(mem, function(x) x$model, ""), c("dessem-linear", "dessem-regdin", "holt-winters", "lgbm", "random-forest"))
  rf <- row("random-forest"); lg <- row("lgbm"); de <- row("dessem-regdin"); dl <- row("dessem-linear")
  expect_identical(rf$train_origin, "2026-09-21"); expect_identical(rf$last_data_day, "2026-09-30")
  expect_identical(lg$train_origin, "2026-09-29")
  expect_false(identical(rf$train_origin, lg$train_origin))                           # origin_agnostic: O-1-H por horizonte
  for (x in list(de, dl)) {
    expect_identical(x$train_origin, "2026-10-01"); expect_identical(x$last_data_day, "2026-09-30")   # origin_scoped
  }
  ## data_lag_days chega com os descriptors (P.0b/M5): ate la o processo usa o default 0 e o declara
  expect_identical(lg$data_lag_days, 0L); expect_identical(lg$data_lag_source, "default")
  expect_identical(lg$last_data_day, "2026-09-30")
})

test_that("LGBM last_data_day = O-3 (data_lag_days 2 do descriptor) pelo mesmo construtor do describe (AC 13)", {
  options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
  dv <- pc_source_lib(c("errors", "runtime", "config", "banner", "cli", "topology", "provenance", "deck_reader",
                        "dates", "introspect"))
  ctx <- dv$prevcarga_ctx(list(`--origin` = "2026-10-01"), pc_root(), NULL)
  d <- dv$prevcarga_build_describe(ctx, model = "lgbm", data_lag_days = c(lgbm = 2L))
  r <- d$cuts$members[[1L]]
  expect_identical(r$last_data_day, "2026-09-28"); expect_identical(r$train_origin, "2026-09-29")
  expect_identical(r$data_lag_source, "descriptor")
})
