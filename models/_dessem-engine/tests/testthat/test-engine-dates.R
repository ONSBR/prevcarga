## test-engine-dates.R -- DL.3: datas do motor em lubridate com tz explicito e sem aritmetica de hora (AC1..AC5).
## Ate o P.0j entregar pc_grep_code()/pc_grep_time_arith(), o grep local abaixo aplica os mesmos padroes do AC1.

## Padroes proibidos no motor (codigo, sem comentarios). A unica excecao versionada e o lag de FEATURE
## (`hours(lagh)` de obtemLagHora; `minutes(lagsh * 30)` de obtemLagSemiHora): periodo variavel vindo do config.
ENG_FORBIDDEN <- c("as\\.Date\\(", "as\\.POSIXct", "strptime", "difftime", "Sys\\.time", "Sys\\.Date", "Sys\\.getenv",
                   "hours\\(1\\)", "hours\\(\\)", "minutes\\(30\\)", "\\b3600\\b", "\\b86400\\b",
                   "to_model_frame\\(", "to_display_frame\\(")

## Devolve "arquivo:linha" de cada ocorrencia (comentarios descartados) + linhas com byte > 127.
eng_grep_time <- function(files) {
  hits <- character()
  for (f in files) {
    ln <- readLines(f, warn = FALSE, encoding = "bytes")
    code <- sub("#.*$", "", ln)
    for (pt in ENG_FORBIDDEN) for (i in grep(pt, code, perl = TRUE)) hits <- c(hits, sprintf("%s:%d", basename(f), i))
    for (i in which(vapply(ln, function(l) any(utf8ToInt(iconv(l, "latin1", "latin1")) > 127L), NA)))
      hits <- c(hits, sprintf("%s:%d(nao-ASCII)", basename(f), i))
  }
  hits
}

test_that("ac-1 motor sem padroes de data/hora proibidos; CONTROL: fixture com cada padrao e acusado linha a linha", {
  files <- list.files(file.path(eng_dir(), "R"), pattern = "\\.R$", full.names = TRUE)
  expect_identical(eng_grep_time(files), character())
  ## o lag de feature e o unico periodo que fica
  src <- paste(readLines(file.path(eng_dir(), "R", "funcoes_apoio.R")), collapse = "\n")
  expect_match(src, "hours(lagh)", fixed = TRUE)
  fx <- tempfile(fileext = ".R")
  writeLines(c("a <- as.Date(x)", "b <- as.POSIXct(x)", "c <- strptime(x, 'f')", "d <- difftime(a, b)", "e <- Sys.time()",
               "f <- Sys.Date()", "g <- Sys.getenv('X')", "h <- DataHora %m-% hours(1)", "i <- DataHora %m+% hours()",
               "j <- DataHora %m-% minutes(30)", "k <- x / 3600", "l <- x * 86400", "m <- to_model_frame(x, 60L)",
               "n <- to_display_frame(x, 60L)", "o <- 'acentuação'", "p <- ok(1)"), fx, useBytes = FALSE)
  hit <- eng_grep_time(fx)
  expect_true(all(sprintf("%s:%d", basename(fx), 1:14) %in% hit))
  expect_true(any(grepl(":15(nao-ASCII)", hit, fixed = TRUE)))
  expect_false(any(grepl(":16", hit, fixed = TRUE)))
})

test_that("ac-2 do ticket: tzone de CARGAHIST diferente de cuts$tz => [prevcarga/contract] nomeando coluna e fusos; CONTROL segue", {
  eng <- eng_env(eng_load(eng_plugin_root()))
  deck <- eng_model_deck(eng_deck())
  bad <- deck
  bad$CARGAHIST <- data.table::copy(deck$CARGAHIST)
  data.table::set(bad$CARGAHIST, j = "DataHora", value = lubridate::with_tz(bad$CARGAHIST$DataHora, "UTC"))
  cond <- tryCatch(eng$TransformaDeck(bad, "2019-02-28", "10DIAS", tz = "Etc/GMT+3"), error = function(e) e)
  expect_s3_class(cond, "error")
  msg <- conditionMessage(cond)
  expect_match(msg, "engine.dessem_tz_mismatch", fixed = TRUE)
  ## sem a fundacao no globalenv o aborto cai em stop() com o id; com ela, details nomeiam coluna e fusos
  lib <- eng_lib
  fa <- function(id, where = NULL, details = NULL) lib$prevcarga_abort(id, where = where, details = details)
  assign("engine_abort", fa, envir = eng)
  cond <- tryCatch(eng$TransformaDeck(bad, "2019-02-28", "10DIAS", tz = "Etc/GMT+3"), error = function(e) e)
  expect_identical(cond$id, "engine.dessem_tz_mismatch")
  expect_identical(cond$class_name, "contract"); expect_identical(cond$exit_code, 2L)
  expect_identical(cond$details$column, "CARGAHIST$DataHora")
  expect_identical(c(cond$details$tz_found, cond$details$tz_expected), c("UTC", "Etc/GMT+3"))
  ok <- suppressMessages(eng$TransformaDeck(deck, "2019-02-28", "10DIAS", tz = "Etc/GMT+3"))
  expect_true(data.table::is.data.table(ok$SERIECOMP))
})

test_that("ac-3 do ticket: HVZ por make_datetime(12h, America/Sao_Paulo) identico ao do oraculo em 2015..2025", {
  eng <- eng_env(eng_load(eng_plugin_root()))
  dias <- seq(lubridate::ymd("2015-01-01"), lubridate::ymd("2025-12-31"), by = "day")
  oraculo <- format(as.POSIXct(format(dias, "%F 12:00:00"), "America/Sao_Paulo"), "%Z")
  expect_identical(eng$.hvz_flag(dias), oraculo)
  dst <- eng$.hvz_flag(lubridate::ymd(c("2018-11-03", "2018-11-04", "2019-02-16", "2019-02-17")))
  expect_identical(dst, c("-03", "-02", "-02", "-03"))
  expect_identical(oraculo[dias == lubridate::ymd("2018-11-04")], "-02")
})

test_that("ac-4 do ticket: mensagens ASCII sem carimbo de hora e saida identica com o relogio mascarado", {
  eng <- eng_env(eng_load(eng_plugin_root()))
  dk <- suppressMessages(eng$TransformaDeck(eng_model_deck(eng_deck()), "2019-02-28", "10DIAS", tz = "Etc/GMT+3"))
  cfg <- eng$getConfigModel("Linear.TmpMedia")
  run <- function() {
    msgs <- character()
    set.seed(42L)
    out <- withCallingHandlers(eng$processaModelosDMPerfil(dk, cfg),
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
    list(out = out, msgs = msgs)
  }
  a <- run()
  assign("Sys.time", function() as.POSIXct("2001-01-01 00:00:00", tz = "UTC"), envir = eng)
  b <- run()
  expect_identical(a$out, b$out)
  expect_true(length(a$msgs) > 0L)
  expect_true(all(vapply(a$msgs, function(m) all(utf8ToInt(m) <= 127L), NA)))
  expect_false(any(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", a$msgs)))
  expect_identical(a$msgs, b$msgs)
})
