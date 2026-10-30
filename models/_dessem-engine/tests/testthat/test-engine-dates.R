## test-engine-dates.R -- DL.3/DL.4: datas do motor em lubridate com tz explicito e sem aritmetica de hora (AC1..AC5).
## O grep do codigo e o da FUNDACAO (P.0j, scripts/lib/tests/helper-plugins.R): `pc_grep_code()` (padroes proibidos) e
## `pc_grep_time_arith()` (aritmetica de hora) com a lista de excecoes VAZIA: o motor nao tem excecao (lag por indice, DL.3).

if (!exists("pc_grep_code", envir = eng_lib, inherits = FALSE))
  sys.source(file.path(eng_root(), "scripts", "lib", "tests", "helper-plugins.R"), envir = eng_lib)

## Padroes proibidos no motor (codigo, sem comentarios; mesma tabela do ticket + as chamadas de hora)
ENG_FORBIDDEN <- list(
  base_date = "as[.]Date[(]", base_posix = "as[.]POSIXct", strptime = "strptime", difftime = "difftime",
  clock = "Sys[.]time", clock_d = "Sys[.]Date", env = "Sys[.]getenv", system = "(^|[^2a-z_])system2?[(]",
  fork = "mclapply", superassign = "<<-", globalenv = "globalenv[(][)]",
  lit_1h = "hours[(]1?[)]", hours_any = "hours[(]", minutes_30 = "minutes[(]", lit_3600 = "(?<![0-9.])3600L?(?![0-9])", lit_86400 = "(?<![0-9.])86400L?(?![0-9])",
  frames = "to_model_frame[(]|to_display_frame[(]")

eng_fmt_hits <- function(h) paste0(basename(h$file), ":", h$line, " [", h$pattern, "]", collapse = "; ")

test_that("ac-1 motor sem padroes de data/hora proibidos (pc_grep_code + pc_grep_time_arith com allow vazio); sem bytes > 127", {
  files <- sort(list.files(file.path(eng_dir(), "R"), pattern = "[.]R$", full.names = TRUE), method = "radix")
  expect_gt(length(files), 9L)
  ## UNICA divida pinada: `globalenv()` em engine_abort (engine.R) -- busca interina de `prevcarga_abort` da facade, a mesma
  ## do `dl_abort` do plugin (discovery do DL.4). Qualquer outro hit, ou um 2o uso de globalenv(), reprova.
  h <- eng_lib$pc_grep_code(files, ENG_FORBIDDEN)
  expect_identical(paste0(basename(h$file), ":", h$line, " ", h$pattern), "engine.R:12 globalenv", info = eng_fmt_hits(h))
  h2 <- eng_lib$pc_grep_code(files, eng_lib$pc_foundation_forbidden_tools)
  expect_identical(nrow(h2), 0L, info = eng_fmt_hits(h2))
  ## aritmetica de hora: SEM excecao (allow vazio); a lista versionada da fundacao tambem nao tem entrada do motor
  r <- eng_lib$pc_grep_time_arith(eng_dir(), allow = list())
  expect_identical(nrow(r), 0L, info = eng_fmt_hits(r))
  expect_false(any(grepl("dessem|engine", names(eng_lib$pc_time_arith_allow()))))
  expect_identical(nrow(eng_lib$pc_grep_time_arith(eng_dir())), 0L)
  ## bytes > 127: em mensagens (helper da fundacao) e em qualquer linha do motor (ASCII puro)
  expect_identical(nrow(eng_lib$pc_bytes_over_127(files)), 0L)
  hi <- unlist(lapply(files, function(f) {
    ln <- readLines(f, warn = FALSE, encoding = "bytes")
    i <- which(vapply(ln, function(l) any(as.integer(charToRaw(l)) > 127L), NA))
    if (length(i)) paste0(basename(f), ":", i) else character()
  }))
  expect_length(hi, 0L)
})

test_that("ac-1 CONTROL: diretorio-fixture com um arquivo por padrao proibido; cada um acusado (inclusive DataHora %m-% hours(1))", {
  d <- withr::local_tempdir(); dir.create(file.path(d, "m", "R"), recursive = TRUE)
  plant <- c(base_date = "a <- as.Date(x)", base_posix = "b <- as.POSIXct(x)", strptime = "c <- strptime(x, 'f')",
             difftime = "d <- difftime(a, b)", clock = "e <- Sys.time()", clock_d = "f <- Sys.Date()", env = "g <- Sys.getenv('X')",
             system = "system('ls')", fork = "r <- parallel::mclapply(x, f)", superassign = "n <<- n + 1L",
             globalenv = "e <- globalenv()", lit_1h = "h <- DataHora %m-% hours(1)", hours_any = "i <- DataHora %m+% hours(lagh)",
             minutes_30 = "j <- DataHora %m-% minutes(30)", lit_3600 = "k <- DataHora + 3600L", lit_86400 = "l <- x * 86400",
             frames = "m <- to_model_frame(x, 60L)")
  fx <- character()
  for (nm in names(plant)) {
    f <- file.path(d, "m", "R", paste0("fx_", nm, ".R")); writeLines(c("# nada aqui", plant[[nm]], "# a <- hours(1)"), f); fx <- c(fx, f)
  }
  h <- eng_lib$pc_grep_code(fx, ENG_FORBIDDEN)
  for (nm in names(plant)) {
    got <- h[basename(file) == paste0("fx_", nm, ".R")]
    expect_true(nm %in% got$pattern, info = nm)
    expect_true(all(got$line == 2L), info = nm)        # a linha so-comentario nunca conta
  }
  ## pc_grep_time_arith (allow vazio) acusa o `DataHora %m-% hours(1)` e o lag `hours(lagh)` do antigo obtemLagHora
  a <- eng_lib$pc_grep_time_arith(file.path(d, "m"), allow = list())
  for (nm in c("lit_1h", "hours_any", "minutes_30", "lit_3600", "frames"))
    expect_true(nrow(a[basename(file) == paste0("fx_", nm, ".R")]) > 0L, info = nm)
  expect_true("period_arith" %in% a[basename(file) == "fx_lit_1h.R"]$pattern)
  ## bytes > 127 em mensagem
  f <- file.path(d, "m", "R", "fx_bytes.R")
  writeBin(c(charToRaw("stop('a"), as.raw(c(0xC3, 0xA7)), charToRaw("')\n")), f)
  expect_identical(eng_lib$pc_bytes_over_127(f)$line, 1L)
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

test_that("lag de feature por indice na grade regular: LAG_k = valor k linhas antes; slot faltante aborta; CONTROL sem lacuna segue", {
  eng <- eng_env(eng_load(eng_plugin_root()))
  assign("engine_abort", function(id, where = NULL, details = NULL) eng_lib$prevcarga_abort(id, where = where, details = details),
         envir = eng)
  t0 <- lubridate::force_tz(lubridate::ymd_hms("2019-01-01 00:00:00"), "Etc/GMT+3")
  n <- 72L
  ser <- data.table::data.table(DataHora = seq(t0, by = "hour", length.out = n), Carga = as.numeric(seq_len(n)^2))
  ser[, `:=`(Data = lubridate::as_date(DataHora), HV = 0, DiaEspecial = 0)]
  out <- eng$obtemLagHora(3L, data.table::copy(ser), hv = 0L)
  expect_identical(out$LAG[[1L]], "LAG_3")
  expect_identical(out$VALLAG, ser$Carga[match(out$DataHora, ser$DataHora) - 3L])
  expect_identical(nrow(out), n - 4L)           # estrito: as lagh + 1 primeiras linhas saem (como o oraculo)
  ## CONTROL: um slot removido => aborto com id novo, sem pegar vizinho
  gap <- data.table::copy(ser)[-30L]
  cond <- tryCatch(eng$obtemLagHora(3L, gap, hv = 0L), error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "engine.dessem_lag_grid_gap")
  expect_identical(cond$class_name, "contract")
  expect_identical(cond$details$row, 30L)
})
