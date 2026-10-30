## test-no-env.R -- RD.5 (AC13 e AC15 do epico, `higiene`): o plugin nao le variavel de ambiente (config = flags > config.yaml >
## default) e o codigo de `models/dessem-regdin/R` passa o grep da fundacao. O grep e o da FUNDACAO (P.0j,
## scripts/lib/tests/helper-plugins.R): `pc_grep_code()`, `pc_bytes_over_127()` e `pc_grep_time_arith()` (zero excecoes: o
## +-1 h e SO do despachante, por `model_time_offset_min: 60L`). CONTROL: uma copia em tempdir com cada padrao plantado
## (inclusive `DataHora + 3600L` na ingestao) e acusada com arquivo:linha. Ambiente sujo (EXOGENA=MAX) => saida identical()
## a do ambiente limpo (o env sujo so existe dentro do teste, por withr::with_envvar).

if (!exists("pc_grep_code", envir = rd_lib, inherits = FALSE))
  sys.source(file.path(rd_root(), "scripts", "lib", "tests", "helper-plugins.R"), envir = rd_lib)

RD5_FORBIDDEN <- c(
  getenv = "Sys[.]getenv", systime = "Sys[.]time", sysdate = "Sys[.]Date", posixct = "as[.]POSIXct", strptime = "strptime",
  asdate = "as[.]Date[(]", difftime = "difftime", superass = "<<-", saverds = "saveRDS", system = "(^|[^2a-z_])system[(]",
  mclapply = "mclapply", s3600 = "(?<![0-9.])3600L?(?![0-9])", hours1 = "hours[(]1[)]", hours0 = "hours[(][)]", minutes30 = "minutes[(]30[)]",
  frame_fns = "to_model_frame[(]|to_display_frame[(]")

RD5_PLANT <- c(
  getenv = "v <- Sys.getenv('EXOGENA')", systime = "t0 <- Sys.time()", sysdate = "d0 <- Sys.Date()", posixct = "t1 <- as.POSIXct(x)",
  strptime = "t2 <- strptime(x, '%H')", asdate = "d1 <- as.Date(x)", difftime = "d2 <- difftime(a, b)", superass = "cnt <<- cnt + 1L",
  saverds = "saveRDS(x, 'a.rds')", system = "system('ls')", mclapply = "r <- parallel::mclapply(x, f)",
  s3600 = "ld[, DataHora := DataHora + 3600L]", hours1 = "t3 <- x + lubridate::hours(1)", hours0 = "t4 <- x + hours()",
  minutes30 = "t5 <- x - lubridate::minutes(30)", frame_fns = "m <- to_model_frame(x, 60L)")

rd5_r_files <- function(dir) sort(list.files(dir, pattern = "[.]R$", full.names = TRUE), method = "radix")
rd5_fmt_hits <- function(h) paste0(basename(h$file), ":", h$line, " [", h$pattern, "]", collapse = "; ")

test_that("[PLUGINS] higiene: zero hits dos padroes proibidos, de bytes > 127 em mensagens e de aritmetica de hora em models/dessem-regdin/R", {
  rdir <- file.path(rd_models(), "dessem-regdin", "R")
  fs <- rd5_r_files(rdir)
  expect_gt(length(fs), 7L)
  h <- rd_lib$pc_grep_code(fs, RD5_FORBIDDEN)
  expect_identical(nrow(h), 0L, info = rd5_fmt_hits(h))
  b <- rd_lib$pc_bytes_over_127(fs)
  expect_identical(nrow(b), 0L, info = rd5_fmt_hits(b))
  ar <- rd_lib$pc_grep_time_arith(file.path(rd_models(), "dessem-regdin"), allow = list())   # ZERO excecoes
  expect_identical(nrow(ar), 0L, info = rd5_fmt_hits(ar))
  ## divida interina pinada: `globalenv()` so nos 3 usos conhecidos (rd_abort/rd_warn em register.R, rd_foundation em inference.R)
  g <- rd_lib$pc_grep_code(fs, c(glob = "globalenv[(][)]"))
  expect_identical(nrow(g), 3L, info = rd5_fmt_hits(g))
  expect_identical(sort(basename(g$file)), c("inference.R", "register.R", "register.R"))
})

test_that("[PLUGINS] higiene: CONTROL copia com cada padrao plantado (um por linha, arquivos em rodizio) e acusada com arquivo:linha", {
  src <- file.path(rd_models(), "dessem-regdin", "R")
  cp <- file.path(withr::local_tempdir(), "dessem-regdin"); dir.create(cp)
  file.copy(src, cp, recursive = TRUE)
  fs <- rd5_r_files(file.path(cp, "R"))
  expect_identical(nrow(rd_lib$pc_grep_code(fs, RD5_FORBIDDEN)), 0L)    # a copia limpa e limpa
  where <- list()
  for (i in seq_along(RD5_PLANT)) {
    f <- fs[[(i - 1L) %% length(fs) + 1L]]
    txt <- readLines(f, warn = FALSE)
    writeLines(c(txt, RD5_PLANT[[i]]), f)
    where[[names(RD5_PLANT)[[i]]]] <- list(file = f, line = length(txt) + 1L)
  }
  h <- rd_lib$pc_grep_code(fs, RD5_FORBIDDEN)
  for (p in names(RD5_PLANT)) {
    w <- where[[p]]
    expect_true(any(h$pattern == p & h$file == w$file & h$line == w$line), info = paste(p, basename(w$file), w$line))
  }
  ## a linha so-comentario nao conta
  f1 <- fs[[1L]]; n1 <- length(readLines(f1, warn = FALSE))
  cat("# Sys.getenv('X') as.Date(x) DataHora + 3600L\n", file = f1, append = TRUE)
  expect_false((n1 + 1L) %in% rd_lib$pc_grep_code(f1, RD5_FORBIDDEN)$line)
  ## aritmetica de hora: `DataHora + 3600L` na ingestao e `+ hours(1)` sao acusados com a linha (allow vazio)
  ing <- file.path(cp, "R", "ingestion.R"); txt <- readLines(ing, warn = FALSE)
  for (bad in c(".rd_plant <- function(ld) ld[, DataHora := DataHora + 3600L]", ".rd_plant <- function(x) x + lubridate::hours(1)",
                ".rd_plant <- function(x) lubridate::with_tz(x, 'UTC')", ".rd_plant <- function(x) lubridate::force_tz(x, 'UTC')")) {
    writeLines(c(txt, bad), ing)
    r <- rd_lib$pc_grep_time_arith(cp, allow = list())
    expect_true(any(basename(r$file) == "ingestion.R" & r$line == length(txt) + 1L), info = bad)
  }
  writeLines(txt, ing)
  ## bytes > 127 numa mensagem de erro: acusado com arquivo:linha
  tr <- file.path(cp, "R", "training.R"); ttxt <- readLines(tr, warn = FALSE)
  writeBin(charToRaw(enc2utf8(paste0(paste(c(ttxt, "stop('açao invalida')"), collapse = "\n"), "\n"))), tr)
  b <- rd_lib$pc_bytes_over_127(fs)
  expect_true(any(basename(b$file) == "training.R" & b$line == length(ttxt) + 1L))
})

test_that("[PLUGINS] higiene: EXOGENA=MAX no ambiente => infer RGD_MED identical() ao do ambiente limpo; RNG do chamador intacto; formals sem params", {
  st <- rd5_store(); cu <- rd5_cuts(st, "09:00"); art <- rd5_art("MED"); p <- rd_fx_plugin()
  pan <- rd5_panel(st, cu, "infer")
  clean <- rd5_infer(art, st, cu, panel = pan)
  set.seed(11L); r0 <- get(".Random.seed", globalenv())
  dirty <- withr::with_envvar(c(EXOGENA = "MAX", PREVCARGA_DATA_DIR = "/nada", PREVCARGA_ARTIFACT_DIR = "/nada"), {
    expect_identical(Sys.getenv("EXOGENA"), "MAX")
    rd5_infer(art, st, cu, panel = pan)
  })
  expect_identical(Sys.getenv("EXOGENA"), "")
  expect_identical(get(".Random.seed", globalenv()), r0)
  expect_identical(dirty, clean)
  expect_identical(dirty$variant, "MED")                               # a variante e a do artefato, nao a do ambiente
  expect_identical(dirty$values, clean$values)
  for (fn in c("plugin_infer", "rd_inference", "plugin_forecast", "rd_forecast", "plugin_inference_features", "rd_inference_features"))
    expect_false("params" %in% names(formals(get(fn, envir = p$env))), info = fn)
})

test_that("[PLUGINS] higiene: treino com ambiente sujo tem o mesmo sha256 e a variante do spec (nao a do ambiente)", {
  st <- rd5_store(); cu <- rd5_cuts(st, "09:00"); base <- rd5_art("MED")
  dirty <- withr::with_envvar(c(EXOGENA = "MAX", PREVCARGA_DATA_DIR = "/nada"), rd5_train(st, cu, rd5_spec("MED")))   # treino real
  expect_identical(rd5_sha(dirty), rd5_sha(base))
  expect_identical(dirty$stamp$variant, "MED")
  expect_identical(dirty, base)
})
