## test-hygiene.R -- gates [PLUGINS] higiene_env e higiene_grep do Holt-Winters (HW.7; AC 13; gate05 do oraculo). O ambiente
## sujo (EXOGENA/PREVCARGA_*) so existe dentro do teste (withr::with_envvar); o grep usa pc_grep_code / pc_grep_time_arith da
## fundacao (P.0j, helper-plugins.R), sem grep local.

if (!exists("pc_grep_code", envir = hw_lib, inherits = FALSE))
  sys.source(file.path(hw_root(), "scripts", "lib", "tests", "helper-plugins.R"), envir = hw_lib)
hg_code <- function(...) hw_lib$pc_grep_code(...)
hg_arith <- function(...) hw_lib$pc_grep_time_arith(...)

test_that("[PLUGINS] higiene_env: EXOGENA/PREVCARGA_* sujos => saida identical ao ambiente limpo; formals sem params", {
  st <- hv_store("09:00"); cuts <- hv_cuts(st); env <- hv_env()
  art <- hv_train(hv_panel(st, cuts, "train"), cuts); pay <- art$payload
  ipan <- hv_panel(st, cuts, "infer")
  run <- function() {
    deck <- suppressWarnings(env$plugin_ingest(ipan, cuts))
    inf <- suppressWarnings(env$plugin_inference_features(deck, pay, cuts))
    list(inf = inf, out = suppressWarnings(env$plugin_infer(pay, inf, cuts)), disp = hv_infer(art, ipan, cuts))
  }
  clean <- run()
  dirty <- withr::with_envvar(c(EXOGENA = "MAX", PREVCARGA_DATA_DIR = "/nada", PREVCARGA_ARTIFACT_DIR = "/nada"), {
    expect_identical(Sys.getenv("EXOGENA"), "MAX")
    run()
  })
  expect_identical(Sys.getenv("EXOGENA"), "")
  expect_identical(dirty$inf, clean$inf)
  expect_identical(dirty$out, clean$out)
  expect_identical(dirty$disp, clean$disp)
  ## nem o treino le o ambiente: mesmo sha256 com o ambiente sujo
  sha_dirty <- withr::with_envvar(c(EXOGENA = "MAX", PREVCARGA_DATA_DIR = "/nada"),
                                  hv_sha(hv_train(hv_panel(st, cuts, "train"), cuts)))
  expect_identical(sha_dirty, hv_sha(art))
  expect_false("params" %in% names(formals(env$plugin_infer)))
  expect_false("params" %in% names(formals(env$plugin_inference_features)))
})

HG_FORBIDDEN <- list(
  getenv = "Sys[.]getenv", systime = "Sys[.]time", sysdate = "Sys[.]Date", posixct = "as[.]POSIXct", strptime = "strptime",
  asdate = "as[.]Date[(]", fmt_j = "format[(].*%j", s3600 = "3600", saverds = "saveRDS", system = "system[(]",
  mclapply = "mclapply", superass = "<<-", to_model_frame = "to_model_frame", to_display_frame = "to_display_frame",
  prevcarga_env = "PREVCARGA_[A-Z_]+")

HG_PLANT <- c(getenv = "Sys.getenv('A')", systime = "Sys.time()", sysdate = "Sys.Date()", posixct = "as.POSIXct(x)",
              strptime = "strptime(a, b)", asdate = "as.Date(x)", fmt_j = "format(d, '%j')", s3600 = "x + 3600",
              saverds = "saveRDS(x, f)", system = "system('ls')", mclapply = "mclapply(x, f)", superass = "a <<- 1",
              to_model_frame = "to_model_frame(x, 60L)", to_display_frame = "to_display_frame(x, 60L)",
              prevcarga_env = "PREVCARGA_DATA_DIR")

## Aritmetica de hora: ZERO excecoes (allow vazio). O flag de horario de verao monta o instante 06:00 UTC com make_datetime e le o
## offset com format(tz =), sem somar horas nem force_tz/with_tz.
hg_arith_residual <- function(model_dir) hg_arith(model_dir, allow = list())

test_that("[PLUGINS] higiene_grep: zero hits dos padroes proibidos e da aritmetica de hora em models/holt-winters/R; CONTROL plantado em copia", {
  rdir <- file.path(hw_root(), "models", "holt-winters", "R")
  fs <- sort(list.files(rdir, pattern = "[.]R$", full.names = TRUE), method = "radix")
  fmt <- function(h) paste0(basename(h$file), ":", h$line, " [", h$pattern, "]", collapse = "; ")
  h <- hg_code(fs, HG_FORBIDDEN)
  expect_identical(nrow(h), 0L, info = fmt(h))
  r <- hg_arith_residual(file.path(hw_root(), "models", "holt-winters"))
  expect_identical(nrow(r), 0L, info = fmt(r))
  ## CONTROL: cada padrao plantado numa copia e apanhado (a linha so-comentario nao conta)
  d <- withr::local_tempdir(); dir.create(file.path(d, "R"))
  f <- file.path(d, "R", "plant.R")
  writeLines(c(unname(HG_PLANT), "# Sys.getenv('X')"), f)
  hp <- hg_code(f, HG_FORBIDDEN)
  for (p in names(HG_PLANT)) expect_true(match(p, names(HG_PLANT)) %in% hp[pattern == p, line], info = p)
  expect_false((length(HG_PLANT) + 1L) %in% hp$line)
  expect_setequal(names(HG_PLANT), names(HG_FORBIDDEN))

  ## CONTROL da aritmetica de hora: copia do plugin limpa => 0; `+ hours(1)`, force_tz e with_tz plantados => apanhados
  cp <- file.path(withr::local_tempdir(), "holt-winters"); dir.create(cp)
  file.copy(rdir, cp, recursive = TRUE)
  expect_identical(nrow(hg_arith_residual(cp)), 0L)
  ing <- file.path(cp, "R", "ingestion.R"); txt <- readLines(ing)
  for (bad in c(".hw_plant <- function(x) x + lubridate::hours(1)", ".hw_plant <- function(x) lubridate::with_tz(x, 'UTC')",
                ".hw_plant <- function(x) lubridate::force_tz(x, 'UTC')")) {
    writeLines(c(txt, bad), ing)
    expect_identical(unique(hg_arith_residual(cp)$line), length(txt) + 1L, info = bad)
  }
  writeLines(txt, ing)
  other <- file.path(cp, "R", "training.R")
  writeLines(c(readLines(other), "z <- y - lubridate::minutes(30)"), other)
  expect_true(any(basename(hg_arith_residual(cp)$file) == "training.R"))
})
