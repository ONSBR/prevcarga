## test-hygiene-clima.R -- gate [PLUGINS] higiene_clima do Holt-Winters univariado (F5.3): so le codigo, nada ingere, treina ou
## infere. Fica fora de test-hygiene.R porque o higiene_env daquele arquivo infere o HW. O grep usa pc_grep_code /
## pc_grep_time_arith da fundacao (helper-plugins.R), sem grep local.

if (!exists("pc_grep_code", envir = hw_lib, inherits = FALSE))
  sys.source(file.path(hw_root(), "scripts", "lib", "tests", "helper-plugins.R"), envir = hw_lib)

HG_WEATHER <- c("TempPre", "CDDPre", "weather_absent", "efeitoTemp", "weather_forecast", "weather_observed")
hc_files <- function(rdir) sort(list.files(rdir, pattern = "[.]R$", full.names = TRUE), method = "radix")
hc_code <- function(rdir, p) {
  h <- hw_lib$pc_grep_code(hc_files(rdir), stats::setNames(p, p))
  h$file <- basename(h$file); h
}
hc_fmt <- function(h) paste0(h$file, ":", h$line, " [", h$pattern, "]", collapse = "; ")
hc_rdir <- function() file.path(hw_root(), "models", "holt-winters", "R")

test_that("[PLUGINS] higiene_clima: zero achados de clima e de aritmetica de hora em models/holt-winters/R (FORBIDDEN)", {
  for (p in HG_WEATHER) { h <- hc_code(hc_rdir(), p); expect_identical(nrow(h), 0L, info = paste(p, hc_fmt(h))) }
  a <- hw_lib$pc_grep_time_arith(file.path(hw_root(), "models", "holt-winters"), allow = list())
  expect_identical(nrow(a), 0L, info = hc_fmt(a))
})

test_that("[PLUGINS] higiene_clima: padrao plantado numa copia e apanhado na linha; comentario nao conta (CONTROL)", {
  d <- withr::local_tempdir(); dir.create(file.path(d, "R"))
  file.copy(list.files(hc_rdir(), full.names = TRUE), file.path(d, "R"))
  f <- file.path(d, "R", "zz_bait.R")
  for (p in HG_WEATHER) {
    writeLines(c(sprintf("# %s so no comentario", p), sprintf("x <- %s", p)), f)
    h <- hc_code(file.path(d, "R"), p)
    expect_identical(h$line[h$file == basename(f)], 2L, info = p)
    expect_identical(nrow(h), 1L, info = p)
  }
})

hc_gate_table <- function() {
  ex <- parse(file.path(hw_root(), "models", "holt-winters", "tests", "run_all_gates.R"), keep.source = FALSE)
  env <- new.env(parent = baseenv())
  for (e in ex) if (is.call(e) && identical(as.character(e[[1L]]), "<-") &&
                    as.character(e[[2L]])[[1L]] %in% c(".hw_gates", "gate_table")) eval(e, env)
  env$gate_table
}

test_that("[PLUGINS] higiene_clima: tabela de gates sem os gates de clima antigos (runner nao executado)", {
  gt <- hc_gate_table()
  novos <- c("clima_cego", "ingest_calendario", "infer_sem_clima", "thaw_contract", "catalogo_hw", "higiene_clima")
  expect_true(all(novos %in% gt$gate[gt$tier == "fast" & gt$owner == "PLUGINS"]))
  expect_false(any(c("ingest_weather", "efeito_temp", "infer_clima", "clima_honesto", "clima_fallback") %in% gt$gate))
  tf <- list.files(file.path(hw_root(), "models", "holt-winters", "tests", "testthat"), "^test-.*[.]R$", full.names = TRUE)
  src <- unlist(lapply(tf, readLines, warn = FALSE))
  for (g in gt$gate[gt$owner == "PLUGINS"])
    expect_true(any(grepl(sprintf("[PLUGINS] %s:", g), src, fixed = TRUE)), info = g)
})
