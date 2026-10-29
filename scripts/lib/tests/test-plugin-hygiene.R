## test-plugin-hygiene.R -- higiene da fundacao de plugins (P.0j). Usa pc_grep_code & cia. de helper-plugins.R.
## Tudo em tempdir; nunca le <repo>/data/. A suite completa roda num Rscript --vanilla separado.

.ph_root <- function() pc_root()
.ph_files <- function() {
  r <- .ph_root()
  file.path(r, c("scripts/lib/features.R", "scripts/lib/registry.R",
                 file.path("scripts/tools", c("fixture_store.R", "plugin_golden.R", "plugin_parity.R", "plugin_gates.R"))))
}
.ph_fmt <- function(h) paste0(basename(h$file), ":", h$line, " [", h$pattern, "]", collapse = "\n")

test_that("[ac-0] codigo da fundacao limpo (zero hits)", {
  files <- .ph_files()
  hits <- pc_grep_code(files, pc_foundation_forbidden)
  expect_equal(nrow(hits), 0L, info = .ph_fmt(hits))
  hi <- pc_bytes_over_127(files)
  expect_equal(nrow(hi), 0L, info = .ph_fmt(hi))
})

test_that("[ac-0][CONTROL] padrao plantado em cada copia e acusado com arquivo:linha; linha so-comentario nao conta", {
  files <- .ph_files()
  d <- withr::local_tempdir()
  tmp <- file.path(d, basename(files)); file.copy(files, tmp, overwrite = TRUE)
  n0 <- vapply(tmp, function(f) length(readLines(f, warn = FALSE)), integer(1L))
  for (f in tmp) cat("x <- as.POSIXct('2026-10-01')\n# Sys.getenv('X')\n", file = f, append = TRUE)
  hits <- pc_grep_code(tmp, list(base_dt = pc_foundation_forbidden$base_dt, env = pc_foundation_forbidden$env))
  expect_setequal(unique(hits$file), tmp)
  expect_false(any(grepl("Sys.getenv", hits$text)))
  for (i in seq_along(tmp)) expect_true((n0[[i]] + 1L) %in% hits[file == tmp[[i]] & pattern == "base_dt", line])
  ## cada padrao da tabela e acusado
  planted <- c(env = "Sys.getenv('X')", clock = "Sys.time()", base_dt = "strptime(a, b)", setwd = "setwd(d)",
               superass = "a <<- 1", system = "system('ls')", fork = "mclapply(x, f)", global = "library(x)",
               byname = "switch(model, a = 1)", upper = "PREVCARGA_FOO")
  expect_setequal(names(planted), names(pc_foundation_forbidden))
  f <- file.path(d, "plant.R"); writeLines(unname(planted), f)
  h <- pc_grep_code(f, pc_foundation_forbidden)
  for (p in names(planted)) expect_true(match(p, names(planted)) %in% h[pattern == p, line], info = p)
  ## system2 e permitido
  writeLines("system2('ls', c('-l'))", f)
  expect_equal(nrow(pc_grep_code(f, pc_foundation_forbidden["system"])), 0L)
  ## byte > 127 em mensagem
  writeLines(c("x <- 'aç'", "prevcarga_abort('aç')"), f)
  expect_equal(pc_bytes_over_127(f)$line, 2L)
})

.ph_toys <- function() file.path(pc_toy_root(), c("toy-30", "toy-60", "_toy-engine"))

test_that("[ac-3] brinquedos sem aritmetica de hora (inclusive toy-60 com offset 60)", {
  for (m in .ph_toys()) {
    h <- if (basename(m) == "_toy-engine") pc_grep_time_arith(m) else pc_grep_time_arith(m, engine_dir = file.path(pc_toy_root(), "_toy-engine"))
    expect_equal(nrow(h), 0L, info = paste(m, .ph_fmt(h)))
  }
  expect_match(paste(readLines(file.path(pc_toy_root(), "toy-60", "R", "plugin.R")), collapse = "\n"), "model_time_offset_min = 60L", fixed = TRUE)
})

test_that("[ac-3][CONTROL] padroes plantados acusados; excecao do HW versionada; comentario e to_hourly nao contam", {
  d <- withr::local_tempdir(); dir.create(file.path(d, "m", "R"), recursive = TRUE)
  f <- file.path(d, "m", "R", "plugin.R")
  writeLines(c("a <- x %m-% hours(1)", "b <- x + 3600L", "c <- to_model_frame(x, 60L)", "# d <- x + hours(1)",
               "e <- to_hourly(x)", "g <- to_semihourly(x)", "h <- x + hours(6)"), f)
  h <- pc_grep_time_arith(file.path(d, "m"))
  expect_true(all(c(1L, 2L, 3L, 7L) %in% h$line))
  expect_false(any(h$line %in% c(4L, 5L, 6L)))
  ## excecao versionada: so no arquivo do HW
  dir.create(file.path(d, "hw", "R"), recursive = TRUE)
  writeLines("dst <- x + hours(6)", file.path(d, "hw", "R", "holt_winters.R"))
  expect_equal(nrow(pc_grep_time_arith(file.path(d, "hw"))), 0L)
  ## CONTROL: sem a excecao o +hours(6) e acusado, com arquivo:linha
  h2 <- pc_grep_time_arith(file.path(d, "hw"), allow = list())
  expect_equal(unique(h2$line), 1L); expect_equal(unique(basename(h2$file)), "holt_winters.R")
  ## fora do arquivo da excecao continua acusado
  expect_true(7L %in% h[basename(file) == "plugin.R", line])
  ## a lista de excecoes e fechada: so HW (DST) e o lag de feature do motor DESSEM
  expect_setequal(vapply(strsplit(names(pc_time_arith_allow()), " "), `[`, "", 1L), c("holt_winters.R", "dessem_engine.R"))
})

test_that("[ac-1] suite da fundacao inteira em Rscript --vanilla: zero falha/skip/warning; sem models/ nem data/ reais", {
  r <- .ph_root(); td <- file.path(r, "scripts", "lib", "tests")
  ## hygiene (este arquivo) fica de fora: o filho nao pode se rodar recursivamente.
  filt <- "^(features|features-facade|plugin-loader|plugin-dispatch|plugin-artifact|plugin-dispatch-honesty|plugin-tools-fixture|plugin-tools-golden|plugin-tools-parity|plugin-tools-gates)$"
  script <- tempfile("suite-", fileext = ".R")
  writeLines(c(
    sprintf("setwd('%s')", r),
    "seed0 <- if (exists('.Random.seed', globalenv())) get('.Random.seed', globalenv()) else NULL",
    "g0 <- ls(globalenv(), all.names = TRUE)",
    sprintf("res <- as.data.frame(testthat::test_dir('%s', filter = '%s', reporter = 'silent', stop_on_failure = FALSE, stop_on_warning = FALSE))", td, filt),
    "seed1 <- if (exists('.Random.seed', globalenv())) get('.Random.seed', globalenv()) else NULL",
    "g1 <- ls(globalenv(), all.names = TRUE)",
    "cat(sprintf('RESULT n=%d failed=%d skipped=%d warning=%d error=%d',",
    "  nrow(res), sum(res$failed), sum(res$skipped), sum(res$warning), sum(res$error)), '\\n')",
    "cat('SEED', identical(seed0, seed1), '\\n'); cat('GLOBAL', identical(setdiff(g0, '.Random.seed'), setdiff(g1, c('seed1', 'g1', 'res', 'g0', '.Random.seed'))), '\\n')"), script)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)), stdout = TRUE, stderr = FALSE))
  expect_match(grep("^RESULT", out, value = TRUE), "failed=0 skipped=0 warning=0 error=0")
  expect_false(grepl("n=0 ", grep("^RESULT", out, value = TRUE)))
  expect_true(any(grepl("^SEED TRUE", out))); expect_true(any(grepl("^GLOBAL TRUE", out)))
  ## nenhum teste da fundacao le <repo>/models ou <repo>/data reais
  tf <- file.path(td, paste0("test-", c("features", "features-facade", "plugin-loader", "plugin-dispatch", "plugin-artifact",
                  "plugin-dispatch-honesty", "plugin-tools-fixture", "plugin-tools-golden", "plugin-tools-parity",
                  "plugin-tools-gates", "plugin-hygiene"), ".R"))
  pat <- list(real_models = "file[.]path[(]pc_root[(][)],[^)]*\"models\"",
              real_data = "file[.]path[(]pc_root[(][)],[^)]*\"data\"")
  h <- pc_grep_code(tf, pat)
  expect_equal(nrow(h), 0L, info = .ph_fmt(h))
})

test_that("[ac-2] write-through: contratos vivos em docs/estado/plugins-fundacao.md e /.state/ no .gitignore", {
  r <- .ph_root()
  est <- paste(readLines(file.path(r, "docs", "estado", "plugins-fundacao.md"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  tokens <- c("data_lag_days", "model_time_offset_min", "delivery_days", "reads_day_O", "weather_forecast_in_train", "uses_rng",
              "history_infer_equals_train", "rng_deviation", "prevcarga_plugin_signatures", "cuts", "last_data_day = O-1-data_lag_days",
              "to_model_frame", "to_display_frame", "pc_grep_time_arith", "prevcarga_delivery_grid", "available_until + 30", "O..O+8", "O..O+1",
              "units.yaml", "oracle_env", "oracle_cut", "day0_source", "golden.lock", "prevcarga_gates_run", "scripts/tools/gates/<dono>.R",
              "applies_to", "plugin.*", "scripts/lib/errors/plugin.tsv", "Modulos e ferramentas", "features.R", "registry.R",
              "fixture_store.R", "plugin_golden.R", "plugin_parity.R", "plugin_gates.R")
  miss <- function(txt) tokens[!vapply(tokens, grepl, logical(1L), x = txt, fixed = TRUE)]
  expect_identical(miss(est), character(), info = paste(miss(est), collapse = ", "))
  ## CONTROL: um token removido do texto reprova
  expect_identical(miss(gsub("uses_rng", "", est, fixed = TRUE)), "uses_rng")
  gi <- readLines(file.path(r, ".gitignore"), warn = FALSE)
  expect_true("/.state/" %in% trimws(gi))
})
