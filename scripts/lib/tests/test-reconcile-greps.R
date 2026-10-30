## test-reconcile-greps.R -- R.V: grep gates de FONTE do Reconcile (scripts/lib/reconcile.R e scripts/lib/cli/reconcile.R).
## Cada padrao = FORBIDDEN real (zero hits no codigo, comentarios fora) + CONTROL por injecao (copia em tempdir com 1 linha violadora
## tem de ser pega por ESSE padrao). Reusa pc_grep_code / pc_time_arith_patterns / pc_bytes_over_127 do P.0j (allow = list():
## nenhuma excecao). Windows: gates de fonte puros, rodam igual (sem skip).
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
lib_dir <- file.path(pc_root(), "scripts", "lib")
rc_files <- c(mod = file.path(lib_dir, "reconcile.R"), verb = file.path(lib_dir, "cli", "reconcile.R"))

## id -> list(regex, inject). `inject` = linha violadora usada no CONTROL.
rc_patterns <- list(
  env     = list("Sys[.](getenv|setenv)[(]",                        'x <- Sys.getenv("TZ")'),
  clock   = list("Sys[.](time|Date)[(]",                            "n <- Sys.time()"),
  base_dt = list("(^|[^_a-zA-Z.])(as[.]POSIXct|as[.]Date|strptime|difftime)[(]", "d <- as.Date(x)"),
  shift30 = list("minutes[(]\\s*30L?|ts_r30",                       "y <- x - lubridate::minutes(30)"),
  proc    = list("(^|[^2a-zA-Z_.])system2?[(]|mclapply|parallel::mc", 'r <- system("ls")'),
  symlink = list("Sys[.]symlink|file[.]symlink",                    'file.symlink("a", "b")'),
  net     = list("s3://|https?://|\\baws\\b|paws|curl::",            'u <- "https://x.org"'),
  wmape   = list("(^|\\s)\\.?(prevcarga_)?wmape\\w*\\s*(<-|=)\\s*function", "wmape_local <- function(a, b) 0"),
  clamp   = list("p(min|max)[(][^)]*\\bk\\b",                       "k <- pmin(k, 1.15)"),
  upper   = list("PREVCARGA_[A-Z_]+",                               "x <- PREVCARGA_WINDOW"),
  staging = list("[.]staging-",                                     'p <- file.path(d, ".staging-x")'),
  topo    = list("[\"'](SECO|PESE|PES|PEN|PENE)[\"']|\\b(24|4)L\\b", 'ss <- "SECO"')
)
rc_pat <- stats::setNames(vapply(rc_patterns, `[[`, "", 1L), names(rc_patterns))

bait_copy <- function(path, line, .env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = .env)
  out <- file.path(d, basename(path))
  writeLines(c(readLines(path, warn = FALSE), line), out)
  out
}

test_that("os arquivos do Reconcile existem", expect_true(all(file.exists(rc_files))))

test_that("FORBIDDEN: zero hits em reconcile.R e cli/reconcile.R para cada padrao proibido", {
  h <- pc_grep_code(unname(rc_files), rc_pat)
  expect_identical(nrow(h), 0L, info = paste(utils::capture.output(print(h)), collapse = "\n"))
})

test_that("FORBIDDEN: nenhuma aritmetica de hora (pc_time_arith_patterns, allow = list())", {
  h <- pc_grep_code(unname(rc_files), pc_time_arith_patterns(), allow = list())
  expect_identical(nrow(h), 0L, info = paste(utils::capture.output(print(h)), collapse = "\n"))
})

test_that("FORBIDDEN: nenhum byte > 127 em strings de mensagem", {
  h <- pc_bytes_over_127(unname(rc_files))
  expect_identical(nrow(h), 0L, info = paste(utils::capture.output(print(h)), collapse = "\n"))
})

test_that("CONTROL: cada padrao injetado numa copia em tempdir() reprova exatamente aquele padrao", {
  for (id in names(rc_patterns)) for (f in unname(rc_files)) {
    h <- pc_grep_code(bait_copy(f, rc_patterns[[id]][[2L]]), rc_pat)
    expect_true(id %in% h$pattern, info = paste(id, basename(f)))
  }
})

test_that("CONTROL: aritmetica de hora e mensagem nao ASCII injetadas reprovam", {
  for (f in unname(rc_files)) {
    expect_gt(nrow(pc_grep_code(bait_copy(f, "y <- x + lubridate::hours(1)"), pc_time_arith_patterns(), allow = list())), 0L)
    expect_gt(nrow(pc_grep_code(bait_copy(f, "z <- to_model_frame(x)"), pc_time_arith_patterns(), allow = list())), 0L)
    expect_gt(nrow(pc_bytes_over_127(bait_copy(f, "stop(\"não ASCII\")"))), 0L)
  }
})

test_that("CONTROL: comentarios nao contam (a mesma violacao em comentario passa)", {
  for (f in unname(rc_files))
    expect_identical(nrow(pc_grep_code(bait_copy(f, "# x <- Sys.getenv('TZ'); n <- Sys.time()"), rc_pat)), 0L)
})
