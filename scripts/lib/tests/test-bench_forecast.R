## test-bench_forecast.R -- bench do NFR de 30 min (M6.6): scripts/tools/bench_forecast.R. Funcao pura sobre forecast injetado (fixture pequena de
## data/ em tempdir): campos do JSON, veredito ok/bloqueante, data/ intacto, estado em tempdir, ausencia de flag que afrouxe o alvo e de oraculo.
## A medicao real (<= 1800 s no data/ real) e do operador; o numero vai para docs/estado/m6-forecast.md no M6.11.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
.bench_file <- file.path(pc_root(), "scripts", "tools", "bench_forecast.R")
.b <- local({
  e <- new.env(parent = globalenv())
  sys.source(file.path(pc_root(), "scripts", "lib", "errors.R"), envir = e)
  sys.source(.bench_file, envir = e)
  e
})

.mk_data <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "load"))
  writeLines("a,b", file.path(d, "load", "x.csv")); writeLines("c", file.path(d, "y.txt"))
  d
}
.fake_forecast <- function(ctx, origin) list(result = list(covered = c("A1", "A2"), stray = "B1"))
.clock <- function(...) { v <- c(...); i <- 0L; function() { i <<- i + 1L; v[[i]] } }
.run <- function(ctx, budget = 1800, clock = .clock(0, 12.34), ...)
  .b$bench_run(ctx, lubridate::ymd("2026-01-15"), 1500, .fake_forecast, function(ctx) 8L, budget_s = budget, clock = clock,
               host = "h1", r_version = "R 4.5.3", ...)

test_that("JSON traz todos os campos e veredito ok quando <= 1800 s", {
  ctx <- list(data_dir = .mk_data())
  r <- .run(ctx, train_counter = list2env(list(train = 3L)))
  js <- jsonlite::fromJSON(.b$bench_render(r, "json"))
  expect_setequal(names(js), c("segundos", "referencia_s", "razao", "alvo_s", "host", "r_version", "n_folhas", "n_membros",
                               "treinou_origin_scoped", "veredito"))
  expect_identical(js$veredito, "ok")
  expect_equal(js$segundos, 12.3); expect_equal(js$referencia_s, 1500); expect_equal(js$razao, 0.008)
  expect_equal(js$alvo_s, 1800); expect_identical(js$host, "h1")
  expect_equal(js$n_folhas, 3L); expect_equal(js$n_membros, 8L); expect_true(js$treinou_origin_scoped)
  expect_false(.run(ctx)$treinou_origin_scoped)                               # sem treino registrado
})

test_that("budget injetado de 1 s => veredito bloqueante e mensagem ASCII 'NFR 30 min estourado'", {
  ctx <- list(data_dir = .mk_data())
  r <- .run(ctx, budget = 1, clock = .clock(0, 5))
  expect_identical(r$veredito, "bloqueante")
  expect_equal(r$alvo_s, 1)
  txt <- .b$bench_render(r, "text")
  expect_true(any(grepl("NFR 30 min estourado", txt, fixed = TRUE)))
  expect_false(any(grepl("[^\\x20-\\x7E]", txt, perl = TRUE)))
  expect_identical(jsonlite::fromJSON(.b$bench_render(r, "json"))$veredito, "bloqueante")
  expect_identical(.run(ctx, budget = 12.34, clock = .clock(0, 12.34))$veredito, "ok")   # limite inclusivo
})

test_that("[FORBIDDEN] data/ intacto: listagem nome/tamanho/sha256 identica antes e depois; [CONTROL] alteracao aborta exit 9", {
  d <- .mk_data()
  before <- .b$.bench_data_listing(d)
  expect_identical(names(before), c("nome", "tamanho", "sha256"))
  expect_identical(before$nome, c("load/x.csv", "y.txt"))
  .run(list(data_dir = d))
  expect_identical(.b$.bench_data_listing(d), before)
  mutate <- function(f) function(ctx, origin) { f(ctx$data_dir); .fake_forecast(ctx, origin) }
  for (f in list(function(p) writeLines("novo", file.path(p, "z.txt")),
                 function(p) writeLines("c2", file.path(p, "y.txt")),
                 function(p) unlink(file.path(p, "y.txt")))) {
    dd <- .mk_data()
    e <- tryCatch(.b$bench_run(list(data_dir = dd), lubridate::ymd("2026-01-15"), 1500, mutate(f), function(ctx) 8L), prevcarga_error = identity)
    expect_s3_class(e, "prevcarga_error")
    expect_identical(e$id, "integrity.bench_data_changed")
    expect_identical(e$exit_code, 9L)
  }
})

test_that("estado: --state e COPIADO para tempdir (fonte intacta); sem --state o estado nasce vazio", {
  src <- withr::local_tempdir(); dir.create(file.path(src, "artifacts", "m"), recursive = TRUE); writeLines("v", file.path(src, "artifacts", "m", "a.rds"))
  before <- .b$.bench_data_listing(src)
  dst <- file.path(tempdir(), "bench-state-test")
  on.exit(unlink(dst, recursive = TRUE), add = TRUE)
  .b$.bench_copy_state(src, dst)
  expect_true(file.exists(file.path(dst, "artifacts", "m", "a.rds")))
  writeLines("w", file.path(dst, "novo.txt"))
  expect_identical(.b$.bench_data_listing(src), before)
  dst2 <- file.path(tempdir(), "bench-state-test2"); on.exit(unlink(dst2, recursive = TRUE), add = TRUE)
  .b$.bench_copy_state(NULL, dst2)
  expect_length(list.files(dst2, all.files = TRUE, no.. = TRUE), 0L)
  expect_s3_class(tryCatch(.b$.bench_copy_state(file.path(src, "nao-existe"), dst2), prevcarga_error = identity), "prevcarga_error")
})

test_that("argv: so --origin/--reference-seconds/--format/--data/--state/--config; nao existe flag que afrouxe o alvo", {
  ok <- .b$.bench_parse(c("--origin", "2026-01-15", "--reference-seconds=1500", "--format", "json"))
  expect_equal(ok[["--reference-seconds"]], 1500); expect_identical(ok[["--format"]], "json")
  for (bad in list(c("--origin", "2026-01-15"), c("--origin", "2026-01-15", "--reference-seconds", "0"),
                   c("--origin", "2026-01-15", "--reference-seconds", "abc"),
                   c("--origin", "2026-01-15", "--reference-seconds", "1", "--alvo", "99999"),
                   c("--origin", "2026-01-15", "--reference-seconds", "1", "--alvo=99999"),
                   c("--origin", "2026-01-15", "--reference-seconds", "1", "--budget", "9"),
                   c("--origin", "2026-01-15", "--reference-seconds", "1", "--format", "xml"))) {
    e <- tryCatch(.b$.bench_parse(bad), prevcarga_error = identity)
    expect_s3_class(e, "prevcarga_error"); expect_identical(e$id, "contract.bench_bad_args")
  }
  expect_identical(.b$BENCH_BUDGET_S, 1800)
})

test_that("processo: args invalidos => exit 2 e mensagem ASCII em stderr; --alvo recusado", {
  run <- function(a) {
    e1 <- tempfile(); on.exit(unlink(e1))
    st <- suppressWarnings(system2(pc_rscript_bin(), c("--vanilla", shQuote(.bench_file), a), stdout = FALSE, stderr = e1))
    list(status = as.integer(st), stderr = readLines(e1, warn = FALSE))
  }
  r <- run(c("--origin", "2026-01-15"))
  expect_identical(r$status, 2L)
  expect_false(any(grepl("[^\\x20-\\x7E]", r$stderr, perl = TRUE)))
  expect_identical(run(c("--origin", "2026-01-15", "--reference-seconds", "1", "--alvo", "9999"))$status, 2L)
})

test_that("o bench nunca executa o oraculo: sem prevcargadessem3, python nem system( no fonte", {
  src <- readLines(.bench_file, warn = FALSE)
  expect_false(any(grepl("prevcargadessem3|python|system\\(|system2\\(|Sys\\.getenv|Sys\\.time|--alvo", src)))
  expect_true(any(grepl("proc.time", src, fixed = TRUE)))
  expect_false(any(grepl("[^\\x01-\\x7F]", src, perl = TRUE)))                 # ASCII puro
})

test_that("listagem de data/ nao segue symlink de diretorio (o store real tem ciclo data/data -> .) e detecta troca de alvo", {
  d <- .mk_data()
  skip_if(!isTRUE(suppressWarnings(file.symlink(".", file.path(d, "ciclo")))), "symlink indisponivel")
  l <- .b$.bench_data_listing(d)
  expect_true("ciclo" %in% l$nome)
  expect_identical(l$tamanho[l$nome == "ciclo"], 0)
  unlink(file.path(d, "ciclo")); file.symlink("load", file.path(d, "ciclo"))
  expect_false(identical(.b$.bench_data_listing(d), l))
})
