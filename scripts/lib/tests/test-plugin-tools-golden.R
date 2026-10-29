## test-plugin-tools-golden.R -- scripts/tools/plugin_golden.R (P.0f): oraculo em processo filho por unidade de units.yaml.
## Processo REAL via pc_rscript contra um oraculo-brinquedo (repo git em tempdir); nunca toca o oraculo verdadeiro.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime"))
sys.source(file.path(pc_root(), "scripts", "tools", "plugin_golden.R"), envir = env)
GOLDEN <- file.path(pc_root(), "scripts", "tools", "plugin_golden.R")

## roda o gerador; `units` = caminho do units.yaml; `...` args extras
gen <- function(root, oracle, out, units = pc_units_two(root), model = "toy-30", extra = character()) {
  pc_rscript(c(GOLDEN, "--model", model, "--root", root, "--units", units, "--oracle", oracle, "--out", out, extra))
}
setup <- function(kind = "ok", model = "toy-30") {
  root <- pc_toy_variant("toy-30"); pc_toy_driver(root, model, kind)
  list(root = root, oracle = pc_fake_oracle(), out = tempfile("golden-"))
}
units_file <- function(lines) { f <- tempfile("units-", fileext = ".yaml"); writeLines(lines, f); f }
U1 <- c("units:", "  - id: u1", "    area: SE", "    origin: 2026-10-01", "    available_until: 2026-10-01T09:00",
        "    weather_issue_cutoff: available_until", "    day0_source: native")

test_that("[ac-0] oracle_env so no filho; start_label => available_until - 30 min; pai limpo; 2 rodadas; layout", {
  s <- setup()
  r <- gen(s$root, s$oracle, s$out)
  expect_identical(r$status, 0L)
  f1 <- file.path(s$out, "plugins/toy-30/_/SE/2026-10-01"); f2 <- file.path(s$out, "plugins/toy-30/_/SECO/2026-10-01")
  for (d in c(f1, f2)) expect_true(all(file.exists(file.path(d, c("native.rds", "long.parquet", "invocation.json")))))
  e1 <- readRDS(file.path(f1, "native.rds")); e2 <- readRDS(file.path(f2, "native.rds"))
  expect_identical(e2$env[["LGBM_TRAIN_CV"]], "0")
  expect_identical(e2$args[["--available-until"]], "2026-10-01T08:30")
  expect_true(is.na(e1$env[["LGBM_TRAIN_CV"]]))
  expect_identical(e1$args[["--available-until"]], "2026-10-01T09:00")
  expect_identical(e1$env[["TZ"]], "UTC"); expect_identical(e1$env[["OMP_NUM_THREADS"]], "1")
  expect_false("LGBM_TRAIN_CV" %in% names(Sys.getenv()))
  inv <- jsonlite::read_json(file.path(f2, "invocation.json"))
  expect_identical(inv$oracle_commit, system2("git", c("-C", shQuote(s$oracle), "rev-parse", "HEAD"), stdout = TRUE))
  expect_false(grepl("golden-scratch", paste(unlist(inv), collapse = " "), fixed = TRUE))
  ## 2 rodadas: o scratch tem run_1/run_2 so durante a execucao; a regravacao e identica byte a byte
  h1 <- tools::md5sum(file.path(f1, c("native.rds", "long.parquet", "invocation.json")))
  r2 <- gen(s$root, s$oracle, s$out)
  expect_identical(r2$status, 0L)
  expect_identical(unname(tools::md5sum(file.path(f1, c("native.rds", "long.parquet", "invocation.json")))), unname(h1))
})

test_that("[ac-1] golden.lock canonico com sha256/commit/digest/fingerprint; oraculo intacto; CONTROL escreve no oraculo", {
  s <- setup(); data <- withr::local_tempdir()
  dir.create(file.path(data, "load/area_code=SE/year=2026"), recursive = TRUE)
  writeLines("p", file.path(data, "load/area_code=SE/year=2026/part-0.parquet"))
  r <- gen(s$root, s$oracle, s$out, extra = c("--data", data))
  expect_identical(r$status, 0L)
  lockf <- file.path(s$root, "toy-30/tests/parity/golden.lock")
  lk <- jsonlite::read_json(lockf)
  expect_identical(names(lk), sort(names(lk), method = "radix"))
  expect_identical(lk$oracle_commit, system2("git", c("-C", shQuote(s$oracle), "rev-parse", "HEAD"), stdout = TRUE))
  u <- lk$units$u1
  expect_true(all(vapply(u$files, function(x) grepl("^[0-9a-f]{64}$", x), NA)))
  expect_setequal(names(u$files), c("native.rds", "long.parquet", "invocation.json"))
  expect_identical(u$files$`native.rds`, digest::digest(file.path(s$out, "plugins/toy-30/_/SE/2026-10-01/native.rds"), algo = "sha256", file = TRUE))
  expect_true(grepl("^[0-9a-f]{64}$", u$data_digest$aggregate))
  expect_identical(names(u$data_digest$partitions), "load/area_code=SE/year=2026")
  expect_true(all(c("R", "platform", "blas", "packages") %in% names(u$fingerprint)))
  expect_identical(u$fingerprint$R, as.character(getRversion()))
  expect_identical(system2("git", c("-C", shQuote(s$oracle), "status", "--porcelain"), stdout = TRUE), character())
  expect_false(grepl("\r", paste(readLines(lockf, warn = FALSE), collapse = ""), fixed = TRUE))
  ## CONTROL: driver que escreve no oraculo => falha, lock nao atualizado
  c1 <- setup("writes_oracle"); lock_c <- file.path(c1$root, "toy-30/tests/parity/golden.lock")
  rc <- gen(c1$root, c1$oracle, c1$out)
  expect_identical(rc$status, 2L)
  expect_match(paste(rc$stderr, collapse = "\n"), "tools.oracle_touched|oracle_touched|tocado: .*touched.txt")
  expect_true(any(grepl("touched.txt", rc$stderr, fixed = TRUE)))
  expect_false(file.exists(lock_c))
})

test_that("[ac-2] CONTROLs: nao-deterministico 12; --out no oraculo 2; units invalido 2; driver status 1; oraculo sujo 2", {
  n <- setup("nondet"); rn <- gen(n$root, n$oracle, n$out, units_file(U1))
  expect_identical(rn$status, 12L)
  expect_true(any(grepl("repro", rn$stderr))); expect_true(any(grepl("u1", rn$stderr)))
  expect_false(file.exists(file.path(n$root, "toy-30/tests/parity/golden.lock")))
  ## --out dentro do oraculo: o driver nao e invocado (nada em --out)
  o <- setup(); inside <- file.path(o$oracle, "golden")
  ro <- gen(o$root, o$oracle, inside)
  expect_identical(ro$status, 2L); expect_false(dir.exists(inside))
  expect_true(any(grepl("contract", ro$stderr)))
  ## units invalido: nomeia a unidade e o campo, sem invocar o driver
  bad <- function(lines) { s <- setup(); r <- gen(s$root, s$oracle, s$out, units_file(lines)); expect_false(dir.exists(s$out)); r }
  no_origin <- bad(U1[!grepl("origin: 2026", U1)])
  expect_identical(no_origin$status, 2L); expect_true(any(grepl("u1.origin", no_origin$stderr, fixed = TRUE)))
  bad_cut <- bad(c(U1, "    oracle_cut: foo"))
  expect_identical(bad_cut$status, 2L); expect_true(any(grepl("u1.oracle_cut", bad_cut$stderr, fixed = TRUE)))
  bad_d0 <- bad(sub("day0_source: native", "day0_source: wat", U1))
  expect_identical(bad_d0$status, 2L); expect_true(any(grepl("u1.day0_source", bad_d0$stderr, fixed = TRUE)))
  dup <- bad(c(U1, U1[-1]))
  expect_identical(dup$status, 2L)
  unk <- bad(c(U1, "    zzz: 1"))
  expect_identical(unk$status, 2L); expect_true(any(grepl("u1.zzz", unk$stderr, fixed = TRUE)))
  nostr <- bad(c(U1, "    oracle_env: {A: 0}"))
  expect_identical(nostr$status, 2L)
  ## driver com status 1: oracle_failed (exit 1) com a cauda do log
  f <- setup("fails"); rf <- gen(f$root, f$oracle, f$out, units_file(U1))
  expect_identical(rf$status, 1L); expect_true(any(grepl("boom-tail-marker", rf$stderr)))
  expect_true(any(grepl("oracle_failed|oraculo terminou", rf$stderr)))
  ## oraculo sujo no inicio
  d <- setup(); writeLines("x", file.path(d$oracle, "dirty.txt"))
  rd <- gen(d$root, d$oracle, d$out)
  expect_identical(rd$status, 2L); expect_false(dir.exists(d$out))
  ## ASCII e JSON unico no envelope
  for (r in list(rn, ro, rf, rd)) expect_false(any(grepl("[^ -~]", r$stderr)))
  j <- gen(f$root, f$oracle, f$out, units_file(U1), extra = c("--format", "json"))
  expect_length(j$stdout, 1L)
  e <- jsonlite::fromJSON(j$stdout)
  expect_false(e$ok); expect_identical(e$error$id, "tools.oracle_failed"); expect_identical(e$exit_code, 1L)
  ok <- setup(); jo <- gen(ok$root, ok$oracle, ok$out, units_file(U1), extra = c("--format", "json"))
  expect_identical(jo$status, 0L); expect_length(jo$stdout, 1L); expect_true(jsonlite::fromJSON(jo$stdout)$ok)
})

test_that("[ac-3] _dessem-engine resolve tests/parity; modelo inexistente => tools.bad_args; grep do script", {
  s <- setup(model = "_dessem-engine")
  r <- gen(s$root, s$oracle, s$out, units_file(U1), model = "_dessem-engine")
  expect_identical(r$status, 0L)
  expect_true(file.exists(file.path(s$root, "_dessem-engine/tests/parity/golden.lock")))
  expect_true(file.exists(file.path(s$out, "plugins/_dessem-engine/_/SE/2026-10-01/native.rds")))
  x <- setup(); rx <- gen(x$root, x$oracle, x$out, units_file(U1), model = "naoexiste")
  expect_identical(rx$status, 2L)
  jx <- gen(x$root, x$oracle, x$out, units_file(U1), model = "naoexiste", extra = c("--format", "json"))
  expect_match(jsonlite::fromJSON(jx$stdout)$error$details$expected, "naoexiste/tests/parity", fixed = TRUE)
  src <- readLines(GOLDEN, warn = FALSE); code <- sub("\\s#.*$", "", src[!grepl("^\\s*##?", src)])
  expect_false(any(grepl("switch\\(|if \\(model ==|(?<![A-Za-z0-9_.])system\\(|Sys\\.(getenv|setenv)", code, perl = TRUE)))
  expect_true(any(grepl("system2\\(", code)))
})

test_that("[ac-4] .state/ ignorado; golden.lock versionado", {
  chk <- function(p) system2("git", c("-C", shQuote(pc_root()), "check-ignore", "-q", p), stdout = FALSE, stderr = FALSE)
  expect_identical(chk(".state/golden/plugins/x/native.rds"), 0L)
  expect_identical(chk("models/toy-30/tests/parity/golden.lock"), 1L)
})

test_that("pc_golden_units: oracle_cut e minutos no tz; variantes no layout", {
  f <- units_file(c(U1, "    variant: MAX", "    oracle_cut: start_label"))
  u <- env$pc_golden_units(f)$units[[1L]]
  expect_identical(env$.pc_golden_cut_arg(u), "2026-10-01T08:30")
  expect_identical(env$.pc_golden_unit_dir("/o", "m", u), "/o/plugins/m/MAX/SE/2026-10-01")
  expect_identical(env$.pc_golden_child_env(u)[-(1:4)], character())
})
