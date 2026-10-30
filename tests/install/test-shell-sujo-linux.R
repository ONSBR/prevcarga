## test-shell-sujo-linux.R -- shell sujo x shell limpo pelo lancador INSTALADO (M8.3, AC 5): TZ/LC_ALL/OPENBLAS/~/.Rprofile do usuario
## nao mudam NENHUM byte da saida (runtime.R fixa TZ, locale e BLAS in-process e sobrescreve o shell). Usa o bundle COMPLETO.
## [DEFERRED M6.9] o verbo `forecast` ainda nao existe na CLI (forecast.R so tem a funcao): os verbos que leem o deck e dependem de
## data/hora/locale -- describe e validate-deck (texto e JSON) -- ocupam o lugar do `forecast --origin`; trocar quando o M6.9 pousar.
## FORBIDDEN: stdout, stderr, exit e arvores (deck e state) byte-identicos entre os dois shells; `grep -c export` no lancador = 0.
## CONTROL 1: a sonda de hora (sem o guard do runtime) DIFERE entre os shells. CONTROL 2: setter de TZ que nao fixa => exit 7, stdout vazio.
library(testthat)
pci_skip_unless_ready()

W <- pci_work_dir()
e <- pci_env(); sys.source(file.path(pci_root(), "scripts", "tools", "fixture_store.R"), envir = e)
O <- as.Date("2026-10-01")
DECK <- file.path(W, "deck")
e$prevcarga_fixture_deck(DECK, seed = 1L, areas = c("RJ", "SP"), days = seq(O - 30, O, by = "day"),
                         load_until = lubridate::ymd_hm("2026-10-01 09:30", tz = "Etc/GMT+3"))
R <- file.path(W, "R")
bundle <- pc_fixture_bundle("vsujo", slim = FALSE)
ins <- pc_install(c("--bundle", bundle$path, "--sha256", bundle$sha256, "--root", R, "--data", DECK, "--format", "json"))
stopifnot(identical(ins$status, 0L))
LAUNCHER <- file.path(R, "bin", "prevcarga")
HOME_SUJO <- file.path(W, "home"); dir.create(HOME_SUJO)
writeLines(c("Sys.setenv(TZ = 'Asia/Tokyo')", "Sys.setlocale('LC_COLLATE', 'C')", "options(digits = 3, OutDec = ',')"), file.path(HOME_SUJO, ".Rprofile"))
STATE <- file.path(R, "state")

## Valores do shell sujo. O locale pedido e pt_BR.UTF-8; se o host nao o tem, o bash (que o R usa em bin/R) avisa em stderr por conta
## propria (ruido do host, nao do pacote) -- cai no 1o locale UTF-8 nao-C instalado. O TZ pedido e America/Sao_Paulo; se o fuso local do
## host JA e esse, a sonda do CONTROL 1 nao distinguiria os shells: usa Asia/Tokyo.
LOCALES <- tryCatch(system2("locale", "-a", stdout = TRUE, stderr = FALSE), error = function(e) character())
LC_SUJO <- c(intersect(c("pt_BR.UTF-8", "pt_BR.utf8", "en_US.UTF-8", "en_US.utf8"), LOCALES), "C.UTF-8")[[1L]]
PROBE <- c("--vanilla", "-e", "cat(format(as.POSIXct(0, origin = '1970-01-01', tz = 'UTC'), tz = '', usetz = TRUE))")
.host_probe <- system2("env", c("-i", "PATH=/usr/bin:/bin", shQuote(file.path(R, "versions", "vsujo", "runtime", "bin", "Rscript")), shQuote(PROBE)),
                       stdout = TRUE, stderr = FALSE)
TZ_SUJO <- if (grepl("-03", paste(.host_probe, collapse = ""))) "Asia/Tokyo" else "America/Sao_Paulo"

## shell = "sujo" | "limpo". `cmd` = argv do executavel (arg 1 = script sh ou binario).
run_in <- function(shell, exe, args) {
  envs <- if (identical(shell, "sujo"))
    c(paste0("TZ=", TZ_SUJO), paste0("LC_ALL=", LC_SUJO), paste0("LANG=", LC_SUJO), "OPENBLAS_NUM_THREADS=4", "OMP_NUM_THREADS=4",
      "MKL_NUM_THREADS=4", paste0("HOME=", HOME_SUJO), paste0("R_PROFILE_USER=", file.path(HOME_SUJO, ".Rprofile")),
      "PATH=/usr/local/bin:/usr/bin:/bin")
  else "PATH=/usr/local/bin:/usr/bin:/bin"
  so <- tempfile(); se <- tempfile(); on.exit(unlink(c(so, se)))
  st <- system2("env", c("-i", envs, exe, vapply(args, shQuote, "")), stdout = so, stderr = se)
  list(status = as.integer(st), stdout = readBin(so, "raw", file.size(so)), stderr = readBin(se, "raw", file.size(se)))
}
## run_id = <relogio UTC>-<pid> do envelope JSON (por desenho, ADR-0040): a UNICA diferenca permitida entre duas execucoes.
norm_run_id <- function(raw) charToRaw(gsub('"run_id":"[^"]*"', '"run_id":"X"', rawToChar(raw)))
via_launcher <- function(shell, args, launcher = LAUNCHER) run_in(shell, "/bin/sh", c(launcher, args))

CMDS <- list(
  describe_json = c("describe", "--origin", "2026-10-01", "--format", "json"),
  describe_txt = c("describe", "--origin", "2026-10-01"),
  validate_json = c("validate-deck", "--origin", "2026-10-01", "--format", "json"),
  validate_txt = c("validate-deck", "--origin", "2026-10-01"),
  version = "--version")

test_that("[AC5] FORBIDDEN: stdout, stderr, exit e arvores deck/state byte-identicos entre shell sujo e `env -i`", {
  d0 <- pc_tree_sha(DECK); s0 <- pc_tree_sha(STATE)
  for (nm in names(CMDS)) {
    a <- via_launcher("sujo", CMDS[[nm]]); b <- via_launcher("limpo", CMDS[[nm]])
    expect_identical(a$status, b$status, info = nm)
    expect_identical(norm_run_id(a$stdout), norm_run_id(b$stdout), info = nm)
    expect_identical(a$stderr, b$stderr, info = nm)
    expect_true(length(a$stdout) > 0L, info = nm)                       # nao e igualdade de dois vazios
    expect_false(any(as.integer(c(a$stdout, a$stderr)) > 127L), info = nm)
  }
  expect_identical(pc_tree_sha(DECK), d0); expect_identical(pc_tree_sha(STATE), s0)   # as saidas (arquivos) tambem nao mudam
  ## o shell sujo chegou de fato ao processo (o doctor mostra TZ/locale/threads encontrados vs efetivos)
  dj <- pci_json(rawToChar(via_launcher("sujo", c("doctor", "--format", "json"))$stdout))$result$checks
  row <- function(nm) Filter(function(x) identical(x$check, nm), dj)[[1L]]
  expect_identical(row("TZ")$found, TZ_SUJO); expect_identical(row("TZ")$effective, "UTC")
  expect_identical(row("OPENBLAS_NUM_THREADS")$found, "4"); expect_identical(row("OPENBLAS_NUM_THREADS")$effective, "1")
  ## o lancador nao exporta nada
  lines <- readLines(file.path(pci_root(), "install", "prevcarga"))
  expect_identical(sum(grepl("export", lines, fixed = TRUE)), 0L)
  expect_identical(sum(grepl("export", readLines(LAUNCHER), fixed = TRUE)), 0L)
})

test_that("[AC5] CONTROL 1: a sonda de hora sem o guard do runtime difere entre os shells (o shell sujo e mesmo sujo)", {
  rs <- file.path(R, "versions", "vsujo", "runtime", "bin", "Rscript")
  a <- run_in("sujo", rs, PROBE); b <- run_in("limpo", rs, PROBE)
  expect_false(identical(a$stdout, b$stdout))
})

test_that("[AC5] CONTROL 2: setter de TZ injetado que nao fixa => exit 7 com stdout vazio; a copia sem a injecao roda (exit 0)", {
  R2 <- file.path(W, "R2"); V2 <- file.path(R2, "versions", "vsujo")
  dir.create(file.path(R2, "bin"), recursive = TRUE); dir.create(V2, recursive = TRUE)
  pci_sh(sprintf("cp -a %s %s && ln -s %s %s && cp %s %s", shQuote(file.path(R, "versions", "vsujo", "code")), shQuote(V2),
                 shQuote(file.path(R, "versions", "vsujo", "runtime")), shQuote(file.path(V2, "runtime")),
                 shQuote(LAUNCHER), shQuote(file.path(R2, "bin", "prevcarga"))))
  writeLines("vsujo", file.path(R2, "current"))
  L2 <- file.path(R2, "bin", "prevcarga")
  ok <- via_launcher("sujo", "models", launcher = L2)
  expect_identical(ok$status, 0L)
  rt <- file.path(V2, "code", "scripts", "lib", "runtime.R")
  cat(c("", "## [CONTROL] setter de TZ que NAO fixa o fuso (so observa), apenas nesta copia",
        ".ctl_orig_setters <- prevcarga_runtime_setters",
        "prevcarga_runtime_setters <- function() { s <- .ctl_orig_setters(); s$tz <- function() .prevcarga_tz_observed(); s }"),
      file = rt, sep = "\n", append = TRUE)
  bad <- via_launcher("sujo", "models", launcher = L2)
  expect_identical(bad$status, 7L)
  expect_length(bad$stdout, 0L)
  expect_match(strsplit(rawToChar(bad$stderr), "\n")[[1L]][1L], "^prevcarga: erro \\[runtime\\] \\(exit 7\\)$")
})
