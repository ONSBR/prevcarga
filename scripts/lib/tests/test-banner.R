## test-banner.R -- banner.R (M1.4c; blueprint "Banner da CLI"): snapshot do texto sem cor, 15 linhas, <= 80 colunas, ASCII,
## eixo de 48 colunas, linha de versao (bundle x dev), ESC so com always/auto+TTY+unix (CONTROL), regra de exibicao (json, redirecionado).
library(testthat)
local({
  e <- pc_source_lib(c("errors", "runtime", "banner"))
  for (n in ls(e, all.names = TRUE)) assign(n, get(n, e), envir = globalenv())
})
ver <- trimws(readLines(file.path(pc_root(), "config", "VERSION"), n = 1L, warn = FALSE))
mk <- function(color = "never", format = "text", root = NULL) list(color = color, format = format, version = ver, root = root)
has_esc <- function(b) grepl(paste0(intToUtf8(27L), "["), paste(b, collapse = ""), fixed = TRUE)
VL <- "  prevcarga 1.0.0 | R 4.5.3 | windows-x86_64 | runtime a1b2c3d4"

## Snapshot = o bloco do blueprint, byte a byte (raw string: barra invertida e crase literais).
SNAP <- strsplit(r"---(   _ __   _ __   ___ __   __  ___   __ _  _ __   __ _   __ _
  | '_ \ | '__| / _ \\ \ / / / __| / _` || '__| / _` | / _` |
  | |_) || |   |  __/ \ V / | (__ | (_| || |   | (_| || (_| |
  | .__/ |_|    \___|  \_/   \___| \__,_||_|    \__, | \__,_|
  |_|                                           |___/

                      ______-''''---_   _----_
                  _--'               '-'      '-_
  -_            _'                               '
    '--_______--
  |-----------+-----------+-----------+-----------|
  0h          6h          12h         18h       24h

  Previsao de carga do SIN - ONS
  prevcarga 1.0.0 | R 4.5.3 | windows-x86_64 | runtime a1b2c3d4)---", "\n", fixed = TRUE)[[1L]]

test_that("snapshot sem cor = blueprint; 15 linhas, <= 63 colunas, ASCII, deterministico", {
  b <- prevcarga_banner(mk(), version_line = VL)
  expect_identical(b, SNAP)
  expect_length(b, 15L)
  expect_lte(max(nchar(b, type = "bytes")), 63L)
  expect_true(all(utf8ToInt(paste(b, collapse = "")) <= 127L))
  expect_identical(b, prevcarga_banner(mk(), version_line = VL))
  src <- readBin(file.path(pc_root(), "scripts", "lib", "banner.R"), "raw", 1e5)
  expect_true(all(as.integer(src) <= 127L))
  ## CONTROL: um caractere trocado na curva quebra o snapshot
  mut <- b; mut[7] <- sub("______", "_____-", mut[7], fixed = TRUE)
  expect_false(identical(mut, SNAP))
})

test_that("eixo: 48 colunas de meia hora (col 3..50) com marcas nas fronteiras; a curva cabe na faixa", {
  cv <- .prevcarga_banner_curve()
  expect_length(cv, 6L)
  ax <- cv[5]
  expect_identical(which(strsplit(ax, "")[[1]] %in% c("|", "+")), c(3L, 15L, 27L, 39L, 51L))   # 0h 6h 12h 18h 24h
  expect_identical(51L - 3L, 48L)
  lab <- cv[6]
  expect_identical(unname(sapply(c("0h", "6h", "12h", "18h"), function(s) regexpr(s, lab, fixed = TRUE)[[1]])), c(3L, 15L, 27L, 39L))
  expect_identical(nchar(lab), 51L)                                          # "24h" termina na fronteira de 24h
  for (l in cv[1:4]) {
    ch <- strsplit(l, "")[[1]]
    used <- which(ch != " ")
    expect_true(all(used >= 3L & used <= 50L), info = l)
    expect_true(all(ch[used] %in% c("_", "-", "'")), info = l)               # niveis _ - '
  }
})

test_that("linha de versao: bundle (RUNTIME.json) x dev (runtime dev, plataforma do host)", {
  d <- tempfile("bnr"); dir.create(file.path(d, "code"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE))
  dev <- .prevcarga_banner_version_line(mk(root = file.path(d, "code")), r_version = "4.5.3", host = "linux-x86_64")
  expect_identical(dev, sprintf("  prevcarga %s | R 4.5.3 | linux-x86_64 | runtime dev", ver))
  jsonlite::write_json(list(version = ver, platform = "windows-x86_64", files = list()), file.path(d, "RUNTIME.json"),
                       auto_unbox = TRUE)
  rid <- prevcarga_runtime_id(file.path(d, "RUNTIME.json"))
  bl <- .prevcarga_banner_version_line(mk(root = file.path(d, "code")), r_version = "4.5.3", host = "linux-x86_64")
  expect_identical(bl, sprintf("  prevcarga %s | R 4.5.3 | windows-x86_64 | runtime %s", ver, substr(rid, 1L, 8L)))
  expect_match(bl, "runtime [0-9a-f]{8}$")
  expect_match(.prevcarga_banner_version_line(mk()), sprintf("^  prevcarga %s \\| R %s \\| ", ver, getRversion()))
  b <- prevcarga_banner(mk())
  expect_length(b, 15L); expect_lte(max(nchar(b)), 80L)
})

test_that("cor: ESC so com always e auto+TTY+unix; partes 34/33/90; sem cor o texto e identico (CONTROL auto sem TTY e Windows)", {
  expect_false(has_esc(prevcarga_banner(mk("never"), tty = TRUE, os = "unix", version_line = VL)))
  expect_true(has_esc(prevcarga_banner(mk("always"), tty = FALSE, os = "windows", version_line = VL)))
  expect_false(has_esc(prevcarga_banner(mk("auto"), tty = FALSE, os = "unix", version_line = VL)))
  expect_false(has_esc(prevcarga_banner(mk("auto"), tty = TRUE, os = "windows", version_line = VL)))
  expect_true(has_esc(prevcarga_banner(mk("auto"), tty = TRUE, os = "unix", version_line = VL)))
  expect_true(prevcarga_use_color(mk("always"), FALSE, "windows"))
  expect_false(prevcarga_use_color(mk("auto"), FALSE, "unix"))
  expect_false(prevcarga_use_color(mk("auto"), TRUE, "windows"))
  expect_true(prevcarga_use_color(mk("auto"), TRUE, "unix"))
  b <- prevcarga_banner(mk("always"), version_line = VL)
  esc <- intToUtf8(27L)
  expect_identical(gsub(paste0(esc, "\\[[0-9]+m"), "", b), SNAP)             # a cor nunca carrega informacao
  cod <- sub(paste0("^", esc, "\\[([0-9]+)m.*$"), "\\1", b)
  expect_identical(cod[1:5], rep("34", 5L)); expect_identical(cod[7:10], rep("33", 4L))
  expect_identical(cod[c(11, 12, 14, 15)], rep("90", 4L)); expect_identical(b[c(6, 13)], c("", ""))
  expect_true(all(utf8ToInt(paste(b, collapse = "")) <= 127L))
})

test_that("regra de exibicao por verbo/formato/TTY: nunca com json nem com stdout redirecionado", {
  expect_true(prevcarga_banner_wanted(mk(), "--help", tty = TRUE))
  expect_true(prevcarga_banner_wanted(mk(), "--version", tty = TRUE))
  expect_true(prevcarga_banner_wanted(mk(), NULL, tty = TRUE))
  expect_true(prevcarga_banner_wanted(mk(), "doctor", tty = TRUE))
  expect_false(prevcarga_banner_wanted(mk(format = "json"), "doctor", tty = TRUE))
  expect_false(prevcarga_banner_wanted(mk(format = "json"), "--help", tty = TRUE))
  expect_false(prevcarga_banner_wanted(mk(), "models", tty = TRUE))
  expect_false(prevcarga_banner_wanted(mk(), "status", tty = TRUE))
  for (v in list(NULL, "--help", "--version", "doctor"))                      # stdout redirecionado: nunca
    expect_false(prevcarga_banner_wanted(mk("always"), v, tty = FALSE))
})
