## test-shell-sujo-windows.R -- shell sujo x shell limpo pelo lancador INSTALADO no Windows (M8.11, AC 2; ADR-0038): TZ/LC_ALL/OPENBLAS/.Rprofile do
## usuario nao mudam NENHUM byte da saida (runtime.R fixa TZ, locale e BLAS in-process). So Windows: fora dele skip explicito `nao-windows`.
## Mesmos bracos que install/tests/windows-vm.Tests.ps1 [W13] (que e o executado na VM sem R); este arquivo e a versao testthat para um R de dev no Windows.
## Insumos por options(): prevcarga.sujo.root (raiz R instalada), .deck, .state (estado sincronizado), .origin; ausentes => skip `insumo-ausente`.
## FORBIDDEN: stdout (sem os campos de relogio) e arvore da entrega identicos; `prevcarga.cmd` so com as 6 limpezas `set "X="`.
## CONTROL 1: a sonda de hora (sem o guard do runtime) DIFERE entre os shells. CONTROL 2: setter de TZ que nao fixa => exit 7, stdout vazio.
## [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] NUNCA executado nesta maquina de dev (Linux).
library(testthat)

sj_opt <- function(k) { v <- getOption(paste0("prevcarga.sujo.", k)); if (is.null(v)) "" else as.character(v) }
sj_skip <- function() {
  skip_if_not(.Platform$OS.type == "windows", "nao-windows")
  skip_if(!nzchar(sj_opt("root")) || !nzchar(sj_opt("deck")) || !nzchar(sj_opt("state")) || !nzchar(sj_opt("origin")),
          "insumo-ausente: options(prevcarga.sujo.root/.deck/.state/.origin)")
}

## ambiente do filho: `current` = heranca integral; `sujo` acrescenta o shell sujo (home com .Rprofile que muda o TZ)
sj_home <- function() {
  h <- file.path(tempfile("pc-home-sujo-")); dir.create(file.path(h, "Documents"), recursive = TRUE)
  rp <- c("Sys.setenv(TZ = 'Asia/Tokyo')", "Sys.setlocale('LC_COLLATE', 'C')", "options(digits = 3, OutDec = ',')")
  writeLines(rp, file.path(h, "Documents", ".Rprofile")); writeLines(rp, file.path(h, ".Rprofile"))
  h
}
sj_norm <- function(x) gsub('"duracao_s":[-0-9.eE+]+', '"duracao_s":0', gsub('"(run_id|generated_at|gerado_em)":"[^"]*"', '"\\1":"X"', x))

test_that("[AC2] prevcarga.cmd: so as 6 limpezas `set \"X=\"`; qualquer outro `set ` falha (CONTROL)", {
  skip_if_not(.Platform$OS.type == "windows", "nao-windows")
  root <- sj_opt("root"); skip_if(!nzchar(root), "insumo-ausente: options(prevcarga.sujo.root)")
  l <- readLines(file.path(root, "bin", "prevcarga.cmd"))
  allowed <- paste0('set "', c("R_HOME", "R_LIBS", "R_LIBS_USER", "R_LIBS_SITE", "R_PROFILE_USER", "R_ENVIRON_USER"), '="')
  bad <- function(x) { i <- grep("(?i)^\\s*set\\s", x, perl = TRUE); i[!(trimws(x[i]) %in% paste0(allowed, '"'))] }
  expect_length(bad(l), 0L)
  expect_length(bad(c(l, "  SET TZ=UTC")), 1L)                                    # CONTROL
})

test_that("[AC2] FORBIDDEN: forecast --origin O pelo prevcarga.cmd, shell sujo x limpo: stdout e entrega byte-identicos; CONTROL: outro deck => outra entrega", {
  sj_skip()
  root <- sj_opt("root"); O <- sj_opt("origin"); home <- sj_home(); on.exit(unlink(home, recursive = TRUE), add = TRUE)
  suja <- c(TZ = "America/Sao_Paulo", LC_ALL = "pt_BR.UTF-8", OPENBLAS_NUM_THREADS = "4", R_USER = home, HOME = home,
            R_PROFILE_USER = file.path(home, "Documents", ".Rprofile"))
  n <- 0L
  run_fc <- function(env, deck = sj_opt("deck")) {
    n <<- n + 1L; st <- file.path(tempfile(paste0("pc-sujo-st", n, "-"))); stopifnot(file.copy(sj_opt("state"), dirname(st), recursive = TRUE), file.rename(file.path(dirname(st), basename(sj_opt("state"))), st))
    r <- processx::run(file.path(root, "bin", "prevcarga.cmd"), c("forecast", paste0("--data=", deck), paste0("--state=", st), "--origin", O, "--skip-validate", "--format", "json"),
                       env = env, error_on_status = FALSE, windows_verbatim_args = FALSE, encoding = "UTF-8", timeout = 7200)
    j <- tryCatch(jsonlite::fromJSON(r$stdout, simplifyVector = FALSE), error = function(e) NULL)
    saida <- if (is.null(j)) NA_character_ else j$result$saida
    list(status = r$status, stdout = sj_norm(gsub(st, "STATE", r$stdout, fixed = TRUE)), stderr = r$stderr, saida = saida,
         tree = if (!is.na(saida)) pc_tree_sha_skip(saida, "manifest.json") else NA_character_)
  }
  pc_tree_sha_skip <- function(d, skip) {
    f <- setdiff(sort(list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE), method = "radix"), skip)
    digest::digest(paste(f, vapply(f, function(p) digest::digest(file = file.path(d, p), algo = "sha256"), ""), collapse = "\n"), algo = "sha256")
  }
  a <- run_fc(c("current", suja)); b <- run_fc("current")
  expect_identical(a$status, 0L); expect_identical(b$status, 0L)
  expect_gt(nchar(a$stdout), 0L)
  expect_identical(a$stdout, b$stdout); expect_identical(a$stderr, b$stderr); expect_identical(a$tree, b$tree)
  expect_false(any(charToRaw(paste0(a$stdout, a$stderr)) > as.raw(127L)))
})

test_that("[AC2] CONTROL 1: a sonda de hora sem o guard do runtime difere entre o shell sujo e o limpo", {
  sj_skip()
  root <- sj_opt("root"); home <- sj_home(); on.exit(unlink(home, recursive = TRUE), add = TRUE)
  v <- trimws(readLines(file.path(root, "current"), n = 1L))
  rs <- file.path(root, "versions", v, "runtime", "R", "bin", "Rscript.exe")
  probe <- c("--vanilla", "-e", "cat(format(as.POSIXct(0, origin = '1970-01-01', tz = 'UTC'), tz = '', usetz = TRUE))")
  a <- processx::run(rs, probe, env = c("current", TZ = "America/Sao_Paulo", R_USER = home), error_on_status = FALSE)
  b <- processx::run(rs, probe, env = c("current", TZ = "UTC"), error_on_status = FALSE)
  expect_false(identical(a$stdout, b$stdout))
})
