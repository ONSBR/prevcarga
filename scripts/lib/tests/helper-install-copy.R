## helper-install-copy.R -- helper do M1.V (auto-carregado por test_dir). Nao e codigo de produto.
## Instalacao-copia em tempdir() para gates de PROCESSO: o filho real `Rscript --vanilla <copia>/scripts/prevcarga`.

pc_repo_root <- function() pc_root()

## fixtures: nomes sem prefixo ("probe", "contract", ...): copia scripts/lib/tests/fixtures/cli_fixture_verbs.R
## e grava UM arquivo de registro scripts/lib/cli/fixture-<x>.R por fixture (descoberto pela fachada da copia).
## broken_tz: injeta runtime_setter_tz_broken (anexa a runtime.R da copia; so a copia e alterada).
## mutate(root): quebra topology/run.yaml/config.yaml da copia.
pc_install_copy <- function(fixtures = character(), mutate = NULL, broken_tz = FALSE, src = pc_repo_root()) {
  root <- file.path(tempfile("pc-install-"), "prevcarga")
  dir.create(root, recursive = TRUE)
  for (d in c("scripts", "config", "install"))
    if (dir.exists(file.path(src, d))) file.copy(file.path(src, d), root, recursive = TRUE)
  unlink(file.path(root, "scripts", "lib", "tests"), recursive = TRUE)
  for (ry in Sys.glob(file.path(src, "models", "*", "run.yaml"))) {
    dst <- file.path(root, "models", basename(dirname(ry)))
    dir.create(dst, recursive = TRUE)
    file.copy(ry, dst)
  }
  if (length(fixtures) || isTRUE(broken_tz)) {
    file.copy(file.path(src, "scripts", "lib", "tests", "fixtures", "cli_fixture_verbs.R"),
              file.path(root, "scripts", "lib"))
    for (fx in fixtures) {
      us <- gsub("-", "_", fx)
      reg <- sprintf(paste(
        "prevcarga_verb_fixture_%s <- list(name = 'fixture-%s', usage = 'prevcarga fixture-%s',",
        "  options = character(), handler = function(ctx, args) cmd_fixture_%s(ctx, args), owner = 'M1.V',",
        "  exempt = FALSE, modules = 'cli_fixture_verbs.R')", sep = "\n"), us, fx, fx, us)
      writeLines(reg, file.path(root, "scripts", "lib", "cli", sprintf("fixture-%s.R", fx)))
    }
  }
  if (isTRUE(broken_tz)) {
    rt <- file.path(root, "scripts", "lib", "runtime.R")
    cat(c("",
          "## [M1.V fixture] setter de TZ quebrado, so nesta copia",
          "source(file.path(getOption('prevcarga.lib'), 'cli_fixture_verbs.R'))",
          ".fx_orig_setters <- prevcarga_runtime_setters",
          "prevcarga_runtime_setters <- function() { s <- .fx_orig_setters(); s$tz <- runtime_setter_tz_broken; s }"),
        file = rt, sep = "\n", append = TRUE)
  }
  if (is.function(mutate)) mutate(root)
  root
}

## Roda `Rscript --vanilla <root>/scripts/prevcarga <args>`; stdout/stderr em arquivos separados.
## env (vetor nomeado, NA = remover) vale para o filho, herdado do processo de teste (portavel no Windows).
pc_cli <- function(root, args = character(), env = character()) {
  tf1 <- tempfile("pc-out"); tf2 <- tempfile("pc-err")
  on.exit(unlink(c(tf1, tf2)), add = TRUE)
  cli <- file.path(root, "scripts", "prevcarga")
  run <- function() suppressWarnings(system2(pc_rscript_bin(), c("--vanilla", shQuote(cli), args),
                                             stdout = tf1, stderr = tf2))
  st <- if (length(env)) pc_with_env(as.list(env), run()) else run()
  bytes <- function(f) { n <- file.info(f)$size; if (is.na(n) || n == 0) raw() else readBin(f, "raw", n) }
  list(status = as.integer(st), stdout = readLines(tf1, warn = FALSE), stderr = readLines(tf2, warn = FALSE),
       raw = c(bytes(tf1), bytes(tf2)), raw_stdout = bytes(tf1), raw_stderr = bytes(tf2))
}

pc_assert_ascii <- function(raw, what = "saida") {
  bad <- which(as.integer(raw) > 127L)
  if (length(bad)) stop(sprintf("%s: %d byte(s) > 127 (1o na posicao %d)", what, length(bad), bad[[1L]]))
  invisible(TRUE)
}

## Locale sujo portavel: pt_BR.UTF-8 se o host tiver; senao um UTF-8 existente (evita aviso de startup do R
## no stderr do filho). NA se nenhum disponivel.
pc_dirty_lc_all <- function() {
  old <- Sys.getlocale("LC_COLLATE"); on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", old)), add = TRUE)
  for (lc in c("pt_BR.UTF-8", "pt_BR.utf8", "en_US.UTF-8", "en_US.utf8", "Portuguese_Brazil.utf8"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", lc)))) return(lc)
  NA_character_
}

pc_dirty_env <- function() c(TZ = "America/Sao_Paulo", LC_ALL = pc_dirty_lc_all(),
                             OMP_NUM_THREADS = "4", OPENBLAS_NUM_THREADS = "4")
