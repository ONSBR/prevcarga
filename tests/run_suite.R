## run_suite.R -- runner da SUITE INTEIRA (M8.12; AC 1). Roda, em sequencia e pelo Rscript em que for chamado (no fechamento: o `Rscript --vanilla`
## do bundle INSTALADO, Linux e Windows): testthat::test_dir sobre scripts/lib/tests (inclui os gates ex-bash do M7), tests/bundle, tests/install e
## tests/ab, e `models/<mdir>/tests/run_all_gates.R --tier fast` de cada membro de prevcarga_members() (a lista vem da topologia, nunca fixa).
## Sai != 0 se houver falha, erro, teste vazio ou SKIP cujo motivo nao esteja na lista permitida (PC_SUITE_ALLOWED_SKIP).
##
## Uso: Rscript --vanilla tests/run_suite.R [--root=<repo>] [--lib=<dir de testthat e deps>] [--dirs=a,b] [--no-models] [--list-skips]
##   --root     raiz do repo (default: o diretorio pai de tests/ deste arquivo)
##   --lib      acrescentado ao .libPaths() do processo e passado aos filhos (o bundle e zero-dep: testthat e deps vem de fora, sem variavel de ambiente do produto)
##   --dirs     subconjunto dos diretorios de testthat (diagnostico; o fechamento roda todos)
##   --no-models  nao roda os run_all_gates.R (diagnostico)
## Ferramenta de DEV/QA (fora do bundle); nao e codigo de produto.

PC_SUITE_DIRS <- c("scripts/lib/tests", "tests/bundle", "tests/install", "tests/ab")
## Motivos de skip aceitos: os 4 do ticket + adiamentos declarados com dono: o gate da VM Windows ("[DEFERRED windows-vm do M8", "[DEFERRED M8" do build Windows),
## o build duplo do mesmo commit (M8.1; "dist/repro-1", ja provado no M8.1), o teste que nao consegue simular a ausencia de pacote com a library do bundle
## e os bracos do A/B que dependem da referencia do oraculo (matriz completa nao rodada: CORTE-BLOQUEADO, dono Gabriel) ou da perna Windows (DEFERIDO ao gate windows-vm). Qualquer outro skip reprova.
PC_SUITE_ALLOWED_SKIP <- c("nao-windows", "nao-linux", "deck-real-ausente", "[TBD: ADR-0036]", "[DEFERRED windows-vm do M8", "[DEFERRED M8",
                          "dist/repro-1", "nao ha como simular a ausencia",
                          "[TBD] referencia do oraculo ausente", "sem options(prevcarga.ab.shared)", "DEFERIDO ao gate windows-vm")

pc_suite_arg <- function(args, name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1L]]) else default
}

## Linhas (1 por teste) de um testthat_results: falhas, erros e motivos de skip (um por expectativa de skip).
pc_suite_rows <- function(res, dir) {
  rows <- lapply(unclass(res), function(t) {
    ex <- t$results
    data.frame(dir = dir, file = as.character(t$file), test = as.character(t$test),
               failed = sum(vapply(ex, inherits, NA, what = "expectation_failure")),
               errored = sum(vapply(ex, inherits, NA, what = "expectation_error")),
               skip_reason = paste(vapply(Filter(function(x) inherits(x, "expectation_skip"), ex), function(x) conditionMessage(x), ""), collapse = " || "),
               stringsAsFactors = FALSE)
  })
  if (!length(rows)) return(data.frame(dir = character(), file = character(), test = character(), failed = integer(), errored = integer(), skip_reason = character()))
  do.call(rbind, rows)
}

## Skip fora da lista: motivo que nao contem nenhum token permitido (comparacao literal). Teste vazio ("empty test") tambem reprova.
pc_suite_bad_skips <- function(rows, allowed = PC_SUITE_ALLOWED_SKIP) {
  s <- rows[nzchar(rows$skip_reason), , drop = FALSE]
  ok <- vapply(s$skip_reason, function(r) any(vapply(allowed, function(a) grepl(a, r, fixed = TRUE), NA)) && !grepl("empty test", r, fixed = TRUE), NA)
  s[!ok, , drop = FALSE]
}

## Veredito: list(ok, falhas, erros, skips_ruins, gates_ruins)
pc_suite_verdict <- function(rows, gates = integer(), allowed = PC_SUITE_ALLOWED_SKIP) {
  bs <- pc_suite_bad_skips(rows, allowed)
  list(ok = sum(rows$failed) == 0L && sum(rows$errored) == 0L && nrow(bs) == 0L && all(gates == 0L),
       falhas = sum(rows$failed), erros = sum(rows$errored), skips_ruins = bs, gates_ruins = names(gates)[gates != 0L])
}

pc_suite_main <- function(args) {
  root <- normalizePath(pc_suite_arg(args, "root", file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])), "..")), winslash = "/", mustWork = TRUE)
  lib <- pc_suite_arg(args, "lib")
  if (!is.null(lib)) .libPaths(c(normalizePath(lib, winslash = "/", mustWork = TRUE), .libPaths()))
  if (!requireNamespace("testthat", quietly = TRUE)) { cat("run_suite: testthat ausente (use --lib=<dir>)\n"); return(2L) }
  dirs <- strsplit(pc_suite_arg(args, "dirs", paste(PC_SUITE_DIRS, collapse = ",")), ",", fixed = TRUE)[[1L]]
  if (identical(dirs, "none")) dirs <- character()                     # so os gates dos membros (diagnostico)
  cat(sprintf("run_suite: Rscript=%s R=%s root=%s\n", file.path(R.home("bin"), "Rscript"), as.character(getRversion()), root))
  all_rows <- list()
  for (d in dirs) {
    cat(sprintf("== testthat: %s\n", d))
    res <- testthat::test_dir(file.path(root, d), reporter = "summary", stop_on_failure = FALSE, stop_on_warning = FALSE)
    all_rows[[d]] <- pc_suite_rows(res, d)
    r <- all_rows[[d]]
    cat(sprintf("== %s: testes=%d falhas=%d erros=%d skips=%d\n", d, nrow(r), sum(r$failed), sum(r$errored), sum(nzchar(r$skip_reason))))
  }
  rows <- if (length(all_rows)) do.call(rbind, all_rows) else pc_suite_rows(list(), "none")
  gates <- integer()
  if (!isTRUE("--no-models" %in% args)) {
    lib_dir <- file.path(root, "scripts", "lib")
    e <- new.env(parent = globalenv())
    for (f in c("errors", "runtime", "topology")) sys.source(file.path(lib_dir, paste0(f, ".R")), envir = e)
    mdirs <- e$prevcarga_members(root = root)$mdir
    rs <- file.path(R.home("bin"), "Rscript")
    envv <- if (!is.null(lib)) paste0("R_LIBS=", normalizePath(lib, winslash = "/")) else character()
    for (m in mdirs) {
      cat(sprintf("== gates: models/%s/tests/run_all_gates.R --tier fast\n", m))
      out <- suppressWarnings(system2(rs, c("--vanilla", shQuote(file.path(root, "models", m, "tests", "run_all_gates.R")), "--tier", "fast"), stdout = TRUE, stderr = TRUE, env = envv))
      st <- if (is.null(attr(out, "status"))) 0L else as.integer(attr(out, "status"))
      cat(utils::tail(out, 8L), sep = "\n")
      gates[[m]] <- st
    }
  }
  v <- pc_suite_verdict(rows, gates)
  ## relatorio: skips por motivo (permitidos e nao) para auditoria
  sk <- rows[nzchar(rows$skip_reason), , drop = FALSE]
  if (nrow(sk)) {
    cat(sprintf("== skips: %d (permitidos: %d)\n", nrow(sk), nrow(sk) - nrow(v$skips_ruins)))
    if (isTRUE("--list-skips" %in% args)) for (i in seq_len(nrow(sk))) cat(sprintf("   [%s] %s :: %s :: %s\n", sk$dir[i], sk$file[i], sk$test[i], sk$skip_reason[i]))
  }
  if (nrow(v$skips_ruins)) for (i in seq_len(nrow(v$skips_ruins))) cat(sprintf("SKIP FORA DA LISTA: [%s] %s :: %s :: %s\n", v$skips_ruins$dir[i], v$skips_ruins$file[i], v$skips_ruins$test[i], v$skips_ruins$skip_reason[i]))
  if (length(v$gates_ruins)) cat("GATES COM FALHA:", paste(v$gates_ruins, collapse = ", "), "\n")
  cat(sprintf("run_suite: %s (falhas=%d erros=%d skips_fora_da_lista=%d gates_com_falha=%d)\n", if (v$ok) "PASS" else "FAIL",
              v$falhas, v$erros, nrow(v$skips_ruins), length(v$gates_ruins)))
  if (v$ok) 0L else 1L
}

if (!interactive() && !isTRUE(getOption("prevcarga.suite.no_main")) && length(grep("^--file=.*run_suite\\.R$", commandArgs(FALSE)))) quit(status = pc_suite_main(commandArgs(TRUE)), save = "no")
