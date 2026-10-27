#!/usr/bin/env Rscript
# Spike R 4.5.3 (ADR-0036): executa o ORACULO INTOCADO (/opt/ons-source/prevcargadessem3) em R 4.2.3
# e R 4.5.3 lado a lado, com o MESMO renv.lock restaurado em bibliotecas proprias em <out>/lib-<versao>/,
# e compara o y_pred por modelo. Ferramenta dev-only Linux (fora do bundle).
#
#   Rscript --vanilla scripts/tools/spike_r453.R --data=<deck> --origin=<O> --area=SECO \
#       --r-a=<Rscript 4.2.3> --r-b=<Rscript 4.5.3> --out=<dir fora do oraculo>
#
# Saidas (ASCII) em --out: delta.tsv, verdict.txt, lib-<versao>/restore.tsv, <versao>/<modelo>/ (LONG e logs).
# O oraculo e somente lido/executado; sua impressao digital e conferida antes e depois.
# As variaveis de ambiente abaixo existem so no processo-FILHO do oraculo (interface do run_job.sh);
# o clean-room nao le env.

spike_oracle_default <- "/opt/ons-source/prevcargadessem3"
spike_models <- c("random-forest", "lgbm", "holt-winters", "dessem-linear", "dessem-regdin")

spike_guard_out <- function(out, oracle = spike_oracle_default) {
  o <- normalizePath(oracle, winslash = "/", mustWork = TRUE)
  d <- normalizePath(out, winslash = "/", mustWork = FALSE)
  if (identical(d, o) || startsWith(d, paste0(o, "/")))
    stop("spike: --out dentro do oraculo e proibido", call. = FALSE)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  normalizePath(d, winslash = "/", mustWork = TRUE)
}

## Impressao digital da arvore do oraculo: git status + caminhos/mtimes (sem .git, cujo index o proprio git retoca).
spike_tree_fingerprint <- function(oracle = spike_oracle_default) {
  o <- normalizePath(oracle, winslash = "/", mustWork = TRUE)
  st <- suppressWarnings(system2("git", c("--no-optional-locks", "-C", o, "status", "--porcelain"),
                                 stdout = TRUE, stderr = TRUE))
  fl <- sort(list.files(o, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE), method = "radix")
  fl <- fl[!(fl == ".git" | startsWith(fl, ".git/"))]
  mt <- format(file.info(file.path(o, fl))$mtime, "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
  paste(c(st, "--", paste(fl, mt)), collapse = "\n")
}

spike_check_fingerprint <- function(before, oracle = spike_oracle_default) {
  if (!identical(before, spike_tree_fingerprint(oracle)))
    stop("spike: o oraculo foi alterado durante a rodada (impressao digital difere)", call. = FALSE)
  invisible(TRUE)
}

spike_r_version <- function(rscript) {
  v <- suppressWarnings(system2(rscript, c("--vanilla", "-e", shQuote("cat(as.character(getRversion()))")),
                                stdout = TRUE, stderr = FALSE))
  if (!length(v) || !nzchar(v[[1]])) stop("spike: nao consegui obter a versao de ", rscript, call. = FALSE)
  v[[length(v)]]
}

## Fecho de dependencias de `pkgs` dentro do lock (nomes).
spike_closure <- function(lock, pkgs) {
  seen <- character()
  todo <- pkgs
  while (length(todo)) {
    p <- todo[[1]]; todo <- todo[-1]
    if (p %in% seen) next
    seen <- c(seen, p)
    req <- lock$Packages[[p]]$Requirements
    todo <- c(todo, intersect(unlist(req), names(lock$Packages)))
  }
  seen
}

## Restaura o renv.lock do oraculo (somente leitura) numa biblioteca propria. Uma falha de compilacao nao
## aborta: o pacote (e quem depende dele) e marcado `failed` e o restore segue com o resto.
## Escreve <lib_dir>/restore.tsv (package, version, status ok|failed|skipped, detail).
## relock = TRUE: NAO honra as versoes do lock; instala os mesmos NOMES nas versoes mais novas do `repos`
## (arm de diagnostico quando o lock nao compila no R alvo). Idempotente: restore.tsv existente e reutilizado.
spike_prepare_lib <- function(rscript, lockfile, lib_dir, repos = NULL, max_rounds = 40L, relock = FALSE) {
  if (file.exists(file.path(lib_dir, "restore.tsv")))
    return(utils::read.delim(file.path(lib_dir, "restore.tsv"), stringsAsFactors = FALSE, colClasses = "character"))
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  lib_dir <- normalizePath(lib_dir, winslash = "/", mustWork = TRUE)
  child <- file.path(lib_dir, "restore-child.R")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "lockfile <- args[[1]]; lib <- args[[2]]; repos <- if (length(args) > 2L) args[[3]] else ''",
    "options(HTTPUserAgent = sprintf('R/%s R (%s)', getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))",
    "dt <- read.delim(file.path(lib, 'lock-deps.tsv'), stringsAsFactors = FALSE, colClasses = 'character')",
    "lock <- list(Packages = lapply(setNames(dt$package, dt$package), function(p) list(Version = dt$version[dt$package == p], Requirements = strsplit(dt$requires[dt$package == p], ',', fixed = TRUE)[[1]])))",
    "closure <- function(pkgs) { seen <- character(); todo <- pkgs",
    "  while (length(todo)) { p <- todo[[1]]; todo <- todo[-1]; if (p %in% seen) next; seen <- c(seen, p)",
    "    todo <- c(todo, intersect(unlist(lock$Packages[[p]]$Requirements), names(lock$Packages))) }",
    "  seen }",
    "all <- names(lock$Packages); failed <- character(); detail <- list(); skipped <- character()",
    "relock <- length(args) > 3L && identical(args[[4]], 'relock')",
    "if (relock) { options(repos = c(CRAN = repos))",
    "  install.packages(setdiff(all, rownames(installed.packages(lib.loc = .Library, noCache = TRUE))), lib = lib, Ncpus = 8L) }",
    "for (round in seq_len(if (relock) 0L else as.integer(Sys.getenv('SPIKE_ROUNDS', '40')))) {",
    "  bad <- failed; for (f in failed) bad <- union(bad, all[vapply(all, function(p) f %in% closure(p), logical(1))])",
    "  pending <- setdiff(all, bad)",
    "  rr <- try(renv::restore(project = dirname(lib), lockfile = lockfile, library = lib, packages = pending,",
    "                          repos = if (nzchar(repos)) c(CRAN = repos) else NULL, prompt = FALSE, clean = FALSE), silent = TRUE)",
    "  if (!inherits(rr, 'try-error')) break",
    "  msg <- as.character(attr(rr, 'condition')$message)",
    "  lg <- unlist(strsplit(readLines(file.path(lib, 'restore.log'), warn = FALSE), rawToChar(as.raw(13)), fixed = TRUE))",
    "  hit <- grep(']: install failed', lg, value = TRUE, fixed = TRUE); hit <- hit[startsWith(hit, '- [')]",
    "  new <- setdiff(unique(substr(hit, 4L, regexpr(']', hit, fixed = TRUE) - 1L)), failed)",
    "  if (!length(new)) { detail[['<restore>']] <- substr(gsub('[^ -~]', '?', gsub('[[:space:]]+', ' ', msg)), 1, 300); break }",
    "  for (m in new) { failed <- c(failed, m)",
    "    h <- which(startsWith(lg, paste0('# ', m, ' ')))",
    "    ce <- if (length(h)) grep(': error', lg[seq(h[[length(h)]], min(length(lg), h[[length(h)]] + 40L))], value = TRUE, fixed = TRUE) else character()",
    "    detail[[m]] <- substr(gsub('[^ -~]', '?', if (length(ce)) ce[[1]] else 'install failed'), 1, 200) }",
    "}",
    "inst <- installed.packages(lib.loc = lib, noCache = TRUE)[, 'Version']",
    "sysv <- installed.packages(lib.loc = .Library, noCache = TRUE)[, 'Version']",
    "rows <- lapply(all, function(p) {",
    "  st <- if (p %in% failed) 'failed' else if (p %in% names(inst)) 'ok' else if (p %in% names(sysv)) 'system' else 'skipped'",
    "  d <- if (!is.null(detail[[p]])) detail[[p]] else if (relock && st == 'ok') paste0('relocked ', inst[[p]]) else if (st == 'system') paste0('system library ', sysv[[p]]) else ''",
    "  c(package = p, version = lock$Packages[[p]]$Version, status = st, detail = d) })",
    "tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)",
    "if (!is.null(detail[['<restore>']])) tab <- rbind(tab, data.frame(package = '<restore>', version = '', status = 'failed', detail = detail[['<restore>']]))",
    "write.table(tab, file.path(lib, 'restore.tsv'), sep = '\\t', quote = FALSE, row.names = FALSE)"
  ), child)
  lock <- jsonlite::fromJSON(lockfile, simplifyVector = FALSE)
  deps <- data.frame(package = names(lock$Packages),
                     version = vapply(lock$Packages, function(p) p$Version, ""),
                     requires = vapply(lock$Packages, function(p) paste(unlist(p$Requirements), collapse = ","), ""),
                     stringsAsFactors = FALSE)
  utils::write.table(deps, file.path(lib_dir, "lock-deps.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  rver <- spike_r_version(rscript)
  ulib <- file.path(path.expand("~"), "R", "x86_64-pc-linux-gnu-library", sub("^([0-9]+\\.[0-9]+).*$", "\\1", rver))
  ## ulib so entra no restore (para achar o proprio renv); a rodada do modelo NAO a ve.
  env <- c("R_LIBS_USER=/nonexistent", "RENV_CONFIG_CACHE_ENABLED=FALSE", "RENV_CONFIG_SANDBOX_ENABLED=FALSE",
           paste0("R_LIBS=", lib_dir, ":", ulib), paste0("SPIKE_ROUNDS=", max_rounds))
  rc <- system2(rscript, c("--vanilla", shQuote(child), shQuote(lockfile), shQuote(lib_dir), if (!is.null(repos)) shQuote(repos), if (relock) "relock"),
                env = env, stdout = file.path(lib_dir, "restore.log"), stderr = file.path(lib_dir, "restore.log"))
  tsv <- file.path(lib_dir, "restore.tsv")
  if (!file.exists(tsv)) stop("spike: restore nao gerou restore.tsv (rc=", rc, "); ver ", file.path(lib_dir, "restore.log"), call. = FALSE)
  utils::read.delim(tsv, stringsAsFactors = FALSE, colClasses = "character")
}

spike_model_deps <- function(model, oracle = spike_oracle_default) {
  f <- file.path(oracle, "models", model, "job.json")
  j <- jsonlite::fromJSON(f, simplifyVector = TRUE)
  as.character(j$r_deps)
}

## Pacotes do lock que nao instalaram e que sao (fecho de) dependencia do modelo.
spike_model_compile_failures <- function(model, restore_tab, lockfile, oracle = spike_oracle_default) {
  lock <- jsonlite::fromJSON(lockfile, simplifyVector = FALSE)
  deps <- intersect(spike_model_deps(model, oracle), names(lock$Packages))
  cl <- spike_closure(lock, deps)
  bad <- restore_tab$package[!restore_tab$status %in% c("ok", "system")]
  sort(intersect(cl, bad), method = "radix")
}

## Executa o oraculo (run_job.sh, modo local, sem S3) para UM modelo e uma versao de R.
spike_run_model <- function(model, rscript, lib_dir, data, origin, area, out, oracle = spike_oracle_default) {
  res_dir <- file.path(out, model)
  dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)
  env <- c(paste0("MODEL=", model), paste0("AREA=", area), paste0("FROM=", origin), paste0("TO=", origin),
           paste0("LOCAL_DATA_DIR=", normalizePath(data, mustWork = TRUE)), paste0("LOCAL_RESULTS_DIR=", res_dir),
           paste0("RSCRIPT=", rscript), paste0("R_LIBS=", lib_dir), "R_LIBS_USER=/nonexistent",
           paste0("WORKDIR=", file.path(out, "work")), "FORCE=1",
           "TZ=UTC", "LANG=C.UTF-8", "LC_ALL=C.UTF-8")   # ambiente do Dockerfile do oraculo
  log <- file.path(res_dir, "run.log")
  rc <- suppressWarnings(system2("bash", file.path(oracle, "jobs", "run_job.sh"), env = env, stdout = log, stderr = log))
  long <- list.files(file.path(res_dir, "results"), pattern = "^long\\.parquet$", recursive = TRUE, full.names = TRUE)
  tail_log <- if (file.exists(log)) utils::tail(readLines(log, warn = FALSE), 3L) else character()
  list(rc = rc, long = if (length(long)) long[[1]] else NA_character_,
       detail = gsub("[^ -~]", "?", paste(tail_log, collapse = " | ")))
}

spike_sha <- function(x) digest::digest(writeBin(as.double(x), raw()), algo = "sha256", serialize = FALSE)

spike_compare <- function(a, b, keys = c("area", "forecast_origin", "target_datetime")) {
  if (!all(c(keys, "y_pred") %in% names(a)) || !all(c(keys, "y_pred") %in% names(b)))
    stop("spike: colunas ausentes (chaves e y_pred)", call. = FALSE)
  a <- as.data.frame(a)[, c(keys, "y_pred")]; b <- as.data.frame(b)[, c(keys, "y_pred")]
  m <- merge(a, b, by = keys, suffixes = c("_a", "_b"))
  if (nrow(m) != nrow(a) || nrow(m) != nrow(b) || anyDuplicated(a[keys]) || anyDuplicated(b[keys]))
    stop("spike: chaves divergentes", call. = FALSE)
  m <- m[do.call(order, unname(as.list(m[keys]))), ]
  d <- abs(m$y_pred_a - m$y_pred_b)
  rel <- d / abs(m$y_pred_a)
  rel[d == 0] <- 0
  list(n = nrow(m), max_abs = max(d), max_rel = max(rel, na.rm = TRUE),
       sha_equal = identical(spike_sha(m$y_pred_a), spike_sha(m$y_pred_b)))
}

## results: lista de listas {model, r_a, r_b, status, detail, [n, max_abs, max_rel, sha_equal]}.
spike_table <- function(results) {
  one <- function(r) data.frame(
    model = r$model, r_a = r$r_a, r_b = r$r_b,
    n = if (is.null(r$n)) NA_integer_ else as.integer(r$n),
    max_abs = if (is.null(r$max_abs)) NA_real_ else r$max_abs,
    max_rel = if (is.null(r$max_rel)) NA_real_ else r$max_rel,
    sha_equal = if (is.null(r$sha_equal)) NA else r$sha_equal,
    status = r$status, detail = gsub("[\t\r\n]+", " ", gsub("[^ -~]", "?", if (is.null(r$detail)) "" else r$detail)),
    stringsAsFactors = FALSE)
  do.call(rbind, lapply(results, one))
}

spike_verdict <- function(tbl, tol = 1e-3) {
  broken <- tbl$status != "ok"
  drift <- !broken & (is.na(tbl$max_rel) | tbl$max_rel > tol) & !(tbl$sha_equal %in% TRUE)
  if (!any(broken) && !any(drift)) {
    return(paste0("ADR-0036 confirmada: ", nrow(tbl), " modelos ok; ",
                  sum(tbl$sha_equal %in% TRUE), " com sha256 igual, os demais com divergencia <= ",
                  format(tol * 100), "% (max_rel).\n"))
  }
  l <- c("rever ADR-0036:")
  for (i in which(broken)) l <- c(l, sprintf("- %s: %s (%s)", tbl$model[i], tbl$status[i], tbl$detail[i]))
  for (i in which(drift)) l <- c(l, sprintf("- %s: divergencia max_rel=%.3g > %s", tbl$model[i], tbl$max_rel[i], format(tol)))
  paste0(paste(l, collapse = "\n"), "\n")
}

spike_read_long <- function(path) {
  x <- as.data.frame(arrow::read_parquet(path))
  x$forecast_origin <- as.character(x$forecast_origin)
  x$target_datetime <- format(x$target_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  x
}

spike_parse_args <- function(argv) {
  kv <- regmatches(argv, regexec("^--([a-z0-9-]+)=(.*)$", argv))
  ok <- lengths(kv) == 3L
  if (!all(ok)) stop("spike: argumento invalido: ", paste(argv[!ok], collapse = " "), call. = FALSE)
  out <- stats::setNames(lapply(kv, `[[`, 3L), vapply(kv, `[[`, "", 2L))
  need <- c("data", "origin", "area", "r-a", "r-b", "out")
  miss <- setdiff(need, names(out))
  if (length(miss)) stop("spike: faltam argumentos: ", paste0("--", miss, collapse = " "), call. = FALSE)
  out
}

spike_main <- function(argv = commandArgs(trailingOnly = TRUE), oracle = spike_oracle_default) {
  a <- spike_parse_args(argv)
  out <- spike_guard_out(a$out, oracle)
  fp <- spike_tree_fingerprint(oracle)
  lockfile <- file.path(oracle, "renv.lock")
  rs <- c(a[["r-a"]], a[["r-b"]])
  ver <- vapply(rs, spike_r_version, "")
  repos <- if (!is.null(a$repos)) a$repos else "https://packagemanager.posit.co/cran/__linux__/noble/2023-04-21"
  relock_b <- identical(a[["relock-b"]], "true")
  rest <- list(); libs <- character()
  for (i in 1:2) {
    libs[i] <- file.path(out, paste0("lib-", ver[i], if (i == 2L && relock_b) "-relock" else ""))
    cat(sprintf("spike: restaurando renv.lock em %s\n", libs[i]))
    rest[[i]] <- spike_prepare_lib(rs[i], lockfile, libs[i],
                                   repos = if (i == 2L && relock_b) "https://packagemanager.posit.co/cran/__linux__/noble/latest" else repos,
                                   relock = i == 2L && relock_b)
  }
  results <- list()
  for (m in spike_models) {
    cat(sprintf("spike: modelo %s\n", m))
    r <- list(model = m, r_a = ver[1], r_b = ver[2], status = "ok", detail = "")
    bad <- unique(c(spike_model_compile_failures(m, rest[[1]], lockfile, oracle),
                    spike_model_compile_failures(m, rest[[2]], lockfile, oracle)))
    if (length(bad)) {
      r$status <- "compile_failed"; r$detail <- paste(bad, collapse = ",")
    } else {
      runs <- lapply(1:2, function(i)
        spike_run_model(m, rs[i], libs[i], a$data, a$origin, a$area, file.path(out, ver[i]), oracle))
      if (any(is.na(vapply(runs, `[[`, "", "long")))) {
        r$status <- "run_failed"
        r$detail <- paste(vapply(1:2, function(i) sprintf("R %s rc=%s %s", ver[i], runs[[i]]$rc, runs[[i]]$detail), ""),
                          collapse = " || ")
      } else {
        cmp <- tryCatch(spike_compare(spike_read_long(runs[[1]]$long), spike_read_long(runs[[2]]$long)),
                        error = function(e) e)
        if (inherits(cmp, "error")) { r$status <- "run_failed"; r$detail <- conditionMessage(cmp) }
        else r <- c(r[c("model", "r_a", "r_b", "status", "detail")], cmp)
      }
    }
    results[[m]] <- r
  }
  tbl <- spike_table(unname(results))
  tsv <- file.path(out, "delta.tsv")
  utils::write.table(tbl, tsv, sep = "\t", quote = FALSE, row.names = FALSE, na = "NA")
  writeLines(sub("\n$", "", spike_verdict(tbl)), file.path(out, "verdict.txt"))
  spike_check_fingerprint(fp, oracle)
  cat(readLines(tsv), sep = "\n"); cat(readLines(file.path(out, "verdict.txt")), sep = "\n")
  invisible(tbl)
}

if (identical(environment(), globalenv()) && !interactive()) {
  status <- tryCatch({ spike_main(); 0L }, error = function(e) {
    message(gsub("[^ -~\n]", "?", conditionMessage(e))); 1L })
  quit(status = status)
}
