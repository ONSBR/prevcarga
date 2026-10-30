## runtime.R -- guarda de determinismo (M1.1; ADR-0022 runtime-first, ADR-0037 sem variaveis de ambiente)
##
## Roda PRIMEIRO em todo comando da CLI. FIXA in-process (sobrescreve o shell sujo): TZ=UTC, locale
## deterministico (LC_CTYPE UTF-8, LC_COLLATE/LC_NUMERIC = C), os 4 pins de threads = 1 (herdados por
## workers PSOCK do M7) e BLAS 1-thread. So aborta (exit 7, via prevcarga_abort de errors.R) se NAO
## conseguir fixar; um shell sujo nunca aborta por si. O pacote nao le env vars: este e o unico arquivo
## com Sys.getenv/Sys.setenv, e a unica leitura e `.prevcarga_runtime_found()`.
##
## Requer errors.R carregado antes (prevcarga_abort). O arquivo so define funcoes (sem efeito ao source).

.prevcarga_runtime_state <- new.env(parent = emptyenv())

.prevcarga_pin_vars <- c(omp = "OMP_NUM_THREADS", openblas = "OPENBLAS_NUM_THREADS",
                         mkl = "MKL_NUM_THREADS", veclib = "VECLIB_MAXIMUM_THREADS")

## --- BLAS --------------------------------------------------------------------------------------------
.prevcarga_blas_lib <- function() {
  lib <- tryCatch(La_library(), error = function(e) "")
  if (is.null(lib) || is.na(lib)) "" else as.character(lib)
}

.prevcarga_blas_reference <- function() {
  lib <- .prevcarga_blas_lib()
  !nzchar(lib) || grepl("Rblas|libRblas", basename(lib))
}

.prevcarga_has_rhpc <- function() requireNamespace("RhpcBLASctl", quietly = TRUE)

## Pool BLAS observado (int|NA): RhpcBLASctl quando instalado; senao so o BLAS de referencia vale 1.
.prevcarga_blas_threads <- function() {
  if (.prevcarga_has_rhpc())
    return(tryCatch(as.integer(RhpcBLASctl::blas_get_num_procs()), error = function(e) NA_integer_))
  if (.prevcarga_blas_reference()) 1L else NA_integer_
}

## --- Passo 1: ambiente cru, so para registro ----------------------------------------------------------
.prevcarga_runtime_found <- function() {
  v <- Sys.getenv(c("TZ", "LC_ALL", "LANG", unname(.prevcarga_pin_vars)), unset = NA)
  list(tz = unname(v["TZ"]), lc_all = unname(v["LC_ALL"]), lang = unname(v["LANG"]),
       lc = Sys.getlocale("LC_CTYPE"),
       lc_collate = Sys.getlocale("LC_COLLATE"), lc_numeric = Sys.getlocale("LC_NUMERIC"),
       omp = unname(v["OMP_NUM_THREADS"]), openblas = unname(v["OPENBLAS_NUM_THREADS"]),
       mkl = unname(v["MKL_NUM_THREADS"]), veclib = unname(v["VECLIB_MAXIMUM_THREADS"]),
       blas_threads = .prevcarga_blas_threads(), blas_lib = .prevcarga_blas_lib())
}

## --- Passo 2: setters reais injetaveis ----------------------------------------------------------------
.prevcarga_tz_observed <- function() {
  if (lubridate::hour(.POSIXct(0)) == 0L) "UTC" else format(.POSIXct(0), "%Z")
}

.prevcarga_locale_candidates <- function() {
  if (identical(.Platform$OS.type, "windows")) c(".UTF-8", "English_United States.utf8")
  else c("C.UTF-8")
}

prevcarga_runtime_setters <- function() {
  list(
    tz = function() {
      Sys.setenv(TZ = "UTC")
      .prevcarga_tz_observed()
    },
    locale = function() {
      for (lc in .prevcarga_locale_candidates()) {
        if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)))) {
          Sys.setlocale("LC_COLLATE", "C")
          Sys.setlocale("LC_NUMERIC", "C")
          return(lc)
        }
      }
      NA_character_
    },
    pins = function() {
      do.call(Sys.setenv, as.list(stats::setNames(rep("1", length(.prevcarga_pin_vars)), .prevcarga_pin_vars)))
      f <- .prevcarga_runtime_found()
      c(omp = f$omp, openblas = f$openblas, mkl = f$mkl, veclib = f$veclib)
    },
    blas = function() {
      if (.prevcarga_has_rhpc()) {
        RhpcBLASctl::blas_set_num_threads(1L)
        RhpcBLASctl::omp_set_num_threads(1L)
        return(as.integer(RhpcBLASctl::blas_get_num_procs()))
      }
      if (.prevcarga_blas_reference()) 1L else NA_integer_
    }
  )
}

## --- Avaliacao do estado efetivo ----------------------------------------------------------------------
.prevcarga_locale_ok <- function(lc) {
  is.character(lc) && length(lc) == 1L && !is.na(lc) && grepl("utf-?8", lc, ignore.case = TRUE)
}

.prevcarga_runtime_offenders <- function(eff) {
  off <- character()
  if (!identical(eff$tz, "UTC")) off <- c(off, "TZ")
  for (k in names(.prevcarga_pin_vars))
    if (!identical(as.character(eff[[k]]), "1")) off <- c(off, .prevcarga_pin_vars[[k]])
  if (!.prevcarga_locale_ok(eff$lc)) off <- c(off, "LC_CTYPE")
  if (!identical(eff$lc_collate, "C")) off <- c(off, "LC_COLLATE")
  if (!identical(eff$lc_numeric, "C")) off <- c(off, "LC_NUMERIC")
  if (!identical(eff$blas_threads, 1L)) off <- c(off, "BLAS")
  off
}

## Estado efetivo observado agora (sem alterar nada).
.prevcarga_runtime_observe <- function(found = .prevcarga_runtime_found()) {
  eff <- found
  eff$tz <- .prevcarga_tz_observed()
  eff
}

.prevcarga_safe <- function(f, na) tryCatch(f(), error = function(e) na)

## Aplica os 4 setters (nunca aborta) e monta found x effective a partir do que eles devolveram.
.prevcarga_runtime_run_setters <- function(setters) {
  found <- .prevcarga_runtime_found()
  r <- list(tz = .prevcarga_safe(setters$tz, NA_character_),
            locale = .prevcarga_safe(setters$locale, NA_character_),
            pins = .prevcarga_safe(setters$pins, NA_character_),
            blas = .prevcarga_safe(setters$blas, NA_integer_))
  eff <- .prevcarga_runtime_found()
  eff$tz <- r$tz
  eff$lc <- r$locale
  for (k in names(.prevcarga_pin_vars)) {
    p <- if (!is.null(names(r$pins))) r$pins[k] else r$pins
    eff[[k]] <- unname(p)[1L]
  }
  eff$blas_threads <- as.integer(r$blas)[1L]
  list(found = found, effective = eff, offenders = .prevcarga_runtime_offenders(eff))
}

## --- Passo 3: guarda fatal runtime-first --------------------------------------------------------------
prevcarga_runtime_enforce <- function(setters = prevcarga_runtime_setters()) {
  r <- .prevcarga_runtime_run_setters(setters)
  eff <- r$effective
  det <- list(found = r$found, effective = eff)
  if (!identical(eff$tz, "UTC"))
    prevcarga_abort("runtime.tz_unfixable", where = "TZ", details = det)
  if (!.prevcarga_locale_ok(eff$lc))
    prevcarga_abort("runtime.locale_unavailable", where = "LC_CTYPE", details = det)
  pins_ok <- all(vapply(names(.prevcarga_pin_vars), function(k) identical(as.character(eff[[k]]), "1"), NA))
  if (!pins_ok || !identical(eff$blas_threads, 1L))
    prevcarga_abort("runtime.blas_unfixable", where = "BLAS", details = det)
  .prevcarga_runtime_state$effective <- eff
  invisible(list(found = r$found, effective = eff))
}

## --- Passo 4: variantes nao fatais --------------------------------------------------------------------
## Isencao fechada do ADR-0022: doctor/status diagnosticam o ambiente sem abortar.
prevcarga_runtime_apply <- function(strict = FALSE, setters = prevcarga_runtime_setters()) {
  if (isTRUE(strict)) {
    r <- prevcarga_runtime_enforce(setters)
    return(invisible(list(ok = TRUE, found = r$found, effective = r$effective, offenders = character())))
  }
  r <- .prevcarga_runtime_run_setters(setters)
  .prevcarga_runtime_state$effective <- r$effective
  list(ok = length(r$offenders) == 0L, found = r$found, effective = r$effective, offenders = r$offenders)
}

prevcarga_runtime_check <- function() {
  found <- .prevcarga_runtime_found()
  eff <- .prevcarga_runtime_observe(found)
  off <- .prevcarga_runtime_offenders(eff)
  list(ok = length(off) == 0L, found = found, effective = eff, offenders = off)
}

prevcarga_runtime_stamp <- function() {
  eff <- .prevcarga_runtime_state$effective
  if (is.null(eff))
    prevcarga_abort("generic.unexpected", where = "prevcarga_runtime_stamp",
                    details = list(reason = "stamp chamado antes de enforce/apply"))
  list(tz = eff$tz, lc = eff$lc, blas_threads = eff$blas_threads)
}

## --- Raiz da instalacao, sem env ----------------------------------------------------------------------
prevcarga_install_root <- function(args = commandArgs(FALSE)) {
  f <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(f) != 1L) prevcarga_abort("runtime.install_root_unresolved", where = "--file=")
  f <- gsub("\\", "/", f, fixed = TRUE)
  root <- dirname(dirname(normalizePath(f, winslash = "/", mustWork = FALSE)))
  if (!file.exists(file.path(root, "scripts", "lib", "runtime.R")))
    prevcarga_abort("runtime.install_root_unresolved", where = root)
  root
}

## --- Contrato do RUNTIME.json do bundle (M8.1) ----------------------------------------------------------
## Fonte unica usada no build (scripts/tools/bundle_manifest.R) e no doctor/validacao do bundle instalado.
## O runtime_id e content-addressed: sha256 da serializacao canonica SEM `built_at` (campo lateral, nunca relogio:
## o build o recebe de fora ou o omite). Nenhuma leitura de variavel de ambiente aqui.

## Ordena chaves de listas nomeadas recursivamente (radix); listas sem nome (arrays) mantem a ordem.
.prevcarga_runtime_sort_keys <- function(x) {
  if (!is.list(x)) return(x)
  if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
  lapply(x, .prevcarga_runtime_sort_keys)
}

## with_built_at = FALSE (default) e a forma que entra no runtime_id; TRUE e a forma gravada em RUNTIME.json.
prevcarga_runtime_json_canonical <- function(x, with_built_at = FALSE) {
  if (!isTRUE(with_built_at)) x$built_at <- NULL
  if (length(x$files))
    x$files <- x$files[order(vapply(x$files, function(f) as.character(f[["path"]]), ""), method = "radix")]
  x <- .prevcarga_runtime_sort_keys(x)
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
  enc2utf8(paste0(gsub("\r\n", "\n", as.character(txt), fixed = TRUE), "\n"))
}

## sha256 (chr 64) do conteudo; symlink = sha256 do texto "symlink:<alvo>" (nao segue o link: o alvo pode ser relativo/pendente).
.prevcarga_runtime_file_sha <- function(path) {
  tgt <- Sys.readlink(path)
  if (nzchar(tgt))
    return(digest::digest(paste0("symlink:", tgt), algo = "sha256", serialize = FALSE))
  digest::digest(file = path, algo = "sha256")
}

prevcarga_runtime_id <- function(path_runtime_json) {
  if (!file.exists(path_runtime_json)) return(NULL)
  x <- jsonlite::read_json(path_runtime_json, simplifyVector = FALSE)
  digest::digest(prevcarga_runtime_json_canonical(x), algo = "sha256", serialize = FALSE)
}

## Recalcula o sha256 de TODO files[] sob `root` (raiz do bundle, onde mora RUNTIME.json). Sem RUNTIME.json => ok = FALSE com
## divergentes = "RUNTIME.json" (arvore de dev nao e um bundle). strict = TRUE aborta com bundle.manifest_mismatch (exit 6).
prevcarga_runtime_validate <- function(root, strict = FALSE) {
  mf <- file.path(root, "RUNTIME.json")
  bad <- character()
  if (!file.exists(mf)) bad <- "RUNTIME.json"
  else {
    fl <- jsonlite::read_json(mf, simplifyVector = FALSE)$files
    for (f in fl) {
      p <- file.path(root, f$path)
      if (!file.exists(p) && !nzchar(Sys.readlink(p))) bad <- c(bad, f$path)
      else if (!identical(.prevcarga_runtime_file_sha(p), f$sha256)) bad <- c(bad, f$path)
    }
  }
  bad <- sort(unique(bad), method = "radix")
  if (strict && length(bad))
    prevcarga_abort("bundle.manifest_mismatch", where = root, details = list(paths = as.list(bad)))
  list(ok = length(bad) == 0L, divergentes = bad)
}

## Pacotes obrigatorios do bundle: aborta (exit 6) no PRIMEIRO ausente, com o nome em details.
prevcarga_bundle_require <- function(pkgs) {
  for (p in pkgs)
    if (!requireNamespace(p, quietly = TRUE))
      prevcarga_abort("bundle.package_missing", where = p, details = list(package = p))
  invisible(pkgs)
}
