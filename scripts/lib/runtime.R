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

## sha256 (chr 64) do conteudo do arquivo (um link e seguido: vale o conteudo do alvo; o bundle de producao nao tem links).
.prevcarga_runtime_file_sha <- function(path) digest::digest(file = path, algo = "sha256")

prevcarga_runtime_id <- function(path_runtime_json) {
  if (!file.exists(path_runtime_json)) return(NULL)
  x <- jsonlite::read_json(path_runtime_json, simplifyVector = FALSE)
  digest::digest(prevcarga_runtime_json_canonical(x), algo = "sha256", serialize = FALSE)
}

## RUNTIME.json do bundle instalado (em `root` ou um nivel acima, como no doctor): list(bundle_root, rt = conteudo lido,
## runtime_id, platform, version) ou NULL fora do bundle (arvore de dev). Unico leitor usado pelo doctor e pelo banner.
prevcarga_bundle_runtime <- function(root) {
  if (is.null(root) || !length(root) || is.na(root[[1L]])) return(NULL)
  cand <- c(root, dirname(root))
  broot <- cand[file.exists(file.path(cand, "RUNTIME.json"))][1L]
  if (is.na(broot)) return(NULL)
  f <- file.path(broot, "RUNTIME.json")
  rt <- jsonlite::read_json(f, simplifyVector = FALSE)
  list(bundle_root = broot, rt = rt, runtime_id = prevcarga_runtime_id(f),
       platform = as.character(rt$platform), version = as.character(rt$version))
}

## Identidade do codigo que roda (bundle_id do run_manifest da entrega; chr 64). Bundle instalado (RUNTIME.json em `root` ou um
## nivel acima, como no doctor) => o runtime_id. Arvore de dev (sem RUNTIME.json) => sha256 das linhas `<path>\t<sha256>` (radix)
## da MESMA selecao do code/ do bundle (scripts/tools/bundle_manifest.R pc_code_tree: scripts, models, config, renv.lock; fora
## scripts/tools, scripts/data-prep, tests/, *.md e models/*/models/): mesmo codigo => mesmo id, qualquer byte de codigo muda o id.
prevcarga_bundle_id <- function(root) {
  cand <- c(root, dirname(root))
  broot <- cand[file.exists(file.path(cand, "RUNTIME.json"))][1L]
  if (!is.na(broot)) return(prevcarga_runtime_id(file.path(broot, "RUNTIME.json")))
  fs <- unlist(lapply(c("scripts", "models", "config"), function(d)
    if (dir.exists(file.path(root, d))) file.path(d, list.files(file.path(root, d), recursive = TRUE, all.files = TRUE)) else character()))
  fs <- c(fs, if (file.exists(file.path(root, "renv.lock"))) "renv.lock")
  fs <- sort(fs[!grepl("^scripts/(tools|data-prep)/|(^|/)tests/|\\.md$|^models/[^/]+/models/", fs)], method = "radix")
  sha <- vapply(file.path(root, fs), .prevcarga_runtime_file_sha, "", USE.NAMES = FALSE)
  digest::digest(paste0(fs, "\t", sha, "\n", collapse = ""), algo = "sha256", serialize = FALSE)
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
      if (!file.exists(p)) bad <- c(bad, f$path)
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

## --- Doctor: plataforma, bundle instalado e artefatos sincronizados (M8.7) -----------------------------------
## prevcarga_platform_check(ctx) devolve list(rows, classes, campos): `rows` (check/status/found/effective, vocabulario
## PASS/WARN/FAIL/N/A do doctor), `classes` (classe de erro por linha, NA nas que nao falham) e `campos` (JSON do doctor:
## runtime_id, version, platform, install_root, state_dir, artefatos[], state_bytes). So le arquivos; sem env, sem rede.
## Bundle instalado = RUNTIME.json em `ctx$root` ou um nivel acima (o `code/` do bundle fica sob a raiz do bundle). Na arvore de
## dev (sem RUNTIME.json) todo item de bundle e `N/A (nao-instalado)`. `current` = arquivo-ponteiro (1 linha, a versao) em <raiz>/current, com o bundle em <raiz>/versions/<v> (layout do
## install.sh do M8.3), sem symlink; difere da versao instalada => WARN. Classes: plataforma/R/integridade = bundle (6),
## artefato ausente = state (13), sha divergente do SHA256SUMS = integrity (9).
.prevcarga_pc_na <- "N/A (nao-instalado)"

.prevcarga_host_platform <- function() {
  os <- if (identical(.Platform$OS.type, "windows")) "windows" else if (grepl("linux", R.version$os)) "linux" else R.version$os
  paste0(os, "-", R.version$arch)
}

## glibc do host (Linux): "2.39" ou NA se nao der para saber.
.prevcarga_host_glibc <- function() {
  if (!grepl("linux", R.version$os)) return(NA_character_)
  out <- tryCatch(suppressWarnings(system2("getconf", "GNU_LIBC_VERSION", stdout = TRUE, stderr = FALSE)),
                  error = function(e) character())
  v <- sub("^glibc[[:space:]]+", "", out[grepl("^glibc", out)])
  if (length(v)) v[[1L]] else NA_character_
}

.prevcarga_pc_row <- function(check, status, found = "-", effective = "-", class = NA_character_) {
  data.frame(check = check, status = status, found = found, effective = effective, class = class, stringsAsFactors = FALSE)
}

## latest.txt sob <state_dir>/artifacts: v_<hash> ausente => "ausente"; SHA256SUMS ('<sha>  <arquivo>') divergente => "sha_divergente".
.prevcarga_artifact_scan <- function(state_dir) {
  base <- file.path(state_dir, "artifacts")
  lat <- if (dir.exists(base)) list.files(base, pattern = "^latest\\.txt$", recursive = TRUE, all.files = TRUE) else character()
  lat <- sort(lat, method = "radix")
  rows <- lapply(lat, function(rel) {
    d <- dirname(rel)
    ver <- trimws(readLines(file.path(base, rel), n = 1L, warn = FALSE))
    vdir <- file.path(base, d, ver)
    st <- "ok"; arq <- NA_character_
    if (!length(ver) || !nzchar(ver) || !dir.exists(vdir)) st <- "ausente"
    else if (file.exists(file.path(vdir, "SHA256SUMS"))) {
      sums <- readLines(file.path(vdir, "SHA256SUMS"), warn = FALSE)
      sums <- sums[nzchar(trimws(sums))]
      for (l in sums) {
        sha <- sub("[[:space:]].*$", "", l)
        f <- sub("^[0-9a-f]+[[:space:]]+\\*?", "", l)
        got <- if (file.exists(file.path(vdir, f))) .prevcarga_runtime_file_sha(file.path(vdir, f)) else NA_character_
        if (is.na(got) || !identical(got, sha)) { st <- "sha_divergente"; arq <- f; break }
      }
    }
    c(list(dir = d, versao = ver, status = st), if (!is.na(arq)) list(arquivo = arq))
  })
  rows
}

.prevcarga_state_bytes <- function(state_dir) {
  if (is.null(state_dir) || !dir.exists(state_dir)) return(structure(list(), names = character()))
  subs <- sort(list.dirs(state_dir, full.names = FALSE, recursive = FALSE), method = "radix")
  out <- lapply(subs, function(s) {
    f <- list.files(file.path(state_dir, s), recursive = TRUE, all.files = TRUE, full.names = TRUE)
    sum(as.numeric(file.size(f)), na.rm = TRUE)
  })
  stats::setNames(out, subs)
}

prevcarga_platform_check <- function(ctx, host = .prevcarga_host_platform(), glibc = .prevcarga_host_glibc(),
                                     r_version = as.character(getRversion())) {
  root <- ctx$root
  br <- prevcarga_bundle_runtime(root)
  installed <- !is.null(br)
  ## install_root = raiz da INSTALACAO do usuario (<R>, M8.3: ctx$install_root); fora do layout instalado, a raiz do codigo.
  ## bundle_root = <R>/versions/<v> (onde mora o RUNTIME.json), so quando instalado.
  campos <- list(runtime_id = NULL, version = ctx$version, platform = host,
                 install_root = if (is.null(ctx$install_root)) root else ctx$install_root, bundle_root = NULL,
                 state_dir = ctx$state_dir, artefatos = list(), state_bytes = .prevcarga_state_bytes(ctx$state_dir))
  na_names <- c("bundle.plataforma", "bundle.r_version", "bundle.integridade", "bundle.versao", "bundle.current",
                "artefatos.presenca", "artefatos.sha")
  if (!installed) {
    rows <- do.call(rbind, lapply(na_names, function(n) .prevcarga_pc_row(n, "N/A", .prevcarga_pc_na, "-")))
    return(list(rows = rows, classes = rows$class, campos = campos))
  }
  rt <- br$rt; broot <- br$bundle_root
  campos$runtime_id <- br$runtime_id
  campos$version <- rt$version
  campos$bundle_root <- broot
  plat_ok <- identical(as.character(rt$platform), host)
  glibc_ok <- !grepl("^linux", host) || is.na(glibc) || utils::compareVersion(glibc, "2.35") >= 0
  r_plat <- if (!plat_ok) .prevcarga_pc_row("bundle.plataforma", "FAIL", as.character(rt$platform), host, "bundle")
            else if (!glibc_ok) .prevcarga_pc_row("bundle.plataforma", "FAIL", paste0("glibc ", glibc), "glibc >= 2.35", "bundle")
            else .prevcarga_pc_row("bundle.plataforma", "PASS", as.character(rt$platform), host)
  r_r <- .prevcarga_pc_row("bundle.r_version", if (identical(as.character(rt$r_version), r_version)) "PASS" else "FAIL",
                           as.character(rt$r_version), r_version,
                           if (identical(as.character(rt$r_version), r_version)) NA_character_ else "bundle")
  v <- prevcarga_runtime_validate(broot)
  r_int <- .prevcarga_pc_row("bundle.integridade", if (v$ok) "PASS" else "FAIL",
                             if (v$ok) "ok" else paste(utils::head(v$divergentes, 3L), collapse = ","), "RUNTIME.json files[]",
                             if (v$ok) NA_character_ else "bundle")
  r_ver <- .prevcarga_pc_row("bundle.versao", "PASS", as.character(rt$version), as.character(campos$runtime_id))
  curf <- file.path(dirname(dirname(broot)), "current")   # layout do install.sh (M8.3): <raiz>/current + <raiz>/versions/<v>/
  r_cur <- if (!file.exists(curf) || dir.exists(curf)) .prevcarga_pc_row("bundle.current", "N/A", "ausente", "-")
           else {
             cv <- trimws(readLines(curf, n = 1L, warn = FALSE))
             .prevcarga_pc_row("bundle.current", if (identical(cv, as.character(rt$version))) "PASS" else "WARN", cv, as.character(rt$version))
           }
  art <- .prevcarga_artifact_scan(ctx$state_dir)
  campos$artefatos <- art
  miss <- Filter(function(a) identical(a$status, "ausente"), art)
  bad <- Filter(function(a) identical(a$status, "sha_divergente"), art)
  r_pre <- .prevcarga_pc_row("artefatos.presenca", if (length(miss)) "FAIL" else "PASS",
                             if (length(miss)) paste0(miss[[1L]]$dir, "/", miss[[1L]]$versao, " ausente") else sprintf("%d latest.txt", length(art)),
                             "v_<hash> presente", if (length(miss)) "state" else NA_character_)
  r_sha <- .prevcarga_pc_row("artefatos.sha", if (length(bad)) "FAIL" else "PASS",
                             if (length(bad)) paste0(bad[[1L]]$dir, "/", bad[[1L]]$arquivo) else "ok", "SHA256SUMS",
                             if (length(bad)) "integrity" else NA_character_)
  rows <- rbind(r_plat, r_r, r_int, r_ver, r_cur, r_pre, r_sha)
  rownames(rows) <- NULL
  list(rows = rows, classes = rows$class, campos = campos)
}

## Anexa as linhas do platform_check ao resultado do construtor do doctor (introspect.R) e recalcula counts/offenders/exit pela
## precedencia do catalogo (ADR-0040): FAIL de bundle=6, integrity=9, state=13 entram junto das classes ja decididas pelo construtor.
prevcarga_doctor_extend <- function(d, ctx, pc = prevcarga_platform_check(ctx)) {
  checks <- rbind(d$checks, pc$rows[c("check", "status", "found", "effective")])
  rownames(checks) <- NULL
  counts <- lapply(stats::setNames(c("PASS", "WARN", "FAIL", "N/A"), c("PASS", "WARN", "FAIL", "N/A")),
                   function(s) as.integer(sum(checks$status == s)))
  code0 <- as.integer(attr(d, "exit_code"))
  cls0 <- names(prevcarga_exit_codes)[match(code0, prevcarga_exit_codes)]
  classes <- c(if (code0 != 0L) cls0, unique(stats::na.omit(pc$classes[pc$rows$status == "FAIL"])))
  code <- if (length(classes)) unname(prevcarga_exit_codes[[prevcarga_exit_precedence(classes)]]) else 0L
  res <- c(list(schema_version = d$schema_version, checks = checks, counts = counts,
                offenders = checks$check[checks$status == "FAIL"]), pc$campos)
  structure(res, exit_code = as.integer(code))
}
