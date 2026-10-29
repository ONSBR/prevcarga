#!/usr/bin/env Rscript
## plugin_golden.R -- gerador do golden do ORACULO por unidade de units.yaml (P.0f). Ferramenta de DESENVOLVIMENTO
## (so Linux, fora do bundle). O oraculo e so lido/executado: cada unidade roda 2x num processo FILHO (system2 com vetor
## de argumentos), com o `oracle_env` da unidade passado SO ao filho (o pai nunca ganha a variavel e nao le ambiente).
##
##   Rscript --vanilla scripts/tools/plugin_golden.R --model <m>|_dessem-engine --units <units.yaml> --oracle <dir>
##       [--variant <v>] [--out <dir>] [--root <models>] [--data <store>] [--rscript <Rscript>] [--format text|json]
##
## Sem switch por modelo: roda `<root>/<model>/tests/parity/oracle_driver.R` (contrato `--oracle <dir> --unit <unit.json>
## --out <dir> --available-until <ISO>`; o driver grava `native.rds` e `long.parquet` em --out). Contrato de units.yaml:
##   units: [{id, area, origin, available_until, weather_issue_cutoff, day0_source, variant?, history_days?,
##            oracle_env?, oracle_cut?}]  (+ opcionais de topo: tz, packages)
## Saidas sob --out: plugins/<model>/<variant|_>/<area>/<origin>/{native.rds, long.parquet, invocation.json}; o lock
## versionado `<root>/<model>/tests/parity/golden.lock` e atualizado so apos provar o oraculo intacto.
## Saida: 0 ok; 2 [contract] args/units/out/oraculo sujo ou tocado; 1 driver falhou; 12 [repro] saidas diferentes.

.PC_GOLDEN_ALLOWED <- c("id", "area", "origin", "available_until", "weather_issue_cutoff", "day0_source", "variant",
                        "history_days", "oracle_env", "oracle_cut")
.PC_GOLDEN_DAY0 <- c("native", "oracle_origin_minus_1", "oracle_discarded_step")
.PC_GOLDEN_CUTOFF <- c("available_until", "origin_midnight_utc")
.PC_GOLDEN_PINS <- c("TZ=UTC", "LC_ALL=C.UTF-8", "OMP_NUM_THREADS=1", "OPENBLAS_NUM_THREADS=1")
.PC_GOLDEN_FILES <- c("native.rds", "long.parquet", "invocation.json")
.PC_GOLDEN_DATASETS <- c("load", "weather/observed", "weather/forecast", "auxiliary/holidays")

## --- units.yaml ------------------------------------------------------------------------------------------------
.pc_golden_bad_units <- function(id, field, why)
  prevcarga_abort("tools.bad_units", where = paste0(id, ".", field), details = list(unit = id, field = field, why = why))

.pc_golden_scalar_chr <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

## Le e valida; devolve list(tz, packages, units = lista de unidades com origin (Date) e available_until (POSIXct no tz)).
pc_golden_units <- function(path, tz = "Etc/GMT+3") {
  doc <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (!is.list(doc) || is.null(names(doc)) || !is.list(doc$units) || !length(doc$units))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = "units", why = "units.yaml sem a lista `units`"))
  extra <- setdiff(names(doc), c("units", "tz", "packages"))
  if (length(extra))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = extra[[1L]], why = "campo de topo desconhecido"))
  if (!is.null(doc$tz)) tz <- doc$tz
  if (!.pc_golden_scalar_chr(tz) || !(tz %in% OlsonNames()))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = "tz", why = "fuso desconhecido"))
  pk <- if (is.null(doc$packages)) character() else unlist(doc$packages)
  if (!is.character(pk) || anyNA(pk) || any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", pk)))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = "packages", why = "lista de nomes de pacote"))
  ids <- character()
  units <- lapply(seq_along(doc$units), function(i) {
    u <- doc$units[[i]]
    id <- if (is.list(u) && .pc_golden_scalar_chr(u$id)) u$id else paste0("#", i)
    if (!is.list(u) || is.null(names(u))) .pc_golden_bad_units(id, "id", "a unidade deve ser um mapa")
    if (!.pc_golden_scalar_chr(u$id) || !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", u$id))
      .pc_golden_bad_units(id, "id", "id obrigatorio (letras, digitos, . _ -)")
    if (id %in% ids) .pc_golden_bad_units(id, "id", "id duplicado")
    ids <<- c(ids, id)
    unk <- setdiff(names(u), .PC_GOLDEN_ALLOWED)
    if (length(unk)) .pc_golden_bad_units(id, unk[[1L]], "campo desconhecido")
    for (f in c("area", "origin", "available_until", "weather_issue_cutoff", "day0_source"))
      if (!.pc_golden_scalar_chr(u[[f]])) .pc_golden_bad_units(id, f, "obrigatorio (texto)")
    if (!grepl("^[A-Za-z0-9_]+$", u$area)) .pc_golden_bad_units(id, "area", "area invalida")
    o <- if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", u$origin)) lubridate::ymd(u$origin, quiet = TRUE) else NA
    if (is.na(o)) .pc_golden_bad_units(id, "origin", "ISO AAAA-MM-DD")
    au <- if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}$", u$available_until))
      lubridate::ymd_hm(u$available_until, tz = tz, quiet = TRUE) else NA
    if (is.na(au)) .pc_golden_bad_units(id, "available_until", "ISO local AAAA-MM-DDTHH:MM")
    if (!(u$weather_issue_cutoff %in% .PC_GOLDEN_CUTOFF))
      .pc_golden_bad_units(id, "weather_issue_cutoff", paste(.PC_GOLDEN_CUTOFF, collapse = " | "))
    if (!(u$day0_source %in% .PC_GOLDEN_DAY0)) .pc_golden_bad_units(id, "day0_source", paste(.PC_GOLDEN_DAY0, collapse = " | "))
    if (!is.null(u$variant) && !.pc_golden_scalar_chr(u$variant)) .pc_golden_bad_units(id, "variant", "texto ou null")
    if (!is.null(u$history_days) && !(is.numeric(u$history_days) && length(u$history_days) == 1L &&
                                      !is.na(u$history_days) && u$history_days >= 1 && u$history_days == round(u$history_days)))
      .pc_golden_bad_units(id, "history_days", "inteiro >= 1")
    if (!is.null(u$oracle_cut) && !identical(u$oracle_cut, "start_label")) .pc_golden_bad_units(id, "oracle_cut", "so start_label")
    ev <- u$oracle_env
    if (!is.null(ev)) {
      if (!is.list(ev) || is.null(names(ev)) || any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", names(ev))) ||
          !all(vapply(ev, .pc_golden_scalar_chr, NA)))
        .pc_golden_bad_units(id, "oracle_env", "mapa nome -> texto (aspas nos valores)")
    }
    list(id = u$id, area = u$area, origin = o, available_until = au, weather_issue_cutoff = u$weather_issue_cutoff,
         day0_source = u$day0_source, variant = u$variant, history_days = u$history_days,
         oracle_env = if (length(ev)) lapply(ev, identity) else list(), oracle_cut = u$oracle_cut)
  })
  list(tz = tz, packages = pk, units = units)
}

## --- argumentos do filho -----------------------------------------------------------------------------------------
## env do filho: pins do oraculo + oracle_env da unidade (so ele); o pai nunca chama Sys.setenv.
.pc_golden_child_env <- function(unit)
  c(.PC_GOLDEN_PINS, if (length(unit$oracle_env)) paste0(names(unit$oracle_env), "=", shQuote(unlist(unit$oracle_env))))

.pc_golden_cut_arg <- function(unit) {
  au <- unit$available_until
  if (identical(unit$oracle_cut, "start_label")) au <- au - lubridate::minutes(30)
  format(au, "%Y-%m-%dT%H:%M")
}

.pc_golden_unit_json <- function(unit, tz, data_dir) {
  list(id = unit$id, area = unit$area, origin = format(unit$origin), available_until = format(unit$available_until, "%Y-%m-%dT%H:%M"),
       tz = tz, weather_issue_cutoff = unit$weather_issue_cutoff, day0_source = unit$day0_source, variant = unit$variant,
       history_days = unit$history_days, oracle_cut = unit$oracle_cut, oracle_env = unit$oracle_env,
       oracle_available_until = .pc_golden_cut_arg(unit), data_dir = data_dir)
}

## --- preflight ---------------------------------------------------------------------------------------------------
.pc_golden_git <- function(oracle, args)
  suppressWarnings(system2("git", c("--no-optional-locks", "-C", shQuote(oracle), args), stdout = TRUE, stderr = FALSE))

pc_golden_preflight <- function(args) {
  oracle <- normalizePath(args$oracle, winslash = "/", mustWork = FALSE)
  if (!nzchar(args$oracle) || !dir.exists(oracle))
    prevcarga_abort("tools.bad_args", where = "--oracle", details = list(expected = args$oracle))
  out <- normalizePath(args$out, winslash = "/", mustWork = FALSE)
  if (identical(out, oracle) || startsWith(paste0(out, "/"), paste0(oracle, "/")))
    prevcarga_abort("tools.out_inside_oracle", where = "--out", details = list(out = out, oracle = oracle))
  st <- .pc_golden_git(oracle, c("status", "--porcelain"))
  if (!is.null(attr(st, "status")))
    prevcarga_abort("tools.bad_args", where = "--oracle", details = list(why = "nao e um repositorio git", oracle = oracle))
  if (length(st)) prevcarga_abort("tools.oracle_dirty", where = oracle, details = list(first = st[[1L]], n = length(st)))
  stamp <- tempfile("golden-stamp-")
  file.create(stamp)
  Sys.sleep(0.05)   # o carimbo precede estritamente qualquer escrita do filho (mtime com subsegundo)
  list(out = out, oracle = oracle, stamp = stamp, commit = .pc_golden_git(oracle, c("rev-parse", "HEAD"))[[1L]])
}

## Oraculo intacto: git status vazio E nenhum arquivo (fora de .git) mais novo que o carimbo.
pc_golden_oracle_touched <- function(pre) {
  st <- .pc_golden_git(pre$oracle, c("status", "--porcelain"))
  fl <- list.files(pre$oracle, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  fl <- fl[!(fl == ".git" | startsWith(fl, ".git/"))]
  newer <- if (length(fl)) fl[file.info(file.path(pre$oracle, fl))$mtime > file.info(pre$stamp)$mtime] else character()
  sort(unique(c(sub("^...", "", st), newer)), method = "radix")
}

## --- execucao ----------------------------------------------------------------------------------------------------
.pc_golden_run_once <- function(unit, paths, k, args, tz) {
  run <- file.path(paths$scratch, unit$id, paste0("run_", k))
  dir.create(run, recursive = TRUE, showWarnings = FALSE)
  log <- file.path(run, "oracle.log")
  ujson <- file.path(paths$scratch, unit$id, "unit.json")
  if (!file.exists(ujson))
    writeLines(jsonlite::toJSON(.pc_golden_unit_json(unit, tz, args$data), auto_unbox = TRUE, null = "null", pretty = TRUE), ujson)
  cargs <- c("--vanilla", shQuote(paths$driver), "--oracle", shQuote(paths$oracle), "--unit", shQuote(ujson),
             "--out", shQuote(run), "--available-until", .pc_golden_cut_arg(unit))
  old <- setwd(file.path(paths$scratch, unit$id))
  on.exit(setwd(old), add = TRUE)
  rc <- suppressWarnings(system2(args$rscript, cargs, env = .pc_golden_child_env(unit), stdout = log, stderr = log))
  tail <- if (file.exists(log)) utils::tail(readLines(log, warn = FALSE), 20L) else character()
  miss <- setdiff(c("native.rds", "long.parquet"), list.files(run))
  if (!identical(as.integer(rc), 0L) || length(miss))
    prevcarga_abort("tools.oracle_failed", where = unit$id,
                    details = list(unit = unit$id, status = as.integer(rc), missing = miss, log = log, log_tail = tail))
  list(dir = run, args = cargs, env = .pc_golden_child_env(unit))
}

.pc_golden_same <- function(a, b) {
  identical(readRDS(file.path(a, "native.rds")), readRDS(file.path(b, "native.rds"))) &&
    identical(readBin(file.path(a, "long.parquet"), "raw", file.size(file.path(a, "long.parquet"))),
              readBin(file.path(b, "long.parquet"), "raw", file.size(file.path(b, "long.parquet"))))
}

.pc_golden_unit_dir <- function(out, model, unit)
  file.path(out, "plugins", model, if (is.null(unit$variant)) "_" else unit$variant, unit$area, format(unit$origin))

.pc_golden_sanitize <- function(x, scratch) gsub(scratch, "<scratch>", x, fixed = TRUE)

pc_golden_publish <- function(unit, run, out, model, paths, commit, rversion) {
  dst <- .pc_golden_unit_dir(out, model, unit)
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  for (f in c("native.rds", "long.parquet")) file.copy(file.path(run$dir, f), file.path(dst, f), overwrite = TRUE)
  inv <- list(model = model, unit = unit$id, args = .pc_golden_sanitize(run$args, paths$scratch),
              env = run$env, pins = .PC_GOLDEN_PINS, cwd = "<scratch>", oracle_commit = commit, r_version = rversion)
  writeLines(jsonlite::toJSON(.pc_golden_canon(inv), auto_unbox = TRUE, null = "null", pretty = TRUE),
             file.path(dst, "invocation.json"))
  dst
}

## --- golden.lock ---------------------------------------------------------------------------------------------------
.pc_golden_canon <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, .pc_golden_canon)
    if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
  }
  x
}

.pc_golden_sha <- function(f) digest::digest(f, algo = "sha256", file = TRUE)

## Particoes `<dataset>/area_code=<area>/year=<y>` que a unidade le (anos de origin-history_days a origin).
pc_data_digest <- function(data_dir, unit) {
  none <- list(aggregate = digest::digest("", algo = "sha256", serialize = FALSE), partitions = stats::setNames(list(), character()))
  if (is.null(data_dir) || !dir.exists(data_dir)) return(none)
  y1 <- lubridate::year(unit$origin)
  y0 <- lubridate::year(unit$origin - lubridate::days(if (is.null(unit$history_days)) 0L else as.integer(unit$history_days)))
  parts <- unlist(lapply(.PC_GOLDEN_DATASETS, function(d) file.path(d, paste0("area_code=", unit$area), paste0("year=", y0:y1))))
  parts <- parts[dir.exists(file.path(data_dir, parts))]
  parts <- sort(parts, method = "radix")
  sh <- vapply(parts, function(p) {
    fs <- sort(list.files(file.path(data_dir, p), recursive = TRUE), method = "radix")
    digest::digest(paste0(fs, "\t", vapply(file.path(data_dir, p, fs), .pc_golden_sha, ""), "\n", collapse = ""),
                   algo = "sha256", serialize = FALSE)
  }, "")
  list(aggregate = digest::digest(paste0(parts, "\t", sh, "\n", collapse = ""), algo = "sha256", serialize = FALSE),
       partitions = if (length(parts)) as.list(stats::setNames(unname(sh), parts)) else none$partitions)
}

## Fingerprint do runtime DO FILHO (mesmo Rscript e mesmo env do oraculo): R, plataforma, pacotes, BLAS.
pc_runtime_fingerprint <- function(packages, rscript, env = .PC_GOLDEN_PINS) {
  expr <- sprintf(paste0("p <- c(%s); v <- vapply(p, function(x) if (requireNamespace(x, quietly = TRUE)) ",
                         "as.character(utils::packageVersion(x)) else 'absent', ''); ",
                         "cat(R.version.string, R.version$platform, extSoftVersion()[['BLAS']], ",
                         "if (length(p)) paste0(p, '=', v), sep = '\\n')"),
                  if (length(packages)) paste0("'", packages, "'", collapse = ",") else "")
  l <- suppressWarnings(system2(rscript, c("--vanilla", "-e", shQuote(expr)), env = env, stdout = TRUE, stderr = FALSE))
  pk <- if (length(l) > 3L) l[-(1:3)] else character()
  list(R = sub("^R version ([0-9.]+).*", "\\1", l[[1L]]), platform = l[[2L]], blas = l[[3L]],
       packages = if (length(pk)) as.list(stats::setNames(sub("^[^=]*=", "", pk), sub("=.*$", "", pk))) else
         stats::setNames(list(), character()))
}

pc_golden_lock_update <- function(lock_path, model, unit_id, files, oracle_commit, data_digest, fingerprint) {
  lock <- if (file.exists(lock_path)) jsonlite::read_json(lock_path, simplifyVector = FALSE) else list()
  if (is.null(lock$units)) lock$units <- stats::setNames(list(), character())
  lock$model <- model
  lock$oracle_commit <- oracle_commit
  lock$units[[unit_id]] <- list(files = as.list(files), oracle_commit = oracle_commit, data_digest = data_digest,
                                fingerprint = fingerprint)
  txt <- jsonlite::toJSON(.pc_golden_canon(lock), auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)
  tmp <- paste0(lock_path, ".tmp")
  writeBin(charToRaw(paste0(paste(txt, collapse = "\n"), "\n")), tmp)
  if (!file.rename(tmp, lock_path)) { unlink(tmp); prevcarga_abort("generic.unexpected", where = lock_path) }
  invisible(lock_path)
}

## --- main ----------------------------------------------------------------------------------------------------------
pc_golden_parse_args <- function(argv, repo) {
  known <- c("model", "variant", "units", "oracle", "out", "root", "rscript", "format", "data")
  if (length(argv) %% 2L != 0L || any(!grepl("^--", argv[seq(1L, length(argv), by = 2L)])))
    prevcarga_abort("tools.bad_args", where = "argv")
  k <- if (length(argv)) sub("^--", "", argv[seq(1L, length(argv), by = 2L)]) else character()
  a <- stats::setNames(as.list(if (length(argv)) argv[seq(2L, length(argv), by = 2L)] else character()), k)
  if (length(bad <- setdiff(k, known))) prevcarga_abort("tools.bad_args", where = paste0("--", bad[[1L]]))
  for (f in c("model", "units", "oracle")) if (is.null(a[[f]])) prevcarga_abort("tools.bad_args", where = paste0("--", f))
  if (is.null(a$root)) a$root <- file.path(repo, "models")
  if (is.null(a$out)) a$out <- file.path(repo, ".state", "golden")
  if (is.null(a$rscript)) a$rscript <- file.path(R.home("bin"), "Rscript")
  if (is.null(a$format)) a$format <- "text"
  if (!(a$format %in% c("text", "json"))) prevcarga_abort("tools.bad_args", where = "--format")
  a
}

pc_golden_main <- function(argv, repo) {
  args <- pc_golden_parse_args(argv, repo)
  parity <- file.path(args$root, args$model, "tests", "parity")
  driver <- file.path(parity, "oracle_driver.R")
  if (!grepl("^[A-Za-z0-9_][A-Za-z0-9._-]*$", args$model) || !dir.exists(parity))
    prevcarga_abort("tools.bad_args", where = "--model", details = list(expected = parity))
  if (!file.exists(driver)) prevcarga_abort("tools.bad_args", where = "--model", details = list(expected = driver))
  if (!file.exists(args$units)) prevcarga_abort("tools.bad_args", where = "--units", details = list(expected = args$units))
  pre <- pc_golden_preflight(args)
  spec <- pc_golden_units(args$units)
  units <- Filter(function(u) is.null(args$variant) || identical(u$variant, args$variant), spec$units)
  scratch <- tempfile("golden-scratch-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  if (startsWith(paste0(normalizePath(scratch, winslash = "/"), "/"), paste0(pre$oracle, "/")))
    prevcarga_abort("tools.out_inside_oracle", where = "scratch")
  paths <- list(scratch = scratch, driver = normalizePath(driver, winslash = "/"), oracle = pre$oracle)
  done <- list()
  for (u in units) {
    r1 <- .pc_golden_run_once(u, paths, 1L, args, spec$tz)
    r2 <- .pc_golden_run_once(u, paths, 2L, args, spec$tz)
    if (!.pc_golden_same(r1$dir, r2$dir))
      prevcarga_abort("tools.oracle_nondeterministic", where = u$id, details = list(unit = u$id, runs = 2L))
    fp <- pc_runtime_fingerprint(spec$packages, args$rscript, r1$env)
    dst <- pc_golden_publish(u, r1, pre$out, args$model, paths, pre$commit, fp$R)
    done[[u$id]] <- list(dir = dst, fingerprint = fp, unit = u)
  }
  touched <- pc_golden_oracle_touched(pre)
  if (length(touched))
    prevcarga_abort("tools.oracle_touched", where = pre$oracle, details = list(files = utils::head(touched, 20L), n = length(touched)))
  lock <- file.path(parity, "golden.lock")
  for (d in done) {
    files <- vapply(.PC_GOLDEN_FILES, function(f) .pc_golden_sha(file.path(d$dir, f)), "")
    pc_golden_lock_update(lock, args$model, d$unit$id, files, pre$commit, pc_data_digest(args$data, d$unit), d$fingerprint)
  }
  list(model = args$model, oracle_commit = pre$commit, out = pre$out, lock = lock,
       units = unname(lapply(done, function(d) list(id = d$unit$id, dir = d$dir))))
}

pc_golden_cli <- function(argv = commandArgs(trailingOnly = TRUE)) {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
  lib <- file.path(dirname(normalizePath(f, winslash = "/")), "..", "lib")
  repo <- normalizePath(file.path(lib, "..", ".."), winslash = "/")
  options(prevcarga.lib = normalizePath(lib, winslash = "/"))
  source(file.path(lib, "errors.R"))
  source(file.path(lib, "runtime.R"))
  invisible(prevcarga_runtime_enforce())
  fmt <- if (length(i <- which(argv == "--format")) && i[[1L]] < length(argv)) argv[[i[[1L]] + 1L]] else "text"
  out <- prevcarga_with_errors(pc_golden_main(argv, repo))
  if (!is.null(out$cond)) {
    if (fmt == "json") {
      writeLines(prevcarga_render_error(out$cond, format = "json", verb = "plugin_golden"), con = stdout())
    } else {
      writeLines(prevcarga_render_error(out$cond, format = "text", verb = "plugin_golden"), con = stderr())
      if (length(out$cond$details$log_tail)) writeLines(paste0("  | ", out$cond$details$log_tail), con = stderr())
      if (length(out$cond$details$files)) writeLines(paste0("  tocado: ", out$cond$details$files), con = stderr())
    }
  } else if (fmt == "json") {
    writeLines(as.character(jsonlite::toJSON(prevcarga_envelope(verb = "plugin_golden", result = out$value),
                                             auto_unbox = TRUE, null = "null", digits = NA)), con = stdout())
  } else {
    cat(sprintf("plugin_golden: %s: %d unidade(s) em %s\n", out$value$model, length(out$value$units), out$value$out))
  }
  quit(save = "no", status = out$exit_code)
}

if (identical(environment(), globalenv()) && !interactive()) pc_golden_cli()
