#!/usr/bin/env Rscript
## reconcile_golden.R -- gerador do golden do ORACULO do Reconcile (R.6). Ferramenta de DESENVOLVIMENTO (so Linux,
## fora do bundle). O oraculo (`scripts/reconcilia_baseline.R` de prevcargadessem3) e SO lido/executado, num processo
## filho; a arvore dele e conferida intacta antes/depois. Deck-fixture e combina-A sinteticos ficam em tempdir().
##
##   Rscript --vanilla scripts/tools/reconcile_golden.R --origin 2026-02-20 [--oracle <dir>] [--rscript <Rscript>]
##       [--out <golden_dir>] [--lock <golden.lock>] [--days 760] [--seed 42] [--oracle-env NOME=VALOR ...]
##
## A entrada (verdade S = RS+PR+SC e folhas da combina-A) e funcao pura de (origem, dias, semente): o teste a regera
## com `rg_build_inputs()` e nada dela e versionado. Grava SO as saidas do oraculo em `<repo>/.state/golden/reconcile/`
## (gitignored): golden_profile.parquet (k por hour x month x dow, rotulo de INICIO) e golden_long.parquet (y_pred
## por intervalo, target_datetime = interval_end). O versionado e so o lock hash-only
## `scripts/lib/tests/fixtures/reconcile/golden.lock` (receita + digest da entrada + sha256 das saidas).
## `--oracle-env` (repetivel) vai SO ao filho (ex.: R_LIBS_USER da biblioteca do oraculo): configuracao, nunca
## literal; o pai nao le ambiente. Saida: 0 ok; 2 args/Linux/oraculo tocado; 1 oraculo falhou.

.RG_TZ <- "Etc/GMT+3"
.RG_AREA <- "S"
.RG_LEAVES <- c("RS", "PR", "SC")
.RG_WINDOW <- 730L
.RG_HORIZON <- 9L
.RG_FILES <- c("golden_profile.parquet", "golden_long.parquet")

.rg_hash <- function(a) sum(utf8ToInt(a))

## --- curva sintetica (funcao pura de (area, instante, semente); sem RNG, sem relogio) ----------------------------
.rg_leaf_curve <- function(st, a, seed) {
  h <- .rg_hash(a)
  slot <- 2L * lubridate::hour(st) + (lubridate::minute(st) %/% 30L)
  d <- as.integer(lubridate::as_date(st, tz = .RG_TZ) - lubridate::ymd("2024-01-01"))
  wk <- c(1.00, 1.03, 1.03, 1.03, 1.02, 0.92, 0.85)[lubridate::wday(st, week_start = 1L)]
  (800 + (h %% 500)) * (1 + 0.25 * sin(2 * pi * (slot - 30) / 48)) * wk * (1 + 0.1 * sin(2 * pi * d / 365)) +
    sin(seed * 1000 + d * 7.13 + slot * 1.37 + h)
}

## S = soma das folhas * perda sazonal (hora, mes, dow) com deriva diaria e ruido pequeno: o ratio varia por bucket
## E por dia (a janela de 730 dias e distinguivel da de 729).
.rg_curve <- function(st, a, seed) {
  if (!identical(a, .RG_AREA)) return(.rg_leaf_curve(st, a, seed))
  bu <- Reduce(`+`, lapply(.RG_LEAVES, function(l) .rg_leaf_curve(st, l, seed)))
  d <- as.integer(lubridate::as_date(st, tz = .RG_TZ) - lubridate::ymd("2024-01-01"))
  hr <- lubridate::hour(st)
  loss <- 1.02 + 0.012 * cos(2 * pi * hr / 24) + 0.002 * lubridate::month(st) +
    0.0015 * lubridate::wday(st, week_start = 7L) + 5e-5 * d + 0.002 * sin(seed * 1000 + d * 3.7 + hr)
  bu * loss
}

.rg_slots <- function(first_day, n_days)
  lubridate::ymd_hms(paste(first_day, "00:00:00"), tz = .RG_TZ) + lubridate::minutes(30L) * (0:(n_days * 48L - 1L))

## --- passo 1: entrada nas duas vistas (pura: nada e gravado) ---------------------------------------------------
## Verdade: [O - days, O + horizon) (o futuro serve ao --eval do oraculo e prova que o ajuste so olha [O-W, O-1]).
## Folhas da combina-A de O: d_plus 1..9, lead 1..432, y_pred = verdade * fator conhecido por folha.
rg_build_inputs <- function(origin, days = 760L, seed = 42L) {
  origin <- lubridate::ymd(origin)
  if (is.na(origin)) prevcarga_abort("tools.bad_args", where = "--origin")
  days <- as.integer(days)
  st <- .rg_slots(origin - days, days + .RG_HORIZON + 1L)
  areas <- c(.RG_AREA, .RG_LEAVES)
  truth <- data.table::rbindlist(lapply(areas, function(a)
    data.table::data.table(area_code = a, interval_start = st, interval_end = st + lubridate::minutes(30L),
                           load_mwh = .rg_curve(st, a, seed))))
  ## combina-A de O: vista do oraculo (inicio, UTC ingenuo) e vista do clean-room (fim, fuso declarado)
  lead <- 1L:(.RG_HORIZON * 48L)
  tstart <- lubridate::ymd_hms(paste(origin + 1L, "00:00:00"), tz = .RG_TZ) + lubridate::minutes(30L) * (lead - 1L)
  factor_of <- function(a, ld) 1 + 0.01 * match(a, .RG_LEAVES) + 0.004 * sin(ld / 7)
  mk <- function(a) {
    yp <- .rg_curve(tstart, a, seed) * factor_of(a, lead)
    dplus <- as.integer(lubridate::as_date(tstart, tz = .RG_TZ) - origin)
    list(oracle = data.table::data.table(forecast_origin = format(origin), target_datetime = lubridate::force_tz(tstart, "UTC"),
                                         lead = lead, d_plus = dplus, y_pred = yp),
         long = data.table::data.table(area = a, forecast_origin = format(origin),
                                       target_datetime = tstart + lubridate::minutes(30L), lead = lead, y_pred = yp,
                                       y_true = NA_real_, d_plus = dplus, granularity_min = 30L))
  }
  parts <- lapply(stats::setNames(.RG_LEAVES, .RG_LEAVES), mk)
  leaves <- data.table::rbindlist(lapply(parts, `[[`, "long"))
  leaves[, `:=`(lead = as.integer(lead), d_plus = as.integer(d_plus))]
  list(truth = truth, leaves = leaves, comb_oracle = lapply(parts, `[[`, "oracle"), origin = origin,
       days = days, seed = as.integer(seed))
}

## Digest canonico da entrada (texto, valores a 6 casas): prova que o teste regera exatamente o que o oraculo leu,
## sem versionar a entrada. Arredondado para nao depender do ultimo ulp do libm da plataforma.
rg_inputs_digest <- function(inp) {
  t <- inp$truth
  l <- inp$leaves
  txt <- c(sprintf("%s\t%.0f\t%.6f", t$area_code, as.numeric(t$interval_end), t$load_mwh),
           sprintf("%s\t%s\t%.0f\t%d\t%d\t%.6f", l$area, l$forecast_origin, as.numeric(l$target_datetime), l$lead,
                   l$d_plus, l$y_pred))
  digest::digest(paste0(txt, "\n", collapse = ""), algo = "sha256", serialize = FALSE)
}

## Vista do oraculo em `tmp`: timestamp ingenuo = interval_start (store declara load_label start) + combina-A.
rg_write_oracle_inputs <- function(inp, tmp) {
  truth <- inp$truth
  for (a in unique(truth$area_code)) for (y in unique(lubridate::year(truth$interval_start))) {
    d <- truth[truth$area_code == a & lubridate::year(truth$interval_start) == y,
               list(timestamp = lubridate::force_tz(interval_start, "UTC"), load_mwh)]
    p <- file.path(tmp, "data", "load", paste0("area_code=", a), paste0("year=", y))
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(d, file.path(p, "data.parquet"))
  }
  for (a in names(inp$comb_oracle)) {
    p <- file.path(tmp, "comb", a, format(inp$origin))
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(inp$comb_oracle[[a]], file.path(p, "long.parquet"))
  }
  utils::modifyList(inp, list(comb = file.path(tmp, "comb"), load = file.path(tmp, "data", "load"),
                              out = file.path(tmp, "out")))
}

## --- passo 2: oraculo SO LEITURA em processo filho ---------------------------------------------------------------
.rg_git <- function(oracle, args)
  suppressWarnings(system2("git", c("--no-optional-locks", "-C", shQuote(oracle), args), stdout = TRUE, stderr = FALSE))

.rg_snap <- function(root) {
  f <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  f <- sort(f[!(f == ".git" | startsWith(f, ".git/"))], method = "radix")
  list(status = .rg_git(root, c("status", "--porcelain")),
       mtime = stats::setNames(as.numeric(file.info(file.path(root, f))$mtime), f))
}

rg_run_oracle <- function(inp, oracle, rscript = file.path(R.home("bin"), "Rscript"), env = character()) {
  if (!identical(.Platform$OS.type, "unix")) prevcarga_abort("tools.bad_args", where = "plataforma",
                                                              details = list(why = "so Linux"))
  script <- file.path(oracle, "scripts", "reconcilia_baseline.R")
  if (!file.exists(script)) prevcarga_abort("tools.bad_args", where = "--oracle", details = list(expected = script))
  commit <- .rg_git(oracle, c("rev-parse", "HEAD"))[[1L]]
  before <- .rg_snap(oracle)
  dir.create(inp$out, recursive = TRUE, showWarnings = FALSE)
  cargs <- c("--vanilla", shQuote(script), "--subsystem", .RG_AREA, "--mode", "full",
             "--window-days", as.character(.RG_WINDOW), "--eval",
             "--comb-dir", shQuote(inp$comb), "--data-dir", shQuote(inp$load), "--out-dir", shQuote(inp$out))
  log <- suppressWarnings(system2(rscript, cargs, env = env, stdout = TRUE, stderr = TRUE))
  st <- attr(log, "status")
  after <- .rg_snap(oracle)
  if (!identical(before, after))
    prevcarga_abort("tools.oracle_touched", where = oracle,
                    details = list(files = utils::head(setdiff(c(before$status, names(before$mtime)),
                                                               c(after$status, names(after$mtime))), 20L)))
  if (!is.null(st) || any(grepl("fallback glob_ratio", log, fixed = TRUE)))
    prevcarga_abort("tools.oracle_failed", where = "reconcilia_baseline.R",
                    details = list(status = if (is.null(st)) 0L else as.integer(st),
                                   why = if (is.null(st)) "fallback glob_ratio: o glob do oraculo usa todo o historico (nao comparavel)" else "exit != 0",
                                   log_tail = utils::tail(log, 20L)))
  list(commit = commit, cmd = paste(c(shQuote(rscript), "--vanilla", "scripts/reconcilia_baseline.R", "--subsystem",
                                      .RG_AREA, "--mode", "full", "--window-days", .RG_WINDOW, "--eval",
                                      "--comb-dir <tmp>/comb --data-dir <tmp>/data/load --out-dir <tmp>/out"), collapse = " "),
       rversion = suppressWarnings(system2(rscript, c("--vanilla", "-e", shQuote("cat(R.version.string)")),
                                           env = env, stdout = TRUE, stderr = FALSE))[[1L]])
}

## --- passo 3: golden em .state + lock hash-only versionado ------------------------------------------------------
.rg_sha <- function(f) digest::digest(f, algo = "sha256", file = TRUE)

.rg_canon <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, .rg_canon)
    if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
  }
  x
}

rg_write_golden <- function(inp, run, golden_dir, lock_path) {
  dir.create(golden_dir, recursive = TRUE, showWarnings = FALSE)
  det <- data.table::as.data.table(arrow::read_parquet(file.path(inp$out, "reconc-S_detail.parquet")))
  lng <- data.table::as.data.table(arrow::read_parquet(file.path(inp$out, "reconc-S_long.parquet")))
  if (!identical(nrow(det), nrow(lng)) || !nrow(det))
    prevcarga_abort("tools.oracle_failed", where = "saida", details = list(why = "detail e long com tamanhos diferentes"))
  ts <- lubridate::with_tz(det$target_datetime, "UTC")
  prof <- data.table::data.table(hour = lubridate::hour(ts), month = lubridate::month(ts),
                                 dow = lubridate::wday(ts, week_start = 7L), k = det$y_pred / det$BU_pred)
  chk <- prof[, list(spread = max(k) - min(k), k = mean(k)), by = c("hour", "month", "dow")]
  if (any(chk$spread > 1e-12))
    prevcarga_abort("tools.oracle_failed", where = "perfil", details = list(why = "mais de um k por bucket"))
  gp <- chk[, list(hour = as.integer(hour), month = as.integer(month), dow = as.integer(dow), k)]
  data.table::setorderv(gp, c("month", "dow", "hour"))
  ## rotulo de inicio (oraculo) -> interval_end no fuso declarado
  end <- lubridate::force_tz(ts, .RG_TZ) + lubridate::minutes(30L)
  gl <- data.table::data.table(forecast_origin = as.character(det$forecast_origin), target_datetime = end,
                               lead = as.integer(det$lead), d_plus = as.integer(det$d_plus), BU_pred = det$BU_pred,
                               y_pred = det$y_pred)
  data.table::setorderv(gl, c("forecast_origin", "target_datetime"))
  arrow::write_parquet(gp, file.path(golden_dir, "golden_profile.parquet"))
  arrow::write_parquet(gl, file.path(golden_dir, "golden_long.parquet"))
  rows <- c(golden_profile.parquet = nrow(gp), golden_long.parquet = nrow(gl))
  files <- lapply(stats::setNames(names(rows), names(rows)), function(f)
    list(rows = as.integer(rows[[f]]), sha256 = .rg_sha(file.path(golden_dir, f))))
  lock <- list(oracle_commit = run$commit, oracle_r = run$rversion, command = run$cmd, origin = format(inp$origin),
               window_days = .RG_WINDOW, days = inp$days, seed = inp$seed, golden_dir = ".state/golden/reconcile",
               inputs = list(truth_rows = nrow(inp$truth), leaves_rows = nrow(inp$leaves),
                             sha256 = rg_inputs_digest(inp)),
               files = files)
  txt <- jsonlite::toJSON(.rg_canon(lock), auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(lock_path, ".tmp")
  writeBin(charToRaw(paste0(paste(txt, collapse = "\n"), "\n")), tmp)
  if (!file.rename(tmp, lock_path)) { unlink(tmp); prevcarga_abort("generic.unexpected", where = lock_path) }
  invisible(list(rows = rows, files = files))
}

## --- main --------------------------------------------------------------------------------------------------------
rg_parse_args <- function(argv, repo) {
  known <- c("origin", "oracle", "rscript", "out", "lock", "days", "seed", "oracle-env")
  if (length(argv) %% 2L != 0L || any(!grepl("^--", argv[seq(1L, length(argv), by = 2L)])))
    prevcarga_abort("tools.bad_args", where = "argv")
  k <- if (length(argv)) sub("^--", "", argv[seq(1L, length(argv), by = 2L)]) else character()
  v <- if (length(argv)) argv[seq(2L, length(argv), by = 2L)] else character()
  if (length(bad <- setdiff(k, known))) prevcarga_abort("tools.bad_args", where = paste0("--", bad[[1L]]))
  a <- list(origin = if (any(k == "origin")) v[k == "origin"][[1L]] else NULL,
            oracle = if (any(k == "oracle")) v[k == "oracle"][[1L]] else "/opt/ons-source/prevcargadessem3",
            rscript = if (any(k == "rscript")) v[k == "rscript"][[1L]] else file.path(R.home("bin"), "Rscript"),
            out = if (any(k == "out")) v[k == "out"][[1L]] else file.path(repo, ".state", "golden", "reconcile"),
            lock = if (any(k == "lock")) v[k == "lock"][[1L]] else
              file.path(repo, "scripts", "lib", "tests", "fixtures", "reconcile", "golden.lock"),
            days = if (any(k == "days")) as.integer(v[k == "days"][[1L]]) else 760L,
            seed = if (any(k == "seed")) as.integer(v[k == "seed"][[1L]]) else 42L,
            env = v[k == "oracle-env"])
  if (is.null(a$origin) || is.na(lubridate::ymd(a$origin, quiet = TRUE))) prevcarga_abort("tools.bad_args", where = "--origin")
  if (is.na(a$days) || is.na(a$seed) || a$days < .RG_WINDOW + 1L) prevcarga_abort("tools.bad_args", where = "--days")
  if (any(!grepl("^[A-Za-z_][A-Za-z0-9_]*=.+$", a$env))) prevcarga_abort("tools.bad_args", where = "--oracle-env")
  a
}

rg_main <- function(argv, repo) {
  a <- rg_parse_args(argv, repo)
  oracle <- normalizePath(a$oracle, winslash = "/", mustWork = FALSE)
  if (!dir.exists(oracle)) prevcarga_abort("tools.bad_args", where = "--oracle", details = list(expected = a$oracle))
  tmp <- tempfile("reconcile-golden-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  if (startsWith(paste0(normalizePath(tmp, winslash = "/"), "/"), paste0(oracle, "/")))
    prevcarga_abort("tools.out_inside_oracle", where = "tempdir")
  out <- normalizePath(a$out, winslash = "/", mustWork = FALSE)
  if (startsWith(paste0(out, "/"), paste0(oracle, "/")))
    prevcarga_abort("tools.out_inside_oracle", where = "--out", details = list(out = out, oracle = oracle))
  inp <- rg_write_oracle_inputs(rg_build_inputs(a$origin, a$days, a$seed), tmp)
  run <- rg_run_oracle(inp, oracle, a$rscript, a$env)
  w <- rg_write_golden(inp, run, out, a$lock)
  list(oracle_commit = run$commit, out = out, lock = a$lock, rows = as.list(w$rows))
}

rg_cli <- function(argv = commandArgs(trailingOnly = TRUE)) {
  if (!identical(.Platform$OS.type, "unix")) {
    writeLines("reconcile_golden: so Linux (ferramenta de desenvolvimento; nada foi executado)", con = stderr())
    quit(save = "no", status = 2L)
  }
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
  lib <- file.path(dirname(normalizePath(f, winslash = "/")), "..", "lib")
  repo <- normalizePath(file.path(lib, "..", ".."), winslash = "/")
  options(prevcarga.lib = normalizePath(lib, winslash = "/"))
  source(file.path(lib, "errors.R"))
  source(file.path(lib, "runtime.R"))
  invisible(prevcarga_runtime_enforce())
  out <- prevcarga_with_errors(rg_main(argv, repo))
  if (!is.null(out$cond)) {
    writeLines(prevcarga_render_error(out$cond, format = "text", verb = "reconcile_golden"), con = stderr())
    if (length(out$cond$details$log_tail)) writeLines(paste0("  | ", out$cond$details$log_tail), con = stderr())
    if (length(out$cond$details$files)) writeLines(paste0("  tocado: ", out$cond$details$files), con = stderr())
  } else {
    cat(sprintf("reconcile_golden: oraculo %s; golden em %s; lock %s\n", out$value$oracle_commit, out$value$out,
                out$value$lock))
  }
  quit(save = "no", status = out$exit_code)
}

if (identical(environment(), globalenv()) && !interactive()) rg_cli()
