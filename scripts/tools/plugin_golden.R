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
##            oracle_env?, oracle_cut?, cuts?, params?, seed?, data_areas?}]  (+ opcionais de topo: tz, packages, groups, rules)
## `--units` e um caminho de units.yaml OU o nome de um grupo de `groups` do `<root>/<model>/tests/parity/units.yaml`.
## `cuts` = cortes POR FASE literais ({train: {train_origin, history_start}, infer: {history_start, available_until,
## weather_issue_cutoff}}); available_until e weather_issue_cutoff podem vir de `cuts.infer` (sobem para a unidade).
## `rules` (topo, opcional) = regras declarativas conferidas em toda unidade (ver .pc_golden_check_rules).
## Saidas sob --out: plugins/<model>/<variant|_>/<area>/<origin>/{native.rds, long.parquet, invocation.json}; o lock
## versionado `<root>/<model>/tests/parity/golden.lock` e atualizado so apos provar o oraculo intacto.
## Saida: 0 ok; 2 [contract] args/units/out/oraculo sujo ou tocado; 1 driver falhou; 12 [repro] saidas diferentes.

.PC_GOLDEN_ALLOWED <- c("id", "area", "origin", "available_until", "weather_issue_cutoff", "day0_source", "variant",
                        "history_days", "oracle_env", "oracle_cut", "cuts", "params", "seed", "data_areas")
.PC_GOLDEN_LOCK_TRIES <- 600L   # ~60 s
.PC_GOLDEN_TOP <- c("units", "tz", "packages", "groups", "rules")
.PC_GOLDEN_RULES <- c("train_origin_offset", "train_history_offset", "infer_history_offset", "forbid", "day0_source",
                      "full_hour", "equals", "oracle_env_empty", "data_areas_is_area")
.PC_GOLDEN_DAY0 <- c("native", "oracle_origin_minus_1", "oracle_discarded_step")
.PC_GOLDEN_CUTOFF <- c("available_until", "origin_midnight_utc")
.PC_GOLDEN_PINS <- c("TZ=UTC", "LC_ALL=C.UTF-8", "OMP_NUM_THREADS=1", "OPENBLAS_NUM_THREADS=1")
.PC_GOLDEN_DATASETS <- c("load", "weather/observed", "weather/forecast", "auxiliary/holidays")

## --- units.yaml ------------------------------------------------------------------------------------------------
.pc_golden_bad_units <- function(id, field, why)
  prevcarga_abort("tools.bad_units", where = paste0(id, ".", field), details = list(unit = id, field = field, why = why))

.pc_golden_scalar_chr <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)

## Regras declarativas de topo (`rules`), conferidas em CADA unidade; a violacao e tools.bad_units (exit 2) nomeando unidade.campo:
## train_origin_offset (O - cuts.train.train_origin), train_history_offset (O - cuts.train.history_start), infer_history_offset
## (O - cuts.infer.history_start), forbid (campos que a unidade nao pode ter), day0_source (fontes aceitas), full_hour
## (available_until em hora cheia), equals (mapa campo -> valor exigido; `params` e mapa), oracle_env_empty, data_areas_is_area.
.pc_golden_check_rules <- function(rules, u, o, au) {
  id <- u$id
  days <- function(x) as.integer(o - lubridate::ymd(x, quiet = TRUE))
  chk <- function(field, ok, why) if (!isTRUE(ok)) .pc_golden_bad_units(id, field, why)
  if (!is.null(rules$train_origin_offset))
    chk("cuts.train.train_origin", identical(days(u$cuts$train$train_origin), as.integer(rules$train_origin_offset)),
        sprintf("train_origin deve ser O - %d", as.integer(rules$train_origin_offset)))
  if (!is.null(rules$train_history_offset))
    chk("cuts.train.history_start", identical(days(u$cuts$train$history_start), as.integer(rules$train_history_offset)),
        sprintf("history_start.train deve ser O - %d", as.integer(rules$train_history_offset)))
  if (!is.null(rules$infer_history_offset))
    chk("cuts.infer.history_start", identical(days(u$cuts$infer$history_start), as.integer(rules$infer_history_offset)),
        sprintf("history_start.infer deve ser O - %d", as.integer(rules$infer_history_offset)))
  for (f in unlist(rules$forbid)) chk(f, is.null(u[[f]]), "campo proibido nesta familia")
  if (!is.null(rules$day0_source)) chk("day0_source", u$day0_source %in% unlist(rules$day0_source),
                                       paste(unlist(rules$day0_source), collapse = " | "))
  if (isTRUE(rules$full_hour)) chk("available_until", lubridate::minute(au) == 0L && lubridate::second(au) == 0, "hora cheia")
  for (f in names(rules$equals)) chk(f, identical(.pc_golden_canon(u[[f]]), .pc_golden_canon(rules$equals[[f]])),
                                     "diferente do valor exigido por rules.equals")
  if (isTRUE(rules$oracle_env_empty)) chk("oracle_env", !length(u$oracle_env), "oracle_env deve ser vazio")
  if (isTRUE(rules$data_areas_is_area)) chk("data_areas", identical(unlist(u$data_areas), u$area), "data_areas deve ser a area")
}

## Le e valida; devolve list(tz, packages, groups, units = lista de unidades com origin (Date) e available_until (POSIXct no tz)).
pc_golden_units <- function(path, tz = "Etc/GMT+3") {
  doc <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (!is.list(doc) || is.null(names(doc)) || !is.list(doc$units) || !length(doc$units))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = "units", why = "units.yaml sem a lista `units`"))
  extra <- setdiff(names(doc), .PC_GOLDEN_TOP)
  if (length(extra))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = extra[[1L]], why = "campo de topo desconhecido"))
  if (!is.null(doc$tz)) tz <- doc$tz
  if (!.pc_golden_scalar_chr(tz) || !(tz %in% OlsonNames()))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = "tz", why = "fuso desconhecido"))
  pk <- if (is.null(doc$packages)) character() else unlist(doc$packages)
  if (!is.character(pk) || anyNA(pk) || any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", pk)))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = "packages", why = "lista de nomes de pacote"))
  all_ids <- vapply(seq_along(doc$units), function(i) {
    u <- doc$units[[i]]
    if (is.list(u) && .pc_golden_scalar_chr(u$id)) u$id else paste0("#", i)
  }, character(1L))
  units <- lapply(seq_along(doc$units), function(i) {
    u <- doc$units[[i]]
    id <- if (is.list(u) && .pc_golden_scalar_chr(u$id)) u$id else paste0("#", i)
    if (!is.list(u) || is.null(names(u))) .pc_golden_bad_units(id, "id", "a unidade deve ser um mapa")
    ## cortes por fase: available_until e weather_issue_cutoff podem ser declarados em cuts.infer (sobem para a unidade)
    for (f in c("available_until", "weather_issue_cutoff"))
      if (is.null(u[[f]]) && is.list(u$cuts) && is.list(u$cuts$infer) && !is.null(u$cuts$infer[[f]])) u[[f]] <- u$cuts$infer[[f]]
    if (!.pc_golden_scalar_chr(u$id) || !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", u$id))
      .pc_golden_bad_units(id, "id", "id obrigatorio (letras, digitos, . _ -)")
    if (id %in% all_ids[seq_len(i - 1L)]) .pc_golden_bad_units(id, "id", "id duplicado")
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
    day <- function(x) is.character(x) && length(x) == 1L && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x) &&
      !is.na(lubridate::ymd(x, quiet = TRUE))
    if (!is.null(u$cuts)) {
      if (!is.list(u$cuts) || !all(names(u$cuts) %in% c("train", "infer")))
        .pc_golden_bad_units(id, "cuts", "mapa com as fases train e infer")
      for (ph in names(u$cuts)) {
        cp <- u$cuts[[ph]]
        if (!is.list(cp) || is.null(names(cp))) .pc_golden_bad_units(id, paste0("cuts.", ph), "mapa de cortes da fase")
        for (f in c("train_origin", "history_start"))
          if (!is.null(cp[[f]]) && !day(cp[[f]])) .pc_golden_bad_units(id, paste0("cuts.", ph, ".", f), "ISO AAAA-MM-DD")
        if (is.null(cp$history_start)) .pc_golden_bad_units(id, paste0("cuts.", ph, ".history_start"), "obrigatorio (ISO AAAA-MM-DD)")
      }
      if (!is.null(u$cuts$train) && is.null(u$cuts$train$train_origin))
        .pc_golden_bad_units(id, "cuts.train.train_origin", "obrigatorio (ISO AAAA-MM-DD)")
    }
    if (!is.null(u$params) && (!is.list(u$params) || is.null(names(u$params)))) .pc_golden_bad_units(id, "params", "mapa")
    if (!is.null(u$seed) && !(is.numeric(u$seed) && length(u$seed) == 1L && !is.na(u$seed) && u$seed == round(u$seed)))
      .pc_golden_bad_units(id, "seed", "inteiro")
    if (!is.null(u$data_areas) && !(is.character(unlist(u$data_areas)) && length(u$data_areas) >= 1L &&
                                    all(grepl("^[A-Za-z0-9_]+$", unlist(u$data_areas)))))
      .pc_golden_bad_units(id, "data_areas", "lista de areas")
    if (is.list(doc$rules)) .pc_golden_check_rules(doc$rules, u, o, au)
    ev <- u$oracle_env
    if (!is.null(ev)) {
      if (!is.list(ev) || is.null(names(ev)) || any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", names(ev))) ||
          !all(vapply(ev, .pc_golden_scalar_chr, NA)))
        .pc_golden_bad_units(id, "oracle_env", "mapa nome -> texto (aspas nos valores)")
    }
    list(id = u$id, area = u$area, origin = o, available_until = au, weather_issue_cutoff = u$weather_issue_cutoff,
         day0_source = u$day0_source, variant = u$variant, history_days = u$history_days,
         oracle_env = if (length(ev)) lapply(ev, identity) else list(), oracle_cut = u$oracle_cut,
         cuts = u$cuts, params = u$params, seed = if (is.null(u$seed)) NULL else as.integer(u$seed),
         data_areas = if (is.null(u$data_areas)) NULL else as.character(unlist(u$data_areas)))
  })
  groups <- .pc_golden_groups(doc$groups, all_ids, path)
  if (is.list(doc$rules)) {
    bad <- setdiff(names(doc$rules), .PC_GOLDEN_RULES)
    if (length(bad)) prevcarga_abort("tools.bad_units", where = path, details = list(field = paste0("rules.", bad[[1L]]), why = "regra desconhecida"))
  } else if (!is.null(doc$rules)) {
    prevcarga_abort("tools.bad_units", where = path, details = list(field = "rules", why = "mapa"))
  }
  list(tz = tz, packages = pk, units = units, groups = groups)
}

## groups: nome -> ids de unidades existentes (nome e id validos; id inexistente => tools.bad_units).
.pc_golden_groups <- function(g, ids, path) {
  if (is.null(g)) return(list())
  if (!is.list(g) || is.null(names(g)) || any(!grepl("^[A-Za-z0-9_][A-Za-z0-9_-]*$", names(g))))
    prevcarga_abort("tools.bad_units", where = path, details = list(field = "groups", why = "mapa nome -> lista de ids"))
  lapply(g, function(v) {
    v <- unlist(v)
    miss <- setdiff(v, ids)
    if (!is.character(v) || !length(v) || length(miss))
      prevcarga_abort("tools.bad_units", where = path, details = list(field = "groups", why = paste0("id de unidade inexistente: ",
                      if (length(miss)) miss[[1L]] else "?")))
    v
  })
}

## `--units`: caminho de units.yaml (todas as unidades) OU nome de grupo de `<root>/<model>/tests/parity/units.yaml`.
## Devolve list(path, group); nenhum dos dois => tools.bad_args em --units.
pc_golden_resolve_units <- function(value, root, model) {
  if (file.exists(value) && !dir.exists(value)) return(list(path = value, group = NULL))
  dflt <- file.path(root, model, "tests", "parity", "units.yaml")
  if (grepl("^[A-Za-z0-9_][A-Za-z0-9_-]*$", value) && file.exists(dflt)) {
    g <- pc_golden_units(dflt)$groups
    if (value %in% names(g)) return(list(path = dflt, group = value))
  }
  prevcarga_abort("tools.bad_args", where = "--units", details = list(expected = value))
}

## Unidades do grupo, na ordem do grupo (grupo NULL => todas).
pc_golden_group_units <- function(spec, group) {
  if (is.null(group)) return(spec$units)
  by_id <- stats::setNames(spec$units, vapply(spec$units, function(u) u$id, ""))
  unname(by_id[spec$groups[[group]]])
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
       oracle_available_until = .pc_golden_cut_arg(unit), data_dir = data_dir,
       cuts = unit$cuts, params = unit$params, seed = unit$seed, data_areas = unit$data_areas)
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
  rc <- withr::with_dir(file.path(paths$scratch, unit$id),
    suppressWarnings(system2(args$rscript, cargs, env = .pc_golden_child_env(unit), stdout = log, stderr = log)))
  tail <- if (file.exists(log)) utils::tail(readLines(log, warn = FALSE), 20L) else character()
  miss <- c(setdiff("native.rds", list.files(run)), if (!length(.pc_golden_longs(run))) "long.parquet")
  if (!identical(as.integer(rc), 0L) || length(miss))
    prevcarga_abort("tools.oracle_failed", where = unit$id,
                    details = list(unit = unit$id, status = as.integer(rc), missing = miss, log = log, log_tail = tail))
  list(dir = run, args = cargs, env = .pc_golden_child_env(unit))
}

## long*.parquet do diretorio (long.parquet, ou long_O.parquet + long_Om1.parquet quando o oraculo roda mais de uma origem)
.pc_golden_longs <- function(dir) sort(list.files(dir, pattern = "^long[A-Za-z0-9_]*[.]parquet$"), method = "radix")

.pc_golden_raw <- function(f) readBin(f, "raw", file.size(f))

.pc_golden_same <- function(a, b) {
  la <- .pc_golden_longs(a)
  identical(readRDS(file.path(a, "native.rds")), readRDS(file.path(b, "native.rds"))) && identical(la, .pc_golden_longs(b)) &&
    all(vapply(la, function(f) identical(.pc_golden_raw(file.path(a, f)), .pc_golden_raw(file.path(b, f))), NA))
}

.pc_golden_unit_dir <- function(out, model, unit)
  file.path(out, "plugins", model, if (is.null(unit$variant)) "_" else unit$variant, unit$area, format(unit$origin))

.pc_golden_sanitize <- function(x, scratch) gsub(scratch, "<scratch>", x, fixed = TRUE)

pc_golden_publish <- function(unit, run, out, model, paths, commit, rversion) {
  dst <- .pc_golden_unit_dir(out, model, unit)
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  for (f in c("native.rds", .pc_golden_longs(run$dir))) file.copy(file.path(run$dir, f), file.path(dst, f), overwrite = TRUE)
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
  first <- if (!is.null(unit$cuts)) min(lubridate::ymd(unlist(lapply(unit$cuts, function(p) p$history_start)))) else
    unit$origin - lubridate::days(if (is.null(unit$history_days)) 0L else as.integer(unit$history_days))
  y0 <- lubridate::year(first)
  areas <- if (length(unit$data_areas)) unit$data_areas else unit$area
  parts <- unlist(lapply(areas, function(a) lapply(.PC_GOLDEN_DATASETS, function(d)
    file.path(d, paste0("area_code=", a), paste0("year=", y0:y1)))))
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
  ## exclusao mutua (geradores de grupos distintos em paralelo): mkdir e atomico
  mtx <- paste0(lock_path, ".lck")
  got <- FALSE
  for (i in seq_len(.PC_GOLDEN_LOCK_TRIES)) { if (dir.create(mtx, showWarnings = FALSE)) { got <- TRUE; break }; Sys.sleep(0.1) }
  if (!got) prevcarga_abort("generic.unexpected", where = paste0("golden.lock ocupado (", basename(mtx), ")"))   # nunca apaga o lock de outro escritor
  on.exit(unlink(mtx, recursive = TRUE), add = TRUE)
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
  ru <- pc_golden_resolve_units(args$units, args$root, args$model)
  pre <- pc_golden_preflight(args)
  spec <- pc_golden_units(ru$path)
  units <- Filter(function(u) is.null(args$variant) || identical(u$variant, args$variant), pc_golden_group_units(spec, ru$group))
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
    files <- vapply(c("native.rds", .pc_golden_longs(d$dir), "invocation.json"), function(f) .pc_golden_sha(file.path(d$dir, f)), "")
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
