## config.R -- unico canal de configuracao dos modulos (M1.4a; ADR-0037: sem variaveis de ambiente).
##
## Precedencia: flag > config.yaml (--config ou <root>/config.yaml) > default. Requer errors.R carregado antes
## (prevcarga_abort) e, para a raiz, runtime.R (prevcarga_install_root). Datas por lubridate (ymd, sem tz de
## relogio local). Saida de terminal: ASCII puro. Nada aqui cria diretorio.

## --- Schema v1 do config.yaml -------------------------------------------------------------------
.prevcarga_chr_ok <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)

## corte intradiario: so a grade de 30 min (HH:00 ou HH:30); usado por backtest.corte_default e por --corte (M7.1)
.prevcarga_corte_re <- "^([01][0-9]|2[0-3]):(00|30)$"

.prevcarga_config_schema <- list(
  schema_version = list(default = 1L, kind = "int"),
  data_train_dir = list(default = NULL, kind = "chr"),
  data_inference_dir = list(default = NULL, kind = "chr"),
  ## data_dir: apelido legado de data_train_dir (uma versao; warning.option_alias)
  data_dir = list(default = NULL, kind = "chr"),
  state_dir = list(default = NULL, kind = "chr"),
  artifacts_dir = list(default = NULL, kind = "chr"),
  load_label = list(default = NULL, kind = "enum", allowed = c("end", "start"),
                    error_id = "config.bad_load_label"),
  weather_issue_cutoff = list(default = "available_until", kind = "enum",
                              allowed = c("available_until", "origin_midnight_utc"),
                              error_id = "config.bad_weather_cutoff"),
  color = list(default = "auto", kind = "enum", allowed = c("auto", "always", "never"),
               error_id = "config.bad_color"),
  format = list(default = "text", kind = "enum", allowed = c("text", "json"),
                error_id = "config.bad_format"),
  backtest = list(default = list(corte_default = "08:00"), kind = "map")
)

.prevcarga_config_defaults <- function() {
  out <- lapply(.prevcarga_config_schema, function(s) s$default)
  out
}

.prevcarga_check_value <- function(key, v, s) {
  if (identical(s$kind, "int")) {
    ok <- (is.numeric(v) && length(v) == 1L && !is.na(v) && v == 1)
    if (!ok) prevcarga_abort("config.bad_type", where = key, details = list(expected = "1"))
  } else if (identical(s$kind, "chr")) {
    if (!.prevcarga_chr_ok(v))
      prevcarga_abort("config.bad_type", where = key, details = list(expected = "texto nao vazio"))
  } else if (identical(s$kind, "enum")) {
    if (!(is.character(v) && length(v) == 1L && !is.na(v)))
      prevcarga_abort("config.bad_type", where = key, details = list(expected = s$allowed))
    if (!(v %in% s$allowed))
      prevcarga_abort(s$error_id, where = key, details = list(allowed = s$allowed, got = v))
  }
  invisible(v)
}

prevcarga_config_load <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path))
    prevcarga_abort("config.bad_type", where = as.character(path)[1L], details = list(reason = "arquivo ausente"))
  raw <- tryCatch(yaml::read_yaml(path, eval.expr = FALSE),
                  error = function(e) prevcarga_abort("config.bad_type", where = path,
                                                      details = list(reason = "yaml invalido")))
  if (is.null(raw)) raw <- list()
  if (!is.list(raw) || (length(raw) && (is.null(names(raw)) || any(!nzchar(names(raw))))))
    prevcarga_abort("config.bad_type", where = path, details = list(reason = "raiz deve ser um mapa"))
  sch <- .prevcarga_config_schema
  bad <- setdiff(names(raw), names(sch))
  if (length(bad)) prevcarga_abort("config.unknown_key", where = bad[[1L]])
  out <- .prevcarga_config_defaults()
  for (k in names(raw)) {
    v <- raw[[k]]; s <- sch[[k]]
    if (identical(s$kind, "map")) {
      if (is.null(v)) next
      if (!is.list(v) || (length(v) && is.null(names(v))))
        prevcarga_abort("config.bad_type", where = k, details = list(expected = "mapa"))
      extra <- setdiff(names(v), "corte_default")
      if (length(extra)) prevcarga_abort("config.unknown_key", where = paste0(k, ".", extra[[1L]]))
      cd <- v[["corte_default"]]
      if (!is.null(cd) && !(is.character(cd) && length(cd) == 1L && grepl(.prevcarga_corte_re, cd)))
        prevcarga_abort("config.bad_type", where = "backtest.corte_default", details = list(expected = "HH:00 ou HH:30"))
      out[[k]] <- list(corte_default = if (is.null(cd)) .prevcarga_config_schema$backtest$default$corte_default else cd)
    } else if (!is.null(v)) {
      .prevcarga_check_value(k, v, s)
      out[[k]] <- if (identical(s$kind, "int")) as.integer(v) else v
    }
  }
  if (is.null(raw[["schema_version"]]))
    prevcarga_abort("config.bad_type", where = "schema_version", details = list(reason = "obrigatorio"))
  out
}

## --- Validadores das flags globais --------------------------------------------------------------
prevcarga_check_enum <- function(x, allowed, id) {
  if (is.null(x)) return(NULL)
  if (!(is.character(x) && length(x) == 1L && !is.na(x) && x %in% allowed))
    prevcarga_abort(id, details = list(allowed = allowed, got = paste(as.character(x), collapse = ",")))
  x
}

prevcarga_parse_origin <- function(x, verb = NULL) {
  if (is.null(x)) return(NULL)
  if (!(is.character(x) && length(x) == 1L && !is.na(x)))
    prevcarga_abort("config.bad_origin", where = "--origin")
  iso <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  one <- grepl(paste0("^", iso, "$"), x)
  rng <- grepl(paste0("^", iso, "\\.\\.", iso, "$"), x)
  if (!one && !(rng && !is.null(verb) && verb %in% c("backtest", "plan")))
    prevcarga_abort("config.bad_origin", where = "--origin", details = list(got = x))
  parts <- if (rng) strsplit(x, "..", fixed = TRUE)[[1L]] else x
  d <- lubridate::ymd(parts, quiet = TRUE)
  if (anyNA(d) || (length(d) == 2L && d[1L] > d[2L]))
    prevcarga_abort("config.bad_origin", where = "--origin", details = list(got = x))
  if (length(d) == 1L) d else list(from = d[1L], to = d[2L])
}

## `--origin A..B` so vale em backtest/plan (M7.1): devolve list(from, to) de Datas; o unico flag humano de data segue
## sendo --origin. Nos demais verbos (ou fora de A..B ISO com A <= B) => config.bad_origin (exit 8).
prevcarga_parse_origin_range <- function(x, verb = NULL) {
  r <- prevcarga_parse_origin(x, verb)
  if (is.null(r)) return(NULL)
  if (inherits(r, "Date")) list(from = r, to = r) else r
}

## --- Montagem do ctx -------------------------------------------------------------------------------
.prevcarga_pick <- function(flag, cfg, default) {
  if (!is.null(flag) && length(flag) && !identical(flag, FALSE) && !is.na(flag[[1L]])) return(flag)
  if (!is.null(cfg)) return(cfg)
  default
}

.prevcarga_norm_path <- function(p) {
  if (is.null(p)) return(NULL)
  normalizePath(p, winslash = "/", mustWork = FALSE)
}

## apelido x nome novo (E1-T2): novo vence; so apelido => aviso warning.option_alias; os dois com valores diferentes => config.usage
.prevcarga_pick_alias <- function(new_val, alias_val, new_name, alias_name) {
  if (!is.null(new_val) && !is.null(alias_val)) {
    if (!identical(.prevcarga_norm_path(new_val), .prevcarga_norm_path(alias_val)))
      prevcarga_abort("config.usage", where = paste(alias_name, new_name, sep = " x "),
                      details = list(hint = sprintf("use so %s", new_name)))
    return(new_val)
  }
  if (!is.null(alias_val)) {
    prevcarga_warn("warning.option_alias", where = alias_name, details = list(alias = alias_name, novo = new_name))
    return(alias_val)
  }
  new_val
}

prevcarga_run_id <- function() {
  sprintf("%s-%d", format(lubridate::now(tzone = "UTC"), "%Y%m%dT%H%M%SZ"), Sys.getpid())
}

## Raiz da INSTALACAO do usuario (M8.3): o codigo instalado mora em <R>/versions/<v>/code, e o config.yaml da instalacao em
## <R>/config.yaml. Fora desse layout (arvore de dev, copia de teste) devolve NULL.
prevcarga_install_dir <- function(root) {
  r <- gsub("\\", "/", root, fixed = TRUE)
  v <- dirname(r)
  if (identical(basename(r), "code") && identical(basename(dirname(v)), "versions")) dirname(dirname(v)) else NULL
}

prevcarga_ctx <- function(args, root, verb = NULL) {
  flag <- function(n) {
    v <- args[[n]]
    if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else v
  }
  cfg_path <- flag("--config")
  if (is.null(cfg_path)) {
    cand <- file.path(root, "config.yaml")
    if (file.exists(cand)) cfg_path <- cand
    inst <- prevcarga_install_dir(root)
    if (is.null(cfg_path) && !is.null(inst) && file.exists(file.path(inst, "config.yaml")))
      cfg_path <- file.path(inst, "config.yaml")
  }
  cfg <- if (is.null(cfg_path)) .prevcarga_config_defaults() else prevcarga_config_load(cfg_path)
  load_label_flag <- prevcarga_check_enum(flag("--load-label"), c("end", "start"), "config.bad_load_label")
  weather <- prevcarga_check_enum(
    .prevcarga_pick(flag("--weather-cutoff"), cfg$weather_issue_cutoff, "available_until"),
    c("available_until", "origin_midnight_utc"), "config.bad_weather_cutoff")
  fmt <- prevcarga_check_enum(.prevcarga_pick(flag("--format"), cfg$format, "text"),
                              c("text", "json"), "config.bad_format")
  color <- prevcarga_check_enum(.prevcarga_pick(flag("--color"), cfg$color, "auto"),
                                c("auto", "always", "never"), "config.bad_color")
  origin <- prevcarga_parse_origin(flag("--origin"), verb)
  ## raiz de artefatos apontada (so leitura): flag > config.yaml > NULL (sem env var); tem de existir
  ad <- .prevcarga_norm_path(.prevcarga_pick(
    .prevcarga_pick_alias(flag("--data-artifacts"), flag("--artifacts-dir"), "--data-artifacts", "--artifacts-dir"),
    cfg$artifacts_dir, NULL))
  if (!is.null(ad) && !dir.exists(ad))
    prevcarga_abort("config.artifacts_dir_missing", where = ad,
                    details = list(hint = "aponte --data-artifacts para um diretorio de artefatos que exista"))
  ## papeis de dados: flag (nome novo x apelido) > config.yaml (data_train_dir x data_dir)
  cfg_train <- cfg$data_train_dir
  if (!is.null(cfg$data_dir)) {
    if (is.null(cfg_train)) {
      prevcarga_warn("warning.option_alias", where = "data_dir", details = list(alias = "data_dir", novo = "data_train_dir"))
      cfg_train <- cfg$data_dir
    } else if (!identical(.prevcarga_norm_path(cfg_train), .prevcarga_norm_path(cfg$data_dir)))
      prevcarga_warn("warning.option_alias", where = "data_dir", details = list(alias = "data_dir", novo = "data_train_dir"))
  }
  train <- .prevcarga_norm_path(.prevcarga_pick(
    .prevcarga_pick_alias(flag("--data-train"), flag("--data"), "--data-train", "--data"), cfg_train, NULL))
  infer <- .prevcarga_norm_path(.prevcarga_pick(flag("--data-inference"), cfg$data_inference_dir, NULL))
  vf <- file.path(root, "config", "VERSION")
  version <- if (file.exists(vf)) trimws(readLines(vf, n = 1L, warn = FALSE)) else "unknown"
  list(
    root = root,
    install_root = prevcarga_install_dir(root),
    config_path = if (is.null(cfg_path)) NULL else .prevcarga_norm_path(cfg_path),
    data_train_dir = train,
    data_inference_dir = infer,
    data_dir = train,  # deck ATIVO provisorio; prevcarga_resolve_deck_role escolhe por papel (verbo nao exempt)
    state_dir = .prevcarga_norm_path(.prevcarga_pick(flag("--state"), cfg$state_dir, file.path(root, ".state"))),
    artifacts_dir = ad,
    origin = origin,
    load_label_flag = load_label_flag,
    load_label_config = cfg$load_label,
    weather_issue_cutoff = weather,
    format = fmt,
    color = color,
    debug = isTRUE(args[["--debug"]]),
    backtest = list(corte_default = cfg$backtest$corte_default),
    version = version,
    run_id = prevcarga_run_id(),
    verb = verb)
}

## Papel do deck do verbo (ADR de papeis de deck): flag > chave de treino > chave de inferencia > train.
## roles = subconjunto de c("train","inference") aceito pelo verbo. Define ctx$data_role, ctx$data_role_option e ctx$data_dir
## (o deck ativo). Recusa de papel (contract.deck_role_refused) e conflito de flags (config.usage) antes de qualquer leitura.
prevcarga_resolve_deck_role <- function(ctx, roles = "train", args = list()) {
  has <- function(n) { v <- args[[n]]; !(is.null(v) || !length(v) || identical(v, FALSE) || all(is.na(v))) }
  f_train <- has("--data-train") || has("--data")
  f_inf <- has("--data-inference")
  if (f_train && f_inf) prevcarga_abort("config.usage", where = "--data-train x --data-inference")
  role <- if (f_inf) "inference" else if (f_train) "train" else if (!is.null(ctx$data_train_dir)) "train" else
    if (!is.null(ctx$data_inference_dir)) "inference" else "train"
  opt <- c(train = "--data-train", inference = "--data-inference")[[role]]
  if (!role %in% roles)
    prevcarga_abort("contract.deck_role_refused", where = opt,
                    details = list(role = role, verb = ctx$verb, accepted = as.list(roles)))
  ctx$data_role <- role
  ctx$data_role_option <- opt
  ctx$data_dir <- if (identical(role, "inference")) ctx$data_inference_dir else ctx$data_train_dir
  ctx
}

## Guarda para verbos que leem o deck (validate-deck e demais do M2+).
prevcarga_require_data_dir <- function(ctx) {
  if (is.null(ctx$data_dir))
    prevcarga_abort("config.data_dir_missing", where = "--data-train",
                    details = list(hint = "use --data-train=<dir> / --data-inference=<dir> ou data_train_dir / data_inference_dir no config.yaml"))
  invisible(ctx)
}

## --- set-data-dir: grava data_dir no config.yaml (unico escritor do config.yaml dentro do pacote) ----------------------------
## Alvo: o config.yaml em uso (--config, o da raiz do codigo ou o da instalacao); sem nenhum, <R>/config.yaml da instalacao,
## criado com schema_version. Fora do layout instalado e sem config.yaml: config.usage (aponte --config). O diretorio tem de
## existir; o caminho vai absoluto, com barra /, entre aspas simples (a mesma forma do instalador, que o le e preserva no
## upgrade). Escrita atomica: arquivo temporario no mesmo diretorio, validado com prevcarga_config_load antes do rename.
.prevcarga_yaml_q <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

prevcarga_config_set_data_dir <- function(ctx, dir) {
  if (!.prevcarga_chr_ok(dir) || !dir.exists(dir))
    prevcarga_abort("config.data_dir_missing", where = as.character(dir)[1L],
                    details = list(hint = "aponte para um diretorio que exista (o deck)"))
  d <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  target <- ctx$config_path
  if (is.null(target) && !is.null(ctx$install_root)) target <- file.path(ctx$install_root, "config.yaml")
  if (is.null(target))
    prevcarga_abort("config.usage", where = "set-data-dir",
                    details = list(hint = "fora de uma instalacao, passe --config=<arquivo>"))
  old <- if (file.exists(target)) readLines(target, warn = FALSE, encoding = "UTF-8") else
    c("# config.yaml da instalacao (schema v1). Precedencia: flag da CLI > este arquivo > default.", "schema_version: 1")
  prev <- if (file.exists(target)) prevcarga_config_load(target)$data_dir else NULL
  lines <- c(old[!startsWith(old, "data_dir:")], paste0("data_dir: ", .prevcarga_yaml_q(d)))
  tmp <- tempfile(".config.yaml.", tmpdir = dirname(target))
  ok <- tryCatch({
    con <- file(tmp, open = "wb")
    tryCatch(writeLines(enc2utf8(lines), con, useBytes = TRUE), finally = close(con))   # fechado antes do rename (Windows)
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(tmp)) {
    unlink(tmp)
    prevcarga_abort("config.write_failed", where = target)
  }
  tryCatch(prevcarga_config_load(tmp), error = function(e) { unlink(tmp); stop(e) })
  if (!file.rename(tmp, target)) {
    unlink(tmp)
    prevcarga_abort("config.write_failed", where = target)
  }
  list(data_dir = d, config_path = .prevcarga_norm_path(target), previous = prev)
}
