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
  backtest = list(default = list(corte_default = "08:00"), kind = "map"),
  combina = list(default = list(window_months = 24L), kind = "map")
)

## Subchaves de cada mapa: validador (TRUE = ok) e o que a mensagem de config.bad_type diz que se espera.
.prevcarga_config_map_keys <- list(
  backtest = list(corte_default = list(
    ok = function(v) is.character(v) && length(v) == 1L && grepl(.prevcarga_corte_re, v),
    expected = "HH:00 ou HH:30")),
  combina = list(window_months = list(
    ok = function(v) is.numeric(v) && length(v) == 1L && !is.na(v) && v >= 0 && v == floor(v),
    expected = "inteiro >= 0 (meses; 0 = sem janela)",
    cast = as.integer))
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
      spec <- .prevcarga_config_map_keys[[k]]
      extra <- setdiff(names(v), names(spec))
      if (length(extra)) prevcarga_abort("config.unknown_key", where = paste0(k, ".", extra[[1L]]))
      m <- s$default
      for (sk in names(spec)) {
        sv <- v[[sk]]
        if (is.null(sv)) next
        if (!isTRUE(spec[[sk]]$ok(sv)))
          prevcarga_abort("config.bad_type", where = paste0(k, ".", sk), details = list(expected = spec[[sk]]$expected))
        m[[sk]] <- if (is.null(spec[[sk]]$cast)) sv else spec[[sk]]$cast(sv)
      }
      out[[k]] <- m
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
  ad <- .prevcarga_norm_path(.prevcarga_pick(flag("--data-artifacts"), cfg$artifacts_dir, NULL))
  if (!is.null(ad) && !dir.exists(ad))
    prevcarga_abort("config.artifacts_dir_missing", where = ad,
                    details = list(hint = "aponte --data-artifacts para um diretorio de artefatos que exista"))
  ## papeis de dados: flag > config.yaml (sem apelidos: --data, --artifacts-dir e data_dir nao existem mais)
  train <- .prevcarga_norm_path(.prevcarga_pick(flag("--data-train"), cfg$data_train_dir, NULL))
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
    combina = list(window_months = cfg$combina$window_months),
    version = version,
    run_id = prevcarga_run_id(),
    verb = verb)
}

## Papel do deck do verbo (ADR de papeis de deck): flag > chave de treino > chave de inferencia > train.
## roles = subconjunto de c("train","inference") aceito pelo verbo. Define ctx$data_role, ctx$data_role_option e ctx$data_dir
## (o deck ativo). Recusa de papel (contract.deck_role_refused) e conflito de flags (config.usage) antes de qualquer leitura.
prevcarga_resolve_deck_role <- function(ctx, roles = "train", args = list()) {
  has <- function(n) { v <- args[[n]]; !(is.null(v) || !length(v) || identical(v, FALSE) || all(is.na(v))) }
  f_train <- has("--data-train")
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

## --- set-data-dir: grava as chaves de dados no config.yaml (unico escritor do config.yaml dentro do pacote) ----------------------------
## Alvo: o config.yaml em uso (--config, o da raiz do codigo ou o da instalacao); sem nenhum, <R>/config.yaml da instalacao,
## criado com schema_version. Fora do layout instalado e sem config.yaml: config.usage (aponte --config). O diretorio tem de
## existir; o caminho vai absoluto, com barra /, entre aspas simples (a mesma forma do instalador, que o le e preserva no
## upgrade). Escrita atomica: arquivo temporario no mesmo diretorio, validado com prevcarga_config_load antes do rename.
.prevcarga_yaml_q <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

## Chaves de dados gravaveis pelo set-data-dir (E1-T5). data_train_dir tem de existir; os outros o chamador pode criar.
.prevcarga_data_keys <- c("data_train_dir", "data_inference_dir", "artifacts_dir")

## Troca so as linhas das chaves dadas em `values` (lista nomeada).
## Preserva comentarios e demais linhas. Devolve list(config_path, changes = list({key, old, new})).
prevcarga_config_set_keys <- function(ctx, values) {
  bad <- setdiff(names(values), .prevcarga_data_keys)
  if (!length(values) || is.null(names(values)) || length(bad))
    prevcarga_abort("config.usage", where = "set-data-dir",
                    details = list(hint = "chaves aceitas: data_train_dir, data_inference_dir, artifacts_dir"))
  for (k in names(values))
    if (!.prevcarga_chr_ok(values[[k]]))
      prevcarga_abort("config.usage", where = k, details = list(hint = "informe um caminho de diretorio"))
  if ("data_train_dir" %in% names(values) && !dir.exists(values[["data_train_dir"]]))
    prevcarga_abort("config.data_dir_missing", where = as.character(values[["data_train_dir"]]),
                    details = list(hint = "aponte para um diretorio que exista (o deck)"))
  vals <- lapply(values, function(v) .prevcarga_norm_path(v))
  target <- ctx$config_path
  if (is.null(target) && !is.null(ctx$install_root)) target <- file.path(ctx$install_root, "config.yaml")
  if (is.null(target))
    prevcarga_abort("config.usage", where = "set-data-dir",
                    details = list(hint = "fora de uma instalacao, passe --config=<arquivo>"))
  old <- if (file.exists(target)) readLines(target, warn = FALSE, encoding = "UTF-8") else
    c("# config.yaml da instalacao (schema v1). Precedencia: flag da CLI > este arquivo > default.", "schema_version: 1")
  prev <- if (file.exists(target)) prevcarga_config_load(target) else list()
  drop <- names(vals)
  gone <- vapply(old, function(l) any(startsWith(l, paste0(drop, ":"))), NA)
  new_lines <- vapply(names(vals), function(k) paste0(k, ": ", .prevcarga_yaml_q(vals[[k]])), "")
  lines <- c(old[!gone], unname(new_lines))
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
  changes <- lapply(names(vals), function(k) {
    list(key = k, old = prev[[k]], new = vals[[k]])
  })
  list(config_path = .prevcarga_norm_path(target), changes = changes)
}

## Wrapper dos chamadores antigos: grava so data_train_dir.
prevcarga_config_set_data_dir <- function(ctx, dir) {
  if (!.prevcarga_chr_ok(dir) || !dir.exists(dir))
    prevcarga_abort("config.data_dir_missing", where = as.character(dir)[1L],
                    details = list(hint = "aponte para um diretorio que exista (o deck)"))
  res <- prevcarga_config_set_keys(ctx, list(data_train_dir = dir))
  ch <- res$changes[[1L]]
  list(data_dir = ch$new, config_path = res$config_path, previous = ch$old)
}

## Modo interativo (sem chave): pergunta treino, artefatos e inferencia (linha vazia aceita a sugestao <base>/train|artifacts|
## inference), mostra o resumo 'chave: antigo -> novo' e so grava com 's'/'S'. Sem terminal aborta antes de ler qualquer coisa.
## Entrada por readLines(input, n = 1L), sem a leitura interativa do R base. O de treino tem de existir; artefatos e inferencia sao criados.
prevcarga_set_data_dir_interactive <- function(ctx, base, input = stdin(), is_tty = isatty(stdin()), output = stdout()) {
  if (!isTRUE(is_tty))
    prevcarga_abort("contract.set_data_dir_noninteractive", where = "set-data-dir",
                    details = list(hint = "passe --train / --inference / --artifacts"))
  ask <- function(key, sug) {
    cat(sprintf("%s [%s]: ", key, sug), file = output)
    a <- readLines(input, n = 1L, warn = FALSE)
    if (!length(a)) return(NULL)
    a <- trimws(a)
    if (nzchar(a)) a else sug
  }
  keys <- c(data_train_dir = "train", artifacts_dir = "artifacts", data_inference_dir = "inference")
  want <- list()
  for (k in names(keys)) {
    a <- ask(k, file.path(base, keys[[k]]))
    if (is.null(a)) {
      cat("\nnada gravado\n", file = output)
      return(list(config_path = ctx$config_path, changes = list(), gravado = FALSE))
    }
    want[[k]] <- .prevcarga_norm_path(a)
  }
  prev <- if (!is.null(ctx$config_path) && file.exists(ctx$config_path)) prevcarga_config_load(ctx$config_path) else list()
  oldv <- function(k) prev[[k]]
  chg <- Filter(function(k) !identical(oldv(k), want[[k]]), names(want))
  for (k in chg)
    cat(sprintf("%s: %s -> %s\n", k, if (is.null(oldv(k))) "-" else oldv(k), want[[k]]), file = output)
  if (!length(chg)) {
    cat("nada a mudar\n", file = output)
    return(list(config_path = ctx$config_path, changes = list(), gravado = FALSE))
  }
  cat("gravar? [s/N]: ", file = output)
  r <- readLines(input, n = 1L, warn = FALSE)
  if (!length(r) || !identical(tolower(trimws(r)), "s")) {
    cat("\nnada gravado\n", file = output)
    return(list(config_path = ctx$config_path, changes = list(), gravado = FALSE))
  }
  if ("data_train_dir" %in% chg && !dir.exists(want$data_train_dir))
    prevcarga_abort("config.data_dir_missing", where = want$data_train_dir,
                    details = list(hint = "o diretorio de treino tem de existir"))
  for (k in setdiff(chg, "data_train_dir"))
    if (!dir.exists(want[[k]])) dir.create(want[[k]], recursive = TRUE, showWarnings = FALSE)
  res <- prevcarga_config_set_keys(ctx, want[chg])
  c(res, list(gravado = TRUE))
}

## Relogio de parede do prefixo do progresso (prevcarga_say, 0.1.8): "HH:MM:SSZ" em UTC. So exibicao, nunca identidade.
prevcarga_wall_hms <- function() format(lubridate::now(tzone = "UTC"), "%H:%M:%SZ")
