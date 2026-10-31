## ab_cases.R -- matriz de casos do A/B do corte (M8.8; ADR-0034, ADR-0035). Ferramenta de DESENVOLVIMENTO (so Linux, fora do bundle).
## Requer carregados (nesta ordem): errors.R, runtime.R (forca TZ/locale), topology.R, deck_reader.R, dates.R, schema_long.R, registry.R
## (prevcarga_plugin_descriptor), e jsonlite/yaml/lubridate. Nao le variavel de ambiente; datas so por lubridate com tz explicito.
##
## A matriz (membro, area) vem de prevcarga_members()/prevcarga_topology() do clean-room (nunca lista fixa em bash). Para cada origem do
## tests/ab/ab_origens.yaml o gerador le o deck pelo leitor unico (corte derivado do DADO), exige hora cheia, calcula o `corte` do ORACULO
## na convencao de rotulo dele (available_until - 30 min: o oraculo rotula pelo INICIO do intervalo, ADR-0035 item 8) pela borda do LONG
## (schema_long.R) e grava cases.tsv para o runner (ab_oracle_run.sh). O protocolo vem do job.json do oraculo (lido, nunca forcado).

pc_ab_case_cols <- c("caso", "membro", "sigla", "mdir", "exogena", "area", "origem", "deck", "tz", "available_until", "corte_oraculo",
                     "protocolo", "split", "delivery_days", "executavel", "motivo")

pc_ab_origens_load <- function(path) {
  bad <- function(why) prevcarga_abort("contract.ab_origens_bad", where = path, details = list(why = why))
  y <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (!is.list(y) || !is.list(y$origens) || !length(y$origens)) bad("sem a lista `origens`")
  unk <- setdiff(names(y), c("weather_cutoff_clean_room", "origens", "sinteticos"))
  if (length(unk)) bad(paste("campo de topo desconhecido:", unk[[1L]]))
  if (!is.null(y$weather_cutoff_clean_room) && !(y$weather_cutoff_clean_room %in% c("available_until", "origin_midnight_utc")))
    bad("weather_cutoff_clean_room invalido")
  ok <- c("origem", "deck", "available_until_esperado", "executavel")
  rows <- lapply(y$origens, function(o) {
    if (!is.list(o) || length(setdiff(names(o), ok))) bad("entrada com campo desconhecido")
    if (!is.character(o$origem) || is.na(lubridate::ymd(o$origem, quiet = TRUE))) bad("origem deve ser AAAA-MM-DD")
    if (!is.character(o$deck) || !nzchar(o$deck)) bad("deck obrigatorio")
    if (!is.character(o$available_until_esperado) || !grepl("^[0-9]{2}:[0-9]{2}$", o$available_until_esperado)) bad("available_until_esperado HH:MM")
    data.frame(origem = o$origem, deck = o$deck, esperado = o$available_until_esperado,
               executavel = if (is.null(o$executavel)) TRUE else isTRUE(o$executavel), stringsAsFactors = FALSE)
  })
  list(origens = do.call(rbind, rows), sinteticos = y$sinteticos, weather_cutoff = y$weather_cutoff_clean_room)
}

## corte do oraculo = available_until expresso no rotulo de INICIO (a unica aritmetica de rotulo: a borda do LONG em schema_long.R)
pc_ab_oracle_cut <- function(available_until, tz) prevcarga_long_interval_start(available_until, 30L, tz)

## origens: caminho do YAML ou o resultado de pc_ab_origens_load. deck_dir: raiz dos decks nomeados (`store` usa ctx$data_dir).
## membros/areas: filtros opcionais da matriz (subconjunto representativo); NULL = a matriz inteira.
pc_ab_cases <- function(origens, deck_dir, ctx, oracle = "/opt/ons-source/prevcargadessem3", membros = NULL, areas = NULL) {
  o <- if (is.character(origens)) pc_ab_origens_load(origens) else origens
  mem <- prevcarga_members(root = ctx$root)
  topo <- prevcarga_topology(root = ctx$root)
  leaves <- if (is.null(areas)) topo$leaves else intersect(topo$leaves, areas)
  if (!is.null(membros)) mem <- mem[mem$sigla %in% membros, , drop = FALSE]
  info <- lapply(unique(mem$mdir), function(m)
    list(split = prevcarga_model_spec(m, ctx$root)$split,
         dd = as.integer(prevcarga_plugin_descriptor(m, prevcarga_models_root(ctx$root))$delivery_days),
         protocolo = jsonlite::read_json(file.path(oracle, "models", m, "job.json"))$protocol))
  names(info) <- unique(mem$mdir)
  rows <- list()
  for (i in seq_len(nrow(o$origens))) {
    e <- o$origens[i, ]
    dctx <- ctx
    dctx$data_dir <- if (identical(e$deck, "store")) ctx$data_dir else file.path(deck_dir, e$deck)
    day <- lubridate::ymd(e$origem)
    tz <- prevcarga_deck_manifest(dctx$data_dir)$timezone$load
    want <- lubridate::ymd_hm(paste(e$origem, e$esperado), tz = tz)
    ld <- prevcarga_read_load(dctx, origin = day, from = day, to = day, truncate_at = want)
    au <- attr(ld, "available_until")
    if (lubridate::minute(au) != 0L || !identical(as.numeric(au), as.numeric(want)))
      prevcarga_abort("contract.ab_deck_not_full_hour", where = paste(e$deck, e$origem),
                      details = list(derivado = format(au, "%Y-%m-%dT%H:%M", tz = tz), esperado = e$esperado))
    au_txt <- format(au, "%Y-%m-%dT%H:%M", tz = tz)
    cut <- pc_ab_oracle_cut(au, tz)
    same_day <- identical(as.character(lubridate::as_date(cut, tz = tz)), e$origem) && lubridate::as_date(au, tz = tz) == day &&
      !(lubridate::hour(au) == 0L && lubridate::minute(au) == 0L)
    for (j in seq_len(nrow(mem))) {
      m <- mem[j, ]
      fi <- info[[m$mdir]]
      membro <- if (is.na(m$exogena)) m$mdir else paste0(m$mdir, "/", m$sigla)
      for (a in leaves) {
        rows[[length(rows) + 1L]] <- data.frame(
          caso = sprintf("%s.%s.%s.%s", m$sigla, a, e$origem, gsub(":", "", e$esperado)), membro = membro, sigla = m$sigla, mdir = m$mdir,
          exogena = m$exogena, area = a, origem = e$origem, deck = e$deck, tz = tz, available_until = au_txt,
          corte_oraculo = format(cut, "%Y-%m-%dT%H:%M", tz = tz), protocolo = fi$protocolo, split = fi$split,
          delivery_days = fi$dd, executavel = isTRUE(e$executavel) && same_day,
          motivo = if (!isTRUE(e$executavel)) "marcado executavel:false em ab_origens.yaml"
                   else if (!same_day) "corte do oraculo cai na vespera de O (sem manha)" else "",
          stringsAsFactors = FALSE)
      }
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$origem, out$mdir, out$sigla, out$area, method = "radix"), pc_ab_case_cols]
  rownames(out) <- NULL
  out
}

pc_ab_cases_write <- function(cases, path) {
  stopifnot(identical(names(cases), pc_ab_case_cols))
  x <- cases
  x$exogena[is.na(x$exogena)] <- ""
  x$executavel <- ifelse(x$executavel, "true", "false")
  utils::write.table(x, path, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  invisible(path)
}

pc_ab_cases_read <- function(path) {
  x <- utils::read.delim(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = character())
  stopifnot(identical(names(x), pc_ab_case_cols))
  x$exogena[x$exogena == ""] <- NA_character_
  x$executavel <- x$executavel == "true"
  x$delivery_days <- as.integer(x$delivery_days)
  x
}

## O store real (data/) nao tem manifest.json: o overlay e um diretorio com symlinks para os 4 datasets e o manifest de config/deck-manifest.store.json
## (so Linux, dev). O store nunca e escrito. Devolve `dir` (o data_dir do deck).
pc_ab_store_overlay <- function(store, manifest, dir) {
  ## `dir` tem de ser NOVO: file.symlink() num destino ja existente (link para diretorio) cria o link DENTRO do alvo, ou seja, dentro do store
  if (dir.exists(dir) && length(list.files(dir, all.files = TRUE, no.. = TRUE)))
    prevcarga_abort("contract.ab_stage_input", where = dir, details = list(reason = "overlay deve ser um diretorio novo (nunca reaproveitar)"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (rel in c("load", file.path("weather", "observed"), file.path("weather", "forecast"), file.path("auxiliary", "holidays"))) {
    dir.create(dirname(file.path(dir, rel)), recursive = TRUE, showWarnings = FALSE)
    rl <- Sys.readlink(file.path(dir, rel))
    if ((!is.na(rl) && nzchar(rl)) || file.exists(file.path(dir, rel)) ||
        !file.symlink(normalizePath(file.path(store, rel), mustWork = TRUE), file.path(dir, rel)))
      prevcarga_abort("contract.ab_stage_input", where = file.path(store, rel))
  }
  file.copy(manifest, file.path(dir, "manifest.json"), overwrite = TRUE)
  invisible(dir)
}
