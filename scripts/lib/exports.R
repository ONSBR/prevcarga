## exports.R -- export da rodada: copia de BYTES do scratch para a chave de entrega (M6.2; ADR-0040, ADR-0030).
##
## O export so COPIA: nunca recalcula k-hat, nunca soma folhas, nunca reserializa parquet (o sha256 entregue e o da fonte).
## Os metadados da rodada (covered/stray/closed, khat, available_until, regra de clima, versao) vem do `run.json` do scratch,
## escrito pelo forecast; aqui sao copiados. O modulo nao le relogio: `gerado_em` vem de ctx$generated_at (a fachada le uma vez).
## Entrega atomica: staging `.tmp-<run_id>` no MESMO volume da chave final + file.rename; erro remove o staging.
## manifest.json: binario com LF final (mesmos bytes em Linux e Windows); nao entra na lista de arquivos nem em hash.
## Requer errors.R, schema_long.R (prevcarga_schema_long), data.table, jsonlite, digest. Nao abre parquet (leitor unico: deck_reader.R).

prevcarga_export_niveis <- c("sin", "subsistemas", "tudo")

.export_chr <- function(x) as.character(unlist(x, use.names = FALSE))

## run.json do scratch (covered, stray, closed, sin_closed, members, khat, ...); ausente/ilegivel => nada a entregar
.export_read_run <- function(scratch) {
  p <- file.path(scratch, "run.json")
  if (!file.exists(p)) prevcarga_abort("state.nothing_to_deliver", where = "prevcarga_export",
                                       details = list(reason = "run.json ausente no scratch", scratch = scratch))
  tryCatch(jsonlite::read_json(p, simplifyVector = FALSE),
           error = function(e) prevcarga_abort("state.nothing_to_deliver", where = "prevcarga_export",
                                               details = list(reason = "run.json ilegivel", scratch = scratch)))
}

## Tabela (rel, nivel, area) a entregar, com niveis-teto: sin = SIN + folhas soltas; subsistemas = + subsistemas;
## tudo = + folhas cobertas; membros so com incluir_membros. A folha solta e sempre entregue.
## Arquivo ausente no scratch ou nao registrado em run.json$arquivos gera aviso (nunca some em silencio); tabela vazia aborta antes de criar diretorio.
.export_select <- function(scratch, run, niveis, incluir_membros) {
  rel <- character(); nivel <- character(); area <- character()
  add <- function(r, n, a) { rel <<- c(rel, r); nivel <<- c(nivel, n); area <<- c(area, a) }
  if (isTRUE(run$sin_closed)) add("SIN.parquet", "sin", "SIN")
  if (niveis %in% c("subsistemas", "tudo"))
    for (ss in .export_chr(run$closed)) add(paste0("subsistemas/", ss, ".parquet"), "subsistema", ss)
  folhas <- c(.export_chr(run$stray), if (identical(niveis, "tudo")) .export_chr(run$covered))
  for (a in folhas) add(paste0("folhas/", a, ".parquet"), "folha", a)
  if (isTRUE(incluir_membros))
    for (m in names(run$members)) for (a in .export_chr(run$members[[m]]))
      add(paste0("membros/", m, "/", a, ".parquet"), "membro", a)
  sel <- data.table::data.table(rel = rel, nivel = nivel, area = area)
  sel[, src := file.path(scratch, rel)]
  listed <- vapply(run$arquivos, function(x) as.character(x$path), "")
  gone <- sel[!file.exists(src) | !rel %in% listed]
  if (nrow(gone))
    prevcarga_warn("completude.export_file_missing", where = "prevcarga_export",
                   details = list(arquivos = gone$rel))
  sel <- sel[file.exists(src) & rel %in% listed]
  if (!nrow(sel)) prevcarga_abort("state.nothing_to_deliver", where = "prevcarga_export", details = list(niveis = niveis))
  sel[order(rel, method = "radix")]
}

## sha256 do byte-stream da fonte, conferido contra o registro do run.json (arquivos[] gravado pelo reconcile/forecast);
## `linhas` vem desse registro (o export nao abre parquet). Divergencia de sha256 => integrity.copy_failed.
.export_entry <- function(src, rel, nivel, area, rec) {
  sha <- digest::digest(file = src, algo = "sha256")
  if (!identical(sha, as.character(rec$sha256)))
    prevcarga_abort("integrity.copy_failed", where = src,
                    details = list(reason = "sha256 da fonte difere do run.json", path = rel))
  list(path = rel, nivel = nivel, area = area, linhas = as.integer(rec$linhas), sha256 = sha)
}

## manifest em ordem de chaves fixa; campos da rodada copiados do run.json (khat nunca recalculado)
.export_manifest <- function(run, entries, niveis, ctx) {
  khat <- lapply(run$khat, function(k) list(ss = k$ss, versao = k$versao, khat_origin = k$khat_origin))
  if (!length(khat)) khat <- list()
  list(produto = "prevcarga-forecast",
       origem = run$origem,
       available_until = run$available_until,
       available_until_por_area = if (is.null(run$available_until_por_area)) structure(list(), names = character())
                                  else run$available_until_por_area,
       timezone = run$timezone,
       load_label = run$load_label,
       weather_issue_cutoff = run$weather_issue_cutoff,
       khat = khat,
       versao = run$versao,
       version = run$version,
       runtime_id = run$runtime_id,
       niveis = niveis,
       schema_long = as.list(prevcarga_schema_long),
       gerado_em = ctx$generated_at,
       arquivos = entries)
}

## JSON UTF-8 indent 2, LF, gravado em modo binario (nada de CRLF do modo texto no Windows)
.export_write_json <- function(obj, path) {
  txt <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = 2, null = "null", na = "null", digits = NA)
  bytes <- charToRaw(enc2utf8(paste0(gsub("\r\n", "\n", txt, fixed = TRUE), "\n")))
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(bytes, con)
  invisible(path)
}

.export_copy <- function(src, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(file.copy(src, dest, overwrite = FALSE, copy.date = FALSE)))
    prevcarga_abort("integrity.copy_failed", where = dest, details = list(src = src))
  invisible(dest)
}

## Entrega a rodada do scratch em `file.path(ctx$state_dir, key$rel)`. key = list(rel =, run_id =) (definida pelo M6.7).
## Devolve list(saida = <diretorio final>, arquivos = entries) para o result do verbo.
prevcarga_export <- function(scratch, key, niveis = "subsistemas", incluir_membros = FALSE, ctx) {
  if (!is.character(niveis) || length(niveis) != 1L || !niveis %in% prevcarga_export_niveis)
    prevcarga_abort("config.usage", where = "--niveis", details = list(hint = "sin, subsistemas ou tudo"))
  if (!is.character(scratch) || length(scratch) != 1L || !dir.exists(scratch))
    prevcarga_abort("state.nothing_to_deliver", where = "prevcarga_export", details = list(reason = "scratch ausente"))
  run <- .export_read_run(scratch)
  sel <- .export_select(scratch, run, niveis, incluir_membros)
  recs <- stats::setNames(run$arquivos, vapply(run$arquivos, function(x) as.character(x$path), ""))
  entries <- lapply(seq_len(nrow(sel)), function(i)
    .export_entry(sel$src[[i]], sel$rel[[i]], sel$nivel[[i]], sel$area[[i]], recs[[sel$rel[[i]]]]))
  final <- file.path(ctx$state_dir, key$rel)
  tmp <- file.path(dirname(final), paste0(".tmp-", key$run_id))
  unlink(tmp, recursive = TRUE)
  if (!dir.create(tmp, recursive = TRUE, showWarnings = FALSE))
    prevcarga_abort("integrity.copy_failed", where = final, details = list(reason = "nao criou o staging"))
  on.exit(if (dir.exists(tmp)) unlink(tmp, recursive = TRUE), add = TRUE)
  for (i in seq_len(nrow(sel))) .export_copy(sel$src[[i]], file.path(tmp, sel$rel[[i]]))
  .export_write_json(.export_manifest(run, entries, niveis, ctx), file.path(tmp, "manifest.json"))
  if (!file.rename(tmp, final)) prevcarga_abort("integrity.copy_failed", where = final, details = list(reason = "rename falhou"))
  list(saida = final, arquivos = entries)
}
