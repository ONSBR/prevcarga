## scripts/lib/cli/cache.R - verbo auto-registrado `cache` (v0.1.8; descoberto por scripts/lib/cli.R, M1.4).
##
## `cache import --from=<dir> [--dry-run]`: traz artefatos prontos (ex.: zip de um gerador externo, qualquer layout com
## v_<hash>/{object.rds|object.parquet, meta.json}) para o cache LOCAL do backtest (<state>/cache/<kind>/<hh>/<key>/).
## Verbo proprio e nao subcomando de `registry`: o registry e o ciclo de vida de producao (<state>/artifacts/, latest/pin),
## e o cache e content-addressed e so do backtest (cache.R); misturar os dois faria o backtest ler `latest` (proibido, M7.3).
## Fachada fina: a logica (varredura, validacao, copia atomica, compatibilidade) mora em cache.R. Texto devolve linhas ASCII,
## JSON devolve o `result`. Invalido/conflito => o relatorio sai e depois o erro classificado (exit 9); nada vazio => exit 13.
## `--dry-run` ja consta no bloco Options de train-models (duplicata torna o docopt ambiguo).
## Requer errors.R, config.R, provenance.R, cache.R.

.prevcarga_cache_text <- function(res) {
  st <- vapply(res$artifacts, function(a) a$status, "")
  head <- c(sprintf("cache import%s: de=%s cache=%s", if (res$dry_run) " (dry-run, nada gravado)" else "", res$from, res$cache_dir),
            sprintf("encontrados=%d %s=%d pulados=%d invalidos=%d conflitos=%d compativeis=%d incompativeis=%d",
                    res$found, if (res$dry_run) "a_importar" else "importados", res$imported, res$skipped, res$invalid,
                    res$conflict, res$compatible, res$incompatible),
            sprintf("codigo corrente: code_commit=%s renv_lock_hash=%s", res$code_commit, res$renv_lock_hash))
  bad <- vapply(res$artifacts[st %in% c("invalid", "conflict")], function(a)
    sprintf("%s %s %s %s", a$status, a$key, a$reason, a$source), "")
  warn <- vapply(Filter(function(a) a$status %in% c("imported", "skipped") && identical(a$compatible, FALSE), res$artifacts),
                 function(a) sprintf("aviso: %s (%s) %s: o backtest nao vai acha-lo (chave diferente)", a$key, a$kind,
                                     paste(vapply(as.character(a$divergent), function(d)
                                       sprintf("%s=%s corrente=%s", d, a[[d]], res[[d]]), ""), collapse = " ")), "")
  c(head, bad, warn)
}

cmd_cache <- function(ctx, args) {
  if (!isTRUE(args[["import"]])) prevcarga_abort("config.usage", where = "cache")
  from <- args[["--from"]]
  if (is.null(from) || identical(from, FALSE) || !length(from) || all(is.na(from)))
    prevcarga_abort("config.cache_import_from", where = "--from")
  res <- prevcarga_cache_import(ctx, as.character(from)[[1L]], dry_run = isTRUE(args[["--dry-run"]]))
  out <- if (identical(ctx$format, "json")) res else .prevcarga_cache_text(res)
  id <- if (res$conflict > 0L) "integrity.cache_import_conflict" else if (res$invalid > 0L) "integrity.cache_import_invalid"
        else if (res$found == 0L) "state.cache_import_empty" else NULL
  if (!is.null(id)) {
    if (!identical(ctx$format, "json")) writeLines(out, con = stdout())
    prevcarga_abort(id, where = res$from, details = list(invalid = res$invalid, conflict = res$conflict), result = res)
  }
  out
}

prevcarga_verb_cache <- list(
  name    = "cache",
  usage   = "prevcarga cache import --from=<dir> [--dry-run]",
  options = "--from=<dir>             Diretorio com artefatos v_<hash>/{object.rds|object.parquet,meta.json} (cache import)",
  scoped  = "--from",
  handler = function(ctx, args) cmd_cache(ctx, args),
  owner   = "v0.1.8",
  exempt  = FALSE,
  deck_roles = c("train", "inference"),
  modules = c("provenance.R", "cache.R")
)
