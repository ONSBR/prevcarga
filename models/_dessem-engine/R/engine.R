## Primitivas do proprio motor: caminho do JSON de parametros. O loader grava `.engine_dir` (diretorio R/
## do motor) no env do motor antes de checar `required_functions`; o JSON mora um nivel acima.
## Nao usa system.file, variavel de ambiente nem auto-localizacao.
engine_config_path <- function() {
  file.path(dirname(.engine_dir), engine_manifest()$config_file)
}

## Aborto classificado do catalogo (errors/dessem_engine.tsv). O env do motor so enxerga base + pacotes
## anexados pelo descriptor; as funcoes da CLI (errors.R) vivem no globalenv da facade, por isso a busca
## explicita. Sem a fundacao carregada (uso isolado do motor) cai em stop() com o mesmo id.
engine_abort <- function(id, where = NULL, details = NULL) {
  f <- get0("prevcarga_abort", envir = globalenv(), mode = "function", inherits = FALSE)
  if (is.null(f)) stop(paste0("[", id, "] ", where), call. = FALSE)
  f(id, where = where, details = details)
}
