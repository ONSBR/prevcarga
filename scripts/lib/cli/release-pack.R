## scripts/lib/cli/release-pack.R - verbo auto-registrado `release-pack` (M8.5; descoberto por scripts/lib/cli.R, barrel por descoberta).
##
## Fachada fina para prevcarga_release_pack (release_pack.R): so valida flag e compoe o result. LOCAL: sem rede, sem env vars.
## Monta `--out` no layout do bucket; nao envia nada (o envio e do utilitario externo scripts/tools/upload). Flags: `--origin` e
## `--model` sao as globais (--model aceita lista separada por virgula); `--out` ja consta no bloco Options (combine). A versao do
## release e `--release` porque `--version` e a flag global; `--latest` escreve os ponteiros mutaveis (latest.txt), nunca por padrao.
## Erros e exits vem do catalogo (errors/release_pack.tsv + core + export): 8 config, 9 integrity, 13 state (ADR-0040).
## Requer errors.R, config.R (da fachada), registry.R, topology.R, exports.R, release_pack.R.

.prevcarga_rp_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

.render_release_pack_text <- function(r)
  c(sprintf("release-pack  what=%s  chaves=%d  arquivos=%d", r$what, length(r$chaves), r$arquivos),
    paste0("saida: ", r$out),
    paste0("chave ", r$chaves),
    if (isTRUE(r$latest)) "latest.txt escrito" else "latest.txt nao escrito",
    "OK")

cmd_release_pack <- function(ctx, args) {
  what <- .prevcarga_rp_opt(args, "--what")
  if (is.null(what)) prevcarga_abort("config.usage", where = "--what")
  out <- .prevcarga_rp_opt(args, "--out")
  if (is.null(out)) prevcarga_abort("config.usage", where = "--out")
  pubf <- .prevcarga_rp_opt(args, "--published")
  if (is.null(pubf)) prevcarga_abort("config.usage", where = "--published")
  mdl <- .prevcarga_rp_opt(args, "--model")
  models <- if (is.null(mdl)) NULL else Filter(nzchar, trimws(strsplit(mdl, ",", fixed = TRUE)[[1L]]))
  res <- prevcarga_release_pack(what, out, pubf, ctx, version = .prevcarga_rp_opt(args, "--release"),
                                bundles = .prevcarga_rp_opt(args, "--bundles"), models = models,
                                origin = .prevcarga_rp_opt(args, "--origin"), latest = isTRUE(args[["--latest"]]))
  if (identical(ctx$format, "json")) res else .render_release_pack_text(res)
}

prevcarga_verb_release_pack <- list(
  name    = "release-pack",
  usage   = paste("prevcarga release-pack --what=<w> --out=<dir> --published=<arq> [--release=<v>] [--bundles=<dir>]",
                  "[--latest]"),
  options = c(
    "--what=<w>              runtime, artifacts ou forecasts",
    "--published=<arq>       Lista de chaves ja publicadas, uma por linha (saida de scripts/tools/upload --list)",
    "--release=<v>           Versao do release (runtime); default = a dos bundles",
    "--bundles=<dir>         Diretorio com os bundles prevcarga-<v>-<plataforma> (runtime)",
    "--latest                Escreve tambem os latest.txt (unica chave mutavel); sem a flag nenhum e escrito"),
  scoped  = c("--what", "--published", "--release", "--bundles", "--latest"),   # so release-pack (cli.R .prevcarga_check_scoped)
  handler = function(ctx, args) cmd_release_pack(ctx, args),
  owner   = "M8",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "schema_long.R", "registry.R", "exports.R", "release_pack.R")
)
