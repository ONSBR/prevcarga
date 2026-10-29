## cli_fixture_verbs.R -- handlers dos verbos-fixture do M1.V (NAO e codigo de produto).
## Copiado para <copia>/scripts/lib/ por pc_install_copy(); cada verbo e registrado por UM arquivo
## <copia>/scripts/lib/cli/fixture-<x>.R que a descoberta real (prevcarga_verbs_discover) encontra sozinha.
## Os ids vem do catalogo carregado (primeiro id da classe, por ordem radix): nada e redigitado aqui.

.fx_catalog_id <- function(cls) {
  cat <- prevcarga_error_catalog()
  cat$id[cat$class == cls][[1L]]
}

## fixture-probe: sonda de determinismo (hora da epoca, ordenacao de strings mistas, BLAS fixo).
cmd_fixture_probe <- function(ctx, args) {
  writeLines(paste(lubridate::hour(.POSIXct(0))), con = stdout())
  writeLines(paste(sort(c("b", "A", "a", "B", "_1")), collapse = " "), con = stdout())
  writeLines(paste(format(crossprod(matrix(1:9 / 7, 3)), digits = 15), collapse = " "), con = stdout())
  NULL
}

## fixture-<classe> (1-15): aborta com um id do catalogo da classe; "generic" faz stop() NAO classificado.
.fx_class_handler <- function(cls) {
  force(cls)
  function(ctx, args) {
    if (identical(cls, "generic")) stop("falha nao classificada do fixture")
    prevcarga_abort(.fx_catalog_id(cls), where = paste0("fixture-", cls))
  }
}
for (.fx_cls in setdiff(names(prevcarga_exit_codes), "ok"))
  assign(paste0("cmd_fixture_", .fx_cls), .fx_class_handler(.fx_cls))
rm(.fx_cls)

## fixture-multi: coletor com 6 conjuntos de classes; --model=<n> em ordem normal, --model=<n>r invertida.
## Vencedores esperados (precedencia ADR-0040): 7, 8, 2, 3, 5, 9.
.fx_multi_sets <- list(
  `1` = c("runtime", "config", "contract", "completude", "topology", "window", "generic"),
  `2` = c("config", "contract", "completude", "topology", "window", "integrity"),
  `3` = c("contract", "completude", "topology", "window", "bundle"),
  `4` = c("completude", "topology", "window", "integrity"),
  `5` = c("window", "bundle", "integrity"),
  `6` = c("integrity", "network", "coherence", "honesty"))

cmd_fixture_multi <- function(ctx, args) {
  spec <- args[["--model"]]
  inverted <- grepl("r$", spec)
  cls <- .fx_multi_sets[[sub("r$", "", spec)]]
  if (inverted) cls <- rev(cls)
  col <- prevcarga_collect_errors()
  for (k in cls) col$add(.fx_catalog_id(k), where = paste0("fixture-multi:", k))
  col$finish()
}

## fixture-boom: erro NAO classificado dentro de uma chamada aninhada (traceback so com --debug).
.fx_inner_boom <- function() stop("boom do fixture")
cmd_fixture_boom <- function(ctx, args) .fx_inner_boom()

## fixture-accent: escreve bytes > 127 no stdout (CONTROL do gate de bytes: DEVE reprovar).
cmd_fixture_accent <- function(ctx, args) {
  writeLines(enc2utf8("saída com acentuação"), con = stdout(), useBytes = TRUE)
  NULL
}

## Setter de TZ que nao fixa nada (devolve o fuso sujo observado). A copia o injeta anexando uma linha a
## <copia>/scripts/lib/runtime.R (ver pc_install_copy(broken_tz = TRUE)): a guarda roda ANTES dos modulos.
runtime_setter_tz_broken <- function() .prevcarga_tz_observed()
