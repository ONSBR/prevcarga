## banner.R -- banner ASCII da CLI (M1.4c; desenho = blueprint "Banner da CLI" 01M5G8B3PEF80G6DZT9G2E5KB3 da spec "Modular CLI").
## 15 linhas, <= 63 colunas: nome `prevcarga` (figlet standard), curva de carga diaria (48 colunas, uma por meia hora) + eixo
## 0h..24h, identidade e versao. Nome, curva e eixo sao guardados como TEXTO, copiados literalmente do blueprint (o blueprint
## permite "ou guardada como texto"; os 48 valores do dia real nao estao no repo). Nao le deck nem relogio.
## Cor so por ctx$color (flag --color > config.yaml), nunca por variavel de ambiente; aplicada por cima do texto puro.
## A linha de versao usa prevcarga_bundle_runtime() de runtime.R (carregado antes pelo shim); as partes fixas
## (.prevcarga_banner_name/_curve/_identity/_static) sao puras e nao dependem de outro modulo (instaladores e testes as usam).

## Nome em figlet standard, indentacao de 2.
.prevcarga_banner_name <- function() c(
  "   _ __   _ __   ___ __   __  ___   __ _  _ __   __ _   __ _",
  "  | '_ \\ | '__| / _ \\\\ \\ / / / __| / _` || '__| / _` | / _` |",
  "  | |_) || |   |  __/ \\ V / | (__ | (_| || |   | (_| || (_| |",
  "  | .__/ |_|    \\___|  \\_/   \\___| \\__,_||_|    \\__, | \\__,_|",
  "  |_|                                           |___/")

## Curva (4 linhas, niveis _ - ') + eixo (2 linhas). Coluna 3 = 00:00-00:30 .. coluna 50 = 23:30-24:00 (48 colunas);
## os marcadores do eixo ficam nas fronteiras: '|' em 0h (col 3), '+' em 6h/12h/18h (col 15/27/39), '|' em 24h (col 51).
.prevcarga_banner_curve <- function(axis = TRUE) {
  curve <- c(
    "                      ______-''''---_   _----_",
    "                  _--'               '-'      '-_",
    "  -_            _'                               '",
    "    '--_______--")
  if (!isTRUE(axis)) return(curve)
  c(curve,
    "  |-----------+-----------+-----------+-----------|",
    "  0h          6h          12h         18h       24h")
}

.prevcarga_banner_identity <- function() "  Previsao de carga do SIN - ONS"

## As 14 linhas fixas (tudo menos a linha de versao), na ordem do banner. E o bloco copiado nos instaladores.
.prevcarga_banner_static <- function()
  c(.prevcarga_banner_name(), "", .prevcarga_banner_curve(), "", .prevcarga_banner_identity())

## "prevcarga <versao> | R <x.y.z> | <plataforma> | runtime <8 hex>". Bundle instalado (RUNTIME.json): plataforma e runtime_id
## do bundle. Arvore de dev: plataforma do host e `runtime dev` (sem RUNTIME.json nao ha runtime_id; nao se inventa).
.prevcarga_banner_version_line <- function(ctx, bundle = prevcarga_bundle_runtime(ctx$root),
                                           r_version = as.character(getRversion()), host = .prevcarga_host_platform()) {
  plat <- if (!is.null(bundle) && length(bundle$platform) && nzchar(bundle$platform)) bundle$platform else host
  rid <- if (!is.null(bundle) && length(bundle$runtime_id)) substr(bundle$runtime_id, 1L, 8L) else "dev"
  sprintf("  prevcarga %s | R %s | %s | runtime %s", ctx$version, r_version, plat, rid)
}

prevcarga_use_color <- function(ctx, tty = isatty(stdout()), os = .Platform$OS.type) {
  switch(ctx$color,
    always = TRUE,
    never  = FALSE,
    auto   = isTRUE(tty) && !identical(os, "windows"),
    FALSE)
}

## stdout e terminal? Isolado para os testes in-process poderem simular o TTY.
.prevcarga_stdout_tty <- function() isTRUE(isatty(stdout()))

## verb: NULL/NA/"" = sem verbo; "--help"/"--version" = flags de topo. Nunca com --format json nem com stdout redirecionado.
prevcarga_banner_wanted <- function(ctx, verb = NULL, tty = .prevcarga_stdout_tty()) {
  if (identical(ctx$format, "json") || !isTRUE(tty)) return(FALSE)
  if (is.null(verb) || length(verb) == 0L || is.na(verb[[1L]]) || !nzchar(verb[[1L]])) return(TRUE)
  verb[[1L]] %in% c("--help", "--version", "doctor")
}

## Cor por cima do texto puro: nome azul (34), curva amarela (33), eixo/identidade/versao cinza (90); linhas vazias sem cor.
prevcarga_banner <- function(ctx, tty = isatty(stdout()), os = .Platform$OS.type, version_line = NULL) {
  if (is.null(version_line)) version_line <- .prevcarga_banner_version_line(ctx)
  txt <- c(.prevcarga_banner_static(), version_line)
  if (!prevcarga_use_color(ctx, tty = tty, os = os)) return(txt)
  n_name <- length(.prevcarga_banner_name()); n_curve <- length(.prevcarga_banner_curve(axis = FALSE))
  code <- c(rep("34", n_name), "", rep("33", n_curve), rep("90", length(txt) - n_name - 1L - n_curve))
  esc <- intToUtf8(27L)
  ifelse(nzchar(txt) & nzchar(code), paste0(esc, "[", code, "m", txt, esc, "[0m"), txt)
}
