## banner.R -- banner ASCII da CLI (M1.4c): nome + curva de carga diaria em 48 colunas + versao.
## Perfil fixo e decorativo (nao le deck nem relogio). Cor so por ctx$color (flag --color), nunca por env.

## 48 meias horas (col i = 00:00-00:30 .. 23:30-24:00), alturas 0..5.
.prevcarga_banner_profile <- c(
  1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L,
  2L, 2L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L,
  5L, 5L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
  5L, 5L, 5L, 5L, 4L, 4L, 3L, 3L, 2L, 2L, 1L, 1L)

.prevcarga_banner_curve <- function(profile = .prevcarga_banner_profile, rows = 6L) {
  stopifnot(length(profile) == 48L, all(profile >= 0L & profile < rows))
  vapply(rev(seq_len(rows)) - 1L, function(r) {
    ch <- ifelse(profile == r, "_", ifelse(profile > r, "|", " "))
    paste(ch, collapse = "")
  }, character(1))
}

prevcarga_use_color <- function(ctx, tty = isatty(stdout()), os = .Platform$OS.type) {
  switch(ctx$color,
    always = TRUE,
    never  = FALSE,
    auto   = isTRUE(tty) && !identical(os, "windows"),
    FALSE)
}

## verb: NULL/NA/"" = sem verbo; "--help"/"--version" = flags de topo.
prevcarga_banner_wanted <- function(ctx, verb = NULL) {
  if (identical(ctx$format, "json")) return(FALSE)
  if (is.null(verb) || length(verb) == 0L || is.na(verb[[1L]]) || !nzchar(verb[[1L]])) return(TRUE)
  verb[[1L]] %in% c("--help", "--version", "doctor")
}

prevcarga_banner <- function(ctx, tty = isatty(stdout()), os = .Platform$OS.type) {
  curve <- .prevcarga_banner_curve()
  if (prevcarga_use_color(ctx, tty = tty, os = os)) {
    esc <- intToUtf8(27L)
    curve <- paste0(esc, "[36m", curve, esc, "[0m")
  }
  c("prevcarga - previsao de carga (CLI modular)", curve, paste("versao", ctx$version))
}
