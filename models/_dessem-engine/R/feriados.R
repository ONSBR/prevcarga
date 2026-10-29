## feriados.R -- porta de scripts/lib/normalize_holiday_codes.R do oraculo (ex-flag-12).
## Codigo de dia especial fora de `modeled` (0..11, os que tem janela em janelaDadosFer) vira 0 (dia normal)
## em todas as datas. Sempre ligado: o kill-switch DESSEM_NORMALIZE_CODES nao e portado (nada le ENV).
## NO-OP (devolve a propria entrada) quando todos os codigos sao modelados.
normalize_special_day_codes <- function(feriados, tipo_col = "Tipo", modeled = 0:11, area = NA_character_) {
  if (is.null(feriados) || nrow(feriados) == 0L) return(feriados)
  stopifnot(tipo_col %in% names(feriados))
  ft <- data.table::as.data.table(feriados)
  idx <- which(!(ft[[tipo_col]] %in% modeled))
  if (length(idx) == 0L) return(feriados)
  bad <- sort(unique(ft[[tipo_col]][idx]))
  ft <- data.table::copy(ft)                                # nunca muta a entrada por referencia
  data.table::set(ft, i = idx, j = tipo_col, value = 0L)
  message(sprintf(paste0("[engine.dessem] area=%s: %d linha(s) com codigo nao-modelado (%s) ",
                         "normalizada(s) -> 0 (dia normal)."),
                  area, length(idx), paste(bad, collapse = ",")))
  ft[]
}
