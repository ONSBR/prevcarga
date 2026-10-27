## schema_long.R -- constante do painel LONG (ADR-0002) e conferencia de nome+ordem (M1.2b).
## Fonte unica de nome + ordem + tipo logico para a combina (M4), Reconcile, build_long_output (M6)
## e prevcarga_assert_long/read_long (M3). Requer errors.R carregado (prevcarga_abort).
##
## - forecast_origin e TEXTO `yyyy-MM-dd` (tipo 'character') em todos os writers e leitores
##   (ADR-0032); gravar Date e FORBIDDEN.
## - target_datetime e rotulado por interval_end.
## - A conferencia aqui e so de nomes e ORDEM; tipos finos, tz e d_plus sao do leitor (M3).

prevcarga_schema_long <- c(
  area = "character", forecast_origin = "character", # yyyy-MM-dd (ADR-0032)
  target_datetime = "POSIXct", lead = "integer", y_pred = "double",
  y_true = "double", d_plus = "integer", granularity_min = "integer")

## Confere nomes e ordem de `dt` contra prevcarga_schema_long; nunca reordena nem modifica `dt`.
prevcarga_schema_long_check <- function(dt) {
  want <- names(prevcarga_schema_long)
  have <- names(dt)
  for (i in seq_along(want)) {
    if (i > length(have) || !identical(have[[i]], want[[i]]))
      prevcarga_abort("contract.long_schema_constant", where = want[[i]],
                      details = list(expected = want, found = have))
  }
  if (length(have) != length(want))
    prevcarga_abort("contract.long_schema_constant", where = have[[length(want) + 1L]])
  invisible(TRUE)
}
