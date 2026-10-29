## schema_long.R -- constante do painel LONG (ADR-0002) e conferencia de nome+ordem (M1.2b).
## Fonte unica de nome + ordem + tipo logico para a combina (M4), Reconcile, build_long_output (M6)
## e prevcarga_assert_long/read_long (M3). Requer errors.R carregado (prevcarga_abort).
##
## - forecast_origin e TEXTO `yyyy-MM-dd` (tipo 'character') em todos os writers e leitores
##   (ADR-0032); gravar Date e FORBIDDEN.
## - target_datetime e rotulado por interval_end.
## - A conferencia aqui e so de nomes e ORDEM; tipos finos, tz e d_plus sao do leitor (M3).
## - Borda do rotulo: o UNICO par interval_end <-> interval_start do LONG mora aqui (leitor e writer M6.1);
##   nenhum consumidor faz +-30 min sobre o LONG (gate `minutes30` em test-static-greps.R).

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

## --- Borda do rotulo do LONG (ADR-0002/0035) ------------------------------------------------------------
## Granularidade so 30 min; `tz` (fuso da carga) e conferido quando dado. Requer lubridate.
.prevcarga_long_edge_check <- function(x, granularity_min, tz, col) {
  if (any(granularity_min != 30L)) prevcarga_abort("contract.long_schema", where = "granularity_min")
  if (!is.null(tz) && !identical(lubridate::tz(x), tz)) prevcarga_abort("contract.long_tz", where = col)
}

## leitura: target_datetime (interval_end) -> interval_start
prevcarga_long_interval_start <- function(target_datetime, granularity_min, tz = NULL) {
  .prevcarga_long_edge_check(target_datetime, granularity_min, tz, "target_datetime")
  target_datetime - lubridate::minutes(30L)
}

## escrita (M6.1): interval_start -> target_datetime (interval_end)
prevcarga_long_target_datetime <- function(interval_start, granularity_min, tz = NULL) {
  .prevcarga_long_edge_check(interval_start, granularity_min, tz, "interval_start")
  interval_start + lubridate::minutes(30L)
}

## Anexa `interval_start` ao LONG em memoria UMA vez (ja presente => intocado); consumidores usam a coluna.
prevcarga_long_attach_interval_start <- function(long, tz = NULL) {
  if ("interval_start" %in% names(long)) return(long)
  long$interval_start <- prevcarga_long_interval_start(long$target_datetime, long$granularity_min, tz)
  long
}
