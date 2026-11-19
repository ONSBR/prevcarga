## Ilhas de feriado (E3-T3): a serie do motor pode trazer a coluna `Bloco` (0L = janela comum + horizonte,
## k >= 1L = k-esima corrida contigua de dias do anexo de feriados). Os lags de feature nunca atravessam
## bloco: cada bloco e uma grade horaria contigua propria. So definicoes; sem estado.

#' Aplica `fn` a cada bloco contiguo de `seriehist` (ordem crescente de `Bloco`) e junta os resultados.
#' Sem coluna `Bloco` ou com um bloco so e a identidade: `fn(seriehist, ...)` uma unica vez.
.engine_by_block <- function(seriehist, fn, ...){
   if(!"Bloco" %in% names(seriehist)) return(fn(seriehist, ...))
   blocos <- sort(unique(seriehist$Bloco))
   if(length(blocos) <= 1L) return(fn(seriehist, ...))
   partes <- lapply(blocos, function(b) fn(seriehist[Bloco == b], ...))
   data.table::rbindlist(partes, use.names = TRUE)
}

#' Remove a coluna `Bloco` por referencia, se existir
.engine_drop_block <- function(dt){
   if("Bloco" %in% names(dt)) dt[, Bloco := NULL]
   invisible(dt)
}
