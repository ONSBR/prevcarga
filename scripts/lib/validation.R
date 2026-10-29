## validation.R — validação model-agnóstica do deck (M2)
##
## Responsabilidade: validar o deck de entrada STANDALONE, antes de gastar compute. Unifica as peças
## dispersas da referência (validate_input.R asserts + scan_completude_24slots.R + port R de
## analyze_data_quality.py) atrás de UM módulo com veredito estruturado.
##
## Checa:
##   - FORMATO/SCHEMA: colunas/tipos/unidades (timestamp, load_mwh numérico, area_code, temperature,
##     domínio do código de feriado 0..12).
##   - COMPLETUDE **DST-aware**: ≥24 slots/dia (:00 e :30), continuidade de dias; dia de DST tem 23h/25h
##     (não dar falso-positivo). Pré-shift +1h na carga.
##   - SANIDADE FÍSICA: carga>0, faixa por subsistema, outlier/spike/flatline, dedup, monotonicidade.
##   - HONESTIDADE DO CLIMA: weather/forecast issue_time ≤ O.
##   - JANELA MÍNIMA por membro (45d/730d/1825d) — reportar "faltam N dias p/ o modelo X".
##
## Saída: veredito estruturado + --format json + exit-codes por classe (2 formato / 3 completude /
## 4 escopo-área / 5 déficit-de-janela). Encadeável como gate-0 (--skip-validate).
##
## Gate falsificável (M2): reprova deck que RF/LGBM comeriam calado com fill-0; sem falso-positivo em DST.

prevcarga_validate_deck <- function(data_dir, as_of, scope = NULL, format = "text") {
  stop("validation.R: prevcarga_validate_deck() não implementado — TODO M2")
}

## --- Predicado unico de completude do dia (M2.3; o M2.4 estende este arquivo) --------------------------
## O store e flat-48: um dia tem 48 intervalos de 30 min, sem excecao de DST (dia de 46 e buraco).
prevcarga_slots_per_day <- 48L

## `load`: carga do leitor (area_code, interval_start, load_mwh; attr "tz"). Devolve (area_code, date, n_slots,
## complete). Intervalo valido = inicio em :00/:30 com load_mwh nao NA; duplicata conta 1; data pelo
## interval_start no fuso da carga (nunca por interval_end).
prevcarga_day_complete <- function(load) {
  if (!all(c("area_code", "interval_start", "load_mwh") %in% names(load)))
    prevcarga_abort("contract.schema_missing", where = "validation.R")
  tz <- attr(load, "tz")
  if (is.null(tz)) tz <- lubridate::tz(load$interval_start)
  v <- load[lubridate::minute(interval_start) %in% c(0L, 30L) & lubridate::second(interval_start) == 0 &
              !is.na(load_mwh)]
  out <- v[, list(n_slots = data.table::uniqueN(interval_start)),
           by = list(area_code, date = lubridate::as_date(interval_start, tz = tz))]
  out[, complete := n_slots == prevcarga_slots_per_day]
  out[]
}
