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
