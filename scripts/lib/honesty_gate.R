## honesty_gate.R — gate de honestidade falsificável (M3, reusável)
##
## Responsabilidade: substituir a garantia de honestidade que o ISOLAMENTO DE PROCESSO dava na
## referência (regra G5: o harness só truncava tempo via --available-until, nunca inferia). Ao colapsar
## em R in-process isso some → honestidade vira invariante de MÓDULO, provado por um gate falsificável.
##
## Mecânica (dois braços, ambos obrigatórios):
##   - FORBIDDEN: perturbar y_true em target_datetime ≥ origem (futuro) ⇒ y_pred deve dar Δ=0
##     (o módulo NÃO pode ter visto o futuro).
##   - CONTROL:   perturbar y_true legal em O−1 (passado) ⇒ y_pred deve dar Δ≠0
##     (prova que o Δ=0 do FORBIDDEN não é vazio — o módulo REALMENTE usa o passado).
## Usar horizonte CHEIO (~9), não max_dplus=0 (o furo do G2 da referência era cego ao spill de horizonte).
##
## Reusado por: combination (M4, 1º a passar), training/inference (M5), cache (M7).

prevcarga_honesty_gate <- function(run_fn, panel, origin, horizon) {
  stop("honesty_gate.R: prevcarga_honesty_gate() não implementado — TODO M3")
}
