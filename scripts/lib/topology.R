## topology.R — fonte única da topologia (M1)
##
## Responsabilidade: ser o ÚNICO lugar que conhece a árvore do SIN e o conjunto de membros/modelos.
## Subsume o quádruplo-hardcode da referência (SUBSYS/LEAVES na CLI Python, MEMBERS no
## combina_baseline.py, árvore SIN no reconcilia_baseline.R, horizonte no job.json).
##
## Expõe (contrato alvo):
##   - folhas (24 áreas-carga) e a árvore folha → região → subsistema (+ perdas) → SIN
##   - membros (8: RF/LGBM/HW + 5 variantes dessem), identidade = (mdir, EXOGENA)
##   - por modelo: horizon_days, history_days{train,infer} — LIDOS de models/<m>/job.json (não replicados)
##
## Gate falsificável (M1): grep prova que nenhum comando re-hardcoda SUBSYS/LEAVES/MEMBERS/árvore.

prevcarga_topology <- function() {
  stop("topology.R: prevcarga_topology() não implementado — TODO M1")
}

prevcarga_members <- function() {
  stop("topology.R: prevcarga_members() não implementado — TODO M1")
}

prevcarga_model_spec <- function(model) {
  # lê horizon_days/history_days/protocol do models/<model>/run.yaml (contrato enxuto, ADR-0021;
  # NÃO do job.json da referência, que mistura orquestração AWS). Default sobrescrevível por flag de CLI.
  stop("topology.R: prevcarga_model_spec() não implementado — TODO M1")
}
