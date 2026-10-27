# ADR-0014 — Combina: VAZAMENTO de horizonte (treino de pesos por mês-de-origem SEM subtrair H) e solver FISTA sem paridade numérica — corrigir no rebuild

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** changed-in-rebuild  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0006](0006-backtest-honesto-corte-treino.md), [ADR-0013](0013-combina-convexa-metodo.md), [ADR-0015](0015-reconciliacao-bottomup-khat.md), [ADR-0018](0018-runtime-r-only-cleanroom.md)

## Contexto
Bug verificado em backtest: o corte de treino de pesos é `tr = mês-de-origem < m`, **sem recuo de H dias** — uma origem de d_plus=8 no mês m−1 tem target já dentro de m, treinando em rótulos que na operação real ainda não existiam. Infla a acurácia aparente da combina-A vs EQW/individuais. O gate G2 é FALSO-VERDE porque perturba por mês-de-ORIGEM (a mesma definição falha), não por target. Separadamente, o ótimo é definido pelo caminho FISTA (projeção de Duchi, 200 iters, warm-start), não por solução fechada.

## Decisão
O rebuild DEVE aplicar o contrato **origem − 1 − horizonte por membro** (o mesmo da reconcilia, `seq(lo, O−1)`, que faz CERTO) e **reescrever o G2** para perturbar por `TARGET_DATETIME`, não por origem. O solver muda conscientemente para QP em R (quadprog/osqp/CVXR): NÃO reproduz o ótimo FISTA bit-a-bit, logo o A/B troca o critério Δ=0 por **tolerância de WMAPE/pesos** ('combina sem paridade de solver').

## Consequências
Corrige o viés de acurácia da combina. O gate G2 atual não pode ser transportado como está (testaria a mesma falha). Entre Python-FISTA e QP-R a paridade cai para 'mesma função objetivo, ótimo numericamente próximo' — se o A/B exigir Δ=0 falhará espúrio. É a **exceção explícita** ao invariante Δ=0 ([ADR-0008](0008-honestidade-determinismo-falsificavel.md)), restrita à combina. (No PLANO, isto é o **M4**.)

## Alternativas consideradas
Manter origin-month (status quo com leak); corte origem−1−H por membro (contrato dos 3 cortes); portar FISTA linha-a-linha para forçar paridade (frágil); QP com solução fechada por face ativa.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/combina_baseline.py:271-272
- scripts/combina_baseline.py:190
- scripts/combina_baseline.py:389-398
- scripts/combina_baseline.py:216-236
- scripts/combina_baseline.py:385-388
- scripts/reconcilia_baseline.R:98-106

## Decisão (resolvida 2026-10-10, Gabriel)
Correção **só no greenfield**:
- O `combination.R` do clean-room adota o corte de treino de pesos **origem − 1 − horizonte por membro** (igual à reconcilia) e **reescreve o G2** para perturbar por **`target_datetime`** em horizonte cheio.
- O repositório de referência `/opt/ons-source/prevcargadessem3` fica **INTOCADO** — mantém o vazamento de propósito (é a baseline congelada). O vazamento é **documentado num doc separado**: [`docs/reference/VAZAMENTO-HORIZONTE-COMBINA.md`](../reference/VAZAMENTO-HORIZONTE-COMBINA.md).
- **Consequência direta no A/B:** como o velho é leaky e o novo é correto, o A/B da combina **nunca** é Δ=0 — é por **propriedade** (simplex + otimalidade do objetivo) + **qualidade** (WMAPE ≤ best-member/EQW no holdout). Solver = QP em R (sem paridade com o FISTA). Os números publicados **re-baselinam** (comunicar o shift ao DESSEM — ver `docs/_archive/roadmap/PLANO.md` M4/M7).
