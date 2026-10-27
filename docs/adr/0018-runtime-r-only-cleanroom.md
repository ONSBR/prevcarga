# ADR-0018 — Runtime R-only no clean-room: colapsar o runtime DUPLO R+Python em R-only (combina reescrita em R, CLI fachada R) e a grade cross-língua round_ts30/read_actual numa função R única

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** changed-in-rebuild  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0014](0014-combina-leak-horizonte-solver.md), [ADR-0016](0016-orquestracao-io-nuvem.md), [ADR-0017](0017-empacotamento-bundle-s3-entrega.md), [ADR-0013](0013-combina-convexa-metodo.md)

## Contexto
O código de referência embute DUAS runtimes — R (modelos/reconcilia/walk_forward) e Python (combina + casca CLI) — e distribui ambas no bundle. A combina-A (Python) e a reconcilia (R) precisam concordar bit-a-bit na grade (`round_ts30`) e na leitura da carga real, acoplamento mantido por convenção DUPLICADA em duas linguagens — uma classe inteira de bugs de paridade de grade.

## Decisão
O rebuild colapsa conscientemente em **R-only** (uma dep só): elimina o venv, reescreve a combina em R e muda a fachada da CLI para R, e colapsa `round_ts30`/`read_actual` numa única função R importável (fim da duplicação Python/R). Com isso, a escolha de veículo S3 ([ADR-0016](0016-orquestracao-io-nuvem.md)) e o empacotamento ([ADR-0017](0017-empacotamento-bundle-s3-entrega.md)) são reabertos (sem venv), e o leak corrigido em voo — que vive justamente na combina Python — passa a ter uma única implementação honesta.

## Consequências
Elimina a manutenção de duas deps e a classe de bugs de paridade de grade cross-língua. Força a reescrita do solver convexo em R (sem paridade numérica com FISTA — ver [ADR-0014](0014-combina-leak-horizonte-solver.md)). Simplifica o bundle (só rlib). É o ADR central do clean-room: habilita 'módulo = função R importável' end-to-end e o A/B como gate contra a referência.

## Alternativas consideradas
Manter duas deps (status quo da referência); Python-only; manter duas implementações de `round_ts30` sincronizadas por teste de paridade.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- ci/build_marmita.sh:68-73
- scripts/prevcarga:1
- scripts/prevcarga:42
- jobs/run_reduce.sh:188-206
- scripts/combina_baseline.py:48-65
- scripts/reconcilia_baseline.R:62
