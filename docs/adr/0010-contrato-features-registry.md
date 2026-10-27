# ADR-0010 — Verificação de contrato de features em WIRE-TIME (fail-loud) — e os DOIS paradigmas de ModelFamilyRegistry coexistentes

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0009](0009-arquitetura-modular-modelo.md), [ADR-0011](0011-ciclo-artefato-registry.md), [ADR-0013](0013-combina-convexa-metodo.md)

## Contexto
A tese 'um ModelFamilyRegistry reusado entre modelos' tem na verdade DUAS encarnações incompatíveis, e a reconstrução precisa escolher qual semântica de verificação de contrato é canônica antes de reintroduzir a divergência.

## Decisão
**Invariante a preservar:** o contrato `requires`/`provides` é verificado em WIRE-TIME (no `register`), fail-loud com a lista completa de gaps nos 4 saltos, mais um guard de colunas em runtime (cinto-e-suspensório) — erro de pipeline surge no registro, não no meio de um treino de horas.

**PENDENTE:** RF/LGBM usam um registry orientado a classes R6 que VERIFICA o contrato no `register` (LGBM sourceia e reusa a classe do RF); HW/dessem usam OUTRO registry keyado por `source_path`/`entry_fn` cujo `verify_contract` é 'lightweight' e NÃO checa `requires`/`provides`.

## Consequências
Escolher sem ADR reintroduz a divergência. Unificar no forte pode quebrar HW/dessem ao expor gaps; unificar no fraco dissolve a garantia de composição segura. O wire-time check é o que torna 'módulo = função R' seguro de compor.

## Alternativas consideradas
Registry único com verificação uniforme (todos declaram `requires`/`provides`); dois tiers explícitos (contract-verified vs vendored-lazy) com fronteira documentada; só check de runtime (erro tardio); schema declarado em arquivo JSON em vez de probe-instances.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- models/random-forest/R/registry/registry.R:330
- models/random-forest/R/registry/registry.R:413-443
- models/random-forest/R/registry/registry.R:543-565
- models/lgbm/R/registry/plugin_registry.R:112-128
- models/dessem-linear/R/registry/plugin_registry.R:120-149
- models/random-forest/R/orchestrator/orchestrator.R:154-170

## Decisão (resolvida 2026-10-10, Gabriel) — DOIS tiers documentados; Tier B VAZIO (DESSEM é nosso → Tier A)
Não se unifica num registry único — define-se uma fronteira **explícita e documentada** em `registry.R` —, mas
na prática **todo modelo atual é contract-verified**:
- **Tier A — contract-verified (R6):** **RF, LGBM, HW e DESSEM** declaram `requires`/`provides` e são
  verificados no `register` (wire-time, fail-loud com a lista completa de gaps). HW e DESSEM migram do registry
  fraco para o forte — aceitamos que isso possa expor gaps reais hoje mascarados (é o objetivo, não efeito
  colateral). **Correção:** o DESSEM NÃO é vendor de terceiro — é código nosso (pacote `prevcargadessem`,
  ver correção de premissa no [ADR-0012](0012-dessem-vendor-wrap-nao-edita.md)) — então não há razão pra
  deixá-lo lazy; ele declara contrato como todos.
- **Tier B — vendored-lazy:** **VAZIO / reservado.** Só habitaria código genuinamente externo de terceiro
  (que não poderíamos editar pra declarar contrato) — não temos nenhum hoje. A categoria existe como válvula
  de escape documentada, não como abrigo de modelo nosso.

Resultado: o registry é **uniformemente contract-verified**. Nenhum modelo nasce lazy — ou declara contrato
(Tier A), ou seria um externo de terceiro justificado por ADR (Tier B, hoje vazio). A fronteira é um ponto
único no `registry.R`. Implementado no M1 (fundação do registry) / honrado no M3 (training) e M5.
