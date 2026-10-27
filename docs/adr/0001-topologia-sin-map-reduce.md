# ADR-0001 — Topologia da árvore SIN e decomposição MAP(modelo×área×origem) / REDUCE(combina+reconcilia)

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0002](0002-grade-semihoraria-schema-long.md), [ADR-0013](0013-combina-convexa-metodo.md), [ADR-0015](0015-reconciliacao-bottomup-khat.md), [ADR-0016](0016-orquestracao-io-nuvem.md)

## Contexto
A previsão do SIN é hierárquica: 24 folhas (áreas/regiões) → 4 subsistemas + perdas de transmissão → SIN no topo, alimentada por 8 membros de modelo, cada um com horizonte/janela próprios. É a estrutura física do domínio e precede toda decisão de engenharia.

## Decisão
A topologia é a **fonte única**: a execução é um MAP embaraçosamente paralelo de células independentes e determinísticas (modelo × área × origem × modo), seguido de um REDUCE separado por origem que combina os membros e reconcilia na árvore. As duas fases têm runners distintos (`run_job` = 1 célula do MAP; `run_reduce` = junção de todos os membros×áreas). Regiões, subsistemas e mapeamento de perdas são declarados num só lugar.

## Consequências
Define a granularidade de paralelismo, retry e idempotência (uma célula falha isolada); habilita A/B e a disciplina aditiva (novo modelo = nova pasta). Mudar a chave do MAP ou a fronteira MAP/REDUCE muda idempotência e combina de uma vez. É a ponte entre domínio e execução que todo ADR de orquestração assume.

## Alternativas consideradas
Pipeline monolítico que combina enquanto mapeia (perde idempotência por-célula e paralelismo); reconciliação que prevê o SIN e desagrega top-down; topologia embutida no código em vez de fonte única.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- jobs/run_job.sh:1
- jobs/run_job.sh:250
- jobs/run_reduce.sh:3
- jobs/run_reduce.sh:104-148
- scripts/reconcilia_baseline.R:5-16
