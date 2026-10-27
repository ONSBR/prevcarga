# ADR-0006 — Backtest honesto: o harness só trunca o tempo (código de backtest = produção) + corte de treino origem−1−horizonte

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0005](0005-contrato-entrada-fail-por-origem.md), [ADR-0007](0007-contrato-corte-intradiario.md), [ADR-0008](0008-honestidade-determinismo-falsificavel.md), [ADR-0016](0016-orquestracao-io-nuvem.md)

## Contexto
A classe de bug mais perigosa é 'o backtest usa um código e a produção outro'. E um erro de ±1 dia no corte de treino infla silenciosamente a acurácia e passa despercebido em todas as métricas agregadas.

## Decisão
`walk_forward.R` é um loop puro de avaliação: para cada origem monta `available_until` e invoca `run_pipeline` como SUBPROCESSO — nenhuma conta de previsão vive no harness, ele só controla o relógio. O corte de treino honesto é **origem − 1 − horizonte** (`jl_train_origin` devolve `first_origin − (horizon+1)`): o −1 extra fecha o off-by-one em que o rótulo do primeiro dia de previsão ainda vazaria para o fit. Idêntico em todos os protocolos e espelhado no `honesty_gate`.

## Consequências
Perturbar o DADO e re-chamar o MESMO binário é o teste; se o harness reimplementasse inferência, um Δ=0 não provaria nada sobre produção. O corte origem−1−H é a definição operacional de 'backtest honesto' do projeto. **CRÍTICO:** a combina VIOLA este corte (ver [ADR-0014](0014-combina-leak-horizonte-solver.md)); a reconcilia o faz CERTO (ver [ADR-0015](0015-reconciliacao-bottomup-khat.md)).

## Alternativas consideradas
Harness com motor de inferência embutido/vetorizado (mais rápido, desacopla backtest de produção); corte em origem−horizonte (sem o −1) ou D-1 simples ignorando o horizonte do modelo.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/walk_forward.R:18-20
- scripts/walk_forward.R:375-421
- jobs/job_lib.sh:197-231
- jobs/run_job.sh:350
- scripts/tools/honesty_gate.R:391-398
