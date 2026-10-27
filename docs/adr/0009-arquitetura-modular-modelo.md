# ADR-0009 — Arquitetura modular de modelo: módulo = função R importável; unidade atômica = (área, origem, modo); FE por-plugin; manifesto job.json declarativo

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0001](0001-topologia-sin-map-reduce.md), [ADR-0010](0010-contrato-features-registry.md), [ADR-0011](0011-ciclo-artefato-registry.md), [ADR-0017](0017-empacotamento-bundle-s3-entrega.md)

## Contexto
Para A/B, composabilidade e disciplina aditiva (novo modelo = nova pasta, zero edição no harness), a fronteira entre harness/jobs e modelo tem de ser estável e a cadência/paralelismo não pode acoplar-se ao modelo.

## Decisão
`run_pipeline()` executa exatamente UMA tupla `(model_family, área, forecast_origin, horizon, mode)` e retorna artefato (train) ou predições de uma origem (infer) — o laço walk-forward NÃO vive no modelo, é harness externo. Feature engineering vive **POR-plugin** dentro de cada modelo (ondaletas/LOESS no LGBM, lags/dummies no dessem); não há camada de FE compartilhada — a única coisa comum é o contrato `requires`/`provides`. Toda variação operacional (horizonte, janela, protocolo, deps, env, recursos, `cut_invariant`) é dado no `job.json`; `run_job` resolve via `jq` e NUNCA edita um `.R`.

## Consequências
Permite MAP trivial (célula independente e determinística) e o split 'treina 1× / infere N×'. Impede acoplamento acidental entre modelos via lib de FE compartilhada. Vazar lógica de modelo pro shell (ou o laço de origens pro modelo) quebra A/B, determinismo por-célula e portabilidade do bundle.

## Alternativas consideradas
Orquestrador que recebe janela [from,to] e itera internamente; camada de FE canônica compartilhada (DRY mas acopla); config embutida no `run_pipeline` de cada modelo; orquestrador que importa modelos como funções em vez de invocar CLIs.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- models/random-forest/R/orchestrator/orchestrator.R:14-18
- models/random-forest/R/orchestrator/orchestrator.R:258
- models/lgbm/R/feature_engineering/feature_engineering.R:1
- models/random-forest/job.json:7
- jobs/run_job.sh:20-21
- jobs/run_job.sh:88-110
