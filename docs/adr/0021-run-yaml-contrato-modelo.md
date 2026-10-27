# ADR-0021 — Contrato de config por-modelo: `run.yaml` enxuto (sem orquestração), override por CLI

- **Status:** Aceito
- **Data:** 2026-10-11
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** changed-in-rebuild  <!-- refina a realização "job.json" do ADR-0009 -->
- **Relacionados:** [ADR-0009](0009-arquitetura-modular-modelo.md) (refina), [ADR-0011](0011-ciclo-artefato-registry.md) (campo `protocol`), [ADR-0013](0013-combina-convexa-metodo.md) (membros via EXOGENA), [ADR-0016](0016-orquestracao-io-nuvem.md) (orquestração externa), [ADR-0018](0018-runtime-r-only-cleanroom.md) (módulo=função, não subprocess)

## Contexto
O ADR-0009 pina que "toda variação operacional por-modelo (horizonte, janela, protocolo) é **dado**, no
`job.json`". Mas o `models/<m>/job.json` da referência é, na prática, um **manifesto de AWS Batch** com o
contrato de modelagem *baked-in*: ao lado de `horizon_days`/`history_days`/`protocol`/`seed`/`cut_invariant`
(modelagem genuína) ele carrega `resources:{vcpu,mem_gb}` (sizing de Batch), `r_deps` (pacotes p/ o
container/renv), e `entrypoint`/`config`/`train_args` (wiring de `subprocess`). Esses últimos são **orquestração
e empacotamento** — exatamente o que o ADR-0016 (orquestração externa, produto local-only) e o ADR-0018
(módulo = função R, não script invocado por subprocess) tiram do produto. (Gabriel, 2026-10-11: "isso tem cara
de orquestração na AWS que ficou perdida no meio do código".)

## Decisão
No clean-room, o contrato de config por-modelo vive em **`models/<m>/run.yaml`** (YAML, como o
`config/topology.yaml`; "run", não "job" — não é um job de Batch). Ele carrega **só o contrato de modelagem**:

```yaml
name: lgbm
horizon_days: 1                      # corte honesto (ADR-0006), lido por topology.R:model_spec
history_days: { train: 730, infer: 45 }
protocol: daily                      # ADR-0011: daily=origem-agnóstico · retrain_harness=origem-escopado
seed: 123
cut_invariant: false
params:                              # flags de modelagem (sobrescrevíveis), NÃO orquestração
  disable_loess: true               # ex. lgbm; dessem usa `exogena: MED`; rf usa `ntree: 100`
```

- **NÃO porta** `resources` (Batch → ADR-0016, orquestração externa), `r_deps` (bundle/renv → M8), nem
  `entrypoint`/`config`/`train_args`-como-wiring (ADR-0018: a invocação é uma função R, não um script).
- **Override por CLI:** os valores do `run.yaml` são **defaults**; uma flag de CLI os sobrescreve
  (`--horizon`, `--exogena`, …). O **valor efetivo** (não o do arquivo) é o que entra no corte honesto e no
  **run-manifest/`version` content-addressed** (ADR-0011) — a proveniência carimba o efetivo. É assim que os
  membros DESSEM nascem: o **mesmo** `run.yaml` com `--exogena {MED,MAX,MXMN}` (ADR-0013).
- **Identidade no `version_hash`:** o fingerprint content-addressed (ADR-0011) inclui o **`model_id`** (o
  `mdir`/`name` do `run.yaml`) **além** da `feature_config` efetiva (incl. `exogena`). Obrigatório porque os
  membros DESSEM abrangem **2 modelos × exogena**: `RGD_MED` (dessem-regdin) e `LIN_MED` (dessem-linear) têm
  `run.yaml` de contrato idêntico e mesma `exogena=MED` — só o `model_id` os distingue; sem ele, colidiriam.
- `topology.R:prevcarga_model_spec()` **lê** o `run.yaml` (fonte única por-modelo); `config/topology.yaml`
  **não** replica horizonte/janela (seria violar ADR-0009). Semeado no M1 (M1.2); honrado no M5 (training/CLI override).

## Consequências
Separa limpo **modelagem** (versionada no `run.yaml`, lida pela CLI) de **orquestração/empacotamento** (externa,
ADR-0016/M8). `model_spec`/`cmd_models` passam a ter fonte real no M1 sem puxar código de modelo. O layering
default→CLI-override exige que a proveniência (M1.3/M5) carimbe o **efetivo**. Refina a realização "job.json"
do ADR-0009 (mecanismo: YAML + override), sem mudar o princípio (variação operacional é dado, não código).

## Alternativas consideradas
Portar o `job.json` como-está (arrasta cruft de Batch pro produto local-only — contradiz ADR-0016); mover
horizonte/janela pro `config/topology.yaml` (viola ADR-0009, que pina no arquivo por-modelo); manter o nome
`job.json` (perpetua a confusão com o job de Batch). Rejeitadas.

## Evidência
- Referência: `models/lgbm/job.json` (horizon_days/protocol/seed + `resources:{vcpu,mem_gb}`/`r_deps`/
  `entrypoint`), `models/holt-winters/job.json` (`cut_invariant:true`), `models/dessem-linear/job.json`
  (`env:{EXOGENA:MED}` → membros via override). Consumo do horizonte: `jl_train_origin` em `jobs/run_job.sh`.
- Clean-room: `docs/_archive/roadmap/M1-FUNDACAO.md` M1.2 (semeia os 5 `run.yaml`; `model_spec` lê), `scripts/lib/topology.R:22`.
