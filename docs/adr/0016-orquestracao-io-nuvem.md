# ADR-0016 — Orquestração e IO de nuvem: modo inferido do prefixo s3:// (sem flag), aws CLI único + retry, idempotência por chave content-addressed, fan-out AWS Batch array + barreira map→reduce via dependsOn

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** changed-in-rebuild  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0001](0001-topologia-sin-map-reduce.md), [ADR-0007](0007-contrato-corte-intradiario.md), [ADR-0017](0017-empacotamento-bundle-s3-entrega.md), [ADR-0018](0018-runtime-r-only-cleanroom.md)

## Contexto
Um único runner tem de servir dev/gate (local) e prod (S3) com Δ=0 entre eles; o fan-out de milhares de shards precisa re-rodar sem duplicar e sobreviver a falhas transitórias de infra, sem orquestrador externo.

## Decisão
O transporte (local vs S3) é inferido DO DADO (`_is_s3` testa `startswith('s3://')`), nunca flag de modo. Toda IO de nuvem passa por UM binário (aws CLI, sem SDK), com wrapper `jl_aws` de retry 3×/backoff (o binário às vezes dá SIGSEGV e o Batch não expõe `retryStrategy` por shard). 'Já feito' = existência do `long.parquet` na chave determinística que codifica todos os eixos (`FORCE=1` recomputa). Fan-out = 1 array job por (MODEL,MONTH,PROTOCOL), index→AREA, recursos do manifesto, imagem/jobdef genérica (`containerOverrides`). map→reduce encadeados por `--depends-on` nativo do Batch (SUCCEEDED; FAILED cancela). Falha parcial sobe as origens boas e propaga exit≠0 (re-submeter com `FORCE=1`).

## Consequências
Toda a dualidade local↔nuvem e o gate 'local==S3 Δ=0' dependem da inferência por prefixo. Chave incompleta = sobrescrita/pulo silencioso (foi o bug que forçou o corte na chave). **OPEN:** o contrato de fan-out por shard está em PROBE (índice vs item resolvido) — fixar após o canário remove o ramo morto. E o veículo S3 reabre no R-only (aws CLI vs paws vs arrow-S3).

## Alternativas consideradas
Flag `--mode` explícito; VFS/fsspec/arrow-fs; SDK boto3/paws; `retryStrategy` do Batch por shard; ledger/DB de estado; jobdef por modelo; Step Functions/Airflow; evento S3→Lambda; fixar no `AWS_BATCH_JOB_ARRAY_INDEX` ou no item do Axon.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/prevcarga:56-57
- scripts/prevcarga:181-197
- jobs/job_lib.sh:263-286
- jobs/run_job.sh:248-255
- jobs/aws/submit.sh:164-182
- jobs/entrypoint.sh:42-76
- jobs/aws/submit_reduce.sh:40-51

## Decisão (resolvida 2026-10-10, Gabriel) — este ADR vira REFERÊNCIA HISTÓRICA (não-alvo do rebuild)
O produto do clean-room é um **pacote R local-only + casca fina de CLI** que o chama; **o miolo (CLI +
pacote) nunca fala S3** — lê e escreve **só filesystem local**, determinístico por construção. O S3 vive
exclusivamente nas **pontas**, fora do pacote:

```
orquestração (Docker, EXTERNA, opcional) ── interfaceia S3 (sync in/out)
  └─ chama → casca CLI (prevcarga ...) ──── fachada fina de args
       └─ chama → pacote R (lib/*.R) ─────── lógica, 100% local
            └─ roda sobre → bundle (R+renv) instalado pelo installer (S3 só aqui, no bootstrap)
```

Consequências para este ADR:
- **Nada de veículo de S3 dentro do produto.** `paws`/`aws` CLI **não** entram como dep do pacote (reverte a
  cogitação de paws). O produto não conhece S3.
- O padrão da referência **"`_is_s3` / modo inferido do prefixo `s3://` dentro da CLI"** (`scripts/prevcarga:56-57`)
  **NÃO é portado** — era S3 vazando pro modelo; no clean-room o modelo é S3-agnóstico.
- Quando houver orquestração de nuvem, ela é um **script separado que roda em Docker**, chama a CLI/pacote
  e **interfaceia o S3** (sync `s3→local`, roda, sync `local→s3`). Esse script é a única coisa que toca aws
  CLI/S3 — e é externo ao pacote, território de empacotamento/deploy ([ADR-0017](0017-empacotamento-bundle-s3-entrega.md)).
- Portanto **todo este ADR** (modo por prefixo, `jl_aws`, fan-out Batch array, idempotência por chave,
  `--depends-on`) passa a ser **descrição do sistema de referência `/opt/ons-source/prevcargadessem3`**, não
  requisito do rebuild. O fan-out Batch/map-reduce sai de escopo do produto. A decisão de fan-out por shard
  (antes "pendente") fica **moot** aqui — se ressurgir, será no script de orquestração externo, em seu próprio ADR.
