# ADR-0011 — Ciclo de artefato: artefato auto-descritivo congelado (saveRDS) + registry 'latest'/v_* no filesystem + split train/infer por CLASSE de modelo

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0007](0007-contrato-corte-intradiario.md), [ADR-0009](0009-arquitetura-modular-modelo.md), [ADR-0016](0016-orquestracao-io-nuvem.md), [ADR-0017](0017-empacotamento-bundle-s3-entrega.md)

## Contexto
A inferência nunca pode divergir do treino, e 'qual é o artefato corrente' precisa de versionamento barato, inspecionável e S3-syncável — sem metastore. Nem todo modelo tem artefato reutilizável.

## Decisão
Reprodutibilidade vem de serializar um objeto fechado e auto-descritivo (schema de features ordenado, min/max, sazonalidade congelada, flags, seed): a inferência HERDA os flags do artefato, sobrescrevendo a sessão. O 'latest' é um arquivo-texto de 1 linha (`v_YYYYMMDD_HHMMSS`) + diretórios versionados imutáveis (sem `--delete` no sync), mesmo padrão em RF/HW/dessem/combina/reconcilia. O protocolo é propriedade da CLASSE do modelo: **origem-agnósticos** (RF/LGBM) reusam o artefato em qualquer origem com shift honesto (corte inference-only); **origem-escopados** (dessem-*/HW) fitam às datas ISO da própria origem e EXIGEM FROM==TO (a lista `.refit_per_origin` no harness aborta se `--retrain` não for passado).

## Consequências
O layout `<root>/<área>/{latest,v_*}` sustenta a sincronização S3 e o split train/infer. Mexer no schema do artefato sem versionar a tag (ex. `dessem_linear_split_v1`) quebra inferências antigas em silêncio. **OPEN:** a lista hardcoded `.refit_per_origin` duplica o manifest (fonte-dupla); e manter duas classes vs forçar um molde único é decisão pendente do rebuild.

## Alternativas consideradas
Reconstruir o estado na inferência a partir de config+dados; metastore/DB (MLflow); content-addressed puro sem latest mutável; symlink em vez de ponteiro-texto (quebra em S3); deduzir o protocolo do manifest em runtime (fonte única); forçar todos a origem-agnóstico ou todos a origem-escopado.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- models/lgbm/R/utils.R:27-47
- models/lgbm/R/utils.R:181-296
- models/holt-winters/R/helpers/artifact_registry.R:79-181
- jobs/run_job.sh:131-153
- jobs/run_job.sh:405-448
- scripts/walk_forward.R:579-589

## Decisão (resolvida 2026-10-10, Gabriel) — duas classes, dirigidas pelo manifest
Mantêm-se as **duas classes** de ciclo de artefato — a distinção é real e load-bearing (forçar molde único
ou desperdiça re-fit em RF/LGBM, ou faz DESSEM/HW fingirem um reuso que não têm):
- **origem-agnósticos** (RF/LGBM): reusam o artefato em qualquer origem com shift honesto (corte inference-only).
- **origem-escopados** (DESSEM/HW): re-fitam às datas ISO da própria origem, exigem `FROM==TO`.

**A classe vira um campo `protocol` DECLARADO no manifest** — fonte única. Elimina-se a lista hardcoded
`.refit_per_origin` no harness (era fonte-dupla, podia divergir do manifest): o harness lê `protocol` do
manifest em runtime e decide refit/reuso a partir dele. Casa com os contratos declarados do
[ADR-0010](0010-contrato-features-registry.md). Nenhum modelo novo escolhe classe no código — declara no
manifest. Implementado no **M3** (training) / honrado no **M5**.

> Nota do rebuild: o versionamento `v_<timestamp>` será substituído por **content-addressed** (ver `scripts/lib/provenance.R`, M1 do PLANO) — `trained_at` vira metadado lateral. Esta decisão pendente é sobre o número de CLASSES de protocolo, não sobre o esquema de versão.
