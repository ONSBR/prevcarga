# ADR-0007 — Contrato de corte intradiário: vem do dado (manifest.json), é um INSTANTE semi-horário, proíbe por issue_time (não valid_time), carimba a chave (cut_invariant difunde)

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0006](0006-backtest-honesto-corte-treino.md), [ADR-0008](0008-honestidade-determinismo-falsificavel.md), [ADR-0012](0012-dessem-vendor-wrap-nao-edita.md), [ADR-0017](0017-empacotamento-bundle-s3-entrega.md)

## Contexto
Uma rodada real ≠ 08:00 cairia num default silencioso que vazaria o dia inteiro. Além disso, proibir o `valid_time` futuro proibiria usar qualquer previsão meteorológica; e dois cortes podem colidir na mesma chave de resultado.

## Decisão
O instante de corte é FATO do dado (proveniência): `_cut_from_data` lê `<dados>/manifest.json` chave `corte` e a CLI RECUSA rodar sem corte — não assume 08:00 (o piso 08:00 só vale nos caminhos internos Axon/backtest). O corte é um **INSTANTE** naive-BRT (hora h só é completa se h:00 E h:30 ≤ `available_until`), não uma data. A região proibida é por TIMESTAMP para load/observed mas por **ISSUE_TIME** para forecast (valid_time futuro é legal). O corte entra no `run_id` (`_cHHMM`) para dois cortes não se sobrescreverem; modelos cut-invariantes (RF/HW, declarado no manifest) rodam 1× SEM sufixo e o combina os difunde por corte.

## Consequências
Separa 'corte como contrato de entrada' de 'corte como conveniência de CLI'. Sem o corte na chave a idempotência do S3 faz dois cortes colidirem silenciosamente. Tratar o corte como data reintroduz o 'lê o dia inteiro'. Declarar `cut_invariant` errado produz previsões iguais para cortes diferentes. **Aspecto changed-in-rebuild:** a auto-detecção do corte é PROIBIDA em contexto de job (`PREVCARGA_JOB` dá `stop` em vez de adivinhar) — a dupla-fonte flag+auto-detect é verruga a eliminar no R-only (corte sempre do dado).

## Alternativas consideradas
`--corte` como flag humana; default fixo 08:00 em todo lugar; derivar o corte do relógio de execução; proibir todo weather com valid_time futuro; rodar todo modelo por corte (fan-out desnecessário de RF/HW); inferir `cut_invariant` em runtime.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/prevcarga:121-136
- scripts/prevcarga:160-165
- models/dessem-linear/R/ingestion/data_adapter.R:35-50
- scripts/tools/honesty_gate.R:43-52
- models/lgbm/R/inference/inference.R:256-273
- jobs/job_lib.sh:148-158
- jobs/run_job.sh:119-125
