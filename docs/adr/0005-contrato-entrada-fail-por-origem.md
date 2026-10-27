# ADR-0005 — Contrato de entrada por-modelo (fail-por-origem, erro classificado) + validação na fronteira (fail-high cedo)

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0002](0002-grade-semihoraria-schema-long.md), [ADR-0006](0006-backtest-honesto-corte-treino.md), [ADR-0007](0007-contrato-corte-intradiario.md), [ADR-0017](0017-empacotamento-bundle-s3-entrega.md)

## Contexto
Dado faltante/curto precisa falhar visível e cedo — nunca imputar silenciosamente nem produzir previsão por default assumido. O harness roda `run_pipeline` como subprocesso, então o erro tem de cruzar o limite de processo de forma parseável.

## Decisão
A validação de entrada é uma biblioteca de asserções componível (`assert_non_empty`/`no_na`/`no_missing_days`/`covers_days`) que CADA modelo instancia com suas janelas (não um schema global); violações lançam `prevcarga_contract_error` com campos grepáveis `area/origin/what/detail` e marcador `[prevcarga/contract]`, cruzando subprocessos via exit≠0 + stderr. A casca valida o contrato na FRONTEIRA antes do MAP pesado (corte obrigatório, carga presente por folha, bundle completo conforme a consulta), com as guardas de `data_adapter`/inferência como 2ª linha (defesa em profundidade).

## Consequências
É a espinha de ENTRADA simétrica ao `build_long_output` (saída) e o que torna o backtest honesto: truncar o parquet às 08:30 faz o modelo abortar com '< 24 slots' — comportamento projetado. Sem validação na fronteira o erro vira stack R críptico ou previsão silenciosamente errada.

## Alternativas consideradas
Validador de schema único global (JSON/arrow-schema) para todas as séries; backfill sintético e seguir (gate08 legado, aposentado); confiar só nas guardas internas do modelo.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/lib/validate_input.R:24-33
- scripts/lib/validate_input.R:55-81
- scripts/prevcarga:152-178
- docs/ESTADO-ATUAL.md:438-439

## Decisão (resolvida 2026-10-10, Gabriel)
**Falhar alto, schema canônico** também na LEITURA (não só na validação de entrada): a assimetria atual (entrada rígida via `validate_input`, leitura de `y_true` tolerante a aliases → NA silencioso) é eliminada. Coluna canônica ausente ⇒ `stop('[prevcarga/contract] …')`. Aliases legados resolvidos 1× no `data-prep`. (Decisão compartilhada com [ADR-0004](0004-store-parquet-hive.md); implementada no módulo `validation.R` / `validate-deck`, M2.)
