# ADR-0004 — Store canônico: parquet Hive `area_code=/year=`, 1 arquivo por partição (não DB, não monólito)

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0002](0002-grade-semihoraria-schema-long.md), [ADR-0005](0005-contrato-entrada-fail-por-origem.md)

## Contexto
A camada de dados precisa de um layout físico previsível, inspecionável e S3-syncável que o leitor de y_true e o scan de completude consigam endereçar por path determinístico.

## Decisão
Granularidade física = (tipo de série × área × ano) com um único `data.parquet` por folha e chaves Hive no path (`data/<tipo>/area_code={area}/year={YYYY}/data.parquet`). A migração e a consolidação ML gravam exatamente nesse layout; o builder lê esse caminho para juntar y_true. Auxiliares de baixa cardinalidade fogem do esquema (load_tiers só por área; dst_periods global).

## Consequências
O caminho é hard-coded no leitor de y_true e no scan de completude; mudar o esquema de partição ou o nome `data.parquet` quebra ingestion de todos os modelos e o join de y_true. É o acoplamento físico que o rebuild R-only honra byte-a-byte ou migra conscientemente. Correlato em aberto: a leitura tolera aliases de coluna (não fixa schema canônico) e pode deixar y_true=NA em silêncio.

## Alternativas consideradas
Dataset Arrow/parquet único multi-arquivo descoberto por filtro; partição por mês; DuckDB/SQLite; registry de schema versionado. Fixar o schema canônico (`load_mwh`/`timestamp`) e falhar alto na leitura em vez de tolerar aliases.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/data-prep/migrate_legacy_data.py:241-245
- scripts/data-prep/consolidate_for_ml.py:116-120
- scripts/lib/build_long_output.R:117-118
- scripts/lib/build_long_output.R:125-127
- scripts/data-prep/migrate_legacy_data.py:45-66

## Decisão (resolvida 2026-10-10, Gabriel)
**Falhar alto, schema canônico.** O store tem schema fixo (`timestamp` + `load_mwh` + `area_code`); a leitura **não** tolera aliases — se a coluna canônica faltar, `stop('[prevcarga/contract] …')`, **nunca** `y_true=NA` silencioso. Aliases de formatos legados são resolvidos **uma única vez** no `data-prep` (migração), jamais em todo read. O `validate-deck` (M2) checa o schema canônico explicitamente. (Decisão compartilhada com [ADR-0005](0005-contrato-entrada-fail-por-origem.md).)
