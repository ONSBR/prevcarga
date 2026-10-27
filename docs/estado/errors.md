# Estado: contrato de erros (ADR-0040)

- `scripts/lib/errors.R` e o unico dono do contrato: `prevcarga_exit_codes` (0-15), `prevcarga_exit_precedence()`,
  `prevcarga_abort/warn`, `prevcarga_collect_errors()`, render texto/JSON, `prevcarga_with_errors()` e o export do catalogo.
- Catalogo distribuido: cada modulo declara seus `error.id` em `scripts/lib/errors/<nome>.tsv`
  (`id class message hint`, ASCII, TAB, LF). `core.tsv` traz os ids do M1. Nunca editar `errors.R` para acrescentar ids.
- Loader: `prevcarga_errors_load()` (ordem radix; aborta `contract.error_catalog_dup` / `contract.error_catalog_schema`).
- `install/error-catalog.tsv` e GERADO por `scripts/tools/export_error_catalog.R` e nunca versionado.
- Testes: `scripts/lib/tests/test-errors.R`, `test-errors-catalog-sync.R` (todo `prevcarga_abort("<id>")` consta no catalogo).
- `partial.report_failed` e declarado pelo M6.10 em `errors/report.tsv`.
- Dependencias: `docopt` (fachada da CLI) nao esta declarado no environment da spec; deve entrar no renv lock/bundle.
- `scripts/tools/export_error_catalog.R` guarda `prevcarga_runtime_fix()` com `exists()` ate o ticket de runtime do M1 existir; manter a guarda ate la.
