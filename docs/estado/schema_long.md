# Estado: schema_long (M1.2b)

- `scripts/lib/schema_long.R`: constante `prevcarga_schema_long` (8 colunas, nome -> tipo logico, ordem do ADR-0002)
  e `prevcarga_schema_long_check(dt)`.
- `forecast_origin` e texto `yyyy-MM-dd` (character, ADR-0032); `target_datetime` rotulado por interval_end.
- O check confere so nomes e ORDEM (1a coluna ausente/fora de ordem => `contract.long_schema_constant`, exit 2,
  `where` = coluna); nunca modifica `dt`. Tipos finos, tz e `d_plus` sao do leitor `prevcarga_read_long` (M3).
- Requer `errors.R` carregado. Testes: `scripts/lib/tests/test-schema-long.R` (usa data.frame; data.table nao e dependencia do M1).
