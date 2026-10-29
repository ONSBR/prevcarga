# Estado: schema_long (M1.2b)

- `scripts/lib/schema_long.R`: constante `prevcarga_schema_long` (8 colunas, nome -> tipo logico, ordem do ADR-0002)
  e `prevcarga_schema_long_check(dt)`.
- `forecast_origin` e texto `yyyy-MM-dd` (character, ADR-0032); `target_datetime` rotulado por interval_end.
- O check confere so nomes e ORDEM (1a coluna ausente/fora de ordem => `contract.long_schema_constant`, exit 2,
  `where` = coluna); nunca modifica `dt`. Tipos finos, tz e `d_plus` sao do leitor `prevcarga_read_long` (M3).
- Borda do rotulo (UNICO par do LONG; regra "o dado de carga e operado 1x na entrada e 1x na saida"):
  `prevcarga_long_interval_start(target_datetime, granularity_min, tz = NULL)` (leitura) e
  `prevcarga_long_target_datetime(interval_start, granularity_min, tz = NULL)` (escrita, para o writer do M6.1);
  `granularity_min != 30` => `contract.long_schema` (`where = granularity_min`), tz dado e divergente => `contract.long_tz`.
  `prevcarga_long_attach_interval_start(long, tz)` anexa a coluna 1x (ja presente => intocado); so o leitor
  (`prevcarga_assert_long`) o chama (gate `long_attach`); consumidores exigem a coluna e abortam sem ela. O writer do M6.1 deve DESCARTAR `interval_start` antes
  do `prevcarga_schema_long_check` (coluna extra reprova). Gate `minutes30` (test-static-greps.R) proibe +-30 min fora de
  `deck_reader.R`, `schema_long.R` e `features.R` (reamostragem).
- Requer `errors.R` carregado (e lubridate para a borda). Testes: `scripts/lib/tests/test-schema-long.R` (usa data.frame; data.table nao e dependencia do M1; ida e volta da borda,
  granularidade e fuso errados).
