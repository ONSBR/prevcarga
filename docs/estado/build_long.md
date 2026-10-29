# Estado: build_long_output (M6.1)

- `scripts/lib/build_long_output.R`: `prevcarga_build_long(delivery, member, area, origin, available_until, truth, ctx)`,
  conversor UNICO e puro (sem I/O, sem env) da entrega do membro (`data.table(interval_end, y_pred)`, ja semi-horaria)
  para o painel LONG 8-col de `prevcarga_schema_long` (nao redefinida). Requer errors.R, features.R, schema_long.R.
- Rotulo: o conversor trabalha em `interval_start` (da entrega, ou derivado por `prevcarga_long_interval_start`) e
  produz `target_datetime` SO por `prevcarga_long_target_datetime` (schema_long.R); nenhum +-30 min aqui. O
  `interval_start` e coluna interna e e descartado antes de `prevcarga_schema_long_check`.
- `target_datetime` = `interval_end` em `ctx$tz`; `d_plus` = dia do `interval_start` - O; `lead = d_plus*48 + slot`;
  `forecast_origin` TEXTO `yyyy-MM-dd` (ADR-0032); `granularity_min = 30L`.
- Faixa: inicio COMUM = regra unica `target_datetime > available_until`; fim por membro = `prevcarga_delivery_grid(cuts,
  member$delivery_days)`. Fora disso aborta `contract.member_misaligned` com `details$member`. Fins diferentes passam.
- `y_true` do truth do leitor unico, casado pelo valor numerico do instante de `interval_end`; truth ausente ou so ate
  available_until => NA sem stop; nunca toca `y_pred`.
- Validador `.long_validate`: `contract.long_duplicate_lead`, `contract.long_grid`, `contract.long_offbyone`
  (declarados em `scripts/lib/errors/build_long.tsv`). `contract.long_tz` (evaluation.tsv) e `contract.member_misaligned`
  (combination.tsv) JA existiam: o loader exige id unico, entao NAO foram redeclarados.
- Testes: `scripts/lib/tests/test-build_long.R` + `helper-m6-fixtures.R` (fixtures de sentinela puros; o M6.4 estende).
