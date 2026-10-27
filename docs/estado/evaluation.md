# Estado: evaluation (M3.1)

- `scripts/lib/evaluation.R` e a UNICA definicao de metrica: `prevcarga_wmape` (100*sum|e|/sum|y_true| nos pares
  `is.finite`; unico `sum(abs(` do repo, via `.prevcarga_abs_total`), `mape` (exclui y_true==0), `mae`, `rmse`,
  `bias` (MW + `bias_pct` ratio-of-sums), `peak_wmape` (janela 18-22h sobre a hora do `interval_start`; [TBD: ADR-0028]).
- `prevcarga_long_interval_start(long, ctx)` = `target_datetime - 30 min` (unico `minutes(30)`); todo bucket
  hora/dia/pico/d_plus usa este ponto. `granularity_min != 30` => `contract.long_schema`; tz != `ctx$tz` => `contract.long_tz`.
- Borda do LONG: `prevcarga_assert_long` (nome+ordem via `prevcarga_schema_long_check`, re-emitido como
  `contract.long_schema`; tipos; tz; `d_plus` pelo dia do interval_start => `contract.long_dplus_label`) e o leitor unico
  `prevcarga_read_long(path, ctx)`; `forecast_origin` e texto `yyyy-MM-dd`, coluna Date e recusada.
- `prevcarga_read_actual(ctx, area, from, to)`: wrapper sobre `prevcarga_read_load` sem `origin` (ex-post, sem corte),
  janela `(from, to]` por `interval_end`; particao ausente = 0 linhas. Falta de `load_mwh` (erro cru do arrow no leitor)
  e reclassificada aqui como `contract.dataset_missing` (`where = load.load_mwh`).
- `prevcarga_baseline` (persistence = O-1; seasonal_naive = ultimo dia da semana < O, d_plus 0..9) le so `interval_end <= O 00:00`
  e refiltra no builder. `prevcarga_skill_score` e `prevcarga_scorecard` sao puros (sem ctx/caminho).
- Catalogo: `scripts/lib/errors/evaluation.tsv` declara todos os `error.id` do M3 (so este ticket declara erros no epico).
- Testes: `scripts/lib/tests/test-evaluation.R` + `helper-eval.R` (fixtures sobre o deck sentinela).
