# Estado: evaluation (M3.1)

- `scripts/lib/evaluation.R` e a UNICA definicao de metrica: `prevcarga_wmape` (100*sum|e|/sum|y_true| nos pares
  `is.finite`; unico `sum(abs(` do repo, via `.prevcarga_abs_total`), `mape` (exclui y_true==0), `mae`, `rmse`,
  `bias` (MW + `bias_pct` ratio-of-sums), `peak_wmape` (janela 18-22h sobre a hora do `interval_start`; [TBD: ADR-0028]).
- `interval_start` vem da borda do LONG (`schema_long.R`): `prevcarga_assert_long` anexa a coluna UMA vez
  (`prevcarga_long_attach_interval_start`, `granularity_min != 30` => `contract.long_schema`, tz != `ctx$tz` =>
  `contract.long_tz`); o check de `d_plus`, os buckets hora/dia/pico e o `scorecard` leem a coluna. `evaluation.R` nao tem
  nenhum `minutes(30)`. `scorecard`/`prevcarga_score` EXIGEM a coluna (ausente => `contract.long_schema`,
  `where = interval_start`); so o leitor chama o attach (gate `long_attach`). Testes montam LONG em memoria com `pc_long_mem`.
- Borda do LONG: `prevcarga_assert_long` (nome+ordem via `prevcarga_schema_long_check`, re-emitido como
  `contract.long_schema`; tipos; tz; `d_plus` pelo dia do interval_start => `contract.long_dplus_label`) e o leitor unico
  `prevcarga_read_long(path, ctx)`; `forecast_origin` e texto `yyyy-MM-dd`, coluna Date e recusada. O frame devolvido tem 9
  colunas (8 do schema + `interval_start`); LONG em disco com `interval_start` e recusado (`contract.long_schema`,
  `where = interval_start`). `prevcarga_assert_long` e idempotente: o frame do leitor (8 + `interval_start` no fim) e
  re-conferido e a coluna tem de bater com a borda (ex.: `prevcarga_check_coherence` re-asserta o LONG do verbo).
- `prevcarga_read_actual(ctx, area, from, to)`: wrapper sobre `prevcarga_read_load` sem `origin` (ex-post, sem corte),
  janela `(from, to]` por `interval_end`; particao ausente = 0 linhas. Falta de `load_mwh` (erro cru do arrow no leitor)
  e reclassificada aqui como `contract.dataset_missing` (`where = load.load_mwh`).
- `prevcarga_baseline` (persistence = O-1; seasonal_naive = ultimo dia da semana < O, d_plus 0..9) le so `interval_end <= O 00:00`
  e refiltra no builder. `prevcarga_skill_score` e `prevcarga_scorecard` sao puros (sem ctx/caminho).
- Catalogo: `scripts/lib/errors/evaluation.tsv` declara os `error.id` do M3; area fora da topologia usa o id unico
  `topology.area_undeclared` (`errors/topology.tsv`, exit 4), o antigo `topology.area_unknown` foi fundido nele.
- Testes: `scripts/lib/tests/test-evaluation.R` + `helper-eval.R` (fixtures sobre o deck sentinela).
