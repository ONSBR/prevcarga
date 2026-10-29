# Estado: verbo check-coherence (M3.4)

- `scripts/lib/cli/check-coherence.R` (auto-descoberto; dono M3, nao isento): handler fino `cmd_check_coherence(ctx, args)`
  (convencao da fachada: texto devolve linhas ASCII, JSON devolve o `result`). `--long=<f>` -> fuso do deck
  (`.prevcarga_eval_tz`, como o score; exige `--data`/deck com manifest) -> `prevcarga_read_long` ->
  `prevcarga_check_coherence`. `--area` e recusado no handler (`config.usage`, exit 8): a flag existe na gramatica global
  (score/validate-deck) e o docopt a aceitaria.
- `prevcarga_check_coherence(long, topology, tol = 1e-5, ctx = NULL)` em `evaluation.R`: PURO, topologia como dado
  (`prevcarga_topology`). Acumula todos os findings; ordem de execucao (e) alinhamento -> (d) `topology.area_undeclared` (1 linha por
  area) -> (c) `completude.node_missing` (regras R1-R4 da cobertura-comum, ADR-0015) -> (a) `coherence.sin_sum` e (b) `coherence.khat`
  (faixa efetiva por subsistema vinda de `prevcarga_topology`; `indefinido` quando soma de folhas <= 0, sem dividir). Origens com
  grade deslocada ficam fora de (c)/(a)/(b). Findings ordenados por (check, forecast_origin, target_datetime, area). Classe-dona
  por `prevcarga_exit_precedence()` (completude 3 > topology 4 > coherence 11). Linha duplicada por (instante, area) =
  `contract.long_schema`. Sem nenhum `error.id` novo (de `errors/evaluation.tsv`, M3.1, e `topology.area_undeclared` de `errors/topology.tsv`).
- Falha: `prevcarga_abort(id, result = ...)` -> o envelope JSON de falha carrega `result.findings` (mudanca minima em `errors.R`: argumento
  opcional `result` na condicao e `render_error` repassa `cond$result` ao envelope). No texto a tabela ASCII vai para stdout e o erro
  classificado para stderr.
- `ctx$tz` ausente em chamada de modulo: o fuso do proprio LONG e usado (a conferencia contra o fuso do deck e do verbo).
- Resolvido (fix/topology-ids-hour): `topology.area_unknown` fundido em `topology.area_undeclared`. Alinhamento (e) compara
  conjuntos de `target_datetime` (sem aritmetica de hora). Em aberto: faixa default de k-hat [TBD: ADR-0028].
- Testes: `scripts/lib/tests/test-coherence.R` + `helper-coherence.R` (reusado pelo M3.V).
