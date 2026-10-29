# Estado: verbo score (M3.3)

- `scripts/lib/cli/score.R` (auto-descoberto; dono M3, nao isento) e o handler fino `cmd_score(ctx, args)`: `--model` (existencia via
  `prevcarga_model_spec`) -> escopo de `--area` pela topologia (`topology.area_undeclared`, exit 4, ANTES de ler o deck) -> gate-0
  `prevcarga_validate_deck` (salvo `--skip-validate`; reprovado = abort com o id do primeiro erro da classe dominante) ->
  `prevcarga_read_long` -> `prevcarga_score`. Convencao da fachada: texto devolve linhas ASCII, JSON devolve o `result`.
- `prevcarga_score(long, ctx, area, baseline_kind, origin)` em `evaluation.R`: areas pontuaveis = folhas + subsistemas
  (`prevcarga_topology`; NULL/`SIN` = todas as presentes no LONG); `y_true` embutido e SEMPRE descartado e re-juntado do deck por
  igualdade exata `target_datetime == interval_end` (`prevcarga_read_actual`); area sem particao some do scorecard; baseline por
  (area x forecast_origin); scorecard por (area, d_plus) pelo motor unico; manchete `d_plus = 1`; `available_until` do leitor
  (nunca relogio); `data_hash` de `prevcarga_data_hash`. Sem nada a pontuar: exit 0 + aviso `warning.nothing_to_score`
  (`errors/score.tsv`); sem carga na origem so e tolerado nesse caso.
- Aviso com `type`: `cli.R` repassa `details$type` da condicao `prevcarga_warning` ao item de `warnings` do envelope (`warnings[[i]]$type`).
- Resolvido (fix/topology-ids-hour): `interval_start` vem da borda do LONG (`schema_long.R`), nao de aritmetica em
  `evaluation.R`; `topology.area_unknown` fundido em `topology.area_undeclared` (mesma classe/exit).
- Testes: `scripts/lib/tests/test-score.R`.
