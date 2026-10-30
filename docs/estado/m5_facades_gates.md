# Estado: sub-gates [M5] pelas fachadas (M5.8)

Codigo: `scripts/tools/gates/m5_facades.R` (pack descoberto por `prevcarga_gates_run`, P.0h); teste barato `scripts/lib/tests/test-m5-facades-gates.R`;
edicao minima em `scripts/tools/plugin_gates.R` (`.pc_gates_substitute`). Nenhum `models/<m>/tests/run_all_gates.R` foi editado.

## Descoberta e substituicao do slot
- O runner procura `prevcarga_gate_pack_<nome do arquivo>`: o arquivo `m5_facades.R` define `prevcarga_gate_pack_m5` (nome do contrato do ticket) e o alias
  `prevcarga_gate_pack_m5_facades`. Linhas: `m5_determinism`, `m5_split_honesty`, `m5_intraday`, `m5_weather_reach`, `m5_day0_infer`, `m5_transparency`
  (dono `M5`, tier `fast`, `applies_to` = os 5 membros, `test` = funcao que o runner executa; erro => FAIL).
- `plugin_gates.R`: um pack descoberto de dono `Mx` SUBSTITUI os slots `DEFERRED Mx` da tabela do runner (e os testes `[DEFERRED Mx] <gate>:` correspondentes).
  `[DEFERRED M6|M7|M8]` seguem SKIP com dono. Duplicata real (mesmo gate em duas fontes) continua `tools.gate_unowned`. Sem o pack o slot volta como SKIP
  (CONTROL: um slot DEFERRED nunca e bloqueante, entao o runner continua saindo 0; a ausencia do pack e visivel na tabela, nao no exit).

## Cenario (um processo filho por modelo)
- O cenario pesado roda num `Rscript --vanilla` filho (a API da fundacao no `globalenv()` do filho, como a CLI; o runner nao e sujado), sem env vars.
  Pelas fachadas `prevcarga_train` / `prevcarga_infer_cell` sobre um deck de `fixture_store.R` (areas SP e ZZ1, 85 dias, O = 2026-10-01; SP com carga ate 12:00,
  ZZ1 ate 09:00 => `available_until` 09:00 com carga "ragged" escondida pelo leitor). `ctx$spec_overrides`: `history_days.train` = 60 (e `.infer` quando o
  descriptor/constraint iguala), `params.ntree` = 5 quando o spec declara `ntree`. Tempo por runner: RF ~3 min, HW ~20 s, LGBM ~5 min, dessem ~5-6 min.
- Bracos escolhidos por CAMPO (`spec$split`, `spec$granularity_min`, `spec$modes`, `descriptor$data_lag_days/reads_day_O/weather_forecast_in_train/uses_rng`):
  `prevcarga_gate_pack_m5_expected` e a matriz; `prevcarga_gate_pack_m5_assert` falha se faltar braco, se algum falhar ou se vier SKIP. Braco que nao se aplica
  (`m5_weather_reach` sem clima previsto no treino; `m5_day0_infer` sem modo forecast+`reads_day_O`) sai PASS com arm `NOT_APPLICABLE` e o motivo.
- Determinismo: 2 rodadas (|delta y_pred| <= 1e-5 e mesmo sha256 do `artifact.rds`). CONTROL (so `uses_rng`): semente +1 (e todo param `*seed*`, ex. `kmeans_seed` do HW).
  O braco passa se o y_pred diverge (> 1e-5) OU o sha256 do artefato muda: HW e dessem-linear ficam com delta y_pred = 0 no fixture (sorteio estavel) e so o sha256 sustenta o CONTROL.
- Honestidade do treino por classe de split (via fachada; `last_data_day` = `row$last_data_day` de `prevcarga_train`, vindo do `dates.R`): FORBIDDEN = carga depois de
  `last_data_day` (inclui O) => sha256 igual; CONTROL = o proprio `last_data_day` => sha256 diferente; `FORBIDDEN_LAG` (data_lag_days > 0, LGBM): so (last_data_day, O-1];
  `DECKS` (origin_scoped): decks de O com `available_until` 09:00 e 12:00 => mesmo `v_<hash>` e sha256. O `prevcarga_honesty_gate` do M3.2 e por y_pred/painel in-memory (nao tem metrica sha256), por isso
  os bracos usam `prevcarga_fixture_perturb` no deck (CONTROL por fixture, mesmo molde FORBIDDEN Delta=0 / CONTROL Delta != 0).
- Intradiario: FORBIDDEN = carga de SP depois do `available_until` (escondida pelo leitor) => delta 0. CONTROL: `descriptor$reads_day_O` => ultima janela antes do corte
  (granularidade 30: 08:30-09:00; 60: 08:00-09:00) => delta != 0; senao carga de O-1 com RETREINO pela fachada (o membro nao le o dia O no infer).
- Clima (so `weather_forecast_in_train`): regra `available_until` (emissao 18Z de O FORBIDDEN, 06Z CONTROL) e `origin_midnight_utc` (06Z FORBIDDEN, 00Z CONTROL; treina de novo com a regra).
- Manha de O no infer (`forecast` em `spec$modes` e `reads_day_O`: dessem): manha <= `available_until` muda o infer (> 1e-5) e NAO muda o sha256 do artefato retreinado; depois do corte delta 0.
  O delta da manha no dessem-regdin e pequeno no fixture (~7e-4; intradiario 4.5e-5), acima da tolerancia.
- Transparencia: artefato publicado pela fachada `identical` (digest do conteudo) ao de `prevcarga_plugin_train` direto, e entrega do infer `identical` a de `prevcarga_plugin_infer` direto.

## Pendencias
- `models/dessem-linear` e `models/dessem-regdin` ainda nao tem `tests/run_all_gates.R` (DL.9/RD.4); o pack foi exercitado nos dois pelo cenario direto (todos os bracos PASS) e entra
  sozinho quando os runners chamarem `prevcarga_gates_run`.
- LGBM nao declara `uses_rng` no descriptor: sem braco CONTROL de semente (o ticket esperava).
- Windows nativo: o codigo usa so `R.home("bin")/Rscript`, `system2`, `tempfile` (sem fork, symlink, shell ou env); nao executado aqui (cross-OS e `[DEFERRED M8]`).
