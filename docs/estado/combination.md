# Estado: combination (M4.1 - nucleo numerico)

- `scripts/lib/combination.R`: modulo numerico in-process (base + data.table + lubridate). Exporta
  `prevcarga_proj_simplex` (Duchi, rho = ULTIMO TRUE), `prevcarga_fit_convex` (FISTA no sistema normal centrado,
  intercepto livre, parada unica por gradient mapping do objetivo BRUTO ou cap com warning + `nonstationary`),
  `prevcarga_proj_grad_norm`, `prevcarga_combina_month_start`, `prevcarga_combina_train_mask`,
  `prevcarga_combina_fit` (1 linha por celula `(lead, cut, ym)`), `prevcarga_produce_combined`, `prevcarga_produce_eqw`
  e `prevcarga_combina_defaults` (n_min 24, iters 500, grad_tol 1e-6).
- Leak-fix em UMA funcao: `train_mask` = dia do `interval_start` (`target_datetime - 30 min`, ADR-0035) anterior a
  inicio(m) no fuso da carga; eixo de agrupamento = mes-de-ORIGEM. Nao usa `prevcarga_train_origin`.
- `grad_tol` e ABSOLUTO: em carga real (~1000 MW) bate o cap; normalizar por `||b||` da falsa parada [TBD: ADR-0026].
  Fixtures de teste em escala O(1) (`helper-combination.R`, formula fechada, sem RNG).
- Coluna `f_*` toda-NA na celula fica fora do X (base da regra por lead do M4.2c); fallbacks `first_month|no_member|n_min` = EQW (`a = 0`).
- Catalogo: `scripts/lib/errors/combination.tsv` declara os 6 ids do M4 (unico ticket do M4 que declara erros).
- Dependencia nova: pacote `data.table` instalado no R 4.5.3 (1.18.6.1).
- Testes: `scripts/lib/tests/test-combination-core.R` (AC1-AC9 com CONTROLs).

## M4.2 -- IO, descoberta, painel por corte, join fresco
- `combination.R` agora tambem: `prevcarga_combina_discover(root, area, ctx, members)` (lista de membros so da topologia;
  o disco apenas LOCALIZA o run por `(mdir, EXOGENA)` + `_cHHMM`, agnostico ao run-mode; nunca le `split`/`protocol`;
  >1 run-mode por identidade = `contract.combina_run_mode_dup`; 0 runs de um membro nao e erro; erro do `read_long`
  propaga com o id original e o caminho do run em `where`), `prevcarga_build_panel` (pivot por
  `(area, forecast_origin, target_datetime, lead, d_plus, cut)`, `f_<sigla>` na ordem canonica, `ym` = mes-de-ORIGEM,
  `y_true` do membro descartado, sem replicar membro entre cortes), `prevcarga_join_fresh_truth` (y_true so por
  `prevcarga_read_actual`, igualdade EXATA `target_datetime == interval_end`), `prevcarga_combina_load` (pipeline unico)
  e `prevcarga_combina_panel_hash` (sobre o `prevcarga_version_hash` do M1.3: o conteudo vai em `data_fingerprint`).
- Layout de run provisorio `<root>/<mdir>/<area>/<from>_<to>_<run_mode>[_<EXO>][_cHHMM]/{long.parquet, run_manifest.json}`
  [TBD: ADR novo de layout de run, M6]. `run_id` NAO e unico entre membros que dividem diretorio-nome; a chave e `(member, run_id)`.
- Area aceita = folhas E nos de subsistema da topologia (SECO, S, N, NE); no de perda (PESE...) ou desconhecida =
  `topology.area_undeclared`.
- `cut` do painel = HHMM de `available_until` do run-manifest (lido por `ymd_hms(tz = load_tz)`), nunca do nome do diretorio.
- `read_actual` e `(from, to]`: o join recua `from` em 1 s para nao perder o primeiro alvo.
- Em aberto (humano): conversoes de -30 min (`train_mask`, `evaluation.R`) fora do leitor unico (principio 7); M4.2 nao adiciona nenhuma.
- Testes: `test-combination-io.R` + `helper-combination-io.R` (fixtures em tempdir; CONTROL io.py e de rotulo trocado).

## M4.2b -- recusa de membro desalinhado
- `prevcarga_assert_members_aligned(long, ctx)` (combination.R), inserido em `prevcarga_combina_load` entre discover e
  build_panel (unico caminho de montagem). Exige INICIO e GRADE comuns; o FIM e por membro: ultimo `target_datetime` =
  `O + delivery_days` 00:00 lido so do run-manifest (faixas diferentes entre membros nao sao desalinhamento).
- Aborta `contract.member_misaligned` (exit 2), `where = "<sigla>:<campo> esperado=.. obtido=.."`. Ordem fixa: membros na
  ordem declarada, runs por run_id; por run: proveniencia (f) -> load_label/load_tz/weather_issue_cutoff vs ctx (a) ->
  tzone (b) -> grade (c) -> sufixo `_cHHMM`/available_until (d) -> primeira entrega/deslocamento (d) -> delivery_days e
  buraco de grade (e). A primeira falha e a reportada.
- Acoplamento: o rotulo esperado vem de `ctx$load_label` ou, sem ele, do deck (`.prevcarga_deck_ctx`, manifest > flag >
  config); `ctx$tz` ou o fuso do manifest do deck. A grade e checada localmente (sem `prevcarga_delivery_grid` do P.0a, ainda
  nao integrado); trocar por ela depois e opcional. `first = available_until + 30 min` e aritmetica de grade da combina,
  nao conversao de rotulo (nao toca o leitor unico).
- `tzone` fora do fuso da carga: o leitor unico (`read_long`) ja recusa com `contract.long_tz` no discover; a recusa da
  combina cobre LONG montado em memoria. Teste exercita os dois.
- Testes: `test-combination-align.R` + `helper-combination-align.R` (8 membros alinhados, 10 mutacoes, CONTROL do sentinela).
