# Estado: combination (M4.1 - nucleo numerico)

- `scripts/lib/combination.R`: modulo numerico in-process (base + data.table + lubridate). Exporta
  `prevcarga_proj_simplex` (Duchi, rho = ULTIMO TRUE), `prevcarga_fit_convex` (FISTA no sistema normal centrado,
  intercepto livre, parada unica por gradient mapping do objetivo BRUTO ou cap com warning + `nonstationary`),
  `prevcarga_proj_grad_norm`, `prevcarga_combina_month_start`, `prevcarga_combina_train_mask`,
  `prevcarga_combina_fit` (1 linha por celula `(lead, cut, ym)`), `prevcarga_produce_combined`, `prevcarga_produce_eqw`
  e `prevcarga_combina_defaults` (n_min 24, iters 500, grad_tol 1e-6).
- Leak-fix em UMA funcao: `train_mask` = coluna `interval_start` do painel (anexada pela borda do LONG, ADR-0035; exige
  a coluna, sem aritmetica de hora propria) anterior a inicio(m) no fuso da carga; eixo de agrupamento = mes-de-ORIGEM.
  Nao usa `prevcarga_train_origin`.
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
- Painel carrega `interval_start` logo apos `target_datetime`, vindo SO do `read_long` (discover): `build_panel` e
  `assert_members_aligned` exigem a coluna (ausente => `contract.combina_input`, `where = build_panel:interval_start` /
  `members_aligned:interval_start`). Nenhuma conversao -30 min na combina; fixtures em memoria usam `pc_long_mem`.
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
  nao integrado); trocar por ela depois e opcional. Comparacao pelo `interval_start` do leitor: 1o esperado = o proprio
  `available_until` (sem +30); fim = ultimo `target_datetime` == `O + delivery_days`; n de alvos = passos de 30 min entre os
  dois. Deslocamento (faixa 1 passo fora) e lido so pelas bordas `prevcarga_long_interval_start/target_datetime`; o `where`
  continua reportando o rotulo do LONG (interval_end).
- `tzone` fora do fuso da carga: o leitor unico (`read_long`) ja recusa com `contract.long_tz` no discover; a recusa da
  combina cobre LONG montado em memoria. Teste exercita os dois.
- Testes: `test-combination-align.R` + `helper-combination-align.R` (8 membros alinhados, 10 mutacoes, CONTROL do sentinela).

## M4.2c -- combinacao por lead (delivery_days)
- Regra do ADR-0032 c em `combination.R`: cada membro cobre `target_datetime <= ymd(O) + days(delivery_days)`; a faixa vem so do
  `delivery_days` do run-manifest (proveniencia), nunca do nome do modelo; nenhuma sigla nem literal de faixa no modulo (grep no teste).
- `prevcarga_member_coverage(provenance, panel)`: tabela longa `(member, forecast_origin, cut, lead)` com `covered` e `last_lead`
  por `(member, cut)`. `delivery_days` NA ou com 2 valores no mesmo `(member, cut)` = `contract.member_misaligned` (campo `delivery_days`).
- `prevcarga_build_panel` grava `NA_real_` em `f_<sigla>` fora da faixa (ausente, nunca 0) e anexa o atributo `coverage`
  (member, cut, delivery_days, last_lead); `join_fresh_truth` o preserva. Sem coluna `delivery_days` na proveniencia nao ha mascara.
  Pelo caminho normal o M4.2b ja recusa a cauda; a mascara defende chamadas diretas.
- Fit/produce nao mudaram: o X da celula `(lead, cut, ym)` ja usa so colunas `f_*` nao-NA (k variavel, simplex nos presentes,
  EQW `1/k_presentes`, warm-start so com o mesmo conjunto); o produce usa so `names(w)`.
- `prevcarga_combina_members_by_lead(weights)`: 1 linha por `(cut, lead)` com siglas presentes (uniao dos meses) e `n_members`;
  lead sem membro e omitido. Consumido pelo freeze (M4.3) e pelo JSON `members_by_lead` (M4.4).
- `.prevcarga_cmb_trim_output(panel, y_pred)`: descarta linhas sem membro cobrindo, aborta `contract.combina_input` com y_pred NA na
  faixa e confere por `(origem, corte)` que os alvos == `prevcarga_delivery_grid` (P.0a, features.R) ate a MAIOR `delivery_days` do corte.
  Dependencia nova de carga: `combination.R` precisa de `features.R` so nesta funcao.
- `prevcarga_combina_full`/`_apply` (M4.3) ainda nao existem: os testes compoem fit + produce + trim, o mesmo caminho.
- Testes: `test-combination-lead.R` + `helper-combination-lead.R` (8 membros, curto ate O+1; FORBIDDEN cauda mascarada, CONTROL dd=9).

## M4.3 -- freeze/apply content-addressed + saida LONG
- `prevcarga_combina_full(panel, ctx, t_cut, ..., detail)`: fit + produce num passo, so do corte selecionado (celulas de cortes sao
  independentes => mesmos pesos do train de todos os cortes: base do split-parity). Consome `panel$y_true` como recebido (nao junta verdade, nao le disco).
- `prevcarga_combina_train(area, panel, artifact_dir, root, members, ..., trained_at)`: layout `<artifact_dir>/<area>/v_<16hex>/{weights.rds,
  weights.rds.manifest.json}` + `<area>/latest.txt` (1 linha, tmp `.tmp-*` + `file.rename`, sem symlink; escrito POR ULTIMO). Staging unico `.tmp-*`
  irmao do destino + `file.rename` (ADR-0030); orfaos sao do `registry gc` (M5). Re-treino identico = `reused = TRUE`, sem reescrever o `weights.rds` (so reaponta o latest).
  Mesmo layout do prefixo `artifacts/combina/` do bucket (ADR-0039); so arquivos locais (bucket nao usado aqui). `root` e argumento (code_commit, renv.lock, topologia): sem env.
- `version_hash` = `prevcarga_version_hash` do M1.3 (8 chaves): `model_id = "combination"`, `data_fingerprint` = `prevcarga_combina_panel_hash` (agora sha256 de 64 hex,
  linhas ordenadas `(area, cut, lead, forecast_origin)`, `f_*` all-NA entram como null), `cutoff` = 1o dia do ultimo mes fitado, `feature_config` (area, n_min, iters,
  grad_tol, cuts, `member_set_hash`, `delivery_days` por `membro@corte`, `members_by_lead`), `code_commit`, `renv_lock_hash`, `load_label`/`load_tz` efetivos da proveniencia.
  `trained_at` fica fora do hash; `code_commit` com `-dirty` muda o hash (comportamento do M1.3).
- Congela `list(meta, weights)` com `prevcarga_freeze` (M1.3); antes, `prevcarga_plugin_scan_pure` (P.0d) varre meta (sem a lateral `trained_at`, que o scan trata como relogio) e
  weights. `weights`/`members_by_lead` vao como data.frame plano (a varredura recusa o ponteiro `.internal.selfref` do data.table). `meta` = `prevcarga_run_manifest` do M1.3
  (`split = origin_scoped`, `seeds = list(combination = 0L)` sem RNG) + `area`, `version_hash`, `members_by_lead`; `model_versions` = lista nomeada vazia
  (provisorio): o validador do run-manifest recusa lista vazia, entao entra um placeholder valido e o campo e trocado pela lista vazia logo apos. Nunca `cutoff` no manifest.
- `prevcarga_combina_apply(area, artifact_dir, panel, ctx, t_cut, members)`: sem `latest.txt` => `state.combina_not_trained` (13); `prevcarga_thaw` confere o sha256 antes do
  readRDS (versao ausente/byte alterado => integrity 9); outro `member_set_hash`/`load_label`/`load_tz` => `contract.combina_artifact_mismatch` (2). Nunca re-fita.
- Selecao de corte `.prevcarga_cmb_select_cut`: '0900' e 'c0900' equivalem; NULL com >1 corte => `contract.combina_input` "corte ambiguo"; inexistente lista os disponiveis.
- LONG: `.prevcarga_cmb_to_long` aplica `trim_output` (M4.2c) e escreve `target_datetime` por `prevcarga_long_target_datetime` (schema_long.R; a coluna `interval_start` NAO sai); nenhuma
  aritmetica de hora na combina. `forecast_origin` character ISO, `lead/d_plus/granularity_min` integer. Validado por `prevcarga_schema_long_check` e `prevcarga_assert_long`.
  Nota de acoplamento: M6.1 (`build_long_output.R`, em outro branch) deve reaproveitar o mesmo par de bordas; este writer fica local em `combination.R`.
- `prevcarga_assert_long` (M3.1) coage `lead` double inteiro-valido; o CONTROL do teste checa a classe, e o de fuso (UTC) falha a asserção.
- Testes: `test-combination-freeze.R` + `helper-combination-freeze.R` (AC 13-16 do epico; CONTROLs de split-parity, hash, bucket e `.tmp-*` orfao).
