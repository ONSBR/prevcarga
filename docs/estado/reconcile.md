# Estado: reconcile (R.1, nucleo puro)

- `scripts/lib/reconcile.R`: funcoes puras (sem I/O/leitor/registry). Requer errors, schema_long, evaluation
  (`prevcarga_assert_long`), data.table e lubridate.
  - `prevcarga_reconcile_plan(leaves_present, topology)` -> `list(closed, stray, sin_closed)` (ordem de `topology$order`).
  - `prevcarga_reconcile_bottomup(long_leaves, subsystem, topology, ctx)`: soma `y_pred` por
    (forecast_origin, target_datetime, lead, d_plus, granularity_min), so onde `n == length(areas)`;
    `attr(, "dropped")` = `data.table(d_plus, n)`; aviso `reconcile.coverage_dropped`. Folha ausente =>
    `completude.leaf_missing`; lead divergente/duplicata => `contract.reconcile_input`.
  - `prevcarga_khat_ratio(truth, subsystem, topology)` (SS_true/BU_true por interval_end, BU_true so com todas as folhas),
    `prevcarga_khat_fit(ratio, origin, window_days)` (janela `[O-W, O-1]` inclusiva, bucket hour x month x dow do
    `interval_start`, dow com `week_start = 7`, `glob` = media dos ratio da JANELA), `prevcarga_khat_apply(bu, profile, ctx)`
    (`y_pred = BU_pred * k`, sem clamp, `attr n_fallback`, aviso `reconcile.khat_fallback`),
    `prevcarga_reconcile_sin(long_subsystems, topology)` (`n == length(topology$subsystems)`),
    `prevcarga_reconcile_long(dt, area, ctx)` (LONG 8 colunas, `forecast_origin` texto, `y_true` NA).
- Regra: "soma por interval_end, bucket por interval_start". O Bottom-Up agrupa por `target_datetime` (= interval_end; bijecao com
  interval_start, mesmo agrupamento); o `interval_start` do bucket vem so de `prevcarga_long_interval_start` (schema_long.R).
  O modulo nao faz aritmetica de hora (gate `minutes30`).
- Divergencias deliberadas do oraculo: bucket por interval_start (o oraculo usa o rotulo de fim), glob da janela (o oraculo usa todo o historico).
- `scripts/lib/errors/reconcile.tsv` declara todos os ids do epico. Os avisos usam classe `completude` (errors.R nao aceita classe `ok`).
- Testes: `test-reconcile-core.R`, `test-reconcile-khat.R`, helper `helper-reconcile-fixtures.R`.

## R.2 -- perfil k-hat como artefato do registry (unify, decisao do Gabriel 2026-10-29)
O perfil k-hat e mais um artefato no layout do registry: `<state_dir>/artifacts/reconcile/<ss>/{latest.txt,
v_<hash>/{artifact.rds, artifact.rds.manifest.json, run_manifest.json, SHA256SUMS}}` (mesmo prefixo do bucket;
copia byte a byte aplica sem conversao). `reconcile.R` guarda SO o que e especifico do k-hat; publicar/resolver/
carregar/mover o ponteiro sao do nucleo `prevcarga_registry_*` (M5.2), sem copia nem excecao no gate `registry_lifecycle`.

API (ctx = `list(state_dir=, tz=)`; a raiz sai de `prevcarga_registry_key`, nao e argumento `root`):
`prevcarga_khat_key(ss, ctx)`, `prevcarga_khat_data_hash(ratio)`, `prevcarga_khat_version(fit, meta)` (fingerprint de 8
chaves, model_id `reconcile/<ss>`, cutoff = O-1), `prevcarga_khat_freeze(fit, meta, ctx)` (no-op se mesmos bytes;
`integrity.khat_conflict` se o v_<hash> existe com bytes diferentes; staging `.tmp-*` do registry),
`prevcarga_khat_advance_latest(ss, versao, khat_origin, ctx)` (so para frente; menor => `reconcile.khat_not_promoted`;
escreve via `prevcarga_registry_promote_latest`), `prevcarga_khat_resolve(ctx, ss, khat_version)` (ausente =>
`state.khat_missing`, exit 13), `prevcarga_khat_thaw(ctx, ss, versao)` (SHA256SUMS do registry + schema =>
`contract.khat_profile`). `trained_at` e lateral: so no `run_manifest.json`, nunca no artefato nem no hash.

Desvios do plano original, por unify: (1) nao existe `prevcarga_khat_promote` (gate registry_lifecycle; vira
`prevcarga_khat_advance_latest` + core); (2) assinaturas recebem `ctx` em vez de `root`; (3) adulteracao no thaw sai
`integrity.artifact_hash` (exit 9, SHA256SUMS do core) e nao `integrity.freeze_sha_mismatch`; (4) manifest e
`run_manifest.json` (nome do core), com `arquivos[{path:'artifact.rds', sha256}]`. Consumidores (R.3, CLI) chamam esta API.

## R.3 -- orquestracao train/apply (`reconcile.R`, secao final)
- `prevcarga_reconcile_train(ctx, origin, subsystems = "all", window_days)`: `last_data_day` de `prevcarga_resolve_cuts(ctx, O)` (dates.R,
  sem conta de corte local); verdade SO por `prevcarga_read_load(ctx, areas = c(folhas, ss), from = O-W, to = O-1)`; ratio -> fit ->
  meta (convencao `load_label`/`tz` vem dos atributos do leitor) -> `prevcarga_khat_freeze` -> `prevcarga_khat_advance_latest` (unify R.2;
  nao existe `prevcarga_khat_promote`). Devolve por ss `{versao, path, khat_origin, n_ratio, promovido, k_hat_resumo{min,max,fora_faixa}}`;
  `fora_faixa` conta celulas do perfil fora de `khat_range` (so informa, nunca clampa).
- `prevcarga_khat_check_policy(prof, ctx, origin)`: unico lugar da politica de uso do perfil. **khat_origin == O sempre, com ou sem pino
  (Gabriel, 2026-10-29)**: o k-hat e REAJUSTADO POR ORIGEM, como o oraculo (Delta = 0); nunca ha reuso de perfil de outra origem.
  Posterior => `honesty.khat_after_origin`; anterior => `state.khat_missing` (rode train para esta origem); convencao =>
  `contract.khat_convention` (`where` = `load_label`|`timezone`). O pino (`khat_version`) so escolhe entre versoes ajustadas EM O.
  `khat_age_days` e sempre 0L (mantido por contrato de saida do `result$khat`).
  `.prevcarga_khat_resolve_for_origin` acha a versao da origem (latest se for dela; senao varre as versoes da celula, pois o backtest
  treina para tras e o `latest.txt` so anda para frente). O registry/`latest.txt` (R.2) segue so armazenando/resolvendo o perfil de uma origem.
  Convencao efetiva = `ctx$load_label`/`ctx$tz`, ou o manifest do deck quando ausentes.
- `prevcarga_reconcile_apply(long_leaves, ctx, subsystems = "all", khat_version = NULL, eval = FALSE)`: valida a entrada (`assert_long`;
  uma unica `forecast_origin`), recusa subsistema pedido que nao fecha (`completude.leaf_missing`, `where` = folhas ausentes; nada devolvido),
  resolve+thaw+politica+khat_apply por ss, SIN so quando todos os subsistemas da topologia foram pedidos. Nada grava em disco.
  Retorno `{long{ss..., SIN|NULL}, khat, cobertura_descartada, wmape|NULL, scorecard|NULL, warnings}`.
- `need_truth = eval`: sem `eval` nenhum leitor e chamado e `y_true` e NA. Com `eval`, `prevcarga_read_actual` por area (a saida nao traz a
  coluna da area), casado por `target_datetime == interval_end`; `wmape{bottomup, reconciliado, teto}` por `prevcarga_wmape`
  (`teto` = BU_pred + (SS_true - BU_true)); `y_true` do SIN = soma dos `y_true` dos subsistemas; `y_pred` independe da verdade.
- `apply` e single-origin (uma `forecast_origin` por chamada, senao `contract.reconcile_input`): a rodada diaria e uma origem; o backtest
  roda muitas origens em SEQUENCIA para tras, um train+infer por origem, chamando train/apply uma vez por origem. Nada a relaxar.
- Testes: `test-reconcile-apply.R` (leitor espionado; FORBIDDEN/CONTROL).

## Golden do oraculo e paridade Delta=0 (R.6)

- `scripts/tools/reconcile_golden.R` (dev-only, Linux, fora do bundle): `Rscript --vanilla scripts/tools/reconcile_golden.R --origin O [--oracle dir] [--rscript <Rscript do oraculo>] [--oracle-env NOME=VALOR ...] [--out dir] [--lock arquivo]`. `rg_build_inputs(O, dias, semente)` e funcao pura (sem RNG, sem gravar nada): verdade S = RS+PR+SC, 760 dias antes de O + 10 dias a frente, e a combina-A de O (d_plus 1..9, lead 1..432). O gerador grava a vista do oraculo em `tempdir()` e roda `reconcilia_baseline.R` em processo filho (`--mode full --subsystem S --window-days 730 --eval`); falha se `git status`/mtimes do oraculo mudarem, se sair com erro ou imprimir `fallback glob_ratio`. `--oracle-env` (repetivel) vai so ao filho (R_LIBS_USER, R_LIBS_SITE, R_HOME do R 4.2.3 do oraculo quando o pai e 4.5.3): configuracao, nunca literal. Windows: sai com "so Linux" (exit 2), sem rodar.
- Onde mora o golden (decisao do Gabriel, 2026-10-29: extrato de dado nunca e versionado): as saidas do oraculo ficam em `.state/golden/reconcile/` (gitignored): `golden_profile.parquet` (k por hour x month x dow, rotulo de inicio) e `golden_long.parquet` (y_pred/BU_pred por intervalo, interval_end). Versionado so o lock hash-only `scripts/lib/tests/fixtures/reconcile/golden.lock` (commit e R do oraculo, comando, origem 2026-02-20, janela 730, dias 760, semente 42, digest canonico da entrada `inputs.sha256`, sha256 + linhas de cada saida) e o README com a receita. A entrada nao e versionada: o teste a regera com `rg_build_inputs()` e confere o digest do lock (valores a 6 casas, independente do ultimo ulp do libm).
- `test-reconcile-parity.R` (Linux e Windows, nunca chama oraculo/Rscript): sempre roda o lock (digest da entrada regerada = lock, com CONTROL de semente) e os bracos sinteticos (FORBIDDEN: inflar a verdade >= O da Delta = 0 em k e y_pred; CONTROL: inflar o dia O-730, `week_start = 1`, janela de 729 dias e bucket por interval_end quebram contra o ajuste base, > 1e-6). Com `.state/golden/reconcile/` presente: sha256 divergente do lock = FALHA; `khat_ratio` + `khat_fit(O, 730)` + `bottomup` + `khat_apply` = Delta=0 (tol 1e-9) em k, y_pred e BU_pred, `n_fallback = 0`, e os 3 CONTROLs quebram a paridade contra o golden. Sem `.state`: SKIP com a receita.
- Achado de leitura do oraculo: a janela real e `seq(O - W, O - 1)` = `[O-730, O-1]` (730 dias), nao o `(O-W, O-1]` do comentario; R.1 segue o codigo. A origem do golden foi escolhida para que o dia O-730 caia num bucket coberto pelos alvos (senao o CONTROL de 729 dias nao teria dente).
- Excecoes conscientes do A/B do M8 (insumo do R.V): glob da janela (oraculo: todo o historico, inclusive futuro); fallback silencioso removido (aviso + `n_fallback`); perfil reutilizado em O >= khat_origin no apply de producao; bucket por interval_start derivado de interval_end - 30 min na borda do LONG (mesmo Delta = 0).

## R.4 -- reach-control das bordas da janela do k-hat
- `prevcarga_reach_control(module_fn, input, arms, compare, tol)` (honesty_gate.R, generico, reusavel por M6/M7): copia profunda da ENTRADA por braco,
  delta por SAIDA (`.prevcarga_reach_delta`: merge nas chaves; chaves diferentes ou NA = Inf). Veredito HONESTO | VAZA | INDETERMINADO
  (forbidden > tol => VAZA; braco que nao alterou a entrada ou control <= tol => INDETERMINADO). Sem disco/subprocesso/env.
  A comparacao da entrada ignora o `index` automatico do data.table (`.prevcarga_reach_same`, via as.data.frame).
- `prevcarga_khat_reach_arms(origin, window_days, ss)` (reconcile.R): FORBIDDEN `futuro` (dias >= O) e `antes_da_janela` (<= O-W-1);
  CONTROL `ultimo_dia` (O-1) e `primeiro_dia` (O-W); perturbam SS_true x1,3+1 so nas linhas do subsistema.
- Canais comparados: k por (hour, month, dow) e y_pred de um BU fixo cobrindo um intervalo em cada dia de [O-W, O-1] (garante que todo
  bucket da janela e atingido; o bucket do dia O nao seria, e cairia no glob). Mutacoes (59 dias, 61 dias, incluindo O) => INDETERMINADO, VAZA, VAZA.
- Testes: `test-honesty_gate-reach_control.R`, `test-reconcile-honesty.R` (verdade de entrada O-120..O+5; ~15 s).
