# Estado: forecast (M6.4) -- rodada operacional diaria

- `scripts/lib/forecast.R`: `prevcarga_forecast(ctx, origin = NULL, escopo = "sin", areas = NULL, subsistema = NULL, report = FALSE,
  report_format = "html", skip_validate = FALSE, incluir_membros = FALSE)` -> `list(result, warnings)`. So COMPOE: nenhum calculo de
  k-hat/BU/SIN, de pesos, de corte de treino nem de hora (greps: sem `khat_fit`, `reconcile_train`, `combina_fit`, `registry_promote`,
  `artifacts/reconcile`, `Sys.time`, `Sys.Date`, `now(`, `Sys.getenv`). A rodada diaria e UMA origem O.
- Ordem invariante: (1) gate-0 -> (2) plano por escopo (escopo invalido: exit 8 `config.usage`; folha/subsistema fora da topologia: exit 4) -> (3) membros ->
  (4) LONG por membro + combina por folha -> (5) reconcile UMA vez, em memoria -> (6) versao/chave imutavel -> scratch + `run.json` -> export.
  Reconcile (exit 15/13/2) acontece ANTES de qualquer scratch ou diretorio de entrega: nada de `forecasts/` nem `scratch/` quando falha.
- Gate-0 (`.forecast_gate0`): origem = argumento > `ctx$origin` (flag) > dia do ultimo intervalo de carga do deck (`prevcarga_resolve_origin`, dates.R;
  nenhum relogio). `prevcarga_validate_deck` reprovado vira o abort `<classe>.<what>` do 1o erro da classe dominante (a mesma regra do verbo
  validate-deck; exit 3 no buraco do dia O). `skip_validate` so pula a validacao: os cortes (`available_until` minimo entre areas, regra de clima,
  fuso, load_label) vem SEMPRE de `prevcarga_resolve_cuts` (leitor): um buraco no dia O continua abortando exit 3 (`completude.origin_day_gap`).
- Plano (`.forecast_plan`): folhas pedidas (NULL = todas; fora da topologia => `topology.area_undeclared`, exit 4, antes do infer). `closed/stray/sin_closed`
  vem do `prevcarga_reconcile_plan`; `covered` = folhas pedidas que fecham algum subsistema (o plano atual nao o devolve: derivado em forecast.R).
- Membros (`.forecast_members`): por folha x linha de `prevcarga_members()` (sigla/mdir/exogena). origin_agnostic so le o artefato (`prevcarga_infer_cell`;
  nunca treina; ausente => `state.artifact_missing` dentro da celula, que vira membro ausente, F3.3). origin_scoped: `.forecast_scoped_version` acha o artefato da origem O pelo `origin` do
  `run_manifest.json` de cada versao do registry (proprio ou oficial sincronizado: mesmo layout; `latest` se for dele, senao a ultima em ordem radix);
  sem artefato de O => `prevcarga_train(member, area, O, ctx)` UMA vez (a fachada deriva `train_origin = O` e `last_data_day = O-1` pelo dates.R);
  deck novo do mesmo O (available_until maior) reutiliza o artefato (mesmo `v_<hash>`) e so o infer muda. O infer recebe `version =` explicita.
- LONG e combina (`.forecast_leaf_panel`/`.forecast_leaves`): `prevcarga_build_long` por membro (faixa por `delivery_days` do descriptor) -> proveniencia em
  memoria no formato do run-manifest da combina -> `prevcarga_assert_members_aligned` -> `prevcarga_build_panel` -> `prevcarga_combina_apply(detail = TRUE)`
  com os pesos congelados de `<state_dir>/artifacts/combina/<area>/` (ausente => **media simples** `prevcarga_produce_eqw`, F3.4: `.forecast_leaves` passa
  `fallback_eqw = TRUE`, sai `warning.combina_eqw` com `where` = folha, `run_manifest.model_versions["combina/<folha>"] = "EQW"` (sobreposto depois do construtor,
  que so valida `v_<hash>`), `run.json$combina_pesos = {folha: "EQW" | v_<hash>}` e o relatorio omite pesos da folha; nunca grava pesos; retreino mensal da combina
  tira a folha do EQW; latest.txt invalido/artefato de outro conjunto seguem abortando; sem corte da combina para o HHMM de
  `available_until` => `contract.combina_input`). Por lead so combinam os membros que cobrem o lead (M4.2c).
- Reconcile: `prevcarga_reconcile_apply(long_leaves[covered], ctx, subsystems = closed, khat_version = NULL, eval = FALSE)` uma vez; folhas soltas nunca passam por ele.
  `result$khat` = `[{ss, versao, khat_origin (texto), khat_age_days}]` (do reconcile) e `result$cobertura_descartada` identical ao do reconcile.
- Versao e chave: `prevcarga_forecast_versao` com `package = ctx$version` (como o M6.7 fixa; o `ctx$code_commit` so entra no run.json), `members` = por sigla x folha
  `<v_hash do artefato>:<sha256 do y_pred entregue>`, `combina_weights` = versao dos pesos por folha, `khat` = versao do perfil por subsistema,
  hash canonico da topologia efetiva, load_label e regra de clima. O hash do y_pred faz o mesmo artefato com deck/clima novo (mesmo `available_until`) virar OUTRA
  chave em vez de `export.key_conflict`. `run_id` = hash da chave (M6.7).
- Scratch `<state_dir>/scratch/<run_id>/` recriado SEMPRE: subsistemas/SIN pelo escritor do reconcile (`prevcarga_reconcile_write`), folhas (soltas sempre; cobertas so
  de todas as folhas do plano) e membros (so com `incluir_membros`) pelo mesmo escritor deterministico do LONG (`.reconcile_write_parquet`); o `run.json` registra EM `arquivos[]`
  {path, area, linhas, sha256} TODO arquivo entregue (folhas, membros, subsistemas e SIN), senao o export os omitiria com `completude.export_file_missing`.
  `runtime_id` = R + plataforma (deterministico); `generated_at` vem do ctx (unico campo nao deterministico do manifest).
- Identidade (`run_manifest.json`, fix/export-run-manifest): depois do `run.json`, `.forecast_run_manifest` grava no scratch o run-manifest do provenance.R
  (`data_hash` do deck, `bundle_id` = `prevcarga_bundle_id`, `model_versions` `<sigla>/<folha>` + `combina/<folha>` + `khat/<ss>`, `split`/`delivery_days`/`seeds` por sigla
  -- o construtor valida `split` escalar e ele e sobreposto depois, como a combina faz com `model_versions` --, `code_commit`/`renv_lock_hash` do ctx quando dados,
  `run_id`/`versao` da chave). O `export` copia o arquivo para a entrega fora de `arquivos[]` e fora da comparacao da chave (rerun continua `ja_publicado`; entregas
  antigas sem ele seguem no-op). E o que o `verificar` (M8.7) le na fase de identidade; teste `test-forecast_verificar.R`.
- Avisos: `warnings` = avisos do catalogo capturados (gate-0 `warning.<what>`, `reconcile.*`, `completude.export_file_missing`) como `list(id, message)`; o verbo (M6.9) os emite.
- Report (M6.5, `scripts/lib/reporting.R`, errors/reporting.tsv `contract.report_input`): `report = TRUE` roda `.forecast_report_step` DEPOIS do export; `result$report = {status "ok", path}` com
  `path = <state_dir>/reports/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/report.html` (fora de `forecasts/`; a entrega e identica com e sem report; falha do report: ver M6.10 abaixo; nunca
  derruba a entrega). Primitivas reusaveis pelo M7: `prevcarga_html_escape` (escapador unico), `prevcarga_html_page`, `prevcarga_html_heatmap`, `prevcarga_svg_profile`; sem script/Plotly/fonte externa, UTF-8+LF,
  sem relogio (byte-identico para o mesmo insumo). Blocos `curva` (dia O por SIN/subsistema/folha), `available-until` (global e por area), `pesos`, `khat` (copia o result do reconcile: versao, khat_origin,
  khat_age_days), `baselines` (diferenca media do previsto vs persistencia O-1 e sazonal O-7; `prevcarga_baseline` le carga <= O 00:00 <= available_until) e `proveniencia`. Nunca le a verdade do painel nem
  emite metrica de acerto. Pesos = media por membro dos pesos congelados do corte da rodada (a coluna de intercepto do relatorio mostra 0 fixo, mas a combina tem intercepto livre `a` por celula, aplicado no `y_pred`; o relatorio nao o exibe, registrado como pendencia). `format = "pdf"`: ver M6.10 abaixo.
  `.forecast_leaves` agora tambem devolve `weights` (tabela do apply por folha). Testes: `test-reporting.R` (blocos, CONTROL peso/available_until, FORBIDDEN y_true/carga apos o corte, escape, entrega com/sem report).
- Report PDF e falha parcial (M6.10, ADR-0033 proposto, errors/report.tsv `partial.report_failed`): `format = "pdf"` -> `.report_pdf` sobre `grDevices::pdf` (base R, zero dependencia nova nem de sistema; A4 paisagem, Helvetica,
  `compress = FALSE`, `useKerning = FALSE` para o texto ficar legivel nos bytes; so ASCII, o resto vira `?`), mesmos dados/linhas dos blocos HTML (`.report_available_until_lines`, `.report_khat_rows`, `.report_proveniencia_lines`,
  `.report_pesos_rows`, `.report_baselines_rows`) em `<state_dir>/reports/<origem>/<available_until>/<versao>/report.pdf`. Parenteses no PDF saem escapados (`\\(`). Falha do report (qualquer erro) DEPOIS do commit
  da entrega: `prevcarga_forecast` aborta `partial.report_failed` (exit 14) com `result` completo (`report = {status "failed", path NULL}`, causa em `details$cause`); `forecasts/` fica identico a uma rodada sem report.
  Os `warnings` da rodada (gate-0/reconcile/export, `list(id, message)`) viajam no exit 14 em `result$warnings`. O PDF e byte-deterministico (`timestamp = FALSE`: sem CreationDate/ModDate). Testes: `test-report_pdf.R`.
- Alteracoes fora do forecast.R: (a) `exports.R` `prevcarga_forecast_versao`: `sorted()` aceita lista vazia (entrega so de folhas soltas tem `khat` vazio; antes `order(NULL)` abortava);
  (b) `inference.R` `.prevcarga_inf_cuts`: `deck_areas` = areas do deck INTERSECAO folhas da topologia. Sem isso o infer de um deck que tem serie de subsistema (necessaria ao k-hat)
  abortava `topology.area_undeclared` no leitor de painel (o deck real tem SECO/S/N/NE).
- Testes: `test-forecast_compose.R` (espioes em `prevcarga_infer_cell`/`prevcarga_train`/reconcile; fixtures `m6f_*` em `helper-m6-fixtures.R`; 3 membros reais: RF, LGBM origin_agnostic + HW
  origin_scoped; k-hat e pesos treinados no setup) cobre AC1..AC8; `test-forecast_e2e.R` roda os 8 membros REAIS num `Rscript --vanilla` filho (lento, ~10-15 min).
- Corte entregavel (decisao do Gabriel, 2026-10-30): `available_until = O 00:00` (rodada das 00h, nenhuma informacao do dia O) e qualquer corte intradiario de O sao validos; so
  `available_until >= O+1 00:00` (dia O completo, ou `--origin` anterior ao ultimo dia do deck) e RECUSADO no gate-0, antes do infer, com `completude.forecast_origin_day_complete`
  (exit 3, `errors/forecast.tsv`; hint: deck vivo do dia O ou truncate_at/--corte). `combination.R` nao muda. Testes: `[00h]` em test-forecast_compose.R (espioes; 1a entrega O 00:30, sem carga de O,
  CONTROL dia O completo => recusa) e `[e2e 00h]` em test-forecast_e2e.R (8 membros reais, fases weights00/run00). Resolvido (fix/dl-00h-paridade + RD): sem hora de O os DESSEM
  rodam sem cabeca mm (dessem-linear: DM(O) pura do motor; regdin: ARIMA puro). O e2e das 00h nao tem mais skip: qualquer erro e regressao.
- ESCOPO (F3.1, substitui o teto `--niveis`, removido do forecast e do export): `area` = so as folhas pedidas (`.forecast_plan`: closed vazio, sin_closed FALSE, stray = folhas), NUNCA reconcilia, mesmo quando as folhas fecham um subsistema; `subsistema` (`--subsistema=S`) = todas as filhas de S + reconcile de S (sem SIN); `sin` (`--subsistema=all` ou sem flags) = folhas + subsistemas + SIN (closed = `topo$order`). `--area` com `--subsistema` => exit 8; subsistema fora da topologia => exit 4. O `run.json` e o result trazem `escopo`/`subsistema`; testes em test-forecast_scope.R (FORBIDDEN area x CONTROL subsistema/sin).
- Chave imutavel x `incluir_membros` (achado do M6.4): a `versao` do M6.7 nao inclui o conjunto de arquivos; a mesma chave com outro `incluir_membros` da `export.key_conflict`
  (exit 9). Opcoes: incluir incluir_membros ordenados na versao, ou o export aceitar superconjunto e publicar so os faltantes. Nao tratado aqui (exports.R e do M6.7).
- Limitacoes conhecidas (nao tratadas aqui): (1) o E2E chama a funcao publica no processo-filho; (2) a combina identifica o corte pelo
  HHMM de `available_until` DENTRO do dia da origem: dia O completo ou `--origin` anterior ao ultimo dia do deck e recusado (ver acima); (3) `incluir_membros` nao entra na `versao` (M6.7): a mesma chave com outro conjunto de arquivos e `export.key_conflict`.

## F3.3 -- tolerancia a membro ausente (celula tolerante)
- `.forecast_cell_safe` roda cada celula (membro x folha) dentro de `tryCatch(prevcarga_error)`: `status = "ok"` (delivery, member, version) ou `"absent"` (error_id, where, message; o formato de `.prevcarga_tr_cell_error`).
  Ids `config.`, `topology.`, `honesty.` e `runtime.` (`.FORECAST_FATAL`) sobem intactos e abortam a rodada na hora (nunca viram membro ausente). Sem flag nem chave de config.
- `.forecast_triage` (depois do infer, antes de combina/reconcile/scratch): `warning.forecast_member_absent` por celula ausente (where = folha; details folha/membro/error_id). Folha sem nenhum membro:
  subsistema/SIN => `completude.forecast_leaf_no_member` (exit 3, where = folha; sem scratch nem `forecasts/`, nada parcial); escopo area => a folha sai do plano (`plan$dropped`, `warning.forecast_leaf_dropped`) e as demais seguem;
  se todas caem, exit 3. Com `dropped` nao vazio a entrega normal acontece e a rodada termina em `partial.forecast_leaf_absent` (exit 14) com o result completo (padrao do `partial.report_failed`).
- `.forecast_leaf_panel` monta o LONG so dos membros presentes (`members_declared` = todos, `members_present` = presentes, como `.bt_reduce_panel`): `f_<sigla>` fica NA e a combina renormaliza (nucleo da combina intocado).
  Versao e `run_manifest.model_versions` listam so os presentes; `run.json` e `result` ganham `membros_ausentes` ([{folha, membro, error_id}]) e `dropped`. Sem falha (CONTROL) a entrega e igual a de antes.
- Efeito colateral: origin_agnostic sem artefato em TODAS as folhas deixou de abortar `state.artifact_missing` (exit 13); vira membro ausente com aviso. Teste: `test-forecast_tolerance.R`.

## F3.5 -- `forecast --jobs=N` (PSOCK, saida identica a serial)
- `.forecast_members(ctx, plan, g, mem, jobs)` monta a tabela de celulas (folha, membro) em ordem radix pela chave `"<folha>/<sigla>"` e roda `.forecast_run_cell` por posicao: `jobs <= 1` em serie (`lapply`, sem cluster);
  `jobs > 1` por `.forecast_cluster_start(ctx, min(jobs, celulas))` (`parallel::makePSOCKcluster`; sem fork/mclapply, ADR-0038) + `parLapplyLB`, com `on.exit(stopCluster)`. O resultado e REORDENADO pela chave antes de qualquer uso:
  nem N nem a ordem de chegada entram na entrega, no `versao` nem no `run_manifest` (FORBIDDEN delta 0 em `test-forecast_jobs.R`; CONTROL: sabotar uma celula muda o `versao`).
- Worker: `prevcarga_worker_init(lib, .forecast_worker_modules, ctx$worker_hooks)` (training.R) carrega a lib em R novo e fixa o runtime in-process (a semente e do despachante/`run.yaml`; nenhum `set.seed` no forecast).
  Init com falha => cluster fechado e `.prevcarga_tr_reraise` (`runtime.worker_unfixable`, exit da classe runtime), antes de qualquer scratch/entrega. Ids novos de erro: nenhum.
- Aborto fatal (`.FORECAST_FATAL`) dentro do worker cruza o PSOCK como dado (`forecast_cell_fatal`) e o pai o re-levanta com o id original (a primeira celula na ordem radix); membro ausente segue como dado (`status = "absent"`).
  Avisos emitidos dentro do worker nao voltam ao pai (as celulas hoje so abortam; sem `prevcarga_warn` na inferencia).
- CLI: `--jobs=<n>` (inteiro >= 1, senao `config.bad_type` exit 8, where `--jobs`; validador proprio em `cli/forecast.R`, pois cada verbo so carrega os proprios arquivos); consta so da usage (o Options do train-models ja o declara).
  `.forecast_stopwatch(clock, jobs)` grava o `jobs` real so no `result` do envelope, nunca em `run.json`/`run_manifest`.

## F3.6 -- forecast com artefatos apontados (`--data-artifacts`)
- `--data-artifacts=<dir>` / `artifacts_dir` do config.yaml aponta a raiz de membros, combina e k-hat. Raiz so leitura: o forecast nunca escreve nela
  (lista + sha256 identicos antes/depois; `<state_dir>/artifacts` nao nasce). Entrega, scratch e relatorio seguem em `<state_dir>`.
- Sem treino implicito: celula origin_scoped sem versao de O (`.forecast_scoped_version` NULL) aborta `state.artifact_missing` dentro da celula,
  que a celula tolerante (F3.3) transforma em membro ausente (`warning.forecast_member_absent`); `prevcarga_train` nao e chamado. Sem a flag, treina como antes.
- Proveniencia: `run.json` e `result` trazem `artefatos = {raiz, modo}` (`modo` = `apontado` | `registry`); o texto mostra `artefatos=<modo> <raiz>`.
  `raiz` e caminho da maquina (como `saida`): fora da comparacao de determinismo entre estados.
- Testes: `scripts/lib/tests/test-forecast_artifacts_dir.R`.

## E1-T4 -- deck de inferencia: sem treino implicito
- Com `ctx$data_role == "inference"` (E1-T3) o forecast so infere. `.forecast_inference_preflight(ctx, plan, mem, origin)` roda logo depois de
  `.forecast_plan`: para cada folha x sigla (ordem radix) exige, na raiz `prevcarga_registry_root(ctx)`, a versao da origem O (origin_scoped,
  `.forecast_scoped_version`) ou o `latest` (origin_agnostic). A primeira falta aborta `state.inference_artifact_missing` (exit 13) com
  `details = {membro, area, origem, split, raiz}`. So leitura: a raiz tem listagem identica antes/depois.
- `.FORECAST_FATAL` casa `state.inference_artifact_missing`: o erro nunca vira membro ausente. `.forecast_cell_safe` com papel inference nunca
  chama `prevcarga_train` (mesmo erro se a versao sumir entre o preflight e a celula).
- Papel train (`--data-train`, ou sem flag com `data_train_dir`): delta 0. Raiz default treina origin_scoped faltante; raiz apontada segue so leitura
  com membro ausente (F3.6).
- Testes: `scripts/lib/tests/test-forecast-inference-preflight.R`.

## Verbos da CLI (M6.9; dono M6)
- `scripts/lib/cli/forecast.R`: `forecast [--area=<a>... | --subsistema=<s>] [--incluir-membros] [--report] [--report-format=<f>] [--skip-validate] [--jobs=<n>] [--artifacts-dir=<dir>]` (`--artifacts-dir` e global, linha de opcao em `cli.R`; `prevcarga forecast --help` imprime so a ajuda do verbo, F2.3, ver `docs/estado/cli.md`).
  `--origin` (global do M1) e o unico flag de data; sem `--corte` (docopt => `config.usage` exit 8). `--area`/`--subsistema`/`--skip-validate`/`--report*` ja constam do bloco Options
  de outros verbos (duplicata torna o docopt ambiguo): so a usage os cita, e o `scoped` do forecast os admite. `--subsistema all` = todas as folhas; subsistema fora => `topology.area_undeclared` (4).
  O flag `--from` do snippet do ticket NAO foi declarado no forecast: `prevcarga_forecast` nao reaproveita scratch (seria flag morta).
  Avisos do catalogo viram `warnings` do envelope (`prevcarga_warn`). Relatorio falho (status `failed`) => entrega mantida e `partial.report_failed` (exit 14) com o `result` no envelope. O id `partial.report_failed` e DONO do M6.10 (errors/report.tsv; `prevcarga_forecast` o aborta); o verbo so propaga e, como fallback, usa o mesmo id sem declara-lo.
- `scripts/lib/cli/features.R`: `features --model=<m> --area=<a>` (+ `--origin`): le o artefato do registry (read-only), monta painel/cortes pelos mesmos passos da inferencia, chama
  `prevcarga_build_features(stage = "infer")` UMA vez e grava `<state_dir>/scratch/features/<m>/<a>/features.parquet`; nunca chama train/infer. Modelo fora dos `members` =>
  `topology.model_undeclared` (4); area fora => `topology.area_undeclared` (4); sem artefato => `state.artifact_missing` (13). Usa os internos `.prevcarga_plugin_stage_input`/`_guarded` do despachante (o ajuste de hora segue dele).
- Erros novos: `scripts/lib/errors/cli_m6.tsv` (so topology.model_undeclared e contract.features_not_tabular). Teste: `scripts/lib/tests/test-cli_m6.R` (processo real; exits 2/3/4/5/7/8 reais, 0/9/13/14/15 por copia com forecast.R stub, features in-process com espioes).

## M6.6 -- sub-gates [M6] do LONG e bench do NFR de 30 min
- `scripts/tools/gates/m6_long.R` (pack descoberto por `prevcarga_gates_run`, dono `M6`, tier fast, `applies_to` = os 5 plugins; nenhum `models/<m>/tests/run_all_gates.R` editado). Gates `m6_schema_long`, `m6_ytrue_real`,
  `m6_anti_leak`, `m6_no_dayshift`, `m6_primeira_entrega`, `m6_fim_da_faixa` (7 gates com `m6_nowcast_dplus0`; o charset do runner e `[a-z0-9_]`, entao `m6_<gate>` e nao `m6_long.<gate>`). O pack de dono `M6` SUBSTITUI os slots `[DEFERRED M6]`
  das tabelas (RF `gate04`/`gate08`, LGBM `gate11_ytrue_real`/`nowcast_dplus0_long`, HW `schema_long`/`ytrue_real`); `harness_*`/`gate12` ([DEFERRED M7]) sao do pack `m7_harness.R` (M7.5; ver `docs/estado/backtest.md`).
- Cada gate roda `prevcarga_long_subgates(panel, member, cuts)` (data.frame gate/passed/detail) sobre o painel de `prevcarga_build_long` com entrega SINTETICA na grade real do membro; `delivery_days` vem do descriptor do plugin (lido num
  `Rscript --vanilla` filho). `no_dayshift` compara a correlacao y_pred x y_true no mesmo dia com a de +-1 dia (48 leads). `primeira_entrega`/`fim_da_faixa` comparam com `prevcarga_delivery_grid` (sem aritmetica de 30 min).
  CONTROLs em `test-long_subgates.R` (lead off-by-one, deslocado 1 dia, y_true = y_pred, fim 1 slot antes, inicio em available_until).
- `scripts/tools/bench_forecast.R --origin O --reference-seconds S [--format text|json] [--data d] [--state d] [--config f]`: cronometra `prevcarga_forecast(report = TRUE)` (proc.time elapsed), alvo FIXO 1800 s (sem flag; so
  `bench_run(budget_s=)` em teste), estouro => veredito `bloqueante`, exit 1, "NFR 30 min estourado". `--state` e COPIADO para tempdir (fora do cronometro); data/ listado (nome, tamanho, sha256) antes/depois (symlink de diretorio nao e
  seguido: o store real tem `data/data -> .`), diferente => `integrity.bench_data_changed` (exit 9, `errors/bench_forecast.tsv`). Nunca executa o oraculo. `treinou_origin_scoped` conta chamadas de `prevcarga_train` durante o forecast.
- Medicao REAL (<= 1800 s no data/ real, com artefatos origin_agnostic e k-hat ja treinados) e do operador: o numero (segundos, razao, host) entra em `docs/estado/` pelo fechamento do epico (M6.11).
- Mapa slot `[DEFERRED M6]` -> gate `m6_*` (nenhum gate some em silencio; o pack de dono M6 substitui os slots):
  | slot substituido (runner) | gate que cobre |
  |---|---|
  | RF `gate04` (schema_long multilead D..D+8, 432 leads) | `m6_schema_long` |
  | RF `gate08`, LGBM `gate11_ytrue_real` (y_true real, nao 0/NA) | `m6_ytrue_real` |
  | HW `schema_long` | `m6_schema_long` |
  | HW `ytrue_real` | `m6_ytrue_real` |
  | LGBM `nowcast_dplus0_long` (oraculo gate_nowcast_dplus0) | `m6_nowcast_dplus0` |
  `m6_nowcast_dplus0`: nos cortes 00:00 (dia O inteiro, 1o rotulo O 00:30), 12:00 (1o rotulo 12:30) e 13:30 (1o rotulo 14:00), d_plus = 0 e o SUFIXO do dia O apos o corte: leads k+1..48 (k = slots ate
  available_until; numeracao por interval_start, entao 12:00 => leads 25..48, 13:30 => 28..48; o oraculo numera pelo rotulo de fim). CONTROL: lead dentro do ja conhecido, ausencia do sufixo e painel de 00h contra corte 12:00 reprovam.

## Clima previsto incompleto corta o horizonte (F3.7; decisao do Gabriel)
- Clima previsto incompleto NAO bloqueia a rodada: corta o horizonte por membro. Dia coberto = a emissao vigente (<= `weather_issue_limit`) traz os 48
  instantes do dia (predicado unico `prevcarga_forecast_coverage_end`, validation.R), sem tolerancia ao 23:30. A rodada so e recusada quando o dia O
  inteiro esta incompleto (o membro dependente de clima aborta `completude.weather_forecast_short`; HW univariado segue).
- `prevcarga_infer_cell` devolve `delivery_days_efetivo` e `weather_forecast_until` (ultimo dia coberto, do leitor); `.forecast_cell_safe` usa o efetivo em
  `member$delivery_days` (build_long e proveniencia da combina; por lead so combinam os membros que cobrem o lead, nunca preenchido).
- `.forecast_cell_capture` captura os `prevcarga_warning` da celula (inclusive no worker PSOCK) e o pai os re-emite no sink na ordem radix das celulas;
  `warning.forecast_weather_horizon_cut` sai UM por folha: `details = {folha, ate, membros}` e mensagem `clima previsto ate <data>; a inferencia sera feita ate essa data`.
- `run.json` e `result` ganham `clima_previsto_ate` (minimo entre as folhas do ultimo dia coberto) e `delivery_days_efetivo` (folha x sigla). Clima completo
  ate O+9 => nenhum aviso, `delivery_days_efetivo` = descriptor e entrega identica a de antes (FORBIDDEN em `test-forecast_weather_cut.R`).
- Membro ausente por clima curto (RF, `weather_cut = "absent"`) vira `warning.forecast_member_absent` (`error_id = completude.weather_forecast_short`); nenhuma linha dele.
- DESSEM (e RF) com clima curto ficam ausentes (`weather_cut = absent`); LGBM corta e HW entrega inteiro; de O+4 em diante a combina usa so os que cobrem o lead.
- Teste: `scripts/lib/tests/test-forecast_weather_cut.R` (stubs de dependencia; o fixture m6f tem 3 membros: RF faz o papel de DESSEM ausente, LGBM o do membro que corta, HW o do univariado; FORBIDDEN com clima completo = DESSEM presente).

## Verificacao do epico 3 (F3.8): matriz de escopo, tolerancia, paralelismo e clima
- `scripts/lib/tests/test-forecast_matrix.R`: 12 linhas pairwise (escopo area/subsistema/sin x membro ausente nenhum/um/folha vazia x pesos
  treinados/EQW x clima O+9/O+3 x `--jobs` 1/2) sobre o fixture m6f, rodando `prevcarga_forecast` de verdade. O espiao de infer fica numa COPIA do
  lib (`ctx$lib_dir`), entao vale igual no processo e nos workers PSOCK; usa o leitor e o corte de clima REAIS com descriptors-stub (RF = DESSEM-like,
  `weather_cut = absent`, offset 60; LGBM = truncate; HW = sem clima). Nenhum modelo real treina ou infere. Pesos EQW = `combina/<folha>/latest.txt` removido.
- Invariantes verificadas em toda linha: nenhum subsistema/SIN com folha ausente ou fora do plano; folha vazia em subsistema/sin aborta
  `completude.forecast_leaf_no_member` (exit 3) sem scratch; escopo area com folha vazia sai `partial.forecast_leaf_absent` (exit 14) com as demais
  entregues; nenhuma linha de membro alem de O + `delivery_days_efetivo` 00:00 (nenhum dia de clima persistido); com clima O+3 o RF (DESSEM-like) fica
  ausente, o LGBM entrega ate O+4 00:00 e `clima_previsto_ate = O+3`. Cada linha com jobs = 2 e re-rodada com jobs = 1 (state separado): versao, run_id,
  sha256 de arquivos[] e do run_manifest.json identicos.
- Comportamento verificado = decisao F3.7 (DESSEM so com horizonte inteiro; clima curto => `completude.weather_forecast_short` e membro ausente), nao o
  texto original do ticket, que previa o DESSEM cortando.
- Comandos (um arquivo por vez, R 4.5.3, nada de `run_all_gates.R` de plugin nem `run_suite` do diretorio inteiro, que traz o e2e de 8 membros reais):
  `Rscript -e 'testthat::test_file("scripts/lib/tests/test-forecast_matrix.R")'`; os 8 testes do epico + `test-errors-catalog-sync.R`; as unidades do 3.7
  (`test-engine-feriados-exogena.R`, `test-exo-prev-cut.R` do dessem-regdin e do dessem-linear, `test-delivery-grid-cut.R` do lgbm).

## Pendencias registradas (sem decisao nesta spec; doc-vs-codigo)
- **Intercepto 0 no relatorio da rodada** (cli-backtest-ops-11, combina-reconcilia-02): `.forecast_weights_table` grava `intercept = 0` fixo e o `weight` e a media dos `w` de todas as celulas do corte (todos os leads e meses),
  nao so das celulas (lead, mes de O) aplicadas; o `a` real esta no artefato e e aplicado no `y_pred`. O comentario de `forecast.R` ("a combina nao tem intercepto") esta defasado. Registrado como pendencia; o codigo nao muda e o site nao afirma comportamento novo.
