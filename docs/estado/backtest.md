# Estado: backtest (M7.3 MAP, M7.7 REDUCE)

Codigo: `scripts/lib/backtest.R`; catalogo `scripts/lib/errors/backtest.tsv`; testes `scripts/lib/tests/test-backtest.R` (toys das duas classes + deck do fixture_store).
O REDUCE (M7.7) e o relatorio (M7.4) estao no fim deste arquivo.

- `prevcarga_backtest(ctx, origins, models, areas, cortes, protocol, ...)`: resolve `cortes` (flag > `ctx$backtest$corte_default` > 08:00), monta `prevcarga_plan()`
  (protocolo incompativel aborta ali, exit 2 `contract.protocol_incompatible`, antes de criar qualquer arquivo) e despacha `prevcarga_orchestrate()` com `.bt_make_run_cell()`.
  Devolve `list(run_id, status, summary, paths)`; `paths$members` = tabela (membro, folha, origem, corte, long_parquet). O verbo `backtest` liga o MAP real por
  `prevcarga_backtest_cell_runner(ctx, plan)` (gancho `.m7_run_cell_for(ctx, plan)` em orchestrate.R; o plano agora carrega `ident`).
- Truncagem: `.bt_cut_instant(tz, O, HH:MM)` (lubridate, fuso do manifest) vai como `truncate_at` ao leitor unico; o `available_until` nasce no leitor. Nenhum derivador aqui.
- Cortes de treino: nunca calculados aqui; a linha do plano (dates.R + `data_lag_days` do descriptor) manda e o treino confere (`contract.cut_mismatch` se divergir).
  `fixed` treina na menor origem (1x), `daily`/`retrain_harness` por origem. `retrain_harness` = `prevcarga_plugin_forecast` (so origin_scoped), painel com a historia do treino.
- Artefato: chave `artifact` do cache.R (folha e spec efetivo em `feature_config`; available_until nunca entra), so em `<state_dir>/cache/artifact/`; treino = `prevcarga_train_inputs` +
  `prevcarga_plugin_train` + `prevcarga_artifact_build` SEM publicar no registry (a fachada `prevcarga_train` sempre publica, por isso nao e usada). Infer = `prevcarga_infer_cell(artifact=, truncate_at=)`.
- LONG da celula: `prevcarga_build_long` (8 colunas, 1a entrega `available_until + 30`) + `prevcarga_schema_long_check` (so nomes/ordem) + `.bt_check_types` (forecast_origin texto) + NA em y_pred
  aborta `contract.backtest_long_na`. `run_cell` devolve o LONG; o orquestrador publica `long.parquet` + `.key` (chave esperada do plano). `member_long` NAO e cacheado: a chave do cache
  nao tem folha na identidade (repete entre folhas do mesmo membro/origem/corte) e colidiria.
- Alteracoes fora do backtest.R: `inference.R` (`prevcarga_infer_cell` aceita `artifact` e `truncate_at`; `prevcarga_infer_read_panel` aceita `truncate_at`, ambos opcionais) e `orchestrate.R`.
- M7.8 (clima previsto ponta a ponta): o M7 so repassa `ctx$weather_issue_cutoff` ao leitor e registra o limite (nenhum filtro de `issue_time`/`freshest` em backtest.R/cache.R; grep em `test-backtest-weather.R`).
  Chave do artefato (`.bt_key_args`): regra + `cell$train_weather_issue_limit` (limite do TREINO, vindo do dates.R no plano) + `weather_forecast_in_train` do descriptor. Chave `member_long` do plano: regra + limite da
  celula (`prevcarga_weather_issue_limit` do leitor sobre o corte simulado e o lag do manifest); regra diferente => `cache_key`/`.key` e `run_id` diferentes (celula `done` de outra regra recomputa no `--resume`).
  `prevcarga_backtest()$summary$weather_issue_cutoff`, `plan --format json` e o `result` do backtest expoem a regra efetiva (flag > config.yaml > default). Desvio: o ticket cita `spec$inputs$weather_forecast$train`;
  o run.yaml nao tem esse campo, a fonte e o descriptor `weather_forecast_in_train`. Testes: `test-backtest-weather.R` (FORBIDDEN/CONTROL por regra, lag, serial x `--jobs 2`, molde honesty_gate_weather), `test-cache.R`.

## REDUCE (M7.7) -- combina honesta por mes, reconcile por origem, avaliacao com y_true fresco

Codigo: `backtest.R` secao 8 (`.bt_reduce`), gancho do verbo `prevcarga_backtest_reduce` (chamado por `prevcarga_backtest_run` em orchestrate.R quando backtest.R esta carregado);
testes `scripts/lib/tests/test-backtest-reduce.R`. `cli/backtest.R` agora carrega evaluation.R, combination.R e reconcile.R. Store `<state_dir>/backtest/<run_id>/{combined,weights,reconciled,scores}`;
o registry de producao (`<state_dir>/artifacts/`) nunca e aberto (grep + spy de leitura nos testes; sha256 de cache/ e backtest/ estavel ao `registry gc`).

- Painel (`.bt_reduce_panel`): por (folha, corte), LONG das celulas `done` pelo leitor unico (`prevcarga_read_long`), proveniencia montada do plano (run_id = origem; delivery_days do descriptor),
  `prevcarga_assert_members_aligned` (membro desalinhado e recusado pelo M4) -> `prevcarga_build_panel` -> `prevcarga_join_fresh_truth` (y_true fresco por `interval_end`). Nao existe `prevcarga_combina_panel`
  (o texto do ticket): o pipeline e o mesmo de `prevcarga_combina_load` (M4.4) sobre o layout do backtest. Membro ausente => coluna `f_*` NA e a combina renormaliza.
- Combina (`.bt_combine`): `prevcarga_combina_full(detail = TRUE)`; a mascara de treino e `prevcarga_combina_train_mask` do M4 (`interval_start < inicio(m)`: o alvo 00:00 do dia 1 entra), nunca redefinida aqui.
  `n_min` = `ctx$combina$n_min` (default 24; nao ha chave em config.yaml). Grava `combined/<FOLHA>/<HHMM>/combined.parquet` e `weights/<AAAA-MM>/<FOLHA>_<HHMM>.rds`; meses em fallback => `summary$fallback_months`
  (folha, corte_hm, mes). Nunca le pesos nem ponteiro de producao.
- Reconcile (`.bt_reconcile`): so quando o plano cobre TODAS as folhas da topologia (um `--area` parcial nao e origem incompleta; nao ha reconcile). Folha sem linha combinada na origem => `summary$incompleto`
  (origem, corte_hm, available_until, folhas_faltando) e nenhum arquivo em `reconciled/` para ela. Completo => bottom-up -> ratio -> `prevcarga_khat_fit(ratio, O, window_days)` (janela [O-W, O-1]; W = `ctx$khat_window_days`
  ou `prevcarga_khat_window_days`, 730; `topology.R` nao tem `khat$window_days`) -> `prevcarga_khat_apply` -> SIN. A verdade do k-hat vem de `prevcarga_read_load` (leitor unico). Falha do reconcile (ex.: sem verdade do
  subsistema) sai em `summary$reconcile_failures` e torna o veredito `partial`. `.bt_khat_apply` e pura sobre `truth` (reach-control das duas bordas no teste).
- Avaliacao (`.bt_evaluate`): y_true re-juntado fresco (`prevcarga_read_actual`, por area) em membros, combinado e reconciliado; SIN nao existe no deck: sua verdade = soma dos subsistemas no instante (so se todos presentes).
  So funcoes de evaluation.R (`prevcarga_scorecard`, `prevcarga_baseline`, `prevcarga_wmape`). `scores` = list(member, combined, sin, peak_fixed_18_22, sin_fresh, awaiting_truth, by_available_until, dplus, fallback_months);
  scorecards por (corte_hm, [member], d_plus, area) com `skill_persistencia_O1`/`skill_sazonal_O7` conforme `--baseline` (`todos` default; `persistence`, `seasonal_naive`, `none`). `awaiting_truth` por (origem, corte_hm): sem nenhuma verdade
  finita => `awaiting = TRUE` e nenhuma linha de metrica (nunca 0). Grava `scores/scores.rds` e `scores.json` atomicos.
- Resumo/veredito: `summary` ganha `incompleto`, `fallback_months`, `reconcile_failures`, `cache = prevcarga_cache_stats()`; `partial` se houver falha de celula, incompleto ou falha de reconcile (exit 14 no verbo,
  com `incompleto` no `result`). A coluna e `corte_hm` (a chave `corte` e proibida pelo grep do M2).
- Testes/suites tocadas por consequencia: test-backtest.R, test-backtest-weather.R e test-orchestrate-psock.R carregam evaluation/combination/reconcile; o deck do psock ganhou a area `X` (subsistema do k-hat).

## Relatorio `backtest --report` (M7.4) -- so render sobre o painel avaliado

Codigo: `reporting.R` secao `.rpt_bt_*` (marcador `## --- Relatorio do backtest (M7.4)`), `backtest.R` secao 9 (`.bt_run_meta`, `.bt_report_step`), fio no verbo em `orchestrate.R` (`prevcarga_backtest_run`);
testes `scripts/lib/tests/test-backtest-report.R` (insumo sintetico + honesty-gate) e o teste `[M7.4]` no fim de `test-backtest-reduce.R` (verbo real). Erro novo `contract.report_without_truth` em `errors/backtest.tsv`.

- `prevcarga_backtest_report(ctx, scores, run_meta, format = c("html","pdf"), out_dir)`: valida `scores` (`member`, `combined`, `sin`, `peak_fixed_18_22`, `sin_fresh`, `fallback_months`, `awaiting_truth` ja avaliados; sem eles ou sem coluna `wmape`
  aborta `contract.report_without_truth`, exit 2) e `run_meta` (`contract.report_input`). Nenhuma metrica e calculada: WMAPE, pico 18-22h (janela sobre `interval_start`), vies com sinal, skill O-1/O-7 e SIN fresco (`sum_check <= 1e-5`) vem de evaluation.R via REDUCE.
- Blocos (mesma lista alimenta HTML e PDF): proveniencia (run_id, janela, load_label, fuso, versao, `weather_issue_cutoff`, `available_until` simulado por corte, protocolo por membro), WMAPE por `d_plus` (combinado + reconciliado, e por membro; curva SVG por area no HTML),
  pico+vies, skill (so `none`/`sem baseline` quando a coluna nao existe), SIN fresco, meses em fallback `n_min`, origens aguardando verdade. NA vira o texto ASCII `aguardando verdade` (ou `sem baseline`), nunca 0.
- Saida: `<state_dir>/backtest/<run_id>/report/backtest.html` (UTF-8, LF, atomico via `.m7_write_atomic`) e, com `pdf`, `backtest.pdf` pelo mesmo motor do M6.10 (`.pdf_open` extraido de `.report_pdf`: `grDevices::pdf`, `timestamp = FALSE`; byte-deterministico).
  `prevcarga_html_page` ganhou o argumento `titles` (default = os titulos do forecast).
- Verbo: `--report`/`--report-format` rodam o passo DEPOIS do REDUCE (`result.report_path`, `result.report_status`); falha do report com veredito `ok` => `partial.report_failed` (exit 14, M6.10) com o result completo e `scores/` intactos; veredito `partial` do backtest tem precedencia (exit 14 `partial.backtest_delivery`).
- `run_meta$cortes` usa a coluna `corte_hm` (o grep m2_no_corte_key proibe `corte =`); o ticket citava `corte`.
- Nao regressao do forecast: nenhum `.rpt_bt_*`/`prevcarga_backtest_report` e alcancavel de `prevcarga_report` (grafo `codetools::findGlobals`); o grep de metricas em `test-reporting.R` agora cobre so a parte do forecast (antes do marcador M7.4).
- Desvio: o ticket cita `prevcarga_rpt_*`/`prevcarga_report_pdf()`; as primitivas reais do M6 sao `prevcarga_html_page`/`.report_table`/`prevcarga_svg_profile`/`.pdf_page_*` e o motor PDF e `.report_pdf`; foram reusadas, nao forkadas.

## Gates de harness dos plugins (M7.5) -- `scripts/tools/gates/m7_harness.R`
- Pack descoberto por `prevcarga_gates_run` (barrel por descoberta; dono `M7`, tier fast); SUBSTITUI os slots `[DEFERRED M7]` das tabelas dos runners sem editar nenhum `models/<m>/tests/run_all_gates.R`. Mapa slot -> gate (`M7H_SLOT_MAP`, coberto por teste; nada some em silencio):
  RF `gate12` -> `gate08_ytrue_real` + `gate12_harness_long`; HW `harness_parity` -> `gate10_harness_parity`; dessem-linear `gate03_sem_crash` -> `gate03_sem_crash`; dessem-regdin `sem_crash_harness` -> `sem_crash_harness` (lgbm nao tem slot M7).
- O HP e o que `prevcarga_backtest()` produz (`$paths$members`, um `long.parquet` por celula) sobre um deck de fixture do proprio pack (SP, carga x8 para a ordem de grandeza real, efeito dia-a-dia deterministico, janela rapida de TESTE via `ctx$spec_overrides`), em `Rscript --vanilla` FILHO (os plugins buscam a fundacao no globalenv); o pai so le o veredito (JSON) e o memoiza por modelo. Nenhum runner cita `walk_forward` nem caminho fixo.
- `gate08_ytrue_real`: verdade FRESCA re-juntada pelo leitor (`prevcarga_read_actual`) por `interval_end`; `sum(!is.na) > 0`, nem tudo 0, mediana > 5000. `gate12_harness_long`: `prevcarga_schema_long_check` + tipos finos (`forecast_origin` TEXTO `yyyy-MM-dd`) + `nrow` e instantes iguais a `prevcarga_delivery_grid` por origem (um HP deslocado de 30 min mantem `nrow`, por isso a grade e comparada instante a instante).
  `gate10_harness_parity`: por origem, UMA chamada ao despachante `prevcarga_plugin_forecast` (painel de `prevcarga_infer_read_panel` com `truncate_at`) e `max|delta y_pred| < 1e-6`. `sem_crash`: veredito `ok`, 0 falhas, todas as celulas publicadas, >= 2 origens e LONG valido.
- Origem do harness por aritmetica de Date (`last - n:0`): `seq(<Date>, <Date>, by = "day")` devolve Date INTEIRO e o HW recusa (`hw.last_day_incomplete`); ver a discovery do M7.5.
- Testes: `test-harness-gates.R` (HP real de RF/HW passa; deslocado/truncado/Date reprovam o gate12, verdade NA o gate08, origem perturbada o gate10, spy == 1 chamada por origem, mapa dos slots, greps) e `test-grammar-m7-siblings.R` (CLI real: `forecast --corte/--protocol/--origin A..B` exit 8 config; `backtest --corte/--origin A..B --dry-run` exit 0; re-roda `test-reporting.R` do M6.5).

