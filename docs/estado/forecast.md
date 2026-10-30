# Estado: forecast (M6.4) -- rodada operacional diaria

- `scripts/lib/forecast.R`: `prevcarga_forecast(ctx, origin = NULL, areas = NULL, niveis = "subsistemas", report = FALSE,
  report_format = "html", skip_validate = FALSE, incluir_membros = FALSE)` -> `list(result, warnings)`. So COMPOE: nenhum calculo de
  k-hat/BU/SIN, de pesos, de corte de treino nem de hora (greps: sem `khat_fit`, `reconcile_train`, `combina_fit`, `registry_promote`,
  `artifacts/reconcile`, `Sys.time`, `Sys.Date`, `now(`, `Sys.getenv`). A rodada diaria e UMA origem O.
- Ordem invariante: (0) `niveis` valido (exit 8 `config.usage`, antes de qualquer trabalho) -> (1) gate-0 -> (2) plano -> (3) membros ->
  (4) LONG por membro + combina por folha -> (5) reconcile UMA vez, em memoria -> (6) versao/chave imutavel -> scratch + `run.json` -> export.
  Reconcile (exit 15/13/2) acontece ANTES de qualquer scratch ou diretorio de entrega: nada de `forecasts/` nem `scratch/` quando falha.
- Gate-0 (`.forecast_gate0`): origem = argumento > `ctx$origin` (flag) > dia do ultimo intervalo de carga do deck (`prevcarga_resolve_origin`, dates.R;
  nenhum relogio). `prevcarga_validate_deck` reprovado vira o abort `<classe>.<what>` do 1o erro da classe dominante (a mesma regra do verbo
  validate-deck; exit 3 no buraco do dia O). `skip_validate` so pula a validacao: os cortes (`available_until` minimo entre areas, regra de clima,
  fuso, load_label) vem SEMPRE de `prevcarga_resolve_cuts` (leitor): um buraco no dia O continua abortando exit 3 (`completude.origin_day_gap`).
- Plano (`.forecast_plan`): folhas pedidas (NULL = todas; fora da topologia => `topology.area_undeclared`, exit 4, antes do infer). `closed/stray/sin_closed`
  vem do `prevcarga_reconcile_plan`; `covered` = folhas pedidas que fecham algum subsistema (o plano atual nao o devolve: derivado em forecast.R).
- Membros (`.forecast_members`): por folha x linha de `prevcarga_members()` (sigla/mdir/exogena). origin_agnostic so le o artefato (`prevcarga_infer_cell`;
  ausente => `state.artifact_missing`, exit 13, nunca treina). origin_scoped: `.forecast_scoped_version` acha o artefato da origem O pelo `origin` do
  `run_manifest.json` de cada versao do registry (proprio ou oficial sincronizado: mesmo layout; `latest` se for dele, senao a ultima em ordem radix);
  sem artefato de O => `prevcarga_train(member, area, O, ctx)` UMA vez (a fachada deriva `train_origin = O` e `last_data_day = O-1` pelo dates.R);
  deck novo do mesmo O (available_until maior) reutiliza o artefato (mesmo `v_<hash>`) e so o infer muda. O infer recebe `version =` explicita.
- LONG e combina (`.forecast_leaf_panel`/`.forecast_leaves`): `prevcarga_build_long` por membro (faixa por `delivery_days` do descriptor) -> proveniencia em
  memoria no formato do run-manifest da combina -> `prevcarga_assert_members_aligned` -> `prevcarga_build_panel` -> `prevcarga_combina_apply(detail = TRUE)`
  com os pesos congelados de `<state_dir>/artifacts/combina/<area>/` (ausente => `state.combina_not_trained`, exit 13; sem corte da combina para o HHMM de
  `available_until` => `contract.combina_input`). Por lead so combinam os membros que cobrem o lead (M4.2c).
- Reconcile: `prevcarga_reconcile_apply(long_leaves[covered], ctx, subsystems = closed, khat_version = NULL, eval = FALSE)` uma vez; folhas soltas nunca passam por ele.
  `result$khat` = `[{ss, versao, khat_origin (texto), khat_age_days}]` (do reconcile) e `result$cobertura_descartada` identical ao do reconcile.
- Versao e chave: `prevcarga_forecast_versao` com `package = ctx$version` (como o M6.7 fixa; o `ctx$code_commit` so entra no run.json), `members` = por sigla x folha
  `<v_hash do artefato>:<sha256 do y_pred entregue>`, `combina_weights` = versao dos pesos por folha, `khat` = versao do perfil por subsistema,
  hash canonico da topologia efetiva, load_label e regra de clima. O hash do y_pred faz o mesmo artefato com deck/clima novo (mesmo `available_until`) virar OUTRA
  chave em vez de `export.key_conflict`. `run_id` = hash da chave (M6.7).
- Scratch `<state_dir>/scratch/<run_id>/` recriado SEMPRE: subsistemas/SIN pelo escritor do reconcile (`prevcarga_reconcile_write`), folhas (soltas sempre; cobertas so
  com `niveis = tudo`) e membros (so com `incluir_membros`) pelo mesmo escritor deterministico do LONG (`.reconcile_write_parquet`); o `run.json` registra EM `arquivos[]`
  {path, area, linhas, sha256} TODO arquivo entregue (folhas, membros, subsistemas e SIN), senao o export os omitiria com `completude.export_file_missing`.
  `runtime_id` = R + plataforma (deterministico); `generated_at` vem do ctx (unico campo nao deterministico do manifest).
- Avisos: `warnings` = avisos do catalogo capturados (gate-0 `warning.<what>`, `reconcile.*`, `completude.export_file_missing`) como `list(id, message)`; o verbo (M6.9) os emite.
- Report (M6.5, `scripts/lib/reporting.R`, errors/reporting.tsv `contract.report_input`): `report = TRUE` roda `.forecast_report_step` DEPOIS do export; `result$report = {status "ok", path}` com
  `path = <state_dir>/reports/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/report.html` (fora de `forecasts/`; a entrega e identica com e sem report; falha do report: ver M6.10 abaixo; nunca
  derruba a entrega). Primitivas reusaveis pelo M7: `prevcarga_html_escape` (escapador unico), `prevcarga_html_page`, `prevcarga_html_heatmap`, `prevcarga_svg_profile`; sem script/Plotly/fonte externa, UTF-8+LF,
  sem relogio (byte-identico para o mesmo insumo). Blocos `curva` (dia O por SIN/subsistema/folha), `available-until` (global e por area), `pesos`, `khat` (copia o result do reconcile: versao, khat_origin,
  khat_age_days), `baselines` (diferenca media do previsto vs persistencia O-1 e sazonal O-7; `prevcarga_baseline` le carga <= O 00:00 <= available_until) e `proveniencia`. Nunca le a verdade do painel nem
  emite metrica de acerto. Pesos = media por membro dos pesos congelados do corte da rodada (a combina nao tem intercepto: 0). `format = "pdf"`: ver M6.10 abaixo.
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
  CONTROL dia O completo => recusa) e `[e2e 00h]` em test-forecast_e2e.R (8 membros reais, fases weights00/run00). PENDENTE [DEFERRED DL/RD]: no e2e das 00h os DESSEM abortam
  `plugin.dessem_linear_mm_missing_origin` (a cabeca mm exige linhas do dia O); a cabeca mm deve ser opcional quando nao ha carga de O. O teste se declara `skip` com esse id ate la.
- Chave imutavel x `niveis`/`incluir_membros` (achado do M6.4): a `versao` do M6.7 nao inclui o conjunto de arquivos; a mesma chave com outro `niveis`/`incluir_membros` da `export.key_conflict`
  (exit 9). Opcoes: incluir niveis/incluir_membros ordenados na versao, ou o export aceitar superconjunto e publicar so os faltantes. Nao tratado aqui (exports.R e do M6.7).
- Limitacoes conhecidas (nao tratadas aqui): (1) o verbo `forecast` e a gramatica docopt sao do M6.9 (hoje stub): o E2E chama a funcao publica no processo-filho; (2) a combina identifica o corte pelo
  HHMM de `available_until` DENTRO do dia da origem: dia O completo ou `--origin` anterior ao ultimo dia do deck e recusado (ver acima); (3) `niveis`/`incluir_membros` nao entram na `versao` (M6.7): a mesma chave com outro conjunto de arquivos e `export.key_conflict`.
