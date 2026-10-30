# Estado: inference (M5.5) -- fachada `prevcarga_infer` + verbo `infer-model`

Codigo: `scripts/lib/inference.R`; verbo `scripts/lib/cli/infer-model.R`; catalogo `scripts/lib/errors/inference.tsv`;
testes `scripts/lib/tests/test-infer-model.R` (plugins-brinquedo com estagio de inferencia real e deck de `fixture_store.R`;
so o RF tem inferencia entre os modelos reais). Espelha a fachada de treino (`docs/estado/training.md`).

- Params 100% do artefato congelado: `artifact$spec_efetivo` (nunca run.yaml atual nem flag). O Usage do verbo nao declara
  `--horizon`/`--exogena` (docopt => exit 8). A janela e `history_days$infer` DO ARTEFATO (lida pelo leitor e reaplicada em
  `prevcarga_infer`); sem literal de janela nem switch por nome de modelo (grep no teste). A fachada nunca chama
  `plugin_train`/`prevcarga_train` e nunca escreve no registry (teste: hash da arvore igual antes/depois).
- `prevcarga_infer_resolve`: chave pelo `prevcarga_registry_key`; modo `latest` (default) | `version` (`--artifact-version`) |
  `pin` (`--pin`) via `prevcarga_resolve_version` + `prevcarga_load_artifact` (M5.2: inexistente => exit 13, malformada => exit 2,
  SHA256SUMS => exit 9). A variante e obrigatoria (`contract.variant_required`, exit 2) quando a topologia declara EXOGENA para o
  mdir OU o descriptor tem `variant_param`; `--variant` num modelo sem variante => `config.usage`. `--artifact-version` NAO dispensa
  `--variant` (a chave da celula contem a variante; sem ela a celula nao existe).
- Lacuna DL.7 (1) fechada na fachada: variante do artefato != variante do membro => `contract.variant_mismatch` (exit 2).
  O mapeamento variante do membro -> `spec$params` (DL.5) segue ESCALADO ao Gabriel: a fachada consome o que o artefato carrega.
- Cortes (`.prevcarga_inf_cuts`): linha do `dates.R` com o `data_lag_days` do descriptor (default 0), conferida (divergencia =>
  `contract.cut_mismatch`, antes de `plugin_infer`); `train_origin`/`last_data_day` por `prevcarga_train_cut` com o spec do
  artefato; `available_until` e `weather_issue_limit` da AREA vem de `panel$attrs`; 1a entrega por `prevcarga_first_delivery`.
  O artefato nao pode ter visto dado alem do teto desta origem (origin_scoped: mesma regra a partir de `artifact$origin`;
  origin_agnostic: `last_data_day` do artefato <= o desta origem). Nenhuma conta de corte/hora no arquivo.
- Resultado da celula: `(model, variant, area, origin, available_until, weather_issue_cutoff, version, resolved_by, first_target,
  split, train_origin, last_data_day, predictions[target_datetime, y_pred])`. `--out` grava parquet de 7 colunas
  (model, variant, area, origin, available_until, target_datetime, y_pred) atomicamente, recusando diretorio ausente e caminhos
  dentro de `<state_dir>/artifacts` (`io.out_unwritable`, classe bundle = exit 6). O LONG de 8 colunas e do M6.
- Erros novos em `errors/inference.tsv`: `contract.infer_spec_divergent`, `contract.variant_mismatch`, `contract.artifact_spec_missing`, `io.out_unwritable`.
- Artefato sem `spec_efetivo`/`fit` (malformado, apos SHA256SUMS ok) => `contract.artifact_spec_missing` (exit 2), nao integrity.
- INTERINO (decisao do Gabriel, 2026-10-29; F1 fica para depois da implementacao): (a) `prevcarga_plugin_infer(plugin, artifact, panel, cuts, ctx)` nao recebe `spec` (usa `plugin$spec`); a fachada
  substitui `plugin$spec` (INTERINO, removivel em 1 linha quando o despachante ganhar `spec`) pelo spec do artefato antes da chamada (pendencia F1 do despachante: canal de spec/cortes); (b)
  [resolvido pelo F4: o leitor ja entrega `valid_time`; a renomeacao da celula saiu]; (c) o handler segue a convencao
  real da fachada `handler(ctx, args)`; `--area` e `--out` ja constam no bloco Options de validate-deck/combine (duplicata torna o
  docopt ambiguo), o Usage os exige.

## M5.6 -- cortes intradiarios, alinhamento da entrega e recusas de convencao (`errors/infer_cut.tsv`; `tests/test-inference.R`)
- Cadeia da celula: resolve -> cortes -> `prevcarga_infer_check_conventions` -> (corte do artefato) -> leitor -> `prevcarga_infer_panel` ->
  `prevcarga_infer` -> `prevcarga_infer_check_alignment`. A conferencia de convencao vem ANTES do `contract.cut_mismatch` do artefato
  (origin_scoped de outra origem sai `contract.origin_mismatch`, nao cut_mismatch).
- Conferencias (tabela `.prevcarga_inf_checks`; acumulam e abortam uma vez, exit 2, id da 1a; `details` traz todas): `load_label`,
  `timezone`, `weather_issue_cutoff` (so se `descriptor$weather_forecast_in_train`) e `origin` (so origin_scoped; ADR-0023, chave = origem;
  `available_until` NUNCA e conferido: o mesmo artefato serve todas as rodadas de O). `--weather-cutoff` chega por `ctx` ao leitor e aos `cuts`.
- `available_until` e o MINIMO entre as areas do deck (M2.2): a celula le o painel com todas as areas do deck (`cuts$deck_areas`) e so depois
  SELECIONA a area (`.prevcarga_inf_select_area`), senao a area com carga mais longa veria o que passa do corte global. O painel segue CRU (30 min)
  ao despachante, que reamostra 1x por `granularity_min`/`weather_resample` (FIX 7; hora incompleta e a meia hora isolada somem no `to_hourly`
  da fundacao); `prevcarga_infer_panel` e `prevcarga_infer_cell` nunca chamam `to_hourly` (espiao em `test-inference.R`). `contract.panel_beyond_cut` se sobrar
  `interval_end > available_until` (nao corta).
- Alinhamento: `target_datetime` `identical` a `prevcarga_delivery_grid(cuts, delivery_days)` (1o = `available_until + 30`, tz da carga).
- [resolvido pelo FIX 7] o `to_hourly` da fachada migrou para o despachante. O despachante ja confere a grade
  (`plugin.output_misaligned`, exit 2) ANTES da fachada, entao `contract.output_misaligned` e a conferencia de defesa em profundidade
  (testada com o despachante substituido). Com artefato de outro `weather_issue_cutoff` em modelo SEM clima no treino, o envelope do despachante
  (`weather_issue_cutoff diferente de cuts`) ainda recusa; a fachada so confere o corte quando o descriptor declara clima previsto no treino.
- Erros em `errors/infer_cut.tsv`: os 6 ids `contract.*` classe 2. Testes FORBIDDEN/CONTROL em `scripts/lib/tests/test-inference.R`.
