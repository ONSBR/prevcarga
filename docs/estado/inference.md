# Estado: inference (M5.5) -- fachada `prevcarga_infer` + verbo `infer-model`

Codigo: `scripts/lib/inference.R`; verbo `scripts/lib/cli/infer-model.R`; catalogo `scripts/lib/errors/inference.tsv`;
testes `scripts/lib/tests/test-infer-model.R` (plugins-brinquedo com estagio de inferencia real e deck de `fixture_store.R`;
fixtures sinteticas; os modelos reais sao exercidos pelos testes de cada plugin e pelo forecast). Espelha a fachada de treino (`docs/estado/training.md`).

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
- Clima previsto (E2-T2, sobre fix/facade-channels): o painel de infer troca o previsto por `prevcarga_read_forecast_by_rule` (deck_reader.R) para TODO membro cuja
  `inference_needs` declara segmentos (a regra do plugin: RF `per_origin`, LGBM `min_lead`, DESSEM `freshest`), entregando so as emissoes que o plugin de fato escolhe
  (`<= weather_issue_limit`); sem segmentos (HW) o painel do leitor fica como esta (mais fresca por `(area, valid_time)`). Testes: `test-forecast-by-rule.R`, `test-facade-members.R`.
- [resolvido pelo FIX 7] o `to_hourly` da fachada migrou para o despachante. O despachante ja confere a grade
  (`plugin.output_misaligned`, exit 2) ANTES da fachada, entao `contract.output_misaligned` e a conferencia de defesa em profundidade
  (testada com o despachante substituido). Com artefato de outro `weather_issue_cutoff` em modelo SEM clima no treino, o envelope do despachante
  (`weather_issue_cutoff diferente de cuts`) ainda recusa; a fachada so confere o corte quando o descriptor declara clima previsto no treino.
- Erros em `errors/infer_cut.tsv`: os 6 ids `contract.*` classe 2. Testes FORBIDDEN/CONTROL em `scripts/lib/tests/test-inference.R`.

## Horizonte efetivo pelo clima previsto (F3.7)
- Descriptor ganha `weather_cut` ("truncate" default | "absent"); RF (tmax/tmin sobre 9 dias fixos, features intocadas) e os dois DESSEM (so horizonte inteiro, por ora) declaram `absent`; LGBM e HW ficam `truncate`.
- `.prevcarga_inf_weather_dependent(plugin, variant)`: algum estagio com `requires` EFETIVO da variante (`prevcarga_plugin_variant_requires`) de prefixo `data.weather_forecast.` (HW univariado e variante sem clima: nao => nunca cortado). A variante vem de `.prevcarga_inf_variant(member)` em `prevcarga_infer_cell` e e repassada por `.prevcarga_inf_apply_weather_cut` e `.prevcarga_inf_effective_days`; NA = requires do estagio (comportamento anterior).
- `.prevcarga_inf_effective_days(plugin, attrs, area, origin, variant)` = `min(delivery_days, until - O + 1)`, menos 1 dia quando `model_time_offset_min > 0` e a emissao nao traz os
  `offset/30` primeiros instantes do dia seguinte ao ultimo coberto (`attrs$weather_forecast_next`). Sem dia O coberto => 0.
- `.prevcarga_inf_apply_weather_cut`: efetivo < declarado e (`weather_cut = "absent"` ou efetivo < 1) => `completude.weather_forecast_short` ANTES de qualquer estagio;
  senao `warning.forecast_weather_horizon_cut` (`details$ate`, `membro`). Grava `cuts$delivery_days` e `cuts$weather_forecast_until`; o despachante do registry e
  `prevcarga_infer_check_alignment` usam `cuts$delivery_days` quando presente (plugin so le; nenhuma aritmetica de data nova).
- So o LGBM (`.lgbm_delivery_grid`) le `cuts$delivery_days`; os DESSEM recusam em vez de cortar (ver `dessem_engine.md`).

## inference_needs -- necessidade de dados por membro como dado (E2-T1; `scripts/lib/inference_needs.R`; `errors/deck_inference.tsv`; `tests/test-inference-needs.R`)
- Campo OPCIONAL do descriptor `data_needs = function(spec, area)` (puro; assinatura conferida por `.prevcarga_plugin_check_signature` com `c("spec", "area")`; nao-funcao => `plugin.bad_descriptor` field `data_needs`; assinatura errada => `plugin.bad_signature` stage `data_needs`; ausente => `out$data_needs` NULL). Devolve `list(infer = <forma v1>)`.
- Forma `schema_version = 1L` (lista fechada; offsets inteiros relativos a O, nenhuma data absoluta): `load$days_before`, `weather_observed$days_before`, `weather_forecast = list(area, segments)` com segmentos `kind` em `freshest|per_origin|min_lead`, `from`, `to`, `lead` (so em `min_lead`), `holidays = list(from, to)`, `holiday_annex` (logico). `prevcarga_inference_needs_normalize(x, where)` converte para integer, ordena os segmentos por (kind, from, to, lead) com radix, deduplica e devolve sempre a mesma forma (sem previsto: `segments = list()`). Qualquer desvio (chave desconhecida, offset nao inteiro, kind fora do enum, from > to, lead ausente em `min_lead`, area de clima vazia) => `contract.inference_needs_invalid` (exit 2; `where` = modelo, `details$field`).
- Default GENERICO (sem `data_needs`), so por campos: carga/observado = `spec$history_days$infer`; previsto = um segmento `freshest [0, horizon_days]` na propria area quando algum `requires` de estagio comeca por `data.weather_forecast.` (mesmo predicado de `.prevcarga_inf_weather_dependent`); feriados `[-infer, +horizon_days]`; anexo FALSE. Sempre passa pelo normalize.
- `prevcarga_inference_needs_aggregate(rows)`: uniao por dataset e area (max de `days_before`, uniao deduplicada e ordenada radix dos segmentos por area de clima, feriados from=min/to=max, OR do anexo); independe da ordem dos membros. `prevcarga_inference_needs_resolve(ctx, origin, areas, source = "artifacts"|"topology")`: uma linha por membro x area (`member, mdir, area, needs, source`, ordem (area, member) radix); `artifacts` le o artefato por `prevcarga_infer_resolve` e exige `inference_needs` (ausente => `contract.inference_needs_missing`, exit 2, `details` member/area/origin: retreinar); `topology` aplica `prevcarga_inference_needs_of` com `prevcarga_model_spec`, sem ler artefato.
- O catalogo `deck_inference.tsv` tambem guarda `completude.need_gap` (exit 3), usado pela validacao do deck de inferencia (ticket seguinte).
- Retreino: artefatos gravados antes deste campo nao tem `inference_needs`; para a rodada com `--data-inference` e preciso retreinar os membros/areas.

## E3-T2 -- `descriptor$history_window` na fachada de inferencia (regdin-linear-refactor; ADR-046)
- `prevcarga_infer_read_panel`: com `descriptor$history_window` (funcao), `w <- history_window(origin, spec)`; `history_days` do leitor = `origin - w$common_from` e `holiday_window = w`. Sem o campo, `history_days$infer` como antes.
- `.prevcarga_inf_select_area` seleciona a area tambem em `panel$holiday_annex$islands/load/weather_observed`, preservando atributos; `prevcarga_infer_panel` confere `holiday_annex$load$interval_end` contra `available_until` (`contract.panel_beyond_cut`).
- A regra de contagem de `window.history_short` e do treino (`training.md`): so dias comuns. O deck de inferencia aponta o anexo por `manifest$holiday_annex` (`deck_reader.md`, E3-T2).
