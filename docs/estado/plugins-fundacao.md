# Estado: fundacao de plugins (epico P.0) -- estado duravel unico

Fecha a fundacao (P.0a..P.0j), nao o porte: cobertura dos 8 membros, higiene global de `models/`, isolamento conjunto e oraculo intacto sao do M5.
`docs/ESTADO-ATUAL.md` e `scripts/lib/README.md` sao editados so pelo M1.V; `docs/roadmap/PLANO.md` por ninguem. Detalhes por ticket ficam nos
arquivos `docs/estado/plugin_dispatch.md`, `plugin_tools_*.md` e `features_facade.md`; este arquivo guarda os contratos vivos.

## Fundacao de plugins

### Descriptor (`registry.R`, `.prevcarga_plugin_normalize_descriptor`)
- Campos aceitos (`.prevcarga_plugin_fields`): `model_id`, `contract_version`, `granularity_supported`, `modes_implemented`, `params_schema`, `variant_param`,
  `native_layout`, `semihourly_rule`, `delivery_days`, `data_lag_days`, `model_time_offset_min`, `reads_day_O`, `weather_forecast_in_train`, `uses_rng`,
  `history_infer_equals_train`, `rng_deviation`, `packages`, `attach`, `engine`, `freeze`, `thaw`, `stages`. Chave fora da lista => `plugin.unknown_key`.
- Defaults: `data_lag_days = 0L` (LGBM declara 2), `model_time_offset_min = 0L` (DESSEM declara 60), `reads_day_O`, `weather_forecast_in_train`, `uses_rng`,
  `history_infer_equals_train` = `FALSE`; `rng_deviation` so com `uses_rng = TRUE`; `delivery_days` obrigatorio (>= 1).
- Assinaturas: `prevcarga_plugin_signatures` e a fonte unica dos formais de cada etapa (`ingestion`, `feature_engineering`, `training`, `inference_features`,
  `inference`, `forecast`) e de `freeze`/`thaw`; `params` so em `feature_engineering` e `training`. Fora da forma => `plugin.bad_signature` no load.

### Cuts e regra de dados
- `cuts` (de `prevcarga_resolve_cuts`): `forecast_origin`, `origin_day`, `split`, `tz`, `load_label`, `weather_issue_cutoff`, `available_until`, `weather_issue_limit`,
  `train_origin`, `last_data_day`. Regra: `last_data_day = O-1-data_lag_days` (LGBM, lag 2 => O-3); `origin_agnostic` `train_origin = O-1-H`; `origin_scoped` `train_origin = O`.
- Regra do dia O: so um plugin com `reads_day_O = TRUE` le o dia O; o despachante confere (`plugin.cuts_mismatch`, `plugin.train_beyond_cut`, `plugin.infer_beyond_cut`,
  `plugin.weather_beyond_limit`, `plugin.raw_timestamp`, `plugin.missing_column`) e nunca corta nem preenche.

### Loader e despachantes
- Loader (`prevcarga_plugin_load`): env isolado por plugin, motor (`descriptor$engine`) por instancia, registro Tier A, varredura de pureza do artefato.
- Despachantes `prevcarga_plugin_train/_infer/_forecast`: composicao identica para todos, sem switch por nome de modelo. Unico ponto de ajuste de hora de modelagem:
  `to_model_frame` na entrada e `to_display_frame` na saida, com `descriptor$model_time_offset_min` (ADR-0012/0019/0035). Nenhum plugin, motor ou adapter faz aritmetica de hora.
- Gate `pc_grep_time_arith(model_dir, allow = pc_time_arith_allow())` (`scripts/lib/tests/helper-plugins.R`): nenhuma aritmetica de hora em `models/<m>/R/*.R` (nem no motor);
  excecao fechada e versionada em `pc_time_arith_allow()`: so o `+ hours(6)` do flag de horario de verao do HW (o motor DESSEM NAO tem excecao desde o DL.3/DL.4: lag por indice, `allow = list()`). Cada
  `run_all_gates.R` de modelo roda o gate sobre o seu `models/<m>/`; o M5.11 faz a varredura global. Reuso geral: `pc_grep_code(paths, patterns, allow)`, `pc_bytes_over_127`, `pc_foundation_forbidden`.

### Envelope e grade de entrega
- O plugin devolve `area`, `values`, `delivery` (+ `meta`); o despachante completa `model_id`, `variant`, `origin_day`, `available_until`, `weather_issue_cutoff`, `layout`,
  `granularity_min` e copia `rng_deviation` para `meta`. `prevcarga_delivery_grid(cuts, delivery_days)` (`features.R`): `interval_end` de `available_until + 30` ate 00:00 de O + `delivery_days`;
  faixa O..O+8 (9 dias) ou O..O+1 (2 dias). Desalinhado => `plugin.output_misaligned`.
- Artefato carimbado (`prevcarga_plugin_stamp`): `model_id`, `variant`, `split`, `granularity_min`, `load_label`, `timezone`, `weather_issue_cutoff`, `params`, `seed`, `contract_version`,
  `train_origin` (+ `origin_day` se origin_scoped); sem `available_until` e sem relogio/host (`plugin.impure_artifact`).

### RNG
- `.prevcarga_plugin_guarded` e o unico ponto que toca `globalenv()`; `prevcarga_preserve_rng` restaura `.Random.seed` inclusive em erro. `uses_rng = FALSE` com consumo do gerador =>
  `plugin.rng` (classe repro); `uses_rng = TRUE` declara `rng_deviation` (texto), copiado ao envelope.

### Ferramentas de paridade (dev-only, fora do bundle)
- `units.yaml`: `units: [{id, area, origin, available_until, weather_issue_cutoff, day0_source, variant?, history_days?, oracle_env?, oracle_cut?}]` + `tz` e `packages`.
  `oracle_env` chega ao filho so pelo argumento `env` do `system2`; `oracle_cut: start_label` => `--available-until = available_until - 30 min`; `day0_source`: `native`,
  `oracle_origin_minus_1` ou `oracle_discarded_step`. `golden.lock` (versionado, em `models/<m>/tests/parity/`) guarda sha256, `oracle_commit`, `data_digest` e fingerprint; o golden (`/.state/`) nunca e versionado.
- Runner `prevcarga_gates_run(gate_table, tier, model)` (`scripts/tools/plugin_gates.R`): sub-gates por descoberta em `scripts/tools/gates/<dono>.R`
  (`prevcarga_gate_pack_<dono>(model, ctx, tier)` + `applies_to`); o runner do modelo nunca e editado por fora. Limiar de paridade unico `1e-5` MW (ADR-0017).
- Ids genericos `plugin.*` (e `features.*`, `tools.*`) em `scripts/lib/errors/plugin.tsv`, usados pelos plugins com `details{model}`.

## Modulos e ferramentas
| Arquivo | Papel |
|---|---|
| `scripts/lib/features.R` | transforms de grade (`to_hourly`, `to_semihourly`, `to_model_frame`, `to_display_frame`), `prevcarga_delivery_grid`; M5.1 acrescentou a fachada de FE (`docs/estado/features_facade.md`) |
| `scripts/lib/registry.R` (parte de plugins) | loader, descriptor, despachantes, envelope, guarda global/RNG, contrato de artefato carimbado; Tier A e registro de artefatos no mesmo arquivo |
| `scripts/lib/errors/plugin.tsv` | catalogo da fundacao (`plugin.*`, `features.*`, `tools.*`) |
| `scripts/tools/fixture_store.R` | deck sintetico deterministico (P.0e) |
| `scripts/tools/plugin_golden.R` | gerador de golden do oraculo + `golden.lock` (P.0f) |
| `scripts/tools/plugin_parity.R` | comparador de paridade por layout (P.0g) |
| `scripts/tools/plugin_gates.R` | runner comum de gates + agregador `--models` (P.0h) |
| `scripts/tools/gates/<dono>.R` | packs de sub-gates por descoberta (m1..m4 sao scripts autonomos, ignorados pelo loader) |
| `scripts/lib/tests/fixtures/plugins/{toy-30,toy-60,_toy-engine}` | brinquedos da fundacao; `helper-plugins.R` (fixtures + `pc_grep_code`/gate de hora) |
| `scripts/lib/tests/test-plugin-hygiene.R` | higiene da fundacao (P.0j): padroes proibidos, gate de hora, suite fast completa, write-through |

`.gitignore` contem `/.state/` (golden regeneravel).

## Pendencias conhecidas (nao tratadas aqui; aguardam decisao)
- F1: workarounds de plugin (`cuts$spec`, `attr(frame, "cuts")`, `plugin$spec`, placeholder MED); F4: adaptador `timestamp -> valid_time` (P.0i/P.0g/leitor).
- Listas centrais de layout: `prevcarga_native_layouts` (`registry.R`) e `prevcarga_parity_layouts` (`plugin_parity.R`); classe de aviso `ok` rejeitada por `errors.R`.
- Higiene P.0j (corrigida no proprio ticket, direcao do team-lead sobre a discovery 01M64E36N66SP746XG73M6AB85):
  - Escopo: `source()`/`library()` proibidos so em `scripts/lib` (AC1, "nos modulos"); as ferramentas dev de `scripts/tools` carregam a lib por `source()` por desenho
    (`pc_foundation_forbidden_tools` = tabela sem `global`; escopo, nao allow-list).
  - `plugin_golden.R`: `setwd` -> `withr::with_dir`; `<<-` -> ids pre-computados; `fixture_store.R`: `lubridate::as_date(character())`.
  - PENDENTE: `plugin_parity.R` usa `difftime()` (linhas 106 e 157). O RF.4 edita o arquivo; o fix e do P.0g pos-RF.4. A assercao existe em
    `test-plugin-hygiene.R` ("PENDENTE", `skip` com motivo) e liga quando o fix entrar; o resto dos padroes ja e varrido em `plugin_parity.R`.
  - Testes: `withr::local_seed` em `test-features.R` e `test-plugin-dispatch.R` (`.Random.seed` restaurado); `test-plugin-tools-fixture.R` gera `run.yaml` sinteticos em tempdir;
    `test-plugin-tools-gates.R` varre so os runners de tempdir; `test-features-facade.R` nao carrega plugins reais (`real_plugins()` vazio; cobertura dos membros e do M5).
