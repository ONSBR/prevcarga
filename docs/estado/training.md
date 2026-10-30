# Estado: training (M5.3) -- fachada `prevcarga_train`

Codigo: `scripts/lib/training.R`; catalogo `scripts/lib/errors/training.tsv`; spec efetivo em `topology.R`; testes `scripts/lib/tests/test-training.R`
(plugins-brinquedo toy-30/toy-60 sobre o deck de `fixture_store.R`; os modelos reais ainda nao tem todas as etapas).

- `prevcarga_model_spec(model, root, overrides = list())`: run.yaml + overrides pontilhados (`horizon_days`, `params.<k>`) so sobre chaves DECLARADAS.
  Chave inexistente => `config.override_unknown` (exit 8); tipo diferente do declarado ou `horizon_days` fora de inteiro >= 1 =>
  `config.override_bad_value` (exit 8). Constraints reaplicadas apos o override. Lista devolvida ordenada por nome.
- `prevcarga_train_inputs(member, area, origin, ctx)`: plugin (`prevcarga_plugin_load(mdir, <root>/models)` + `prevcarga_plugin_check_signature`), spec efetivo
  (`ctx$spec_overrides` + variante do membro na chave `descriptor$variant_param`), cortes por `prevcarga_resolve_cuts(ctx, origin, model, data_lag_days = descriptor)` e
  `prevcarga_train_cut(spec EFETIVO, ...)` (o `resolve_cuts` le o run.yaml sem overrides; so o `train_origin` com H efetivo vem do `prevcarga_train_cut`). Linha de corte com
  `data_lag_days` != descriptor => `contract.cut_mismatch` (exit 2). O M5 nao soma/subtrai dias de corte (teste varre o corpo de `training.R`).
- Painel (W1): janela de treino = a que o plugin confere. origin_agnostic: `[train_origin - history_days.train, last_data_day]` (history_days.train + H + 1 -
  data_lag_days dias; o RF exige o 1o dia `train_origin - 365`, senao `plugin.window`); origin_scoped: `history_days.train` dias ate `last_data_day` (inalterado).
  Cortes so de `prevcarga_resolve_cuts`/`prevcarga_train_cut`; a fachada so mede a janela a partir deles. `prevcarga_read_panel` le a janela + 1 dia; carga e clima
  observado cortados nela; o clima observado chega do leitor com `valid_time` (F4; sem adaptador na fachada); plugin com `weather_forecast_in_train` recebe todas as emissoes `<= weather_issue_limit` (nao so a mais fresca) (INTERINO, pendente da decisao do Gabriel sobre emissoes historicas do painel).
  Dias distintos de carga < dias da janela => `window.history_short` (exit 5; `details`: `available_days`, `required_days` = dias da janela, `member`), antes de qualquer escrita.
  O painel vai CRU (30 min) ao despachante, que reamostra por `granularity_min`/`weather_resample` (FIX 7); a fachada nunca chama `to_hourly`. O mapeamento area -> serie de clima (ex.: LGBM SECO -> SP) e do plugin: o painel traz o clima da propria area.
- Identidade: `prevcarga_train_fingerprint` preenche as 8 chaves do contrato de `provenance.R`: `model_id` = `member$mdir`; `data_fingerprint` = sha256 do CONTEUDO do painel ja
  cortado (carga, clima observado/previsto, feriados; `attrs` e `available_until` fora) -- NAO e o `prevcarga_data_hash` (esse hasheia os arquivos do deck e mudaria com o dia O);
  `cutoff` = origin_scoped `{origin}` | origin_agnostic `{train_origin, last_data_day}`; `feature_config` = `{spec (name, split, granularity_min, horizon_days, history_days, seed, params),
  exogena}`; `code_commit`/`renv_lock_hash` de `ctx` (ausentes => calculados); `load_label` e `load_tz` efetivos. Laterais (`trained_at`, `run_id`, `version`, plataforma) so no run_manifest.
- Artefato publicado (classe `prevcarga_train_artifact`, dado puro): `model_id, variant, area, split, spec_efetivo, cuts{train_origin,last_data_day}, load_label, timezone,
  weather_issue_cutoff, fit` (= artefato carimbado do despachante, serve a `prevcarga_plugin_infer`), `contract`; `origin` so no origin_scoped; nunca `available_until`.
  `run_manifest.json` sem `available_until`, com `weather_issue_cutoff` efetivo e o `fingerprint` canonico.
- `prevcarga_train(member, area, origin, ctx, force)`: `v_<hash>` existente e `!force` => `status = "reused"` (plugin nao chamado, pasta intocada); senao
  `prevcarga_plugin_train` -> `prevcarga_registry_publish` -> `prevcarga_registry_promote_latest` (M5.2). Devolve 1 linha
  `(model, variant, area, split, train_origin, last_data_day, version, status, path)`.
- Gate `registry_lifecycle` (`test-static-greps.R`): allow-list segue so `registry.R`; o padrao foi refinado para casar DEFINICAO/uso nu de promote/rollback/gc (e `prevcarga_registry_* <- function`) e poupar CHAMADAS da API (`prevcarga_registry_promote_latest(...)`), com CONTROLs. `training.R` so chama o registry.
- Testes W1: `toy-rfw` (H=9 origin_agnostic, ingestao que confere a janela como o RF) treina pela fachada (falha com a janela antiga), CONTROL origin_scoped com 10 dias;
  deck dos testes com 21 dias. Smoke do random-forest REAL por `prevcarga_train` NAO e viavel antes do F1: o estagio `rf_stage_ingest` exige `cuts$spec` (que a fachada
  nao repassa: canal de spec aos estagios e decisao do F1) e o deck precisaria de 375 dias (history_days.train 365 nao tem override).
- Pendencia F1 (despachante): os estagios continuam com formals fixos; esta fachada NAO escolheu canal para repassar spec/cortes aos estagios (params e seed ja chegam por `spec$params`/`spec$seed`).

## M5.4 -- verbo `train-models` (`scripts/lib/cli/train-models.R`; logica em `training.R`; catalogo `errors/train_models.tsv`; testes `test-train-models.R`)
- Gramatica: `train-models [--model=<lista>] [--area=<a> | --subsistema=<s> | --areas=<a,b>] [--origin] [--horizon=<h>] [--exogena=<e>] [--dry-run] [--jobs=<n>] [--force]`
  (+ globais). Handler `cmd_train_models(ctx, args)` (convencao da fachada). `--model/--area/--origin` ja sao flags globais/de outro verbo (nao redeclarados).
- `prevcarga_resolve_areas(area, subsistema, areas, ctx)` (em `training.R`, nao em `topology.R`: o ticket o listava la, mas `topology.R` e arquivo disputado por outros
  tickets; mover e trivial): >1 seletor => `contract.area_selector_conflict` (exit 2); fora da topologia => `topology.area_undeclared` (exit 4). `--area`/`--areas` aceitam folha ou no de
  subsistema (a chave da celula e o proprio nome); `--subsistema S` expande para as folhas-filhas; nenhum seletor = todas as folhas. Seletores e `--model` sao validados antes de ler o deck.
- `--model`: lista de `mdir` ou `mdir/exogena`; `--exogena`: filtro de variante (nao override de param). `--horizon` = override `horizon_days` (so chave declarada; M5.3).
- Matriz `prevcarga_train_matrix` ordenada por (model, variant, area) radix; `--dry-run` = `prevcarga_train_plan` (le o painel como o treino, calcula cortes e `v_<hash>`, status `planned`, nada gravado;
  a versao-alvo e identica a do treino real). Result: `{schema_version, dry_run, origin, available_until, load_label, weather_issue_cutoff, cells[]}`; `variant` NA => `null`.
- `prevcarga_train_models(cells, ctx, origin, jobs, force)`: serial em ordem (1o erro aborta) ou `parallel::makePSOCKcluster` (sem fork). `prevcarga_worker_init` (R novo): carrega a lib
  (`ctx$lib_dir`/`prevcarga.lib`), roda `prevcarga_runtime_enforce()` in-process e devolve dado (nunca levanta); falha => o pai re-levanta o id original (`runtime.*` => exit 7) ou
  `runtime.worker_unfixable`. Celulas devolvem `prevcarga_cell_error` (id/where/details) re-levantado com a classe original; resultado reordenado pela chave. Semente: spec$seed aplicada pelo plugin no worker.
  Hooks de teste so por `ctx$worker_hooks` (`pre_runtime`/`post_runtime`), nunca por env; a CLI nao os expoe.
- INTERINO / lacuna conhecida (nao decidida aqui): o RF real (RF.1) nao treina via `prevcarga_train`: (1) o estagio de ingestao le `cuts$spec`/`cuts$phase`, que o despachante nao repassa (F1);
  (2) a janela de carga do facade e `history_days.train` dias terminando em `last_data_day`, mas o RF exige `[train_origin - history_days.train, last_data_day]` (history_days + H dias). Os testes
  usam o plugin-brinquedo `toy-rng` (toy-30 origin_agnostic que consome o RNG com o seed do spec).
- ABERTO (decidido no M5.4, follow-up contra o M5.3): a correcao da janela de carga do facade (history_days.train + H, origin_agnostic; ajustar window.history_short) e a prova
  ponta a ponta com o random-forest REAL via `train-models` continuam em aberto; as provas dos ACs 1/4/5 usam o plugin-brinquedo `toy-rng` (interino aceito). O canal cuts$spec/phase segue sob a F1 escalada.
