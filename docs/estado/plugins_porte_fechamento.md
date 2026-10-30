# Fechamento do porte (1/2) -- M5.11 (ex-P.6): cobertura dos 8 membros, higiene e isolamento/portabilidade conjuntos

Verificacao transversal dos 5 plugins (random-forest, lgbm, holt-winters, dessem-linear, dessem-regdin) + motor `_dessem-engine`.
So VERIFICA: nenhum plugin, motor, golden ou fachada foi alterado; achado vira correcao no epico dono (ver "Achados").

## Arquivos
- `scripts/lib/tests/helper-porte-closure.R` -- helpers `pcc_*` (nao edita o `helper-plugins.R` da fundacao; so o usa).
- `scripts/lib/tests/test-members-coverage.R` (AC38) -- matriz dos 8 membros + CONTROLs de topologia + grade de entrega.
- `scripts/lib/tests/test-plugins-hygiene.R` (AC39) -- tabela `PCC_HYGIENE` (19 linhas) + CONTROL plantado por linha.
- `scripts/lib/tests/test-plugins-isolation.R` (AC40, AC41) -- sessao limpa, sementes, globalenv, freeze/thaw/ENV sujo.
- Comandos: `Rscript --vanilla -e 'testthat::test_dir("scripts/lib/tests", filter="members-coverage|plugins-hygiene|plugins-isolation", stop_on_failure=TRUE)'`
  (rodar cada filtro em separado: coverage ~4 min, isolation ~6 min, hygiene ~2 s; 1 processo, 1 thread).

## Desenho
- Os cenarios pesados (treino/inferencia pelas fachadas `prevcarga_train` / `prevcarga_infer_cell`) rodam num `Rscript --vanilla`
  FILHO limpo (`pcc_run_child`, saida JSON em tempdir): a lib precisa estar no `globalenv()` (interino dos plugins) e o processo do
  testthat nao deve sujar. `pcc_child_*` sao as funcoes que o filho executa.
- Topologia-fixture: `pcc_topology_fixture(edit)` copia `config/` para tempdir, edita `topology.yaml` (membro `(dessem-regdin, MXMN)` ou
  `mdir: nao-existe`) e devolve o `config.yaml` copiado (o que se passaria por `--config`; a topologia mora ao lado). Nunca env.
  `pcc_check_members` (filho) levanta `contract.topology_member_invalid` com `where` citando `exogena=MXMN` / `models/nao-existe/run.yaml`
  (exit 2; o catalogo so tem a mensagem generica, o detalhe vai em `where`).
- `pcc_all_plugins()` = `unique(prevcarga_members()$mdir)` (nunca lista fixa). `pcc_models_copy()` copia so `R/`, `run.yaml` e `README.md`.
- Higiene: escopo `models/*/R` + `models/_dessem-engine/R` (+ `run.yaml`); `*.md` nunca e varrido; linha so-comentario nao conta.
  `globalenv()` esta PINADO por diretorio (motor 1, dessem-linear 3, dessem-regdin 3; todos `get0(..., envir = globalenv())`): hit novo reprova.
  Aritmetica de hora: zero hit com `allow = list()` em todo plugin e no motor; as excecoes da lista versionada `pc_time_arith_allow()`
  (hoje: `holt_winters.R`, nenhum arquivo com esse nome existe mais) nao sao usadas, e o teste so as consome (sem lista paralela).
  O CONTROL das excecoes cria `holt-winters/R/holt_winters.R` na copia.
- Deck de teste (`pcc_child_deck`): fixture_store.R, SP + ZZ1, 460 dias terminando em O = 2024-10-15, `available_until` 09:30, `weather_issue_cutoff:
  origin_midnight_utc` (a emissao 00Z cobre o dia O inteiro; com `available_until` o clima previsto do dia O perde as horas 00-03h e o
  DESSEM aborta com `EXOGENAPREV ... 21/24 horas`). O feriado futuro O+3 (codigo 3) vira codigo 12 (sem amostra historica o motor recusa).
  `pcc_day_effects`: componente por dia, quase-aleatoria (fase em d^2), em temperatura/carga/previsto; sem ela o ARIMA do regdin nao ajusta.
- Params rapidos (`pcc_fast_params`): RF `history_days.train=40` + `ntree=5`; LGBM 115; HW 40; dessem-linear 40/40; dessem-regdin 400/400
  (abaixo de ~400 dias o ARIMA com dummies de mes + lag 7 do fixture da verossimilhanca nao-finita). Areas: LGBM em SP (unica com `heat_area`), demais ZZ1.
- Sem relogio e sem env: `trained_at`/`code_commit`/`renv_lock_hash` injetados no ctx; o CONTROL do artefato com campo de relogio usa dois
  `POSIXct` fixos diferentes (nao `Sys.time()`); o ENV sujo do AC41 entra so no filho (`withr::with_envvar`).

## Resultados da verificacao
- AC38: 8/8 membros registram; `exogena` NA <=> `variant_param` NA; `exogena` em `allowed`; `mdir` da topologia == `models/*/run.yaml` (5, sem orfao).
  CONTROLs: `(dessem-regdin, MXMN)` e `mdir: nao-existe` saem exit 2. Entrega dos 8 pelas fachadas `identical()` a `prevcarga_delivery_grid`:
  413 interval_end (10:00 de O ate O+9 00:00) nos 7 de horizonte longo, 77 (ate O+2 00:00) no LGBM.
- AC39: 19 greps vazios na arvore real (global = interino pinado); cada um tem dente numa copia plantada (arquivo:linha).
- AC40: 5 plugins na mesma sessao, treino 1x cada: `pcc_snapshot()` (globalenv/search/`.Random.seed`) identico antes/depois (FORBIDDEN); 5 envs distintos; motores DL e RD distintos.
  CONTROLs: RF seeds 123 x 124 => max|Delta| = 148,8 (> 1e-5), 123 x 123 => 0; `toy` com `assign(globalenv())` => `plugin.global_side_effect`.
- AC41: freeze 2x de 2 treinos identicos => sha256 igual nos 5 (e distintos entre plugins); thaw -> infer `identical()`; ENV sujo `identical()`;
  `names(formals(...))` de `inference_features`/`inference` sem `params`; CONTROL do campo de relogio => sha256 diferente.
- Braco cross-OS: `[DEFERRED M8]` (Windows nativo nao roda aqui); no teste so o grep estatico de portabilidade dos 4 arquivos do fechamento.

## Achados (nao corrigidos aqui; donos nos epicos)
- RESOLVIDO (fix/facade-channels): o despachante preenche `cuts$spec`/`cuts$phase` em toda etapa e RF/LGBM declaram
  `weather_forecast_in_train`; o shim `pcc_install_f1_channel` do teste saiu e os 8 membros rodam pelas fachadas sem ajuda.
- Os plugins RF, LGBM e HW usam `.GlobalEnv` (nao `globalenv()`) no mesmo interino de busca da fundacao; o padrao de higiene do ticket (`globalenv()`) nao os pega.
  Quando o loader expuser a API, os pins e esse padrao devem ser revistos juntos.
- `pc_time_arith_allow()` guarda uma excecao (`holt_winters.R`) que ja nao corresponde a arquivo algum; as excecoes `hours(lagh)` do motor citadas
  no ticket tambem nao existem (lag por indice). Candidata a limpeza no epico da fundacao.
