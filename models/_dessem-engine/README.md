# _dessem-engine

Motor DESSEM unico (ADR-0031/0012). Codigo nosso, refactor que preserva a saida: as primitivas
`pkgfun_*` do oraculo `/opt/ons-source/prevcargadessem3/models/dessem-linear/vendor/` com os nomes
internos preservados e so o caminho serial de um modelo. Sem `run.yaml`: prefixo `_`, nao e
descobrivel como modelo. Plugins (`dessem-linear`, `dessem-regdin`) declaram `engine = "_dessem-engine"`
no descriptor e o loader da fundacao (`prevcarga_plugin_load`, `scripts/lib/registry.R`) monta um env do
motor por instancia a partir de `engine_manifest()` (em `R/manifest.R`).

Regra: o motor so tem PRIMITIVAS. A cabeca de nowcast e do plugin (`models/dessem-*/R/helpers_nowcast.R`);
o motor nao a contem. Cada arquivo de `R/` so cria funcoes (sem estado, sem expressao de topo).

## Ordem de avaliacao

`R/manifest.R` (lido primeiro pelo loader) e depois `engine_manifest()$files`:
`funcoes_apoio.R`, `dados_entrada.R`, `entrada_demanda_media.R`, `entrada_perfil.R`,
`modelos_demanda_media.R`, `modelos_perfil.R`, `exec_modelos.R`, `engine.R`. O DL.2 acrescenta robustez,
feriados, exogena e svm_serial.

O env do motor so enxerga `base` e os pacotes que o descriptor do plugin lista em `attach`
(o loader copia as exportacoes; nao ha `library()`): o plugin deve anexar `data.table`, `lubridate`,
`jsonlite`, `rlist`, `WeightSVM`, `e1071`, `stats` e `utils` (o regdin soma `fable`, `fabletools`, `feasts`,
`urca`, `tsibble`, listados em `engine_manifest()$packages_ts`).

## required_functions: funcao -> arquivo do motor -> pkgfun_*.R:linha do oraculo

```
obterEntradaFormulaDM -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:316
getConfig -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:361
desagregaModeloSH -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:389
TransformaDeck -> dados_entrada.R -> pkgfun_dadosEntrada.R:9
SVM_DemandaMedia -> modelos_demanda_media.R -> pkgfun_modelosDemandaMedia.R:2
RD_DemandaMediaHyndman -> modelos_demanda_media.R -> pkgfun_modelosDemandaMedia.R:27
SVMhorario -> modelos_perfil.R -> pkgfun_modelosPerfil.R:3
RL_perfilFeriado -> modelos_perfil.R -> pkgfun_modelosPerfil.R:27
execDemandaMedia -> exec_modelos.R -> pkgfun_execModelos.R:4
execDemandaMediaTS -> exec_modelos.R -> pkgfun_execModelos.R:16
processaModelosDMPerfil -> exec_modelos.R -> pkgfun_execModelos.R:82
processaModelosDMTSPerfil -> exec_modelos.R -> pkgfun_execModelos.R:106
```

## Demais funcoes portadas (mesmo mapa)

```
obtemLag -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:1
obtemLagExo -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:21
obtemLagsExo -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:28
obtemLagsExogenas -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:33
obtemLagHora -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:41
obtemLags -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:67
obtemLagsHora -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:79
obtemLagsDiaSemana -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:86
obtemLagDiaSemana -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:93
obtemSD -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:106
aggregaSerie -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:114
dividepor -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:145
mxmn -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:149
normalizaSerieRef -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:153
normalizafun -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:159
getExoCols -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:168
getFuncTemp -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:172
rangelst -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:187
getSVMWeights -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:192
padronizaCargaHorariaRef -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:203
validaEntrada -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:220
getNormalizaCarga -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:229
getNormalizaExogena -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:236
obtemLagSemiHora -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:244
obtemJanelaDiaria -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:268
obterDiaEspDMConfig -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:275
obterDiaEspPerfilConfig -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:292
obterEntradaFormulaDM_Reg -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:328
filtraDadosTreinamento -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:339
getFormula -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:344
getLagsExoReg -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:353
lagsExoStr -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:357
getConfigModel -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:366
obterModelos -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:376
addValuesDMRef -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:380
addValueDMRef -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:385
deckEntrada -> dados_entrada.R -> pkgfun_dadosEntrada.R:106
entradaDemandaMedia -> entrada_demanda_media.R -> pkgfun_entradaDemandaMedia.R:27
entradaDemandaMediaDefault_ML -> entrada_demanda_media.R -> pkgfun_entradaDemandaMedia.R:61
entradaDemandaMediaLocal_ML -> entrada_demanda_media.R -> pkgfun_entradaDemandaMedia.R:79
entradaTimeSeriesDefault -> entrada_demanda_media.R -> pkgfun_entradaDemandaMedia.R:94
entradaTimeSeriesFactor -> entrada_demanda_media.R -> pkgfun_entradaDemandaMedia.R:102
inclui_dummy_DE -> entrada_demanda_media.R -> pkgfun_entradaDemandaMedia.R:116
inclui_dummy_dia -> entrada_demanda_media.R -> pkgfun_entradaDemandaMedia.R:131
inclui_dummy_mes -> entrada_demanda_media.R -> pkgfun_entradaDemandaMedia.R:140
entradaPerfil -> entrada_perfil.R -> pkgfun_entradaPerfil.R:20
entradaPerfilDefault -> entrada_perfil.R -> pkgfun_entradaPerfil.R:40
inclui_CargaMediaRef -> entrada_perfil.R -> pkgfun_entradaPerfil.R:47
entradaPerfilFeriadoReg -> entrada_perfil.R -> pkgfun_entradaPerfil.R:53
execDMPerfil -> exec_modelos.R -> pkgfun_execModelos.R:46
execDMTSPerfil -> exec_modelos.R -> pkgfun_execModelos.R:56
execDatasPerfil -> exec_modelos.R -> pkgfun_execModelos.R:68
execHorasPerfil -> exec_modelos.R -> pkgfun_execModelos.R:72
```

Mudancas deliberadas em relacao ao vendor (so estas): `getConfig`/`getConfigModel` leem
`model_parameters.json` do diretorio do motor (`engine_config_path()` em `R/engine.R`, a partir do `.engine_dir`
que o loader grava) em vez de `system.file`; `getConfigModel` aborta com `engine.dessem_unknown_model_key`
(catalogo `scripts/lib/errors/dessem_engine.tsv`, via `engine_abort`) para chave inexistente em vez de devolver NULL;
`fromJSON` qualificado como `jsonlite::fromJSON`; as mensagens de progresso de `exec_modelos.R` perdem o carimbo
`Sys.time()` (so log). As datas ainda usam a forma do oraculo (`%m+% hours()`, `%m-% minutes(30)`): a reescrita
e o DL.3. `obterEntradaFormulaDM` e a versao ORIGINAL do vendor (sem o termo `mm`).

## Fato verificado: vendor linear == vendor regdin

Os 7 `pkgfun_*.R` e o `model_parameters.json` de `dessem-linear/vendor` e `dessem-regdin/vendor` sao
byte-identicos (`cmp`); so o `_load_engine.R` difere. `model_parameters.json` copiado byte-a-byte:
md5 `56f8fc5ed7b8db4275827a3a8122c0b0`, sha256 `463f61c3a184e1ef1960b6fd804ec4eb74648747d48c29d220479a3e8cfb8b88`.

## Nao portado

`aplicaPesos`, `writeOutDP`, `writeOutCSV`, `plotOutputData`, `RN_DemandaMedia` (neuralnet), `modelCores`,
paralelismo (`pkgfun_paralelismo.R`, `mclapply`), self-location (`_load_engine.R:61-102` e
`PREVCARGA_MODEL_R_DIR`), kill-switches por ENV, wrappers de nowcast (`_load_engine.R:213-251`) e a cabeca
`.dessem_nowcast`.

## Testes

`Rscript --vanilla -e 'testthat::test_dir("models/_dessem-engine/tests/testthat", stop_on_failure=TRUE)'`
(a partir da raiz do repo; R 4.5.3). `test-engine-golden.R` roda o oraculo em processo filho (R 4.4.3) e
confere `max|delta Carga| <= 1e-5` MW em `processaModelosDMPerfil` e `desagregaModeloSH`; e pulado se o oraculo
estiver ausente.
