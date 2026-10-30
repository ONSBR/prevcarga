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
`modelos_demanda_media.R`, `modelos_perfil.R`, `exec_modelos.R`, `engine.R`, e (DL.2) `robustez.R`, `feriados.R`,
`exogena.R`, `svm_serial.R`.

O env do motor so enxerga `base` e os pacotes que o descriptor do plugin lista em `attach`
(o loader copia as exportacoes; nao ha `library()`): o plugin deve anexar `data.table`, `lubridate`,
`jsonlite`, `rlist`, `WeightSVM`, `e1071`, `stats` e `utils` (o regdin soma `fable`, `fabletools`, `feasts`,
`urca`, `tsibble`, listados em `engine_manifest()$packages_ts`).

## required_functions: funcao -> arquivo do motor -> pkgfun_*.R:linha do oraculo

```
obterEntradaFormulaDM -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:316
getConfig -> funcoes_apoio.R -> pkgfun_funcoesApoio.R:361
engine_svm_freeze -> svm_serial.R -> novo (sem equivalente no oraculo)
engine_svm_thaw -> svm_serial.R -> novo (sem equivalente no oraculo)
normalize_special_day_codes -> feriados.R -> scripts/lib/normalize_holiday_codes.R:33
fill_forecast_tail -> exogena.R -> scripts/lib/fill_forecast_tail.R:68
dessem_engine_guard -> robustez.R -> novo (condicao classificada no lugar do texto de message)
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

## Robustez do tune_wsvm (DL.2): funcao -> arquivo -> `_load_engine.R:linha` do oraculo (vendor de dessem-linear)

O oraculo aplicava estas guardas como patch pos-source no globalenv; aqui vivem no env do motor
(SVM_DemandaMedia/SVMhorario chamam `tune_wsvm` sem `::`, que resolve no env do motor antes do pacote anexado).
`.tune_wsvm_real` (`_load_engine.R:264`, alias do pacote) vira funcao que chama `WeightSVM::tune_wsvm`. Caminho feliz bit-identico (chama o real primeiro). Sinalizam `dessem_engine_guard` (subclasse de `message`,
codigos `warning.engine_empty_model_guard` e `warning.engine_cross_clamped` do catalogo); o despachante do plugin
as coleta com `withCallingHandlers`; o motor nao guarda estado.

```
.wsvm_guard_retry -> robustez.R -> _load_engine.R:265
.wsvm_clamp_cross -> robustez.R -> _load_engine.R:311
tune_wsvm -> robustez.R -> _load_engine.R:331
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
`Sys.time()` (so log). As datas foram reescritas no DL.3 (secao abaixo). `obterEntradaFormulaDM` e a versao ORIGINAL do vendor (sem o termo `mm`).

## Datas (DL.3): construcao do oraculo -> forma lubridate / removido

O motor nao faz aritmetica de hora (ADR-0012/0019): o despachante da fundacao aplica `to_model_frame` na carga
na entrada e `to_display_frame` na saida pelo `model_time_offset_min` do descriptor (DESSEM 60); o carimbo de
30 min do `interval_end` e do `to_semihourly`. O motor nao chama `to_model_frame`/`to_display_frame`. Datas so
por lubridate com tz explicito; `TransformaDeck(..., tz)` aborta `engine.dessem_tz_mismatch` se o `tzone` de
`DataHora` de CARGAHIST/EXOGENAHIST/EXOGENAPREV diferir de `tz` (`cuts$tz`).

| oraculo (arquivo:linha) | forma no motor |
|---|---|
| pkgfun_dadosEntrada.R:14 `as.Date(dataInicioPrev)` | `.as_day(dataInicioPrev)` (`ymd` / `as_date`) |
| pkgfun_dadosEntrada.R:25, 46, 64, 74 `as.Date(format(DataHora,'%F'))` | `as_date(DataHora)` (usa o `tzone` da carga) |
| pkgfun_dadosEntrada.R:45 `DataHora %m-% hours(1)` | REMOVIDO: deslocamento no despachante via `model_time_offset_min` |
| pkgfun_dadosEntrada.R:87 `as.POSIXct(format(Data,'%F 12:00:00'),'America/Sao_Paulo')` | `.hvz_flag(Data)` = `make_datetime(..., 12L, tz = .engine_hv_tz())` |
| pkgfun_funcoesApoio.R:41, 51, 54 `%m+%/%m-% hours(lagh)` | lag por INDICE na grade horaria (`.IDX - lagh`); `.engine_assert_grid` |
| pkgfun_funcoesApoio.R:242, 245 `%m-% minutes(30*lagsh)` | lag por INDICE na grade de 30 min (`.IDX - lagsh`) |
| pkgfun_funcoesApoio.R:256, 269, 287, 311 `as.Date(dataPrev)` | `.as_day(dataPrev)` |
| pkgfun_funcoesApoio.R:391 `DataHora %m+% hours()` | REMOVIDO: `to_display_frame` do despachante |
| pkgfun_funcoesApoio.R:405 `DataHora %m-% minutes(30)` | REMOVIDO: carimbo de meia hora no `to_semihourly` |
| pkgfun_execModelos.R mensagens com `Sys.time()` | `message(paste(nomModelo, "- etapa", dataPrev))`, ASCII, sem relogio |

Sem excecao: nenhum `hours(...)`/`minutes(...)` no motor. O lag de feature e por indice na grade regular
(`obtemLagHora` horaria, `obtemLagSemiHora` de 30 min); serie com slot faltante aborta
`engine.dessem_lag_grid_gap`.
`desagregaModeloSH` devolve 2 linhas por ancora horaria na ordem de `Passo` (impar = meia hora anterior a
ancora, par = a ancora), ambas com o `DataHora` da ancora no frame de modelagem.

## Testes do motor: teste -> AC do epico -> tier (DL.4)

`Rscript --vanilla -e 'testthat::test_dir("models/_dessem-engine/tests/testthat", stop_on_failure = TRUE)'` (R 4.5.3, a
partir da raiz). Tier `fast` = tudo menos as linhas `parity`; o tier `parity` entra com
`options(prevcarga.tier = "parity")` antes do `test_dir` (ou pelo runner de gates, que fixa o tier) e exige o golden.

| teste | AC do epico | tier |
|---|---|---|
| test-engine-golden.R (unidades `engine` e `robustez` do golden isolado; CONTROL offset 0 e sem set.seed) | AC2 | parity (+ fast: units.yaml, golden.lock) |
| test-engine-dates.R (pc_grep_code + pc_grep_time_arith sem excecao, HVZ, tz) | AC3 | fast |
| test-engine-isolation.R (duas instancias, globalenv intacto, CONTROL sem desagregaModeloSH) | AC1 | fast |
| test-engine-robustez.R (modelo-vazio, clamp de cross, flag-12, cauda curta; kill-switches de ENV sem efeito) | AC16 (parte motor) | fast (+ unidades `robustez` no parity) |
| test-engine-svm-serial.R (freeze/thaw puro, sha256 estavel, predict identico) | AC14 (parte motor) | fast |
| test-engine-readme.R (mapa funcao -> arquivo -> linha do oraculo, tabelas) | AC14 (documentacao) | fast |

## Golden isolado: regeneracao

O golden (`native.rds`: deck cru + deck processado + saida horaria e semi-horaria do oraculo, `long.parquet`,
`invocation.json`) mora em `.state/golden/plugins/_dessem-engine/<variante>/<area>/<origem>/` (gitignorado); so o
`models/_dessem-engine/tests/parity/golden.lock` (sha256, commit do oraculo, fingerprint de R/pacotes/BLAS, digest dos dados)
e versionado. O oraculo roda no R 4.4.3 com a biblioteca que tem WeightSVM/e1071 (`oracle_env.R_LIBS_USER` do `units.yaml`) e
de um clone limpo do mesmo commit (o oraculo real tem arquivos nao rastreados que o preflight recusa):

```
git clone --no-hardlinks /opt/ons-source/prevcargadessem3 <tmp>/oracle-clone
export PATH=/opt/R/4.5.3/bin:$PATH
Rscript --vanilla scripts/tools/plugin_golden.R --model _dessem-engine --units engine   --oracle <tmp>/oracle-clone --data data --rscript /opt/R/4.4.3/bin/Rscript
Rscript --vanilla scripts/tools/plugin_golden.R --model _dessem-engine --units robustez --oracle <tmp>/oracle-clone --data data --rscript /opt/R/4.4.3/bin/Rscript
```

Cada unidade roda 2x (saidas `identical`, senao exit 12) e leva ~4 min por rodada; as unidades podem ser geradas em paralelo (`--variant MED|MAX|MXMN`, ou um units.yaml de uma unidade; o lock tem exclusao mutua). O teste compara o clean-room (motor no frame de
modelagem, saida devolvida por `to_display_frame` + carimbo de 30 min, `set.seed(42L)`) com `max|delta Carga| <= 1e-5` MW em
`processaModelosDMPerfil` (horario) e `desagregaModeloSH` (semi-horario). Golden ausente ou fingerprint/sha256 divergente do lock
=> falha pedindo para regerar (nunca skip no tier parity).

## Fato verificado: vendor linear == vendor regdin

Os 7 `pkgfun_*.R` e o `model_parameters.json` de `dessem-linear/vendor` e `dessem-regdin/vendor` sao
byte-identicos (`cmp`); so o `_load_engine.R` difere. `model_parameters.json` copiado byte-a-byte:
md5 `56f8fc5ed7b8db4275827a3a8122c0b0`, sha256 `463f61c3a184e1ef1960b6fd804ec4eb74648747d48c29d220479a3e8cfb8b88`.

## Nao portado

`aplicaPesos`, `writeOutDP`, `writeOutCSV`, `plotOutputData`, `RN_DemandaMedia` (neuralnet), `modelCores`,
paralelismo (`pkgfun_paralelismo.R`, `mclapply`), self-location (`_load_engine.R:61-102` e
`PREVCARGA_MODEL_R_DIR`), kill-switches por ENV (`DESSEM_NORMALIZE_CODES`, `DESSEM_EXOG_TAIL_FILL`: sempre ligados, como o default do oraculo), wrappers de nowcast (`_load_engine.R:213-251`) e a cabeca
`.dessem_nowcast`.

## Testes

`Rscript --vanilla -e 'testthat::test_dir("models/_dessem-engine/tests/testthat", stop_on_failure=TRUE)'`
(a partir da raiz do repo; R 4.5.3). `test-engine-golden.R` roda o oraculo em processo filho (R 4.4.3) e
confere `max|delta Carga| <= 1e-5` MW em `processaModelosDMPerfil` e `desagregaModeloSH`; e pulado se o oraculo
estiver ausente.
