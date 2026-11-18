# Estado: aceite em Docker local (spec RegDin Linear Refactor, epico E8)

Ferramenta de DESENVOLVIMENTO (so Linux, fora do bundle): `scripts/tools/accept_docker.sh`, com `accept_cases.R` (expansao das unidades), `accept_delta.R` (delta e veredito do aceite) e `accept_report.R` (relatorio da medicao). Nunca usa Batch, ECR, push, login nem AWS: tudo roda na maquina local, a partir da imagem do bundle. Nenhuma variavel de ambiente e lida.

## Como rodar

```
bash scripts/tools/accept_docker.sh --stage all --out /opt/ons-source/prevcarga-runs/accept-<RUN_ID> \
  --data /opt/ons-source/prevcarga-runs/deck-20261104-desagrega-pad [--jobs 12-14] [--resume] [--format text|json]
```

- `--out` fora de `/var`, do repo e do store (recusado com `contract.accept_out_forbidden`). Precisa de espaco livre (`--min-free-gb-docker`, `--min-free-gb-out`).
- `--data` e o store, montado SO leitura (`:ro`). A listagem (caminho, tamanho, sha256) antes e depois tem que ser igual.
- Sem `--ref-bundle`/`--new-bundle`, o script constroi os dois bundles (ref = v0.1.5, `ref_rev` do YAML; nova = HEAD, arvore suja recusada). Com eles, confere o `.sha256` de cada tar.
- `--config` (default `scripts/tools/accept_docker.yaml`): jobs (12-14), memory, cpus, min_free e as listas da medicao. Precedencia: flag > YAML > default embutido.
- `--resume` (so a medicao): pula a unidade cuja ULTIMA linha em `index.tsv` tem rc 0, `runtime_id` igual ao da imagem atual e sha256 dos LONGs recalculado igual ao gravado; senao apaga `units/<unit>/state` e roda de novo.
- `--receipts-dir` (default `<repo>/tests/.receipts`): onde vai o recibo `docker-accept`.

## O que cada estagio prova

| Estagio | Prova |
|---|---|
| `smoke` | `prevcarga doctor` roda nas duas imagens com `--network none`; a imagem tem tzdata (America/Sao_Paulo) e nao tem AWS CLI. |
| `aceite` | Delta zero entre ref e nova nas partes que nao mudaram (backtest S, infer-model train x inference), feriado do dessem-linear sem `generic.unexpected` e inferencia da nova estritamente mais rapida (`aceite.json`, `delta.tsv`, `delta.inference.tsv`). |
| `medicao` | 384 unidades `backtest --origin=O --area=A --model=<mdir>` (12 origens x 8 areas x dessem-linear e dessem-regdin x ref e nova) na fila de 13 containers. Recusa classificada (rc 2-15 com `error_id`) e resultado, nao defeito: vira celula ausente. So rc 1 sem envelope ou rc 137 contam failed (`partial.accept_units_failed`, exit 14). |
| `relatorio` | `accept_report.R` monta `report.tsv`, `report.md` e `report.json` e a limpeza confere residuo zero; grava o recibo `<sha256 do bundle novo>.docker-accept.json` (PASS so com smoke, aceite e medicao verdes, relatorio ok, residuo zero e store intacto). `all` roda o relatorio mesmo apos falha de um estagio (smoke falho pula aceite e medicao). |

## Origem e area das unidades de tempo do aceite

As celulas de corretude do aceite (backtest S e os 8 feriados) ficam em `aceite.origem` (2023-06-01, com Corpus Christi de 2023-06-08 no horizonte). As unidades de TEMPO (train-models, infer-model train e inference, deck-inference) usam `aceite.origem_tempo` (opcional; sem ela vale `aceite.origem`), na area `aceite.area_tempo` e no membro `aceite.membro_tempo`.

Escolha atual: origem_tempo = 2023-05-10, area RJ, membro dessem-linear/MED. Motivo (RUN_ID 20261117T123739Z-1a854584): em 2023-06-01 a ref v0.1.5 nao treina o dessem-linear em RJ (`generic.unexpected` "cross must not exceed sampling size": amostra de 5 dias do feriado, o clamp do cross falha na nova tentativa e o erro original sobe), entao nao ha tempo da ref para comparar. 2023-05-10 nao tem feriado no horizonte (2023-05-10 a 2023-05-19) em nenhuma das 8 areas, e e uma das 12 origens da medicao; ali a ref treina e infere. O delta de inferencia (train x inference) e o tempo passam a ser lidos nessa origem. A origem 2023-06-01 segue so como unidade de corretude (celulas de feriado da nova).

## Relatorio da medicao

- LONG por (versao, membro, area, origem), com o membro tirado do caminho `members/<membro>/<folha>/<origem>/<corte>/long.parquet` das unidades rc 0. So `d_plus == 1`.
- `y_true` e SEMPRE a verdade fresca de `prevcarga_read_actual` (igualdade exata por `interval_end`); o `y_true` do LONG e descartado. Verdade ausente para um alvo exclui a chave.
- So entram as chaves (membro, area, origem, corte, alvo) presentes nas DUAS versoes com `y_true` e `y_pred` finitos. O membro que so existe na nova (LIN_UNI) entra com `wmape_ref`/`mape_ref` '-'. Celula com LONG de um lado so e ausencia, nao entra na metrica.
- O score e SO `prevcarga_scorecard(by = c(versao, membro, area))`; nada e recalculado no relatorio.
- `report.tsv` (TAB, ASCII, ordem radix por membro e area): `membro area n_ref n_new wmape_ref wmape_new wmape_diff mape_ref mape_new mape_diff`; '-' = sem valor. `report.md` traz RUN_ID, runtime_ids, contagem de unidades e a secao Ausencias (versao, origem, area, membro, error_id, rc). `report.json` traz as mesmas linhas, as ausencias, o sha256 de `report.tsv` e `store_unchanged`.
- `accept_report.R` sai com 2 (`contract.accept_report_input`: `index.tsv`, `units.medicao.tsv` ou LONG listado ausente/ilegivel) ou 9 (`integrity.accept_store_changed`, store diferente antes e depois; nenhuma saida e gravada).

## Regra de limpeza (maquina compartilhada)

As imagens saem de um builder buildx DEDICADO por RUN_ID (`pc-accept-<RUN_ID>`, driver docker-container, `docker buildx build --load`): o build cache e do proprio builder e nao se mistura com o do builder default (outras sessoes podem construir ao mesmo tempo). O trap `pc_accept_cleanup` (EXIT/INT/TERM, instalado antes do primeiro build, idempotente) remove SO o que a execucao criou: containers e imagens com o label `prevcarga.accept.run=<RUN_ID>` (ou a tag `prevcarga-accept:<RUN_ID>-*`), o builder dedicado (`docker buildx rm --force`: container e volume de estado) e a imagem `moby/buildkit` se nao existia antes (rmi sem -f). NAO faz prune algum (nem de cache, nem global, nem `docker system prune`) e nunca toca imagem preexistente. Sobra de container, imagem ou builder => exit 9 (`integrity.accept_residue`), que prevalece sobre o rc original. `cache.snapshot` (ids do cache default) e so informativo.

## Exits

| Exit | Ids |
|---|---|
| 0 | ok |
| 2 | `contract.accept_bad_args`, `contract.accept_bad_config`, `contract.accept_out_forbidden`, `contract.accept_disk_low`, `contract.accept_report_input` |
| 6 | `bundle.accept_image_build`, `bundle.accept_image_smoke` |
| 9 | `integrity.accept_residue`, `integrity.accept_store_changed` |
| 12 | `repro.accept_delta_nonzero`, `repro.accept_unexpected_error`, `repro.accept_infer_not_faster` |
| 14 | `partial.accept_units_failed` |
| 130 / 143 | INT / TERM (limpeza roda antes de sair) |

`runtime.accept_oom_killed` e o `error_id` de uma unidade rc 137 em `index.tsv`, nao um exit do script.

## Arquivos em --out

`run.json`, `store.before.tsv`, `store.after.tsv`, `cache.snapshot`, `residue.json`, `index.tsv` (uma linha por execucao de unidade, a ultima vale), `units.<estagio>.tsv`, `stages.tsv` (veredito por estagio), `aceite.json`, `delta*.tsv`, `report.tsv`, `report.md`, `report.json` e `units/<unit>/` (config, montagens, `result.json`, `stderr.log`, `state/`).

## Registro

Execucao do E8-T4 em 2026-11-17/18, nesta maquina (16 CPUs, 125 GB, Docker 28.5.0), a partir do HEAD 7c414e1 da integracao (fallback do tune 5f74416, config/VERSION 0.1.6, ferramentas de aceite). Docker local so, a partir da imagem do bundle; zero AWS, Batch, ECR, push, login, PR ou tag. Medicao autorizada pelo Gabriel em 2026-11-17 (repassada pelo coordenador da spec).

| Campo | Valor |
|---|---|
| Saida (`--out`) | /opt/ons-source/prevcarga-runs/accept-20261117 |
| RUN_ID smoke | 20261117T223644Z-e3d72f55 |
| RUN_ID aceite | 20261117T224847Z-269c671a |
| RUN_ID medicao e relatorio | 20261117T234854Z-a370b08d (cada estagio gera o proprio RUN_ID; os bundles e o `--out` sao os mesmos) |
| runtime_id ref (v0.1.5, de24021) | c872c96740ecbc3c7e06e9f973bd2f4b09f560801d42a8381f5e82e82aed99f3 |
| runtime_id nova (HEAD 7c414e1) | 49ab0ce5b4b60b9bb7d14e2b18256308f745ffe78a97d695c4fd32778812e75b |
| sha256 do bundle ref | 1dc033f1ddcc03901ccff6a0cd755f20e34ca43f51228bf7d9327ce060b06262 |
| sha256 do bundle novo | 817fb03e0d331c5f00e3759370809f00672bae75f94f85f9fa44879b6847cf8b |
| sha256 de report.tsv | 7e530e9b15186faf73f1a26c5c96750b3aafbd7d8c3f5248572042ebef924eb6 |
| sha256 de accept_docker.yaml | 9032fcc5b3e2c9b6dad30dff168586b1dbb4bf9e10e85dc08e6f098486caeab9 |
| Veredito smoke | PASS (rc 0; doctor 0, tzdata, sem aws, `--network none` verificado por docker inspect; residuo 0/0/0) |
| Veredito aceite | PASS (rc 0; 16 unidades ok; delta max_abs 0 e zero chaves exclusivas; 0 generic.unexpected; infer RJ nova 10 s < ref 23 s) |
| Veredito medicao | PASS (rc 0; 384 unidades, 336 ok, 48 recusas classificadas, 0 failed) |
| Veredito relatorio | PASS (48 linhas, 0 bytes nao ASCII, store identico, residuo 0/0/0) |
| Recibo docker-accept | tests/.receipts/817fb03e...cf8b.docker-accept.json, resultado PASS, runtime_id da nova (nao versionado: a pasta e ignorada pelo git) |
| Aprovacao do Gabriel | pendente |

## Resultado da execucao (E8-T4)

### Aceite (origem 2023-06-01, subsistema S e 8 areas de feriado)

- `delta.tsv` (27 linhas de dados: 8 membros x 3 folhas PR, RS e SC, as 3 folhas combinadas e o subsistema S): max_abs 0 e keys_only 0 em todas. `delta.inference.tsv`: max_abs 0 (384 chaves).
- `aceite.json`: verdict PASS, culprits vazio, refusals vazio, zero `generic.unexpected` nas 8 areas de feriado (AM, BAOE, CE, DF, GO, PA, RJ, TON), todas rc 0 na nova.
- A origem de tempo e 2023-05-10, area RJ, membro dessem-linear/MED (a ref nao treina o MED em RJ em 2023-06-01; ver acima). Tempos nessa origem em RJ, ref contra nova:

| Etapa (RJ, 2023-05-10) | ref | nova |
|---|---|---|
| train-models dessem-linear/MED | 434 s | 479 s |
| infer-model sobre o deck de treino (deck longo, 3 anos + anexo) | 23 s | 10 s |
| infer-model sobre o deck de inferencia (deck curto) | - (a ref nao tem deck de inferencia) | 6 s |
| deck-inference (gerar o deck curto) | - | 10 s |
| backtest S 2023-06-01 (3 folhas, 8 membros) | 3249 s | 3332 s |

A inferencia da nova e mais rapida que a da ref (10 s contra 23 s, 2,3x) e o deck curto reduz mais (6 s, 3,8x sobre a ref). O treino da nova e 10% mais lento (479 s contra 434 s) nessa origem; o criterio do aceite e so o infer-model.

### Medicao (12 origens x 8 areas x 2 mdirs x ref e nova, 384 unidades, `--jobs 13`)

- Duracao: de 2026-11-17 23:48Z a 2026-11-18 07:38Z (cerca de 7 h 50 min), 13 containers no maximo. Memoria observada por amostragem do `docker stats`: no maximo 1,57 GiB por container (limite 8 GiB); nenhum OOM nem 137.
- Tempo por unidade (rc 0): dessem-linear nova media 1361 s (max 4049 s), ref media 1710 s (max 5251 s); dessem-regdin nova media 369 s (max 476 s), ref media 478 s (max 661 s). Nesse backtest (3 a 4 membros por area) a nova e mais rapida que a ref nos dois mdirs (media 20% e 23% menor).
- 384 unidades distintas em `index.tsv`: 336 rc 0 (nova 88 linear + 81 regdin; ref 85 linear + 82 regdin) e 48 rc 14 (`partial.backtest_delivery`, recusa classificada com envelope; 0 failed: nenhuma rc 1 sem envelope nem 137). Exit do estagio 0.

#### Recusas (48 unidades, classificadas; herdada x nova)

| Causa | Ref | Nova | Classificacao |
|---|---|---|---|
| `plugin.dessem_linear_series_gap` e `plugin.regdin_series_gap` em MS, origens 2023-02-19, 03-15, 05-10, 06-07, 08-16, 10-18 (12 unidades por versao; causa na secao "MS: recusas e causa" abaixo) | 12 | 12 | HERDADA (dado: mesma recusa nas duas versoes) |
| `plugin.dessem_linear_series_gap` e `plugin.regdin_series_gap`, MS origem 2023-12-31 | 0 | 2 | NOVA: a ref entrega MS 2023-12-31 e a nova recusa, porque a janela de feriado da ADR-046 (historico desde 01-01 de ano(O)-5) alcanca 2018, onde a fonte de MS esta zerada (secao "MS: recusas e causa") |
| "contrasts can be applied only to factors with 2 or more levels" (lm do perfil de feriado do regdin, RGD_MAX, origem 2024-05-29, as 8 areas; sem error_id) | 8 | 8 | HERDADA (bug do regdin, mesmo erro nas duas versoes) |
| "'cross' must not exceed sampling size!" (dessem-linear LIN_MAX, origem 2023-06-07; sem error_id) em AM, CE, GO, PA e RJ | 5 | 0 | HERDADA da ref; a nova entrega AM, CE, PA e RJ (CE, PA e AM pelo fallback do tune) |
| `engine.dessem_tune_small_sample` (LIN_MAX, GO, 2023-06-07; fallback sem tune tambem nao resolveu) | 0 | 1 | A celula tambem falha na ref (cross); na nova o erro passou a ser classificado. Falha NOVA como id, mesma celula herdada |
| Total | 25 | 23 | 48 |

O verbo `features` e o "contrasts" do regdin sao falhas herdadas; o `features` nao faz parte desta medicao. A unidade recusada leva consigo todos os membros do mdir naquela origem e area (a celula so entrega com todos os membros), por isso as ausencias em `report.md` somam 123 linhas (membro x unidade recusada).

#### MS: recusas e causa (confirmado em 2026-11-18)

Contagem em `index.tsv` (RUN_ID 20261117T234854Z-a370b08d, 384 unidades): MS tem 15 unidades recusadas na nova e 13 na ref, todas `partial.backtest_delivery` (rc 14).

- Nova (15): as 14 de MS em 2023 (7 origens: 2023-02-19, 03-15, 05-10, 06-07, 08-16, 10-18 e 12-31, x dessem-linear e dessem-regdin) mais a de 2024-05-29 do dessem-regdin ("contrasts", herdada).
- Ref (13): 12 de 2023 (as mesmas 6 origens, sem 2023-12-31) mais a de 2024-05-29 do dessem-regdin.

Causa, confirmada na fonte DynamoDB g_cargaverificada (prod): em MS, de junho a dezembro de 2018, `val_cargaglobal` e `val_cargaglobalcons` valem 0 exatamente quando `val_cargasup` < 0 (389 meias-horas em 54 dias). A janela de feriados da ADR-046 comeca em 01-01 de ano(O)-5 e por isso alcanca 2018; a validacao do anexo recusa o deck. A ilha de 2018-06-10 vem do codigo 12 (Copa), que o motor normaliza para 0. A descricao anterior (lacuna de carga, CARGAFER 2018-06-10 com 22/24 horas) era so o sintoma.

#### Decisoes do Gabriel em 2026-11-18

- Aceitas as recusas como estao (a do "contrasts" de 2024-05-29 no regdin e herdada).
- Aceito o relatorio de MAPE/WMAPE.
- Aceito o fallback sem tune, que fica no codigo deste ticket; a emenda ou ADR sai com a spec do modelo de feriados, que remove o perfil de feriado do DESSEM.
- Aceito o skip da paridade, porque o golden esta fora do host (pendencia registrada).
- A area nao entra no warning de fallback.
- Correcao do historico de MS 2018 fica como discovery tech_debt nao bloqueante ("dever de casa").

#### Fallback do tune (`warning.engine_tune_fallback`)

14 celulas de feriado do dessem-linear da nova entregaram pelo fallback sem tune (AM 1, CE 7, PA 5, RS 1), nenhuma do dessem-regdin. Isso NAO e falha. Lista e marcacao por area em `aceite-docker-feriado.md`.

#### Metricas D+1 por area (`report.tsv`, 6 membros x 8 areas = 48 linhas; MAPE e WMAPE em %)

Pareado ref x nova com verdade fresca; LIN_UNI so existe na nova. MS tem poucas chaves porque as recusas tiram origens inteiras (n menor). Diferenca = nova - ref (positivo = a nova erra mais).

| membro | area | n ref | n nova | WMAPE ref | WMAPE nova | diff | MAPE ref | MAPE nova | diff |
|---|---|---|---|---|---|---|---|---|---|
| LIN_MAX | AM | 528 | 528 | 5.89 | 6.10 | 0.21 | 6.21 | 6.36 | 0.15 |
| LIN_MAX | CE | 528 | 528 | 4.50 | 5.87 | 1.37 | 4.63 | 6.06 | 1.43 |
| LIN_MAX | GO | 528 | 528 | 5.76 | 6.70 | 0.94 | 6.02 | 7.03 | 1.01 |
| LIN_MAX | MS | 240 | 240 | 10.25 | 11.93 | 1.68 | 11.51 | 12.88 | 1.37 |
| LIN_MAX | PA | 528 | 528 | 2.10 | 2.36 | 0.26 | 2.09 | 2.37 | 0.28 |
| LIN_MAX | RJ | 528 | 528 | 5.13 | 6.18 | 1.04 | 5.29 | 6.47 | 1.19 |
| LIN_MAX | RS | 576 | 576 | 5.23 | 6.07 | 0.84 | 5.06 | 6.31 | 1.25 |
| LIN_MAX | SP | 576 | 576 | 3.37 | 4.29 | 0.92 | 3.32 | 4.46 | 1.15 |
| LIN_MED | AM | 528 | 528 | 6.06 | 7.18 | 1.12 | 6.52 | 7.82 | 1.29 |
| LIN_MED | CE | 528 | 528 | 4.62 | 6.14 | 1.52 | 4.76 | 6.33 | 1.57 |
| LIN_MED | GO | 528 | 528 | 5.59 | 6.60 | 1.01 | 5.97 | 7.03 | 1.06 |
| LIN_MED | MS | 240 | 240 | 9.91 | 11.85 | 1.94 | 11.20 | 12.85 | 1.65 |
| LIN_MED | PA | 528 | 528 | 2.11 | 2.51 | 0.40 | 2.09 | 2.52 | 0.42 |
| LIN_MED | RJ | 528 | 528 | 5.06 | 6.03 | 0.97 | 5.29 | 6.34 | 1.05 |
| LIN_MED | RS | 576 | 576 | 5.00 | 5.89 | 0.89 | 4.87 | 6.20 | 1.33 |
| LIN_MED | SP | 576 | 576 | 3.37 | 3.99 | 0.62 | 3.33 | 4.19 | 0.86 |
| LIN_MXMN | AM | 528 | 528 | 6.08 | 6.21 | 0.13 | 6.44 | 6.46 | 0.02 |
| LIN_MXMN | CE | 528 | 528 | 4.61 | 6.06 | 1.44 | 4.73 | 6.23 | 1.51 |
| LIN_MXMN | GO | 528 | 528 | 5.51 | 6.38 | 0.87 | 5.76 | 6.74 | 0.97 |
| LIN_MXMN | MS | 240 | 240 | 10.22 | 11.63 | 1.41 | 11.53 | 12.56 | 1.03 |
| LIN_MXMN | PA | 528 | 528 | 1.99 | 2.48 | 0.49 | 1.98 | 2.49 | 0.51 |
| LIN_MXMN | RJ | 528 | 528 | 4.86 | 6.10 | 1.24 | 5.01 | 6.39 | 1.38 |
| LIN_MXMN | RS | 576 | 576 | 5.26 | 6.31 | 1.05 | 5.10 | 6.54 | 1.44 |
| LIN_MXMN | SP | 576 | 576 | 3.54 | 4.22 | 0.67 | 3.49 | 4.42 | 0.93 |
| LIN_UNI | AM | 0 | 576 | - | 9.09 | - | - | 9.93 | - |
| LIN_UNI | CE | 0 | 576 | - | 5.29 | - | - | 5.53 | - |
| LIN_UNI | GO | 0 | 528 | - | 6.74 | - | - | 7.41 | - |
| LIN_UNI | MS | 0 | 240 | - | 12.65 | - | - | 13.72 | - |
| LIN_UNI | PA | 0 | 576 | - | 2.57 | - | - | 2.56 | - |
| LIN_UNI | RJ | 0 | 576 | - | 10.30 | - | - | 10.72 | - |
| LIN_UNI | RS | 0 | 576 | - | 6.98 | - | - | 7.11 | - |
| LIN_UNI | SP | 0 | 576 | - | 4.85 | - | - | 5.09 | - |
| RGD_MAX | AM | 528 | 528 | 5.61 | 5.68 | 0.08 | 5.96 | 5.98 | 0.02 |
| RGD_MAX | CE | 528 | 528 | 5.12 | 5.73 | 0.62 | 5.47 | 6.09 | 0.63 |
| RGD_MAX | GO | 528 | 528 | 5.30 | 5.69 | 0.40 | 5.61 | 6.10 | 0.49 |
| RGD_MAX | MS | 192 | 192 | 9.90 | 11.92 | 2.02 | 11.61 | 13.75 | 2.14 |
| RGD_MAX | PA | 528 | 528 | 3.43 | 3.48 | 0.04 | 3.43 | 3.48 | 0.05 |
| RGD_MAX | RJ | 528 | 528 | 5.86 | 6.21 | 0.34 | 5.95 | 6.35 | 0.40 |
| RGD_MAX | RS | 528 | 528 | 4.73 | 5.11 | 0.37 | 5.01 | 5.71 | 0.70 |
| RGD_MAX | SP | 528 | 528 | 3.51 | 4.76 | 1.25 | 3.53 | 5.06 | 1.53 |
| RGD_MED | AM | 528 | 528 | 6.62 | 6.76 | 0.14 | 7.10 | 7.30 | 0.19 |
| RGD_MED | CE | 528 | 528 | 5.18 | 5.81 | 0.62 | 5.61 | 6.21 | 0.60 |
| RGD_MED | GO | 528 | 528 | 5.13 | 5.36 | 0.23 | 5.52 | 5.78 | 0.26 |
| RGD_MED | MS | 192 | 192 | 9.26 | 11.17 | 1.91 | 10.90 | 13.18 | 2.28 |
| RGD_MED | PA | 528 | 528 | 3.25 | 3.18 | -0.08 | 3.25 | 3.18 | -0.07 |
| RGD_MED | RJ | 528 | 528 | 5.83 | 6.17 | 0.34 | 5.98 | 6.37 | 0.39 |
| RGD_MED | RS | 528 | 528 | 4.46 | 4.73 | 0.28 | 4.79 | 5.33 | 0.54 |
| RGD_MED | SP | 528 | 528 | 3.68 | 4.95 | 1.28 | 3.68 | 5.24 | 1.56 |

Leitura: a nova erra mais que a ref em 39 das 40 linhas pareadas (diferenca media de WMAPE: LIN_MED +1,06, LIN_MAX +0,91, LIN_MXMN +0,91, RGD_MAX +0,64, RGD_MED +0,59 ponto percentual). So RGD_MED PA melhora (-0,08). Isso e a mudanca de previsao prevista pela ADR-046 (janela de feriado), que "exige o relatorio de WMAPE/MAPE D+1 (12 origens x 8 areas) aprovado pelo Gabriel". A aprovacao e do Gabriel; este registro nao a decide. MAPE e WMAPE por area e por origem (todos os membros) estao em `porigem.tsv`; as variantes MED estao em `aceite-docker-feriado.md`. O `report.tsv` do estagio relatorio sai com espacos de preenchimento antes de alguns numeros (`formatC` sem `trim`); o sha256 acima e do arquivo como gerado.

#### MAPE e WMAPE por grupo de dia (feriado, vizinho, comum), pedido do Gabriel

Em `aceite-docker-feriado.md` (por membro DESSEM e area, numero de dias de cada grupo e celulas com fallback). Resumo D+1, nova, media simples das 8 areas (dias somados; MAPE; WMAPE):

| membro | feriado (dias; MAPE; WMAPE) | vizinho | comum |
|---|---|---|---|
| LIN_MAX | 40; 8.13; 7.77 | - | 44; 4.98; 4.93 |
| LIN_MED | 40; 8.48; 8.06 | - | 44; 4.95; 4.87 |
| LIN_MXMN | 40; 8.12; 7.78 | - | 44; 4.88; 4.85 |
| LIN_UNI | 44; 9.95; 9.45 | - | 44; 5.47; 5.43 |
| RGD_MAX | 37; 8.62; 7.91 | - | 44; 4.78; 4.67 |
| RGD_MED | 37; 8.64; 7.84 | - | 44; 4.79; 4.64 |

Em feriado o erro e 1,6 a 1,7 vez o de dia comum (D+1); no horizonte inteiro a diferenca cai (ver o arquivo). O grupo vizinho nao tem dias em D+1 nas 12 origens; aparece so na cobertura de horizonte.

### Combinacao (combina)

Por decisao do Gabriel (2026-11-17, "esquece combinacao. gerando resultado ta bom"), a medicao so precisa mostrar que a combina executou e entregou, mesmo em EQW.

- As 336 unidades rc 0 da nova e da ref entregaram `combined/<area>/0800/combined.parquet` (336 de 336) e a tabela de pesos do mes (`weights/<ano-mes>/<area>_0800.rds`, presente em 336 de 336).
- A tabela de pesos vem vazia em todas as origens (cada unidade e uma origem so, sem historico de combina: a combinacao cai em EQW, igual peso). Verificacao numerica na unidade RJ 2023-05-10 dessem-linear da nova: o `y_pred` combinado e a media simples dos 4 membros (LIN_MAX, LIN_MED, LIN_MXMN, LIN_UNI), diferenca maxima 9,1e-13 em 416 linhas.
- No aceite, o delta `combined/{PR,RS,SC}` entre ref e nova tambem foi 0.

### Fora de escopo por decisao do Gabriel, 2026-11-17

- sensibilidade com n_min = 5;
- correlacao LIN x LIN_UNI (nas tres variantes LIN_MED, LIN_MAX e LIN_MXMN);
- contagem de `warning.combina_nonstationary` com 9 membros contra 8 membros;
- distribuicao de iteracoes da combina nas duas configuracoes;
- inclusao de RF, LGBM e HW na medicao (e mais origens).

Nada disso foi rodado. `scripts/tools/accept_combina.R` (ferramenta dessas analises) nao foi executado.

### Paridade pendente por golden ausente

O tier parity do `_dessem-engine` e do reconcile NAO foi executado: os goldens nao estao nesta maquina (estao em /home/gabri, gerados em outra maquina; o R daqui nao tem WeightSVM e o oraculo prevcargadessem3 c4af737 nao confirma que gerou o golden). Registrado como PENDENTE por golden ausente, sem referencia inventada e sem regenerar golden do oraculo. Depende do Gabriel disponibilizar o golden. Os testes de paridade ficam como skipped.

### Limpeza e integridade (conferidas por comandos independentes do script)

- `residue.json` = {"containers":0,"images":0,"cache":0}; `docker ps -aq --filter label=prevcarga.accept.run=<RUN_ID>`, `docker images -q --filter label=...` e `docker images 'prevcarga-accept:*'` voltaram 0 linhas para os tres RUN_ID; nenhum volume `pc-accept`, nenhum container `buildx_buildkit_*`, nenhuma imagem `moby/buildkit`.
- Antes e depois (listagens completas em `<out>/docker-proof/`): `docker ps -a` com os mesmos 26 containers (nomes identicos), `docker images` com as mesmas 189 imagens (id e tag identicos; `prevcarga-cli:0.1.5-de24021` e a imagem ECR local `...prevcarga:cli-0.1.5-de24021` intactas, 321cee27f89d), `docker buildx ls` so com `default*` antes e depois. Nenhum build de terceiros em andamento antes de comecar.
- `store.before.tsv` = `store.after.tsv` (1011 arquivos listados; store montado `:ro`); `diff` sem saida.
- AWS: `command -v aws` vazio nas duas imagens e `NetworkMode none` (smoke), Dockerfile sem aws/awscli, `grep -riE 'amazonaws|s3://|batch'` nos `stderr.log` e `result.json` das 400 unidades sem nenhum acerto, nenhum comando aws executado.
- `report.tsv`, `report.md` e `report.json` sem byte nao ASCII.

Aprovacao do Gabriel: pendente.
