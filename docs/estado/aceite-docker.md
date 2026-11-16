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

## Relatorio da medicao

- LONG por (versao, membro, area, origem), com o membro tirado do caminho `members/<membro>/<folha>/<origem>/<corte>/long.parquet` das unidades rc 0. So `d_plus == 1`.
- `y_true` e SEMPRE a verdade fresca de `prevcarga_read_actual` (igualdade exata por `interval_end`); o `y_true` do LONG e descartado. Verdade ausente para um alvo exclui a chave.
- So entram as chaves (membro, area, origem, corte, alvo) presentes nas DUAS versoes com `y_true` e `y_pred` finitos. O membro que so existe na nova (LIN_UNI) entra com `wmape_ref`/`mape_ref` '-'. Celula com LONG de um lado so e ausencia, nao entra na metrica.
- O score e SO `prevcarga_scorecard(by = c(versao, membro, area))`; nada e recalculado no relatorio.
- `report.tsv` (TAB, ASCII, ordem radix por membro e area): `membro area n_ref n_new wmape_ref wmape_new wmape_diff mape_ref mape_new mape_diff`; '-' = sem valor. `report.md` traz RUN_ID, runtime_ids, contagem de unidades e a secao Ausencias (versao, origem, area, membro, error_id, rc). `report.json` traz as mesmas linhas, as ausencias, o sha256 de `report.tsv` e `store_unchanged`.
- `accept_report.R` sai com 2 (`contract.accept_report_input`: `index.tsv`, `units.medicao.tsv` ou LONG listado ausente/ilegivel) ou 9 (`integrity.accept_store_changed`, store diferente antes e depois; nenhuma saida e gravada).

## Regra de limpeza (maquina compartilhada)

O trap `pc_accept_cleanup` (EXIT/INT/TERM, instalado antes do primeiro `docker build`, idempotente) remove SO o que a execucao criou: containers e imagens com o label `prevcarga.accept.run=<RUN_ID>` (ou a tag `prevcarga-accept:<RUN_ID>-*`), a base pinada se nao existia antes, e os ids de build cache ausentes de `cache.snapshot` (`docker builder prune -f --filter id=<id>`). NUNCA usa `docker system prune` nem prune sem filtro, e nunca toca imagem preexistente. Sobra de container, imagem ou cache => exit 9 (`integrity.accept_residue`), que prevalece sobre o rc original.

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

Preenchido pelo ticket de execucao (E8-T4); este ticket so entrega as ferramentas e os testes (docker falso, fixture sintetica).

| Campo | Valor |
|---|---|
| RUN_ID | pendente |
| runtime_id ref | pendente |
| runtime_id nova | pendente |
| sha256 de report.tsv | pendente |
| Veredito smoke | pendente |
| Veredito aceite | pendente |
| Veredito medicao | pendente |
| Veredito relatorio | pendente |
| Recibo docker-accept | pendente |
| Aprovacao do Gabriel | pendente |
