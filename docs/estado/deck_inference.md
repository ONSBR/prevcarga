# Estado: deck-inference (E2-T4; regdin-linear-refactor; ADR-044, ADR-046)

- Verbo `prevcarga deck-inference [--area=<a>...] [--from-topology] --out=<dir>` (`scripts/lib/cli/deck-inference.R`, auto-registrado pelo barrel `cli/*.R`,
  `deck_roles = "train"`). `--origin`, `--area` e `--out` ja constam das opcoes globais/Options de outros verbos (duplicata torna o docopt ambiguo): a usage cita
  so `--area`, `--from-topology` e `--out`; a origem e obrigatoria (`config.usage`, `--origin`). `--area` aceita folha ou subsistema (expande para as folhas);
  sem `--area` = todas as folhas da topologia; fora dela `topology.area_undeclared` (exit 4).
- Motor: `scripts/lib/deck_inference.R`, `prevcarga_deck_inference_build(ctx, origin, areas, out, from_topology = FALSE)` -> `list(result, warnings)`. Nenhum nome de modelo,
  nenhum literal de janela, nenhuma variavel de ambiente.

## Fluxo
1. `--out` existente e nao vazio => `contract.deck_out_not_empty` (exit 2, `where` = `--out`) ANTES de qualquer leitura; `--out` vazio e aceito.
2. `prevcarga_resolve_cuts(ctx, origin)` sobre o deck de treino: `available_until`, `weather_issue_limit`, fuso e `load_label`. `train_deck_hash` = `prevcarga_data_hash` do deck de treino.
3. `prevcarga_inference_needs_resolve(..., on_missing = "topology")`: por membro x area, o artefato; artefato ausente (`state.*`) ou sem `inference_needs` cai em
   `prevcarga_inference_needs_of` sobre o `run.yaml`, linha com `source = "topology"` e aviso `warning.inference_needs_from_topology` (`where` = `membro/area`, `details` member, area).
   `--from-topology` nao le artefato. O default `on_missing = "abort"` segue o de sempre (validacao e forecast). Fonte no manifest: `artifacts`, `topology` ou `mixed`.
4. `prevcarga_inference_needs_aggregate`: uniao por dataset e area. Recorte por area, linhas CRUAS do store (colunas, tipos e `timestamp[ns]` sem fuso; particao `year` da fonte):
   carga e observado em `[O - days_before, O]` ate `available_until` (mesmo predicado do leitor); previsto so as chaves `(area, issue_time, valid_time)` devolvidas por
   `prevcarga_read_forecast_by_rule` com o `weather_issue_limit` da origem; feriados `[O + from, O + to]`. Area sem carga na janela => `completude.no_load` (exit 3; `details` area, from, to).
5. Anexo de feriados (so com need agregada `holiday_annex`): ver abaixo.
6. Escrita a mao `<dataset>/area_code=<a>/year=<y>/part-0.parquet` (`arrow::write_parquet`, `snappy`, `version 2.6`, sem dicionario), linhas em ordem radix das chaves
   (carga/observado `area_code,timestamp`; previsto `area_code,issue_time,valid_time`; feriados `area_code,date`). Em `<out>.partial-<hash>` irmao (hash = sha256 de origem + `train_deck_hash` + needs agregadas);
   `manifest.json` por ultimo; `--out` vazio removido e UM `file.rename`. Falha no meio => `unlink` do `.partial` e `io.out_unwritable`.

## Manifest
Chaves em ordem fixa, sem relogio, host nem caminho absoluto: `schema_version`, `kind` (`inference`), `timezone`, `load_label` (o efetivo, explicito), `weather_forecast_publication_lag_hours`
(fuso e atraso copiados do deck de treino) e `inference` = `origin`, `available_until` (AAAA-MM-DDTHH:MM:SS no fuso da carga), `train_deck_hash`, `windows` (dataset -> `{from, to}`,
uniao das areas; `load`, `weather_observed`, `weather_forecast` e `holidays`), `holiday_annex` (so quando ha anexo) e `inference_needs_source`. A chave nova `inference_needs_source` entrou
nas opcionais do bloco `inference` de `deck_reader.R` (enum `artifacts|topology|mixed`).

## Anexo de feriados (decisao do Gabriel; resolucao 01M7FHWSY20K1FXP3RZNT0XNW0; ADR-044 atualizada)
O ticket previa um dataset `holiday_annex` particionado por area e ano e o campo `inference.holiday_annex` booleano. Foi substituido pelo layout que o leitor do E3-T2
(`prevcarga_read_holiday_annex`, `docs/estado/deck_reader.md`) le: um SUBDECK `<out>/holiday_annex/` com `manifest.json` proprio (`{schema_version, kind: "holiday_annex"}`; fuso, `load_label` e
`train_deck_hash` vem do deck dono), `load/` e `weather/observed/` em hive `area_code=<a>/year=<y>/part-0.parquet` (so dias de ilha, nenhum dia >= O) e `islands/area_code=<a>/part-0.parquet`
(uma linha por data de feriado: `island_id, from, to, holiday_date`). O manifest do deck aponta `inference.holiday_annex = "holiday_annex"` (caminho relativo ao deck); sem need de anexo o subdeck
nao existe e a chave nao e gravada (o leitor recusa booleano). As ilhas vem de `prevcarga_read_holiday_annex` sobre o deck de treino com `prevcarga_dessem_window(O, "ano_civil")` (corte em O-1).
Sem linhas de carga/observado nas ilhas, grava-se um parquet de 0 linhas so para o schema (o leitor nao abre dataset sem arquivo); sem ilhas, `islands/` fica sem particoes (zero ilhas).
Cada deck carrega uma copia propria do anexo. O `data_hash` do deck gerado cobre os quatro datasets canonicos; o anexo entra em `files` (sha256 por arquivo) mas nao no `data_hash`.

## Saida
JSON (envelope ADR-040): `out`, `origin`, `available_until`, `train_deck_hash`, `data_hash`, `inference_needs_source`, `windows` (dataset -> area -> `{from, to}`), `files` (`path`, `sha256`, `rows`; `manifest.json` incluso, ordem radix) e
`needs_from_topology` (`[{member, area}]`). O coletor de avisos da fachada (`cli.R`) guarda so `id` e mensagem; por isso member/area tambem vao no `result`. Texto ASCII: cabecalho, uma linha por dataset x area (from/to/linhas) e `OK`.

## Determinismo
Dois geradores com as mesmas entradas dao arquivos com sha256 identico, manifest incluido. Os bytes do parquet dependem da versao do arrow: pinados ao arrow da imagem do bundle (renv.lock), nao entre versoes de arrow.

- Catalogo: `scripts/lib/errors/deck_inference.tsv` (`contract.deck_out_not_empty`, `warning.inference_needs_from_topology`; `completude.no_load` ja existia em `deck_cut.tsv`).
- Testes: `scripts/lib/tests/test-deck-inference.R` (deck de treino sintetico + artefatos-brinquedo; verbo em processo via `prevcarga_main`).
