# Estado: M2 (leitor unico, datas honestas, validacao e validate-deck) - fechamento do epico (M2.V)

Documento de estado do epico M2, indexado por `docs/ESTADO-ATUAL.md` (so o M1.V edita o indice). O detalhe por modulo mora em
`docs/estado/deck_reader.md`, `validation.md` e `validate_deck.md`; este arquivo e a visao do epico e o registro dos gates de verificacao.

## O que o M2 entrega (vivo)
- **Leitor unico** `scripts/lib/deck_reader.R` (ADR-0035, ADR-0037): unico lugar que le o `manifest.json` do deck e que chama `arrow::open_dataset`.
  O manifest (schema v1 fechado) declara o fuso de cada coluna de data-hora (`timezone`) e o `load_label` (`end` | `start`); nunca ha fuso default.
  O rotulo da carga e convertido UMA vez na entrada: internamente tudo e `interval_start`/`interval_end` (`end` => -30 min, `start` => identidade) no fuso da carga,
  independente do fuso/locale da sessao. `--load-label` x manifest diferentes => `contract.load_label_conflict` (o manifest vence quando concorda).
- **Corte intradiario derivado do dado**: `available_until` por area = `interval_end` do ultimo intervalo contiguo desde `O 00:00`; global = minimo; buraco no dia da origem
  => `completude.origin_day_gap`; areas divergentes => aviso `warning.available_until_spread`. Sem relogio, sem flag.
- **Painel do leitor** (M2.7): `prevcarga_read_panel` entrega carga, clima observado e previsto (por area, emissao mais fresca permitida por `weather_issue_cutoff`),
  feriados por area (`prevcarga_read_holidays`, sem corte) e os atributos do deck; area fora da topologia => `topology.area_undeclared`.
- **`weather_issue_cutoff`** (`available_until` | `origin_midnight_utc`): limite de emissao do clima previsto; emissao do dia O depois do limite => aviso `climate_after_cut` no validate-deck.
- **`dates.R`** (M2.3): os tres cortes distintos. `--origin` e o unico flag humano de data; omitido, vale o dia do ultimo intervalo de carga do deck (`origin_source = deck`), nunca o relogio.
  `train_origin` por classe de split (`origin_agnostic`: `O-1-H`; `origin_scoped`: `O`, 1 re-treino por dia) e `last_data_day = O-1-data_lag_days` (teto O-1).
  `prevcarga_resolve_cuts(..., truncate_at)` leva o mesmo `truncate_at` do painel ao leitor: corte da manha (O 00:00) num store que ja tem o dia O; o corte de treino nao muda.
- **Grade flat-48**: o store escreve 48 intervalos por dia, contados por `interval_start`; dia de 46 ou 47 e buraco (`completude.day_incomplete`). A contagem independe de DST.
- **`validate-deck`** (`scripts/lib/cli/validate-deck.R`, auto-registrado pela descoberta da fachada): gate-0 do deck com veredito acumulativo; exit 0 limpo, 2 contract,
  3 completude, 4 topology, 5 window, 8 config. `result` com `data_hash` (identidade do deck, model-agnostica: nao muda com `--model`; muda com 1 byte do parquet, com o `load_label` ou com o fuso do manifest).
- **Catalogo distribuido** (barrel por descoberta; nenhuma lista central): `scripts/lib/errors/deck_reader.tsv` (8 ids), `deck_cut.tsv` (3), `dates.tsv` (5), `validation.tsv` (15),
  `store_manifest.tsv` (1, ferramenta `scripts/tools/write_store_manifest.R`) e `topology.tsv` (`topology.area_undeclared`, unico id; nao existe `panel.tsv`).
  `install/error-catalog.tsv` e gerado, nunca versionado.

## Documentacao dos modulos
- `scripts/lib/deck_reader.R`: o cabecalho declara o leitor unico, fuso por lubridate com tz explicito e ASCII puro. Detalhe em `docs/estado/deck_reader.md`.
- `scripts/lib/dates.R`: o cabecalho descreve os tres cortes. O modulo nao conhece nome de modelo: `data_lag_days` chega por argumento (descriptor do plugin).
- `scripts/lib/validation.R`: o cabecalho descreve o veredito acumulativo, as classes (contract, completude, topology, window) e os avisos. Detalhe em `docs/estado/validation.md`.
- A visao transversal dos modulos de `scripts/lib` fica em `scripts/lib/README.md` (do M1.V); este epico nao a edita.

## Gates de verificacao do M2 (M2.V)
- `scripts/lib/tests/test-validate-deck-cli.R` (PROCESSO): `Rscript --vanilla <copia>/scripts/prevcarga validate-deck` sobre instalacao-copia com historicos pequenos:
  exit 0/2/3/4/5/8 por classe, texto e json (envelope unico com `exit_code == status`, stderr vazio no json, `prevcarga: erro [<classe>] (exit <n>)` no texto, nenhum byte > 127, nenhum traceback);
  shell sujo (TZ/LC_ALL/BLAS) simulado por wrapper R portavel => json identico ao limpo salvo `run_id`; CONTROL com a mesma sujeira sem `prevcarga_runtime_enforce()` desloca o instante (hora 21);
  parquet corrompido => exit 2 `parquet_unreadable` com `where` relativo; `data_hash` entre processos/modelos; `describe --origin 2026-10-01` por membro.
- `scripts/lib/tests/test-deck-matrix.R` (in-process): `MUTATIONS` com uma linha por `what` (deck limpo com zero findings x uma mutacao isolada), teste de cobertura por parse de
  `.pc_finding`/`prevcarga_abort`/`prevcarga_warn`; excecao justificada: `constraint_violated` (so do painel).
- `scripts/lib/tests/test-static-greps-m2.R`: gates de fonte do M2 com isca CONTROL (leitor unico, deslocamento de carga, relogio, env, data-hora base, nomes legados, chave `corte`,
  `--origin` como unico flag de data, nenhum teste sob `data/`).
- Sub-gate de processo/fonte: `scripts/tools/gates/m2.R` (barrel por dono; roda os tres arquivos acima).
- Windows nativo: nao ha maquina Windows neste ambiente. Os arquivos so usam `file.path`, `tempfile()`, `system2` sem shell e wrapper R (sem `env VAR=`); o braco Windows do AC 15
  fica pendente de execucao real e nao e declarado verde por inferencia.

## Pendencias conhecidas (nao decididas aqui)
- **Receita do `data_hash`** sem ADR proprio: arquivos, ordem, manifest e `load_label` entram hoje por implementacao (`prevcarga_data_hash`, provenance.R); falta o ADR que a fixa.
- **`data_lag_days` dos descriptors** chega ao `describe`/`validate-deck` por processo (`prevcarga_plugin_data_lags`, registry.R): LGBM O-3, RF O-1, ambos `descriptor`
  (gate de processo do M2.V); membro sem plugin fica `default`.
- O resultado do `validate-deck` nao depende do fuso da sessao (todo calculo usa `tz` explicito): a sujeira do shell so e observavel na sonda de instante, por isso o CONTROL e a sonda e nao o json.
- `docs/roadmap/M2-DATAS.md` ja foi arquivado em `docs/_archive/roadmap/M2-DATAS.md` (somente leitura); nada a apagar neste PR.
- Smoke do `validate-deck` sobre o store real segue pendente do `manifest.json` do store (M2.6); nada foi gravado em `data/`.
