# Estado: M6 -- Forecast, export e reporting full-R (fechamento do epico, M6.11)

Documento de estado do EPICO M6. O detalhe por modulo mora em `docs/estado/forecast.md` (M6.4/M6.5/M6.9/M6.10/M6.6), `export.md` (M6.2/M6.7),
`build_long.md` (M6.1) e `verificar.md` (M8.7); aqui ficam o mapa, o layout, a composicao com o Reconcile, o wall-clock e as decisoes L2.
`docs/ESTADO-ATUAL.md` nao e editado por este ticket (indice mantido pelo M1.V).

## Modulos vivos
| Modulo | Arquivo | Papel |
|---|---|---|
| build_long_output | `scripts/lib/build_long_output.R` | `prevcarga_build_long`: conversor unico e puro da entrega do membro para o LONG 8-col (`forecast_origin` texto `yyyy-MM-dd`) |
| exports | `scripts/lib/exports.R` | `prevcarga_export` (copia bytes do scratch), `prevcarga_forecast_versao`, `prevcarga_export_key`, manifest.json; chave imutavel |
| forecast | `scripts/lib/forecast.R` | `prevcarga_forecast`: rodada operacional diaria (gate-0 -> plano -> membros -> LONG + combina -> reconcile -> chave -> scratch -> export) |
| reporting | `scripts/lib/reporting.R` | relatorio HTML/PDF deterministico, depois do export; falha => `partial.report_failed` (exit 14) com a entrega intacta |
| verbos | `scripts/lib/cli/forecast.R`, `export.R`, `features.R` | fachada docopt fina (dono M6); `--origin` e o unico flag de data, sem `--corte` |
| gates | `scripts/tools/gates/m6_long.R`, `scripts/tools/bench_forecast.R` | 7 sub-gates `m6_*` do LONG e o bench do NFR de 30 min |

forecast.R so COMPOE: nenhum calculo de k-hat/BU/SIN, de pesos, de corte de treino nem de hora; nunca treina o Reconcile nem promove no registry
(greps do `test-m6_greps.R`).

## Layout da entrega e dos relatorios
- `forecasts/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/` em `<state_dir>`: data da ORIGEM, hora do `available_until` no fuso da carga, `<versao>` = 12 hex
  de `prevcarga_forecast_versao`. Dentro: `SIN.parquet`, `subsistemas/<ss>.parquet`, `folhas/<a>.parquet` (folhas soltas sempre; cobertas so com
  `--niveis=tudo`), `membros/<m>/<a>.parquet` (so com `--incluir-membros`), `manifest.json` (unico campo nao deterministico: `gerado_em`) e
  `run_manifest.json` (identidade da rodada, lido pelo `verificar`; fora de `arquivos[]`).
- Imutavel: mesma chave + mesmos bytes = no-op (`ja_publicado`); qualquer diferenca = `export.key_conflict` (exit 9). A `versao` carrega o pacote, os membros
  (`v_<hash do artefato>:<sha256 do y_pred entregue>`), os pesos da combina, o perfil k-hat por subsistema, o hash da topologia, o `load_label` e a regra de
  clima (`weather_issue_cutoff`): outra regra de clima ou outro rotulo declarado => outra versao e outra chave, as duas preservadas.
- `reports/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/report.html|report.pdf` em `<state_dir>`: fora de `forecasts/`; a entrega e identica com e sem relatorio.
- Scratch `<state_dir>/scratch/<run_id>/` recriado sempre (`run_id` = hash da chave), nunca consultado como entrega.

## Composicao com o Reconcile (epico R)
- `prevcarga_reconcile_plan` dita `closed/stray/sin_closed`; `prevcarga_reconcile_apply(long_leaves[covered], ctx, subsystems = closed, khat_version = NULL,
  eval = FALSE)` roda UMA vez, em memoria, antes de qualquer scratch: erros (`honesty.khat_after_origin` exit 15, `state.khat_missing` exit 13,
  `contract.*` exit 2) sobem sem deixar `forecasts/` nem `scratch/`.
- O perfil k-hat `latest` e treinado FORA do forecast (`prevcarga_reconcile_train`, verbo `reconcile --mode train`); `khat_origin == O` sempre. O
  `result$khat` (`ss`, `versao`, `khat_origin`, `khat_age_days`) e copiado do reconcile para o `manifest.json` e entra na `versao`.
- A honestidade do clima e verificada com `prevcarga_reach_control` (R.4): `test-forecast.R` roda o forecast como modulo e veredita HONESTO nas duas regras.

## Wall-clock do NFR de 30 min (bench M6.6)
- Alvo FIXO: 1800 s para `prevcarga_forecast(report = TRUE)` completo (todas as folhas, todos os membros, k-hat e pesos ja treinados).
- **Medicao REAL: pendente do operador.** Ela exige o deck real (o store `data/` nao tem `manifest.json`; as camadas de paridade montam um overlay a
  partir de `config/deck-manifest.store.json`), os artefatos origin_agnostic e o k-hat `latest` treinados e o tempo do oraculo na mesma maquina. Nenhum
  numero e inventado aqui; quando medido, preencher a linha abaixo.
- Como o operador mede (maquina de producao, instalacao com o bundle):
  1. `prevcarga train-models` (origin_agnostic) e `prevcarga reconcile --mode train --origin <O>` (k-hat) ja feitos para a origem O;
  2. medir o comando de previsao do ORACULO (`/opt/ons-source/prevcargadessem3`, somente leitura) na mesma maquina => `<S>` segundos;
  3. `Rscript --vanilla scripts/tools/bench_forecast.R --origin <O> --reference-seconds <S> --state <state_dir> --format json`
     (`--data <deck>` e `--config <config.yaml>` se fora do padrao; `data/` e conferido por sha256 antes/depois, o `--state` e copiado para tempdir);
  4. veredito `bloqueante` (exit 1, "NFR 30 min estourado") se `segundos > 1800`.

| segundos | razao vs referencia | referencia_s | host | R | data da medicao |
|---|---|---|---|---|---|
| (pendente: medir no deck real) | | | | | |

## Verificacao ponta a ponta (M6.11)
- `test-forecast.R` (deck sentinela do `fixture_store.R`, k-hat `latest` pelo `prevcarga_reconcile_train` real, pesos da combina treinados no setup; membros RF/LGBM/HW
  com espiao do infer): dois modos de `load_label` (end x start deslocado => as mesmas folhas/subsistemas/SIN; CONTROL: deslocado lido como end e acusado,
  `available_until` 08:30), alinhamento (1o rotulo O 09:30 em folha/subsistema/SIN/membro, 48 rotulos por dia completo, o valor decodifica o proprio intervalo apos
  combina, reconcile e export; CONTROL: saida da combina e do reconcile deslocada 1 slot reprova; `forecast_origin` texto; writer-fixture com Date => `contract.long_schema`),
  clima nas duas regras com `prevcarga_reach_control` (HONESTO; VAZA e INDETERMINADO reprovam; versoes/chaves distintas e preservadas, regra efetiva no manifest) e a rodada
  das 00h (`available_until` = O 00:00, 1a entrega O 00:30 sem carga do dia O; CONTROL: dia O completo recusado, `completude.forecast_origin_day_complete`).
- `test-forecast_e2e.R` (8 membros REAIS em processo filho; fases `weights00`/`run00` = rodada das 00h com os 8 membros entregando a partir de O 00:30; ~11 min).
- `test-forecast_shell.R`: a FACHADA real (`Rscript --vanilla scripts/prevcarga forecast --origin O --format json`) com TZ=America/Sao_Paulo, LC_ALL=pt_BR.UTF-8,
  OPENBLAS_NUM_THREADS=4 x shell limpo (variaveis removidas): entregas byte-identicas (sha256 por arquivo e manifest sem `gerado_em`), tambem entre duas rodadas.
- `test-m6_greps.R`: greps portaveis (`pc_grep_code`) dos 4 arquivos do M6 com CONTROL por padrao, e a conferencia das secoes deste documento.
- Achado: o `load_label` DECLARADO entra no `manifest.json`, no `run_manifest.json` e na `versao` (M6.7) e o perfil k-hat o carrega na proveniencia; por isso dois decks que
  so diferem no rotulo (end x start deslocado) entregam os MESMOS parquets (sha256 por arquivo) e o mesmo manifest, exceto `load_label`, `versao`, `khat[].versao` e `gerado_em`.

## Decisoes taticas L2 do epico
- Rodada diaria = UMA origem O (o backtest e quem itera origens). Gate-0 recusa so `available_until >= O+1 00:00` (`completude.forecast_origin_day_complete`, exit 3);
  `available_until = O 00:00` (rodada das 00h) e qualquer corte intradiario de O sao validos (decisao do Gabriel, 2026-10-30).
- Horas incompletas sao descartadas em toda serie, clima previsto incluido (divergencia deliberada do oraculo, documentada por plugin).
- Clima: regra `available_until` aplica o lag de publicacao do deck (`weather_forecast_publication_lag_hours`: o store declara 6, decks sinteticos 0), regra
  `origin_midnight_utc` nao tem lag; a regra efetiva vai no manifest e na `versao`.
- origin_agnostic so le o artefato (ausente => `state.artifact_missing`, exit 13, nunca treina); origin_scoped treina UMA vez por celula quando nao ha artefato da origem.
- `niveis`/`incluir_membros` nao entram na `versao`: a mesma chave com outro conjunto de arquivos e `export.key_conflict` (aberto desde o M6.4; solucao fica com o dono do M6.7).
- `--scratch` (nao `--from`) no verbo `export`; `--origin` e o unico flag de data; sem `--corte`. Relatorio so depois do commit da entrega; falha => exit 14, entrega mantida.
- `run_manifest.json` em toda entrega (identidade para o `verificar`); `bundle_id` = `prevcarga_bundle_id`; `runtime_id` = R + plataforma (deterministico).
- Terminal e mensagens so ASCII; nenhum relogio, variavel de ambiente, S3/HTTP ou `system()` nos 4 arquivos do M6 (greps).
