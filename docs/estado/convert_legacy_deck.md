# Estado: convert_legacy_deck (deck legado do ONS -> deck novo)

- Fonte: `s3://ons-dl-prod-containers/prevcarga/decks/<AREA>/*.csv.gz` (profile AWS `analytics-prod`; 28 areas = 24 folhas + SECO/S/N/NE;
  `;` + virgula decimal; atualizado diariamente). Download e passo separado, fora do pacote:
  `aws --profile analytics-prod s3 sync s3://ons-dl-prod-containers/prevcarga/decks/ <dir> --exclude "*" --include "*/CARGASHHIST.csv.gz"
  --include "*/TEMPHIST.csv.gz" --include "*/TEMPPREVHIST.csv.gz" --include "*/FERIADOS.csv.gz" --include "*/HORAVERAO.csv.gz"`
  (`MDLHIST*` ~5 GB nao sao usados; `_backup/` e `source/` ficam de fora).
- Conversor: `scripts/tools/convert_legacy_deck.R --source <dir> --dest <dir-vazio> [--areas A,B] [--load-label end|start]` (dev, R 4.5.3:
  `/opt/R/4.5.3/bin/Rscript`; o Rscript 4.2.3 do PATH aborta no runtime por falta do RhpcBLASctl). ~7 min para as 28 areas.
- Rotulo da carga: o CSV rotula pelo INICIO do intervalo (`00:00` = 00:00-00:30; Gabriel 2026-11-04). O deck novo grava no FIM (+30 min) e
  declara `load_label: end` (o leitor desloca -30). Particao `year` = ano do inicio do intervalo.
- Regras espelham `scripts/data-prep/migrate_legacy_data.py` do oraculo (gerou o store `data/`), com 3 desvios decididos pelo Gabriel em 2026-11-04 (o 2 revisto em 2026-11-11/12):
  1. carga <= 0 vira faltante; meia hora isolada faltando recebe o ponto medio dos vizinhos (GO 8 negativas 2020-2022, BAOE 2018-02-11 07:30,
     PA 2025-07-30 09:00, MS 14). MS 2018 tem 389 zeros exatos; 375 em blocos ficam fora do deck (dias incompletos em 2018).
  2. temperatura (observada e prevista): desde 2026-11-12 a spline not-a-knot do oraculo deu lugar a desagregaModeloSH do legado
     (`prevcargadessem3/docs/legacy-workflow/PrevCargaDESSEM/R/pkgfun_funcoesApoio.R:388-410`: media de duas `monoH.FC` no eixo de indice,
     ancoras em 2k-1 e 2k) com ajuste por hora ADITIVO `Vout + Vin - Vm` (o legado multiplica) e o par da hora H em (H:00, H:30), inclusive
     a ultima hora (Gabriel 2026-11-11: H = media de [H, H+1)). Desagrega por corrida contigua de horas; meia hora dentro de buraco sai da
     reta entre as meias horas das bordas (MG 2024-2025 tem ~2700 h de buraco); fora dos pontos validos, NA. Teste escopado:
     `sandbox/desagrega/test-desagrega.R`.
  3. TON: reparo add-only de feriados por consenso (>= 19 de 23 folhas), 81 -> 177. O store `data/` nunca recebeu esse reparo (59 feriados).
  Previsao de clima: `issue_time` = `dat_referencia` 00:00 (manifest: UTC, lag 6 h), desagregacao por emissao (regra 2; a emissao que termina
  em O+9 23:00 ganha o 23:30; a reta de buraco cobre as emissoes trihorarias 2020-05-01/09 e 2024-02-20/21).
  `load_tiers` <- `PATAMARES` no formato novo (`Patamar;Intervalo;DiaSemana;Mes;HoraIni;HoraFim`, 312 linhas): UM arquivo para todas as areas
  (Gabriel 2026-11-04), `auxiliary/load_tiers/data.parquet` sem particao, colunas `tier, interval, weekday, month, hour_start, hour_end`. Na fonte
  so SECO/S/N/NE trazem o formato novo (identicos); as folhas ainda tem o arquivo velho (`Hora` x estacao, o do store `data/`), ignorado.
  Nenhum modulo le hoje (o motor DESSEM vendorizado so repassa `PATAMARES` em `DADOSLIDOS`). As series de perda (`PESE/PES/PEN/PENE`) nao sao geradas: o k-hat do reconcile usa so
  SS_true/BU_true (subsistema / soma das folhas), a perda so existe como nome na topologia.
- Equivalencia contra o store `data/` (2018-01-01..2025-11-21, 28 areas), medida com a spline antiga: previsao de clima bit-a-bit (<= 1.4e-14),
  feriados iguais (exceto TON +96), observado igual exceto a borda final do store antigo e os buracos. Com a desagregaModeloSH o clima deixa de
  ser igual ao `data/` por definicao (|dif| mediana 0,13 C, p95 ~0,6); a propriedade passa a ser media do par (H:00, H:30) = horario de origem
  (max 1,4e-14 no previsto, 7,1e-15 no observado, deck 2026-11-04). Carga identica ate 2022; de 2023 em diante o ONS
  reintegrou a carga verificada (`din_atualizacao` 2026-08), entao a diferenca e da fonte.
- Deck de 2026-11-04: `/opt/source/prevcarga-R/decks/20261104` (448 MB; zip em `decks/deck-20261104.zip` do repo, gitignored, sha256 91b077db...; CSVs em `/opt/source/prevcarga-R/legacy-decks/20261104`), carga ate
  2026-11-04 08:00. `validate-deck --origin=2026-11-04`: OK (so avisos `outlier`), `available_until` 08:00. Esse deck e da spline antiga.
- Deck de 2026-11-04 com a desagregaModeloSH: `/opt/ons-source/prevcarga-runs/deck-20261104-desagrega` (327 MB; mesmos CSVs e argumentos).
  Linhas: carga/feriados/dst/patamares iguais; observado +28 (23:30 final por area); previsto +78.345 (um 23:30 por emissao). Emissao de O em
  todas as 28 areas: 10 dias x 48 instantes, ultimo O+9 23:30. `validate-deck --origin=2026-11-04`: OK, achados identicos ao deck antigo.
