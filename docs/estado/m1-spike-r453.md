# Estado: spike R 4.5.3 (M1.7, ADR-0036)

Ferramenta: `scripts/tools/spike_r453.R` (dev-only Linux, fora do bundle). Executa o oraculo `prevcargadessem3` intocado
(`jobs/run_job.sh` em modo local) em R 4.2.3 x R 4.5.3, mesma origem (2024-06-01), area SECO, mesmo deck; saidas em `--out`.
O oraculo roda com `TZ=UTC` (Dockerfile dele; sem isso o join de previsao de temperatura falha com o store naive visto em -3h).

## Resultado

**Braco estrito (mesmo renv.lock do oraculo, snapshot 2023-04-21, em 4.5.3): NAO compila.** Falham bit 4.0.5, data.table 1.14.8,
digest 0.6.31, jsonlite 1.8.4, randomForest 4.7-1.1, ggplot2 3.4.2 (erros de C "expected expression before int/char/double" com os
headers do R 4.5) e, em cascata, arrow, lightgbm, forecast, fable/fabletools/feasts, rlist, quantmod, tseries. Os 5 modelos
saem `compile_failed`; veredito do runner: "rever ADR-0036".

**Braco relock (`--relock-b=true`: mesmos nomes de pacote, versoes mais novas do snapshot noble/latest de 2026-10-27, em 4.5.3):**
os 5 modelos rodam; veredito do runner: "ADR-0036 confirmada" (tolerancia 0,1%).

| modelo | n | max_abs | max_rel | sha256 igual |
|---|---|---|---|---|
| random-forest | 432 | 0 | 0 | sim |
| lgbm | 79 | 0 | 0 | sim |
| holt-winters | 384 | 0 | 0 | sim |
| dessem-linear | 463 | 7.3e-9 | 2.2e-13 | nao |
| dessem-regdin | 463 | 0.416 | 1.3e-5 | nao |

O braco relock muda pacotes e R juntos: o Delta nao e atribuivel so ao R.

## Decisao (Gabriel, relayed pelo orquestrador)

R 4.5.3 roda com um renv.lock NOVO, tirado do braco relock. RF, LGBM e HW permanecem em Delta=0 estrito.
**dessem-regdin PODE divergir do oraculo** (max_rel 1.3e-5): excecao ao gate Delta=0, atribuida a atualizacao de
WeightSVM 1.7-11 -> 1.7-16, e1071 1.7-13 -> 1.7-17 e fable 0.3.3 -> 0.5.0.

**Pendente (Gabriel): dessem-linear** mede max_rel 2.2e-13 (max_abs 7.3e-9), sha256 diferente. A decisao relatada nao o cobre;
fica registrado so o valor medido, NAO aceito. A emenda da ADR-0036 e do gate A/B e seguimento do Gabriel (nenhuma ADR foi editada).

## Onde mora o conjunto relock

Nao ha renv.lock novo entregue por este ticket. O conjunto (nome -> versao instalada em 4.5.3) e regenerado por
`spike_r453.R --relock-b=true` e fica em `<out>/lib-4.5.3-relock/restore.tsv` (versao do lock no campo `version`, instalada no `detail`).
Versoes da rodada de 2026-10-27 (pacote do lock -> relock): arrow 11.0.0.3 -> 26.0.0; lightgbm 3.3.5 -> 4.7.0;
randomForest 4.7-1.1 -> 4.7-1.2; data.table 1.14.8 -> 1.18.6.1; jsonlite 1.8.4 -> 2.0.0; digest 0.6.31 -> 0.6.39;
ggplot2 3.4.2 -> 4.0.3; dplyr 1.1.1 -> 1.2.1; forecast 8.21 -> 9.0.2; fable 0.3.3 -> 0.5.0; fabletools 0.3.3 -> 0.8.0;
feasts 0.3.1 -> 0.5.0; tsibble 1.1.3 -> 1.2.0; WeightSVM 1.7-11 -> 1.7-16; e1071 1.7-13 -> 1.7-17; lubridate 1.9.2 -> 1.9.5;
rlang 1.1.0 -> 1.3.0; vctrs 0.6.2 -> 0.7.3; R6 2.5.1 -> 2.6.1; Rcpp 1.0.10 -> 1.1.2; bit 4.0.5 -> 4.6.0; bit64 4.0.5 -> 4.8.6.
Gerar o renv.lock definitivo de 4.5.3 (`renv::snapshot`) e trabalho dos epicos de modelo / M8, nao deste spike.
