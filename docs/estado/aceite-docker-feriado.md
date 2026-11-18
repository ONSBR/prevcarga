# Estado: medicao por grupo de dia (feriado, vizinho, comum) e por origem (E8-T4)

Pos-processamento da medicao do aceite em Docker local (RUN_ID 20261117T234854Z-a370b08d), pedido do Gabriel em 2026-11-17. Linha de base para uma proposta FUTURA: tirar o perfil de feriado do DESSEM e passar para um modelo de feriados separado. Essa proposta NAO esta implementada nem decidida aqui.

Fonte: so as previsoes (LONG) que a medicao ja gerou e a verdade fresca do store (`prevcarga_read_actual`, somente leitura). Nenhuma rodada nova, nenhuma alteracao do motor. Ferramenta: `scripts/tools/accept_feriado.R` (teste escopado `scripts/lib/tests/test-accept-feriado.R`). Saidas em `<out>`: `feriado.tsv`, `feriado_fallback.tsv`, `porigem.tsv`.

## Definicoes

- Membros DESSEM medidos: dessem-linear = LIN_MED, LIN_MAX, LIN_MXMN e LIN_UNI (UNI so existe na nova); dessem-regdin = RGD_MED e RGD_MAX. O dessem-regdin NAO tem as variantes MXMN nem UNI na topologia atual; nao ha o que medir.
- Dia = data de `interval_start` (carga de fim de intervalo, leitor desloca 30 min) no fuso da carga do store (Etc/GMT+3).
- Grupo `feriado`: a data consta em `auxiliary/holidays` da area (tabela inteira do store, todos os tipos). Grupo `vizinho`: nao e feriado e e o dia anterior ou o posterior (+-1 dia, os dias LAGDS) de um feriado da mesma area. Grupo `comum`: o resto. Os lags especificos de cada tipo de feriado do motor (lags 2 e 7 de alguns tipos) NAO entram: vizinho aqui e so +-1 dia.
- "dias" = pares distintos (origem, data) com previsao pareada. Chaves pareadas ref x nova com y_true e y_pred finitos (as mesmas do `report.tsv`); LIN_UNI entra so pela nova.
- Duas coberturas. `d1`: so D+1 (a mesma do `report.tsv`; 12 origens da medicao dao no maximo 12 dias por area, e nenhum cai em dia vizinho: o grupo vizinho fica vazio). `horizonte`: todos os D+k do LONG (10 dias por origem), onde o grupo vizinho aparece.
- MAPE e WMAPE em %. Celula `F`: a area tem celula de feriado que entregou pelo `warning.engine_tune_fallback` (lista abaixo). A marca vale para o par (membro de dessem-linear, area); a ref nunca tem o fallback (ele e da nova).
- Ausencias: unidade recusada (ver `aceite-docker.md`) tira o membro da area naquela origem; por isso o numero de dias varia entre membros e areas (MS tem poucos dias (MS tem 15 recusas na nova e 13 na ref em `index.tsv`: as 14 de MS em 2023 mais a de 2024-05-29 do regdin; causa: zeros na fonte de MS em jun-dez/2018 quando val_cargasup < 0, alcancados pela janela de 5 anos da ADR-046; detalhe em `aceite-docker.md`).

## Celulas de feriado que usaram warning.engine_tune_fallback

14 celulas distintas, todas do dessem-linear da NOVA, em 4 areas (AM 1, CE 7, PA 5, RS 1), nas origens 2023-06-07 (Corpus Christi 2023-06-08, 13 celulas) e 2024-05-29 (Corpus Christi 2024-05-30, 1 celula). Cada celula aparece 3 vezes no log (uma por variante treinada). O dessem-regdin nao usou o fallback. A celula GO 2023-06-07 NAO entregou pelo fallback: terminou em `engine.dessem_tune_small_sample` (ver `aceite-docker.md`).

| versao | membro (mdir) | area | origem | celula (data, hora, tipo de dia especial) | ocorrencias no log |
|---|---|---|---|---|---|
| new | dessem-linear | AM | 2023-06-07 | data=2023-06-08 hora=23 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | CE | 2023-06-07 | data=2023-06-08 hora=0 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | CE | 2023-06-07 | data=2023-06-08 hora=1 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | CE | 2023-06-07 | data=2023-06-08 hora=20 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | CE | 2023-06-07 | data=2023-06-08 hora=22 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | CE | 2023-06-07 | data=2023-06-08 hora=23 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | CE | 2023-06-07 | data=2023-06-08 hora=5 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | CE | 2023-06-07 | data=2023-06-08 hora=6 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | PA | 2023-06-07 | data=2023-06-08 hora=0 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | PA | 2023-06-07 | data=2023-06-08 hora=1 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | PA | 2023-06-07 | data=2023-06-08 hora=12 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | PA | 2023-06-07 | data=2023-06-08 hora=3 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | PA | 2024-05-29 | data=2024-05-30 hora=0 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |
| new | dessem-linear | RS | 2023-06-07 | data=2023-06-08 hora=1 tipo_dia_especial=1,2,3,4,5,6,7,8,9,10,11 | 3 |

## Resumo por membro e grupo de dia

### Resumo, cobertura d1, versao new (dias somados nas 8 areas; MAPE e WMAPE = media simples das areas)

| membro | feriado (dias; MAPE; WMAPE) | vizinho | comum |
|---|---|---|---|
| LIN_MAX | 40; 8.13; 7.77 | - | 44; 4.98; 4.93 |
| LIN_MED | 40; 8.48; 8.06 | - | 44; 4.95; 4.87 |
| LIN_MXMN | 40; 8.12; 7.78 | - | 44; 4.88; 4.85 |
| LIN_UNI | 44; 9.95; 9.45 | - | 44; 5.47; 5.43 |
| RGD_MAX | 37; 8.62; 7.91 | - | 44; 4.78; 4.67 |
| RGD_MED | 37; 8.64; 7.84 | - | 44; 4.79; 4.64 |

### Resumo, cobertura d1, versao ref (dias somados nas 8 areas; MAPE e WMAPE = media simples das areas)

| membro | feriado (dias; MAPE; WMAPE) | vizinho | comum |
|---|---|---|---|
| LIN_MAX | 40; 6.17; 5.86 | - | 44; 4.91; 4.84 |
| LIN_MED | 40; 6.28; 5.92 | - | 44; 4.75; 4.65 |
| LIN_MXMN | 40; 6.10; 5.79 | - | 44; 4.92; 4.83 |
| RGD_MAX | 37; 7.16; 6.56 | - | 44; 4.65; 4.57 |
| RGD_MED | 37; 7.32; 6.69 | - | 44; 4.59; 4.48 |

### Resumo, cobertura horizonte, versao new (dias somados nas 8 areas; MAPE e WMAPE = media simples das areas)

| membro | feriado (dias; MAPE; WMAPE) | vizinho | comum |
|---|---|---|---|
| LIN_MAX | 164; 8.14; 7.93 | 75; 5.28; 5.27 | 517; 6.63; 6.44 |
| LIN_MED | 164; 8.10; 7.84 | 75; 5.09; 4.94 | 517; 6.47; 6.23 |
| LIN_MXMN | 164; 8.33; 8.13 | 75; 5.11; 5.07 | 517; 6.87; 6.65 |
| LIN_UNI | 168; 9.29; 8.94 | 83; 6.18; 6.25 | 541; 6.41; 6.28 |
| RGD_MAX | 153; 7.93; 7.59 | 69; 5.38; 5.27 | 507; 6.40; 6.14 |
| RGD_MED | 153; 7.84; 7.44 | 69; 5.03; 4.90 | 507; 6.34; 6.08 |

### Resumo, cobertura horizonte, versao ref (dias somados nas 8 areas; MAPE e WMAPE = media simples das areas)

| membro | feriado (dias; MAPE; WMAPE) | vizinho | comum |
|---|---|---|---|
| LIN_MAX | 164; 6.89; 6.73 | 75; 5.42; 5.36 | 517; 6.51; 6.28 |
| LIN_MED | 164; 6.68; 6.51 | 75; 5.05; 4.93 | 517; 6.36; 6.12 |
| LIN_MXMN | 164; 6.91; 6.75 | 75; 5.32; 5.24 | 517; 6.68; 6.44 |
| RGD_MAX | 153; 7.31; 7.01 | 69; 5.17; 5.08 | 507; 6.33; 6.07 |
| RGD_MED | 153; 7.31; 6.98 | 69; 4.90; 4.79 | 507; 6.17; 5.88 |

## Tabelas por membro e area

### Cobertura d1, versao new (dias; MAPE; WMAPE; F = celula(s) com warning.engine_tune_fallback na area)

| membro | area | feriado | vizinho | comum |
|---|---|---|---|---|
| LIN_MAX | AM | 5 d; 7.85; 8.01 F | - | 6 d; 5.12; 4.88 |
| LIN_MAX | CE | 5 d; 8.34; 8.15 F | - | 6 d; 4.16; 4.22 |
| LIN_MAX | GO | 5 d; 9.43; 8.98 | - | 6 d; 5.03; 5.20 |
| LIN_MAX | MS | 3 d; 14.01; 13.13 | - | 2 d; 11.18; 10.41 |
| LIN_MAX | PA | 5 d; 2.92; 2.89 F | - | 6 d; 1.90; 1.94 |
| LIN_MAX | RJ | 5 d; 8.87; 8.20 | - | 6 d; 4.48; 4.58 |
| LIN_MAX | RS | 6 d; 7.88; 7.42 F | - | 6 d; 4.74; 4.85 |
| LIN_MAX | SP | 6 d; 5.72; 5.41 | - | 6 d; 3.21; 3.33 |
| LIN_MED | AM | 5 d; 10.81; 10.75 F | - | 6 d; 5.32; 4.88 |
| LIN_MED | CE | 5 d; 8.43; 8.22 F | - | 6 d; 4.58; 4.63 |
| LIN_MED | GO | 5 d; 9.94; 9.37 | - | 6 d; 4.61; 4.77 |
| LIN_MED | MS | 3 d; 14.29; 13.41 | - | 2 d; 10.70; 9.86 |
| LIN_MED | PA | 5 d; 2.90; 2.86 F | - | 6 d; 2.19; 2.23 |
| LIN_MED | RJ | 5 d; 7.88; 7.13 | - | 6 d; 5.06; 5.16 |
| LIN_MED | RS | 6 d; 8.16; 7.60 F | - | 6 d; 4.23; 4.36 |
| LIN_MED | SP | 6 d; 5.44; 5.12 | - | 6 d; 2.93; 3.03 |
| LIN_MXMN | AM | 5 d; 7.93; 8.19 F | - | 6 d; 5.24; 4.95 |
| LIN_MXMN | CE | 5 d; 8.33; 8.14 F | - | 6 d; 4.49; 4.55 |
| LIN_MXMN | GO | 5 d; 9.22; 8.76 | - | 6 d; 4.66; 4.81 |
| LIN_MXMN | MS | 3 d; 14.53; 13.65 | - | 2 d; 9.61; 9.07 |
| LIN_MXMN | PA | 5 d; 2.94; 2.91 F | - | 6 d; 2.12; 2.14 |
| LIN_MXMN | RJ | 5 d; 8.12; 7.51 | - | 6 d; 4.95; 5.00 |
| LIN_MXMN | RS | 6 d; 8.24; 7.71 F | - | 6 d; 4.84; 5.06 |
| LIN_MXMN | SP | 6 d; 5.68; 5.36 | - | 6 d; 3.15; 3.24 |
| LIN_UNI | AM | 6 d; 13.63; 13.31 F | - | 6 d; 6.23; 5.75 |
| LIN_UNI | CE | 6 d; 7.38; 7.19 F | - | 6 d; 3.68; 3.62 |
| LIN_UNI | GO | 5 d; 11.59; 10.68 | - | 6 d; 3.94; 4.14 |
| LIN_UNI | MS | 3 d; 16.78; 15.46 | - | 2 d; 9.13; 9.08 |
| LIN_UNI | PA | 6 d; 3.06; 3.06 F | - | 6 d; 2.06; 2.11 |
| LIN_UNI | RJ | 6 d; 11.19; 10.44 | - | 6 d; 10.26; 10.17 |
| LIN_UNI | RS | 6 d; 9.40; 9.34 F | - | 6 d; 4.82; 4.86 |
| LIN_UNI | SP | 6 d; 6.55; 6.14 | - | 6 d; 3.63; 3.75 |
| RGD_MAX | AM | 5 d; 6.58; 6.48 | - | 6 d; 5.47; 5.16 |
| RGD_MAX | CE | 5 d; 8.59; 8.10 | - | 6 d; 4.01; 4.00 |
| RGD_MAX | GO | 5 d; 8.66; 8.07 | - | 6 d; 3.96; 4.14 |
| RGD_MAX | MS | 2 d; 17.08; 14.56 | - | 2 d; 10.41; 9.37 |
| RGD_MAX | PA | 5 d; 4.75; 4.81 | - | 6 d; 2.43; 2.43 |
| RGD_MAX | RJ | 5 d; 7.27; 7.04 | - | 6 d; 5.59; 5.55 |
| RGD_MAX | RS | 5 d; 8.71; 7.43 | - | 6 d; 3.21; 3.35 |
| RGD_MAX | SP | 5 d; 7.31; 6.79 | - | 6 d; 3.19; 3.31 |
| RGD_MED | AM | 5 d; 8.62; 8.29 | - | 6 d; 6.20; 5.75 |
| RGD_MED | CE | 5 d; 8.77; 8.19 | - | 6 d; 4.07; 4.07 |
| RGD_MED | GO | 5 d; 8.27; 7.66 | - | 6 d; 3.70; 3.86 |
| RGD_MED | MS | 2 d; 16.21; 13.29 | - | 2 d; 10.14; 9.12 |
| RGD_MED | PA | 5 d; 4.76; 4.83 | - | 6 d; 1.87; 1.88 |
| RGD_MED | RJ | 5 d; 7.05; 6.72 | - | 6 d; 5.80; 5.73 |
| RGD_MED | RS | 5 d; 8.31; 7.10 | - | 6 d; 2.85; 2.95 |
| RGD_MED | SP | 5 d; 7.12; 6.61 | - | 6 d; 3.66; 3.78 |

### Cobertura d1, versao ref (dias; MAPE; WMAPE; F = celula(s) com warning.engine_tune_fallback na area)

| membro | area | feriado | vizinho | comum |
|---|---|---|---|---|
| LIN_MAX | AM | 5 d; 7.17; 7.10 | - | 6 d; 5.42; 5.12 |
| LIN_MAX | CE | 5 d; 6.27; 6.19 | - | 6 d; 3.27; 3.28 |
| LIN_MAX | GO | 5 d; 7.19; 6.57 | - | 6 d; 5.05; 5.22 |
| LIN_MAX | MS | 3 d; 12.29; 10.83 | - | 2 d; 10.34; 9.50 |
| LIN_MAX | PA | 5 d; 2.35; 2.37 | - | 6 d; 1.87; 1.89 |
| LIN_MAX | RJ | 5 d; 5.93; 5.53 | - | 6 d; 4.75; 4.82 |
| LIN_MAX | RS | 6 d; 5.21; 5.35 | - | 6 d; 4.91; 5.12 |
| LIN_MAX | SP | 6 d; 2.98; 2.93 | - | 6 d; 3.66; 3.74 |
| LIN_MED | AM | 5 d; 7.53; 7.43 | - | 6 d; 5.68; 5.18 |
| LIN_MED | CE | 5 d; 6.33; 6.23 | - | 6 d; 3.46; 3.45 |
| LIN_MED | GO | 5 d; 7.89; 7.15 | - | 6 d; 4.36; 4.56 |
| LIN_MED | MS | 3 d; 12.43; 10.95 | - | 2 d; 9.36; 8.59 |
| LIN_MED | PA | 5 d; 2.05; 2.06 | - | 6 d; 2.13; 2.15 |
| LIN_MED | RJ | 5 d; 5.79; 5.27 | - | 6 d; 4.87; 4.90 |
| LIN_MED | RS | 6 d; 5.28; 5.44 | - | 6 d; 4.45; 4.60 |
| LIN_MED | SP | 6 d; 2.93; 2.85 | - | 6 d; 3.72; 3.81 |
| LIN_MXMN | AM | 5 d; 7.21; 7.15 | - | 6 d; 5.80; 5.40 |
| LIN_MXMN | CE | 5 d; 6.24; 6.16 | - | 6 d; 3.47; 3.49 |
| LIN_MXMN | GO | 5 d; 6.88; 6.28 | - | 6 d; 4.83; 5.00 |
| LIN_MXMN | MS | 3 d; 12.64; 11.15 | - | 2 d; 9.86; 9.05 |
| LIN_MXMN | PA | 5 d; 2.20; 2.21 | - | 6 d; 1.80; 1.82 |
| LIN_MXMN | RJ | 5 d; 5.13; 4.77 | - | 6 d; 4.91; 4.93 |
| LIN_MXMN | RS | 6 d; 5.49; 5.64 | - | 6 d; 4.71; 4.93 |
| LIN_MXMN | SP | 6 d; 3.00; 2.93 | - | 6 d; 3.97; 4.06 |
| RGD_MAX | AM | 5 d; 6.84; 6.72 | - | 6 d; 5.22; 4.87 |
| RGD_MAX | CE | 5 d; 7.29; 6.75 | - | 6 d; 3.94; 3.92 |
| RGD_MAX | GO | 5 d; 7.38; 6.83 | - | 6 d; 4.13; 4.30 |
| RGD_MAX | MS | 2 d; 14.17; 11.58 | - | 2 d; 9.04; 8.29 |
| RGD_MAX | PA | 5 d; 4.88; 4.95 | - | 6 d; 2.22; 2.26 |
| RGD_MAX | RJ | 5 d; 6.57; 6.44 | - | 6 d; 5.44; 5.41 |
| RGD_MAX | RS | 5 d; 6.34; 5.59 | - | 6 d; 3.90; 4.09 |
| RGD_MAX | SP | 5 d; 3.84; 3.66 | - | 6 d; 3.27; 3.40 |
| RGD_MED | AM | 5 d; 8.32; 8.06 | - | 6 d; 6.09; 5.66 |
| RGD_MED | CE | 5 d; 7.93; 7.32 | - | 6 d; 3.67; 3.63 |
| RGD_MED | GO | 5 d; 7.69; 7.09 | - | 6 d; 3.71; 3.85 |
| RGD_MED | MS | 2 d; 12.92; 10.43 | - | 2 d; 8.88; 8.12 |
| RGD_MED | PA | 5 d; 4.89; 4.96 | - | 6 d; 1.89; 1.93 |
| RGD_MED | RJ | 5 d; 6.25; 6.02 | - | 6 d; 5.76; 5.69 |
| RGD_MED | RS | 5 d; 6.74; 5.99 | - | 6 d; 3.17; 3.30 |
| RGD_MED | SP | 5 d; 3.84; 3.67 | - | 6 d; 3.55; 3.68 |

### Cobertura horizonte, versao new (dias; MAPE; WMAPE; F = celula(s) com warning.engine_tune_fallback na area)

| membro | area | feriado | vizinho | comum |
|---|---|---|---|---|
| LIN_MAX | AM | 21 d; 8.07; 8.17 F | 9 d; 6.83; 7.08 | 69 d; 7.17; 7.08 |
| LIN_MAX | CE | 21 d; 6.91; 6.82 F | 10 d; 4.69; 4.79 | 68 d; 6.49; 6.56 |
| LIN_MAX | GO | 21 d; 8.78; 8.58 | 9 d; 5.37; 5.17 | 69 d; 6.38; 6.50 |
| LIN_MAX | MS | 15 d; 11.94; 10.88 | 6 d; 9.70; 9.65 | 24 d; 15.36; 13.46 |
| LIN_MAX | PA | 21 d; 3.46; 3.45 F | 10 d; 2.58; 2.57 | 68 d; 4.02; 4.05 |
| LIN_MAX | RJ | 21 d; 10.22; 10.08 | 9 d; 4.12; 4.11 | 69 d; 5.23; 5.41 |
| LIN_MAX | RS | 22 d; 9.35; 9.28 F | 11 d; 5.33; 5.30 | 75 d; 5.20; 5.29 |
| LIN_MAX | SP | 22 d; 6.38; 6.21 | 11 d; 3.61; 3.50 | 75 d; 3.18; 3.17 |
| LIN_MED | AM | 21 d; 8.60; 8.51 F | 9 d; 6.05; 6.02 | 69 d; 6.87; 6.67 |
| LIN_MED | CE | 21 d; 6.53; 6.41 F | 10 d; 3.70; 3.79 | 68 d; 5.22; 5.30 |
| LIN_MED | GO | 21 d; 9.16; 8.90 | 9 d; 5.83; 5.53 | 69 d; 6.00; 6.13 |
| LIN_MED | MS | 15 d; 12.47; 11.44 | 6 d; 9.99; 9.28 | 24 d; 16.22; 14.11 |
| LIN_MED | PA | 21 d; 3.40; 3.38 F | 10 d; 2.45; 2.43 | 68 d; 3.77; 3.79 |
| LIN_MED | RJ | 21 d; 9.61; 9.41 | 9 d; 4.12; 4.13 | 69 d; 5.52; 5.66 |
| LIN_MED | RS | 22 d; 8.55; 8.41 F | 11 d; 5.42; 5.32 | 75 d; 4.90; 4.97 |
| LIN_MED | SP | 22 d; 6.46; 6.27 | 11 d; 3.13; 3.03 | 75 d; 3.27; 3.26 |
| LIN_MXMN | AM | 21 d; 7.88; 7.99 F | 9 d; 6.13; 6.32 | 69 d; 7.18; 7.06 |
| LIN_MXMN | CE | 21 d; 7.35; 7.33 F | 10 d; 3.98; 4.09 | 68 d; 6.54; 6.65 |
| LIN_MXMN | GO | 21 d; 9.66; 9.42 | 9 d; 5.71; 5.45 | 69 d; 6.71; 6.80 |
| LIN_MXMN | MS | 15 d; 11.70; 10.68 | 6 d; 8.91; 8.72 | 24 d; 16.29; 14.15 |
| LIN_MXMN | PA | 21 d; 3.47; 3.45 F | 10 d; 3.05; 3.03 | 68 d; 4.47; 4.50 |
| LIN_MXMN | RJ | 21 d; 10.19; 10.05 | 9 d; 4.60; 4.60 | 69 d; 5.42; 5.57 |
| LIN_MXMN | RS | 22 d; 9.65; 9.54 F | 11 d; 5.14; 5.15 | 75 d; 5.21; 5.31 |
| LIN_MXMN | SP | 22 d; 6.77; 6.59 | 11 d; 3.32; 3.18 | 75 d; 3.14; 3.15 |
| LIN_UNI | AM | 22 d; 14.69; 14.24 F | 11 d; 5.73; 5.78 | 75 d; 7.70; 7.54 |
| LIN_UNI | CE | 22 d; 6.74; 6.52 F | 12 d; 3.91; 3.87 | 74 d; 4.47; 4.32 |
| LIN_UNI | GO | 21 d; 10.73; 10.30 | 9 d; 7.63; 7.44 | 69 d; 5.02; 5.05 |
| LIN_UNI | MS | 15 d; 13.88; 12.98 | 6 d; 11.52; 12.13 | 24 d; 13.64; 13.02 |
| LIN_UNI | PA | 22 d; 3.10; 3.08 F | 12 d; 1.95; 1.92 | 74 d; 2.68; 2.69 |
| LIN_UNI | RJ | 22 d; 8.92; 8.45 | 11 d; 8.14; 8.07 | 75 d; 8.43; 8.21 |
| LIN_UNI | RS | 22 d; 9.88; 9.82 F | 11 d; 6.32; 6.56 | 75 d; 5.24; 5.38 |
| LIN_UNI | SP | 22 d; 6.40; 6.17 | 11 d; 4.20; 4.21 | 75 d; 4.07; 4.06 |
| RGD_MAX | AM | 20 d; 8.15; 7.96 | 9 d; 5.81; 5.89 | 70 d; 6.79; 6.60 |
| RGD_MAX | CE | 20 d; 7.12; 6.84 | 10 d; 3.22; 3.19 | 69 d; 5.04; 5.01 |
| RGD_MAX | GO | 20 d; 8.42; 8.09 | 9 d; 4.95; 4.72 | 70 d; 4.98; 5.07 |
| RGD_MAX | MS | 13 d; 11.61; 10.58 | 4 d; 9.84; 9.38 | 19 d; 16.14; 14.05 |
| RGD_MAX | PA | 20 d; 4.05; 4.03 | 10 d; 2.30; 2.28 | 69 d; 3.43; 3.40 |
| RGD_MAX | RJ | 20 d; 7.45; 7.27 | 9 d; 5.61; 5.50 | 70 d; 6.53; 6.64 |
| RGD_MAX | RS | 20 d; 10.21; 9.74 | 9 d; 6.07; 5.98 | 70 d; 4.98; 4.99 |
| RGD_MAX | SP | 20 d; 6.45; 6.17 | 9 d; 5.26; 5.19 | 70 d; 3.32; 3.38 |
| RGD_MED | AM | 20 d; 9.26; 8.90 | 9 d; 4.38; 4.39 | 70 d; 7.12; 6.80 |
| RGD_MED | CE | 20 d; 6.86; 6.53 | 10 d; 3.29; 3.22 | 69 d; 4.97; 5.02 |
| RGD_MED | GO | 20 d; 8.09; 7.74 | 9 d; 5.20; 4.96 | 70 d; 5.06; 5.12 |
| RGD_MED | MS | 13 d; 11.53; 10.38 | 4 d; 8.88; 8.49 | 19 d; 15.36; 13.42 |
| RGD_MED | PA | 20 d; 3.98; 3.95 | 10 d; 2.23; 2.20 | 69 d; 3.35; 3.33 |
| RGD_MED | RJ | 20 d; 7.79; 7.61 | 9 d; 5.18; 5.00 | 70 d; 7.00; 6.99 |
| RGD_MED | RS | 20 d; 8.79; 8.29 | 9 d; 6.30; 6.22 | 70 d; 4.65; 4.67 |
| RGD_MED | SP | 20 d; 6.40; 6.11 | 9 d; 4.80; 4.74 | 70 d; 3.23; 3.29 |

### Cobertura horizonte, versao ref (dias; MAPE; WMAPE; F = celula(s) com warning.engine_tune_fallback na area)

| membro | area | feriado | vizinho | comum |
|---|---|---|---|---|
| LIN_MAX | AM | 21 d; 6.38; 6.42 | 9 d; 7.28; 7.49 | 69 d; 6.94; 6.83 |
| LIN_MAX | CE | 21 d; 7.77; 7.84 | 10 d; 4.18; 4.32 | 68 d; 5.02; 5.02 |
| LIN_MAX | GO | 21 d; 8.62; 8.34 | 9 d; 6.81; 6.58 | 69 d; 6.21; 6.32 |
| LIN_MAX | MS | 15 d; 11.67; 10.50 | 6 d; 9.39; 8.99 | 24 d; 16.20; 14.05 |
| LIN_MAX | PA | 21 d; 2.81; 2.84 | 10 d; 2.32; 2.31 | 68 d; 3.11; 3.11 |
| LIN_MAX | RJ | 21 d; 7.20; 7.15 | 9 d; 4.06; 3.88 | 69 d; 5.80; 5.98 |
| LIN_MAX | RS | 22 d; 6.99; 7.07 | 11 d; 5.93; 6.06 | 75 d; 5.43; 5.53 |
| LIN_MAX | SP | 22 d; 3.72; 3.71 | 11 d; 3.42; 3.28 | 75 d; 3.39; 3.38 |
| LIN_MED | AM | 21 d; 6.07; 6.05 | 9 d; 6.25; 6.18 | 69 d; 6.78; 6.55 |
| LIN_MED | CE | 21 d; 7.59; 7.68 | 10 d; 3.52; 3.63 | 68 d; 4.83; 4.86 |
| LIN_MED | GO | 21 d; 8.35; 8.03 | 9 d; 6.34; 6.01 | 69 d; 5.62; 5.74 |
| LIN_MED | MS | 15 d; 11.62; 10.57 | 6 d; 9.67; 9.17 | 24 d; 16.33; 14.30 |
| LIN_MED | PA | 21 d; 2.70; 2.72 | 10 d; 2.05; 2.01 | 68 d; 3.17; 3.15 |
| LIN_MED | RJ | 21 d; 6.47; 6.35 | 9 d; 3.87; 3.73 | 69 d; 5.57; 5.69 |
| LIN_MED | RS | 22 d; 6.91; 6.95 | 11 d; 5.52; 5.68 | 75 d; 5.22; 5.30 |
| LIN_MED | SP | 22 d; 3.75; 3.73 | 11 d; 3.21; 3.05 | 75 d; 3.37; 3.36 |
| LIN_MXMN | AM | 21 d; 6.25; 6.29 | 9 d; 6.78; 6.93 | 69 d; 6.99; 6.87 |
| LIN_MXMN | CE | 21 d; 7.89; 7.99 | 10 d; 3.93; 4.04 | 68 d; 5.63; 5.66 |
| LIN_MXMN | GO | 21 d; 7.84; 7.58 | 9 d; 6.64; 6.39 | 69 d; 6.07; 6.18 |
| LIN_MXMN | MS | 15 d; 11.81; 10.67 | 6 d; 9.99; 9.45 | 24 d; 16.46; 14.22 |
| LIN_MXMN | PA | 21 d; 3.07; 3.09 | 10 d; 2.44; 2.43 | 68 d; 3.73; 3.73 |
| LIN_MXMN | RJ | 21 d; 6.94; 6.89 | 9 d; 4.21; 4.10 | 69 d; 5.89; 6.07 |
| LIN_MXMN | RS | 22 d; 7.46; 7.51 | 11 d; 5.73; 5.89 | 75 d; 5.42; 5.50 |
| LIN_MXMN | SP | 22 d; 4.03; 4.00 | 11 d; 2.80; 2.70 | 75 d; 3.26; 3.27 |
| RGD_MAX | AM | 20 d; 8.15; 7.98 | 9 d; 5.99; 6.04 | 70 d; 6.79; 6.60 |
| RGD_MAX | CE | 20 d; 6.34; 6.03 | 10 d; 3.38; 3.37 | 69 d; 5.20; 5.13 |
| RGD_MAX | GO | 20 d; 7.08; 6.79 | 9 d; 4.86; 4.61 | 70 d; 4.88; 4.99 |
| RGD_MAX | MS | 13 d; 11.59; 10.42 | 4 d; 8.51; 7.94 | 19 d; 14.69; 12.50 |
| RGD_MAX | PA | 20 d; 4.07; 4.04 | 10 d; 2.22; 2.19 | 69 d; 3.11; 3.09 |
| RGD_MAX | RJ | 20 d; 7.62; 7.50 | 9 d; 4.07; 4.05 | 70 d; 6.39; 6.54 |
| RGD_MAX | RS | 20 d; 8.89; 8.68 | 9 d; 7.26; 7.42 | 70 d; 6.11; 6.17 |
| RGD_MAX | SP | 20 d; 4.71; 4.61 | 9 d; 5.06; 5.01 | 70 d; 3.49; 3.51 |
| RGD_MED | AM | 20 d; 8.76; 8.45 | 9 d; 4.47; 4.48 | 70 d; 6.60; 6.29 |
| RGD_MED | CE | 20 d; 6.26; 5.92 | 10 d; 3.68; 3.57 | 69 d; 4.63; 4.59 |
| RGD_MED | GO | 20 d; 7.14; 6.80 | 9 d; 5.02; 4.77 | 70 d; 4.94; 5.01 |
| RGD_MED | MS | 13 d; 11.17; 10.07 | 4 d; 7.53; 7.00 | 19 d; 14.08; 12.00 |
| RGD_MED | PA | 20 d; 4.04; 4.02 | 10 d; 2.30; 2.26 | 69 d; 3.12; 3.11 |
| RGD_MED | RJ | 20 d; 7.70; 7.55 | 9 d; 3.76; 3.67 | 70 d; 6.75; 6.75 |
| RGD_MED | RS | 20 d; 8.84; 8.61 | 9 d; 7.75; 7.95 | 70 d; 5.93; 5.96 |
| RGD_MED | SP | 20 d; 4.53; 4.45 | 9 d; 4.67; 4.62 | 70 d; 3.31; 3.35 |

## MAPE e WMAPE D+1 por origem e area

Todos os membros e as duas versoes estao em `porigem.tsv` (917 linhas). Abaixo, as variantes MED. Celula ausente (-) = unidade recusada em ambas as versoes ou so em uma.

### LIN_MED, MAPE D+1 por origem e area (ref / nova; - = celula ausente)

| origem | AM | CE | GO | MS | PA | RJ | RS | SP |
|---|---|---|---|---|---|---|---|---|
| 2023-02-19 | 9.32 / 13.53 | 2.73 / 6.93 | 5.08 / 10.54 | - / - | 1.81 / 3.61 | 1.84 / 1.89 | 6.89 / 7.79 | 2.44 / 3.98 |
| 2023-03-15 | 4.33 / 4.28 | 3.80 / 4.07 | 2.88 / 3.47 | - / - | 2.39 / 1.20 | 2.87 / 2.57 | 3.95 / 2.59 | 3.21 / 2.01 |
| 2023-05-10 | 4.76 / 2.88 | 2.68 / 3.45 | 3.53 / 5.48 | - / - | 1.24 / 1.53 | 5.03 / 5.14 | 2.93 / 2.11 | 1.10 / 1.21 |
| 2023-06-07 | - / - | - / - | - / - | - / - | - / - | - / - | 4.68 / 4.55 | 1.53 / 1.25 |
| 2023-08-16 | 2.00 / 2.64 | 3.95 / 6.78 | 4.10 / 3.42 | - / - | 3.69 / 3.57 | 2.96 / 1.97 | 5.42 / 6.29 | 2.59 / 2.76 |
| 2023-10-18 | 12.09 / 12.07 | 2.57 / 2.71 | 7.07 / 7.62 | - / - | 1.78 / 0.95 | 5.19 / 5.22 | 4.66 / 5.46 | 5.21 / 1.62 |
| 2023-12-31 | 6.36 / 7.59 | 7.53 / 8.66 | 4.09 / 14.04 | - / - | 1.76 / 2.76 | 8.21 / 17.88 | 4.60 / 22.47 | 6.25 / 16.90 |
| 2024-02-11 | 2.85 / 10.12 | 9.32 / 14.03 | 5.55 / 5.75 | 6.18 / 8.67 | 1.73 / 4.39 | 1.83 / 2.72 | 7.61 / 5.67 | 2.66 / 2.50 |
| 2024-04-17 | 9.27 / 8.60 | 4.09 / 5.95 | 3.69 / 3.41 | 12.30 / 14.44 | 2.47 / 3.76 | 7.75 / 8.37 | 4.70 / 5.26 | 7.42 / 7.41 |
| 2024-05-29 | 16.07 / 17.58 | 6.88 / 7.66 | 3.31 / 3.64 | 7.86 / 6.77 | 2.39 / 1.52 | 12.25 / 10.91 | 4.23 / 4.61 | 1.88 / 1.50 |
| 2024-09-11 | 1.65 / 1.48 | 3.65 / 4.52 | 4.91 / 4.25 | 6.41 / 6.96 | 1.20 / 2.15 | 5.41 / 7.10 | 5.06 / 3.68 | 2.81 / 2.61 |
| 2024-12-24 | 3.08 / 5.24 | 5.20 / 4.87 | 21.43 / 15.74 | 23.25 / 27.41 | 2.58 / 2.22 | 4.84 / 6.00 | 3.68 / 3.86 | 2.81 / 6.50 |

### LIN_MED, WMAPE D+1 por origem e area (ref / nova; - = celula ausente)

| origem | AM | CE | GO | MS | PA | RJ | RS | SP |
|---|---|---|---|---|---|---|---|---|
| 2023-02-19 | 9.41 / 13.29 | 2.71 / 6.76 | 5.17 / 11.08 | - / - | 1.80 / 3.62 | 1.83 / 1.88 | 6.83 / 7.26 | 2.43 / 3.96 |
| 2023-03-15 | 4.35 / 4.26 | 3.67 / 4.03 | 3.08 / 3.57 | - / - | 2.43 / 1.22 | 2.88 / 2.59 | 4.03 / 2.72 | 3.32 / 2.11 |
| 2023-05-10 | 4.77 / 2.90 | 2.67 / 3.42 | 3.46 / 5.47 | - / - | 1.24 / 1.54 | 5.10 / 5.19 | 2.84 / 2.06 | 1.07 / 1.24 |
| 2023-06-07 | - / - | - / - | - / - | - / - | - / - | - / - | 4.60 / 4.71 | 1.48 / 1.26 |
| 2023-08-16 | 1.96 / 2.54 | 4.02 / 6.87 | 4.33 / 3.65 | - / - | 3.75 / 3.62 | 2.82 / 1.89 | 5.66 / 6.56 | 2.55 / 2.75 |
| 2023-10-18 | 10.62 / 10.67 | 2.54 / 2.74 | 7.22 / 7.64 | - / - | 1.81 / 0.96 | 5.26 / 5.28 | 4.77 / 6.03 | 5.12 / 1.65 |
| 2023-12-31 | 6.68 / 7.68 | 7.24 / 8.60 | 3.98 / 13.81 | - / - | 1.71 / 2.75 | 8.24 / 17.75 | 4.35 / 21.44 | 6.31 / 16.88 |
| 2024-02-11 | 2.88 / 10.51 | 9.03 / 13.52 | 5.69 / 6.03 | 6.15 / 8.78 | 1.76 / 4.40 | 1.81 / 2.73 | 7.44 / 5.54 | 2.55 / 2.38 |
| 2024-04-17 | 9.07 / 8.45 | 4.19 / 6.05 | 3.61 / 3.37 | 12.07 / 14.27 | 2.46 / 3.73 | 7.71 / 8.32 | 4.89 / 5.30 | 7.71 / 7.68 |
| 2024-05-29 | 16.17 / 17.70 | 6.86 / 7.69 | 3.24 / 3.63 | 7.97 / 6.84 | 2.38 / 1.52 | 12.08 / 10.81 | 4.35 / 4.91 | 1.85 / 1.57 |
| 2024-09-11 | 1.63 / 1.47 | 3.67 / 4.64 | 5.11 / 4.44 | 5.88 / 6.42 | 1.25 / 2.20 | 5.40 / 7.08 | 5.44 / 3.97 | 2.88 / 2.60 |
| 2024-12-24 | 3.03 / 5.28 | 5.22 / 4.66 | 19.20 / 13.96 | 20.44 / 25.50 | 2.55 / 2.18 | 5.11 / 6.28 | 3.59 / 3.88 | 2.93 / 6.60 |

### RGD_MED, MAPE D+1 por origem e area (ref / nova; - = celula ausente)

| origem | AM | CE | GO | MS | PA | RJ | RS | SP |
|---|---|---|---|---|---|---|---|---|
| 2023-02-19 | 11.84 / 13.62 | 10.77 / 9.89 | 4.33 / 5.02 | - / - | 3.39 / 3.47 | 4.91 / 5.58 | 3.83 / 4.43 | 2.34 / 4.07 |
| 2023-03-15 | 4.67 / 5.19 | 5.40 / 5.36 | 3.06 / 3.25 | - / - | 2.17 / 2.70 | 3.24 / 2.20 | 1.38 / 1.31 | 1.84 / 2.34 |
| 2023-05-10 | 3.50 / 2.69 | 2.36 / 1.62 | 2.71 / 2.75 | - / - | 1.43 / 1.44 | 6.44 / 8.35 | 2.21 / 1.91 | 2.12 / 2.26 |
| 2023-06-07 | 3.73 / 3.96 | 2.95 / 2.96 | 3.42 / 3.25 | - / - | 6.67 / 7.15 | 3.38 / 4.03 | 5.56 / 6.05 | 1.78 / 1.86 |
| 2023-08-16 | 2.62 / 2.50 | 4.19 / 5.22 | 2.66 / 2.99 | - / - | 2.81 / 2.99 | 5.21 / 3.00 | 3.39 / 3.37 | 2.92 / 3.08 |
| 2023-10-18 | 13.38 / 14.27 | 2.08 / 3.28 | 7.93 / 7.55 | - / - | 1.39 / 1.14 | 5.52 / 6.05 | 3.99 / 3.84 | 4.39 / 3.90 |
| 2023-12-31 | 7.29 / 7.71 | 12.17 / 18.95 | 9.38 / 16.22 | - / - | 3.94 / 3.46 | 13.89 / 16.06 | 16.30 / 22.82 | 10.29 / 19.72 |
| 2024-02-11 | 11.73 / 12.12 | 8.76 / 7.24 | 3.72 / 3.65 | 3.00 / 4.79 | 4.99 / 4.88 | 3.65 / 3.96 | 3.77 / 1.66 | 2.66 / 2.56 |
| 2024-04-17 | 9.83 / 10.03 | 3.56 / 4.21 | 3.85 / 3.28 | 10.09 / 12.64 | 1.60 / 1.38 | 9.49 / 10.56 | 2.78 / 2.20 | 6.92 / 6.73 |
| 2024-05-29 | - / - | - / - | - / - | - / - | - / - | - / - | - / - | - / - |
| 2024-09-11 | 2.54 / 2.52 | 4.43 / 4.73 | 2.03 / 2.40 | 7.67 / 7.65 | 1.95 / 1.55 | 4.66 / 4.66 | 5.26 / 4.44 | 3.12 / 3.66 |
| 2024-12-24 | 7.02 / 5.68 | 5.01 / 4.82 | 17.58 / 13.21 | 22.84 / 27.63 | 5.46 / 4.86 | 5.42 / 5.60 | 4.22 / 6.59 | 2.12 / 7.42 |

### RGD_MED, WMAPE D+1 por origem e area (ref / nova; - = celula ausente)

| origem | AM | CE | GO | MS | PA | RJ | RS | SP |
|---|---|---|---|---|---|---|---|---|
| 2023-02-19 | 11.78 / 13.58 | 10.61 / 9.84 | 4.39 / 5.08 | - / - | 3.37 / 3.46 | 4.98 / 5.74 | 4.03 / 4.54 | 2.43 / 4.27 |
| 2023-03-15 | 4.59 / 5.19 | 5.27 / 5.25 | 3.17 / 3.37 | - / - | 2.21 / 2.72 | 3.30 / 2.23 | 1.38 / 1.34 | 1.97 / 2.46 |
| 2023-05-10 | 3.50 / 2.70 | 2.34 / 1.59 | 2.70 / 2.70 | - / - | 1.43 / 1.43 | 6.45 / 8.34 | 2.22 / 1.96 | 2.18 / 2.26 |
| 2023-06-07 | 3.71 / 4.04 | 2.97 / 3.01 | 3.36 / 3.18 | - / - | 6.65 / 7.15 | 3.40 / 4.06 | 5.56 / 6.02 | 1.88 / 1.98 |
| 2023-08-16 | 2.53 / 2.41 | 4.27 / 5.27 | 2.72 / 3.10 | - / - | 2.86 / 3.05 | 4.99 / 2.88 | 3.65 / 3.63 | 2.93 / 3.06 |
| 2023-10-18 | 12.09 / 12.75 | 2.08 / 3.31 | 8.04 / 7.63 | - / - | 1.43 / 1.17 | 5.59 / 6.13 | 4.36 / 4.14 | 4.52 / 4.02 |
| 2023-12-31 | 7.19 / 7.48 | 11.63 / 18.35 | 9.39 / 16.18 | - / - | 4.02 / 3.53 | 14.13 / 15.96 | 14.93 / 21.19 | 10.33 / 19.53 |
| 2024-02-11 | 11.54 / 11.93 | 7.98 / 6.61 | 3.60 / 3.50 | 2.98 / 4.69 | 4.98 / 4.88 | 3.65 / 3.97 | 3.56 / 1.78 | 2.57 / 2.39 |
| 2024-04-17 | 9.65 / 9.90 | 3.64 / 4.30 | 3.87 / 3.39 | 10.06 / 12.49 | 1.58 / 1.37 | 9.43 / 10.45 | 2.96 / 2.25 | 7.17 / 6.96 |
| 2024-05-29 | - / - | - / - | - / - | - / - | - / - | - / - | - / - | - / - |
| 2024-09-11 | 2.54 / 2.52 | 4.49 / 4.86 | 2.10 / 2.55 | 6.61 / 6.50 | 2.03 / 1.62 | 4.67 / 4.65 | 5.69 / 4.76 | 3.19 / 3.76 |
| 2024-12-24 | 7.12 / 5.76 | 4.18 / 4.38 | 16.28 / 12.02 | 21.49 / 26.05 | 5.57 / 4.95 | 5.42 / 5.64 | 4.17 / 6.44 | 2.11 / 7.27 |

## Decisoes do Gabriel em 2026-11-18

- Aceitas as recusas como estao (inclui as 15 de MS na nova e 13 na ref; a do "contrasts" de 2024-05-29 no regdin e herdada).
- Aceito este relatorio de MAPE/WMAPE.
- Aceito o fallback sem tune, que fica no codigo do E8-T4; a emenda ou ADR sai com a spec do modelo de feriados, que remove o perfil de feriado do DESSEM.
- Aceito o skip da paridade, porque o golden esta fora do host (pendencia registrada).
- A area nao entra no warning de fallback.
