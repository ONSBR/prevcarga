# Estado: holiday_islands (E3-T1) -- regra de datas da janela do DESSEM e ilhas de feriado

- Arquivo: `scripts/lib/holiday_islands.R` (so definicoes; requer `errors.R`). Testes: `scripts/lib/tests/test-holiday-islands.R` (calendario sintetico, sem store).
- Regra unica (ADR-046): o leitor (E3-T2) e o gerador do deck de inferencia (E2) chamam as MESMAS duas funcoes; nenhum deles recalcula datas.
- `prevcarga_dessem_window(origin, mode = c("ano_civil", "referencia"))` devolve `list(common_from, common_to, holiday_from, mode)`.
  - `ano_civil` (default): `common_from = O - 1095`, `common_to = O - 1`, `holiday_from = 01-01 de ano(O) - 5`.
  - `referencia`: `common_from = O - 1825`, `common_to = O - 1`, `holiday_from = NULL` (sem anexo; modo dos goldens v0.1.5).
  - modo fora dos dois valores (match exato) => `contract.constraint_violated` (exit 2), `where = dessem_window.mode`.
- `prevcarga_holiday_islands(holidays, origin, common_from, first_day, lag_days = 7L, holiday_from = NULL)`:
  - `holidays`: data.table com `area_code`, `prevcarga_code`, `date`. Entram ocorrencias com `prevcarga_code != 0` e `holiday_from <= date <= O - 1`; `holiday_from = NULL` usa o do modo `ano_civil`.
  - Por ocorrencia d a ilha e `[d - lag_days, d]`, menos `[common_from, O - 1]` (ilha que cruza `common_from` termina em `common_from - 1`), cortada em `first_day`.
  - Ilhas da mesma area que se encostam ou sobrepoem (`from_k <= to_{k-1} + 1`) sao juntadas; `holiday_dates` acumula as ocorrencias.
  - Saida: `data.table(area_code, island_id, from, to, holiday_dates)` ordenada por `(area_code, from)`; `island_id` 1..n por area. Nunca contem dia `>= O` nem dia da janela comum. Zero ilhas => tabela vazia com as mesmas colunas e tipos.
- Justificativa do lag de 7 dias: cobre `LAGDS_1`/`LAGDS_7` dos tipos 1-8 e os lags 2 e 7 dos tipos 9-11 de `janelaDadosFer`, e o `hrlag = 1` de `obterDiaEspPerfilConfig`.
- Corte no inicio do store: `first_day` (2018-01-01) corta `from` sem recusa e sem erro (feriado em 01-01 de ano(O)-5 gera ilha de 1 dia).
- Datas so por lubridate (Date, sem fuso de hora); saida deterministica (`identical()` entre chamadas); ordenacao radix, sem dependencia de locale.
