# Deck canônico de entrada — contrato do store (validate-deck, M2)

> **Tier:** referência (`docs/reference/`, vivo). **Fonte:** schema observado em `/opt/source/prevcarga-R/data`
> (store limpo) + auditoria dos 5 adapters da referência `/opt/ons-source/prevcargadessem3` (o que cada modelo
> de fato lê). **Decisões associadas:** [ADR-0004/0005](../adr/0004-store-parquet-hive.md) (schema fail-high),
> [ADR-0002](../adr/0002-grade-semihoraria-schema-long.md) (grade), [ADR-0003](../adr/0003-tempo-naive-brt-runtime-env.md)
> (tempo/emissão), [ADR-0007](../adr/0007-contrato-corte-intradiario.md) (corte intradiário; superado: o corte é derivado do dado).

## Princípio
O deck é um **store parquet Hive-particionado**, em nomes **canônicos** já padronizados. Os 5 modelos da
referência leem um **subconjunto** destes dados com **nomes idênticos** — não há divergência de nome no
boundary do deck (as divergências `Carga`/`CargaGlobal`/`Exo_Temp`/`Exo_Temperatura` são internas de engine,
pós-leitura, e não pertencem ao deck). A padronização real acontece **uma vez**, no ingest cru → canônico
(tabela de alias no fim), nunca espalhada pelos modelos (ADR-0004/0005).

Tempo: colunas `timestamp[ns]` **sem fuso**; o fuso de cada dataset/coluna é **declarado no `manifest.json`** (`timezone`, ADR-0037). Store atual: carga e clima observado `Etc/GMT+3` (−03:00 fixo, sem horário de verão), `weather_forecast.valid_time` `Etc/GMT+3`, `weather_forecast.issue_time` `UTC`. ⚠ Lidas numa sessão R com o fuso do host, essas colunas aparecem deslocadas.

## `manifest.json`
Arquivo na raiz do `data_dir`, lido **só** pelo leitor único (`prevcarga_deck_manifest()`, M2.1). Schema **v1 fechado**:
chave desconhecida ⇒ `contract.manifest_unknown_key`. O produto nunca o escreve; o utilitário de desenvolvimento
`scripts/tools/write_store_manifest.R --target <copia>` grava o template versionado `config/deck-manifest.store.json`
numa cópia/overlay do store (valida pelo leitor; manifest existente idêntico ⇒ no-op; diferente ⇒ `contract.manifest_exists_different`).

| chave | valor | nota |
|---|---|---|
| `schema_version` | `1` | obrigatório |
| `load_label` | `start` \| `end` | rótulo da carga do store (ADR-0035) |
| `timezone.load` / `.weather_observed` / `.holidays` | nome Olson | fuso do dataset (ADR-0037) |
| `timezone.weather_forecast.valid_time` / `.issue_time` | nome Olson | fuso por coluna do clima previsto |

Precedência do `load_label`: **manifest > `--load-label` > `config.yaml` > `end`**; flag/config em conflito com o manifest ⇒ `contract`.
Fusos nunca têm default: ausente ⇒ `contract.tz_missing`, inválido ⇒ `contract.tz_invalid`. A chave legada `corte` **não existe**
(o corte intradiário é derivado do dado, `available_until`). Exemplo do store atual (valores a confirmar pelo Gabriel):
```json
{ "schema_version": 1, "load_label": "start",
  "timezone": { "load": "Etc/GMT+3", "weather_observed": "Etc/GMT+3",
                "weather_forecast": { "valid_time": "Etc/GMT+3", "issue_time": "UTC" }, "holidays": "Etc/GMT+3" } }
```

## Painel do leitor
`prevcarga_read_panel(ctx, origin, areas, horizon_days, history_days)` agrega, por `area_code`: `load` (`interval_start`/`interval_end`,
`load_mwh`), `weather_observed`, `weather_forecast` (emissão escolhida pela regra `weather_issue_cutoff`, padrão `available_until`)
e `holidays` (`area_code`, `date`, `holiday_type_id`, `prevcarga_code`, `simplified_code`; os três códigos de feriado; data civil no fuso da
carga, sem filtro de `available_until`). `attrs`: `origin`, `available_until`, `available_until_by_area`, `load_label`, `load_label_source`,
`tz`, `weather_issue_cutoff`, `weather_issue_limit`. Divergência de `tz` ou `available_until` entre os leitores ⇒ `contract`.

## Datasets, particionamento e colunas

Convenção: **R** = required (ausência ⇒ `validate-deck` falha alto, ADR-0004/0005) · **O** = optional
(validado por tipo/esquema **se presente**, nunca exigido).

### `load/` — carga realizada · partição `area_code=/year=`
| coluna | tipo | R/O | nota |
|---|---|---|---|
| `timestamp` | `timestamp[ns]` naive-BRT | **R** | grade semi-horária (:00/:30). Convenção declarada no `manifest.json` (`load_label`). Este store está deslocado para o **INÍCIO** do intervalo (`load_label: start`; `00:00` = 00:00–00:30), já pronto para médias; a fonte chega em fim e é deslocada −30 min. O leitor único entrega `interval_start` e `interval_end` (ADR-0035) |
| `load_mwh` | `double` | **R** | carga em MWmed |
| `area_code` | `string` (partição) | **R** | código de área |

### `weather/forecast/` — clima previsto · partição `area_code=/year=`
| coluna | tipo | R/O | nota |
|---|---|---|---|
| `issue_time` | `timestamp[ns]` | **R** | **instante de emissão, resolução até 6h** (00/06/12/18Z); NÃO achatar pra data (ADR-0003) |
| `valid_time` | `timestamp[ns]` naive-BRT | **R** | instante-alvo da previsão |
| `temperature` | `double` | **R** | °C; única exógena lida hoje |
| `area_code` | `string` (partição) | **R** | |
| `heat_index` | `double` | O | derivado; nenhum modelo lê hoje |

### `weather/observed/` — clima observado · partição `area_code=/year=`
| coluna | tipo | R/O | nota |
|---|---|---|---|
| `timestamp` | `timestamp[ns]` naive-BRT | **R** | lido por HW, dessem-linear, dessem-regdin |
| `temperature` | `double` | **R** | °C |
| `area_code` | `string` (partição) | **R** | |
| `heat_index` | `double` | O | derivado; não lido |
| `rh` | `double` | O | umidade relativa; não lido |

### `auxiliary/holidays/` — dias especiais · partição `area_code=/year=`
| coluna | tipo | R/O | nota |
|---|---|---|---|
| `date` | `date32[day]` | **R** | |
| `holiday_type_id` | `int64` | **R** | HW, dessem-* consomem |
| `prevcarga_code` | `int64` | **R** | LGBM (≥1), dessem-* consomem |
| `simplified_code` | `int64` | **R** | HW, dessem-regdin consomem |
| `area_code` | `string` (partição) | **R** | |

> Os **três códigos** são required porque modelos diferentes consomem códigos diferentes (RF usa só a
> presença de `date`; LGBM `prevcarga_code≥1`; HW `simplified_code`+`holiday_type_id`; dessem-linear
> `prevcarga_code`/fallback `holiday_type_id`; dessem-regdin os três). Pra servir todos, o deck carrega todos.

### `auxiliary/load_tiers/` — patamares · partição `area_code=` *(dataset OPTIONAL inteiro)*
Colunas: `hour`,`weekday_winter`,`weekend_winter`,`weekday_intermediate`,`weekend_intermediate`,
`weekday_summer`,`weekend_summer`,`area_code`. **Nenhum modelo lê hoje** — mantido por ser padrão de domínio
(patamares) e forward-looking.

### `auxiliary/dst_periods/` — horário de verão *(dataset OPTIONAL inteiro)*
Colunas: `start_date`,`end_date` (`timestamp[ns]`). **Nenhum modelo lê** — o HW calcula o flag `HV` sozinho.
Mantido como rede de segurança explícita.

## Contrato de emissão do clima previsto (ADR-0003)
- `issue_time` preserva o **instante** de emissão (até 4 rodadas/dia, 6-em-6h a partir de 00 UTC). É o único
  campo em UTC; `valid_time` e a grade estão em −03:00 (fusos declarados no manifest, ADR-0037).
- **Não** reatribuir `issue_time` de `dat_referencia` (erro da migração legada que matava 06/12/18Z).
- Corte intradiário (ADR-0007): usar a emissão mais fresca com `issue_time ≤ instante de corte`.
- **A desenhar no M5:** regra explícita de **dia-de-origem** da rodada + cálculo de horizonte **UTC-safe**
  (evita o horizonte negativo do relabel ingênuo). Ver [[clima-previsto-emissoes]] na memória.

## Tabela de alias: canônico ← legado (ONS cru) — resolvido 1× no data-prep (ADR-0004/0005)
| canônico | alias legado |
|---|---|
| `area_code` | `cod_areacarga` |
| `timestamp` (load) | `din_referencia` |
| `load_mwh` | `val_carga` *(fallback y-true tolerante: `load_mw`,`carga`,`carga_mwmed`,`val_cargacons`,`MW`)* |
| `temperature` | `val_tmp` |
| `valid_time` (forecast) | `din_referencia` |
| `issue_time` (forecast) | `din_origemprevisaoutc` **(preservado no instante, não reatribuído)** |
| `date` (holidays) | `dat_diaespecial` |
| `holiday_type_id` | `id_tipodiaespecial` |
| `prevcarga_code` | `cod_prevcarga` |
| `simplified_code` | `cod_simplificado` |

> Nomes internos de engine (fora do deck, pós-leitura): `DataHora`/`Carga`/`CargaGlobal`/`Temperatura`/
> `Exo_Temp`/`Exo_Temperatura`/`H01..H48`/`T01..T48`/`CargaMedia`/`TempMedia`/`Feriado`/`TIPO_FER`/`Tipo`.
> Não são deck — cada adapter os produz após ler as colunas canônicas.

## Matriz "quem lê o quê" (da auditoria da referência)
| dataset | RF | LGBM | HW | dessem-linear | dessem-regdin |
|---|---|---|---|---|---|
| load | ✓ | ✓ | ✓ | ✓ | ✓ |
| weather/forecast | ✓ | ✓ | ✓ | ✓ | ✓ |
| weather/observed | — | — | ✓ | ✓ | ✓ |
| holidays | só `date` | `prevcarga_code` | `simplified_code`+`holiday_type_id` | `prevcarga_code` | 3 códigos |
| load_tiers | — | — | — | — | — |
| dst_periods | — | — | — | — | — |

## Consequência para o `validate-deck` (M2)
`validate-deck` impõe: (1) presença dos datasets/colunas **R**; (2) tipos corretos; (3) particionamento Hive
`area_code=/year=`; (4) grade semi-horária completa (ADR-0002); (5) colunas **O** validadas por tipo só se
presentes; (6) o contrato de emissão multi-rodada do `weather/forecast`. Falha **alto** por origem, sem NA
silencioso (ADR-0004/0005).
