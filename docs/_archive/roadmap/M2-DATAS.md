# M2 — Datas honestas (3 cortes) + validação model-agnóstica + `validate-deck` (gate-0)

> ✅ **VERIFY-READY (M2):** `verify-milestone` **ready:true · 0 blockers · 0 decisions · 6/6 reviewers unânime** (2026-10-12;
> convergiu nos 2 últimos rounds — logs em `verify-log/M2-DATAS.jsonl`). **Pronto pra `implement`** (`CHECKPOINTS['M2']`
> wirado + sincronizado). ⚠ **3 pré-requisitos / decisões do Gabriel ANTES do implement** (ver §Decisões): **(1)** o corte
> por-protocolo — adotei a **opção (b)** (M2.1 cobre só `daily`=origem−1−horizonte, conforme ADR-0006; dessem/HW
> `retrain_harness` → M5), evitando contradizer o ADR-0006 Aceito; o Gabriel pode preferir a **opção (a)** (ramificar já no
> M2.1) criando um **ADR superseder do 0006** antes do implement. **(2)** 3 correções de premissa "repo vence" (fill-0
> inexistente, completude flat-48, infer=train p/ dessem). **(3)** 4 candidatos a ADR (precedência, `data_hash`-recipe,
> schema-veredito, corte-por-protocolo→M5). Material residual não-bloqueante anotado nas §. **Tier:** `docs/roadmap/`
> (plano ativo; some ao concluir). **Programa:** [PLANO.md](./PLANO.md) (M1…M8). **Depende de:** M1 (fachada docopt +
> runtime-guard + exit-code handler + `topology.R`/`model_spec`, já ✅ VERIFY-READY). **Checkpoint:** infra de datas/validação — fecha batch próprio, sem A/B (ver §Checkpoint).

> **Intent (Gabriel, 2026-10-10/11).** Colisão de datas é o **risco #1** do sistema: um erro de ±1 dia no
> corte de treino infla a acurácia em silêncio e passa por todas as métricas agregadas (ADR-0006). O M2
> colapsa em R-only os **três** conceitos de data que a frase "cortar dados pra trás" funde perigosamente —
> hoje espalhados por 3 linguagens na referência (o corte honesto no bash do harness, o forecast-origin no
> argparse Python, o corte intradiário no Python, o "último dia completo" só no adapter R do LGBM). E sobe
> **cedo** o `validate-deck`: barato, só depende da fundação, **protege compute** (falhar antes de gastar a
> manhã) e **falsifica a camada de datas** antes de combina (M4) e training (M5). ⚠ **Nada numérico de modelo
> migra aqui** — M2 é `dates.R` + `validation.R` + o verbo `validate-deck`. O sistema de produção segue em
> `/opt/ons-source/prevcargadessem3` (oráculo do A/B) até o corte no M8.

---

## 0. Evidência (o que a referência faz hoje)

Levantado por varredura de `/opt/ons-source/prevcargadessem3` (congelado, 2026-10-11, via Explore). ⚠ **Todos os
`path:line` do §0 são do oráculo congelado `/opt/ons-source/prevcargadessem3/` (repo-relativos NESSE repo — que é o
cwd do processo) — a maioria NÃO existe no clean-room**; são o que o M2 substitui/subsume/unifica, não arquivos-alvo.
Os arquivos-alvo (clean-room `/opt/ons-source/prevcarga`) estão em §Superfícies. ⚠ **Colisão de nome a notar:**
`scripts/prevcarga` do §0 é a **CLI Python do oráculo** (~485 linhas; as linhas `:122-134/:152-178/:363/:485/:493` são
DELA) — homônimo mas distinto da **fachada R do clean-room** `scripts/prevcarga` (~40 linhas, stub docopt, citada em
§Superfícies/§M2.3 por `:24/:25`). Mesmo nome, arquivos diferentes.

**As três datas vivem em 3 lugares, em 3 linguagens** (o alvo do M2.1):
- (1) **corte-de-treino honesto — DIFERE POR PROTOCOLO** no BASH do harness (⚠ não é uniforme — ver §Decisões):
  - **origem-agnósticos (RF/LGBM):** `jl_train_origin()` em `jobs/job_lib.sh:227-230` devolve
    `first_origin − (horizon+1)` (linha `:230` `jl_date_minus "$first_origin" "$(( horizon + 1 ))"`) = `origem − 1 −
    horizonte` — o `−1` extra fecha o off-by-one em que o rótulo do 1º dia de previsão vazaria pro fit (artefato
    reusado cross-origem com shift honesto, ADR-0011). `horizon_days` lido em `jobs/run_job.sh:94`; as **três** chamadas
    de `jl_train_origin` — `run_job.sh:215` (split-train), `:302` (fixed), `:350` (daily) — são **todas** no caminho
    RF/LGBM (os três modos origem-agnósticos; `protocol=daily` é um deles). `jl_date_minus` em `job_lib.sh:183`.
  - **origem-escopados (dessem-*/HW, `protocol=retrain_harness`):** **NÃO** chamam `jl_train_origin`. Re-fitam na
    **própria origem O**, com a série ancorada em **O−1** (dessem `models/dessem-linear/R/ingestion/data_adapter.R:455`
    `series_end <- origin_day - 1L`; HW cap estrutural em O−1, `run_job.sh:202-204`) + a manhã de O ≤ corte via
    `--available-until` (dessem `data_adapter.R:174/:231`). Exigem `FROM==TO` (`run_job.sh:208`). O corte honesto deles
    é **O−1 + available_until**, não o shift `O−1−horizonte`. (Confirmado por varredura, 2026-10-11; alinha ao
    CLI-OBJETIVO §4.1 "RF C−10, LGBM C−2, HW/dessem **--retrain**".)
- (2) **forecast-origin** é `--origem` (YYYY-MM-DD) **required, SEM default** no CLI Python — `scripts/prevcarga:485`
  (treinar), `:493` (prever). O "último dia completo do deck" **não** é o default da CLI: vive como auto-detect do
  `available_until` SÓ no adapter LGBM — `models/lgbm/R/ingestion/data_adapter.R:655-664` (`available_until_ts <-
  max(real_rows)`, última linha com `CargaGlobal` não-NA e `Data <= origin_day`). O walk-forward constrói o instante
  do `--cut` (default `08:00:00`) em `scripts/walk_forward.R:12` + `.build_available_until():344`.
- (3) **corte-intradiário** lido do dado: `scripts/prevcarga:122-134` lê `<dados>/manifest.json` chave `corte`
  (`:134` `json.loads(txt).get("corte")`); **recusa rodar sem corte** `:163-164` (`sys.exit('corte AUSENTE …')`);
  carimbado de volta na proveniência em `:363`. ⚠ O **runtime** usa a coluna canônica `issue_time` (ver item do
  clima); o nome legado `din_origemprevisaoutc` aparece **só na tabela de alias do data-prep**
  (`scripts/data-prep/migrate_legacy_data.py:54`, `"din_origemprevisaoutc": "issue_time"`; comentários `:338/:364/:366`),
  resolvido 1× no ingest cru→canônico — nunca no runtime/adapters.

**A validação vive em 3 lugares dispersos e INCOMPLETOS** (o alvo do M2.2):
- `scripts/lib/validate_input.R` (81 linhas): `prevcarga_contract_error` via `.contract_stop():24-32`; asserts
  `assert_non_empty():36`, `assert_no_na():43` (presença `:46` + zero-NA `:47-49`), `assert_no_missing_days():55`,
  `assert_covers_days():69` (`:74` `short <- which(cnt < slots_per_day)`). ⚠ **Ausente** aqui: checagem de **carga>0**,
  de **tipo/dtype** (só presença, `:46`), e de **tz** explícita (o header `:14` só manda `lubridate::date()`).
- `scripts/tools/scan_completude_24slots.R` (143 linhas): completude **flat-48** — roda sob `Sys.setenv(TZ="UTC"):25`,
  exige os 2 slots/hora (`:63` `complete := nslot >= 2L`, em `hour_completeness():57`) e marca dia ruim em `:88`
  (`ncomp < 24L` → 24h/48 slots). ⚠ **O 48-fixo está CERTO, não é bug** (correção de premissa — ver §Decisões): o store
  é flat-48 porque o DST **já foi absorvido na malha** (ADR-0003) — o pivot de carga força todo dia ao grid H01..H48
  (`holt-winters/.../data_adapter.R:178`), preenche slots ausentes com NA (`:184-185`) e colapsa a hora repetida do dia
  de 25h com `mean` (`:175`); RF declara "Pre-2019 DST out of scope" (`random-forest/.../data_adapter.R:135`); o flag
  `HV` de DST é binário 0/1 (`holt-winters/.../data_adapter.R:296`), não um comprimento-de-dia; `auxiliary/dst_periods`
  **não é lido por ninguém**. **Nenhum** contador no repo conta 46/50 slots. O M2 **porta o 48-fixo** e **adiciona** o
  que o scan não cobre (tipo/tz, sanidade física, clima honesto, janela).
- `scripts/tools/analyze_data_quality.py` (548 linhas, **Python** — a portar pra R): `detect_outliers_iqr():84`
  (1.5×IQR), `detect_outliers_zscore():94` (3.0), `analyze_timeline_gaps():100` (grade 30-min `:112`),
  `analyze_load_data():161` (null `:188-189`, outlier `:193-204`, **zero** `:216-219` `zero_count=(load_series==0).sum()`),
  `analyze_weather_observed():236` (**faixa de clima −10..50 °C** `:281` `out_of_range=((temp<-10)|(temp>50))`, corpo `:280-284`). Não-wired; não sobrevive ao R-only.

**⚠ CORREÇÃO DE PREMISSA (o repositório vence o PLANO/stub).** O stub `scripts/lib/validation.R:19` e o PLANO §M2
dizem "reprova deck que RF/LGBM **comeriam calado com fill-0**". **Isso é FALSO da referência:** uma varredura ampla
(`nafill(...,fill=0)`/`fcoalesce(...,0)`/`replace_na(...,0)`/`fillna(0)`/`is.na(...)<-0`) em `models/`+`scripts/`
retornou **zero** hit pra carga. A referência **mantém a carga faltante como NA** e **falha alto** (`validate_input.R`)
ou **trunca honesto** (LGBM `models/lgbm/R/ingestion/data_adapter.R:625-630` junta `all.x=TRUE`, gaps ficam NA; `:674-677`
trunca pós-corte pra `CargaGlobal=NA_real_`+`is_synthetic=TRUE`). O **único** `nafill` é **LOCF em CLIMA**, nunca 0:
RF `models/random-forest/R/ingestion/data_adapter.R:291-292` (`nafill(tmax, type="locf")`). Logo o `validate-deck` do
M2 **unifica e antecipa** os fail-high hoje dispersos num módulo standalone, **adiciona** o que falta (carga>0, tipo,
DST-aware, janela, clima honesto no nível-deck) e **impede a regressão** pra fill-0 silencioso no R-only — **não**
conserta um bug de fill-0 que não existe. (Ver §Decisões.)

**Honestidade do clima JÁ existe — espalhada nos adapters** (o M2.2 sobe pro nível-deck): o filtro `issue_time ≤ O`
vive em cada modelo — HW `models/holt-winters/R/ingestion/data_adapter.R:362` (`raw <- raw[issue_utc <= issue_cutoff]`,
`issue_cutoff` = meia-noite-UTC-em-O `:328`, dedup da emissão mais fresca `:388-394`); RF em forma de conjunto-de-datas
`models/random-forest/R/ingestion/data_adapter.R:236` (`raw_wx[issue_time %in% issue_times_utc]`). Coluna chamada
`issue_time` em todo lugar (instante cheio, ADR-0003); contrato documentado em `models/dessem-linear/MANIFEST.md:38`
(`issue_time (≤ O)`). O M2 **reporta antes do compute**, não filtra em silêncio.

**Janela mínima por membro** (o M2.2 lê, não hardcoda): são **duas chaves distintas** — `history_days.train`
(janela de TREINO) e `history_days.infer` (janela de INFERÊNCIA). `jobs/run_job.sh:97` lê `HIST_TRAIN` com o
**default de bash 730**, `:98` lê `HIST_INFER` com default **45**; mas o valor efetivo vem do `run.yaml`
**por-modelo** (o default 730 é só o fallback quando o manifest não declara). Os valores de treino por-membro
(M1.2 seed): HW **730**, RF **365**, LGBM **730**, dessem-linear/regdin **1825** (5 anos). ⚠ A inferência é **45** p/
RF/LGBM/HW, mas **NÃO comum a todos**: `run_job.sh:104` (`dessem-*) HIST_INFER=HIST_TRAIN`) **casa infer=train=1825**
p/ dessem (senão o schema de colunas do infer diverge do train). Início da janela por `jl_history_start`
(`job_lib.sh:233-235`), aplicado em `run_job.sh:218,303,318,351-352`.

**Store parquet Hive `area_code=/year=`** (o M2.2 abre): `arrow::open_dataset` em HW `data_adapter.R:145` (load),
`:208/:263/:330`; RF `:139` (load `:127`), `:206`, `:345`; LGBM lê por-partição explícita `data_adapter.R:170-182`
(`load/area_code=<AREA>/year=<Y>/data.parquet`). Layout em `models/holt-winters/README.md:259`,
`models/random-forest/README.md:286`, `models/dessem-linear/MANIFEST.md:36`.

**A combina VIOLA o corte** (contexto — fora do escopo do M2, é o M4): `scripts/combina_baseline.py:190`
(`panel["ym"] = panel["forecast_origin"].str.slice(0,7)`) + `:271` (`tr = ymv < m`, `ymv` em `:262`) treina pesos por
mês-de-origem **sem subtrair o horizonte** → leak. Corrigido só no M4 (ADR-0014, [[combina-vazamento-horizonte]]).

**Exit-codes: a referência não tem taxonomia estável** — argparse + `subprocess check=True` propaga `1`; o
`_validate_contract` da CLI Python (`scripts/prevcarga:152-178`) usa o label `[prevcarga]`/`prevcarga:` (ex. `corte
AUSENTE`), **sem** classe. O prefixo estruturado `[prevcarga/contract]` só existe no R, em `.contract_stop()`
(`scripts/lib/validate_input.R:25`, `sprintf("[prevcarga/contract] area=%s origin=%s …")`). O M2 passa a emitir 2/3/4/5 (ADR-0020).

## O modelo (alvo do M2)

Dois módulos na `scripts/lib/` do clean-room, hoje **stubs que `stop("… TODO M2")`** — `dates.R` (`:15/:20`) e
`validation.R` (`:21`) — mais o verbo `validate-deck` na fachada `scripts/prevcarga` (já na gramática docopt `:25`,
hoje caindo no `stop()` de não-dono do M1.4). O M2 os implementa: `dates.R` deriva os **3 cortes** do único `--as-of`;
`validation.R` dá o veredito estruturado model-agnóstico; `validate-deck` compõe os dois com exit-codes por classe.
⚠ **M2 pressupõe M1 implementado** (dispatcher docopt, `prevcarga_runtime_enforce()` runtime-first, o handler
prefixo→exit-code, `topology.R`/`model_spec`) — é **dependência**, não escopo do M2.

---

## Dependência de M1 (criada lá; **ainda NÃO materializada** no clean-room)

⚠ **M2 pressupõe M1 implementado.** O PLANO ordena M1 → M2; o M1 está ✅ VERIFY-READY mas **ainda não foi
implementado**, e o clean-room tem **0 commits** — logo, no HEAD atual, as peças abaixo **ainda não existem**
(nem como stub, no caso dos configs). Elas são **deliverables do M1** que o M2 **consome**, não cria nem
re-hardcoda — a ausência é **sequenciamento** (M1 pousa primeiro), não lacuna do M2. (Quando o `implement`
do M2 rodar, o M1 já terá pousado e elas existirão; é por isso que o `verify` as vê ausentes agora.)

- **`config/topology.yaml`** — criado pelo **M1.2** (`M1-FUNDACAO.md:101`); o M2 consome via `prevcarga_topology()`/
  `prevcarga_members()` (escopo-área: `area_code` no deck sem declaração no config = fail-high).
- **`models/<m>/run.yaml` × 5** — semeados pelo **M1.2** (`M1-FUNDACAO.md:102`); o M2 consome via
  `prevcarga_model_spec()` (`horizon_days` p/ o corte-de-treino, `history_days.train`/`.infer` p/ a janela).
- **Fachada docopt + `prevcarga_runtime_enforce()` runtime-first + handler prefixo→exit-code** — M1.4
  (`M1-FUNDACAO.md` §M1.4); o M2 pendura `cmd_validate_deck` nessa máquina.
- **`prevcarga_model_spec(model, root = ...)`** — M1.2 adiciona a alavanca `root` de injeção de path
  (`M1-FUNDACAO.md:209`); o stub atual `scripts/lib/topology.R:22` é `function(model)` (sem `root`) — o `root` é
  deliverable do M1, consumido pelos ACs de fixture do M2.

## Superfícies que isso toca (clean-room `/opt/ons-source/prevcarga`)

- [ ] `scripts/lib/dates.R` — `prevcarga_train_origin()`/`prevcarga_resolve_cuts()` (hoje stub `:15/:20`). **(M2.1)**
- [ ] `scripts/lib/validation.R` — `prevcarga_validate_deck()` + `prevcarga_day_complete()` (hoje stub `:21`). **(M2.2)**
- [ ] `scripts/lib/provenance.R` — **novo primitivo** `prevcarga_data_hash(data_dir)` (digest store→fingerprint; o módulo é o dono do content-addressing, M1.3). **(M2.3)**
- [ ] `scripts/prevcarga` — `cmd_validate_deck()` + wiring do `class`→exit-code do veredito + re-liga `cmd_describe` (origem + corte-de-treino `daily` via `train_origin`; retrain_harness = "[M5]"); verbo já na gramática `:25` (hoje `stop()` de não-dono do M1.4). **(M2.3)**
- [ ] `docs/ESTADO-ATUAL.md` — write-through "Onde estamos": M2 vivo (3 cortes + validate-deck) **+ a correção de premissa do fill-0**. **(DoD cross-cutting)**
- [x] `.claude/workflows/implement-milestone.js` — entrada `CHECKPOINTS['M2']`/`'M2-DATAS'` → `_M2_CP` **(feito pré-implement; ver §Checkpoint)**
- [ ] `scripts/lib/tests/` ou `tests/` — suíte testthat nova do M2 (fixtures de deck parquet Hive minúsculos em tmp). **(todos os tickets)**
- [ ] `docs/reference/DECK-CANONICO.md` — **consumido** (fonte única do contrato de deck: datasets/colunas R/O, tipos, Hive, emissão). NÃO muda no M2; o enum-fino do código de feriado é [DEFERRED] a formalizar aqui (ver M2.2 Step 2). **(cross)**
- [ ] `scripts/lib/README.md` — write-through PARCIAL: só a linha de `validation.R` ("completude DST-aware" → **flat-48**) corrige **agora** (alinha ADR-0002/0003). A linha de `dates.R` ("treino origem−1−horizonte por modelo") **espelha o ADR-0006** e sob a **opção (b)** fica **INTACTA e conforme** (o M2.1 só cobre `daily`=origem−1−horizonte; a reconciliação por-protocolo é do M5 + ADR superseder — não reescrever). **(DoD cross-cutting)**
- *(consumidos de M1, não tocados: `config/topology.yaml`, `models/<m>/run.yaml`×5 — ver §Dependência de M1 acima.)*

---

## Tickets

Cada gate NOVO que afirma Δ=0/reprovação/aceite carrega **CONTROL com dente** (braço que DEVE divergir) — senão o
FORBIDDEN Δ=0 é vácuo (disciplina herdada do oráculo `/opt/ons-source/prevcargadessem3/jobs/tests/test_prevcarga_honesty.sh:14-16`;
molde no clean-room = `scripts/lib/honesty_gate.R`, stub do M3).

### M2.1 — `dates.R`: os TRÊS cortes derivados do único `--as-of` (corte-de-treino POR-PROTOCOLO)

**Causa:** §0 — as 3 datas vivem em 3 linguagens (corte-de-treino no bash `jl_train_origin` `job_lib.sh:230` **só p/
RF/LGBM**; forecast-origin `--origem` required Python `prevcarga:485/:493`; corte-intradiário `manifest.json` 'corte'
Python `prevcarga:122-134`; "último dia completo" só no adapter LGBM `data_adapter.R:655-664`). "Cortar dados pra trás"
as funde — e um corte único pra todos vazaria horizonte (ADR-0006). ⚠ **O corte-de-treino DIFERE por PROTOCOLO** (não
só por horizonte): origem-agnósticos `daily` (RF/LGBM) usam `origem−1−horizonte`; origem-escopados `retrain_harness`
(dessem-*/HW) re-fitam na própria origem O com série em O−1 (ADR-0011; §0). ⚠ **ESCOPO DO M2.1 = protocolo `daily` só**
(RF/LGBM) — ver §Decisões "opção (b)": o corte `daily` (`origem−1−horizonte`) é derivável puramente de datas E é
**exatamente o que o ADR-0006 ratifica**; a semântica de corte do `retrain_harness` (série O−1 + `available_until`) é
**training**, nasce no **M5** (ADR-0011 "honrado no M5"), e afirmá-la num gate executável aqui **contradiria o ADR-0006
Aceito** sem um superseder (que é ato do Gabriel). Então o M2.1 **não** ramifica dessem/HW agora.

**Steps:**
1. `prevcarga_train_origin(as_of, model, root = ...)` (`dates.R:15`): lê `protocol`+`horizon_days` via
   `prevcarga_model_spec(model, root)` (M1.2) e:
   - `protocol == "daily"` (RF/LGBM): devolve `as_of − 1 − horizon_days` (shift honesto do artefato origem-agnóstico —
     porta `jl_train_origin` `job_lib.sh:230`; é o `origem−1−horizonte` do ADR-0006).
   - `protocol == "retrain_harness"` (dessem-*/HW): **`stop("[prevcarga/todo] corte de retrain_harness derivado no M5")`
     — [DEFERRED M5].** O corte honesto deles (série O−1 + manhã ≤ `available_until`) é semântica de training, não de
     datas puras; o M5 (dono do training) o deriva, junto com o ADR superseder do 0006 (ver §Decisões). O M2.1 **não**
     afirma `O−1` num gate executável nem no cutoff de proveniência — evita contradizer o ADR-0006 Aceito.
   ⚠ **NUNCA** um valor único run-level entre os modelos daily. `as_of`/retorno são `Date` naive-BRT (ADR-0003; aritmética
   em dias-civis, sem conversão de tz). `root` = **alavanca de injeção de path** (default `models/`, sobrescrevível por
   `PREVCARGA_CONFIG_DIR`) pros ACs apontarem a fixtures (precedente M1.2 `topology.R`).
2. `prevcarga_resolve_cuts(as_of, data_dir, model = NULL)` (`dates.R:20`): recebe o **caminho do store** `data_dir`
   (não o manifest já parseado — precisa varrer o parquet p/ o "último dia completo" E ler o `manifest.json`); devolve
   a lista nomeada `{forecast_origin, train_cut, intraday_cut}`, **os três ancorados na MESMA origem resolvida**:
   - `forecast_origin` = resolve a origem PRIMEIRO: `as_of` se dado; se `NULL`, default = **último dia COMPLETO** do
     store (varre `data_dir` chamando o **helper único `prevcarga_day_complete()`** exposto pelo `validation.R`, M2.2 —
     flat-48, **não** um contador paralelo de 24-slots; realiza o auto-detect que hoje só vive no adapter LGBM
     `data_adapter.R:655-664`).
   - `train_cut` = `prevcarga_train_origin(forecast_origin, model)` para um modelo `daily`, derivado da **origem
     resolvida** (⚠ não do `as_of` cru: com `as_of=NULL` os 3 cortes compartilham a MESMA origem). Com `model=NULL`,
     devolve a tabela por-modelo dos **daily** (RF/LGBM) + marca os `retrain_harness` (dessem/HW) como **"[M5]"** (não um
     corte computado) — **nunca** um escalar único (o escalar único run-level é o anti-padrão de leak do ADR-0006).
   - `intraday_cut` = o `corte` lido de `<data_dir>/manifest.json` (ADR-0007; porta `_cut_from_data`
     `prevcarga:122-134`). ⚠ **NUNCA** flag humana em produção; `stop("[prevcarga/contract] …")` (exit 2) se ausente
     (espelha `prevcarga:163-164`). O `data_dir` é também a **alavanca de injeção** dos ACs de fixture (irmã do `root` de
     `train_origin`): um store-fixture em tmp exercita o default-de-origem sem tocar o `data/` real.
3. Tempo **naive-BRT** (ADR-0003): aritmética de `Date` em dias-civis; `intraday_cut` é **instante** semi-horário
   (hora `h` só é completa se `h:00` **E** `h:30` ≤ `corte`, ADR-0007), não uma data. Sem conversão de tz.
4. Prefixo de erro estável + exit-code (handler do M1.4, ADR-0020): `model` inexistente ou `run.yaml`/manifest
   faltante/malformado → `[prevcarga/contract]` (exit 2, via `model_spec`). ⚠ `dates.R` **não** valida área nem emite
   `[prevcarga/topology]`/completude/janela — esses são do `validate-deck` (M2.2/M2.3); o único exit que `dates.R` emite
   é **2** (contract).

**AC:**
- **CONTROL (dente — gate #1a: o horizonte morde DENTRO do `daily`):** com um ÚNICO `as_of`,
  `prevcarga_train_origin(as_of, "random-forest")` (daily, h9 → `as_of−10`) e `… "lgbm")` (daily, h1 → `as_of−2`)
  **DIVERGEM em exatamente 8 dias** (`expect_equal(as.integer(rf_cut - lgbm_cut), -8)`). Um impl que devolva corte único
  no ramo daily **falha** — prova que o corte daily é função do horizonte, não vazando (ADR-0006).
- **retrain_harness é [DEFERRED M5] — não afirma corte num gate (opção b):** `prevcarga_train_origin(as_of, "dessem-linear")`
  e `… "holt-winters")` **param** com `[prevcarga/todo] … M5` (`expect_error(..., "M5")`); o M2.1 **não** asserta
  `as_of−1` pra eles (isso seria afirmar, num gate executável, um corte que contradiz o ADR-0006 Aceito sem superseder —
  §Decisões). A semântica de corte do retrain_harness é exercida no M5, junto com o ADR superseder do 0006.
- **FORBIDDEN (Δ=0 — pureza/independência no ramo `daily`):** (a) `prevcarga_train_origin(as_of, "random-forest")` chamada
  2× é idempotente (Δ=0); (b) editar `horizon_days` de `lgbm` no fixture **não move** o corte de `random-forest`
  (independência — cada daily lê o próprio horizonte). (⚠ A invariância-ao-horizonte do retrain_harness é [DEFERRED M5].)
- **corte-de-treino exato (porta `jl_train_origin` no ramo daily):** `prevcarga_train_origin(as.Date("2024-06-11"),
  "lgbm") == as.Date("2024-06-09")` (daily h1: −1−1) e `… "random-forest") == as.Date("2024-06-01")` (daily h9: −1−9),
  batendo `job_lib.sh:230`. Editar o `horizon_days` do fixture de um daily reflete no retorno (prova que lê `horizon`,
  não replica). *(retrain_harness não tem corte exato aqui — [DEFERRED M5].)*
- **corte-intradiário vem do manifest, NUNCA de flag (anti-leak ADR-0007):** com um store-fixture cujo
  `<data_dir>/manifest.json` tem `{"corte":"08:00:00"}`, `prevcarga_resolve_cuts(as_of, data_dir)$intraday_cut`
  devolve o instante do manifest; um store-fixture **sem** `corte` no manifest → `stop()` com `[prevcarga/contract]`
  (exit 2, espelha `prevcarga:163-164`). `grep` prova que `resolve_cuts` lê o corte intradiário SÓ do `manifest.json`
  do `data_dir`, não de um arg/flag humano. *(A varredura `--corte` do backtest é M7 — [DEFERRED], fora do escopo do M2.)*
- **forecast-origin default = último dia completo, E os 3 cortes compartilham a origem resolvida:**
  `prevcarga_resolve_cuts(as_of = NULL, data_dir = <store-fixture>, model = "lgbm")` devolve `forecast_origin` = o último
  dia **COMPLETO** do store-fixture (não um dia parcial — o fixture tem um último dia com slots faltando, que NÃO deve
  ser escolhido; equivale ao `available_until` de `data_adapter.R:655-664`), **e** `train_cut == forecast_origin − 1 − 1`
  (derivado da origem RESOLVIDA, não de `NULL` → prova a coerência das 3 datas); com `as_of` explícito, `forecast_origin`
  é verbatim (`expect_identical`).
- **`forecast_origin` ≠ `train_cut` (anti-confusão das 3 datas):** `resolve_cuts(.., model="lgbm")` devolve
  `forecast_origin` e `train_cut` como campos DISTINTOS (`expect_false(identical(res$forecast_origin, res$train_cut))`)
  — os dois não colapsam num só (o colapso é o anti-padrão ADR-0006).
- **Predicado de completude é fonte única (dente anti-contador-paralelo flat-48):** um store-fixture cujo último dia
  tem **47 de 48** slots (uma meia-hora faltando) → `resolve_cuts(as_of=NULL)` **NÃO** escolhe esse dia como origem
  (escolhe o anterior, de 48) **e** `prevcarga_day_complete(esse_dia)==FALSE`, o MESMO veredito que `validate_deck` daria
  (`expect_equal`). ⚠ **Por que 47 discrimina:** o limiar **stale** do stub (`validation.R:10` "≥24 slots/dia") ACEITA 47
  (47 ≥ 24), enquanto o flat-48 o REJEITA (47 ≠ 48) — um `resolve_cuts` com contador próprio `≥24` pegaria o dia de 47
  como origem e divergiria do `validate_deck`. Este braço reprova esse drift (prova que os dois chamam o mesmo
  `prevcarga_day_complete` flat-48, não contadores paralelos).
- **Travessia dates→provenance exercida AQUI (daily; não só na DoD):** `prevcarga_train_origin(as_of, "random-forest")`
  devolve o valor que entra como o campo **`cutoff`** do fingerprint de `prevcarga_version_hash` (M1.3) — um teste monta
  o fingerprint de um membro daily com esse `cutoff` e confirma que perturbá-lo (±1 dia) muda o `version_hash` (liga
  dates→provenance; `cutoff` é componente distinto do `data_fingerprint`). *(O cutoff de proveniência do retrain_harness é
  [DEFERRED M5], junto com o ADR superseder do 0006.)*
- **`resolve_cuts(model=NULL)` devolve tabela POR-MODELO, nunca escalar (anti-leak ADR-0006, risco #1):**
  `prevcarga_resolve_cuts(as_of, data_dir, model = NULL)$train_cut` é uma estrutura **por-modelo** (um por membro,
  nomeado) — `expect_true(length(res$train_cut) > 1)`; os **daily** diferem por horizonte (RF `O−10` ≠ LGBM `O−2`) e os
  **retrain_harness** aparecem marcados `"[M5]"` (não um corte computado) — **nunca** um único escalar aplicado a todos (o
  escalar run-level é o anti-padrão de vazamento do ADR-0006). Um impl que colapse num escalar falha o braço.

### M2.2 — `validation.R`: validação model-agnóstica do deck (schema/tipo + completude flat-48 + sanidade + clima honesto + janela)

**Causa:** §0 — a validação vive em 3 lugares dispersos e INCOMPLETOS (`validate_input.R` sem carga>0/tipo/tz;
`scan_completude_24slots.R` 48-fixo mas isolado/não-wired; `analyze_data_quality.py` em Python não-wired). ⚠ **Duas
premissas foram corrigidas pelo repo** (§0 Correção): (1) a referência **não** preenche carga com 0 — mantém NA + falha
alto / trunca honesto; (2) a completude é **flat-48**, não DST-aware (o DST foi absorvido na malha, ADR-0003; o 48-fixo
do scan está CERTO). O M2.2 **unifica e antecipa** os fail-high num módulo standalone, **adiciona** o que falta
(carga>0/tipo/tz/sanidade/clima/janela) e **impede a regressão** pra fill-0 silencioso — não conserta bugs inexistentes.

**Steps:**
1. `prevcarga_validate_deck(data_dir, as_of, scope = NULL, format = "text")` (`validation.R:21`): abre o store parquet
   Hive `area_code=/year=` (`arrow::open_dataset`, como `holt-winters/.../data_adapter.R:145`) e roda as 5 classes de
   checagem **acumulando** um veredito (NÃO aborta no 1º erro — coleta todos por origem/área, ADR-0005 fail-por-origem).
   Devolve `list(ok=<lgl>, class=<chr>, findings=<data.frame: area/dataset/what/detail/class/level>)` — ⚠ **NÃO** devolve
   `exit_code`: o módulo computa só a `class` dominante; o inteiro é mapeado **uma vez** pelo handler do M1.4 (ADR-0020,
   fonte única) dentro do `cmd_validate_deck` (M2.3), sem duplicar a tabela classe→código.
   ⚠ O campo **`what`** de cada finding é um **código estável** (ex. `schema_missing`, `type_mismatch`, `day_incomplete`,
   `load_nonpositive`, `climate_future`, `window_train_deficit`, `window_infer_deficit`, `area_undeclared`,
   `outlier`, `flatline`) — as ACs casam `what` (estruturado), não substring de `detail` (texto-livre humano).
   ⚠ Cada finding tem **`class`** ∈ {contract, completude, topology, window} **e** **`level`** ∈ {error, warning}. `ok`
   e a **class-dona** olham **só os findings `level=="error"`** (a precedência de `class` roda sobre eles; o `exit_code`
   em si é derivado depois pelo `cmd_validate_deck`); findings `level=="warning"` (outlier, flatline) aparecem no veredito
   mas **não** reprovam. ⚠ **Âncoras temporais:** o `as_of` ancora a
   **janela** (history antes de `as_of`); o **clima honesto** ancora no `intraday_cut` (INSTANTE), que o `validate_deck`
   lê de `<data_dir>/manifest.json` — a MESMA fonte que o `resolve_cuts` (M2.1) usa (`validate_deck` tem `data_dir`, logo
   deriva o instante ele mesmo, sem precisar de um parâmetro `intraday_cut` na assinatura). Ver Step 5.
2. **FORMATO/SCHEMA/TZ (classe `contract` → exit 2):** presença das colunas **Required** de cada dataset (DECK-CANONICO)
   + **TIPO** correto (`timestamp[ns]`/`double`/`string`/`date32[day]`/`int64`) + **tz naive-BRT** (a coluna `timestamp`
   é `timestamp[ns]` **sem** fuso — relabel-UTC, nunca tz-aware `America/Sao_Paulo`; ADR-0003) + particionamento Hive
   `area_code=/year=` + **código de feriado**: `int64` + a faixa que o **deck canônico (fonte única) declara** —
   `prevcarga_code ≥ 1` (DECK-CANONICO:53/:58 "LGBM prevcarga_code≥1") e `holiday_type_id`/`simplified_code` **não-negativos**
   (DECK-CANONICO só declara `int64` p/ estes dois). ⚠ **O domínio FINO por-código (enum) NÃO é imposto pelo M2** —
   `simplified_code ∈ {0,1,2}` (`holt-winters/.../data_adapter.R:274-275`) e `prevcarga_code ∈ {0..12}`
   (`dessem-linear/.../data_adapter.R:382`, vendor `pkgfun_entradaPerfil.R:53` `tpFer=1:12`) vivem nos **adapters/vendor**,
   não no deck canônico; o `0..12` do stub `validation.R:9` é impreciso (erra `simplified_code`). Impor o enum por-código
   sem fonte única seria o mesmo pecado do "faixa-por-subsistema" (ADR-0001) — então o M2 valida **tipo + a faixa
   canônica do deck** (prevcarga_code≥1, demais ≥0); o **enum fino é [DEFERRED]**
   (candidato a formalizar em DECK-CANONICO). Coluna R ausente / tipo errado / tz-aware / código negativo → finding
   `[prevcarga/contract]`. Colunas **Optional** validadas por tipo SÓ se presentes (nunca exigidas). ⚠ **ADICIONA** o que
   `validate_input.R:46` não tem: **TIPO** (hoje só presença), **tz** (hoje só `:14` manda `lubridate::date()`) e
   **não-negatividade** dos códigos.
3. **COMPLETUDE flat-48 (classe `completude` → exit 3):** um **predicado único exposto** `prevcarga_day_complete(day)`
   (em `validation.R`) decide "dia completo = **48 slots/dia** (:00/:30)", uniforme (ADR-0002 "a dimensão 48 fixa a
   matriz"; DST absorvido na malha — ADR-0003 — **não** há 46/50). ⚠ **É o MESMO predicado que o `resolve_cuts` (M2.1)
   chama** para o "último dia completo" — fonte única, não um contador paralelo de 24-slots. Dia ≠ 48 (slot faltante /
   carga NA num slot) → finding `[prevcarga/completude]`. **Porta o 48-fixo do `scan_completude_24slots.R:88` (que está
   CERTO — §0 Correção), não o "corrige"**; unifica `assert_covers_days:69`+`assert_no_na:43` num ponto único — é **aqui**
   que o deck que seria consumido com NA é REPROVADO antes do compute. ⚠ O limiar é **48**, não o `≥24` que o stub
   `validation.R:10` e o `ESTADO-ATUAL.md:96` ainda citam (write-through corrige ambos).
   ⚠ **Carve-out da cauda parcial (anti-falso-positivo D+0/nowcast):** a checagem 48-slots vale só para os dias de
   **histórico** `< forecast_origin` (resolvida). O **dia da origem** (e qualquer cauda parcial até o `intraday_cut` — num
   snapshot D+0, "hoje" é parcial por construção) é validado contra o `intraday_cut` (hora `h` completa só se `h:00` E
   `h:30` ≤ corte, ADR-0007), **não** contra 48 — senão um deck de produção D+0 legítimo (cauda de hoje parcial) seria
   reprovado pelo gate-0 que deveria **proteger** o compute. Alinha com o `resolve_cuts`, que escolhe a origem como o
   último dia completo e trata a cauda parcial como região-de-corte, não furo.
4. **SANIDADE FÍSICA:** **hard-fail `[prevcarga/contract]` (exit 2)** — **carga>0** (⚠ ADICIONA — `validate_input.R` não
   tem; cobre o zero-count da referência `analyze_data_quality.py:216-219`), **dedup** (timestamp duplicado por área —
   `group_by(area_code, timestamp)` count>1, propriedade de conjunto bem-definida), **faixa de clima −10..50 °C**
   (constante física universal — porta `analyze_weather_observed:281`, corpo `:280-284`). ⚠ **Dois sub-checks são
   `level="warning"`, NÃO hard-fail** (não entram no exit-code; só aparecem no veredito p/ inspeção — ambos heurísticos,
   sem precedente de hard-fail na referência, e propensos a falso-positivo): **(i) outlier** (IQR 1.5× / z-score 3.0,
   porta `:84/:94`) — um IQR global dá falso-positivo em picos sazonais legítimos; **(ii) flatline/stuck-sensor** (carga
   **exatamente** idêntica, bit-a-bit, por ≥ 6 slots) — pode bater numa área pequena quantizada legítima. Reprovar por
   qualquer um mataria deck real no gate-0 (ver §Decisões). ⚠ **Monotonicidade do timestamp NÃO entra** — sobre um store
   Hive aberto por `open_dataset` a ordem de linhas cross-arquivo não é invariante do deck (seria testar a ordem de
   leitura, não um fato); o que importa — sem gaps nem duplicatas — já é coberto por **completude flat-48 + dedup**.
   ⚠ **Faixa de carga por subsistema NÃO entra** — nem `topology.yaml`
   (árvore/perdas/membros) nem `run.yaml` (horizon/history/protocol/seed/params) carregam limites de MW, e hardcodá-los
   no R violaria o ADR-0001. `carga>0` + o warning de outlier cobrem a intenção de sanidade sem tabela de limites sem-fonte.
5. **HONESTIDADE DO CLIMA (classe `contract` → exit 2):** `weather/forecast` com `issue_time` **depois** do INSTANTE de
   corte (o `intraday_cut` que o `validate_deck` lê de `<data_dir>/manifest.json` — a mesma fonte do `_cut_from_data`/
   `resolve_cuts`, ADR-0007) → finding `[prevcarga/contract]` — **não** o `as_of` nível-dia (mais frouxo, deixaria passar
   emissão do mesmo dia após o corte; ADR-0007:13 "região proibida por ISSUE_TIME, corte = INSTANTE").
   ⚠ **Reconciliação de frame (obrigatória — senão erra ~3h em silêncio):** `issue_time` é o único instante **genuinamente
   UTC** (00/06/12/18Z; DECK-CANONICO:33, ADR-0003), enquanto `intraday_cut` é **naive-BRT** (UTC−3). A comparação é feita
   **no frame UTC**: `issue_time (UTC) ≤ to_utc(intraday_cut)` com `to_utc(t_BRT) = t_BRT + 3h`. Comparar cross-frame sem
   converter é exatamente o ±horas que o ADR-0007 existe para impedir. (A **regra de dia-de-origem + horizonte UTC-safe**
   fica pro M5 — DECK-CANONICO:75, [[clima-previsto-emissoes]]; o M2 só precisa do filtro de honestidade no frame certo.)
   **Unifica no nível-DECK** o filtro dos adapters (`holt-winters/.../data_adapter.R:362` `issue_utc ≤ issue_cutoff`;
   `random-forest/.../data_adapter.R:236`), reportando ANTES do compute. `issue_time` instante cheio (ADR-0003).
6. **JANELA MÍNIMA por membro (classe `window` → exit 5):** p/ cada membro em `scope` (ou todos), o store cobre as
   **duas** janelas que o `run.yaml` declara — `history_days.train` (TREINO: HW 730, RF 365, LGBM 730, dessem 1825) **e**
   `history_days.infer` (INFERÊNCIA)? Ambas **por-modelo** via `model_spec` (porta `run_job.sh:97-98` + `jl_history_start`
   de `job_lib.sh:233-235`), **nunca** um literal. ⚠ **`infer` NÃO é 45 p/ todos:** RF/LGBM/HW = **45**, mas o harness
   **casa `infer=train` p/ dessem-*** (`run_job.sh:104` `dessem-*) HIST_INFER=HIST_TRAIN` → **1825**, pra o schema de
   colunas do infer não divergir do train). O seed `run.yaml` do M1.2 declara `{1825,45}` p/ dessem; essa regra de
   override `infer=train` é **semântica de training do `retrain_harness`** — reconciliada no **M5** (consistente com a
   opção b). No M2 a checagem lê `history_days.infer` do `run.yaml` por-modelo. São **chaves distintas** (train vs infer,
   findings separados). Déficit → finding `[prevcarga/window]` reportando "faltam N dias p/ modelo X" **antes** do abort profundo.
7. **ESCOPO-ÁREA (classe `topology` → exit 4):** `area_code` presente no deck **sem** declaração em `config/topology.yaml`
   (M1.2) → finding `[prevcarga/topology]` (área nova = edição consciente do config, não passthrough — Decisão do M1).
   Idem `--area`/`--model` de `scope` fora da topologia.

**AC:**
- **CONTROL (dente — deck que seria consumido calado é REPROVADO; a classe-chave do M2):** um deck-fixture com carga
  **NA** num dia (ou **0 MW**) → `prevcarga_validate_deck` devolve `ok=FALSE` com finding de classe `completude` (NA/slot
  faltante) ou `contract` (0 MW) — `expect_false(res$ok)`. Um impl que aceite tal deck falha o braço. ⚠ Prova que o
  validate-deck **antecipa** o fail-high (que na referência vive disperso em `validate_input.R`/truncação honesta) num
  ponto único, **antes** do compute — e **não** que a referência preenchia com 0 (ela não preenche; §0 Correção).
- **FORBIDDEN (Δ=0 — deck válido PASSA, sem falso-positivo):** um deck-fixture sintético **com `scope="random-forest"`**
  (⚠ `scope` pinado a UM modelo daily — senão `scope=NULL` roda a janela de dessem=1825d e uma fixture minúscula falharia
  por janela, reprovando uma impl correta), completo (**48 slots/dia**), cobrindo a janela de RF (≥365d), **bem-comportado**
  (carga>0, curva suave sem picos nem flatline) → `ok=TRUE` e **zero findings `level=="error"`** (`expect_true(res$ok)`).
  ⚠ Afere `level=="error"`, **não** `nrow(findings)==0`: o outlier é `warning` e um deck real poderia disparar um warning
  sem reprovar — o fixture limpo é suave de propósito. O invariante aferido é **ok=TRUE sem erros**. Prova que a validação
  não reprova um deck canônico correto.
- **Completude flat-48 com dente:** um dia de **histórico** (`< forecast_origin`) com **47 de 48** slots (ou carga NA num
  slot) → finding `completude` exit 3 (`expect_equal(res$class, "completude")`); um dia de histórico **completo de 48** →
  **não** reprova (`expect_true(res$ok)`). Os dois braços provam a contagem 48-fixa (ADR-0002/0003; §0 Correção) — **sem**
  exceção de DST (store flat-48; dia de 46 slots é FURO, não "DST-mola" — aceitá-lo seria falso-negativo).
- **Carve-out da cauda parcial não reprova (anti-falso-positivo D+0):** um deck-fixture (`scope="random-forest"`, janela
  RF coberta) cujo **dia da origem** (hoje) é parcial — slots só até o `intraday_cut` (ex. 08:00 → 17 slots) — com todo o
  histórico `< origin` completo (48) → `ok=TRUE`, **sem** finding `completude` (`expect_true(res$ok)`). ⚠ Dente pareado:
  mover esse mesmo déficit para um dia **interno** do histórico (47/48 num dia `< origin`) → reprova `completude`. Prova que
  a 48-completude vale só p/ o histórico, isentando a cauda parcial da origem (senão um deck D+0 real seria reprovado).
- **Schema/tipo/tz/não-negatividade falha alto (ADICIONA sobre `validate_input.R`):** (a) deck sem a coluna Required
  `load_mwh` → finding `contract` exit 2; (b) `load_mwh` como `string` em vez de `double` → `contract` (⚠ a referência
  `validate_input.R:46` só cobre PRESENÇA — prova o TIPO); (c) coluna `timestamp` **tz-aware** (`America/Sao_Paulo`) em
  vez de naive-`timestamp[ns]` → `contract` (prova a checagem de tz, ADR-0003); (d) `holiday_type_id` **negativo**
  (ex. −1) → `contract`, **`simplified_code = −1`** → `contract`, **`prevcarga_code = 0`** → `contract` (os 3 códigos: os
  dois primeiros exercitam ≥0, o `prevcarga_code=0` viola o `≥1` canônico de DECK-CANONICO:53 — prova a faixa do deck, não
  só não-negatividade; o enum FINO por-código é [DEFERRED], §Step 2); (e) um store
  **não particionado Hive** (parquet plano sem os diretórios `area_code=…/year=…`) → `contract` (prova a checagem de
  particionamento do Step 2); (f) coluna Optional `heat_index` ausente → `ok=TRUE` (optional não exigido). Cada
  sub-checagem do Step 2 (presença/tipo/tz/particionamento Hive/não-negatividade) tem seu braço — senão fecha verde com
  check ausente.
- **Sanidade física com dente (um braço por sub-checagem do Step 4 — senão fecha verde com check ausente):** injetar
  num fixture, **isoladamente**: braços **hard-fail `class=="contract"`/`level=="error"`** — (a) **0 MW / carga negativa**;
  (b) **timestamp duplicado** (mesmo `area_code`+`timestamp`); (c) **temperatura fora da faixa** (ex. 60 °C, porta
  `analyze_weather_observed:281`) — cada um → `res$ok==FALSE` com finding `contract` (`expect_true`). ⚠ Braços **warning
  (NÃO reprovam)** — (d) **outlier** (pico > 1.5×IQR / z-score > 3.0, porta `:84/:94`) e (e) **flatline** (≥6 slots
  bit-idênticos) → cada um gera finding `level=="warning"` **com `res$ok` seguindo `TRUE`** e `exit_code ≠ 2`
  (`expect_true(res$ok)` + `expect_true(any(res$findings$level=="warning"))`) — prova que são reportados sem matar o
  gate-0. O deck limpo/suave (braço FORBIDDEN acima) não gera finding `level=="error"` **nem** `warning`. ⚠ Monotonicidade
  **não** tem braço (foi removida do Step 4 — ver lá). Se qualquer sub-checagem HARD do Step 4 não tiver braço, o AC está incompleto.
- **Honestidade do clima com dente (anti-leak + cross-frame, ancorado no INSTANTE de corte):** o fixture usa
  `intraday_cut = 10:00` (BRT) → `to_utc` = **13:00Z**, e só rodadas **físicas** (00/06/12/18Z). (a) **dente anti-leak:**
  a **18Z** (`18:00Z` > 13:00Z; em BRT 15:00 > 10:00, emitida após o corte) → finding `contract` (`expect_true`). (b)
  **dente CROSS-FRAME (rodada física, sem contrafactual):** a **12Z** (`12:00Z`; em BRT 09:00, legitimamente disponível ≤
  10:00) → **sem finding** no frame certo (12:00Z ≤ 13:00Z, `expect_false`); mas um impl que compare o `issue_time` (dígitos
  UTC `12:00`) contra o corte `10:00` BRT **cru** (sem o `+3h`) → `12:00 > 10:00` → **flag espúrio** (falso-positivo que
  dropa uma emissão válida). O braço falha se a conversão de frame não for feita — e usa a 12Z **real**, não uma emissão
  sintética. As demais (00Z/06Z, `≤ 13:00Z`) → sem finding. ⚠ Com o corte típico `08:00` (=11:00Z) o bug ficaria invisível
  (nenhuma rodada física cai em `(08:00,11:00]Z`), por isso o fixture escolhe `10:00`. Unifica no nível-deck o
  `issue_utc ≤ issue_cutoff` de `holt-winters/.../data_adapter.R:362` (ADR-0007:13/ADR-0003).
- **Janela mínima por membro reporta déficit (exit 5; train E infer são chaves distintas, casadas por `what` estruturado):**
  um deck-fixture de **400 dias** → p/ `scope="holt-winters"` (`history_days.train`=730) um finding com
  `what=="window_train_deficit"`, class `window`, exit 5; p/ `scope="random-forest"` (`history_days.train`=365, semeado em
  `M1-FUNDACAO.md:204`) o MESMO deck **passa** o treino (400 ≥ 365). **Braço da inferência (dente específico):** como
  `infer=45 < train` p/ RF/LGBM/HW, um deck curto sempre reprova treino também — então o braço asserta a presença do
  finding **`what=="window_infer_deficit"`** (campo ESTRUTURADO, não substring de `detail`): um deck de **30 dias** p/
  `scope="holt-winters"` (infer=45) produz `what=="window_infer_deficit"` **além** do `window_train_deficit`
  (`expect_true(any(res$findings$what=="window_infer_deficit"))`). Um impl que ignore `history_days.infer` daria só o
  `window_train_deficit` → o braço o reprova. ⚠ **dessem usa infer=train=1825** (`run_job.sh:104`); essa regra de override
  é [DEFERRED M5]. Tudo via `model_spec`, nunca literal.
- **Escopo-área fail-high (exit 4) — dois braços (deck E scope-arg):** (a) deck com um `area_code` ("XX") ausente do
  `topology.yaml` → finding `topology` exit 4 (área nova sem declaração = fail-high, Decisão M1); (b) **`--area=XX`
  (ou `--model=yy`) de `scope` fora da topologia** → finding `topology` exit 4 (⚠ braço do arg de scope — Step 7 manda
  validar o scope contra a topologia, não só a coluna `area_code`; um impl que valide só o deck e ignore o arg fecharia
  verde sem ele). Braço positivo: `--area=SECO` (declarada) não gera finding de escopo.
- **Veredito acumula por origem (ADR-0005 fail-por-origem):** um deck com DOIS defeitos (coluna faltante numa área +
  carga NA noutra) → `res$findings` tem ≥2 linhas (não aborta no 1º) e `res$class` é a classe-dona pela ordem de
  severidade `contract > completude > topology > window` (aqui `contract`). Prova que não há abort-no-primeiro nem NA
  silencioso. ⚠ **Escopo honesto (igual ao M2.3):** o M2 afere o **comportamento** (class-dona correta + ≥2 findings), não
  a implementação por-lista-vs-`min()` — essas COINCIDEM no M2 (2<3<4<5) e a distinção só é falsificável quando o ADR-0020
  anexar uma classe futura de número MAIOR; a lista-desacoplada é **[DEFERRED]** ao ADR de precedência (§Decisões).

### M2.3 — comando `validate-deck`: veredito estruturado + `--format json` + exit-codes por classe

**Causa:** §0 — a referência não tem comando de validação standalone: a validação é side-effect do `_validate_contract`
na fronteira do CLI Python (`prevcarga:152-178`) + as ferramentas soltas rodadas à mão. O M2.3 expõe `validate-deck`
como **verbo dono** (já está na gramática docopt do stub `scripts/prevcarga:25`, hoje caindo no `stop()` de não-dono do
M1.4). ⚠ **Pressupõe M1 implementado:** o dispatcher docopt, o `prevcarga_runtime_enforce()` runtime-first, o handler
prefixo→exit-code (ADR-0020) e `topology.R`/`model_spec` são do M1 (VERIFY-READY) — dependência, não escopo do M2.

**Steps:**
1. `cmd_validate_deck(args)` em `scripts/prevcarga`: resolve `--dados`/`--as-of`/`--model`/`--area`/`--format`, chama
   `prevcarga_resolve_cuts` (M2.1) pra derivar a origem (default = último dia completo) + `prevcarga_validate_deck`
   (M2.2) com o `scope` de `--model`/`--area`. Dispatch **fino** — a lógica mora nos módulos.
2. **Veredito estruturado:** `--format text` imprime a tabela de findings (área/dataset/what/detail/class/level) + um
   resumo OK/FALHA; `--format json` emite `{ok, class, exit_code, as_of, data_hash, findings:[…]}` parseável
   (`jsonlite::fromJSON` não erra), com o **MESMO** exit-code das duas saídas (ADR-0020: exit ortogonal à saída).
3. **Exit-codes por classe (ADR-0020):** ⚠ **fonte única da precedência = `validation.R` (M2.2)** — é lá que a
   precedência `contract > completude > topology > window` colapsa os múltiplos findings numa **única** `class` (campo do
   veredito). O `cmd_validate_deck` **não re-aplica** a precedência: só **mapeia** essa `class` final ao inteiro pelo
   handler do M1.4 — `contract→2`, `completude→3`, `topology→4`, `window→5`, `ok→0`. ⚠ **Escopo honesto no M2:** como a
   ordem de severidade COINCIDE com a numérica no M2 (2<3<4<5), o M2 **não exige** uma implementação específica (lista
   nomeada vs `min()` do inteiro dão o mesmo resultado e ambas passam) — o requisito de **lista nomeada desacoplada do
   inteiro** (que só morde quando o ADR-0020 anexar uma classe futura de número MAIOR porém mais-severa) é **[DEFERRED]**
   ao ADR de precedência (§Decisões). O M2 exige só o **comportamento**: a classe-dona é a 1ª da ordem declarada e o
   veredito lista todas (testável).
4. **`data_hash` (pin de entrada) — primitivo NOVO que o M2.3 POSSUI:** ⚠ o `version_hash` do M1.3 recebe o
   `data_fingerprint` como **INPUT** (hasheia uma lista já montada — `M1-FUNDACAO.md:252-253`); **nada no M1 converte o
   store parquet Hive num digest**. O M2.3 é o **primeiro consumidor** e portanto **cria** `prevcarga_data_hash(data_dir)`
   **em `provenance.R`** (o dono declarado do content-addressing/fingerprint — `provenance.R:7-16`; NÃO em `validation.R`,
   senão M5/M6 importariam o módulo de validação só pra um primitivo de proveniência): `data_hash = sha256` sobre a lista
   **ordenada por caminho Hive** dos
   `sha256` de cada `area_code=<A>/year=<Y>/data.parquet` do store (precedente real = o `_sha256` por-arquivo +
   `_check_integrity` da referência, `scripts/prevcarga:279-282`/`:400-409`; **não** o `version_hash` de artefato, que
   mistura `model_id`/`cutoff`/`code_commit` — bytes que não são do deck). O `corte` intradiário do `manifest.json`
   **não** entra no digest (é metadado de run, não conteúdo do deck). Esse `data_hash` é exatamente o valor que
   `train`/`forecast` alimentam como `data_fingerprint` ao `version_hash`/run-manifest do M1.3 (travessia nomeada,
   fonte única). *(O consumo via `--expect-data-hash` em train/forecast é M5/M6 — [DEFERRED]. O contrato exato do digest
   é candidato a ADR — §Decisões.)*
5. **Gate-0 encadeável:** `validate-deck` é standalone no M2. A flag `--skip-validate` que o DESARMA vive na gramática de
   `forecast`/`train-models`/`backtest` — mas **esses verbos são M5/M6/M7**. [DEFERRED] o encadeamento real; o M2 entrega
   o comando + o contrato de exit-code + o `data_hash` que **tornam** o encadeamento possível (sem isso M5/M6 não teriam
   o que encadear).
6. **Re-liga `cmd_describe` (consumidor habilitado pelo M2):** o M1.5 entregou `describe` com o placeholder
   "corte-de-treino: derivado no M2" (`M1-FUNDACAO.md` §M1.5 Step 4) — o M2 entrega `dates.R`. ⚠ A gramática de `describe`
   (`scripts/prevcarga:24`) é `[--as-of] [--model] [--format]` — **sem `--dados`**; logo `describe` chama
   `prevcarga_train_origin(--as-of, --model)` (que só precisa de `as_of`+`model_spec`, **não** do store), mostrando os
   cortes **deriváveis sem o deck**: forecast-origin (= `--as-of`) e o corte-de-treino dos **daily** (`O−1−horizon`); p/ os
   `retrain_harness` (dessem/HW) imprime **"[derivado no M5]"** (não um número — escopo da opção b). O corte-**intradiário**
   é propriedade do snapshot (precisa do `manifest.json`), então `describe` o declara "do deck — veja `validate-deck
   --dados`", **não** inventa nem chama `resolve_cuts` (que exigiria `--dados`, ausente na gramática M1). Substitui o
   placeholder sem tocar a gramática M1. ⚠ **O M2 POSSUI aposentar o teste-irmão do M1.5:** o AC do M1.5
   (`M1-FUNDACAO.md:416`) asserta o OPOSTO — "`describe` **não** imprime corte-de-treino numérico (declara M2)". Como o M2
   habilita isso p/ os daily, este step **atualiza o testthat do M1.5** (substitui a asserção-negativa pela positiva:
   `describe --model random-forest` imprime o corte daily), senão o M2.3 torna o gate do M1.5 vermelho sem dono.
   ⚠ **Flags opcionais (a gramática M1 torna `--as-of`/`--model` opcionais):** **sem `--as-of`**, `describe` **não**
   tenta o default "último dia completo do deck" (não tem `--dados`/store) — mostra a **fórmula simbólica** (`O` /
   daily `O−1−horizon`; retrain_harness "[M5]") sem datas concretas; **sem `--model`**, imprime a **tabela por-modelo**
   (daily com corte, retrain_harness com "[M5]"). Nenhum caminho fica indefinido. *(O re-wire de `forecast`/`train`
   — os outros consumidores de `dates.R` — é M5/M6, [DEFERRED].)*

**AC:**
- **Exit-codes por classe exercidos no nível-CLI (ADR-0020):** `scripts/prevcarga validate-deck --dados=<fixture>` com
  (a) coluna Required faltante → exit **2**; (b) dia curto genuíno → exit **3**; (c) `area_code` fora da topologia →
  exit **4**; (d) deck curto p/ o membro do `--model` → exit **5**; (e) deck limpo → exit **0**. Cada falha com
  `[prevcarga/<classe>]` no stderr (text) e o mesmo código no `--format json`.
- **Precedência determinística (por severidade declarada):** um deck com defeito de schema (`contract`) **E** déficit
  de janela (`window`) → exit **2** (a classe-dona é a 1ª da lista `contract > completude > topology > window`); o
  veredito JSON lista AMBAS as classes em `findings`, e não aborta-no-primeiro. ⚠ **Ressalva honesta (limitação
  divulgada, não escondida):** no M2 a ordem declarada COINCIDE com a numérica (2<3<4<5), então **nenhum fixture do M2
  falsifica "lista-declarada vs menor-inteiro"** — uma impl por `min()` do inteiro passaria todos os braços. O AC do M2
  prova só o **comportamento observável** (exit 2 + ambas as classes listadas + não-aborta-no-primeiro), que é genuíno e
  não-vácuo; a distinção lista-vs-inteiro **só é falsificável quando o ADR-0020 anexar uma classe futura mais-severa de
  número MAIOR** — por isso a regra de precedência é **candidato a ADR** (§Decisões), a ser ratificada antes de M3+ a
  herdar. (Não finjo um grep que "prova o uso": grep mostra que a lista existe, não que ela decide o exit.)
- **`--format json` estável:** `validate-deck --format json` produz `{ok,class,exit_code,as_of,data_hash,findings}` que
  `jsonlite::fromJSON` parseia; `exit_code` no JSON **==** o exit-code real do processo (ortogonal à saída, ADR-0020).
- **runtime-first herdado (M1.4) — `validate-deck` é verbo GUARDADO:** sob `TZ=America/Sao_Paulo`, `scripts/prevcarga
  validate-deck --dados=<fixture>` sai exit **7** `[prevcarga/runtime]` com **stdout VAZIO** (nada da validação roda
  antes do guard — `validate-deck` **não** está na isenção `doctor`/`status`/`-h` do M1.4). Reusa o AC do M1.4, não
  duplica a lógica.
- **`data_hash` emitido (CONTROL com dente + model-agnóstico):** o veredito de um deck válido inclui `data_hash`
  (64-hex sha256, regex); dois `validate-deck` do MESMO deck → MESMO `data_hash` (`expect_identical`); mudar 1 byte do
  deck-fixture → `data_hash` **muda** (`expect_false(identical(h1, h2))`). ⚠ **Independe do modelo:** `validate-deck
  --model=lgbm` e `--model=random-forest` sobre o MESMO deck → MESMO `data_hash` (`expect_identical`) — prova que é
  digest do **deck**, não do artefato (ao contrário do `version_hash`, que mistura `model_id`). Se o hash não mudar sob
  perturbação de byte, o teste falha (Δ=0 não-vácuo).
- **`data_hash` EXCLUI o `corte` do manifest (dente da propriedade definidora):** mudar **só** o `corte` em
  `<data_dir>/manifest.json` (ex. `08:00:00` → `09:00:00`), mantendo os parquets do store byte-idênticos → `data_hash`
  **NÃO muda** (`expect_identical`). ⚠ Sem este braço, um impl que itere **todos** os arquivos sob `data_dir` (incl. o
  `manifest.json` — plausível, pois o precedente `_sha256`/`_check_integrity` itera arquivos) incluiria o `corte` no
  digest e passaria os outros braços (corte igual entre runs/modelos), quebrando o `--expect-data-hash` de M5/M6 em
  silêncio. O digest é só dos **parquets de conteúdo**, não do metadado de run.
- **`data_hash` é ordenado por caminho Hive (dente anti-ordem-de-enumeração):** o teste alimenta o digest com a lista de
  arquivos numa **ordem embaralhada** (`sample()` dos caminhos, ou um enumerador não-ordenado como `fs::dir_ls(sort=FALSE)`/
  fragments do arrow) e confirma que o `data_hash` é **idêntico** ao do caminho ordenado (`expect_identical`). ⚠ **Não
  basta** "ordem de criação diferente": o R `list.files()` já **ordena por nome em C locale** (pinado pelo `runtime.R`,
  M1), então um impl que concatene na ordem de `list.files()` daria a MESMA ordem nos dois fixtures e passaria trivialmente
  — por isso o braço embaralha EXPLICITAMENTE a entrada do digest. Prova que o impl **re-ordena por caminho Hive** antes
  de hashear (estabilidade cross-máquina que o `--expect-data-hash` de M5/M6 exige), não confia na ordem de enumeração.
- **Default de origem (sem `--as-of`):** `validate-deck --dados=<fixture>` **sem** `--as-of` usa o último dia completo
  (via `resolve_cuts`/`prevcarga_day_complete`), **não** aborta por falta de flag; com `--as-of` explícito, usa-o.
- **`describe` mostra os cortes deriváveis sem o deck (placeholder do M1.5 resolvido):** `scripts/prevcarga describe
  --as-of 2024-06-11 --model lgbm` imprime forecast-origin `2024-06-11` e corte-de-treino `2024-06-09` (daily h1) via
  `prevcarga_train_origin` (sem `--dados`, que a gramática M1 não tem), e **não** contém mais a string placeholder
  "derivado no M2" (`grep` prova a ausência); `--model dessem-linear` (retrain_harness) imprime **"[derivado no M5]"**, não
  um número; o corte-**intradiário** aparece como "do deck (via `validate-deck --dados`)", não um valor inventado. Prova
  que o consumidor habilitado foi re-ligado, não deixado obsoleto.
- **`describe` com flags opcionais ausentes (nenhum caminho indefinido):** `describe` **sem `--as-of`** não aborta e
  **não** inventa origem — imprime a fórmula simbólica (`O−1−horizon` p/ daily; "[M5]" p/ retrain_harness) sem datas
  (`grep` prova que não há data concreta nem `stop`); `describe --as-of X` **sem `--model`** imprime a tabela por-modelo
  (daily com corte; retrain_harness com "[M5]"). Fecha o caso default que a gramática M1 (opcionais) deixaria em aberto.
- **Teste-irmão do M1.5 aposentado pelo M2 (ownership do gate contraditado):** o AC do M1.5 (`M1-FUNDACAO.md:416`,
  "`describe` NÃO imprime corte numérico, declara M2") é **atualizado** por este ticket para a asserção-positiva (imprime
  o corte daily; retrain_harness = "[M5]"); a suíte testthat (M1.5 + M2) roda verde — não há dois testes contraditórios vivos. Prova que o
  M2 possui a transição, não deixa o gate do M1.5 vermelho.
- **gate-0 [DEFERRED] declarado, não órfão:** o doc declara explicitamente que o encadeamento `forecast/train
  --skip-validate` é M5/M6/M7 (os verbos não existem no M2); o M2 entrega só `validate-deck` standalone + o contrato de
  exit-code + o `data_hash`. (Evita lacuna [UNOWNED] falsa no verify.)

---

## Decisões (ratificadas nesta frente)

- **⚠ Premissa do fill-0 corrigida (o repositório vence o PLANO/stub).** A referência **NÃO** preenche carga com 0 —
  mantém NA + falha alto (`validate_input.R`) ou trunca honesto (`lgbm/.../data_adapter.R:674-677` `CargaGlobal=NA_real_`);
  o único `nafill` é **LOCF em CLIMA** (`rf/.../data_adapter.R:291-292`), nunca 0. O `validate-deck` do M2 **unifica e
  antecipa** os fail-high dispersos num módulo standalone e **impede a regressão** pra fill-0 silencioso no R-only — não
  conserta um bug inexistente. Isso diverge do PLANO §M2 e do stub `validation.R:19` ("RF/LGBM comeriam calado com fill-0");
  o gate falsificável permanece válido (deck com NA/0 → REPROVA), só a narrativa muda. (Write-through no ESTADO-ATUAL.)
- **`--origem` (required, sem default) → `--as-of` (default = último dia completo):** o único flag de data humano. O
  default realiza em `dates.R` o auto-detect que hoje só vive no adapter LGBM (`data_adapter.R:655-664`). Os outros dois
  cortes são **derivado** (treino, por-modelo via protocolo) / **do-dado** (intradiário), NUNCA flags (ADR-0006/0007).
- **Corte intradiário sempre do `manifest.json` 'corte' (ADR-0007):** porta `_cut_from_data` (`prevcarga:122-134`); a
  dupla-fonte flag+auto-detect da referência é verruga eliminada no R-only. `--corte` só no backtest (M7).
- **⚠ Premissa DST-aware corrigida → completude flat-48 (o repositório vence; gêmeo do fill-0).** A 1ª versão do M2.2
  dizia "completude DST-aware (46/48/50), o scan fixo-48 dá falso-positivo". **Falso:** o store é **flat-48** — o DST foi
  absorvido na malha (ADR-0003 rejeita o armazenamento tz-aware; ADR-0002 "a dimensão 48 fixa a matriz"); o pivot força
  H01..H48 (`holt-winters/.../data_adapter.R:178`, NA nos ausentes `:184-185`, `mean` na hora de 25h `:175`), RF diz
  "Pre-2019 DST out of scope" (`:135`), o flag `HV` é binário 0/1 (`:296`), `dst_periods` **não é lido por ninguém**, e
  nenhum contador conta 46/50. O 48-fixo do `scan_completude_24slots.R:88` está **CERTO**. Exigir "dia de 46 → PASSA"
  abriria um **falso-NEGATIVO** (aceitaria dia truncado como "DST-mola"). O M2.2 volta a **48-fixo** (dia ≠ 48 reprova).
  (Confirmado por varredura, 2026-10-11; mesma disciplina que derrubou o fill-0 no §0.)
- **⚠ Corte-de-treino difere por PROTOCOLO (fato do repo) — mas o M2.1 escopa SÓ `daily` (opção b).** `jl_train_origin`
  (`origem−1−horizonte`) é chamado **só** no caminho RF/LGBM (`run_job.sh:215/:302/:350`, todos origem-agnósticos);
  dessem-*/HW (`retrain_harness`) **nunca** o chamam — re-fitam na própria origem O com série em **O−1** (dessem
  `data_adapter.R:455`; HW cap O−1 `run_job.sh:202-204`) + manhã ≤ corte via `--available-until` (ADR-0011; CLI-OBJETIVO
  §4.1 "HW/dessem **--retrain**"; `honesty_gate.R:391` ramifica por protocolo). Logo o `origem−1−horizonte` **só** vale
  pro `daily`.
- **⚠⚠ MUDEI de (a) para (b) — por quê (a 2ª divergência "repo vence" + o conflito com o ADR-0006 Aceito).** A 1ª redação
  escolheu a **opção (a)**: ramificar `prevcarga_train_origin` por protocolo já no M2.1 (daily `O−1−h`; retrain_harness
  `O−1`). O `verify` convergiu mostrando que (a) tem um **defeito de registro inescapável**: ela **bakeia** o corte do
  retrain_harness (`O−1`, por-protocolo) num **gate executável** + no **cutoff de proveniência**, contradizendo o
  **ADR-0006 Aceito** ("origem−1−horizonte **idêntico em todos os protocolos**") e seu espelho durável `ESTADO-ATUAL:94` +
  `README`. ⚠ O ADR-0006 é **imutável**: só um **ADR superseder** (regra do `docs/adr/README.md`) o reconcilia — e criar
  esse ADR é **ato do Gabriel**, que **nenhum ticket do M2 pode possuir**. Sob (a), o M2.1 fecharia VERDE afirmando um
  corte que o único doc-verdade durável segue negando (lacuna `[UNOWNED]`). **Portanto adotei a opção (b):** o M2.1 cobre
  **só `daily`** (RF/LGBM) — que **É** o `origem−1−horizonte` do ADR-0006, logo **conforme**; o `retrain_harness`
  (dessem/HW) faz `stop` **[DEFERRED M5]** (a semântica de corte deles — série O−1 + `available_until` — é **training**,
  nasce no M5, onde o ADR-0011 diz "honrado"). Consequências: **`ESTADO-ATUAL:94` + `README` dates.R ficam INTACTOS e
  conformes** (nada a reescrever, zero doc-verdade estale, zero ADR novo no M2); o gate falsificável #1 do M2 permanece
  (RF C−10 ≠ LGBM C−2, daily). ⚠ **Reversível pelo Gabriel:** se preferir a opção (a), ele cria o ADR-00XX que supersede o
  0006 (corte por-protocolo: daily `O−1−h`; retrain_harness `O−1`; honesty_gate ramifica) **antes do implement do M2.1** e
  reescreve `:94`+README — mas isso é escolha dele, não minha, porque toca um ADR Aceito. A reconciliação do ADR-0006
  acontece **naturalmente no M5**, quando o training de dessem/HW nasce. *(O `honesty_gate.R:391` da referência já ramifica
  por protocolo — o "idêntico em todos os protocolos" do ADR-0006 é factualmente impreciso; o superseder só formaliza isso.)*
- **Taxonomia de exit-code (ADR-0020), M2 emite 2/3/4/5:** `contract→2`, `completude→3`, `topology`/escopo-área`→4`,
  `window→5`. Herdado do handler do M1.4; contrato transversal M2–M8, imutável.
- **Leak de clima + sanidade física hard → classe `contract` (exit 2), como schema.** O leak (`issue_time > intraday_cut`)
  e a sanidade **hard** (carga>0/dedup/faixa-clima; outlier E flatline são `warning`, não entram; monotonicidade removida)
  caem na MESMA classe `contract`/exit 2 de um erro trivial de schema. ⚠ Consequência: um
  consumidor de CI **não distingue** "deck vazou clima do futuro" (risco #1) de "faltou uma coluna" — ambos exit 2.
  **Default de trabalho = manter o fold em `contract`** (leitura ampla de "contrato do deck"; ADR-0020 é append-only e a
  parentética classe→prefixo é ilustrativa; cresce zero). **Decisão do dono:** se quiser o leak CI-distinguível, anexar
  uma classe nova no FIM do ADR-0020 (ex. `[prevcarga/honesty]→8`) — isso cresce o milestone (emenda ADR + linha de
  exit-code transversal M2–M8), então não absorvo sem decisão. (Registrado, não bloqueia.)
- **⚠ Precedência multi-classe = por severidade DECLARADA, NÃO por inteiro (candidato a ADR).** Quando o veredito tem
  várias classes, a classe-dona do exit sai de uma **lista explícita** `contract > completude > topology > window`,
  **desacoplada** do valor numérico. Ancorar no "menor código" brigaria com a regra append-only do ADR-0020 ("classe
  nova = código no fim, nunca renumerar"): uma classe futura mais fundamental ganharia número MAIOR e seria mal-rankeada.
  A precedência é um **contrato transversal novo** (M2–M8 herdam) que **nenhum ADR cobre** — o dono decide registrá-la
  (emendar ADR-0020 ou novo). O M2 usa a lista declarada como default de trabalho.
- **⚠ `data_hash` do deck = primitivo do M2.3, com contrato candidato a ADR.** O `version_hash` do M1.3 recebe o
  `data_fingerprint` como INPUT; nada no M1 digere o store. O M2.3 cria `prevcarga_data_hash(data_dir)` (sha256 sobre os
  sha256 por-partição ordenados por caminho Hive; o `corte` do manifest **fora** do digest; precedente `_sha256`/
  `_check_integrity` da referência). ⚠ **O que entra no digest** (quais datasets/colunas/partições/ordem) é **candidato
  a ADR** — M5/M6 herdam via `--expect-data-hash`; o dono decide emendar o ADR-0011 (ou novo) ANTES de M5/M6. O M2 usa a
  receita acima como default de trabalho, model-agnóstica (ao contrário do `version_hash`, que mistura `model_id`).
- **Reconciliação de frame UTC−3 do filtro de clima — CONFORME ao ADR-0003, não candidato a ADR no caminho crítico.** O
  filtro compara `issue_time` (único campo genuinamente UTC) contra o `intraday_cut` (instante naive-BRT) no **frame UTC**
  via `to_utc(t_BRT)=t_BRT+3h`. ⚠ Isso **não** é a "aritmética de fuso no pipeline" que o ADR-0003 proíbe (que trata de
  **converter a GRADE/datetime armazenado**): o ADR-0003:44-45 **explicitamente** prevê que "a emissão é o único instante
  genuinamente UTC, usado só pra **ordenação/freshness do corte**" — exatamente o que este `+3h` faz (comparar dois
  instantes p/ freshness, sem tocar a grade naive-BRT). Logo é **conforme**, não contradiz ADR Aceito, e **não** precisa
  de ADR novo no caminho crítico (ao contrário do corte-por-protocolo vs ADR-0006). ⚠ **Divergência de precedente (dita, não
  escondida):** a referência compara contra um `issue_cutoff` **dia-level** (meia-noite-UTC-do-dia-O, `holt-winters/.../data_adapter.R:328`),
  **não** o corte intradiário-instante convertido; o M2 usa o `intraday_cut` **instante** (mais fino/honesto — pega a
  emissão do mesmo dia após o corte). Esse refinamento intradiário é **novo** vs o precedente dia-level, mas segue o
  espírito do ADR-0003 (freshness por `issue_time` UTC). A **regra de dia-de-origem + horizonte UTC-safe** do M5 (que
  herda esse frame) é que será formalizada lá ([[clima-previsto-emissoes]]).
- **⚠ Esquema do veredito-máquina = candidato a ADR.** O envelope `{ok, class, exit_code, as_of, data_hash, findings}` +
  o schema da linha de finding `{area, dataset, what, detail, class, level}` é um contrato de **saída-máquina transversal
  novo** (M3+ copiam pro `score`/`check-coherence`) que nenhum ADR congela — o mais próximo, ADR-0020, cobre só o
  exit-code+`--format`, não o payload. **Candidato a ADR** (emendar ADR-0020 ou novo): o dono congela o envelope + o
  schema de findings antes de M3+ copiar. O M2 usa o esquema acima como default de trabalho.
- **⚠ Outlier E flatline são WARNING, não hard-fail (seguem o precedente report-only; heurísticas sem precedente de
  reprovação).** O `analyze_data_quality.py` trata outlier (IQR/z-score) como **report**, não reprovação; flatline/
  stuck-sensor não existe na referência. Promover qualquer um a gate DURO exit-2 (a) não tem precedente e (b) um IQR/
  z-score GLOBAL dá falso-positivo em picos sazonais legítimos, e o flatline bate numa área pequena quantizada legítima —
  reprovar por isso mataria deck real no gate-0 que deveria "proteger compute". Então o M2 emite **ambos como `level="warning"`**
  (não entram no exit-code; aparecem no veredito p/ inspeção). `carga>0`/dedup/faixa-clima seguem hard-fail `contract`;
  **monotonicidade foi removida** (mal-definida sobre store Hive não-ordenado; subsumida por completude+dedup). (Correção
  de design vs a 1ª versão, que punha outlier/flatline em `contract`/exit-2 e incluía monotonicidade.) O limiar ≥6 slots do
  flatline e a escolha warning-vs-hard são pendência de ratificação do dono (heurística sem precedente).
- **`resolve_cuts` recebe `data_dir` (não o manifest parseado):** o default-de-origem "último dia completo" varre o
  **store**, não o manifest — então a função precisa do `data_dir` (que também lhe dá o `manifest.json` p/ o corte
  intradiário). Alinha a assinatura às irmãs (`train_origin(…, root)`, `validate_deck(data_dir, …)`) e serve de alavanca
  de fixture. (Stub `dates.R:20` atualizado p/ `(as_of, data_dir, model=NULL)`.)
- **`cmd_describe` re-ligado no M2 (não adiado):** o placeholder "derivado no M2" do M1.5 é resolvido aqui (M2.3 Step 6),
  mostrando origem + corte-de-treino **daily** via `prevcarga_train_origin` (retrain_harness = "[M5]"; ⚠ **não**
  `resolve_cuts` — a gramática M1 de `describe` não tem `--dados`; o corte-intradiário é anotado "do deck, via
  `validate-deck`"). Edição pequena que mantém `describe` honesto assim que `dates.R` pousa (`forecast`/`train` em M5/M6).
- **Honestidade do clima ancora no INSTANTE de corte (não no dia):** o filtro é `issue_time ≤ intraday_cut` (ADR-0007:13
  "região proibida por ISSUE_TIME, corte = INSTANTE"; ADR-0003 resolução "emissão mais fresca com `issue_time ≤ instante
  de corte`") — **não** `issue_time ≤ as_of` (nível-dia, mais frouxo, deixaria passar emissão do mesmo dia após o corte).
  O `validate-deck` reporta ANTES do compute o filtro que hoje vive nos adapters (`holt-winters/.../data_adapter.R:362`).
  `issue_time` é o instante cheio (ADR-0003), não achatado ([[clima-previsto-emissoes]]).
- **dates.R alimenta provenance.R (travessia de módulo — carregada por DoD):** o `train_cut` que `prevcarga_train_origin`
  produz p/ um membro **daily** é o `cutoff` que entra no fingerprint de `prevcarga_version_hash` (M1.3) — o corte nunca
  vira um `cutoff` run-level único (o run-manifest carrega `as_of`, não um corte único — ADR-0006, M1.3). *(O cutoff do
  retrain_harness é [DEFERRED M5].)* ⚠ Carregada por um item de DoD (não fica só em prosa).
- **M2 pressupõe M1 implementado:** fachada docopt + runtime-guard + handler de exit-code + `topology.R`/`model_spec` são
  do M1 (VERIFY-READY), **dependência** não-escopo do M2 (o PLANO ordena M1 → M2).
- **`validate-deck` standalone no M2; encadeamento gate-0 em M5/M6/M7:** os verbos `forecast`/`train-models`/`backtest`
  que o encadeiam com `--skip-validate` não existem no M2 — o M2 entrega o comando + exit-code + `data_hash`; o
  encadeamento é [DEFERRED] aos milestones que criam os consumidores.

---

## Checkpoint & gates

M2 é infra de datas/validação — **NENHUM modelo/combina existe** no clean-room ainda (portam no M5). Logo é seu
**próprio checkpoint** (fecha batch de 1), **sem A/B de deploy** (o A/B numérico é o M8; aqui não há `y_pred` novo pra
comparar). A entrada já foi adicionada em `.claude/workflows/implement-milestone.js`
(`CHECKPOINTS['M2']`/`'M2-DATAS'` → `_M2_CP`):

- `fullGates`: a **suíte testthat nova do M2** toda verde — **dates** (3 cortes da origem resolvida; corte-de-treino
  **daily** — gate #1a RF(C−10)≠LGBM(C−2) diverge 8 dias + FORBIDDEN idempotência/independência Δ=0; retrain_harness faz
  `stop` [DEFERRED M5]; corte-intradiário do manifest, nunca flag) +
  **validation** (schema/tipo/tz/domínio-de-feriado FORBIDDEN/CONTROL; completude **flat-48** — dia≠48 reprova, dia=48
  passa; sanidade física — carga>0/dedup/clima−10..50 (hard) + outlier/flatline (warning), sem monotonicidade; clima
  honesto `issue_time ≤ intraday_cut` com dente; janela mínima — treino (HW 730/RF 365/LGBM 730/dessem 1825) **e**
  inferência (45 p/ RF/LGBM/HW; 1825 p/ dessem, `run_job.sh:104`)) + **dispatch** `validate-deck` (exit 2/3/4/5 por classe
  + precedência por severidade declarada + `--format json` + `data_hash` com dente/model-agnóstico + runtime-first/stdout-vazio + `describe` origem+corte por
  protocolo) — mais parse/lint R dos arquivos tocados. ⚠ As fixtures de deck são **parquet Hive minúsculos materializados
  pelo teste em tmp** — NÃO dependem do `data/` real (NUNCA apagar/regerar o `data/` real pra "limpar").
- `abReference`: **nenhum** no M2 (`deployByDefault: false`). O A/B entra no M8.
- `passCriteria`: testthat do M2 + parse/lint verdes; **NÃO** exigir `run_all_gates.R`/`jobs/tests/`/combina/A-B
  (nada disso existe/se-aplica no M2 — os modelos portam no M5; o A/B é o M8).
- Write-through: `docs/ESTADO-ATUAL.md` "Onde estamos" reflete M2 vivo (3 cortes + validate-deck) **+ as correções de
  premissa** (fill-0 inexistente; completude flat-48 não-DST; corte-de-treino `daily` = `O−1−horizon`, retrain_harness
  adiado ao M5 — `ESTADO-ATUAL:94`/ADR-0006 ficam conformes e intactos).

---

## Definition of Done

- [ ] **M2.1** `dates.R` deriva os 3 cortes da **origem resolvida** (um único `--as-of`, ou o último dia completo do
  store): corte-de-treino **do protocolo `daily`** (RF/LGBM) = `O−1−horizon` (gate #1a RF(C−10)≠LGBM(C−2) diverge 8 dias +
  FORBIDDEN idempotência/independência Δ=0); `retrain_harness` (dessem/HW) → `stop` **[DEFERRED M5]** (opção b — evita
  contradizer o ADR-0006 Aceito; §Decisões); corte-intradiário do `manifest.json` 'corte' (nunca flag, `stop` se ausente);
  forecast-origin default = último dia completo. FORBIDDEN + CONTROL (horizonte morde no daily) verde.
- [ ] **M2.2** `validation.R` dá veredito estruturado model-agnóstico: schema+TIPO+**tz**+faixa-de-feriado (prevcarga_code≥1),
  completude **flat-48** (dia≠48 reprova; DST absorvido, ADR-0002/0003 — **não** 46/50), sanidade física hard
  (carga>0/dedup/clima−10..50) **+ outlier/flatline como `warning`** (não reprovam, sem monotonicidade); **sem** faixa-por-subsistema (sem fonte de config, ADR-0001),
  clima honesto (`issue_time ≤ intraday_cut`, instante não dia), janela mínima por membro (treino E inferência, chaves
  distintas) — **acumula por origem** (ADR-0005), **cada sub-checagem** com braço CONTROL com dente. Reprova deck com NA/0.
- [ ] **M2.3** `validate-deck` compõe os dois: veredito estruturado, `--format json` (exit ortogonal), exit-codes por
  classe **2/3/4/5** com **precedência por severidade declarada** (contract>completude>topology>window), `data_hash`
  **próprio do M2.3** (recipe store→digest ordenada por caminho Hive; CONTROL com dente + model-agnóstico + exclui o
  `corte` + ordem-estável), verbo GUARDADO (runtime-first, stdout-vazio sob env sujo), `describe` re-ligado (origem +
  corte `daily` via `train_origin`, retrain_harness = "[M5]", sem `--dados`) **com aposentadoria do AC-negativo do M1.5**
  (`M1-FUNDACAO.md:416`); encadeamento gate-0 [DEFERRED] a M5/M6/M7 (declarado, não órfão).
- [ ] **Exit-codes (ADR-0020):** M2 emite `contract→2`/`completude→3`/`topology→4`/`window→5`; **precedência por
  severidade declarada** (não por inteiro — candidato a ADR, §Decisões); mapeados pelo handler do M1.4; um teste por
  classe asserta `exit == <code>`.
- [ ] **Travessia dates→provenance (possuída pelo AC do M2.1):** o teste confirma que o `train_cut` de um membro **daily**
  (`prevcarga_train_origin`) é o valor passável como o campo **`cutoff`** do fingerprint de `prevcarga_version_hash` (M1.3)
  (⚠ `cutoff` e `data_fingerprint` são componentes **distintos** — `provenance.R:8`; o `train_cut` é o **`cutoff`**, o
  `data_hash` do M2.3 é o **`data_fingerprint`** — não confundir). O corte nunca vira um `cutoff` run-level único
  (ADR-0006). *(O cutoff de proveniência do retrain_harness — boundary O−1, distinto do `--origin=O` de invocação do
  treino, `run_job.sh:209`/`data_adapter.R:455` — é [DEFERRED M5], onde o training de dessem/HW nasce.)*
- [ ] **Gates falsificáveis (cross-cutting):** todo gate novo do M2 que afirma reprovação/aceite/Δ=0 tem braço CONTROL
  com dente que DEVE divergir (corte-de-treino **daily** RF≠LGBM, completude **flat-48**, sub-checagens hard de sanidade,
  clima no instante de corte, `data_hash`). Suíte testthat do M2 verde.
- [ ] **Fixtures:** decks parquet Hive minúsculos materializados em tmp pelos testes (não o `data/` real); cobrem dia
  **≠48 slots** (faltante) e dia completo de 48, deck com NA/0, clima emitido **após o `intraday_cut`** do mesmo dia,
  deck curto por-janela (treino e inferência), `area_code` fora da topologia. **Sem** fixtures de 46/50 (store é flat-48).
- [ ] **Regressão (cross-cutting):** não há gate de modelo a regredir no M2 — os `models/<m>/tests/` portam no M5
  (M2 não cria nem toca código de modelo; só **consome** os configs que o M1.2 cria — ver §Dependência de M1).
- [ ] **Write-through:** `docs/ESTADO-ATUAL.md` reflete M2 vivo (3 cortes + validate-deck) + as correções de premissa que
  **alinham com ADR Aceito** (nenhuma toca o ADR-0006, graças à opção (b)): `ESTADO-ATUAL.md:96` ("Completude DESSEM:
  ≥24 slots/dia" → **48 slots/dia**, alinha ADR-0002); o stub `validation.R:10` (`≥24`+"DST-aware" → flat-48); a **linha de
  `validation.R`** do `scripts/lib/README.md` ("completude DST-aware" → flat-48); a narrativa fill-0-inexistente.
  ⚠ **NÃO tocar** `ESTADO-ATUAL.md:94` nem a **linha de `dates.R`** do README (ambos espelho do ADR-0006 "origem−1−horizonte,
  idêntico em todos os protocolos") — sob a opção (b) o M2.1 cobre só `daily` (= `origem−1−horizonte`), então `:94` e o
  README seguem **conformes** e INTACTOS; a reconciliação por-protocolo é do M5 + ADR superseder (Gabriel). Zero doc-verdade estale.
  Registra os **4 candidatos a ADR** (corte-por-protocolo→superseder-0006-no-M5, precedência, data_hash-recipe,
  schema-veredito) como pendências — **decisão do Gabriel**, não write-through. (A reconciliação de frame UTC−3 do clima
  é conforme ao ADR-0003, não candidato — §Decisões.)
- [x] **Checkpoint wiring (feito pré-implement, SINCRONIZADO com as premissas corrigidas):** `CHECKPOINTS['M2']`/
  `'M2-DATAS'` → `_M2_CP` no `implement-milestone.js` (`deployByDefault:false`, `passCriteria` testthat-only — sem
  `run_all_gates.R`/jobs-tests/combina/A-B). ⚠ O `batch`+`fullGates` do `_M2_CP` refletem o corpo ATUAL (corte-de-treino
  **daily** `O−1−horizon`; retrain_harness **[DEFERRED M5]**; completude **flat-48**; outlier/flatline **warning**;
  CONTROL "deck NA/0 reprova" — não "fill-0"; clima `≤ intraday_cut` cross-frame) — não as premissas pré-correção.
- [ ] **Sem dep de S3** (herdado do M1.4, ADR-0016): `grep -rE "paws|aws|s3://" scripts/` não ganha hit novo no M2
  (o store é lido por `arrow::open_dataset` de filesystem local; produto local-only).

---

## Relacionados

- [PLANO.md](./PLANO.md) — M2 no programa M1…M8 (depende de M1; destrava M3/M5).
- [CLI-MODULAR-OBJETIVO.md](./CLI-MODULAR-OBJETIVO.md) — §4.1 (as 3 datas), §4.2 (exit-codes + `--format`), §3 (verbo `validate-deck`), §7 P2 (DST/sanidade/clima/janela).
- [docs/reference/DECK-CANONICO.md](../reference/DECK-CANONICO.md) — contrato do deck (6 datasets, colunas R/O, tipos, Hive, matriz quem-lê-o-quê) que o `validate-deck` impõe.
- `../adr/0002-grade-semihoraria-schema-long.md` (grade semi-horária), `0003-tempo-naive-brt-runtime-env.md` (tempo/emissão do clima), `0004-store-parquet-hive.md`/`0005-contrato-entrada-fail-por-origem.md` (store + fail-high/fail-por-origem), **`0006-backtest-honesto-corte-treino.md`** (corte honesto `origem−1−horizonte` no `daily`; retrain na própria origem — ADR-0011), **`0007-contrato-corte-intradiario.md`** (corte do dado/`manifest.json`), **`0020-taxonomia-exit-code-cli.md`** (exit-codes 2/3/4/5), `0021-run-yaml-contrato-modelo.md` (`horizon_days`/`history_days` no `run.yaml`).
- Memória: [[clima-previsto-emissoes]] (emissão `issue_time` cheio + horizonte UTC-safe), [[combina-vazamento-horizonte]] (o leak é M4, não M2), [[adrs-prevcarga]], [[cli-modular-objetivo]], [[padrao-milestone-specforge]].

## Fonte do design
PLANO.md §M2 (re-sequenciamento `wf_4d2ca349-18b`) + varredura de ground-truth da referência
`/opt/ons-source/prevcargadessem3` (`file:line` em §0, 2026-10-11, via Explore) + os stubs `scripts/lib/dates.R`
(`:15/:20`) e `scripts/lib/validation.R` (`:21`) do clean-room + o `DECK-CANONICO.md`.
