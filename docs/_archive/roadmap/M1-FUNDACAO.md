# M1 — Fundação: fachada + runtime-guard + topologia + proveniência

> ✅ **VERIFY-READY (M1):** `verify-milestone` **ready:true · 0 blockers · 6/6 reviewers unânime** (2026-10-11;
> convergiu em 2 rounds consecutivos limpos — log em `verify-log/M1-FUNDACAO.jsonl`). **Pronto pra `implement`**
> (`CHECKPOINTS['M1']` wirado). Material não-bloqueante residual anotado nas §; nenhum blocker em aberto.
> **Tier:** `docs/roadmap/` (plano ativo; some ao concluir). **Programa:** [PLANO.md](./PLANO.md) (M1…M8). **Checkpoint:** este é o 1º milestone; fecha batch próprio (ver §Checkpoint).

> **Intent (Gabriel, 2026-10-10/11).** O determinismo some no instante em que a 1ª linha numérica roda
> **in-process** (sem o isolamento de `subprocess`+ENV que a referência tinha) → a fundação tem de **asserir
> ANTES** de qualquer número. A fachada Rscript+docopt escrita **uma vez** mata o big-bang e a
> re-modularização das duas trilhas antigas. Topologia e proveniência entram como **contratos** para combina
> (M4) e training (M5) não nascerem com local descartável (saveRDS ad-hoc, `version=timestamp`) que uma fase
> posterior re-centraliza — matando 4 laços port-depois-refatora. **Nada numérico migra neste milestone**: o
> M1 é só a casca + os 3 contratos + 5 comandos de introspecção. O sistema de produção segue em
> `/opt/ons-source/prevcargadessem3` (oráculo do A/B) até o corte no M8.

---

## 0. Evidência (o que a referência faz hoje)

Levantado por varredura de `/opt/ons-source/prevcargadessem3` (congelado). ⚠ **Todos os `path:line` do §0 são do
oráculo congelado `/opt/ons-source/prevcargadessem3/` (absolutos nesse repo) — NÃO existem no clean-room**; são o
que o M1 substitui/subsume, não arquivos-alvo. Os arquivos-alvo (clean-room) estão em §Superfícies.

**Fachada é Python/argparse, 3 comandos.** `scripts/prevcarga:21` (`import argparse`); dispatcher em
`:479-510` (`add_subparsers(dest="cmd", required=True)` → `args.func(args)` em `:510`). Só existem
`treinar` (`:483`→`cmd_treinar:250`), `prever` (`:491`→`cmd_prever:261`), `verificar` (`:503`→`cmd_verificar:433`).
O shell-out roda runners por `subprocess.run(["bash", script], env=e, …)` (`_run:200`, chamada `:205`;
runners `RUN_JOB`/`RUN_REDUCE` `:39-40`; ENV montado por `_mode_env:181`). O export inline (`_deliver:318`)
e o modo-por-prefixo-`s3://` (`_is_s3:56`, consumido em `:126/:142/:324/:390`) **não** são portados no M1
(são M6 e ADR-0016 respectivamente).

⚠ **Não há `status`/`doctor`/`models`/`areas`/`describe` na referência** (confirmado por grep). Logo os 5
comandos de introspecção do M1 são **novos** — **não existe saída de referência a espelhar** (isto corrige o
AC genérico "comando legado espelha a referência" do PLANO: vale só para os verbos numéricos — na referência
`treinar`/`prever`, no clean-room `train-models`/`forecast` (nomes vivos da gramática; ver Decisões) — que no M1
ainda não existem e fazem `stop()` com o milestone-dono). O que os novos comandos podem **reusar
como dado** (não como oráculo de saída): `_resolve_areas:104`/`_plan_areas:208` (áreas), `_validate_contract:152`
(doctor/status), `_load_manifest:386`/`_check_integrity:400` (describe/status).

**Topologia é quádruplo-hardcoded** (o alvo do M1.2):
- (a) `SUBSYS`+`LEAVES` na CLI Python — `scripts/prevcarga:47-53` (24 folhas, 4 subsistemas; ordem
  `[SECO,S,N,NE,SIN]` re-hardcoded em `_plan_areas:217`).
- (b) `MEMBERS` (8 membros) — `scripts/combina_baseline.py:27-36`, re-importado pela CLI em
  `scripts/prevcarga:42-44` (`import combina_baseline as _cb; MEMBERS = _cb.MEMBERS`).
- (c) árvore SIN (folha→subsistema + perda) — `scripts/reconcilia_baseline.R:53-57`; identidade de perda
  `L_true := SS_true − BU_true` em `:140`; composição SIN `names(SIN)` em `:228/:249-252`.
- (d) horizonte/janela por modelo — chave **`horizon_days`** + `history_days:{train,infer}` em `models/*/job.json`:
  HW `horizon_days:8` (`:5`) `{train:730,infer:45}` (`:10`) · RF `9`/`{365,45}` · LGBM `1`/`{730,45}` ·
  dessem-linear `9`/`{1825,45}` · dessem-regdin `9`/`{1825,45}`.

> **Realização no M1 = config declarativo, não hardcode no R.** O ADR-0001 já manda "fonte única, **não
> embutida no código**" (rejeita a alternativa "topologia embutida no código"). O precedente do `prevcarga-R`
> declarava **áreas como lista YAML** (`/opt/source/prevcarga-R/docs/legacy/randon-forest/config/config.yaml`
> → `data.areas:`, no 3º repo de referência — não sob o clean-room nem o `prevcargadessem3`; ali só
> `[SP]` — era protótipo de 1 área; a árvore SECO/S/N/NE **nunca** existiu como config lá, só hardcoded na
> referência). O M1.2 realiza a fonte única como **`config/topology.yaml`** (seed = os valores da referência
> acima), que `topology.R` só **carrega e valida** — áreas/subsistemas novos entram **editando o YAML**, sem
> tocar código. "Podem surgir áreas novas ou não" (Gabriel, 2026-10-11): a topologia **não fixa contagem**.

**Determinismo por thread-pin NÃO vive no runtime in-process** (o alvo do M1.1): TZ/LC são setados no
wrapper de container `jobs/entrypoint.sh:23,27,29-30`, no launcher gerado pelo install `scripts/install.sh:129`
e em `jobs/run_reduce.sh:177,188,195`. Mas o quarteto `OMP_/OPENBLAS_/MKL_NUM_THREADS` + `VECLIB_MAXIMUM_THREADS=1`
só aparece em (1) o launcher gerado `scripts/install.sh:130` e (2) harness de teste/gate
(`jobs/tests/test_prevcarga_honesty.sh:43`, `test_marmita_e2e.sh:53`, `test_split_parity.sh:261/273/290`,
`scripts/tools/honesty_gate.R:349-350`, …). `jobs/entrypoint.sh`/`run_job.sh`/`job_lib.sh` **não setam
nenhum** `*_NUM_THREADS` (grep confirmou). → in-process o Matrix/data.table/arrow pode multithread e quebrar
Δ=0 em silêncio. **M1 move o pin pra dentro do R, setado+asserido.**

**Proveniência é `version=timestamp` + `bundle_ref=caminho`** (o alvo do M1.3):
- version = relógio de parede: `models/random-forest/run_pipeline.R:369`
  (`paste0("v_", format(Sys.time(), "%Y%m%d_%H%M%S"))`), `models/holt-winters/R/training/hw_training.R:530`
  (+`trained_at=Sys.time()` `:484`), `models/dessem-linear/R/training/dessem_linear_fit.R:157` (`trained_at`),
  `models/random-forest/R/registry/registry.R:221` (`registered_at`). → dois treinos idênticos geram **IDs
  diferentes** e podem colidir no mesmo segundo num laço 24×5.
- `bundle_ref` é **caminho**, não hash: `scripts/prevcarga:366` (`"bundle_ref": str(bundle), # só o caminho`);
  ponteiro de versão é texto lido como caminho (`models/random-forest/R/utils/artifact_registry.R:167`).
- freeze já é `saveRDS` binário (a base a formalizar): HW `hw_training.R:525/:537`,
  RF `training_plugin.R:718`, dessem-linear `dessem_linear_training.R:261`, dessem-regdin `:197/:213`.
- **Precedente de gate falsificável** (o padrão que o M1 reusa, não reinventa): o par
  **FORBIDDEN Δ=0 / CONTROL Δ≠0** vive em `jobs/tests/test_prevcarga_honesty.sh:14-16` (`FORBIDDEN→Δ=0`,
  `CONTROL→Δ≠0`, `CLEAN2→Δ=0`; veredito `:127-130/:142-148`; driver `scripts/tools/honesty_gate.R:349-350`).
  O Δ=0 container×container está em `jobs/tests/determinism_gate.sh` (exit `:27`, falha `:113`, passa `:114/:118`).
  Slots "GATE 10" por-modelo: HW `run_all_gates.R:178`, RF `:139`, dessem-linear `:203`.

## O modelo (alvo do M1)

Quatro peças na `scripts/lib/` do clean-room (`/opt/ons-source/prevcarga`), hoje **stubs que `stop("… TODO M1")`**:
`runtime.R`, `topology.R`, `provenance.R`, + a fachada `scripts/prevcarga` (Rscript+docopt). O M1 as implementa
e liga 5 comandos de introspecção (`status`/`doctor`/`models`/`areas`/`describe`). Invariante de ordem:
`prevcarga_runtime_enforce()` roda **antes** do corpo de todo comando **guardado** — é a 1ª coisa que a fachada
chama — **salvo a lista de isenção** `-h/--help` e `doctor`/`status` (que rodam `runtime_check()` não-fatal no
corpo, para diagnosticar um ambiente sujo em vez de abortar nele; ver M1.4 Step 2 / M1.5).

---

## Superfícies que isso toca (clean-room `/opt/ons-source/prevcarga`)

- [ ] `scripts/lib/runtime.R` — `prevcarga_runtime_enforce()`/`prevcarga_runtime_check()` (hoje stub `:15-20`). **(M1.1)**
- [ ] `scripts/lib/topology.R` — `prevcarga_topology()`/`prevcarga_members()`/`prevcarga_model_spec()` (stub `:14-25`). **(M1.2)**
- [ ] `config/topology.yaml` — **novo** (clean-room): config declarativo de subsistemas/áreas/perdas + membros (fonte única, extensível). **(M1.2)**
- [ ] `models/<m>/run.yaml` × 5 — **novo** (clean-room): contrato de config por-modelo enxuto (ADR-0021; horizon/history/protocol/seed), sem orquestração. **(M1.2)**
- [ ] `scripts/lib/registry.R` — **novo**: fronteira de contrato de tiers (ADR-0010); ciclo-de-vida no M5. **(M1.6)**
- [ ] `scripts/lib/provenance.R` — `prevcarga_version_hash()`/`prevcarga_run_manifest()`/`prevcarga_freeze()`/`prevcarga_thaw()` (stub `:19-32`). **(M1.3)**
- [ ] `scripts/prevcarga` — dispatcher docopt + runtime-first + taxonomia de exit-code (stub `main():36-38`). **(M1.4)**
- [ ] `scripts/prevcarga` (comandos) — `status`/`doctor`/`models`/`areas`/`describe` compondo os 3 módulos. **(M1.5)**
- [ ] `scripts/lib/README.md` — já descreve o mapa módulo→milestone; não muda (os stubs viram funções). **(cross)**
- [ ] `docs/ESTADO-ATUAL.md` — write-through "Onde estamos": M1 implementado (comandos vivos). **(DoD cross-cutting)**
- [x] `.claude/workflows/implement-milestone.js` — entrada `CHECKPOINTS['M1']`/`'M1-FUNDACAO'` → `_M1_CP` **(feito pré-implement; ver §Checkpoint)**
- [ ] `scripts/lib/tests/` ou `tests/` — suíte testthat nova do M1 (3 gates falsificáveis). **(todos os tickets)**

---

## Tickets

Cada gate NOVO que afirma Δ=0/round-trip/abort carrega **CONTROL com dente** (braço que DEVE divergir) — senão o
FORBIDDEN Δ=0 é vácuo (disciplina herdada de `test_prevcarga_honesty.sh:14-16`).

### M1.1 — `runtime.R`: guarda de determinismo in-process (seta + ASSERE)

**Causa:** §0 — o quarteto de thread-pin não existe no runtime in-process da referência (só no launcher
`install.sh:130` e em harness de teste); in-process o BLAS multithreada e quebra Δ=0 em silêncio. O M1 traz
o pin pra dentro do R, **asserido**, não só setado.

**Steps:**
1. `prevcarga_runtime_enforce(strict = TRUE)` (`runtime.R:15`) — **capta o ENV CRU de entrada PRIMEIRO**
   (`Sys.getenv` dos 6 + `Sys.timezone()`, antes de setar nada). Sob `strict`, aborta **só se o cru estiver
   ativamente SUJO** — TZ **presente e ≠`UTC`** (`TZ` não-vazio e ≠"UTC") **ou** qualquer
   `OMP_/OPENBLAS_/MKL_NUM_THREADS`/`VECLIB_MAXIMUM_THREADS` **presente e >1** — `stop("[prevcarga/runtime] …")`
   nomeando a variável ofensora e seu valor, ANTES de setar. ⚠ **Unset/vazio é BENIGNO** (não é "sujo"): o Step 2
   seta o default; só um valor explicitamente errado derruba. É aqui que o CONTROL morde: um ENV sujo de ENTRADA
   (TZ=SP, OMP=2) precisa abortar — setar-e-só-depois-checar mascararia (o set sobrescreveria de volta pra 1/UTC).
2. **Só então seta** o ambiente limpo: `TZ=UTC`, `LC_ALL=C.UTF-8`, os 4 `*_NUM_THREADS`/`VECLIB_MAXIMUM_THREADS=1`
   via `Sys.setenv`, `Sys.setlocale("LC_ALL","C.UTF-8")`, e os setters in-process que o ENV não cobre com o R já
   de pé — `RhpcBLASctl::blas_set_num_threads(1)`+`omp_set_num_threads(1)` se o pacote existir (senão degrada com
   aviso; não aborta por falta do pacote — o ENV é o piso).
3. **Observa o pool de BLAS no startup** (`blas_get_num_procs()` **antes** do `blas_set_num_threads(1)` do step 2,
   p/ pegar um pool já cacheado antes do R subir): sob `strict`, >1 → `stop("[prevcarga/runtime] …")`. ⚠ Isto é
   **best-effort, não um braço falsificável em unit-test** (pré-cachear um pool BLAS>1 num teste é inviável, e
   depois do step 2 o `blas_get_num_procs()` volta a 1) — logo **não** carrega claim de CONTROL; a falsificação do
   M1.1 vive toda no step 1 (ENV cru). O `doctor` (M1.5) reporta este pool como linha WARN/FAIL, não como gate.
4. `prevcarga_runtime_check()` (`runtime.R:19`): variante **não-fatal** — devolve `list(ok=<lgl>, offenders=<chr>,
   observed=<named list>)` sem `stop()` (checa o cru também). É o que o `doctor` (M1.5) consome para reportar sem abortar.
5. Prefixo de erro estável `[prevcarga/runtime]`; **exit-code `7`** (classe `runtime`, ADR-0020).

**AC:**
- **SETA de fato (braço positivo do Step 2 — senão um impl que só assere passa):** partindo de um ambiente com os
  4 `*_NUM_THREADS`/`VECLIB` **unset** e `TZ` unset/vazio (estado **benigno** que o Step 1 **não** aborta — unset
  ≠ sujo), `prevcarga_runtime_enforce()` retorna sem erro **e** depois `Sys.getenv` mostra `TZ=="UTC"`,
  `LC_ALL=="C.UTF-8"` e os 4 pins `=="1"` (`expect_equal`), e `blas_get_num_procs()==1` quando disponível — prova
  que o Step 2 realmente setou (não só o Step 1 asseriu).
- **FORBIDDEN (ambiente conforme → sem abort, idempotente):** com `TZ=UTC` e os 4 pins=1, `prevcarga_runtime_enforce()`
  retorna sem erro; chamá-la 2× seguidas não muda `Sys.getenv` dos 6 (Δ=0 no estado efetivo) e `prevcarga_runtime_check()$ok==TRUE`.
- **CONTROL (dente — ambiente sujo → DEVE abortar; UM braço por pin):** setar isoladamente **cada um dos 4 pins**
  — `OMP_NUM_THREADS=2`, `OPENBLAS_NUM_THREADS=2`, `MKL_NUM_THREADS=2` e **`VECLIB_MAXIMUM_THREADS=2`** (⚠ nome
  **fora** do padrão `*_NUM_THREADS` — um `enforce` que enumere por glob o ignoraria em silêncio) — **e** `TZ="America/Sao_Paulo"`
  faz `prevcarga_runtime_enforce(strict=TRUE)` **parar** nomeando a variável ofensora (`expect_error(..., "<VAR>")`
  por braço). Se qualquer um **não** abortar, o teste falha (FORBIDDEN Δ=0 não-vácuo, cobrindo os 4 pins, não só OMP).
- `prevcarga_runtime_check()` devolve `ok=FALSE`+`offenders` no ambiente sujo **sem** `stop()` (o `doctor`
  precisa reportar, não crashar).
- Nenhum pin fica hardcoded fora de `runtime.R` no clean-room: `grep -rE "OMP_NUM_THREADS|OPENBLAS_NUM_THREADS|MKL_NUM_THREADS|VECLIB_MAXIMUM_THREADS"`
  em `scripts/` acha só `runtime.R` + testes (os **4** nomes, incluindo o `VECLIB_` fora do padrão).

### M1.2 — `topology.R`: carrega + valida o config declarativo de topologia (fonte única, extensível)

**Causa:** §0 (a–d) — topologia vive em 4 lugares divergentes na referência; `validate-deck`/`train-models`/
`reconcile`/`exports` **todos** vão precisar dela. ADR-0001 manda "fonte única, **não embutida no código**"; o
precedente do `prevcarga-R` declarava áreas como **lista YAML**. O M1 realiza isso como **`config/topology.yaml`**
que `topology.R` carrega — áreas/subsistemas novos entram editando o YAML, sem tocar código. *(Corrige o
desenho anterior "portar os valores pra `topology.R`", que era ele mesmo "embutir no código".)*

**Steps:**
1. Criar **`config/topology.yaml`** (versionado, parte do bundle) declarando a árvore folha→subsistema(+perda)→SIN.
   **Seed** = os valores da referência (`reconcilia_baseline.R:53-57`): `SECO:{areas:[RJ,RO,MG,ES,MT,SP,AC,MS,GO,DF],
   loss:PESE}`, `S:{[RS,PR,SC],PES}`, `N:{[PA,TON,AP,AM,MA],PEN}`, `NE:{[ALPE,CE,PBRN,BAOE,PI,BASE],PENE}`, e a
   ordem de agregação `order:[SECO,S,N,NE]`→SIN (portada de `_plan_areas:217`). Uma chave `members:` declara os
   **8 membros** `(sigla,mdir,EXOGENA)` (portados de `combina_baseline.py:27-36`) — mesma fonte única (ADR-0001:
   "8 membros … num só lugar"); o M4 **enriquece** cada membro com `protocol` (ADR-0011) sem mudar a fonte.
2. `prevcarga_topology(path = ...)` (`topology.R:14`) — ⚠ **aceita o caminho do config** (default = o canônico
   `config/topology.yaml`, sobrescrevível por arg **ou** env `PREVCARGA_CONFIG_DIR`; idem `prevcarga_model_spec(model,
   root = ...)` no step 6). Essa **alavanca de injeção de path** é o que torna os ACs de fixture executáveis
   (precedente: `reconcilia_baseline.R:31` `ROOT` + `:50` `Sys.getenv("RECONC_ARTIFACTS", unset=...)`); sem ela, os
   braços de consistência/paridade não têm como apontar pra um fixture e fechariam VERDE no config embarcado. Lê e
   parseia o YAML (`yaml::read_yaml`) e devolve `$subsystems`
   (nome→{areas, loss}), `$leaves` (**união** das áreas — DERIVADA, não redigitada), `$order`, e a relação de
   agregação do SIN (SIN=Σsubsistemas; e a relação subsistema↔áreas, cuja perda é resíduo **multiplicativo** —
   ADR-0015). ⚠ O `check-coherence` do M3 assere isso como **(a) SIN=Σsubsistemas** (aditiva) **+ (b) k̂∈faixa**
   (identidade multiplicativa — a perda **não** é série `y_pred` emitida; ver `M3-AVALIACAO.md` §M3.4/§Decisões),
   **não** uma identidade aditiva `subsistema=Σáreas+perda` com linha de perda. **Não** hardcoda contagem: o que vale é o que o YAML declara.
3. **Validação de consistência** no carregamento (`stop()`/`stopifnot` com prefixo **`[prevcarga/contract]`** —
   invalidez ESTRUTURAL de config é erro de **schema/contrato** (exit 2), **não** `[prevcarga/topology]`/escopo-área,
   que é reservado p/ "área pedida fora da topologia" no M2/M5): toda área pertence a **exatamente 1** subsistema
   — como áreas vivem **aninhadas** sob subsistemas e `$leaves` é a UNIÃO derivada, a única violação **construível**
   de "exatamente 1" é a **duplicata** (área em 2 subsistemas); órfã (0 subsistemas) é impossível por construção.
   Além disso: todo subsistema tem `loss`; `loss` únicas. Fail-high (ADR-0005).
4. `prevcarga_members()` (`topology.R:18`): lê a chave `members:` → 8 membros com identidade declarativa
   `(sigla,mdir,EXOGENA)` (ADR-0013, **não** glob+regex).
5. Criar os **5 `models/<m>/run.yaml`** (YAML, contrato de config por-modelo — **ADR-0021**), **enxutos** (só o
   contrato de modelagem; NÃO portar a cruft de orquestração/Batch do `job.json` da referência —
   `resources{vcpu,mem_gb}`/`r_deps`/`entrypoint`/`train_args`-wiring ficam de fora, ADR-0016/0018/M8). Valores-seed
   (de `models/*/job.json` da referência):

   | modelo | horizon_days | history_days {train,infer} | protocol | seed | cut_invariant | params |
   |---|---|---|---|---|---|---|
   | holt-winters | 8 | {730, 45} | retrain_harness | 1234 | true | — |
   | random-forest | 9 | {365, 45} | daily | 123 | true | ntree: 100 |
   | lgbm | 1 | {730, 45} | daily | 123 | false | disable_loess/calendar_yearend/train_cv: true |
   | dessem-linear | 9 | {1825, 45} | retrain_harness | null | false | exogena: MED |
   | dessem-regdin | 9 | {1825, 45} | retrain_harness | null | false | exogena: MED |

6. `prevcarga_model_spec(model, root = ...)` (`topology.R:22`; `root` = alavanca de injeção do step 2, default
   `models/`, sobrescrevível por `PREVCARGA_CONFIG_DIR` — p/ os ACs apontarem a fixtures): **lê**
   `horizon_days`+`history_days`+`protocol` de
   `models/<model>/run.yaml` (config **por-modelo** — ADR-0009/0021; o `topology.yaml` **não** replica). O valor
   é o **default**: uma flag de CLI pode sobrescrevê-lo (ADR-0021), honrada pelos comandos consumidores no M5 —
   no M1, `model_spec` devolve o default do arquivo. ⚠ O M1 **lê** só `horizon_days`/`history_days`/`protocol`;
   `seed`/`cut_invariant`/`params` são **carregados no `run.yaml` mas consumidos só no M4/M5** (`seed`→treino
   determinístico, `cut_invariant`→combina/cut-broadcast, `params`→FE) — forward-config, não lida nem validada
   pelo M1 além da presença do arquivo. `stop()` alto com **`[prevcarga/contract]`** (exit 2) se o `run.yaml`
   ou a chave faltar (config faltante/malformado = erro de **contrato** — como a invalidez estrutural do
   `topology.yaml` no step 3; `[prevcarga/topology]`/escopo-área/exit 4 é reservado p/ "área pedida fora da
   topologia" no M2/M5, **não** para config malformado). 5 modelos de produção; `models/_experimental/*` **fora de escopo**.

**AC:**
- **Config-driven (o gate do M1.2):** um grep abrangente (NÃO uma amostra de 4 tokens) em `scripts/` (fora de
  `config/`, `models/*/run.yaml` e testes) retorna **zero** — nenhum valor de topologia hardcoded no R: as **4
  perdas** (`PESE|PES|PEN|PENE`), as **8 siglas de membro** (`RF|LGBM|HW|RGD_MED|RGD_MAX|LIN_MED|LIN_MAX|LIN_MXMN`),
  e os **24 `area_code`** como literais de string (`"RJ"|"RO"|…|"BASE"`). `topology.R` só **carrega** o YAML.
  Adicionar uma área fictícia a um YAML-fixture e re-chamar `prevcarga_topology()` a reflete **sem** mudar código
  (`$leaves` cresce) — prova a extensibilidade.
- **Paridade do SEED com a referência (teste à parte = baseline do A/B, NÃO contagem travada):** o
  `config/topology.yaml` shippado reproduz a referência — `length($leaves)==24`, 4 subsistemas com as perdas
  exatas (PESE/PES/PEN/PENE), `nrow(members)==8` com as 8 siglas. Afere o **seed**; se uma área nova entrar
  depois, o baseline do A/B é re-ancorado conscientemente (o código não quebra).
- **Consistência falha alto (um fixture por violação CONSTRUÍVEL):** `prevcarga_topology()` **para** com
  **`[prevcarga/contract]`** (exit 2) nomeando o ofensor em cada caso — (a) área **duplicada** entre 2 subsistemas
  (o lado violável de "exatamente 1"; órfã é impossível por construção), (b) **subsistema sem `loss`**,
  (c) **`loss` duplicada** entre 2 subsistemas. Os 3 braços cobrem os 3 invariantes.
- **`model_spec` lê do `run.yaml` semeado (step 5):** `prevcarga_model_spec("lgbm")$horizon_days==1` /
  `holt-winters==8` / `random-forest==9` / `dessem-linear==9` / `dessem-regdin==9`, e `$protocol` ∈
  {daily, retrain_harness} batendo a tabela do step 5. Editar `horizon_days` num dos `run.yaml` e re-chamar
  reflete o novo valor (prova que lê, não replica). `prevcarga_model_spec("inexistente")` ou `run.yaml` sem a
  chave → `stop()` `[prevcarga/contract]` (exit 2), **não** `[prevcarga/topology]`.
- **Contrato enxuto (ADR-0021):** `grep -rE "resources|vcpu|mem_gb|r_deps|entrypoint" models/*/run.yaml` retorna
  **zero** — nenhum campo de orquestração/Batch/empacotamento foi portado pro `run.yaml`.

### M1.3 — `provenance.R`: version content-addressed + run-manifest + freeze (CONTRATO)

**Causa:** §0 — `version=timestamp` (`run_pipeline.R:369`, `hw_training.R:530`) dá IDs diferentes p/ treinos
idênticos e colide no mesmo segundo num laço 24×5; `bundle_ref` é caminho (`prevcarga:366`), não hash. O
contrato tem de nascer no M1 para combina (M4, 1º freezer) e training (M5) não nascerem ad-hoc.

**Steps:**
1. `prevcarga_version_hash(fingerprint)` (`provenance.R:19`): `version = "v_" + substr(digest(fingerprint),1,N)`
   onde `fingerprint` é a **lista ordenada** `{model_id, data_fingerprint, cutoff, feature_config, code_commit,
   renv_lock_hash}` (aqui `cutoff` = o corte de treino **por-modelo** `origem−1−horizonte`, distinto do `as_of`
   run-level do step 2). ⚠ **`model_id` (o `mdir`/`name` do `run.yaml`) E `feature_config` (config EFETIVA: defaults do
   `run.yaml` + overrides de CLI — horizon/seed/protocol/`params` incl. `exogena`) ambos entram no hash.** Os 5
   membros DESSEM abrangem **2 modelos × exogena** (`LIN_* = dessem-linear`, `RGD_* = dessem-regdin`; cada um
   ×{MED,MAX,MXMN/—}): `RGD_MED` e `LIN_MED` têm `run.yaml` de contrato **idêntico** e **mesma** `exogena=MED` —
   só o `model_id` os distingue. Se o fingerprint não tiver `model_id` **e** `exogena`, membros **colidem** num
   mesmo `version_hash` e o M4 (1º freezer dos 8 membros) herda a colisão (ADR-0013/0021). `trained_at`/`package_version` **NÃO entram**
   no hash (só no run-manifest, metadado lateral — senão reintroduzem não-determinismo, GATE 10). Serialização
   determinística antes do hash: chave canônica ordenada (não `toJSON` com ordem de inserção).
2. `prevcarga_run_manifest(...)` (`provenance.R:23`): monta o manifesto por-execução — os **12 campos de
   proveniência** `{data_hash, as_of, member_set_hash, model_versions, code_commit, renv_lock_hash, seed,
   platform, blas_threads, tz, lc, bundle_id}` **mais** os **metadados laterais** `{trained_at, package_version}`
   — ⚠ o run-manifest carrega **`as_of`** (o forecast-origin, data humana ÚNICA da rodada), **não** um corte de
   treino único (um só corte p/ todos seria o anti-padrão de leak que o ADR-0006 proíbe; o corte de treino é
   **por-modelo**, `origem−1−horizonte`, e vive no `cutoff` do fingerprint de CADA membro — step 1)
   (que vivem **só** aqui, fora do `version_hash` — Step 1). `member_set_hash` = hash de `prevcarga_members()`
   (liga M1.2). `bundle_id` é **hash** do conteúdo do bundle, não caminho (corrige `prevcarga:366`).
3. `prevcarga_freeze(obj, path)` (`provenance.R:27`): `saveRDS(obj, path, compress=<fixo>)` **binário**
   (IEEE-754), nunca JSON — formaliza o padrão já usado em `training_plugin.R:718`/`dessem_linear_training.R:261`.
   Grava ao lado um `.manifest.json` com `sha256` + `nrows`/`length` do objeto congelado.
4. `prevcarga_thaw(path)` (`provenance.R:31`): `readRDS` + valida o `sha256` do `.manifest` antes de devolver
   (fail-high se o arquivo mudou sob você).

**AC:**
- **FORBIDDEN (round-trip Δ=0):** `prevcarga_thaw(path)` após `prevcarga_freeze(x, path)` devolve objeto
  `identical()` a `x` para um objeto-alvo realista (data.frame de painel LONG + lista de params); e dois freezes
  de **inputs idênticos** → **mesmo `version_hash`** (ao contrário da referência, cujo `v_<timestamp>` difere —
  o teste pode exibir o contraste).
- **CONTROL (dente):** perturbar **um** campo do `fingerprint` (ex. `cutoff` +1 dia) → `prevcarga_version_hash`
  **muda** (`expect_false(identical(h1,h2))`); e corromper 1 byte do `.rds` congelado → `prevcarga_thaw`
  **para** na checagem de `sha256`. Se o hash **não** mudar sob perturbação, o teste falha (Δ=0 não-vácuo).
- **Canonicidade (braço POSITIVO — content-addressing é por conteúdo, não por ordem):** o **mesmo** conteúdo de
  `fingerprint` com os campos em **ordem diferente** (lista nomeada permutada) → **mesmo** `version_hash`
  (`expect_true(identical(h_a, h_b))`). ⚠ A permutação cobre **tanto o nível de topo quanto as chaves ANINHADAS**
  (`feature_config`/`params` é named-list aninhada — ex. ordem de `exogena`/`ntree` dentro de `params`): um impl
  que ordene só o topo deixa a ordem interna de `params` mudar o hash. Sem este braço, um `serialize()` sem
  canonicalização recursiva é silenciosamente order-sensitive e passa VERDE com o content-addressing quebrado.
- **Membros DESSEM não colidem (dois eixos: `exogena` E `model_id`):** (a) mesmo modelo, `exogena` MED≠MAX≠MXMN →
  `version_hash` distintos (`expect_false(identical(...))` par-a-par); (b) **`RGD_MED` vs `LIN_MED`** — `run.yaml`
  idêntico **e mesma** `exogena=MED`, só `model_id` diferente → `version_hash` **DEVE** divergir
  (`expect_false(identical(h_rgd_med, h_lin_med))`). Juntos provam que `model_id` e o override efetivo entram no
  hash, e os 5 membros DESSEM ganham ids distintos (sem colisão herdada pelo M4).
- **`trained_at` E `package_version` fora do hash (GATE-10):** variar `trained_at` (2 instantes) **e** variar
  `package_version` (ex. upgrade de pacote), com todo o resto igual → **mesmo** `version_hash` nos dois casos
  (`expect_true(identical(...))`); ambos aparecem só no run-manifest, nunca no id (senão o content-addressing
  quebra em upgrade de pacote).
- **Run-manifest completo (12 campos + laterais):** `prevcarga_run_manifest(...)` devolve os 12 campos de
  proveniência **todos presentes** (chave existe) e os não-opcionais não-nulos — ⚠ `seed` **pode ser `null`**
  (dessem-linear/dessem-regdin têm `seed: null` no `run.yaml`, legítimo), então a asserção é "chave presente",
  não "valor não-nulo", para `seed` — incl. `member_set_hash == digest(prevcarga_members())` (o elo com M1.2) e os
  carimbos de determinismo `blas_threads==1`/`tz=="UTC"`/`lc=="C.UTF-8"` — mais `trained_at`/`package_version`.
  `bundle_id` é um hash de 64 hex (sha256), não um caminho de filesystem (regex). Um campo ausente → teste falha.
  ⚠ **Anti-leak:** o manifest tem **`as_of`** (forecast-origin único) e **NÃO** um campo `cutoff` run-level
  (`expect_true("as_of" %in% names(m))` + `expect_false("cutoff" %in% names(m))`) — um corte de treino único
  run-level seria o anti-padrão do ADR-0006; o corte por-modelo vive só no fingerprint de cada membro.

### M1.4 — Fachada `scripts/prevcarga`: dispatcher docopt fino + runtime-first + exit-codes

**Causa:** §0 — a fachada da referência é Python/argparse com 3 verbos e shell-out; o clean-room é Rscript+docopt
(`scripts/prevcarga` já tem o bloco `usage` docopt em `:17-34`, mas `main()` só faz `stop("… TODO M1")` em `:36-38`).

**Steps:**
1. **Ampliar o bloco `usage` docopt** (`prevcarga:17-34`, hoje só `doctor/status/models/areas/describe/validate-deck`)
   para enumerar **TODOS os verbos** — os de introspecção do M1 **mais** os não-donos
   (`validate-deck`→M2, `train-models/infer-model`→M5, `forecast`/`score`/`check-coherence`→M3/M6, `backtest`→M7,
   `features`→M5, `combine`/`reconcile`/`export`→M4/M6, `verificar`→M8, `plan`/`publish`/`registry`→M5/M7) **e os
   aliases de compatibilidade `prever`→`forecast` / `treinar`→`train-models`** (ratificados em `CLI-MODULAR-OBJETIVO.md
   §3 — há cron dependendo deles): também na gramática, com `cmd_<alias>` que faz `stop()` nomeando o verbo-alvo
   + o milestone-dono + aviso de depreciação.
   **A lista do Step 1 é fonte única** — alimenta TANTO a gramática docopt QUANTO os handlers `stop()` do Step 2;
   nenhum verbo nem alias ratificado (`CLI-MODULAR-OBJETIVO.md §3`) pode faltar, senão `prevcarga <verbo>` cai no
   dump de usage do docopt em vez do `stop()` nomeando o dono.
   ⚠ **Obrigatório:** docopt **rejeita** um verbo fora da gramática com seu próprio erro de usage ANTES de chegar
   a qualquer dispatch — se os não-donos não estiverem no `usage`, o `stop()` do Step 2 nunca roda e o AC
   (`grep "M5"`) é insatisfazível. Então o `usage` declara todos (com as flags comuns `--format`/`--as-of` onde cabe).
2. `main()` (`prevcarga:36`): `docopt::docopt(usage)`, resolve o verbo, e **a 1ª ação é
   `source("scripts/lib/runtime.R"); prevcarga_runtime_enforce()`** — antes de tocar o corpo de todo comando,
   **exceto a lista de isenção**: `-h/--help` (não roda guard) **e `doctor`/`status`** (estes rodam
   `prevcarga_runtime_check()` **não-fatal** no próprio corpo — ver M1.5 — justamente para **diagnosticar** um
   ambiente sujo em vez de abortar nele; um `doctor` que aborta sob env quebrado seria inútil no seu caso de uso).
   Depois `source()` de `topology.R`/`provenance.R` conforme o verbo. Despacho fino: cada verbo chama
   `cmd_<verbo>(args)`; a fachada **não** reimplementa lógica de modelo. Os verbos não-donos-do-M1 (lista do step 1)
   têm `cmd_<verbo>` que faz `stop("[prevcarga/todo] <verbo> — implementado no <M_n>")` nomeando o milestone-dono —
   agora alcançável, pois o verbo está na gramática.
3. **Taxonomia de exit-code por classe (ADR-0020):** `0` ok · `2` formato · `3` completude · `4` escopo-área ·
   `5` déficit-de-janela · `6` bundle · **`7` runtime**. Um `handler` de topo mapeia o prefixo do erro
   (`[prevcarga/runtime]`→7, `[prevcarga/topology]`→4, `[prevcarga/contract]`→2, `[prevcarga/completude]`→3,
   `[prevcarga/todo]`→1, …) ao código — **prefixo único por classe** (ADR-0020), senão o handler prefixo-keyed não
   distingue 2 de 3.
4. `--format text|json` global (default `text`): cada comando de introspecção (M1.5) emite os dois; a fachada
   roteia o renderizador.
5. Ativar o guard de ambiente da própria invocação. ⚠ O shebang `#!/usr/bin/env Rscript` (`prevcarga:1`) **não**
   carrega `--vanilla` portavelmente (`env -S` não é portável), e `doctor`/`status` são isentos do enforce — logo
   um `~/.Rprofile` do dev seria herdado. Então o **launcher** gerado pelo install (M8, `bin/prevcarga`) invoca
   **`Rscript --vanilla "$REPO/scripts/prevcarga"`** (`--vanilla` ⊃ `--no-init-file`, suprime `~/.Rprofile`; e
   `--no-environ`, `~/.Renviron`) — ou, mínimo, `R_PROFILE_USER=/dev/null` (⚠ **não** `R_ENVIRON_USER=`, que
   governa `~/.Renviron`). No M1 (sem launcher ainda), a invocação de teste usa `Rscript --vanilla scripts/prevcarga`.

**AC:**
- `scripts/prevcarga doctor` e `… models` **despacham** (não caem no `stop("TODO M1")` do stub) e saem `0`
  num ambiente conforme.
- **runtime-first asserido (prova a ORDEM, não só o exit):** um teste que injeta `TZ=America/Sao_Paulo` e roda
  `scripts/prevcarga models` (verbo **guardado**) sai com **exit-code `7`** e `stderr` com `[prevcarga/runtime]`
  **E `stdout` VAZIO** — nenhuma linha da tabela de modelos foi impressa. O `stdout` vazio é o que distingue
  *enforce-antes-do-corpo* (nada imprime) de *enforce-depois-do-corpo* (imprimiria a tabela e só então abortaria
  com o mesmo exit 7) — sem ele, o exit-code sozinho não falsifica a ordem (`models` é inerte). **Complemento
  (isenção):** o **mesmo** env sujo em `scripts/prevcarga doctor` **não** aborta — reporta a linha do pin como
  FAIL e **imprime** a tabela de diagnóstico (ver M1.5), provando a lista de isenção.
- Um verbo não-dono (ex. `train-models`) **está na gramática docopt** (step 1) e sai com erro nomeando o milestone
  (`grep "M5"` no stderr, exit `1` classe `[prevcarga/todo]`), **não** com o dump de usage do docopt nem traceback.
- **Aliases de compat na gramática:** `prevcarga prever` e `prevcarga treinar` **não** caem no dump de usage do
  docopt — saem com `stop()` nomeando o verbo-alvo (`forecast`/`train-models`) + o milestone-dono + **aviso de
  depreciação** (`grep -E "forecast|deprec"` / `"train-models|deprec"` no stderr). Prova que os aliases do §3 estão na gramática.
- **Exit-codes por classe exercidos no nível da CLI** (não só o 7): `prevcarga areas` com um `topology.yaml`-fixture
  **estruturalmente inválido** (área **duplicada** entre 2 subsistemas) → exit **2** (`[prevcarga/contract]`); `prevcarga models` com um
  `run.yaml` faltante → exit **2** (`[prevcarga/contract]`). Fecha o mapa prefixo→código com teste. (Exit **4**
  escopo-área / **5** janela / **6** bundle **não** são emitidos no M1 — entram no M2+ quando `validate-deck`/
  `train-models` checam área/janela contra a topologia; só o mapa do handler os declara.)
- `--format json` em `doctor`/`models`/`areas`/`describe`/`status` produz JSON parseável (`jsonlite::fromJSON`
  não erra) com um schema estável; exit-code idêntico ao do `--format text`.
- `-h`/`--help` imprime o `usage` e sai `0` **sem** rodar o guard (não exige ambiente conforme pra ler ajuda).
- **Sem dep de S3 (grep possuído aqui):** `grep -rE "paws|aws|s3://" scripts/` retorna **zero** — a fachada e os
  módulos do M1 não introduzem nenhuma dependência de nuvem (produto local-only, ADR-0016).
- **Isolamento de `~/.Rprofile` (exerce o Step do `--vanilla`):** um teste invoca **`Rscript --vanilla
  scripts/prevcarga doctor`** (o mecanismo que aplica `--vanilla`, já que o shebang nu não o faz e `doctor` é
  isento do enforce) com um `~/.Rprofile`-fixture que faz `Sys.setenv(OMP_NUM_THREADS=4)` (⚠ **não**
  `options(mc.cores=4)` — invisível ao pin-check, gate vácuo) e confirma que o `doctor` **não** herda o pin
  (linha do pin segue PASS=1). Um controle sem `--vanilla` herdaria o OMP=4 (linha FAIL) — provando que o gate morde.

### M1.5 — Comandos de introspecção: `status`/`doctor`/`models`/`areas`/`describe`

**Causa:** §0 — **não existem** na referência (nada a espelhar); são capacidade nova que compõe os 3 módulos do
M1. São o 1º consumidor real de `runtime.R`/`topology.R`/`provenance.R` — e o `doctor` é o **portador do gate
falsificável** do M1.1 (reporta, degradando, em vez de abortar).

**Steps:**
1. `cmd_doctor`: chama `prevcarga_runtime_check()` (não `enforce` — reporta sem crashar) e lista cada checagem
   (TZ, LC, os 4 thread-pins, **pool de BLAS `blas_get_num_procs`** — a linha best-effort do M1.1 Step 3, versão
   R/renv, arrow disponível) com PASS/WARN/FAIL. Sai `0` se tudo PASS; exit de classe runtime (`7`) se algum FAIL
   **duro** (TZ≠UTC / thread>1); o pool de BLAS e o resto são WARN. Best-effort: nunca trava.
2. `cmd_models`: `prevcarga_members()` + `prevcarga_model_spec()` por modelo → tabela
   `sigla · mdir · exogena · horizon_days · history{train,infer}`. Lê **só** de `topology.R` (nenhum valor
   local).
3. `cmd_areas`: `prevcarga_topology()` → árvore folha→subsistema→SIN com as perdas (imprime **completa** no M1;
   sem flag de filtro — `areas` só declara `--format` na gramática; filtrar por área fica p/ depois).
4. `cmd_describe`: resolve os cortes de uma `--as-of` **na medida do M1** — como `dates.R` é M2, no M1 o
   `describe` mostra a topologia + a spec do modelo + (se houver `manifest.json` no deck) o `corte` lido; declara
   explicitamente "corte-de-treino: derivado no M2" em vez de inventar. Sem alucinação de data.
5. `cmd_status`: deck mais recente usável (lê `manifest.json` se existir, `_load_manifest`-equivalente), idade
   do bundle, e um resumo do `doctor`. No M1 é best-effort e pode reportar "sem deck" sem erro.

**AC:**
- `doctor` num ambiente conforme → todas as linhas PASS (**incluindo a linha do pool de BLAS**), exit `0`; num
  ambiente com `OMP_NUM_THREADS=2` → a linha do pin é **FAIL** e o exit é de classe runtime `7` (consome o M1.1
  sem duplicar a lógica).
- `models` lista os membros e modelos **lidos do config** (no seed: 8 membros, 5 modelos) com `horizon_days`
  vindo de cada `models/<m>/run.yaml` (`lgbm=1`, `hw=8`, `rf=9`); `grep` prova que `cmd_models` não tem nenhuma
  contagem nem número de horizonte **literal** — tudo vem de `topology.R`/`run.yaml` (contagem não travada: área
  ou modelo novo no config muda a saída sem tocar código).
- `areas` imprime as folhas sob os subsistemas **que o `topology.yaml` declara** na ordem `$order` + SIN, com as
  perdas (no seed: 24 folhas / 4 subsistemas / PESE/PES/PEN/PENE). A asserção afere **contra o config carregado**,
  não contra um literal `24` (coerente com M1.2: contagem = baseline do A/B, não invariante; área nova = editar YAML).
- `describe --as-of 2024-06-01` **não** imprime um corte-de-treino numérico (declara "M2"); não há `stop()`.
- **`status` é isento do enforce (como `doctor`):** sob env sujo (`OMP_NUM_THREADS=2`), `scripts/prevcarga status`
  **reporta** (embute o resumo do `doctor` com a linha do pin FAIL) e **não aborta** — prova que `status` está na
  lista de isenção do M1.4 Step 2 (um `status` guardado abortaria e o teste falha).
- `--format json` de cada um devolve JSON com o mesmo conteúdo da saída text (chaves estáveis).

### M1.6 — `registry.R`: fundação do contrato (fronteira de tiers); ciclo-de-vida no M5

**Causa:** reconciliação de write-through — ADR-0010 diz "fronteira explícita em `registry.R` … **Implementado
no M1** (fundação do registry)" e `ESTADO-ATUAL.md:49` idem, mas `scripts/lib/README.md` e `PLANO.md §M5` põem o
**ciclo-de-vida** (ls/promote/rollback/pin/gc) no M5. Resolução (Gabriel, 2026-10-11): a **fronteira de contrato**
é M1; o **ciclo-de-vida** é M5.

**Steps:**
1. Criar `scripts/lib/registry.R` com a **fronteira de contrato do ADR-0010**: o ponto ÚNICO onde um modelo se
   **registra** declarando `requires`/`provides` (Tier A = R6 contract-verified: RF/LGBM/HW/DESSEM). Tier B
   (vendored-lazy) fica **vazio/reservado** (ADR-0010: só habitaria externo de terceiro, que não temos).
2. `prevcarga_register(spec)` / `prevcarga_registry()`: registra e verifica em **wire-time** que `requires` é
   satisfeito pelos `provides` disponíveis — **fail-loud** com `stop("[prevcarga/contract] registry: …")`
   (violação do contrato do registry = classe **contract**/exit 2, ADR-0010/0020; o detalhe "registry:" fica na
   mensagem, não num prefixo novo fora da taxonomia). **NÃO** implementa `ls/promote/rollback/pin/gc` (M5) — só a fronteira.
3. A identidade de membro vem de `topology.R` (M1.2, fonte única) — `registry.R` **não** tem lista de membros local.

**AC:**
- `scripts/lib/registry.R` existe com o ponto único de registro + verificação de contrato (ADR-0010 Tier A);
  `grep` prova **zero** lógica de `ls/promote/rollback/gc` no M1 (essas são M5).
- **CONTROL (dente):** registrar um spec cujo `requires` NÃO é satisfeito → `stop()` fail-loud em wire-time cujo
  texto casa **`[prevcarga/contract]`** E `requires` (`expect_error(..., "\\[prevcarga/contract\\].*requires")`) —
  prova o prefixo (senão o handler mapeia pra exit 1 genérico, classificando errado a classe contract); um spec
  conforme registra sem erro (`expect_silent`). *(O braço de nível-CLI — um comando que dispare o registro saindo
  com exit 2 — só é exercível no M5, quando um verbo consumidor registra um modelo; no M1 nenhum comando dispara
  `prevcarga_register` (sem código de modelo ainda), então o gate do M1.6 é o de FUNÇÃO acima.)*
- `grep` prova que `registry.R` lê membros de `topology.R`, sem lista local de membros.

---

## Decisões (ratificadas nesta frente)

- **Comandos de introspecção são novos, sem oráculo de referência.** A referência tem 3 verbos (argparse);
  `status/doctor/models/areas/describe` não existem lá. O A/B (M8) cobre os verbos **numéricos**
  (`forecast`/`verificar`), não a introspecção. (Corrige o AC genérico "espelha a referência" do PLANO para o M1.)
- **Thread-pin vira invariante in-process, asserido** (não só ENV). ENV é o piso; `blas_set_num_threads` cobre
  o pool já cacheado. Razão: a referência só pinava no launcher + testes; in-process o Δ=0 fura em silêncio
  (ADR-0008, [[adrs-prevcarga]]).
- **Topologia é config declarativo (`config/topology.yaml`), não hardcoded no R.** Realiza a "fonte única, não
  embutida no código" do ADR-0001 (que já rejeita "topologia embutida no código"); precedente `prevcarga-R`
  (`data.areas:` em YAML). Áreas/subsistemas novos = **editar o YAML**, sem tocar código; a topologia **não fixa
  contagem** (seed 24 folhas/4 subsistemas/8 membros serve só de baseline do A/B). Membros no mesmo config
  (ADR-0001), enriquecidos com `protocol` no M4 (ADR-0011). **Consequência p/ M2:** o `validate-deck` cruza —
  `area_code` presente no deck **sem** declaração no `topology.yaml` = **fail-high** (área nova é edição
  consciente do config, não passthrough silencioso).
- **Config por-modelo = `run.yaml` enxuto, não `job.json` (ADR-0021).** O `job.json` da referência é um manifesto
  de AWS Batch com o contrato de modelagem baked-in. O clean-room porta só o **contrato de modelagem**
  (`horizon_days`/`history_days`/`protocol`/`seed`/`cut_invariant`/`params`) num `models/<m>/run.yaml` (YAML, como
  o `topology.yaml`); **não porta** `resources`(Batch)/`r_deps`(bundle)/`entrypoint`/`train_args`-wiring
  (ADR-0016/0018/M8). Valores são **defaults sobrescrevíveis por flag de CLI** (ex. `--exogena` gera os membros
  DESSEM — ADR-0013); a proveniência carimba o **efetivo**. `topology.R` lê o `run.yaml`, não replica (honra
  ADR-0009, que pina a variação operacional no arquivo por-modelo). Semeado no M1.2; override honrado no M5.
- **`registry.R`: fronteira de contrato no M1, ciclo-de-vida no M5.** Reconcilia a contradição ADR-0010 ("fundação
  no M1") vs PLANO/README ("M5"): o **contrato/fronteira** (tiers A/B, verificação wire-time) nasce no M1.6; o
  **ciclo-de-vida** (ls/promote/rollback/pin/gc) é M5.
- **Taxonomia de exit-code da CLI (ADR-0020).** Classes `0/2/3/4/5/6` + **`7`=runtime** (fecha a referência
  circular M1.1↔M1.4). Contrato transversal herdado por M2–M8; número de classes imutável (renumerar quebra CI).
- **`version` content-addressed já no M1** (contrato), embora o 1º freezer real seja a combina (M4). Nasce aqui
  para nenhum módulo criar `version=timestamp` ad-hoc (ADR-0011).
- **`_deliver`/`_is_s3` NÃO portados no M1** — export é M6; modo-por-prefixo-`s3://` é não-alvo (ADR-0016,
  produto local-only). O M1 não introduz nenhuma dependência de S3.

---

## Checkpoint & gates

M1 é um **milestone de infra pura** — não toca número de modelo. Logo é seu **próprio checkpoint** (fecha batch
de 1), **sem A/B de deploy** (o A/B numérico é o M8; aqui não há `y_pred` novo pra comparar). A entrada já foi
adicionada em `.claude/workflows/implement-milestone.js` (`CHECKPOINTS['M1']`/`'M1-FUNDACAO'` → `_M1_CP`):

- `fullGates`: a **suíte testthat nova do M1** (os gates falsificáveis: runtime cru/pós-set FORBIDDEN/CONTROL,
  provenance round-trip + canonicidade + CONTROL, topology grep+paridade+consistência, registry fail-loud, dispatch)
  toda verde. ⚠ **NÃO** há `models/<m>/tests/run_all_gates.R` no clean-room (os modelos — e seus gates —
  portam no M5); logo **não existe braço de regressão de modelo no M1** (M1 não cria nem toca código de modelo,
  só semeia os `run.yaml` de config). O gate de regressão por-modelo entra junto com os modelos, no M5.
- `abReference`: **nenhum** no M1 (`deployByDefault: false`). O A/B entra no M8.
- Write-through: `docs/ESTADO-ATUAL.md` "Onde estamos" reflete M1 implementado (stubs→funções, comandos vivos).

---

## Definition of Done

- [ ] **M1.1** `runtime.R` seta+assere TZ/LC + 4 thread-pins in-process; `enforce` aborta sob TZ≠UTC/thread>1
  (CONTROL com dente verde), `check` reporta sem crashar; nenhum pin hardcoded fora de `runtime.R`.
- [ ] **M1.2** topologia é **config declarativo** `config/topology.yaml` (grep prova zero valor hardcoded no R;
  `topology.R` só carrega+valida); o seed reproduz a referência (24 folhas/4 subsistemas/8 membros) como
  baseline do A/B, **não** contagem travada (área nova = editar YAML); consistência falha alto; **os 5
  `models/<m>/run.yaml` enxutos são semeados** (ADR-0021) e `model_spec` lê `horizon_days`/`history_days`/`protocol`
  deles (sem campo de orquestração portado).
- [ ] **M1.3** `provenance.R` dá `version` content-addressed (dois inputs idênticos → mesmo id; ordem de campo
  não muda o hash; `trained_at` fora do hash), run-manifest com `bundle_id`=hash, e freeze/thaw `saveRDS` com
  round-trip Δ=0 (CONTROL verde).
- [ ] **M1.4** `scripts/prevcarga` despacha via docopt (**`usage` enumera TODOS os verbos**, senão docopt rejeita
  antes do `stop()`), roda `prevcarga_runtime_enforce()` **antes** do corpo de todo comando **guardado**
  (`doctor`/`status`/`-h` isentos — rodam `check()` não-fatal), mapeia erro→exit-code por classe
  (**ADR-0020**, runtime=`7`), verbos não-donos param nomeando o milestone, roda `--vanilla` (isolamento de
  `~/.Rprofile`); `--format json` global.
- [ ] **M1.5** `status/doctor/models/areas/describe` compõem os 3 módulos (sem valores/contagens locais); `doctor`
  porta o gate de runtime; `models`/`areas` aferem **contra o config carregado** (não literal 24/8/5); `describe`
  não inventa corte-de-treino (M2).
- [ ] **M1.6** `registry.R` tem a fronteira de contrato de tiers (ADR-0010, fail-loud wire-time); ciclo-de-vida
  (ls/promote/rollback/gc) **não** está no M1 (é M5); lê membros de `topology.R`.
- [ ] **Exit-codes (ADR-0020):** taxonomia `0/2/3/4/5/6/7` fixada; classe `runtime`=`7` atribuída.
- [ ] **Gates falsificáveis (cross-cutting):** todo gate novo do M1 que afirma Δ=0/abort/fail-loud tem braço
  CONTROL com dente que DEVE divergir (runtime, provenance, topology, registry). Suíte testthat do M1 verde.
- [ ] **Regressão (cross-cutting):** não há gate de modelo a regredir no M1 — os `models/<m>/tests/` portam no M5
  (M1 não cria nem toca código de modelo, só os `run.yaml` de config).
- [ ] **Write-through:** `docs/ESTADO-ATUAL.md` reflete M1 implementado (comandos vivos; stubs→funções).
- [x] **Checkpoint wiring (feito pré-implement):** `CHECKPOINTS['M1']`/`'M1-FUNDACAO'` → `_M1_CP` no
  `implement-milestone.js` (`deployByDefault:false`, `fullGates` = suíte testthat do M1, `passCriteria` testthat-only).
  O juiz de gate agora lê **`${CP.passCriteria}`** (não mais o texto combina hardcoded), então o M1 não exige
  run_all_gates.R/jobs-tests/combina inexistentes no clean-room. O implement do M1 só confirma.
- [ ] **Sem dep de S3** (AC do M1.4): grep prova zero `paws`/`aws`/`s3://` introduzido no M1 (local-only, ADR-0016).

---

## Relacionados
- [PLANO.md](./PLANO.md) — M1 no programa M1…M8; dependências (M1 → M2 → M3 → M4/M5 → …).
- [CLI-MODULAR-OBJETIVO.md](./CLI-MODULAR-OBJETIVO.md) — §1 princípios, §4.2 exit-codes, §4.3 determinismo, §4.4 proveniência.
- `../adr/0001-topologia-sin-map-reduce.md` (topologia config-driven), `0008-…falsificavel.md` (gate falsificável),
  `0009-arquitetura-modular-modelo.md` (config por-modelo), `0010-contrato-features-registry.md` (tiers/registry),
  `0011-ciclo-artefato-registry.md` (version/freeze), `0016-orquestracao-io-nuvem.md` (local-only),
  **`0020-taxonomia-exit-code-cli.md`** (exit-codes), **`0021-run-yaml-contrato-modelo.md`** (run.yaml),
  **`0022-runtime-guard-dispatch.md`** (enforce-first + isenção doctor/status).
- Memória: [[adrs-prevcarga]], [[cli-modular-objetivo]], [[padrao-milestone-specforge]], [[empacotamento-cli-multiplataforma]].

## Fonte do design
PLANO.md §M1 (re-sequenciamento `wf_4d2ca349-18b`) + varredura de ground-truth da referência
`/opt/ons-source/prevcargadessem3` (`file:line` em §0, 2026-10-11) + os stubs de `scripts/lib/*.R` do clean-room.
