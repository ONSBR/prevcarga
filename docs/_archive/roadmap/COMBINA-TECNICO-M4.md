# Migração da combina Python→R — REFERÊNCIA TÉCNICA do M4

> ⚠️ **SUPERSEDIDO pelo re-sequenciamento (2026-10-10).** Esta não é mais uma trilha M1…M6 própria:
> a combina foi **dissolvida no M4** do `docs/roadmap/PLANO.md` (escrita direto como `scripts/lib/
> combination.R`, consumindo topologia/datas/evaluation já prontos — sem port-depois-refatora). O bundle
> R-only/remoção do Python foi para o M8. **Este arquivo é mantido como a REFERÊNCIA TÉCNICA do M4**:
> o conteúdo numérico (Duchi/FISTA, corte por data-alvo, freeze saveRDS, gates de invariante/otimalidade/
> honestidade/qualidade) continua válido — muda só QUANDO/em que ordem. A numeração M1…M6 abaixo é
> **histórica**; leia-a como os tickets internos do M4.

> 📝 **DRAFT / plano ativo.** Estrutura de milestones/tickets/DoD/AC adaptada do padrão
> `specforge-features/.vault/lifecycle/milestones` (ver [[padrao-milestone-specforge]] no §Fonte).
> Ao concluir, este arquivo sai de `roadmap/` (→ apaga ou `_archive/`) e o estado durável vai
> para `docs/ESTADO-ATUAL.md` (write-through — DoD item final).

> 🔎 **Verify (M-combina):** ⚠ **verify PENDENTE** — rodar `/verify-milestone M-combina` (adaptado)
> antes do implement. Alvo: `ready:true` · DoD presente · todo ticket M1…M6 com Steps + AC.

> **Decisão do Gabriel (2026-10-09):** colapsar o runtime numa linguagem só — **R + renv**
> ("não vou criar duas dependências"). Toda a combinação em Python migra para R. ⚠ **Sem
> paridade com o solver Python** (decisão explícita): o oráculo Python está contaminado pelo
> [[combina-vazamento-horizonte]] — casar com ele reproduziria um bug. A combina R é validada
> por **propriedades**, não por output-parity. Origem: avaliação de arquitetura + 3 workflows
> desta sessão (auditoria, CLI multiplataforma, mapa de porte).

---

## 0. Spec — o que migra e por quê

A combina (ensemble convexo + congelamento de pesos) é o **único Python que o `prever` diário
executa** (`jobs/run_reduce.sh:83` invoca `scripts/combina_baseline.py`) somado à **própria CLI
`scripts/prevcarga`** (argparse + pandas). A reconcilia **já é R** (`scripts/reconcilia_baseline.R`)
— não migra. Enquanto existir Python no caminho, o bundle exige um venv Python **não-autocontido**
(buildado contra o `/usr/bin/python3.10` do host, `ci/build_marmita.sh:72`) — o **gap #1** do
empacotamento zero-dep ([[empacotamento-cli-multiplataforma]]). (O bundle é o conceito antes chamado
de "marmita"; nome real do script ainda é `ci/build_marmita.sh`.)

O núcleo numérico é **`numpy`-puro → R-base, SEM scipy** (confirmado no fonte: `np.sort`/`cumsum`/
`linalg.eigvalsh`/matmul; nenhum `scipy.optimize`). Deps científicas da combina: `numpy`+`pandas`+
`pyarrow` só — e os equivalentes R (`arrow`, `data.table`, `jsonlite`, `Matrix`) **já estão no
`renv.lock`**. Zero linha nova no lock de produção; só `testthat` entra, e é dev-only.

## Intent
Tirar o Python do caminho EMPACOTADO (combina + CLI + gate), deixando um runtime **R-only**, e
no processo **corrigir o vazamento de horizonte da combina em voo** (escrever a combina R já
correta). O bundle passa a ser só R+renv — o mesmo runtime dos `models/*`.

---

## Decisões (ratificadas — Gabriel, 2026-10-09)

- ✅ **Runtime R-only.** Combina + CLI + gate migram para R. Sem segunda linguagem no produto.
  Write-through no `ESTADO-ATUAL.md` ao concluir (M6).
- ✅ **SEM paridade com o solver Python.** Não é preciso reproduzir o resultado numérico da combina
  Python. Motivo: o oráculo está contaminado pelo [[combina-vazamento-horizonte]]. A combina R é
  validada por **(1) invariantes** (schema LONG, simplex `w≥0`/`Σw=1`, determinismo), **(2) otimalidade
  convexa** (mesmo valor-objetivo, NÃO o mesmo `w` — não-identificado sob `G` quase-singular),
  **(3) honestidade** (o G2 reescrito, nasce verde) e **(4) qualidade preditiva** (bate best-member/EQW
  no holdout). ⚠ **Gatear SEMPRE no `y_pred` combinado (`a+Xw`), NUNCA nos pesos `w`** — `G` é
  quase-singular (variantes dessem LIN/RGD colineares) ⇒ `w` não-identificado.
- ✅ **Corrigir o vazamento EM VOO** (não "fiel-primeiro"): a combina R nasce com o corte de treino
  por **data-alvo** (`origem−1−horizonte`, regra do CLAUDE.md §6), não por mês-da-origem (`ym<m`).
  Como não há paridade a preservar, não há motivo para portar o bug. Separar em commits (port vs
  semântica) vira higiene opcional de revisão, não exigência.
- ✅ **Freeze binário.** Pesos congelados via `saveRDS` (IEEE-754 bit-a-bit), NÃO JSON — `jsonlite`
  não reproduz o shortest-repr e `digits=4` quebra o round-trip em silêncio. Runtime R-only não
  precisa de JSON interoperável.
- ✅ **Esperado:** os números de backtest publicados VÃO mudar (sem paridade + leak corrigido) —
  re-baselina junto com a F8/re-run honesto pendente. Dito, não escondido.

---

## Superfícies que isso toca (checklist pré-verify)
- **`scripts/`:** `combina_baseline.py` → `combina_baseline.R` (núcleo+IO+freeze); `prevcarga`
  (Python) → `Rscript`+`docopt`; **novo** `combina_members.R` (fonte única de `MEMBERS`, hoje
  `import combina_baseline._cb.MEMBERS`).
- **`jobs/`:** `run_reduce.sh:83` repontado de `$PYTHON $COMBINA` → `$RSCRIPT combina_baseline.R`.
  Os runners bash **ficam** (não são Python — fora de escopo).
- **`renv.lock`:** nenhuma dep de produção nova (`arrow`/`data.table`/`jsonlite`/`Matrix`/`docopt`/
  `digest` já presentes); **+`testthat`** dev-only.
- **Gates:** 5 invariantes pytest (`tests/unit/test_combina_*.py`) re-expressos em R (`testthat`);
  **novo** gate de honestidade G2 (corte por data-alvo, horizonte cheio) + gate de otimalidade.
- **Dev-only (NÃO migram agora):** `report_A.py` + `scripts/lib/combina/` (diagnóstico peak/ramp),
  `tools/*.py`, `data-prep/*.py`, `jobs/aws/watch_backtest.py` — não entram no bundle (a regra
  "duas dependências" é sobre o PRODUTO). Reclassificar em MIGRAR-DEPOIS/MANTER-DEV-ONLY no M6.
- **`docs/ESTADO-ATUAL.md`:** write-through do runtime R-only + do fix do vazamento (M6).
- **Bundle (`ci/build_marmita.sh`):** remover o passo do venv Python quando M6 fechar.

---

## Milestones (tickets)

Ordem = dependência (M1→M6). Cada milestone cita a seção-fonte. **AC = asserções verificáveis**
(gates Δ=0 / testthat distribuídos por milestone). Sem paridade vs Python — ver §Decisões.

### M1 — Núcleo numérico correto em R-base (sem IO)
**Steps:** (1) `_proj_simplex(v)` (Duchi 2008): `sort(decreasing)`+`cumsum`+`pmax`; ⚠ `rho<-max(which(cond))`
**não** `which.max`; `length(v)==1 → 1.0`. (2) `fit_convex(X,y,w0,iters=200,tol=1e-8)` (FISTA):
`L<-2*max(eigen(G,symmetric=TRUE,only.values=TRUE)$values)+1e-9`; centrar (`colMeans`/`crossprod`);
loop com `as.vector(G%*%z)`/`drop()` p/ manter vetor; `a<-ym-sum(xm*w)`. (3) `produce_combined`:
intercepto `a` **fixo** (NÃO renormaliza) + `M%*%(w renormalizado sobre membros presentes)`;
`fallback → base=0`; `s==0 → mean(na.rm=TRUE)`. (4) `produce_eqw` (`rowMeans(na.rm=TRUE)`).
(5) `fit_area_expanding` **já com o corte correto**: treino por **data-alvo** (`target_datetime <
origem−horizonte`), NÃO `ym<m` (§Decisões — fix do vazamento em voo). `Sys.setlocale('LC_COLLATE','C')`
+ `setorder` explícito p/ a cadeia de warm-start ser determinística.
**AC:** (invariante) p/ qualquer entrada, `_proj_simplex` devolve `w≥-1e-9` e `|Σw−1|<1e-9`
(teste property-based). (otimalidade) o `(a,w)` ajustado é estacionário: gradiente projetado do
objetivo `‖y−(a+Xw)‖²` com `w` no simplex `< 1e-6` (prova que o FISTA minimiza, sem oráculo externo).
(determinismo) mesma entrada → `max|Δw|<1e-12` em 2 runs. **NÃO há AC de paridade vs Python.**
`Rscript` do núcleo roda standalone. `testthat` verde.

### M2 — IO / painel / descoberta de membro em R
**Steps:** (1) `member_runs(root,mdir,area,exo)` — **uma** descoberta com a regex agnóstica de
protocolo `_(daily|retrain_harness|infer)` (+exo +`_cHHMM`), `perl=TRUE`; `stop()` ALTO no invariante
§6.1 (anti dupla-contagem). ⚠ NÃO portar o bug do `io.py` que ignora runs `infer`. (2) `load_area_long`
+ `_read_member` via `arrow::read_parquet` (S3-capable). (3) `build_panel`: `member<-factor(levels=MEMBER_LABELS)`
+ `dcast(...,drop=c(TRUE,FALSE))` + `setcolorder` (senão coluna de membro ausente some e desalinha `X`);
`y_true` = "first APÓS sort RF-no-topo"; `ym<-substr(as.character(forecast_origin),1,7)`. (4) `_join_fresh_truth`
+ `round_ts30`/`ts_r30` **reusando `prevcarga_read_actual`** (criado no **M3.3**, `evaluation.R` — leitura **CANÔNICA** `load_mwh` com fail-high; ⚠ **NÃO** a `read_actual_all` alias-tolerante do oráculo, rejeitada por ADR-0004/0005 — só a mecânica `ts_r30`/`mean` é a mesma recipe); merge por chave
`sort=FALSE` (nunca posicional). (5) `_broadcast_cut_invariant` com `copy()` explícito.
**AC:** descoberta lê **8/8 membros**; variantes `MED/MAX/MXMN` disjuntas; painel com `f_*` na ordem
`MEMBER_LABELS`. (honestidade de tz) `round_ts30` por `as.numeric(POSIXct)` (invariante ao `tzone`);
teste prova que **nenhum** caminho usa `with_tz` (sem shift −3h). (join) no braço backtest, `y_true` do
join == verdade do dado (Δ=0 de identidade). `testthat` verde.

### M3 — Freeze / registry binário (`saveRDS`)
**Steps:** (1) `train_area` congela `list(meta, weights)` via `saveRDS(..., vdir/weights.rds)` +
ponteiro `latest` texto (`writeLines`), espelhando `models/*/R/helpers/artifact_registry.R`.
(2) `apply`: `readRDS` + reconstruir a chave `(ym,lead[,cut])` idêntica ao `key_of`; `write_long_A`
via `arrow::write_parquet` no schema LONG das 8 colunas. **Abandonar JSON** (§Decisões).
**AC:** `apply(load(train()))==full` com **Δ=0 EXATO** (round-trip binário garante por construção).
Área não-treinada **falha ALTO** (`expect_error` no `latest`), não devolve vazio mudo. `testthat` verde.

### M4 — Gates em R: invariantes + otimalidade + honestidade G2 + qualidade preditiva
**Steps:** (1) re-expressar os 5 invariantes de `tests/unit/test_combina_*.py` em `testthat`
(`freeze_parity`→round-trip binário, `member_discovery`, `ytrue_join`, `panel_append`, `interp`);
fixtures por **fórmula fechada** (o `np.random` do pytest não é portável → materializar parquet 1×
e commitar, OU gerar por fórmula). (2) **gate de honestidade G2 reescrito** (nasce verde): perturbar
`y_true` em `target_datetime ≥ origem` **não move** `y_pred`; `max_dplus = horizonte cheio` (~9),
NÃO `max_dplus=0` (o G2 antigo era cego). (3) gate de otimalidade (do M1) sobre 24 áreas. (4) gate
de **qualidade preditiva**: a combina bate/iguala best-member e EQW num holdout por `(área,d_plus)`.
**AC:** `panel_append`: `append==rebuild` Δ=0 **E** controle-com-dente (dropar origem de mês anterior
muda o fit, Δ≠0 — prova que o Δ=0 não é vazio). G2: braço FORBIDDEN `y_pred` Δ=0, braço CONTROL
(perturbar `O−1` legal) Δ≠0 (falsificável, no molde `honesty_gate.R`). Qualidade: `WMAPE_combina ≤
WMAPE_best_member + ε` por `(área,d_plus)` reportado. `testthat` + os gates verdes.

### M5 — Virar produção + CLI em R
**Steps:** (1) `jobs/run_reduce.sh:83` → `$RSCRIPT combina_baseline.R` (modos `train`/`apply`),
preservando a interface de env/caminhos atual. (2) `scripts/prevcarga` → `Rscript` + `docopt`,
mantendo subcomandos `prever`/`treinar`/`verificar` + o contrato de saída (`manifest.json` + layout
plano); `verificar`/`_worst_delta` com `arrow`+`data.table`; `MEMBERS` de `combina_members.R` (fonte
única). (3) smoke E2E: `prever`/`treinar`/`verificar` passam **sem `python3` no PATH**.
**AC:** `jobs/tests/test_reduce_split_parity.sh` e `test_prevcarga_{cli,subset,layout}.sh` **seguem
verdes** com a combina R (o split train/apply == full continua Δ=0 — esses gates comparam R-vs-R,
não Python). `grep` confirma zero `$PYTHON`/`python3` em `run_reduce.sh` e `prevcarga`. Smoke sem
Python no PATH verde.

### M6 — Bundle R-only + write-through
**Steps:** (1) remover `combina_baseline.py` do caminho de produção; os pytest do combina ficam só
como referência histórica (ou apagados). (2) `ci/build_marmita.sh`: remover o passo do venv Python
(e o `python-build-standalone` sai do roteiro de [[empacotamento-cli-multiplataforma]]). (3) reclassificar
a cauda dev-only (`report_A`/`lib/combina`/`tools`/`data-prep`) em MIGRAR-DEPOIS vs MANTER-DEV-ONLY.
(4) **write-through no `docs/ESTADO-ATUAL.md`**: runtime R-only + o fix do vazamento (com a ressalva
de que os números re-baselinam).
**AC:** `grep` confirma zero `python3`/venv em `jobs/run_reduce.sh`, `scripts/prevcarga` e no caminho
empacotado; o bundle builda **sem** `python-build-standalone`. `docs/ESTADO-ATUAL.md` reflete o
estado novo (write-through). Os `models/*/tests/run_all_gates.R` + `jobs/tests/` seguem verdes.

---

## Definition of Done
- [ ] **M1** núcleo R-base (Duchi/FISTA/produce/eqw) com corte por data-alvo; invariante-simplex +
      otimalidade + determinismo verdes; SEM AC de paridade vs Python.
- [ ] **M2** IO/painel/descoberta unificada (lê `infer`); tz sem shift; join Δ=0 de identidade no backtest.
- [ ] **M3** freeze `saveRDS`; `apply(load(train()))==full` Δ=0; área não-treinada falha ALTO.
- [ ] **M4** 5 invariantes em `testthat` + G2 reescrito (falsificável) + otimalidade (24 áreas) +
      qualidade preditiva (bate best-member/EQW); gates verdes.
- [ ] **M5** `run_reduce` + `prevcarga` em R; `test_reduce_split_parity` + `test_prevcarga_*` verdes
      (R-vs-R); smoke sem Python no PATH.
- [ ] **M6** Python fora do caminho empacotado; bundle builda sem venv Python; **write-through no
      `ESTADO-ATUAL.md`**; `run_all_gates` + `jobs/tests` verdes.
- [ ] **renv:** nenhuma dep de produção nova; `testthat` adicionado (dev-only); `renv::status()` limpo.
- [ ] **Disciplina:** implementado em branch curta → PR (CLAUDE.md §versionamento); nada solto na raiz;
      `sandbox/` limpo do que era desta tarefa.
- [ ] **Verify:** `/verify-milestone M-combina` (adaptado) `ready:true` · 0 blockers, antes do implement.

## Relacionados
- [[combina-r-only-migration]] — a decisão de runtime R-only (memória de sessão).
- [[combina-vazamento-horizonte]] — o leak corrigido em voo no M1.
- [[empacotamento-cli-multiplataforma]] — M6 fecha o gap #1 (venv Python) desse roteiro.
- `docs/roadmap/PLANO-MODELO-FECHADO.md §6.2` — a CLI `prevcarga` atual (Python) que o M5 reescreve.
- `scripts/reconcilia_baseline.R` — já em R; a verdade fresca vem de `prevcarga_read_actual` (**criado no M3.3**, canônico `load_mwh` fail-high — **não** a `read_actual_all` alias-tolerante do oráculo); só a mecânica `ts_r30`/`mean` é reusada.

## Fonte do padrão
Estrutura adaptada de `specforge-features/.vault/lifecycle/milestones/MP.12-*.md` + os workflows
`verify-milestone.js`/`implement-milestone.js` (contrato: `ready=true` sse DoD + todo ticket com
Steps+AC; severidades blocking/decision/material/polish; gaps OWNED/DEFERRED/UNOWNED; verify-log
em `docs/roadmap/verify-log/M-combina.jsonl`).
