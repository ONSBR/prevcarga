# PLANO — reconstrução modular R-only (M1…M8)

> **Status:** ATIVO · 2026-10-10 · clean-room `/opt/ons-source/prevcarga`.
> **Destino:** `docs/roadmap/CLI-MODULAR-OBJETIVO.md`. **Referência técnica da combina:** `COMBINA-TECNICO-M4.md`.
> **Origem:** re-sequenciamento por workflow (3 planos independentes + síntese, run `wf_4d2ca349-18b`),
> colapsando as duas trilhas antigas (combina M1–M6 + modular M7–M13) numa só, ordenada por **risco +
> dependência**. **13 milestones → 8.**

## Regra organizadora
*Nenhum código numérico migra para R in-process antes que o teste falsificável do invariante que ele pode
violar esteja VERDE.* Os quatro "matadores" são: colisão de datas (vazamento de horizonte), perda de
honestidade ao sair do isolamento de processo (G5), determinismo não-asserido in-process, topologia
divergente. Cada um vira um gate falsificável (braço **FORBIDDEN Δ=0** + braço **CONTROL Δ≠0**) antes do
volume (as ~192 jobs, os 5 plugins de FE, o backtest 5×24×N).

## Estratégia de execução (decidida com o Gabriel, 2026-10-10)
- **Clean-room:** construir tudo limpo aqui; `prevcargadessem3` fica congelado como **oráculo do A/B**.
- **A/B é o gate de corte (M8), não um flip de produção no meio.** Durante o desenvolvimento cada módulo é
  comparado contra a referência; o A/B completo aceita no fim. Escopo: **Δ=0** no determinístico
  (inferência por-modelo, reconcile, exports — mesmos engines R portados) e **propriedade/qualidade** na
  combina (leak corrigido ⇒ diverge; **sem paridade de solver**). O `prevcarga verificar` (sha256 + TOL) é
  o motor do lado determinístico.
- **R-only por ACREÇÃO, não por flip:** cada milestone remove sua peça Python; o M8 remove a última.
- **Solo → serializado:** uma frente. **M4 (combina) antes de M5 (training)** — de-risca o leak e prova
  honestidade/determinismo in-process na fatia de maior risco-por-byte antes de tocar o volume de treino.
  (Com duas frentes, M4∥M5 seriam paralelos — ambos gateados só por M1/M2.)
- **Disciplina:** `verify-milestone` (adaptado) roda ANTES do `implement` de cada milestone.

---

## Sequência M1…M8

### M1 — Fundação: fachada + runtime-guard + topologia + contrato de proveniência
**Entrega:** `scripts/prevcarga` vira Rscript+docopt como **dispatcher fino**. `scripts/lib/runtime.R`
roda PRIMEIRO em todo comando (seta+assere TZ=UTC/LC=C.UTF-8/OMP/OPENBLAS/MKL/VECLIB=1; aborta sob
threads>1 ou TZ≠UTC). `scripts/lib/topology.R` = fonte única (24 folhas, 4 subsistemas+perdas, 8 membros,
horizonte/janela lidos do `job.json`), subsumindo SUBSYS/LEAVES/MEMBERS/árvore. `scripts/lib/provenance.R`
= contrato de `version` content-addressed + run-manifest + freeze `saveRDS` binário (IEEE-754, não JSON).
Comandos vivos: `status`, `doctor`, `models`, `areas`, `describe`.
**Racional:** o determinismo some no instante em que a 1ª linha numérica roda in-process → asserir ANTES.
A fachada escrita UMA vez mata o big-bang + a re-modularização das duas trilhas antigas. Topologia e
proveniência entram como **contratos** para combina/training não nascerem com local descartável que uma
fase posterior re-centraliza (mata 4 laços port-depois-refatora).
**Gates/AC:** grep prova fonte única de topologia · `doctor` aborta sob threads>1 e TZ≠UTC · round-trip
binário `saveRDS` Δ=0 · todo comando legado espelha a saída da referência.
**Depende de:** —

### M2 — Datas honestas (3 cortes) + validação + `validate-deck` (gate-0)
**Entrega:** `scripts/lib/dates.R` deriva os 3 cortes do único `--as-of` (forecast-origin; corte-de-treino
`origem−1−horizonte` **por modelo**; corte intradiário do `manifest.json`). `scripts/lib/validation.R`
model-agnóstico (schema/formato, completude **DST-aware**, sanidade física carga>0/faixa/outlier/flatline/
dedup, honestidade do clima `issue_time≤O`, janela mínima por membro). Comando `validate-deck` com veredito
estruturado, `--format json` e exit-codes por classe; encadeável como **gate-0** (`--skip-validate`).
**Racional:** colisão de datas é o risco #1; o falsificável precisa estar verde antes de combina/training.
`validate-deck` sobe cedo (barato, só depende da fundação, protege compute, falsifica a camada de datas).
**Gates/AC:** `--as-of` uniforme **não** iguala corte entre RF(C−10) e LGBM(C−2) · reprova deck que
RF/LGBM comeriam com fill-0 · sem falso-positivo em DST · encadeável como gate-0.
**Depende de:** M1

### M3 — Avaliação fonte-única + honesty-gate + `score` + `check-coherence`
**Entrega:** `scripts/lib/evaluation.R` = **única** definição de WMAPE/MAPE/MAE/RMSE/BIAS/peak/ramp/timing
+ skill-score vs baseline (persistência/sazonal-ingênuo). `scripts/lib/honesty_gate.R` falsificável
reusável. Comando `score` (pontua previsão publicada vs `y_true` fresco — review D+1). Comando
`check-coherence` (SIN=Σsubsistemas; subsistema=Σregiões·k̂ — identidade multiplicativa, k̂ em faixa física; perda = resíduo multiplicativo, ADR-0015).
**Racional:** honestidade (que o isolamento de processo garantia) vira gate falsificável ANTES de a combina
migrar. WMAPE em fonte única precede score/backtest/report. `score` cedo torna o leak **mensurável** como
evidência de regressão antes do fix (M4).
**Gates/AC:** uma única def de WMAPE (grep) · `score`=`backtest` no mesmo input · honesty-gate tem braço
CONTROL Δ≠0 (prova que o FORBIDDEN Δ=0 não é vazio) · `check-coherence` falha se SIN≠Σ.
**Depende de:** M1, M2

### M4 — Módulo combination (combina dissolvida, leak corrigido em voo)
**Entrega:** `scripts/lib/combination.R` escrito **direto como módulo** (Duchi simplex/FISTA/produce/eqw +
IO/painel/descoberta-lendo-`infer` + freeze `saveRDS`), consumindo topology(M1)/dates(M2, corte por
data-alvo `origem−horizonte` **NÃO** `ym<m`)/evaluation(M3)/freeze(M1). É o **1º módulo** a tornar verde o
honesty-gate(M3) e a assertiva de determinismo(M1) in-process. Comando `combine` standalone.
**Racional:** colapsa M1–M4 da trilha combina antiga, sem port-depois-refatora. Melhor fatia-prova: único
Python diário, numericamente contida (numpy→R-base, sem scipy, zero dep nova), portadora do leak #1, baixo
volume (um reduce). É onde se perde o isolamento de processo → onde honestidade vira invariante de módulo.
**Gates/AC:** simplex w≥−1e-9 ∧ |Σw−1|<1e-9 · otimalidade ‖grad projetado‖<1e-6 · determinismo max|Δw|<1e-12
· split-parity R-vs-R `apply(load(train()))==full` Δ=0 + controle-com-dente · **G2 falsificável em horizonte
cheio (~9)** · qualidade WMAPE≤best-member+ε e ≤EQW por (área,d_plus) · A/B qualidade vs referência.
**Depende de:** M1, M2, M3 · **SEM paridade de solver** (oráculo Python contaminado).

### M5 — Training/Inference fachadas + FE-fachada + `train-models`/`infer-model` + registry
**Entrega:** fachada de FE padronizando as **5 assinaturas divergentes** dos plugins (trabalho real) +
fachadas R de training/inference. `train-models` com `--model/--area/--as-of` derivando corte honesto **por
modelo** (M2), subset **desbloqueado** (remove `_require_sin_scope`), default=todos. `infer-model` com
params **lidos do artefato**. `version=hash` content-addressed + run-manifest (M1). Comandos `registry`
(ls/show/promote-atômico/rollback/pin/gc).
**Racional:** 1ª migração de VOLUME + capacidade nova (treinar um modelo/área — hoje impossível). Só pousa
depois de datas(M2)+honestidade/determinismo(M1/M3) asseridos (training é onde `--as-of` uniforme vazaria).
Fecha o treino não-idempotente (version=timestamp, latest sem lock). **Serializado após M4** (decisão solo).
**Gates/AC:** treino de subset roda · 2 treinos idênticos → **mesmo id** · `infer-model` recusa re-especificar
params (lê do artefato) · `gc` poda `v_*`, promote atômico · corte por-modelo asserido (reusa falsificável M2).
**Depende de:** M1, M2

### M6 — Forecast/Export/Reporting full-R + standalone de composição
**Entrega:** `scripts/lib/exports.R` (extrai o `_deliver` inline Python); `prever`→`forecast` (+alias+aviso
de depreciação); `scripts/lib/reporting.R` motor R, **HTML E PDF** (corte/proveniência em destaque);
`forecast --report` = **diagnóstico** (curva/corte/pesos/vs-persistência, nunca acurácia). Comandos
standalone `combine`/`reconcile`/`export`/`features`.
**Racional:** completa o caminho diário em R só depois de a combina ser R (M4) — aqui o `_deliver` não tem
mais Python. Report em R sobrevive ao R-only (o `report_A.py` Plotly não) e PDF é o que o operador anexa à
submissão DESSEM. `forecast --report` não mostra acurácia (sem `y_true` no futuro).
**Gates/AC:** `forecast` A/B Δ=0 vs `prever` · report roda sem `python3` · `--report` não exibe acurácia ·
PDF arquivável gerado.
**Depende de:** M3, M4, M5

### M7 — Orquestração + cache content-addressed + `backtest` unificado
**Entrega:** `scripts/lib/orchestrate.R` (ledger modelo×área×origem, progresso 47/192, `plan`/`--dry-run`,
`--jobs`, `--resume`, `--keep-going`, resumo duro + exit≠0 em entrega parcial). Cache content-addressed
**honrando o corte honesto** (fingerprint de proveniência M1). Comando `backtest` unificado (treino→infer-
pra-trás→combina→reconcile→avaliação dia-a-dia sobre os 8 membros) + `--report` + skill-score.
**Racional:** maior volume (5×24×N) e o cache é onde a honestidade é mais tentada a furar por custo → por
último, gateado por M2 sólido + honesty-gate(M3). Fan-out sobre todos os módulos (M4–M6) → só depois de
existirem. Entrega a régua honesta de re-baselina e o backtest de 1 ano retomável.
**Gates/AC:** backtest de 1 ano retomável (`--resume`) · cache **não** viola `origem−1−horizonte`
(falsificável) · entrega parcial falha com exit≠0 · `backtest`=`score` no mesmo input (fonte única M3).
**Depende de:** M2, M3, M4, M5, M6

### M8 — Bundle R-only (S3) + install bootstrap + remoção final do Python + write-through
**Entrega:** remove o **último** Python do caminho empacotado (cada fase já removeu a sua peça). Script de
build do bundle R-only (hoje `ci/build_marmita.sh`) publica no S3; `install.sh`/`install.ps1` baixam o
**mesmo** bundle + verificam hash + extraem user-local + apontam a CLI. Verbo `morning`
(status→validate-deck→score(ontem)→forecast→report). **Write-through no `docs/ESTADO-ATUAL.md`.**
**Racional:** R-only por acreção; empacotamento é o menor risco de projeto, gateado por tudo acima estar
honesto-in-process. É também o momento do **A/B completo** contra `prevcargadessem3` para decidir o corte.
**Gates/AC:** bundle builda sem venv Python/`python-build-standalone` · grep zero `python3`/venv no caminho
empacotado · WSL2 jammy ⇒ Δ=0 / nativo-Win ⇒ TOL_REL=0,1% · `run_all_gates.R`+`jobs/tests/` verdes ·
A/B verde (Δ=0 determinístico + qualidade combina) · `ESTADO-ATUAL.md` reflete.
**Depende de:** M4, M5, M6, M7

---

## Dependências
```
M1 (fundação: fachada · runtime-guard · topologia · proveniência)
 └─▶ M2 (datas · validação · validate-deck)
      ├─▶ M3 (evaluation · honesty-gate · score · check-coherence)
      │    └─▶ M4 (combination — leak corrigido)  ──┐
      └────────▶ M5 (train/infer · registry)  ──────┤   (M4 antes de M5: solo)
                                                     ▼
                                   M6 (forecast/export/reporting full-R)
                                                     ▼
                                   M7 (orquestração · cache · backtest)
                                                     ▼
                                   M8 (bundle R-only S3 · install · A/B · write-through)
```

## Mudanças vs o plano antigo (M1–M13)
- **Dissolvida** a trilha MIGRACAO-COMBINA-R: casca big-bang (M5-antigo) → fachada fina em M1; núcleo
  combina (M1–M4-antigos) → M4 escrito direto como módulo; bundle (M6-antigo) → M8.
- **Fundação adiantada:** topologia/runtime/datas (eram M7-antigo) → M1/M2, antes de qualquer numérico R.
- **M5-antigo + M11-antigo colapsam** numa fachada escrita uma vez (sem reescrever+re-modularizar).
- **Avaliação partida:** WMAPE+honesty-gate sobem pra M3 (antes da combina); `score`/`check-coherence` cedo.
- **Proveniência/evaluation como contratos cedo** (M1/M3), comandos (registry, score) quando o consumidor nasce.
- **Honestidade+determinismo = AC transversal** (FORBIDDEN Δ=0 / CONTROL Δ≠0) verde antes de cada volume.

## Decisões resolvidas (do re-sequenciamento)
- **Payoff zero-dep tardio (bundle só em M8):** aceito — M4 já deixa o caminho **diário** R-honesto no meio
  do programa; bundle intermediário seria esforço jogado fora sobre superfície em movimento.
- **`score` cedo (M3):** sim — mede o leak como evidência de regressão antes do fix.
- **Proveniência/freeze na fundação (M1):** sim — combina (M4) é o 1º freezer; sem o contrato nasceria ad-hoc.
- **Re-baselina DESSEM:** re-baselinar na régua honesta do **backtest (M7)**; **avisar os stakeholders do
  shift esperado já em M4** (dito, não escondido).
- **verify antes do implement:** sim, rodar `verify-milestone` adaptado sobre M1–M8.

## Definition of Done (programa)
- [ ] **M1** fundação (fachada+runtime+topologia+proveniência); `doctor` aborta; round-trip saveRDS Δ=0.
- [ ] **M2** 3 cortes + validação model-agnóstica + `validate-deck` gate-0; falsificável de datas verde.
- [ ] **M3** WMAPE fonte-única + honesty-gate falsificável + `score` + `check-coherence`.
- [ ] **M4** `combination.R` (leak corrigido); invariantes+otimalidade+G2+qualidade; A/B combina ok.
- [ ] **M5** FE-fachada + `train-models`/`infer-model` (subset) + registry content-addressed.
- [ ] **M6** `forecast`/export/reporting full-R (HTML+PDF); A/B Δ=0 vs `prever`.
- [ ] **M7** orquestração+cache honesto+`backtest` unificado; retomável; cache não vaza.
- [ ] **M8** bundle R-only S3 + install bootstrap; Python fora; A/B completo; write-through ESTADO-ATUAL.
