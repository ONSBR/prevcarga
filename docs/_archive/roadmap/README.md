# Roadmap — índice de milestones

> **Tier:** `docs/roadmap/` (plano ativo; cada doc some ao concluir → apaga ou `docs/_archive/`). Índice vivo.
> **Fonte de verdade durável** do estado do sistema é `docs/ESTADO-ATUAL.md`; aqui mora o **plano de trabalho**.

A reconstrução é um programa de **8 milestones (M1…M8)**, ordenados por risco+dependência. O racional do
sequenciamento vive em [`PLANO.md`](./PLANO.md); o alvo da CLI em [`CLI-MODULAR-OBJETIVO.md`](./CLI-MODULAR-OBJETIVO.md);
a referência técnica da combina em [`COMBINA-TECNICO-M4.md`](./COMBINA-TECNICO-M4.md).

## Ciclo de vida de cada milestone
```
PLANO.md (spec resumida: Entrega/Racional/Gates/Depende)
   └─▶ EXPANDIR em M<n>-<slug>.md   (prosa + tickets M<n>.k com Steps+AC + DoD; formato specforge)
        └─▶ verify-milestone  →  ready:true   (gate read-only; log em verify-log/)
             └─▶ adicionar CHECKPOINTS['M<n>'] em .claude/workflows/implement-milestone.js
                  └─▶ implement-milestone   (implementa + gates + commit em branch → PR)
```
Disciplina (decidida 2026-10-10): **solo/serializado** — um milestone por vez; `verify` ANTES do `implement`.

## Estado dos milestones

| # | Milestone | Doc expandido | Estado | Depende de |
|---|---|---|---|---|
| **M1** | Fundação: fachada + runtime-guard + topologia + proveniência | [`M1-FUNDACAO.md`](./M1-FUNDACAO.md) | ✅ **VERIFY-READY** (0 blockers · 6/6 unânime; `CHECKPOINTS['M1']` wirado) — aguarda revisão → `implement` | — |
| **M2** | Datas honestas (3 cortes) + validação + `validate-deck` (gate-0) | [`M2-DATAS.md`](./M2-DATAS.md) | ✅ **VERIFY-READY** (0 blockers · 6/6 unânime; `CHECKPOINTS['M2']` wirado) — aguarda revisão do Gabriel (3 decisões: opção-a/b do corte-por-protocolo vs ADR-0006, candidatos a ADR) → `implement` | M1 |
| **M3** | Avaliação fonte-única + honesty-gate + `score` + `check-coherence` | [`M3-AVALIACAO.md`](./M3-AVALIACAO.md) | ✅ **VERIFY-READY** (0 blockers · 6/6 unânime; `CHECKPOINTS['M3']` wirado) — aguarda revisão do Gabriel (candidatos a ADR: contrato-de-métrica + faixa k̂, molde honesty in-process, `coherence→8`/completude-estrutural; + correções "repo vence": read_actual canônico, identidade multiplicativa) → `implement` | M1, M2 |
| **M4** | Módulo combination (combina dissolvida, leak corrigido em voo) | [`M4-COMBINA.md`](./M4-COMBINA.md) | ✅ **VERIFY-READY** (6/6 unânime · 0 landmines/controles bloqueantes por 6 rounds R8–R13; `CHECKPOINTS['M4']` wirado + `_COMBINA_CP` aposentado) — aguarda revisão do Gabriel (ratificações de ADR: solver FISTA-R-base vs "QP em R", ε do A/B, corte per-mês, eixo=mês-de-origem vs texto 0013; candidatos a ADR: escopo-G2/layout-run/model_versions/dono-protocol; correções "repo/frozen vence": descoberta run-mode-agnóstica, member_set_hash=def-M1.3, gatear-em-y_pred) → `implement` | M1, M2, M3 |
| **M5** | Training/Inference + FE-fachada + `train-models`/`infer-model` + registry | [`M5-TRAINING.md`](./M5-TRAINING.md) | ✅ **VERIFY-READY** (6/6 unânime · 0 landmines/controles bloqueantes, 12 rounds R1–R12 [0 blockers R10/R12]; `CHECKPOINTS['M5']` wirado) — aguarda revisão do Gabriel (**SUPERSEDER ADR-0006** [daily vs retrain_harness] = decisão-chave; ratificações: lifecycle-registry+variant-layout, FE-fachada vs ADR-0009, model_spec(overrides), infer-dessem verbo, exit-codes, freshness-clima; correções repo-vence: dispatch-fino/config-driven/content-addressed/build_long_output→M6/reach-control→M6) → `implement` | M1, M2 |
| **M6** | Forecast/Export/Reporting full-R + `build_long_output` + standalones de composição | [`M6-FORECAST.md`](./M6-FORECAST.md) | ✅ **VERIFY-READY** (6/6 unânime · 0 blockers · 0 false-claims, 5 rounds R1–R5 [0 blockers R5]; `CHECKPOINTS['M6']` wirado) — aguarda revisão do Gabriel (**RATIFICAÇÃO freshness-clima** [instante vs midnight-UTC, ADR-0007] = decisão-chave, bloqueia só o reach-control clima-previsto M6.6(2b); ratificações: motor R→PDF, emenda ADR-0002 [único CONVERSOR + tipo forecast_origin], campo run.yaml ancoragem-em-O, host do report_A→M7, k̂ clamp-vs-check, chave `<ss>` do perfil k̂; correções repo-vence: topologia config-driven no reconcile+forecast, SIN=Σ count-agnóstico, build_long dono só do model-native, infer fica model-native [M5.3 intacto], reach-control só k̂/reconcile, "flip"=acreção) → `implement` | M3, M4, M5 |
| **M7** | Orquestração + cache content-addressed + `backtest` unificado | [`M7-ORQUESTRACAO.md`](./M7-ORQUESTRACAO.md) | ✅ **VERIFY-READY** (0 blockers, 6 rounds R1–R6 [0 no R6]; `CHECKPOINTS['M7']` wirado) — aguarda revisão do Gabriel (**chave do cache class-aware + acessor `cut_invariant` do `model_spec`/M1 [PRÉ-REQ HARD, key-time]**; GC/namespace do store-de-backtest [sobrevive ao gc/M5.4]; régua-de-FORMA regime/ramp/timing [hot-potato M3→M4→M7]; ε do A/B [holdout real, 0014]; skill-score baseline; política-de-parcial fail-fast; correções repo-vence: ledger/cache LOCAIS, eixo membro×folha×origem, combina-full-honesta-no-y_pred, retrain-não-contamina-latest, `--corte` só backtest) → `implement` | M2, M3, M4, M5, M6 |
| **M8** | Bundle R-only (S3) + install bootstrap + `verificar` + `morning` + A/B (gate de corte) + remoção final do Python | [`M8-BUNDLE.md`](./M8-BUNDLE.md) | ✅ **VERIFY-READY** (R22: ready:true · 0 blockers · 6/6; 22 rounds; `CHECKPOINTS['M8']` wirado) — aguarda revisão do Gabriel (D1–D32; chaves: **D2** gate de frota WSL2/GPO, **D14** superseder ADR-0006 [bloqueia o corte], **D15** re-fit diário, **D12/D13** estado e artefatos no host, **D23** DESSEM no Δ; ADRs supersedentes ⟪S⟫ antes do ticket que governam) + 22 materiais residuais anotados no banner → `implement` (após M1–M7 implementados) | M4, M5, M6, M7 |

> **Estado** = `só em PLANO.md` (spec resumida, não ticketizada) · `expandido` (doc M<n> com Steps+AC+DoD) ·
> `verify ready:true` · `implementado`. Atualizar esta coluna **na hora** a cada transição (write-through).

## Cadeia de dependência (de `PLANO.md`)
```
M1 ─▶ M2 ─▶ M3 ─▶ M4 ─┐
       └──────▶ M5 ───┤   (M4 antes de M5: solo/serializado)
                      ▼
                     M6 ─▶ M7 ─▶ M8
```

## Convenções
- **Nome do doc:** `M<n>-<SLUG>.md` (ex. `M1-FUNDACAO.md`). Tickets são sub-seções `### M<n>.k — …`.
- **verify-log:** [`verify-log/`](./verify-log/) — um jsonl por run (`{milestone,runId,date,ready,consensus,counts,findings[]}`).
- **Workflows:** `.claude/workflows/{verify-milestone,implement-milestone}.js` (adaptados do specforge; ver
  [[padrao-milestone-specforge]] na memória). O `verify` é read-only e checa conformidade com os **ADRs**
  (`docs/adr/`) + invariantes do `ESTADO-ATUAL`; sinaliza mecanismo arquitetural novo sem ADR como `decision`
  (candidato a ADR).
- **CHECKPOINTS:** `.claude/workflows/implement-milestone.js` (`CHECKPOINTS` map) define onde rodam os gates
  completos + o A/B. Cada milestone expandido adiciona sua entrada antes do `implement`.

## Relacionados
- [`PLANO.md`](./PLANO.md) · [`CLI-MODULAR-OBJETIVO.md`](./CLI-MODULAR-OBJETIVO.md) · [`COMBINA-TECNICO-M4.md`](./COMBINA-TECNICO-M4.md)
- `../adr/README.md` (22/22 ADRs resolvidas) · `../ESTADO-ATUAL.md` (estado durável) · `../reference/DECK-CANONICO.md` (deck de entrada)
- [`_canvas/canvas.html`](./_canvas/canvas.html) — **canvas da topologia interna por milestone** (verbos → pipeline [treino ⫽ diário + motores] → transversais → config/fachada/gates/ferramentas, com o delta M1…M8 em destaque; self-contained, abre offline; estado na URL `#m=M6&mode=delta&pin=mod%3Aforecast`). Derivado dos docs M*.md: dado em `_canvas/data/<M>.json` + `_resolve.json`; **adicionar um milestone** = `/canvas-milestone M8` (roda `.claude/workflows/milestone-canvas.js` com `args:"M8"` → extrai só o M8 e roda `node _canvas/build.cjs`); sem args reextrai tudo; só re-renderizar = `node _canvas/build.cjs`. Artefato derivado, não é fonte de verdade.
