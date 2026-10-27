# ADR-0015 — Reconciliação hierárquica = Bottom-Up × perfil sazonal MULTIPLICATIVO k̂ (hora×mês×dow, janela 730d, corte < origem); SIN=Σ subsistemas; L_true por identidade; interseção não-muda

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0001](0001-topologia-sin-map-reduce.md), [ADR-0006](0006-backtest-honesto-corte-treino.md), [ADR-0013](0013-combina-convexa-metodo.md), [ADR-0014](0014-combina-leak-horizonte-solver.md)

## Contexto
A coerência regiões→subsistema→SIN precisa de um método com alternativas clássicas fortes (MinT, top-down), e a honestidade do corte aqui é o contraste exato com o VAZAMENTO da combina.

## Decisão
`subsistema_hat = BU_pred × k̂`, onde `BU_pred = Σ` previsões OOS das regiões (combina-A) e `k̂ = média de SS_true/BU_true` — correção de perda **MULTIPLICATIVA e sazonal**, não MinT/top-down/aditiva. O perfil k̂ é por hora×mês×dow com janela **MÓVEL de 730d** (não expansível, porque a perda drifta) e corte `seq(lo, O−1)` estritamente ANTES da origem (honestidade feita CERTO). O SIN não é previsto: é a **soma dos 4 subsistemas** (coerente por construção); a perda `L_true = SS_true − BU_true` é resíduo contábil por identidade, nunca série medida. A soma bottom-up só usa instantes com cobertura COMUM de todas as regiões (interseção), e a perda de dados é SEMPRE reportada, nunca silenciosa.

## Consequências
É o corte honesto que o rebuild deve carregar PARA a combina ([ADR-0014](0014-combina-leak-horizonte-solver.md)). A escolha multiplicativa assume que a perda escala com a carga — plausível mas revisável. Trocar chaves/janela reabre o gap ao oráculo. A identidade `L=SS−ΣReg` fecha Δ=0 no y_pred das regiões. O drop no d_plus=0 parcial por cortes desalinhados conecta com a interpolação de corte da combina.

## Alternativas consideradas
MinT (reconciliação ótima de Wickramasuriya); OLS reconciliation; top-down (prever SIN e desagregar); perda ADITIVA; janela expansível; chaves hora-only ou hora×dow; prever SIN direto; medir perda de transmissão de fonte independente; soma parcial reescalada ou imputação.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/reconcilia_baseline.R:5-16
- scripts/reconcilia_baseline.R:59
- scripts/reconcilia_baseline.R:98-106
- scripts/reconcilia_baseline.R:86-95
- scripts/reconcilia_baseline.R:244-254
