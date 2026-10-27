# Vazamento de horizonte na combina (referência) — o que é e como o clean-room corrige

> **Tier:** referência (`docs/reference/`, vivo). **Decisão associada:** [ADR-0014](../adr/0014-combina-leak-horizonte-solver.md).
> **Escopo:** este doc descreve um **bug conhecido do sistema de referência** `/opt/ons-source/prevcargadessem3`,
> que fica **INTOCADO** (baseline congelada do A/B). A correção vive **só** no clean-room (`combination.R`, M4).

## O bug, em uma frase
A combina treina os pesos do ensemble agrupando por **mês da origem** sem recuar o horizonte de previsão,
então origens do fim de um mês usam, no ajuste dos pesos, `y_true` de alvos que **caem no mês seguinte** —
valores que, na operação real daquele dia, **ainda não existiam**. É lookahead no nível do peso.

## Onde está no código de referência
- `scripts/combina_baseline.py:190` — `ym = forecast_origin[:7]` → chave de agrupamento = **mês da ORIGEM**.
- `scripts/combina_baseline.py:271-272` — `tr = ymv < m` → treina os pesos com todas as origens de meses
  **anteriores** ao mês-alvo `m`, **sem subtrair o horizonte**.
- `scripts/combina_baseline.py:389-398` — o gate **G2** (anti-vazamento) perturba `y_true` por
  **mês-de-origem** — a mesma definição que vaza — logo **não enxerga o furo**: é **falso-verde**.

## Por que é um vazamento
Uma origem `O` no fim do mês `m−1` produz previsões até `d+8`, cujos `target_datetime` caem dentro de `m`.
Ao treinar os pesos aplicados às origens do **início de `m`** com o critério `mês-de-origem < m`, entram no
ajuste os `y_true` desses alvos de `m` observados por origens de `m−1`. Em produção honesta, no dia `O` esse
`y_true` futuro é `NA` (cai no filtro) e o problema **auto-cura**; o furo morde **só no backtest** (onde o
`data/` já contém o futuro). Efeito: **infla** a acurácia aparente da combina-A vs EQW e vs membros individuais
em D+1..D+8.

## Contraste: a reconcilia faz CERTO
`scripts/reconcilia_baseline.R:98-106` monta o perfil k̂ com `seq(lo, O−1)` — janela estritamente **anterior
à origem**. É exatamente o corte honesto que a combina deveria ter. Serve de molde para o rebuild.

## Como o clean-room corrige (M4, `scripts/lib/combination.R`)
1. **Corte de treino de pesos por data-alvo:** `origem − 1 − horizonte` **por membro** (lê `horizon_days`
   via `topology.R`), não `mês-de-origem < m`. Mesmo contrato honesto do resto do sistema (ADR-0006).
2. **G2 reescrito:** perturba `y_true` por **`target_datetime`** em **horizonte cheio** (não por mês-de-origem,
   não `max_dplus=0`), com os dois braços falsificáveis (FORBIDDEN `|Δ|≤1e-5` / CONTROL `|Δ|≫1e-5`), no molde
   do `honesty_gate.R` (ADR-0008).

## Implicação para o A/B
Como a referência **mantém** o leak e o clean-room **corrige**, a saída da combina **vai divergir de
propósito**. Portanto o A/B da combina **não** é Δ=0: é por **propriedade** (simplex w≥0/Σw=1 + otimalidade
do objetivo) + **qualidade** (WMAPE ≤ best-member/EQW no holdout). As partes determinísticas (inferência
por-modelo, reconcile, exports) seguem com A/B Δ=0 (≤1e-5 mesmo-hardware). Os números de backtest publicados
**re-baselinam** na régua honesta do M7; o shift esperado é comunicado aos stakeholders do DESSEM já no M4.

## Relacionados
- [ADR-0014](../adr/0014-combina-leak-horizonte-solver.md) · [ADR-0006](../adr/0006-backtest-honesto-corte-treino.md)
  (corte honesto) · [ADR-0013](../adr/0013-combina-convexa-metodo.md) (método da combina) ·
  [ADR-0015](../adr/0015-reconciliacao-bottomup-khat.md) (o corte feito certo) ·
  [ADR-0008](../adr/0008-honestidade-determinismo-falsificavel.md) (honesty-gate).
