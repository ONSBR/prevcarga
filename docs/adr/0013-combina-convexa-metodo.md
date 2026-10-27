# ADR-0013 — Ensemble (combina): média CONVEXA dos 8 membros + intercepto livre, pesos por (mês-alvo × lead × corte) com fallback EQW, freeze train/apply e y_true por JOIN FRESCO

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0014](0014-combina-leak-horizonte-solver.md), [ADR-0016](0016-orquestracao-io-nuvem.md), [ADR-0001](0001-topologia-sin-map-reduce.md)

## Contexto
A combinação precisa de pesos interpretáveis e robustos (um membro ruim satura em 0, não explode), de granularidade que capture variação por passo/mês, e de uma fonte de verdade que nunca treine em rótulo inexistente (o membro grava y_true=NA porque o futuro não existe em O).

## Decisão
A combinação é **MÉDIA CONVEXA** (w≥0, Σw=1) com intercepto escalar livre (não stacking por regressão irrestrita); 'EQW é o piso'. Pesos variam por lead (+corte) e, dentro do grupo, por mês-alvo com janela expansível e warm-start. Fallback para equal-weight no 1º mês ou treino < `n_min=24`; na aplicação, linha com membro NaN renormaliza o simplex sobre os presentes (não descarta). y_true vem de **JOIN FRESCO** do `data/load` (`COMBINA_DATA_DIR`), não do membro. Train congela pesos em `weights.json` versionado (latest texto + `v_<ts>`); apply carrega sem re-ajustar (round-trip Δ=0). A identidade de membro = (dir-modelo, EXOGENA) descoberta por glob+regex, com invariante 1-protocolo-por-membro. Membros cut-invariantes (RF/HW) são difundidos por todos os cortes do painel.

## Consequências
A convexidade é o que torna os pesos auditáveis e impede extrapolação. O freeze torna a combina reproduzível em produção. **OPEN:** a identidade por glob (vs manifesto declarativo) e a interpolação convexa de pesos no eixo-corte ('peso é coef de mistura, não dado' vs re-fit por corte) merecem decisão no rebuild. Este ADR descreve o MÉTODO correto; o vazamento de horizonte e o solver estão no [ADR-0014](0014-combina-leak-horizonte-solver.md).

## Alternativas consideradas
OLS irrestrito/ridge/lasso (pesos livres, extrapolam); Bates-Granger; BMA; pesos globais ou por hora/estação; carregar pesos do mês anterior/shrink bayesiano no fallback; manifesto declarativo de membros; nearest-cut ou re-fit por corte em vez de interpolar.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/combina_baseline.py:216-236
- scripts/combina_baseline.py:239-282
- scripts/combina_baseline.py:40-78
- scripts/combina_baseline.py:602-664
- scripts/combina_baseline.py:90-118
- scripts/combina_baseline.py:681-710

## Decisão (resolvida 2026-10-10, Gabriel)
- **(a) Identidade de membro = MANIFESTO declarativo.** No clean-room a combina não descobre membros por
  glob+regex no path; lê uma **lista explícita** (id + dir + protocolo + exógena) de um manifest versionado.
  Rename de pasta ou exógena nova ⇒ **erro explícito** (fail-loud), não quebra silenciosa. Casa com os dois
  tiers declarados do [ADR-0010](0010-contrato-features-registry.md). O invariante "1 protocolo por membro"
  passa a ser asserção sobre o manifest, não sobre o disco.
  - **Localização do manifest (clarificado 2026-10-11):** esse manifesto declarativo de membros vive na chave
    **`members:`** de `config/topology.yaml` (o mesmo config declarativo da topologia — [ADR-0001](0001-topologia-sin-map-reduce.md)),
    carregado por `topology.R::prevcarga_members()` (M1.2). Cada registro é `(sigla, mdir, EXOGENA)`; o M4 o
    enriquece com o campo `protocol` lido do `run.yaml` do modelo ([ADR-0011](0011-ciclo-artefato-registry.md)/[ADR-0021](0021-run-yaml-contrato-modelo.md)).
    `registry.R` (M1.6) e a combina (M4) leem os membros **dessa** fonte única, nunca de glob no disco.
- **(b) Pesos no eixo-corte = RE-FITAR por corte.** Nada de interpolação convexa entre cortes. Cada corte
  intradiário ganha seu **próprio ajuste convexo honesto** (corte de treino `origem−1−H` por membro, mesmo
  contrato do [ADR-0014](0014-combina-leak-horizonte-solver.md)), com **piso EQW** quando `n_corte < n_min`.
  Remove a hipótese de que a skill relativa dos membros varia suave com o corte. Custo aceito: mais ajustes
  e cortes raros caindo no piso EQW. Membros cut-invariantes (RF/HW) continuam difundidos por todos os cortes
  — a difusão é da *previsão do membro*, não do *peso* (o peso é sempre do corte alvo). Implementado no **M4**.
