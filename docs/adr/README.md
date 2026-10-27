# ADRs — Architecture Decision Records

Registros de decisão arquitetural do PrevCarga (clean-room modular R-only). Cada ADR é **imutável** uma
vez `Aceito`: se a decisão mudar, cria-se um novo ADR que **supersede** o anterior (nunca se reescreve a
história). Formato leve: Contexto → Decisão → Consequências → Alternativas consideradas → Evidência.

Muitos ADRs documentam decisões **já embutidas no código de referência** (`/opt/ons-source/prevcargadessem3`),
com `file:line` — a reconstrução precisa honrá-las ou mudá-las conscientemente.

**Status:** `Proposto` · `Aceito` · `Supersedido por ADR-XXXX` · `Obsoleto`.
**Tipo:** `carry-over` (invariante a preservar) · `changed-in-rebuild` (mudança consciente no rebuild) ·
`open-question` (decisão ainda pendente).

| # | Título | Tipo | Status |
|---|---|---|---|
| [0001](0001-topologia-sin-map-reduce.md) | Topologia da árvore SIN + decomposição MAP/REDUCE | carry-over | Aceito |
| [0002](0002-grade-semihoraria-schema-long.md) | Grade semi-horária (48 slots) + schema LONG de 8 colunas + re-ancoragem em O | carry-over | Aceito |
| [0003](0003-tempo-naive-brt-runtime-env.md) | Tempo naive-BRT (relabel tz=UTC) + contrato de ENV (TZ=UTC/locale C) | carry-over | Aceito |
| [0004](0004-store-parquet-hive.md) | Store canônico parquet Hive `area_code=/year=` | carry-over | Aceito |
| [0005](0005-contrato-entrada-fail-por-origem.md) | Contrato de entrada por-modelo (fail-por-origem) + validação na fronteira | carry-over | Aceito |
| [0006](0006-backtest-honesto-corte-treino.md) | Backtest honesto (harness só trunca tempo) + corte treino origem−1−horizonte | carry-over | Aceito |
| [0007](0007-contrato-corte-intradiario.md) | Contrato de corte intradiário (vem do dado, instante, issue_time, na chave) | carry-over | Aceito |
| [0008](0008-honestidade-determinismo-falsificavel.md) | Honestidade + determinismo falsificável (FORBIDDEN Δ=0 / CONTROL Δ≠0) | carry-over | Aceito |
| [0009](0009-arquitetura-modular-modelo.md) | Arquitetura modular (módulo = função R; unidade = área×origem×modo; FE por-plugin; job.json) | carry-over | Aceito |
| [0010](0010-contrato-features-registry.md) | Contrato em wire-time + dois tiers (A contract-verified: RF/LGBM/HW/DESSEM · B vendored-lazy: vazio) | carry-over | Aceito |
| [0011](0011-ciclo-artefato-registry.md) | Ciclo de artefato (saveRDS auto-descritivo + latest/v_* + split por classe, classe no manifest) | carry-over | Aceito |
| [0012](0012-dessem-vendor-wrap-nao-edita.md) | Motor DESSEM (código NOSSO, não vendor); ±1h = semântica de domínio → ADR-0019 | changed-in-rebuild | Aceito |
| [0013](0013-combina-convexa-metodo.md) | Combina: média convexa + intercepto, pesos mês×lead×corte, freeze, EQW piso | carry-over | Aceito |
| [0014](0014-combina-leak-horizonte-solver.md) | Combina: vazamento de horizonte + solver sem paridade — corrigir no rebuild | changed-in-rebuild | Aceito |
| [0015](0015-reconciliacao-bottomup-khat.md) | Reconciliação Bottom-Up × k̂ multiplicativo; SIN=Σ; perda por identidade | carry-over | Aceito |
| [0016](0016-orquestracao-io-nuvem.md) | Orquestração/IO de nuvem (referência histórica — produto é local-only; S3 só nas pontas) | changed-in-rebuild | Aceito |
| [0017](0017-empacotamento-bundle-s3-entrega.md) | Produto fechado: bundle R no S3 + bootstrap; entrega byte-copy; verificar por tolerância | carry-over | Aceito |
| [0018](0018-runtime-r-only-cleanroom.md) | Runtime R-only no clean-room (colapsa R+Python; grade numa função R única) | changed-in-rebuild | Aceito |
| [0019](0019-alinhamento-temp-carga-invariante.md) | Alinhamento temp↔carga = invariante de dado (alocação fim-de-intervalo); transforms compartilhados; LGBM sob spike | changed-in-rebuild | Aceito |
| [0020](0020-taxonomia-exit-code-cli.md) | Taxonomia de exit-code da CLI por classe (0/2/3/4/5/6 + 7=runtime); contrato transversal M2–M8 | changed-in-rebuild | Aceito |
| [0021](0021-run-yaml-contrato-modelo.md) | Config por-modelo = `run.yaml` enxuto (sem orquestração AWS), override por CLI; refina ADR-0009 | changed-in-rebuild | Aceito |
| [0022](0022-runtime-guard-dispatch.md) | Contrato de dispatch do runtime-guard (enforce-first + isenção doctor/status/-h); transversal M2–M8 | changed-in-rebuild | Aceito |

## Decisões em aberto (aguardam Gabriel)
Cada item vive na seção "Decisão pendente" do ADR correspondente.

**Resolvidas em 2026-10-10** (viraram seção "Decisão (resolvida)" no ADR):
- ✅ **[ADR-0014](0014-combina-leak-horizonte-solver.md)** — leak corrigido **só no greenfield**; referência intocada; vazamento documentado em [`docs/reference/VAZAMENTO-HORIZONTE-COMBINA.md`](../reference/VAZAMENTO-HORIZONTE-COMBINA.md); A/B da combina por propriedade/qualidade.
- ✅ **[ADR-0004](0004-store-parquet-hive.md)/[ADR-0005](0005-contrato-entrada-fail-por-origem.md)** — schema canônico, **falhar alto** na leitura (sem NA silencioso); aliases resolvidos 1× no data-prep.
- ✅ **[ADR-0017](0017-empacotamento-bundle-s3-entrega.md)** (+ nota no [ADR-0008](0008-honestidade-determinismo-falsificavel.md)) — dois trilhos: **≤1e-5 mesmo-hardware**, **≤0,1 MW cross-machine** (aperta o 1 MW+0,1% antigo). 0,1 MW é meta aferida no A/B, afrouxável por-alvo.
- ✅ **[ADR-0010](0010-contrato-features-registry.md)** — **dois tiers**; na prática **todos contract-verified**: Tier A = RF/LGBM/HW/**DESSEM** (declaram `requires`/`provides` no register) · Tier B vendored-lazy = **vazio/reservado** (só p/ externo de terceiro, que não temos). DESSEM é código nosso → Tier A.
- ✅ **[ADR-0016](0016-orquestracao-io-nuvem.md)** — vira **referência histórica (não-alvo)**: produto = pacote R local-only + casca CLI, **S3-agnóstico**; S3 só nas pontas (bundle no bootstrap; staging num script de orquestração Docker externo). paws/aws CLI **não** entram no pacote. Fan-out Batch fora de escopo.
- ✅ **[ADR-0013](0013-combina-convexa-metodo.md)** — identidade de membro por **manifesto declarativo** (não glob+regex) · pesos no eixo-corte = **re-fitar por corte** (não interpolar), corte honesto `origem−1−H` + piso EQW.
- ✅ **[ADR-0011](0011-ciclo-artefato-registry.md)** — **duas classes** (origem-agnóstico RF/LGBM vs origem-escopado DESSEM/HW), mas a classe é um campo `protocol` **declarado no manifest** (fonte única; elimina a lista hardcoded `.refit_per_origin`).
- ✅ **[ADR-0012](0012-dessem-vendor-wrap-nao-edita.md)** (premissa corrigida) — motor DESSEM é **código nosso**, não vendor; o ±1h é **semântica de domínio** (correto, fica), re-expresso via transforms do **[ADR-0019](0019-alinhamento-temp-carga-invariante.md)**, output-preserving (A/B Δ=0).
- ✅ **[ADR-0019](0019-alinhamento-temp-carga-invariante.md)** (novo) — alinhamento temp↔carga é **invariante de dado** (alocação fim-de-intervalo); transforms `to_model_frame`/`to_display_frame` compartilhados em `scripts/lib`; **LGBM sob spike no M5** antes de corrigir (A/B diverge só se o spike confirmar o desalinhamento); RF/HW inalterados.
- ✅ **[ADR-0003](0003-tempo-naive-brt-runtime-env.md)** (subparte do clima) — **preservar a emissão no instante cheio** (opção B): `issue_time` = `timestamp`, suporta 4 rodadas/dia (00/06/12/18Z), alias `din_origemprevisaoutc` **não** reatribuído. Regra de dia-de-origem + horizonte UTC-safe → M5. Contrato em [`docs/reference/DECK-CANONICO.md`](../reference/DECK-CANONICO.md).

**Novas (2026-10-11, surgidas no `verify-milestone` do M1 — a lente de ADR fez o seu trabalho):**
- ✅ **[ADR-0020](0020-taxonomia-exit-code-cli.md)** — taxonomia de exit-code da CLI por classe (`0/2/3/4/5/6` + **`7`=runtime**); contrato transversal que M2–M8 herdam, número de classes imutável. Fecha a referência circular M1.1↔M1.4.
- ✅ **[ADR-0021](0021-run-yaml-contrato-modelo.md)** — config por-modelo = **`run.yaml` enxuto** (só modelagem: horizon/history/protocol/seed/params), override por flag de CLI; **não** porta a orquestração AWS do `job.json` (resources/r_deps/entrypoint → ADR-0016/0018/M8). Refina a realização "job.json" do ADR-0009.

- ✅ **[ADR-0022](0022-runtime-guard-dispatch.md)** — contrato de dispatch do runtime-guard: `enforce()` como 1ª ação de todo comando **guardado** (asserir ANTES de qualquer número) + **lista de isenção** fixa (`doctor`/`status`/`-h` rodam `check()` não-fatal, reportam em vez de abortar); ordem falsificável (env sujo + verbo guardado → stdout vazio + exit 7). Transversal M2–M8, par do ADR-0020 (que cobria só o exit-code).

**Nenhuma decisão em aberto.** As **22 ADRs** estão resolvidas (2026-10-10/11). O contrato do deck de entrada vive em [`docs/reference/DECK-CANONICO.md`](../reference/DECK-CANONICO.md).

> Fontes: decisões tomadas nesta frente (2026-10-10) + garimpo do código de referência (workflow `adr-mining`,
> run `wf_1907e4d2-1df`), refletidas em `docs/_archive/roadmap/PLANO.md`, `docs/_archive/roadmap/CLI-MODULAR-OBJETIVO.md` e
> `docs/ESTADO-ATUAL.md`.
