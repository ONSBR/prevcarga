# ADR-0022 — Contrato de dispatch do runtime-guard in-process (enforce-first + lista de isenção)

- **Status:** Aceito
- **Data:** 2026-10-11
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** changed-in-rebuild  <!-- contrato novo do clean-room R-only; a referência pinava só no launcher/container -->
- **Relacionados:** [ADR-0008](0008-honestidade-determinismo-falsificavel.md) (invariante de determinismo), [ADR-0018](0018-runtime-r-only-cleanroom.md) (R-only in-process), [ADR-0020](0020-taxonomia-exit-code-cli.md) (exit-code 7=runtime), [ADR-0003](0003-tempo-naive-brt-runtime-env.md) (TZ=UTC/locale C)

## Contexto
Ao colapsar o pipeline em R **in-process** (ADR-0018), some o isolamento de `subprocess`+ENV que, na referência,
garantia o determinismo pinando BLAS/OMP só no launcher gerado (`install.sh:130`) e nos harness de teste. O
ADR-0008 fixa o **invariante** (TZ=UTC, LC=C.UTF-8, BLAS/OMP/MKL/VECLIB=1) e o ADR-0020 fixa o **exit-code** (`7`)
que o guard emite — mas **nenhum ADR ratifica a ORDEM de dispatch nem a lista de isenção**, que são contrato
transversal: todo comando de M2–M8 herda "o guard roda antes do corpo". O M1 descobriu o acoplamento
M1.1↔M1.4: um `doctor` que abortasse sob ambiente sujo seria inútil (não diagnosticaria o quê existe pra
diagnosticar), então precisa ser **isento** do enforce fatal.

## Decisão
A fachada da CLI (`scripts/prevcarga`, Rscript+docopt) obedece a um **contrato de dispatch** fixo:

1. **enforce-first (comandos guardados):** `prevcarga_runtime_enforce(strict=TRUE)` é a **1ª ação** — antes do
   corpo de qualquer comando **guardado** —, abortando (exit `7`, `[prevcarga/runtime]`) se o ENV de **entrada**
   estiver ativamente sujo (TZ presente e ≠UTC, ou thread-pin presente e >1). Unset/vazio é benigno (o guard seta
   o default). "Asserir ANTES de qualquer número" é o invariante que o isolamento de processo garantia de graça.
2. **Lista de isenção fixa:** `-h/--help` (não roda guard) e **`doctor`/`status`** (rodam
   `prevcarga_runtime_check()` **não-fatal** no corpo e **reportam** o ambiente sujo em vez de abortar — é o caso
   de uso: diagnosticar um ambiente quebrado). Nenhum outro comando é isento.
3. **Ordem falsificável:** sob ambiente sujo, um comando guardado (ex. `models`) sai com exit `7` **e `stdout`
   vazio** (o corpo nunca correu) — o `stdout` vazio é o que distingue enforce-antes de enforce-depois; o
   exit-code sozinho não falsifica a ordem.

## Consequências
O determinismo vira invariante de dispatch asserido in-process, não um pino que vive só no shell/testes. M2–M8
ganham o guard de graça (a fachada é escrita uma vez, M1.4). A lista de isenção é **fechada** — adicionar um
comando a ela é mudança consciente (um comando numérico nunca é isento). O `doctor` é o portador do gate
falsificável do runtime (reporta, não aborta).

## Alternativas consideradas
Pinar só no launcher/container (como a referência) — fura em silêncio in-process quando o Matrix/data.table
multithreada; pinar só nos testes — não protege a execução real; isentar nada (doctor também aborta) — torna o
diagnóstico impossível justamente no ambiente quebrado; enforce no fim do comando — perde a garantia "antes de
qualquer número".

## Evidência
- Clean-room: `docs/_archive/roadmap/M1-FUNDACAO.md` M1.1 (enforce/check), M1.4 Step 2 (dispatch + isenção + ordem),
  M1.5 (`doctor`/`status` consomem `check()`); `scripts/lib/runtime.R` (contrato no cabeçalho).
- Referência (`/opt/ons-source/prevcargadessem3`): pino só no launcher `scripts/install.sh:130` e em
  `jobs/tests/*.sh`; `jobs/entrypoint.sh`/`run_job.sh` **não** setam `*_NUM_THREADS` (o buraco que o M1 fecha).
