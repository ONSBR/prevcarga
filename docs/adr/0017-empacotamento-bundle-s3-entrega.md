# ADR-0017 — Produto fechado distribuível: bundle R+runtime no S3 (relocável, glibc do host + preflight, jammy pinado, Rscript shim) + instalador bootstrap; entrega byte-copy; painel append-only; verificar por TOLERÂNCIA; semântica forecast/score/verificar

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0008](0008-honestidade-determinismo-falsificavel.md), [ADR-0011](0011-ciclo-artefato-registry.md), [ADR-0012](0012-dessem-vendor-wrap-nao-edita.md), [ADR-0018](0018-runtime-r-only-cleanroom.md)

> Nota de nomenclatura: o conceito antes chamado de "marmita" é o **bundle**. O nome mudou; a coisa continua. O script de build no repo de referência ainda se chama `ci/build_marmita.sh`.

## Contexto
O produto é entregue a um agente como modelo fechado portátil (sem admin/Docker), e a proveniência (sha no manifest) + o gate de reprodutibilidade dependem de não re-serializar o parquet. O baseline Δ=0 está amarrado a uma plataforma validada.

## Decisão
Distribui-se um runtime inteiro pré-compilado no CI (R 4.2.3 + rlib `renv.lock` snapshot pinado + venv), relocável em user-space, com a glibc core FORA (assumida do host) → preflight falha alto se host < 2.35; plataforma v1 = jammy x86_64 só; `Rscript` (ELF com path cravado) é substituído por shim bash para relocatabilidade. O **bundle congelado** = pesos da combina + perfil k̂ da reconcilia; train escreve, apply lê. A CLI entrega em layout plano por **CÓPIA DE BYTES** (não re-serializa, preserva sha256 + Δ=0); o painel `results/` é append-only (prever alimenta, treinar só congela). `verificar` compara artefatos DENTRO DE TOLERÂNCIA (piso 1 MW + 0,1% rel), não Δ=0 estrito — a folga cobre regdin multi-thread/arrow fora do jammy. Semântica dos comandos: `forecast`=diagnóstico, `score`=acurácia, `verificar`=reprodutibilidade.

## Consequências
O acoplamento ao glibc do host e ao snapshot pinado é o que garante o baseline validado; falhar fundo (GLIBC não encontrada) só no primeiro `prever` seria péssimo. O shim é frágil (uma forma nova `--file` vs `-e` pode quebrar silenciosa). **TENSÃO:** a tolerância do `verificar` vs o invariante Δ=0 falsificável ([ADR-0008](0008-honestidade-determinismo-falsificavel.md)) — afrouxar mascara bug, apertar quebra fora do jammy. (No rebuild: bundle R-only no S3 + `install.sh`/`install.ps1` bootstrap — ver PLANO M8; o venv Python some com o R-only, [ADR-0018](0018-runtime-r-only-cleanroom.md).)

## Alternativas consideradas
Container Docker; static musl; empacotar a glibc; conda/renv no host; multi-plataforma arm64/mac; recompilar R com path relativo/patchelf; re-serializar no delivery (quebra o sha); painel mutável; Δ=0 bit-exato como veredito público; só tolerância relativa; re-executar o pipeline em vez de comparar.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- ci/build_marmita.sh:20
- ci/build_marmita.sh:81-99
- scripts/install.sh:37-46
- scripts/install.sh:91-115
- jobs/run_reduce.sh:186-253
- scripts/prevcarga:318-342
- scripts/prevcarga:382-383
- scripts/prevcarga:413-468

## Decisão (resolvida 2026-10-10, Gabriel) — veredito em DOIS trilhos, com limiares APERTADOS
Substitui o piso frouxo de 1 MW + 0,1% por limiares explícitos e mais rígidos:
- **Mesmo hardware, duas rodadas (reprodutibilidade/determinismo):** `|Δ| ≤ 1e-5` (quase-estrito; cobre ruído de FP, não folga de plataforma). É o limiar do gate de determinismo e do braço FORBIDDEN do honesty-gate ([ADR-0008](0008-honestidade-determinismo-falsificavel.md)).
- **Máquinas diferentes (`verificar` do produto + A/B cross-machine):** `|Δ| ≤ 0,1 MW` **absoluto** (sem componente relativo). Cobre reordenação de FP entre máquinas da MESMA baseline (jammy/WSL2).
- **Nota de plataforma:** 0,1 MW em escala de GW é muito apertado — só é atingível na mesma baseline binária (jammy x86_64, inclusive WSL2). Alvo de **arquitetura/libm diferente** (ex. Windows nativo) provavelmente **não** fecha 0,1 MW → ou roda em WSL2, ou a paridade cai para um limiar declarado maior nesse alvo (decisão por-alvo, não aqui).
Isso resolve a tensão com o 0008: o invariante falsificável continua **apertado** (1e-5 mesmo-hardware), e a folga cross-machine é **estreita e absoluta** (0,1 MW), não o 1 MW+0,1% antigo.

**Aceite (2026-10-10, Gabriel): "acho que bate, vamos testar, qualquer coisa folgamos".** O 0,1 MW é meta **comprometida mas aferida no portão A/B** (M8): se máquinas-alvo da mesma baseline não fecharem 0,1 MW na prática, **afrouxa** declarando um limiar maior **por-alvo** (não se reabre o ADR nem se mexe no 1e-5 mesmo-hardware). O número só se consolida depois do primeiro A/B cross-machine real.
