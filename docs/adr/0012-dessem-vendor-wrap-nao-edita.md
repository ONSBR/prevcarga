# ADR-0012 — Motor legado DESSEM vendorizado byte-idêntico; correções vivem em wrappers no loader, nunca no vendor (wrap-não-edita)

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** changed-in-rebuild  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0003](0003-tempo-naive-brt-runtime-env.md), [ADR-0010](0010-contrato-features-registry.md), [ADR-0018](0018-runtime-r-only-cleanroom.md)

> **⚠️ Correção de premissa (2026-10-10, Gabriel).** O Contexto/Decisão abaixo (garimpados do código) leem o
> DESSEM como **motor de terceiro vendorizado byte-idêntico** — isso está **ERRADO**. O motor é **código
> NOSSO** (pacote `prevcargadessem`, autoria interna ONS); o diretório `vendor/` e o padrão `pkgfun_*` são
> convenção de organização, não software de terceiro. Não existe oráculo externo a preservar: a única razão
> para manter o comportamento idêntico à referência é o **A/B Δ=0 do DESSEM** (rede de segurança do rebuild),
> que é uma *escolha*, não uma *amarra*. A seção "Decisão (resolvida)" ao final vale sobre a leitura antiga.

## Contexto
A paridade bit-a-bit com o motor de referência vendido é o que torna os gates Δ=0 significativos; editar o vendor dissolveria a linha de base de paridade. Mas o motor tem fragilidades herdadas (modelo-vazio do `tune_wsvm`, cross>n, nowcast).

## Decisão
Os `pkgfun_*` vendorizados são sourceados em ordem de dependência (falha alto se faltar função) e NUNCA editados. Toda adaptação (robustez, nowcast, config, override de `obterEntradaFormulaDM`, `getConfig` lendo o JSON vendorizado) é WRAPPER numa camada nossa que chama o real primeiro — bit-idêntico no caminho feliz — e só age no erro.

## Consequências
É o que torna a paridade falsificável. Qualquer correção futura precisa seguir wrap-não-edita ou perde a paridade. **OPEN/changed-in-rebuild:** o pré-shift +1h assimétrico do CARGAHIST (compensa o −1h hard-coded do engine vendido) é artefato de compatibilidade — num rebuild R-only que reescreva o motor (sem paridade de solver) a assimetria +1h/sem-shift deveria desaparecer, re-originando tudo limpo. Decisão 'honrar enquanto vendido / mudar ao reescrever'.

## Alternativas consideradas
Reescrever o motor em R-limpo (perde paridade byte-a-byte, alto risco); patchar o vendor diretamente (dissolve a baseline); mover a compensação +1h para dentro do engine.

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- models/dessem-linear/vendor/_load_engine.R:161-192
- models/dessem-linear/vendor/_load_engine.R:228-238
- models/dessem-linear/vendor/_load_engine.R:265-341
- models/dessem-linear/R/ingestion/data_adapter.R:189-195
- models/dessem-regdin/R/ingestion/data_adapter.R:76-80

## Decisão (resolvida 2026-10-10, Gabriel)
Duas correções sobre a leitura antiga:

1. **O motor é NOSSO, não vendor de terceiro** (ver banner de correção no topo). Portanto a disciplina não é
   "nunca editar código de terceiro" — é: **manter o comportamento idêntico à referência para preservar o
   A/B Δ=0 do DESSEM** (rede de segurança do rebuild), aceitando **refactors que preservem a saída** e tratando
   qualquer mudança de comportamento como divergência consciente com justificativa própria. O padrão de
   wrappers (nowcast, guards de erro) continua válido como *organização*, não como tabu de edição.

2. **O par −1h (motor) / +1h (adapter) NÃO é artefato de compatibilidade a eliminar — é SEMÂNTICA de domínio.**
   É a implementação da alocação da integral de carga no fim do intervalo + o join temp(`t`) → `∫carga[t,t+1]`.
   Está **correto** e **fica**. Essa semântica foi **elevada a invariante de sistema** no
   [ADR-0019](0019-alinhamento-temp-carga-invariante.md) (não é mais detalhe do DESSEM).

**O que muda no clean-room:** o shift do DESSEM é **re-expresso** pelos transforms nomeados compartilhados do
ADR-0019 — `to_model_frame` (carimbo-fim→início, −1h, para o join de modelagem) e `to_display_frame`
(início→fim, +1h, para a saída) — em vez do mecanismo atual `+1h-no-adapter / −1h-dentro-do-motor` espalhado.
É **output-preserving** (round-trip identidade) → **A/B Δ=0 do DESSEM preservado**; só torna a intenção legível
e testável. O desenho concreto dos transforms fica no **M5**.

Não se elimina shift algum nem se reescreve o motor agora: "eliminar" era baseado na premissa falsa de que o
±1h era hack de compatibilidade. Uma reescrita limpa do motor (se um dia escopada) seria frente própria, com
ADR próprio, e teria de preservar a mesma semântica de alinhamento do ADR-0019.
