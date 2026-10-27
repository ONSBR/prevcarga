# ADR-0019 — Alinhamento temperatura↔carga é invariante de DADO do sistema (alocação fim-de-intervalo), não quirk de um modelo; transforms nomeados compartilhados

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** changed-in-rebuild  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0002](0002-grade-semihoraria-schema-long.md), [ADR-0009](0009-arquitetura-modular-modelo.md), [ADR-0012](0012-dessem-vendor-wrap-nao-edita.md), [ADR-0014](0014-combina-leak-horizonte-solver.md)

## Contexto
A carga é energia = integral da potência num intervalo, e por convenção do sistema a integral de `[t−1, t]`
é **carimbada no fim do intervalo** (`t`): a leitura em `01:00` = energia consumida em `00:00–01:00`. A relação
física modelada é **temperatura no instante `t` → integral da carga de `t` a `t+1`** (a temperatura agora dirige
o consumo da hora que vem). Logo, para juntar temp(`t`) com a carga que ela explica (carimbada em `t+1`), é
preciso **deslocar a carga −1h** no frame de modelagem (o alvo vai de `t+1` para `t`, casando com a temperatura)
e **+1h de volta** no frame de exibição (restaura o carimbo fim-de-intervalo). Isso **não é** detalhe do motor
DESSEM — é verdade sobre o **dado**, e portanto vale para **todo** join sub-diário temp↔carga do sistema.

O garimpo inicial tratou esse shift como quirk do DESSEM ([ADR-0012](0012-dessem-vendor-wrap-nao-edita.md)). A
auditoria dos modelos (2026-10-10) mostrou que ele está hoje implementado **só** no DESSEM, e que os demais
divergem **por granularidade**, não por aplicarem um alinhamento diferente:
- **RF** — temperatura **diária** (tmax/tmin do dia de horizonte); não há join contemporâneo sub-diário, o
  shift não é aplicável como tal (`models/random-forest/R/ingestion/data_adapter.R:513-519,542-547`).
- **LGBM** — **único** com join contemporâneo temp↔carga em meia-hora, casado no **mesmo slot `DataHora` SEM
  shift** (`models/lgbm/R/ingestion/data_adapter.R:625-640`, `feature_engineering.R:49-53` "TempWav no shift")
  → temp(`t`)↔carga(`t`), **potencialmente desalinhado por um intervalo** vs a semântica correta.
- **HW** — univariado na carga; temperatura só como **efeito diário** (quantil ECDF), sem join horário
  (`models/holt-winters/R/training/hw_training.R:21`, `inference_features/hw_inference_features.R:28-31`).

## Decisão (resolvida 2026-10-10, Gabriel) — invariante centralizado; LGBM sob SPIKE antes de corrigir
1. **Invariante de sistema.** O alinhamento temp↔carga pela alocação fim-de-intervalo é elevado a **invariante
   de dado**, não mais semântica local do DESSEM. Dois **transforms nomeados compartilhados** vivem em
   `scripts/lib` (FE/dado), não em adapter de modelo:
   - `to_model_frame(load)`: carimbo-fim → carimbo-início para o join de modelagem (desloca −1h / −1 intervalo).
   - `to_display_frame(pred)`: carimbo-início → carimbo-fim para saída/exibição (+1h / +1 intervalo).
   São **output-preserving** (round-trip identidade) e testáveis isoladamente. Qualquer modelo que faça join
   sub-diário temp↔carga — hoje LGBM, amanhã qualquer novo — **herda** o alinhamento por padrão, em vez de
   re-derivá-lo (ou errar em silêncio).
2. **DESSEM.** Já segue a semântica; no clean-room re-expressa o par ±1h via esses mesmos transforms
   compartilhados (ver [ADR-0012](0012-dessem-vendor-wrap-nao-edita.md)). Saída idêntica → A/B Δ=0 preservado.
3. **LGBM — SPIKE antes de corrigir (M5).** Antes de mexer no LGBM, roda-se um spike que confirma
   **empiricamente** se o join mesmo-slot é de fato desalinhamento (e não algo já compensado upstream no
   `Temperatura_prevista_BLF`). **Se confirmar** → LGBM passa a usar `to_model_frame`, corrige o alinhamento
   → **diverge da referência de propósito**, e o A/B do LGBM vira **propriedade/qualidade, não Δ=0** (mesma
   natureza do leak da combina, [ADR-0014](0014-combina-leak-horizonte-solver.md)). **Se não confirmar** →
   mantém mesmo-slot, A/B Δ=0 do LGBM preservado. Decisão de corrigir fica condicionada ao spike, não cega.
4. **RF/HW — inalterados.** Não fazem join sub-diário temp↔carga; nada a deslocar. Mas consomem a mesma
   convenção caso passem a fazê-lo. (Nota fina, não-bloqueante: a agregação **diária** de carga do RF, sob
   carimbo-fim, deveria agrupar intervalos pelo dia a que pertencem — o slot `00:00` é do dia anterior; um
   helper `interval_to_day` compartilhado resolve, avaliar no M5, provável ruído no tmax/tmin.)

## Consequências
Centraliza uma verdade de domínio que estava implícita e espalhada (presente só no DESSEM, ausente no LGBM).
Torna o alinhamento **testável** (dois transforms com round-trip) e **herdável** (novo modelo não re-inventa).
Abre uma possível divergência de A/B no LGBM — **condicionada ao spike**, não automática. O round-trip
output-preserving mantém DESSEM em Δ=0. Risco: se o spike for mal desenhado, "corrige" um desalinhamento que
não existia; por isso o spike precede a correção.

## Alternativas consideradas
Manter o shift só no DESSEM (0012) e LGBM como está (preserva A/B do LGBM, mas perpetua desalinhamento
potencial e deixa a verdade de domínio implícita); corrigir o LGBM cegamente sem spike (arrisca divergir A/B
por engano); embutir o alinhamento em cada adapter (re-deriva, espalha, diverge); tratar como feature de
calendário em vez de transform de frame (não captura a semântica de integral).

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- models/dessem-linear/R/ingestion/data_adapter.R:189-195 (o +1h, com comentário da semântica)
- models/dessem-regdin/R/ingestion/data_adapter.R:76-80 (idem)
- models/lgbm/R/ingestion/data_adapter.R:625-640 (join temp↔carga no mesmo slot, sem shift)
- models/lgbm/R/.../feature_engineering.R:49-53 ("TempWav no shift")
- models/random-forest/R/ingestion/data_adapter.R:513-519,542-547 (temperatura diária, sem join horário)
- models/holt-winters/R/training/hw_training.R:21 (univariado, sem exógena no núcleo)
