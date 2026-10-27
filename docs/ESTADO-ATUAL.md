# ESTADO ATUAL — PrevCarga (clean-room modular R-only)

> **Fonte de verdade durável** (write-through). Atualizar **na hora** a cada fato que sobrevive à sessão.
> Este repositório é o **clean-room** da reconstrução; o sistema de referência vive em
> `/opt/ons-source/prevcargadessem3` (congelado, oráculo do A/B).

## Onde estamos (2026-10-15)
- **Plano canônico = SpecForge (2026-10-19):** spec "Modular CLI" (`.specforge/config.json`) em **`ready` desde
  2026-10-27** (15 épicos, 128 tickets; implementação pelo work-session loop). O antigo `docs/roadmap/` foi arquivado
  em `docs/_archive/roadmap/` nessa data e é só insumo histórico; nenhum fechamento o edita. As 22 ADRs foram
  importadas na entidade ADR do SpecForge como `proposed` (aceite do Gabriel no webapp). Dados locais: `data/` →
  symlink `/opt/source/prevcarga-R/data`. Orçamento da rodada operacional (`forecast`, a cada atualização do deck) ≤30 min; o verbo `morning` foi removido (2026-10-21). CI fora de escopo (gates locais). Bucket S3
  público (releases + artefatos + inferências oficiais, ADR-0039): nome [TBD].
- **Rótulo da carga (Gabriel, 2026-10-21; ADR-0035 proposed).** A carga chega da fonte integralizada no FIM do intervalo
  (`09:30` = 09:00–09:30) e, antes de qualquer média, é deslocada −30 min para o INÍCIO (média da hora = mesma hora; do dia =
  mesmo dia). O store `data/` já está deslocado (arquivos anuais 00:00–23:30; "−30min convention" no adapter DESSEM do
  oráculo) e o seu deck declara `load_label: start` no `manifest.json` (hoje ausente: precisa ser criado).
  Regra: `load_label: end|start` com precedência manifest > `--load-label` (só se o deck não declara; contradição aborta) >
  config > default `end`. `end` ⇒ o leitor único desloca −30 min; `start` ⇒ pula. O leitor entrega `interval_start` (médias e
  agrupamentos: hora, dia, k̂, pico, agregação horária) e `interval_end` (corte intradiário e rótulo de entrega). Granularidade
  em `run.yaml: granularity_min` (30: LGBM/HW; 60: RF/dessem), com `to_hourly`/`to_semihourly` únicos. Exógenas são
  instantâneas e não mudam. `load_label` efetivo entra na proveniência/cache. Sentinela nos dois modos.
- **Os dois cortes (definição do Gabriel, 2026-10-21):** (1) **corte de treino pelo horizonte** decide quais DIAS entram
  no treino; nenhum dia futuro entra (`origin_agnostic`: rótulos até `O−1−H`; `origin_scoped`: treino da origem O com série
  ≤ O−1). (2) **corte intradiário de operação** — QUEM DITA É O DADO, não o relógio nem o manifest: `available_until` é DERIVADO da
  carga presente no deck = `interval_end` do último intervalo sem buraco desde 00:00 do dia da origem, MÍNIMO entre as áreas
  (veredito mostra por área); buraco antes do último intervalo presente reprova no `validate-deck`. Substitui a chave `corte`
  do manifest (ADR-0007). O modelo já treinado lê `interval_end ≤ available_until` e
  prevê a partir do intervalo seguinte (carga até 08:30–09:00 ⇒ primeira entrega `09:30`, rode o comando às 09:05 ou às 11:00);
  **semi-horários (LGBM, HW)** usam todo intervalo `≤ available_until`; **horários (RF, dessem)** usam só hora completa — uma
  hora com apenas uma meia hora verificada é IGNORADA no horário (mas usada no semi-horário). Todo membro entrega a partir de
  `available_until + 30` (no horário, a meia hora já observada da 1ª hora prevista é descartada) para alinhar a combina; sem
  correção da hora pela meia hora observada na v1. São independentes.
- **Decisões do Gabriel em 2026-10-21 (spec re-entregue na fase 1):**
  - **Fuso declarado + CLI sem env vars (ADR-0037 proposed).** O `manifest.json` do deck declara `timezone` por dataset (por
    coluna quando mistura): store atual `load`/`weather_observed` = `Etc/GMT+3` (−03:00 fixo; 48 intervalos/dia inclusive nos
    dias de horário de verão de 2018), `weather_forecast.valid_time` = `Etc/GMT+3`, `weather_forecast.issue_time` = `UTC`
    (dígitos 00/06/12/18Z) [a verificar pelo Gabriel]. O parquet guarda `timestamp[ns]` SEM fuso: numa sessão R com o fuso do
    host (`America/Sao_Paulo`) as colunas aparecem −3h (o `issue_time` aparece 21:00) — armadilha real, caí nela. O leitor faz
    `force_tz` no fuso declarado e `with_tz` para o fuso da carga. Comparar `issue_time` × corte no mesmo fuso pode mudar a
    emissão escolhida vs o oráculo (ADR-0024). O pacote/CLI NÃO lê variável de ambiente: o runtime fixa TZ/locale/BLAS dentro
    do processo (sobrescreve o shell; só aborta se não conseguir); config = flags > `config.yaml` da instalação > default;
    instalador por argumentos. Somem `PREVCARGA_DATA/STATE/HOME/CONFIG_DIR/JOB/MARMITA/RUNTIME_*`.
  - **Datas só por lubridate com `tz` explícito** (fuso declarado); proibidos `as.POSIXct`/`strptime`/`as.Date` sobre data-hora/
    `difftime`/relógio (`Sys.time`, `Sys.Date`) no código de cálculo — grep no M1.V.
  - **R 4.5.3 (ADR-0036 proposed).** Engines nas versões do `renv.lock` da referência onde compilarem; spike no M1 roda o
    oráculo em 4.5.3 vs 4.2.3 e mede Δ por modelo antes do porte.
  - **`--origin`** é o único flag humano de data (sem alias). **Sem verbo `morning`**: a rodada operacional é
    `forecast` (validate-deck→infer→combina→reconcile→export→report) a cada atualização do deck; `score` é separado.
  - **Linux + Windows nativo (ADR-0038 proposed).** Instalador e bundle funcionam em PowerShell (Windows nativo, sem WSL2,
    sem admin) e em Linux: dois bundles por release (`linux-x86_64` jammy, `windows-x86_64`), `install.sh` + `install.ps1`
    (PS 5.1/7, `-ExecutionPolicy Bypass`), lançadores `prevcarga` / `prevcarga.cmd`, user-local, `current`/`latest` como
    arquivo-ponteiro. Código de produto portável: sem bash, `system()` p/ Unix, `fork`/`mclapply` (`--jobs` = PSOCK), symlink
    ou caminho Unix; gates bash de `jobs/tests/` viram testthat. Δ=0 na mesma plataforma; Windows nativo vs Linux ≤ 0,1%.
    Pendente: máquina/VM Windows para o gate do M8; motor PDF sem dependência de sistema nos dois bundles.
  - **Bucket S3 de leitura pública (ADR-0039 proposed).** HTTPS, sem credencial no cliente. Publica `releases/<v>/`
    (runtimes Linux/Windows + `SHA256SUMS`), `artifacts/…/v_<hash>/` + `latest.txt` (registry oficial do ONS, as duas classes
    de split) e `forecasts/<origem>/<available_until>/<versão>/` (inferências oficiais). Nunca: deck, dados de entrada,
    credenciais, `config.yaml`. Versionado + Object Lock (republicar falha; só `latest.txt` muda); todo download confere sha256.
    Publicação por `scripts/tools/` (Linux, credencial do publicador); consumo por instaladores + `prevcarga-sync` (.sh/.ps1),
    fora do pacote — a CLI segue sem S3/HTTP. Modelos treinados com a carga do ONS ficam públicos: risco aceito pelo Gabriel.
    Pendente: nome/região do bucket e IAM do publicador.
  - **Publicação em dois passos (ADR-0039):** o verbo local `prevcarga release-pack --what {runtime,artifacts,forecasts}`
    monta e valida o que vai ao bucket (layout, manifests, `SHA256SUMS`, conteúdo proibido, chave já publicada); o upload é
    um script trivial `scripts/tools/upload`, único ponto com credencial. O requisito de distribuição da spec ainda diz
    "scripts de publicação"; o detalhe do `release-pack` entra no corpo do épico M8 (fase 3).
  - **Banner da CLI:** `prevcarga` + curva de carga diária de 48 intervalos, só ASCII, ≤ 63 colunas; cor opcional
    (`color: auto|always|never` / `--color`; no Windows `auto` = sem cor); só em `--help`/`--version`/sem verbo/`doctor`.
  - **Contrato de erros (ADR-0040 proposed):** classes novas no fim da tabela do ADR-0020 — `8` config, `9` integridade,
    `10` rede (só utilitários). Toda saída de terminal em **ASCII puro, sem acento** (decisão do Gabriel: compatibilidade
    com cmd/PowerShell 5.1); HTML/PDF podem ter acento. stdout = resultado, stderr = avisos/erros; `--format json` = um
    único JSON em stdout (`ok, verb, exit_code, error{class,id,message,where,hint,details}, warnings, result`). Catálogo
    único de `error.id` em `scripts/lib/`; traceback só com `--debug`; utilitários .sh/.ps1 seguem o mesmo contrato.
    Vira um ticket no M1 (fase 4).
  - **Consolidação da fase 3 (épicos reescritos):**
    - **Tabela única de exit-codes (ADR-0040, absorve a 0027):** 0 ok · 1 genérico · 2 contract · 3 completude · 4 topology
      · 5 window · 6 bundle · 7 runtime · 8 config (inclui usage) · 9 integrity · 10 network · 11 coherence · 12 repro · 13 state
      · 14 partial · 15 honesty. Precedência runtime > config > contract > completude > topology > window > demais.
    - **ADR-0022 revista:** shell sujo é sobrescrito (saída idêntica); exit 7 só se não conseguir fixar.
    - **`cut_invariant` deixa de existir:** todo membro entrega a partir de `available_until + 30`; a classe vem só de `split`.
    - **Bucket público também leva pesos da combina e perfis k̂** (sem eles não se reproduz a inferência oficial).
    - **A/B dos horários:** com `available_until` em :30 o oráculo DESSEM usaria a meia hora parcial; a paridade dos
      horários usa decks com `available_until` em hora cheia.
    - **Nomes:** `scripts/lib/errors.R` (catálogo), `config.R`/`prevcarga_ctx`, `scripts/lib/deck_reader.R` (leitor),
      `to_hourly`/`to_semihourly` em `features.R` (porte), motor DESSEM único em `models/_dessem-engine/`.
    - **Decisões do Gabriel (2026-10-23):** (1) default de `--origin` = dia do último intervalo de carga presente no deck;
      (2) clima previsto configurável (ADR-0024): `weather_issue_cutoff: available_until` (default) | `origin_midnight_utc`
      (paridade com o oráculo, usado no A/B), via config.yaml ou `--weather-cutoff`, na proveniência e no cache;
      (3) `origin_scoped` (ADR-0023, confirmado com a Sofia): 1 re-treino por dia com dados até ontem (O−1), sem shift por H;
      novo `available_until` não re-treina; a carga do dia O entra só no `infer`; artefato chaveado só pela origem. A/B do
      dessem compara com o caminho de `infer` do oráculo (que passa `--available-until` ao treino).
      (4) **Último dia de dado por modelo** (ADR-0023): regra única "nada do dia O em diante entra no treino"; o
      `last_data_day` segue o oráculo de cada modelo — RF O−1, LGBM O−3 (o oráculo lê só até `train_origin − 1`),
      origin_scoped O−1. `dates.R` devolve `train_origin` e `last_data_day` por modelo; o CONTROL do honesty-gate
      perturba esse dia. No HW o `infer` só lê o artefato; no dessem usa a manhã de O (`mm`, só horas completas).
      (5) **Faixa de entrega por membro** (ADR-0032 c): todos começam em `available_until + 30` (inclusive o resto do
      dia O) na mesma grade; o fim é `delivery_days` do descriptor do plugin (LGBM O..O+1; RF, HW e dessem O..O+8).
      A combina junta, por lead, só os membros que o cobrem e renormaliza os pesos; alinhamento exigido = início e
      grade, não fim. A/B: regdin com `available_until` = 00:00 é exceção consciente (~0,2% no d_plus=0, RNG preservado).
      (6) **Barrel por descoberta, não por edição** (Gabriel, 2026-10-25; ADR-0040 itens 7–8 emendados): R não tem barrel,
      então toda "lista central" que vários módulos alimentam vira uma pasta lida por um loader determinístico
      (`list.files` + `sort(method = "radix")`, sob `LC_ALL=C` do runtime). Catálogo de erros = `scripts/lib/errors.R`
      (máquina + loader) + `scripts/lib/errors/<modulo>.tsv` (um por módulo; id único, classe da tabela, ASCII);
      `install/error-catalog.tsv` é GERADO por `scripts/tools/export_error_catalog.R` (não versionado), com teste de
      sincronia. Verbos da CLI = `scripts/lib/cli/<verbo>.R` auto-registrados; `scripts/prevcarga` só despacha. Sub-gates
      dos plugins = `scripts/tools/gates/<dono>.R` descobertos por `prevcarga_gates_run`. Motivo: no plano, todo par de
      tickets que escreve no mesmo arquivo precisa de ordem; `errors.R` com 30 escritores em 11 épicos serializava a
      implementação inteira (107 ondas para 128 tickets). Pelo mesmo motivo o estado durável passa a ser este índice +
      `docs/estado/<modulo>.md` (um por módulo, criado pelo fechamento do épico); `scripts/lib/README.md` só pelo M1;
      `docs/_archive/roadmap/PLANO.md` deixa de ser editado pelos fechamentos. CLAUDE.md atualizado (Fonte de verdade, tiers, princípio 6).
      (7) **Plano SpecForge fechado (2026-10-27, validação cruzada sem findings):** spec "Modular CLI" com 15 épicos e 128
      tickets (91 impl / 37 verificação), 771 dependências, 28 ondas de paralelismo (maior onda 11), caminho crítico de 27
      tickets. Regra de grafo: dependência entre épicos aponta para o ticket que CRIA o arquivo consumido, nunca para o
      fechamento/verificação do épico. Corpos de épico, spec e tickets alinhados entre si (2026-10-26/27). Decisões pendentes
      do Gabriel listadas no PR da spec (ADR-0015 emenda, aceite das ADRs 0023–0040, fusos do store, bucket/IAM, VM Windows,
      ADR-0031).
      (8) **Convenções fixadas no fechamento do plano (2026-10-26/27):** estado local de runtime em `.state/` (registry local
      `.state/artifacts/<model>/<area>/...`, golden, cache, backtest), nunca versionado; arquivos de raiz da spec: `.gitignore`,
      `README.md` (M8.12), `renv.lock` (M8.1). Erros de plugin: os genéricos `plugin.*` do `scripts/lib/errors/plugin.tsv` (P.0a)
      são o padrão; plugin ou motor com erro específico declara o próprio TSV no seu épico (`dessem_regdin.tsv` RD.1,
      `dessem_engine.tsv` DL.1, `dessem_linear.tsv` DL.5), avisos com classe `ok`. Ninguém edita `docs/_archive/roadmap/` nem
      `.claude/workflows/` (o SpecForge é o plano canônico). Passos de teste usam a categoria `test` também em tickets de
      implementação.
      (9) **Revisão dos 40 ADRs (Gabriel, 2026-10-27; relatórios em `sandbox/adr-review/`):** aceitos 0004, 0022, 0035–0040;
      superados 0003 (→0037), 0006 (→0023), 0007 (→0035), 0020 e 0027 (→0040); os demais ajustados e ainda `proposed`.
      Decisões novas: **ajuste de hora centralizado** (princípio 7 do CLAUDE.md; offset de modelagem no descriptor,
      aplicado pelo despachante); **`forecast_origin` = texto `yyyy-MM-dd`** em todos os writers e leitores (ADR-0032);
      **qualidade da combina** só bloqueia nos fixtures do M4 (ε=0), no holdout real só é reportada (ADR-0025);
      **staging único `.tmp-*`** para todo escritor do registry, limpo pelo `registry gc`, com teste de promote
      interrompido também no Windows nativo (ADR-0030); **oráculo validado uma vez** no A/B do corte e descartado, sem
      período em paralelo (ADR-0034). Essas decisões ainda precisam entrar nos tickets (próxima reabertura do plano),
      junto com a correção do texto da spec que cita `--as-of` como alias.
  - **Sem aliases (Gabriel, 2026-10-23):** nomes herdados do oráculo não existem no clean-room — `prever`, `treinar`,
    `morning`, `publish` e a flag `--as-of` dão erro de uso (exit 8); `--origin` é o único flag de data. Não há aviso
    `deprecated` no M1 (o mecanismo genérico de aviso fica). O porte virou 1 épico por modelo (RF, LGBM, HW,
    dessem-linear + motor, dessem-regdin) + "Plugins — fundação" + um épico Reconcile separado do M6 (15 épicos). A spec (fase 1) ainda cita os aliases; épicos e blueprints já não.
  - **Porte do LGBM:** o `job.json` da referência liga comportamento por env var (`LGBM_DISABLE_LOESS`,
    `LGBM_CALENDAR_YEAREND`, `LGBM_TRAIN_CV`); no clean-room viram `params` do `run.yaml` (ADR-0037). O freeze do LGBM é o
    modelo em texto do lightgbm (artefato portável Linux↔Windows), não `saveRDS` cru.
  - **Guardrails novos:** semente do `run.yaml` obrigatória; saída publicada imutável por (origem, `available_until`, versão);
    `verificar` só compara saídas em disco (o oráculo roda por script em `scripts/tools/`); sentinela de `load_label` nos dois
    modos só no leitor e ponta a ponta; honesty-gate só nos módulos que consomem `y_true`; buraco no meio da série ≠ corte.
- **CORREÇÃO (2026-10-20, Sofia): `daily`/`retrain_harness`/`fixed` são PROTOCOLOS DE BACKTEST, não atributo do modelo.**
  Os 5 modelos têm `train`+`infer` (produção); HW/dessem mantêm `forecast`/`walk` acoplado como régua de paridade
  (`infer`≡`forecast` Δ=0 no dia D) e caminho do backtest. A propriedade do modelo é a CLASSE DO SPLIT (ADR-0011):
  `origin_agnostic` (RF/LGBM: infer só lê o artefato) vs `origin_scoped` (HW/dessem: 1 treino = 1 origem; o infer
  re-ajusta parte — no regdin refita o ARIMA da demanda-média ⇒ `history_days.infer == train`, senão ~150 MW no 1º dia).
  `run.yaml` passa a ter `split:` + `backtest.protocol_default`. Plano SpecForge sendo corrigido (spec→épicos→tickets).
- **ADRs 0023–0034 (proposed, 2026-10-20, só no SpecForge):** rascunhos com recomendação para as 12 decisões pendentes do
  Gabriel. 0023 supersede o 0006 (corte por protocolo), 0024 freshness do clima, 0025 ε do A/B, 0026 solver/eixo da
  combina, 0027 emenda dos exit-codes, 0028 contrato de métrica + k̂, 0029 honesty-gate in-process (G5/0008), 0030
  registry/cache/store, 0031 FE-fachada + motor DESSEM, 0032 LONG/run.yaml, 0033 motor PDF, 0034 distribuição M8.
  Nenhuma vale até ser aceita no webapp. Tickets dependentes carregam `[TBD: ADR-00NN]` + skip explícito.
- **Fase:** semeadura do esqueleto + ADRs fechadas + M1–M8 expandidos e VERIFY-READY (o M8 em 22 rounds). Nada de produção implementado ainda.
- **ADRs:** **22/22 resolvidas** (`docs/adr/`, nenhuma em aberto; 0020 exit-codes, 0021 run.yaml, 0022 dispatch do
  runtime-guard surgiram no `verify-milestone` do M1). **Deck de entrada** contratado em `docs/reference/DECK-CANONICO.md`.
- **Plano vivo (até 2026-10-27; hoje arquivado):** `docs/_archive/roadmap/README.md` (índice de milestones: estado de cada M1…M8 + ciclo de vida) +
  `docs/_archive/roadmap/PLANO.md` (sequenciamento/racional). Trabalho **solo/serializado**: M4 (combina) antes de M5.
- **Objetivo:** `docs/_archive/roadmap/CLI-MODULAR-OBJETIVO.md`. **Ref. técnica da combina:** `COMBINA-TECNICO-M4.md`.
- **M1 expandido** em `docs/_archive/roadmap/M1-FUNDACAO.md` (6 tickets M1.1–M1.6 com Steps+AC+DoD, formato specforge).
- **M1 ✅ VERIFY-READY** (`verify-milestone` ready:true · 0 blockers · 6/6 unânime, 2026-10-11; `CHECKPOINTS['M1']`
  já wirado). (Histórico: o plano migrou para o SpecForge.)
- **M2 expandido** em `docs/_archive/roadmap/M2-DATAS.md` (3 tickets M2.1 `dates.R` / M2.2 `validation.R` / M2.3 `validate-deck`,
  formato specforge) e **✅ VERIFY-READY** (ready:true · 0 blockers · 0 decisions · 6/6 unânime, 2026-10-12;
  `CHECKPOINTS['M2']` wirado+sincronizado). ⚠ **3 decisões do Gabriel ANTES do implement do M2:**
  - **Corte-de-treino por-protocolo vs ADR-0006:** o oráculo trata `daily` (RF/LGBM, `origem−1−horizonte`) ≠
    `retrain_harness` (dessem/HW, re-fit na origem O com série O−1) — `honesty_gate.R:391` ramifica. Isso **contradiz o
    texto do ADR-0006 Aceito** ("idêntico em todos os protocolos"). O M2.1 adotou a **opção (b)**: cobre só `daily`
    (conforme ADR-0006); dessem/HW → M5. Opção (a) (ramificar já no M2) exigiria um **ADR superseder do 0006** (ato do
    Gabriel) antes do implement. **`ESTADO-ATUAL` §Invariantes e o ADR-0006 ficam INTACTOS/conformes até essa decisão.**
  - **3 correções de premissa "repo vence"** (achados verificados no oráculo, a aplicar no implement do M2): (1) a
    referência **NÃO** preenche carga com 0 (mantém NA + falha alto / trunca honesto) — o `validate-deck` unifica/antecipa
    o fail-high, não conserta fill-0 inexistente; (2) completude é **flat-48** (DST absorvido na malha, ADR-0002/0003 —
    não há dias de 46/50; o `≥24` do §Invariantes abaixo e do stub `validation.R:10` vira 48 no implement); (3)
    `history_days.infer` **não** é 45 p/ todos — `run_job.sh:104` casa infer=train=1825 p/ dessem.
  - **4 candidatos a ADR** (sinalizados, não criados — ato do Gabriel): regra de precedência multi-classe de exit-code;
    contrato do digest `data_hash` do deck; esquema do veredito-máquina JSON; e o superseder do 0006 (corte por-protocolo, no M5).
- **M3 expandido** em `docs/_archive/roadmap/M3-AVALIACAO.md` (4 tickets M3.1 `evaluation.R` / M3.2 `honesty_gate.R` / M3.3 `score` /
  M3.4 `check-coherence`, formato specforge) e **✅ VERIFY-READY** (ready:true · 0 blockers · 6/6 unânime, 2026-10-13;
  convergiu em ~11 rounds; `CHECKPOINTS['M3']` wirado). ⚠ **Decisões do Gabriel ANTES do implement do M3:**
  - **Candidatos a ADR** (sinalizados, não criados): **contrato de métrica** (WMAPE-com-abs / MAPE-exclui-zero / peak-fixo-18-22h /
    baselines-por-slot / limiar `|Δ|≤1e-5` **+ faixa k̂ [1.0,1.15]**); **molde de honesty-gate in-process** `(run_fn,panel)` que
    anota/supersede o isolamento-de-processo (G5) do ADR-0008; **classe `coherence→8`** + extensão da classe-3 `completude` à
    **completude estrutural de nó** (hoje só temporal) — ambos emendam o ADR-0020.
  - **Correções de premissa "repo vence"** (verificadas no oráculo, a aplicar no implement): (1) `read_actual`/leitura de carga é
    **canônica `load_mwh` fail-high** (ADR-0004/0005) — **não** a auto-detecção por aliases do oráculo; (2) a identidade do
    `check-coherence` é **multiplicativa** `subsistema=Σregiões·k̂` (ADR-0015, perda = resíduo) — **não** aditiva com linha
    `y_pred(perda)`; (3) WMAPE vive em **~11 lugares** divergentes (não "≥4"); (4) as 3 cópias inline de MAPE **têm**
    zero-handling (`y_true>0` a montante), a divergência real é exclui-zero vs exclui-não-positivo vs floor.
  - **Honesty-gate é protocolo-agnóstico** (afere o invariante de não-vazamento `target≥origem`, perturbando **só `y_true`**) —
    **NÃO** reabre a decisão opção-a/b do corte-por-protocolo (essa é do M2.1/M5); `ESTADO-ATUAL` §Invariantes e o ADR-0006 ficam intactos.
  - **Deferimentos:** ramp/timing/peak-por-regime → M4 (dependem de `regimes.py`, a régua de qualidade da combina); MORNING +
    `available_until` → **M5** (o M5 entrega o braço MORNING, SÓ dessem); ⚠ **reach-control generalizado p/ módulos-sem-`y_true` re-sequenciado M5→M6** (seu módulo-sujeito é o `reconcile`/M6 + depende da ratificação freshness-clima; ver `M5-TRAINING.md`); verbo `backtest` → M7; `build_long_output.R` → **M6**; pontuação fresca em nível-SIN → M7; `METRICAS-CANONICO.md` → decisão do Gabriel.
- **M4 expandido** em `docs/_archive/roadmap/M4-COMBINA.md` (4 tickets M4.1 núcleo R-base Duchi/FISTA / M4.2 IO-painel-descoberta /
  M4.3 freeze-apply saveRDS / M4.4 gates+`combine`, formato specforge) e **✅ VERIFY-READY** (ready:true · 6/6 unânime ·
  0 landmines/controles bloqueantes por **6 rounds consecutivos** R8–R13, 2026-10-13; `CHECKPOINTS['M4']` wirado +
  `_COMBINA_CP` estale **APOSENTADO**; `verify-log/M4-COMBINA.jsonl`). ⚠ **Honestidade:** o R13 final ainda sinalizou 1
  blocker de dívida-de-revisão + 3 materiais (TODOS auto-consistência dos fixes anteriores — a cauda de amplificação
  prevista); corrigidos em-lugar, loop **parado deliberadamente**. ⚠ **Decisões do Gabriel ANTES do implement do M4:**
  - **Combina = leak #1 corrigido EM VOO:** corte por data-alvo `target<início-do-mês(m)` por (lead,corte,**mês-de-ORIGEM**),
    NÃO `ym<m`; sem paridade com o Python (contaminado) → **os números de backtest re-baselinam** (avisar stakeholders; M7).
  - **Ratificações de ADR Aceito** (não absorvo unilateralmente): **solver** FISTA-R-base (zero dep) vs "QP em R" literal
    (ADR-0014:13); **ε** do A/B (estrito `ε=0` ratificado vs folga); **corte per-mês** (clarif. do "por membro" do ADR-0014);
    **eixo = mês-de-ORIGEM** (o texto do ADR-0013 diz "alvo", o código/oráculo é "origem" — clarif. 0013).
  - **Candidatos a ADR** (sinalizados, não criados): escopo-G2 p/ `run_fn` expansível (`forecast_origin==origin`);
    layout/run-mode dos dirs de run dos membros; `model_versions` no manifest do ensemble; dono do enriquecimento-`protocol`
    (ADR-0011:39=M3-training vs 0013:38=M4 — reconciliar p/ M3).
  - **Correções "repo/frozen vence"** (a aplicar no implement): descoberta **run-mode-agnóstica** por identidade
    `(mdir,EXOGENA)` (NÃO o campo `protocol`=classe-de-re-fit, que em produção é `_infer`); `member_set_hash` = definição
    **congelada do M1.3** (`digest(prevcarga_members())`, não um 2º hash); gatear em `y_pred` **nunca em `w`** (não-identificado);
    freeze **`saveRDS`** content-addressed `v_<hash>` (não JSON, não `v_timestamp`); **re-fit-por-corte** não interpolação
    (ADR-0013(b)); membros do **manifesto** não glob-no-disco (ADR-0013(a)).
  - **Escopo:** SÓ o módulo `combination.R` + o verbo `combine`; flip de produção (`run_reduce`/`prevcarga`→R) = **M6**,
    bundle R-only/remoção-do-Python = **M8**. ⚠ **Write-through pendente nos docs duráveis** (PLANO §M4 corte+Δw;
    COMBINA-TECNICO Δw/interp/24-áreas; CLI-MODULAR ponteiros tri-partidos) — enumerado na §DoD do M4-COMBINA.md, vários
    atados às ratificações de ADR acima (a aplicar no implement/pós-decisão, não pré-review).
- **M5 expandido** em `docs/_archive/roadmap/M5-TRAINING.md` (5 tickets M5.1 FE-fachada / M5.2 training+`train-models` / M5.3 inference+`infer-model` / M5.4 registry-lifecycle / M5.5 gates-por-modelo, formato specforge) e **✅ VERIFY-READY** (ready:true · 6/6 unânime · 0 landmines/controles bloqueantes, convergido em **12 rounds** R1–R12 [0 blockers em R10 e R12], 2026-10-14; `CHECKPOINTS['M5']` wirado; `verify-log/M5-TRAINING.jsonl`). ⚠ **Honestidade:** do R7 em diante os achados viraram cauda de auto-consistência (AC-mais-fraco-que-Step, rótulos [pós-superseder]); R12 fechou 0-blockers, residuais corrigidos em-lugar, loop parado deliberadamente. ⚠ **Decisões do Gabriel ANTES do implement do M5:**
  - **SUPERSEDER do ADR-0006 (a DECISÃO-CHAVE):** formaliza as 2 semânticas de corte-de-treino — **daily** (RF/LGBM) `origem−1−horizonte` vs **retrain_harness** (dessem/HW) `série≤O−1`+nowcast (HW cut-invariante). O texto do 0006 diz "idêntico em todos os protocolos" mas o oráculo ramifica (`honesty_gate.R:407`); **bloqueia os 3 gates retrain_harness + o MORNING** (os 2 daily RF/LGBM são superseder-independentes e fecham sem ele). Pré-sinalizado em `M2-DATAS.md`.
  - **Ratificações de ADR:** lifecycle-registry (promote-atômico/gc-retenção-LOCAL/pin/rollback **+ dimensão variant/EXOGENA no layout** `<model>/<variant>/<area>` herdada por M6/M7); **FE-fachada** nome/local + legitimidade vs **ADR-0009** (normalização-de-assinatura + dispatch-fino ≠ "camada de FE canônica compartilhada" rejeitada); `prevcarga_model_spec(overrides=)` resolve o efetivo (clarif. **ADR-0021**); verbo `infer-model` do dessem (`_infer`-fatia vs `predict`-SVM); classe de exit-code p/ artefato-ausente + usage-error-CLI (**ADR-0020**, argparse=2 colidiria com contract=2); freshness-clima **instante vs midnight-UTC** (ADR-0007/0003, destrava o reach-control M6); dono do `protocol` (ADR-0011:39=M3 vs 0013:38=M4); magnitude/estágio dos transforms (ADR-0019).
  - **Correções "repo/frozen vence"** (no implement): FE-fachada **dispatch-fino, zero FE compartilhada** (ADR-0009); descoberta/janela **config-driven** (nunca `case dessem-*` — ADR-0001); `version` **content-addressed** (fecha o `version=timestamp`); `build_long_output.R` e o reach-control-generalizado → **M6**; `--jobs` com **runtime_enforce POR-WORKER** (BLAS pin não herdado em PSOCK).
  - ⚠ **Pré-requisitos M1/M2 ausentes** (dívida a resgatar ANTES do implement, NÃO escopo do M5): `provenance.R`/`dates.R`/`topology.R` são stubs; `models/`/`config/topology.yaml`/os 5 `run.yaml` + o **código R dos 5 plugins** (`models/<m>/R/…`, via skills `modularize-model`/`modularize-dessem`) não existem no clean-room.
  - ⚠ **Write-through pendente nos docs duráveis** (a aplicar no implement): `PLANO.md §M5`, `CLI-MODULAR-OBJETIVO.md` (módulos 1/2/3/8 vivos + reconciliar `:60` `build_long_output.R`=módulo-4 vs M6).
- **M6 expandido** em `docs/_archive/roadmap/M6-FORECAST.md` (6 tickets M6.1 build_long_output / M6.2 exports+`export` / M6.3 reconcile+`reconcile` / M6.4 verbo `forecast`+standalones / M6.5 reporting+`forecast --report` / M6.6 gates-diferidos+reach-control, formato specforge) e **✅ VERIFY-READY** (ready:true · 6/6 unânime · 0 blockers · 0 false-claims · 0 landmines/controles bloqueantes, convergido em **5 rounds** R1–R5 [0 blockers no R5], 2026-10-15; `verify-log/M6-FORECAST.jsonl`). ⚠ **Honestidade:** do R2 ao R4 os blockers foram majoritariamente dívida-de-revisão/deriva-de-citação dos próprios fixes (2 false-claims corrigidas; o split do reach-control 2a/2b propagando); R5 fechou 0-blockers, materiais genuínos corrigidos em-lugar, loop parado deliberadamente. ⚠ **Decisões do Gabriel ANTES do implement do M6:**
  - **RATIFICAÇÃO freshness-clima (a DECISÃO-CHAVE):** instante `available_until` vs midnight-UTC (ADR-0007) — **bloqueia SÓ o alvo clima-previsto do reach-control (M6.6 (2b))**; o reach-control **módulo-nível k̂/reconcile (2a) é ratificação-independente e RODA no M6**. Pré-sinalizado em `M5-TRAINING.md` e `:47,:73`.
  - **Ratificações/candidatos a ADR:** motor R→PDF + transporte Plotly (emenda ADR-0017); **emenda ADR-0002** ("único CONVERSOR de forecast model-native" + tipo canônico de `forecast_origin` Date-vs-character); **campo `run.yaml` de ancoragem-em-O/`first_forecast_day`** (refino ADR-0021, precedente `protocol`/0011); **hospedeiro do `report_A` port** (`backtest --report`/M7 vs `score --report`); k̂ clamp-vs-check (0015); atomicidade do export; exit-code artefato-ausente (0020, herdado do M5); chave `<model>/<ss>` do perfil k̂ herda o layout/gc do M5.4.
  - **Correções "repo/frozen vence"** (no implement): reconcile topologia de `topology.R` (não hardcoded) + SIN=Σ **count-agnóstico** (ADR-0001 — o membro "CLI Py" do reach-control do `forecast.R` idem); `build_long_output` dono **só** do caminho model-native de folha (o writer fino agregado da reconcilia é concern DISTINTO, `forecast_origin=character`, isento); `prevcarga_infer` **fica model-native** (M5.3 INTACTO — a camada LONG é do M6/`forecast.R`, spec-driven); reach-control **só k̂/reconcile** (export e combina-apply = erro-de-categoria, sem janela de honestidade); o **recorte-por-origem** (produtor do dir datado) é do `forecast.R` (substitui `run_reduce.sh:256-277`); "flip"=**acreção** (`:80`, não switch Python→R); `report_A`-sem-Plotly (é HTML/CSS/SVG puro).
  - ⚠ **Pré-requisitos M1–M5 ausentes/stub** (dívida a resgatar ANTES do implement, NÃO escopo do M6): `topology.R`/`provenance.R`/`dates.R`/`evaluation.R`/`honesty_gate.R` stubs; `combination.R`/`inference.R`/`features.R`/`registry.R`-ciclo + `config/`/`models/`/os 5 `run.yaml` + o código R dos 5 plugins + `run_all_gates.R` (via `modularize-*`) não existem. O M6 consome.
  - ⚠ **Write-through pendente nos docs duráveis** (pós-decisão): `PLANO.md §M6`, `CLI-MODULAR-OBJETIVO.md` (:60 `build_long_output`→M6 confirmado; módulos 6/7/10 + `forecast` + standalones vivos).
- **M7 expandido** em `docs/_archive/roadmap/M7-ORQUESTRACAO.md` (5 tickets M7.1 `orchestrate.R` / M7.2 `cache.R` / M7.3 verbo `backtest` / M7.4 `backtest --report` / M7.5 gates-de-harness, formato specforge) e **✅ VERIFY-READY** (ready:true · 0 blockers, convergido em **6 rounds** R1–R6 [0 blockers no R6], 2026-10-15; `verify-log/M7-ORQUESTRACAO.jsonl`). ⚠ **Honestidade:** o mais churny até aqui — do R2 ao R5 misturou achados genuínos profundos (eixo **membro×folha×origem**; combina **FITADA honesta por-mês** [`combina_full`, não congelada]; gatear no **`y_pred` combinado NUNCA em `w`** — correção de um erro que introduzi no R4, pego quando o consenso caiu a 1/6; identidade de membro **`(mdir,exogena)`**) com dívida-de-revisão dos próprios fixes; R6 fechou 0-blockers, materiais genuínos corrigidos em-lugar, loop parado deliberadamente. ⚠ **Decisões do Gabriel ANTES do implement do M7:**
  - **Chave do cache content-addressed** class-aware-vs-universal (corte-na-chave honra ADR-0007/0011: só data/feature-table + dessem re-treinado [protocol∧cut_invariant] re-keyam por corte; RF/LGBM/HW reusam o artefato) + **estender o `prevcarga_model_spec`/M1 p/ expor `cut_invariant`** como PRÉ-REQ HARD (key-time; a convenção `_cHHMM` é discovery-time, não serve) vs mapa estático. Candidato a ADR (refino 0011).
  - **GC/evicção do cache+ledger** (default: fora-do-módulo/staging externo, ADR-0016) + **namespace do store de backtest DISTINTO do registry de produção** (sobrevive ao `gc` do M5.4, senão `--resume` perde cache). **Política de entrega parcial** fail-fast-default + `--keep-going`. **Exit-code de parcial** (ADR-0020, herdado).
  - **Régua-de-FORMA** (regime×d_plus/rampa/timing/peak-regime + `regimes.py`) é hot-potato M3→M4→M7 que ninguém portou: absorver no M7 (cresce `evaluation.R`) vs recortar o `backtest --report` ao que o `evaluation.R`/M3 provê (default). **ε do A/B** (dominância no holdout real, herdado do M4/ADR-0014). **Skill-score baseline** (persistência O−1 + sazonal O−7) + shift DESSEM comunicado. **Transporte Plotly** (motor R-nativo vs vendor).
  - **Correções "repo/frozen vence"** (no implement): ledger/cache **LOCAIS** (nunca S3, ADR-0016 — o `present_set`/`aws s3 ls` do oráculo não é portado); eixo da matriz = **membro (`prevcarga_members()`, 8)** não modelo (5 dirs); backtest **FITA** a combina honesta por-mês gateando no `y_pred` (não aplica pesos congelados); retrain **não contamina** o `latest` de produção (store distinto); `--corte` só no `backtest` (concilia ADR-0007, que rejeita `--corte` humano em produção); `report_A` só-render (cálculo em `evaluation.R`); reconcile-**engine** segue Δ=0 (ESTADO-ATUAL:90 — o pós-combina herda a divergência da ENTRADA, não é reclassificado).
  - ⚠ **Pré-requisitos M1–M6 ausentes/stub** (dívida a resgatar ANTES do implement, NÃO escopo do M7): todos os módulos M1–M6 + `config/`/`models/`/5 `run.yaml` + código R dos 5 plugins + `run_all_gates.R` + **o refino `cut_invariant` no `model_spec`/M1**. O M7 **consome**.
  - ⚠ **Write-through pendente nos docs duráveis** (pós-decisão): `PLANO.md §M7` (dimensão membro×folha×origem; cache corte-na-chave), `CLI-MODULAR-OBJETIVO.md` (:70/:165 report_A-sem-Plotly; :201 "47/192"→count-agnóstico).
- **M8 expandido** em `docs/_archive/roadmap/M8-BUNDLE.md` (8 tickets: M8.1 build do **runtime** R-only + `RUNTIME.json`/`runtime_id` / M8.2 publish S3 imutável / M8.3 `install/install.sh` + launcher `Rscript --vanilla` + **3 raízes** (`versions/`·`current`·`PREVCARGA_STATE`) + artefatos treinados via `registry import` / M8.4 `install/install.ps1`→WSL2 / M8.5 `verificar` R (2 trilhos, modo legado, exit `9`=`[prevcarga/repro]` default) + `doctor` de plataforma / M8.6 `morning` / M8.7 A/B por estágio = gate de corte / M8.8 Python-free no artefato + encerramento; `CHECKPOINTS['M8']` wirado, `deployByDefault:false`). Verify R1: ready:false (19 blockers) → v2; R2: ready:false (9 blockers, 3 false-claims de citação minhas; consenso 3/6) → v3; R3: ready:false (12 blockers, ~metade dívida-de-revisão; consenso 1/6) → v4 (referência do oráculo gerada com o runtime do próprio oráculo + pins; `run_manifest.json` na entrega; `forecast --versao-membro` p/ o re-fit do cache; trilho do Estágio A = 1e-5 com mesma máquina+runtime, senão 0,1 MW; D17/D18 novos; D10 [o corte] é DEPOIS do A/B); R4: ready:false (8 blockers, 2 false-claims herdadas do coletor sobre `VECLIB`/pins — o launcher do oráculo já pina `VECLIB_MAXIMUM_THREADS`, `scripts/install.sh:130`; consenso 3/6) → v5; R5: 3 blockers (consenso 5/6) → v6; R6: 9 blockers (consenso 0/6, amplificação dos mecanismos do R5) → v7 **simplifica** (sem `--corte` no `verificar` [ADR-0007]; `forecast --pins` por `(membro,área)` + sidecar `prov.json` class-aware no cache; D-propriedade sempre no veredito, D-qualidade após ε; exceções do Δ só via `jobs/tests/ab_excecoes.yaml`; `morning --dados`; 4 emendas declaradas a testes de M1/M6); verify em curso até 0 blockers e consenso 6/6 (pedido do Gabriel). D19 (pós-corte) é [DEFERRED]. R7: ready:true · 6/6 · 2 blockers → v8 … **R22: ✅ ready:true · 0 blockers · consenso 6/6** (22 rounds). Mudanças estruturais acumuladas desde o R7: honesty-gate com `forbidden_from` estrito por `cut_invariant` (+ deck-sombra em `tools/`); `backtest --out/--variant`; `meta$train_origin` lateral via `prevcarga_train_origin(..., spec=)`; Estágio C' (membros `daily` do `prever` do oráculo × `infer-model`); Estágio A roda sem `registry/`; `test_ab_harness.sh` no fullGates; 6 emendas declaradas a testes de M1/M6; `code/` exclui `tools/`/`data-prep/`/`*.md`; decisões D1–D32 do Gabriel (+ ADRs supersedentes ⟪S⟫). 22 materiais residuais do R22 anotados no banner do doc, não aplicados. ⚠ **Nomenclatura:** **runtime** (`runtime_id`, lateral) ≠ **bundle congelado** de pesos combina/k̂ (`bundle_id` do M1.3). ⚠ **Correções "repo vence"** do oráculo: publica no **GitHub Releases, não S3**; `verificar` usa a régua **aposentada** `max(1 MW, 0,1%)`; `install.sh` pula hash de tarball local e faz wipe-and-replace (apagaria estado); launcher dá `exec python` (os pins de thread do launcher estão certos — `VECLIB_MAXIMUM_THREADS`, `scripts/install.sh:130`); **não há `install.ps1` nem `morning`**. A fase A/B e "Real tests" do `implement-milestone.js` usavam a régua aposentada / `run_reduce.sh` — corrigidas. ⚠ **Decisões do Gabriel ANTES do implement do M8:** D1 bucket/prefixo+HTTPS; **D2 gate de frota WSL2/GPO**; D3; D4 `morning`; D5 ε; D6 exit `9`; D7 snapshot/R+PDF; D8 amostra A/B; **D10 o corte**; D11 CI; **D12 raiz de estado `PREVCARGA_STATE`** (refino da fronteira M1/M5/M7); **D13 canal dos artefatos treinados**; **D14 superseder do ADR-0006 é pré-req HARD** do Estágio A e do `morning`; **D15 re-fit diário dos `retrain_harness` em produção** (o `forecast`/M6 é apply-only — default: o `morning` re-fita no store do cache/M7.2, nunca no registry de produção; GC no host sem dono); D16 `limiares_ab.yaml` fora do runtime. Pareamento legado deriva o sufixo `_c<HHMM>` do `cut_invariant`; o pacote de artefatos tem identidade própria `artefatos_id` (≠ `bundle_id`) e só leva membros `daily`.

## Decisões estruturantes já tomadas
- **R-only por acreção** (sem flip); Python só existe na referência, não entra no bundle final.
- **A/B contra `prevcargadessem3` é o gate de corte (M8):** Δ=0 no determinístico (inferência por-modelo,
  reconcile, exports); propriedade/qualidade na combina (leak corrigido ⇒ diverge; **sem paridade de solver**).
- **Vazamento de horizonte da combina** (treino de pesos por mês-de-origem sem subtrair horizonte,
  `combina_baseline.py:190/:271`): corrigido **só no greenfield** (M4: eixo mês-de-ORIGEM, treino de cada célula
  (lead, corte, m) só com alvos de `interval_start` anterior a início(m), solver FISTA R-base — ADR-0026 — + G2 pelo
  honesty-gate). A **referência fica intocada** (leak de propósito, baseline do A/B); o
  vazamento é documentado em `docs/reference/VAZAMENTO-HORIZONTE-COMBINA.md`. Números re-baselinam no M7;
  shift comunicado ao DESSEM já no M4. (ADR-0014)
- **Schema na leitura = falhar alto** (ADR-0004/0005): store canônico `timestamp`+`load_mwh`+`area_code`;
  coluna ausente ⇒ `stop()`, nunca `y_true=NA` silencioso; aliases legados resolvidos 1× no `data-prep`.
- **Veredito de reprodutibilidade em dois trilhos** (ADR-0017/0008/0038): **mesmo hardware, duas rodadas → |Δ|≤1e-5**;
  **máquinas Linux diferentes → |Δ|≤0,1 MW absoluto** (aperta o 1 MW+0,1% da referência); **Windows nativo vs Linux →
  ≤0,1% relativo** (ADR-0038); alvo de arquitetura diferente cai p/ limiar declarado por-alvo.
  O 0,1 MW é **meta comprometida, aferida no A/B (M8)**: se não bater na prática, afrouxa por-alvo (o 1e-5
  mesmo-hardware não muda). "acho que bate, vamos testar, qualquer coisa folgamos" (Gabriel, 2026-10-10).
- **Camadas (produto local-only):** o produto é um **pacote R local-only** + uma **casca fina de CLI** que o
  chama; o miolo (CLI + pacote) **nunca fala S3** — lê/escreve só filesystem local, determinístico por
  construção. O S3 vive **só nas pontas**: (a) download do bundle no bootstrap do installer; (b) staging
  `s3↔local` num **script de orquestração que roda em Docker, externo ao pacote** — ele chama a CLI/pacote e
  é a única coisa que toca S3. O padrão da referência "modo inferido do prefixo `s3://` dentro da CLI"
  (`_is_s3`) **não é portado**; o fan-out Batch/map-reduce sai de escopo (ADR-0016 vira referência histórica).
- **Topologia = config declarativo (`config/topology.yaml`), não hardcoded** (ADR-0001, realizado no M1.2): a
  árvore subsistema→área→perda + os 8 membros vivem num YAML que `topology.R` **carrega e valida**; áreas novas
  entram **editando o YAML**, sem tocar código (a topologia **não fixa contagem** — o seed 24 folhas/4 subsistemas
  é só baseline do A/B). Precedente: `prevcarga-R` declarava áreas como lista YAML (`data.areas:`). Consequência:
  `validate-deck` (M2) cruza — `area_code` no deck sem declaração no config = fail-high.
- **Config por-modelo = `run.yaml` enxuto, não `job.json`** (ADR-0021): o `job.json` da referência é manifesto de
  AWS Batch com modelagem baked-in. O clean-room porta só o contrato (`split`, `granularity_min`, `modes`, `seed`,
  `horizon_days`, `history_days`, `backtest.protocol_default`, `constraints`, `params`) num `models/<m>/run.yaml`; **não**
  porta `resources`(Batch)/`r_deps`/`entrypoint` (ADR-0016/0018/M8). Override por flag só de chave declarada (senão exit 8);
  `infer-model` é read-only nos params; proveniência carimba o efetivo (ADR-0021/0032).
- **Exit-codes da CLI por classe** (ADR-0040): tabela única 0–15 (só cresce no fim), texto ASCII e JSON único; catálogo
  distribuído em `scripts/lib/errors/<modulo>.tsv`.
- **`registry.R`:** fronteira de contrato de tiers no **M1** (ADR-0010, fail-loud wire-time); ciclo-de-vida no **M5**.
- **Registry de modelos = dois tiers, mas todos contract-verified** (ADR-0010): Tier A (R6, declara
  `requires`/`provides`, verificado no `register`, fail-loud em wire-time) = **RF, LGBM, HW e DESSEM**; Tier B
  vendored-lazy = **vazio/reservado** (só habitaria externo de terceiro, que não temos). O DESSEM é **código
  nosso** (pacote `prevcargadessem`, não vendor — ADR-0012), por isso declara contrato como os demais.
  Fronteira num ponto único do `registry.R` (M1).
- **Combina (ADR-0013):** identidade dos 8 membros por **manifesto declarativo** (não glob+regex; rename ⇒
  erro); pesos no eixo-corte intradiário **re-fitados por corte** (não interpolados), corte honesto por mês de
  origem (ADR-0026) + piso EQW; em cada lead entram só os membros cuja faixa `delivery_days` o cobre. (M4)
- **Ciclo de artefato (ADR-0011/0023):** mantêm-se **duas classes** (`origin_agnostic` RF/LGBM reusam artefato;
  `origin_scoped` DESSEM/HW têm 1 re-treino por dia, chaveado pela origem), e a classe é o campo `split` do
  `run.yaml` — fonte única, elimina a lista hardcoded `.refit_per_origin`. `protocol` é só protocolo de backtest (M7). (M5)
- **Alinhamento temp↔carga = invariante de dado; ajuste de hora centralizado** (ADR-0012/0019/0035; Gabriel,
  2026-10-27): o rótulo da carga é resolvido só no leitor único; o deslocamento de modelagem é aplicado pelo
  despachante da fundação com `to_model_frame`/`to_display_frame` (∓`granularity_min`), pelo offset declarado no
  descriptor (DESSEM 60, demais 0). Nenhum modelo, motor ou adapter faz aritmética de hora; DESSEM com Δ=0. **LGBM**
  faz join contemporâneo no mesmo slot sem shift → **spike de avaliação no M5.10**; se confirmar, a correção é fatia
  pós-M5 pelo mesmo offset. **RF** (temp diária) e **HW** (univariado) não fazem join sub-diário → inalterados.
- **Deck canônico de entrada (ADR-0004/0005/0002/0003):** contrato completo em `docs/reference/DECK-CANONICO.md`
  (6 datasets Hive `area_code=/year=`, colunas Required/Optional, tabela de alias legado→canônico, matriz
  "quem lê o quê"). Validado pelo `validate-deck` (M2), falha alto por origem. **Achado:** a referência lê um
  **subconjunto** do que o store `prevcarga-R/data` oferece com nomes **já padronizados** — não há renomeação
  a fazer nos modelos; a padronização é só a camada de alias no ingest cru. `load_tiers`/`dst_periods`/
  `heat_index`/`rh` ficam **optional** (ninguém lê hoje).
- **Clima previsto = emissão no instante cheio (ADR-0003):** `issue_time` é `timestamp`, suporta **4 rodadas/dia
  (00/06/12/18Z)** — **não** achatar pra data (achatamento legado matava 06/12/18Z). Regra de dia-de-origem +
  horizonte UTC-safe → M5. Habilita corte intradiário de clima (ADR-0007).
- **Empacotamento:** bundle R-only hospedado no S3; `install.sh`/`install.ps1` baixam o mesmo bundle (mesma
  versão p/ todos). **Nenhuma dep de S3 (paws/aws CLI) entra no pacote** — o produto é S3-agnóstico.

## O que já existe no repo (esqueleto)
- `CLAUDE.md`, `docs/_archive/roadmap/` (OBJETIVO + PLANO + COMBINA-TECNICO-M4), este `ESTADO-ATUAL.md`.
- `.claude/` (agents prevcarga-*, commands consolidate/tidy, workflows modularize-* e verify/implement-
  milestone adaptados), `.githooks/` (pre-commit/pre-push).
- `scripts/lib/` — **stubs** das fundações (runtime/topology/provenance/dates/validation) marcados com o milestone
  que os implementa; `evaluation.R`/`honesty_gate.R` **VERIFY-READY pelo M3**, `combination.R` **VERIFY-READY pelo M4**
  e `features.R`/`training.R`/`inference.R`/`registry.R`-ciclo **VERIFY-READY pelo M5**,
  e `build_long_output.R`/`exports.R`/`reconcile.R`/`reporting.R`/`forecast.R` **VERIFY-READY pelo M6**,
  e `orchestrate.R`/`cache.R`/`backtest.R` (+ `reporting.R` estendido c/ o conteúdo de acurácia) **VERIFY-READY pelo M7** (ainda `CRIAR`/stub até o
  `implement`, mas o contrato está congelado em `M3-AVALIACAO.md`/`M4-COMBINA.md`/`M5-TRAINING.md`/`M6-FORECAST.md`/`M7-ORQUESTRACAO.md`).
  `scripts/prevcarga` — stub da fachada docopt (verbo `combine`=M4; `train-models`/`infer-model`/`registry`=M5; `forecast`+alias `prever`/`export`/`reconcile`/`features`=M6; `backtest`/`plan`/`--dry-run`=M7).

## Pendências de arranque
- Ativar hooks após 1º commit/remote (`git config core.hooksPath .githooks`).
- Implementação pelo work-session loop do SpecForge (`/sf-start-resume-implementation`); onda 1 = M1.4b. Os workflows
  de milestone (`verify-milestone`/`implement-milestone`/`milestone-canvas`) foram apagados em 2026-10-27.

## Invariantes herdados (da referência, a preservar)
- **Schema LONG (8 col):** area, forecast_origin, target_datetime, lead, y_pred, y_true, d_plus, granularity_min.
- **Corte honesto de treino por classe de split (ADR-0023):** `origin_agnostic` `train_origin = O − 1 − H`;
  `origin_scoped` `train_origin = O` (1 re-treino por dia); `last_data_day = O − 1 − data_lag_days` (LGBM O−3).
  Nada do dia O em diante entra no treino; o harness de backtest só trunca o tempo.
- **Determinismo:** TZ/locale/BLAS 1-thread fixados DENTRO do processo (independente do shell, ADR-0037) + semente do `run.yaml`; gates Δ=0 falsificáveis.
- **Completude DESSEM:** ≥24 slots/dia (:00 e :30). O "pré-shift +1h" **não** é quirk do DESSEM — é a
  implementação da alocação da integral de carga no fim do intervalo (invariante de sistema, ADR-0019),
  aplicado pelo despachante da fundação via `to_model_frame`/`to_display_frame`, nunca dentro do motor.
