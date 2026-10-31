# Estado: A/B do corte contra prevcargadessem3 (M8.8; ADR-0034 D23, ADR-0035, ADR-0036, ADR-0025)

Ferramentas DEV-ONLY (so Linux, fora do bundle) em `scripts/tools/ab_*`; dados e listas em `tests/ab/`. O oraculo e validado UMA vez, neste A/B do
corte, e depois descartado: sem periodo paralelo nem comparacao recorrente (ADR-0034). Erros: `scripts/lib/errors/ab.tsv`.

## Pecas
- `tests/ab/ab_origens.yaml` + `scripts/tools/ab_cases.R`: `pc_ab_cases(origens, deck_dir, ctx, oracle, membros, areas)` monta a matriz (membro x area) de
  `prevcarga_members()`/`prevcarga_topology()` (nunca lista fixa), le cada deck pelo leitor unico (`prevcarga_read_load(truncate_at = ...)`), exige
  hora cheia e igual a `available_until_esperado` (`contract.ab_deck_not_full_hour`), deriva o `corte` do ORACULO como `available_until - 30 min` pela
  borda do LONG (`prevcarga_long_interval_start`, ADR-0035 item 8) e le o `protocolo` do `job.json` do oraculo (nunca forcado). `cases.tsv`
  (`pc_ab_case_cols`): caso, membro, sigla, mdir, exogena, area, origem, deck, tz, available_until, corte_oraculo, protocolo, split, delivery_days,
  executavel, motivo. Caso cujo corte do oraculo cai na vespera de O (`available_until = O 00:00`) e `executavel = false` (o `run_job.sh` so aceita
  corte como hora do dia O): o regdin 00:00 nao e medido pelo runner. `pc_ab_store_overlay`: o store real nao tem `manifest.json`; o overlay e um
  diretorio NOVO com symlinks e o manifest de `config/deck-manifest.store.json` (recusa diretorio existente: `file.symlink` num destino que e link de
  diretorio cria o link DENTRO do alvo -- ver Incidente).
- `scripts/tools/ab_oracle_run.sh <cases.tsv> <ref_out> --data <store> [--runtime docker|host]`: checksum da arvore do oraculo (inclusive
  `models/*/models/` e `.git`) antes/depois, copia `rsync -a` sem `data/`, `models/*/models/` e `.git/` num `mktemp -d`, `aws` FALSO no PATH (grava em
  `aws.log` e sai 1), runtime proprio construido com o `ci/build_marmita.sh` DA COPIA num container `ubuntu:22.04` (jammy, descartavel; mounts: copia rw,
  `--data` `:ro`, `ref_out` rw), cada caso em modo LOCAL (`LOCAL_DATA_DIR`/`LOCAL_RESULTS_DIR`, `CUT` = hora de `corte_oraculo`, `PROTOCOL` do
  `job.json`, janela `FROM = O-1 .. TO = O`; EXOGENA so como variavel do filho), `ref/A/{index.tsv,manifest.sha256,results/...}`, e no fim reconfere
  checksum, `git status --porcelain` (com `GIT_OPTIONAL_LOCKS=0`, sem travar o indice) e `aws.log` vazio: diferenca => exit 3. O
  `git status` do oraculo real tem 3 `docs/roadmap/*.md` e `verify-log/` NAO rastreados (estado conhecido, ESTADO-ATUAL/contexto 2026-10-30):
  `tests/ab/oracle_untracked_allow.txt` lista exatamente esse conjunto; qualquer outra linha reprova. `--runtime host` (so depuracao: exige
  `--rscript`/`--rlib`). `--b-comb/--b-load/--b-subsystem/--b-window`: estagio B, o `reconcilia_baseline.R` do oraculo em modo `full` na copia.
- `scripts/tools/ab_relayout.R`: `pc_ab_relayout(ref_dir, out_dir, ctx, relabel = TRUE)` converte o LONG do oraculo (rotulo de INICIO, digitos civis
  sem fuso, `forecast_origin` Date) para o LONG nativo de 8 colunas: declara o fuso da carga, re-rotula para `interval_end` (+granularity_min), recalcula
  `lead = d_plus*48 + slot(interval_start)` (mesma regra do `build_long_output.R`), grava `<out>/<origem>/membros/<sigla>/<area>.parquet` e REGERA
  `manifest.json`. `relabel = FALSE` so para o CONTROL. O layout do oraculo nunca entra no produto.
- `scripts/tools/ab_stages.R`: linhas `pc_ab_row_cols` (estagio, membro, mdir, sigla, exogena, area, origem, available_until, tz, plataforma, modo,
  khat_origin, delta_max, delta_rel, n, status PASS|FAIL|EXCECAO|REPORT, excecao_id, detalhe).
  - A (`pc_ab_stage_a`): por caso, grade `prevcarga_delivery_grid` (inicio `available_until + 30`, fim por `delivery_days` do descriptor); clean-room =
    LONG do membro na origem O; oraculo = por instante, o passo da origem O quando existe e senao o da origem O-1 (RF/HW: o passo que o oraculo
    calcula e descarta; dessem em corte intradiario: o proprio O; a coluna `detalhe` traz `fonte_dia_O`). Chave sem par => FAIL (nunca Delta = 0
    silencioso); entrada ausente => FAIL; instante do clean-room fora da faixa e contado em `fora_da_faixa=` (nao comparado); pedir um dia alem da faixa
    (`request_end`) => `contract.ab_fora_da_faixa`. Antes dos valores roda `pc_vr_integrity` (fase 1 do `verificar`) nas duas arvores. Limiar `1e-5`
    (`limiares_ab.yaml`). O comparador de valores NAO chama `prevcarga_verify_repro` inteiro: o oraculo nao tem identidade (`run_manifest.json`),
    entao so a fase de integridade e os helpers de valor do `verify_repro.R` sao reusados.
  - B (`pc_ab_stage_b`): `prevcarga_khat_fit(ratio, O, window)` x perfil do oraculo (hour x month x dow, rotulo de inicio) `|D| <= 1e-5`; bucket sem par
    => FAIL. Apply de producao: `khat_origin >= O` (politica atual, Gabriel 2026-10-29: `khat_origin == O` sempre) compara o LONG ajustado e
    exige `<= 1e-5`; `khat_origin < O` (perfil reutilizado) vira linha `EXCECAO` com `khat_origin`, `khat_age_days`, `n_fallback` SO se a lista fechada a
    conhece, senao FAIL. Ver Discoveries.
  - C (`pc_ab_stage_c`): conjunto de arquivos e sha256 (do DISCO, nao do manifest) de `SIN.parquet`, `subsistemas/`, `folhas/` entre duas entregas.
  - D (`pc_ab_stage_d`): so `prevcarga_combina_gate(...)$pass` (propriedade: simplex + gradiente); sem `eps`, sem qualidade/dominancia, sem paridade de solver.
  - E (`pc_ab_stage_e`): `prevcarga_scorecard` por d_plus x area, status `REPORT`; nunca entra no veredito.
  - `pc_ab_clean_run(cases, ctx, out_dir)`: produtor do clean-room (fachada `prevcarga_train` + `prevcarga_infer_cell(truncate_at = au)` na regra
    `origin_midnight_utc` + `prevcarga_build_long`). Exige as libs no `globalenv()` (os plugins resolvem a API da fundacao la; busca interina pinada).
  - `pc_ab_load_excecoes`/`pc_ab_match_excecao`/`pc_ab_match_all`/`pc_ab_load_limiares`.
- `scripts/tools/ab_report.R`: `pc_ab_verdict(linhas, excecoes, recibos)` -> `CORTE-OK` sse A, B, C, D tem linhas e nenhum FAIL (estagio sem linha =
  `estagio_<E>_ausente`), >= 1 membro em Delta = 0 (todas as celulas dele PASS no A), toda linha `EXCECAO` casa com a lista pela condicao (reavaliada
  sobre o proprio caso) e os recibos PASS `cross-platform:linux`, `cross-platform:windows` (M8.9), `windows-vm` (M8.11) e `nfr-30min` (M8.10) existem
  (`pc_ab_recibos_collect` le por sha do bundle via `pc_receipt_read`). Senao `CORTE-BLOQUEADO` com os motivos EXATOS. `pc_ab_report_write` grava
  `ab_veredito.json` + `ab_veredito.md` (ASCII) e, so com `bundles`, o recibo `ab-corte` por bundle (`pc_receipt_write`).

## Lista fechada de excecoes (`tests/ab/ab_excecoes.yaml`) -- dono: Gabriel (ADR-0034 D23)
Campos obrigatorios `id, membro, estagio, condicao, justificativa, adr`; opcionais `limite {max_abs, max_rel}` (`max_rel = max|D| / max|ref|`),
`janela_final_h`, `decisao`, `fonte`. Carregador estrito: campo ausente, desconhecido, `condicao` vazia/generica (sem campo do caso, constante) ou com
funcao fora da whitelist, estagio `E`, id duplicado => `contract.ab_excecoes_bad`. Casa so por membro + estagio + condicao (+ teto/janela).
Semeada SO com decisoes ja tomadas e citadas (`decisao`, `fonte`):
1. `regdin.versao_pacotes` -- dessem-regdin, A, `max_rel 1.3e-5` (ADR-0036; `docs/estado/m1-spike-r453.md`).
2. `dessem_linear.hora_incompleta_clima_previsto` -- dessem-linear, A, so as ultimas 3 h da faixa (Gabriel 2026-10-30; `docs/estado/dessem_linear.md`).
3. `dessem_linear.sp_2024_11_15_residuo_numerico` -- dessem-linear, A, SP em 2024-11-15, tetos `max_abs 0.5`, `max_rel 5e-5` (DL9-GAP-2; medido 0,446 MW).
NAO semeadas (sem decisao registrada do Gabriel; cada uma e um item para ele): `reconcile.apply_profile_reuse`, `regdin.mm_horas_completas`,
`regdin.sem_manha_0000`, `regdin.mxmn_recusado`, plano B do RNG. As citadas no AC 21 do ticket existem como fixture so nos testes de injecao
(`ab_fixture_excecoes` em `tests/ab/helper-ab.R`). Qualquer divergencia nova: FAIL na celula + discovery com os numeros, nunca entrada nova.

## Testes (`tests/ab/`, `TZ=UTC LC_ALL=C.UTF-8 Rscript --vanilla -e 'testthat::test_dir("tests/ab", filter=...)'`)
`test-ab-oracle-intact.R` (oraculo FALSO em tempdir: checksum, git status, aws.log, corte = au - 30 min; CONTROLs de toque em `models/*/models/`, `aws s3 ls`,
arquivo novo), `test-ab-stages.R` (A com FORBIDDEN e CONTROLs +0,2 MW / sem re-rotulagem / regra de clima no deck 06Z / faixa e `fora_da_faixa`; B; C; D; E x10),
`test-ab-verdict-injection.R` (um FAIL por vez em A, B, C, D e perna Windows; fora da lista; 00:00 em deck com manha; 0 membros; E x10; carregador estrito;
relatorio ASCII e recibos), `test-ab-corte.R` (pipeline real sobre a referencia congelada; skip explicito sem ela). Fixtures em tempdir, nunca `data/`.

## Medido nesta sessao (subconjunto representativo, SP x 2024-10-01 09:00)
Referencia gerada de verdade por `ab_oracle_run.sh --runtime docker` (container `ubuntu:22.04`, `ci/build_marmita.sh` da copia: R 4.2.3 + renv.lock do
oraculo, build ~3 min; runtime em `runtime.txt`), 3 casos (RF, LGBM, HW) x SP x origem 2024-10-01, `available_until` 09:00, corte do oraculo 08:30,
protocolo do `job.json` (RF/LGBM `daily`, HW `retrain_harness`), janela O-1..O. Oraculo: checksum identico antes/depois, `git status --porcelain` = so o
conjunto nao rastreado conhecido (lista permitida), `aws.log` vazio (0 chamadas). Clean-room: `pc_ab_clean_run` em R 4.5.3 sobre o overlay do store, regra
`origin_midnight_utc`. Relayout + estagio A reais (`test-ab-corte.R` com options): o pipeline roda de ponta a ponta e o veredito e `CORTE-BLOQUEADO`:

| membro | instantes | max abs D (MW) | max rel | leitura |
|---|---|---|---|---|
| random-forest | 414 | 145,10 | 6,2e-3 | origem O (8 dias, 384 instantes): D = 0 EXATO; os 30 instantes do dia O (passo da origem O-1 do oraculo): 145 MW |
| lgbm | 78 | 1620,46 | 6,6e-2 | origem O inteira (O..O+1): mediana 197,6 MW |
| holt-winters | 414 | 1146,00 | 4,9e-2 | origem O (384): max 528 MW, mediana 43,5 MW; dia O (passo O-1) pior |

Nenhuma excecao da lista cobre isso (nenhuma e do RF/LGBM/HW): FAIL nas tres celulas, Estagios B, C, D ausentes (sem entradas reais) e recibos ausentes.
O RF no Delta = 0 da origem O prova que o relayout (rotulo, lead, chaves) esta certo. Hipoteses (NAO verificadas, ver discovery): (i) RF dia O: o
`daily` do oraculo re-treina por origem (O-1 em O-12) e o clean-room entrega o dia O com o artefato de O (O-10), enquanto os goldens de paridade usam o
MESMO artefato (`train` em O-10 + `infer` em O-1 e O) -- o `run_job.sh` sem forcar o protocolo nao isola esse passo; (ii) LGBM/HW: os goldens de
paridade rodam o oraculo por driver (SECO/areas de subsistema, `available_until` proprio, `history_days` literal) e nao por `run_job.sh`; nenhuma
paridade de area FOLHA (SP) contra `run_job.sh` foi medida antes. Reproduzir: `ref_out`, `clean` e `cases-sub.tsv` desta sessao ficaram no scratchpad
(nao versionados; dados reais nao entram no git).

## Pendencias (nao fabricadas)
- A/B COMPLETO (todas as areas folha x membros x origens de `ab_origens.yaml`): pesado (oraculo por caso = treino + infer de 2 origens). Comando:
  1. `Rscript --vanilla -e 'source(...); cs <- pc_ab_cases("tests/ab/ab_origens.yaml", NULL, ctx); pc_ab_cases_write(cs, "cases.tsv")'` (ctx com o overlay do store);
  2. `bash scripts/tools/ab_oracle_run.sh cases.tsv <ref_out> --data /opt/source/prevcarga-R/data --runtime docker --allow-untracked tests/ab/oracle_untracked_allow.txt`;
  3. `pc_ab_clean_run(cs, ctx, <clean>)` (R 4.5.3, libs no globalenv); 4. `options(prevcarga.ab.ref=, prevcarga.ab.clean=, prevcarga.ab.cases=, prevcarga.ab.stages=, prevcarga.ab.bundles=)` e `test_dir("tests/ab", filter="ab-corte")`.
- Estagios B/C/D/E sobre dados reais: o runner tem o gancho `--b-*` e os comparadores estao provados em sintetico; falta montar as entradas reais
  (folhas combinadas + `ratio` da verdade) -- sem elas o veredito sai `CORTE-BLOQUEADO` por `estagio_<E>_ausente`, nunca OK.
- Recibos das pernas Linux/Windows (M8.9), VM limpa (M8.11) e NFR (M8.10) por sha do bundle: sem eles `recibo:<gate>` bloqueia. Windows deferido ao gate da VM.
- Estagio A do regdin em `available_until` = 00:00 (sem manha): o runner nao o executa (corte do oraculo na vespera); decisao do Gabriel pendente.

## Incidente (corrigido) e Discoveries
- Incidente: uma execucao minha de `pc_ab_store_overlay` (versao inicial, sem a guarda de diretorio novo) criou 4 symlinks DENTRO do store canonico
  (`data/load/load`, `data/auxiliary/holidays/holidays`, `data/weather/forecast/forecast`, `data/weather/observed/observed`). Removidos na hora (so os 4
  links, `rm` de link, nenhum dado tocado); a funcao agora recusa diretorio existente. Ver `find /opt/source/prevcarga-R/data -type l` = so `data/data`.
