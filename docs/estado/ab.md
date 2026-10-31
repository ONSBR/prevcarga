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
- `scripts/tools/ab_oracle_run.sh <cases.tsv> <ref_out> --data <store> [--runtime docker|host] [--patches <dir>]`: checksum da arvore do oraculo
  (inclusive `models/*/models/` e `.git`) antes/depois, copia `rsync -a` sem `data/`, `models/*/models/` e `.git/` num `mktemp -d`, patches de
  `--patches` (default `scripts/tools/ab_oracle_patches/*.patch`) aplicados com `patch -p1` SO na copia (cabecalho `# ab-patch: mdir=<m>
  fonte_dia_O=<f>`; listados com sha256 em `<ref_out>/patches.tsv`; sem cabecalho, nao aplica ou sem o dia O => `contract.ab_oracle_patch`, exit 1),
  `aws` FALSO no PATH (grava em `aws.log` e sai 1), runtime proprio construido com o `ci/build_marmita.sh` DA COPIA num container `ubuntu:22.04`
  (jammy, descartavel, com `jq` instalado; mounts: copia rw, `--data` `:ro`, `ref_out` rw). Cada caso roda POR ORIGEM, com o modo pela classe de
  split do clean-room (ADR-0023):
  - `job` (origin_scoped: HW, dessem): `run_job.sh` em modo LOCAL com `FROM = TO = O`, `CUT` = hora de `corte_oraculo`, `PROTOCOL` do `job.json`;
    EXOGENA so como variavel do filho; `AB_DAY0_DIR` para o sidecar do patch.
  - `split` (origin_agnostic: RF, LGBM): o `run_pipeline.R` da copia direto, como os goldens (`plugin_golden.R` + `models/*/tests/parity/oracle_driver.R`):
    `--mode train` UMA vez em `train_origin = O-1-H` (`history_start = train_origin - history_days.train`, `train_args` do `job.json`), depois
    `--mode infer` em O-1 e em O com o MESMO `--artifact` e o MESMO `history_start` absoluto `O - history_days.infer`, `--available-until "<dia> <CUT>"`,
    `PREVCARGA_JOB=1` e o `env{}` do `job.json` exportado pelo runner (jq).
  Todo caso confere no proprio log que o `env{}` do `job.json` foi exportado: linha `jq absent` ou chave sem `manifest env: export K=`/`K ja setado`
  => `contract.ab_oracle_env`, caso FAIL (exit 1). `index.tsv` = caso, sigla, mdir, exogena, area, origem, result (LONGs separados por `;`), rc, modo,
  fonte_dia_O. No fim reconfere checksum, `git status --porcelain` (com `GIT_OPTIONAL_LOCKS=0`) e `aws.log` vazio: diferenca => exit 3. O `git status`
  do oraculo real tem 3 `docs/roadmap/*.md` e `verify-log/` NAO rastreados (estado conhecido): `tests/ab/oracle_untracked_allow.txt` lista exatamente
  esse conjunto. `--runtime host` (so depuracao: exige `--rscript`/`--rlib`). `--b-*`: estagio B, o `reconcilia_baseline.R` do oraculo na copia.
- `scripts/tools/ab_oracle_patches/hw_keep_step1.patch` (decisao do Gabriel 2026-10-31: descartar o passo 1 do fit em O esta errado; o A/B compara o
  dia O do clean-room com o passo 1 DO PROPRIO oraculo, via copia patchada): `hw_infer` guarda o passo `first_idx - 1` de `$mean` (o dia O) pela
  mesma cadeia do horizonte (assemble + LOESS 0,13 se `p_Suaviza`; SemiFer NAO, o `horizon_meta` do oraculo comeca em O+1: um dia O feriado/semi
  divergiria e REPROVA, nunca passa calado) e o `run_pipeline.R --mode forecast` grava esse dia como LONG proprio (`first_forecast_day = O`) em
  `$AB_DAY0_DIR/<area>_<origin>.parquet`; o runner o poe ao lado do LONG do harness (`day0_<O>.parquet`, que fica byte-igual ao do oraculo sem patch).
- `scripts/tools/ab_relayout.R`: `pc_ab_relayout(ref_dir, out_dir, ctx, relabel = TRUE)` converte o LONG do oraculo (rotulo de INICIO, digitos civis
  sem fuso, `forecast_origin` Date) para o LONG nativo de 8 colunas: declara o fuso da carga, re-rotula para `interval_end` (+granularity_min), recalcula
  `lead = d_plus*48 + slot(interval_start)` (mesma regra do `build_long_output.R`), grava `<out>/<origem>/membros/<sigla>/<area>.parquet` e REGERA
  `manifest.json`. `result` com varios LONG (`;`) e re-rotulado arquivo a arquivo e unido (instante repetido => `contract.ab_stage_input`); a coluna
  `fonte_dia_O` do index vai para `<out>/fonte_dia_O.tsv` (raiz). `relabel = FALSE` so para o CONTROL. O layout do oraculo nunca entra no produto.
- `scripts/tools/ab_stages.R`: linhas `pc_ab_row_cols` (estagio, membro, mdir, sigla, exogena, area, origem, available_until, tz, plataforma, modo,
  khat_origin, delta_max, delta_rel, n, status PASS|FAIL|EXCECAO|REPORT, excecao_id, detalhe).
  - A (`pc_ab_stage_a`): por caso, grade `prevcarga_delivery_grid` (inicio `available_until + 30`, fim por `delivery_days` do descriptor); clean-room =
    LONG do membro na origem O; oraculo = por instante, o passo da origem O quando existe e senao o da origem O-1 (RF: infer em O-1 com o
    MESMO artefato, modo split; HW: o passo 1 do fit em O pelo patch; LGBM e dessem: o proprio O). `detalhe` traz `fonte_dia_O` (de qual origem) e
    `oraculo_dia_O` (do `fonte_dia_O.tsv` do runner: `O-1:mesmo_artefato(train_origin=...)` no modo split -- inclusive no LGBM, cujo dia O e o nativo
    de O --, `O:hw_keep_step1.patch:...` no HW). Chave sem par => FAIL (nunca Delta = 0
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
`test-ab-oracle-intact.R` (oraculo FALSO em tempdir: checksum, git status, aws.log, corte = au - 30 min; modo job POR ORIGEM (`FROM = TO = O`) e modo
split como os goldens (1 train em O-1-H + infer O-1 e O, mesmo artefato e history_start, env{} e PREVCARGA_JOB no filho), com CONTROLs de dentes
(janela O-1..O e re-treino por origem reprovam); CONTROLs de toque em `models/*/models/`, `aws s3 ls`, arquivo novo, `jq absent` no log e env{} sem
export (`contract.ab_oracle_env`); patch so na copia + `patches.tsv` + sidecar do dia O, e CONTROLs de patch sem cabecalho/que nao aplica/sem dia O), `test-ab-stages.R` (A com FORBIDDEN e CONTROLs (+ relayout de varios LONG por caso com `oraculo_dia_O` no detalhe; instante repetido reprova) +0,2 MW / sem re-rotulagem / regra de clima no deck 06Z / faixa e `fora_da_faixa`; B; C; D; E x10),
`test-ab-verdict-injection.R` (um FAIL por vez em A, B, C, D e perna Windows; fora da lista; 00:00 em deck com manha; 0 membros; E x10; carregador estrito;
relatorio ASCII e recibos), `test-ab-corte.R` (pipeline real sobre a referencia congelada; skip explicito sem ela). Fixtures em tempdir, nunca `data/`.

## Medido: SP x 2024-10-01, available_until 09:00 (RF, LGBM, HW)
1a rodada (M8.8; runner antigo: janela `FROM = O-1 .. TO = O`, sem jq no container) => `CORTE-BLOQUEADO` com FAIL nas tres celulas: RF 414 instantes
max 145,10 MW (origem O exata; os 30 do dia O divergiam), LGBM 78 max 1620,46 MW (mediana 197,6), HW 414 max 1146,00 MW. Investigacao (2026-10-31):
os tres eram artefatos da COMPARACAO, nenhum defeito do clean-room:
- LGBM: o container nao tinha `jq`; o `jobs/job_lib.sh:136-138` do oraculo logou `[job_lib][WARN] jq absent; skipping manifest env{} export` e o
  `LGBM_DISABLE_LOESS=1` do `models/lgbm/job.json` nao foi aplicado. Com LOESS forcado no clean-room a diferenca caiu a 1,8e-11.
- HW origem O: no `retrain_harness` com janela `FROM = O-1`, o `run_job.sh:459` fixa o history-start da janela inteira em `FROM - 730`: a origem O
  era ajustada com 731 dias (o clean-room usa 730). Por origem (`FROM = TO`) o historico e o mesmo; controles deram Delta = 0.
- RF dia O: o estagio A tirava o dia O do passo da origem O-1 do `daily`, que re-treina por origem: artefato com `train_origin` 2024-09-20 (= O-11;
  esta nota dizia O-12, errado) x 2024-09-21 (O-10) do clean-room. Os goldens usam `--mode train` em O-1-H e `--mode infer` em O-1 e O com o MESMO
  artefato.
- HW dia O: o clean-room entrega o dia O pelo passo 1 do fit em O (`models/holt-winters/R/training.R`, `h_fit = H+1`); o oraculo calcula esse passo
  e o DESCARTA, entao nenhuma saida dele o tinha (o passo 2 da origem O-1 nao e o mesmo numero). Decisao do Gabriel (2026-10-31): descartar e errado;
  a referencia do dia O e o passo 1 do proprio oraculo em O (patch na copia), nem o passo 2 de O-1 nem um REPORT.

2a rodada (este runner; `ref_out2` + `clean2` no scratchpad, nao versionados): oraculo R 4.2.3 (docker, `ci/build_marmita.sh` da copia, jq presente),
checksum da arvore igual antes/depois (`6e951c10...`), `git status` = so o conjunto conhecido, `aws.log` vazio, `patches.tsv` = `hw_keep_step1.patch`
(sha256 `eda5fbc7...`). Logs: `manifest env: export LGBM_DISABLE_LOESS=1` (+ `LGBM_CALENDAR_YEAREND`, `LGBM_TRAIN_CV`); RF `train_origin` 2024-09-21,
LGBM 2024-09-29. Clean-room R 4.5.3 (`pc_ab_clean_run`, `origin_midnight_utc`). Estagio A, limiar 1e-5:

| membro | modo | instantes | dia O (n, max abs D) | O+1.. (n, max abs D) | status |
|---|---|---|---|---|---|
| random-forest | split | 414 | 30, 0 (O-1 do mesmo artefato) | 384, 0 | PASS |
| lgbm | split | 78 | 30, 0 (nativo de O) | 48, 0 | PASS |
| holt-winters | job + patch | 414 | 30, 0 (passo 1 do fit em O) | 384, 0 | PASS |

Delta = 0 EXATO (nao so <= 1e-5), sem excecao. `test-ab-corte.R` sobre essa referencia: 3 membros em Delta = 0; veredito `CORTE-BLOQUEADO` so por
`estagio_{B,C,D}_ausente` e pelos recibos das pernas (pendencias abaixo).

## Pendencias (nao fabricadas)
- A/B COMPLETO (todas as areas folha x membros x origens de `ab_origens.yaml`): pesado (oraculo por caso = treino + infer de 2 origens). Comando:
  1. `Rscript --vanilla -e 'source(...); cs <- pc_ab_cases("tests/ab/ab_origens.yaml", NULL, ctx); pc_ab_cases_write(cs, "cases.tsv")'` (ctx com o overlay do store);
  2. `bash scripts/tools/ab_oracle_run.sh cases.tsv <ref_out> --data /opt/source/prevcarga-R/data --runtime docker --allow-untracked tests/ab/oracle_untracked_allow.txt`
     (o store entra `:ro`; o runner nunca escreve nele);
  3. `pc_ab_clean_run(cs, ctx, <clean>)` (R 4.5.3, libs no globalenv); 4. `options(prevcarga.ab.ref=, prevcarga.ab.clean=, prevcarga.ab.cases=, prevcarga.ab.stages=, prevcarga.ab.bundles=)` e `test_dir("tests/ab", filter="ab-corte")`.
- Estagios B/C/D/E sobre dados reais: o runner tem o gancho `--b-*` e os comparadores estao provados em sintetico; falta montar as entradas reais
  (folhas combinadas + `ratio` da verdade) -- sem elas o veredito sai `CORTE-BLOQUEADO` por `estagio_<E>_ausente`, nunca OK.
- Recibos das pernas Linux/Windows (M8.9), VM limpa (M8.11) e NFR (M8.10) por sha do bundle: sem eles `recibo:<gate>` bloqueia. Windows deferido ao gate da VM.
- Estagio A do regdin em `available_until` = 00:00 (sem manha): o runner nao o executa (corte do oraculo na vespera); decisao do Gabriel pendente.

## Incidente (corrigido) e Discoveries
- Incidente: uma execucao minha de `pc_ab_store_overlay` (versao inicial, sem a guarda de diretorio novo) criou 4 symlinks DENTRO do store canonico
  (`data/load/load`, `data/auxiliary/holidays/holidays`, `data/weather/forecast/forecast`, `data/weather/observed/observed`). Removidos na hora (so os 4
  links, `rm` de link, nenhum dado tocado); a funcao agora recusa diretorio existente. Ver `find /opt/source/prevcarga-R/data -type l` = so `data/data`.
