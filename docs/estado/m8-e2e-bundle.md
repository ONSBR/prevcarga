# Estado: ponta a ponta no bundle instalado (M8.9; ADR-0036, ADR-0037, ADR-0038, ADR-0039, ADR-0040)

## O que e
Testes que exercitam o bundle Linux REAL (`scripts/tools/build_bundle_linux.sh`, container jammy) INSTALADO por `install/install.sh` numa raiz temporaria,
sempre pelo lancador (`<R>/bin/prevcarga`, `processx::run`), nunca pelo Rscript da arvore de dev: `tests/install/test-load-label-e2e.R`,
`tests/install/test-oficial-e2e.R`, `tests/ab/test-cross-platform.R`. Helper: `tests/install/helper-e2e.R` (usa `helper-install.R`/`helper-sync.R`).
Nada fala com bucket real nem com a rede: o "bucket" e um diretorio local montado pelo `release-pack` REAL e servido por httpuv; o `prevcarga-sync` instalado o baixa.
Tudo roda em `sandbox/` (gitignored). Sem bundle em `dist/` os bracos fazem skip com motivo (`pci_skip_unless_ready`), como os e2e do M8.3.

## O mundo de teste (`pc_e2e_world()`, `pc_e2e_world_end()`)
- Bundle candidato de `dist/` reempacotado com UMA mudanca: `config/topology.yaml` de fixture (RF, LGBM, HW; SECO = SP, RJ; S = RS, PR) e `RUNTIME.json` regerado.
  Motivo: a topologia real (8 membros, ~28 areas) levaria horas para treinar. Runtime, library e codigo sao os do build.
  Areas REAIS (SP, RJ, RS, PR): o LGBM so aceita as areas da tabela `heat_area` do descriptor. Deck: `fixture_store.R`, 800 dias ate O = 2026-10-15 09:00.
- Treino real pelo lancador (`train-models` RF e LGBM, origin_agnostic); o HW (origin_scoped) treina dentro do `forecast`. k-hat (reconcile real) e pesos da combina
  vem do codigo da arvore de dev num `Rscript` filho (`pce_prepare_combina`, fixtures do M6 `helper-m6-fixtures.R`: o treino da combina pede painel de varias origens).
- Rodada OFICIAL (`forecast` pelo lancador) + `release-pack --what=artifacts|forecasts --latest` do lancador => bucket; `prevcarga-sync` instalado => estado local
  (artefatos) e estado oficial (inferencia).
- O artefato carrega o `load_label` do treino e o forecast recusa deck de outro rotulo (`contract.load_label_mismatch`): a perna `end` tem mundo proprio (`pc_e2e_world_end`).
- Cache: `sandbox/pc-e2e-world-<chave>` (chave = sha do bundle + arvores `scripts/models/config/install` do HEAD + diff). Primeira montagem ~35 min (treino); depois ~0.
  Mudou o codigo de produto => chave nova => mundo novo (o `/tidy` apaga por idade).
- O filho do lancador HERDA o ambiente do teste inteiro (inclusive `R_LIBS_USER` do R hospedeiro): o e2e prova que o lancador e hermetico (o achado virou correcao do lancador
  neste ticket, `docs/estado/install.md`). So o git e cegado ao repo que hospeda `sandbox/` (`GIT_CEILING_DIRECTORIES`), para o `code_commit` ser `unknown` como numa raiz real.

## Resultado (Linux; bundle `0.9.0-e2e`, build jammy desta ponta)
- AC1: deck `end` x o MESMO deck pre-deslocado `start`: parquets da entrega byte-identicos (Delta 0); `target_datetime` = `interval_end` (1o previsto = available_until + 30 min,
  grade de 30 min). CONTROL: pre-deslocado declarado `end` recua o corte exatamente 30 min (plano do `train-models --dry-run`) e a entrega muda (Delta != 0).
- AC2: `prevcarga-sync` (artefatos + forecasts) -> `forecast --origin O` -> `verificar --saida <s> --oficial`: PASS `mesma-plataforma`, exit 0, delta 0.
  CONTROL: valor oficial adulterado (manifest regerado) => exit 12 `repro.value_out_of_tol`. Load perturbado em O-1 NAO da exit 12 (ver desvios).
- AC3 Linux: `train-models --jobs 4` == `--jobs 1` byte a byte (artefatos). A perna Windows x Linux esta DEFERIDA (abaixo).

## Desvios do texto do ticket (decididos aqui; motivo)
- `forecast` nao tem `--jobs` (so `train-models`): a igualdade `--jobs 4` == `--jobs 1` e do treino (PSOCK), onde o paralelismo existe.
- Load perturbado em O-1: muda o `data_hash` e a `versao` da chave, logo `verificar --oficial` sai exit 13 `state.official_forecast_missing` (e `--referencia`, exit 2
  `contract.identity_mismatch`), nunca 12. O exit 12 e provado adulterando o VALOR da inferencia oficial (mesma chave, manifest regerado).
- CONTROL do rotulo: o corte de 08:30 nao tem pesos da combina (`contract.combina_input`); a entrega do CONTROL usa um intervalo a mais de carga para manter o corte em 09:00.

## Correcoes de integracao achadas pelo e2e (commits proprios nesta branch)
- `install/prevcarga` e `install/prevcarga.cmd`: hermeticos a `R_LIBS*`/`R_PROFILE_USER`/`R_ENVIRON_USER` herdados (decisao do Gabriel; ver `docs/estado/install.md`).
- `release_pack.R`: a sonda de manifest de deck lia `timezone$load` de TODO arquivo antes do filtro de nome; o `artifact.rds.manifest.json` real tem `timezone` em texto =>
  `generic.unexpected` em todo `release-pack --what=artifacts` sobre artefatos reais.
- `verify_repro.R`: `SHA256SUMS` na raiz de uma chave de forecasts (gravado pelo release-pack, copiado pelo sync) era "arquivo nao listado" => `verificar --oficial` nunca passava
  sobre entrega sincronizada.

## Pendencias
- [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] Windows x Linux: `verificar --trilho entre-plataformas` <= 0,1%, `--jobs` no Windows, CONTROL +0,2% (exit 12) e os recibos
  `cross-platform` (`pc_receipt_write`, um por bundle) so existem nesse gate. O corpo do braco esta em `tests/ab/test-cross-platform.R` (nunca executado) com SKIP explicito.
  O braco Linux publica a saida de referencia em `options(prevcarga.ab.shared)`. Nenhum recibo e gravado enquanto o braco Windows nao passar.
- (corrigido no M8.9) `areas`/`models`/`describe` serializavam campos de lista de 1 elemento como escalar; agora `as.list` (ADR-0040: lista e sempre array), regressao com 1 subsistema/1 area/1 modo + CONTROL em `test-introspection.R`.

## Fechamento (2026-10-31)
- `test-shell-sujo-linux.R` roda `forecast --origin O` pelo lancador instalado (shell sujo x `env -i`, entrega byte-identica) e fecha o DEFERRED M6.9; `docs/estado/sync.md` fecha o catalogo == bundle e o e2e install.sh+sync.
- ACs reescritos para o contrato real (discovery resolvido): dado alterado => exit 13/2; valor alterado => exit 12; `--jobs` provado no `train-models`.


- **Gate Windows (M8.11)**: os itens Windows deste modulo (Windows x Linux, `--jobs`, CONTROL +0,2%, recibos) sao EXECUTADOS pelo gate da VM Windows (`tests/windows/Invoke-WindowsGate.ps1`; roteiro `docs/reference/windows-gate.md`; estado `docs/estado/windows-gate.md`, itens W14). Pendente: Gabriel.
